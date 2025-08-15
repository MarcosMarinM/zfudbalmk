################################################################################
##                                                                            ##
##                     COMET MATCH REPORT ANALYSIS SCRIPT                     ##
##                                                                            ##
################################################################################


#### 1. INITIAL SETUP ####

# 1.1.1. Load necessary packages. 'pacman' is used to efficiently install and
# 1.1.2. load all dependencies.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  pdftools, stringr, dplyr, tidyr, purrr, knitr, readxl
)


#### 2. HELPER FUNCTIONS DEFINITION ####

### 2.1. Format Match Minutes

#' @title Format a vector of match minutes to handle added time.
#' @description Converts numeric minutes to a text format. The main logic is to
#' transform values like 451 into "45+1" or 902 into "90+2", which represent
#' stoppage time.
#' @param minutos A numeric vector of minutes.
#' @return A character vector with the formatted minutes.
formatear_minuto_partido <- function(minutos) {
  sapply(minutos, function(minuto) {
    # 2.1.1. Handle NA or non-numeric cases.
    if (is.na(minuto) || !is.numeric(minuto)) {
      return(as.character(minuto))
    }
    
    # 2.1.2. Business logic to detect added time (e.g., 451 -> "45+1").
    # 2.1.3. This is applied if the minute is greater than 140 and has at least 3 digits.
    if (minuto > 140 && nchar(as.character(minuto)) >= 3) {
      minuto_str <- as.character(minuto)
      base <- substr(minuto_str, 1, 2)
      added <- substr(minuto_str, 3, nchar(minuto_str))
      return(paste0(base, "+", added))
    } else {
      # 2.1.4. For standard minutes, return the value as a character.
      return(as.character(minuto))
    }
  })
}


### 2.2. Load and Process Player Data (from XLS)

#' @title Load and process player data from an XLS file.
#' @description Reads an .xlsx file containing demographic and position information
#' for players. It filters by player IDs found in the match reports and extracts
#' the most specific position available.
#' @param ruta_xls Path to the .xlsx file with player data.
#' @param ids_jugadoras_validas Vector of player IDs extracted from the reports.
#' @return A tibble with `id`, `posicion`, `nacionalidad`, `fecha_nacimiento`, and
#'   `ciudad_nacimiento` for the found players.
cargar_y_procesar_posiciones <- function(ruta_xls, ids_jugadoras_validas) {
  
  # 2.2.1. Verify that the file exists.
  if (!file.exists(ruta_xls)) {
    warning(paste("Player positions file not found:", ruta_xls, "- Continuing without position data."))
    return(tibble(id = character(), posicion = character(), nacionalidad = character(), fecha_nacimiento = as.POSIXct(character()), ciudad_nacimiento = character()))
  }
  
  message("Loading and processing positions file: ", basename(ruta_xls))
  
  # 2.2.2. Attempt to read the .xls file safely.
  datos_xls <- tryCatch({ read_excel(ruta_xls) }, error = function(e) {
    warning("Error reading the .xls file. Continuing without position data.")
    return(NULL)
  })
  
  # 2.2.3. Verify the existence of critical columns.
  columnas_requeridas <- c("FA ID Number", "Nationality Name", "Date Of Birth", "City Of Birth Name")
  if (is.null(datos_xls) || !all(columnas_requeridas %in% names(datos_xls))) {
    columnas_faltantes <- paste(setdiff(columnas_requeridas, names(datos_xls)), collapse=", ")
    warning(paste("The XLS file does not contain the required columns. Missing:", columnas_faltantes))
    return(tibble(id = character(), posicion = character(), nacionalidad = character(), fecha_nacimiento = as.POSIXct(character()), ciudad_nacimiento = character()))
  }
  
  # 2.2.4. Rename and prepare columns.
  datos_xls <- datos_xls %>%
    select(-any_of("ID")) %>% 
    rename(
      ID = `FA ID Number`,
      nacionalidad = `Nationality Name`,
      fecha_nacimiento = `Date Of Birth`,
      ciudad_nacimiento = `City Of Birth Name`
    ) %>%
    mutate(ID = as.character(ID))
  
  # 2.2.5. Filter to keep only players present in the match reports.
  datos_relevantes_xls <- datos_xls %>% 
    filter(ID %in% as.character(ids_jugadoras_validas))
  
  if(nrow(datos_relevantes_xls) == 0) {
    message("None of the players in the XLS file match the players from the reports.")
    return(tibble(id = character(), posicion = character(), nacionalidad = character(), fecha_nacimiento = as.POSIXct(character()), ciudad_nacimiento = character()))
  }
  
  # 2.2.6. Define position and demographic data columns.
  columnas_posiciones <- c("GK", "DL", "DC", "DR", "DM", "WBL", "WBR", "ML", "MC", "MR", "AML", "AMC", "AMR", "SC")
  columnas_adicionales <- c("nacionalidad", "fecha_nacimiento", "ciudad_nacimiento")
  
  columnas_a_seleccionar <- intersect(
    c("ID", "Position Partially Known", columnas_posiciones, columnas_adicionales), 
    names(datos_relevantes_xls)
  )
  datos_filtrados <- datos_relevantes_xls %>% select(all_of(columnas_a_seleccionar))
  
  # 2.2.7. Iterate over each player from the XLS file to extract their position.
  posiciones_final <- pmap_dfr(datos_filtrados, function(...) {
    fila_actual <- list(...)
    id_jugadora <- fila_actual$ID
    
    # 2.2.8. Extract demographic data.
    nacionalidad_jugadora <- fila_actual$nacionalidad
    fecha_nac_jugadora <- fila_actual$fecha_nacimiento
    ciudad_nac_jugadora <- fila_actual$ciudad_nacimiento
    
    # 2.2.9. STEP 1: Search for a specific position (value in position column == 20).
    pos_exactas_encontradas <- character(0)
    for (pos_code in intersect(columnas_posiciones, names(fila_actual))) {
      if (!is.na(fila_actual[[pos_code]]) && fila_actual[[pos_code]] == 20) {
        pos_exactas_encontradas <- c(pos_exactas_encontradas, pos_code)
      }
    }
    
    # 2.2.10. If a specific position is found, use it and finalize for this player.
    if (length(pos_exactas_encontradas) > 0) {
      return(tibble(
        id = id_jugadora, 
        posicion = pos_exactas_encontradas,
        nacionalidad = nacionalidad_jugadora,
        fecha_nacimiento = fecha_nac_jugadora,
        ciudad_nacimiento = ciudad_nac_jugadora
      ))
    }
    
    # 2.2.11. STEP 2: If no specific position, use the general position (Defender, Midfielder, etc.).
    pos_parcial <- fila_actual[["Position Partially Known"]]
    if (!is.null(pos_parcial) && !is.na(pos_parcial)) {
      posicion_agrupada <- case_when(
        pos_parcial == 1 ~ "Defensa",
        pos_parcial == 2 ~ "Centrocampista",
        pos_parcial == 3 ~ "Delantera",
        TRUE ~ NA_character_
      )
      if (!is.na(posicion_agrupada)) {
        return(tibble(
          id = id_jugadora, 
          posicion = posicion_agrupada,
          nacionalidad = nacionalidad_jugadora,
          fecha_nacimiento = fecha_nac_jugadora,
          ciudad_nacimiento = ciudad_nac_jugadora
        ))
      }
    }
    
    # 2.2.12. STEP 3: If no data is found, return an empty tibble to maintain consistency.
    return(tibble(id = character(), posicion = character(), nacionalidad = character(), fecha_nacimiento = as.POSIXct(character()), ciudad_nacimiento = character()))
  })
  
  # 2.2.13. Final cleanup to remove players without an assigned position and duplicates.
  posiciones_final <- posiciones_final %>% 
    filter(!is.na(posicion), trimws(posicion) != "") %>%
    distinct()
  
  message("Position processing completed. Found ", nrow(posiciones_final), " assignments.")
  
  return(posiciones_final)
}


#### 3. PDF PARSING FUNCTIONS ####

### 3.1. Parse Lineups and Substitutions Block

#' @title Parse the lineup text block to extract players and substitutions.
#' @description A complex function that identifies home and away team columns,
#' extracts data for each player (jersey number, name, ID, if they are a goalkeeper
#' or captain), and associates substitutions with the players entering the field.
#' @param bloque_texto The text fragment from the PDF containing the lineups.
#' @return A list with dataframes for `alineacion_local`, `cambios_local`,
#'   `alineacion_visitante`, and `cambios_visitante`.
parsear_bloque_jugadoras_final <- function(bloque_texto, es_partido_seleccion = FALSE, equipo_local_nombre = "") {
  lineas_raw_originales <- str_split(bloque_texto, "\\n")[[1]]
  regex_sub_local <- "\\((\\d{1,2})\\)\\s*\\d{1,3}\\b"
  all_matches <- str_locate_all(lineas_raw_originales, regex_sub_local)
  candidate_splits <- map_dbl(all_matches, function(matches_on_line) {
    if (nrow(matches_on_line) == 0) return(NA_real_)
    local_subs <- matches_on_line[matches_on_line[, "start"] < 90, , drop = FALSE]
    if (nrow(local_subs) == 0) return(NA_real_)
    return(max(local_subs[, "end"]))
  })
  split_col <- 70
  if (any(!is.na(candidate_splits))) {
    split_col <- max(candidate_splits, na.rm = TRUE) + 2
  }
  if (length(lineas_raw_originales) > 0) {
    linea_referencia <- lineas_raw_originales[which.max(nchar(lineas_raw_originales))]
    split_col <- min(split_col, nchar(linea_referencia))
    while (split_col > 1 && str_sub(linea_referencia, split_col, split_col) != " ") {
      split_col <- split_col - 1
    }
  }
  
  lineas_divididas <- map(lineas_raw_originales, function(linea) {
    split_col_linea <- split_col
    if (split_col_linea > 1 && nchar(linea) >= split_col_linea) {
      char_antes <- str_sub(linea, split_col_linea - 1, split_col_linea - 1)
      char_en <- str_sub(linea, split_col_linea, split_col_linea)
      if (str_detect(char_antes, "\\d") && str_detect(char_en, "\\d")) {
        split_col_linea <- split_col_linea - 1
      }
    }
    list(
      local = str_sub(linea, 1, split_col_linea - 1),
      visitante = str_sub(linea, split_col_linea, -1)
    )
  })
  
  lineas_local <- map_chr(lineas_divididas, "local")
  lineas_visitante <- map_chr(lineas_divididas, "visitante")
  
  # 3.1.1. MODIFICATION START: The internal function is modified to handle conditional parsing logic.
  extraer_de_columna <- function(lineas_columna, equipo_tag, es_partido_seleccion, equipo_local_nombre) {
    lineas_procesadas <- list()
    i <- 1
    while (i <= length(lineas_columna)) {
      linea_actual_texto <- lineas_columna[i]
      if (i < length(lineas_columna)) {
        linea_siguiente_texto <- lineas_columna[i+1]
        
        # 3.1.2. Logic to join split lines (e.g., name on one line, ID on the next).
        # 3.1.3. For foreign teams (without ID), this logic should not apply.
        usa_logica_id <- TRUE
        if (es_partido_seleccion) {
          es_local_macedonia <- str_detect(equipo_local_nombre, "Македонија")
          # 3.1.4. The current column is Macedonian if (it's home and home is Macedonia) OR (it's away and home is NOT Macedonia)
          es_columna_macedonia <- (equipo_tag == "local" && es_local_macedonia) || (equipo_tag == "visitante" && !es_local_macedonia)
          if (!es_columna_macedonia) usa_logica_id <- FALSE
        }
        
        if ( usa_logica_id && (str_detect(linea_actual_texto, "\\b\\d{1,2}\\b\\s+[\\p{L}]") && !str_detect(linea_actual_texto, "\\b\\d{5,6}\\b")) &&
             (str_detect(linea_siguiente_texto, "^\\s*(\\d{5,6})\\s*$") || str_detect(linea_siguiente_texto, "^\\s+[\\p{L}]")) ) {
          linea_unida <- paste(str_squish(linea_actual_texto), str_squish(linea_siguiente_texto))
          lineas_procesadas[[length(lineas_procesadas) + 1]] <- list(texto = linea_unida, idx_original = i)
          i <- i + 2
          next
        }
        if (str_detect(linea_actual_texto, "\\(\\d{1,3}\\)") && str_detect(str_squish(linea_siguiente_texto), "^(\\((\\d{1,3})\\)\\s*)+$")) {
          linea_unida <- paste(trimws(linea_actual_texto, "right"), str_squish(linea_siguiente_texto))
          lineas_procesadas[[length(lineas_procesadas) + 1]] <- list(texto = linea_unida, idx_original = i)
          i <- i + 2
          next
        }
      }
      lineas_procesadas[[length(lineas_procesadas) + 1]] <- list(texto = linea_actual_texto, idx_original = i)
      i <- i + 1
    }
    
    jugadoras_col <- list()
    
    # 3.1.5. Determine if the current column corresponds to the foreign team.
    es_columna_extranjera <- FALSE
    if (es_partido_seleccion) {
      es_local_macedonia <- str_detect(equipo_local_nombre, "Македонија")
      es_columna_macedonia <- (equipo_tag == "local" && es_local_macedonia) || (equipo_tag == "visitante" && !es_local_macedonia)
      es_columna_extranjera <- !es_columna_macedonia
    }
    
    # 3.1.6. Choose the regular expression based on the team type.
    if (es_columna_extranjera) {
      # 3.1.7. Improved regex: Defines the name structure (words separated by spaces)
      # 3.1.8. instead of depending on what comes after. This correctly captures
      # 3.1.9. names even if the line ends immediately after them.
      regex_jugadora <- "^\\s*(\\d{1,2})\\b\\s+([\\p{L}'’.-]+(?:\\s[\\p{L}'’.-]+)*)"
    } else {
      # 3.1.10. Original regex: strict, requires a 5 or 6-digit numeric ID.
      regex_jugadora <- "^\\s*(\\d{1,2})\\b\\s+([\\p{L}][\\p{L}\\s'-]*)\\s+.*?(\\d{5,6})\\b"
    }
    
    for(item in lineas_procesadas) {
      linea_texto <- item$texto
      linea_idx_original <- item$idx_original
      player_matches <- str_locate_all(linea_texto, regex_jugadora)[[1]]
      player_data <- str_match_all(linea_texto, regex_jugadora)[[1]]
      
      if (nrow(player_data) > 0) {
        for (p in 1:nrow(player_data)) {
          nombre_raw <- player_data[p, 3]
          es_portera <- str_detect(linea_texto, "\\s+Г\\b") # 3.1.11. Search original line to capture "Г К"
          es_capitana <- str_detect(linea_texto, "\\s+К\\b")
          nombre_limpio <- str_remove_all(nombre_raw, "\\b[GKКГ]\\b") %>% str_squish()
          
          # 3.1.12. Assign ID if it exists, or NA if it's the foreign team.
          id_jugadora <- if (es_columna_extranjera) NA_character_ else player_data[p, 4]
          
          jugadoras_col[[length(jugadoras_col) + 1]] <- list(
            dorsal = as.integer(player_data[p, 2]),
            nombre = nombre_limpio,
            es_portera = es_portera,
            es_capitana = es_capitana,
            id = id_jugadora,
            equipo = equipo_tag,
            pos_start = player_matches[p, "start"],
            pos_end = player_matches[p, "end"],
            linea_idx = linea_idx_original
          )
        }
      }
    }
    return(jugadoras_col)
  }
  # 3.1.13. MODIFICATION END
  
  # 3.1.14. Process the home and away columns, passing the new arguments.
  jugadoras_local <- extraer_de_columna(lineas_local, "local", es_partido_seleccion, equipo_local_nombre)
  jugadoras_visitante <- extraer_de_columna(lineas_visitante, "visitante", es_partido_seleccion, equipo_local_nombre)
  jugadoras_visitante <- map(jugadoras_visitante, function(j) { j$pos_start <- j$pos_start + split_col; j$pos_end <- j$pos_end + split_col; return(j) })
  jugadoras <- c(jugadoras_local, jugadoras_visitante)
  
  # 3.1.15. Logic to identify and associate substitutions with players.
  cambios <- list()
  for(i in seq_along(lineas_raw_originales)) {
    linea <- lineas_raw_originales[i]
    sub_matches <- str_locate_all(linea, "\\((\\d{1,2})\\)\\s*(\\d{1,2})\\b")[[1]]
    sub_data <- str_match_all(linea, "\\((\\d{1,2})\\)\\s*(\\d{1,2})\\b")[[1]]
    if (nrow(sub_data) > 0) {
      for (s in 1:nrow(sub_data)) {
        cambios[[length(cambios) + 1]] <- list(d_sale = as.integer(sub_data[s, 2]), minuto = as.integer(sub_data[s, 3]), pos_start = sub_matches[s, "start"], linea_idx = i)
      }
    }
  }
  for (i in seq_along(cambios)) {
    cambio_actual <- cambios[[i]]
    es_cambio_local <- cambio_actual$pos_start < split_col
    indices_candidatas <- which(sapply(jugadoras, function(j) {
      es_jugadora_local <- j$equipo == "local"
      misma_columna <- (es_jugadora_local && es_cambio_local) || (!es_jugadora_local && !es_cambio_local)
      misma_linea <- j$linea_idx == cambio_actual$linea_idx
      esta_antes <- j$pos_end < cambio_actual$pos_start
      return(misma_columna && misma_linea && esta_antes)
    }))
    if (length(indices_candidatas) > 0) {
      distancias <- sapply(indices_candidatas, function(idx) cambio_actual$pos_start - jugadoras[[idx]]$pos_end)
      jugadora_candidata_idx <- indices_candidatas[which.min(distancias)]
    } else { jugadora_candidata_idx <- 0 }
    if (length(jugadora_candidata_idx) > 0 && jugadora_candidata_idx > 0) {
      if (is.null(jugadoras[[jugadora_candidata_idx]]$sustitucion)) {
        jugadoras[[jugadora_candidata_idx]]$sustitucion <- list(d_sale = cambio_actual$d_sale, min = cambio_actual$minuto)
      }
    }
  }
  
  # 3.1.16. Internal function to build the final dataframes for each team.
  construir_equipo_df <- function(lista_jugadoras, equipo_filtro) {
    jugadoras_equipo <- Filter(function(j) j$equipo == equipo_filtro, lista_jugadoras)
    if (length(jugadoras_equipo) == 0) return(list(alineacion = data.frame(id=character(), dorsal = integer(), nombre = character(), es_portera = logical(), es_capitana = logical(), tipo = character()), cambios = data.frame(minuto = integer(), texto = character())))
    jugadoras_equipo <- jugadoras_equipo[order(sapply(jugadoras_equipo, `[[`, "linea_idx"), sapply(jugadoras_equipo, `[[`, "pos_start"))]
    alineacion_df <- map_dfr(jugadoras_equipo, ~data.frame(id = .x$id, dorsal = .x$dorsal, nombre = .x$nombre, es_portera = .x$es_portera, es_capitana = .x$es_capitana))
    alineacion_df$tipo <- "Suplente"
    if(nrow(alineacion_df) > 0) alineacion_df$tipo[1:min(11, nrow(alineacion_df))] <- "Titular"
    cambios_df <- data.frame(minuto = integer(), texto = character())
    for (j in jugadoras_equipo) {
      if (!is.null(j$sustitucion)) {
        n_sale_row <- filter(alineacion_df, dorsal == j$sustitucion$d_sale)
        n_sale <- if(nrow(n_sale_row) > 0) n_sale_row$nombre[1] else paste("Dorsal", j$sustitucion$d_sale)
        cambios_df <- rbind(cambios_df, data.frame(minuto = j$sustitucion$min, texto = paste0("  - Min ", j$sustitucion$min, ": Entra ", j$nombre, " (", j$dorsal, ") por ", n_sale, " (", j$sustitucion$d_sale, ")")))
      }
    }
    return(list(alineacion = alineacion_df, cambios = cambios_df))
  }
  
  res_local <- construir_equipo_df(jugadoras, "local")
  res_visitante <- construir_equipo_df(jugadoras, "visitante")
  
  return(list(alineacion_local = res_local$alineacion, cambios_local = res_local$cambios, alineacion_visitante = res_visitante$alineacion, cambios_visitante = res_visitante$cambios))
}


### 3.2. Extract Cards (Yellow and Red)

#' @title Extract card information from the report text.
#' @description Searches for the "Amonestaciones" (Cautions) and "Expulsiones"
#' (Dismissals) sections and parses the data for each card: player, team, minute,
#' type, and reason. It handles both direct red cards and double yellow cards.
#' @param texto_acta Full text from the PDF.
#' @param equipo_local_nombre Home team name.
#' @param equipo_visitante_nombre Away team name.
#' @param alineacion_local Dataframe with the home team's lineup.
#' @param alineacion_visitante Dataframe with the away team's lineup.
#' @return A dataframe with all the cards from the match.
extraer_tarjetas <- function(texto_acta, equipo_local_nombre, equipo_visitante_nombre, alineacion_local, alineacion_visitante) {
  tarjetas_df_final <- data.frame()
  
  # 3.2.1. Extract yellow cards.
  bloque_amarillas_raw <- str_extract(texto_acta, "Опомени:([\\s\\S]*?)(?=Исклучување:|Ж: Жолт картон|ПОТВРДЕН|ОДИГРАН)")
  if (!is.na(bloque_amarillas_raw)) {
    texto_limpio <- str_remove(bloque_amarillas_raw, "Опомени:") %>% str_replace_all("\\n", " ") %>% str_squish()
    regex_tarjeta_amarilla <- "(\\d{1,2})\\s+[\\p{L}\\s.'-]+?\\s+\\((\\d{1,3})\\s*-\\s*([^)]+?)\\)"
    bloques_por_equipo <- str_split(texto_limpio, paste0("(?=", equipo_local_nombre, "|", equipo_visitante_nombre, ")"))[[1]]
    for (bloque_equipo in bloques_por_equipo) {
      if (str_trim(bloque_equipo) == "") next
      equipo_actual <- if (str_detect(bloque_equipo, paste0("^", equipo_local_nombre))) equipo_local_nombre else equipo_visitante_nombre
      matches <- str_match_all(bloque_equipo, regex_tarjeta_amarilla)[[1]]
      if (nrow(matches) > 0) {
        for (i in 1:nrow(matches)) {
          dorsal_tarjeta <- as.integer(matches[i, 2]); minuto_tarjeta <- as.integer(matches[i, 3]); motivo_tarjeta <- str_trim(matches[i, 4])
          alineacion_a_buscar <- if (equipo_actual == equipo_local_nombre) alineacion_local else alineacion_visitante
          info_jugadora <- filter(alineacion_a_buscar, dorsal == dorsal_tarjeta)
          nombre_jugadora <- if(nrow(info_jugadora) > 0) info_jugadora$nombre[1] else paste("Desconocida (Dorsal", dorsal_tarjeta, ")")
          id_jugadora <- if(nrow(info_jugadora) > 0) info_jugadora$id[1] else NA_character_
          tarjetas_df_final <- rbind(tarjetas_df_final, data.frame(jugadora = nombre_jugadora, id_jugadora = id_jugadora, equipo = equipo_actual, dorsal = dorsal_tarjeta, minuto = minuto_tarjeta, tipo = "Amarilla", motivo = motivo_tarjeta))
        }
      }
    }
  }
  
  # 3.2.2. Extract red cards.
  bloque_rojas_raw <- str_extract(texto_acta, "Исклучување:([\\s\\S]*?)(?=Опомени:|Ж: Жолт картон|ПОТВРДЕН|ОДИГРАН|Забелешка)")
  if (!is.na(bloque_rojas_raw)) {
    texto_limpio <- str_remove(bloque_rojas_raw, "Исклучување:") %>% str_replace_all("\\n", " ") %>% str_squish()
    bloques_por_equipo <- str_split(texto_limpio, paste0("(?=", equipo_local_nombre, "|", equipo_visitante_nombre, ")"))[[1]]
    for (bloque_equipo in bloques_por_equipo) {
      if (str_trim(bloque_equipo) == "") next
      equipo_actual <- if (str_detect(bloque_equipo, paste0("^", equipo_local_nombre))) equipo_local_nombre else equipo_visitante_nombre
      
      # 3.2.3. Case: Double yellow card.
      regex_doble_amarilla <- "(\\d{1,2})\\s+([\\p{L}\\s.'-]+?)\\s+\\((\\d{1,3})\\s*-\\s*(.+?)\\s+и\\s+(\\d{1,3})\\s*-\\s*(.+?)\\)"
      matches_doble <- str_match_all(bloque_equipo, regex_doble_amarilla)[[1]]
      if (nrow(matches_doble) > 0) {
        for(i in 1:nrow(matches_doble)) {
          dorsal_tarjeta <- as.integer(matches_doble[i, 2]); nombre_raw <- str_trim(matches_doble[i, 3]); minuto1 <- as.integer(matches_doble[i, 4]); motivo1 <- str_trim(matches_doble[i, 5]); minuto2 <- as.integer(matches_doble[i, 6]); motivo2 <- str_trim(matches_doble[i, 7])
          alineacion_a_buscar <- if (equipo_actual == equipo_local_nombre) alineacion_local else alineacion_visitante
          info_jugadora <- filter(alineacion_a_buscar, dorsal == dorsal_tarjeta)
          nombre_jugadora <- if(nrow(info_jugadora) > 0) info_jugadora$nombre[1] else nombre_raw
          id_jugadora <- if(nrow(info_jugadora) > 0) info_jugadora$id[1] else NA_character_
          tarjetas_df_final <- rbind(tarjetas_df_final, data.frame(jugadora=nombre_jugadora, id_jugadora=id_jugadora, equipo=equipo_actual, dorsal=dorsal_tarjeta, minuto=minuto1, tipo="Amarilla", motivo=motivo1),
                               data.frame(jugadora=nombre_jugadora, id_jugadora=id_jugadora, equipo=equipo_actual, dorsal=dorsal_tarjeta, minuto=minuto2, tipo="Amarilla", motivo=motivo2), data.frame(jugadora=nombre_jugadora, id_jugadora=id_jugadora, equipo=equipo_actual, dorsal=dorsal_tarjeta, minuto=minuto2, tipo="Roja", motivo="Doble amarilla"))
        }
        bloque_equipo <- str_remove_all(bloque_equipo, regex_doble_amarilla)
      }
      
      # 3.2.4. Case: Direct red card.
      if (str_trim(bloque_equipo) != "") {
        regex_roja_directa <- "(\\d{1,2})\\s+[\\p{L}\\s.'-]+?\\s+\\((\\d{1,3})\\s*-\\s*([^)]+?)\\)"
        matches_roja <- str_match_all(bloque_equipo, regex_roja_directa)[[1]]
        if(nrow(matches_roja) > 0) {
          for(i in 1:nrow(matches_roja)) {
            dorsal_tarjeta <- as.integer(matches_roja[i, 2]); minuto_tarjeta <- as.integer(matches_roja[i, 3]); motivo_tarjeta <- str_trim(matches_roja[i, 4])
            alineacion_a_buscar <- if (equipo_actual == equipo_local_nombre) alineacion_local else alineacion_visitante
            info_jugadora <- filter(alineacion_a_buscar, dorsal == dorsal_tarjeta)
            nombre_jugadora <- if(nrow(info_jugadora) > 0) info_jugadora$nombre[1] else paste("Desconocida (Dorsal", dorsal_tarjeta, ")")
            id_jugadora <- if(nrow(info_jugadora) > 0) info_jugadora$id[1] else NA_character_
            tarjetas_df_final <- rbind(tarjetas_df_final, data.frame(jugadora=nombre_jugadora, id_jugadora=id_jugadora, equipo=equipo_actual, dorsal=dorsal_tarjeta, minuto=minuto_tarjeta, tipo="Roja", motivo=motivo_tarjeta))
          }
        }
      }
    }
  }
  return(tarjetas_df_final)
}

### 3.3. Extract Penalty Shootout Data

#' @title Extract penalty shootout information from the report text.
#' @description Searches for a specific text block containing the penalty takers,
#' parses each shot to identify the player, their team, and whether they scored or missed.
#' @param texto_acta Full text from the PDF.
#' @param equipo_local_nombre Home team name.
#' @param equipo_visitante_nombre Away team name.
#' @param alineacion_local Dataframe with the home team's lineup.
#' @param alineacion_visitante Dataframe with the away team's lineup.
#' @return A dataframe with the penalty shootout attempts.
extraer_tanda_penales <- function(texto_acta, equipo_local_nombre, equipo_visitante_nombre, alineacion_local, alineacion_visitante) {
  # 3.3.1. Pattern to find the start of the section of interest.
  # 3.3.2. Can be "Пен/АГ" or the goalscorers line "Стрелец/Мин..."
  inicio_bloque_pattern <- "Пен/АГ|Стрелец/Мин[^\n]+"
  
  # 3.3.3. Extract all text AFTER our starting point.
  texto_despues_inicio <- str_extract(texto_acta, paste0(inicio_bloque_pattern, "[\\s\\S]*"))
  
  if (is.na(texto_despues_inicio)) {
    return(data.frame())
  }
  
  # 3.3.4. Now, from that extracted text, take only up to the next section ("Опомени:").
  # 3.3.5. This avoids a variable-length look-behind.
  bloque_penales_raw <- str_extract(texto_despues_inicio, "[\\s\\S]+?(?=Опомени:|Забелешка|ПОТВРДЕН)")
  
  # 3.3.6. Remove the "Пен/АГ" line itself from the block to avoid processing it by mistake.
  bloque_penales_raw <- str_remove(bloque_penales_raw, inicio_bloque_pattern)
  
  if (is.na(bloque_penales_raw)) {
    return(data.frame())
  }
  
  lineas_penales <- str_split(bloque_penales_raw, "\\n")[[1]]
  lineas_penales <- lineas_penales[str_detect(lineas_penales, "^\\s*[PO]\\s+\\d{1,2}")]
  
  if (length(lineas_penales) == 0) {
    return(data.frame())
  }
  
  # 3.3.7. Determine the split column (similar to lineups).
  split_col <- 80 
  
  lineas_local <- str_sub(lineas_penales, 1, split_col - 1)
  lineas_visitante <- str_sub(lineas_penales, split_col, -1)
  
  penales_df <- data.frame()
  
  # 3.3.8. Internal function to process a column of penalty takers.
  procesar_columna_penales <- function(lineas, equipo_nombre, alineacion_equipo) {
    df_columna <- data.frame()
    regex_lanzador <- "^\\s*([PO])\\s+(\\d{1,2})\\s+([\\p{L}\\s.'-]+)"
    
    for (linea in lineas) {
      match <- str_match(linea, regex_lanzador)
      if (!is.na(match[1, 1])) {
        resultado_lanzamiento <- ifelse(match[1, 2] == "P", "Gol", "Fallo")
        dorsal_lanzador <- as.integer(match[1, 3])
        nombre_lanzador_raw <- str_trim(match[1, 4])
        
        info_jugadora <- filter(alineacion_equipo, dorsal == dorsal_lanzador)
        
        nombre_jugadora <- if(nrow(info_jugadora) > 0) info_jugadora$nombre[1] else nombre_lanzador_raw
        id_jugadora <- if(nrow(info_jugadora) > 0) info_jugadora$id[1] else NA_character_
        
        df_columna <- rbind(df_columna, data.frame(
          jugadora = nombre_jugadora,
          id_jugadora = id_jugadora,
          dorsal = dorsal_lanzador,
          equipo = equipo_nombre,
          resultado_penal = resultado_lanzamiento
        ))
      }
    }
    return(df_columna)
  }
  
  penales_local_df <- procesar_columna_penales(lineas_local, equipo_local_nombre, alineacion_local)
  penales_visitante_df <- procesar_columna_penales(lineas_visitante, equipo_visitante_nombre, alineacion_visitante)
  
  # 3.3.9. Combine the results from both teams.
  penales_df <- rbind(penales_local_df, penales_visitante_df)
  
  return(penales_df)
}


#### 4. MAIN PDF PROCESSING FUNCTION ####

#' @title Process a single PDF match report file.
#' @description This is the main orchestrator function for a single report. It
#' reads the PDF, extracts general match information, calls the parsing functions
#' for lineups, goals, and cards, and finally compiles all data into a
#' structured list and a text summary.
#' @param acta_path The path to the PDF report file.
#' @return A list containing dataframes for `partido_info`, `goles`, `tarjetas`,
#'   `alineacion_local`, etc., as well as a `resumen_texto` and `nota_arbitro`.
procesar_acta <- function(acta_path) {
  # 4.1.1. Read and perform initial preparation of the PDF text.
  nombre_archivo <- basename(acta_path)
  id_partido_match <- str_match(nombre_archivo, "match_(\\d+)_")
  id_partido <- if (!is.na(id_partido_match[1, 2])) id_partido_match[1, 2] else tools::file_path_sans_ext(nombre_archivo)
  texto_acta <- tryCatch({ paste(pdf_text(acta_path), collapse = "\n\n") }, error = function(e) { stop(paste("Error reading PDF:", e$message)) })
  if (is.null(texto_acta) || nchar(texto_acta) == 0) stop("PDF is empty or text could not be read.")
  
  # 4.1.2. SPECIAL CORRECTION: Clean up the team name "ЖФК ЏИ-ЏИ" throughout the text BEFORE parsing.
  texto_acta <- str_replace_all(texto_acta, "ЖФК ЏИ-ЏИ", "ЖФК ЏИ")
  
  # 4.1.3. Extract the main match information (competition, teams, result).
  regex_principal <- "ЗАПИСНИК\\s*\\n\\s*([\\p{L}\\s.'’\\d-]+?)(?:\\s+(\\d{2}/\\d{2}))?\\s*\\n\\s*([^-–\\n]+(?:\\s*/\\s*[^ -–\\n]+)?)\\s*[-–]\\s*([^-–\\n]+(?:\\s*/\\s*[^ -–\\n]+)?)[\\s\\S]*?(\\d+:\\d+.*)"
  partido_info_match <- str_match(texto_acta, regex_principal)
  if (is.na(partido_info_match[1, 1])) {
    stop(paste("Main information not found in", nombre_archivo))
  }
  competicion_nombre <- str_trim(partido_info_match[, 2])
  competicion_temporada <- str_trim(partido_info_match[, 3])
  equipo_local <- str_trim(partido_info_match[, 4])
  equipo_visitante <- str_trim(partido_info_match[, 5])
  resultado_final_str <- str_trim(str_split(partido_info_match[, 6], "\\n")[[1]][1])
  
  # 4.1.4. Detect national team matches.
  # 4.1.5. A boolean flag is created to condition subsequent logic.
  es_partido_seleccion <- str_detect(equipo_local, "Македонија") |
    str_detect(equipo_visitante, "Македонија")  
  # 4.1.6. Detect official result and penalty shootout.
  es_resultado_oficial <- str_detect(resultado_final_str, "\\*") || str_detect(tolower(texto_acta), "службен резултат")
  penales_match <- str_match(resultado_final_str, "PEN\\s*(\\d+):(\\d+)")
  penales_local <- if (!is.na(penales_match[1,1])) as.integer(penales_match[1,2]) else NA_integer_
  penales_visitante <- if (!is.na(penales_match[1,1])) as.integer(penales_match[1,3]) else NA_integer_
  
  resultado_limpio <- str_remove_all(resultado_final_str, "\\(PEN.*?\\)|\\*|\\(.*?\\)") %>% str_trim()
  goles_split <- as.integer(str_split(resultado_limpio, ":", simplify = TRUE))
  goles_local <- goles_split[1]
  goles_visitante <- goles_split[2]
  
  # 4.1.7. Extract specific data (date, time, matchday, stadium).
  fecha_hora_match <- str_match(texto_acta, "Датум/време: (\\d{2}\\.\\d{2}\\.\\d{4})\\s+(\\d{2}:\\d{2})")
  fecha <- if (!is.na(fecha_hora_match[1, 1])) fecha_hora_match[, 2] else "Desconocida"
  hora <- if (!is.na(fecha_hora_match[1, 1])) fecha_hora_match[, 3] else "Desconocida"
  jornada_match <- str_match(texto_acta, "Коло:\\s*([^\\s\\n]+)"); jornada <- if (!is.na(jornada_match[1, 2])) str_trim(jornada_match[, 2]) else NA_character_
  estadio_match <- str_match(texto_acta, "Игралиште:\\s*([^\n]+)")
  estadio <- if (!is.na(estadio_match[1, 1])) str_remove(estadio_match[1, 2], "\\s+Р\\.бр:.*$") %>% str_trim() else "Desconocido"
  
  # 4.1.8. Extract match category.
  categoria_match <- str_match(texto_acta, "Категорија:\\s*([^\\n]+)")
  categoria_partido <- if (!is.na(categoria_match[1, 2])) str_trim(categoria_match[, 2]) else NA_character_
  
  # 4.1.9. Internal function to extract data from lines with "Label: Value" format.
  extraer_info <- function(texto, etiqueta) {
    patron_regex <- paste0(etiqueta, "\\s*(.+?)(?:\\s{2,}|$)")
    match <- str_match(texto, patron_regex)
    if (!is.na(match[1, 2])) str_trim(match[1, 2]) else "Desconocido"
  }
  
  # 4.1.10. New helper function to parse referee's name and city.
  parsear_arbitro <- function(texto_completo) {
    if (is.na(texto_completo) || texto_completo == "Desconocido") {
      return(list(nombre = "Desconocido", ciudad = NA_character_))
    }
    match <- str_match(texto_completo, "^\\s*(.*?)\\s*(?:\\(([^)]+)\\))?\\s*$")
    nombre <- if (!is.na(match[1, 2])) trimws(match[1, 2]) else texto_completo
    ciudad <- if (!is.na(match[1, 3])) trimws(match[1, 3]) else NA_character_
    return(list(nombre = nombre, ciudad = ciudad))
  }
  
  # 4.1.11. Extract names and cities of the referees.
  info_arb_principal <- parsear_arbitro(extraer_info(texto_acta, "Гл\\.судија:"))
  arbitro_principal_nombre <- info_arb_principal$nombre
  arbitro_principal_ciudad <- info_arb_principal$ciudad
  info_arb_asist_1 <- parsear_arbitro(extraer_info(texto_acta, "1\\.\\s*помошен:"))
  arbitro_asist_1_nombre <- info_arb_asist_1$nombre
  arbitro_asist_1_ciudad <- info_arb_asist_1$ciudad
  info_arb_asist_2 <- parsear_arbitro(extraer_info(texto_acta, "2\\.\\s*помошен:"))
  arbitro_asist_2_nombre <- info_arb_asist_2$nombre
  arbitro_asist_2_ciudad <- info_arb_asist_2$ciudad
  
  entrenador_local <- "Desconocido"; entrenador_visitante <- "Desconocido"
  linea_entrenadores_match <- str_extract(texto_acta, ".*Шеф на стручен штаб.*")
  if (!is.na(linea_entrenadores_match)) {
    partes_entrenador <- str_split(linea_entrenadores_match, "Шеф на стручен штаб")[[1]]
    if (length(partes_entrenador) >= 1 && nchar(str_trim(partes_entrenador[1])) > 0) entrenador_local <- str_trim(partes_entrenador[1])
    if (length(partes_entrenador) >= 2 && nchar(str_trim(partes_entrenador[2])) > 0) entrenador_visitante <- str_trim(partes_entrenador[2])
    entrenador_local <- str_remove_all(entrenador_local, "\\d") %>% str_trim()
    entrenador_visitante <- str_remove_all(entrenador_visitante, "\\d") %>% str_trim()
  }
  
  nota_arbitro <- NA_character_
  if(es_resultado_oficial) {
    regex_nota <- "Забелешка(?:\\s+и\\s+потпис\\s+на\\s+главен\\s+судија)?:?\\s*([\\s\\S]*?)(?=Ж:\\s*Жолт\\s*картон|ПОТВРДЕН|ОДИГРАН)"
    nota_match <- str_match(texto_acta, regex_nota)
    if (!is.na(nota_match[1, 2])) {
      nota_arbitro <- str_trim(nota_match[1, 2])
    }
  }
  
  partido_df <- data.frame(
    id_partido, competicion_nombre, competicion_temporada, jornada, 
    categoria = categoria_partido, fecha, hora, 
    local = equipo_local, visitante = equipo_visitante, 
    goles_local, goles_visitante, penales_local, penales_visitante, 
    es_resultado_oficial
  )
  
  pos_start_players <- str_locate(texto_acta, "Голови\\s+Ж Ц\\s+З")
  pos_end_players <- str_locate(texto_acta, "Шеф на стручен штаб|Резервни играчи|Официјални претставници")
  alineacion_local <- data.frame(); cambios_local_df <- data.frame()
  alineacion_visitante <- data.frame(); cambios_visitante_df <- data.frame()
  
  if (!is.na(pos_start_players[1,1]) && !is.na(pos_end_players[1,1])) {
    bloque_jugadoras_completo <- str_sub(texto_acta, pos_start_players[1, "start"], pos_end_players[1, "start"] -1)
    resultados_parseo <- parsear_bloque_jugadoras_final(
      bloque_texto = bloque_jugadoras_completo,
      es_partido_seleccion = es_partido_seleccion,
      equipo_local_nombre = equipo_local
    )
    if(length(resultados_parseo) > 0) {
      alineacion_local <- resultados_parseo$alineacion_local; cambios_local_df <- resultados_parseo$cambios_local
      alineacion_visitante <- resultados_parseo$alineacion_visitante; cambios_visitante_df <- resultados_parseo$cambios_visitante
    }
  }
  
  goles_partido_actual <- data.frame()
  linea_resultados_full <- str_extract(texto_acta, "Резултат[^\n]+")
  linea_goleadores_full <- str_extract(texto_acta, "Стрелец/Мин[^\n]+")
  linea_pen_ag_full <- str_extract(texto_acta, "Пен/АГ[^\n]+")
  if (!is.na(linea_resultados_full) && !is.na(linea_goleadores_full)) {
    resultados_parciales <- str_split(str_trim(str_match(linea_resultados_full, "Резултат\\s+(.*)")[, 2]), "\\s+")[[1]]
    
    # 4.1.12. KEY MODIFICATION START: Extract goalscorer/minute text without converting to number yet.
    goleadores_minutos_raw <- str_split(str_trim(str_match(linea_goleadores_full, "Стрелец/Мин\\s+(.*)")[, 2]), "\\s+")[[1]]
    # 4.1.13. KEY MODIFICATION END
    
    es_autogol <- rep(FALSE, length(resultados_parciales))
    if (!is.na(linea_pen_ag_full)) {
      pos_goleadores_abs <- str_locate_all(linea_goleadores_full, "\\b\\d{1,2}\\s+(\\d{1,3}|[Xx])\\b")[[1]]
      pos_ag_todas_abs <- str_locate_all(linea_pen_ag_full, "АГ")[[1]]
      pos_header_ag <- str_locate(linea_pen_ag_full, "Пен/АГ")
      pos_ag_validas_abs <- pos_ag_todas_abs[pos_ag_todas_abs[, "start"] > pos_header_ag[, "end"], , drop = FALSE]
      if (!is.null(pos_goleadores_abs) && nrow(pos_goleadores_abs) > 0 && !is.null(pos_ag_validas_abs) && nrow(pos_ag_validas_abs) > 0) {
        for (i in 1:nrow(pos_goleadores_abs)) {
          for (j in 1:nrow(pos_ag_validas_abs)) {
            if (pos_ag_validas_abs[j, "start"] >= pos_goleadores_abs[i, "start"] && pos_ag_validas_abs[j, "start"] <= pos_goleadores_abs[i, "end"]) {
              if(length(es_autogol) >= i) es_autogol[i] <- TRUE
            }
          }
        }
      }
    }
    resultados_previo <- c(0, 0)
    for (i in 1:length(resultados_parciales)) {
      if ((i * 2) > length(goleadores_minutos_raw)) break
      marcador_actual <- as.integer(str_split(resultados_parciales[i], ":")[[1]]); if (any(is.na(marcador_actual))) next
      
      # 4.1.14. KEY MODIFICATION START
      dorsal_gol <- as.integer(goleadores_minutos_raw[(i * 2) - 1])
      minuto_gol_str <- goleadores_minutos_raw[i * 2]
      
      minuto_gol <- if (is.na(minuto_gol_str) || tolower(minuto_gol_str) == "x") {
        NA_integer_
      } else {
        as.integer(minuto_gol_str)
      }
      
      if (is.na(dorsal_gol)) next
      # 4.1.15. KEY MODIFICATION END
      
      tipo_gol <- if (length(es_autogol) >= i && es_autogol[i]) "Autogol" else "Normal"
      equipo_acreditado_gol <- NA
      if (marcador_actual[1] > resultados_previo[1]) equipo_acreditado_gol <- equipo_local
      else if (marcador_actual[2] > resultados_previo[2]) equipo_acreditado_gol <- equipo_visitante
      if (!is.na(equipo_acreditado_gol)) {
        equipo_jugadora_es_local <- if (tipo_gol == "Autogol") equipo_acreditado_gol == equipo_visitante else equipo_acreditado_gol == equipo_local
        alineacion_a_buscar <- if(equipo_jugadora_es_local) alineacion_local else alineacion_visitante
        equipo_jugadora <- if(equipo_jugadora_es_local) equipo_local else equipo_visitante
        jugadora_info <- filter(alineacion_a_buscar, dorsal == dorsal_gol)
        nombre_jugadora <- if (nrow(jugadora_info) > 0) jugadora_info$nombre[1] else paste("Desconocida (Dorsal", dorsal_gol, ")")
        goles_partido_actual <- rbind(goles_partido_actual, data.frame(id_partido, jugadora = nombre_jugadora, equipo_jugadora, equipo_acreditado = equipo_acreditado_gol, minuto = minuto_gol, dorsal = dorsal_gol, tipo = tipo_gol))
      }
      resultados_previo <- marcador_actual
    }
  }
  
  tarjetas_partido_actual <- extraer_tarjetas(texto_acta, equipo_local, equipo_visitante, alineacion_local, alineacion_visitante)
  if(nrow(tarjetas_partido_actual) > 0) tarjetas_partido_actual$id_partido <- id_partido
  
  penales_partido_actual <- extraer_tanda_penales(texto_acta, equipo_local, equipo_visitante, alineacion_local, alineacion_visitante)
  if(nrow(penales_partido_actual) > 0) penales_partido_actual$id_partido <- id_partido
  
  formatear_jugadoras <- function(df, tipo_jugadora) {
    if (is.null(df) || nrow(df) == 0) return(paste("  No", tolower(tipo_jugadora), "s found."))
    subset_df <- filter(df, tipo == tipo_jugadora)
    if (nrow(subset_df) == 0) return(paste("  No", tolower(tipo_jugadora), "s found."))
    apply(subset_df, 1, function(j) {
      nombre_display <- if(as.logical(j['es_capitana'])) paste0(j['nombre'], " (C)") else j['nombre']
      paste("  -", j['dorsal'], nombre_display)
    })
  }
  
  # 4.1.16. KEY MODIFICATION START
  formatear_goles_texto <- function(df_goles) {
    if (is.null(df_goles) || nrow(df_goles) == 0) return("No goals were scored or they could not be processed.")
    apply(df_goles, 1, function(g) {
      minuto_num <- as.numeric(g['minuto'])
      
      prefix_minuto <- if (is.na(minuto_num)) {
        "" # 4.1.17. Display nothing if the minute is NA
      } else {
        paste0("Min ", formatear_minuto_partido(minuto_num), " - ")
      }
      
      if (g['tipo'] == "Autogol") {
        paste0(prefix_minuto, "Goal by ", g['jugadora'], " (", g['equipo_jugadora'], ", own goal)")
      } else {
        paste0(prefix_minuto, "Goal by ", g['jugadora'], " (", g['equipo_acreditado'], ")")
      }
    })
  }
  # 4.1.18. KEY MODIFICATION END
  
  formatear_tarjetas_texto <- function(df_tarjetas) {
    if(is.null(df_tarjetas) || nrow(df_tarjetas) == 0) return("  No cards were recorded.")
    df_tarjetas <- df_tarjetas[order(df_tarjetas$minuto), ]
    apply(df_tarjetas, 1, function(t) paste0("  - Min ", formatear_minuto_partido(as.numeric(t['minuto'])), ": [", t['tipo'], "] for ", t['jugadora'], " (", t['equipo'], ") for '", t['motivo'], "'"))
  }
  
  formatear_penales_texto <- function(df_penales) {
    if (is.null(df_penales) || nrow(df_penales) == 0) return(NULL)
    
    lanzadores_local <- df_penales %>% filter(equipo == equipo_local)
    lanzadores_visitante <- df_penales %>% filter(equipo == equipo_visitante)
    
    textos_local <- apply(lanzadores_local, 1, function(p) {
      paste0("    ", if(p['resultado_penal'] == "Gol") "✓" else "✕", " ", p['jugadora'], " (", p['dorsal'], ")")
    })
    
    textos_visitante <- apply(lanzadores_visitante, 1, function(p) {
      paste0("    ", if(p['resultado_penal'] == "Gol") "✓" else "✕", " ", p['jugadora'], " (", p['dorsal'], ")")
    })
    
    c(paste("  ", equipo_local), textos_local, paste("  ", equipo_visitante), textos_visitante)
  }
  
  if (!is.null(cambios_local_df) && nrow(cambios_local_df) > 0) cambios_local_df <- cambios_local_df[order(cambios_local_df$minuto), ]
  if (!is.null(cambios_visitante_df) && nrow(cambios_visitante_df) > 0) cambios_visitante_df <- cambios_visitante_df[order(cambios_visitante_df$minuto), ]
  
  arbitro_principal_str <- if (!is.na(arbitro_principal_ciudad)) paste0(arbitro_principal_nombre, " (", arbitro_principal_ciudad, ")") else arbitro_principal_nombre
  arbitro_asist_1_str <- if (!is.na(arbitro_asist_1_ciudad)) paste0(arbitro_asist_1_nombre, " (", arbitro_asist_1_ciudad, ")") else arbitro_asist_1_nombre
  arbitro_asist_2_str <- if (!is.na(arbitro_asist_2_ciudad)) paste0(arbitro_asist_2_nombre, " (", arbitro_asist_2_ciudad, ")") else arbitro_asist_2_nombre
  
  resumen_actual_lines <- c(
    "\n======================================================================", paste("START OF REPORT:", nombre_archivo), "======================================================================",
    paste("COMPETITION:", competicion_nombre, if (!is.na(competicion_temporada)) competicion_temporada else ""),
    paste("MATCH SUMMARY:", equipo_local, "vs", equipo_visitante, "(ID:", id_partido, ")"),
    paste(
      paste("Date:", fecha, "| Time:", hora),
      if (!is.na(jornada)) paste("| Matchday:", jornada) else "",
      if (!is.na(categoria_partido)) paste("| Category:", categoria_partido) else ""
    ),
    paste("Stadium:", estadio), 
    paste("Final Score:", resultado_final_str, if(es_resultado_oficial) "(Official result)" else ""),
    "\n--- GOALS ---", formatear_goles_texto(goles_partido_actual),
    if (!is.na(penales_local)) c("\n--- PENALTY SHOOTOUT ---", formatear_penales_texto(penales_partido_actual)),
    "\n--- SANCTIONS (CARDS) ---", formatear_tarjetas_texto(tarjetas_partido_actual),
    "\n--- REFEREES AND OFFICIALS ---", 
    paste("  Head Referee:", arbitro_principal_str), 
    paste("  1st Assistant:", arbitro_asist_1_str), 
    paste("  2nd Assistant:", arbitro_asist_2_str),
    if (!is.na(nota_arbitro) && nchar(nota_arbitro) > 0) c("\n--- REFEREE'S NOTE ---", nota_arbitro),
    "\n--- LINEUPS ---", paste("\nHome Team:", equipo_local), paste("  Coach:", entrenador_local), "  Starters:", formatear_jugadoras(alineacion_local, "Titular"),
    "  Substitutes:", formatear_jugadoras(alineacion_local, "Suplente"), "  Changes:", if(!is.null(cambios_local_df) && nrow(cambios_local_df) > 0) cambios_local_df$texto else "  No changes were recorded.",
    paste("\nAway Team:", equipo_visitante), paste("  Coach:", entrenador_visitante), "  Starters:", formatear_jugadoras(alineacion_visitante, "Titular"),
    "  Substitutes:", formatear_jugadoras(alineacion_visitante, "Suplente"), "  Changes:", if(!is.null(cambios_visitante_df) && nrow(cambios_visitante_df) > 0) cambios_visitante_df$texto else "  No changes were recorded.",
    "\n======================================================================\n"
  )
  
  return(list(
    partido_info = partido_df, 
    goles = goles_partido_actual, 
    tarjetas = tarjetas_partido_actual, 
    penales = penales_partido_actual,
    resumen_texto = unlist(resumen_actual_lines), 
    alineacion_local = alineacion_local, 
    alineacion_visitante = alineacion_visitante, 
    cambios_local = cambios_local_df, 
    cambios_visitante = cambios_visitante_df, 
    arbitro_principal_nombre = arbitro_principal_nombre,
    arbitro_principal_ciudad = arbitro_principal_ciudad,
    arbitro_asist_1_nombre = arbitro_asist_1_nombre,
    arbitro_asist_1_ciudad = arbitro_asist_1_ciudad,
    arbitro_asist_2_nombre = arbitro_asist_2_nombre,
    arbitro_asist_2_ciudad = arbitro_asist_2_ciudad,
    estadio = estadio, 
    nota_arbitro = nota_arbitro
  ))
}


#### 5. MAIN EXECUTION WORKFLOW (WITH CACHING) ####

### 5.1. Define Paths and Load Cache

# 5.1.1. Path to the folder containing the PDF reports.
ruta_pdfs <- "Actas"
# 5.1.2. Path for the cache file where processed results will be saved.
ruta_cache <- "actas_cache.rds"

# 5.1.3. List all current PDF files in the folder.
archivos_pdf_actuales <- list.files(path = ruta_pdfs, pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE)
if (length(archivos_pdf_actuales) == 0) {
  stop("No PDF files found at the specified path. Please check the path.")
}

# 5.1.4. Attempt to load results from previous runs from the cache.
resultados_cacheados <- list()
if (file.exists(ruta_cache)) {
  message(paste("Loading previous results from:", ruta_cache))
  tryCatch({
    resultados_cacheados <- readRDS(ruta_cache)
    # 5.1.5. Ensure the cache is a list. If it's corrupt, start from scratch.
    if (!is.list(resultados_cacheados)) resultados_cacheados <- list()
  }, error = function(e) {
    warning("The cache file appears to be corrupt. All files will be reprocessed.")
    resultados_cacheados <- list()
  })
}

### 5.2. Detect File Changes

# 5.2.1. Get the base names of the files to use them as unique identifiers.
nombres_archivos_actuales <- basename(archivos_pdf_actuales)
nombres_archivos_cacheados <- names(resultados_cacheados)

# 5.2.2. Identify new files (on disk but not in cache) and deleted files (in cache but not on disk).
archivos_a_procesar_nombres <- setdiff(nombres_archivos_actuales, nombres_archivos_cacheados)
archivos_eliminados_nombres <- setdiff(nombres_archivos_cacheados, nombres_archivos_actuales)

# 5.2.3. Inform the user if reports have been deleted or modified.
if (length(archivos_eliminados_nombres) > 0) {
  message(paste(length(archivos_eliminados_nombres), "deleted or modified reports detected. Updating..."))
  # 5.2.4. Remove them from the cached results set.
  resultados_cacheados <- resultados_cacheados[!names(resultados_cacheados) %in% archivos_eliminados_nombres]
}

### 5.3. Process New Files Incrementally

# 5.3.1. Prepare the safe processor that wraps the main parsing function.
procesador_seguro <- purrr::safely(procesar_acta)
nuevos_resultados_completos <- list()

if (length(archivos_a_procesar_nombres) > 0) {
  message(paste("Starting processing for", length(archivos_a_procesar_nombres), "new report(s)..."))
  
  # 5.3.2. Get the full paths of the files that need to be processed.
  rutas_a_procesar <- archivos_pdf_actuales[basename(archivos_pdf_actuales) %in% archivos_a_procesar_nombres]
  names(rutas_a_procesar) <- basename(rutas_a_procesar)
  
  # 5.3.3. Apply the processing function only to the new files.
  nuevos_resultados_completos <- purrr::map(rutas_a_procesar, ~{
    message(paste("Processing:", basename(.x)))
    procesador_seguro(.x)
  })
  
} else {
  message("No new reports to process. All results loaded from cache.")
}

### 5.4. Handle Errors from New Processing

# 5.4.1. Separate successful results from errors, only for the newly processed files.
nuevos_resultados_exitosos <- purrr::map(nuevos_resultados_completos, "result") %>% purrr::compact()
errores_nuevos <- purrr::map(nuevos_resultados_completos, "error") %>% purrr::compact()

# 5.4.2. If there were errors in the new files, inform the user in detail.
if (length(errores_nuevos) > 0) {
  message("\n--- WARNING: Errors were found in ", length(errores_nuevos), " of the ", length(archivos_a_procesar_nombres), " new files. ---")
  purrr::walk2(names(errores_nuevos), errores_nuevos, ~message(paste0("\nERROR in file: ", .x, "\nMESSAGE: ", .y$message)))
} else if (length(archivos_a_procesar_nombres) > 0) {
  message("\nAll new files were processed without errors!")
}

### 5.5. Consolidate and Update Cache

# 5.5.1. Identify the results that were removed to get their data before deleting them.
resultados_eliminados <- list()
if (length(archivos_eliminados_nombres) > 0) {
  # 5.5.2. Load the old cache to find the data of the deleted files.
  if (file.exists(ruta_cache)) {
    cache_antiguo <- readRDS(ruta_cache)
    if (is.list(cache_antiguo)) {
      resultados_eliminados <- cache_antiguo[names(cache_antiguo) %in% archivos_eliminados_nombres]
    }
  }
}

# 5.5.3. Combine the results loaded from the cache with the results from the newly processed files.
resultados_exitosos <- c(resultados_cacheados, nuevos_resultados_exitosos)

# 5.5.4. Save the combined and updated list to the cache file for future runs.
tryCatch({
  saveRDS(resultados_exitosos, file = ruta_cache)
  message(paste("Cache updated with", length(resultados_exitosos), "processed reports. Saved to:", ruta_cache))
}, error = function(e) {
  warning("Could not save the cache file. Results will not be available in the next run.")
  message(paste("Original error:", e$message))
})


### 5.6. Save File Change Status

# 5.6.1. Determine if there were any changes (new or deleted files).
hubo_cambios <- length(archivos_a_procesar_nombres) > 0 || length(archivos_eliminados_nombres) > 0

# 5.6.2. Create a simple list containing only the names of the files that have changed.
# 5.6.3. A second script can use this information to determine which entities are affected.
info_cambios = list(
  hubo_cambios = hubo_cambios,
  archivos_nuevos_nombres = archivos_a_procesar_nombres,
  archivos_eliminados_nombres = archivos_eliminados_nombres
)

# 5.6.4. Save this information for use by a potential second script.
ruta_cache_info <- "cache_info.rds"
saveRDS(info_cambios, file = ruta_cache_info)
message(paste("File change information saved to:", ruta_cache_info))


#### 6. DATA AGGREGATION AND REPORTING ####

### 6.1. Consolidate Data

# 6.1.1. Combine the results from all reports into single dataframes.
partidos_df <- purrr::map_dfr(resultados_exitosos, "partido_info")
goles_df <- purrr::map_dfr(resultados_exitosos, "goles")
tarjetas_df <- purrr::map_dfr(resultados_exitosos, "tarjetas")


### 6.2. Enrich with Player Data (from XLS file)

# 6.2.1. Combine all lineups into a single dataframe.
alineaciones_completas <- map_dfr(resultados_exitosos, ~bind_rows(
  .x$alineacion_local,
  .x$alineacion_visitante
))

# 6.2.2. Verify the existence of the 'id' column before proceeding.
if ("id" %in% names(alineaciones_completas)) {
  ids_validas_de_actas <- alineaciones_completas %>%
    dplyr::pull("id") %>%
    unique() %>%
    na.omit()
} else {
  warning("The 'id' column was not found in the combined lineup data. Player data cannot be enriched.")
  ids_validas_de_actas <- character(0)
}

message(paste("\nA total of", length(ids_validas_de_actas), "unique players were found across all reports."))

# 6.2.3. Load and process additional data (position, nationality, etc.).
ruta_xls_posiciones <- "igraci.xlsx"
posiciones_df <- cargar_y_procesar_posiciones(ruta_xls_posiciones, ids_validas_de_actas)


### 6.3. Calculate Derived Tables

# 6.3.1. Function to generate the league standings table.
calcular_clasificacion <- function(partidos) {
  if (is.null(partidos) || nrow(partidos) == 0) return(data.frame(Message = "No valid matches were processed."))
  locales <- partidos %>% select(equipo = local, GF = goles_local, GC = goles_visitante)
  visitantes <- partidos %>% select(equipo = visitante, GF = goles_visitante, GC = goles_local)
  resultados_por_equipo <- bind_rows(locales, visitantes) %>%
    mutate(
      Pts = case_when(GF > GC ~ 3, GF < GC ~ 0, TRUE ~ 1),
      resultado = case_when(GF > GC ~ "PG", GF < GC ~ "PP", TRUE ~ "PE")
    )
  clasificacion <- resultados_por_equipo %>%
    group_by(Equipo = equipo) %>%
    summarise(PJ = n(), Pts = sum(Pts), PG = sum(resultado == "PG"), PE = sum(resultado == "PE"), PP = sum(resultado == "PP"), GF = sum(GF), GC = sum(GC), .groups = 'drop') %>%
    mutate(DG = GF - GC) %>%
    arrange(desc(Pts), desc(DG), desc(GF))
  return(clasificacion)
}

clasificacion_df <- calcular_clasificacion(partidos_df)

# 6.3.2. Create the top goalscorers table.
if (!is.null(goles_df) && nrow(goles_df) > 0) {
  # 6.3.3. Consider only 'Normal' goals and use 'equipo_acreditado' for the table.
  tabla_goleadoras <- goles_df %>% 
    filter(tipo == "Normal") %>% 
    group_by(Jugadora = jugadora, Equipo = equipo_acreditado) %>% 
    summarise(Goles = n(), .groups = 'drop') %>% 
    arrange(desc(Goles))
} else {
  tabla_goleadoras <- data.frame(Message = "No valid goals were processed.")
}

# 6.3.4. Create the sanctions table.
if (!is.null(tarjetas_df) && nrow(tarjetas_df) > 0) {
  tabla_sanciones <- tarjetas_df %>%
    group_by(Jugadora = jugadora, Equipo = equipo) %>%
    summarise(Amarillas = sum(tipo == "Amarilla"), Rojas = sum(tipo == "Roja"), .groups = 'drop') %>%
    filter(Amarillas > 0 | Rojas > 0) %>%
    arrange(desc(Rojas), desc(Amarillas))
} else {
  tabla_sanciones <- data.frame(Message = "No valid sanctions were processed.")
}


### 6.4. Write Output File

# 6.4.1. Generate the final text file with all summaries and tables.
ruta_salida_txt <- file.path(Sys.getenv("HOME"), "Downloads", "resumen_total.txt")
tryCatch({
  # 6.4.2. Collect all the text summaries from each match.
  resumenes_completos <- unlist(purrr::map(resultados_exitosos, "resumen_texto"))
  
  # 6.4.3. Format the final tables using 'knitr' for better visualization.
  goleadoras_formateadas <- c("\n\n========================= TOP GOALSCORERS TABLE =========================", capture.output(print(knitr::kable(tabla_goleadoras, format = "pipe", row.names = FALSE))), "")
  sanciones_formateadas <- c("\n\n========================== SANCTIONS TABLE ==========================", capture.output(print(knitr::kable(tabla_sanciones, format = "pipe", row.names = FALSE))), "")
  clasificacion_formateada <- c("\n\n============================= LEAGUE STANDINGS =============================", capture.output(print(knitr::kable(clasificacion_df, format = "pipe", row.names = FALSE))), "")
  
  # 6.4.4. Write the file in UTF-8 to ensure correct character display.
  con <- file(ruta_salida_txt, open = "wt", encoding = "UTF-8")
  writeLines(resumenes_completos, con)
  writeLines(goleadoras_formateadas, con)
  writeLines(sanciones_formateadas, con)
  writeLines(clasificacion_formateada, con)
  close(con)
  
  message(paste("\nPROCESS COMPLETED SUCCESSFULLY!"))
  message(paste("Check the output file at:", ruta_salida_txt))
  
}, error = function(e) {
  message("\nError writing the output file.")
  message(paste("Original error:", e$message))
})