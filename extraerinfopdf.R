################################################################################
##                                                                            ##
##                  SCRIPT DE ANÁLISIS DE ACTAS DE FÚTBOL                     ##
##                                                                            ##
################################################################################


## -------------------------------------------------------------------------- ##
##  0. CONFIGURACIÓN INICIAL
## -------------------------------------------------------------------------- ##

# Carga de paquetes necesarios. Se usa 'pacman' para instalar y cargar
# eficientemente todas las dependencias.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  pdftools, stringr, dplyr, tidyr, purrr, knitr, readxl
)


## -------------------------------------------------------------------------- ##
##  1. DEFINICIÓN DE FUNCIONES AUXILIARES
## -------------------------------------------------------------------------- ##

### 1.1. Formatear minutos de partido ----

#' Formatea un vector de minutos para manejar el tiempo añadido.
#'
#' Convierte minutos numéricos a formato de texto. La lógica principal es transformar
#' valores como 451 en "45+1" o 902 en "90+2", que representan el tiempo de descuento.
#'
#' @param minutos Un vector numérico de minutos.
#' @return Un vector de caracteres con los minutos formateados.
formatear_minuto_partido <- function(minutos) {
  sapply(minutos, function(minuto) {
    # Control de casos NA o no numéricos.
    if (is.na(minuto) || !is.numeric(minuto)) {
      return(as.character(minuto))
    }
    
    # Lógica de negocio para detectar tiempo añadido (e.g., 451 -> "45+1").
    # Se aplica si el minuto es mayor de 140 y tiene al menos 3 dígitos.
    if (minuto > 140 && nchar(as.character(minuto)) >= 3) {
      minuto_str <- as.character(minuto)
      base <- substr(minuto_str, 1, 2)
      added <- substr(minuto_str, 3, nchar(minuto_str))
      return(paste0(base, "+", added))
    } else {
      # Para minutos estándar, se devuelve el valor como caracter.
      return(as.character(minuto))
    }
  })
}


### 1.2. Cargar y procesar datos de jugadoras (XLS) ----

#' Carga y procesa datos de jugadoras desde un archivo XLS.
#'
#' Lee un archivo .xlsx que contiene información demográfica y de posición de las
#' jugadoras. Filtra por los IDs de jugadoras encontradas en las actas y extrae
#' la posición más específica disponible.
#'
#' @param ruta_xls Ruta al archivo .xlsx con datos de jugadoras.
#' @param ids_jugadoras_validas Vector de IDs de jugadoras extraídos de las actas.
#' @return Un tibble con `id`, `posicion`, `nacionalidad`, `fecha_nacimiento` y
#'   `ciudad_nacimiento` para las jugadoras encontradas.
cargar_y_procesar_posiciones <- function(ruta_xls, ids_jugadoras_validas) {
  
  # Verificación de la existencia del archivo.
  if (!file.exists(ruta_xls)) {
    warning(paste("Archivo de posiciones no encontrado:", ruta_xls, "- Se continuará sin datos de posición."))
    return(tibble(id = character(), posicion = character(), nacionalidad = character(), fecha_nacimiento = as.POSIXct(character()), ciudad_nacimiento = character()))
  }
  
  message("Cargando y procesando el archivo de posiciones: ", basename(ruta_xls))
  
  # Intenta leer el archivo .xls de forma segura.
  datos_xls <- tryCatch({ read_excel(ruta_xls) }, error = function(e) {
    warning("Error al leer el archivo .xls. Se continuará sin datos de posición.")
    return(NULL)
  })
  
  # Verificación de la existencia de columnas críticas.
  columnas_requeridas <- c("FA ID Number", "Nationality Name", "Date Of Birth", "City Of Birth Name")
  if (is.null(datos_xls) || !all(columnas_requeridas %in% names(datos_xls))) {
    columnas_faltantes <- paste(setdiff(columnas_requeridas, names(datos_xls)), collapse=", ")
    warning(paste("El archivo XLS no contiene las columnas requeridas. Faltan:", columnas_faltantes))
    return(tibble(id = character(), posicion = character(), nacionalidad = character(), fecha_nacimiento = as.POSIXct(character()), ciudad_nacimiento = character()))
  }
  
  # Renombrado y preparación de columnas.
  datos_xls <- datos_xls %>%
    select(-any_of("ID")) %>% 
    rename(
      ID = `FA ID Number`,
      nacionalidad = `Nationality Name`,
      fecha_nacimiento = `Date Of Birth`,
      ciudad_nacimiento = `City Of Birth Name`
    ) %>%
    mutate(ID = as.character(ID))
  
  # Filtrado para mantener solo las jugadoras presentes en las actas.
  datos_relevantes_xls <- datos_xls %>% 
    filter(ID %in% as.character(ids_jugadoras_validas))
  
  if(nrow(datos_relevantes_xls) == 0) {
    message("Ninguna de las jugadoras del archivo XLS coincide con las jugadoras de las actas.")
    return(tibble(id = character(), posicion = character(), nacionalidad = character(), fecha_nacimiento = as.POSIXct(character()), ciudad_nacimiento = character()))
  }
  
  # Definición de las columnas de posición y datos demográficos.
  columnas_posiciones <- c("GK", "DL", "DC", "DR", "DM", "WBL", "WBR", "ML", "MC", "MR", "AML", "AMC", "AMR", "SC")
  columnas_adicionales <- c("nacionalidad", "fecha_nacimiento", "ciudad_nacimiento")
  
  columnas_a_seleccionar <- intersect(
    c("ID", "Position Partially Known", columnas_posiciones, columnas_adicionales), 
    names(datos_relevantes_xls)
  )
  datos_filtrados <- datos_relevantes_xls %>% select(all_of(columnas_a_seleccionar))
  
  # Se itera sobre cada jugadora del archivo XLS para extraer su posición.
  posiciones_final <- pmap_dfr(datos_filtrados, function(...) {
    fila_actual <- list(...)
    id_jugadora <- fila_actual$ID
    
    # Extracción de datos demográficos.
    nacionalidad_jugadora <- fila_actual$nacionalidad
    fecha_nac_jugadora <- fila_actual$fecha_nacimiento
    ciudad_nac_jugadora <- fila_actual$ciudad_nacimiento
    
    # 1. Búsqueda de posición específica (valor en columna de posición == 20).
    pos_exactas_encontradas <- character(0)
    for (pos_code in intersect(columnas_posiciones, names(fila_actual))) {
      if (!is.na(fila_actual[[pos_code]]) && fila_actual[[pos_code]] == 20) {
        pos_exactas_encontradas <- c(pos_exactas_encontradas, pos_code)
      }
    }
    
    # Si se encuentra una posición específica, se utiliza y se finaliza para esta jugadora.
    if (length(pos_exactas_encontradas) > 0) {
      return(tibble(
        id = id_jugadora, 
        posicion = pos_exactas_encontradas,
        nacionalidad = nacionalidad_jugadora,
        fecha_nacimiento = fecha_nac_jugadora,
        ciudad_nacimiento = ciudad_nac_jugadora
      ))
    }
    
    # 2. Si no hay posición específica, se usa la posición general (Defensa, Centrocampista, etc.).
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
    
    # 3. Si no hay datos, se devuelve un tibble vacío para mantener la consistencia.
    return(tibble(id = character(), posicion = character(), nacionalidad = character(), fecha_nacimiento = as.POSIXct(character()), ciudad_nacimiento = character()))
  })
  
  # Limpieza final para eliminar jugadoras sin posición asignada y duplicados.
  posiciones_final <- posiciones_final %>% 
    filter(!is.na(posicion), trimws(posicion) != "") %>%
    distinct()
  
  message("Procesamiento de posiciones completado. Se encontraron ", nrow(posiciones_final), " asignaciones.")
  
  return(posiciones_final)
}


## -------------------------------------------------------------------------- ##
##  2. DEFINICIÓN DE FUNCIONES DE PARSEO DEL PDF
## -------------------------------------------------------------------------- ##

### 2.1. Parsear bloque de alineaciones y cambios ----

#' Parsea el bloque de texto de alineaciones para extraer jugadoras y cambios.
#'
#' Función compleja que identifica las columnas de equipo local y visitante, extrae
#' los datos de cada jugadora (dorsal, nombre, ID, si es portera o capitana) y
#' asocia las sustituciones con las jugadoras que entran al campo.
#'
#' @param bloque_texto El fragmento de texto del PDF que contiene las alineaciones.
#' @return Una lista con dataframes para `alineacion_local`, `cambios_local`,
#'   `alineacion_visitante` y `cambios_visitante`.
parsear_bloque_jugadoras_final <- function(bloque_texto) {
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
  lineas_local <- str_sub(lineas_raw_originales, 1, split_col - 1)
  lineas_visitante <- str_sub(lineas_raw_originales, split_col, -1)
  
  # Función interna para extraer datos de una columna de texto.
  extraer_de_columna <- function(lineas_columna, equipo_tag) {
    lineas_procesadas <- list()
    i <- 1
    while (i <= length(lineas_columna)) {
      linea_actual_texto <- lineas_columna[i]
      if (i < length(lineas_columna)) {
        linea_siguiente_texto <- lineas_columna[i+1]
        if ( (str_detect(linea_actual_texto, "\\b\\d{1,2}\\b\\s+[\\p{L}]") && !str_detect(linea_actual_texto, "\\b\\d{5,6}\\b")) &&
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
    regex_jugadora_definitiva <- "^\\s*(\\d{1,2})\\b\\s+([\\p{L}][\\p{L}\\s'-]*)\\s+.*?(\\d{5,6})\\b"
    for(item in lineas_procesadas) {
      linea_texto <- item$texto
      linea_idx_original <- item$idx_original
      player_matches <- str_locate_all(linea_texto, regex_jugadora_definitiva)[[1]]
      player_data <- str_match_all(linea_texto, regex_jugadora_definitiva)[[1]]
      if (nrow(player_data) > 0) {
        for (p in 1:nrow(player_data)) {
          nombre_raw <- player_data[p, 3]
          es_portera <- str_detect(nombre_raw, "\\s+Г\\b")
          es_capitana <- str_detect(nombre_raw, "\\s+К\\b")
          nombre_limpio <- str_remove_all(nombre_raw, "\\b[GKКГ]\\b") %>% str_squish()
          jugadoras_col[[length(jugadoras_col) + 1]] <- list(dorsal = as.integer(player_data[p, 2]), nombre = nombre_limpio, es_portera = es_portera, es_capitana = es_capitana, id = player_data[p, 4], equipo = equipo_tag, pos_start = player_matches[p, "start"], pos_end = player_matches[p, "end"], linea_idx = linea_idx_original)
        }
      }
    }
    return(jugadoras_col)
  }
  
  # Se procesan las columnas local y visitante.
  jugadoras_local <- extraer_de_columna(lineas_local, "local")
  jugadoras_visitante <- extraer_de_columna(lineas_visitante, "visitante")
  jugadoras_visitante <- map(jugadoras_visitante, function(j) { j$pos_start <- j$pos_start + split_col; j$pos_end <- j$pos_end + split_col; return(j) })
  jugadoras <- c(jugadoras_local, jugadoras_visitante)
  
  # Lógica para identificar y asociar las sustituciones a las jugadoras.
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
  
  # Función interna para construir los dataframes finales por equipo.
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


### 2.2. Extraer tarjetas (amarillas y rojas) ----

#' Extrae la información de tarjetas del texto del acta.
#'
#' Busca las secciones de "Amonestaciones" (Опомени) y "Expulsiones" (Исклучување)
#' y parsea los datos de cada tarjeta: jugadora, equipo, minuto, tipo y motivo.
#' Maneja tanto rojas directas como dobles amarillas.
#'
#' @param texto_acta Texto completo del PDF.
#' @param equipo_local_nombre Nombre del equipo local.
#' @param equipo_visitante_nombre Nombre del equipo visitante.
#' @param alineacion_local Dataframe con la alineación del equipo local.
#' @param alineacion_visitante Dataframe con la alineación del equipo visitante.
#' @return Un dataframe con todas las tarjetas del partido.
extraer_tarjetas <- function(texto_acta, equipo_local_nombre, equipo_visitante_nombre, alineacion_local, alineacion_visitante) {
  tarjetas_df_final <- data.frame()
  
  # Extracción de tarjetas amarillas.
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
  
  # Extracción de tarjetas rojas.
  bloque_rojas_raw <- str_extract(texto_acta, "Исклучување:([\\s\\S]*?)(?=Опомени:|Ж: Жолт картон|ПОТВРДЕН|ОДИГРАН|Забелешка)")
  if (!is.na(bloque_rojas_raw)) {
    texto_limpio <- str_remove(bloque_rojas_raw, "Исклучување:") %>% str_replace_all("\\n", " ") %>% str_squish()
    bloques_por_equipo <- str_split(texto_limpio, paste0("(?=", equipo_local_nombre, "|", equipo_visitante_nombre, ")"))[[1]]
    for (bloque_equipo in bloques_por_equipo) {
      if (str_trim(bloque_equipo) == "") next
      equipo_actual <- if (str_detect(bloque_equipo, paste0("^", equipo_local_nombre))) equipo_local_nombre else equipo_visitante_nombre
      
      # Caso: Doble amarilla.
      regex_doble_amarilla <- "(\\d{1,2})\\s+([\\p{L}\\s.'-]+?)\\s+\\((\\d{1,3})\\s*-\\s*(.+?)\\s+и\\s+(\\d{1,3})\\s*-\\s*(.+?)\\)"
      matches_doble <- str_match_all(bloque_equipo, regex_doble_amarilla)[[1]]
      if (nrow(matches_doble) > 0) {
        for(i in 1:nrow(matches_doble)) {
          dorsal_tarjeta <- as.integer(matches_doble[i, 2]); nombre_raw <- str_trim(matches_doble[i, 3]); minuto1 <- as.integer(matches_doble[i, 4]); motivo1 <- str_trim(matches_doble[i, 5]); minuto2 <- as.integer(matches_doble[i, 6]); motivo2 <- str_trim(matches_doble[i, 7])
          alineacion_a_buscar <- if (equipo_actual == equipo_local_nombre) alineacion_local else alineacion_visitante
          info_jugadora <- filter(alineacion_a_buscar, dorsal == dorsal_tarjeta)
          nombre_jugadora <- if(nrow(info_jugadora) > 0) info_jugadora$nombre[1] else nombre_raw
          id_jugadora <- if(nrow(info_jugadora) > 0) info_jugadora$id[1] else NA_character_
          tarjetas_df_final <- rbind(tarjetas_df_final, data.frame(jugadora=nombre_jugadora, id_jugadora=id_jugadora, equipo=equipo_actual, dorsal=dorsal_tarjeta, minuto=minuto1, tipo="Amarilla", motivo=motivo1), data.frame(jugadora=nombre_jugadora, id_jugadora=id_jugadora, equipo=equipo_actual, dorsal=dorsal_tarjeta, minuto=minuto2, tipo="Amarilla", motivo=motivo2), data.frame(jugadora=nombre_jugadora, id_jugadora=id_jugadora, equipo=equipo_actual, dorsal=dorsal_tarjeta, minuto=minuto2, tipo="Roja", motivo="Doble amarilla"))
        }
        bloque_equipo <- str_remove_all(bloque_equipo, regex_doble_amarilla)
      }
      
      # Caso: Roja directa.
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

### 2.3. Extraer tanda de penaltis ----

#' Extrae la información de la tanda de penaltis del texto del acta.
#'
#' Busca un bloque de texto específico que contiene los lanzadores de penaltis,
#' parsea cada lanzamiento para identificar al jugador, su equipo y si marcó o falló.
#'
#' @param texto_acta Texto completo del PDF.
#' @param equipo_local_nombre Nombre del equipo local.
#' @param equipo_visitante_nombre Nombre del equipo visitante.
#' @param alineacion_local Dataframe con la alineación del equipo local.
#' @param alineacion_visitante Dataframe con la alineación del equipo visitante.
#' @return Un dataframe con los lanzamientos de la tanda de penaltis.
extraer_tanda_penales <- function(texto_acta, equipo_local_nombre, equipo_visitante_nombre, alineacion_local, alineacion_visitante) {
  # Patrón para encontrar el inicio de la sección de interés.
  # Puede ser "Пен/АГ" o la línea de goleadores "Стрелец/Мин..."
  inicio_bloque_pattern <- "Пен/АГ|Стрелец/Мин[^\n]+"
  
  # Extraer todo el texto DESPUÉS de nuestro punto de inicio.
  texto_despues_inicio <- str_extract(texto_acta, paste0(inicio_bloque_pattern, "[\\s\\S]*"))
  
  if (is.na(texto_despues_inicio)) {
    return(data.frame())
  }
  
  # Ahora, desde ese texto extraído, tomamos solo hasta la siguiente sección ("Опомени:").
  # Esto evita el look-behind de longitud variable.
  bloque_penales_raw <- str_extract(texto_despues_inicio, "[\\s\\S]+?(?=Опомени:|Забелешка|ПОТВРДЕН)")
  
  # Eliminamos la propia línea de "Пен/АГ" del bloque para no procesarla por error.
  bloque_penales_raw <- str_remove(bloque_penales_raw, inicio_bloque_pattern)
  
  if (is.na(bloque_penales_raw)) {
    return(data.frame())
  }
  
  lineas_penales <- str_split(bloque_penales_raw, "\\n")[[1]]
  lineas_penales <- lineas_penales[str_detect(lineas_penales, "^\\s*[PO]\\s+\\d{1,2}")]
  
  if (length(lineas_penales) == 0) {
    return(data.frame())
  }
  
  # Se determina la columna de división (similar a las alineaciones)
  split_col <- 80 
  
  lineas_local <- str_sub(lineas_penales, 1, split_col - 1)
  lineas_visitante <- str_sub(lineas_penales, split_col, -1)
  
  penales_df <- data.frame()
  
  # Función interna para procesar una columna de lanzadores
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
  
  # Combinar los resultados de ambos equipos
  penales_df <- rbind(penales_local_df, penales_visitante_df)
  
  return(penales_df)
}

## -------------------------------------------------------------------------- ##
##  3. FUNCIÓN PRINCIPAL DE PROCESAMIENTO POR ACTA
## -------------------------------------------------------------------------- ##

#' Procesa un único archivo PDF de un acta de partido.
#'
#' Esta es la función principal que orquesta todo el proceso para un acta:
#' lee el PDF, extrae la información general del partido, llama a las funciones
#' de parseo de alineaciones, goles y tarjetas, y finalmente compila todos los
#' datos en una lista estructurada y un resumen de texto.
#'
#' @param acta_path La ruta al archivo PDF del acta.
#' @return Una lista que contiene dataframes para `partido_info`, `goles`, `tarjetas`,
#'   `alineacion_local`, etc., así como un `resumen_texto` y `nota_arbitro`.
#' Procesa un único archivo PDF de un acta de partido.
#'
#' Esta es la función principal que orquesta todo el proceso para un acta:
#' lee el PDF, extrae la información general del partido, llama a las funciones
#' de parseo de alineaciones, goles y tarjetas, y finalmente compila todos los

#' datos en una lista estructurada y un resumen de texto.
#'
#' @param acta_path La ruta al archivo PDF del acta.
#' @return Una lista que contiene dataframes para `partido_info`, `goles`, `tarjetas`,
#'   `alineacion_local`, etc., así como un `resumen_texto` y `nota_arbitro`.
procesar_acta <- function(acta_path) {
  # Lectura y preparación inicial del texto del PDF.
  nombre_archivo <- basename(acta_path)
  id_partido_match <- str_match(nombre_archivo, "match_(\\d+)_")
  id_partido <- if (!is.na(id_partido_match[1, 2])) id_partido_match[1, 2] else tools::file_path_sans_ext(nombre_archivo)
  texto_acta <- tryCatch({ paste(pdf_text(acta_path), collapse = "\n\n") }, error = function(e) { stop(paste("Error al leer PDF:", e$message)) })
  if (is.null(texto_acta) || nchar(texto_acta) == 0) stop("El PDF está vacío o no se pudo leer el texto.")
  
  # Extracción de la información principal del partido (competición, equipos, resultado).
  regex_principal <- "ЗАПИСНИК\\s*\\n\\s*([\\p{L}\\s]+?)\\s+(\\d{2}/\\d{2})\\s*\\n\\s*([^-–\\n]+(?:\\s*/\\s*[^ -–\\n]+)?)\\s*[-–]\\s*([^-–\\n]+(?:\\s*/\\s*[^ -–\\n]+)?)[\\s\\S]*?(\\d+:\\d+.*)"
  partido_info_match <- str_match(texto_acta, regex_principal)
  if (is.na(partido_info_match[1, 1])) {
    stop(paste("Información principal no encontrada en", nombre_archivo))
  }
  competicion_nombre <- str_trim(partido_info_match[, 2])
  competicion_temporada <- str_trim(partido_info_match[, 3])
  equipo_local <- str_trim(partido_info_match[, 4])
  equipo_visitante <- str_trim(partido_info_match[, 5])
  resultado_final_str <- str_trim(str_split(partido_info_match[, 6], "\\n")[[1]][1])
  
  # Detección de resultado oficial y de tanda de penaltis
  es_resultado_oficial <- str_detect(resultado_final_str, "\\*") || str_detect(tolower(texto_acta), "службен резултат")
  penales_match <- str_match(resultado_final_str, "PEN\\s*(\\d+):(\\d+)")
  penales_local <- if (!is.na(penales_match[1,1])) as.integer(penales_match[1,2]) else NA_integer_
  penales_visitante <- if (!is.na(penales_match[1,1])) as.integer(penales_match[1,3]) else NA_integer_
  
  resultado_limpio <- str_remove_all(resultado_final_str, "\\(PEN.*?\\)|\\*|\\(.*?\\)") %>% str_trim()
  goles_split <- as.integer(str_split(resultado_limpio, ":", simplify = TRUE))
  goles_local <- goles_split[1]
  goles_visitante <- goles_split[2]
  
  # Extracción de datos específicos (fecha, hora, jornada, estadio).
  fecha_hora_match <- str_match(texto_acta, "Датум/време: (\\d{2}\\.\\d{2}\\.\\d{4})\\s+(\\d{2}:\\d{2})")
  fecha <- if (!is.na(fecha_hora_match[1, 1])) fecha_hora_match[, 2] else "Desconocida"
  hora <- if (!is.na(fecha_hora_match[1, 1])) fecha_hora_match[, 3] else "Desconocida"
  jornada_match <- str_match(texto_acta, "Коло:\\s*([^\\s\\n]+)"); jornada <- if (!is.na(jornada_match[1, 2])) str_trim(jornada_match[, 2]) else NA_character_
  estadio_match <- str_match(texto_acta, "Игралиште:\\s*([^\n]+)")
  estadio <- if (!is.na(estadio_match[1, 1])) str_remove(estadio_match[1, 2], "\\s+Р\\.бр:.*$") %>% str_trim() else "Desconocido"
  
  # Función interna para extraer datos de líneas con formato "Etiqueta: Valor".
  extraer_info <- function(texto, etiqueta) {
    patron_regex <- paste0(etiqueta, "\\s*(.+?)(?:\\s{2,}|$)")
    match <- str_match(texto, patron_regex)
    if (!is.na(match[1, 2])) str_trim(match[1, 2]) else "Desconocido"
  }
  
  # Nueva función auxiliar para parsear el nombre y la ciudad del árbitro.
  parsear_arbitro <- function(texto_completo) {
    if (is.na(texto_completo) || texto_completo == "Desconocido") {
      return(list(nombre = "Desconocido", ciudad = NA_character_))
    }
    match <- str_match(texto_completo, "^\\s*(.*?)\\s*(?:\\(([^)]+)\\))?\\s*$")
    nombre <- if (!is.na(match[1, 2])) trimws(match[1, 2]) else texto_completo
    ciudad <- if (!is.na(match[1, 3])) trimws(match[1, 3]) else NA_character_
    return(list(nombre = nombre, ciudad = ciudad))
  }
  
  # Extracción de los nombres y ciudades de los árbitros.
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
  
  partido_df <- data.frame(id_partido, competicion_nombre, competicion_temporada, jornada, fecha, hora, local = equipo_local, visitante = equipo_visitante, goles_local, goles_visitante, penales_local, penales_visitante, es_resultado_oficial)
  
  pos_start_players <- str_locate(texto_acta, "Голови\\s+Ж Ц\\s+З")
  pos_end_players <- str_locate(texto_acta, "Шеф на стручен штаб|Резервни играчи|Официјални претставници")
  alineacion_local <- data.frame(); cambios_local_df <- data.frame()
  alineacion_visitante <- data.frame(); cambios_visitante_df <- data.frame()
  
  if (!is.na(pos_start_players[1,1]) && !is.na(pos_end_players[1,1])) {
    bloque_jugadoras_completo <- str_sub(texto_acta, pos_start_players[1, "start"], pos_end_players[1, "start"] -1)
    resultados_parseo <- parsear_bloque_jugadoras_final(bloque_jugadoras_completo)
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
    goleadores_minutos_nums <- as.integer(str_split(str_trim(str_match(linea_goleadores_full, "Стрелец/Мин\\s+(.*)")[, 2]), "\\s+")[[1]])
    es_autogol <- rep(FALSE, length(resultados_parciales))
    if (!is.na(linea_pen_ag_full)) {
      pos_goleadores_abs <- str_locate_all(linea_goleadores_full, "\\b\\d{1,2}\\s+\\d{1,2}\\b")[[1]]
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
      if (any(is.na(goleadores_minutos_nums)) || (i * 2) > length(goleadores_minutos_nums)) break
      marcador_actual <- as.integer(str_split(resultados_parciales[i], ":")[[1]]); if (any(is.na(marcador_actual))) next
      dorsal_gol <- goleadores_minutos_nums[(i * 2) - 1]; minuto_gol <- goleadores_minutos_nums[i * 2]; if (is.na(dorsal_gol) || is.na(minuto_gol)) next
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
    if (is.null(df) || nrow(df) == 0) return(paste("  No se encontraron", tolower(tipo_jugadora), "s."))
    subset_df <- filter(df, tipo == tipo_jugadora)
    if (nrow(subset_df) == 0) return(paste("  No se encontraron", tolower(tipo_jugadora), "s."))
    apply(subset_df, 1, function(j) {
      nombre_display <- if(as.logical(j['es_capitana'])) paste0(j['nombre'], " (C)") else j['nombre']
      paste("  -", j['dorsal'], nombre_display)
    })
  }
  formatear_goles_texto <- function(df_goles) {
    if (is.null(df_goles) || nrow(df_goles) == 0) return("No hubo goles o no se pudieron procesar.")
    apply(df_goles, 1, function(g) {
      minuto_formateado <- formatear_minuto_partido(as.numeric(g['minuto']))
      if (g['tipo'] == "Autogol") paste0("Min ", minuto_formateado, " - Gol de ", g['jugadora'], " (", g['equipo_jugadora'], ", autogol)")
      else paste0("Min ", minuto_formateado, " - Gol de ", g['jugadora'], " (", g['equipo_acreditado'], ")")
    })
  }
  formatear_tarjetas_texto <- function(df_tarjetas) {
    if(is.null(df_tarjetas) || nrow(df_tarjetas) == 0) return("  No se registraron tarjetas.")
    df_tarjetas <- df_tarjetas[order(df_tarjetas$minuto), ]
    apply(df_tarjetas, 1, function(t) paste0("  - Min ", formatear_minuto_partido(as.numeric(t['minuto'])), ": [", t['tipo'], "] para ", t['jugadora'], " (", t['equipo'], ") por '", t['motivo'], "'"))
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
    "\n======================================================================", paste("INICIO DEL ACTA:", nombre_archivo), "======================================================================",
    paste("COMPETICIÓN:", competicion_nombre, competicion_temporada),
    paste("RESUMEN DEL PARTIDO:", equipo_local, "vs", equipo_visitante, "(ID:", id_partido, ")"), paste("Fecha:", fecha, "| Hora:", hora, "| Jornada:", jornada),
    paste("Estadio:", estadio), 
    paste("Resultado final:", resultado_final_str, if(es_resultado_oficial) "(Resultado oficial)" else ""),
    "\n--- GOLES ---", formatear_goles_texto(goles_partido_actual),
    if (!is.na(penales_local)) c("\n--- TANDA DE PENALTIS ---", formatear_penales_texto(penales_partido_actual)),
    "\n--- SANCIONES (TARJETAS) ---", formatear_tarjetas_texto(tarjetas_partido_actual),
    "\n--- ÁRBITRAS Y OFICIALES ---", 
    paste("  Árbitro/a principal:", arbitro_principal_str), 
    paste("  1.ª Asistente:", arbitro_asist_1_str), 
    paste("  2.ª Asistente:", arbitro_asist_2_str),
    if (!is.na(nota_arbitro) && nchar(nota_arbitro) > 0) c("\n--- NOTA DEL ÁRBITRO ---", nota_arbitro),
    "\n--- ALINEACIONES ---", paste("\nEquipo local:", equipo_local), paste("  Entrenador/a:", entrenador_local), "  Titulares:", formatear_jugadoras(alineacion_local, "Titular"),
    "  Suplentes:", formatear_jugadoras(alineacion_local, "Suplente"), "  Cambios:", if(!is.null(cambios_local_df) && nrow(cambios_local_df) > 0) cambios_local_df$texto else "  No se registraron cambios.",
    paste("\nEquipo visitante:", equipo_visitante), paste("  Entrenador/a:", entrenador_visitante), "  Titulares:", formatear_jugadoras(alineacion_visitante, "Titular"),
    "  Suplentes:", formatear_jugadoras(alineacion_visitante, "Suplente"), "  Cambios:", if(!is.null(cambios_visitante_df) && nrow(cambios_visitante_df) > 0) cambios_visitante_df$texto else "  No se registraron cambios.",
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


## -------------------------------------------------------------------------- ##
##  4. EJECUCIÓN DEL PROCESO PRINCIPAL (CON CACHÉ)
## -------------------------------------------------------------------------- ##

### 4.1. Definición de rutas y carga de caché ----

# Ruta a la carpeta que contiene las actas en PDF.
ruta_pdfs <- "Actas"
# Ruta para el archivo de caché donde se guardarán los resultados procesados.
ruta_cache <- "actas_cache.rds"

# Se listan todos los archivos PDF actuales en la carpeta.
archivos_pdf_actuales <- list.files(path = ruta_pdfs, pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE)
if (length(archivos_pdf_actuales) == 0) {
  stop("No se encontraron archivos PDF en la ruta especificada. Revisa la ruta.")
}

# Se intenta cargar los resultados de ejecuciones anteriores desde el caché.
resultados_cacheados <- list()
if (file.exists(ruta_cache)) {
  message(paste("Cargando resultados previos desde:", ruta_cache))
  tryCatch({
    resultados_cacheados <- readRDS(ruta_cache)
    # Se asegura que el caché sea una lista. Si está corrupto, se empieza de cero.
    if (!is.list(resultados_cacheados)) resultados_cacheados <- list()
  }, error = function(e) {
    warning("El archivo de caché parece estar corrupto. Se procesarán todos los archivos de nuevo.")
    resultados_cacheados <- list()
  })
}

### 4.2. Detección de cambios en archivos ----

# Se obtienen los nombres base de los archivos para usarlos como identificadores únicos.
nombres_archivos_actuales <- basename(archivos_pdf_actuales)
nombres_archivos_cacheados <- names(resultados_cacheados)

# Se identifican los archivos nuevos (en disco pero no en caché) y los eliminados (en caché pero no en disco).
archivos_a_procesar_nombres <- setdiff(nombres_archivos_actuales, nombres_archivos_cacheados)
archivos_eliminados_nombres <- setdiff(nombres_archivos_cacheados, nombres_archivos_actuales)

# Se informa al usuario si se han eliminado o modificado actas.
if (length(archivos_eliminados_nombres) > 0) {
  message(paste("Se han detectado", length(archivos_eliminados_nombres), "actas eliminadas o modificadas. Actualizando..."))
  # Se eliminan del conjunto de resultados cacheados.
  resultados_cacheados <- resultados_cacheados[!names(resultados_cacheados) %in% archivos_eliminados_nombres]
}

### 4.3. Procesamiento incremental de actas nuevas ----

# Se prepara el procesador seguro que envuelve la función principal de parseo.
procesador_seguro <- purrr::safely(procesar_acta)
nuevos_resultados_completos <- list()

if (length(archivos_a_procesar_nombres) > 0) {
  message(paste("Iniciando procesamiento de", length(archivos_a_procesar_nombres), "acta(s) nueva(s)..."))
  
  # Se obtienen las rutas completas de los archivos que necesitan ser procesados.
  rutas_a_procesar <- archivos_pdf_actuales[basename(archivos_pdf_actuales) %in% archivos_a_procesar_nombres]
  names(rutas_a_procesar) <- basename(rutas_a_procesar)
  
  # Se aplica la función de procesamiento únicamente a los archivos nuevos.
  nuevos_resultados_completos <- purrr::map(rutas_a_procesar, ~{
    message(paste("Procesando:", basename(.x)))
    procesador_seguro(.x)
  })
  
} else {
  message("No hay actas nuevas que procesar. Todos los resultados se han cargado desde el caché.")
}

### 4.4. Gestión de errores en nuevos procesamientos ----

# Se separan los resultados exitosos de los errores, solo para los archivos recién procesados.
nuevos_resultados_exitosos <- purrr::map(nuevos_resultados_completos, "result") %>% purrr::compact()
errores_nuevos <- purrr::map(nuevos_resultados_completos, "error") %>% purrr::compact()

# Si hubo errores en los nuevos archivos, se informa al usuario de forma detallada.
if (length(errores_nuevos) > 0) {
  message("\n--- AVISO: Se encontraron errores en ", length(errores_nuevos), " de los ", length(archivos_a_procesar_nombres), " archivos nuevos. ---")
  purrr::walk2(names(errores_nuevos), errores_nuevos, ~message(paste0("\nERROR en archivo: ", .x, "\nMENSAJE: ", .y$message)))
} else if (length(archivos_a_procesar_nombres) > 0) {
  message("\n¡Todos los archivos nuevos se procesaron sin errores!")
}

### 4.5. Consolidación y actualización del caché ----

# Identificar los resultados que fueron eliminados para obtener sus datos antes de borrarlos.
resultados_eliminados <- list()
if (length(archivos_eliminados_nombres) > 0) {
  # Carga el caché antiguo para encontrar los datos de los archivos eliminados.
  if (file.exists(ruta_cache)) {
    cache_antiguo <- readRDS(ruta_cache)
    if (is.list(cache_antiguo)) {
      resultados_eliminados <- cache_antiguo[names(cache_antiguo) %in% archivos_eliminados_nombres]
    }
  }
}

# Se combinan los resultados cargados del caché con los resultados de los archivos recién procesados.
resultados_exitosos <- c(resultados_cacheados, nuevos_resultados_exitosos)

# Se guarda la lista combinada y actualizada en el archivo de caché para futuras ejecuciones.
tryCatch({
  saveRDS(resultados_exitosos, file = ruta_cache)
  message(paste("Caché actualizado con", length(resultados_exitosos), "actas procesadas. Guardado en:", ruta_cache))
}, error = function(e) {
  warning("No se pudo guardar el archivo de caché. Los resultados no estarán disponibles en la próxima ejecución.")
  message(paste("Error original:", e$message))
})


### 4.6. Guardar estado de cambios de archivos ----

# Se determina si hubo algún cambio (archivos nuevos o eliminados).
hubo_cambios <- length(archivos_a_procesar_nombres) > 0 || length(archivos_eliminados_nombres) > 0

# Se crea una lista simple que solo contiene los nombres de los archivos que han cambiado.
# El Script 2 usará esta información para determinar qué entidades se ven afectadas.
info_cambios = list(
  hubo_cambios = hubo_cambios,
  archivos_nuevos_nombres = archivos_a_procesar_nombres,
  archivos_eliminados_nombres = archivos_eliminados_nombres
)

# Guardar esta información para que la use el Script 2.
ruta_cache_info <- "cache_info.rds"
saveRDS(info_cambios, file = ruta_cache_info)
message(paste("Información de cambios de archivos guardada en:", ruta_cache_info))


## -------------------------------------------------------------------------- ##
##  5. AGREGACIÓN Y GENERACIÓN DE REPORTES
## -------------------------------------------------------------------------- ##

### 5.1. Consolidación de datos ----

# Se combinan los resultados de todas las actas en dataframes únicos.
partidos_df <- purrr::map_dfr(resultados_exitosos, "partido_info")
goles_df <- purrr::map_dfr(resultados_exitosos, "goles")
tarjetas_df <- purrr::map_dfr(resultados_exitosos, "tarjetas")


### 5.2. Enriquecimiento con datos de jugadoras (desde archivo XLS) ----

# 1. Se unen las alineaciones en un único dataframe.
alineaciones_completas <- map_dfr(resultados_exitosos, ~bind_rows(
  .x$alineacion_local,
  .x$alineacion_visitante
))

# 2. Se verifica la existencia de la columna 'id' antes de proceder.
if ("id" %in% names(alineaciones_completas)) {
  ids_validas_de_actas <- alineaciones_completas %>%
    dplyr::pull("id") %>%
    unique() %>%
    na.omit()
} else {
  warning("La columna 'id' no se encontró en los datos de alineación combinados. No se podrán enriquecer los datos de las jugadoras.")
  ids_validas_de_actas <- character(0)
}

message(paste("\nSe encontraron un total de", length(ids_validas_de_actas), "jugadoras únicas en todas las actas."))

# Se cargan y procesan los datos adicionales (posición, nacionalidad, etc.).
ruta_xls_posiciones <- "igraci.xlsx"
posiciones_df <- cargar_y_procesar_posiciones(ruta_xls_posiciones, ids_validas_de_actas)


### 5.3. Cálculo de tablas derivadas ----

# Función para generar la tabla de clasificación.
calcular_clasificacion <- function(partidos) {
  if (is.null(partidos) || nrow(partidos) == 0) return(data.frame(Mensaje = "No se procesaron partidos válidos."))
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

# Creación de la tabla de goleadoras.
if (!is.null(goles_df) && nrow(goles_df) > 0) {
  # Se consideran solo goles 'Normales' y se usa 'equipo_acreditado' para la tabla.
  tabla_goleadoras <- goles_df %>% 
    filter(tipo == "Normal") %>% 
    group_by(Jugadora = jugadora, Equipo = equipo_acreditado) %>% 
    summarise(Goles = n(), .groups = 'drop') %>% 
    arrange(desc(Goles))
} else {
  tabla_goleadoras <- data.frame(Mensaje = "No se procesaron goles válidos.")
}

# Creación de la tabla de sanciones.
if (!is.null(tarjetas_df) && nrow(tarjetas_df) > 0) {
  tabla_sanciones <- tarjetas_df %>%
    group_by(Jugadora = jugadora, Equipo = equipo) %>%
    summarise(Amarillas = sum(tipo == "Amarilla"), Rojas = sum(tipo == "Roja"), .groups = 'drop') %>%
    filter(Amarillas > 0 | Rojas > 0) %>%
    arrange(desc(Rojas), desc(Amarillas))
} else {
  tabla_sanciones <- data.frame(Mensaje = "No se procesaron sanciones válidas.")
}


### 5.4. Escritura del archivo de salida ----

# Generación del archivo de texto final con todos los resúmenes y tablas.
ruta_salida_txt <- file.path(Sys.getenv("HOME"), "Downloads", "resumen_liga_futbol_R_final.txt")
tryCatch({
  # Se recopilan todos los resúmenes de texto de cada partido.
  resumenes_completos <- unlist(purrr::map(resultados_exitosos, "resumen_texto"))
  
  # Se formatean las tablas finales usando 'knitr' para una mejor visualización.
  goleadoras_formateadas <- c("\n\n========================= TABLA DE GOLEADORAS =========================", capture.output(print(knitr::kable(tabla_goleadoras, format = "pipe", row.names = FALSE))), "")
  sanciones_formateadas <- c("\n\n========================== TABLA DE SANCIONES ==========================", capture.output(print(knitr::kable(tabla_sanciones, format = "pipe", row.names = FALSE))), "")
  clasificacion_formateada <- c("\n\n============================= CLASIFICACIÓN =============================", capture.output(print(knitr::kable(clasificacion_df, format = "pipe", row.names = FALSE))), "")
  
  # Escritura del archivo en UTF-8 para asegurar la correcta visualización de caracteres.
  con <- file(ruta_salida_txt, open = "wt", encoding = "UTF-8")
  writeLines(resumenes_completos, con)
  writeLines(goleadoras_formateadas, con)
  writeLines(sanciones_formateadas, con)
  writeLines(clasificacion_formateada, con)
  close(con)
  
  message(paste("\n¡PROCESO COMPLETADO CON ÉXITO!"))
  message(paste("Revisa el archivo de salida en:", ruta_salida_txt))
  
}, error = function(e) {
  message("\nError al escribir el archivo de salida.")
  message(paste("Error original:", e$message))
})