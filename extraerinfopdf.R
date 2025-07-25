# =========================================================================
# SCRIPT DE ANÁLISIS DE ACTAS DE FÚTBOL (VERSIÓN CON EXTRACCIÓN DINÁMICA DE COMPETICIÓN)
# =========================================================================

# -------------------------------------------------------------------------
# PASO 0: INSTALAR Y CARGAR PAQUETES
# -------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  pdftools, stringr, dplyr, tidyr, purrr, knitr, readxl
)

# =========================================================================
# FUNCIÓN AUXILIAR PARA FORMATEAR MINUTOS DE TIEMPO AÑADIDO
# =========================================================================
formatear_minuto_partido <- function(minutos) {
  # Esta función se aplica a un vector de minutos
  sapply(minutos, function(minuto) {
    # Si el minuto es NA o no es un número, lo devolvemos tal cual
    if (is.na(minuto) || !is.numeric(minuto)) {
      return(as.character(minuto))
    }
    
    # La regla: si es mayor de 140, es un minuto de tiempo añadido
    # y nos aseguramos de que tenga al menos 3 dígitos (ej. 451, 901)
    if (minuto > 140 && nchar(as.character(minuto)) >= 3) {
      minuto_str <- as.character(minuto)
      base <- substr(minuto_str, 1, 2)
      added <- substr(minuto_str, 3, nchar(minuto_str))
      # Devolvemos el formato "base+añadido"
      return(paste0(base, "+", added))
    } else {
      # Si no cumple la regla, devolvemos el minuto normal
      return(as.character(minuto))
    }
  })
}

# =========================================================================
# FUNCIÓN PARA CARGAR Y PROCESAR POSICIONES DESDE XLS 
# =========================================================================

cargar_y_procesar_posiciones <- function(ruta_xls, ids_jugadoras_validas) {
  
  if (!file.exists(ruta_xls)) {
    warning(paste("Archivo de posiciones no encontrado en:", ruta_xls, "- Se continuará sin datos de posición."))
    return(tibble(id = character(), posicion = character()))
  }
  
  message("Cargando y procesando el archivo de posiciones: ", basename(ruta_xls))
  
  datos_xls <- tryCatch({ read_excel(ruta_xls) }, error = function(e) {
    warning("Error al leer el archivo .xls. Se continuará sin datos de posición.")
    return(NULL)
  })
  
  if (is.null(datos_xls) || !"FA ID Number" %in% names(datos_xls)) {
    warning("El archivo XLS no contiene la columna 'FA ID Number'. No se pueden procesar las posiciones.")
    return(tibble(id = character(), posicion = character()))
  }
  
  datos_xls <- datos_xls %>%
    select(-any_of("ID")) %>% 
    rename(ID = `FA ID Number`)
  
  ids_jugadoras_validas <- as.character(ids_jugadoras_validas)
  datos_xls <- datos_xls %>% mutate(ID = as.character(ID))
  
  datos_relevantes_xls <- datos_xls %>% 
    filter(ID %in% ids_jugadoras_validas)
  
  if(nrow(datos_relevantes_xls) == 0) {
    message("Ninguna de las jugadoras del archivo XLS coincide con las jugadoras procesadas de las actas.")
    return(tibble(id = character(), posicion = character()))
  }
  
  # ============================ INICIO DE LA CORRECCIÓN LÓGICA ============================
  
  # Definimos todas las columnas de posiciones posibles
  columnas_posiciones <- c("GK", "DL", "DC", "DR", "DM", "WBL", "WBR", "ML", "MC", "MR", "AML", "AMC", "AMR", "SC")
  
  # Seleccionamos solo las columnas que nos interesan para el procesamiento
  columnas_existentes <- intersect(c("ID", "Position Partially Known", columnas_posiciones), names(datos_relevantes_xls))
  datos_filtrados <- datos_relevantes_xls %>% select(all_of(columnas_existentes))
  
  # Usamos pmap para iterar por cada fila de manera eficiente
  posiciones_final <- pmap_dfr(datos_filtrados, function(...) {
    fila_actual <- list(...)
    id_jugadora <- fila_actual$ID
    
    # 1. BUSCAR POSICIONES EXACTAS (REGLA DEL 20)
    pos_exactas_encontradas <- character(0)
    
    # Iteramos solo sobre las columnas de posición que existen en el archivo
    for (pos_code in intersect(columnas_posiciones, names(fila_actual))) {
      # Comprobamos que el valor es 20 y no es NA
      if (!is.na(fila_actual[[pos_code]]) && fila_actual[[pos_code]] == 20) {
        # Añadimos el código de la posición (ej. "DC", "MC") directamente
        pos_exactas_encontradas <- c(pos_exactas_encontradas, pos_code)
      }
    }
    
    # Si encontramos una o más posiciones con valor 20, las usamos y terminamos.
    if (length(pos_exactas_encontradas) > 0) {
      return(tibble(id = id_jugadora, posicion = pos_exactas_encontradas))
    }
    
    # 2. SI NO HAY POSICIÓN EXACTA, USAR LA POSICIÓN PARCIALMENTE CONOCIDA
    pos_parcial <- fila_actual[["Position Partially Known"]]
    if (!is.null(pos_parcial) && !is.na(pos_parcial)) {
      # Aquí sí usamos la posición agrupada como fallback
      posicion_agrupada <- case_when(
        pos_parcial == 1 ~ "Defensa",
        pos_parcial == 2 ~ "Centrocampista",
        pos_parcial == 3 ~ "Delantera",
        TRUE ~ NA_character_
      )
      if (!is.na(posicion_agrupada)) {
        return(tibble(id = id_jugadora, posicion = posicion_agrupada))
      }
    }
    
    # 3. SI NO HAY NINGUNA INFORMACIÓN, NO DEVOLVEMOS NADA PARA ESTA JUGADORA
    # Devolver un tibble vacío es más seguro que devolver NA
    return(tibble(id = character(), posicion = character()))
  })
  
  # ============================ FIN DE LA CORRECCIÓN LÓGICA ============================
  
  # Limpiar filas donde la posición esté vacía (aunque ya no debería haber NAs) y asegurar unicidad
  posiciones_final <- posiciones_final %>% 
    filter(!is.na(posicion), trimws(posicion) != "") %>%
    distinct()
  
  message("Procesamiento de posiciones completado. Se encontraron ", nrow(posiciones_final), " asignaciones de posición para las jugadoras de las actas.")
  
  return(posiciones_final)
}



# -------------------------------------------------------------------------
# PASO 1: DEFINIR RUTA Y OBTENER LISTA DE ARCHIVOS PDF
# -------------------------------------------------------------------------
ruta_pdfs <- "Actas"
archivos_pdf <- list.files(path = ruta_pdfs, pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE)

if (length(archivos_pdf) == 0) {
  stop("No se encontraron archivos PDF en la ruta especificada. Revisa la ruta.")
}


# -------------------------------------------------------------------------
# PASO 2: FUNCIÓN DE PARSEO DE JUGADORAS (SIN CAMBIOS EN ESTA SECCIÓN)
# -------------------------------------------------------------------------
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
  jugadoras_local <- extraer_de_columna(lineas_local, "local")
  jugadoras_visitante <- extraer_de_columna(lineas_visitante, "visitante")
  jugadoras_visitante <- map(jugadoras_visitante, function(j) { j$pos_start <- j$pos_start + split_col; j$pos_end <- j$pos_end + split_col; return(j) })
  jugadoras <- c(jugadoras_local, jugadoras_visitante)
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

# -------------------------------------------------------------------------
# PASO 2.5: FUNCIÓN AUXILIAR PARA EXTRAER TARJETAS (SIN CAMBIOS)
# -------------------------------------------------------------------------
extraer_tarjetas <- function(texto_acta, equipo_local_nombre, equipo_visitante_nombre, alineacion_local, alineacion_visitante) {
  tarjetas_df_final <- data.frame()
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
  bloque_rojas_raw <- str_extract(texto_acta, "Исклучување:([\\s\\S]*?)(?=Опомени:|Ж: Жолт картон|ПОТВРДЕН|ОДИГРАН|Забелешка)")
  if (!is.na(bloque_rojas_raw)) {
    texto_limpio <- str_remove(bloque_rojas_raw, "Исклучување:") %>% str_replace_all("\\n", " ") %>% str_squish()
    bloques_por_equipo <- str_split(texto_limpio, paste0("(?=", equipo_local_nombre, "|", equipo_visitante_nombre, ")"))[[1]]
    for (bloque_equipo in bloques_por_equipo) {
      if (str_trim(bloque_equipo) == "") next
      equipo_actual <- if (str_detect(bloque_equipo, paste0("^", equipo_local_nombre))) equipo_local_nombre else equipo_visitante_nombre
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

# =========================================================================
# PASO 3: FUNCIÓN PRINCIPAL DE PROCESAMIENTO
# =========================================================================
procesar_acta <- function(acta_path) {
  nombre_archivo <- basename(acta_path)
  id_partido_match <- str_match(nombre_archivo, "match_(\\d+)_")
  id_partido <- if (!is.na(id_partido_match[1, 2])) id_partido_match[1, 2] else tools::file_path_sans_ext(nombre_archivo)
  texto_acta <- tryCatch({ paste(pdf_text(acta_path), collapse = "\n\n") }, error = function(e) { stop(paste("Error al leer PDF:", e$message)) })
  if (is.null(texto_acta) || nchar(texto_acta) == 0) stop("El PDF está vacío o no se pudo leer el texto.")
  
  # --- INICIO DE LA MODIFICACIÓN: Captura dinámica de nombre y temporada de la competición ---
  # Regex mejorado para capturar el nombre de la competición y la temporada por separado
  regex_principal <- "ЗАПИСНИК\\s*\\n\\s*([\\p{L}\\s]+?)\\s+(\\d{2}/\\d{2})\\s*\\n\\s*([^-–\\n]+(?:\\s*/\\s*[^ -–\\n]+)?)\\s*[-–]\\s*([^-–\\n]+(?:\\s*/\\s*[^ -–\\n]+)?)[\\s\\S]*?(\\d+:\\d+)"
  partido_info_match <- str_match(texto_acta, regex_principal)
  
  if (is.na(partido_info_match[1, 1])) {
    stop(paste("Info principal (incluyendo competición y temporada) no encontrada en", nombre_archivo))
  }
  
  competicion_nombre <- str_trim(partido_info_match[, 2])
  competicion_temporada <- str_trim(partido_info_match[, 3])
  equipo_local <- str_trim(partido_info_match[, 4])
  equipo_visitante <- str_trim(partido_info_match[, 5])
  resultado_final_str <- partido_info_match[, 6]
  goles_split <- as.integer(str_split(resultado_final_str, ":", simplify = TRUE))
  goles_local <- goles_split[1]
  goles_visitante <- goles_split[2]
  # --- FIN DE LA MODIFICACIÓN ---
  
  fecha_hora_match <- str_match(texto_acta, "Датум/време: (\\d{2}\\.\\d{2}\\.\\d{4})\\s+(\\d{2}:\\d{2})")
  fecha <- if (!is.na(fecha_hora_match[1, 1])) fecha_hora_match[, 2] else "Desconocida"
  hora <- if (!is.na(fecha_hora_match[1, 1])) fecha_hora_match[, 3] else "Desconocida"
  jornada_match <- str_match(texto_acta, "Коло:\\s*([^\\s\\n]+)"); jornada <- if (!is.na(jornada_match[1, 2])) str_trim(jornada_match[, 2]) else NA_character_
  estadio_match <- str_match(texto_acta, "Игралиште:\\s*([^\n]+)")
  estadio <- if (!is.na(estadio_match[1, 1])) str_remove(estadio_match[1, 2], "\\s+Р\\.бр:.*$") %>% str_trim() else "Desconocido"
  
  extraer_info <- function(texto, etiqueta) {
    patron_regex <- paste0(etiqueta, "\\s*(.+?)(?:\\s{2,}|$)")
    match <- str_match(texto, patron_regex)
    if (!is.na(match[1, 2])) str_trim(match[1, 2]) else "Desconocido"
  }
  
  arbitro_principal <- extraer_info(texto_acta, "Гл\\.судија:")
  arbitro_asist_1 <- extraer_info(texto_acta, "1\\.\\s*помошен:")
  arbitro_asist_2 <- extraer_info(texto_acta, "2\\.\\s*помошен:")
  
  # --- INICIO DE LA MODIFICACIÓN: Añadir columnas de competición al dataframe ---
  partido_df <- data.frame(
    id_partido = id_partido,
    competicion_nombre = competicion_nombre,
    competicion_temporada = competicion_temporada,
    jornada, fecha, hora,
    local = equipo_local, visitante = equipo_visitante,
    goles_local, goles_visitante
  )
  # --- FIN DE LA MODIFICACIÓN ---
  
  entrenador_local <- "Desconocido"; entrenador_visitante <- "Desconocido"
  linea_entrenadores_match <- str_extract(texto_acta, ".*Шеф на стручен штаб.*")
  if (!is.na(linea_entrenadores_match)) {
    partes_entrenador <- str_split(linea_entrenadores_match, "Шеф на стручен штаб")[[1]]
    if (length(partes_entrenador) >= 1 && nchar(str_trim(partes_entrenador[1])) > 0) entrenador_local <- str_trim(partes_entrenador[1])
    if (length(partes_entrenador) >= 2 && nchar(str_trim(partes_entrenador[2])) > 0) entrenador_visitante <- str_trim(partes_entrenador[2])
    entrenador_local <- str_remove_all(entrenador_local, "\\d") %>% str_trim()
    entrenador_visitante <- str_remove_all(entrenador_visitante, "\\d") %>% str_trim()
  }
  
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
      marcador_actual <- as.integer(str_split(resultados_parciales[i], ":")[[1]])
      if (any(is.na(marcador_actual))) next
      dorsal_gol <- goleadores_minutos_nums[(i * 2) - 1]; minuto_gol <- goleadores_minutos_nums[i * 2]
      if (is.na(dorsal_gol) || is.na(minuto_gol)) next
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
  
  formatear_jugadoras <- function(df, tipo_jugadora) {
    if (is.null(df) || nrow(df) == 0) return(paste("  No se encontraron", tolower(tipo_jugadora), "s."))
    subset_df <- filter(df, tipo == tipo_jugadora)
    if (nrow(subset_df) == 0) return(paste("  No se encontraron", tolower(tipo_jugadora), "s."))
    apply(subset_df, 1, function(j) {
      nombre_display <- if(as.logical(j['es_capitana'])) paste0(j['nombre'], " (C)") else j['nombre']
      paste("  -", j['dorsal'], nombre_display)
    })
  }
  
  if (!is.null(cambios_local_df) && nrow(cambios_local_df) > 0) cambios_local_df <- cambios_local_df[order(cambios_local_df$minuto), ]
  if (!is.null(cambios_visitante_df) && nrow(cambios_visitante_df) > 0) cambios_visitante_df <- cambios_visitante_df[order(cambios_visitante_df$minuto), ]
  
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
  
  resumen_actual_lines <- c(
    "\n======================================================================", paste("INICIO DEL ACTA:", nombre_archivo), "======================================================================",
    paste("COMPETICIÓN:", competicion_nombre, competicion_temporada),
    paste("RESUMEN DEL PARTIDO:", equipo_local, "vs", equipo_visitante, "(ID:", id_partido, ")"), paste("Fecha:", fecha, "| Hora:", hora, "| Jornada:", jornada),
    paste("Estadio:", estadio), paste("Resultado Final:", goles_local, "-", goles_visitante), "\n--- GOLES ---", formatear_goles_texto(goles_partido_actual),
    "\n--- SANCIONES (TARJETAS) ---", formatear_tarjetas_texto(tarjetas_partido_actual),
    "\n--- ÁRBITROS Y OFICIALES ---", paste("  Árbitro Principal:", arbitro_principal), paste("  1.er Asistente:", arbitro_asist_1), paste("  2.º Asistente:", arbitro_asist_2),
    "\n--- ALINEACIONES ---", paste("\nEquipo Local:", equipo_local), paste("  Entrenador:", entrenador_local), "  Titulares:", formatear_jugadoras(alineacion_local, "Titular"),
    "  Suplentes:", formatear_jugadoras(alineacion_local, "Suplente"), "  Cambios:", if(!is.null(cambios_local_df) && nrow(cambios_local_df) > 0) cambios_local_df$texto else "  No se registraron cambios.",
    paste("\nEquipo Visitante:", equipo_visitante), paste("  Entrenador:", entrenador_visitante), "  Titulares:", formatear_jugadoras(alineacion_visitante, "Titular"),
    "  Suplentes:", formatear_jugadoras(alineacion_visitante, "Suplente"), "  Cambios:", if(!is.null(cambios_visitante_df) && nrow(cambios_visitante_df) > 0) cambios_visitante_df$texto else "  No se registraron cambios.",
    "\n======================================================================\n"
  )
  
  goles_normales_df <- if(nrow(goles_partido_actual) > 0) filter(goles_partido_actual, tipo == "Normal") else data.frame()
  if(nrow(goles_normales_df) > 0) names(goles_normales_df)[names(goles_normales_df) == 'equipo_acreditado'] <- 'equipo'
  
  return(list(partido_info = partido_df, goles = goles_normales_df, tarjetas = tarjetas_partido_actual, resumen_texto = unlist(resumen_actual_lines), alineacion_local = alineacion_local, alineacion_visitante = alineacion_visitante, cambios_local = cambios_local_df, cambios_visitante = cambios_visitante_df, arbitro_principal = arbitro_principal, arbitro_asist_1 = arbitro_asist_1, arbitro_asist_2 = arbitro_asist_2, estadio = estadio))
}

# -------------------------------------------------------------------------
# PASO 4: BUCLE DE PROCESAMIENTO
# -------------------------------------------------------------------------
message("Iniciando procesamiento de ", length(archivos_pdf), " actas...")
procesador_seguro <- purrr::safely(procesar_acta)
names(archivos_pdf) <- basename(archivos_pdf)
lista_resultados <- purrr::map(archivos_pdf, ~{ message(paste("Procesando:", basename(.x))); procesador_seguro(.x) })
resultados_exitosos <- purrr::map(lista_resultados, "result") %>% purrr::compact()
errores <- purrr::map(lista_resultados, "error") %>% purrr::compact()
if (length(errores) > 0) {
  message("\n--- AVISO: Se encontraron errores en ", length(errores), " de ", length(archivos_pdf), " archivos. ---")
  purrr::walk2(names(errores), errores, ~message(paste0("\nERROR en archivo: ", .x, "\nMENSAJE: ", .y$message)))
} else { message("\n¡Todos los archivos se procesaron sin errores!") }

# -------------------------------------------------------------------------
# PASO 5: AGREGACIÓN DE RESULTADOS Y ESCRITURA DE ARCHIVO
# -------------------------------------------------------------------------
partidos_df <- purrr::map_dfr(resultados_exitosos, "partido_info")
goles_df <- purrr::map_dfr(resultados_exitosos, "goles")
tarjetas_df <- purrr::map_dfr(resultados_exitosos, "tarjetas")

# --- NUEVO: OBTENER LISTA DE JUGADORAS Y PROCESAR POSICIONES ---

# 1. Obtener una lista única de todas las IDs de jugadoras que aparecen en las actas
#    La fuente más completa es la lista de resultados exitosos.
ids_validas_de_actas <- map_dfr(resultados_exitosos, ~bind_rows(
  .x$alineacion_local,
  .x$alineacion_visitante
)) %>%
  pull(id) %>%
  unique() %>%
  na.omit()

message(paste("\nSe encontraron un total de", length(ids_validas_de_actas), "jugadoras únicas en todas las actas."))

# 2. Llamar a la función para procesar posiciones, pasándole los IDs válidos
ruta_xls_posiciones <- "/Users/marcosmarinm/Documents/жфМ/zfudbalmk/20238-mkd-players-since-2023.xls"
posiciones_df <- cargar_y_procesar_posiciones(ruta_xls_posiciones, ids_validas_de_actas)


calcular_clasificacion <- function(partidos) {
  if (is.null(partidos) || nrow(partidos) == 0) return(data.frame(Mensaje = "No se procesaron partidos válidos."))
  locales <- partidos %>% select(equipo = local, GF = goles_local, GC = goles_visitante)
  visitantes <- partidos %>% select(equipo = visitante, GF = goles_visitante, GC = goles_local)
  resultados_por_equipo <- bind_rows(locales, visitantes) %>% mutate(Pts = case_when(GF > GC ~ 3, GF < GC ~ 0, TRUE ~ 1), resultado = case_when(GF > GC ~ "PG", GF < GC ~ "PP", TRUE ~ "PE"))
  clasificacion <- resultados_por_equipo %>% group_by(Equipo = equipo) %>% summarise(PJ = n(), Pts = sum(Pts), PG = sum(resultado == "PG"), PE = sum(resultado == "PE"), PP = sum(resultado == "PP"), GF = sum(GF), GC = sum(GC), .groups = 'drop') %>% mutate(DG = GF - GC) %>% arrange(desc(Pts), desc(DG), desc(GF))
  return(clasificacion)
}
clasificacion_df <- calcular_clasificacion(partidos_df)

if (!is.null(goles_df) && nrow(goles_df) > 0) {
  tabla_goleadoras <- goles_df %>% group_by(Jugadora = jugadora, Equipo = equipo) %>% summarise(Goles = n(), .groups = 'drop') %>% arrange(desc(Goles))
} else { tabla_goleadoras <- data.frame(Mensaje = "No se procesaron goles válidos.") }

if (!is.null(tarjetas_df) && nrow(tarjetas_df) > 0) {
  tabla_sanciones <- tarjetas_df %>% group_by(Jugadora = jugadora, Equipo = equipo) %>% summarise(Amarillas = sum(tipo == "Amarilla"), Rojas = sum(tipo == "Roja"), .groups = 'drop') %>% filter(Amarillas > 0 | Rojas > 0) %>% arrange(desc(Rojas), desc(Amarillas))
} else { tabla_sanciones <- data.frame(Mensaje = "No se procesaron sanciones válidas.") }

ruta_salida_txt <- file.path(Sys.getenv("HOME"), "Downloads", "resumen_liga_futbol_R_final.txt")
tryCatch({
  resumenes_completos <- unlist(purrr::map(resultados_exitosos, "resumen_texto"))
  goleadoras_formateadas <- c("\n\n========================= TABLA DE GOLEADORAS =========================", capture.output(print(knitr::kable(tabla_goleadoras, format = "pipe", row.names = FALSE))), "")
  sanciones_formateadas <- c("\n\n========================== TABLA DE SANCIONES ==========================", capture.output(print(knitr::kable(tabla_sanciones, format = "pipe", row.names = FALSE))), "")
  clasificacion_formateada <- c("\n\n============================= CLASIFICACIÓN =============================", capture.output(print(knitr::kable(clasificacion_df, format = "pipe", row.names = FALSE))), "")
  
  con <- file(ruta_salida_txt, open = "wt", encoding = "UTF-8")
  writeLines(resumenes_completos, con)
  writeLines(goleadoras_formateadas, con)
  writeLines(sanciones_formateadas, con)
  writeLines(clasificacion_formateada, con)
  close(con)
  
  message(paste("\n¡PROCESO COMPLETADO CON ÉXITO!"))
  message(paste("Revisa el archivo de salida en:", ruta_salida_txt))
}, error = function(e) { message("\nError al escribir el archivo de salida."); message(paste("Error original:", e$message)) })