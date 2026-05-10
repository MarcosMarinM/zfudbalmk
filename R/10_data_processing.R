#### 10. MAIN DATA PROCESSING AND TRANSFORMATION (VERSI\u00d3N REORDENADA Y CORREGIDA) ####

### 10.0. Load, Unify, and Cleanse All Raw Data
message("Step 10.0: Loading, unifying, and cleansing all raw data sources...")

# 10.0.1. Load from caches
ruta_cache_pdf <- "actas_cache.rds"
pdf_results <- if (file.exists(ruta_cache_pdf)) readRDS(ruta_cache_pdf) else list()
ruta_cache_web <- "web_cache.rds"
web_results_raw <- if (file.exists(ruta_cache_web)) readRDS(ruta_cache_web) else list()
# Filter out sentinel entries for ignored/unplayed matches
ignorados <- keep(web_results_raw, ~ isTRUE(.x$ignorado))
if (length(ignorados) > 0) {
  message(paste("   > Skipping", length(ignorados), "ignored/unplayed web matches."))
}
web_results <- discard(web_results_raw, ~ isTRUE(.x$ignorado))
resultados_exitosos <- c(pdf_results, web_results)
message(paste("   > Total unified reports to process:", length(resultados_exitosos)))

# 10.0.2. Standardize all team and stadium names to Cyrillic
message("   > Standardizing all names to Cyrillic...")
is_latin <- function(text) {
  if (is.na(text) || nchar(text) == 0) {
    return(FALSE)
  }
  # Detect presence of any classic Latin character or common diacritics (c, s, z with caron, etc.)
  grepl("[A-Za-z\u00C0-\u017F]", text)
}

latin_to_cyrillic <- function(nombres) {
  mapa_digrafos <- c("zh" = "\u0436", "sh" = "\u0448", "ch" = "\u0447", "nj" = "\u045a", "lj" = "\u0459", "kj" = "\u045c", "gj" = "\u0453", "dz" = "\u0455", "dj" = "\u045f")
  mapa_simple <- c(
    "a" = "\u0430", "b" = "\u0431", "c" = "\u0446", "d" = "\u0434", "e" = "\u0435", "f" = "\u0444", "g" = "\u0433", "h" = "\u0445", "i" = "\u0438", "j" = "\u0458", "k" = "\u043a", "l" = "\u043b", "m" = "\u043c", "n" = "\u043d", "o" = "\u043e", "p" = "\u043f", "r" = "\u0440", "s" = "\u0441", "t" = "\u0442", "u" = "\u0443", "v" = "\u0432", "z" = "\u0437", "w" = "\u0432", "q" = "\u043a", "x" = "\u043a\u0441", "y" = "\u0458",
    "\u010d" = "\u0447", # č -> ч
    "\u0161" = "\u0448", # š -> ш
    "\u017e" = "\u0436", # ž -> ж
    "\u0111" = "\u0453", # đ -> ѓ
    "\u0107" = "\u045c", # ć -> ќ
    "\u00e7" = "\u0447"  # ç -> ч
  )
  sapply(nombres, function(nombre) {
    if (is.na(nombre) || nchar(trimws(nombre)) == 0) {
      return(nombre)
    }
    nombre_lower <- tolower(nombre)
    nombre_convertido <- str_replace_all(nombre_lower, mapa_digrafos)
    nombre_convertido <- str_replace_all(nombre_convertido, mapa_simple)
    str_split(nombre_convertido, " ")[[1]] %>%
      map_chr(~ paste0(toupper(substr(.x, 1, 1)), substr(.x, 2, nchar(.x)))) %>%
      paste(collapse = " ")
  }, USE.NAMES = FALSE)
}

resultados_exitosos <- map(resultados_exitosos, function(res) {
  if (is.null(res) || is.null(res$partido_info)) {
    return(res)
  }
  if (is_latin(res$partido_info$local)) res$partido_info$local <- latin_to_cyrillic(res$partido_info$local)
  if (is_latin(res$partido_info$visitante)) res$partido_info$visitante <- latin_to_cyrillic(res$partido_info$visitante)
  if (!is.null(res$estadio) && is_latin(res$estadio)) res$estadio <- latin_to_cyrillic(res$estadio)
  if (!is.null(res$partido_info$estadio) && is_latin(res$partido_info$estadio)) res$partido_info$estadio <- latin_to_cyrillic(res$partido_info$estadio)
  if (!is.null(res$kontrolor) && length(res$kontrolor) == 1 && !is.na(res$kontrolor) && is_latin(res$kontrolor)) res$kontrolor <- latin_to_cyrillic(res$kontrolor)
  if (!is.null(res$delegado_nombre) && length(res$delegado_nombre) == 1 && !is.na(res$delegado_nombre) && is_latin(res$delegado_nombre)) res$delegado_nombre <- latin_to_cyrillic(res$delegado_nombre)
  if (!is.null(res$var_1_nombre) && length(res$var_1_nombre) == 1 && !is.na(res$var_1_nombre) && is_latin(res$var_1_nombre)) res$var_1_nombre <- latin_to_cyrillic(res$var_1_nombre)
  if (!is.null(res$var_2_nombre) && length(res$var_2_nombre) == 1 && !is.na(res$var_2_nombre) && is_latin(res$var_2_nombre)) res$var_2_nombre <- latin_to_cyrillic(res$var_2_nombre)
  if (!is.null(res$var_3_nombre) && length(res$var_3_nombre) == 1 && !is.na(res$var_3_nombre) && is_latin(res$var_3_nombre)) res$var_3_nombre <- latin_to_cyrillic(res$var_3_nombre)
  if (!is.null(res$arbitro_asist_4_nombre) && length(res$arbitro_asist_4_nombre) == 1 && !is.na(res$arbitro_asist_4_nombre) && is_latin(res$arbitro_asist_4_nombre)) res$arbitro_asist_4_nombre <- latin_to_cyrillic(res$arbitro_asist_4_nombre)

  # Standardize player names in rosters and events (Case: Scraper sends Latin names)
  if (nrow(res$alineacion_local) > 0 && "nombre" %in% names(res$alineacion_local)) {
    res$alineacion_local$nombre <- sapply(res$alineacion_local$nombre, function(n) if(!is.na(n) && is_latin(n)) latin_to_cyrillic(n) else n)
  }
  if (nrow(res$alineacion_visitante) > 0 && "nombre" %in% names(res$alineacion_visitante)) {
    res$alineacion_visitante$nombre <- sapply(res$alineacion_visitante$nombre, function(n) if(!is.na(n) && is_latin(n)) latin_to_cyrillic(n) else n)
  }
  if (nrow(res$goles) > 0 && "jugadora" %in% names(res$goles)) {
    res$goles$jugadora <- sapply(res$goles$jugadora, function(n) if(!is.na(n) && is_latin(n)) latin_to_cyrillic(n) else n)
  }
  if (nrow(res$tarjetas) > 0 && "jugadora" %in% names(res$tarjetas)) {
    res$tarjetas$jugadora <- sapply(res$tarjetas$jugadora, function(n) if(!is.na(n) && is_latin(n)) latin_to_cyrillic(n) else n)
  }
  if (!is.null(res$penales) && nrow(res$penales) > 0 && "jugadora" %in% names(res$penales)) {
    res$penales$jugadora <- sapply(res$penales$jugadora, function(n) if(!is.na(n) && is_latin(n)) latin_to_cyrillic(n) else n)
  }

  return(res)
})

# 10.0.3. Apply master ID unification (NEW LOGIC: based on incorrect name)
if (!is.null(mapa_unificacion_id_df) && nrow(mapa_unificacion_id_df) > 0) {
  message("   > Applying master ID unification rules (by name)...")

  # Asegurar que el nombre can\u00f3nico en el mapa tambi\u00e9n est\u00e9 en el formato correcto.
  mapa_unificacion_id_df$nombre_canonico <- reordenar_nombre_idempotente(mapa_unificacion_id_df$nombre_canonico)

  if (exists("desambiguacion_df") && !is.null(desambiguacion_df) && nrow(desambiguacion_df) > 0) {
    message("   > Incorporating homonym disambiguation rules into ID unification...")
    desambiguacion_norm <- desambiguacion_df
    names(desambiguacion_norm) <- tolower(names(desambiguacion_norm))

    if ("incorrect_name" %in% names(desambiguacion_norm)) {
      desambiguacion_norm <- rename(desambiguacion_norm, nombre_incorrecto = incorrect_name)
    }
    if ("name" %in% names(desambiguacion_norm) && !"nombre_incorrecto" %in% names(desambiguacion_norm)) {
      desambiguacion_norm <- rename(desambiguacion_norm, nombre_incorrecto = name)
    }
    if ("original_name" %in% names(desambiguacion_norm) && !"nombre_incorrecto" %in% names(desambiguacion_norm)) {
      desambiguacion_norm <- rename(desambiguacion_norm, nombre_incorrecto = original_name)
    }
    if ("correct_name" %in% names(desambiguacion_norm)) {
      desambiguacion_norm <- rename(desambiguacion_norm, nombre_canonico = correct_name)
    }
    if ("canonical_name" %in% names(desambiguacion_norm)) {
      desambiguacion_norm <- rename(desambiguacion_norm, nombre_canonico = canonical_name)
    }
    if ("id" %in% names(desambiguacion_norm) && !"id_canonico" %in% names(desambiguacion_norm)) {
      desambiguacion_norm <- rename(desambiguacion_norm, id_canonico = id)
    }
    if ("canonical_id" %in% names(desambiguacion_norm) && !"id_canonico" %in% names(desambiguacion_norm)) {
      desambiguacion_norm <- rename(desambiguacion_norm, id_canonico = canonical_id)
    }

    if ("nombre_incorrecto" %in% names(desambiguacion_norm) && "nombre_canonico" %in% names(desambiguacion_norm)) {
      desambiguacion_norm <- desambiguacion_norm %>%
        mutate(
          nombre_incorrecto = trimws(as.character(nombre_incorrecto)),
          nombre_canonico = trimws(as.character(nombre_canonico)),
          id_canonico = if ("id_canonico" %in% names(.) ) as.character(id_canonico) else NA_character_
        ) %>%
        filter(!is.na(nombre_incorrecto) & nombre_incorrecto != "") %>%
        distinct(nombre_incorrecto, nombre_canonico, id_canonico)

      if (nrow(desambiguacion_norm) > 0) {
        desambiguacion_norm$nombre_canonico <- reordenar_nombre_idempotente(desambiguacion_norm$nombre_canonico)
        mapa_unificacion_id_df <- bind_rows(mapa_unificacion_id_df, desambiguacion_norm)
      }
    }
  }

  # Nueva funci\u00f3n que unifica usando un left_join por el nombre.
  aplicar_unificacion_por_nombre <- function(df, mapa, col_nombre_df, col_id_df) {
    # Si el dataframe es nulo, est\u00e1 vac\u00edo o no tiene las columnas necesarias, devolverlo sin cambios.
    if (is.null(df) || nrow(df) == 0 || !col_nombre_df %in% names(df) || !col_id_df %in% names(df)) {
      return(df)
    }

    # Preparar el mapa para el join, renombrando 'nombre_incorrecto' para que coincida con la columna del df.
    mapa_para_join <- mapa %>%
      rename(!!col_nombre_df := nombre_incorrecto)

    df %>%
      left_join(mapa_para_join, by = col_nombre_df) %>%
      mutate(
        # Reemplazar el nombre y el ID originales por los can\u00f3nicos solo si se encontr\u00f3 una coincidencia.
        # coalesce() elige el primer valor no-NA.
        !!col_nombre_df := coalesce(nombre_canonico, .data[[col_nombre_df]]),
        !!col_id_df := coalesce(id_canonico, .data[[col_id_df]])
      ) %>%
      # Eliminar las columnas auxiliares que se a\u00f1adieron desde el mapa.
      select(-any_of(c("id_canonico", "nombre_canonico")))
  }

  # Aplicar la nueva funci\u00f3n de unificaci\u00f3n a todas las partes relevantes de los datos.
  resultados_exitosos <- map(resultados_exitosos, function(res) {
    if (is.null(res)) {
      return(NULL)
    }

    # Estandarizar nombres de columnas de ID si es necesario (ej. 'id_jugadora' -> 'id')
    if ("id_jugadora" %in% names(res$goles)) res$goles <- rename(res$goles, id = id_jugadora)
    if ("id_jugadora" %in% names(res$tarjetas)) res$tarjetas <- rename(res$tarjetas, id = id_jugadora)
    if ("id_jugadora" %in% names(res$penales)) res$penales <- rename(res$penales, id = id_jugadora)

    # Aplicar la unificaci\u00f3n por nombre a cada dataframe
    res$alineacion_local <- aplicar_unificacion_por_nombre(res$alineacion_local, mapa_unificacion_id_df, col_nombre_df = "nombre", col_id_df = "id")
    res$alineacion_visitante <- aplicar_unificacion_por_nombre(res$alineacion_visitante, mapa_unificacion_id_df, col_nombre_df = "nombre", col_id_df = "id")
    res$goles <- aplicar_unificacion_por_nombre(res$goles, mapa_unificacion_id_df, col_nombre_df = "jugadora", col_id_df = "id")
    res$tarjetas <- aplicar_unificacion_por_nombre(res$tarjetas, mapa_unificacion_id_df, col_nombre_df = "jugadora", col_id_df = "id")
    res$penales <- aplicar_unificacion_por_nombre(res$penales, mapa_unificacion_id_df, col_nombre_df = "jugadora", col_id_df = "id")

    return(res)
  })
}

# 10.0.4. Apply name corrections (conversions.txt) and reorder names idempotently
message("Applying idempotent name corrections and reordering...")

if (is.null(attr(resultados_exitosos, "nombres_procesados"))) {
  message("   > Data has not been processed yet. Applying corrections and reordering now...")

  resultados_exitosos <- map(resultados_exitosos, function(res) {
    if (is.null(res)) {
      return(NULL)
    }

    # STEP 1: APLICAR CORRECCIONES de conversions.txt de forma exhaustiva
    if (!is.null(mapa_conversiones_df)) {
      # Dataframes principales
      res$partido_info <- aplicar_conversiones(res$partido_info, c("local", "visitante", "competicion_nombre"), mapa_conversiones_df)
      if (nrow(res$alineacion_local) > 0 && "nombre" %in% names(res$alineacion_local)) res$alineacion_local <- aplicar_conversiones(res$alineacion_local, "nombre", mapa_conversiones_df)
      if (nrow(res$alineacion_visitante) > 0 && "nombre" %in% names(res$alineacion_visitante)) res$alineacion_visitante <- aplicar_conversiones(res$alineacion_visitante, "nombre", mapa_conversiones_df)

      # CORRECCI\u00d3N DEFINITIVA PARA LA CRONOLOG\u00cdA: Aplicar a los dataframes de eventos AHORA.
      if (nrow(res$goles) > 0 && "jugadora" %in% names(res$goles)) res$goles <- aplicar_conversiones(res$goles, c("jugadora", "equipo_jugadora", "equipo_acreditado"), mapa_conversiones_df)
      if (nrow(res$tarjetas) > 0 && "jugadora" %in% names(res$tarjetas)) res$tarjetas <- aplicar_conversiones(res$tarjetas, c("jugadora", "equipo"), mapa_conversiones_df)
      if (!is.null(res$penales) && nrow(res$penales) > 0 && "jugadora" %in% names(res$penales)) res$penales <- aplicar_conversiones(res$penales, c("jugadora", "equipo"), mapa_conversiones_df)

      # Vectores de texto
      res$estadio <- aplicar_conversiones(res$estadio, mapa_df = mapa_conversiones_df)
      res$arbitro_principal_nombre <- aplicar_conversiones(res$arbitro_principal_nombre, mapa_df = mapa_conversiones_df)
      res$arbitro_asist_1_nombre <- aplicar_conversiones(res$arbitro_asist_1_nombre, mapa_df = mapa_conversiones_df)
      res$arbitro_asist_2_nombre <- aplicar_conversiones(res$arbitro_asist_2_nombre, mapa_df = mapa_conversiones_df)
      res$kontrolor <- aplicar_conversiones(res$kontrolor, mapa_df = mapa_conversiones_df)
      res$delegado_nombre <- aplicar_conversiones(res$delegado_nombre, mapa_df = mapa_conversiones_df)
      res$var_1_nombre <- aplicar_conversiones(res$var_1_nombre, mapa_df = mapa_conversiones_df)
      res$var_2_nombre <- aplicar_conversiones(res$var_2_nombre, mapa_df = mapa_conversiones_df)
      res$var_3_nombre <- aplicar_conversiones(res$var_3_nombre, mapa_df = mapa_conversiones_df)
      res$arbitro_asist_4_nombre <- aplicar_conversiones(res$arbitro_asist_4_nombre, mapa_df = mapa_conversiones_df)
    }

    # STEP 2: REORDENAR NOMBRES
    reordenar_nombre_simple <- function(nombres) {
      sapply(nombres, function(nombre) {
        if (is.null(nombre) || is.na(nombre) || !stringr::str_detect(nombre, "\\s+")) {
          return(nombre)
        }
        palabras <- stringr::str_split(nombre, "\\s+")[[1]]
        primer_nombre <- palabras[length(palabras)]
        apellido <- paste(palabras[-length(palabras)], collapse = " ")
        return(paste(primer_nombre, apellido))
      }, USE.NAMES = FALSE)
    }

    if (nrow(res$alineacion_local) > 0 && "nombre" %in% names(res$alineacion_local)) res$alineacion_local$nombre <- reordenar_nombre_simple(res$alineacion_local$nombre)
    if (nrow(res$alineacion_visitante) > 0 && "nombre" %in% names(res$alineacion_visitante)) res$alineacion_visitante$nombre <- reordenar_nombre_simple(res$alineacion_visitante$nombre)
    if (nrow(res$goles) > 0 && "jugadora" %in% names(res$goles)) res$goles$jugadora <- reordenar_nombre_simple(res$goles$jugadora)
    if (nrow(res$tarjetas) > 0 && "jugadora" %in% names(res$tarjetas)) res$tarjetas$jugadora <- reordenar_nombre_simple(res$tarjetas$jugadora)
    if (!is.null(res$penales) && nrow(res$penales) > 0 && "jugadora" %in% names(res$penales)) res$penales$jugadora <- reordenar_nombre_simple(res$penales$jugadora)

    res$arbitro_principal_nombre <- reordenar_nombre_simple(res$arbitro_principal_nombre)
    res$arbitro_asist_1_nombre <- reordenar_nombre_simple(res$arbitro_asist_1_nombre)
    res$arbitro_asist_2_nombre <- reordenar_nombre_simple(res$arbitro_asist_2_nombre)
    res$arbitro_asist_4_nombre <- reordenar_nombre_simple(res$arbitro_asist_4_nombre)
    res$delegado_nombre <- reordenar_nombre_simple(res$delegado_nombre)
    res$var_1_nombre <- reordenar_nombre_simple(res$var_1_nombre)
    res$var_2_nombre <- reordenar_nombre_simple(res$var_2_nombre)
    res$var_3_nombre <- reordenar_nombre_simple(res$var_3_nombre)
    res$kontrolor <- reordenar_nombre_simple(res$kontrolor)

    return(res)
  })

  attr(resultados_exitosos, "nombres_procesados") <- TRUE
  message("   > Name corrections and reordering complete. Data has been marked as processed.")
} else {
  message("   > Names have already been corrected and reordered in a previous run. Skipping.")
}

# 10.0.5. Simplify referee names (post-correction)
message("   > Simplifying referee names...")
resultados_exitosos <- map(resultados_exitosos, function(res) {
  if (is.null(res)) {
    return(NULL)
  }
  if (is.list(res$arbitro_principal_nombre)) res$arbitro_principal_nombre <- res$arbitro_principal_nombre$nombre
  if (is.list(res$arbitro_asist_1_nombre)) res$arbitro_asist_1_nombre <- res$arbitro_asist_1_nombre$nombre
  if (is.list(res$arbitro_asist_2_nombre)) res$arbitro_asist_2_nombre <- res$arbitro_asist_2_nombre$nombre
  return(res)
})

# 10.0.6. Standardize competition names (Fixing case mismatches like \u0421\u0423\u041f\u0415\u0420 vs \u0441\u0443\u043f\u0435\u0440)
message("   > Standardizing competition names...")
resultados_exitosos <- map(resultados_exitosos, function(res) {
  if (is.null(res) || is.null(res$partido_info)) return(res)
  
  if (!is.null(res$partido_info$competicion_nombre)) {
  }
  
      res$competicion_nombre <- str_replace_all(res$competicion_nombre, "(?i)\u0421\u0423\u041f\u0415\u0420", "\u0441\u0443\u043f\u0435\u0440")
  
  return(res)
})

message("Step 10.0: Raw data cleansing complete.")

### 10.1. Create Master Dataframes from Cleaned Source
message("Step 10.1: Creating master dataframes from the clean source...")

# Load official_results.txt into official_results_df so the existing merge logic works.
if (!exists("official_results_df")) {
  official_results_df <- tibble(id_partido = character(), goles_local = numeric(), goles_visitante = numeric())
  ruta_oficial_candidates <- c("official_results.txt", "dictionaries/official_results.txt")
  ruta_oficial_existente <- ruta_oficial_candidates[file.exists(ruta_oficial_candidates)]
  if (length(ruta_oficial_existente) > 0) {
    tryCatch({
      official_results_df <- read.csv(ruta_oficial_existente[[1]], stringsAsFactors = FALSE, encoding = "UTF-8") %>%
        mutate(id_partido = as.character(id_partido)) %>%
        distinct(id_partido, .keep_all = TRUE)
      message(paste("   > Loaded", nrow(official_results_df), "official results from", ruta_oficial_existente[[1]]))
    }, error = function(e) {
      warning("Could not load official results. Continuing with an empty dataframe.")
    })
  } else {
    message("   > official_results.txt not found. Continuing with an empty official_results_df.")
  }
}

# Defensive fallback in case the loader script did not define cancelled IDs/rules.
if (!exists("cancelled_matches_ids") || !exists("cancelled_matches_rules")) {
  cancelled_matches_ids <- if (exists("cancelled_matches_ids")) cancelled_matches_ids else character(0)
  cancelled_matches_rules <- if (exists("cancelled_matches_rules")) cancelled_matches_rules else tibble(club = character(), competicion = character(), jornada_inicio = integer())
  
  ruta_cancelados_candidates <- c("cancelled_matches.txt", "dictionaries/cancelled_matches.txt")
  ruta_cancelados_existente <- ruta_cancelados_candidates[file.exists(ruta_cancelados_candidates)]

  if (length(ruta_cancelados_existente) > 0) {
    tryCatch({
      temp_lines <- readLines(ruta_cancelados_existente[[1]], warn = FALSE, encoding = "UTF-8")
      temp_lines <- trimws(temp_lines)
      temp_lines <- temp_lines[temp_lines != "" & !startsWith(temp_lines, "#")]
      
      # Rules with commas (retroactive withdrawal)
      rules_lines <- temp_lines[grepl(",", temp_lines)]
      if (length(rules_lines) > 0) {
        cancelled_matches_rules <- map_dfr(rules_lines, function(l) {
          parts <- str_split(l, ",", simplify = TRUE)
          if (ncol(parts) >= 3) {
            tibble(
              club = str_trim(parts[1]),
              competicion = str_trim(parts[2]),
              jornada_inicio = as.integer(str_trim(parts[3]))
            )
          } else {
            NULL
          }
        })
      }
      
      # Plain IDs
      cancelled_matches_ids <- temp_lines[!grepl(",", temp_lines)]
      
      # Apply name conversions to rules to match processed data
      if (nrow(cancelled_matches_rules) > 0 && exists("mapa_conversiones_df") && !is.null(mapa_conversiones_df)) {
        cancelled_matches_rules <- aplicar_conversiones(cancelled_matches_rules, c("club", "competicion"), mapa_conversiones_df)
      }
      
      message(paste("   > Loaded", nrow(cancelled_matches_rules), "withdrawal rules and", length(cancelled_matches_ids), "plain IDs from", ruta_cancelados_existente[[1]]))
    }, error = function(e) {

      warning("Could not load cancelled matches list. Continuing with empty lists.")
    })
  } else {
    message("   > Cancelled matches list not found. Continuing with empty lists.")
  }
}

# 10.1.1. Create master matches dataframe and merge with future calendars
partidos_df_reales <- map_dfr(resultados_exitosos, "partido_info")
partidos_df_placeholders <- cargar_calendarios_excel()

if (nrow(partidos_df_placeholders) > 0 && "competicion_nombre" %in% names(partidos_df_placeholders)) {
  partidos_df_placeholders <- partidos_df_placeholders %>%
    mutate(competicion_nombre = str_replace_all(competicion_nombre, "(?i)\u0421\u0423\u041f\u0415\u0420", "\u0441\u0443\u043f\u0435\u0440"))
}

if (!is.null(mapa_conversiones_df)) {
  partidos_df_placeholders <- aplicar_conversiones(partidos_df_placeholders, c("local", "visitante"), mapa_conversiones_df)
}
if (nrow(partidos_df_placeholders) > 0 && nrow(partidos_df_reales) > 0) {
  partidos_df_reales <- partidos_df_reales %>% mutate(match_key = paste(local, visitante, competicion_nombre, competicion_temporada, jornada))
  partidos_df_placeholders <- partidos_df_placeholders %>% mutate(match_key = paste(local, visitante, competicion_nombre, competicion_temporada, jornada))
  placeholders_a_mantener <- partidos_df_placeholders %>% anti_join(partidos_df_reales, by = "match_key")
  partidos_df <- bind_rows(partidos_df_reales %>% select(-match_key), placeholders_a_mantener %>% select(-match_key))
} else {
  partidos_df <- partidos_df_reales
}

if (!"es_resultado_oficial" %in% names(partidos_df)) {
  partidos_df <- partidos_df %>% mutate(es_resultado_oficial = FALSE)
}
if (exists("official_results_df") && nrow(official_results_df) > 0 && nrow(partidos_df) > 0) {
  partidos_df <- partidos_df %>%
    left_join(official_results_df, by = "id_partido") %>%
    mutate(
      es_resultado_oficial = if_else(!is.na(goles_local.y) & !is.na(goles_visitante.y), TRUE, es_resultado_oficial),
      goles_local = if_else(!is.na(goles_local.y), as.numeric(goles_local.y), as.numeric(goles_local.x)),
      goles_visitante = if_else(!is.na(goles_visitante.y), as.numeric(goles_visitante.y), as.numeric(goles_visitante.x))
    ) %>%
    select(-goles_local.x, -goles_local.y, -goles_visitante.x, -goles_visitante.y)
}

message("   > Master `partidos_df` created.")

# Treat Baraz competitions as part of the previous season for grouping/sorting,
# but preserve the original season digits for display.
ajustar_temporada_baraz <- function(nombre_competicion, temporada) {
  nombre_chr <- as.character(nombre_competicion)
  temporada_chr <- as.character(temporada)

  ifelse(
    !is.na(nombre_chr) & str_detect(nombre_chr, regex("\\bбараж\\b", ignore_case = TRUE)) &
      !is.na(temporada_chr) & str_detect(temporada_chr, "^\\d{2}/\\d{2}$"),
    sprintf(
      "%02d/%02d",
      as.numeric(substr(temporada_chr, 1, 2)) - 1,
      as.numeric(substr(temporada_chr, 4, 5)) - 1
    ),
    temporada_chr
  )
}

partidos_df <- partidos_df %>%
  mutate(
    temporada_display = competicion_temporada,
    competicion_temporada = ajustar_temporada_baraz(competicion_nombre, competicion_temporada)
  )

# 10.1.1.0 Calculate cancellation status: combine explicit IDs + scraper-set flags + retroactive withdrawal rules
partidos_df <- partidos_df %>%
  mutate(
    # Apply withdrawal rules retroactively to all matches (even if already in cache)
    .es_retirado_por_regla = sapply(seq_len(n()), function(i) {
      if (nrow(cancelled_matches_rules) == 0) return(FALSE)
      
      # Helper to normalize club names by removing common prefixes (FK, ФК, etc.)
      norm_club <- function(x) {
        tolower(trimws(str_replace_all(x, "(?i)^\\s*(fk|фк|жфк|zfk|shfk|шфк)\\s+", "")))
      }
      
      p_local <- norm_club(local[i])
      p_visit <- norm_club(visitante[i])
      # Reconstruct full competition name as found in rules (Name + Season)
      p_comp_completa <- tolower(trimws(paste(competicion_nombre[i], temporada_display[i])))
      p_jornada <- as.integer(jornada[i])
      
      any(cancelled_matches_rules$jornada_inicio <= p_jornada &
          tolower(trimws(cancelled_matches_rules$competicion)) == p_comp_completa &
          (norm_club(cancelled_matches_rules$club) == p_local | 
           norm_club(cancelled_matches_rules$club) == p_visit))
    }),
    # Identify if a team is retired from the competition AT ALL (ignoring jornada_inicio for point exclusion)
    .es_equipo_retirado = sapply(seq_len(n()), function(i) {
      if (nrow(cancelled_matches_rules) == 0) return(FALSE)
      
      norm_club <- function(x) {
        tolower(trimws(str_replace_all(x, "(?i)^\\s*(fk|фк|жфк|zfk|shfk|шфк)\\s+", "")))
      }
      
      p_local <- norm_club(local[i])
      p_visit <- norm_club(visitante[i])
      p_comp_completa <- tolower(trimws(paste(competicion_nombre[i], temporada_display[i])))
      
      any(tolower(trimws(cancelled_matches_rules$competicion)) == p_comp_completa &
          (norm_club(cancelled_matches_rules$club) == p_local | 
           norm_club(cancelled_matches_rules$club) == p_visit))
    }),
    es_cancelado = coalesce(
      if_else(.es_retirado_por_regla, TRUE, NA),
      # 1. If the scraper already set es_cancelado, use that
      if ("es_cancelado" %in% names(.)) es_cancelado else NA,
      # 2. Otherwise check if the match ID is in the cancelled list
      as.character(id_partido) %in% cancelled_matches_ids,
      # 3. Default to FALSE
      FALSE
    ),
    es_retirado = .es_equipo_retirado
  ) %>%
  select(-.es_retirado_por_regla, -.es_equipo_retirado)



# 10.1.1.1. Normalize and attach competition category to each match
normalize_competition_name_for_lookup <- function(name) {
  name %>%
    as.character() %>%
    str_squish() %>%
    tolower() %>%
    na_if("")
}

build_competicion_nombre_raw_for_category <- function(competicion_nombre, competicion_temporada = NULL) {
  nombre <- as.character(competicion_nombre) %>% str_squish()
  nombre[nombre == ""] <- NA_character_

  temporada <- if (!is.null(competicion_temporada)) {
    as.character(competicion_temporada) %>% str_squish()
  } else {
    NA_character_
  }
  temporada[temporada == ""] <- NA_character_

  purrr::map2_chr(nombre, temporada, function(nom, temp) {
    if (is.na(nom) || nom == "") {
      return(NA_character_)
    }
    if (is.na(temp) || temp == "") {
      return(nom)
    }

    trailing_season <- str_extract(nom, "(?:\\d{4}-\\d{4}|\\d{2}/\\d{2})\\s*$")
    if (!is.na(trailing_season) && trailing_season == temp) {
      return(nom)
    }
    if (!is.na(trailing_season) && trailing_season != temp) {
      return(str_c(nom, temp, sep = " "))
    }
    str_c(nom, temp, sep = " ")
  })
}

if (!"categoria" %in% names(partidos_df)) {
  partidos_df$categoria <- NA_character_
}
if (!"vid_natprevaruvanje" %in% names(partidos_df)) {
  partidos_df$"vid_natprevaruvanje" <- NA_character_
}
if (!"min_age" %in% names(partidos_df)) {
  partidos_df$min_age <- NA_real_
}
if (!"max_age" %in% names(partidos_df)) {
  partidos_df$max_age <- NA_real_
}
if (exists("mapa_categorias_competiciones") && nrow(mapa_categorias_competiciones) > 0 && nrow(partidos_df) > 0) {
  mapa_categorias_competiciones_raw <- mapa_categorias_competiciones %>%
    mutate(
      competicion_nombre_key_raw = normalize_competition_name_for_lookup(competicion_nombre)
    ) %>%
    filter(!is.na(competicion_nombre_key_raw)) %>%
    select(
      competicion_nombre_key_raw,
      categoria_mapa = categoria,
      vid_natprevaruvanje_mapa = vid_natprevaruvanje,
      min_age_mapa = min_age,
      max_age_mapa = max_age
    ) %>%
    distinct(competicion_nombre_key_raw, .keep_all = TRUE)

  partidos_df <- partidos_df %>%
    mutate(
      competicion_nombre_raw_for_categoria = build_competicion_nombre_raw_for_category(
        competicion_nombre,
        if ("competicion_temporada" %in% names(.)) competicion_temporada else NULL
      ),
      competicion_nombre_key_raw = normalize_competition_name_for_lookup(competicion_nombre_raw_for_categoria)
    ) %>%
    left_join(mapa_categorias_competiciones_raw, by = "competicion_nombre_key_raw") %>%
    mutate(
      categoria = categoria_mapa,
      "vid_natprevaruvanje" = coalesce(vid_natprevaruvanje_mapa, "vid_natprevaruvanje"),
      min_age = coalesce(min_age_mapa, min_age),
      max_age = coalesce(
        max_age_mapa,
        case_when(
        categoria == "\u041c\u043b\u0430\u0434\u0438\u043d\u0446\u0438" ~ 19,
        categoria == "\u041a\u0430\u0434\u0435\u0442\u0438" ~ 18,
        categoria == "\u041f\u0438\u043e\u043d\u0435\u0440\u0438" ~ 17,
        categoria == "\u041c\u043b\u0430\u0434\u0438 \u041f\u0438\u043e\u043d\u0435\u0440\u0438" ~ 16,
        categoria == "\u041f\u0435\u0442\u043b\u0438\u045a\u0430" ~ 15,
        categoria == "\u041f\u043e\u043c\u0430\u043b\u0438 \u043f\u0435\u0442\u043b\u0438\u045a\u0430" ~ 14,
        categoria == "\u0414\u0435\u0442\u0441\u043a\u0430 \u043b\u0438\u0433\u0430" ~ 13,
        TRUE ~ NA_real_
      ),
      max_age
      )

    ) %>%
    select(
      -competicion_nombre_raw_for_categoria,
      -competicion_nombre_key_raw,
      -categoria_mapa,
      -vid_natprevaruvanje_mapa,
      -min_age_mapa,
      -max_age_mapa
    )
} else {
  warning("Competition category mapping from comps_ffm.xlsx is unavailable. Match categories will remain unknown.")
  partidos_df <- partidos_df %>% 
    mutate(
      categoria = NA_character_,
      max_age = NA_real_,
      vid_natprevaruvanje = NA_character_, 
      min_age = NA_real_
    )
}

# 10.1.1.2. Create division label separate from category (senior vs youth etc.)
calcular_division <- function(comp_name) {
  comp_clean <- tolower(trimws(as.character(comp_name)))
  case_when(
    str_detect(comp_clean, "\u043f\u0440\u0432\u0430\\s+\u043c\u0444\u043b|prva\\s+mfl") ~ "\u041f\u0440\u0432\u0430 \u041c\u0424\u041b",
    str_detect(comp_clean, "\u0432\u0442\u043e\u0440\u0430\\s+\u043c\u0444\u043b|vtora\\s+mfl") ~ "\u0412\u0442\u043e\u0440\u0430 \u041c\u0424\u041b",
    str_detect(comp_clean, "\u0442\u0440\u0435\u0442\u0430\\s+\u043c\u0444\u043b|treta\\s+mfl") ~ "\u0422\u0440\u0435\u0442\u0430 \u041c\u0424\u041b",
    TRUE ~ NA_character_
  )
}

if (nrow(partidos_df) > 0) {
  partidos_df <- partidos_df %>%
    mutate(
      division = calcular_division(competicion_nombre)
    )
}

# 10.1.2. Create other master dataframes
goles_df_unificado <- map_dfr(resultados_exitosos, "goles")
tarjetas_df_unificado <- map_dfr(resultados_exitosos, "tarjetas")
penales_df_unificado <- map_dfr(resultados_exitosos, "penales")

  # Exclude stats for matches that are cancelled, involve retired teams, or have an official result override
  ids_excluidos_stats <- partidos_df %>% filter((es_cancelado %in% TRUE) | (es_retirado %in% TRUE) | (es_resultado_oficial %in% TRUE)) %>% pull(id_partido)

  if (length(ids_excluidos_stats) > 0) {
    if (nrow(goles_df_unificado) > 0) goles_df_unificado <- goles_df_unificado %>% filter(!id_partido %in% ids_excluidos_stats)
    if (nrow(tarjetas_df_unificado) > 0) tarjetas_df_unificado <- tarjetas_df_unificado %>% filter(!id_partido %in% ids_excluidos_stats)
    if (!is.null(penales_df_unificado) && nrow(penales_df_unificado) > 0) penales_df_unificado <- penales_df_unificado %>% filter(!id_partido %in% ids_excluidos_stats)
  }


arbitros_df <- map_dfr(resultados_exitosos, function(res) {
  if (is.null(res) || is.null(res$partido_info)) {
    return(NULL)
  }

  df_arbitros_partido <- tibble(
    id_partido = character(),
    ime = character(),
    ciudad = character(),
    uloga = character(),
    uloga_raw = character()
  )

  id_p <- res$partido_info$id_partido

  # Funci\u00f3n auxiliar para extraer de forma segura y LIMPIAR el nombre del \u00e1rbitro.
  extraer_info_arbitro <- function(obj) {
    if (is.null(obj)) {
      return(list(nombre = NULL, ciudad = NULL))
    }

    nombre_raw <- if (is.list(obj)) obj$nombre %||% NULL else as.character(obj)
    if (is.null(nombre_raw)) {
      return(list(nombre = NULL, ciudad = NULL))
    }

    # CORRECCI\u00d3N DEFINITIVA: Limpiar el par\u00e9ntesis del nombre.
    nombre_limpio <- str_remove(nombre_raw, "\\s*\\(.*\\)$") %>% trimws()

    # CORRECCI\u00d3N DEFINITIVA: Forzar la ciudad a ser NA para que nunca se muestre.
    ciudad_final <- NA_character_

    return(list(nombre = nombre_limpio, ciudad = ciudad_final))
  }

  mapear_clave_uloga <- function(rol_raw) {
    if (is.null(rol_raw) || is.na(rol_raw) || trimws(rol_raw) == "") {
      return(NA_character_)
    }

    rol_norm <- rol_raw %>%
      as.character() %>%
      tolower() %>%
      str_replace_all("\\s+", " ") %>%
      str_trim()

    case_when(
      str_detect(rol_norm, "\u0433\u043b\u0430\u0432\u0435\u043d\\s+\u0441\u0443\u0434\u0438\u0458\u0430|main\\s+referee|referee\\s+main") ~ "referee_main",
      str_detect(rol_norm, "(^|\\b)1\\.?\\s*\u043f\u043e\u043c\u043e\u0448\u0435\u043d|assistant\\s*1|1st\\s*assistant|first\\s*assistant") ~ "referee_asst1",
      str_detect(rol_norm, "(^|\\b)2\\.?\\s*\u043f\u043e\u043c\u043e\u0448\u0435\u043d|assistant\\s*2|2nd\\s*assistant|second\\s*assistant") ~ "referee_asst2",
      str_detect(rol_norm, "\u0447\u0435\u0442\u0432\u0440\u0442|4\\.?\\s*\u0441\u0443\u0434\u0438\u0458\u0430|assistant\\s*4|fourth\\s*official") ~ "referee_asst4",
      str_detect(rol_norm, "\u0430\u0432\u0430\u0440|avar|var\\s*assistant|\u0432\u0430\u0440\\s*2|var\\s*2") ~ "var_assistant",
      str_detect(rol_norm, "\u0432\u0430\u0440\\s*\u043e\u043f\u0435\u0440\u0430\u0442\u043e\u0440|var\\s*operator|\u0432\u0430\u0440\\s*3|var\\s*3|\u0432\u0430\u0440\\s*4|var\\s*4") ~ "var_operator",
      str_detect(rol_norm, "(^|\\b)\u0432\u0430\u0440\\s*\u0441\u0443\u0434\u0438\u0458\u0430|var\\s*referee|\u0432\u0430\u0440\\s*1|var\\s*1") ~ "var_referee",
      str_detect(rol_norm, "\u0434\u0435\u043b\u0435\u0433\u0430\u0442|delegate") ~ "match_delegate",
      str_detect(rol_norm, "\u043a\u043e\u043d\u0442\u0440\u043e\u043b\u043e\u0440|controller") ~ "match_kontrolor",
      str_detect(rol_norm, "\u043a\u043e\u043c\u0435\u0441\u0430\u0440\\s*\u0437\u0430\\s*\u0431\u0435\u0437\u0431\u0435\u0434\u043d\u043e\u0441\u0442|security\\s*commissioner") ~ "security_commissioner",
      TRUE ~ NA_character_
    )
  }

  limpiar_nombre_oficial <- function(nombre_raw) {
    if (is.null(nombre_raw) || is.na(nombre_raw)) {
      return(NULL)
    }
    nombre_txt <- as.character(nombre_raw)
    if (
      exists("is_latin") && is.function(is_latin) &&
      exists("latin_to_cyrillic") && is.function(latin_to_cyrillic) &&
      isTRUE(is_latin(nombre_txt))
    ) {
      nombre_txt <- latin_to_cyrillic(nombre_txt)
    }
    nombre_txt <- str_remove(nombre_txt, "\\s*\\(.*\\)$") %>% str_trim()
    if (nombre_txt == "") {
      return(NULL)
    }
    nombre_txt
  }

  add_ref_row <- function(df, nombre, uloga_key, uloga_raw_val = NA_character_, ciudad_val = NA_character_) {
    if (is.null(nombre) || is.na(nombre) || trimws(nombre) == "" || is.na(uloga_key) || trimws(uloga_key) == "") {
      return(df)
    }
    bind_rows(df, tibble(
      id_partido = id_p,
      ime = as.character(nombre),
      ciudad = ciudad_val,
      uloga = as.character(uloga_key),
      uloga_raw = uloga_raw_val
    ))
  }

  # As requested: ONLY use referees coming from delegiranje (skip old report parsing)
  # AND reverse their names like the old logic used to.
  if (!is.null(res$oficiales_delegacion) && is.data.frame(res$oficiales_delegacion) && nrow(res$oficiales_delegacion) > 0) {
    oficiales_extra <- res$oficiales_delegacion %>%
      mutate(
        rol = if ("rol" %in% names(.)) as.character(rol) else NA_character_,
        nombre = if ("nombre" %in% names(.)) as.character(nombre) else NA_character_
      ) %>%
      select(rol, nombre)

    for (r in seq_len(nrow(oficiales_extra))) {
      rol_raw <- oficiales_extra$rol[r] %||% NA_character_
      nombre_limpio_raw <- limpiar_nombre_oficial(oficiales_extra$nombre[r])
      if (is.null(nombre_limpio_raw)) next
      
      # Reverse Name
      nombre_limpio <- reordenar_nombre_idempotente(nombre_limpio_raw)

      rol_key <- mapear_clave_uloga(rol_raw)
      if (is.na(rol_key) || trimws(rol_key) == "") {
        rol_key <- paste0("referee_role_", generar_id_seguro(rol_raw %||% "unknown"))
      }

      df_arbitros_partido <- add_ref_row(df_arbitros_partido, nombre_limpio, rol_key, uloga_raw_val = rol_raw)
    }
  }

  df_arbitros_partido %>%
    arrange(desc(!is.na(uloga_raw) & trimws(uloga_raw) != "")) %>%
    distinct(id_partido, ime, uloga, .keep_all = TRUE)
}) %>%
  {
    if ("ime" %in% names(.)) {
      filter(., !is.na(ime), ime != "Desconocido")
    } else {
      tibble(id_partido = character(), ime = character(), ciudad = character(), uloga = character(), uloga_raw = character())
    }
  }


estadios_df <- map_dfr(resultados_exitosos, function(res) {
  if (is.null(res) || is.null(res$partido_info)) {
    return(NULL)
  }
  nombre_estadio <- res$estadio %||% res$partido_info$estadio %||% NA_character_
  tibble(id_partido = res$partido_info$id_partido, estadio = nombre_estadio)
}) %>%
  # CORRECCI\u00d3N: A\u00f1adir las columnas de competici\u00f3n y categor\u00eda al join.
  left_join(select(partidos_df, id_partido, local, visitante, fecha, competicion_nombre, competicion_temporada, categoria), by = "id_partido")

# 10.1.3. Create master staff dataframe (coaches, medical)
staff_df <- map_dfr(resultados_exitosos, function(res) {
  if (is.null(res) || is.null(res$partido_info)) {
    return(NULL)
  }
  id_p <- res$partido_info$id_partido
  local <- res$partido_info$local
  visitante <- res$partido_info$visitante

  filas <- tibble(
    id_partido = character(), nombre = character(),
    rol = character(), equipo = character()
  )

  # Staff completo desde najava (priority if available)
  tiene_staff_local <- !is.null(res$staff_local) && is.data.frame(res$staff_local) && nrow(res$staff_local) > 0
  tiene_staff_visitante <- !is.null(res$staff_visitante) && is.data.frame(res$staff_visitante) && nrow(res$staff_visitante) > 0

  if (tiene_staff_local) {
    filas <- bind_rows(filas, res$staff_local %>% mutate(id_partido = id_p, equipo = local))
  } else if (!is.null(res$entrenador_local) && !is.na(res$entrenador_local) && res$entrenador_local != "Desconocido") {
    # Fallback: solo entrenador jefe (t\u00edpicamente desde PDF)
    filas <- add_row(filas, id_partido = id_p, nombre = res$entrenador_local, rol = "head_coach", equipo = local)
  }

  if (tiene_staff_visitante) {
    filas <- bind_rows(filas, res$staff_visitante %>% mutate(id_partido = id_p, equipo = visitante))
  } else if (!is.null(res$entrenador_visitante) && !is.na(res$entrenador_visitante) && res$entrenador_visitante != "Desconocido") {
    filas <- add_row(filas, id_partido = id_p, nombre = res$entrenador_visitante, rol = "head_coach", equipo = visitante)
  }

  filas
}) %>%
  filter(!is.na(nombre), nchar(trimws(nombre)) > 0)

# 10.1.4. Normalize staff names (Cyrillic, corrections, reordering)
if (nrow(staff_df) > 0) {
  staff_df <- staff_df %>%
    mutate(nombre = if_else(sapply(nombre, is_latin), latin_to_cyrillic(nombre), nombre))
  if (!is.null(mapa_conversiones_df)) {
    staff_df <- aplicar_conversiones(staff_df, "nombre", mapa_conversiones_df)
  }
  staff_df <- staff_df %>%
    mutate(nombre = reordenar_nombre_idempotente(nombre))
  # Fix role keys that fell through the web scraper's fallback as raw Macedonian
  staff_df <- staff_df %>%
    mutate(rol = case_when(
      rol == "\u043a\u043e\u043d\u0434\u0438\u0446\u0438\u0441\u043a\u0438_\u0442\u0440\u0435\u043d\u0435\u0440" ~ "fitness_coach",
      rol == "\u0431\u0435\u0437\u0431\u0435\u0434\u043d\u043e\u0441\u043d\u043e_\u043b\u0438\u0446\u0435" ~ "security_commissioner",
      rol == "\u0442\u0440\u0435\u043d\u0435\u0440_\u043d\u0430_\u0433\u043e\u043b\u043c\u0430\u043d\u0438" ~ "goalkeeping_coach",
      TRUE ~ rol
    ))
}
message(paste("   > Master `staff_df` created with", nrow(staff_df), "entries."))
# Final event joins for cancellation propagation
# Final event joins for cancellation propagation
for (df_name in c("goles_df_unificado", "tarjetas_df_unificado", "penales_df_unificado", "staff_df")) {
  if (exists(df_name) && nrow(get(df_name)) > 0 && "id_partido" %in% names(get(df_name))) {
    df_temp <- get(df_name)
    df_temp <- df_temp %>%
      select(-any_of("es_cancelado")) %>%
      left_join(partidos_df %>% select(id_partido, es_cancelado), by = "id_partido")
    assign(df_name, df_temp, envir = .GlobalEnv)
  }
}

message(paste("   > All other master dataframes created."))

### 10.2. Assign Match Duration (national team logic disabled)
message("Step 10.2: Applying business logic (match duration; national team logic disabled)...")
partidos_df <- partidos_df %>%
  mutate(
    categoria_normalizada_duracion = normalizar_categoria_competicion(categoria, competicion_nombre),
    duracion_partido = case_when(
      categoria_normalizada_duracion %in% c("\u041f\u0435\u0442\u043b\u0438\u045a\u0430", "\u041f\u043e\u043c\u0430\u043b\u0438 \u043f\u0435\u0442\u043b\u0438\u045a\u0430") ~ 80,
      TRUE ~ 90
    ),
    es_partido_seleccion = FALSE
  ) %>%
  select(-categoria_normalizada_duracion)

### 10.3. Consolidate Player Appearances and Unify IDs
message("Step 10.3: Consolidating player appearances and unifying IDs...")
apariciones_df_raw <- map_dfr(resultados_exitosos, ~ bind_rows(
  .x$alineacion_local %>% mutate(id_partido = .x$partido_info$id_partido, equipo = .x$partido_info$local),
  .x$alineacion_visitante %>% mutate(id_partido = .x$partido_info$id_partido, equipo = .x$partido_info$visitante)
)) %>% mutate(nombre = str_squish(nombre))

if (exists("ids_excluidos_stats") && length(ids_excluidos_stats) > 0) {
  apariciones_df_raw <- apariciones_df_raw %>% filter(!id_partido %in% ids_excluidos_stats)
}
# --- Mapeo de Identificadores y Algoritmo ADE (Automated Disambiguation Engine) ---
message("   > Running Automated Disambiguation Engine (ADE)...")

# Paso 1: Unir con datos del partido para obtener fechas cronol\u00f3gicas y categor\u00edas
apariciones_con_fechas <- apariciones_df_raw %>%
  filter(!is.na(nombre) & nchar(trimws(nombre)) > 2) %>%
  left_join(partidos_df %>% select(id_partido, fecha, categoria), by = "id_partido") %>%
  mutate(
    # Convertir a formato Date
    fecha_date = as.Date(fecha, format = "%d.%m.%Y")
  )

# Regla Biol\u00f3gica: Secciones compatibles/incompatibles
es_categoria_mayor <- function(cat) {
  sapply(cat, function(x) {
    if (is.na(x) || trimws(as.character(x)) == "") {
      return(FALSE)
    }
    str_detect(tolower(as.character(x)), "senior|\u0441\u0435\u043d\u0438\u043e\u0440\u0438|\u043c\u043b\u0430\u0434\u0438\u043d\u0446\u0438|youth|\u043a\u0430\u0434\u0435\u0442\u0438|cadet")
  }, USE.NAMES = FALSE)
}
es_categoria_menor <- function(cat) {
  sapply(cat, function(x) {
    if (is.na(x) || trimws(as.character(x)) == "") {
      return(FALSE)
    }
    str_detect(tolower(as.character(x)), "\u043f\u0438\u043e\u043d\u0435\u0440\u0438|pioner|\u0434\u0435\u0442\u0441\u043a\u0430|kids|petlinja")
  }, USE.NAMES = FALSE)
}

# Excepcion de mercado: durante ventanas de fichajes permitimos transiciones
# cortas entre equipos sin forzar homonimia, salvo conflicto biologico.
es_ventana_fichajes <- function(fecha_a, fecha_b) {
  if (any(is.na(c(fecha_a, fecha_b)))) {
    return(FALSE)
  }

  mes_a <- as.integer(format(fecha_a, "%m"))
  mes_b <- as.integer(format(fecha_b, "%m"))

  en_verano <- mes_a %in% c(6, 7, 8, 9) || mes_b %in% c(6, 7, 8, 9)
  en_invierno <- mes_a %in% c(11, 12, 1, 2) && mes_b %in% c(11, 12, 1, 2)
  cruza_invierno <- (mes_a %in% c(11, 12) && mes_b %in% c(1, 2)) ||
    (mes_b %in% c(11, 12) && mes_a %in% c(1, 2))

  en_verano || en_invierno || cruza_invierno
}

calcular_gap_intervalos <- function(min_a, max_a, min_b, max_b) {
  if (any(is.na(c(min_a, max_a, min_b, max_b)))) {
    return(Inf)
  }

  inicio_tardio <- max(min_a, min_b)
  fin_temprano <- min(max_a, max_b)
  as.numeric(inicio_tardio - fin_temprano)
}

# Paso 2: Resumir intervalos de fechas y compatibilidad biol\u00f3gica por nombre + equipo
id_assignments <- apariciones_con_fechas %>%
  filter(!is.na(fecha_date), !is.na(equipo)) %>%
  group_by(nombre, equipo) %>%
  summarise(
    min_fecha = min(fecha_date, na.rm = TRUE),
    max_fecha = max(fecha_date, na.rm = TRUE),
    has_mayor = any(es_categoria_mayor(categoria), na.rm = TRUE),
    has_menor = any(es_categoria_menor(categoria), na.rm = TRUE),
    .groups = "drop"
  )

# Paso 3: Agrupaci\u00f3n (Clustering) Detecci\u00f3n y Generaci\u00f3n del Sufijo ("hristijan_micovski_1")
list_res <- lapply(split(id_assignments, id_assignments$nombre), function(teams_data) {
    teams_data <- teams_data %>% arrange(min_fecha)
    n_teams <- nrow(teams_data)
    umbral_dias_conflicto <- 15

    # Jugador sin homonimos obvios (solo en un equipo)
    if (n_teams == 1) {
      teams_data$cluster <- 1
      teams_data$is_homonym <- FALSE
      return(teams_data)
    }

    clusters <- list()
    for (i in seq_len(n_teams)) {
      t_row <- teams_data[i, ]
      assigned <- FALSE

      for (c_idx in seq_along(clusters)) {
        c_items <- clusters[[c_idx]]

        # Comprobar conflicto biologico (Senior/Jugador Infantil)
        c_has_mayor <- any(sapply(c_items, function(x) isTRUE(x$has_mayor)), na.rm = TRUE) || isTRUE(t_row$has_mayor)
        c_has_menor <- any(sapply(c_items, function(x) isTRUE(x$has_menor)), na.rm = TRUE) || isTRUE(t_row$has_menor)
        bio_conflict <- c_has_mayor && c_has_menor

        # Por defecto, un gap <= 15 dias se interpreta como conflicto temporal.
        # Excepcion: en mercado (verano/invierno) se permite gap corto positivo
        # para no separar fichajes reales, salvo conflicto biologico.
        temporal_conflict <- FALSE
        for (ct in c_items) {
          gap_dias <- calcular_gap_intervalos(
            t_row$min_fecha,
            t_row$max_fecha,
            ct$min_fecha,
            ct$max_fecha
          )

          conflicto_temporal_ct <- is.finite(gap_dias) && gap_dias <= umbral_dias_conflicto
          es_gap_positivo <- is.finite(gap_dias) && gap_dias > 0

          inicio_tardio <- max(t_row$min_fecha, ct$min_fecha)
          fin_temprano <- min(t_row$max_fecha, ct$max_fecha)
          excepcion_mercado_ct <- es_gap_positivo && es_ventana_fichajes(fin_temprano, inicio_tardio)

          if (conflicto_temporal_ct && !(excepcion_mercado_ct && !bio_conflict)) {
            temporal_conflict <- TRUE
            break
          }
        }

        if (!temporal_conflict && !bio_conflict) {
          clusters[[c_idx]] <- append(clusters[[c_idx]], list(t_row))
          assigned <- TRUE
          break
        }
      }

      if (!assigned) {
        clusters <- append(clusters, list(list(t_row)))
      }
    }

    res <- bind_rows(lapply(seq_along(clusters), function(c_idx) {
      bind_rows(clusters[[c_idx]]) %>% mutate(cluster = c_idx)
    }))

    res$is_homonym <- length(clusters) > 1
    return(res)
})

id_mapping_heuristic <- bind_rows(list_res) %>%
  mutate(
    base_id = generar_id_seguro(nombre),
    final_id = if_else(is_homonym, paste0(base_id, "_", cluster), base_id)
  )

message(paste("   > ADE resolved", sum(id_mapping_heuristic$is_homonym)/2, "pairs of homonyms mathematically."))

# Paso 4: Devolver los IDs finales a las apariciones
apariciones_df_raw <- apariciones_df_raw %>%
  filter(!is.na(nombre) & nchar(trimws(nombre)) > 2) %>%
  left_join(id_mapping_heuristic %>% select(nombre, equipo, final_id), by = c("nombre", "equipo")) %>%
  mutate(id = if_else(!is.na(final_id), as.character(final_id), generar_id_seguro(nombre))) %>%
  select(-any_of("final_id"))

# Diccionario seguro de {nombre_jugador + equipo} a ID
id_mapping <- apariciones_df_raw %>%
  mutate(
    nombre_key = str_squish(as.character(nombre)),
    equipo_key = str_squish(as.character(equipo))
  ) %>%
  filter(!is.na(nombre_key), nombre_key != "", !is.na(equipo_key), equipo_key != "") %>%
  distinct(nombre_key, equipo_key, canonical_id = id)

id_mapping_dorsal <- apariciones_df_raw %>%
  mutate(equipo_key = str_squish(as.character(equipo))) %>%
  filter(!is.na(id_partido), !is.na(equipo_key), equipo_key != "", !is.na(dorsal)) %>%
  distinct(id_partido, equipo_key, dorsal, canonical_id_dorsal = id)

# Fallback 1: nombre dentro del partido (solo cuando no hay ambiguedad de homonimos en ese partido)
id_mapping_partido_nombre <- apariciones_df_raw %>%
  mutate(nombre_key = str_squish(as.character(nombre))) %>%
  filter(!is.na(id_partido), !is.na(nombre_key), nombre_key != "") %>%
  group_by(id_partido, nombre_key) %>%
  summarise(
    canonical_id_partido_nombre = if (n_distinct(id) == 1) as.character(first(id)) else NA_character_,
    .groups = "drop"
  ) %>%
  filter(!is.na(canonical_id_partido_nombre))

# Fallback 2: dorsal dentro del partido (solo cuando el dorsal identifica de forma unica)
id_mapping_partido_dorsal <- apariciones_df_raw %>%
  filter(!is.na(id_partido), !is.na(dorsal)) %>%
  group_by(id_partido, dorsal) %>%
  summarise(
    canonical_id_partido_dorsal = if (n_distinct(id) == 1) as.character(first(id)) else NA_character_,
    .groups = "drop"
  ) %>%
  filter(!is.na(canonical_id_partido_dorsal))

# 10.3.6. Aplicar ID mapping a las tablas de eventos
# Buscamos por nombre Y equipo para ser inmunes a los hom\u00f3nimos cruzados en un mismo partido.
if (exists("goles_df_unificado") && nrow(goles_df_unificado) > 0) {
  if (!"equipo_jugadora" %in% names(goles_df_unificado)) {
    goles_df_unificado$equipo_jugadora <- if ("equipo" %in% names(goles_df_unificado)) goles_df_unificado$equipo else NA_character_
  }
  if (!"dorsal" %in% names(goles_df_unificado)) {
    goles_df_unificado$dorsal <- NA_real_
  }

  goles_df_unificado <- goles_df_unificado %>%
    mutate(
      nombre_key = str_squish(as.character(jugadora)),
      equipo_key = str_squish(as.character(equipo_jugadora))
    ) %>%
    left_join(id_mapping, by = c("nombre_key", "equipo_key")) %>%
    left_join(id_mapping_dorsal, by = c("id_partido", "equipo_key", "dorsal")) %>%
    left_join(id_mapping_partido_nombre, by = c("id_partido", "nombre_key")) %>%
    left_join(id_mapping_partido_dorsal, by = c("id_partido", "dorsal")) %>%
    select(-any_of(c("id", "id_jugadora"))) %>%
    mutate(id = coalesce(canonical_id, canonical_id_dorsal, canonical_id_partido_nombre, canonical_id_partido_dorsal, generar_id_seguro(jugadora))) %>%
    select(-any_of(c("nombre_key", "equipo_key", "canonical_id", "canonical_id_dorsal", "canonical_id_partido_nombre", "canonical_id_partido_dorsal")))
}
if (exists("tarjetas_df_unificado") && nrow(tarjetas_df_unificado) > 0) {
  if (!"equipo" %in% names(tarjetas_df_unificado)) {
    tarjetas_df_unificado$equipo <- NA_character_
  }
  if (!"dorsal" %in% names(tarjetas_df_unificado)) {
    tarjetas_df_unificado$dorsal <- NA_real_
  }

  tarjetas_df_unificado <- tarjetas_df_unificado %>%
    mutate(
      nombre_key = str_squish(as.character(jugadora)),
      equipo_key = str_squish(as.character(equipo))
    ) %>%
    left_join(id_mapping, by = c("nombre_key", "equipo_key")) %>%
    left_join(id_mapping_dorsal, by = c("id_partido", "equipo_key", "dorsal")) %>%
    left_join(id_mapping_partido_nombre, by = c("id_partido", "nombre_key")) %>%
    left_join(id_mapping_partido_dorsal, by = c("id_partido", "dorsal")) %>%
    select(-any_of(c("id", "id_jugadora"))) %>%
    mutate(id = coalesce(canonical_id, canonical_id_dorsal, canonical_id_partido_nombre, canonical_id_partido_dorsal, generar_id_seguro(jugadora))) %>%
    select(-any_of(c("nombre_key", "equipo_key", "canonical_id", "canonical_id_dorsal", "canonical_id_partido_nombre", "canonical_id_partido_dorsal")))
}
if (exists("penales_df_unificado") && nrow(penales_df_unificado) > 0) {
  if (!"equipo" %in% names(penales_df_unificado)) {
    penales_df_unificado$equipo <- NA_character_
  }
  if (!"dorsal" %in% names(penales_df_unificado)) {
    penales_df_unificado$dorsal <- NA_real_
  }

  penales_df_unificado <- penales_df_unificado %>%
    mutate(
      nombre_key = str_squish(as.character(jugadora)),
      equipo_key = str_squish(as.character(equipo))
    ) %>%
    left_join(id_mapping, by = c("nombre_key", "equipo_key")) %>%
    left_join(id_mapping_dorsal, by = c("id_partido", "equipo_key", "dorsal")) %>%
    left_join(id_mapping_partido_nombre, by = c("id_partido", "nombre_key")) %>%
    left_join(id_mapping_partido_dorsal, by = c("id_partido", "dorsal")) %>%
    select(-any_of(c("id", "id_jugadora"))) %>%
    mutate(id = coalesce(canonical_id, canonical_id_dorsal, canonical_id_partido_nombre, canonical_id_partido_dorsal, generar_id_seguro(jugadora))) %>%
    select(-any_of(c("nombre_key", "equipo_key", "canonical_id", "canonical_id_dorsal", "canonical_id_partido_nombre", "canonical_id_partido_dorsal")))
}

minutos_df_raw <- map_dfr(resultados_exitosos, function(res) {
  if (is.null(res)) {
    return(NULL)
  }
  id_p <- res$partido_info$id_partido
  duracion <- (partidos_df %>% filter(id_partido == id_p) %>% pull(duracion_partido))[1]
  if (length(duracion) == 0 || is.na(duracion)) {
    duracion <- 90
  }
  calcular_minutos_equipo <- function(alineacion, cambios, duracion_partido) {
    if (is.null(alineacion) || nrow(alineacion) == 0) {
      return(NULL)
    }
    if (!"tipo" %in% names(alineacion)) {
      alineacion <- alineacion %>% mutate(tipo = "Desconocido")
    }
    jugadoras_con_minutos <- alineacion %>% 
      mutate(
        min_entra = if_else(tipo == "Titular", 0, NA_real_), 
        min_sale = if_else(tipo == "Titular", duracion_partido, 0),
        fue_sustituido = FALSE
      )
    if (!is.null(cambios) && nrow(cambios) > 0) {
      cambios_procesados <- cambios %>%
        mutate(d_entra = as.numeric(str_match(texto, "Entra .*?\\((\\d+)\\)")[, 2]), d_sale = as.numeric(str_match(texto, "por .*?\\((\\d+)\\)")[, 2])) %>%
        select(minuto, d_entra, d_sale) %>%
        filter(!is.na(d_entra) & !is.na(d_sale))
      for (i in 1:nrow(cambios_procesados)) {
        cambio <- cambios_procesados[i, ]
        jugadoras_con_minutos <- jugadoras_con_minutos %>% 
          mutate(
            min_sale = if_else(dorsal == cambio$d_sale, as.numeric(cambio$minuto), min_sale), 
            min_entra = if_else(dorsal == cambio$d_entra, as.numeric(cambio$minuto), min_entra),
            fue_sustituido = if_else(dorsal == cambio$d_sale, TRUE, fue_sustituido)
          )
      }
    }
    jugadoras_con_minutos %>%
      mutate(min_sale = if_else(!is.na(min_entra) & tipo == "Suplente" & min_sale == 0, duracion_partido, min_sale), minutos_jugados = if_else(is.na(min_entra), 0, min_sale - min_entra)) %>%
      mutate(minutos_jugados = pmax(0, pmin(duracion_partido, minutos_jugados)))
  }
  min_local <- calcular_minutos_equipo(res$alineacion_local, res$cambios_local, duracion)
  min_visitante <- calcular_minutos_equipo(res$alineacion_visitante, res$cambios_visitante, duracion)
  bind_rows(min_local, min_visitante) %>% mutate(id_partido = id_p)
})
# 10.3.7. Build apariciones_df with minutes and unified IDs
apariciones_con_minutos <- apariciones_df_raw %>%
  left_join(minutos_df_raw %>% select(id_partido, nombre, dorsal, tipo, min_entra, min_sale, minutos_jugados, fue_sustituido), by = c("id_partido", "nombre", "dorsal", "tipo"))

apariciones_df <- apariciones_con_minutos

apariciones_df <- apariciones_df %>%
  left_join(partidos_df %>% select(id_partido, competicion_nombre, competicion_temporada, es_cancelado), by = "id_partido") %>%
  select(id, id_partido, nombre, dorsal, tipo, equipo, es_portera, es_capitana, competicion_nombre, competicion_temporada, es_cancelado, everything())

if (!is.null(mapa_roles_forzados_df) && nrow(mapa_roles_forzados_df) > 0) {
  apariciones_df <- apariciones_df %>%
    left_join(mapa_roles_forzados_df, by = "id") %>%
    mutate(es_portera = if_else(!is.na(force_role) & force_role == "goalkeeper", TRUE, es_portera)) %>%
    select(-force_role)

  # Deduplicar: m\u00e1ximo una portera titular por equipo/partido.
  # La primera titular de la lista siempre es la portera real;
  # cualquier otra marcada como portera (por override) se desmarca.
  apariciones_df <- apariciones_df %>%
    group_by(id_partido, equipo) %>%
    mutate(
      es_portera = if_else(
        tipo == "Titular" & es_portera & cumsum(tipo == "Titular" & es_portera) > 1,
        FALSE,
        es_portera
      )
    ) %>%
    ungroup()

  # Suplentes del override: solo son porteras si:
  # (a) entraron en el mismo minuto que sali\u00f3 la portera titular (sustituci\u00f3n directa), O
  # (b) la portera titular fue expulsada y la suplente entr\u00f3 en los 10 min siguientes
  #     (entra por una de campo, pero para ocupar la porter\u00eda).
  # En cualquier otro caso, la suplente entr\u00f3 como jugadora de campo.
  porteras_titulares_info <- apariciones_df %>%
    filter(tipo == "Titular", es_portera == TRUE) %>%
    select(id_partido, equipo, id_gk = id, gk_min_sale = min_sale)

  # Minuto de la roja a la portera titular (si la hubo)
  rojas_a_porteras_titulares <- porteras_titulares_info %>%
    inner_join(
      tarjetas_df_unificado %>% filter(tipo == "Roja") %>% select(id_partido, id, minuto_roja = minuto),
      by = c("id_partido", "id_gk" = "id")
    ) %>%
    select(id_partido, equipo, minuto_roja)

  apariciones_df <- apariciones_df %>%
    left_join(porteras_titulares_info %>% select(id_partido, equipo, gk_min_sale),
      by = c("id_partido", "equipo")
    ) %>%
    left_join(rojas_a_porteras_titulares, by = c("id_partido", "equipo")) %>%
    mutate(
      .reemplaza_portera = !is.na(min_entra) & !is.na(gk_min_sale) & min_entra == gk_min_sale,
      # Roja a la titular Y la suplente entr\u00f3 dentro de los 10 min siguientes
      .entra_tras_roja = !is.na(min_entra) & !is.na(minuto_roja) & min_entra >= minuto_roja & min_entra <= minuto_roja + 10,
      es_portera = if_else(
        tipo == "Suplente" & es_portera & id %in% ids_forzadas &
          !is.na(gk_min_sale) & !.reemplaza_portera & !.entra_tras_roja,
        FALSE,
        es_portera
      )
    ) %>%
    select(-gk_min_sale, -minuto_roja, -.reemplaza_portera, -.entra_tras_roja)
}
message("   > Player appearances consolidated and IDs unified.")

### 10.4. Process and Translate Demographic Data
message("Step 10.4: Skipping player position datapath (not used for this deployment).")

# Simplificamos: no necesitamos el bloque de posiciones externas, lo dejamos vac\u00edo.
posiciones_procesadas_df <- tibble(
  id = character(),
  posicion_final_unificada = character(),
  nacionalidad = character(),
  fecha_nacimiento = as.Date(character()),
  ciudad_nacimiento = character()
)

### 10.5. Create Master Translation and Entity Dataframes
message("Step 10.5: Creating master translation and entity dataframes...")
nombres_equipos <- unique(c(partidos_df$local, partidos_df$visitante))
nombres_arbitros <- unique(arbitros_df$ime)
nombres_estadios <- unique(na.omit(estadios_df$estadio))
nombres_staff <- if (exists("staff_df") && nrow(staff_df) > 0) unique(staff_df$nombre) else character(0)
entidades_base_df <- tibble(original_name = c(nombres_equipos, nombres_arbitros, nombres_estadios, nombres_staff)) %>% distinct()

if (!is.null(mapa_nombres_entidades_long) || !is.null(mapa_nombres_jugadoras_long)) {
  entity_translations_wide <- bind_rows(mapa_nombres_entidades_long, mapa_nombres_jugadoras_long) %>%
    distinct(original_mk, lang, .keep_all = TRUE) %>%
    pivot_wider(id_cols = original_mk, names_from = lang, values_from = translated_name, names_prefix = "translated_name_")
  entidades_maestro_df <- entidades_base_df %>%
    left_join(entity_translations_wide, by = c("original_name" = "original_mk"))
} else {
  entidades_maestro_df <- entidades_base_df
}

map_transliteration_entity <- MAP_CYR_TO_LAT_DIACRITICS

entidades_maestro_df <- entidades_maestro_df %>% mutate(translated_name_mk = original_name)

for (lang_code in setdiff(IDIOMAS_SOPORTADOS, "mk")) {
  target_col <- paste0("translated_name_", lang_code)
  if (!target_col %in% names(entidades_maestro_df)) {
    entidades_maestro_df[[target_col]] <- NA_character_
  }
  entidades_maestro_df <- entidades_maestro_df %>%
    mutate(!!target_col := coalesce(
      .data[[target_col]],
      str_replace_all(tolower(original_name), map_transliteration_entity) %>% str_to_title()
    ))
}

if (!is.null(mapa_traducciones_paises_df) && nrow(mapa_traducciones_paises_df) > 0) {
  paises_para_unir <- mapa_traducciones_paises_df %>%
    rename(original_name = original_mk) %>%
    rename_with(~ paste0("country_", .), .cols = -original_name)
  entidades_maestro_df <- entidades_maestro_df %>%
    left_join(paises_para_unir, by = "original_name")
  for (lang_code in setdiff(IDIOMAS_SOPORTADOS, "mk")) {
    col_entidad <- paste0("translated_name_", lang_code)
    col_pais <- paste0("country_translation_", lang_code)
    if (col_entidad %in% names(entidades_maestro_df) && col_pais %in% names(entidades_maestro_df)) {
      entidades_maestro_df <- entidades_maestro_df %>%
        mutate(!!col_entidad := coalesce(.data[[col_pais]], .data[[col_entidad]]))
    }
  }
  entidades_maestro_df <- entidades_maestro_df %>% select(-starts_with("country_"))
  message("   > Country translations have been successfully integrated.")
}

# 10.5.1 Apply club latin corrections from clubs_latin.txt if available
if (exists("mapa_clubs_latin") && !is.null(mapa_clubs_latin) && nrow(mapa_clubs_latin) > 0) {
  clubes_map <- mapa_clubs_latin %>%
    mutate(map_key = tolower(str_squish(original))) %>%
    select(map_key, normalized)

  entidades_maestro_df <- entidades_maestro_df %>%
    mutate(entity_key = tolower(str_squish(original_name))) %>%
    left_join(clubes_map, by = c("entity_key" = "map_key")) %>%
    mutate(
      translated_name_en = coalesce(normalized, translated_name_en),
      translated_name_sq = coalesce(normalized, translated_name_sq),
      translated_name_es = coalesce(normalized, translated_name_es)
    ) %>%
    select(-normalized, -entity_key)

  message(paste("   > Club latin corrections applied for", nrow(mapa_clubs_latin), "teams (fuzzy key)."))
}

### 10.6. Identify and Order Competitions
message("Step 10.6: Identifying, translating, and sorting unique competitions...")
if (exists("partidos_df") && nrow(partidos_df) > 0) {
  max_real_season_numeric_raw <- partidos_df %>%
    filter(competicion_nombre != "\u0420\u0435\u043f\u0440\u0435\u0437\u0435\u043d\u0442\u0430\u0446\u0438\u0458\u0430") %>%
    distinct(competicion_temporada) %>%
    mutate(
      start_year = as.integer(str_extract(competicion_temporada, "^\\d{2,4}")),
      sort_year = if_else(nchar(as.character(start_year)) == 2, 2000 + start_year, start_year)
    ) %>%
    pull(sort_year)
  if (length(max_real_season_numeric_raw) == 0 || all(is.na(max_real_season_numeric_raw))) {
    max_real_season_numeric <- 2000
  } else {
    max_real_season_numeric <- max(max_real_season_numeric_raw, na.rm = TRUE)
  }
  if (is.infinite(max_real_season_numeric) || is.na(max_real_season_numeric)) max_real_season_numeric <- 2000

  competiciones_base_df <- partidos_df %>%
    filter(competicion_nombre != "\u0420\u0435\u043f\u0440\u0435\u0437\u0435\u043d\u0442\u0430\u0446\u0438\u0458\u0430") %>%
    distinct(competicion_nombre, competicion_temporada, categoria, division, vid_natprevaruvanje, min_age, max_age) %>%
    mutate(
      nombre_lower = tolower(competicion_nombre),
      start_year = as.integer(str_extract(competicion_temporada, "^\\d{2,4}")),
      sort_year = if_else(nchar(as.character(start_year)) == 2, 2000 + start_year, start_year)
    ) %>%
    mutate(
      importancia_score = case_when(
        str_detect(nombre_lower, "(?i)\u0441\u0443\u043f\u0435\u0440") ~ 1,
        str_detect(nombre_lower, "(?i)(\u043a\u0443\u043f|kup)") ~ 2,
        str_detect(nombre_lower, "(?i)\u043f\u0440\u0432\u0430") ~ 3,
        str_detect(nombre_lower, "(?i)\u0432\u0442\u043e\u0440\u0430") ~ 4,
        str_detect(nombre_lower, "(?i)\u0442\u0440\u0435\u0442\u0430") ~ 5,
        str_detect(nombre_lower, "(?i)\u043c\u043b\u0430\u0434\u0438\u043d\u0441\u043a") ~ 6,
        str_detect(nombre_lower, "(?i)\u043a\u0430\u0434\u0435\u0442\u0441\u043a") ~ 7,
        TRUE ~ 8
      ),
      baraz_modifier = if_else(str_detect(nombre_lower, "(?i)\u0431\u0430\u0440\u0430\u0436"), 0.5, 0),
      final_score = importancia_score + baraz_modifier
    )

  competiciones_combinadas_df <- competiciones_base_df %>%
    mutate(orden_primario = case_when(sort_year == max_real_season_numeric ~ 1, TRUE ~ 2))

  # === Apply competitions disambiguation: if a competitions_disambiguation entry exists that
  # === matches the competition name and season, try to assign the match to the proper group
  # === by checking the local/visitante team membership and append subset notation.
  if (exists("desambiguacion_competiciones_df") && !is.null(desambiguacion_competiciones_df) && nrow(desambiguacion_competiciones_df) > 0 && nrow(partidos_df) > 0) {
    message("Applying competition disambiguation rules to matches...")
    desamb_comp <- desambiguacion_competiciones_df %>%
      mutate(season = ifelse(is.na(season) | trimws(season) == "", NA_character_, season))

    cambios <- 0L
    for (i in seq_len(nrow(partidos_df))) {
      comp_raw <- partidos_df$competicion_nombre[i]
      temp <- partidos_df$competicion_temporada[i] %||% NA_character_
      if (is.na(comp_raw) || trimws(comp_raw) == "") next
      # Clean name: strip trailing season formats (YY/YY etc.) but preserve birth years (YYYY-YYYY) and parentheses.
      nombre_clean <- tolower(trimws(gsub("\\s*(?:\\d{4}/\\d{4}|\\d{2}/\\d{2}|\\d{4}/\\d{2})\\s*$", "", comp_raw)))
      possible <- desamb_comp %>% filter(original_clean == nombre_clean & (is.na(season) | season == temp))
      if (nrow(possible) == 0) next
      local_l <- tolower(trimws(partidos_df$local[i] %||% ""))
      visit_l <- tolower(trimws(partidos_df$visitante[i] %||% ""))
      if (local_l == "" && visit_l == "") next
      found_group <- NA_character_
      found_j <- NA_integer_
      for (j in seq_len(nrow(possible))) {
        teams_j <- possible$teams_list[[j]]
        if (is.null(teams_j)) next
        teams_j_norm <- tolower(trimws(teams_j))
        match_local <- (local_l != "" && local_l %in% teams_j_norm)
        match_visit <- (visit_l != "" && visit_l %in% teams_j_norm)
        if (match_local || match_visit) {
          if (is.na(found_group)) {
            found_group <- possible$group[j]
            found_j <- j
          } else {
            # prefer group that matches both teams
            if (match_local && match_visit) {
              found_group <- possible$group[j]
              found_j <- j
              break
            } else if (match_local && !(local_l %in% tolower(trimws(possible$teams_list[[found_j]])))) {
              # prefer local match over previous
              found_group <- possible$group[j]
              found_j <- j
            }
          }
        }
      }
      if (!is.na(found_group)) {
        ya_tiene_grupo <- stringr::str_detect(partidos_df$competicion_nombre[i], "\\([^)]*\\)\\s*$")
        if (!ya_tiene_grupo) {
          partidos_df$competicion_nombre[i] <- paste0(partidos_df$competicion_nombre[i], " (", found_group, ")")
          cambios <- cambios + 1L
        }
      }
    }
    message(paste("Competition disambiguation applied to", cambios, "matches."))
    # Recompute combined competitions after adjusting match competition names
    competiciones_base_df <- partidos_df %>%
      filter(competicion_nombre != "\u0420\u0435\u043f\u0440\u0435\u0437\u0435\u043d\u0442\u0430\u0446\u0438\u0458\u0430") %>%
      distinct(competicion_nombre, competicion_temporada, temporada_display, categoria, division, vid_natprevaruvanje, min_age, max_age) %>%
      mutate(
        nombre_lower = tolower(competicion_nombre),
        start_year = as.integer(str_extract(competicion_temporada, "^\\d{2,4}")),
        sort_year = if_else(nchar(as.character(start_year)) == 2, 2000 + start_year, start_year)
      ) %>%
      mutate(
        importancia_score = case_when(
          str_detect(nombre_lower, "(?i)\\u0441\\u0443\\u043f\\u0435\\u0440") ~ 1,
          str_detect(nombre_lower, "(?i)(\\u043a\\u0443\\u043f|kup)") ~ 2,
          str_detect(nombre_lower, "(?i)\\u043f\\u0440\\u0432\\u0430") ~ 3,
          str_detect(nombre_lower, "(?i)\\u0432\\u0442\\u043e\\u0440\\u0430") ~ 4,
          str_detect(nombre_lower, "(?i)\\u0442\\u0440\\u0435\\u0442\\u0430") ~ 5,
          str_detect(nombre_lower, "(?i)\\u043c\\u043b\\u0430\\u0434\\u0438\\u043d\\u0441\\u043a") ~ 6,
          str_detect(nombre_lower, "(?i)\\u043a\\u0430\\u0434\\u0435\\u0442\\u0441\\u043a") ~ 7,
          TRUE ~ 8
        ),
        baraz_modifier = if_else(str_detect(nombre_lower, "(?i)\\u0431\\u0430\\u0440\\u0430\\u0436"), 0.5, 0),
        final_score = importancia_score + baraz_modifier
      )
    competiciones_combinadas_df <- competiciones_base_df %>%
      mutate(orden_primario = case_when(sort_year == max_real_season_numeric ~ 1, TRUE ~ 2))
  }

  # Keep all downstream datasets aligned with the final competition name used in matches.
  # This avoids mismatches in player/team/stadium profiles when disambiguation appends groups.
  sincronizar_contexto_competicion <- function(df) {
    if (is.null(df) || nrow(df) == 0 || !"id_partido" %in% names(df)) {
      return(df)
    }

    df %>%
      select(-any_of(c("competicion_nombre", "competicion_temporada"))) %>%
      left_join(
        partidos_df %>% select(id_partido, competicion_nombre, competicion_temporada),
        by = "id_partido"
      )
  }

  if (exists("apariciones_df") && !is.null(apariciones_df) && nrow(apariciones_df) > 0) {
    apariciones_df <- sincronizar_contexto_competicion(apariciones_df)
  }

  if (exists("estadios_df") && !is.null(estadios_df) && nrow(estadios_df) > 0) {
    estadios_df <- sincronizar_contexto_competicion(estadios_df)
  }

  if (!is.null(mapa_nombres_competiciones_long)) {
    comp_translations_wide <- mapa_nombres_competiciones_long %>%
      # Ensure dictionary keys are cleaned using the SAME logic (strip season suffix AND group parentheses)
      mutate(original_mk_clean = stringr::str_to_lower(trimws(gsub("\\s*\\([^)]*\\)\\s*$", "", gsub("\\s*(?:\\d{4}/\\d{4}|\\d{2}/\\d{2}|\\d{4}/\\d{2})\\s*$", "", original_mk))))) %>% 
      distinct(original_mk_clean, lang, .keep_all = TRUE) %>%
      pivot_wider(id_cols = original_mk_clean, names_from = lang, values_from = translated_name, names_prefix = "base_name_")

    comp_translations_wide_vid <- mapa_nombres_competiciones_long %>%
      mutate(original_mk_clean = stringr::str_to_lower(trimws(gsub("\\s*\\([^)]*\\)\\s*$", "", gsub("\\s*(?:\\d{4}/\\d{4}|\\d{2}/\\d{2}|\\d{4}/\\d{2})\\s*$", "", original_mk))))) %>% 
      distinct(original_mk_clean, lang, .keep_all = TRUE) %>%
      pivot_wider(id_cols = original_mk_clean, names_from = lang, values_from = translated_name, names_prefix = "vid_name_")

    competiciones_unicas_df <- competiciones_combinadas_df %>%
      mutate(
        # Same cleaning logic as dictionary building: strip season slash-formats AND group label parentheses
        competicion_nombre_clean = stringr::str_to_lower(trimws(gsub("\\s*\\([^)]*\\)\\s*$", "", gsub("\\s*(?:\\d{4}/\\d{4}|\\d{2}/\\d{2}|\\d{4}/\\d{2})\\s*$", "", competicion_nombre)))),
        vid_clean = stringr::str_to_lower(trimws(vid_natprevaruvanje)),
        group_label = stringr::str_match(competicion_nombre, "\\(([^)]*)\\)\\s*$")[,2]
      ) %>%
      left_join(comp_translations_wide, by = c("competicion_nombre_clean" = "original_mk_clean")) %>%
      left_join(comp_translations_wide_vid, by = c("vid_clean" = "original_mk_clean")) %>%
      mutate(
        name_for_id = if_else(
          !is.na(base_name_mk) & !is.na(group_label) & nzchar(group_label),
          paste0(base_name_mk, " (", group_label, ")"),
          coalesce(base_name_mk, competicion_nombre)
        ),
        competicion_id = generar_id_seguro(paste(name_for_id, coalesce(temporada_display, competicion_temporada)))
      ) %>%
      select(-name_for_id, -group_label)
  } else {
    competiciones_unicas_df <- competiciones_combinadas_df
  }

  # Ensure expected translation helper columns exist even when no translation files are loaded.
  for (lang_code in IDIOMAS_SOPORTADOS) {
    base_col <- paste0("base_name_", lang_code)
    vid_col <- paste0("vid_name_", lang_code)
    if (!base_col %in% names(competiciones_unicas_df)) competiciones_unicas_df[[base_col]] <- NA_character_
    if (!vid_col %in% names(competiciones_unicas_df)) competiciones_unicas_df[[vid_col]] <- NA_character_
  }
  if (!"competicion_id" %in% names(competiciones_unicas_df)) {
    competiciones_unicas_df <- competiciones_unicas_df %>%
      mutate(competicion_id = generar_id_seguro(paste(competicion_nombre, competicion_temporada)))
  }


  # Build full names and group names for all languages
  for (lang_code in IDIOMAS_SOPORTADOS) {
    target_col <- paste0("nombre_completo_", lang_code)
    base_col <- paste0("base_name_", lang_code)
    vid_target_col <- paste0("vid_natprevaruvanje_", lang_code)
    vid_col <- paste0("vid_name_", lang_code)

    if (lang_code == "mk") {
      # Use corrected base name if available, otherwise original. Preserve group suffix if present.
      competiciones_unicas_df <- competiciones_unicas_df %>%
        mutate(
          .group_label = stringr::str_match(competicion_nombre, "\\(([^)]*)\\)\\s*$")[,2],
          .display_season = coalesce(temporada_display, competicion_temporada),
          !!target_col := if_else(competicion_nombre == "\u0420\u0435\u043f\u0440\u0435\u0437\u0435\u043d\u0442\u0430\u0446\u0438\u0458\u0430",
            "\u0420\u0435\u043f\u0440\u0435\u0437\u0435\u043d\u0442\u0430\u0446\u0438\u0458\u0430",
            paste0(coalesce(base_name_mk, competicion_nombre), if_else(!is.na(.group_label) & nzchar(.group_label), paste0(" (", .group_label, ")"), ""), " ", .display_season)
          )
        )
      
      # FIX FOR SHOUTING NAMES: If after all translations, the name is still mostly uppercase,
      # force it to Title Case but preserve acronyms like OFL, FFM, etc.
      if (lang_code == "mk") {
        competiciones_unicas_df <- competiciones_unicas_df %>%
          mutate(!!target_col := sapply(!!sym(target_col), function(name) {
            if (is.na(name)) return(name)
            if (stringr::str_detect(name, "[\u0410-\u042f]{3,}")) {
               words <- str_split(name, "\\s+")[[1]]
               new_words <- sapply(words, function(w) {
                  acronyms <- c("\u041e\u0424\u041b", "\u0424\u0424\u041c", "\u041c\u0424\u041b", "\u041a\u0423\u041f", "\u0420\u041c", "\u041e\u0424\u0421")
                  lowercase_words <- c("\u0437\u0430\u043f\u0430\u0434", "\u0458\u0443\u0433", "\u0441\u0435\u0432\u0435\u0440", "\u0438\u0441\u0442\u043e\u043a", "\u0458\u0443\u0433\u043e\u0437\u0430\u043f\u0430\u0434", "\u0458\u0443\u0433\u043e\u0438\u0441\u0442\u043e\u043a", "\u0437\u0430", "\u043d\u0430", "\u0438")
                  w_clean <- tolower(stringr::str_replace_all(w, "[^\u0410-\u042f\u0430-\u044fA-Za-z0-9]", ""))
                  if (toupper(w_clean) %in% acronyms) return(toupper(w))
                  if (w_clean %in% lowercase_words) return(tolower(w))
                  return(stringr::str_to_title(tolower(w)))
                })
               return(paste(new_words, collapse = " "))
            } else {
               return(name)
            }
          }))
      }

      competiciones_unicas_df <- competiciones_unicas_df %>%
        mutate(!!vid_target_col := if_else(!is.na(vid_natprevaruvanje),
          coalesce(vid_name_mk, vid_natprevaruvanje),
          vid_natprevaruvanje
        )) %>%
        select(-.group_label)
    } else {
      if (!base_col %in% names(competiciones_unicas_df)) {
        # If no translation file loaded, fallback to NA (transliteration happens below)
        competiciones_unicas_df[[target_col]] <- NA_character_
        competiciones_unicas_df[[vid_target_col]] <- NA_character_
      } else {
        competiciones_unicas_df <- competiciones_unicas_df %>%
          mutate(
            .group_label = stringr::str_match(competicion_nombre, "\\(([^)]*)\\)\\s*$")[,2],
            .group_label_lat = if_else(!is.na(.group_label), stringr::str_replace_all(stringr::str_to_lower(.group_label), MAP_CYR_TO_LAT_DIACRITICS) %>% stringr::str_to_upper(), NA_character_),
            .display_season = coalesce(temporada_display, competicion_temporada)
          ) %>%
          mutate(!!target_col := if_else(!is.na(.data[[base_col]]),
            paste0(.data[[base_col]], if_else(!is.na(.group_label_lat) & nzchar(.group_label_lat), paste0(" (", .group_label_lat, ")"), ""), " ", .display_season),
            NA_character_
          ))

        competiciones_unicas_df <- competiciones_unicas_df %>%
          mutate(!!vid_target_col := if_else(!is.na(vid_natprevaruvanje),
            coalesce(.data[[vid_col]], vid_natprevaruvanje),
            vid_natprevaruvanje
          )) %>%
          select(-.group_label) # Default to MK vid if no translation
      }
    }
  }
  map_transliteration_comp <- MAP_CYR_TO_LAT_DIACRITICS
  for (lang_code in setdiff(IDIOMAS_SOPORTADOS, "mk")) {
    target_col <- paste0("nombre_completo_", lang_code)
    if (!target_col %in% names(competiciones_unicas_df)) competiciones_unicas_df[[target_col]] <- NA_character_
  }
  for (lang_code in setdiff(IDIOMAS_SOPORTADOS, "mk")) {
    target_col <- paste0("nombre_completo_", lang_code)
    competiciones_unicas_df <- competiciones_unicas_df %>%
      mutate(!!target_col := case_when(
        !is.na(.data[[target_col]]) ~ .data[[target_col]],
        TRUE ~ str_to_title(str_replace_all(tolower(nombre_completo_mk), map_transliteration_comp))
      ))
  }
  competiciones_unicas_df <- competiciones_unicas_df %>% arrange(orden_primario, final_score, desc(competicion_temporada), nombre_completo_mk)

  # Calculate nesting (anidamiento)
  if (nrow(competiciones_unicas_df) > 0) {
    counts_vid <- competiciones_unicas_df %>%
      filter(!is.na(vid_natprevaruvanje)) %>%
      group_by(competicion_temporada, vid_natprevaruvanje) %>%
      summarise(n_in_group = n(), .groups = "drop")

    competiciones_unicas_df <- competiciones_unicas_df %>%
      left_join(counts_vid, by = c("competicion_temporada", "vid_natprevaruvanje")) %>%
      mutate(
        es_anidada = !is.na(vid_natprevaruvanje) & n_in_group > 1
      )
  }
} else {
  competiciones_unicas_df <- tibble()
}

### 10.7. Determine Scope of Changes for Incremental Generation
message("Step 10.7: Checking for changes for incremental generation using build log...")

# Load modified match IDs from extraerinfopdf.R (PDFs whose content hash changed).
ruta_cache_info <- "cache_info.rds"
partidos_modificados_ids <- character(0)
if (file.exists(ruta_cache_info)) {
  info_cambios <- readRDS(ruta_cache_info)
  if (!is.null(info_cambios$partidos_modificados_ids)) {
    partidos_modificados_ids <- info_cambios$partidos_modificados_ids
  }
}
if (length(partidos_modificados_ids) > 0) {
  message(paste("   >", length(partidos_modificados_ids), "modified match(es) detected from content hash changes:", paste(partidos_modificados_ids, collapse = ", ")))
}

# Check for modified official results to trigger incremental builds
ruta_cache_oficiales <- "official_results_cache.rds"
official_results_previos <- if (file.exists(ruta_cache_oficiales)) readRDS(ruta_cache_oficiales) else tibble(id_partido = character(), goles_local = numeric(), goles_visitante = numeric())

if (exists("official_results_df")) {
  cambiados <- anti_join(official_results_df, official_results_previos, by = c("id_partido", "goles_local", "goles_visitante"))
  if (nrow(cambiados) > 0) {
    partidos_modificados_ids <- unique(c(partidos_modificados_ids, cambiados$id_partido))
  }
  revertidos <- anti_join(official_results_previos, official_results_df, by = "id_partido")
  if (nrow(revertidos) > 0) {
    partidos_modificados_ids <- unique(c(partidos_modificados_ids, revertidos$id_partido))
  }
  saveRDS(official_results_df, ruta_cache_oficiales)
}

# >>> INICIO DEL C\u00d3DIGO A\u00d1ADIDO (Paso 1) <<<
# Cargar el historial de competiciones para detectar ligas completamente nuevas
ruta_comp_log <- "competition_log.rds"
competiciones_previas_ids <- if (file.exists(ruta_comp_log)) readRDS(ruta_comp_log) else character(0)
competiciones_actuales_ids <- competiciones_unicas_df$competicion_id
competiciones_nuevas_ids <- setdiff(competiciones_actuales_ids, competiciones_previas_ids)

# Cargar caché de cancelados previos (IDs y Reglas)
ruta_cancelados_cache <- "cancelled_matches_cache.rds"
cancelados_cache_previo <- if (file.exists(ruta_cancelados_cache)) readRDS(ruta_cancelados_cache) else list(ids = character(0), rules = tibble())

# Extraer componentes (manejo robusto si el cache antiguo solo tenía un vector de IDs)
if (is.list(cancelados_cache_previo) && "ids" %in% names(cancelados_cache_previo)) {
  cancelled_previos_ids <- cancelados_cache_previo$ids
  cancelled_previas_rules <- if ("rules" %in% names(cancelados_cache_previo)) cancelados_cache_previo$rules else tibble()
} else {
  cancelled_previos_ids <- cancelados_cache_previo
  cancelled_previas_rules <- tibble()
}

new_cancelled_ids <- setdiff(cancelled_matches_ids, cancelled_previos_ids)
rules_changed <- !identical(cancelled_matches_rules, cancelled_previas_rules)
# >>> FIN DEL CÓDIGO AÑADIDO (Paso 1) <<<

ruta_build_log <- "build_log.rds"
partidos_previamente_construidos_ids <- if (file.exists(ruta_build_log)) readRDS(ruta_build_log) else character(0)
partidos_actuales_ids <- partidos_df %>%
  filter(!is.na(id_partido)) %>%
  pull(id_partido) %>%
  unique()
partidos_nuevos_ids <- setdiff(partidos_actuales_ids, partidos_previamente_construidos_ids)
partidos_eliminados_ids <- setdiff(partidos_previamente_construidos_ids, partidos_actuales_ids)
full_rebuild_needed <- FALSE
if (length(partidos_eliminados_ids) > 0) {
  message(paste("   > WARNING:", length(partidos_eliminados_ids), "deleted reports detected. Full rebuild initiated."))
  full_rebuild_needed <- TRUE
  affected_match_ids <- partidos_actuales_ids
} else if (length(partidos_nuevos_ids) > 0) {
  message(paste("   >", length(partidos_nuevos_ids), "new reports detected. Incremental update will be performed."))
  affected_match_ids <- partidos_nuevos_ids
} else {
  message("   > No changes detected from new/deleted reports.")
  affected_match_ids <- character(0)
}

# Incorporate modified matches (content hash changes) into the affected scope.
if (length(partidos_modificados_ids) > 0) {
  affected_match_ids <- unique(c(affected_match_ids, partidos_modificados_ids))
  message(paste("   >", length(partidos_modificados_ids), "modified match(es) added to incremental update scope."))
}

hubo_cambios <- length(affected_match_ids) > 0 || length(partidos_eliminados_ids) > 0 || length(new_cancelled_ids) > 0 || rules_changed

affected_competition_ids <- character(0)
affected_player_ids <- character(0)
affected_team_ids <- character(0)
affected_referee_ids <- character(0)
affected_stadium_ids <- character(0)
affected_staff_ids <- character(0)

# If any match was just cancelled (by ID or because rules changed), we MUST regenerate its competition hub
if (length(new_cancelled_ids) > 0 || rules_changed) {
  ids_a_revisar <- new_cancelled_ids
  if (rules_changed) {
    # Si las reglas han cambiado, regeneramos las competiciones que tengan partidos cancelados
    ids_a_revisar <- unique(c(ids_a_revisar, partidos_df %>% filter(es_cancelado) %>% pull(id_partido)))
  }
  
  comps_to_regen <- partidos_df %>%
    filter(id_partido %in% ids_a_revisar) %>%
    distinct(competicion_nombre, competicion_temporada) %>%
    left_join(competiciones_unicas_df %>% select(-any_of("categoria")), by = c("competicion_nombre", "competicion_temporada")) %>%
    pull(competicion_id)

  affected_competition_ids <- unique(c(affected_competition_ids, comps_to_regen))
  message(paste("   >", length(new_cancelled_ids), "newly cancelled match(es) or rules change detected. Competing hubs will be updated."))
}

if (hubo_cambios) {
  if (length(affected_match_ids) > 0) {
    partidos_afectados_df <- partidos_df %>%
      filter(id_partido %in% affected_match_ids) %>%
        left_join(competiciones_unicas_df %>% select(-any_of("categoria")), by = c("competicion_nombre", "competicion_temporada"))
    if (nrow(partidos_afectados_df) > 0) {
      affected_competition_ids <- unique(c(affected_competition_ids, na.omit(partidos_afectados_df$competicion_id)))
      affected_team_ids <- unique(c(affected_team_ids, generar_id_seguro(partidos_afectados_df$local), generar_id_seguro(partidos_afectados_df$visitante)))
    }
    jugadoras_afectadas_df <- apariciones_df %>% filter(id_partido %in% affected_match_ids)
    if (nrow(jugadoras_afectadas_df) > 0) affected_player_ids <- unique(c(affected_player_ids, na.omit(jugadoras_afectadas_df$id)))
    arbitros_afectados_df <- arbitros_df %>% filter(id_partido %in% affected_match_ids)
    if (nrow(arbitros_afectados_df) > 0) affected_referee_ids <- unique(c(affected_referee_ids, generar_id_seguro(na.omit(arbitros_afectados_df$ime))))
    estadios_afectados_df <- estadios_df %>% filter(id_partido %in% affected_match_ids)
    if (nrow(estadios_afectados_df) > 0) affected_stadium_ids <- unique(c(affected_stadium_ids, generar_id_seguro(na.omit(estadios_afectados_df$estadio))))
    if (exists("staff_df") && nrow(staff_df) > 0) {
      staff_afectados <- staff_df %>% filter(id_partido %in% affected_match_ids)
      if (nrow(staff_afectados) > 0) affected_staff_ids <- unique(generar_id_seguro(na.omit(staff_afectados$nombre)))
    }
  }
  # Forzar la actualizaci\u00f3n si hay competiciones enteras que son nuevas
  if (length(competiciones_nuevas_ids) > 0) {
    message(paste("   >", length(competiciones_nuevas_ids), "new competitions detected. Forcing their regeneration."))
    affected_competition_ids <- unique(c(affected_competition_ids, competiciones_nuevas_ids))
    # Forzamos 'hubo_cambios' a TRUE para asegurar que la generaci\u00f3n se ejecute
    hubo_cambios <- TRUE
  }
  message(paste("   > Identified", length(affected_competition_ids), "competitions,", length(affected_match_ids), "matches, and", length(affected_player_ids), "players for update."))
}

### 10.X. Load club sanctions (point deductions)
ruta_sanciones_clubes <- "sanctions.txt"
if (file.exists(ruta_sanciones_clubes)) {
  sanciones_clubes_df <- read.csv(ruta_sanciones_clubes, stringsAsFactors = FALSE, encoding = "UTF-8", 
                                  colClasses = c("equipo" = "character", "competicion_nombre" = "character", 
                                                 "competicion_temporada" = "character", "puntos_deducidos" = "numeric"))
  # Defensive cast to handle any NAs introduced by coercion if data was shifted
  sanciones_clubes_df <- sanciones_clubes_df %>%
    mutate(
      equipo = as.character(equipo),
      competicion_nombre = as.character(competicion_nombre),
      competicion_temporada = as.character(competicion_temporada),
      puntos_deducidos = as.numeric(puntos_deducidos)
    )
  message(paste("   > Loaded", nrow(sanciones_clubes_df), "club sanctions from", ruta_sanciones_clubes))
} else {
  sanciones_clubes_df <- tibble(
    competicion_nombre = character(), competicion_temporada = character(),
    equipo = character(), puntos_deducidos = numeric(), motivo = character()
  )
}

# Resolve "corrected" (translated/display) competition names to internal names.
# This lets users write e.g. "Прва лига" (from competitions.txt column B) instead of the raw "Прва МФЛ".
if (nrow(sanciones_clubes_df) > 0 && exists("competiciones_unicas_df") && nrow(competiciones_unicas_df) > 0) {
  comp_name_lookup <- bind_rows(
    competiciones_unicas_df %>% transmute(name_variant = base_name_mk,   canonical_name = competicion_nombre),
    competiciones_unicas_df %>% transmute(name_variant = base_name_sq,   canonical_name = competicion_nombre),
    competiciones_unicas_df %>% transmute(name_variant = base_name_es,   canonical_name = competicion_nombre),
    competiciones_unicas_df %>% transmute(name_variant = base_name_en,   canonical_name = competicion_nombre)
  ) %>%
    filter(!is.na(name_variant)) %>%
    distinct(name_variant, canonical_name)
  n_resolved <- sanciones_clubes_df %>%
    inner_join(comp_name_lookup, by = c("competicion_nombre" = "name_variant")) %>%
    nrow()
  sanciones_clubes_df <- sanciones_clubes_df %>%
    left_join(comp_name_lookup, by = c("competicion_nombre" = "name_variant")) %>%
    mutate(competicion_nombre = coalesce(canonical_name, competicion_nombre)) %>%
    select(-canonical_name)
  if (n_resolved > 0) message(paste("   > Resolved", n_resolved, "competition names via translation lookup."))
}

# 10.9. Save Cancelled Matches Cache (IDs and Rules)
if (exists("cancelled_matches_ids") && exists("cancelled_matches_rules")) {
  saveRDS(list(ids = cancelled_matches_ids, rules = cancelled_matches_rules), "cancelled_matches_cache.rds")
}
