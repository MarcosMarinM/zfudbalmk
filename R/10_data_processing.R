#### 10. MAIN DATA PROCESSING AND TRANSFORMATION (VERSIÓN REORDENADA Y CORREGIDA) ####

### 10.0. Load, Unify, and Cleanse All Raw Data
message("Step 10.0: Loading, unifying, and cleansing all raw data sources...")

# 10.0.1. Load from caches
ruta_cache_pdf <- "actas_cache.rds"
pdf_results <- if (file.exists(ruta_cache_pdf)) readRDS(ruta_cache_pdf) else list()
ruta_cache_web <- "web_cache.rds"
web_results <- if (file.exists(ruta_cache_web)) readRDS(ruta_cache_web) else list()
resultados_exitosos <- c(pdf_results, web_results)
message(paste("   > Total unified reports to process:", length(resultados_exitosos)))

# 10.0.2. Standardize all team and stadium names to Cyrillic
message("   > Standardizing all names to Cyrillic...")
is_latin <- function(text) {
  if (is.na(text) || nchar(text) == 0) {
    return(FALSE)
  }
  chars <- strsplit(text, "")[[1]]
  alpha_chars <- chars[grepl("[A-Za-z]", chars)]
  if (length(alpha_chars) == 0) {
    return(FALSE)
  }
  return(sum(grepl("^[A-Za-z]$", alpha_chars)) / length(alpha_chars) > 0.5)
}

latin_to_cyrillic <- function(nombres) {
  mapa_digrafos <- c("zh" = "ж", "sh" = "ш", "ch" = "ч", "nj" = "њ", "lj" = "љ", "kj" = "ќ", "gj" = "ѓ", "dz" = "ѕ", "dj" = "џ")
  mapa_simple <- c("a" = "а", "b" = "б", "c" = "ц", "d" = "д", "e" = "е", "f" = "ф", "g" = "г", "h" = "х", "i" = "и", "j" = "ј", "k" = "к", "l" = "л", "m" = "м", "n" = "н", "o" = "о", "p" = "п", "r" = "р", "s" = "с", "t" = "т", "u" = "у", "v" = "в", "z" = "з", "w" = "в", "q" = "к", "x" = "кс", "y" = "ј")
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
  return(res)
})

# 10.0.3. Apply master ID unification (NEW LOGIC: based on incorrect name)
if (!is.null(mapa_unificacion_id_df) && nrow(mapa_unificacion_id_df) > 0) {
  message("   > Applying master ID unification rules (by name)...")

  # Asegurar que el nombre canónico en el mapa también esté en el formato correcto.
  mapa_unificacion_id_df$nombre_canonico <- reordenar_nombre_idempotente(mapa_unificacion_id_df$nombre_canonico)

  # Nueva función que unifica usando un left_join por el nombre.
  aplicar_unificacion_por_nombre <- function(df, mapa, col_nombre_df, col_id_df) {
    # Si el dataframe es nulo, está vacío o no tiene las columnas necesarias, devolverlo sin cambios.
    if (is.null(df) || nrow(df) == 0 || !col_nombre_df %in% names(df) || !col_id_df %in% names(df)) {
      return(df)
    }

    # Preparar el mapa para el join, renombrando 'nombre_incorrecto' para que coincida con la columna del df.
    mapa_para_join <- mapa %>%
      rename(!!col_nombre_df := nombre_incorrecto)

    df %>%
      left_join(mapa_para_join, by = col_nombre_df) %>%
      mutate(
        # Reemplazar el nombre y el ID originales por los canónicos solo si se encontró una coincidencia.
        # coalesce() elige el primer valor no-NA.
        !!col_nombre_df := coalesce(nombre_canonico, .data[[col_nombre_df]]),
        !!col_id_df := coalesce(id_canonico, .data[[col_id_df]])
      ) %>%
      # Eliminar las columnas auxiliares que se añadieron desde el mapa.
      select(-any_of(c("id_canonico", "nombre_canonico")))
  }

  # Aplicar la nueva función de unificación a todas las partes relevantes de los datos.
  resultados_exitosos <- map(resultados_exitosos, function(res) {
    if (is.null(res)) {
      return(NULL)
    }

    # Estandarizar nombres de columnas de ID si es necesario (ej. 'id_jugadora' -> 'id')
    if ("id_jugadora" %in% names(res$goles)) res$goles <- rename(res$goles, id = id_jugadora)
    if ("id_jugadora" %in% names(res$tarjetas)) res$tarjetas <- rename(res$tarjetas, id = id_jugadora)
    if ("id_jugadora" %in% names(res$penales)) res$penales <- rename(res$penales, id = id_jugadora)

    # Aplicar la unificación por nombre a cada dataframe
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
      res$partido_info <- aplicar_conversiones(res$partido_info, c("local", "visitante"), mapa_conversiones_df)
      if (nrow(res$alineacion_local) > 0) res$alineacion_local <- aplicar_conversiones(res$alineacion_local, "nombre", mapa_conversiones_df)
      if (nrow(res$alineacion_visitante) > 0) res$alineacion_visitante <- aplicar_conversiones(res$alineacion_visitante, "nombre", mapa_conversiones_df)

      # CORRECCIÓN DEFINITIVA PARA LA CRONOLOGÍA: Aplicar a los dataframes de eventos AHORA.
      if (nrow(res$goles) > 0) res$goles <- aplicar_conversiones(res$goles, c("jugadora", "equipo_jugadora", "equipo_acreditado"), mapa_conversiones_df)
      if (nrow(res$tarjetas) > 0) res$tarjetas <- aplicar_conversiones(res$tarjetas, c("jugadora", "equipo"), mapa_conversiones_df)
      if (!is.null(res$penales) && nrow(res$penales) > 0) res$penales <- aplicar_conversiones(res$penales, c("jugadora", "equipo"), mapa_conversiones_df)

      # Vectores de texto
      res$estadio <- aplicar_conversiones(res$estadio, mapa_df = mapa_conversiones_df)
      res$arbitro_principal_nombre <- aplicar_conversiones(res$arbitro_principal_nombre, mapa_df = mapa_conversiones_df)
      res$arbitro_asist_1_nombre <- aplicar_conversiones(res$arbitro_asist_1_nombre, mapa_df = mapa_conversiones_df)
      res$arbitro_asist_2_nombre <- aplicar_conversiones(res$arbitro_asist_2_nombre, mapa_df = mapa_conversiones_df)
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

    if (nrow(res$alineacion_local) > 0) res$alineacion_local$nombre <- reordenar_nombre_simple(res$alineacion_local$nombre)
    if (nrow(res$alineacion_visitante) > 0) res$alineacion_visitante$nombre <- reordenar_nombre_simple(res$alineacion_visitante$nombre)
    if (nrow(res$goles) > 0) res$goles$jugadora <- reordenar_nombre_simple(res$goles$jugadora)
    if (nrow(res$tarjetas) > 0) res$tarjetas$jugadora <- reordenar_nombre_simple(res$tarjetas$jugadora)
    if (!is.null(res$penales) && nrow(res$penales) > 0) res$penales$jugadora <- reordenar_nombre_simple(res$penales$jugadora)

    res$arbitro_principal_nombre <- reordenar_nombre_simple(res$arbitro_principal_nombre)
    res$arbitro_asist_1_nombre <- reordenar_nombre_simple(res$arbitro_asist_1_nombre)
    res$arbitro_asist_2_nombre <- reordenar_nombre_simple(res$arbitro_asist_2_nombre)

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
message("Step 10.0: Raw data cleansing complete.")

### 10.1. Create Master Dataframes from Cleaned Source
message("Step 10.1: Creating master dataframes from the clean source...")

# 10.1.1. Create master matches dataframe and merge with future calendars
partidos_df_reales <- map_dfr(resultados_exitosos, "partido_info")
partidos_df_placeholders <- cargar_calendarios_excel()
if (!is.null(mapa_conversiones_df)) {
  partidos_df_placeholders <- aplicar_conversiones(partidos_df_placeholders, c("local", "visitante"), mapa_conversiones_df)
}
if (nrow(partidos_df_placeholders) > 0 && nrow(partidos_df_reales) > 0) {
  partidos_df_reales <- partidos_df_reales %>% mutate(match_key = paste(local, visitante, competicion_nombre, competicion_temporada, jornada))
  partidos_df_placeholders <- partidos_df_placeholders %>% mutate(match_key = paste(local, visitante, competicion_nombre, competicion_temporada, jornada))
  placeholders_a_mantener <- partidos_df_placeholders %>% anti_join(partidos_df_reales, by = "match_key")
  partidos_df <- bind_rows(partidos_df_reales %>% select(-match_key), placeholders_a_mantener %>% select(-match_key))
} else if (nrow(partidos_df_placeholders) > 0) {
  partidos_df <- partidos_df_placeholders
} else {
  partidos_df <- partidos_df_reales
}
message("   > Master `partidos_df` created.")

# 10.1.2. Create other master dataframes
goles_df_unificado <- map_dfr(resultados_exitosos, "goles")
tarjetas_df_unificado <- map_dfr(resultados_exitosos, "tarjetas")
penales_df_unificado <- map_dfr(resultados_exitosos, "penales")
arbitros_df <- map_dfr(resultados_exitosos, function(res) {
  if (is.null(res) || is.null(res$partido_info)) {
    return(NULL)
  }

  df_arbitros_partido <- tibble(
    id_partido = character(),
    ime = character(),
    ciudad = character(),
    uloga = character()
  )

  id_p <- res$partido_info$id_partido

  # Función auxiliar para extraer de forma segura y LIMPIAR el nombre del árbitro.
  extraer_info_arbitro <- function(obj) {
    if (is.null(obj)) {
      return(list(nombre = NULL, ciudad = NULL))
    }

    nombre_raw <- if (is.list(obj)) obj$nombre %||% NULL else as.character(obj)
    if (is.null(nombre_raw)) {
      return(list(nombre = NULL, ciudad = NULL))
    }

    # CORRECCIÓN DEFINITIVA: Limpiar el paréntesis del nombre.
    nombre_limpio <- str_remove(nombre_raw, "\\s*\\(.*\\)$") %>% trimws()

    # CORRECCIÓN DEFINITIVA: Forzar la ciudad a ser NA para que nunca se muestre.
    ciudad_final <- NA_character_

    return(list(nombre = nombre_limpio, ciudad = ciudad_final))
  }

  # 1. Árbitro Principal
  info_principal <- extraer_info_arbitro(res$arbitro_principal_nombre)
  if (!is.null(info_principal$nombre)) {
    df_arbitros_partido <- df_arbitros_partido %>%
      add_row(id_partido = id_p, ime = info_principal$nombre, ciudad = info_principal$ciudad, uloga = "referee_main")
  }

  # 2. Asistente 1
  info_asist1 <- extraer_info_arbitro(res$arbitro_asist_1_nombre)
  if (!is.null(info_asist1$nombre)) {
    df_arbitros_partido <- df_arbitros_partido %>%
      add_row(id_partido = id_p, ime = info_asist1$nombre, ciudad = info_asist1$ciudad, uloga = "referee_asst1")
  }

  # 3. Asistente 2
  info_asist2 <- extraer_info_arbitro(res$arbitro_asist_2_nombre)
  if (!is.null(info_asist2$nombre)) {
    df_arbitros_partido <- df_arbitros_partido %>%
      add_row(id_partido = id_p, ime = info_asist2$nombre, ciudad = info_asist2$ciudad, uloga = "referee_asst2")
  }

  return(df_arbitros_partido)
}) %>%
  filter(!is.na(ime), ime != "Desconocido")


estadios_df <- map_dfr(resultados_exitosos, function(res) {
  if (is.null(res) || is.null(res$partido_info)) {
    return(NULL)
  }
  nombre_estadio <- res$estadio %||% res$partido_info$estadio %||% NA_character_
  tibble(id_partido = res$partido_info$id_partido, estadio = nombre_estadio)
}) %>%
  # CORRECCIÓN: Añadir las columnas de competición al join.
  left_join(select(partidos_df, id_partido, local, visitante, fecha, competicion_nombre, competicion_temporada), by = "id_partido")

message("   > All other master dataframes created.")

### 10.2. Assign Match Duration and Reassign National Team Matches
message("Step 10.2: Applying business logic (match duration, national teams)...")
partidos_df <- partidos_df %>%
  mutate(
    duracion_partido = case_when(
      str_detect(tolower(competicion_nombre), "младинска") ~ 80,
      str_detect(tolower(competicion_nombre), "кадетска") ~ 60,
      TRUE ~ 90
    ),
    es_partido_seleccion = (local == "Македонија" | visitante == "Македонија"),
    competicion_nombre = if_else(es_partido_seleccion, "Репрезентација", competicion_nombre),
    competicion_temporada = if_else(es_partido_seleccion, "Сите", competicion_temporada)
  )

### 10.3. Consolidate Player Appearances and Unify IDs
message("Step 10.3: Consolidating player appearances and unifying IDs...")
apariciones_df_raw <- map_dfr(resultados_exitosos, ~ bind_rows(
  .x$alineacion_local %>% mutate(id_partido = .x$partido_info$id_partido, equipo = .x$partido_info$local),
  .x$alineacion_visitante %>% mutate(id_partido = .x$partido_info$id_partido, equipo = .x$partido_info$visitante)
)) %>% mutate(nombre = str_squish(nombre))
preferred_id_map <- apariciones_df_raw %>%
  filter(!is.na(nombre), !is.na(id), str_detect(id, "^\\d{5,6}$")) %>%
  count(nombre, id, name = "frequency") %>%
  group_by(nombre) %>%
  filter(frequency == max(frequency)) %>%
  slice(1) %>%
  ungroup() %>%
  select(nombre, canonical_id = id)
id_mapping <- apariciones_df_raw %>%
  filter(!is.na(nombre) & nchar(trimws(nombre)) > 2) %>%
  distinct(nombre) %>%
  left_join(preferred_id_map, by = "nombre") %>%
  mutate(final_id = if_else(!is.na(canonical_id), as.character(canonical_id), paste0("player_gen_", generar_id_seguro(nombre)))) %>%
  select(nombre, canonical_id = final_id)

if (nrow(goles_df_unificado) > 0) {
  goles_df_unificado <- goles_df_unificado %>%
    left_join(id_mapping, by = c("jugadora" = "nombre")) %>%
    select(-any_of(c("id", "id_jugadora"))) %>%
    rename(id = canonical_id)
}
if (nrow(tarjetas_df_unificado) > 0) {
  tarjetas_df_unificado <- tarjetas_df_unificado %>%
    left_join(id_mapping, by = c("jugadora" = "nombre")) %>%
    select(-any_of(c("id", "id_jugadora"))) %>%
    rename(id = canonical_id)
}
if (nrow(penales_df_unificado) > 0) {
  penales_df_unificado <- penales_df_unificado %>%
    left_join(id_mapping, by = c("jugadora" = "nombre")) %>%
    select(-any_of(c("id", "id_jugadora"))) %>%
    rename(id = canonical_id)
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
    jugadoras_con_minutos <- alineacion %>% mutate(min_entra = if_else(tipo == "Titular", 0, NA_real_), min_sale = if_else(tipo == "Titular", duracion_partido, 0))
    if (!is.null(cambios) && nrow(cambios) > 0) {
      cambios_procesados <- cambios %>%
        mutate(d_entra = as.numeric(str_match(texto, "Entra .*?\\((\\d+)\\)")[, 2]), d_sale = as.numeric(str_match(texto, "por .*?\\((\\d+)\\)")[, 2])) %>%
        select(minuto, d_entra, d_sale) %>%
        filter(!is.na(d_entra) & !is.na(d_sale))
      for (i in 1:nrow(cambios_procesados)) {
        cambio <- cambios_procesados[i, ]
        jugadoras_con_minutos <- jugadoras_con_minutos %>% mutate(min_sale = if_else(dorsal == cambio$d_sale, as.numeric(cambio$minuto), min_sale), min_entra = if_else(dorsal == cambio$d_entra, as.numeric(cambio$minuto), min_entra))
      }
    }
    jugadoras_con_minutos %>%
      mutate(min_sale = if_else(!is.na(min_entra) & tipo == "Suplente" & min_sale == 0, duracion_partido, min_sale), minutos_jugados = if_else(is.na(min_entra), 0, min_sale - min_entra)) %>%
      mutate(minutos_jugados = pmax(0, minutos_jugados))
  }
  min_local <- calcular_minutos_equipo(res$alineacion_local, res$cambios_local, duracion)
  min_visitante <- calcular_minutos_equipo(res$alineacion_visitante, res$cambios_visitante, duracion)
  bind_rows(min_local, min_visitante) %>% mutate(id_partido = id_p)
})
apariciones_df <- apariciones_df_raw %>%
  left_join(minutos_df_raw %>% select(id_partido, nombre, dorsal, tipo, min_entra, min_sale, minutos_jugados), by = c("id_partido", "nombre", "dorsal", "tipo")) %>%
  select(-id) %>%
  left_join(id_mapping, by = "nombre") %>%
  rename(id = canonical_id) %>%
  left_join(partidos_df %>% select(id_partido, competicion_nombre, competicion_temporada), by = "id_partido") %>%
  select(id, id_partido, nombre, dorsal, tipo, equipo, es_portera, es_capitana, competicion_nombre, competicion_temporada, everything())

if (!is.null(mapa_roles_forzados_df) && nrow(mapa_roles_forzados_df) > 0) {
  apariciones_df <- apariciones_df %>%
    left_join(mapa_roles_forzados_df, by = "id") %>%
    mutate(es_portera = if_else(!is.na(force_role) & force_role == "goalkeeper", TRUE, es_portera)) %>%
    select(-force_role)
}
message("   > Player appearances consolidated and IDs unified.")

### 10.4. Process and Translate Demographic Data
message("Step 10.4: Processing demographic data...")
mapa_posicion_unificada <- c("GK" = "goalkeeper", "Portera" = "goalkeeper", "DL" = "defender", "DC" = "defender", "DR" = "defender", "DM" = "defender", "WBL" = "defender", "WBR" = "defender", "Defensa" = "defender", "ML" = "midfielder", "MC" = "midfielder", "MR" = "midfielder", "AMC" = "midfielder", "Centrocampista" = "midfielder", "AML" = "forward", "AMR" = "forward", "SC" = "forward", "Delantera" = "forward")
posiciones_procesadas_df <- posiciones_df %>%
  mutate(posicion_unificada = recode(posicion, !!!mapa_posicion_unificada, .default = NA_character_)) %>%
  filter(!is.na(posicion_unificada)) %>%
  group_by(id) %>%
  summarise(posicion_final_unificada = paste(unique(posicion_unificada), collapse = " / "), nacionalidad = first(nacionalidad), fecha_nacimiento = first(fecha_nacimiento), ciudad_nacimiento = first(ciudad_nacimiento), .groups = "drop")

### 10.5. Create Master Translation and Entity Dataframes
message("Step 10.5: Creating master translation and entity dataframes...")
nombres_equipos <- unique(c(partidos_df$local, partidos_df$visitante))
nombres_arbitros <- unique(arbitros_df$ime)
nombres_estadios <- unique(na.omit(estadios_df$estadio))
entidades_base_df <- tibble(original_name = c(nombres_equipos, nombres_arbitros, nombres_estadios)) %>% distinct()

if (!is.null(mapa_nombres_entidades_long)) {
  entity_translations_wide <- mapa_nombres_entidades_long %>%
    pivot_wider(id_cols = original_mk, names_from = lang, values_from = translated_name, names_prefix = "translated_name_")
  entidades_maestro_df <- entidades_base_df %>%
    left_join(entity_translations_wide, by = c("original_name" = "original_mk"))
} else {
  entidades_maestro_df <- entidades_base_df
}

map_transliteration_entity <- c("а" = "a", "б" = "b", "в" = "v", "г" = "g", "д" = "d", "ѓ" = "gj", "е" = "e", "ж" = "ž", "з" = "z", "ѕ" = "dz", "и" = "i", "ј" = "j", "к" = "k", "л" = "l", "љ" = "lj", "м" = "m", "н" = "n", "њ" = "nj", "о" = "o", "п" = "p", "р" = "r", "с" = "s", "т" = "t", "ќ" = "kj", "у" = "u", "ф" = "f", "х" = "h", "ц" = "c", "ч" = "č", "џ" = "dž", "ш" = "š")

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

### 10.6. Identify and Order Competitions
message("Step 10.6: Identifying, translating, and sorting unique competitions...")
if (exists("partidos_df") && nrow(partidos_df) > 0) {
  max_real_season_numeric <- partidos_df %>%
    filter(competicion_nombre != "Репрезентација") %>%
    distinct(competicion_temporada) %>%
    mutate(
      start_year = as.integer(str_extract(competicion_temporada, "^\\d{2,4}")),
      sort_year = if_else(nchar(as.character(start_year)) == 2, 2000 + start_year, start_year)
    ) %>%
    pull(sort_year) %>%
    max(na.rm = TRUE)
  if (is.infinite(max_real_season_numeric)) max_real_season_numeric <- 2000

  competiciones_base_df <- partidos_df %>%
    filter(competicion_nombre != "Репрезентација") %>%
    distinct(competicion_nombre, competicion_temporada) %>%
    mutate(
      competicion_id = generar_id_seguro(paste(competicion_nombre, competicion_temporada)),
      nombre_lower = tolower(competicion_nombre),
      start_year = as.integer(str_extract(competicion_temporada, "^\\d{2,4}")),
      sort_year = if_else(nchar(as.character(start_year)) == 2, 2000 + start_year, start_year)
    ) %>%
    mutate(
      importancia_score = case_when(str_detect(nombre_lower, "куп") ~ 1, str_detect(nombre_lower, "прва") ~ 2, str_detect(nombre_lower, "втора") ~ 3, str_detect(nombre_lower, "трета") ~ 4, str_detect(nombre_lower, "младинска") ~ 5, str_detect(nombre_lower, "кадетска") ~ 6, str_detect(nombre_lower, "пријателски") ~ 7, TRUE ~ 7),
      baraz_modifier = if_else(str_detect(nombre_lower, "бараж"), 0.5, 0),
      final_score = importancia_score + baraz_modifier
    )

  competicion_seleccion_df <- tibble(competicion_nombre = "Репрезентација", competicion_temporada = "Сите", competicion_id = "reprezentacija", nombre_lower = "репрезентација", importancia_score = 0, baraz_modifier = 0, final_score = 0, sort_year = max_real_season_numeric + 0.5)

  competiciones_combinadas_df <- bind_rows(competiciones_base_df, competicion_seleccion_df) %>%
    mutate(orden_primario = case_when(sort_year == max_real_season_numeric ~ 1, competicion_id == "reprezentacija" ~ 2, TRUE ~ 3))

  if (!is.null(mapa_nombres_competiciones_long)) {
    competiciones_combinadas_df_temp <- competiciones_combinadas_df %>% mutate(original_mk_join_key = paste(competicion_nombre, competicion_temporada))
    comp_translations_wide <- mapa_nombres_competiciones_long %>% pivot_wider(id_cols = original_mk, names_from = lang, values_from = translated_name, names_prefix = "nombre_completo_")
    competiciones_unicas_df <- competiciones_combinadas_df_temp %>%
      left_join(comp_translations_wide, by = c("original_mk_join_key" = "original_mk")) %>%
      select(-original_mk_join_key)
  } else {
    competiciones_unicas_df <- competiciones_combinadas_df
  }

  competiciones_unicas_df <- competiciones_unicas_df %>% mutate(nombre_completo_mk = if_else(competicion_nombre == "Репрезентација", "Репрезентација", paste(competicion_nombre, competicion_temporada)))
  map_transliteration_comp <- c("а" = "a", "б" = "b", "в" = "v", "г" = "g", "д" = "d", "ѓ" = "gj", "е" = "e", "ж" = "ž", "з" = "z", "ѕ" = "dz", "и" = "i", "ј" = "j", "к" = "k", "л" = "l", "љ" = "lj", "м" = "m", "н" = "n", "њ" = "nj", "о" = "o", "п" = "p", "р" = "r", "с" = "s", "т" = "t", "ќ" = "kj", "у" = "u", "ф" = "f", "х" = "h", "ц" = "c", "ч" = "č", "џ" = "dž", "ш" = "š")
  for (lang_code in setdiff(IDIOMAS_SOPORTADOS, "mk")) {
    target_col <- paste0("nombre_completo_", lang_code)
    if (!target_col %in% names(competiciones_unicas_df)) competiciones_unicas_df[[target_col]] <- NA_character_
  }
  for (lang_code in setdiff(IDIOMAS_SOPORTADOS, "mk")) {
    target_col <- paste0("nombre_completo_", lang_code)
    competiciones_unicas_df <- competiciones_unicas_df %>%
      mutate(!!target_col := case_when(competicion_id == "reprezentacija" ~ t("competition_reprezentacija"), !is.na(.data[[target_col]]) ~ .data[[target_col]], TRUE ~ str_to_title(str_replace_all(tolower(nombre_completo_mk), map_transliteration_comp))))
  }
  competiciones_unicas_df <- competiciones_unicas_df %>% arrange(orden_primario, final_score, desc(competicion_temporada), nombre_completo_mk)
} else {
  competiciones_unicas_df <- tibble()
}

### 10.7. Determine Scope of Changes for Incremental Generation
message("Step 10.7: Checking for changes for incremental generation using build log...")

# >>> INICIO DEL CÓDIGO AÑADIDO (Paso 1) <<<
# Cargar el historial de competiciones para detectar ligas completamente nuevas
ruta_comp_log <- "competition_log.rds"
competiciones_previas_ids <- if (file.exists(ruta_comp_log)) readRDS(ruta_comp_log) else character(0)
competiciones_actuales_ids <- competiciones_unicas_df$competicion_id
competiciones_nuevas_ids <- setdiff(competiciones_actuales_ids, competiciones_previas_ids)
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
  message("   > No changes detected. No need to regenerate HTML files.")
  affected_match_ids <- character(0)
}
hubo_cambios <- length(partidos_nuevos_ids) > 0 || length(partidos_eliminados_ids) > 0
if (hubo_cambios) {
  affected_competition_ids <- character(0)
  affected_player_ids <- character(0)
  affected_team_ids <- character(0)
  affected_referee_ids <- character(0)
  affected_stadium_ids <- character(0)
  if (length(affected_match_ids) > 0) {
    partidos_afectados_df <- partidos_df %>%
      filter(id_partido %in% affected_match_ids) %>%
      left_join(competiciones_unicas_df, by = c("competicion_nombre", "competicion_temporada"))
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
  }
  # Forzar la actualización si hay competiciones enteras que son nuevas
  if (length(competiciones_nuevas_ids) > 0) {
    message(paste("   >", length(competiciones_nuevas_ids), "new competitions detected. Forcing their regeneration."))
    affected_competition_ids <- unique(c(affected_competition_ids, competiciones_nuevas_ids))
    # Forzamos 'hubo_cambios' a TRUE para asegurar que la generación se ejecute
    hubo_cambios <- TRUE
  }
  message(paste("   > Identified", length(affected_competition_ids), "competitions,", length(affected_match_ids), "matches, and", length(affected_player_ids), "players for update."))
}
