#!/usr/bin/env Rscript
# scrape_ffm_masculino.R
# Scraper inteligente con tracking de estados, early-exit, y optimización de peticiones

# --- Instalación y carga robusta de paquetes ---
pkgs <- c("rvest", "xml2", "dplyr", "tidyr", "stringr", "purrr", 
          "tibble", "readr", "lubridate", "httr", "jsonlite")

# Instalar los que falten, incluyendo dependencias
installed_pkgs <- pkgs %in% installed.packages()
if (any(!installed_pkgs)) {
  install.packages(pkgs[!installed_pkgs], dependencies = TRUE)
}

# Cargar todos explícitamente
invisible(lapply(pkgs, library, character.only = TRUE))

# ============================================================================
# RANGOS DE IDS
# Cada línea de rangos_ids.txt es una competición distinta (inicio,fin).
# Se devuelve una lista donde cada elemento tiene $label y $ids.
# ============================================================================
leer_rangos_ids <- function(ruta = "rangos_ids.txt") {
  if (!file.exists(ruta)) stop("No existe rangos_ids.txt en la raíz")
  lineas <- readLines(ruta, warn = FALSE, encoding = "UTF-8")
  lineas <- str_trim(lineas)

  competiciones <- list()
  for (ln in lineas) {
    if (ln == "" || str_starts(ln, "#")) next
    parts <- str_split(ln, ",", simplify = TRUE)
    if (length(parts) < 2) next
    desde <- suppressWarnings(as.integer(str_trim(parts[1])))
    hasta <- suppressWarnings(as.integer(str_trim(parts[2])))
    if (is.na(desde) || is.na(hasta)) next
    if (desde <= hasta) {
      competiciones[[length(competiciones) + 1]] <- list(
        label = paste0("Rango_", desde, "-", hasta),
        ids = seq(desde, hasta)
      )
    }
  }
  competiciones
}

# ============================================================================
# TRANSLITERACIÓN LATIN → CIRÍLICO
# ============================================================================
latin_to_cyrillic <- function(nombres) {
  digraph_map <- c("zh" = "ж", "sh" = "ш", "ch" = "ч", "nj" = "њ", "lj" = "љ", "kj" = "ќ", "gj" = "ѓ", "dz" = "ѕ", "dj" = "џ")
  simple_map <- c(
    "a" = "а", "b" = "б", "c" = "ц", "d" = "д", "e" = "е", "f" = "ф", "g" = "г", "h" = "х", "i" = "и",
    "j" = "ј", "k" = "к", "l" = "л", "m" = "м", "n" = "н", "o" = "о", "p" = "п", "r" = "р", "s" = "с",
    "t" = "т", "u" = "у", "v" = "в", "z" = "з", "w" = "в", "q" = "к", "x" = "кс", "y" = "ј",
    "\u010d" = "\u0447", # č -> ч
    "\u0161" = "\u0448", # š -> ш
    "\u017e" = "\u0436", # ž -> ж
    "\u0111" = "\u0453", # đ -> ѓ
    "\u0107" = "\u045c", # ć -> ќ
    "\u00e7" = "\u0447"  # ç -> ч
  )
  sapply(nombres, function(nombre) {
    if (is.na(nombre) || nombre == "") {
      return(nombre)
    }
    txt <- tolower(nombre)
    txt <- str_replace_all(txt, digraph_map)
    txt <- str_replace_all(txt, simple_map)
    partes <- str_split(txt, " ")[[1]]
    partes <- partes[partes != ""]
    if (length(partes) == 0) {
      return("")
    }
    paste0(map_chr(partes, ~ paste0(toupper(substr(.x, 1, 1)), substr(.x, 2, nchar(.x)))), collapse = " ")
  }, USE.NAMES = FALSE)
}

# ============================================================================
# PARSEO DE MINUTO
# ============================================================================
parsear_minuto_web <- function(minuto_str) {
  sapply(minuto_str, function(x) {
    if (is.na(x) || x == "") {
      return(NA_integer_)
    }
    m <- str_remove(x, "'") %>% str_trim()
    if (str_detect(m, "\\+")) {
      parts <- str_split(m, "\\+", simplify = TRUE)
      if (length(parts) == 2) {
        return(as.integer(paste0(parts[1], parts[2])))
      }
    }
    suppressWarnings(as.integer(m))
  }, USE.NAMES = FALSE)
}

# ============================================================================
# EXTRACCIÓN DE INFO BÁSICA DE JUGADORES (IZVESTAJ)
# ============================================================================
extraer_info_basica_jugadoras <- function(columna_nodo, tipo_jugadoras) {
  nodos <- columna_nodo %>% html_elements(".ffm-rez__start")
  if (length(nodos) == 0) {
    return(tibble())
  }
  map_dfr(nodos, function(n) {
    tibble(
      nombre_latin = n %>% html_element(".ffm-rez__name span:first-child") %>% html_text(trim = TRUE),
      dorsal = n %>% html_element(".ffm-rez__name span:last-child") %>% html_text(trim = TRUE) %>% as.integer(),
      tipo_jugadoras = tipo_jugadoras
    )
  })
}
# ============================================================================
# EXTRACCIÓN DEL TOKEN SS PARA AJAX
# ============================================================================
obtener_ss_token <- function(match_id) {
  if (is.null(match_id) || is.na(match_id)) return(NULL)
  url <- sprintf("https://www.ffm.mk/delegiranje/%s/", match_id)
  message(sprintf("   -> Obteniendo nuevo token AJAX desde %s", url))
  
  res <- tryCatch(
    {
      GET(url, add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"), timeout(10))
    },
    error = function(e) {
      message(sprintf("      ! Error de conexión al obtener token: %s", e$message))
      NULL
    }
  )
  
  if (is.null(res)) return(NULL)
  if (status_code(res) != 200) {
    message(sprintf("      ! HTTP %d al obtener token", status_code(res)))
    return(NULL)
  }
  
  cont <- content(res, as = "text", encoding = "UTF-8")
  token <- str_match(cont, "ss:\\s*['\"]([^'\"\\s]+)['\"]")[, 2]
  
  if (!is.na(token)) {
    message(sprintf("   -> Token detectado: %s", token))
    return(token)
  }
  message("      ! No se encontró el token 'ss' en el HTML")
  return(NULL)
}

# ============================================================================
# EXTRACCIÓN DE ÁRBITROS Y OFICIALES VÍA AJAX (DELEGIRANJE)
# ============================================================================
get_delegiranje_data <- function(match_id, ss_token = NULL) {
  # Si no hay token, intentar obtener uno para este match_id específico
  if (is.null(ss_token) || ss_token == "") {
    ss_token <- obtener_ss_token(match_id)
  }
  
  if (is.null(ss_token) || ss_token == "") return(NULL)
  url <- "https://www.ffm.mk/wp-admin/admin-ajax.php"
  
  # Función interna para realizar la petición con un estado específico
  solicitar <- function(status_val) {
    body <- list(
      action = "igalcom_get_officials",
      match_id = as.character(match_id),
      status = status_val,
      ss = ss_token
    )
    
    res <- tryCatch(
      {
        POST(url, body = body, encode = "form", 
             add_headers("X-Requested-With" = "XMLHttpRequest",
                         "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"), 
             timeout(15))
      },
      error = function(e) NULL
    )
    
    if (is.null(res) || status_code(res) != 200) return(NULL)
    
    cont <- content(res, as = "text", encoding = "UTF-8")
    json_data <- tryCatch(fromJSON(cont), error = function(e) NULL)
    
    if (is.null(json_data) || !isTRUE(json_data$success) || is.null(json_data$data) || json_data$data == "") {
      return(NULL)
    }
    
    html_snippet <- read_html(json_data$data)
    nodos <- html_snippet %>% html_elements(".ffm-deleg__official")
    
    if (length(nodos) == 0) return(NULL)
    
    map_dfr(nodos, function(n) {
      rol_raw <- n %>% html_element(".ffm-deleg__official-role") %>% html_text(trim = TRUE)
      nombre <- n %>% html_element(".ffm-deleg__official-name") %>% html_text(trim = TRUE)
      # Limpiar el rol (quitar los dos puntos si los tiene)
      rol <- str_remove(rol_raw, ":$") %>% str_trim()
      tibble(rol = rol, nombre = nombre)
    })
  }
  
  # --- INTENTO 1: FINISHED ---
  res_final <- solicitar("FINISHED")
  
  # --- INTENTO 2: PLAYED (muy común para partidos completados en Comet) ---
  if (is.null(res_final) || nrow(res_final) == 0) {
    res_final <- solicitar("PLAYED")
  }
  
  # --- INTENTO 3: SCHEDULED (fallback extremo) ---
  if (is.null(res_final) || nrow(res_final) == 0) {
    res_final <- solicitar("SCHEDULED")
  }
  
  # Si después de todos los intentos sigue siendo NULL, devolvemos tibble vacío para persistencia satisfactoria
  if (is.null(res_final)) {
    return(tibble(rol = character(), nombre = character()))
  }
  
  return(res_final)
}

# ============================================================================
# EXTRACCIÓN DE INFO BÁSICA DE JUGADORES (IZVESTAJ)
# ============================================================================
extraer_info_basica_jugadoras <- function(columna_nodo, tipo_jugadoras) {
  nodos <- columna_nodo %>% html_elements(".ffm-rez__start")
  if (length(nodos) == 0) {
    return(tibble())
  }
  map_dfr(nodos, function(n) {
    tibble(
      nombre_latin = n %>% html_element(".ffm-rez__name span:first-child") %>% html_text(trim = TRUE),
      dorsal = n %>% html_element(".ffm-rez__name span:last-child") %>% html_text(trim = TRUE) %>% as.integer(),
      tipo_jugadoras = tipo_jugadoras
    )
  })
}
# ============================================================================
# EXTRACCIÓN DE EVENTOS (IZVESTAJ)
# ============================================================================
extraer_eventos <- function(columna_nodo) {
  nodos <- columna_nodo %>% html_elements(".ffm-rez__start")
  if (length(nodos) == 0) {
    return(tibble(nombre_latin = character(), dorsal = integer(), tipo_evento = character(), minuto_raw = character()))
  }

  res <- map_dfr(nodos, function(n) {
    nombre_latin <- n %>%
      html_element(".ffm-rez__name span:first-child") %>%
      html_text(trim = TRUE)
    dorsal <- n %>%
      html_element(".ffm-rez__name span:last-child") %>%
      html_text(trim = TRUE) %>%
      as.integer()
    eventos_nodos <- n %>% html_elements(".ffm-rez__evts > div")

    if (length(eventos_nodos) == 0) {
      return(tibble())
    }

    map_dfr(eventos_nodos, function(ev) {
      clase <- ev %>% html_attr("class")
      tipo_evento_raw <- word(clase, -1)
      tipo_evento <- case_when(
        tipo_evento_raw == "goal" ~ "goal",
        tipo_evento_raw == "penalty" ~ "penalty",
        tipo_evento_raw == "own_goal" ~ "own_goal",
        tipo_evento_raw == "yellow" ~ "yellow",
        tipo_evento_raw == "red" ~ "red",
        tipo_evento_raw == "second_yellow" ~ "second_yellow",
        tipo_evento_raw == "substitution" ~ "substitution_in",
        tipo_evento_raw == "substitution_out" ~ "substitution_out",
        TRUE ~ NA_character_
      )
      minuto_raw <- ev %>%
        html_element("span:last-child") %>%
        html_text(trim = TRUE)
      tibble(nombre_latin, dorsal, tipo_evento, minuto_raw)
    })
  })

  if (nrow(res) > 0 && "tipo_evento" %in% names(res)) {
    res %>% filter(!is.na(tipo_evento))
  } else {
    tibble(nombre_latin = character(), dorsal = integer(), tipo_evento = character(), minuto_raw = character())
  }
}

# ============================================================================
# PARSEO DEL INFORME (IZVESTAJ)
# ============================================================================
parsear_informe_web <- function(url, id_partido, competicion_info, ss_token = NULL) {
  message(paste0("   -> Scrapeando izvestaj: ", url))
  doc <- read_html(url)

  if (!is.na(xml_find_first(doc, "//*[contains(text(), 'Натпреварот не е одигран')]"))) {
    message(paste0("      -> Partido ", id_partido, " no jugado o sin datos"))
    return(NULL)
  }

  equipos_nodos <- doc %>% html_elements(".ffm-match-header__name")
  if (length(equipos_nodos) < 2) stop("No se encontraron dos nombres de equipo en la cabecera")
  equipo_local <- equipos_nodos[[1]] %>% html_text(trim = TRUE)
  equipo_visitante <- equipos_nodos[[2]] %>% html_text(trim = TRUE)

  goles_nodos <- doc %>% html_elements(".ffm-match-header__goal")
  goles_local <- goles_nodos[[1]] %>%
    html_text(trim = TRUE) %>%
    as.integer()
  goles_visitante <- goles_nodos[[2]] %>%
    html_text(trim = TRUE) %>%
    as.integer()

  info_text <- doc %>%
    html_elements(".ffm-match-meta__details span") %>%
    html_text(trim = TRUE)
  jornada <- str_extract(str_subset(info_text, "(?i)коло"), "\\d+")
  fecha <- str_extract(str_subset(info_text, "\\d{2}\\.\\d{2}\\.\\d{4}"), "\\d{2}\\.\\d{2}\\.\\d{4}")
  hora <- str_extract(str_subset(info_text, "\\d{2}:\\d{2}"), "\\d{2}:\\d{2}")
  estadio_raw <- str_remove(str_subset(info_text, "(?i)Стадион:"), "(?i)Стадион:\\s*")

  # Árbitros (Intentar delegiranje AJAX primero por ser más completo)
  delegacion_oficiales <- get_delegiranje_data(id_partido, ss_token)

  arbitros_raw <- doc %>%
    html_element(".ffm-match-meta__refs") %>%
    html_text(trim = TRUE)
  arbitro_principal <- str_match(arbitros_raw, "(?:Главен\\s+)?[Сс]удија:\\s*([^,]+)")[, 2] %>% trimws()
  asistente_1 <- str_match(arbitros_raw, "1\\.\\s*помошен судија:\\s*([^,]+)")[, 2] %>% trimws()
  asistente_2 <- str_match(arbitros_raw, "2\\.\\s*помошен судија:\\s*([^,]+)")[, 2] %>% trimws()

  # Si tenemos datos de delegiranje, priorizar nombres mapeados
  if (!is.null(delegacion_oficiales)) {
    m_princ <- delegacion_oficiales %>%
      filter(str_detect(rol, "(?i)Главен судија")) %>%
      pull(nombre)
    m_as1 <- delegacion_oficiales %>%
      filter(str_detect(rol, "(?i)1\\. помошен")) %>%
      pull(nombre)
    m_as2 <- delegacion_oficiales %>%
      filter(str_detect(rol, "(?i)2\\. помошен")) %>%
      pull(nombre)
    m_deleg <- delegacion_oficiales %>%
      filter(str_detect(rol, "(?i)Делегат")) %>%
      pull(nombre)
    m_as4 <- delegacion_oficiales %>%
      filter(str_detect(rol, "(?i)Четврти судија")) %>%
      pull(nombre)
    m_kont <- delegacion_oficiales %>%
      filter(str_detect(rol, "(?i)Контролор")) %>%
      pull(nombre)
    m_var <- delegacion_oficiales %>%
      filter(str_detect(rol, "(?i)ВАР судија")) %>%
      pull(nombre)
    m_avar <- delegacion_oficiales %>%
      filter(str_detect(rol, "(?i)АВАР судија")) %>%
      pull(nombre)
    m_var_op <- delegacion_oficiales %>%
      filter(str_detect(rol, "(?i)ВАР оператор")) %>%
      pull(nombre)

    if (length(m_princ) > 0) arbitro_principal <- m_princ[1]
    if (length(m_as1) > 0) asistente_1 <- m_as1[1]
    if (length(m_as2) > 0) asistente_2 <- m_as2[1]

    # Estos campos se añadirán al partido_df
    delegado <- if (length(m_deleg) > 0) m_deleg[1] else NA_character_
    arbitro_asist_4 <- if (length(m_as4) > 0) m_as4[1] else NA_character_
    kontrolor <- if (length(m_kont) > 0) m_kont[1] else NA_character_
    var_1 <- if (length(m_var) > 0) m_var[1] else NA_character_
    var_2 <- if (length(m_avar) > 0) m_avar[1] else NA_character_
    var_3 <- if (length(m_var_op) > 0) m_var_op[1] else NA_character_
  } else {
    delegado <- NA_character_
    arbitro_asist_4 <- NA_character_
    kontrolor <- NA_character_
    var_1 <- NA_character_
    var_2 <- NA_character_
    var_3 <- NA_character_
  }

  competicion_nombre_val <- if (!is.null(competicion_info$competition_name)) competicion_info$competition_name else NA_character_
  competicion_temporada_val <- if (!is.null(competicion_info$season)) competicion_info$season else NA_character_
  if (!is.na(competicion_nombre_val) && competicion_nombre_val != "") {
    season_match <- str_match(competicion_nombre_val, "(\\d{4}/\\d{2}|\\d{2}/\\d{2})")
    if (length(season_match) > 1 && !is.na(season_match[2])) {
      extracted_season <- season_match[2]
      if (is.na(competicion_temporada_val) || competicion_temporada_val == "") {
        competicion_temporada_val <- extracted_season
      }
      competicion_nombre_val <- str_replace_all(competicion_nombre_val, fixed(extracted_season), "")
      competicion_nombre_val <- str_replace_all(competicion_nombre_val, "\\(\\s*\\)", "") %>%
        str_replace_all("\\s{2,}", " ") %>%
        str_trim()
    }
  }

  partido_df <- tibble(
    id_partido = as.character(id_partido),
    competicion_nombre = competicion_nombre_val,
    competicion_temporada = competicion_temporada_val,
    jornada = ifelse(length(jornada) == 0, NA_character_, jornada),
    fecha = ifelse(length(fecha) == 0, NA_character_, fecha),
    hora = ifelse(length(hora) == 0, NA_character_, hora),
    local = equipo_local,
    visitante = equipo_visitante,
    goles_local = goles_local,
    goles_visitante = goles_visitante,
    penales_local = NA_integer_,
    penales_visitante = NA_integer_,
    es_resultado_oficial = FALSE,
    estadio = ifelse(length(estadio_raw) == 0, NA_character_, estadio_raw),
    arbitro_principal_nombre = ifelse(is.na(arbitro_principal) || arbitro_principal == "", "Desconocido", latin_to_cyrillic(arbitro_principal)),
    arbitro_asist_1_nombre = ifelse(is.na(asistente_1) || asistente_1 == "", "Desconocido", latin_to_cyrillic(asistente_1)),
    arbitro_asist_2_nombre = ifelse(is.na(asistente_2) || asistente_2 == "", "Desconocido", latin_to_cyrillic(asistente_2)),
    arbitro_asist_4_nombre = ifelse(is.na(arbitro_asist_4), NA_character_, latin_to_cyrillic(arbitro_asist_4)),
    delegado_nombre = ifelse(is.na(delegado), NA_character_, latin_to_cyrillic(delegado)),
    kontrolor = ifelse(is.na(kontrolor), NA_character_, latin_to_cyrillic(kontrolor)),
    var_1_nombre = ifelse(is.na(var_1), NA_character_, latin_to_cyrillic(var_1)),
    var_2_nombre = ifelse(is.na(var_2), NA_character_, latin_to_cyrillic(var_2)),
    var_3_nombre = ifelse(is.na(var_3), NA_character_, latin_to_cyrillic(var_3))
  )

  columnas_equipos <- doc %>% html_elements(".ffm-rez__oneteam")
  if (length(columnas_equipos) < 4) stop("No se hallaron suficientes columnas de alineación")

  titulares_local <- extraer_info_basica_jugadoras(columnas_equipos[[1]], "Titular")
  titulares_visitante <- extraer_info_basica_jugadoras(columnas_equipos[[2]], "Titular")
  suplentes_local <- extraer_info_basica_jugadoras(columnas_equipos[[3]], "Suplente")
  suplentes_visitante <- extraer_info_basica_jugadoras(columnas_equipos[[4]], "Suplente")

  creador_alineacion <- function(df_tit, df_sup) {
    bind_rows(df_tit, df_sup) %>%
      mutate(
        nombre = latin_to_cyrillic(nombre_latin),
        es_portera = (row_number() == 1 & tipo_jugadoras == "Titular"),
        es_capitana = FALSE,
        id = NA_character_
      ) %>%
      select(id, dorsal, nombre, nombre_latin, es_portera, es_capitana, tipo = tipo_jugadoras)
  }

  alineacion_local <- creador_alineacion(titulares_local, suplentes_local)
  alineacion_visitante <- creador_alineacion(titulares_visitante, suplentes_visitante)

  eventos_todos <- bind_rows(
    extraer_eventos(columnas_equipos[[1]]) %>% mutate(equipo = equipo_local),
    extraer_eventos(columnas_equipos[[2]]) %>% mutate(equipo = equipo_visitante),
    extraer_eventos(columnas_equipos[[3]]) %>% mutate(equipo = equipo_local),
    extraer_eventos(columnas_equipos[[4]]) %>% mutate(equipo = equipo_visitante)
  ) %>% mutate(minuto = parsear_minuto_web(minuto_raw), nombre = latin_to_cyrillic(nombre_latin))

  procesar_cambios <- function(subs_in, subs_out, equipo) {
    if (nrow(subs_in) == 0 || nrow(subs_out) == 0 || nrow(subs_in) != nrow(subs_out)) {
      return(tibble(minuto = integer(), texto = character()))
    }
    in_ord <- arrange(subs_in, minuto)
    out_ord <- arrange(subs_out, minuto)
    bind_cols(
      in_ord %>% select(nombre_in = nombre, dorsal_in = dorsal, minuto_in = minuto, equipo),
      out_ord %>% select(nombre_out = nombre, dorsal_out = dorsal)
    ) %>%
      transmute(minuto = minuto_in, texto = paste0("  - Min ", parsear_minuto_web(minuto_in), ": Entra ", nombre_in, " (", dorsal_in, ") por ", nombre_out, " (", dorsal_out, ")"))
  }

  subs_in <- eventos_todos %>% filter(tipo_evento == "substitution_in")
  subs_out <- eventos_todos %>% filter(tipo_evento == "substitution_out")

  cambios_local <- procesar_cambios(filter(subs_in, equipo == equipo_local), filter(subs_out, equipo == equipo_local), equipo_local)
  cambios_visitante <- procesar_cambios(filter(subs_in, equipo == equipo_visitante), filter(subs_out, equipo == equipo_visitante), equipo_visitante)

  goles_df <- if (nrow(eventos_todos) > 0) {
    eventos_todos %>%
      filter(tipo_evento %in% c("goal", "penalty", "own_goal")) %>%
      transmute(
        id_partido = as.character(id_partido), jugadora = nombre, equipo_jugadora = equipo,
        equipo_acreditado = if_else(tipo_evento == "own_goal", if_else(equipo == equipo_local, equipo_visitante, equipo_local), equipo),
        minuto, dorsal, tipo = if_else(tipo_evento == "own_goal", "Autogol", "Normal")
      )
  } else {
    tibble()
  }

  if (nrow(goles_df) > 0) {
    goles_local_reales <- sum(goles_df$equipo_acreditado == equipo_local, na.rm = TRUE)
    goles_visitante_reales <- sum(goles_df$equipo_acreditado == equipo_visitante, na.rm = TRUE)
    partido_df$goles_local <- goles_local_reales
    partido_df$goles_visitante <- goles_visitante_reales
  }

  tarjetas_df <- if (nrow(eventos_todos) > 0) {
    tarjetas_norm <- eventos_todos %>%
      filter(tipo_evento %in% c("yellow", "red")) %>%
      transmute(
        id_partido = as.character(id_partido), jugadora = nombre, id_jugadora = NA_character_, equipo,
        dorsal, minuto, tipo = if_else(tipo_evento == "yellow", "Amarilla", "Roja"), motivo = "Falta (Web)"
      )
    tarjetas_2a <- eventos_todos %>% filter(tipo_evento == "second_yellow")
    tarjetas_doble <- if (nrow(tarjetas_2a) > 0) {
      bind_rows(
        tarjetas_2a %>% transmute(
          id_partido = as.character(id_partido), jugadora = nombre, id_jugadora = NA_character_, equipo,
          dorsal, minuto, tipo = "Amarilla", motivo = "Falta (Web)"
        ),
        tarjetas_2a %>% transmute(
          id_partido = as.character(id_partido), jugadora = nombre, id_jugadora = NA_character_, equipo,
          dorsal, minuto, tipo = "Roja", motivo = "Doble amarilla"
        )
      )
    } else {
      tibble()
    }
    bind_rows(tarjetas_norm, tarjetas_doble)
  } else {
    tibble()
  }

  list(
    partido_info = partido_df,
    goles = goles_df,
    tarjetas = tarjetas_df,
    alineacion_local = alineacion_local,
    alineacion_visitante = alineacion_visitante,
    cambios_local = cambios_local,
    cambios_visitante = cambios_visitante,
    arbitro_principal_nombre = partido_df$arbitro_principal_nombre,
    arbitro_asist_1_nombre = partido_df$arbitro_asist_1_nombre,
    arbitro_asist_2_nombre = partido_df$arbitro_asist_2_nombre,
    arbitro_asist_4_nombre = partido_df$arbitro_asist_4_nombre,
    delegado_nombre = partido_df$delegado_nombre,
    kontrolor = partido_df$kontrolor,
    var_1_nombre = partido_df$var_1_nombre,
    var_2_nombre = partido_df$var_2_nombre,
    var_3_nombre = partido_df$var_3_nombre,
    estadio = partido_df$estadio,
    oficiales_delegacion = delegacion_oficiales # Guardar el bloque completo enriquecido
  )
}

# ============================================================================
# IDS EXCLUIDOS (CANCELADOS + RESULTADOS OFICIALES)
# ============================================================================
# ============================================================================
# EXCLUSIONES (CANCELADOS, OFICIALES Y EQUIPOS RETIRADOS)
# ============================================================================
leer_exclusiones <- function() {
  ids_excl <- character(0)
  reglas_retirados <- tibble(club = character(), competicion = character(), jornada_inicio = integer())

  if (file.exists("cancelled_matches.txt")) {
    lineas <- readLines("cancelled_matches.txt", warn = FALSE, encoding = "UTF-8") %>% str_trim()
    lineas <- lineas[lineas != "" & !str_starts(lineas, "#")]

    for (l in lineas) {
      parts <- str_split(l, ",", simplify = TRUE)
      if (ncol(parts) >= 3) {
        # Regla de equipo retirado: Club,Competición,JornadaInicio
        reglas_retirados <- bind_rows(reglas_retirados, tibble(
          club = str_trim(parts[1]),
          competicion = str_trim(parts[2]),
          jornada_inicio = as.integer(str_trim(parts[3]))
        ))
      } else if (ncol(parts) == 1) {
        # ID simple
        ids_excl <- c(ids_excl, parts[1])
      }
    }
  }

  if (file.exists("official_results.txt")) {
    res_ofic <- tryCatch(
      {
        read_csv("official_results.txt", show_col_types = FALSE) %>%
          pull(id_partido) %>%
          as.character()
      },
      error = function(e) character(0)
    )
    ids_excl <- c(ids_excl, res_ofic)
  }

  list(ids = unique(ids_excl[ids_excl != ""]), reglas = reglas_retirados)
}

# ============================================================================
# TRACKING: Sistema de seguimiento de estados de partido
#
# Estados:
#   "Scheduled"  → Najava encontrada con fecha futura; no re-consultar hasta
#                   120 min después del pitido inicial.
#   "Live_Post"  → Ya pasaron 120 min pero el izvestaj aún no tiene datos
#                   completos; reintentar en la próxima ejecución.
#   "Archived"   → Izvestaj con alineaciones = datos definitivos; NUNCA
#                   volver a consultar.
# ============================================================================
cargar_tracking <- function(ruta = "tracking.rds") {
  if (file.exists(ruta)) {
    tracking <- readRDS(ruta)
    # Asegurar zona horaria correcta en columnas POSIXct
    if ("fecha_partido" %in% names(tracking) && !is.null(tracking$fecha_partido)) {
      tracking$fecha_partido <- force_tz(tracking$fecha_partido, tzone = "Europe/Skopje")
    }
    if ("ultima_consulta" %in% names(tracking) && !is.null(tracking$ultima_consulta)) {
      tracking$ultima_consulta <- force_tz(tracking$ultima_consulta, tzone = "Europe/Skopje")
    }
    return(tracking)
  }
  # Crear tibble vacío con el esquema completo
  tibble(
    id_partido = character(),
    estado = character(),
    fecha_partido = as.POSIXct(character(), tz = "Europe/Skopje"),
    ultima_consulta = as.POSIXct(character(), tz = "Europe/Skopje"),
    intentos_izvestaj = integer(),
    tiene_datos_completos = logical(),
    competicion_nombre = character(),
    competicion_temporada = character()
  )
}

guardar_tracking <- function(tracking_df, ruta = "tracking.rds") {
  saveRDS(tracking_df, ruta)
  message(sprintf("Tracking guardado en %s (%d entradas)", ruta, nrow(tracking_df)))
}

# ============================================================================
# DECISIÓN: Máquina de estados para cada ID
#
# Retorna: "skip", "scrape_ambos", "scrape_izvestaj", "scrape_najava_only"
# ============================================================================
decidir_accion <- function(id_chr, tracking_df, exclusiones, cache_ids, ahora) {
  # 1. Excluidos por ID directo → scrape najava only if not yet in cache
  if (id_chr %in% exclusiones$ids) {
    if (id_chr %in% cache_ids) {
      return("skip")  # Already cached (metadata or full data)
    }
    return("scrape_najava_only")  # Need to fetch metadata from najava
  }

  # 2. Ya en caché pero sin tracking → datos legacy, tratar como archivado
  if (id_chr %in% cache_ids && !(id_chr %in% tracking_df$id_partido)) {
    return("skip")
  }

  # 3. Consultar estado en tracking
  idx <- which(tracking_df$id_partido == id_chr)
  if (length(idx) == 0) {
    return("scrape_ambos")
  } # Nuevo descubrimiento

  estado <- tracking_df$estado[idx[1]]

  if (estado == "Archived") {
    return("skip")
  }

  if (estado == "Scheduled") {
    fecha_p <- tracking_df$fecha_partido[idx[1]]
    if (!is.na(fecha_p) && ahora < fecha_p + minutes(120)) {
      return("skip") # Demasiado pronto, el partido no ha terminado
    }
    return("scrape_ambos") # Must fetch najava again to check withdrawal rules
  }

  if (estado == "Live_Post") {
    return("scrape_ambos") # Must fetch najava again to check withdrawal rules
  } # Reintento

  return("scrape_ambos") # Estado desconocido, intentar todo
}

# Función auxiliar para actualizar una fila en el tracking
actualizar_tracking <- function(tracking_df, id_chr, estado, fecha_partido, ahora,
                                intentos, tiene_datos, comp_nombre, comp_temporada) {
  # Eliminar entrada anterior si existe
  tracking_df <- tracking_df %>% filter(id_partido != id_chr)
  # Añadir nueva entrada
  bind_rows(tracking_df, tibble(
    id_partido = id_chr,
    estado = estado,
    fecha_partido = fecha_partido,
    ultima_consulta = ahora,
    intentos_izvestaj = as.integer(intentos),
    tiene_datos_completos = tiene_datos,
    competicion_nombre = comp_nombre,
    competicion_temporada = comp_temporada
  ))
}

# ============================================================================
# PARSEO DE NAJAVA (con extracción de fecha/hora para tracking)
# ============================================================================
parsear_najava <- function(url, id_partido, ss_token = NULL) {
  message(sprintf("   -> Najava %s", url))
  doc <- read_html(url)

  # Intentar obtener delegaciones AJAX (más completo)
  delegacion_oficiales <- get_delegiranje_data(id_partido, ss_token)

  equipo_local_cyr <- doc %>%
    html_element(".ffm-najava__team-home") %>%
    html_text(trim = TRUE)
  equipo_visitante_cyr <- doc %>%
    html_element(".ffm-najava__team-away") %>%
    html_text(trim = TRUE)
  competicion_najava_raw <- doc %>%
    html_element(".ffm-najava__competition") %>%
    html_text(trim = TRUE)

  # Extraer jornada de la najava
  jornada_najava <- NA_character_
  try(
    {
      info_text_najava <- doc %>%
        html_elements(".ffm-najava__meta span, .ffm-match-meta__details span, .ffm-najava__info span") %>%
        html_text(trim = TRUE)
      # Try standard league round: "N. коло"
      kolo_texts <- str_subset(info_text_najava, "(?i)коло")
      if (length(kolo_texts) > 0) {
        jornada_najava <- str_extract(kolo_texts[1], "\\d+")
      } else {
        # Fallback: cup rounds like "1/16", "1/8", "1/4", "1/2", "Ф" (final), etc.
        cup_texts <- str_subset(info_text_najava, "(?i)1/\\d+|полуфинале|финале|четвртфинале|3/4")
        if (length(cup_texts) > 0) {
          jornada_najava <- str_trim(cup_texts[1])
        }
      }
    },
    silent = TRUE
  )

  # Extraer temporada del texto de competición
  competicion_season <- NA_character_
  competicion_nombre_limpia <- NA_character_
  if (!is.na(competicion_najava_raw) && competicion_najava_raw != "") {
    season_match <- str_match(competicion_najava_raw, "(\\d{4}/\\d{2}|\\d{2}/\\d{2})")
    competicion_season <- ifelse(length(season_match) > 1 && !is.na(season_match[2]), season_match[2], NA_character_)
    if (!is.na(competicion_season)) {
      competicion_nombre_limpia <- str_replace_all(competicion_najava_raw, fixed(competicion_season), "")
      competicion_nombre_limpia <- str_replace_all(competicion_nombre_limpia, "\\(\\s*\\)", "") %>%
        str_replace_all("\\s{2,}", " ") %>%
        str_trim()
    } else {
      competicion_nombre_limpia <- competicion_najava_raw
    }
  }
  competicion_najava <- competicion_nombre_limpia

  # Secciones opcionales
  jugadoras_local <- tibble()
  jugadoras_visitante <- tibble()
  staff_local <- tibble()
  staff_visitante <- tibble()
  entrenador_local <- NA_character_
  entrenador_visitante <- NA_character_
  arbitro_principal_nombre <- NA_character_
  arbitro_asist_1_nombre <- NA_character_
  arbitro_asist_2_nombre <- NA_character_
  arbitro_asist_4_nombre <- NA_character_
  delegado_nombre <- NA_character_
  kontrolor <- NA_character_
  var_1_nombre <- NA_character_
  var_2_nombre <- NA_character_
  var_3_nombre <- NA_character_

  buscar_seccion <- function(patron) {
    secciones <- doc %>% html_elements(".ffm-najava__section")
    for (sec in secciones) {
      titulo <- sec %>%
        html_element(".ffm-najava__section-title") %>%
        html_text(trim = TRUE)
      if (!is.na(titulo) && str_detect(titulo, patron)) {
        return(sec)
      }
    }
    return(NULL)
  }

  extraer_jugadoras_najava <- function(team_col_node) {
    jugadoras <- team_col_node %>% html_elements(".ffm-najava__player")
    if (length(jugadoras) == 0) {
      return(tibble())
    }
    map_dfr(jugadoras, function(nodo) {
      tibble(
        dorsal = nodo %>% html_element(".ffm-najava__shirt") %>% html_text(trim = TRUE) %>% as.integer(),
        es_portera = length(html_elements(nodo, ".ffm-najava__pos-g")) > 0,
        es_capitana = length(html_elements(nodo, ".ffm-najava__captain")) > 0
      )
    })
  }

  mapa_roles_staff <- c(
    "Главен тренер" = "head_coach",
    "Помошен тренер" = "assistant_coach",
    "Физиотерапевт" = "physiotherapist",
    "Доктор" = "doctor",
    "Клупски претставник" = "club_representative",
    "Администратор" = "administrator",
    "Кондициски тренер" = "fitness_coach",
    "Безбедносно лице" = "security_commissioner",
    "Тренер на голмани" = "goalkeeping_coach"
  )

  extraer_staff_equipo <- function(team_col_node) {
    items <- team_col_node %>% html_elements(".ffm-najava__staff")
    if (length(items) == 0) {
      return(tibble())
    }
    map_dfr(items, function(item) {
      rol_raw <- item %>%
        html_element(".ffm-najava__staff-role") %>%
        html_text(trim = TRUE)
      nombre <- item %>%
        html_element(".ffm-najava__staff-name") %>%
        html_text(trim = TRUE)
      if (is.na(rol_raw) || is.na(nombre)) {
        return(tibble())
      }
      rol_limpio <- str_remove_all(rol_raw, ":") %>% str_trim()
      rol_key <- mapa_roles_staff[rol_limpio]
      if (is.na(rol_key)) rol_key <- tolower(str_replace_all(rol_limpio, " ", "_"))
      tibble(rol = unname(rol_key), nombre = nombre)
    })
  }

  extraer_oficiales_najava <- function(sec_oficiales) {
    oficiales <- sec_oficiales %>% html_elements(".ffm-najava__official")
    if (length(oficiales) == 0) {
      return(tibble(role = character(), nombre = character()))
    }
    map_dfr(oficiales, function(of) {
      role_raw <- of %>%
        html_element(".ffm-najava__official-role") %>%
        html_text(trim = TRUE)
      nombre <- of %>%
        html_element(".ffm-najava__official-name") %>%
        html_text(trim = TRUE)
      if (is.na(role_raw)) role_raw <- ""
      tibble(role = str_trim(role_raw), nombre = nombre)
    })
  }

  try(
    {
      sec_sostav <- buscar_seccion("Состав")
      if (!is.null(sec_sostav)) {
        cols <- sec_sostav %>% html_elements(".ffm-najava__team-col")
        if (length(cols) >= 2) {
          jugadoras_local <- extraer_jugadoras_najava(cols[[1]])
          jugadoras_visitante <- extraer_jugadoras_najava(cols[[2]])
        }
      }

      sec_staff <- buscar_seccion("Стручен штаб")
      if (!is.null(sec_staff)) {
        cols <- sec_staff %>% html_elements(".ffm-najava__team-col")
        for (col in cols) {
          if (str_detect(html_attr(col, "class"), "away")) {
            staff_visitante <- extraer_staff_equipo(col)
            coach_row <- staff_visitante %>% filter(rol == "head_coach")
            if (nrow(coach_row) > 0) entrenador_visitante <- coach_row$nombre[1]
          } else {
            staff_local <- extraer_staff_equipo(col)
            coach_row <- staff_local %>% filter(rol == "head_coach")
            if (nrow(coach_row) > 0) entrenador_local <- coach_row$nombre[1]
          }
        }
      }

      sec_oficiales <- buscar_seccion("Судии и делегати")
      if (!is.null(sec_oficiales)) {
        oficiales_najava <- extraer_oficiales_najava(sec_oficiales)
        for (i in seq_len(nrow(oficiales_najava))) {
          rol <- oficiales_najava$role[i]
          nombre <- oficiales_najava$nombre[i]
          if (is.na(nombre) || nombre == "") next
          if (str_detect(rol, "^Главен\\s+судија$")) {
            arbitro_principal_nombre <- nombre
          } else if (str_detect(rol, "^1\\.\\s*помошен\\s+судија$")) {
            arbitro_asist_1_nombre <- nombre
          } else if (str_detect(rol, "^2\\.\\s*помошен\\s+судија$")) {
            arbitro_asist_2_nombre <- nombre
          } else if (str_detect(rol, "^Четврти\\s+судија$")) {
            arbitro_asist_4_nombre <- nombre
          } else if (str_detect(rol, "^Контролор$")) {
            kontrolor <- nombre
          } else if (str_detect(rol, "^Делегат$")) {
            delegado_nombre <- nombre
          } else if (str_detect(rol, "^ВАР судија$")) {
            var_1_nombre <- nombre
          } else if (str_detect(rol, "^АВАР судија$")) {
            var_2_nombre <- nombre
          } else if (str_detect(rol, "^ВАР оператор$")) {
            var_3_nombre <- nombre
          }
        }
      }

      # Si tenemos datos de delegiranje, priorizar nombres mapeados
      if (!is.null(delegacion_oficiales)) {
        m_princ <- delegacion_oficiales %>%
          filter(str_detect(rol, "(?i)Главен судија")) %>%
          pull(nombre)
        m_as1 <- delegacion_oficiales %>%
          filter(str_detect(rol, "(?i)1\\. помошен")) %>%
          pull(nombre)
        m_as2 <- delegacion_oficiales %>%
          filter(str_detect(rol, "(?i)2\\. помошен")) %>%
          pull(nombre)
        m_as4 <- delegacion_oficiales %>%
          filter(str_detect(rol, "(?i)Четврти судија")) %>%
          pull(nombre)
        m_kont <- delegacion_oficiales %>%
          filter(str_detect(rol, "(?i)Контролор")) %>%
          pull(nombre)

        if (length(m_princ) > 0) arbitro_principal_nombre <- m_princ[1]
        if (length(m_as1) > 0) arbitro_asist_1_nombre <- m_as1[1]
        if (length(m_as2) > 0) arbitro_asist_2_nombre <- m_as2[1]
        if (length(m_as4) > 0) arbitro_asist_4_nombre <- m_as4[1]
        if (length(m_kont) > 0) kontrolor <- m_kont[1]
      }
    },
    silent = TRUE
  )

  # ---- EXTRACCIÓN DE FECHA Y HORA DEL PARTIDO (para tracking) ----
  fecha_partido_str <- NA_character_
  hora_partido_str <- NA_character_
  fecha_hora_partido <- as.POSIXct(NA, tz = "Europe/Skopje")

  try(
    {
      # Estrategia 1: Selectores específicos de la najava
      info_spans <- doc %>%
        html_elements(".ffm-najava__info span, .ffm-najava__date, .ffm-najava__time, .ffm-najava__meta span, .ffm-najava__details span") %>%
        html_text(trim = TRUE)

      # Estrategia 2: Buscar en los textos de sección más amplios
      if (length(info_spans) == 0) {
        info_spans <- doc %>%
          html_elements(".ffm-najava__header span, .ffm-najava span") %>%
          html_text(trim = TRUE)
      }

      # Estrategia 3: Último recurso — todo el texto del documento
      if (length(info_spans) == 0) {
        all_text_lines <- doc %>%
          html_elements("body *") %>%
          html_text(trim = TRUE)
        info_spans <- all_text_lines[nchar(all_text_lines) > 0 & nchar(all_text_lines) < 100]
      }

      fecha_candidates <- str_subset(info_spans, "\\d{2}\\.\\d{2}\\.\\d{4}")
      hora_candidates <- str_subset(info_spans, "\\d{2}:\\d{2}")

      if (length(fecha_candidates) > 0) {
        fecha_partido_str <- str_extract(fecha_candidates[1], "\\d{2}\\.\\d{2}\\.\\d{4}")
      }
      if (length(hora_candidates) > 0) {
        hora_partido_str <- str_extract(hora_candidates[1], "\\d{2}:\\d{2}")
      }

      if (!is.na(fecha_partido_str) && !is.na(hora_partido_str)) {
        fecha_hora_partido <- dmy_hm(paste(fecha_partido_str, hora_partido_str), tz = "Europe/Skopje")
      } else if (!is.na(fecha_partido_str)) {
        fecha_hora_partido <- dmy(fecha_partido_str, tz = "Europe/Skopje")
      }
    },
    silent = TRUE
  )

  list(
    equipo_local_cyr = if (!is.na(equipo_local_cyr) && equipo_local_cyr != "") equipo_local_cyr else NA_character_,
    equipo_visitante_cyr = if (!is.na(equipo_visitante_cyr) && equipo_visitante_cyr != "") equipo_visitante_cyr else NA_character_,
    competicion_najava = if (!is.na(competicion_najava) && competicion_najava != "") competicion_najava else NA_character_,
    competicion_season = if (!is.na(competicion_season) && competicion_season != "") competicion_season else NA_character_,
    jugadoras_local = jugadoras_local,
    jugadoras_visitante = jugadoras_visitante,
    staff_local = staff_local,
    staff_visitante = staff_visitante,
    entrenador_local = entrenador_local,
    entrenador_visitante = entrenador_visitante,
    arbitro_principal_nombre = arbitro_principal_nombre,
    arbitro_asist_1_nombre = arbitro_asist_1_nombre,
    arbitro_asist_2_nombre = arbitro_asist_2_nombre,
    arbitro_asist_4_nombre = arbitro_asist_4_nombre,
    delegado_nombre = delegado_nombre,
    kontrolor = kontrolor,
    var_1_nombre = var_1_nombre,
    var_2_nombre = var_2_nombre,
    var_3_nombre = var_3_nombre,
    fecha_hora_partido = fecha_hora_partido,
    jornada = jornada_najava,
    competicion_completa = competicion_najava_raw,
    oficiales_delegacion = delegacion_oficiales
  )
}

# ============================================================================
# ENRIQUECER RESULTADO DE IZVESTAJ CON DATOS DE NAJAVA
# ============================================================================
enriquecer_con_najava <- function(resultado, najava) {
  if (is.null(resultado) || is.null(najava)) {
    return(resultado)
  }

  if (!is.null(najava$equipo_local_cyr) && !is.na(najava$equipo_local_cyr)) {
    resultado$partido_info$local <- najava$equipo_local_cyr
  }
  if (!is.null(najava$equipo_visitante_cyr) && !is.na(najava$equipo_visitante_cyr)) {
    resultado$partido_info$visitante <- najava$equipo_visitante_cyr
  }
  if (!is.null(najava$competicion_najava) && !is.na(najava$competicion_najava) && najava$competicion_najava != "") {
    resultado$partido_info$competicion_nombre <- najava$competicion_najava
  }

  if ((is.null(resultado$partido_info$competicion_temporada) || is.na(resultado$partido_info$competicion_temporada) || resultado$partido_info$competicion_temporada == "") &&
    !is.null(najava$competicion_season) && !is.na(najava$competicion_season) && najava$competicion_season != "") {
    resultado$partido_info$competicion_temporada <- najava$competicion_season
  }

  actualizar_alineacion <- function(alineacion_df, najava_jug) {
    if (nrow(alineacion_df) == 0 || nrow(najava_jug) == 0) {
      return(alineacion_df)
    }
    alineacion_df %>%
      left_join(najava_jug %>% select(dorsal, es_portera_n = es_portera, es_capitana_n = es_capitana), by = "dorsal") %>%
      mutate(
        es_portera = if_else(!is.na(es_portera_n), es_portera_n, es_portera),
        es_capitana = if_else(!is.na(es_capitana_n), es_capitana_n, es_capitana)
      ) %>%
      select(-es_portera_n, -es_capitana_n)
  }

  resultado$alineacion_local <- actualizar_alineacion(resultado$alineacion_local, najava$jugadoras_local)
  resultado$alineacion_visitante <- actualizar_alineacion(resultado$alineacion_visitante, najava$jugadoras_visitante)

  resultado$staff_local <- najava$staff_local
  resultado$staff_visitante <- najava$staff_visitante
  resultado$entrenador_local <- najava$entrenador_local
  resultado$entrenador_visitante <- najava$entrenador_visitante

  if (!is.null(najava$arbitro_principal_nombre) && !is.na(najava$arbitro_principal_nombre) && najava$arbitro_principal_nombre != "") {
    resultado$arbitro_principal_nombre <- najava$arbitro_principal_nombre
  }
  if (!is.null(najava$arbitro_asist_1_nombre) && !is.na(najava$arbitro_asist_1_nombre) && najava$arbitro_asist_1_nombre != "") {
    resultado$arbitro_asist_1_nombre <- najava$arbitro_asist_1_nombre
  }
  if (!is.null(najava$arbitro_asist_2_nombre) && !is.na(najava$arbitro_asist_2_nombre) && najava$arbitro_asist_2_nombre != "") {
    resultado$arbitro_asist_2_nombre <- najava$arbitro_asist_2_nombre
  }
  if (!is.null(najava$arbitro_asist_4_nombre) && !is.na(najava$arbitro_asist_4_nombre) && najava$arbitro_asist_4_nombre != "") {
    resultado$arbitro_asist_4_nombre <- najava$arbitro_asist_4_nombre
  }
  if (!is.null(najava$kontrolor) && !is.na(najava$kontrolor) && najava$kontrolor != "") {
    resultado$kontrolor <- najava$kontrolor
  }
  if (!is.null(najava$delegado_nombre) && !is.na(najava$delegado_nombre) && najava$delegado_nombre != "") {
    resultado$delegado_nombre <- najava$delegado_nombre
  }
  if (!is.null(najava$var_1_nombre) && !is.na(najava$var_1_nombre) && najava$var_1_nombre != "") {
    resultado$var_1_nombre <- najava$var_1_nombre
  }
  if (!is.null(najava$var_2_nombre) && !is.na(najava$var_2_nombre) && najava$var_2_nombre != "") {
    resultado$var_2_nombre <- najava$var_2_nombre
  }
  if (!is.null(najava$var_3_nombre) && !is.na(najava$var_3_nombre) && najava$var_3_nombre != "") {
    resultado$var_3_nombre <- najava$var_3_nombre
  }

  if (!is.null(najava$oficiales_delegacion)) {
    resultado$oficiales_delegacion <- najava$oficiales_delegacion
  }

  resultado
}

# ============================================================================
# MAIN: Flujo principal con tracking de estados y early-exit
# ============================================================================
main <- function() {
  competiciones_rangos <- leer_rangos_ids("rangos_ids.txt")
  if (length(competiciones_rangos) == 0) stop("No hay rangos válidos en rangos_ids")

  ruta_cache <- "actas_cache.rds"
  cache <- if (file.exists(ruta_cache)) readRDS(ruta_cache) else list()

  tracking <- cargar_tracking("tracking.rds")
  exclusiones <- leer_exclusiones()
  ids_excluidos <- exclusiones$ids
  reglas_retirados <- exclusiones$reglas
  cache_ids <- names(cache)
  ahora <- now(tzone = "Europe/Skopje")

  # Load official results for score lookup during najava-only scraping
  official_results_lookup <- tibble(id_partido = character(), goles_local = numeric(), goles_visitante = numeric())
  if (file.exists("official_results.txt")) {
    tryCatch({
      official_results_lookup <- read_csv("official_results.txt", show_col_types = FALSE) %>%
        mutate(id_partido = as.character(id_partido)) %>%
        distinct(id_partido, .keep_all = TRUE)
    }, error = function(e) message("   ! Error loading official_results.txt: ", e$message))
  }

  # Evaluate withdrawal rules against the cache to dynamically find excluded IDs
  ids_excluidos_dinamicos <- character(0)
  if (nrow(exclusiones$reglas) > 0) {
    ids_excluidos_dinamicos <- unlist(lapply(cache_ids, function(id) {
      p_info <- cache[[id]]$partido_info
      if (is.null(p_info)) return(NULL)
      j_match <- suppressWarnings(as.integer(p_info$jornada))
      if (is.na(j_match)) return(NULL)

      match_regla <- exclusiones$reglas %>%
        rowwise() %>%
        filter(
          (grepl(tolower(trimws(club)), tolower(trimws(p_info$local)), fixed = TRUE) |
           grepl(tolower(trimws(p_info$local)), tolower(trimws(club)), fixed = TRUE) |
           grepl(tolower(trimws(club)), tolower(trimws(p_info$visitante)), fixed = TRUE) |
           grepl(tolower(trimws(p_info$visitante)), tolower(trimws(club)), fixed = TRUE)),
          (grepl(tolower(trimws(competicion)), tolower(trimws(p_info$competicion_nombre)), fixed = TRUE) |
           grepl(tolower(trimws(p_info$competicion_nombre)), tolower(trimws(competicion)), fixed = TRUE)),
          j_match >= jornada_inicio
        ) %>%
        ungroup()

      if (nrow(match_regla) > 0) return(id)
      return(NULL)
    }))
  }
  
  ids_excluidos_all <- unique(c(ids_excluidos, ids_excluidos_dinamicos))

  # --- RETROACTIVE DETECTION: Detect previously-normal matches now in exclusion list ---
  ids_retroactivos <- intersect(cache_ids, ids_excluidos_all)
  # Filter to only those that are NOT already marked as excluded in cache
  if (length(ids_retroactivos) > 0) {
    ids_retroactivos <- ids_retroactivos[vapply(ids_retroactivos, function(id) {
      p_info <- cache[[id]]$partido_info
      if (is.null(p_info)) return(TRUE)
      !isTRUE(p_info$es_resultado_oficial) && !isTRUE(p_info$es_cancelado)
    }, FUN.VALUE = logical(1))]
  }

  # --- REVERSE RETROACTIVE DETECTION: Detect matches that were excluded but no longer are ---
  ids_retroactivos_reverse <- setdiff(cache_ids, ids_excluidos_all)
  if (length(ids_retroactivos_reverse) > 0) {
    ids_retroactivos_reverse <- ids_retroactivos_reverse[vapply(ids_retroactivos_reverse, function(id) {
      p_info <- cache[[id]]$partido_info
      if (is.null(p_info)) return(FALSE)
      isTRUE(p_info$es_resultado_oficial) || isTRUE(p_info$es_cancelado)
    }, FUN.VALUE = logical(1))]
  }

  ids_retroactivos_all <- unique(c(ids_retroactivos, ids_retroactivos_reverse))

  if (length(ids_retroactivos_all) > 0) {
    message(sprintf("   > RETROACTIVE: %d match(es) changed exclusion status. Will re-scrape.", length(ids_retroactivos_all)))
    # Remove from cache_ids and tracking so decidir_accion returns "scrape_ambos" or "scrape_najava_only"
    cache_ids <- setdiff(cache_ids, ids_retroactivos_all)
    tracking <- tracking %>% filter(!id_partido %in% ids_retroactivos_all)
  }

  # Migrar entradas legacy del caché al tracking como Archived
  ids_legacy <- setdiff(cache_ids, tracking$id_partido)
  if (length(ids_legacy) > 0) {
    message(sprintf("Migrando %d IDs del caché legacy al tracking como Archived", length(ids_legacy)))
    tracking <- bind_rows(tracking, tibble(
      id_partido = ids_legacy,
      estado = "Archived",
      fecha_partido = as.POSIXct(NA, tz = "Europe/Skopje"),
      ultima_consulta = ahora,
      intentos_izvestaj = 0L,
      tiene_datos_completos = TRUE,
      competicion_nombre = NA_character_,
      competicion_temporada = NA_character_
    ))
  }

  total_ids <- sum(sapply(competiciones_rangos, function(c) length(c$ids)))
 
  # Obtener el token SS dinámicamente (intentar con varios IDs si es necesario)
  ss_token_sesion <- NULL
  if (total_ids > 0) {
    # Intentar con el primero, el del medio y el último de la primera competición
    ids_prueba <- c(
      as.character(competiciones_rangos[[1]]$ids[1]),
      as.character(competiciones_rangos[[1]]$ids[floor(length(competiciones_rangos[[1]]$ids)/2) + 1]),
      as.character(competiciones_rangos[[1]]$ids[length(competiciones_rangos[[1]]$ids)])
    )
    for (id_t in unique(ids_prueba)) {
      ss_token_sesion <- obtener_ss_token(id_t)
      if (!is.null(ss_token_sesion)) break
    }
  }
 
  message(sprintf(
    "Competiciones: %d | IDs totales: %d | Caché: %d | Tracking: %d | Excluidos: %d | Token: %s",
    length(competiciones_rangos), total_ids, length(cache), nrow(tracking), length(ids_excluidos),
    ifelse(is.null(ss_token_sesion), "FALLO (se intentará bajo demanda)", ss_token_sesion)
  ))

  partidos_procesados <- 0
  partidos_skipped <- 0

  for (comp_idx in seq_along(competiciones_rangos)) {
    comp <- competiciones_rangos[[comp_idx]]
    message(sprintf(
      "\n=== Competición %d/%d: %s (%d IDs) ===",
      comp_idx, length(competiciones_rangos), comp$label, length(comp$ids)
    ))
    consecutivos_vacios <- 0
    consecutivos_sin_acta <- 0

    for (id_num in comp$ids) {
      id_chr <- as.character(id_num)

      accion <- decidir_accion(id_chr, tracking, exclusiones, cache_ids, ahora)

      if (accion == "skip") {
        # MODO BACKFILL: Si está en caché pero sin los nuevos datos de oficiales
        if (id_chr %in% names(cache) && is.null(cache[[id_chr]]$oficiales_delegacion)) {
          message(sprintf("  [%s] ID %s -> backfill delegiranje (surgical AJAX)", comp$label, id_chr))
          del_data <- get_delegiranje_data(id_chr, ss_token_sesion)
          
          # Actualizar el token de sesión si get_delegiranje_data obtuvo uno nuevo satisfactoriamente
          # (Aunque get_delegiranje_data no devuelve el token, si del_data es v'alido, el token que usamos funciona)
          
          if (is.null(del_data)) {
            # Error de red o token inválido, no contamos como vacío real aún
            message(sprintf("   ID %s: error en delegiranje (posible token/red)", id_chr))
          } else if (nrow(del_data) == 0) {
            consecutivos_vacios <- consecutivos_vacios + 1
            message(sprintf("   ID %s: delegiranje vacío (%d/15 consecutivos)", id_chr, consecutivos_vacios))
            if (consecutivos_vacios >= 15) { # Más permisivo en backfill
              message(sprintf("   >>> EARLY EXIT (Backfill): 15 vacíos en %s.", comp$label))
              break
            }
          } else {
            consecutivos_vacios <- 0
            consecutivos_sin_acta <- 0
            cache[[id_chr]]$oficiales_delegacion <- del_data
            partidos_procesados <- partidos_procesados + 1
            # Forzar guardado inmediato en backfill ya que 'next' saltará el guardado al final del loop
            saveRDS(cache, ruta_cache)
          }
          Sys.sleep(runif(1, 0.8, 2.0))
          next
        }
        # Si ya tiene delegiranje, resetear contador (es un partido válido en caché)
        consecutivos_vacios <- 0
        consecutivos_sin_acta <- 0
        partidos_skipped <- partidos_skipped + 1
        next
      }

      # --- SCRAPE NAJAVA ONLY (excluded matches: cancelled / official result) ---
      if (accion == "scrape_najava_only") {
        message(sprintf("  [%s] ID %s -> scrape_najava_only (excluded match)", comp$label, id_chr))
        url_najava <- sprintf("https://ffm.mk/najava/%s/", id_chr)
        Sys.sleep(runif(1, 1.5, 3))

        najava_excl <- tryCatch(
          parsear_najava(url_najava, id_chr, ss_token_sesion),
          error = function(e) {
            message(sprintf("   Najava %s fallo: %s", id_chr, e$message))
            NULL
          }
        )

        najava_vacia_excl <- is.null(najava_excl) ||
          (is.na(najava_excl$equipo_local_cyr) && is.na(najava_excl$equipo_visitante_cyr))

        if (najava_vacia_excl) {
          consecutivos_vacios <- consecutivos_vacios + 1
          message(sprintf("   ID %s: najava vacía (excluded, %d/7 consecutivos)", id_chr, consecutivos_vacios))
          if (consecutivos_vacios >= 7) {
            message(sprintf("   >>> EARLY EXIT: 7 IDs vacíos consecutivos en %s.", comp$label))
            break
          }
          next
        }
        consecutivos_vacios <- 0

        # Determine if this is an official result or a plain cancellation
        is_official <- id_chr %in% official_results_lookup$id_partido
        is_cancelled_match <- !is_official

        # Build a minimal partido_info tibble
        comp_nombre_excl <- najava_excl$competicion_najava %||% NA_character_
        comp_temporada_excl <- najava_excl$competicion_season %||% NA_character_
        jornada_excl <- if (length(najava_excl$jornada) > 0 && !is.na(najava_excl$jornada[1])) najava_excl$jornada[1] else NA_character_
        fecha_excl <- NA_character_
        hora_excl <- NA_character_
        if (!is.null(najava_excl$fecha_hora_partido) && !is.na(najava_excl$fecha_hora_partido)) {
          fecha_excl <- format(najava_excl$fecha_hora_partido, "%d.%m.%Y")
          hora_excl <- format(najava_excl$fecha_hora_partido, "%H:%M")
        }

        # Get official score if applicable
        goles_l <- NA_integer_
        goles_v <- NA_integer_
        if (is_official) {
          or_row <- official_results_lookup %>% filter(id_partido == id_chr)
          if (nrow(or_row) > 0) {
            goles_l <- as.integer(or_row$goles_local[1])
            goles_v <- as.integer(or_row$goles_visitante[1])
          }
        }

        partido_info_excl <- tibble(
          id_partido = id_chr,
          competicion_nombre = comp_nombre_excl,
          competicion_temporada = comp_temporada_excl,
          jornada = jornada_excl,
          fecha = fecha_excl,
          hora = hora_excl,
          local = najava_excl$equipo_local_cyr %||% NA_character_,
          visitante = najava_excl$equipo_visitante_cyr %||% NA_character_,
          goles_local = goles_l,
          goles_visitante = goles_v,
          penales_local = NA_integer_,
          penales_visitante = NA_integer_,
          es_resultado_oficial = is_official,
          es_cancelado = is_cancelled_match,
          estadio = NA_character_,
          arbitro_principal_nombre = najava_excl$arbitro_principal_nombre %||% NA_character_,
          arbitro_asist_1_nombre = najava_excl$arbitro_asist_1_nombre %||% NA_character_,
          arbitro_asist_2_nombre = najava_excl$arbitro_asist_2_nombre %||% NA_character_,
          arbitro_asist_4_nombre = najava_excl$arbitro_asist_4_nombre %||% NA_character_,
          delegado_nombre = najava_excl$delegado_nombre %||% NA_character_,
          kontrolor = najava_excl$kontrolor %||% NA_character_,
          var_1_nombre = najava_excl$var_1_nombre %||% NA_character_,
          var_2_nombre = najava_excl$var_2_nombre %||% NA_character_,
          var_3_nombre = najava_excl$var_3_nombre %||% NA_character_
        )

        # Build cache entry with empty event data
        resultado_excl <- list(
          partido_info = partido_info_excl,
          goles = tibble(id_partido = character(), jugadora = character(), equipo_jugadora = character(),
                         equipo_acreditado = character(), minuto = integer(), dorsal = integer(), tipo = character()),
          tarjetas = tibble(id_partido = character(), jugadora = character(), id_jugadora = character(),
                            equipo = character(), dorsal = integer(), minuto = integer(), tipo = character(), motivo = character()),
          alineacion_local = tibble(id = character(), dorsal = integer(), nombre = character(),
                                    nombre_latin = character(), es_portera = logical(), es_capitana = logical(), tipo = character()),
          alineacion_visitante = tibble(id = character(), dorsal = integer(), nombre = character(),
                                        nombre_latin = character(), es_portera = logical(), es_capitana = logical(), tipo = character()),
          cambios_local = tibble(minuto = integer(), texto = character()),
          cambios_visitante = tibble(minuto = integer(), texto = character()),
          arbitro_principal_nombre = najava_excl$arbitro_principal_nombre %||% NA_character_,
          arbitro_asist_1_nombre = najava_excl$arbitro_asist_1_nombre %||% NA_character_,
          arbitro_asist_2_nombre = najava_excl$arbitro_asist_2_nombre %||% NA_character_,
          arbitro_asist_4_nombre = najava_excl$arbitro_asist_4_nombre %||% NA_character_,
          delegado_nombre = najava_excl$delegado_nombre %||% NA_character_,
          kontrolor = najava_excl$kontrolor %||% NA_character_,
          var_1_nombre = najava_excl$var_1_nombre %||% NA_character_,
          var_2_nombre = najava_excl$var_2_nombre %||% NA_character_,
          var_3_nombre = najava_excl$var_3_nombre %||% NA_character_,
          estadio = NA_character_,
          oficiales_delegacion = najava_excl$oficiales_delegacion,
          staff_local = najava_excl$staff_local,
          staff_visitante = najava_excl$staff_visitante,
          entrenador_local = najava_excl$entrenador_local,
          entrenador_visitante = najava_excl$entrenador_visitante
        )

        cache[[id_chr]] <- resultado_excl
        cache_ids <- c(cache_ids, id_chr)
        partidos_procesados <- partidos_procesados + 1

        fecha_hora_excl <- najava_excl$fecha_hora_partido
        tracking <- actualizar_tracking(
          tracking, id_chr, "Archived",
          fecha_partido = if (!is.null(fecha_hora_excl) && !is.na(fecha_hora_excl)) fecha_hora_excl else as.POSIXct(NA, tz = "Europe/Skopje"),
          ahora = ahora, intentos = 0L, tiene_datos = FALSE,
          comp_nombre = comp_nombre_excl, comp_temporada = comp_temporada_excl
        )

        message(sprintf("   OK ID %s: ARCHIVED (excluded - %s)", id_chr, if (is_official) "official result" else "cancelled"))

        saveRDS(cache, ruta_cache)
        guardar_tracking(tracking, "tracking.rds")
        next
      }

      message(sprintf("  [%s] ID %s -> acción: %s", comp$label, id_chr, accion))

      url_najava <- sprintf("https://ffm.mk/najava/%s/", id_chr)
      url_izvestaj <- sprintf("https://ffm.mk/izvestaj/%s/", id_chr)
      najava <- NULL
      comp_nombre_tracking <- NA_character_
      comp_temporada_tracking <- NA_character_

      # --- PASO 1: Scrape Najava (si necesario) ---
      if (accion == "scrape_ambos") {
        Sys.sleep(runif(1, 1.5, 3))
        najava <- tryCatch(
          parsear_najava(url_najava, id_chr, ss_token_sesion),
          error = function(e) {
            message(sprintf("   Najava %s fallo: %s", id_chr, e$message))
            NULL
          }
        )

        # Comprobar si la najava está vacía (404 o sin datos)
        najava_vacia <- is.null(najava) ||
          (is.na(najava$equipo_local_cyr) && is.na(najava$equipo_visitante_cyr))

        if (najava_vacia) {
          consecutivos_vacios <- consecutivos_vacios + 1
          message(sprintf("   ID %s: najava vacía (%d/7 consecutivos)", id_chr, consecutivos_vacios))
          if (consecutivos_vacios >= 7) {
            message(sprintf("   >>> EARLY EXIT: 7 IDs vacíos consecutivos en %s. Saltando competición.", comp$label))
            break
          }
          next
        }

        # Najava OK → resetear contador
        consecutivos_vacios <- 0

        comp_nombre_tracking <- najava$competicion_najava %||% NA_character_
        comp_temporada_tracking <- najava$competicion_season %||% NA_character_

        # Extraer fecha/hora del partido
        fecha_hora_partido <- najava$fecha_hora_partido

        # Registrar en tracking como Scheduled
        tracking <- actualizar_tracking(
          tracking, id_chr, "Scheduled",
          fecha_partido = if (!is.null(fecha_hora_partido) && !is.na(fecha_hora_partido)) fecha_hora_partido else as.POSIXct(NA, tz = "Europe/Skopje"),
          ahora = ahora, intentos = 0L, tiene_datos = FALSE,
          comp_nombre = comp_nombre_tracking, comp_temporada = comp_temporada_tracking
        )

        # --- NUEVA LÓGICA: Equipo retirado (mid-season) ---
        if (nrow(reglas_retirados) > 0 && length(najava$jornada) > 0 && !is.na(najava$jornada[1])) {
          j_match <- as.integer(najava$jornada[1])

          match_regla <- reglas_retirados %>%
            rowwise() %>%
            filter(
              (grepl(tolower(trimws(club)), tolower(trimws(najava$equipo_local_cyr)), fixed = TRUE) |
               grepl(tolower(trimws(najava$equipo_local_cyr)), tolower(trimws(club)), fixed = TRUE) |
               grepl(tolower(trimws(club)), tolower(trimws(najava$equipo_visitante_cyr)), fixed = TRUE) |
               grepl(tolower(trimws(najava$equipo_visitante_cyr)), tolower(trimws(club)), fixed = TRUE)),
              (grepl(tolower(trimws(competicion)), tolower(trimws(najava$competicion_completa)), fixed = TRUE) |
               grepl(tolower(trimws(najava$competicion_completa)), tolower(trimws(competicion)), fixed = TRUE)),
              j_match >= jornada_inicio
            ) %>%
            ungroup()

          if (nrow(match_regla) > 0) {
            message(sprintf(
              "   -> Partido %s omitido: Equipo retirado (%s en %s)",
              id_chr, match_regla$club[1], match_regla$competicion[1]
            ))
            
            fecha_excl <- NA_character_
            hora_excl <- NA_character_
            if (!is.null(fecha_hora_partido) && !is.na(fecha_hora_partido)) {
              fecha_excl <- format(fecha_hora_partido, "%d.%m.%Y")
              hora_excl <- format(fecha_hora_partido, "%H:%M")
            }
            
            partido_info_excl <- tibble(
              id_partido = id_chr,
              competicion_nombre = comp_nombre_tracking,
              competicion_temporada = comp_temporada_tracking,
              jornada = najava$jornada[1],
              fecha = fecha_excl,
              hora = hora_excl,
              local = najava$equipo_local_cyr %||% NA_character_,
              visitante = najava$equipo_visitante_cyr %||% NA_character_,
              goles_local = NA_integer_,
              goles_visitante = NA_integer_,
              penales_local = NA_integer_,
              penales_visitante = NA_integer_,
              es_resultado_oficial = FALSE,
              es_cancelado = TRUE,
              estadio = NA_character_,
              arbitro_principal_nombre = najava$arbitro_principal_nombre %||% NA_character_,
              arbitro_asist_1_nombre = najava$arbitro_asist_1_nombre %||% NA_character_,
              arbitro_asist_2_nombre = najava$arbitro_asist_2_nombre %||% NA_character_,
              arbitro_asist_4_nombre = najava$arbitro_asist_4_nombre %||% NA_character_,
              delegado_nombre = najava$delegado_nombre %||% NA_character_,
              kontrolor = najava$kontrolor %||% NA_character_,
              var_1_nombre = najava$var_1_nombre %||% NA_character_,
              var_2_nombre = najava$var_2_nombre %||% NA_character_,
              var_3_nombre = najava$var_3_nombre %||% NA_character_
            )
            
            cache[[id_chr]] <- list(
              partido_info = partido_info_excl,
              goles = tibble(id_partido = character(), jugadora = character(), equipo_jugadora = character(), equipo_acreditado = character(), minuto = integer(), dorsal = integer(), tipo = character()),
              tarjetas = tibble(id_partido = character(), jugadora = character(), id_jugadora = character(), equipo = character(), dorsal = integer(), minuto = integer(), tipo = character(), motivo = character()),
              sustituciones = tibble(id_partido = character(), sale_dorsal = integer(), sale_jugadora = character(), entra_dorsal = integer(), entra_jugadora = character(), minuto = integer(), equipo = character()),
              alineacion_local = if(is.null(najava$jugadoras_local)) tibble() else najava$jugadoras_local,
              alineacion_visitante = if(is.null(najava$jugadoras_visitante)) tibble() else najava$jugadoras_visitante,
              staff_local = if(is.null(najava$staff_local)) tibble() else najava$staff_local,
              staff_visitante = if(is.null(najava$staff_visitante)) tibble() else najava$staff_visitante,
              oficiales_delegacion = if(is.null(najava$oficiales_delegacion)) tibble() else najava$oficiales_delegacion
            )
            cache_ids <- c(cache_ids, id_chr)
            partidos_procesados <- partidos_procesados + 1

            # Marcar como Archived para no volver a preguntar
            tracking <- actualizar_tracking(
              tracking, id_chr, "Archived",
              fecha_partido = if (!is.null(fecha_hora_partido) && !is.na(fecha_hora_partido)) fecha_hora_partido else as.POSIXct(NA, tz = "Europe/Skopje"),
              ahora = ahora, intentos = 0L, tiene_datos = FALSE,
              comp_nombre = comp_nombre_tracking, comp_temporada = comp_temporada_tracking
            )
            
            # Guardado intermedio
            saveRDS(cache, ruta_cache)
            guardar_tracking(tracking, "tracking.rds")
            
            next
          }
        }

        # ¿Hay que esperar? Si el partido es futuro + 120 min, skip izvestaj
        if (isTRUE(!is.na(fecha_hora_partido) && ahora < fecha_hora_partido + minutes(120))) {
          message(sprintf(
            "   ID %s: partido programado para %s. Esperando 120 min post-inicio.",
            id_chr, format(fecha_hora_partido, "%d.%m.%Y %H:%M")
          ))
          consecutivos_sin_acta <- consecutivos_sin_acta + 1
          if (consecutivos_sin_acta >= 8) {
            message(sprintf("   >>> EARLY EXIT (Resultados): 8 partidos seguidos sin acta en %s.", comp$label))
            break
          }
          next # No intentar izvestaj todavía
        }
        # Si fecha pasada + 120 min, caer al paso 2 (izvestaj)
      }

      # --- PASO 2: Scrape Izvestaj ---
      if (accion %in% c("scrape_ambos", "scrape_izvestaj")) {
        # Recuperar info de competición del tracking (para reintentos sin najava)
        fila_actual_idx <- which(tracking$id_partido == id_chr)
        intentos_prev <- if (length(fila_actual_idx) > 0) tracking$intentos_izvestaj[fila_actual_idx[1]] else 0L
        fecha_p_prev <- if (length(fila_actual_idx) > 0) tracking$fecha_partido[fila_actual_idx[1]] else as.POSIXct(NA, tz = "Europe/Skopje")

        # Construir competicion_info para el izvestaj
        if (is.null(najava)) {
          # Reintento: usar datos del tracking
          comp_nombre_tracking <- if (length(fila_actual_idx) > 0) tracking$competicion_nombre[fila_actual_idx[1]] else NA_character_
          comp_temporada_tracking <- if (length(fila_actual_idx) > 0) tracking$competicion_temporada[fila_actual_idx[1]] else NA_character_
        }
        competicion_info <- list(
          competition_name = comp_nombre_tracking,
          season = comp_temporada_tracking
        )

        if (is.null(najava)) {
          najava <- tryCatch(
            parsear_najava(url_najava, id_chr, ss_token_sesion),
            error = function(e) {
              message(sprintf("   Najava fallback %s fallo: %s", id_chr, e$message))
              NULL
            }
          )
        }

        Sys.sleep(runif(1, 1.5, 3))
        resultado <- tryCatch(
          parsear_informe_web(url_izvestaj, id_chr, competicion_info, ss_token_sesion),
          error = function(e) {
            message(sprintf("   Izvestaj %s fallo: %s", id_chr, e$message))
            NULL
          }
        )

        if (!is.null(resultado)) {
          # Enriquecer con najava si disponible
          if (!is.null(najava)) resultado <- enriquecer_con_najava(resultado, najava)

          # ¿Datos completos? = tiene alineaciones
          tiene_datos <- (nrow(resultado$alineacion_local) > 0 || nrow(resultado$alineacion_visitante) > 0)

          if (tiene_datos) {
            cache[[id_chr]] <- resultado
            cache_ids <- c(cache_ids, id_chr)
            partidos_procesados <- partidos_procesados + 1
            consecutivos_sin_acta <- 0
            message(sprintf("   OK ID %s: ARCHIVED (datos completos)", id_chr))

            tracking <- actualizar_tracking(
              tracking, id_chr, "Archived",
              fecha_partido = fecha_p_prev, ahora = ahora,
              intentos = intentos_prev + 1L, tiene_datos = TRUE,
              comp_nombre = competicion_info$competition_name,
              comp_temporada = competicion_info$season
            )
          } else {
            # Izvestaj existe pero sin alineaciones todavía
            message(sprintf(
              "   ~ ID %s: Live/Post (izvestaj sin alineaciones, intento %d)",
              id_chr, intentos_prev + 1L
            ))
            consecutivos_sin_acta <- consecutivos_sin_acta + 1
            if (consecutivos_sin_acta >= 8) {
              message(sprintf("   >>> EARLY EXIT (Resultados): 8 partidos seguidos sin acta en %s.", comp$label))
              break
            }
            tracking <- actualizar_tracking(
              tracking, id_chr, "Live_Post",
              fecha_partido = fecha_p_prev, ahora = ahora,
              intentos = intentos_prev + 1L, tiene_datos = FALSE,
              comp_nombre = competicion_info$competition_name,
              comp_temporada = competicion_info$season
            )
          }
        } else {
          # Izvestaj falló (sin datos / "no jugado" / error)
          message(sprintf(
            "   ~ ID %s: Live/Post (izvestaj no disponible, intento %d)",
            id_chr, intentos_prev + 1L
          ))
          consecutivos_sin_acta <- consecutivos_sin_acta + 1
          if (consecutivos_sin_acta >= 8) {
            message(sprintf("   >>> EARLY EXIT (Resultados): 8 partidos seguidos sin acta en %s.", comp$label))
            break
          }
          tracking <- actualizar_tracking(
            tracking, id_chr, "Live_Post",
            fecha_partido = fecha_p_prev, ahora = ahora,
            intentos = intentos_prev + 1L, tiene_datos = FALSE,
            comp_nombre = if (!is.na(comp_nombre_tracking)) comp_nombre_tracking else NA_character_,
            comp_temporada = if (!is.na(comp_temporada_tracking)) comp_temporada_tracking else NA_character_
          )
        }
      }
      # AUTO-GUARDADO después de cada ID procesado para evitar pérdida de datos si hay fallos
      saveRDS(cache, ruta_cache)
      guardar_tracking(tracking, "tracking.rds")
    } # fin del loop de IDs
  } # fin del loop de competiciones

  # ---- GUARDAR TODO ----
  guardar_tracking(tracking, "tracking.rds")

  saveRDS(cache, ruta_cache)
  message(sprintf("\nCaché guardada en %s (total %d partidos)", ruta_cache, length(cache)))

  # Include retroactive IDs in modified match list for incremental builds
  partidos_modificados_para_info <- if (length(ids_retroactivos_all) > 0) {
    unique(c(names(cache), ids_retroactivos_all))
  } else {
    names(cache)
  }
  info_cambios <- list(
    hubo_cambios = (partidos_procesados > 0),
    partidos_procesados = partidos_modificados_para_info,
    partidos_modificados_ids = if (length(ids_retroactivos_all) > 0) ids_retroactivos_all else character(0)
  )
  saveRDS(info_cambios, "cache_info.rds")

  # ---- RESUMEN FINAL ----
  message(sprintf("\n=== RESUMEN ==="))
  message(sprintf("Partidos procesados (nuevos/actualizados): %d", partidos_procesados))
  message(sprintf("Partidos omitidos (ya archivados/programados/excluidos): %d", partidos_skipped))
  message(sprintf(
    "Total en tracking: %d (Scheduled: %d, Live_Post: %d, Archived: %d)",
    nrow(tracking),
    sum(tracking$estado == "Scheduled"),
    sum(tracking$estado == "Live_Post"),
    sum(tracking$estado == "Archived")
  ))
}

if (sys.nframe() == 0L) {
  main()
}
