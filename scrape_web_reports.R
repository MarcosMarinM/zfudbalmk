################################################################################
##                                                                            ##
##           SCRIPT 1.5 - WEB MATCH REPORT SCRAPING & CACHING                 ##
##                      (VERSION CON UNIFICACIÓN DE ID)                       ##
##             ** VERSIÓN FINAL - CORRECCIÓN DE LÓGICA FINAL **               ##
##                                                                            ##
################################################################################

#### 1. INITIAL SETUP ####

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  rvest, xml2, dplyr, tidyr, stringr, purrr, tibble, readr
)

# Definir rutas para la configuración y la caché
ruta_config_web <- "web_competitions.txt"
ruta_cache_web <- "web_cache.rds"
ruta_cache_pdf <- "actas_cache.rds"


#### 2. HELPER FUNCTIONS (Sin cambios) ####

latin_to_cyrillic <- function(nombres) {
  mapa_digrafos <- c("zh" = "ж", "sh" = "ш", "ch" = "ч", "nj" = "њ", "lj" = "љ", "kj" = "ќ", "gj" = "ѓ", "dz" = "ѕ", "dj" = "џ")
  mapa_simple <- c("a" = "а", "b" = "б", "c" = "ц", "d" = "д", "e" = "е", "f" = "ф", "g" = "г", "h" = "х", "i" = "и", "j" = "ј", "k" = "к", "l" = "л", "m" = "м", "n" = "н", "o" = "о", "p" = "п", "r" = "р", "s" = "с", "t" = "т", "u" = "у", "v" = "в", "z" = "з", "w" = "в", "q" = "к", "x" = "кс", "y" = "ј")
  sapply(nombres, function(nombre) {
    if (is.na(nombre) || nchar(trimws(nombre)) == 0) return(nombre)
    nombre_lower <- tolower(nombre)
    nombre_convertido <- str_replace_all(nombre_lower, mapa_digrafos)
    nombre_convertido <- str_replace_all(nombre_convertido, mapa_simple)
    str_split(nombre_convertido, " ")[[1]] %>%
      map_chr(~ paste0(toupper(substr(.x, 1, 1)), substr(.x, 2, nchar(.x)))) %>%
      paste(collapse = " ")
  }, USE.NAMES = FALSE)
}
formatear_minuto_partido <- function(minutos) { sapply(minutos, function(minuto) { if (is.na(minuto) || !is.numeric(minuto)) return(as.character(minuto)); if (minuto > 140 && nchar(as.character(minuto)) >= 3) { minuto_str <- as.character(minuto); base <- substr(minuto_str, 1, 2); added <- substr(minuto_str, 3, nchar(minuto_str)); return(paste0(base, "+", added)) } else { return(as.character(minuto)) } }) }
parsear_minuto_web <- function(minuto_str) { sapply(minuto_str, function(m) { if (is.na(m) || m == "") return(NA_integer_); minuto_limpio <- str_remove(m, "'") %>% trimws(); if (str_detect(minuto_limpio, "\\+")) { partes <- str_split(minuto_limpio, "\\+")[[1]]; return(as.integer(paste0(partes[1], partes[2]))) } else { return(as.integer(minuto_limpio)) } }, USE.NAMES = FALSE) }


#### 3. FUNCIONES DE PARSEO WEB (CON LÓGICA CORREGIDA) ####

extraer_info_basica_jugadoras <- function(columna_nodo, tipo_jugadoras) {
  jugadoras_nodos <- columna_nodo %>% html_elements(".ffm-rez__start")
  if (length(jugadoras_nodos) == 0) return(tibble())
  
  map_dfr(jugadoras_nodos, function(jugadora_nodo) {
    tibble(
      nombre_latin = jugadora_nodo %>% html_element(".ffm-rez__name span:first-child") %>% html_text(trim = TRUE),
      dorsal = jugadora_nodo %>% html_element(".ffm-rez__name span:last-child") %>% html_text(trim = TRUE) %>% as.integer(),
      tipo_jugadoras = tipo_jugadoras
    )
  })
}

extraer_eventos <- function(columna_nodo) {
  jugadoras_nodos <- columna_nodo %>% html_elements(".ffm-rez__start")
  if (length(jugadoras_nodos) == 0) return(tibble())
  
  result <- map_dfr(jugadoras_nodos, function(jugadora_nodo) {
    nombre_latin <- jugadora_nodo %>% html_element(".ffm-rez__name span:first-child") %>% html_text(trim = TRUE)
    dorsal <- jugadora_nodo %>% html_element(".ffm-rez__name span:last-child") %>% html_text(trim = TRUE) %>% as.integer()
    eventos_nodos <- jugadora_nodo %>% html_elements(".ffm-rez__evts > div")
    
    if (length(eventos_nodos) > 0) {
      map_dfr(eventos_nodos, function(evento_nodo) {
        clase <- evento_nodo %>% html_attr("class")
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
        minuto_raw <- evento_nodo %>% html_element("span:last-child") %>% html_text(trim = TRUE)
        tibble(nombre_latin, dorsal, tipo_evento, minuto_raw)
      })
    } else {
      tibble()
    }
  })
  if (nrow(result) == 0 || !"tipo_evento" %in% names(result)) return(tibble())
  result %>% filter(!is.na(tipo_evento))
}

parsear_informe_web <- function(url, id_partido, competicion_info) {
  message(paste("   -> Scrapeando:", url))
  doc <- read_html(url)
  
  if (!is.na(xml_find_first(doc, "//*[contains(text(), 'Натпреварот не е одигран')]"))) {
    message(paste("      --> Partido", id_partido, "ignorado (no jugado o sin datos)."))
    return(NULL)
  }
  
  equipos_nodos <- doc %>% html_elements(".ffm-match-header__name")
  equipo_local <- html_text(equipos_nodos[[1]], trim = TRUE)
  equipo_visitante <- html_text(equipos_nodos[[2]], trim = TRUE)
  goles_nodos <- doc %>% html_elements(".ffm-match-header__goal")
  goles_local <- as.integer(html_text(goles_nodos[[1]], trim = TRUE))
  goles_visitante <- as.integer(html_text(goles_nodos[[2]], trim = TRUE))
  info_text <- doc %>% html_elements(".ffm-match-meta__details span") %>% html_text(trim = TRUE)
  jornada <- str_extract(str_subset(info_text, "коло"), "\\d+")
  fecha <- str_extract(str_subset(info_text, "\\d{2}\\.\\d{2}\\.\\d{4}"), "\\d{2}\\.\\d{2}\\.\\d{4}")
  hora <- str_extract(str_subset(info_text, "\\d{2}:\\d{2}"), "\\d{2}:\\d{2}")
  estadio_raw <- str_remove(str_subset(info_text, "Стадион:"), "Стадион:\\s*")
  
  arbitros_raw <- doc %>% html_element(".ffm-match-meta__refs") %>% html_text(trim = TRUE)
  # Handle both old format ("Судија:") and new format ("Главен судија:")
  arbitro_principal_nombre <- str_match(arbitros_raw, "(?:Главен\\s+)?[Сс]удија:\\s*([^,]+)")[, 2] %>% trimws()
  asistente_1_nombre <- str_match(arbitros_raw, "1\\.\\s*помошен судија:\\s*([^,]+)")[, 2] %>% trimws()
  asistente_2_nombre <- str_match(arbitros_raw, "2\\.\\s*помошен судија:\\s*([^,]+)")[, 2] %>% trimws()
  arbitro_principal_nombre_cyr <- latin_to_cyrillic(arbitro_principal_nombre)
  asistente_1_nombre_cyr <- latin_to_cyrillic(asistente_1_nombre)
  asistente_2_nombre_cyr <- latin_to_cyrillic(asistente_2_nombre)
  
  partido_df <- tibble(
    id_partido, competicion_nombre = competicion_info$competition_name, competicion_temporada = competicion_info$season,
    jornada = if(length(jornada) == 0) NA_character_ else jornada,
    fecha = if(length(fecha) == 0) NA_character_ else fecha, hora = if(length(hora) == 0) NA_character_ else hora,
    local = equipo_local, visitante = equipo_visitante, goles_local, goles_visitante,
    penales_local = NA, penales_visitante = NA, es_resultado_oficial = FALSE, estadio = estadio_raw,
    arbitro_principal_nombre = arbitro_principal_nombre_cyr,
    arbitro_asist_1_nombre = asistente_1_nombre_cyr, arbitro_asist_2_nombre = asistente_2_nombre_cyr
  )
  
  columnas_equipos <- doc %>% html_elements(".ffm-rez__oneteam")
  
  titulares_local_base <- extraer_info_basica_jugadoras(columnas_equipos[[1]], "Titular")
  titulares_visitante_base <- extraer_info_basica_jugadoras(columnas_equipos[[2]], "Titular")
  suplentes_local_base <- extraer_info_basica_jugadoras(columnas_equipos[[3]], "Suplente")
  suplentes_visitante_base <- extraer_info_basica_jugadoras(columnas_equipos[[4]], "Suplente")
  
  eventos_todos <- bind_rows(
    extraer_eventos(columnas_equipos[[1]]) %>% mutate(equipo = equipo_local),
    extraer_eventos(columnas_equipos[[2]]) %>% mutate(equipo = equipo_visitante),
    extraer_eventos(columnas_equipos[[3]]) %>% mutate(equipo = equipo_local),
    extraer_eventos(columnas_equipos[[4]]) %>% mutate(equipo = equipo_visitante)
  ) %>%
    mutate(minuto = parsear_minuto_web(minuto_raw), nombre = latin_to_cyrillic(nombre_latin))
  
  crear_alineacion <- function(df_titulares, df_suplentes){
    bind_rows(df_titulares, df_suplentes) %>%
      mutate(
        nombre = latin_to_cyrillic(nombre_latin),
        es_portera = (row_number() == 1 & tipo_jugadoras == "Titular"),
        es_capitana = FALSE, id = NA_character_
      ) %>%
      select(id, dorsal, nombre, nombre_latin, es_portera, es_capitana, tipo = tipo_jugadoras)
  }
  
  alineacion_local <- crear_alineacion(titulares_local_base, suplentes_local_base)
  alineacion_visitante <- crear_alineacion(titulares_visitante_base, suplentes_visitante_base)
  
  subs_in <- eventos_todos %>% filter(tipo_evento == "substitution_in")
  subs_out <- eventos_todos %>% filter(tipo_evento == "substitution_out")
  
  procesar_cambios_equipo <- function(subs_in_team, subs_out_team) {
    if (nrow(subs_in_team) == 0 || nrow(subs_out_team) == 0 || nrow(subs_in_team) != nrow(subs_out_team)) return(tibble())
    in_sorted <- subs_in_team %>% arrange(minuto)
    out_sorted <- subs_out_team %>% arrange(minuto)
    bind_cols(
      in_sorted %>% select(nombre_in = nombre, dorsal_in = dorsal, minuto_in = minuto, equipo),
      out_sorted %>% select(nombre_out = nombre, dorsal_out = dorsal)
    ) %>% select(minuto = minuto_in, equipo, nombre_in, dorsal_in, nombre_out, dorsal_out)
  }
  
  cambios_unidos <- bind_rows(
    procesar_cambios_equipo(filter(subs_in, equipo == equipo_local), filter(subs_out, equipo == equipo_local)),
    procesar_cambios_equipo(filter(subs_in, equipo == equipo_visitante), filter(subs_out, equipo == equipo_visitante))
  )
  
  # --- INICIO DE LA CORRECCIÓN FINAL ---
  # Se ELIMINA el bloque que modificaba el estado 'tipo' de las jugadoras en las alineaciones.
  # La alineación ahora se mantiene con su estado original ("Titular", "Suplente"),
  # que es el comportamiento esperado por el script de generación de HTML.
  # El script HTML utilizará la información de `cambios_local_df` y `cambios_visitante_df`
  # para calcular correctamente los minutos jugados.
  
  # if (nrow(cambios_unidos) > 0) { ... }  <- BLOQUE ELIMINADO
  
  # --- FIN DE LA CORRECCIÓN FINAL ---
  
  goles_df <- if (nrow(eventos_todos) > 0) { eventos_todos %>% filter(tipo_evento %in% c("goal", "penalty", "own_goal")) %>% transmute(id_partido, jugadora = nombre, equipo_jugadora = equipo, equipo_acreditado = if_else(tipo_evento == "own_goal", if_else(equipo == equipo_local, equipo_visitante, equipo_local), equipo), minuto, dorsal, tipo = if_else(tipo_evento == "own_goal", "Autogol", "Normal")) } else { tibble() }
  tarjetas_df <- if (nrow(eventos_todos) > 0) {
    # Standard yellow and red cards
    tarjetas_normales <- eventos_todos %>%
      filter(tipo_evento %in% c("yellow", "red")) %>%
      transmute(id_partido, jugadora = nombre, id_jugadora = NA_character_, equipo, dorsal, minuto,
                tipo = if_else(tipo_evento == "yellow", "Amarilla", "Roja"), motivo = "Falta (Web)")
    # second_yellow generates both a yellow card AND a red card (double yellow)
    tarjetas_2a_amarilla <- eventos_todos %>% filter(tipo_evento == "second_yellow")
    tarjetas_doble <- if (nrow(tarjetas_2a_amarilla) > 0) {
      bind_rows(
        tarjetas_2a_amarilla %>% transmute(id_partido, jugadora = nombre, id_jugadora = NA_character_, equipo, dorsal, minuto, tipo = "Amarilla", motivo = "Falta (Web)"),
        tarjetas_2a_amarilla %>% transmute(id_partido, jugadora = nombre, id_jugadora = NA_character_, equipo, dorsal, minuto, tipo = "Roja", motivo = "Doble amarilla")
      )
    } else { tibble() }
    bind_rows(tarjetas_normales, tarjetas_doble)
  } else { tibble() }
  
  crear_df_cambios_pdf_format <- function(cambios, equipo_filtro) {
    if(nrow(cambios) == 0) return(tibble(minuto = integer(), texto = character()))
    df <- filter(cambios, equipo == equipo_filtro)
    if (nrow(df) == 0) return(tibble(minuto = integer(), texto = character()))
    df %>% transmute(minuto, texto = paste0("  - Min ", formatear_minuto_partido(minuto), ": Entra ", nombre_in, " (", dorsal_in, ") por ", nombre_out, " (", dorsal_out, ")")) %>% arrange(minuto)
  }
  cambios_local_df <- crear_df_cambios_pdf_format(cambios_unidos, equipo_local)
  cambios_visitante_df <- crear_df_cambios_pdf_format(cambios_unidos, equipo_visitante)
  
  return(list(
    partido_info = partido_df, goles = goles_df, tarjetas = tarjetas_df, 
    alineacion_local = alineacion_local, alineacion_visitante = alineacion_visitante, 
    cambios_local = cambios_local_df, cambios_visitante = cambios_visitante_df,
    arbitro_principal_nombre = arbitro_principal_nombre_cyr,
    arbitro_asist_1_nombre = asistente_1_nombre_cyr,
    arbitro_asist_2_nombre = asistente_2_nombre_cyr,
    estadio = estadio_raw
  ))
}


#### 3B. NAJAVA PARSING AND ENRICHMENT FUNCTIONS ####

#' @title Parse a "najava" (pre-match announcement) page from ffm.mk.
#' @description Extracts structured data not available in izvestaj: goalkeeper and
#'   captain markers, coaching staff, match delegate, team names in Cyrillic,
#'   and competition name.
#' @param url URL of the najava page (e.g., https://www.ffm.mk/najava/5442020/).
#' @param id_partido Match ID (for logging purposes).
#' @return A list with enrichment data, or NULL if parsing fails.
parsear_najava <- function(url, id_partido) {
  message(paste("   -> Scrapeando najava:", url))
  doc <- read_html(url)

  # Team names in Cyrillic
  equipo_local_cyr <- doc %>% html_element(".ffm-najava__team-home") %>% html_text(trim = TRUE)
  equipo_visitante_cyr <- doc %>% html_element(".ffm-najava__team-away") %>% html_text(trim = TRUE)

  # Competition name from page
  competicion_najava <- doc %>% html_element(".ffm-najava__competition") %>% html_text(trim = TRUE)

  # Helper to find a section by title pattern
  secciones <- doc %>% html_elements(".ffm-najava__section")
  buscar_seccion <- function(patron) {
    for (sec in secciones) {
      titulo <- sec %>% html_element(".ffm-najava__section-title") %>% html_text(trim = TRUE)
      if (!is.na(titulo) && str_detect(titulo, patron)) return(sec)
    }
    return(NULL)
  }

  # -- Player data (portera, capitana) by dorsal --
  extraer_jugadoras_najava <- function(team_col_node) {
    jugadoras <- team_col_node %>% html_elements(".ffm-najava__player")
    if (length(jugadoras) == 0) return(tibble())
    map_dfr(jugadoras, function(nodo) {
      tibble(
        dorsal = nodo %>% html_element(".ffm-najava__shirt") %>% html_text(trim = TRUE) %>% as.integer(),
        es_portera = length(html_elements(nodo, ".ffm-najava__pos-g")) > 0,
        es_capitana = length(html_elements(nodo, ".ffm-najava__captain")) > 0
      )
    })
  }

  jugadoras_local <- tibble()
  jugadoras_visitante <- tibble()
  sec_sostav <- buscar_seccion("Состав")
  if (!is.null(sec_sostav)) {
    cols <- sec_sostav %>% html_elements(".ffm-najava__team-col")
    for (tc in cols) {
      if (str_detect(html_attr(tc, "class"), "away")) {
        jugadoras_visitante <- extraer_jugadoras_najava(tc)
      } else {
        jugadoras_local <- extraer_jugadoras_najava(tc)
      }
    }
  }

  # -- Coaching staff (all roles per team) --
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
    if (length(items) == 0) return(tibble())
    map_dfr(items, function(item) {
      rol_raw <- item %>% html_element(".ffm-najava__staff-role") %>% html_text(trim = TRUE)
      nombre <- item %>% html_element(".ffm-najava__staff-name") %>% html_text(trim = TRUE)
      if (is.na(rol_raw) || is.na(nombre)) return(tibble())
      rol_limpio <- str_remove_all(rol_raw, ":") %>% str_trim()
      rol_key <- mapa_roles_staff[rol_limpio]
      # Fallback for unexpected roles
      if (is.na(rol_key)) rol_key <- tolower(str_replace_all(rol_limpio, " ", "_"))
      tibble(rol = unname(rol_key), nombre = nombre)
    })
  }

  staff_local <- tibble()
  staff_visitante <- tibble()
  entrenador_local <- NA_character_
  entrenador_visitante <- NA_character_
  sec_staff <- buscar_seccion("Стручен штаб")
  if (!is.null(sec_staff)) {
    staff_cols <- sec_staff %>% html_elements(".ffm-najava__team-col")
    for (sc in staff_cols) {
      if (str_detect(html_attr(sc, "class"), "away")) {
        staff_visitante <- extraer_staff_equipo(sc)
        coach_row <- staff_visitante %>% filter(rol == "head_coach")
        if (nrow(coach_row) > 0) entrenador_visitante <- coach_row$nombre[1]
      } else {
        staff_local <- extraer_staff_equipo(sc)
        coach_row <- staff_local %>% filter(rol == "head_coach")
        if (nrow(coach_row) > 0) entrenador_local <- coach_row$nombre[1]
      }
    }
  }

  # -- Match delegate (Контролор / Делегат) --
  delegado <- NA_character_
  sec_oficiales <- buscar_seccion("Судии и делегати")
  if (!is.null(sec_oficiales)) {
    oficiales <- sec_oficiales %>% html_elements(".ffm-najava__official")
    for (of in oficiales) {
      rol <- of %>% html_element(".ffm-najava__official-role") %>% html_text(trim = TRUE)
      if (!is.na(rol) && str_detect(rol, "Контролор|Делегат")) {
        delegado <- of %>% html_element(".ffm-najava__official-name") %>% html_text(trim = TRUE)
      }
    }
  }

  return(list(
    equipo_local_cyr = equipo_local_cyr,
    equipo_visitante_cyr = equipo_visitante_cyr,
    competicion_najava = competicion_najava,
    jugadoras_local = jugadoras_local,
    jugadoras_visitante = jugadoras_visitante,
    staff_local = staff_local,
    staff_visitante = staff_visitante,
    entrenador_local = entrenador_local,
    entrenador_visitante = entrenador_visitante,
    delegado = delegado
  ))
}


#' @title Enrich an izvestaj result with data from the corresponding najava page.
#' @description Merges najava data into the parsed izvestaj result: replaces Latin
#'   team names with Cyrillic, sets explicit es_portera/es_capitana flags from
#'   najava (by dorsal matching), and adds coaching staff + delegate fields.
#' @param resultado The list returned by parsear_informe_web().
#' @param najava The list returned by parsear_najava(), or NULL.
#' @return The enriched resultado list.
enriquecer_con_najava <- function(resultado, najava) {
  if (is.null(najava)) return(resultado)

  old_local <- resultado$partido_info$local
  old_visitante <- resultado$partido_info$visitante
  new_local <- najava$equipo_local_cyr
  new_visitante <- najava$equipo_visitante_cyr

  # Helper: replace team name in specified columns of a dataframe
  reemplazar_equipo <- function(df, cols, old_name, new_name) {
    if (is.null(df) || nrow(df) == 0 || is.na(new_name) || is.na(old_name)) return(df)
    for (col in cols) {
      if (col %in% names(df)) {
        idx <- !is.na(df[[col]]) & df[[col]] == old_name
        df[[col]][idx] <- new_name
      }
    }
    return(df)
  }

  # 1. Replace Latin team names with Cyrillic from najava (partido_info + events)
  if (!is.na(new_local) && nchar(new_local) > 0) {
    resultado$partido_info$local <- new_local
    resultado$goles <- reemplazar_equipo(resultado$goles, c("equipo_jugadora", "equipo_acreditado"), old_local, new_local)
    resultado$tarjetas <- reemplazar_equipo(resultado$tarjetas, "equipo", old_local, new_local)
  }
  if (!is.na(new_visitante) && nchar(new_visitante) > 0) {
    resultado$partido_info$visitante <- new_visitante
    resultado$goles <- reemplazar_equipo(resultado$goles, c("equipo_jugadora", "equipo_acreditado"), old_visitante, new_visitante)
    resultado$tarjetas <- reemplazar_equipo(resultado$tarjetas, "equipo", old_visitante, new_visitante)
  }

  # 2. Update es_portera and es_capitana in alineaciones by dorsal matching
  actualizar_alineacion <- function(alin, najava_jug) {
    if (nrow(alin) == 0 || nrow(najava_jug) == 0) return(alin)
    alin %>%
      left_join(najava_jug %>% select(dorsal, nj_portera = es_portera, nj_capitana = es_capitana), by = "dorsal") %>%
      mutate(
        es_portera = if_else(!is.na(nj_portera), nj_portera, es_portera),
        es_capitana = if_else(!is.na(nj_capitana), nj_capitana, es_capitana)
      ) %>%
      select(-nj_portera, -nj_capitana)
  }

  resultado$alineacion_local <- actualizar_alineacion(resultado$alineacion_local, najava$jugadoras_local)
  resultado$alineacion_visitante <- actualizar_alineacion(resultado$alineacion_visitante, najava$jugadoras_visitante)

  # 3. Add fields from najava that izvestaj lacks (equivalent to PDF extraction)
  resultado$staff_local <- najava$staff_local
  resultado$staff_visitante <- najava$staff_visitante
  resultado$entrenador_local <- najava$entrenador_local
  resultado$entrenador_visitante <- najava$entrenador_visitante
  resultado$delegado <- najava$delegado

  return(resultado)
}


#### 4. LECTURA DE CONFIGURACIÓN Y GESTIÓN DE CACHÉ ####

#### 4.1. Load competition ranges (web_competitions.txt) ####
partidos_a_procesar_df <- tibble(id_partido = character(), competition_name = character(), season = character())
if (file.exists(ruta_config_web)) {
  config_df <- read_csv(ruta_config_web, show_col_types = FALSE)
  partidos_competiciones <- config_df %>%
    rowwise() %>%
    reframe(id_partido = as.character(seq(start_id, end_id)), competition_name, season) %>%
    distinct()
  partidos_a_procesar_df <- bind_rows(partidos_a_procesar_df, partidos_competiciones)
  message(paste("   > web_competitions.txt:", nrow(partidos_competiciones), "partidos de", nrow(config_df), "competiciones."))
}

#### 4.2. Load individual matches (web_single_matches.txt) ####
ruta_config_single <- "web_single_matches.txt"
if (file.exists(ruta_config_single)) {
  single_df <- read_csv(ruta_config_single, show_col_types = FALSE) %>%
    mutate(id_partido = as.character(match_id)) %>%
    select(id_partido, competition_name, season)
  partidos_a_procesar_df <- bind_rows(partidos_a_procesar_df, single_df) %>% distinct()
  message(paste("   > web_single_matches.txt:", nrow(single_df), "partidos individuales añadidos."))
}

if (nrow(partidos_a_procesar_df) == 0) {
  stop("No se encontraron partidos para procesar. Revisa web_competitions.txt y/o web_single_matches.txt.")
}

resultados_cacheados <- list()
if (file.exists(ruta_cache_web)) {
  message(paste("Cargando resultados previos desde:", ruta_cache_web))
  resultados_cacheados <- readRDS(ruta_cache_web)
}

ids_ya_cacheados <- names(resultados_cacheados)
partidos_pendientes_df <- partidos_a_procesar_df %>%
  filter(!id_partido %in% ids_ya_cacheados)

mapa_pdf <- tibble()
if (file.exists(ruta_cache_pdf)) {
  message("Cargando datos de jugadoras desde la caché de PDFs...")
  datos_pdf <- readRDS(ruta_cache_pdf)
  mapa_pdf <- map_dfr(datos_pdf, ~bind_rows(.x$alineacion_local, .x$alineacion_visitante)) %>%
    filter(!is.na(id)) %>%
    distinct(nombre, .keep_all = TRUE) %>%
    select(id_real = id, nombre)
  message(paste(" -> Encontradas", nrow(mapa_pdf), "jugadoras únicas en la caché de PDFs."))
} else {
  warning("No se encontró 'actas_cache.rds'. La asignación de IDs reales no será posible.")
}

#### 5. EJECUCIÓN DEL SCRAPING Y ASIGNACIÓN DE IDs ####

nuevos_resultados <- list()
if (nrow(partidos_pendientes_df) > 0) {
  message(paste("\nSe encontraron", nrow(partidos_pendientes_df), "nuevos informes web para procesar."))
  
  safe_parser <- safely(parsear_informe_web)
  safe_najava <- safely(parsear_najava)
  
  mapa_jugadoras_nuevas <- tibble(nombre_latin = character(), id_asignado = character())
  siguiente_id_falso <- 990001
  
  resultados_procesados <- list()
  
  for (i in 1:nrow(partidos_pendientes_df)) {
    partido_actual <- partidos_pendientes_df[i, ]
    id_partido_actual <- partido_actual$id_partido
    competicion_info_actual <- list(competition_name = partido_actual$competition_name, season = partido_actual$season)
    url_actual <- paste0("https://www.ffm.mk/izvestaj/", id_partido_actual, "/")
    
    Sys.sleep(0.5) 
    
    resultado_scrape <- safe_parser(url_actual, id_partido_actual, competicion_info_actual)
    
    if (is.null(resultado_scrape$result)) {
      # Match not played or error: skip without caching so it's retried next run
      if (is.null(resultado_scrape$error)) {
        message(paste("      --> Partido", id_partido_actual, "ignorado (no jugado/sin datos). No se cachea."))
      } else {
        message(paste("      --> Error en partido", id_partido_actual, ":", resultado_scrape$error$message))
      }
      next
    }
    
    if (is.null(resultado_scrape$error)) {
      # Enrich with najava data (portera, capitana, entrenadores, Cyrillic team names)
      url_najava <- paste0("https://www.ffm.mk/najava/", id_partido_actual, "/")
      Sys.sleep(0.3)
      najava_result <- safe_najava(url_najava, id_partido_actual)
      if (!is.null(najava_result$result)) {
        resultado_scrape$result <- enriquecer_con_najava(resultado_scrape$result, najava_result$result)
      } else if (!is.null(najava_result$error)) {
        message(paste("      --> Najava no disponible para partido", id_partido_actual, ":", najava_result$error$message))
      }

      alineaciones_partido <- bind_rows(
        resultado_scrape$result$alineacion_local,
        resultado_scrape$result$alineacion_visitante
      )
      
      if (nrow(alineaciones_partido) > 0) {
        mapa_ids_partido <- alineaciones_partido %>%
          distinct(nombre, nombre_latin) %>%
          mutate(
            id_final = case_when(
              nombre %in% mapa_pdf$nombre ~ mapa_pdf$id_real[match(nombre, mapa_pdf$nombre)],
              nombre_latin %in% mapa_jugadoras_nuevas$nombre_latin ~ mapa_jugadoras_nuevas$id_asignado[match(nombre_latin, mapa_jugadoras_nuevas$nombre_latin)],
              TRUE ~ NA_character_
            )
          )
        
        jugadoras_a_asignar <- mapa_ids_partido %>% filter(is.na(id_final))
        
        if(nrow(jugadoras_a_asignar) > 0) {
          for(j in 1:nrow(jugadoras_a_asignar)) {
            id_nuevo <- as.character(siguiente_id_falso)
            nombre_lat_nuevo <- jugadoras_a_asignar$nombre_latin[j]
            mapa_ids_partido$id_final[mapa_ids_partido$nombre_latin == nombre_lat_nuevo] <- id_nuevo
            mapa_jugadoras_nuevas <- bind_rows(mapa_jugadoras_nuevas, tibble(nombre_latin = nombre_lat_nuevo, id_asignado = id_nuevo))
            siguiente_id_falso <- siguiente_id_falso + 1
          }
        }
        
        resultado_scrape$result$alineacion_local <- resultado_scrape$result$alineacion_local %>%
          left_join(select(mapa_ids_partido, nombre_latin, id_final), by = "nombre_latin") %>%
          mutate(id = id_final) %>%
          select(-id_final, -nombre_latin)
        
        resultado_scrape$result$alineacion_visitante <- resultado_scrape$result$alineacion_visitante %>%
          left_join(select(mapa_ids_partido, nombre_latin, id_final), by = "nombre_latin") %>%
          mutate(id = id_final) %>%
          select(-id_final, -nombre_latin)
        
        resultados_procesados[[id_partido_actual]] <- resultado_scrape
      } else {
        resultados_procesados[[id_partido_actual]] <- resultado_scrape
      }
    } else {
      resultados_procesados[[id_partido_actual]] <- resultado_scrape
    }
  }
  
  nuevos_resultados <- map(resultados_procesados, "result") %>% compact()
  errores <- map(resultados_procesados, "error") %>% compact()
  
  if (length(errores) > 0) {
    message("\n--- Se encontraron errores durante el scraping: ---")
    iwalk(errores, ~cat(paste0("Error en ID ", .y, ": ", .x$message, "\n")))
  }
} else {
  message("\nNo hay nuevos informes web para procesar. Todo está en caché.")
}


#### 6. ACTUALIZAR Y GUARDAR CACHÉ ####

if (length(nuevos_resultados) > 0) {
  n_ignorados <- sum(sapply(nuevos_resultados, function(x) isTRUE(x$ignorado)))
  n_reales <- length(nuevos_resultados) - n_ignorados
  resultados_finales <- c(resultados_cacheados, nuevos_resultados)
  saveRDS(resultados_finales, file = ruta_cache_web)
  message(paste0("\nCaché web actualizada: ", n_reales, " informes nuevos, ", n_ignorados, " ignorados (no jugados/sin datos)."))
  message(paste("Total de entradas en web_cache.rds:", length(resultados_finales)))
}

message("\nProceso de scraping y cacheo web completado.")