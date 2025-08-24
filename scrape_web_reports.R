################################################################################
##                                                                            ##
##           SCRIPT 1.5 - WEB MATCH REPORT SCRAPING & CACHING                 ##
##                      (VERSION CON UNIFICACIÓN DE ID)                       ##
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
parsear_minuto_web <- function(minuto_str) { sapply(minuto_str, function(m) { if (is.na(m)) return(NA_integer_); minuto_limpio <- str_remove(m, "'") %>% trimws(); if (str_detect(minuto_limpio, "\\+")) { partes <- str_split(minuto_limpio, "\\+")[[1]]; return(as.integer(paste0(partes[1], partes[2]))) } else { return(as.integer(minuto_limpio)) } }, USE.NAMES = FALSE) }


#### 3. FUNCIONES DE PARSEO WEB ####

extraer_jugadoras_y_eventos_largo <- function(columna_nodo, tipo_jugadoras) {
  jugadoras_nodos <- columna_nodo %>% html_elements(".ffm-rez__start")
  if (length(jugadoras_nodos) == 0) return(tibble())
  map_dfr(jugadoras_nodos, function(jugadora_nodo) {
    nombre_latin <- jugadora_nodo %>% html_element(".ffm-rez__name span:first-child") %>% html_text(trim = TRUE)
    dorsal <- jugadora_nodo %>% html_element(".ffm-rez__name span:last-child") %>% html_text(trim = TRUE) %>% as.integer()
    eventos_nodos <- jugadora_nodo %>% html_elements(".ffm-rez__evts > div")
    if (length(eventos_nodos) > 0) {
      map_dfr(eventos_nodos, function(evento_nodo) {
        clase <- evento_nodo %>% html_attr("class")
        tipo_evento <- str_extract(clase, "goal|penalty|own_goal|yellow|red|substitution|substitution_out")
        minuto_raw <- evento_nodo %>% html_element("span:last-child") %>% html_text(trim = TRUE)
        tibble(nombre_latin, dorsal, tipo_jugadoras, tipo_evento, minuto_raw)
      })
    } else { tibble(nombre_latin, dorsal, tipo_jugadoras, tipo_evento = NA_character_, minuto_raw = NA_character_) }
  })
}

parsear_informe_web <- function(url, id_partido, competicion_info) {
  message(paste("   -> Scrapeando:", url))
  doc <- read_html(url)
  
  nodo_advertencia <- doc %>% html_element(".col-md-6:contains('Натпреварот не е одигран')")
  if (!is.na(nodo_advertencia)) {
    message(paste("      --> Partido", id_partido, "ignorado (no jugado o sin datos)."))
    return(NULL)
  }
  
  equipos_nodos <- doc %>% html_elements("h1.ffm-rez__title span")
  equipo_local <- html_text(equipos_nodos[[1]], trim = TRUE)
  equipo_visitante <- html_text(equipos_nodos[[2]], trim = TRUE)
  goles_nodos <- doc %>% html_elements(".ffm-rez__rez .col-6 span")
  goles_local <- as.integer(html_text(goles_nodos[[1]], trim = TRUE))
  goles_visitante <- as.integer(html_text(goles_nodos[[2]], trim = TRUE))
  info_text <- doc %>% html_elements(".ffm-rez__info div") %>% html_text(trim = TRUE)
  jornada <- str_extract(str_subset(info_text, "коло"), "\\d+")
  fecha <- str_extract(str_subset(info_text, "\\d{2}\\.\\d{2}\\.\\d{4}"), "\\d{2}\\.\\d{2}\\.\\d{4}")
  hora <- str_extract(str_subset(info_text, "\\d{2}:\\d{2}"), "\\d{2}:\\d{2}")
  estadio_raw <- str_remove(str_subset(info_text, "Стадион:"), "Стадион: ")
  
  arbitros_raw <- doc %>% html_element(".row.pt-5.pb-4 .col-12:last-child div") %>% html_text(trim = TRUE)
  
  arbitro_principal_nombre <- str_match(arbitros_raw, "Судија:\\s*([^,]+)")[, 2] %>% trimws()
  asistente_1_nombre <- str_match(arbitros_raw, "1\\.\\s*помошен судија:\\s*([^,]+)")[, 2] %>% trimws()
  asistente_2_nombre <- str_match(arbitros_raw, "2\\.\\s*помошен судија:\\s*([^,]+)")[, 2] %>% trimws()
  
  arbitro_principal_nombre_cyr <- latin_to_cyrillic(arbitro_principal_nombre)
  asistente_1_nombre_cyr <- latin_to_cyrillic(asistente_1_nombre)
  asistente_2_nombre_cyr <- latin_to_cyrillic(asistente_2_nombre)

  partido_df <- tibble(
    id_partido = id_partido, 
    competicion_nombre = competicion_info$competition_name, 
    competicion_temporada = competicion_info$season,
    jornada = if(length(jornada) == 0) NA_character_ else jornada,
    fecha = if(length(fecha) == 0) NA_character_ else fecha,
    hora = if(length(hora) == 0) NA_character_ else hora,
    local = equipo_local, visitante = equipo_visitante,
    goles_local, goles_visitante, penales_local = NA, penales_visitante = NA,
    es_resultado_oficial = FALSE, estadio = estadio_raw, 
    arbitro_principal_nombre = arbitro_principal_nombre_cyr,
    arbitro_asist_1_nombre = asistente_1_nombre_cyr,
    arbitro_asist_2_nombre = asistente_2_nombre_cyr
  )
  
  columnas_equipos <- doc %>% html_elements(".ffm-rez__oneteam")
  
  titulares_local_raw <- extraer_jugadoras_y_eventos_largo(columnas_equipos[[1]], "Titular")
  titulares_visitante_raw <- extraer_jugadoras_y_eventos_largo(columnas_equipos[[2]], "Titular")
  suplentes_local_raw <- extraer_jugadoras_y_eventos_largo(columnas_equipos[[3]], "Suplente")
  suplentes_visitante_raw <- extraer_jugadoras_y_eventos_largo(columnas_equipos[[4]], "Suplente")
  
  eventos_todos <- bind_rows(
    titulares_local_raw %>% mutate(equipo = equipo_local),
    titulares_visitante_raw %>% mutate(equipo = equipo_visitante),
    suplentes_local_raw %>% mutate(equipo = equipo_local),
    suplentes_visitante_raw %>% mutate(equipo = equipo_visitante)
  ) %>%
    filter(!is.na(tipo_evento)) %>%
    mutate(minuto = parsear_minuto_web(minuto_raw), nombre = latin_to_cyrillic(nombre_latin))
  
  crear_alineacion <- function(df_titulares, df_suplentes){
    bind_rows(df_titulares, df_suplentes) %>%
      distinct(nombre_latin, dorsal, tipo_jugadoras) %>%
      mutate(
        nombre = latin_to_cyrillic(nombre_latin),
        es_portera = (row_number() == 1 & tipo_jugadoras == "Titular"),
        es_capitana = FALSE, 
        id = NA_character_
      ) %>%
      select(id, dorsal, nombre, nombre_latin, es_portera, es_capitana, tipo = tipo_jugadoras)
  }
  alineacion_local <- crear_alineacion(titulares_local_raw, suplentes_local_raw)
  alineacion_visitante <- crear_alineacion(titulares_visitante_raw, suplentes_visitante_raw)
  
  goles_df <- if (nrow(eventos_todos) > 0) { eventos_todos %>% filter(tipo_evento %in% c("goal", "penalty", "own_goal")) %>% transmute(jugadora = nombre, equipo_jugadora = equipo, equipo_acreditado = if_else(tipo_evento == "own_goal", if_else(equipo == equipo_local, equipo_visitante, equipo_local), equipo), minuto = minuto, dorsal = dorsal, tipo = if_else(tipo_evento == "own_goal", "Autogol", "Normal")) %>% tibble::add_column(id_partido = id_partido, .before = 1) } else { tibble() }
  tarjetas_df <- if (nrow(eventos_todos) > 0) { eventos_todos %>% filter(tipo_evento %in% c("yellow", "red")) %>% transmute(jugadora = nombre, equipo = equipo, dorsal = dorsal, minuto = minuto, tipo = if_else(tipo_evento == "yellow", "Amarilla", "Roja"), motivo = "Falta (Web)") %>% tibble::add_column(id_partido = id_partido, .before = 1) } else { tibble() }
  cambios_unidos <- if (nrow(eventos_todos) > 0) { subs_in <- eventos_todos %>% filter(tipo_evento == "substitution"); subs_out <- eventos_todos %>% filter(tipo_evento == "substitution_out"); if (nrow(subs_in) > 0 && nrow(subs_out) > 0) { inner_join(subs_in, subs_out, by = c("minuto", "equipo"), suffix = c("_in", "_out")) } else { tibble() } } else { tibble() }
  crear_df_cambios <- function(cambios, equipo_filtro) { if(nrow(cambios) == 0) return(tibble(minuto = integer(), texto = character())); df <- filter(cambios, equipo == equipo_filtro); if (nrow(df) == 0) return(tibble(minuto = integer(), texto = character())); df %>% transmute(minuto = minuto, texto = paste0("  - Min ", minuto, ": Entra ", nombre_in, " (", dorsal_in, ") por ", nombre_out, " (", dorsal_out, ")")) %>% arrange(minuto) }
  cambios_local_df <- crear_df_cambios(cambios_unidos, equipo_local)
  cambios_visitante_df <- crear_df_cambios(cambios_unidos, equipo_visitante)
  
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


#### 4. LECTURA DE CONFIGURACIÓN Y GESTIÓN DE CACHÉ ####

if (!file.exists(ruta_config_web)) stop(paste("El archivo de configuración no se encuentra en:", ruta_config_web))
config_df <- read_csv(ruta_config_web, show_col_types = FALSE)
partidos_a_procesar_df <- config_df %>%
  rowwise() %>%
  reframe(id_partido = as.character(seq(start_id, end_id)), competition_name, season) %>%
  distinct()

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
    resultados_procesados[[id_partido_actual]] <- resultado_scrape
    
    if (is.null(resultado_scrape$error)) {
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
      }
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
  resultados_finales <- c(resultados_cacheados, nuevos_resultados)
  saveRDS(resultados_finales, file = ruta_cache_web)
  message(paste("\nCaché web actualizada con", length(nuevos_resultados), "nuevos informes."))
  message(paste("Total de informes en web_cache.rds:", length(resultados_finales)))
}

message("\nProceso de scraping y cacheo web completado.")