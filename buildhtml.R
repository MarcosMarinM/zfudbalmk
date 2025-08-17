################################################################################
##                                                                            ##
##           HTML REPORT GENERATION SCRIPT - CONTINUATION                     ##
##                                                                            ##
################################################################################


#### 7. INITIAL SETUP AND ENVIRONMENT CONFIGURATION ####

### 7.1. Load Packages
# 7.1.1. Load necessary packages for HTML report generation.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, tidyr, purrr, htmltools, stringr, jsonlite, readxl
)


#### 8. HELPER FUNCTIONS AND TEMPLATE DEFINITIONS ####

### 8.1. Text Formatting and Sanitization Functions

#' @title Generate a comprehensive yet optimized set of search terms.
#' @description This function uses a "linear expansion" strategy to avoid
#' combinatorial explosion. Instead of creating permutations of permutations, it
#' generates a new term for each substitution rule applied to the original name.
#' This maintains the richness of search variants desired by the user
#' without the search index size growing exponentially.
#' @param nombre A string with the name in Cyrillic.
#' @return A string with all unique and optimized search terms separated by spaces.
generar_terminos_busqueda <- function(nombre) {
  if (is.na(nombre) || nchar(trimws(nombre)) == 0) return("")
  
  nombre_lower <- tolower(nombre)
  
  # 8.1.1. Container for all generated terms.
  all_terms <- c(nombre_lower)
  
  # 8.1.2. User-provided variant lists.
  map_base <- c(
    'б'='b', 'в'='v', 'г'='g', 'д'='d', 'з'='z', 'и'='i',
    'к'='k', 'м'='m', 'н'='n', 'о'='o', 'п'='p', 'р'='r',
    'т'='t', 'ф'='f', 'х'='h'
  )
  
  mapa_variaciones <- list(
    'а' = c('a', 'ah'), 'с' = c('s', 'ss', 'ß'),
    'ч' = c('č', 'ch', 'c', 'ç', 'cz', 'tch'),
    'ш' = c('š', 'sh', 's', 'sch', 'x'),
    'ж' = c('ž', 'zh', 'z', 'x', 'j', 'gs'),
    'ѓ' = c('ǵ', 'gj', 'đ', 'g', 'dj', 'gh', 'dgh'),
    'ќ' = c('ḱ', 'kj', 'ć', 'q', 'k', 'c', 'qu', 'ky'), 
    'њ' = c('ń', 'nj', 'ñ', 'n', 'ny', 'nh'),
    'љ' = c('ĺ', 'lj', 'll', 'l', 'ly', 'gl'),
    'у' = c('u', 'y', 'oo', 'w'),
    'л' = c('l', 'll', 'el'),
    'е' = c('e', 'ë', 'ye', 'ie', 'ea'),
    'ц' = c('c', 'ts', 'tz', 'z', 'cz'),
    'ѕ' = c('dz', 'z', 'ds'),
    'џ' = c('dž', 'dzh', 'xh', 'dz', 'dj', 'j', 'chj'),
    'ј' = c('j', 'y', 'i', 'g')
  )
  
  # 8.1.3. Linear Generation Process.
  
  # 8.1.4. Step 1: Add the base transliteration.
  all_terms <- c(all_terms, str_replace_all(nombre_lower, map_base))
  
  # 8.1.5. Step 2: Linear expansion. For each rule in `mapa_variaciones`, create a new
  # 8.1.6. term from the ORIGINAL name.
  for (char_cyrillic in names(mapa_variaciones)) {
    # 8.1.7. Optimization: only process if the character exists in the name.
    if (str_detect(nombre_lower, fixed(char_cyrillic))) {
      for (variant in mapa_variaciones[[char_cyrillic]]) {
        new_term <- str_replace_all(nombre_lower, fixed(char_cyrillic), variant)
        all_terms <- c(all_terms, new_term)
      }
    }
  }
  
  # 8.1.8. Step 3: Apply final simplification to all generated terms.
  map_ascii_simplification <- c(
    'č'='c', 'š'='s', 'ž'='z', 'đ'='d', 'ć'='c', 'ǵ'='g',
    'ḱ'='k', 'ń'='n', 'ĺ'='l', 'ñ'='n', 'ë'='e', 'ç'='c', 'q'='k', 'x'='z', 'ß'='s'
  )
  
  simplified_terms <- str_replace_all(all_terms, map_ascii_simplification)
  
  # 8.1.9. Step 4: Combine, remove duplicates, and return.
  final_terms <- unique(c(all_terms, simplified_terms))
  return(paste(final_terms, collapse = " "))
}


#' @title Create a safe identifier for use in URLs and filenames.
#' @description Transliterates, converts to lowercase, replaces spaces, and removes invalid characters.
#' @param nombre The original string.
#' @return A sanitized string.
generar_id_seguro <- function(nombre) {
  # 8.1.10. "Flat" transliteration map for simple and readable URL IDs.
  map_id_plain <- c(
    'а'='a', 'б'='b', 'в'='v', 'г'='g', 'д'='d', 'ѓ'='g', 'е'='e', 
    'ж'='z', 'з'='z', 'ѕ'='dz', 'и'='i', 'ј'='j', 'к'='k', 'л'='l', 
    'љ'='lj', 'м'='m', 'н'='n', 'њ'='n', 'о'='o', 'п'='p', 'р'='r', 
    'с'='s', 'т'='t', 'ќ'='kj', 'у'='u', 'ф'='f', 'х'='h', 'ц'='c', 
    'ч'='c', 'џ'='dz', 'ш'='s'
  )
  nombre_latin <- str_replace_all(tolower(nombre), map_id_plain)
  id_sanitizada <- str_replace_all(nombre_latin, "[\\s/]+", "_")
  id_sanitizada <- str_replace_all(id_sanitizada, "[^a-z0-9_\\-]+", "")
  id_sanitizada <- str_replace_all(id_sanitizada, "_{2,}", "_")
  id_sanitizada <- str_replace_all(id_sanitizada, "^_+|_+$", "")
  return(id_sanitizada)
}

#' @title Get the ISO 3166-1 alpha-2 code for a national team.
#' @param team_name_mk Team name in Macedonian.
#' @return The ISO code (e.g., "mk", "gb") or NA if it's not a known national team.
get_national_team_iso <- function(team_name_mk) {
  # 8.1.11. Special case for "Македонија".
  if (team_name_mk == "Македонија") {
    return("mk")
  }
  
  # 8.1.12. Search in the nationality mapping.
  if (!is.null(mapeo_completo_df) && nrow(mapeo_completo_df) > 0) {
    match_row <- mapeo_completo_df %>%
      filter(nombre_macedonio == team_name_mk) %>%
      head(1) # 8.1.13. Take the first match.
    
    if (nrow(match_row) > 0 && !is.na(match_row$codigo_iso)) {
      return(tolower(match_row$codigo_iso)) # 8.1.14. Return in lowercase for the flag URL.
    }
  }
  return(NA_character_) # 8.1.15. Not a national team or ISO code not found.
}

### 8.2. Data Manipulation Functions

#' @title Apply a conversion map (dictionary) to dataframe columns.
#' @param df The dataframe to modify.
#' @param columnas Vector of column names to apply the map to.
#' @param mapa_conversiones A named vector (original=corrected).
#' @return The modified dataframe.
aplicar_conversiones <- function(df, columnas, mapa_conversiones) {
  if (is.null(mapa_conversiones) || nrow(df) == 0) return(df)
  df %>%
    mutate(across(any_of(columnas), ~ recode(.x, !!!mapa_conversiones)))
}

#' @title Reorder names from "Lastname Firstname" to "Firstname Lastname" idempotently.
#' @description This function uses heuristics to detect if a two-word name is in
#' the "Lastname Firstname" format by checking for common Macedonian feminine
#' surname endings. It only swaps the words if this pattern is detected, otherwise
#' it returns the name unchanged, ensuring idempotency.
#' @param nombres A vector of strings with names.
#' @return A vector with the correctly and permanently ordered names.
reordenar_nombre_idempotente <- function(nombres) {
  sapply(nombres, function(nombre) {
    if (is.na(nombre) || !stringr::str_detect(nombre, "\\s+")) {
      # Return immediately if NA, empty, or a single word.
      return(nombre) 
    }
    palabras <- stringr::str_split(nombre, "\\s+")[[1]]
    
    # The simple inversion logic
    primer_nombre <- palabras[length(palabras)]
    apellido <- paste(palabras[-length(palabras)], collapse = " ")
    return(paste(primer_nombre, apellido))
    
  }, USE.NAMES = FALSE)
}


### 8.2.bis. Calculate Current Season

#' @title Get the current season string based on system date.
#' @description Determines the current football season (e.g., "24/25"). The season
#' is considered to start in August.
#' @return A character string representing the current season.
obtener_temporada_actual <- function() {
  fecha_actual <- Sys.Date()
  ano_actual <- as.numeric(format(fecha_actual, "%Y"))
  mes_actual <- as.numeric(format(fecha_actual, "%m"))
  
  ano_inicio_temporada <- if (mes_actual >= 8) {
    ano_actual
  } else {
    ano_actual - 1
  }
  
  ano_inicio_corto <- substr(as.character(ano_inicio_temporada), 3, 4)
  ano_fin_corto <- substr(as.character(ano_inicio_temporada + 1), 3, 4)
  
  return(paste0(ano_inicio_corto, "/", ano_fin_corto))
}


### 8.3. HTML Component Generation Functions

#' @title Create the language selector.
#' @description Generates links to switch between supported languages.
#' @param idioma_pagina_actual The language code of the current page (e.g., "mk").
#' @return An htmltools `div` object with the language links.
crear_selector_idioma <- function(idioma_pagina_actual) {
  tags$div(
    class = "language-selector",
    style = "text-align: right; margin-bottom: 15px; font-size: 0.9em;",
    tagList(
      lapply(seq_along(IDIOMAS_SOPORTADOS), function(i) {
        lang_code <- IDIOMAS_SOPORTADOS[i]
        
        # 8.3.1. Get the language name from the target language's dictionary for display.
        lang_name <- textos[[lang_code]][["lang_name"]] %||% lang_code
        
        tag_element <- if (lang_code == idioma_pagina_actual) {
          tags$span(style = "font-weight: bold; color: #333;", paste0("[ ", lang_name, " ]"))
        } else {
          # 8.3.2. The JS dynamically replaces the language code in the current URL.
          js_onclick <- sprintf(
            "window.location.href = window.location.href.replace('/%s/', '/%s/'); return false;",
            idioma_pagina_actual, 
            lang_code
          )
          tags$a(href = "#", onclick = js_onclick, paste0("[ ", lang_name, " ]"))
        }
        
        if (i < length(IDIOMAS_SOPORTADOS)) { tagList(tag_element, " ") } else { tag_element }
      })
    )
  )
}


#' @title Create the standard navigation buttons (Back, Home).
#' @param path_to_lang_root Relative path to the current language root (e.g., '..').
#' @return An htmltools `div` object.
crear_botones_navegacion <- function(path_to_lang_root = ".") {
  tags$div(class = "nav-buttons",
           tags$a(t("back_button"), href = "#", onclick = "history.back(); return false;", class = "back-link"),
           tags$a(t("home_button"), href = file.path(path_to_lang_root, "index.html"), class = "back-link")
  )
}

#' @title Create the base structure of an HTML page (template).
#' @param contenido_principal The main content of the page (an htmltools object).
#' @param titulo_pagina The title that will appear in the browser tab (already translated).
#' @param path_to_root_dir Relative path to the root 'docs/' directory.
#' @param script_contraseña The script tag for password protection.
#' @return A complete htmltools `html` object.
crear_pagina_html <- function(contenido_principal, titulo_pagina, path_to_root_dir = ".", script_contraseña) {
  tags$html(lang = idioma_actual,
            tags$head(
              tags$meta(charset="UTF-8"),
              tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
              tags$title(titulo_pagina),
              tags$link(rel = "stylesheet", href = file.path(path_to_root_dir, nombres_carpetas_relativos$assets, "style.css")),
              script_contraseña
            ),
            tags$body(
              `data-search-results-title` = t("search_results_for"),
              `data-no-search-results-msg` = t("no_search_results"),
              `data-search-prompt-msg` = t("search_prompt"),
              tags$div(class = "container",
                       crear_selector_idioma(idioma_pagina_actual = idioma_actual),
                       tags$h1(tags$a(href = file.path(path_to_root_dir, idioma_actual, "index.html"), style = "color: inherit; text-decoration: none;", t("site_title"))),
                       tags$div(class = "search-container",
                                tags$form(action = "#", onsubmit = "showSearchResults(); return false;",
                                          tags$input(type = "text", id = "search-input", class = "search-input", placeholder = t("search_placeholder"), onkeyup = "handleSearchInput(event)"),
                                          tags$button(type = "submit", class = "search-button", t("search_button"))
                                ),
                                tags$div(id = "search-suggestions")
                       ),
                       tags$div(id = "main-content",
                                contenido_principal
                       )
              ),
              tags$script(defer = NA, src = file.path(path_to_root_dir, nombres_carpetas_relativos$assets, "script.js"))
            )
  )
}


### 8.4. Function to Generate Match Timeline

#' @title Generate a dataframe with the timeline of match events (goals, cards, changes).
#' @param id_p The match ID.
#' @param resumen_partido The results list for a match, generated by `procesar_acta`.
#' @param entidades_lang_df The entities dataframe already filtered for the current language.
#' @param jugadoras_lang_df The players dataframe already filtered for the current language.
#' @return A dataframe sorted by minute with the match events.
generar_cronologia_df <- function(id_p, resumen_partido, entidades_lang_df, jugadoras_lang_df) {
  lista_eventos <- list()
  path_jugadoras <- file.path("..", nombres_carpetas_relativos$jugadoras)
  path_timovi <- file.path("..", nombres_carpetas_relativos$timovi)
  
  # 8.4.1. Get the flag indicating if it's a national team match.
  # 8.4.2. We extract it from the `partidos_df` dataframe which already contains this logic.
  es_partido_seleccion <- (partidos_df %>% filter(id_partido == id_p) %>% pull(es_partido_seleccion))[1]
  if (is.na(es_partido_seleccion)) {
    es_partido_seleccion <- FALSE
  }
  
  # 8.4.3. Internal helper function to generate conditional links.
  # 8.4.4. It decides if an entity (player or team) should have a link.
  crear_link_condicional <- function(nombre_a_mostrar, ruta_base, id_seguro, nombre_equipo_original_mk) {
    # 8.4.5. A link is created IF:
    # 8.4.6. 1. It is NOT a national team match.
    # 8.4.7. OR
    # 8.4.8. 2. It IS a national team match AND the entity belongs to "Македонија".
    debe_ser_enlazable <- !es_partido_seleccion || (es_partido_seleccion && nombre_equipo_original_mk == "Македонија")
    
    if (debe_ser_enlazable && !is.na(id_seguro) && nchar(id_seguro) > 0) {
      return(sprintf("<a href='%s/%s.html'>%s</a>", ruta_base, id_seguro, nombre_a_mostrar))
    } else {
      # 8.4.9. Otherwise, just return the name as plain text.
      return(nombre_a_mostrar)
    }
  }
  
  # 8.4.10. Step 1: Goals
  goles_data <- goles_df_unificado %>% 
    filter(id_partido == id_p) %>%
    left_join(jugadoras_lang_df, by = "id")
  
  if (!is.null(goles_data) && nrow(goles_data) > 0) {
    goles_eventos <- goles_data %>% 
      mutate(
        equipo_jugadora_lang = entidades_df_lang$current_lang_name[match(equipo_jugadora, entidades_df_lang$original_name)],
        equipo_acreditado_lang = entidades_df_lang$current_lang_name[match(equipo_acreditado, entidades_df_lang$original_name)],
        texto_evento = pmap_chr(list(tipo, id, PlayerName, equipo_jugadora, equipo_jugadora_lang, equipo_acreditado, equipo_acreditado_lang), function(t, id_j, j_lang, ej_mk, ej_lang, ea_mk, ea_lang) {
          
          # 8.4.11. Conditional logic for player and teams.
          link_jugadora <- crear_link_condicional(j_lang, path_jugadoras, id_j, ej_mk)
          link_equipo_jugadora <- crear_link_condicional(ej_lang, path_timovi, generar_id_seguro(ej_mk), ej_mk)
          link_equipo_acreditado <- crear_link_condicional(ea_lang, path_timovi, generar_id_seguro(ea_mk), ea_mk)
          
          if (t == "Autogol") { 
            sprintf(t("match_timeline_own_goal"), link_jugadora, link_equipo_jugadora, link_equipo_acreditado) 
          } else { 
            sprintf(t("match_timeline_goal"), link_jugadora, link_equipo_acreditado) 
          }
        })
      ) %>% select(minuto, icono = tipo, texto_evento) %>% mutate(icono = recode(icono, "Normal" = "⚽", "Autogol" = "⚽"))
    lista_eventos[[length(lista_eventos) + 1]] <- goles_eventos
  }
  
  # 8.4.12. Step 2: Cards
  tarjetas_data <- tarjetas_df_unificado %>% 
    filter(id_partido == id_p) %>%
    left_join(jugadoras_lang_df, by = "id") %>%
    left_join(entidades_df_lang, by = c("equipo" = "original_name"))
  
  if (!is.null(tarjetas_data) && nrow(tarjetas_data) > 0) {
    tarjetas_eventos <- tarjetas_data %>%
      mutate(
        texto_evento = pmap_chr(list(id, PlayerName, equipo, current_lang_name), function(id_j, j_lang, e_mk, e_lang) {
          
          # 8.4.13. Conditional logic for player and team.
          link_jugadora <- crear_link_condicional(j_lang, path_jugadoras, id_j, e_mk)
          link_equipo <- crear_link_condicional(e_lang, path_timovi, generar_id_seguro(e_mk), e_mk)
          
          sprintf(t("match_timeline_card"), link_jugadora, link_equipo)
        }),
        icono = if_else(tipo == "Amarilla", "🟨", "🟥")
      ) %>% select(minuto, icono, texto_evento)
    lista_eventos[[length(lista_eventos) + 1]] <- tarjetas_eventos
  }
  
  # 8.4.14. Step 3: Substitutions
  procesar_cambios <- function(cambios_df, nombre_equipo_mk, alineacion_equipo) {
    if (is.null(cambios_df) || nrow(cambios_df) == 0 || is.null(alineacion_equipo) || nrow(alineacion_equipo) == 0) return(NULL)
    map_dfr(1:nrow(cambios_df), function(i) {
      cambio <- cambios_df[i,]
      match_info <- str_match(cambio$texto, "Entra (.*?) \\((\\d+)\\) por (.*?) \\((\\d+)\\)")
      if (is.na(match_info[1,1])) return(NULL)
      nombre_entra_raw <- match_info[1, 2]; dorsal_entra <- as.numeric(match_info[1, 3])
      nombre_sale_raw <- match_info[1, 4]; dorsal_sale <- as.numeric(match_info[1, 5])
      
      # 8.4.15. More robust join to find IDs.
      id_entra <- (alineacion_equipo %>% filter(dorsal == dorsal_entra))$id[1]
      id_sale <- (alineacion_equipo %>% filter(dorsal == dorsal_sale))$id[1]
      
      nombre_entra_lang <- (jugadoras_lang_df %>% filter(id == id_entra))$PlayerName[1] %||% nombre_entra_raw
      nombre_sale_lang <- (jugadoras_lang_df %>% filter(id == id_sale))$PlayerName[1] %||% nombre_sale_raw
      nombre_equipo_lang <- (entidades_df_lang %>% filter(original_name == nombre_equipo_mk))$current_lang_name[1]
      
      # 8.4.16. Conditional logic for players and team.
      link_entra <- crear_link_condicional(nombre_entra_lang, path_jugadoras, id_entra, nombre_equipo_mk)
      link_sale <- crear_link_condicional(nombre_sale_lang, path_jugadoras, id_sale, nombre_equipo_mk)
      link_equipo <- crear_link_condicional(nombre_equipo_lang, path_timovi, generar_id_seguro(nombre_equipo_mk), nombre_equipo_mk)
      
      texto_final <- sprintf(t("match_timeline_substitution"), link_equipo, link_entra, dorsal_entra, link_sale, dorsal_sale)
      tibble(minuto = cambio$minuto, icono = "🔄", texto_evento = texto_final)
    })
  }
  alineacion_partido <- apariciones_df %>% filter(id_partido == id_p)
  cambios_local_eventos <- procesar_cambios(resumen_partido$cambios_local, resumen_partido$partido_info$local, filter(alineacion_partido, equipo == resumen_partido$partido_info$local))
  cambios_visitante_eventos <- procesar_cambios(resumen_partido$cambios_visitante, resumen_partido$partido_info$visitante, filter(alineacion_partido, equipo == resumen_partido$partido_info$visitante))
  
  lista_eventos <- c(lista_eventos, list(cambios_local_eventos), list(cambios_visitante_eventos))
  lista_eventos_compacta <- purrr::compact(lista_eventos)
  if (length(lista_eventos_compacta) == 0) { return(tibble(minuto = integer(), icono = character(), texto_evento = character())) }
  bind_rows(lista_eventos_compacta) %>% filter(!is.na(minuto)) %>% arrange(minuto)
}


#' @title Create a link for a team only if it is not a national team.
#' @param nombre_equipo_mk The original team name in Macedonian.
#' @param nombre_equipo_lang The team name in the current language.
#' @param path_to_root Relative path to the teams folder (e.g., "..").
#' @return An htmltools `<a>` tag if it's a club, or a `<span>` if it's a national team.
crear_enlace_equipo_condicional <- function(nombre_equipo_mk, nombre_equipo_lang, path_to_root = "..") {
  # 8.4.17. Use the existing function to determine if it's a national team.
  es_seleccion_nacional <- !is.na(get_national_team_iso(nombre_equipo_mk))
  
  if (es_seleccion_nacional) {
    # 8.4.18. If it's a national team, return the name as plain text (inside a span for consistency).
    return(tags$span(nombre_equipo_lang))
  } else {
    # 8.4.19. If it's a club, create the hyperlink to its profile page.
    ruta_perfil_equipo <- file.path(path_to_root, nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(nombre_equipo_mk), ".html"))
    return(tags$a(href = ruta_perfil_equipo, nombre_equipo_lang))
  }
}


#' @title Create a self-contained, horizontally scrollable HTML block for a competition's latest results.
#' @description Generates the competition title (clickable to menu), a link to all results (clickable to schedule),
#' and a horizontally scrollable row of match blocks.
#' @param comp_info A row from `competiciones_unicas_df` for the target competition.
#' @param partidos_jornada_df A dataframe with the matches for the latest matchday.
#' @param comp_nombre_lang The translated name of the competition.
#' @param entidades_df_lang The dataframe for translating entity names.
#' @param ancho_ficha_px The calculated width in pixels for each match block.
#' @return An htmltools `div` object with the complete component.
crear_bloque_resultados_competicion <- function(comp_info, partidos_jornada_df, comp_nombre_lang, entidades_df_lang, ancho_ficha_px) {
  
  # Ruta a la página de MENÚ de la competición (para el título principal)
  ruta_competicion_completa <- file.path(nombres_carpetas_relativos$competiciones, paste0(comp_info$competicion_id, ".html"))
  
  # ¡AQUÍ ESTÁ LA CORRECCIÓN CLAVE!
  # Nueva ruta que apunta directamente a la página de CALENDARIO (Распоред)
  ruta_raspored <- file.path(
    nombres_carpetas_relativos$competiciones, 
    paste0(comp_info$competicion_id, "_", nombres_archivos_mk$partidos, ".html")
  )
  
  # Helper to create each individual match block (vertical layout)
  crear_item_partido_vertical <- function(partido) {
    local_name <- entidades_df_lang$current_lang_name[match(partido$local, entidades_df_lang$original_name)]
    visitante_name <- entidades_df_lang$current_lang_name[match(partido$visitante, entidades_df_lang$original_name)]
    
    logo_local_path <- paste0(generar_id_seguro(partido$local), ".png")
    if (!file.exists(file.path(RUTA_LOGOS_DESTINO, logo_local_path))) { logo_local_path <- "NOLOGO.png" }
    
    logo_visitante_path <- paste0(generar_id_seguro(partido$visitante), ".png")
    if (!file.exists(file.path(RUTA_LOGOS_DESTINO, logo_visitante_path))) { logo_visitante_path <- "NOLOGO.png" }
    
    ruta_partido_html <- if(!is.na(partido$id_partido)) file.path(nombres_carpetas_relativos$partidos, paste0(partido$id_partido, ".html")) else "#"
    
    fila_local <- tags$div(class="match-row",
                           tags$div(class="team-info", tags$img(src = file.path("..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, logo_local_path), class="logo"), tags$span(class="name", local_name)),
                           if(!is.na(partido$id_partido)) tags$span(class="score", partido$goles_local) else NULL
    )
    fila_visitante <- tags$div(class="match-row",
                               tags$div(class="team-info", tags$img(src = file.path("..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, logo_visitante_path), class="logo"), tags$span(class="name", visitante_name)),
                               if(!is.na(partido$id_partido)) tags$span(class="score", partido$goles_visitante) else NULL
    )
    
    tags$a(
      class = if(!is.na(partido$id_partido)) "match-block" else "match-block placeholder",
      href = ruta_partido_html,
      style = paste0("width: ", ancho_ficha_px, "px;"),
      tags$div(class="match-round", paste(t("round_prefix_short"), partido$jornada)),
      tags$div(class="match-content",
               fila_local,
               if(is.na(partido$id_partido)) tags$span(class="score-separator-placeholder", "-") else tags$span(class="score-separator", "-"),
               fila_visitante
      )
    )
  }
  
  # Main block structure
  tags$div(class = "competition-results-block",
           tags$div(class = "results-header",
                    # El título principal sigue apuntando a la ruta del MENÚ
                    tags$a(href = ruta_competicion_completa, class="competition-title-link",
                           tags$h2(class = "competition-title", comp_nombre_lang)
                    ),
                    # El enlace "Сите резултати" ahora apunta a la nueva ruta del CALENDARIO
                    tags$a(href = ruta_raspored, class = "results-link", paste0(t("all_results_link"), " >"))
           ),
           tags$hr(class="separator"),
           tags$div(class = "results-scroll-container",
                    tags$div(class = "results-row",
                             map(1:nrow(partidos_jornada_df), ~crear_item_partido_vertical(partidos_jornada_df[.x,]))
                    )
           )
  )
}


#' @title Create a simplified mini-standings table with a clickable header.
#' @description Generates a container with a clickable competition title and a compact HTML table.
#' @param comp_info A row from `competiciones_unicas_df`.
#' @param stats_clasificacion_por_comp_df Master standings data.
#' @param entidades_df_lang Translated entity names.
#' @param abreviaturas_lang_df Abbreviation data for the current language.
#' @param estilos_data Standings color styles data.
#' @param comp_nombre_lang The translated name of the competition for the header.
#' @return An htmltools `div` object for the entire column (header + table).
crear_tabla_clasificacion_portada <- function(comp_info, stats_clasificacion_por_comp_df, entidades_df_lang, abreviaturas_lang_df, estilos_data, comp_nombre_lang) {
  
  clasificacion_comp <- stats_clasificacion_por_comp_df %>%
    filter(competicion_id == comp_info$competicion_id)
  
  if(nrow(clasificacion_comp) == 0) return(NULL)
  
  clasificacion_final_df <- clasificacion_comp %>%
    left_join(entidades_df_lang, by = c("team" = "original_name")) %>%
    left_join(abreviaturas_lang_df, by = c("team" = "original_mk")) %>%
    mutate(
      final_abbr = coalesce(abbreviation, toupper(substr(current_lang_name, 1, 3)))
    )
  
  clave_estilo_comp <- paste(comp_info$competicion_nombre, comp_info$competicion_temporada)
  estilos_comp <- estilos_data[[clave_estilo_comp]]
  
  # ¡NUEVO! Ruta a la página de la TABLA COMPLETA de la competición.
  ruta_tabla_completa <- file.path(
    nombres_carpetas_relativos$competiciones, 
    paste0(comp_info$competicion_id, "_", nombres_archivos_mk$clasificacion, ".html")
  )
  
  # El contenedor principal ahora incluye el título y la tabla.
  tags$div(class="mini-standings-container",
           # ¡NUEVO! Título clicable de la competición.
           tags$a(href = ruta_tabla_completa, class="mini-standings-title-link",
                  tags$h4(class="mini-standings-title", comp_nombre_lang)
           ),
           
           # La tabla existente, sin cambios.
           tags$table(class="mini-standings-table",
                      tags$thead(tags$tr(
                        tags$th(t("mini_standings_pos")),
                        tags$th(t("mini_standings_team"), colspan="2"),
                        tags$th(t("mini_standings_p")),
                        tags$th(t("mini_standings_pts"))
                      )),
                      tags$tbody(
                        map(1:nrow(clasificacion_final_df), function(i) {
                          fila <- clasificacion_final_df[i, ]
                          logo_path <- paste0(generar_id_seguro(fila$team), ".png")
                          if (!file.exists(file.path(RUTA_LOGOS_DESTINO, logo_path))) { logo_path <- "NOLOGO.png" }
                          
                          estilo_borde <- ""
                          if (!is.null(estilos_comp)) {
                            regla_match <- estilos_comp$reglas %>% filter(puesto == fila$Pos)
                            if (nrow(regla_match) > 0) {
                              estilo_borde <- paste0("border-left: 3px solid ", regla_match[1, "color"], ";")
                            }
                          }
                          
                          tags$tr(
                            tags$td(style = estilo_borde, class="pos-cell", fila$Pos),
                            tags$td(class="logo-cell", tags$img(class="mini-logo", src = file.path("..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, logo_path))),
                            tags$td(class="abbr-cell", fila$final_abbr),
                            tags$td(class="pj-cell", fila$P),
                            tags$td(class="pts-cell", fila$Pts)
                          )
                        })
                      )
           )
  )
}


# ============================================================================ #
# ==                      PASSWORD PROTECTION SWITCH                        == #
# ============================================================================ #
# Change to TRUE to generate the website with an access password.
# Change to FALSE to generate a public-access website with no password.
PROTEGER_CON_CONTRASENA <- FALSE 
# ============================================================================ #

### 8.5. Language and Translation Management

# 8.5.1. Load translations from an external file.
ruta_traducciones <- "translations.txt"
textos <- list()
IDIOMAS_SOPORTADOS <- character(0)

if (!file.exists(ruta_traducciones)) {
  warning("The 'translations.txt' file was not found. Multilanguage functionality will be limited.")
} else {
  tryCatch({
    # 8.5.2. Read the CSV file ensuring UTF-8 encoding.
    traducciones_df <- read.csv(ruta_traducciones, stringsAsFactors = FALSE, encoding = "UTF-8")
    
    # 8.5.3. Supported languages are the column names, except the first ('key').
    IDIOMAS_SOPORTADOS <- names(traducciones_df)[-1]
    
    # 8.5.4. Transform the dataframe from wide to long format.
    traducciones_long_df <- traducciones_df %>%
      pivot_longer(
        cols = -key,
        names_to = "lang",
        values_to = "translation"
      )
    
    # 8.5.5. Create the nested list `textos` from the long dataframe.
    textos <- traducciones_long_df %>%
      split(.$lang) %>%
      map(~ setNames(as.list(.$translation), .$key))
    
    message(paste("Translations loaded successfully for languages:", paste(IDIOMAS_SOPORTADOS, collapse = ", ")))
    
  }, error = function(e) {
    warning(paste("Error reading or processing the translations file:", e$message))
  })
}

# 8.5.6. If no languages were loaded, set a minimal fallback so the script doesn't fail.
if (length(IDIOMAS_SOPORTADOS) == 0) {
  IDIOMAS_SOPORTADOS <- c("mk") # 8.5.7. Default language.
}

# 8.5.8. Global variable for the current language and helper function for translations.
# 8.5.9. This will be set inside the main generation loop.
idioma_actual <- IDIOMAS_SOPORTADOS[1]

#' @title Helper function to get translated text.
#' @description Accesses the 'textos' dictionary and returns the string for the
#' current language. If a key doesn't exist for a language, it returns the key
#' itself as a fallback.
#' @param key The key of the text string to translate.
#' @return The translated string.
t <- function(key) {
  traduccion <- textos[[idioma_actual]][[key]]
  if (is.null(traduccion)) {
    # 8.5.10. Fallback: try to get from the default language.
    traduccion_fallback <- textos[[IDIOMAS_SOPORTADOS[1]]][[key]]
    if (is.null(traduccion_fallback)) {
      warning(paste("Translation key not found in any language:", key))
      return(key) # 8.5.11. Return the key if not found anywhere.
    }
    return(traduccion_fallback)
  }
  return(traduccion)
}

### 8.6. Path Definitions and Multilingual Directory Creation

# 8.6.1. Define base folder and file names.
# 8.6.2. Names are defined in Macedonian to maintain internal consistency.
# 8.6.3. These will be the folder names INSIDE each language directory.
nombres_carpetas_relativos <- list(
  assets = "assets", 
  competiciones = "natprevaruvanja", 
  partidos = "natprevari", 
  jugadoras = "igraci", 
  timovi = "timovi", 
  arbitros = "sudii", 
  estadios = "stadioni",
  logos = "logos"
)

nombres_archivos_mk <- list(
  partidos = "raspored", 
  clasificacion = "tabela", 
  goleadoras = "strelci", 
  sanciones = "disciplinska"
)

# 8.6.4. Create the directory structure.
# 8.6.5. Root directory for the entire website output.
RUTA_SALIDA_RAIZ <- "docs"
dir.create(RUTA_SALIDA_RAIZ, showWarnings = FALSE, recursive = TRUE)

# 8.6.6. Directory for shared assets (CSS, JS, Logos).
RUTA_ASSETS_COMPARTIDOS <- file.path(RUTA_SALIDA_RAIZ, nombres_carpetas_relativos$assets)
RUTA_LOGOS_DESTINO <- file.path(RUTA_ASSETS_COMPARTIDOS, nombres_carpetas_relativos$logos)
dir.create(RUTA_ASSETS_COMPARTIDOS, showWarnings = FALSE, recursive = TRUE)
dir.create(RUTA_LOGOS_DESTINO, showWarnings = FALSE, recursive = TRUE)

# 8.6.7. Loop to create the folder structure for each supported language.
for (lang in IDIOMAS_SOPORTADOS) {
  ruta_base_lang <- file.path(RUTA_SALIDA_RAIZ, lang)
  
  # 8.6.8. Create language-specific folders (competitions, matches, etc.).
  walk(nombres_carpetas_relativos[c("competiciones", "partidos", "jugadoras", "timovi", "arbitros", "estadios")], 
       ~ dir.create(file.path(ruta_base_lang, .x), showWarnings = FALSE, recursive = TRUE))
}
message("Multilingual directory structure created in: ", RUTA_SALIDA_RAIZ)

# 8.6.9. Copy shared assets (Logos).
ruta_logos_fuente <- "Logos"
if (dir.exists(ruta_logos_fuente)) {
  archivos_logo_fuente <- list.files(ruta_logos_fuente, pattern = "\\.png$", full.names = TRUE)
  
  if (length(archivos_logo_fuente) > 0) {
    # 8.6.10. Iterate over each logo, sanitize its name, and copy it to the shared 'assets' folder.
    walk(archivos_logo_fuente, function(ruta_completa_fuente) {
      nombre_archivo_original <- basename(ruta_completa_fuente)
      
      nombre_archivo_destino <- if (nombre_archivo_original == "NOLOGO.png") {
        "NOLOGO.png"
      } else {
        nombre_base_sin_ext <- tools::file_path_sans_ext(nombre_archivo_original)
        nombre_sanitizado <- generar_id_seguro(nombre_base_sin_ext)
        paste0(nombre_sanitizado, ".png")
      }
      
      ruta_completa_destino <- file.path(RUTA_LOGOS_DESTINO, nombre_archivo_destino)
      file.copy(from = ruta_completa_fuente, to = ruta_completa_destino, overwrite = TRUE)
    })
    
    message(paste(length(archivos_logo_fuente), " logos copied to the shared assets folder: ", RUTA_LOGOS_DESTINO))
    if (!file.exists(file.path(ruta_logos_fuente, "NOLOGO.png"))) {
      warning("WARNING: The placeholder logo 'NOLOGO.png' was not found.")
    }
  } else {
    warning("The logos folder exists but contains no .png files.")
  }
} else {
  warning("The logos folder was not found. Logos will not be copied.")
}


#### 9. EXTERNAL DATA LOADING (CONFIGURATION AND MAPPINGS) ####

message("Starting HTML report generation...")

### 9.1. Load Classification Styles
message("Loading custom styles for tables...")

#' @title Parse a text file to define color styles for classification tables.
#' @param ruta_archivo Path to the styles file.
#' @return A nested list by competition with style rules and the legend.
parsear_estilos_clasificacion <- function(ruta_archivo) {
  if (!file.exists(ruta_archivo)) {
    warning(paste("Table styles file not found at:", ruta_archivo))
    return(list())
  }
  tryCatch({
    lineas <- readLines(ruta_archivo, warn = FALSE, encoding = "UTF-8")
    lista_estilos <- list()
    competicion_actual <- NULL
    for (linea in lineas) {
      linea <- trimws(linea)
      if (linea == "" || startsWith(linea, "#")) next
      if (startsWith(linea, "[COMPETICION:")) {
        competicion_actual <- str_match(linea, "\\[COMPETICION:\\s*(.*?)\\]$")[1, 2]
        if (!is.na(competicion_actual)) {
          lista_estilos[[competicion_actual]] <- list(reglas = data.frame(), leyenda = list())
        }
      } else if (!is.null(competicion_actual)) {
        partes <- str_split(linea, ",", n = 3)[[1]]
        if (length(partes) == 3) {
          puesto <- as.integer(trimws(partes[1]))
          color <- trimws(partes[2])
          texto_key <- trimws(partes[3]) 
          if (!is.na(puesto) && nchar(color) > 1 && nchar(texto_key) > 0) {
            regla_actual <- data.frame(puesto = puesto, color = color, texto_key = texto_key, stringsAsFactors = FALSE)
            lista_estilos[[competicion_actual]]$reglas <- rbind(lista_estilos[[competicion_actual]]$reglas, regla_actual)
            if (!any(sapply(lista_estilos[[competicion_actual]]$leyenda, function(l) l$color == color && l$texto_key == texto_key))) {
              lista_estilos[[competicion_actual]]$leyenda[[length(lista_estilos[[competicion_actual]]$leyenda) + 1]] <- list(color = color, texto_key = texto_key)
            }
          }
        }
      }
    }
    message("Table styles loaded successfully.")
    return(lista_estilos)
  }, error = function(e) {
    warning("Error loading the table styles file."); message("Error was: ", e$message)
    return(list())
  })
}
ruta_estilos_clasificacion <- "estilos_clasificacion.txt"
estilos_clasificacion_data <- parsear_estilos_clasificacion(ruta_estilos_clasificacion)


### 9.2. Load Nationality Mappings
message("Loading nationality mappings...")

ruta_mapeo_iso <- "nacionalidades_mapeo.txt"
ruta_traduccion_mk <- "nacionalidades_traduccion.txt"
mapeo_completo_df <- NULL

if (file.exists(ruta_mapeo_iso) && file.exists(ruta_traduccion_mk)) {
  tryCatch({
    mapeo_iso_df <- read.csv(ruta_mapeo_iso, stringsAsFactors = FALSE)
    traduccion_mk_df <- read.csv(ruta_traduccion_mk, stringsAsFactors = FALSE, encoding = "UTF-8")
    mapeo_completo_df <- merge(mapeo_iso_df, traduccion_mk_df, by = "nombre_ingles", all = TRUE)
    mapeo_completo_df$clave_lower <- tolower(trimws(mapeo_completo_df$nombre_ingles))
    message("Nationality mappings loaded successfully.")
  }, error = function(e) {
    warning("Error loading nationality mappings. Flag functionality will be disabled.")
    message("Error was: ", e$message)
  })
} else {
  warning("Nationality mapping files not found. Flag functionality will be disabled.")
}


### 9.3. Load Name Corrections
message("Loading name corrections file...")

ruta_conversiones <- "conversions.txt"
mapa_conversiones <- NULL
if (file.exists(ruta_conversiones)) {
  tryCatch({
    conversiones_df <- read.csv(ruta_conversiones, stringsAsFactors = FALSE, encoding = "UTF-8")
    mapa_conversiones <- setNames(conversiones_df$corregido, conversiones_df$original)
    message("Corrections file loaded.")
  }, error = function(e) {
    warning("Error loading conversions.txt. No corrections will be applied.")
  })
} else {
  message("conversions.txt file not found. Continuing without corrections.")
}

### 9.4. Load Dynamic Name Translations and Corrections
message("Loading dynamic name translations/corrections...")

# 9.4.1. Generic function to load a translation/correction file and convert it to a long format.
cargar_mapa_traduccion <- function(ruta_archivo) {
  if (!file.exists(ruta_archivo)) {
    message(paste("File", basename(ruta_archivo), "not found."))
    return(NULL)
  }
  tryCatch({
    df <- read.csv(ruta_archivo, stringsAsFactors = FALSE, encoding = "UTF-8", check.names = FALSE)
    
    # 9.4.2. 1. Identify the name of the first column (the key, e.g., 'mk').
    # 9.4.3. 2. Pivot using that column name dynamically.
    # 9.4.4. 3. Rename the key column to 'original_mk' for internal consistency.
    key_col_name <- names(df)[1]
    
    df %>%
      pivot_longer(
        cols = -all_of(key_col_name),
        names_to = "lang",
        values_to = "translated_name"
      ) %>%
      rename(original_mk = !!sym(key_col_name)) %>%
      # 9.4.5. Remove any unnecessary prefixes from the language codes.
      mutate(lang = str_remove(lang, "latin_|translation_"))
    
  }, error = function(e) {
    warning(paste("Error loading", basename(ruta_archivo), ":", e$message))
    return(NULL)
  })
}

mapa_nombres_jugadoras_long <- cargar_mapa_traduccion("name_corrections.txt")
mapa_nombres_entidades_long <- cargar_mapa_traduccion("entity_corrections.txt")
mapa_nombres_competiciones_long <- cargar_mapa_traduccion("competition_translations.txt")

### 9.5. Load Entity Name Corrections (Teams, Referees)
message("Loading entity name corrections file...")

ruta_correcciones_entidades <- "entity_corrections.txt"
mapa_correcciones_entidades <- NULL
if (file.exists(ruta_correcciones_entidades)) {
  tryCatch({
    correcciones_df <- read.csv(ruta_correcciones_entidades, stringsAsFactors = FALSE, encoding = "UTF-8")
    mapa_correcciones_entidades <- setNames(correcciones_df$latin_sq, correcciones_df$original_mk)
    message("Entity name corrections file loaded.")
  }, error = function(e) {
    warning("Error loading entity_corrections.txt.")
  })
} else {
  message("entity_corrections.txt file not found. Standard transliteration will be used.")
}

### 9.6. Load Competition Translations
message("Loading competition translations...")

ruta_traducciones_comp <- "competition_translations.txt"
mapa_traducciones_comp <- NULL
if (file.exists(ruta_traducciones_comp)) {
  tryCatch({
    traducciones_comp_df <- read.csv(ruta_traducciones_comp, stringsAsFactors = FALSE, encoding = "UTF-8")
    mapa_traducciones_comp <- setNames(traducciones_comp_df$translation_sq, traducciones_comp_df$original_mk)
    message("Competition translations loaded.")
  }, error = function(e) {
    warning("Error loading competition_translations.txt.")
  })
} else {
  message("competition_translations.txt file not found. Original names will be used.")
}

### 9.7. Load Calendars from Excel Files
message("Loading calendars from Excel files...")

#' @title Load future match calendars from Excel files.
#' @description Scans the 'Calendarios' folder, extracts competition and season
#' from the filename, reads the matches, and formats them into a dataframe
#' compatible with `partidos_df`.
#' @param ruta_carpeta_calendarios Path to the folder containing the Excel files.
#' @return A dataframe with "placeholder" matches.
cargar_calendarios_excel <- function(ruta_carpeta_calendarios = "Calendarios") {
  if (!dir.exists(ruta_carpeta_calendarios)) {
    message("The 'Calendarios' directory was not found. Skipping future match loading.")
    return(tibble())
  }
  
  archivos_excel <- list.files(
    path = ruta_carpeta_calendarios, 
    pattern = "\\.xlsx?$", 
    full.names = TRUE, 
    recursive = TRUE
  )
  
  if (length(archivos_excel) == 0) {
    message("No Excel files found in the 'Calendarios' directory.")
    return(tibble())
  }
  
  map_dfr(archivos_excel, function(ruta_archivo) {
    nombre_base <- tools::file_path_sans_ext(basename(ruta_archivo))
    
    # 9.7.1. Extract competition name and season from the filename.
    match_nombre <- str_match(nombre_base, "^(.*?)\\s+(\\d{2}_\\d{2})$")
    
    if (is.na(match_nombre[1, 1])) {
      warning(paste("Filename", basename(ruta_archivo), "is not in the expected format ('Competition Name YY_YY'). Skipping."))
      return(NULL)
    }
    
    comp_nombre <- str_trim(match_nombre[1, 2])
    comp_temporada <- str_replace(match_nombre[1, 3], "_", "/")
    
    message(paste("   > Loading calendar for:", comp_nombre, comp_temporada))
    
    # 9.7.2. Read the Excel file.
    df_excel <- tryCatch({
      read_excel(ruta_archivo)
    }, error = function(e) {
      warning(paste("Error reading Excel file:", ruta_archivo, "-", e$message))
      return(NULL)
    })
    
    if (is.null(df_excel) || ncol(df_excel) < 4) return(NULL)
    
    # 9.7.3. Assign standard names to the expected columns.
    names(df_excel)[1:4] <- c("jornada", "fecha_hora", "lugar", "partido_raw")
    
    df_excel %>%
      filter(!is.na(partido_raw)) %>%
      mutate(
        # 9.7.4. Convert matchday to character to ensure consistency.
        jornada = as.character(jornada),
        competicion_nombre = comp_nombre,
        competicion_temporada = comp_temporada,
        equipos_split = str_split_fixed(partido_raw, "\\s*-\\s*", 2),
        local = str_trim(equipos_split[, 1]),
        visitante = str_trim(equipos_split[, 2])
      ) %>%
      select(competicion_nombre, competicion_temporada, jornada, local, visitante)
  })
}


### 9.8. Load ID Unification Mappings
message("Loading ID unification mappings...")

ruta_unificacion_id <- "id_unification.txt"
mapa_unificacion_id_df <- NULL
if (file.exists(ruta_unificacion_id)) {
  tryCatch({
    mapa_unificacion_id_df <- read.csv(ruta_unificacion_id, stringsAsFactors = FALSE, encoding = "UTF-8", colClasses = "character")
    message(paste("ID unification file loaded with", nrow(mapa_unificacion_id_df), "rules."))
  }, error = function(e) {
    warning("Error loading id_unification.txt. ID unification will not be applied.")
  })
} else {
  message("id_unification.txt file not found. Continuing without ID unification.")
}


### 9.9. Load Country Translations
message("Loading country name translations...")

ruta_traducciones_paises <- "country_translations.txt"
mapa_traducciones_paises_df <- NULL
if (file.exists(ruta_traducciones_paises)) {
  tryCatch({
    mapa_traducciones_paises_df <- read.csv(
      ruta_traducciones_paises, 
      stringsAsFactors = FALSE, 
      encoding = "UTF-8",
      check.names = FALSE # 9.9.1. Important for handling columns like 'translation_es'.
    )
    message(paste("Country translations loaded successfully with", nrow(mapa_traducciones_paises_df), "entries."))
  }, error = function(e) {
    warning("Error loading country_translations.txt. Country names may not be translated correctly.")
  })
} else {
  message("country_translations.txt file not found. Standard transliteration will be used for country names.")
}


### 9.10. Load Team Abbreviations
message("Loading team abbreviations...")

ruta_abreviaturas <- "abbreviations.txt"
mapa_abreviaturas_df <- NULL
if (file.exists(ruta_abreviaturas)) {
  tryCatch({
    mapa_abreviaturas_df <- read.csv(
      ruta_abreviaturas, 
      stringsAsFactors = FALSE, 
      encoding = "UTF-8",
      check.names = FALSE
    )
    # Pivot to long format for easier joins
    mapa_abreviaturas_long_df <- mapa_abreviaturas_df %>%
      pivot_longer(
        cols = -original_mk,
        names_to = "lang",
        values_to = "abbreviation"
      ) %>%
      mutate(lang = str_remove(lang, "abbr_"))
    
    message(paste("Team abbreviations loaded with", nrow(mapa_abreviaturas_df), "entries."))
  }, error = function(e) {
    warning("Error loading abbreviations.txt. Default abbreviations will be used.")
  })
} else {
  message("abbreviations.txt file not found. Default abbreviations will be used.")
}


### 9.11. Load Player Role Overrides
message("Loading player role overrides...")

ruta_roles_forzados <- "player_role_overrides.txt"
mapa_roles_forzados_df <- NULL
if (file.exists(ruta_roles_forzados)) {
  tryCatch({
    mapa_roles_forzados_df <- read.csv(
      ruta_roles_forzados, 
      stringsAsFactors = FALSE, 
      encoding = "UTF-8",
      colClasses = "character" # Read IDs as character to match main data
    )
    message(paste("Player role overrides loaded with", nrow(mapa_roles_forzados_df), "rules."))
  }, error = function(e) {
    warning("Error loading player_role_overrides.txt. No roles will be forced.")
  })
} else {
  message("player_role_overrides.txt not found. Continuing without role overrides.")
}

#### 10. MAIN DATA PROCESSING AND TRANSFORMATION ####

### 10.1. Merge Real Data with Future Calendars
message("Combining real data with future calendars...")

# Step 1: Load the raw data from both sources
partidos_df_reales <- map_dfr(resultados_exitosos, "partido_info")
partidos_df_placeholders <- cargar_calendarios_excel()


# Step 2: Apply name corrections from conversions.txt to BOTH dataframes BEFORE merging.
# This is the crucial step to standardize team names from different sources.
if (!is.null(mapa_conversiones)) {
  message("   > Applying pre-merge corrections to standardize team names...")
  partidos_df_reales <- aplicar_conversiones(partidos_df_reales, c("local", "visitante"), mapa_conversiones)
  partidos_df_placeholders <- aplicar_conversiones(partidos_df_placeholders, c("local", "visitante"), mapa_conversiones)
}

# Step 3: Now, with clean names, perform the merge logic.
if (nrow(partidos_df_placeholders) > 0 && nrow(partidos_df_reales) > 0) {
  
  # Create a robust unique key for each match, including the matchday.
  partidos_df_reales <- partidos_df_reales %>%
    mutate(match_key = paste(local, visitante, competicion_nombre, competicion_temporada, jornada))
  
  partidos_df_placeholders <- partidos_df_placeholders %>%
    mutate(match_key = paste(local, visitante, competicion_nombre, competicion_temporada, jornada))
  
  # Filter placeholders to remove those that already have a real, played report.
  placeholders_a_mantener <- partidos_df_placeholders %>%
    anti_join(partidos_df_reales, by = "match_key")
  
  # Unify the two dataframes into the final master dataframe.
  partidos_df <- bind_rows(
    partidos_df_reales %>% select(-match_key),
    placeholders_a_mantener %>% select(-match_key)
  )
  
} else if (nrow(partidos_df_placeholders) > 0) {
  partidos_df <- partidos_df_placeholders
} else {
  partidos_df <- partidos_df_reales
}

### 10.2. Assign Match Duration by Competition
message("Assigning match duration by competition...")
partidos_df <- partidos_df %>%
  mutate(
    duracion_partido = case_when(
      str_detect(tolower(competicion_nombre), "младинска") ~ 80, # 80-min matches
      str_detect(tolower(competicion_nombre), "кадетска")  ~ 60, # 60-min matches
      TRUE                                                  ~ 90  # Default for the rest
    )
  )

message("Summary of match durations:")
print(
  partidos_df %>%
    count(competicion_nombre, duracion_partido) %>%
    as.data.frame()
)


### 10.4. Apply Master ID Unification
# 10.4.1. This block runs BEFORE any other name corrections.
if (!is.null(mapa_unificacion_id_df) && nrow(mapa_unificacion_id_df) > 0) {
  message("Applying master ID unification rules...")
  
  # 10.4.2. First, reorder canonical names BEFORE creating the map.
  mapa_unificacion_id_df$nombre_canonico <- reordenar_nombre_idempotente(mapa_unificacion_id_df$nombre_canonico)
  
  id_map <- setNames(mapa_unificacion_id_df$id_canonico, mapa_unificacion_id_df$id_a_unificar)
  name_map <- setNames(mapa_unificacion_id_df$nombre_canonico, mapa_unificacion_id_df$id_a_unificar)
  
  aplicar_mapeo_id <- function(df, col_id = "id", col_nombre = "nombre") {
    if (is.null(df) || nrow(df) == 0 || !col_id %in% names(df) || !col_nombre %in% names(df)) return(df)
    
    indices_a_cambiar <- which(df[[col_id]] %in% names(id_map))
    if (length(indices_a_cambiar) > 0) {
      ids_originales_en_filas <- df[[col_id]][indices_a_cambiar]
      df[[col_id]][indices_a_cambiar] <- id_map[ids_originales_en_filas]
      df[[col_nombre]][indices_a_cambiar] <- name_map[ids_originales_en_filas]
    }
    return(df)
  }
  
  resultados_exitosos <- map(resultados_exitosos, function(res) {
    if(is.null(res)) return(NULL)
    # 10.4.3. Rename 'id_jugadora' to 'id' in event dataframes to use a single function.
    if ("id_jugadora" %in% names(res$goles)) res$goles <- rename(res$goles, id = id_jugadora)
    if ("id_jugadora" %in% names(res$tarjetas)) res$tarjetas <- rename(res$tarjetas, id = id_jugadora)
    if ("id_jugadora" %in% names(res$penales)) res$penales <- rename(res$penales, id = id_jugadora)
    
    res$alineacion_local <- aplicar_mapeo_id(res$alineacion_local, "id", "nombre")
    res$alineacion_visitante <- aplicar_mapeo_id(res$alineacion_visitante, "id", "nombre")
    res$goles <- aplicar_mapeo_id(res$goles, "id", "jugadora")
    res$tarjetas <- aplicar_mapeo_id(res$tarjetas, "id", "jugadora")
    res$penales <- aplicar_mapeo_id(res$penales, "id", "jugadora")
    
    return(res)
  })
  
  message("ID unification completed.")
}


### 10.5. Apply Name Reordering Idempotently
message("Applying idempotent name reordering...")

# This attribute acts as a flag. It's saved to the cache along with the data.
# On the next run, if the attribute exists, this block will be skipped.
if (is.null(attr(resultados_exitosos, "nombres_reordenados"))) {
  
  message("   > Data has not been processed yet. Applying corrections and reordering now...")
  
  # This map loop modifies the core data that gets cached.
  resultados_exitosos <- map(resultados_exitosos, function(res) {
    if (is.null(res)) return(NULL)
    
    # --- INICIO DE LA CORRECCIÓN ---
    # STEP 1: APPLY CORRECTIONS from conversions.txt CONSISTENTLY
    # This ensures team names inside the cached object match the main partidos_df.
    if (!is.null(mapa_conversiones)) {
      # Correct team names in the main info block and event dataframes
      res$partido_info <- aplicar_conversiones(res$partido_info, c("local", "visitante"), mapa_conversiones)
      if (nrow(res$goles) > 0) res$goles <- aplicar_conversiones(res$goles, c("equipo_jugadora", "equipo_acreditado"), mapa_conversiones)
      if (nrow(res$tarjetas) > 0) res$tarjetas <- aplicar_conversiones(res$tarjetas, "equipo", mapa_conversiones)
      if (!is.null(res$penales) && nrow(res$penales) > 0) {
        res$penales <- aplicar_conversiones(res$penales, "equipo", mapa_conversiones)
      }
    }
    # --- FIN DE LA CORRECCIÓN ---
    
    # STEP 2: REORDER PLAYER AND REFEREE NAMES
    if (nrow(res$alineacion_local) > 0) res$alineacion_local$nombre <- reordenar_nombre_idempotente(res$alineacion_local$nombre)
    if (nrow(res$alineacion_visitante) > 0) res$alineacion_visitante$nombre <- reordenar_nombre_idempotente(res$alineacion_visitante$nombre)
    if (nrow(res$goles) > 0) res$goles$jugadora <- reordenar_nombre_idempotente(res$goles$jugadora)
    if (nrow(res$tarjetas) > 0) res$tarjetas$jugadora <- reordenar_nombre_idempotente(res$tarjetas$jugadora)
    if (!is.null(res$penales) && nrow(res$penales) > 0) res$penales$jugadora <- reordenar_nombre_idempotente(res$penales$jugadora)
    
    res$arbitro_principal_nombre <- reordenar_nombre_idempotente(res$arbitro_principal_nombre)
    res$arbitro_asist_1_nombre <- reordenar_nombre_idempotente(res$arbitro_asist_1_nombre)
    res$arbitro_asist_2_nombre <- reordenar_nombre_idempotente(res$arbitro_asist_2_nombre)
    
    return(res)
  })
  
  # Also apply to the unification map, which is rebuilt on every run.
  if (!is.null(mapa_unificacion_id_df)) {
    mapa_unificacion_id_df$nombre_canonico <- reordenar_nombre_idempotente(mapa_unificacion_id_df$nombre_canonico)
  }
  
  # Set the flag to TRUE after processing.
  attr(resultados_exitosos, "nombres_reordenados") <- TRUE
  message("   > Name reordering and corrections complete. Data has been marked as processed.")
  
} else {
  message("   > Names have already been reordered and corrected in a previous run. Skipping.")
}

### 10.6. Consolidate and Unify Player Data

# 10.6.1. Create the RAW dataframe without competition info, which will be added later.
apariciones_df_raw <- map_dfr(resultados_exitosos, ~bind_rows(
  .x$alineacion_local %>% mutate(id_partido = .x$partido_info$id_partido, equipo = .x$partido_info$local),
  .x$alineacion_visitante %>% mutate(id_partido = .x$partido_info$id_partido, equipo = .x$partido_info$visitante)
)) %>% 
  mutate(nombre = str_squish(nombre)) %>%
  # 10.6.2. Join with the ALREADY CORRECTED `partidos_df` to get the correct competition names.
  left_join(
    partidos_df %>% select(id_partido, competicion_nombre, competicion_temporada), 
    by = "id_partido"
  )

minutos_df_raw <- map_dfr(resultados_exitosos, function(res) {
  if(is.null(res)) return(NULL)
  id_p <- res$partido_info$id_partido
  
  # 10.6.3. Get the correct duration for this match from the modified dataframe.
  duracion <- (partidos_df %>% filter(id_partido == id_p) %>% pull(duracion_partido))[1]
  # 10.6.4. If for some reason it's not found, use 90 as a safe fallback.
  if (length(duracion) == 0 || is.na(duracion)) { duracion <- 90 }
  
  calcular_minutos_equipo <- function(alineacion, cambios, duracion_partido) {
    if(is.null(alineacion) || nrow(alineacion) == 0) return(NULL)
    # 10.6.5. Use duracion_partido instead of a hardcoded '90'.
    jugadoras_con_minutos <- alineacion %>% mutate(min_entra = if_else(tipo == "Titular", 0, NA_real_), min_sale = if_else(tipo == "Titular", duracion_partido, 0))
    if (!is.null(cambios) && nrow(cambios) > 0) {
      cambios_procesados <- cambios %>% mutate(d_entra = as.numeric(str_match(texto, "Entra .*?\\((\\d+)\\)")[, 2]), d_sale  = as.numeric(str_match(texto, "por .*?\\((\\d+)\\)")[, 2])) %>% select(minuto, d_entra, d_sale) %>% filter(!is.na(d_entra) & !is.na(d_sale))
      for (i in 1:nrow(cambios_procesados)) {
        cambio <- cambios_procesados[i, ]; jugadoras_con_minutos <- jugadoras_con_minutos %>% mutate(min_sale = if_else(dorsal == cambio$d_sale, as.numeric(cambio$minuto), min_sale), min_entra = if_else(dorsal == cambio$d_entra, as.numeric(cambio$minuto), min_entra))
      }
    }
    # 10.6.6. Use duracion_partido here as well for unused substitutes.
    jugadoras_con_minutos %>% mutate(min_sale = if_else(!is.na(min_entra) & tipo == "Suplente" & min_sale == 0, duracion_partido, min_sale), minutos_jugados = if_else(is.na(min_entra), 0, min_sale - min_entra)) %>% mutate(minutos_jugados = pmax(0, minutos_jugados))
  }
  
  # 10.6.7. Pass the correct duration to the function.
  min_local <- calcular_minutos_equipo(res$alineacion_local, res$cambios_local, duracion)
  min_visitante <- calcular_minutos_equipo(res$alineacion_visitante, res$cambios_visitante, duracion)
  
  bind_rows(min_local, min_visitante) %>% mutate(id_partido = id_p)
})

# 10.6.8. Crucial logic to create a canonical (master) ID for each player,
# 10.6.9. resolving inconsistencies in IDs or names in the reports.
preferred_id_map <- apariciones_df_raw %>% filter(!is.na(nombre), !is.na(id), str_detect(id, "^\\d{5,6}$")) %>% count(nombre, id, name = "frequency") %>% group_by(nombre) %>% filter(frequency == max(frequency)) %>% slice(1) %>% ungroup() %>% select(nombre, canonical_id = id)
id_mapping <- apariciones_df_raw %>% filter(!is.na(nombre) & nchar(trimws(nombre)) > 2) %>% distinct(nombre) %>% left_join(preferred_id_map, by = "nombre") %>% mutate(final_id = if_else(!is.na(canonical_id), as.character(canonical_id), paste0("player_gen_", generar_id_seguro(nombre)))) %>% select(nombre, canonical_id = final_id)

# 10.6.10. Create the final appearances dataframe, joining minutes and the canonical ID.
apariciones_df <- apariciones_df_raw %>% left_join(minutos_df_raw %>% select(id_partido, nombre, dorsal, tipo, min_entra, min_sale, minutos_jugados), by = c("id_partido", "nombre", "dorsal", "tipo")) %>% select(-id) %>% left_join(id_mapping, by = "nombre") %>% rename(id = canonical_id) %>% select(id, id_partido, nombre, dorsal, tipo, equipo, es_portera, es_capitana, competicion_nombre, competicion_temporada, everything())

# --- APPLY ROLE OVERRIDES ---
if (!is.null(mapa_roles_forzados_df) && nrow(mapa_roles_forzados_df) > 0) {
  message("Applying player role overrides...")
  
  # Join the override rules to the main appearances dataframe
  apariciones_df <- apariciones_df %>%
    left_join(mapa_roles_forzados_df, by = "id")
  
  # Apply the override logic
  apariciones_df <- apariciones_df %>%
    mutate(
      es_portera = if_else(!is.na(force_role) & force_role == "goalkeeper", 
                           TRUE, 
                           es_portera)
    ) %>%
    # Clean up the helper column
    select(-force_role)
  
  message("Overrides applied successfully.")
}

### 10.7. Unify Event Data (Goals and Cards)

# 10.7.1. Process the goal and card dataframes to assign the canonical ID to each player.
goles_raw_df <- map_dfr(resultados_exitosos, "goles")
if (nrow(goles_raw_df) > 0) { 
  goles_df_unificado <- goles_raw_df %>% 
    mutate(jugadora = str_squish(jugadora)) %>% # 10.7.2. Already reordered and corrected from 10.5
    left_join(id_mapping, by = c("jugadora" = "nombre")) %>% 
    select(-any_of(c("id", "id_jugadora"))) %>% 
    rename(id = canonical_id)
} else { goles_df_unificado <- tibble(id_partido = character(), jugadora = character(), equipo_jugadora = character(), equipo_acreditado = character(), minuto = integer(), dorsal = integer(), tipo = character(), id = character()) }

tarjetas_raw_df <- map_dfr(resultados_exitosos, "tarjetas")
if(nrow(tarjetas_raw_df) > 0) { 
  tarjetas_df_unificado <- tarjetas_raw_df %>% 
    mutate(jugadora = str_squish(jugadora)) %>% # 10.7.3. Already reordered and corrected from 10.5
    left_join(id_mapping, by = c("jugadora" = "nombre")) %>% 
    select(-any_of(c("id", "id_jugadora"))) %>% 
    rename(id = canonical_id)
} else { tarjetas_df_unificado <- tibble(jugadora = character(), equipo = character(), dorsal = integer(), minuto = integer(), tipo = character(), motivo = character(), id_partido = character(), id = character()) }

penales_raw_df <- map_dfr(resultados_exitosos, "penales")
if(nrow(penales_raw_df) > 0) {
  penales_df_unificado <- penales_raw_df %>%
    mutate(
      jugadora = str_squish(jugadora) # 10.7.4. 'jugadora' is already reordered and corrected from section 10.5.
    ) %>%
    left_join(id_mapping, by = c("jugadora" = "nombre")) %>%
    select(-any_of(c("id", "id_jugadora"))) %>%
    rename(id = canonical_id)
} else { 
  penales_df_unificado <- tibble(jugadora = character(), dorsal = integer(), equipo = character(), resultado_penal = character(), id_partido = character(), id = character()) 
}


### 10.8. Process and Translate Demographic Data
message("Processing and translating player positions and demographic data...")

# 10.8.1. Dictionary to unify positions to neutral keys.
mapa_posicion_unificada <- c(
  "GK" = "goalkeeper", "Portera" = "goalkeeper",
  "DL" = "defender", "DC" = "defender", "DR" = "defender", "DM" = "defender",
  "WBL" = "defender", "WBR" = "defender", "Defensa" = "defender",
  "ML" = "midfielder", "MC" = "midfielder", "MR" = "midfielder", "AMC" = "midfielder",
  "Centrocampista" = "midfielder",
  "AML" = "forward", "AMR" = "forward", "SC" = "forward", "Delantera" = "forward"
)

posiciones_procesadas_df <- posiciones_df %>%
  mutate(posicion_unificada = recode(posicion, !!!mapa_posicion_unificada, .default = NA_character_)) %>%
  filter(!is.na(posicion_unificada)) %>%
  group_by(id) %>%
  summarise(
    posicion_final_unificada = paste(unique(posicion_unificada), collapse = " / "),
    nacionalidad = first(nacionalidad),
    fecha_nacimiento = first(fecha_nacimiento),
    ciudad_nacimiento = first(ciudad_nacimiento),
    .groups = 'drop'
  )

### 10.9. Reassign National Team Matches
message("Reallocating national team matches...")

# 10.9.1. Identify national team matches and assign them a fictitious competition and season.
# 10.9.2. This will group them under a single virtual entity.
partidos_df <- partidos_df %>%
  mutate(
    es_partido_seleccion = (local == "Македонија" | visitante == "Македонија"),
    competicion_nombre = if_else(es_partido_seleccion, "Репрезентација", competicion_nombre),
    competicion_temporada = if_else(es_partido_seleccion, "Сите", competicion_temporada)
  )

### 10.10. Identify and Order Competitions
message("Identifying, translating, and sorting unique competitions...")

if (exists("partidos_df") && nrow(partidos_df) > 0) {
  
  # 10.10.1. Calculate the most recent numeric year among the real competitions.
  # 10.10.2. This will be used to correctly position the "National Team" entry.
  max_real_season_numeric <- partidos_df %>%
    filter(competicion_nombre != "Репрезентација") %>%
    distinct(competicion_temporada) %>%
    mutate(
      start_year = as.integer(str_extract(competicion_temporada, "^\\d{2,4}")),
      # 10.10.3. Convert '23' to '2023' for correct comparison.
      sort_year = if_else(nchar(as.character(start_year)) == 2, 2000 + start_year, start_year)
    ) %>%
    pull(sort_year) %>%
    max(na.rm = TRUE) # 10.10.4. Ensure NA is handled if the list is empty.
  
  # 10.10.5. If there are no real seasons (e.g., empty dataframe), set a safe default.
  if (is.infinite(max_real_season_numeric)) max_real_season_numeric <- 2000 
  
  # 10.10.6. Step 1: Process all real competitions, EXCLUDING the "National Team" one.
  competiciones_base_df <- partidos_df %>%
    filter(competicion_nombre != "Репрезентација") %>%
    distinct(competicion_nombre, competicion_temporada) %>%
    mutate(
      competicion_id = generar_id_seguro(paste(competicion_nombre, competicion_temporada)),
      nombre_lower = tolower(competicion_nombre),
      # 10.10.7. Calculate the numeric start year for sorting.
      start_year = as.integer(str_extract(competicion_temporada, "^\\d{2,4}")),
      sort_year = if_else(nchar(as.character(start_year)) == 2, 2000 + start_year, start_year)
    ) %>%
    mutate(
      importancia_score = case_when(
        str_detect(nombre_lower, "куп") ~ 1, str_detect(nombre_lower, "прва") ~ 2,
        str_detect(nombre_lower, "втора") ~ 3, str_detect(nombre_lower, "трета") ~ 4,
        str_detect(nombre_lower, "младинска") ~ 5, str_detect(nombre_lower, "кадетска") ~ 6, str_detect(nombre_lower, "пријателски") ~ 7,
        TRUE ~ 7
      ),
      baraz_modifier = if_else(str_detect(nombre_lower, "бараж"), 0.5, 0),
      final_score = importancia_score + baraz_modifier
    )
  
  # 10.10.8. Step 2: Manually create the entry for the "National Team" pseudo-competition.
  # 10.10.9. Assign it an intermediate 'sort_year'.
  competicion_seleccion_df <- tibble(
    competicion_nombre = "Репрезентација",
    competicion_temporada = "Сите", 
    competicion_id = "reprezentacija", 
    nombre_lower = "репрезентација",
    importancia_score = 0, 
    baraz_modifier = 0,
    final_score = 0,
    # 10.10.10. This places it numerically just above the most recent real season.
    sort_year = max_real_season_numeric + 0.5 
  )
  
  # 10.10.11. Step 3: Combine real competitions with the national team one.
  # 10.10.12. The `orden_primario` column is created HERE to ensure all necessary columns exist.
  competiciones_combinadas_df <- bind_rows(competiciones_base_df, competicion_seleccion_df) %>%
    mutate(
      orden_primario = case_when(
        # 10.10.13. KEY CORRECTION: Use `sort_year` for numeric comparison.
        sort_year == max_real_season_numeric ~ 1, # Most recent season: first.
        competicion_id == "reprezentacija"   ~ 2, # National team: right after the most recent season.
        TRUE                                 ~ 3  # All other (older) seasons: last.
      )
    )
  
  # 10.10.14. Robust Translation Logic for Competitions.
  if (!is.null(mapa_nombres_competiciones_long)) {
    competiciones_combinadas_df_temp <- competiciones_combinadas_df %>%
      mutate(original_mk_join_key = paste(competicion_nombre, competicion_temporada))
    
    comp_translations_wide <- mapa_nombres_competiciones_long %>%
      pivot_wider(
        id_cols = original_mk, 
        names_from = lang, 
        values_from = translated_name, 
        names_prefix = "nombre_completo_"
      )
    
    competiciones_unicas_df <- competiciones_combinadas_df_temp %>%
      left_join(comp_translations_wide, by = c("original_mk_join_key" = "original_mk")) %>%
      select(-original_mk_join_key)
  } else {
    competiciones_unicas_df <- competiciones_combinadas_df
  }
  
  competiciones_unicas_df <- competiciones_unicas_df %>%
    mutate(nombre_completo_mk = if_else(
      competicion_nombre == "Репрезентација", 
      "Репрезентација", 
      paste(competicion_nombre, competicion_temporada))
    )
  
  map_transliteration_comp <- c('а'='a','б'='b','в'='v','г'='g','д'='d','ѓ'='gj','е'='e','ж'='ž','з'='z','ѕ'='dz','и'='i','ј'='j','к'='k','л'='l','љ'='lj','м'='m','н'='n','њ'='nj','о'='o','п'='p','р'='r','с'='s','т'='t','ќ'='kj','у'='u','ф'='f','х'='h','ц'='c','ч'='č','џ'='dž','ш'='š')
  
  for (lang_code in setdiff(IDIOMAS_SOPORTADOS, "mk")) {
    target_col <- paste0("nombre_completo_", lang_code)
    if (!target_col %in% names(competiciones_unicas_df)) {
      competiciones_unicas_df[[target_col]] <- NA_character_
    }
  }
  
  for (lang_code in setdiff(IDIOMAS_SOPORTADOS, "mk")) {
    target_col <- paste0("nombre_completo_", lang_code)
    competiciones_unicas_df <- competiciones_unicas_df %>%
      mutate(!!target_col := case_when(
        competicion_id == "reprezentacija" ~ t("competition_reprezentacija"), 
        !is.na(.data[[target_col]]) ~ .data[[target_col]],
        TRUE ~ str_to_title(str_replace_all(tolower(nombre_completo_mk), map_transliteration_comp))
      ))
  }
  
  # 10.10.15. Modify the arrange() function for final sorting.
  competiciones_unicas_df <- competiciones_unicas_df %>%
    arrange(orden_primario,              # 1. Priority: Current season, National Team, or old season?
            final_score,                 # 2. Priority: Competition importance (Cup, First Div, Second Div...)
            desc(competicion_temporada), # 3. Priority: Most recent seasons first (for 'orden_primario = 3')
            nombre_completo_mk)          # 4. Priority: Alphabetical tie-breaker by name.
  
} else {
  competiciones_unicas_df <- tibble()
}


### 10.11. Generate Referee and Stadium Data
# 10.11.1. Restructure the creation of arbitros_df to use separate fields and
# 10.11.2. apply name reordering.
arbitros_df <- map_dfr(resultados_exitosos, function(res) {
  if (is.null(res) || is.null(res$arbitro_principal_nombre)) return(NULL)
  
  tibble(
    id_partido = res$partido_info$id_partido,
    ime_raw = c(res$arbitro_principal_nombre, res$arbitro_asist_1_nombre, res$arbitro_asist_2_nombre),
    ciudad = c(res$arbitro_principal_ciudad, res$arbitro_asist_1_ciudad, res$arbitro_asist_2_ciudad),
    uloga = c("referee_main", "referee_asst1", "referee_asst2")
  )
}) %>%
  filter(!is.na(ime_raw), ime_raw != "Desconocido") %>%
  rename(ime = ime_raw) %>%
  select(id_partido, ime, ciudad, uloga)

estadios_df <- map_dfr(resultados_exitosos, ~if(is.null(.x)||is.null(.x$estadio)) NULL else data.frame(id_partido=.x$partido_info$id_partido,estadio=.x$estadio)) %>% left_join(partidos_df,by="id_partido")

### 10.12. Create Master Entity Dataframe (Teams, Referees, Stadiums)
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

map_transliteration_entity <- c('а'='a','б'='b','в'='v','г'='g','д'='d','ѓ'='gj','е'='e','ж'='ž','з'='z','ѕ'='dz','и'='i','ј'='j','к'='k','л'='l','љ'='lj','м'='m','н'='n','њ'='nj','о'='o','п'='p','р'='r','с'='s','т'='t','ќ'='kj','у'='u','ф'='f','х'='h','ц'='c','ч'='č','џ'='dž','ш'='š')

entidades_maestro_df <- entidades_maestro_df %>% mutate(translated_name_mk = original_name)

# 10.12.1. Correct the fallbacks for entity name generation.
for (lang_code in setdiff(IDIOMAS_SOPORTADOS, "mk")) {
  target_col <- paste0("translated_name_", lang_code)
  
  # 10.12.2. Ensure the target column exists.
  if (!target_col %in% names(entidades_maestro_df)) {
    entidades_maestro_df[[target_col]] <- NA_character_
  }
  
  entidades_maestro_df <- entidades_maestro_df %>%
    mutate(!!target_col := coalesce(
      # 10.12.3. 1. Try to use the manual translation if it exists.
      .data[[target_col]],
      # 10.12.4. 2. If not, apply automatic transliteration as a fallback.
      str_replace_all(tolower(original_name), map_transliteration_entity) %>% str_to_title()
    ))
}


### 10.13. Integrate Country Translations into Master Entity Dataframe
message("Integrating country translations into the master entity dataframe...")

if (!is.null(mapa_traducciones_paises_df) && nrow(mapa_traducciones_paises_df) > 0) {
  
  # 10.13.1. Prepare the country translations dataframe for joining.
  paises_para_unir <- mapa_traducciones_paises_df %>%
    # 10.13.2. KEY CORRECTION: Rename 'original_mk' to 'original_name' to match the join column of the master dataframe.
    rename(original_name = original_mk) %>%
    # 10.13.3. Now rename the translation columns to avoid conflicts.
    rename_with(~ paste0("country_", .), .cols = -original_name)
  
  # 10.13.4. Join the master entity dataframe with country translations.
  # 10.13.5. This left_join will now work because both dataframes have an 'original_name' column.
  entidades_maestro_df <- entidades_maestro_df %>%
    left_join(paises_para_unir, by = "original_name")
  
  # 10.13.6. Loop to update each language column, prioritizing the country translation.
  for (lang_code in setdiff(IDIOMAS_SOPORTADOS, "mk")) {
    col_entidad <- paste0("translated_name_", lang_code)
    col_pais <- paste0("country_translation_", lang_code) # Corrected to match the prefix.
    
    # 10.13.7. Only proceed if both columns exist.
    if (col_entidad %in% names(entidades_maestro_df) && col_pais %in% names(entidades_maestro_df)) {
      entidades_maestro_df <- entidades_maestro_df %>%
        mutate(
          !!col_entidad := coalesce(.data[[col_pais]], .data[[col_entidad]])
        )
    }
  }
  
  # 10.13.8. Clean up the auxiliary "country_" columns.
  entidades_maestro_df <- entidades_maestro_df %>%
    select(-starts_with("country_"))
  
  message("Country translations have been successfully integrated.")
}

### 10.14. Determine Scope of Changes for Incremental Generation
message("Checking for changes for incremental generation...")

# 10.14.1. Load the change information saved by Script 1.
ruta_cache_info <- "cache_info.rds"
if (!file.exists(ruta_cache_info)) {
  stop("Cache info file (cache_info.rds) not found. Please run Script 1 first.")
}
info_cambios <- readRDS(ruta_cache_info)

# 10.14.2. Determine if there were changes in PDFs or if new calendars were loaded.
hubo_cambios_pdf <- info_cambios$hubo_cambios
hubo_cambios_excel <- exists("partidos_df_placeholders") && nrow(partidos_df_placeholders) > 0
hubo_cambios <- hubo_cambios_pdf || hubo_cambios_excel

# 10.14.3. Control variable for full rebuild.
full_rebuild_needed <- FALSE

# 10.14.4. If there were no changes of any kind, regeneration can be skipped.
if (!hubo_cambios) {
  message("No changes found in reports or calendars. No need to regenerate HTML files.")
} else {
  # 10.14.5. If PDF files were deleted, a full rebuild is necessary to ensure consistency.
  if (length(info_cambios$archivos_eliminados_nombres) > 0) {
    message("Deleted reports detected. A full site rebuild will be performed.")
    full_rebuild_needed <- TRUE
  } else {
    message("New or modified reports/calendars detected. An incremental update will be performed.")
    
    # 10.14.6. Initialize sets of affected entities.
    affected_competition_ids <- character(0)
    affected_match_ids <- character(0)
    affected_player_ids <- character(0)
    affected_team_ids <- character(0)
    affected_referee_ids <- character(0)
    affected_stadium_ids <- character(0)
    
    # 10.14.7. Step 1: Identify entities affected by new/modified PDF reports.
    ids_partidos_afectados <- str_match(info_cambios$archivos_nuevos_nombres, "match_(\\d+)_")[, 2]
    
    if (length(na.omit(ids_partidos_afectados)) > 0) {
      partidos_afectados_df <- partidos_df %>% 
        filter(id_partido %in% ids_partidos_afectados) %>%
        left_join(competiciones_unicas_df, by = c("competicion_nombre", "competicion_temporada"))
      
      if(nrow(partidos_afectados_df) > 0) {
        affected_competition_ids <- unique(c(affected_competition_ids, na.omit(partidos_afectados_df$competicion_id)))
        affected_match_ids <- unique(c(affected_match_ids, partidos_afectados_df$id_partido))
        affected_team_ids <- unique(c(affected_team_ids, 
                                      generar_id_seguro(partidos_afectados_df$local), 
                                      generar_id_seguro(partidos_afectados_df$visitante)))
      }
      
      jugadoras_afectadas_df <- apariciones_df %>% filter(id_partido %in% ids_partidos_afectados)
      if(nrow(jugadoras_afectadas_df) > 0) {
        affected_player_ids <- unique(c(affected_player_ids, na.omit(jugadoras_afectadas_df$id)))
      }
      
      arbitros_afectados_df <- arbitros_df %>% filter(id_partido %in% ids_partidos_afectados)
      if(nrow(arbitros_afectados_df) > 0) {
        affected_referee_ids <- unique(c(affected_referee_ids, generar_id_seguro(na.omit(arbitros_afectados_df$ime))))
      }
      
      estadios_afectados_df <- estadios_df %>% filter(id_partido %in% ids_partidos_afectados)
      if(nrow(estadios_afectados_df) > 0) {
        affected_stadium_ids <- unique(c(affected_stadium_ids, generar_id_seguro(na.omit(estadios_afectados_df$estadio))))
      }
    }
    
    # 10.14.8. Step 2: Add competitions from Excel calendars to the affected list.
    if (exists("partidos_df_placeholders") && nrow(partidos_df_placeholders) > 0) {
      ids_placeholders <- partidos_df_placeholders %>%
        left_join(competiciones_unicas_df, by = c("competicion_nombre", "competicion_temporada")) %>%
        pull(competicion_id)
      
      # 10.14.9. Add placeholder competition IDs to the affected set.
      affected_competition_ids <- unique(c(affected_competition_ids, na.omit(ids_placeholders)))
    }
    
    # 10.14.10. Final summary message of changes.
    message(paste("Identified", length(affected_competition_ids), "competitions,",
                  length(affected_match_ids), "matches, and",
                  length(affected_player_ids), "players for update."))
  }
}

#### 11. CREATION OF AGGREGATED DATASETS FOR PROFILES AND STATISTICS ####
message("Centralized calculation of all statistics...")

### 11.1. Generate Global Player Statistics
if (!exists("apariciones_df") || nrow(apariciones_df) == 0) {
  jugadoras_stats_df <- data.frame()
} else {
  stats_generales <- apariciones_df %>% 
    filter(!is.na(id)) %>% 
    group_by(id) %>% 
    summarise(
      PlayerName_mk = first(nombre), Team = last(equipo),
      CalledUp = n_distinct(id_partido), Starter = sum(tipo=="Titular", na.rm = T),
      Minutes = sum(minutos_jugados, na.rm = T), Played = sum(minutos_jugados>0, na.rm=T),
      .groups='drop'
    )
  goles_por_jugadora_global <- goles_df_unificado %>% filter(!is.na(id), tipo == "Normal") %>% group_by(id) %>% summarise(Goals = n(), .groups = 'drop')
  tarjetas_por_jugadora_global <- tarjetas_df_unificado %>% filter(!is.na(id)) %>% group_by(id) %>% summarise(Yellows=sum(tipo=="Amarilla",na.rm=T),Reds=sum(tipo=="Roja",na.rm=T),.groups='drop')
  
  jugadoras_stats_temp <- stats_generales %>% 
    left_join(goles_por_jugadora_global, by = "id") %>% 
    left_join(tarjetas_por_jugadora_global, by = "id") %>%
    left_join(posiciones_procesadas_df, by = "id") %>%
    mutate(
      across(c(Goals, Yellows, Reds), ~replace_na(., 0)),
      edad = if_else(!is.na(fecha_nacimiento), floor(as.numeric(difftime(Sys.Date(), fecha_nacimiento, units = "days")) / 365.25), NA_integer_),
      clave_lower = tolower(trimws(nacionalidad))
    )
  
  if (!is.null(mapa_nombres_jugadoras_long)) {
    player_translations_wide <- mapa_nombres_jugadoras_long %>%
      pivot_wider(id_cols = original_mk, names_from = lang, values_from = translated_name, names_prefix = "PlayerName_")
    jugadoras_stats_temp <- jugadoras_stats_temp %>% left_join(player_translations_wide, by = c("PlayerName_mk" = "original_mk"))
  }
  
  map_transliteration_player <- c('а'='a','б'='b','в'='v','г'='g','д'='d','ѓ'='gj','е'='e','ж'='ž','з'='z','ѕ'='dz','и'='i','ј'='j','к'='k','л'='l','љ'='lj','м'='m','н'='n','њ'='nj','о'='o','п'='p','р'='r','с'='s','т'='t','ќ'='kj','у'='u','ф'='f','х'='h','ц'='c','ч'='č','џ'='dž','ш'='š')
  
  # 11.1.1. Correct the fallbacks for name generation.
  for (lang_code in setdiff(IDIOMAS_SOPORTADOS, "mk")) {
    target_col <- paste0("PlayerName_", lang_code)
    
    # 11.1.2. Ensure the target column exists before mutation.
    if (!target_col %in% names(jugadoras_stats_temp)) {
      jugadoras_stats_temp[[target_col]] <- NA_character_
    }
    
    jugadoras_stats_temp <- jugadoras_stats_temp %>%
      mutate(!!target_col := coalesce(
        # 11.1.3. 1. Try to use the manual translation if it exists.
        .data[[target_col]],
        # 11.1.4. 2. If not, apply automatic transliteration as a fallback.
        str_replace_all(tolower(PlayerName_mk), map_transliteration_player) %>% str_to_title()
      ))
  }
  
  if (!is.null(mapeo_completo_df)) {
    jugadoras_stats_df <- jugadoras_stats_temp %>% left_join(mapeo_completo_df, by = "clave_lower")
  } else {
    jugadoras_stats_df <- jugadoras_stats_temp %>% mutate(codigo_iso = NA_character_, nombre_macedonio = NA_character_)
  }
  
  jugadoras_stats_df <- jugadoras_stats_df %>%
    select(id, starts_with("PlayerName_"), Team, posicion_final_unificada, nacionalidad, edad, codigo_iso, nombre_macedonio, CalledUp, Played, Starter, Minutes, Goals, Yellows, Reds) %>% 
    arrange(desc(Goals), desc(Minutes))
}

### 11.2. Calculate Statistics by Competition (Standings, Scorers, Sanctions)
stats_clasificacion_por_comp_df <- competiciones_unicas_df %>%
  filter(competicion_id != "reprezentacija", !str_detect(tolower(competicion_nombre), "куп")) %>%
  group_by(competicion_id, competicion_nombre, competicion_temporada) %>%
  reframe({
    grupo_actual <- cur_group()
    partidos_comp_raw <- partidos_df %>% 
      filter(
        competicion_nombre == grupo_actual$competicion_nombre, 
        competicion_temporada == grupo_actual$competicion_temporada,
        !is.na(id_partido)
      )
    
    if (nrow(partidos_comp_raw) == 0) return(tibble())
    
    partidos_comp <- partidos_comp_raw %>%
      mutate(
        goles_local_calc = goles_local,
        goles_visitante_calc = goles_visitante,
        goles_local_calc = case_when(
          isTRUE(es_resultado_oficial) & goles_local > goles_visitante ~ 3,
          isTRUE(es_resultado_oficial) & goles_visitante > goles_local ~ 0,
          isTRUE(es_resultado_oficial) & goles_local == goles_visitante ~ 3,
          TRUE ~ goles_local_calc
        ),
        goles_visitante_calc = case_when(
          isTRUE(es_resultado_oficial) & goles_local > goles_visitante ~ 0,
          isTRUE(es_resultado_oficial) & goles_visitante > goles_local ~ 3,
          isTRUE(es_resultado_oficial) & goles_local == goles_visitante ~ 0,
          TRUE ~ goles_visitante_calc
        )
      )
    
    locales <- partidos_comp %>% 
      select(team = local, GF = goles_local_calc, GA = goles_visitante_calc)
    
    visitantes <- partidos_comp %>% 
      select(team = visitante, GF = goles_visitante_calc, GA = goles_local_calc)
    
    bind_rows(locales, visitantes) %>%
      mutate(
        Pts = case_when(GF > GA ~ 3, GF < GA ~ 0, TRUE ~ 1),
        result = case_when(GF > GA ~ "W", GF < GA ~ "L", TRUE ~ "D")
      ) %>%
      group_by(team) %>%
      summarise(
        P = n(),
        Pts = sum(Pts, na.rm = TRUE),
        W = sum(result == "W", na.rm = TRUE),
        D = sum(result == "D", na.rm = TRUE),
        L = sum(result == "L", na.rm = TRUE),
        GF = sum(GF, na.rm = TRUE),
        GA = sum(GA, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(GD = GF - GA) %>%
      arrange(desc(Pts), desc(GD), desc(GF)) %>%
      mutate(Pos = row_number())
  })

stats_goleadoras_por_comp_df <- apariciones_df %>%
  group_by(id, competicion_nombre, competicion_temporada) %>%
  summarise(TeamNames_mk = paste(unique(equipo), collapse = " / "), .groups = 'drop') %>%
  right_join(
    goles_df_unificado %>% filter(tipo == "Normal", !is.na(id)) %>%
      left_join(partidos_df %>% select(id_partido, competicion_nombre, competicion_temporada), by = "id_partido") %>%
      group_by(id, competicion_nombre, competicion_temporada) %>%
      summarise(Goals = n(), .groups = 'drop'),
    by = c("id", "competicion_nombre", "competicion_temporada")
  ) %>%
  left_join(competiciones_unicas_df %>% filter(competicion_id != "reprezentacija") %>% select(competicion_id, competicion_nombre, competicion_temporada), by=c("competicion_nombre", "competicion_temporada")) %>%
  filter(!is.na(competicion_id)) %>%
  arrange(competicion_id, desc(Goals)) %>%
  group_by(competicion_id) %>%
  mutate(Pos = min_rank(desc(Goals))) %>%
  ungroup()

stats_sanciones_por_comp_df <- apariciones_df %>%
  group_by(id, competicion_nombre, competicion_temporada) %>%
  summarise(TeamNames_mk = paste(unique(equipo), collapse = " / "), .groups = 'drop') %>%
  right_join(
    tarjetas_df_unificado %>% filter(!is.na(id)) %>%
      left_join(partidos_df %>% select(id_partido, competicion_nombre, competicion_temporada), by = "id_partido") %>%
      group_by(id, competicion_nombre, competicion_temporada) %>%
      summarise(YellowCards = sum(tipo == "Amarilla", na.rm = TRUE), RedCards = sum(tipo == "Roja", na.rm = TRUE), .groups = 'drop') %>%
      filter(YellowCards > 0 | RedCards > 0),
    by = c("id", "competicion_nombre", "competicion_temporada")
  ) %>%
  left_join(competiciones_unicas_df %>% filter(competicion_id != "reprezentacija") %>% select(competicion_id, competicion_nombre, competicion_temporada), by=c("competicion_nombre", "competicion_temporada")) %>%
  filter(!is.na(competicion_id)) %>%
  arrange(competicion_id, desc(RedCards), desc(YellowCards)) %>%
  group_by(competicion_id) %>%
  mutate(Pos = row_number()) %>%
  ungroup()

### 11.3. Calculate Goalkeeper Statistics by Competition
porteras_apariciones_df <- apariciones_df %>%
  filter(es_portera == TRUE, !is.na(id), minutos_jugados > 0) %>%
  select(id, id_partido, equipo, competicion_nombre, competicion_temporada, min_entra, min_sale, minutos_jugados)

goles_recibidos_df <- goles_df_unificado %>%
  left_join(partidos_df %>% select(id_partido, local, visitante), by = "id_partido") %>%
  mutate(equipo_que_recibio_gol = if_else(equipo_acreditado == local, visitante, local)) %>%
  select(id_partido, equipo_conceded = equipo_que_recibio_gol, minuto_gol = minuto)

# 11.3.1. Step 1: Calculate Goals Against (GA) explicitly and robustly.
stats_ga <- porteras_apariciones_df %>%
  left_join(goles_recibidos_df, by = c("id_partido", "equipo" = "equipo_conceded"), relationship = "many-to-many") %>%
  filter(!is.na(minuto_gol) & minuto_gol >= min_entra & minuto_gol <= min_sale) %>%
  group_by(id, competicion_nombre, competicion_temporada, TeamName_mk = equipo) %>%
  summarise(GA = n(), .groups = 'drop')

# 11.3.2. Step 2: Calculate Clean Sheets (CS) explicitly and robustly.
stats_cs <- porteras_apariciones_df %>%
  # 11.3.3. Join with partidos_df to get the duration of each match.
  left_join(partidos_df %>% select(id_partido, duracion_partido, local, visitante, goles_local, goles_visitante), by = "id_partido") %>%
  # 11.3.4. A CS requires playing the full match (minutes >= duration).
  filter(minutos_jugados >= duracion_partido) %>%
  mutate(goles_recibidos_partido = if_else(local == equipo, goles_visitante, goles_local)) %>%
  filter(goles_recibidos_partido == 0) %>%
  group_by(id, competicion_nombre, competicion_temporada, TeamName_mk = equipo) %>%
  summarise(CS = n(), .groups = 'drop')

# 11.3.5. Step 3: Calculate total minutes.
stats_minutos <- porteras_apariciones_df %>%
  group_by(id, competicion_nombre, competicion_temporada, TeamName_mk = equipo) %>%
  summarise(Minutes = sum(minutos_jugados, na.rm = TRUE), .groups = 'drop')

# 11.3.6. Step 4: Join all statistics into a final dataframe.
stats_porteras_por_comp_df <- stats_minutos %>%
  full_join(stats_ga, by = c("id", "competicion_nombre", "competicion_temporada", "TeamName_mk")) %>%
  full_join(stats_cs, by = c("id", "competicion_nombre", "competicion_temporada", "TeamName_mk")) %>%
  mutate(across(c(GA, CS), ~replace_na(., 0))) %>%
  mutate(GA90 = if_else(Minutes > 0, (GA / Minutes) * 90, 0)) %>%
  left_join(competiciones_unicas_df %>% filter(competicion_id != "reprezentacija") %>% select(competicion_id, competicion_nombre, competicion_temporada), by = c("competicion_nombre", "competicion_temporada"))

### 11.4. Calculate Defensive Trio Statistics by Competition
defensas_apariciones_df <- apariciones_df %>%
  left_join(posiciones_procesadas_df, by = "id") %>%
  filter(str_detect(posicion_final_unificada, "defender"), !is.na(min_entra), minutos_jugados > 0) %>%
  select(id, id_partido, equipo, competicion_nombre, competicion_temporada, min_entra, min_sale)

trio_minutos_partido_df <- defensas_apariciones_df %>%
  group_by(id_partido, equipo) %>%
  filter(n() >= 3) %>%
  group_modify(~ {
    combn(.x$id, 3, simplify = FALSE) %>%
      map_dfr(function(trio_ids) {
        jugadoras_trio <- .x %>% filter(id %in% trio_ids)
        minutos_compartidos <- max(0, min(jugadoras_trio$min_sale) - max(jugadoras_trio$min_entra))
        tibble(
          trio_key = paste(sort(trio_ids), collapse = "-"),
          minutos_compartidos = minutos_compartidos,
          start_shared = max(jugadoras_trio$min_entra),
          end_shared = min(jugadoras_trio$min_sale)
        )
      })
  }) %>%
  ungroup() %>%
  filter(minutos_compartidos > 0) %>%
  left_join(partidos_df %>% select(id_partido, competicion_nombre, competicion_temporada), by="id_partido")

# 11.4.1. Step 1: Calculate the total minutes each trio played together.
stats_minutes_trios <- trio_minutos_partido_df %>%
  group_by(trio_key, competicion_nombre, competicion_temporada, TeamName_mk = equipo) %>%
  summarise(MinutesTogether = sum(minutos_compartidos, na.rm = TRUE), .groups = 'drop')

# 11.4.2. Step 2: Calculate goals conceded by each trio while they were playing together.
stats_ga_trios <- trio_minutos_partido_df %>%
  left_join(goles_recibidos_df, by = c("id_partido", "equipo" = "equipo_conceded"), relationship = "many-to-many") %>%
  filter(!is.na(minuto_gol) & minuto_gol >= start_shared & minuto_gol <= end_shared) %>%
  group_by(trio_key, competicion_nombre, competicion_temporada, TeamName_mk = equipo) %>%
  summarise(GA_Together = n(), .groups = 'drop')

# 11.4.3. Step 3: Join the statistics using left_join to prevent NAs in trio_key.
# 11.4.4. Start from trios that have minutes and add goals to them (if any).
stats_trios_defensivos_df <- stats_minutes_trios %>%
  left_join(stats_ga_trios, by = c("trio_key", "competicion_nombre", "competicion_temporada", "TeamName_mk")) %>%
  filter(!is.na(trio_key)) %>% # 11.4.5. Extra safety filter, although left_join should prevent this.
  mutate(GA_Together = replace_na(GA_Together, 0)) %>%
  mutate(GA90_Together = if_else(MinutesTogether > 0, (GA_Together / MinutesTogether) * 90, 0)) %>%
  left_join(competiciones_unicas_df %>% filter(competicion_id != "reprezentacija") %>% select(competicion_id, competicion_nombre, competicion_temporada), by = c("competicion_nombre", "competicion_temporada"))

### 11.5. Calculate National Team Career Summaries per Player
message("Calculating career summaries for the National Team per player...")

national_team_career_summary_df <- apariciones_df %>%
  filter(!is.na(id)) %>%
  # 11.5.1. Join with partidos_df to identify national team matches.
  left_join(partidos_df %>% select(id_partido, es_partido_seleccion, local, visitante), by = "id_partido") %>%
  # 11.5.2. Filter for national team matches where the player is from Macedonia.
  filter(es_partido_seleccion == TRUE, equipo == "Македонија") %>%
  # 11.5.3. Group by player, assigning fixed values for the pseudo-competition.
  group_by(id, 
           competicion_temporada = "Сите", 
           competicion_nombre = "Репрезентација", 
           equipo = "Македонија") %>%
  summarise(
    CalledUp = n_distinct(id_partido),
    Played = sum(minutos_jugados > 0, na.rm=TRUE),
    Starter = sum(tipo=="Titular", na.rm=TRUE),
    Minutes = sum(minutos_jugados, na.rm=TRUE),
    .groups = 'drop'
  ) %>%
  # 11.5.4. Join with national team goals for this player.
  left_join(
    goles_df_unificado %>%
      filter(!is.na(id), tipo == "Normal") %>%
      left_join(partidos_df %>% select(id_partido, es_partido_seleccion), by = "id_partido") %>%
      filter(es_partido_seleccion == TRUE, equipo_jugadora == "Македонија") %>%
      group_by(id) %>%
      summarise(Goals = n(), .groups = 'drop'),
    by = "id"
  ) %>%
  # 11.5.5. Join with national team cards for this player.
  left_join(
    tarjetas_df_unificado %>%
      filter(!is.na(id)) %>%
      left_join(partidos_df %>% select(id_partido, es_partido_seleccion), by = "id_partido") %>%
      filter(es_partido_seleccion == TRUE, equipo == "Македонија") %>%
      group_by(id) %>%
      summarise(Yellows = sum(tipo == "Amarilla", na.rm=T), Reds = sum(tipo == "Roja", na.rm=T), .groups = 'drop'),
    by = "id"
  ) %>%
  mutate(across(c(CalledUp, Played, Starter, Minutes, Goals, Yellows, Reds), ~replace_na(., 0)))

# 11.5.6. Optional: print a summary to verify.
message("   > National team career summaries processed. Rows: ", nrow(national_team_career_summary_df))


### 11.6. Calculate Career Summaries per Player
career_summary_jugadoras_df <- apariciones_df %>%
  filter(!is.na(id)) %>%
  group_by(id, competicion_temporada, competicion_nombre, equipo) %>%
  summarise(
    CalledUp = n_distinct(id_partido), Played = sum(minutos_jugados > 0, na.rm=TRUE),
    Starter = sum(tipo=="Titular", na.rm=TRUE), Minutes = sum(minutos_jugados, na.rm=TRUE),
    .groups = 'drop'
  ) %>%
  full_join(
    goles_df_unificado %>% filter(!is.na(id)) %>%
      left_join(partidos_df %>% select(id_partido, competicion_temporada, competicion_nombre), by="id_partido") %>%
      group_by(id, competicion_temporada, competicion_nombre, equipo = equipo_jugadora) %>%
      summarise(Goals = sum(tipo == "Normal"), .groups = 'drop'),
    by = c("id", "competicion_temporada", "competicion_nombre", "equipo")
  ) %>%
  full_join(
    tarjetas_df_unificado %>% filter(!is.na(id)) %>%
      left_join(partidos_df %>% select(id_partido, competicion_temporada, competicion_nombre), by="id_partido") %>%
      group_by(id, competicion_temporada, competicion_nombre, equipo) %>%
      summarise(Yellows = sum(tipo == "Amarilla", na.rm=T), Reds = sum(tipo == "Roja", na.rm=T), .groups = 'drop'),
    by = c("id", "competicion_temporada", "competicion_nombre", "equipo")
  ) %>%
  mutate(across(c(CalledUp, Played, Starter, Minutes, Goals, Yellows, Reds), ~replace_na(., 0))) %>%
  arrange(id, desc(competicion_temporada))

### 11.7. Calculate Team Profile Summaries
stats_equipos_por_temporada_df <- partidos_df %>%
  mutate(fecha_date = as.Date(fecha, format = "%d.%m.%Y")) %>%
  group_by(local, competicion_temporada, competicion_nombre) %>%
  summarise(last_match_date = max(fecha_date, na.rm=TRUE), .groups='drop') %>%
  rename(equipo = local) %>%
  bind_rows(
    partidos_df %>%
      mutate(fecha_date = as.Date(fecha, format = "%d.%m.%Y")) %>%
      group_by(visitante, competicion_temporada, competicion_nombre) %>%
      summarise(last_match_date = max(fecha_date, na.rm=TRUE), .groups='drop') %>%
      rename(equipo = visitante)
  ) %>%
  group_by(equipo, competicion_temporada, competicion_nombre) %>%
  summarise(last_match_date = max(last_match_date, na.rm=TRUE), .groups='drop') %>%
  arrange(equipo, desc(last_match_date))

stats_jugadoras_por_equipo_temporada_df <- apariciones_df %>%
  group_by(id, nombre, equipo, competicion_nombre, competicion_temporada) %>%
  summarise(
    CalledUp = n_distinct(id_partido),
    Played = sum(minutos_jugados > 0, na.rm = TRUE),
    Minutes = sum(minutos_jugados, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  left_join(
    goles_df_unificado %>%
      filter(tipo == "Normal") %>%
      left_join(partidos_df %>% select(id_partido, competicion_nombre, competicion_temporada), by = "id_partido") %>%
      group_by(id, equipo = equipo_jugadora, competicion_nombre, competicion_temporada) %>%
      summarise(Goals = n(), .groups = 'drop'),
    by = c("id", "equipo", "competicion_nombre", "competicion_temporada")
  ) %>%
  left_join(
    tarjetas_df_unificado %>%
      left_join(partidos_df %>% select(id_partido, competicion_nombre, competicion_temporada), by = "id_partido") %>%
      group_by(id, equipo, competicion_nombre, competicion_temporada) %>%
      summarise(Yellows = sum(tipo == "Amarilla", na.rm = TRUE), Reds = sum(tipo == "Roja", na.rm = TRUE), .groups = 'drop'),
    by = c("id", "equipo", "competicion_nombre", "competicion_temporada")
  ) %>%
  mutate(across(c(Goals, Yellows, Reds), ~replace_na(., 0))) %>%
  arrange(equipo, competicion_temporada, desc(Minutes))

### 11.8. Calculate Referee Profile Summaries
stats_arbitros_por_temporada_df <- arbitros_df %>%
  left_join(partidos_df, by = "id_partido") %>%
  mutate(fecha_date = as.Date(fecha, format = "%d.%m.%Y")) %>%
  group_by(ime, competicion_temporada, competicion_nombre) %>%
  summarise(
    num_matches = n_distinct(id_partido),
    last_match_date = max(fecha_date, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(ime, desc(last_match_date))

### 11.9. Identify Entities to Exclude from Individual Page Generation
message("Identifying entities to exclude from individual page generation...")

# 11.9.1. Get IDs of national team matches.
ids_partidos_seleccion <- partidos_df %>%
  filter(es_partido_seleccion == TRUE) %>%
  pull(id_partido) %>%
  unique()

# 11.9.2. Teams to exclude from individual page generation:
# 11.9.3. These are all national teams that have played against Macedonia.
equipos_en_partidos_seleccion <- c(
  (partidos_df %>% filter(id_partido %in% ids_partidos_seleccion) %>% pull(local)),
  (partidos_df %>% filter(id_partido %in% ids_partidos_seleccion) %>% pull(visitante))
)
team_names_to_skip_mk <- unique(equipos_en_partidos_seleccion[equipos_en_partidos_seleccion != "Македонија"])

# 11.9.4. Players to exclude from individual page generation:
# 11.9.5. They appear in a national team match AND do not play for "Македонија" in that match.
player_ids_to_skip <- apariciones_df %>%
  filter(id_partido %in% ids_partidos_seleccion, # The player played in a national team match
         equipo != "Македонија") %>%             # AND their team was NOT "Македонија"
  pull(id) %>%
  unique()

# 11.9.6. Referees who participated in national team matches.
# 11.9.7. They are excluded if they refereed a national team match.
referee_ids_to_skip <- arbitros_df %>%
  filter(id_partido %in% ids_partidos_seleccion) %>%
  pull(ime) %>%
  unique() %>%
  generar_id_seguro() # 11.9.8. Convert to safe ID for comparison in the loop.

# 11.9.9. Stadiums where national team matches were played.
# 11.9.10. They are excluded if they hosted a national team match.
stadium_ids_to_skip <- estadios_df %>%
  filter(id_partido %in% ids_partidos_seleccion) %>%
  pull(estadio) %>%
  unique() %>%
  na.omit() %>% # 11.9.11. Ensure there are no NAs in stadium names.
  generar_id_seguro() # 11.9.12. Convert to safe ID for comparison in the loop.

message(paste("   >", length(team_names_to_skip_mk), "foreign national teams will be excluded from team profiles."))
message(paste("   >", length(player_ids_to_skip), "non-Macedonian players in national team matches will be excluded from individual profiles."))
message(paste("   >", length(referee_ids_to_skip), "referees from national team matches will be excluded from individual profiles."))
message(paste("   >", length(stadium_ids_to_skip), "stadiums from national team matches will be excluded from individual profiles."))


#### 12. EXTERNALIZATION OF ASSETS (CSS AND JAVASCRIPT) ####

### 12.1. Save Stylesheet (style.css)
# 12.1.1. The entire CSS is defined as a string and written to an external file.
estilo_css <- r"(
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; line-height: 1.6; background-color: #f8f9fa; color: #212529; margin: 0; }
.container { max-width: 900px; margin: 20px auto; padding: 20px; background-color: #ffffff; border-radius: 8px; box-shadow: 0 0 15px rgba(0,0,0,0.05); }
.page { display: none; } #portal { display: block; }
h1, h2, h3 { color: #8B0000; border-bottom: 2px solid #dee2e6; padding-bottom: 10px; }
h1 { font-size: 2.5em; text-align: center; } h2 { font-size: 1.8em; margin-top: 40px; } h3 { font-size: 1.5em; }
a { color: #CC0000; text-decoration: none; font-weight: bold; } a:hover { text-decoration: underline; }
table { width: 100%; border-collapse: collapse; margin-top: 20px; }
th, td { padding: 12px; border: 1px solid #dee2e6; text-align: left; vertical-align: middle; }
th { background-color: #f2f2f2; }
.summary-row { cursor: pointer; } .summary-row:hover { background-color: #FFF0F0; }
.details-row { display: none; } .details-row > td { padding: 0; }
.details-content { padding: 20px; background-color: #fdfdfd; border-top: 2px solid #8B0000; }
.details-content h4 { font-size: 1.3em; color: #8B0000; margin-top: 10px; border-bottom: 1px solid #e0e0e0; padding-bottom: 5px;}
.back-link, .menu-button, .portal-button { display: inline-block; margin-top: 20px; padding: 10px 15px; background-color: #6c757d; color: white !important; border-radius: 5px; font-weight: bold; text-decoration: none; text-align: center;}
.back-link:hover, .menu-button:hover, .portal-button:hover { background-color: #5a6268; text-decoration: none; }
.menu-container, .portal-container { text-align: center; padding: 20px 0; display: flex; flex-wrap: wrap; justify-content: center; gap: 15px; }
.menu-button { padding: 15px 30px; font-size: 1.1em; background-color: #8B0000; color: white !important; } .menu-button:hover { background-color: #660000; }
.portal-button { width: 80%; padding: 20px; font-size: 1.3em; background-color: #8B0000; } .portal-button:hover { background-color: #660000; }
.sortable-header { cursor: pointer; user-select: none; } .sortable-header::after { content: ' '; display: inline-block; margin-left: 5px; }
.sortable-header.asc::after { content: '▲'; } .sortable-header.desc::after { content: '▼'; }
.partido-link, .partido-link-placeholder { display: flex; justify-content: space-between; align-items: center; padding: 15px; margin: 10px 0; background-color: #e9ecef; border-radius: 5px; }
.partido-link { transition: background-color 0.2s; }
.partido-link-placeholder { cursor: default; }
.partido-link:hover { background-color: #ced4da; }
.partido-link span.equipo, .partido-link-placeholder span.equipo { flex: 1 1 40%; display: flex; align-items: center; font-weight: bold; }
.partido-link span.equipo-local, .partido-link-placeholder span.equipo-local { justify-content: flex-end; }
.partido-link span.equipo-visitante, .partido-link-placeholder span.equipo-visitante { justify-content: flex-start; }
.partido-link span.resultado, .partido-link-placeholder span.resultado { flex: 0 0 12%; font-size: 1.2em; font-weight: bold; text-align: center; }
.jornada-header { background-color: #8B0000; color: white; padding: 10px; border-radius: 5px; margin-top: 30px; }
.timeline { list-style: none; padding-left: 0; } .timeline li { padding: 8px 0; border-bottom: 1px dotted #ccc; display: flex; align-items: center; }
.timeline .icon { margin-right: 10px; font-size: 1.2em; width: 24px; text-align: center; }
.alineaciones-container, .penales-container { display: flex; gap: 30px; align-items: flex-start; } .columna-alineacion, .columna-penales { flex: 1; }
.columna-alineacion h4, .columna-penales h4 { margin-top: 15px; margin-bottom: 10px; font-size: 1.2em; color: #111; border-bottom: 1px solid #ccc; padding-bottom: 5px; }
.columna-alineacion ul, .columna-penales ul { list-style: none; padding: 0; margin: 0 0 20px 0; } .columna-alineacion li, .columna-penales li { padding: 6px 3px; border-bottom: 1px solid #f0f0f0; }
.player-event { margin-left: 8px; font-size: 0.9em; color: #444; vertical-align: middle; } .player-event.goal { font-weight: bold; }
.sub-in { color: #28a745; font-style: italic; vertical-align: middle; } .sub-out { color: #dc3545; font-style: italic; vertical-align: middle; }
.card-yellow, .card-red { display: inline-block; width: 12px; height: 16px; border: 1px solid #777; border-radius: 2px; vertical-align: middle; margin-left: 4px; }
.card-yellow { background-color: #ffc107; } .card-red { background-color: #dc3545; }
.search-container { position: relative; margin: 25px 0; }
.search-container form { display: flex; }
.search-input { flex-grow: 1; font-size: 1.1em; padding: 12px; border: 1px solid #ccc; border-radius: 5px 0 0 5px; }
.search-button { font-size: 1.1em; padding: 12px 20px; border: 1px solid #8B0000; background-color: #8B0000; color: white; cursor: pointer; border-radius: 0 5px 5px 0; }
#search-suggestions { display: none; position: absolute; top: 100%; left: 0; right: 0; background-color: white; border: 1px solid #ccc; border-top: none; z-index: 1000; max-height: 300px; overflow-y: auto; box-shadow: 0 4px 8px rgba(0,0,0,0.1); }
#search-suggestions a { display: block; padding: 12px; color: #333; text-decoration: none; border-bottom: 1px solid #f0f0f0; }
#search-suggestions a:last-child { border-bottom: none; }
#search-suggestions a:hover { background-color: #f2f2f2; }
#search-suggestions a strong { color: #8B0000; }
#search-results-list ul { list-style-type: none; padding: 0; }
#search-results-list li { margin-bottom: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 4px; }
#search-results-list a { font-size: 1.2em; text-decoration: none; }
#search-results-list a:hover { text-decoration: underline; }
.search-result-type { font-size: 0.85em; color: #6c757d; margin-left: 8px; }
.clickable-row { cursor: pointer; }
.clickable-row:hover { background-color: #FFF0F0; }
.legend { margin-top: 20px; padding: 10px; text-align: left; font-size: 0.9em; }
.legend-item { display: inline-flex; align-items: center; margin-right: 20px; margin-bottom: 5px; }
.legend-color-box { width: 15px; height: 15px; border: 1px solid #ccc; margin-right: 8px; flex-shrink: 0; }
.team-logo { height: 24px; width: 24px; object-fit: contain; }
.team-logo.national-team-flag { border-radius: 50%; border: 1px solid #ccc; }
.team-cell { display: flex; align-items: center; }
.team-cell .team-logo { margin-right: 12px; }
.partido-link .team-logo { height: 28px; width: 28px; }
.partido-link .equipo-local .team-logo { margin-right: 10px; }
.partido-link .equipo-visitante .team-logo { margin-left: 10px; }
.alineacion-header { text-align: center; margin-bottom: 20px; }
.alineacion-header .match-page-crest, .team-page-crest { width: 120px; height: 120px; object-fit: contain; margin: 10px auto; display: block; }
.alineacion-header h3 { border-bottom: none; padding-bottom: 0; margin-bottom: 5px; }
.alineacion-header h3 a { color: #8B0000; }
/* --- STYLES FOR THE NEW HOMEPAGE RESULTS BLOCKS (V2) --- */
.main-content-title { text-align: left; font-size: 1.5em; color: #333; margin-bottom: 20px; }
.competition-results-block { margin-bottom: 50px; }
.results-header { display: flex; justify-content: space-between; align-items: flex-end; }
.competition-title-link { text-decoration: none; }
.competition-title { color: #CC0000; font-size: 2em; margin: 0; padding: 0; border: none; }
.results-link { color: #6c757d; font-weight: bold; text-decoration: none; font-size: 0.9em; }
.results-link:hover { color: #333; }
.separator { border: 0; border-top: 2px solid #CC0000; margin: 5px 0 15px 0; }
.results-scroll-container { overflow-x: auto; overflow-y: hidden; white-space: nowrap; padding-bottom: 15px; }
.results-row { display: flex; flex-wrap: nowrap; gap: 15px; }
.match-block { display: inline-flex; flex-direction: column; text-decoration: none; color: #333; background-color: #f8f9fa; border-radius: 5px; padding: 10px; transition: background-color 0.2s ease; border: 1px solid #e9ecef; flex-shrink: 0; }.match-block:not(.placeholder):hover { background-color: #e9ecef; }
.match-round { font-size: 0.8em; color: #6c757d; text-align: center; margin-bottom: 8px; }
.match-content { display: flex; flex-direction: column; align-items: center; }
.match-row { display: flex; justify-content: space-between; align-items: center; width: 100%; padding: 4px 0; }
.team-info { display: flex; align-items: center; }
.logo { width: 20px; height: 20px; object-fit: contain; margin-right: 8px; }
.name { font-weight: bold; font-size: 0.95em; white-space: normal; }
.score { font-weight: bold; font-size: 1.1em; color: #CC0000; }
.score-separator, .score-separator-placeholder { font-size: 1.1em; color: #CC0000; line-height: 0.5; }
.score-separator-placeholder { visibility: hidden; }
/* Custom scrollbar for better aesthetics */
.results-scroll-container::-webkit-scrollbar { height: 8px; }
.results-scroll-container::-webkit-scrollbar-track { background: #f1f1f1; border-radius: 4px; }
.results-scroll-container::-webkit-scrollbar-thumb { background: #ccc; border-radius: 4px; }
.results-scroll-container::-webkit-scrollbar-thumb:hover { background: #aaa; }
/* --- STYLES FOR THE NEW HOMEPAGE STANDINGS GRID --- */
.standings-grid-container {
  display: grid;
  grid-template-columns: repeat(4, 1fr);
  gap: 20px;
  margin-top: 20px;
}
.mini-standings-container { background-color: #fff; border: 1px solid #e9ecef; border-radius: 5px; padding: 10px; }
.mini-standings-table { width: 100%; border-collapse: collapse; font-size: 0.85em; }
.mini-standings-table th { text-align: center; color: #6c757d; font-weight: bold; padding-bottom: 8px; border-bottom: 1px solid #dee2e6;}
.mini-standings-table td { text-align: center; padding: 6px 2px; border-bottom: 1px solid #f1f1f1; }
.mini-standings-table tr:last-child td { border-bottom: none; }
.mini-logo { width: 16px; height: 16px; object-fit: contain; vertical-align: middle; }
.pos-cell { font-weight: bold; text-align: left !important; padding-left: 5px !important; }
.logo-cell { width: 20px; padding-right: 4px !important; padding-left: 0 !important; }
.abbr-cell { text-align: left !important; font-weight: bold; }
.pts-cell { font-weight: bold; }

/* Responsive Grid for Standings */
@media (max-width: 900px) {
  .standings-grid-container {
    grid-template-columns: repeat(2, 1fr);
  }
}
@media (max-width: 500px) {
  .standings-grid-container {
    grid-template-columns: 1fr;
  }
}
/* --- STYLES FOR MINI-STANDINGS HEADERS --- */
.standings-grid-title { margin-top: 40px !important; }
.mini-standings-title-link { text-decoration: none; color: inherit; }
.mini-standings-title {
  font-size: 1em;
  text-align: center;
  margin: 0 0 10px 0;
  padding-bottom: 8px;
  border-bottom: 1px solid #dee2e6;
  font-weight: bold;
  color: #333;
}
.mini-standings-title-link:hover .mini-standings-title {
  color: #CC0000;
}
)"



writeLines(estilo_css, file.path(RUTA_ASSETS_COMPARTIDOS, "style.css"))


### 12.2. Save Functionality Script (script.js)
script_js <- r"(
let searchData = [];

document.addEventListener('DOMContentLoaded', initializeSearch);

// Make the path logic more robust to work on both the `servr` local server
// and GitHub Pages.
function getSiteBasePath() {
  const path = window.location.pathname;
  // Find the part of the path before the first language folder.
  // E.g., from "/repo/mk/page.html" extracts "/repo/"
  const match = path.match(/^(.*\/)(mk|sq|es|en)\//);
  if (match && match[1]) {
    return match[1];
  }
  // Fallback for the root (e.g., `servr` locally)
  return "/";
}

function getCurrentLanguageFromPath() {
  const path = window.location.pathname;
  // Look for the 2-letter language code in the path.
  const match = path.match(/\/(mk|sq|es|en)\//);
  if (match && match[1]) {
    return match[1];
  }
  const langAttr = document.documentElement.lang;
  if (langAttr) return langAttr;
  return 'mk';
}

function initializeSearch() {
  const lang = getCurrentLanguageFromPath();
  const basePath = getSiteBasePath();
  const jsonUrl = `${basePath}${lang}/../assets/search_data_${lang}.json`;

  const searchInput = document.getElementById('search-input');
  const body = document.body;

  fetch(jsonUrl)
    .then(response => {
      if (!response.ok) {
        throw new Error('Network response was not ok for search data.');
      }
      return response.json();
    })
    .then(data => {
      searchData = data;
      if (searchInput) {
        searchInput.disabled = false;
        searchInput.placeholder = body.dataset.searchPlaceholder || 'Search...';
      }
      console.log(`Search index for '${lang}' loaded successfully.`);
    })
    .catch(error => {
      console.error('Error loading search data:', error);
      if (searchInput) {
        searchInput.placeholder = body.dataset.searchError || 'Search unavailable';
      }
    });

  document.addEventListener('click', function(event) {
    const searchContainer = document.querySelector('.search-container');
    if (searchContainer && !searchContainer.contains(event.target)) {
      const suggestions = document.getElementById('search-suggestions');
      if(suggestions) suggestions.style.display = 'none';
    }
  });
  document.addEventListener('click', function(event) {
    const clickableRow = event.target.closest('.clickable-row');
    if (clickableRow && clickableRow.dataset.href) { window.location.href = clickableRow.dataset.href; }
  });
}

function toggleDetails(elementId) {
  const detailsRow = document.getElementById(elementId);
  if (detailsRow) { detailsRow.style.display = (detailsRow.style.display === 'table-row') ? 'none' : 'table-row'; }
}

function generateLink(target_id) {
  const basePath = getSiteBasePath();
  const lang = getCurrentLanguageFromPath();
  const parts = target_id.split('-');
  const type = parts[0];
  const id_parts = parts.slice(1);
  let id = id_parts.join('-');
  let folder;

  switch(type) {
    case 'jugadora': folder = 'igraci'; break;
    case 'equipo': folder = 'timovi'; break;
    case 'arbitro': folder = 'sudii'; break;
    case 'стадион': folder = 'stadioni'; break;
    case 'menu': folder = 'natprevaruvanja'; id = id.replace('competicion-', ''); break;
    default: return `${basePath}${lang}/index.html`;
  }
  return `${basePath}${lang}/${folder}/${id}.html`;
}

function handleSearchInput(event) {
  if (event.key === 'Enter') { event.preventDefault(); showSearchResults(); return; }
  if (searchData.length === 0) return; 
  const input = document.getElementById('search-input');
  const suggestionsContainer = document.getElementById('search-suggestions');
  const query = input.value.trim().toLowerCase();
  if (query.length < 2) { suggestionsContainer.innerHTML = ''; suggestionsContainer.style.display = 'none'; return; }
  const searchTokens = query.split(' ').filter(t => t.length > 0);
  const filteredResults = searchData.filter(item => searchTokens.every(token => item.search_terms.includes(token)));
  const top5 = filteredResults.slice(0, 5);
  if (top5.length === 0) { suggestionsContainer.innerHTML = ''; suggestionsContainer.style.display = 'none'; return; }
  suggestionsContainer.innerHTML = top5.map(item => `<a href='${generateLink(item.target_id)}'><strong>${item.Име}</strong> <span class='search-result-type'>(${item.Тип})</span></a>`).join('');
  suggestionsContainer.style.display = 'block';
}

function showSearchResults() {
  if (searchData.length === 0) {
     alert(document.body.dataset.searchError || 'Search index is still loading or has failed to load. Please try again in a moment.');
     return;
  }
  const input = document.getElementById('search-input');
  const suggestionsContainer = document.getElementById('search-suggestions');
  const mainContent = document.getElementById('main-content');
  const body = document.body;
  if (!input || !mainContent) return;
  suggestionsContainer.style.display = 'none';
  const query = input.value.trim().toLowerCase();
  const originalQuery = input.value.trim();
  const basePath = getSiteBasePath();
  const lang = getCurrentLanguageFromPath();
  
  if (query.length < 2) {
    mainContent.innerHTML = `<h2>${body.dataset.searchResultsTitle || 'Search Results'}</h2><p>${body.dataset.searchPromptMsg || 'Please enter at least 2 characters.'}</p><div class="nav-buttons"><a href="${basePath}${lang}/index.html" class="back-link">← Back</a></div>`;
    return;
  }
  
  const searchTokens = query.split(' ').filter(t => t.length > 0);
  const results = searchData.filter(item => searchTokens.every(token => item.search_terms.includes(token)));
  
  let resultsHtml = `<h2>${body.dataset.searchResultsTitle || 'Search Results for'}: "${originalQuery}"</h2>`;
  if (results.length > 0) {
    resultsHtml += '<div id="search-results-list"><ul>';
    results.forEach(item => { resultsHtml += `<li><a href="${generateLink(item.target_id)}">${item.Име}<span class="search-result-type">(${item.Тип})</span></a></li>`; });
    resultsHtml += '</ul></div>';
  } else {
    resultsHtml += `<p>${body.dataset.noSearchResultsMsg || 'No results found for'} "${originalQuery}".</p>`;
  }
  resultsHtml += `<div class="nav-buttons"><a href="#" onclick="history.back(); return false;" class="back-link">← Back</a></div>`;
  mainContent.innerHTML = resultsHtml;
}

function sortTable(tableId, columnIndex) {
  const table = document.getElementById(tableId); if(!table) return;
  const tbody = table.querySelector('tbody'); const rows = Array.from(tbody.querySelectorAll('tr')); const header = table.querySelectorAll('th')[columnIndex];
  let currentDir = table.dataset.sortDir || 'desc'; let newDir = 'asc';
  if (table.dataset.sortCol == columnIndex) { newDir = currentDir === 'asc' ? 'desc' : 'asc'; }
  table.dataset.sortCol = columnIndex; table.dataset.sortDir = newDir;
  rows.sort((a, b) => {
    const valA = a.children[columnIndex].innerText; const valB = b.children[columnIndex].innerText;
    const numA = parseFloat(valA); const numB = parseFloat(valB); let comparison = 0;
    if (!isNaN(numA) && !isNaN(numB)) { comparison = numA - numB; } else { comparison = valA.localeCompare(valB, 'mk', { sensitivity: 'base' }); }
    return newDir === 'asc' ? comparison : -comparison;
  });
  tbody.innerHTML = ''; rows.forEach(row => tbody.appendChild(row));
  table.querySelectorAll('th').forEach(th => th.classList.remove('asc', 'desc'));
  if(header) header.classList.add(newDir);
}
)"
writeLines(script_js, file.path(RUTA_ASSETS_COMPARTIDOS, "script.js"))
message("style.css and script.js files saved to the assets folder.")


#### 13. HTML PAGE GENERATION (OPTIMIZED ARCHITECTURE) ####

if (hubo_cambios) {
  
  # ============================================================================ #
  # ==      MAIN GENERATION LOOP: Iterates over each language and builds the site      ==
  # ============================================================================ #
  
  for (lang in IDIOMAS_SOPORTADOS) {
    
    idioma_actual <<- lang
    message(paste("\n--- Generating pages for language:", toupper(lang), "---"))
    
    # 13.1.1. Prepare language-specific data and scripts.
    message("   > Preparing data and scripts for '", lang, "'...")
    
    player_name_col <- paste0("PlayerName_", lang)
    comp_name_col <- paste0("nombre_completo_", lang)
    entity_name_col <- paste0("translated_name_", lang)
    
    jugadoras_lang_df <- jugadoras_stats_df %>% 
      select(id, PlayerName = !!sym(player_name_col))
    
    entidades_df_lang <- entidades_maestro_df %>% 
      select(original_name, current_lang_name = !!sym(entity_name_col))
    
    search_jugadoras_data <- jugadoras_stats_df %>%
      select(id, DisplayName = !!sym(player_name_col), CyrillicName = PlayerName_mk)
    
    search_jugadoras <- search_jugadoras_data %>% 
      mutate(Тип = t("player_type"), 
             target_id = paste0("jugadora-", id), 
             search_terms = sapply(CyrillicName, generar_terminos_busqueda, USE.NAMES = FALSE)) %>% 
      select(Име = DisplayName, Тип, target_id, search_terms)
    
    search_equipos <- entidades_df_lang %>% 
      filter(original_name %in% nombres_equipos) %>% 
      mutate(Тип = t("team_type"), 
             target_id = paste0("equipo-", generar_id_seguro(original_name)), 
             search_terms = sapply(original_name, generar_terminos_busqueda, USE.NAMES=F)) %>% 
      select(Име = current_lang_name, Тип, target_id, search_terms)
    
    search_arbitros <- entidades_df_lang %>% 
      filter(original_name %in% nombres_arbitros) %>% 
      mutate(Тип = t("referee_type"), 
             target_id = paste0("arbitro-", generar_id_seguro(original_name)), 
             search_terms = sapply(original_name, generar_terminos_busqueda, USE.NAMES=F)) %>% 
      select(Име = current_lang_name, Тип, target_id, search_terms)
    
    search_estadios <- entidades_df_lang %>% 
      filter(original_name %in% nombres_estadios) %>% 
      mutate(Тип = t("stadium_type"), 
             target_id = paste0("стадион-", generar_id_seguro(original_name)), 
             search_terms = sapply(original_name, generar_terminos_busqueda, USE.NAMES=F)) %>% 
      select(Име = current_lang_name, Тип, target_id, search_terms)
    
    search_competiciones <- competiciones_unicas_df %>% 
      mutate(Име = !!sym(comp_name_col), 
             Тип = t("competition_type"), 
             target_id = paste0("menu-competicion-", competicion_id), 
             search_terms = sapply(nombre_completo_mk, generar_terminos_busqueda, USE.NAMES = FALSE)) %>% 
      select(Име, Тип, target_id, search_terms)
    
    search_index_df_lang <- bind_rows(search_jugadoras, search_equipos, search_arbitros, search_competiciones, search_estadios) %>% arrange(Име)
    
    search_data_json_lang <- toJSON(search_index_df_lang, auto_unbox = TRUE)
    ruta_json_salida <- file.path(RUTA_ASSETS_COMPARTIDOS, paste0("search_data_", lang, ".json"))
    writeLines(search_data_json_lang, ruta_json_salida, useBytes = TRUE)
    message("     > Search index saved to: ", basename(ruta_json_salida))
    
    if (PROTEGER_CON_CONTRASENA) {
      la_contrasena <- "secreto123"
      message("     > Password protection ENABLED.")
      script_contraseña_lang <- tags$script(HTML(
        sprintf(
          "(function() { var p = '%s'; var s = sessionStorage; var d = document; if (s.getItem('zfudbalmk-password-ok') === p) return; var i; var m = '%s'; while (true) { i = prompt(m, ''); if (i === p) { s.setItem('zfudbalmk-password-ok', i); break; } if (i === null) { d.body.innerHTML = '<div style=\"text-align:center; padding: 50px; font-family: sans-serif;\"><h1>%s</h1><p>%s</p></div>'; throw new Error('Access denied'); } m = '%s'; } })();",
          la_contrasena, t("password_prompt"), t("access_denied_header"), t("access_denied_body"), t("password_wrong")
        )
      ))
    } else {
      message("     > Password protection DISABLED.")
      script_contraseña_lang <- NULL
    }
    
    # 13.1.2. Generate Home Page (Portal) with Latest Results and Standings.
    message("   > Generating new dynamic index.html...")
    
    temporada_actual_str <- obtener_temporada_actual()
    message(paste("     > Current season determined as:", temporada_actual_str))
    
    competiciones_en_portada_mk <- c(
      "Прва ЖФЛ", "Втора ЖФЛ", "Младинска Женска Фудбалска Лига", "Кадетска Женска Фудбалска Лига"
    )
    
    equipos_en_portada_mk <- partidos_df %>%
      filter(competicion_temporada == temporada_actual_str, competicion_nombre %in% competiciones_en_portada_mk) %>%
      distinct(local, visitante) %>%
      pivot_longer(everything(), names_to = NULL, values_to = "equipo") %>%
      distinct(equipo) %>% pull(equipo)
    
    if (length(equipos_en_portada_mk) > 0) {
      nombres_traducidos <- (entidades_df_lang %>% filter(original_name %in% equipos_en_portada_mk))$current_lang_name
      longitud_max_nombre <- max(nchar(nombres_traducidos), na.rm = TRUE)
      ancho_ficha_calculado_px <- (longitud_max_nombre * 8) + 60
    } else {
      ancho_ficha_calculado_px <- 220
    }
    message(paste("     > Calculated match block width:", ancho_ficha_calculado_px, "px"))
    
    abreviaturas_actual_lang_df <- if (!is.null(mapa_abreviaturas_long_df)) {
      mapa_abreviaturas_long_df %>% filter(lang == idioma_actual)
    } else {
      tibble(original_mk = character(), lang=character(), abbreviation = character())
    }
    
    lista_componentes_html <- map(competiciones_en_portada_mk, function(nombre_comp_mk) {
      comp_info <- competiciones_unicas_df %>%
        filter(competicion_nombre == nombre_comp_mk, competicion_temporada == temporada_actual_str)
      if (nrow(comp_info) == 0) return(NULL)
      
      comp_nombre_actual_lang <- comp_info[[comp_name_col]][1]
      
      ultima_jornada_jugada_df <- partidos_df %>%
        filter(competicion_nombre == nombre_comp_mk, competicion_temporada == temporada_actual_str, !is.na(id_partido))
      
      bloque_resultados_html <- NULL
      if (nrow(ultima_jornada_jugada_df) > 0) {
        ultima_jornada_jugada <- max(as.numeric(ultima_jornada_jugada_df$jornada), na.rm = TRUE)
        if (!is.infinite(ultima_jornada_jugada) && !is.na(ultima_jornada_jugada)) {
          partidos_para_mostrar <- partidos_df %>%
            filter(competicion_nombre == nombre_comp_mk, competicion_temporada == temporada_actual_str, as.numeric(jornada) == ultima_jornada_jugada)
          if (nrow(partidos_para_mostrar) > 0) {
            bloque_resultados_html <- crear_bloque_resultados_competicion(
              comp_info, partidos_para_mostrar, comp_nombre_actual_lang, entidades_df_lang, ancho_ficha_calculado_px
            )
          }
        }
      }
      
      tabla_clasificacion_html <- crear_tabla_clasificacion_portada(
        comp_info, stats_clasificacion_por_comp_df, entidades_df_lang, 
        abreviaturas_actual_lang_df, estilos_clasificacion_data,
        # ¡Pasamos el nombre de la competición a la función de la tabla!
        comp_nombre_lang = comp_nombre_actual_lang 
      )
      
      list(resultados = bloque_resultados_html, clasificacion = tabla_clasificacion_html)
    }) %>% purrr::compact()
    
    contenido_portal_dinamico <- tagList(
      tags$h3(class="main-content-title", t("latest_results_title")),
      map(lista_componentes_html, "resultados"),
      
      # ¡Añadimos el título principal para las clasificaciones!
      tags$h3(class="main-content-title standings-grid-title", t("standings_main_title")),
      tags$div(class="standings-grid-container",
               map(lista_componentes_html, "clasificacion")
      )
    )
    
    pagina_portal_final <- crear_pagina_html(
      contenido_principal = contenido_portal_dinamico,
      titulo_pagina = t("site_title"),
      path_to_root_dir = "..",
      script_contraseña = script_contraseña_lang
    )
    save_html(pagina_portal_final, file = file.path(RUTA_SALIDA_RAIZ, lang, "index.html"))
    
    # 13.1.3. Generate Competition Pages.
    message("   > Generating competition pages...")
    walk(1:nrow(competiciones_unicas_df), function(i) {
      comp_info <- competiciones_unicas_df[i,]; comp_id <- comp_info$competicion_id
      
      # 13.1.4. If the id is "reprezentacija", use the new logic.
      if (comp_id == "reprezentacija") {
        
        # 13.1.5. LOGIC FOR THE NATIONAL TEAM PAGE.
        if (!full_rebuild_needed && !(comp_id %in% affected_competition_ids)) { return() }
        
        comp_nombre_current_lang <- comp_info[[comp_name_col]]
        
        # 13.1.6. 1. Find all unique categories for the national team matches.
        categorias_seleccion <- partidos_df %>%
          filter(es_partido_seleccion == TRUE) %>%
          distinct(categoria) %>%
          filter(!is.na(categoria)) %>%
          arrange(categoria) %>%
          pull(categoria)
        
        # 13.1.7. 2. Create the menu buttons, one for each category.
        lista_botones_menu_seleccion <- map(categorias_seleccion, function(cat) {
          # 13.1.8. The HTML filename is based on the sanitized category.
          nombre_archivo_cat <- paste0(comp_id, "_", generar_id_seguro(cat), ".html")
          tags$a(href = nombre_archivo_cat, class = "menu-button", cat)
        })
        
        # 13.1.9. 3. Generate the main (menu) page for "Репрезентација".
        contenido_menu_seleccion <- tagList(
          crear_botones_navegacion(path_to_lang_root = ".."),
          tags$h2(comp_nombre_current_lang),
          tags$div(class = "menu-container", lista_botones_menu_seleccion)
        )
        save_html(
          crear_pagina_html(contenido_menu_seleccion, comp_nombre_current_lang, "../..", script_contraseña_lang),
          file = file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, paste0(comp_id, ".html"))
        )
        
        # 13.1.10. 4. Generate a page for EACH category.
        walk(categorias_seleccion, function(cat_actual) {
          
          # 13.1.11. Filter national team matches for this category and sort them.
          partidos_categoria <- partidos_df %>%
            filter(es_partido_seleccion == TRUE, categoria == cat_actual) %>%
            mutate(fecha_parsed = as.Date(fecha, format = "%d.%m.%Y")) %>%
            arrange(desc(fecha_parsed))
          
          # 13.1.12. Function to create a team logo tag.
          get_logo_tag <- function(nombre_equipo_mk) { 
            iso_code <- get_national_team_iso(nombre_equipo_mk)
            if (!is.na(iso_code)) {
              # 13.1.13. It's a national team, use flag URL.
              flag_url <- paste0("https://hatscripts.github.io/circle-flags/flags/", iso_code, ".svg")
              tags$img(class="team-logo national-team-flag", src = flag_url, alt = nombre_equipo_mk)
            } else {
              # 13.1.14. It's a club team, use local logo path.
              nombre_archivo_final <- paste0(generar_id_seguro(nombre_equipo_mk), ".png")
              if (!file.exists(file.path(RUTA_LOGOS_DESTINO, nombre_archivo_final))) { nombre_archivo_final <- "NOLOGO.png" }
              ruta_relativa_logo_html <- file.path("..", "..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, nombre_archivo_final)
              tags$img(class="team-logo", src = ruta_relativa_logo_html, alt = nombre_equipo_mk) 
            }
          }
          
          # 13.1.15. Generate the list of matches (without grouping by matchday).
          contenido_lista_partidos <- tagList(
            crear_botones_navegacion(path_to_lang_root = ".."), 
            tags$h2(paste(comp_nombre_current_lang, "-", cat_actual)),
            
            map(1:nrow(partidos_categoria), function(k) {
              partido <- partidos_categoria[k,]; is_placeholder_match <- is.na(partido$id_partido)
              local_name <- entidades_df_lang$current_lang_name[match(partido$local, entidades_df_lang$original_name)]
              visitante_name <- entidades_df_lang$current_lang_name[match(partido$visitante, entidades_df_lang$original_name)]
              resultado_texto <- if (is_placeholder_match) " - " else { res_base <- paste(partido$goles_local, "-", partido$goles_visitante); if (!is.na(partido$penales_local)) res_base <- sprintf("%s (%s - %s)", res_base, partido$penales_local, partido$penales_visitante); if (isTRUE(partido$es_resultado_oficial)) res_base <- paste(res_base, "*"); res_base }
              
              # 13.1.16. The visual content of the match (teams and result).
              contenido_comun <- tagList(
                tags$span(class="equipo equipo-local", get_logo_tag(partido$local), tags$span(local_name)), 
                tags$span(class="resultado", resultado_texto), 
                tags$span(class="equipo equipo-visitante", tags$span(visitante_name), get_logo_tag(partido$visitante))
              )
              
              # 13.1.17. Wrap everything in a tagList to add the date above the match link.
              tagList(
                # 13.1.18. Add the match date here, using the 'fecha' column from the 'partido' object.
                tags$p(
                  style = "text-align: center; margin-bottom: 2px; margin-top: 15px; font-size: 0.9em; color: #555;", 
                  partido$fecha
                ),
                # 13.1.19. The original if/else block to create the link or placeholder.
                if (is_placeholder_match) {
                  tags$div(class = "partido-link-placeholder", contenido_comun)
                } else {
                  tags$a(class = "partido-link", href = file.path("..", nombres_carpetas_relativos$partidos, paste0(partido$id_partido, ".html")), contenido_comun)
                }
              )
            })
          )
          
          # 13.1.20. Save the HTML file for the category.
          nombre_archivo_cat_final <- paste0(comp_id, "_", generar_id_seguro(cat_actual), ".html")
          titulo_pagina_cat <- paste(t("category_page_title"), "-", comp_nombre_current_lang, "-", cat_actual)
          save_html(
            crear_pagina_html(contenido_lista_partidos, titulo_pagina_cat, "../..", script_contraseña_lang),
            file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, nombre_archivo_cat_final)
          )
        })
        
      } else {
        
        # 13.1.21. ORIGINAL LOGIC FOR NORMAL COMPETITIONS.
        if (!full_rebuild_needed && !(comp_id %in% affected_competition_ids)) { return() }
        comp_nombre_current_lang <- comp_info[[comp_name_col]]
        is_cup <- str_detect(tolower(comp_info$competicion_nombre), "куп")
        is_friendly_comp <- str_detect(tolower(comp_info$competicion_nombre), "пријателски")
        player_name_col_sym <- rlang::sym(player_name_col)
        lista_botones_menu <- list()
        partidos_comp <- partidos_df %>% filter(competicion_nombre == comp_info$competicion_nombre, competicion_temporada == comp_info$competicion_temporada)
        is_placeholder_only_comp <- all(is.na(partidos_comp$id_partido))
        jornadas_comp <- if (nrow(partidos_comp) > 0) { 
          data.frame(jornada = unique(partidos_comp$jornada)) %>% 
            mutate(order_key = case_when(
              str_detect(jornada, "1/64") ~ 1,
              str_detect(jornada, "1/32") ~ 2,
              str_detect(jornada, "1/16") ~ 3,
              str_detect(jornada, "1/8") ~ 4,
              str_detect(jornada, "1/4") ~ 5,
              str_detect(jornada, "1/2") ~ 6,
              str_detect(jornada, "3/4") ~ 6.5, # 13.1.22. ADDED LINE: Recognizes the 3rd place match.
              str_detect(jornada, "Ф$|ф$|финале") ~ 7,
              !is_cup ~ as.numeric(suppressWarnings(jornada)),
              TRUE ~ 99
            )) %>% 
            arrange(order_key) %>% 
            pull(jornada) 
        }
        else { 
          c() 
        }       
        contenido_partidos <- tagList(crear_botones_navegacion(path_to_lang_root = ".."), tags$h2(paste(t("schedule_title"), "-", comp_nombre_current_lang)), map(jornadas_comp, function(j) { partidos_jornada <- partidos_comp %>% filter(jornada == j) %>% arrange(local); header_text <- if(is_cup || is_friendly_comp) as.character(j) else paste(t("round_prefix"), j); get_logo_tag <- function(nombre_equipo_mk) { nombre_archivo_final <- paste0(generar_id_seguro(nombre_equipo_mk), ".png"); if (!file.exists(file.path(RUTA_LOGOS_DESTINO, nombre_archivo_final))) { nombre_archivo_final <- "NOLOGO.png" }; ruta_relativa_logo_html <- file.path("..", "..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, nombre_archivo_final); tags$img(class="team-logo", src = ruta_relativa_logo_html, alt = nombre_equipo_mk) }; tagList(tags$h3(class="jornada-header", header_text), map(1:nrow(partidos_jornada), function(k) { partido <- partidos_jornada[k,]; is_placeholder_match <- is.na(partido$id_partido); local_name <- entidades_df_lang$current_lang_name[match(partido$local, entidades_df_lang$original_name)]; visitante_name <- entidades_df_lang$current_lang_name[match(partido$visitante, entidades_df_lang$original_name)]; resultado_texto <- if (is_placeholder_match) " - " else { res_base <- paste(partido$goles_local, "-", partido$goles_visitante); if (!is.na(partido$penales_local)) res_base <- sprintf("%s (%s - %s)", res_base, partido$penales_local, partido$penales_visitante); if (isTRUE(partido$es_resultado_oficial)) res_base <- paste(res_base, "*"); res_base }; contenido_comun <- tagList(tags$span(class="equipo equipo-local", get_logo_tag(partido$local), tags$span(local_name)), tags$span(class="resultado", resultado_texto), tags$span(class="equipo equipo-visitante", tags$span(visitante_name), get_logo_tag(partido$visitante))); if (is_placeholder_match) tags$div(class = "partido-link-placeholder", contenido_comun) else tags$a(class = "partido-link", href = file.path("..", nombres_carpetas_relativos$partidos, paste0(partido$id_partido, ".html")), contenido_comun) })) }))
        nombre_archivo_partidos <- paste0(comp_id, "_", nombres_archivos_mk$partidos, ".html"); save_html(crear_pagina_html(contenido_partidos, paste(t("schedule_title"), "-", comp_nombre_current_lang), "../..", script_contraseña_lang), file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, nombre_archivo_partidos))
        lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href=nombre_archivo_partidos, class="menu-button", t("schedule_title"))
        if (!is_placeholder_only_comp) {
          tabla_goleadoras_comp <- stats_goleadoras_por_comp_df %>% filter(competicion_id == comp_id) %>% left_join(jugadoras_stats_df %>% select(id, !!player_name_col_sym), by = "id") %>% filter(!is.na(!!player_name_col_sym)) %>% select(Pos, id, PlayerName = !!player_name_col_sym, TeamNames_mk, Goals); headers_traducidos <- c(t("standings_pos"), t("player_type"), t("team_type"), t("stats_goals")); contenido_goleadoras <- tagList(crear_botones_navegacion(path_to_lang_root = ".."), tags$h2(paste(t("scorers_title"), "-", comp_nombre_current_lang)), tags$table(tags$thead(tags$tr(map(headers_traducidos, tags$th))), tags$tbody(map(1:nrow(tabla_goleadoras_comp), function(j){ g <- tabla_goleadoras_comp[j,]; tags$tr(tags$td(g$Pos), tags$td(tags$a(href=file.path("..", nombres_carpetas_relativos$jugadoras, paste0(g$id, ".html")), g$PlayerName)), tags$td({ teams_mk <- str_split(g$TeamNames_mk, " / ")[[1]]; team_tags <- list(); for (i in seq_along(teams_mk)) { team_name_mk <- teams_mk[i]; team_name <- entidades_df_lang %>% filter(original_name == team_name_mk) %>% pull(current_lang_name); nombre_archivo_final <- paste0(generar_id_seguro(team_name_mk), ".png"); if (!file.exists(file.path(RUTA_LOGOS_DESTINO, nombre_archivo_final))) { nombre_archivo_final <- "NOLOGO.png" }; ruta_relativa_logo_html <- file.path("..", "..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, nombre_archivo_final); team_element <- tags$span(class="team-cell", tags$img(class="team-logo", src = ruta_relativa_logo_html, alt = team_name), tags$a(href = file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(team_name_mk), ".html")), team_name)); team_tags <- append(team_tags, list(team_element)); if (i < length(teams_mk)) { team_tags <- append(team_tags, list(tags$span(style="margin: 0 5px;", "/"))) } }; tagList(team_tags) }), tags$td(g$Goals)) }))))
          nombre_archivo_goleadoras <- paste0(comp_id, "_", nombres_archivos_mk$goleadoras, ".html"); save_html(crear_pagina_html(contenido_goleadoras, paste(t("scorers_title"), "-", comp_nombre_current_lang), "../..", script_contraseña_lang), file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, nombre_archivo_goleadoras))
          lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href=nombre_archivo_goleadoras, class="menu-button", t("scorers_title"))
        }
        if (!is_cup && !is_friendly_comp && !is_placeholder_only_comp) {
          clasificacion_df_comp_raw <- stats_clasificacion_por_comp_df %>% filter(competicion_id == comp_id); clave_estilo_comp <- paste(comp_info$competicion_nombre, comp_info$competicion_temporada); contenido_tabla <- if (nrow(clasificacion_df_comp_raw) == 0) { tags$p(t("standings_no_data_message")) } else { clasificacion_df_comp_raw_lang <- clasificacion_df_comp_raw %>% left_join(entidades_df_lang, by = c("team" = "original_name")) %>% select(Pos, team_lang = current_lang_name, P, W, D, L, GF, GA, GD, Pts); nombres_neutros <- c("Pos", "team_lang", "P", "W", "D", "L", "GF", "GA", "GD", "Pts"); claves_traduccion <- c("standings_pos", "standings_team", "standings_p", "standings_w", "standings_d", "standings_l", "standings_gf", "standings_ga", "standings_gd", "standings_pts"); nombres_traducidos <- sapply(claves_traduccion, t, USE.NAMES = FALSE); mapa_nombres_col <- setNames(as.list(nombres_neutros), nombres_traducidos); clasificacion_df_comp <- clasificacion_df_comp_raw_lang %>% rename(!!!mapa_nombres_col); estilos_comp <- estilos_clasificacion_data[[clave_estilo_comp]]; tagList(tags$table(tags$thead(tags$tr(map(names(clasificacion_df_comp), tags$th))), tags$tbody(map(1:nrow(clasificacion_df_comp), function(j) { fila <- clasificacion_df_comp[j,]; nombre_equipo <- fila[[t("standings_team")]]; posicion_equipo <- fila[[t("standings_pos")]]; nombre_equipo_original <- clasificacion_df_comp_raw$team[j]; nombre_archivo_final <- paste0(generar_id_seguro(nombre_equipo_original), ".png"); if (!file.exists(file.path(RUTA_LOGOS_DESTINO, nombre_archivo_final))) { nombre_archivo_final <- "NOLOGO.png" }; ruta_relativa_logo_html <- file.path("..", "..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, nombre_archivo_final); regla_actual <- NULL; if (!is.null(estilos_comp)) { regla_match <- estilos_comp$reglas %>% filter(puesto == posicion_equipo); if (nrow(regla_match) > 0) { regla_actual <- regla_match[1,] } }; tags$tr(map(seq_along(fila), function(k) { cell_value <- fila[[k]]; col_name <- names(fila)[k]; if (col_name == t("standings_pos") && !is.null(regla_actual)) { tags$td(style = paste0("border-left: 5px solid ", regla_actual$color, "; font-weight: bold;"), cell_value) } else if (col_name == t("standings_team")) { tags$td(class = "team-cell", tags$img(class="team-logo", src = ruta_relativa_logo_html, alt = nombre_equipo), tags$a(href=file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(nombre_equipo_original), ".html")), cell_value)) } else { tags$td(cell_value) }})) }))), if (!is.null(estilos_comp) && length(estilos_comp$leyenda) > 0) { tags$div(class = "legend", map(estilos_comp$leyenda, function(item_leyenda) { tags$div(class = "legend-item", tags$span(class = "legend-color-box", style = paste0("background-color: ", item_leyenda$color, ";")), tags$span(t(item_leyenda$texto_key))) })) }) }; contenido_clasificacion <- tagList(crear_botones_navegacion(path_to_lang_root = ".."), tags$h2(paste(t("standings_title"), "-", comp_nombre_current_lang)), contenido_tabla); nombre_archivo_clasif <- paste0(comp_id, "_", nombres_archivos_mk$clasificacion, ".html"); save_html(crear_pagina_html(contenido_clasificacion, paste(t("standings_title"), "-", comp_nombre_current_lang), "../..", script_contraseña_lang), file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, nombre_archivo_clasif)); lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href=nombre_archivo_clasif, class="menu-button", t("standings_title"))
          # 1. Calcular minutos totales POSIBLES solo de partidos JUGADOS.
          minutos_totales_equipo_comp <- partidos_df %>%
            filter(
              competicion_nombre == comp_info$competicion_nombre,
              competicion_temporada == comp_info$competicion_temporada,
              !is.na(id_partido) # ¡LA CLAVE! Solo partidos con acta (ID).
            ) %>%
            select(local, visitante, duracion_partido) %>%
            pivot_longer(cols = c(local, visitante), names_to = "tipo_equipo", values_to = "equipo") %>%
            group_by(equipo) %>%
            summarise(minutos_totales_posibles = sum(duracion_partido, na.rm = TRUE), .groups = 'drop')
          
          # 2. El resto de la lógica permanece igual, pero ahora usará el total correcto.
          tabla_porteras_comp_raw <- stats_porteras_por_comp_df %>% filter(competicion_id == comp_id) %>% left_join(jugadoras_stats_df %>% select(id, !!player_name_col_sym), by = "id") %>% left_join(entidades_df_lang, by = c("TeamName_mk" = "original_name")) %>% left_join(minutos_totales_equipo_comp, by = c("TeamName_mk" = "equipo")) %>% mutate(pct_minutos = if_else(!is.na(minutos_totales_posibles) & minutos_totales_posibles > 0, (Minutes / minutos_totales_posibles) * 100, 0), group = if_else(pct_minutos >= 50, "mas_50", "menos_50")) %>% select(id, PlayerName = !!player_name_col_sym, TeamName = current_lang_name, TeamName_mk, GA90, GA, Minutes, CS, group)
          if (nrow(tabla_porteras_comp_raw) > 0) { porteras_mas_50 <- tabla_porteras_comp_raw %>% filter(group == "mas_50") %>% arrange(GA90, desc(CS)) %>% mutate(Pos = row_number()); porteras_menos_50 <- tabla_porteras_comp_raw %>% filter(group == "menos_50", Minutes > 0) %>% arrange(GA90, desc(CS)) %>% mutate(Pos = row_number()); generar_tabla_html_porteras <- function(df, table_id) { if (is.null(df) || nrow(df) == 0) { return(tags$p(t("no_data_in_category")))}; tags$table(id = table_id, `data-sort-col` = "3", `data-sort-dir` = "asc", tags$thead(tags$tr(tags$th(t("standings_pos")), tags$th(t("player_type")), tags$th(t("team_type")), tags$th(class="sortable-header asc", onclick=sprintf("sortTable('%s', 3)", table_id), t("gk_ga_90")), tags$th(t("gk_ga")), tags$th(t("stats_minutes")), tags$th(class="sortable-header", onclick=sprintf("sortTable('%s', 6)", table_id), t("gk_cs")))), tags$tbody(map(1:nrow(df), function(j){ p <- df[j,]; nombre_equipo <- p$TeamName; nombre_equipo_mk <- p$TeamName_mk; nombre_archivo_final <- paste0(generar_id_seguro(nombre_equipo_mk), ".png"); if (!file.exists(file.path(RUTA_LOGOS_DESTINO, nombre_archivo_final))) { nombre_archivo_final <- "NOLOGO.png" }; ruta_relativa_logo_html <- file.path("..", "..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, nombre_archivo_final); tags$tr(tags$td(p$Pos), tags$td(tags$a(href=file.path("..", nombres_carpetas_relativos$jugadoras, paste0(p$id, ".html")), p$PlayerName)), tags$td(class = "team-cell", tags$img(class="team-logo", src = ruta_relativa_logo_html, alt = nombre_equipo), tags$a(href=file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(nombre_equipo_mk), ".html")), nombre_equipo)), tags$td(format(round(p$GA90, 2), nsmall = 2)), tags$td(p$GA), tags$td(p$Minutes), tags$td(p$CS)) })))}; contenido_porteras <- tagList(crear_botones_navegacion(path_to_lang_root = ".."), tags$h2(paste(t("goalkeepers_title"), "-", comp_nombre_current_lang)), tags$h3(t("gk_stats_header_over_50")), generar_tabla_html_porteras(porteras_mas_50, "tabla-porteras-mas-50"), tags$h3(t("gk_stats_header_under_50")), generar_tabla_html_porteras(porteras_menos_50, "tabla-porteras-menos-50")); nombre_archivo_porteras <- paste0(comp_id, "_golmanki.html"); save_html(crear_pagina_html(contenido_porteras, paste(t("goalkeepers_title"), "-", comp_nombre_current_lang), "../..", script_contraseña_lang), file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, nombre_archivo_porteras)); lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href=nombre_archivo_porteras, class="menu-button", t("goalkeepers_title")) }
          
          # --- BLOQUE DE TRÍOS DEFENSIVOS DESACTIVADO TEMPORALMENTE ---
          # if (str_detect(comp_info$competicion_nombre, "Прва|Втора")) {
          #   partidos_en_comp <- partidos_df %>% filter(competicion_nombre == comp_info$competicion_nombre, competicion_temporada == comp_info$competicion_temporada); minutos_totales_equipo_comp <- bind_rows(partidos_en_comp %>% count(TeamName_mk = local), partidos_en_comp %>% count(TeamName_mk = visitante)) %>% group_by(TeamName_mk) %>% summarise(P = sum(n), .groups = 'drop') %>% mutate(minutos_totales_posibles = P * 90) %>% select(TeamName_mk, minutos_totales_posibles)
          #   tabla_final_defensas <- stats_trios_defensivos_df %>%
          #     filter(competicion_id == comp_id) %>%
          #     left_join(minutos_totales_equipo_comp, by = "TeamName_mk") %>%
          #     filter(!is.na(minutos_totales_posibles), MinutesTogether >= (minutos_totales_posibles * 0.5)) %>%
          #     group_by(TeamName_mk) %>%
          #     arrange(GA90_Together, GA_Together, desc(MinutesTogether)) %>%
          #     slice_head(n = 1) %>%
          #     ungroup() %>%
          #     left_join(entidades_df_lang, by = c("TeamName_mk" = "original_name")) %>%
          #     mutate(TeamName = current_lang_name) %>%
          #     rowwise() %>%
          #     mutate(TrioNames = {
          #       if (is.na(trio_key) || nchar(trio_key) == 0) {
          #         NA_character_
          #       } else {
          #         ids_del_trio <- strsplit(trio_key, "-")[[1]]
          #         nombres_encontrados <- jugadoras_stats_df %>% filter(id %in% ids_del_trio) %>% select(id, PlayerName = !!player_name_col_sym)
          #         nombres_ordenados <- sapply(ids_del_trio, function(id_actual) {
          #           nombre <- (nombres_encontrados %>% filter(id == id_actual) %>% pull(PlayerName))[1]
          #           if (is.na(nombre)) id_actual else nombre
          #         })
          #         paste(nombres_ordenados, collapse = " - ")
          #       }
          #     }) %>%
          #     ungroup() %>%
          #     filter(!is.na(TrioNames)) %>%
          #     arrange(GA90_Together, GA_Together, desc(MinutesTogether)) %>%
          #     mutate(Pos = row_number()) %>%
          #     select(Pos, TrioNames, TeamName, TeamName_mk, MinutesTogether, GA_Together, GA90_Together, trio_key)
          #   if (nrow(tabla_final_defensas) > 0) { contenido_defensas <- tagList(crear_botones_navegacion(path_to_lang_root = ".."), tags$h2(paste(t("defensive_trio_title"), "-", comp_nombre_current_lang)), tags$p(style="text-align:center; font-style:italic; color:#555;", t("defensive_trio_subtitle")), tags$table(class = "main-summary-table", tags$thead(tags$tr(tags$th(t("standings_pos")), tags$th(t("defensive_trio_header_trio")), tags$th(t("team_type")), tags$th(t("defensive_trio_header_minutes")), tags$th(t("defensive_trio_header_ga")), tags$th(t("defensive_trio_header_ga90")))), tags$tbody(pmap(tabla_final_defensas, function(...) { fila <- list(...); nombre_equipo <- fila$TeamName; nombre_equipo_mk <- fila$TeamName_mk; nombre_archivo_final <- paste0(generar_id_seguro(nombre_equipo_mk), ".png"); if (!file.exists(file.path(RUTA_LOGOS_DESTINO, nombre_archivo_final))) { nombre_archivo_final <- "NOLOGO.png" }; ruta_relativa_logo_html <- file.path("..", "..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, nombre_archivo_final); tags$tr(tags$td(fila$Pos), tags$td(fila$TrioNames), tags$td(class="team-cell", tags$img(class="team-logo", src = ruta_relativa_logo_html, alt = nombre_equipo), tags$a(href=file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(nombre_equipo_mk), ".html")), onclick="event.stopPropagation();", nombre_equipo)), tags$td(round(fila$MinutesTogether)), tags$td(fila$GA_Together), tags$td(format(round(fila$GA90_Together, 2), nsmall = 2))) }))))
          #   nombre_archivo_defensas <- paste0(comp_id, "_defanzivno_trio.html"); save_html(crear_pagina_html(contenido_defensas, paste(t("defensive_trio_title"), "-", comp_nombre_current_lang), "../..", script_contraseña_lang), file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, nombre_archivo_defensas)); lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href=nombre_archivo_defensas, class="menu-button", t("defensive_trio_title")) }
          # }
          
          tabla_sanciones_comp <- stats_sanciones_por_comp_df %>% filter(competicion_id == comp_id) %>% left_join(jugadoras_stats_df %>% select(id, !!player_name_col_sym), by = "id") %>% filter(!is.na(!!player_name_col_sym)) %>% select(Pos, id, PlayerName = !!player_name_col_sym, TeamNames_mk, YellowCards, RedCards); contenido_sanciones <- tagList(crear_botones_navegacion(path_to_lang_root = ".."), tags$h2(paste(t("disciplinary_title"), "-", comp_nombre_current_lang)), tags$table(tags$thead(tags$tr(tags$th(t("standings_pos")), tags$th(t("player_type")), tags$th(t("team_type")), tags$th(HTML("<span class='card-yellow'></span>")), tags$th(HTML("<span class='card-red'></span>")))), tags$tbody(if(nrow(tabla_sanciones_comp) > 0) { map(1:nrow(tabla_sanciones_comp), function(j) { s <- tabla_sanciones_comp[j,]; tags$tr(tags$td(s$Pos), tags$td(tags$a(href=file.path("..", nombres_carpetas_relativos$jugadoras, paste0(s$id, ".html")), s$PlayerName)), tags$td({ teams_mk <- str_split(s$TeamNames_mk, " / ")[[1]]; team_tags <- list(); for (i in seq_along(teams_mk)) { team_name_mk <- teams_mk[i]; team_name <- entidades_df_lang %>% filter(original_name == team_name_mk) %>% pull(current_lang_name); nombre_archivo_final <- paste0(generar_id_seguro(team_name_mk), ".png"); if (!file.exists(file.path(RUTA_LOGOS_DESTINO, nombre_archivo_final))) { nombre_archivo_final <- "NOLOGO.png" }; ruta_relativa_logo_html <- file.path("..", "..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, nombre_archivo_final); team_element <- tags$span(class="team-cell", tags$img(class="team-logo", src = ruta_relativa_logo_html, alt = team_name), tags$a(href = file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(team_name_mk), ".html")), team_name)); team_tags <- append(team_tags, list(team_element)); if (i < length(teams_mk)) { team_tags <- append(team_tags, list(tags$span(style="margin: 0 5px;", "/"))) }}; tagList(team_tags) }), tags$td(s$YellowCards), tags$td(s$RedCards)) })} else { tags$tr(tags$td(colspan="5", t("disciplinary_no_cards_message"))) })))
          nombre_archivo_sanciones <- paste0(comp_id, "_", nombres_archivos_mk$sanciones, ".html"); save_html(crear_pagina_html(contenido_sanciones, paste(t("disciplinary_title"), "-", comp_nombre_current_lang), "../..", script_contraseña_lang), file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, nombre_archivo_sanciones)); lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href=nombre_archivo_sanciones, class="menu-button", t("disciplinary_title"))
        }
        contenido_menu_final <- tagList(crear_botones_navegacion(path_to_lang_root = ".."), tags$h2(comp_nombre_current_lang), tags$div(class="menu-container", lista_botones_menu))
        save_html(crear_pagina_html(contenido_menu_final, comp_nombre_current_lang, "../..", script_contraseña_lang), file = file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, paste0(comp_id, ".html")))
      }
    })
    
    # 13.1.23. Generate Individual Profile Pages.
    message("   > Generating individual profiles (matches, players, etc.)...")
    walk(1:nrow(partidos_df), function(i) {
      partido_info <- partidos_df[i,]; id_p <- partido_info$id_partido
      if (is.na(id_p) || (!full_rebuild_needed && !(id_p %in% affected_match_ids))) { return() }
      
      local_name <- (entidades_df_lang %>% filter(original_name == partido_info$local))$current_lang_name[1]
      visitante_name <- (entidades_df_lang %>% filter(original_name == partido_info$visitante))$current_lang_name[1]
      
      resumen_partido <- purrr::keep(resultados_exitosos, ~.x$partido_info$id_partido == id_p)[[1]]
      cronologia <- generar_cronologia_df(id_p, resumen_partido, entidades_df_lang, jugadoras_lang_df)
      arbitros_partido_mk <- arbitros_df %>% filter(id_partido == id_p); arbitros_partido_lang <- arbitros_partido_mk %>% left_join(entidades_df_lang, by = c("ime" = "original_name"))
      estadio_info_mk <- estadios_df %>% filter(id_partido == id_p) %>% head(1)
      estadio_name_lang <- if(nrow(estadio_info_mk) > 0) (entidades_df_lang %>% filter(original_name == estadio_info_mk$estadio))$current_lang_name[1] else t("match_unknown")
      goles_partido <- goles_df_unificado %>% filter(id_partido == id_p) %>% left_join(jugadoras_lang_df, by = "id")
      tarjetas_partido <- tarjetas_df_unificado %>% filter(id_partido == id_p) %>% left_join(jugadoras_lang_df, by = "id")
      penales_partido <- penales_df_unificado %>% filter(id_partido == id_p) %>% left_join(jugadoras_lang_df, by = "id")
      partido_comp_info <- competiciones_unicas_df %>% filter(competicion_nombre == partido_info$competicion_nombre, competicion_temporada == partido_info$competicion_temporada)
      comp_nombre_current_lang <- partido_comp_info[[comp_name_col]][1]
      is_cup_match <- str_detect(tolower(partido_info$competicion_nombre), "куп")
      jornada_texto <- if(partido_info$es_partido_seleccion) { 
        # 13.1.24. If it's a national team match, use the match category.
        partido_info$categoria 
      } else if(is_cup_match) { # 13.1.25. If it's a cup match.
        partido_info$jornada
      } else { # 13.1.26. For regular leagues.
        paste(t("round_prefix"), partido_info$jornada)
      }
      nota_arbitro <- resumen_partido$nota_arbitro; if (!is.na(nota_arbitro)) { nota_arbitro <- str_remove(nota_arbitro, "^[\\s:]*") }
      path_rel_competiciones <- file.path("..", nombres_carpetas_relativos$competiciones); path_rel_timovi <- file.path("..", nombres_carpetas_relativos$timovi); path_rel_jugadoras <- file.path("..", nombres_carpetas_relativos$jugadoras); path_rel_arbitros <- file.path("..", nombres_carpetas_relativos$arbitros); path_rel_estadios <- file.path("..", nombres_carpetas_relativos$estadios)
      crear_cabecera_alineacion <- function(nombre_equipo_mk, nombre_equipo_lang) {
        iso_code <- get_national_team_iso(nombre_equipo_mk)
        
        if (!is.na(iso_code)) {
          # 13.1.27. It's a national team, use flag URL.
          logo_src <- paste0("https://hatscripts.github.io/circle-flags/flags/", iso_code, ".svg")
          logo_class <- "match-page-crest national-team-flag"
        } else {
          # 13.1.28. It's a club team, use local logo path.
          nombre_archivo_final <- paste0(generar_id_seguro(nombre_equipo_mk), ".png")
          if (!file.exists(file.path(RUTA_LOGOS_DESTINO, nombre_archivo_final))) { nombre_archivo_final <- "NOLOGO.png" }
          logo_src <- file.path("..", "..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, nombre_archivo_final)
          logo_class <- "match-page-crest"
        }
        
        tags$div(class = "alineacion-header", 
                 tags$img(class = logo_class, src = logo_src, alt = nombre_equipo_lang), 
                 
                 tags$h3(crear_enlace_equipo_condicional(nombre_equipo_mk, nombre_equipo_lang))
        )
      }
      alineacion_partido_lang <- apariciones_df %>% filter(id_partido == id_p) %>% left_join(jugadoras_lang_df, by="id")
      render_equipo_html <- function(df_equipo, goles_del_partido, tarjetas_del_partido, is_national_team_match, team_original_mk_name) {
        if (is.null(df_equipo) || nrow(df_equipo) == 0) { return(tags$p(t("match_no_data"))) }
        starters <- df_equipo %>% filter(tipo == "Titular")
        subs <- df_equipo %>% filter(tipo == "Suplente")
        
        # 13.1.29. Inner function to create the player list items (li tags).
        crear_lista_jugadoras <- function(df_j) {
          if (nrow(df_j) == 0) { return(tags$p(style = "color:#777;", t("match_no_players"))) }
          tags$ul(pmap(df_j, function(id, PlayerName, dorsal, tipo, es_portera, es_capitana, min_entra, min_sale, minutos_jugados, ...) {
            eventos_html <- tagList()
            goles_jugadora <- goles_del_partido %>% filter(id == !!id, tipo == "Normal")
            if (nrow(goles_jugadora) > 0) { walk(1:nrow(goles_jugadora), function(g) { gol <- goles_jugadora[g,]; eventos_html <<- tagAppendChild(eventos_html, tags$span(class = "player-event goal", HTML(paste0("⚽︎ ", formatear_minuto_partido(gol$minuto), "'")))) }) }
            tarjetas_jugadora <- tarjetas_del_partido %>% filter(id == !!id)
            if (nrow(tarjetas_jugadora) > 0) { walk(1:nrow(tarjetas_jugadora), function(c) { tarjeta <- tarjetas_jugadora[c,]; card_span <- tags$span(class = if (tarjeta$tipo == "Amarilla") "card-yellow" else "card-red"); eventos_html <<- tagAppendChild(eventos_html, tags$span(class = "player-event", card_span, HTML(paste0("︎ ", formatear_minuto_partido(tarjeta$minuto), "'")))) }) }
            if (!is.na(min_entra) && tipo == "Suplente") { eventos_html <- tagAppendChild(eventos_html, tags$span(class = "player-event sub-in", paste0("↑", min_entra, "'"))) }
            if (!is.na(min_sale) && min_sale < 90 && !is.na(minutos_jugados) && minutos_jugados > 0) { eventos_html <- tagAppendChild(eventos_html, tags$span(class = "player-event sub-out", paste0("↓", min_sale, "'"))) }
            icono_p <- if (isTRUE(es_portera)) "🧤" else ""
            icono_c <- if (isTRUE(es_capitana)) "(C)" else ""
            
            # 13.1.30. Conditional logic for player links.
            should_be_clickable <- !is_national_team_match || (is_national_team_match && team_original_mk_name == "Македонија")
            
            if (should_be_clickable) {
              player_element <- tags$a(href = file.path(path_rel_jugadoras, paste0(id, ".html")), PlayerName)
            } else {
              player_element <- PlayerName
            }
            
            tags$li(paste0(dorsal, ". "), player_element, icono_p, icono_c, eventos_html)
          }))
        }
        tagList(tags$h4(t("match_starting_lineup")), crear_lista_jugadoras(starters), tags$h4(t("match_substitutes")), crear_lista_jugadoras(subs))
      }
      render_penales_html <- function(df_equipo) { if(is.null(df_equipo) || nrow(df_equipo) == 0) { return(NULL) }; tags$ul(pmap(df_equipo, function(PlayerName, id, dorsal, resultado_penal, ...) { tags$li(if(resultado_penal=="Gol") "✅" else "❌", " ", if(is.na(PlayerName)) "NA" else tags$a(href=file.path(path_rel_jugadoras, paste0(id, ".html")), PlayerName), paste0(" (", dorsal, ")")) })) }
      
      contenido_partido <- tagList(
        crear_botones_navegacion(path_to_lang_root = ".."),
        tags$h2(paste(local_name, "vs", visitante_name)),
        tags$p(style = "text-align:center; font-size: 1.1em; color: #555; margin-top: -15px; margin-bottom: 20px;", tags$a(href = file.path(path_rel_competiciones, paste0(partido_comp_info$competicion_id, ".html")), comp_nombre_current_lang), " - ", jornada_texto),
        tags$h3({
          resultado_texto <- paste(t("final_score"), ":", partido_info$goles_local, "-", partido_info$goles_visitante)
          if(!is.na(partido_info$penales_local)) { resultado_texto <- paste0(resultado_texto, " (", t("penalties_short"), " ", partido_info$penales_local, "-", partido_info$penales_visitante, ")") }
          if (isTRUE(partido_info$es_resultado_oficial)) { resultado_texto <- paste(resultado_texto, "*") }
          resultado_texto
        }),
        if(isTRUE(partido_info$es_resultado_oficial)) { tags$p(style="text-align:center; font-weight:bold; color: #8B0000;", t("match_official_result")) },
        tags$p(
          paste0(t("match_date"), ": ", partido_info$fecha, " | ", t("match_time"), ": ", partido_info$hora, " | ", t("match_stadium"), ": "), 
          if (nrow(estadio_info_mk) > 0) {
            # 13.1.31. Conditional logic for the stadium link.
            is_stadium_excluded_for_national_match <- partido_info$es_partido_seleccion && (generar_id_seguro(estadio_info_mk$estadio) %in% stadium_ids_to_skip)
            
            if (!is_stadium_excluded_for_national_match) {
              estadio_element <- tags$a(href = file.path(path_rel_estadios, paste0(generar_id_seguro(estadio_info_mk$estadio), ".html")), estadio_name_lang)
            } else {
              estadio_element <- estadio_name_lang # 13.1.32. Plain text.
            }
            estadio_element
          } else {
            t("match_unknown")
          }
        ),
        tags$h3(t("referees_title")),
        tags$ul(class = "sudii-lista", map(1:nrow(arbitros_partido_lang), function(a) {
          arb <- arbitros_partido_lang[a,]
          nombre_mostrado <- if (!is.na(arb$ciudad)) { paste0(arb$current_lang_name, " (", arb$ciudad, ")") } else { arb$current_lang_name }
          
          # 13.1.33. Conditional logic for referee links.
          is_arb_excluded_for_national_match <- partido_info$es_partido_seleccion && (generar_id_seguro(arb$ime) %in% referee_ids_to_skip)
          
          if (!is_arb_excluded_for_national_match) {
            ref_element <- tags$a(href = file.path(path_rel_arbitros, paste0(generar_id_seguro(arb$ime), ".html")), nombre_mostrado)
          } else {
            ref_element <- nombre_mostrado # 13.1.34. Plain text.
          }
          tags$li(paste0(t(arb$uloga), ": "), ref_element)
        })),
        if (!is.na(nota_arbitro) && nchar(nota_arbitro) > 0) { tagList(tags$h3(t("officials_notes")), tags$p(style = "white-space: pre-wrap; background-color: #f9f9f9; border-left: 3px solid #ccc; padding: 10px;", nota_arbitro)) },
        
        tags$h3(t("lineups_title")),
        tags$div(class = "alineaciones-container", 
                 tags$div(class = "columna-alineacion", 
                          crear_cabecera_alineacion(partido_info$local, local_name), 
                          render_equipo_html(
                            filter(alineacion_partido_lang, equipo == partido_info$local), 
                            goles_partido, 
                            tarjetas_partido,
                            partido_info$es_partido_seleccion,
                            partido_info$local
                          )
                 ), 
                 tags$div(class = "columna-alineacion", 
                          crear_cabecera_alineacion(partido_info$visitante, visitante_name), 
                          render_equipo_html(
                            filter(alineacion_partido_lang, equipo == partido_info$visitante), 
                            goles_partido, 
                            tarjetas_partido,
                            partido_info$es_partido_seleccion,
                            partido_info$visitante
                          )
                 )
        ),
        
        tags$h3(t("timeline_title")),
        tags$ul(class = "timeline", if (exists("cronologia") && nrow(cronologia) > 0) { map(1:nrow(cronologia), function(c) { e <- cronologia[c,]; tags$li(HTML(paste0("<span class='icon'>", e$icono, "</span>")), paste0(formatear_minuto_partido(e$minuto), "' - "), HTML(e$texto_evento)) }) } else { tags$li(t("match_timeline_no_events")) }),
        
        if (!is.na(partido_info$penales_local) && nrow(penales_partido) > 0) {
          tagList(
            tags$h3(t("penalties_title")),
            tags$div(
              class = "penales-container",
              tags$div(
                class = "columna-penales", 
                tags$h4(local_name), 
                render_penales_html(filter(penales_partido, equipo == partido_info$local))
              ),
              tags$div(
                class = "columna-penales", 
                tags$h4(visitante_name), 
                render_penales_html(filter(penales_partido, equipo == partido_info$visitante))
              )
            )
          )
        },
        
        crear_botones_navegacion(path_to_lang_root = "..")
      )
      pagina_partido_final <- crear_pagina_html(contenido_partido, paste(local_name, "vs", visitante_name), path_to_root_dir = "../..", script_contraseña_lang)
      save_html(pagina_partido_final, file = file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$partidos, paste0(id_p, ".html")))
    })
    
    walk(1:nrow(jugadoras_stats_df), function(i) {
      jugadora <- jugadoras_stats_df[i,]; id_j <- jugadora$id;
      if (id_j %in% player_ids_to_skip) { return() }
      
      if (!full_rebuild_needed && !(id_j %in% affected_player_ids)) { return() }
      
      current_player_name <- jugadora[[player_name_col]]
      
      # 13.1.35. Prepare relative paths.
      path_rel_timovi <- file.path("..", nombres_carpetas_relativos$timovi)
      path_rel_partidos <- file.path("..", nombres_carpetas_relativos$partidos)
      
      # 13.1.36. List to accumulate career table HTML rows.
      lista_filas_carrera <- list()
      
      # 13.1.37. STEP 1: Process NATIONAL TEAM career.
      nat_team_player_summary <- national_team_career_summary_df %>% filter(id == id_j)
      if (nrow(nat_team_player_summary) > 0) {
        stage_nat <- nat_team_player_summary[1,]
        details_id_nat <- paste0("details-", id_j, "-national")
        
        historial_nacional_partidos <- apariciones_df %>%
          filter(id == id_j, equipo == "Македонија") %>%
          left_join(partidos_df %>% 
                      select(id_partido, fecha, local, visitante, goles_local, goles_visitante, categoria, es_partido_seleccion), 
                    by = "id_partido") %>%
          filter(es_partido_seleccion == TRUE) %>%
          mutate(fecha_parsed = as.Date(fecha, format="%d.%m.%Y")) %>%
          arrange(desc(fecha_parsed)) %>%
          left_join(entidades_df_lang %>% select(original_name, local_lang=current_lang_name), by=c("local"="original_name")) %>% 
          left_join(entidades_df_lang %>% select(original_name, visitante_lang=current_lang_name), by=c("visitante"="original_name"))
        
        tabla_partidos_nacional <- tags$table(
          tags$thead(tags$tr(
            tags$th(t("team_header_date")), tags$th(t("match_header_match")), 
            tags$th(t("match_header_category")), tags$th(t("match_header_result")), 
            tags$th(t("match_header_status")), tags$th(t("player_mins"))
          )),
          tags$tbody(if(nrow(historial_nacional_partidos) > 0) { 
            map(1:nrow(historial_nacional_partidos), function(p_idx){ 
              partido_row <- historial_nacional_partidos[p_idx,]
              status_partido <- if (partido_row$tipo == "Titular") t("player_starter") else if (!is.na(partido_row$minutos_jugados) && partido_row$minutos_jugados > 0) t("player_status_played_sub") else t("player_status_called_up")
              tags$tr(tags$td(partido_row$fecha), tags$td(tags$a(href=file.path(path_rel_partidos, paste0(partido_row$id_partido, ".html")), paste(partido_row$local_lang, "vs", partido_row$visitante_lang))), tags$td(partido_row$categoria), tags$td(paste(partido_row$goles_local,"-",partido_row$goles_visitante)), tags$td(status_partido), tags$td(if(is.na(partido_row$minutos_jugados)) 0 else partido_row$minutos_jugados))
            }) 
          } else { 
            tags$tr(tags$td(colspan="6", t("player_no_matches")))
          })
        )
        
        details_div_nat <- tags$div(class="details-content", tags$h4(t("player_match_list")), tabla_partidos_nacional)
        nombre_equipo_stage_nat_lang <- (entidades_df_lang %>% filter(original_name == stage_nat$equipo))$current_lang_name[1]
        nombre_comp_stage_nat_lang <- (competiciones_unicas_df %>% filter(competicion_id == "reprezentacija"))[[comp_name_col]][1]
        flag_url_mk <- paste0("https://hatscripts.github.io/circle-flags/flags/", get_national_team_iso("Македонија"), ".svg")
        
        summary_row_nat <- tags$tr(
          class="summary-row", onclick=sprintf("toggleDetails('%s')", details_id_nat), 
          tags$td(stage_nat$competicion_temporada), 
          tags$td(class="team-cell", 
                  tags$img(class="team-logo national-team-flag", src = flag_url_mk, alt = nombre_equipo_stage_nat_lang), 
                  crear_enlace_equipo_condicional(stage_nat$equipo, nombre_equipo_stage_nat_lang)
          ), 
          tags$td(nombre_comp_stage_nat_lang), 
          tags$td(stage_nat$Played), 
          tags$td(stage_nat$Goals), 
          tags$td(stage_nat$Minutes)
        )
        
        details_row_nat <- tags$tr(id=details_id_nat, class="details-row", tags$td(colspan="6", details_div_nat))
        
        lista_filas_carrera <- append(lista_filas_carrera, list(summary_row_nat, details_row_nat))
      }
      
      # 13.1.38. STEP 2: Process CLUB career.
      player_career_clubs_df <- career_summary_jugadoras_df %>% 
        filter(id == id_j, equipo != "Македонија") %>%
        left_join(competiciones_unicas_df %>% select(competicion_nombre, competicion_temporada, !!sym(comp_name_col)), by=c("competicion_nombre", "competicion_temporada")) %>% 
        left_join(entidades_df_lang, by = c("equipo" = "original_name"))
      
      if (nrow(player_career_clubs_df) > 0) {
        partidos_jugadora_details <- apariciones_df %>% filter(id == id_j) %>%
          left_join(partidos_df %>% select(id_partido, jornada, fecha, local, visitante, goles_local, goles_visitante), by = "id_partido") %>% 
          left_join(entidades_df_lang %>% select(original_name, local_lang=current_lang_name), by=c("local"="original_name")) %>% 
          left_join(entidades_df_lang %>% select(original_name, visitante_lang=current_lang_name), by=c("visitante"="original_name"))
        
        filas_club_nested <- map(1:nrow(player_career_clubs_df), function(j) {
          stage <- player_career_clubs_df[j,]; details_id <- paste0("details-", id_j, "-club-", j)
          nombre_equipo_stage_mk <- stage$equipo; nombre_equipo_stage_lang <- stage$current_lang_name
          nombre_comp_stage_lang <- stage[[comp_name_col]]
          
          nombre_archivo_final_stage <- paste0(generar_id_seguro(nombre_equipo_stage_mk), ".png")
          if (!file.exists(file.path(RUTA_LOGOS_DESTINO, nombre_archivo_final_stage))) { nombre_archivo_final_stage <- "NOLOGO.png" }
          logo_src_stage <- file.path("..", "..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, nombre_archivo_final_stage)
          
          partidos_stage <- partidos_jugadora_details %>% filter(competicion_temporada == stage$competicion_temporada, competicion_nombre == stage$competicion_nombre, equipo == stage$equipo)
          goles_stage <- goles_df_unificado %>% filter(id == id_j, id_partido %in% partidos_stage$id_partido, equipo_jugadora == stage$equipo)
          tarjetas_stage <- tarjetas_df_unificado %>% filter(id == id_j, id_partido %in% partidos_stage$id_partido, equipo == stage$equipo)
          
          tabla_detalles_stats <- tags$table(tags$tbody(tags$tr(tags$td(t("team_type")), tags$td(nombre_equipo_stage_lang)), tags$tr(tags$td(t("player_called_up")), tags$td(stage$CalledUp)), tags$tr(tags$td(t("player_played")), tags$td(stage$Played)), tags$tr(tags$td(t("player_starter")), tags$td(stage$Starter)), tags$tr(tags$td(t("player_mins")), tags$td(stage$Minutes)), tags$tr(tags$td(t("player_goals")), tags$td(stage$Goals)), tags$tr(tags$td(t("player_yellow_cards")), tags$td(stage$Yellows)), tags$tr(tags$td(t("player_red_cards")), tags$td(stage$Reds))))
          tabla_partidos <- tags$table(tags$thead(tags$tr(tags$th(t("round_prefix")),tags$th(t("match_header_match")),tags$th(t("match_header_result")),tags$th(t("player_status")), tags$th(t("player_mins")))), tags$tbody(if(nrow(partidos_stage)>0) { map(1:nrow(partidos_stage), function(p_idx){ partido_row <- partidos_stage[p_idx,]; status_partido <- if (partido_row$tipo == "Titular") t("player_starter") else if (!is.na(partido_row$minutos_jugados) && partido_row$minutos_jugados > 0) t("player_status_played_sub") else t("player_status_called_up"); tags$tr(tags$td(partido_row$jornada), tags$td(tags$a(href=file.path(path_rel_partidos, paste0(partido_row$id_partido, ".html")),paste(partido_row$local_lang,"vs",partido_row$visitante_lang))), tags$td(paste(partido_row$goles_local,"-",partido_row$goles_visitante)), tags$td(status_partido), tags$td(if(is.na(partido_row$minutos_jugados)) 0 else partido_row$minutos_jugados)) }) } else { tags$tr(tags$td(colspan="5", t("player_no_matches"))) }))
          tabla_goles <- tags$table(tags$thead(tags$tr(tags$th(t("round_prefix")), tags$th(t("match_header_match")), tags$th(t("match_header_minute")))), tags$tbody(if(nrow(goles_stage)>0){ map(1:nrow(goles_stage), function(g_idx){ goal_row <- goles_stage[g_idx,]; g_partido<-filter(partidos_stage, id_partido==goal_row$id_partido) %>% head(1); tags$tr(tags$td(g_partido$jornada), tags$td(tags$a(href=file.path(path_rel_partidos, paste0(goal_row$id_partido, ".html")),paste(g_partido$local_lang,"vs",g_partido$visitante_lang))), tags$td(formatear_minuto_partido(goal_row$minuto)))}) } else { tags$tr(tags$td(colspan="3", t("player_no_goals"))) }))
          tabla_tarjetas <- tags$table(tags$thead(tags$tr(tags$th(t("round_prefix")), tags$th(t("match_header_match")), tags$th(t("match_header_card")), tags$th(t("match_header_minute")), tags$th(t("match_header_reason")))), tags$tbody(if(nrow(tarjetas_stage)>0){ map(1:nrow(tarjetas_stage), function(t_idx){ card_row <- tarjetas_stage[t_idx,]; t_partido<-filter(partidos_stage, id_partido==card_row$id_partido) %>% head(1); icon<-if(card_row$tipo=="Amarilla")tags$span(class="card-yellow")else tags$span(class="card-red"); tags$tr(tags$td(t_partido$jornada), tags$td(tags$a(href=file.path(path_rel_partidos, paste0(card_row$id_partido, ".html")),paste(t_partido$local_lang,"vs",t_partido$visitante_lang))), tags$td(icon), tags$td(formatear_minuto_partido(card_row$minuto)), tags$td(card_row$motivo))}) } else { tags$tr(tags$td(colspan="5", t("player_no_cards"))) }))
          
          details_div <- tags$div(class="details-content", tags$h4(t("player_detailed_stats")), tabla_detalles_stats, tags$h4(t("player_match_list")), tabla_partidos, tags$h4(t("player_goal_list")), tabla_goles, tags$h4(t("player_card_list")), tabla_tarjetas)
          summary_row <- tags$tr(class="summary-row", onclick=sprintf("toggleDetails('%s')", details_id), tags$td(stage$competicion_temporada), tags$td(class="team-cell", tags$img(class = "team-logo", src = logo_src_stage, alt = nombre_equipo_stage_lang), tags$a(href=file.path(path_rel_timovi, paste0(generar_id_seguro(nombre_equipo_stage_mk), ".html")), onclick="event.stopPropagation();", nombre_equipo_stage_lang)), tags$td(nombre_comp_stage_lang), tags$td(stage$Played), tags$td(stage$Goals), tags$td(stage$Minutes))
          details_row <- tags$tr(id=details_id, class="details-row", tags$td(colspan="6", details_div))
          
          list(summary_row, details_row)
        })
        
        lista_filas_carrera <- append(lista_filas_carrera, unlist(filas_club_nested, recursive = FALSE))
      }
      
      # 13.1.39. STEP 3: Build the final table.
      if (length(lista_filas_carrera) > 0) {
        tbody_content <- tagList(lista_filas_carrera)
      } else {
        tbody_content <- tags$tr(tags$td(colspan="6", t("player_no_career_data")))
      }
      
      tabla_resumen_carrera <- tags$table(
        class="career-summary-table",
        tags$thead(tags$tr(tags$th(t("player_season")), tags$th(t("team_type")), tags$th(t("player_competition")), tags$th(t("player_apps")), tags$th(t("player_goals")), tags$th(t("player_mins")))),
        tags$tbody(tbody_content)
      )
      
      # 13.1.40. Profile header.
      icono_bandera <- if (!is.na(jugadora$codigo_iso)) {
        texto_emergente <- if_else(!is.na(jugadora$nombre_macedonio), jugadora$nombre_macedonio, jugadora$nacionalidad)
        url_bandera <- paste0("https://kapowaz.github.io/square-flags/flags/", jugadora$codigo_iso, ".svg")
        tags$img(src = url_bandera, alt = texto_emergente, title = texto_emergente, style = "height: 0.9em; width: auto; border: 1px solid #ccc;")
      }
      info_edad <- if (!is.na(jugadora$edad)) {
        tags$span(style = "font-size: 0.6em; color: #555; vertical-align: middle; font-weight: normal;", paste0(jugadora$edad, t("player_age_suffix")))
      }
      mapa_pos_traducida <- c("goalkeeper" = t("position_goalkeeper"), "defender" = t("position_defender"), "midfielder" = t("position_midfielder"), "forward" = t("position_forward"))
      posicion_traducida <- recode(jugadora$posicion_final_unificada, !!!mapa_pos_traducida, .default = jugadora$posicion_final_unificada)
      info_posicion <- if (!is.na(jugadora$posicion_final_unificada)) {
        tags$span(style = "font-size: 0.6em; color: #555; vertical-align: middle; font-weight: normal;", posicion_traducida)
      }
      titulo_perfil <- tags$h2(
        style = "display: flex; align-items: center; gap: 15px;",
        current_player_name, icono_bandera, info_edad, info_posicion
      )
      
      # 13.1.41. Final content and file saving.
      contenido_jugadora <- tagList(
        crear_botones_navegacion(path_to_lang_root = ".."),
        titulo_perfil,
        tags$h3(t("player_career_summary")),
        tabla_resumen_carrera
      )
      pagina_jugadora_final <- crear_pagina_html(
        contenido_jugadora, current_player_name, path_to_root_dir = "../..", 
        script_contraseña_lang
      )
      save_html(pagina_jugadora_final, file = file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$jugadoras, paste0(id_j, ".html")))
    })
    
    walk(unique(c(partidos_df$local, partidos_df$visitante)), function(team_mk) {
      if (!is.na(get_national_team_iso(team_mk))) {
        return() 
      }
      
      id_t <- generar_id_seguro(team_mk); if (!full_rebuild_needed && !(id_t %in% affected_team_ids)) { return() }
      
      current_team_name <- entidades_df_lang %>% filter(original_name == team_mk) %>% pull(current_lang_name)
      nombre_archivo_final <- paste0(id_t, ".png"); if (!file.exists(file.path(RUTA_LOGOS_DESTINO, nombre_archivo_final))) { nombre_archivo_final <- "NOLOGO.png" }; ruta_relativa_logo_html <- file.path("..", "..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, nombre_archivo_final)
      temporadas_summary <- stats_equipos_por_temporada_df %>% filter(equipo == team_mk) %>% left_join(competiciones_unicas_df, by = c("competicion_nombre", "competicion_temporada")) %>% select(competicion_temporada, competicion_nombre, !!sym(comp_name_col))
      path_rel_jugadoras <- file.path("..", nombres_carpetas_relativos$jugadoras); path_rel_partidos <- file.path("..", nombres_carpetas_relativos$partidos)
      tabla_resumen_temporadas <- tags$table(class="team-career-summary", tags$thead(tags$tr(tags$th(t("player_season")), tags$th(t("player_competition")))), tags$tbody(map(1:nrow(temporadas_summary), function(j) { stage <- temporadas_summary[j,]; details_id <- paste0("details-", id_t, "-", j); nombre_competicion_mostrado <- stage[[comp_name_col]]; historial_stage_mk <- partidos_df %>% filter(competicion_temporada == stage$competicion_temporada, competicion_nombre == stage$competicion_nombre, local == team_mk | visitante == team_mk) %>% mutate(fecha_date = as.Date(fecha, format="%d.%m.%Y")) %>% arrange(fecha_date); historial_stage <- historial_stage_mk %>% left_join(entidades_df_lang %>% select(original_name, home_name = current_lang_name), by = c("local" = "original_name")) %>% left_join(entidades_df_lang %>% select(original_name, away_name = current_lang_name), by = c("visitante" = "original_name")); player_name_col_sym <- sym(if (player_name_col %in% names(jugadoras_stats_df)) player_name_col else "PlayerName_mk"); stats_jugadoras_stage_lang <- stats_jugadoras_por_equipo_temporada_df %>% filter(equipo == team_mk, competicion_temporada == stage$competicion_temporada, competicion_nombre == stage$competicion_nombre) %>% left_join(jugadoras_stats_df %>% select(id, !!player_name_col_sym), by = "id") %>% select(id, PlayerName = !!player_name_col_sym, CalledUp, Played, Minutes, Goals, Yellows, Reds); headers_stats <- c(t("player_type"), t("player_called_up"), t("player_played"), t("player_mins"), t("player_goals"), t("player_yellow_cards"), t("player_red_cards")); tabla_stats_jugadoras <- tags$table(tags$thead(tags$tr(map(headers_stats, tags$th))), tags$tbody(if(nrow(stats_jugadoras_stage_lang) > 0) { map(1:nrow(stats_jugadoras_stage_lang), function(p_idx) { p <- stats_jugadoras_stage_lang[p_idx,]; tags$tr(tags$td(tags$a(href=file.path(path_rel_jugadoras, paste0(p$id, ".html")), p$PlayerName)), tags$td(p$CalledUp), tags$td(p$Played), tags$td(p$Minutes), tags$td(p$Goals), tags$td(p$Yellows), tags$td(p$Reds)) }) } else { tags$tr(tags$td(colspan=length(headers_stats), t("match_no_data"))) })); tabla_historial_partidos <- tags$table(tags$thead(tags$tr(tags$th(t("round_prefix")), tags$th(t("team_header_date")), tags$th(t("team_header_home")), tags$th(t("team_header_away")), tags$th(t("match_header_result")))), tags$tbody(map(1:nrow(historial_stage), function(p_idx) { partido <- historial_stage[p_idx,]; tags$tr(tags$td(partido$jornada), tags$td(partido$fecha), tags$td(partido$home_name), tags$td(partido$away_name), tags$td(tags$a(href=file.path(path_rel_partidos, paste0(partido$id_partido, ".html")), paste(partido$goles_local, "-", partido$goles_visitante)))) }))); tagList(tags$tr(class="summary-row", onclick=sprintf("toggleDetails('%s')", details_id), tags$td(stage$competicion_temporada), tags$td(nombre_competicion_mostrado)), tags$tr(id = details_id, class="details-row", tags$td(colspan="2", tags$div(class="details-content", tags$h4(t("team_player_stats")), tabla_stats_jugadoras, tags$h4(t("team_match_list")), tabla_historial_partidos)))) })))
      # 13.1.42. Logic for the crest/flag image on the team profile.
      iso_code_team <- get_national_team_iso(team_mk)
      if (!is.na(iso_code_team)) {
        team_logo_src <- paste0("https://hatscripts.github.io/circle-flags/flags/", iso_code_team, ".svg")
        team_logo_class <- "team-page-crest national-team-flag"
      } else {
        nombre_archivo_final <- paste0(id_t, ".png")
        if (!file.exists(file.path(RUTA_LOGOS_DESTINO, nombre_archivo_final))) { nombre_archivo_final <- "NOLOGO.png" }
        team_logo_src <- file.path("..", "..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, nombre_archivo_final)
        team_logo_class <- "team-page-crest"
      }
      
      contenido_equipo <- tagList(
        crear_botones_navegacion(path_to_lang_root = ".."), 
        tags$h2(current_team_name), 
        tags$img(class=team_logo_class, src=team_logo_src, alt=paste("Crest of", current_team_name)), 
        tags$h3(t("team_history_by_competition")), 
        tabla_resumen_temporadas
      )
      pagina_equipo_final <- crear_pagina_html(contenido_equipo, current_team_name, path_to_root_dir = "../..", script_contraseña_lang)
      save_html(pagina_equipo_final, file = file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$timovi, paste0(id_t, ".html")))
    })
    
    walk(unique(arbitros_df$ime), function(arb_mk) {
      id_a <- generar_id_seguro(arb_mk)
      if (id_a %in% referee_ids_to_skip) { return() }
      
      if (!full_rebuild_needed && !(id_a %in% affected_referee_ids)) { return() }
      
      current_arb_name <- (entidades_df_lang %>% filter(original_name == arb_mk))$current_lang_name[1]
      temporadas_summary <- stats_arbitros_por_temporada_df %>% 
        filter(ime == arb_mk) %>%
        left_join(competiciones_unicas_df, by = c("competicion_nombre", "competicion_temporada")) %>%
        select(competicion_temporada, competicion_nombre, !!sym(comp_name_col), num_matches)
      
      path_rel_partidos <- file.path("..", nombres_carpetas_relativos$partidos)
      
      tbody_content <- if (nrow(temporadas_summary) > 0) {
        map(1:nrow(temporadas_summary), function(j) {
          stage <- temporadas_summary[j,]
          details_id <- paste0("details-arbitro-", id_a, "-", j)
          nombre_competicion_mostrado <- stage[[comp_name_col]]
          
          historial_stage_mk <- arbitros_df %>% 
            filter(ime == arb_mk) %>% 
            left_join(partidos_df, by = "id_partido") %>% 
            filter(competicion_temporada == stage$competicion_temporada, competicion_nombre == stage$competicion_nombre) %>% 
            mutate(fecha_date = as.Date(fecha, format="%d.%m.%Y")) %>% 
            arrange(desc(fecha_date))
          
          historial_stage <- historial_stage_mk %>% 
            left_join(entidades_df_lang %>% select(original_name, home_name = current_lang_name), by = c("local" = "original_name")) %>% 
            left_join(entidades_df_lang %>% select(original_name, away_name = current_lang_name), by = c("visitante" = "original_name"))
          
          tabla_detalles <- tags$table(
            tags$thead(tags$tr(
              tags$th(t("team_header_date")), 
              tags$th(t("round_prefix")), 
              tags$th(t("match_header_match")), 
              tags$th(t("match_header_result")), 
              tags$th(t("referee_header_role"))
            )),
            tags$tbody(map(1:nrow(historial_stage), function(p_idx) {
              partido <- historial_stage[p_idx,]
              tags$tr(
                tags$td(partido$fecha),
                tags$td(partido$jornada),
                tags$td(tags$a(href=file.path(path_rel_partidos, paste0(partido$id_partido, ".html")), paste(partido$home_name, "vs", partido$away_name))),
                tags$td(paste(partido$goles_local, "-", partido$goles_visitante)),
                tags$td(t(partido$uloga))
              )
            }))
          )
          
          summary_row <- tags$tr(
            class = "summary-row", onclick = sprintf("toggleDetails('%s')", details_id),
            tags$td(stage$competicion_temporada),
            tags$td(nombre_competicion_mostrado),
            tags$td(stage$num_matches)
          )
          
          details_row <- tags$tr(
            id = details_id, class = "details-row",
            tags$td(colspan = "3", tags$div(class = "details-content", tabla_detalles))
          )
          
          tagList(summary_row, details_row)
        })
      } else {
        tags$tr(tags$td(colspan="3", t("player_no_matches")))
      }
      
      ciudad_arbitra <- (arbitros_df %>% filter(ime == arb_mk) %>% slice(1))$ciudad
      
      titulo_perfil_arbitra <- tags$h2(
        current_arb_name,
        if (!is.na(ciudad_arbitra)) {
          tags$span(style = "font-size: 0.6em; color: #555; vertical-align: middle; font-weight: normal;", paste0("(", ciudad_arbitra, ")"))
        }
      )
      
      contenido_arbitro <- tagList(
        crear_botones_navegacion(path_to_lang_root = ".."),
        titulo_perfil_arbitra,
        tags$h3(t("referee_history_by_competition")),
        tags$table(
          tags$thead(tags$tr(tags$th(t("player_season")), tags$th(t("player_competition")), tags$th(t("referee_header_matches")))),
          tags$tbody(tbody_content)
        )
      )
      
      pagina_arbitro_final <- crear_pagina_html(
        contenido_principal = contenido_arbitro,
        titulo_pagina = current_arb_name,
        path_to_root_dir = "../..",
        script_contraseña = script_contraseña_lang
      )
      save_html(pagina_arbitro_final, file = file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$arbitros, paste0(id_a, ".html")))
    })
    
    walk(unique(na.omit(estadios_df$estadio)), function(est_mk) {
      id_e <- generar_id_seguro(est_mk); 
      
      # 13.1.43. NEW EXCLUSION LOGIC: If the stadium is in the exclusion list, skip.
      if (id_e %in% stadium_ids_to_skip) { return() }
      
      if (!full_rebuild_needed && !(id_e %in% affected_stadium_ids)) { return() }
      current_est_name <- entidades_df_lang %>% filter(original_name == est_mk) %>% pull(current_lang_name)
      historial_mk <- estadios_df %>% filter(estadio == est_mk) %>% mutate(fecha_date = as.Date(fecha, format = "%d.%m.%Y")) %>% arrange(desc(fecha_date))
      historial <- historial_mk %>% left_join(entidades_df_lang %>% select(original_name, home_name = current_lang_name), by = c("local" = "original_name")) %>% left_join(entidades_df_lang %>% select(original_name, away_name = current_lang_name), by = c("visitante" = "original_name")) %>% left_join(competiciones_unicas_df %>% select(competicion_nombre, competicion_temporada, !!sym(comp_name_col)), by = c("competicion_nombre", "competicion_temporada"))
      path_rel_partidos <- file.path("..", nombres_carpetas_relativos$partidos)
      contenido_estadio <- tagList(crear_botones_navegacion(path_to_lang_root = ".."), tags$h2(current_est_name), tags$h3(t("stadium_match_history")), tags$table(tags$thead(tags$tr(tags$th(t("team_header_date")), tags$th(t("player_season")), tags$th(t("player_competition")), tags$th(t("round_prefix")), tags$th(t("match_header_match")), tags$th(t("match_header_result")))), tags$tbody(if (nrow(historial) > 0) { map(1:nrow(historial), function(p_idx) { partido <- historial[p_idx, ]; nombre_competicion_mostrado <- partido[[comp_name_col]]; tags$tr(tags$td(partido$fecha), tags$td(partido$competicion_temporada), tags$td(nombre_competicion_mostrado), tags$td(partido$jornada), tags$td(tags$a(href=file.path(path_rel_partidos, paste0(partido$id_partido, ".html")), paste(partido$home_name, "vs", partido$away_name))), tags$td(paste(partido$goles_local, "-", partido$goles_visitante))) }) } else { tags$tr(tags$td(colspan = "6", t("player_no_matches"))) })))
      pagina_estadio_final <- crear_pagina_html(contenido_estadio, current_est_name, path_to_root_dir = "../..", script_contraseña_lang)
      save_html(pagina_estadio_final, file = file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$estadios, paste0(id_e, ".html")))
    })
    
  } # 13.1.44. End of the main language loop.
  
  # 13.1.45. Create the redirect page at the site root.
  message("\nCreating redirect file at the site root...")
  redirect_html_content <- c('<!DOCTYPE html>', '<html>', '<head>', '<title>Redirecting...</title>', '<meta charset="utf-8">', paste0('<meta http-equiv="refresh" content="0; url=', IDIOMAS_SOPORTADOS[1], '/index.html">'), '</head>', '<body>', '<p>If you are not redirected automatically, follow this <a href="', IDIOMAS_SOPORTADOS[1], '/index.html">link</a>.</p>', '</body>', '</html>')
  writeLines(redirect_html_content, file.path(RUTA_SALIDA_RAIZ, "index.html"))
  
}

#### 14. PROCESS FINALIZATION ####
if (hubo_cambios) {
  # 14.1.1. Reset to the default language upon completion.
  idioma_actual <<- IDIOMAS_SOPORTADOS[1] 
  message(paste("\n", t("final_process_success")));
  if (full_rebuild_needed) { 
    message(t("final_full_rebuild")) 
  } else { 
    message(t("final_incremental_update")) 
  }
  message("The process first generated the master pages and then translated them.")
  message(paste(t("final_site_location"), RUTA_SALIDA_RAIZ));
  message(t("final_navigate_prompt"))
} else {
  message("\nNo changes were detected. The website is already up to date.")
}