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

#' @title Apply a conversion map (dictionary) to dataframe columns or vectors flexibly.
#' @description This version is case-insensitive, supports many-to-one mappings,
#' and handles both dataframes and character vectors as input.
#' @param data_input The dataframe or vector to modify.
#' @param columnas Vector of column names (only used if input is a dataframe).
#' @param mapa_df A dataframe with two columns: `original_lower` and `corregido`.
#' @return The modified dataframe or vector.
aplicar_conversiones <- function(data_input, columnas = NULL, mapa_df) {
  # --- GUARD CLAUSES ---
  # 1. Si no hay mapa de conversión, devolver los datos originales sin cambios.
  if (is.null(mapa_df) || nrow(mapa_df) == 0) return(data_input)
  # 2. Si la entrada de datos está vacía (NULL, 0 filas, 0 longitud), devolverla.
  if (is.null(data_input) || (is.data.frame(data_input) && nrow(data_input) == 0) || (!is.data.frame(data_input) && length(data_input) == 0)) {
    return(data_input)
  }
  
  # --- LOGIC BRANCHING ---
  if (is.data.frame(data_input)) {
    # --- CASO 1: La entrada es un DATAFRAME ---
    df <- data_input
    for (col_name in intersect(columnas, names(df))) {
      # Crear una columna temporal con el nombre en minúsculas para el cruce.
      df <- df %>%
        mutate(.join_col_lower = tolower(as.character(.data[[col_name]]))) %>%
        left_join(mapa_df, by = c(".join_col_lower" = "original_lower")) %>%
        mutate(!!col_name := coalesce(corregido, .data[[col_name]])) %>%
        select(-.join_col_lower, -corregido)
    }
    return(df)
    
  } else if (is.character(data_input)) {
    # --- CASO 2: La entrada es un VECTOR de texto ---
    vector_input <- data_input
    # Crear un dataframe temporal para hacer el cruce
    temp_df <- tibble(
      original_value = vector_input,
      .join_col_lower = tolower(original_value)
    )
    # Unir, reemplazar y devolver el vector resultante
    result_df <- temp_df %>%
      left_join(mapa_df, by = c(".join_col_lower" = "original_lower")) %>%
      mutate(final_value = coalesce(corregido, original_value))
    
    return(result_df$final_value)
    
  } else {
    # --- CASO 3: Otro tipo de entrada (lista, etc.) ---
    # Devolver sin cambios para evitar errores.
    return(data_input)
  }
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
#' @description THIS FUNCTION IS INTENTIONALLY DISABLED. It now returns NULL to
#' prevent the Back/Home buttons from being rendered on any page.
#' @param path_to_lang_root Relative path to the current language root (e.g., '..').
#' @return NULL
crear_botones_navegacion <- function(path_to_lang_root = ".") {
  # Returns NULL to effectively remove the buttons from the entire site.
  return(NULL)
}

#' @title Create the base structure of an HTML page (template).
#' @param contenido_principal The main content of the page (an htmltools object).
#' @param titulo_pagina The title that will appear in the browser tab (already translated).
#' @param path_to_root_dir Relative path to the root 'docs/' directory.
#' @param script_contraseña The script tag for password protection.
#' @param current_page_id A string identifying the current page for the navbar (e.g., "home", "players").
#' @return A complete htmltools `html` object.
crear_pagina_html <- function(contenido_principal, titulo_pagina, path_to_root_dir = ".", script_contraseña, current_page_id = "home") {
  
  # Determine the relative path to the language root for links in the navbar
  path_to_lang_root <- if (path_to_root_dir == "..") "." else ".."
  
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
                       
                       # --- NUEVO: Contenedor para alinear título y menú ---
                       tags$div(class="header-main",
                                tags$h1(tags$a(href = file.path(path_to_lang_root, "index.html"), style = "color: inherit; text-decoration: none;", t("site_title"))),
                                crear_barra_navegacion(path_to_lang_root = path_to_lang_root, current_page_id = current_page_id)
                       ),
                       
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



#' @title Create the main navigation bar.
#' @description Generates the full <nav> element with dropdowns and hamburger icon.
#' @param path_to_lang_root Relative path to the current language root (e.g., '.').
#' @param current_page_id An identifier for the current page to set the 'active' class.
#' @return An htmltools `nav` object.
crear_barra_navegacion <- function(path_to_lang_root = ".", current_page_id = "home") {
  
  crear_item_menu <- function(id, texto, href) {
    tags$li(tags$a(class = if (id == current_page_id) "active" else "", href = href, texto))
  }
  
  temporada_actual <- obtener_temporada_actual()
  competiciones_menu_df <- competiciones_unicas_df %>%
    filter(competicion_temporada == temporada_actual, !str_detect(competicion_nombre, "Пријателски|Бараж"))
  
  comp_dropdown_items <- map(1:nrow(competiciones_menu_df), function(i) {
    comp <- competiciones_menu_df[i, ]; comp_name_lang <- comp[[paste0("nombre_completo_", idioma_actual)]]
    tags$a(href = file.path(path_to_lang_root, nombres_carpetas_relativos$competiciones, paste0(comp$competicion_id, ".html")), comp_name_lang)
  })
  
  comp_dropdown_items <- append(comp_dropdown_items,
                                list(tags$hr(), tags$a(href = file.path(path_to_lang_root, paste0(nombres_archivos_traducidos$archive, ".html")), t("menu_archive")))
  )
  
  tags$nav(
    class = "navbar",
    tags$a(href = "javascript:void(0);", class = "hamburger-icon", id = "hamburger-icon",
           tags$span(class="bar"), tags$span(class="bar"), tags$span(class="bar")
    ),
    tags$ul(
      class = "nav-links",
      tags$li(class="close-btn-li", tags$a(href="javascript:void(0);", id="close-nav-btn", "×")),
      
      crear_item_menu("home", t("menu_home"), file.path(path_to_lang_root, "index.html")),
      tags$li(
        class = "dropdown",
        tags$a(href = "javascript:void(0)", class = if (current_page_id == "competitions") "dropbtn active" else "dropbtn", 
               HTML(paste(t("menu_competitions"), " &#9662;"))
        ),
        tags$div(class = "dropdown-content", comp_dropdown_items)
      ),
      crear_item_menu("national_team", t("menu_national_team"), file.path(path_to_lang_root, nombres_carpetas_relativos$competiciones, "reprezentacija.html")),
      crear_item_menu("players", t("menu_players"), file.path(path_to_lang_root, paste0(nombres_archivos_traducidos$players, ".html"))),
      crear_item_menu("teams", t("menu_teams"), file.path(path_to_lang_root, paste0(nombres_archivos_traducidos$teams, ".html"))),
      crear_item_menu("about", t("menu_about"), file.path(path_to_lang_root, paste0(nombres_archivos_traducidos$about, ".html")))
    )
  )
}


#' @title Extracts the surname for sorting purposes.
#' @description Assumes the name is in "Firstname Lastname" format and returns the last word.
#' @param nombre A single string with a full name.
#' @return The extracted surname, or the full name if it's a single word.
extraer_apellido <- function(nombre) {
  if (is.na(nombre) || !str_detect(nombre, "\\s")) {
    return(nombre)
  }
  palabras <- str_split(nombre, "\\s+")[[1]]
  return(palabras[length(palabras)])
}

### 8.4. Function to Generate Match Timeline

#' @title Generate a dataframe with the timeline of match events (goals, cards, changes).
#' @description Generates a dataframe sorted by minute with the match events. This version is robust
#' against inconsistent team names from different data sources.
#' @param id_p The match ID.
#' @param resumen_partido The results list for a match.
#' @param entidades_lang_df The entities dataframe already filtered for the current language.
#' @param jugadoras_lang_df The players dataframe already filtered for the current language.
#' @return A dataframe sorted by minute with the match events.
generar_cronologia_df <- function(id_p, resumen_partido, entidades_lang_df, jugadoras_lang_df) {
  lista_eventos <- list()
  path_jugadoras <- file.path("..", nombres_carpetas_relativos$jugadoras)
  path_timovi <- file.path("..", nombres_carpetas_relativos$timovi)
  
  info_partido_canonico <- partidos_df %>% filter(id_partido == id_p) %>% head(1)
  es_partido_seleccion <- info_partido_canonico$es_partido_seleccion[1]
  if (is.na(es_partido_seleccion)) es_partido_seleccion <- FALSE
  
  mapa_jugadora_a_equipo <- apariciones_df %>%
    filter(id_partido == id_p) %>%
    distinct(id, equipo_canonico = equipo)
  
  crear_link_condicional <- function(nombre_a_mostrar, ruta_base, id_seguro, nombre_equipo_original_mk) {
    debe_ser_enlazable <- !es_partido_seleccion || (es_partido_seleccion && nombre_equipo_original_mk == "Македонија")
    if (debe_ser_enlazable && !is.na(id_seguro) && nchar(id_seguro) > 0) {
      return(sprintf("<a href='%s/%s.html'>%s</a>", ruta_base, id_seguro, nombre_a_mostrar))
    } else {
      return(nombre_a_mostrar)
    }
  }
  
  # Step 1: Goals
  goles_data_raw <- goles_df_unificado %>% filter(id_partido == id_p)
  
  if (!is.null(goles_data_raw) && nrow(goles_data_raw) > 0) {
    goles_data <- goles_data_raw %>%
      # CORRECCIÓN: Añadido 'relationship' para silenciar el aviso.
      left_join(mapa_jugadora_a_equipo, by = "id", relationship = "many-to-many") %>%
      mutate(
        equipo_acreditado_canonico = if_else(
          equipo_acreditado == resumen_partido$partido_info$local,
          info_partido_canonico$local,
          info_partido_canonico$visitante
        )
      ) %>%
      left_join(jugadoras_lang_df, by = "id") %>%
      left_join(entidades_lang_df, by = c("equipo_canonico" = "original_name")) %>%
      rename(equipo_jugadora_lang = current_lang_name) %>%
      left_join(entidades_lang_df, by = c("equipo_acreditado_canonico" = "original_name")) %>%
      rename(equipo_acreditado_lang = current_lang_name)
    
    goles_eventos <- goles_data %>% 
      mutate(
        texto_evento = pmap_chr(list(tipo, id, PlayerName, equipo_canonico, equipo_jugadora_lang, equipo_acreditado_canonico, equipo_acreditado_lang), function(t, id_j, j_lang, ej_mk, ej_lang, ea_mk, ea_lang) {
          link_jugadora <- crear_link_condicional(j_lang, path_jugadoras, id_j, ej_mk)
          link_equipo_jugadora <- crear_link_condicional(ej_lang, path_timovi, generar_id_seguro(ej_mk), ej_mk)
          link_equipo_acreditado <- crear_link_condicional(ea_lang, path_timovi, generar_id_seguro(ea_mk), ea_mk)
          if (t == "Autogol") sprintf(t("match_timeline_own_goal"), link_jugadora, link_equipo_jugadora, link_equipo_acreditado) else sprintf(t("match_timeline_goal"), link_jugadora, link_equipo_acreditado)
        })
      ) %>% select(minuto, icono = tipo, texto_evento) %>% mutate(icono = recode(icono, "Normal" = "⚽", "Autogol" = "⚽"))
    lista_eventos[[length(lista_eventos) + 1]] <- goles_eventos
  }
  
  # Step 2: Cards
  tarjetas_data_raw <- tarjetas_df_unificado %>% filter(id_partido == id_p)
  
  if (!is.null(tarjetas_data_raw) && nrow(tarjetas_data_raw) > 0) {
    tarjetas_data <- tarjetas_data_raw %>%
      # CORRECCIÓN: Añadido 'relationship' para silenciar el aviso.
      left_join(mapa_jugadora_a_equipo, by = "id", relationship = "many-to-many") %>%
      left_join(jugadoras_lang_df, by = "id") %>%
      left_join(entidades_lang_df, by = c("equipo_canonico" = "original_name"))
    
    tarjetas_eventos <- tarjetas_data %>%
      mutate(
        texto_evento = pmap_chr(list(id, PlayerName, equipo_canonico, current_lang_name), function(id_j, j_lang, e_mk, e_lang) {
          link_jugadora <- crear_link_condicional(j_lang, path_jugadoras, id_j, e_mk)
          link_equipo <- crear_link_condicional(e_lang, path_timovi, generar_id_seguro(e_mk), e_mk)
          sprintf(t("match_timeline_card"), link_jugadora, link_equipo)
        }),
        icono = if_else(tipo == "Amarilla", "🟨", "🟥")
      ) %>% select(minuto, icono, texto_evento)
    lista_eventos[[length(lista_eventos) + 1]] <- tarjetas_eventos
  }
  
  # Step 3: Substitutions (sin cambios)
  procesar_cambios <- function(cambios_df, nombre_equipo_mk, alineacion_equipo) {
    if (is.null(cambios_df) || nrow(cambios_df) == 0 || is.null(alineacion_equipo) || nrow(alineacion_equipo) == 0) return(NULL)
    map_dfr(1:nrow(cambios_df), function(i) {
      cambio <- cambios_df[i,]; match_info <- str_match(cambio$texto, "Entra (.*?) \\((\\d+)\\) por (.*?) \\((\\d+)\\)")
      if (is.na(match_info[1,1])) return(NULL)
      nombre_entra_raw <- match_info[1, 2]; dorsal_entra <- as.numeric(match_info[1, 3])
      nombre_sale_raw <- match_info[1, 4]; dorsal_sale <- as.numeric(match_info[1, 5])
      id_entra <- (alineacion_equipo %>% filter(dorsal == dorsal_entra))$id[1]
      id_sale <- (alineacion_equipo %>% filter(dorsal == dorsal_sale))$id[1]
      nombre_entra_lang <- (jugadoras_lang_df %>% filter(id == id_entra))$PlayerName[1] %||% nombre_entra_raw
      nombre_sale_lang <- (jugadoras_lang_df %>% filter(id == id_sale))$PlayerName[1] %||% nombre_sale_raw
      nombre_equipo_lang <- (entidades_lang_df %>% filter(original_name == nombre_equipo_mk))$current_lang_name[1]
      link_entra <- crear_link_condicional(nombre_entra_lang, path_jugadoras, id_entra, nombre_equipo_mk)
      link_sale <- crear_link_condicional(nombre_sale_lang, path_jugadoras, id_sale, nombre_equipo_mk)
      link_equipo <- crear_link_condicional(nombre_equipo_lang, path_timovi, generar_id_seguro(nombre_equipo_mk), nombre_equipo_mk)
      texto_final <- sprintf(t("match_timeline_substitution"), link_equipo, link_entra, dorsal_entra, link_sale, dorsal_sale)
      tibble(minuto = cambio$minuto, icono = "🔄", texto_evento = texto_final)
    })
  }
  alineacion_partido <- apariciones_df %>% filter(id_partido == id_p)
  cambios_local_eventos <- procesar_cambios(resumen_partido$cambios_local, info_partido_canonico$local, filter(alineacion_partido, equipo == info_partido_canonico$local))
  cambios_visitante_eventos <- procesar_cambios(resumen_partido$cambios_visitante, info_partido_canonico$visitante, filter(alineacion_partido, equipo == info_partido_canonico$visitante))
  
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
    paste0(comp_info$competicion_id, "_", nombres_archivos_traducidos$partidos, ".html")
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
    paste0(comp_info$competicion_id, "_", nombres_archivos_traducidos$clasificacion, ".html")
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

nombres_archivos_traducidos <- list(
  partidos = "raspored", 
  clasificacion = "tabela", 
  goleadoras = "strelci", 
  sanciones = "disciplinska",
  archive = "arhiva",
  about = "za-zfudbalmk",
  players = "fudbalerki",
  teams = "klubovi"
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
# El mapa ahora se guarda como un dataframe para la nueva función `aplicar_conversiones`.
mapa_conversiones_df <- NULL 
if (file.exists(ruta_conversiones)) {
  tryCatch({
    conversiones_df <- read.csv(ruta_conversiones, stringsAsFactors = FALSE, encoding = "UTF-8")
    
    # CLAVE: Crear un dataframe de mapeo que es case-insensitive y permite duplicados en la entrada.
    mapa_conversiones_df <- conversiones_df %>%
      # Convertir la columna 'original' a minúsculas para el cruce.
      mutate(original_lower = tolower(original)) %>%
      # Seleccionar solo las columnas necesarias.
      select(original_lower, corregido) %>%
      # Asegurarse de que el mapa en sí no tenga filas duplicadas.
      distinct()
    
    message("Corrections file loaded and processed for case-insensitive matching.")
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
#' compatible with `partidos_df`, ensuring 'fecha' and 'hora' columns exist.
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
    match_nombre <- str_match(nombre_base, "^(.*?)\\s+(\\d{2}_\\d{2})$")
    
    if (is.na(match_nombre[1, 1])) {
      warning(paste("Filename", basename(ruta_archivo), "is not in the expected format ('Competition Name YY_YY'). Skipping."))
      return(NULL)
    }
    
    comp_nombre <- str_trim(match_nombre[1, 2])
    comp_temporada <- str_replace(match_nombre[1, 3], "_", "/")
    
    message(paste("   > Loading calendar for:", comp_nombre, comp_temporada))
    
    df_excel <- tryCatch({ read_excel(ruta_archivo) }, error = function(e) {
      warning(paste("Error reading Excel file:", ruta_archivo, "-", e$message)); return(NULL)
    })
    
    if (is.null(df_excel) || ncol(df_excel) < 4) return(NULL)
    names(df_excel)[1:4] <- c("jornada", "fecha_hora", "lugar", "partido_raw")
    
    df_excel %>%
      filter(!is.na(partido_raw)) %>%
      mutate(
        jornada = as.character(jornada),
        competicion_nombre = comp_nombre,
        competicion_temporada = comp_temporada,
        # Extraer fecha y hora de forma segura
        fecha_hora_str = as.character(fecha_hora),
        fecha = str_extract(fecha_hora_str, "\\d{2}\\.\\d{2}\\.\\d{4}"),
        hora = str_extract(fecha_hora_str, "\\d{2}:\\d{2}"),
        # Dividir nombres de equipos
        equipos_split = str_split_fixed(partido_raw, "\\s*-\\s*", 2),
        local = str_trim(equipos_split[, 1]),
        visitante = str_trim(equipos_split[, 2])
      ) %>%
      # Seleccionar las columnas finales, asegurando que 'fecha' y 'hora' están presentes
      select(competicion_nombre, competicion_temporada, jornada, fecha, hora, local, visitante)
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

### 9.12. Load City Translations
message("Loading city name translations...")

ruta_traducciones_ciudades <- "gradovi.txt"
mapa_ciudades_long_df <- NULL
if (file.exists(ruta_traducciones_ciudades)) {
  tryCatch({
    mapa_ciudades_df <- read.csv(
      ruta_traducciones_ciudades, 
      stringsAsFactors = FALSE, 
      encoding = "UTF-8",
      check.names = FALSE
    )
    
    mapa_ciudades_long_df <- mapa_ciudades_df %>%
      pivot_longer(
        cols = -en, 
        names_to = "lang",
        values_to = "translated_city"
      )
    
    message(paste("City translations loaded successfully with", nrow(mapa_ciudades_df), "entries."))
  }, error = function(e) {
    warning("Error loading gradovi.txt. City names will not be translated.")
  })
} else {
  message("gradovi.txt file not found. City names will not be translated.")
}

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
  if (is.na(text) || nchar(text) == 0) return(FALSE)
  chars <- strsplit(text, "")[[1]]; alpha_chars <- chars[grepl("[A-Za-z]", chars)]
  if (length(alpha_chars) == 0) return(FALSE)
  return(sum(grepl("^[A-Za-z]$", alpha_chars)) / length(alpha_chars) > 0.5)
}
resultados_exitosos <- map(resultados_exitosos, function(res) {
  if (is.null(res) || is.null(res$partido_info)) return(res)
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
    if(is.null(res)) return(NULL)
    
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
    if (is.null(res)) return(NULL)
    
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
        if (is.null(nombre) || is.na(nombre) || !stringr::str_detect(nombre, "\\s+")) return(nombre)
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
  if (is.null(res)) return(NULL)
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
  if (is.null(res) || is.null(res$partido_info)) return(NULL)
  
  df_arbitros_partido <- tibble(
    id_partido = character(),
    ime = character(),
    ciudad = character(),
    uloga = character()
  )
  
  id_p <- res$partido_info$id_partido
  
  # Función auxiliar para extraer de forma segura y LIMPIAR el nombre del árbitro.
  extraer_info_arbitro <- function(obj) {
    if (is.null(obj)) return(list(nombre = NULL, ciudad = NULL))
    
    nombre_raw <- if (is.list(obj)) obj$nombre %||% NULL else as.character(obj)
    if (is.null(nombre_raw)) return(list(nombre = NULL, ciudad = NULL))
    
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
  if (is.null(res) || is.null(res$partido_info)) return(NULL)
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
      str_detect(tolower(competicion_nombre), "кадетска")  ~ 60,
      TRUE ~ 90
    ),
    es_partido_seleccion = (local == "Македонија" | visitante == "Македонија"),
    competicion_nombre = if_else(es_partido_seleccion, "Репрезентација", competicion_nombre),
    competicion_temporada = if_else(es_partido_seleccion, "Сите", competicion_temporada)
  )

### 10.3. Consolidate Player Appearances and Unify IDs
message("Step 10.3: Consolidating player appearances and unifying IDs...")
apariciones_df_raw <- map_dfr(resultados_exitosos, ~bind_rows(
  .x$alineacion_local %>% mutate(id_partido = .x$partido_info$id_partido, equipo = .x$partido_info$local),
  .x$alineacion_visitante %>% mutate(id_partido = .x$partido_info$id_partido, equipo = .x$partido_info$visitante)
)) %>% mutate(nombre = str_squish(nombre))
preferred_id_map <- apariciones_df_raw %>% filter(!is.na(nombre), !is.na(id), str_detect(id, "^\\d{5,6}$")) %>% count(nombre, id, name = "frequency") %>% group_by(nombre) %>% filter(frequency == max(frequency)) %>% slice(1) %>% ungroup() %>% select(nombre, canonical_id = id)
id_mapping <- apariciones_df_raw %>% filter(!is.na(nombre) & nchar(trimws(nombre)) > 2) %>% distinct(nombre) %>% left_join(preferred_id_map, by = "nombre") %>% mutate(final_id = if_else(!is.na(canonical_id), as.character(canonical_id), paste0("player_gen_", generar_id_seguro(nombre)))) %>% select(nombre, canonical_id = final_id)

if (nrow(goles_df_unificado) > 0) goles_df_unificado <- goles_df_unificado %>% left_join(id_mapping, by = c("jugadora" = "nombre")) %>% select(-any_of(c("id", "id_jugadora"))) %>% rename(id = canonical_id)
if (nrow(tarjetas_df_unificado) > 0) tarjetas_df_unificado <- tarjetas_df_unificado %>% left_join(id_mapping, by = c("jugadora" = "nombre")) %>% select(-any_of(c("id", "id_jugadora"))) %>% rename(id = canonical_id)
if (nrow(penales_df_unificado) > 0) penales_df_unificado <- penales_df_unificado %>% left_join(id_mapping, by = c("jugadora" = "nombre")) %>% select(-any_of(c("id", "id_jugadora"))) %>% rename(id = canonical_id)

minutos_df_raw <- map_dfr(resultados_exitosos, function(res) {
  if(is.null(res)) return(NULL)
  id_p <- res$partido_info$id_partido
  duracion <- (partidos_df %>% filter(id_partido == id_p) %>% pull(duracion_partido))[1]
  if (length(duracion) == 0 || is.na(duracion)) { duracion <- 90 }
  calcular_minutos_equipo <- function(alineacion, cambios, duracion_partido) {
    if(is.null(alineacion) || nrow(alineacion) == 0) return(NULL)
    jugadoras_con_minutos <- alineacion %>% mutate(min_entra = if_else(tipo == "Titular", 0, NA_real_), min_sale = if_else(tipo == "Titular", duracion_partido, 0))
    if (!is.null(cambios) && nrow(cambios) > 0) {
      cambios_procesados <- cambios %>% mutate(d_entra = as.numeric(str_match(texto, "Entra .*?\\((\\d+)\\)")[, 2]), d_sale  = as.numeric(str_match(texto, "por .*?\\((\\d+)\\)")[, 2])) %>% select(minuto, d_entra, d_sale) %>% filter(!is.na(d_entra) & !is.na(d_sale))
      for (i in 1:nrow(cambios_procesados)) {
        cambio <- cambios_procesados[i, ]; jugadoras_con_minutos <- jugadoras_con_minutos %>% mutate(min_sale = if_else(dorsal == cambio$d_sale, as.numeric(cambio$minuto), min_sale), min_entra = if_else(dorsal == cambio$d_entra, as.numeric(cambio$minuto), min_entra))
      }
    }
    jugadoras_con_minutos %>% mutate(min_sale = if_else(!is.na(min_entra) & tipo == "Suplente" & min_sale == 0, duracion_partido, min_sale), minutos_jugados = if_else(is.na(min_entra), 0, min_sale - min_entra)) %>% mutate(minutos_jugados = pmax(0, minutos_jugados))
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
  apariciones_df <- apariciones_df %>% left_join(mapa_roles_forzados_df, by = "id") %>%
    mutate(es_portera = if_else(!is.na(force_role) & force_role == "goalkeeper", TRUE, es_portera)) %>%
    select(-force_role)
}
message("   > Player appearances consolidated and IDs unified.")

### 10.4. Process and Translate Demographic Data
message("Step 10.4: Processing demographic data...")
mapa_posicion_unificada <- c("GK"="goalkeeper","Portera"="goalkeeper","DL"="defender","DC"="defender","DR"="defender","DM"="defender","WBL"="defender","WBR"="defender","Defensa"="defender","ML"="midfielder","MC"="midfielder","MR"="midfielder","AMC"="midfielder","Centrocampista"="midfielder","AML"="forward","AMR"="forward","SC"="forward","Delantera"="forward")
posiciones_procesadas_df <- posiciones_df %>%
  mutate(posicion_unificada = recode(posicion, !!!mapa_posicion_unificada, .default = NA_character_)) %>%
  filter(!is.na(posicion_unificada)) %>% group_by(id) %>%
  summarise(posicion_final_unificada = paste(unique(posicion_unificada), collapse = " / "), nacionalidad = first(nacionalidad), fecha_nacimiento = first(fecha_nacimiento), ciudad_nacimiento = first(ciudad_nacimiento), .groups = 'drop')

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

map_transliteration_entity <- c('а'='a','б'='b','в'='v','г'='g','д'='d','ѓ'='gj','е'='e','ж'='ž','з'='z','ѕ'='dz','и'='i','ј'='j','к'='k','л'='l','љ'='lj','м'='m','н'='n','њ'='nj','о'='o','п'='p','р'='r','с'='s','т'='t','ќ'='kj','у'='u','ф'='f','х'='h','ц'='c','ч'='č','џ'='dž','ш'='š')

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
    filter(competicion_nombre != "Репрезентација") %>% distinct(competicion_temporada) %>%
    mutate(start_year = as.integer(str_extract(competicion_temporada, "^\\d{2,4}")),
           sort_year = if_else(nchar(as.character(start_year)) == 2, 2000 + start_year, start_year)) %>%
    pull(sort_year) %>% max(na.rm = TRUE)
  if (is.infinite(max_real_season_numeric)) max_real_season_numeric <- 2000 
  
  competiciones_base_df <- partidos_df %>%
    filter(competicion_nombre != "Репрезентација") %>% distinct(competicion_nombre, competicion_temporada) %>%
    mutate(competicion_id = generar_id_seguro(paste(competicion_nombre, competicion_temporada)),
           nombre_lower = tolower(competicion_nombre),
           start_year = as.integer(str_extract(competicion_temporada, "^\\d{2,4}")),
           sort_year = if_else(nchar(as.character(start_year)) == 2, 2000 + start_year, start_year)) %>%
    mutate(importancia_score = case_when(str_detect(nombre_lower, "куп") ~ 1, str_detect(nombre_lower, "прва") ~ 2, str_detect(nombre_lower, "втора") ~ 3, str_detect(nombre_lower, "трета") ~ 4, str_detect(nombre_lower, "младинска") ~ 5, str_detect(nombre_lower, "кадетска") ~ 6, str_detect(nombre_lower, "пријателски") ~ 7, TRUE ~ 7),
           baraz_modifier = if_else(str_detect(nombre_lower, "бараж"), 0.5, 0),
           final_score = importancia_score + baraz_modifier)
  
  competicion_seleccion_df <- tibble(competicion_nombre = "Репрезентација", competicion_temporada = "Сите", competicion_id = "reprezentacija", nombre_lower = "репрезентација", importancia_score = 0, baraz_modifier = 0, final_score = 0, sort_year = max_real_season_numeric + 0.5)
  
  competiciones_combinadas_df <- bind_rows(competiciones_base_df, competicion_seleccion_df) %>%
    mutate(orden_primario = case_when(sort_year == max_real_season_numeric ~ 1, competicion_id == "reprezentacija" ~ 2, TRUE ~ 3))
  
  if (!is.null(mapa_nombres_competiciones_long)) {
    competiciones_combinadas_df_temp <- competiciones_combinadas_df %>% mutate(original_mk_join_key = paste(competicion_nombre, competicion_temporada))
    comp_translations_wide <- mapa_nombres_competiciones_long %>% pivot_wider(id_cols = original_mk, names_from = lang, values_from = translated_name, names_prefix = "nombre_completo_")
    competiciones_unicas_df <- competiciones_combinadas_df_temp %>% left_join(comp_translations_wide, by = c("original_mk_join_key" = "original_mk")) %>% select(-original_mk_join_key)
  } else {
    competiciones_unicas_df <- competiciones_combinadas_df
  }
  
  competiciones_unicas_df <- competiciones_unicas_df %>% mutate(nombre_completo_mk = if_else(competicion_nombre == "Репрезентација", "Репрезентација", paste(competicion_nombre, competicion_temporada)))
  map_transliteration_comp <- c('а'='a','б'='b','в'='v','г'='g','д'='d','ѓ'='gj','е'='e','ж'='ž','з'='z','ѕ'='dz','и'='i','ј'='j','к'='k','л'='l','љ'='lj','м'='m','н'='n','њ'='nj','о'='o','п'='p','р'='r','с'='s','т'='t','ќ'='kj','у'='u','ф'='f','х'='h','ц'='c','ч'='č','џ'='dž','ш'='š')
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
ruta_build_log <- "build_log.rds"
partidos_previamente_construidos_ids <- if (file.exists(ruta_build_log)) readRDS(ruta_build_log) else character(0)
partidos_actuales_ids <- partidos_df %>% filter(!is.na(id_partido)) %>% pull(id_partido) %>% unique()
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
  affected_competition_ids <- character(0); affected_player_ids <- character(0); affected_team_ids <- character(0); affected_referee_ids <- character(0); affected_stadium_ids <- character(0)
  if (length(affected_match_ids) > 0) {
    partidos_afectados_df <- partidos_df %>% filter(id_partido %in% affected_match_ids) %>% left_join(competiciones_unicas_df, by = c("competicion_nombre", "competicion_temporada"))
    if(nrow(partidos_afectados_df) > 0) {
      affected_competition_ids <- unique(c(affected_competition_ids, na.omit(partidos_afectados_df$competicion_id)))
      affected_team_ids <- unique(c(affected_team_ids, generar_id_seguro(partidos_afectados_df$local), generar_id_seguro(partidos_afectados_df$visitante)))
    }
    jugadoras_afectadas_df <- apariciones_df %>% filter(id_partido %in% affected_match_ids)
    if(nrow(jugadoras_afectadas_df) > 0) affected_player_ids <- unique(c(affected_player_ids, na.omit(jugadoras_afectadas_df$id)))
    arbitros_afectados_df <- arbitros_df %>% filter(id_partido %in% affected_match_ids)
    if(nrow(arbitros_afectados_df) > 0) affected_referee_ids <- unique(c(affected_referee_ids, generar_id_seguro(na.omit(arbitros_afectados_df$ime))))
    estadios_afectados_df <- estadios_df %>% filter(id_partido %in% affected_match_ids)
    if(nrow(estadios_afectados_df) > 0) affected_stadium_ids <- unique(c(affected_stadium_ids, generar_id_seguro(na.omit(estadios_afectados_df$estadio))))
  }
  message(paste("   > Identified", length(affected_competition_ids), "competitions,", length(affected_match_ids), "matches, and", length(affected_player_ids), "players for update."))
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
    select(
      id, starts_with("PlayerName_"), Team, 
      posicion_final_unificada, 
      fecha_nacimiento,           
      ciudad_nacimiento,         
      nacionalidad, edad, codigo_iso, nombre_macedonio, 
      CalledUp, Played, Starter, Minutes, Goals, Yellows, Reds
    ) %>% 
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

message("Calculating career summaries for the National Team by category...")

national_team_career_by_category_df <- apariciones_df %>%
  filter(!is.na(id)) %>%
  left_join(partidos_df %>% select(id_partido, es_partido_seleccion, categoria), by = "id_partido") %>%
  filter(es_partido_seleccion == TRUE, equipo == "Македонија", !is.na(categoria)) %>%
  group_by(id, categoria) %>%
  summarise(
    CalledUp = n_distinct(id_partido),
    Played = sum(minutos_jugados > 0, na.rm=TRUE),
    Starter = sum(tipo=="Titular", na.rm=TRUE),
    Minutes = sum(minutos_jugados, na.rm=TRUE),
    .groups = 'drop'
  ) %>%
  left_join(
    goles_df_unificado %>%
      filter(!is.na(id), tipo == "Normal") %>%
      left_join(partidos_df %>% select(id_partido, es_partido_seleccion, categoria), by = "id_partido") %>%
      filter(es_partido_seleccion == TRUE, equipo_jugadora == "Македонија", !is.na(categoria)) %>%
      group_by(id, categoria) %>%
      summarise(Goals = n(), .groups = 'drop'),
    by = c("id", "categoria")
  ) %>%
  left_join(
    tarjetas_df_unificado %>%
      filter(!is.na(id)) %>%
      left_join(partidos_df %>% select(id_partido, es_partido_seleccion, categoria), by = "id_partido") %>%
      filter(es_partido_seleccion == TRUE, equipo == "Македонија", !is.na(categoria)) %>%
      group_by(id, categoria) %>%
      summarise(Yellows = sum(tipo == "Amarilla", na.rm=T), Reds = sum(tipo == "Roja", na.rm=T), .groups = 'drop'),
    by = c("id", "categoria")
  ) %>%
  mutate(across(c(CalledUp, Played, Starter, Minutes, Goals, Yellows, Reds), ~replace_na(., 0)))

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
    {
      # CORRECCIÓN: Renombrar 'equipo' a 'equipo_canonico' AL CREAR el mapa.
      mapa_partido_jugadora_a_equipo <- apariciones_df %>% 
        distinct(id, id_partido, equipo_canonico = equipo)
      
      goles_df_unificado %>% 
        filter(!is.na(id)) %>%
        left_join(mapa_partido_jugadora_a_equipo, by = c("id", "id_partido"), relationship = "many-to-many") %>%
        left_join(partidos_df %>% select(id_partido, competicion_temporada, competicion_nombre), by="id_partido") %>%
        filter(!is.na(equipo_canonico)) %>% 
        group_by(id, competicion_temporada, competicion_nombre, equipo = equipo_canonico) %>%
        summarise(Goals = sum(tipo == "Normal"), .groups = 'drop')
    },
    by = c("id", "competicion_temporada", "competicion_nombre", "equipo")
  ) %>%
  full_join(
    {
      # CORRECCIÓN: Aplicar el mismo renombrado aquí.
      mapa_partido_jugadora_a_equipo <- apariciones_df %>% 
        distinct(id, id_partido, equipo_canonico = equipo)
      
      tarjetas_df_unificado %>% 
        filter(!is.na(id)) %>%
        left_join(mapa_partido_jugadora_a_equipo, by = c("id", "id_partido"), relationship = "many-to-many") %>%
        left_join(partidos_df %>% select(id_partido, competicion_temporada, competicion_nombre), by="id_partido") %>%
        filter(!is.na(equipo_canonico)) %>%
        group_by(id, competicion_temporada, competicion_nombre, equipo = equipo_canonico) %>%
        summarise(Yellows = sum(tipo == "Amarilla", na.rm=T), Reds = sum(tipo == "Roja", na.rm=T), .groups = 'drop')
    },
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

### 11.7. Calculate Team Profile Summaries (VERSIÓN ALINEADA CON SCRIPT DE PRUEBAS)
stats_jugadoras_por_equipo_temporada_df <- apariciones_df %>%
  group_by(id, equipo, competicion_nombre, competicion_temporada) %>%
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


# ==============================================================================
# ==      AÑADIR ESTE BLOQUE PARA CREAR LA INSTANTÁNEA DE DATOS PARA PRUEBAS      ==
# ==============================================================================
message(">>>>>> CREANDO INSTANTÁNEA DE DATOS PARA PRUEBAS (entorno_procesado.RData)...")

# Lista de todos los objetos que necesitamos para generar las páginas
objetos_a_guardar <- c(
  # Dataframes principales de datos
  "partidos_df", "apariciones_df", "goles_df_unificado", "tarjetas_df_unificado",
  "arbitros_df", "estadios_df",
  
  # Dataframes de estadísticas agregadas
  "jugadoras_stats_df", "stats_clasificacion_por_comp_df", 
  "stats_goleadoras_por_comp_df", "stats_sanciones_por_comp_df",
  "stats_porteras_por_comp_df", "stats_trios_defensivos_df",
  "career_summary_jugadoras_df", "stats_equipos_por_temporada_df",
  "stats_jugadoras_por_equipo_temporada_df", "stats_arbitros_por_temporada_df",
  "national_team_career_summary_df", "national_team_career_by_category_df",
  
  # Dataframes de configuración y mapeo
  "entidades_maestro_df", "competiciones_unicas_df", "mapeo_completo_df",
  
  # Variables y listas de configuración
  "textos", "IDIOMAS_SOPORTADOS", "nombres_carpetas_relativos", 
  "estilos_clasificacion_data", "nombres_archivos_traducidos",
  
  # Variables de exclusión
  "player_ids_to_skip", "team_names_to_skip_mk", "referee_ids_to_skip", "stadium_ids_to_skip"
)

# Guardar solo los objetos existentes en el entorno
save(
  list = intersect(objetos_a_guardar, ls()), 
  file = "entorno_procesado.RData"
)

message(">>>>>> Instantánea guardada. Puedes detener el script si solo querías esto.")
# ==============================================================================

# Crear el historial de partidos enriquecido que necesita la página de equipo
message("Creating enriched match history for team pages...")
historial_partidos_con_categoria <- partidos_df %>%
  filter(!is.na(id_partido)) %>%
  left_join(competiciones_unicas_df, by = c("competicion_nombre", "competicion_temporada")) %>%
  mutate(
    categoria = case_when(
      str_detect(competicion_nombre, "Младинска") ~ "Младинки",
      str_detect(competicion_nombre, "Кадетска") ~ "Кадетки",
      TRUE ~ "Сениорки"
    ),
    category_key = case_when(
      categoria == "Младинки" ~ "category_youth",
      categoria == "Кадетки" ~ "category_cadet",
      TRUE ~ "category_senior"
    ),
    home_logo_url = paste0("../../assets/logos/", generar_id_seguro(local), ".png"),
    away_logo_url = paste0("../../assets/logos/", generar_id_seguro(visitante), ".png")
  )

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
.clickable-row:hover { background-color: #f8f9fa; }
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
.header-main { display: flex; justify-content: space-between; align-items: center; border-bottom: 2px solid #dee2e6; padding-bottom: 10px; }
.header-main h1 { border-bottom: none; padding-bottom: 0; margin: 0; text-align: left; }
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
.results-scroll-container::-webkit-scrollbar { height: 8px; }
.results-scroll-container::-webkit-scrollbar-track { background: #f1f1f1; border-radius: 4px; }
.results-scroll-container::-webkit-scrollbar-thumb { background: #ccc; border-radius: 4px; }
.results-scroll-container::-webkit-scrollbar-thumb:hover { background: #aaa; }
.standings-grid-container { display: grid; grid-template-columns: repeat(4, 1fr); gap: 20px; margin-top: 20px; }
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
@media (max-width: 900px) { .standings-grid-container { grid-template-columns: repeat(2, 1fr); } }
@media (max-width: 500px) { .standings-grid-container { grid-template-columns: 1fr; } }
.standings-grid-title { margin-top: 40px !important; }
.mini-standings-title-link { text-decoration: none; color: inherit; }
.mini-standings-title { font-size: 1em; text-align: center; margin: 0 0 10px 0; padding-bottom: 8px; border-bottom: 1px solid #dee2e6; font-weight: bold; color: #333; }
.mini-standings-title-link:hover .mini-standings-title { color: #CC0000; }
.navbar { background-color: transparent; position: relative; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; }
.nav-links { list-style-type: none; margin: 0; padding: 0; display: flex; background-color: #333; border-radius: 5px; }
.nav-links li a, .dropbtn { display: block; color: white; text-align: center; padding: 10px 14px; text-decoration: none; transition: background-color 0.3s; border-radius: 5px; }
.nav-links li > a:hover, .dropdown:hover .dropbtn { background-color: #CC0000; }
.nav-links li a.active { background-color: #8B0000; font-weight: bold; }
.dropdown { position: relative; }
.dropdown-content { display: none; position: absolute; background-color: #8B0000; min-width: 200px; box-shadow: 0px 8px 16px 0px rgba(0,0,0,0.2); z-index: 1001; border-radius: 5px; }
.dropdown-content a { color: #CC0100; font-weight: bold; padding: 12px 16px; text-decoration: none; display: block; text-align: left; }
.dropdown-content a:hover { background-color: #CC0100; }
.dropdown:hover .dropdown-content { display: block; }
.dropdown-content hr { border: 0; border-top: 1px solid #ddd; margin: 5px 0; }
.hamburger-icon { display: none; }
.close-btn-li { display: none; }
@media screen and (max-width: 850px) {
  .header-main { border-bottom: none; }
  .navbar { background-color: transparent; }
  .nav-links { display: none; flex-direction: column; position: fixed; top: 0; left: 0; width: 100%; height: 100%; background-color: rgba(20, 20, 20, 0.98); justify-content: center; align-items: center; z-index: 2000; border-radius: 0; }
  .navbar.mobile-active .nav-links { display: flex; }
  .nav-links li { margin: 15px 0; }
  .nav-links li a { font-size: 1.5em; background-color: transparent; }
  .dropdown-content { position: static; display: none; background-color: transparent; box-shadow: none; padding-left: 20px; border: none; }
  .dropdown:hover .dropdown-content { display: none; }
  .dropdown.open .dropdown-content { display: block; }
  .dropdown .dropbtn { display: flex; align-items: center; justify-content: center; }
  .hamburger-icon { display: block; cursor: pointer; padding: 10px; z-index: 2001; }
  .hamburger-icon .bar { display: block; width: 25px; height: 3px; margin: 5px auto; background-color: #333; transition: all 0.3s ease-in-out; }
  .close-btn-li { display: block; position: absolute; top: 20px; right: 25px; }
  #close-nav-btn { font-size: 3em; font-weight: bold; color: white; }
}
.letter-nav { text-align: center; margin-bottom: 20px; padding: 10px; background: #f1f1f1; border-radius: 5px; }
.letter-nav a { display: inline-block; padding: 5px 10px; margin: 2px; background-color: #fff; border: 1px solid #ccc; border-radius: 3px; color: #333; text-decoration: none; font-weight: bold; }
.letter-nav a:hover { background-color: #CC0000; color: white; }
.letter-nav a.active { background-color: #8B0000; color: white; border-color: #8B0000; }
.letter-group { display: none; }
.letter-group.active { display: block; }
.player-list ul, .team-list ul { list-style: none; padding: 0; columns: 3; column-gap: 20px; }
.player-list li, .team-list li { padding: 5px 0; }
@media (max-width: 768px) { .player-list ul, .team-list ul { columns: 2; } }
@media (max-width: 480px) { .player-list ul, .team-list ul { columns: 1; } }
.player-profile-header-new { display: flex; align-items: center; background-color: #f8f9fa; border-radius: 8px; padding: 20px; gap: 30px; margin-bottom: 30px; border: 1px solid #e9ecef; flex-wrap: wrap; }
.player-name-container { flex: 1 1 40%; }
.player-name-new { font-size: 2.5em; color: #1a1a1a; margin: 0; padding: 0; border: none; font-weight: 700; line-height: 1.2; }
.player-bio { flex: 1 1 50%; display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 10px; }
.bio-item { display: flex; align-items: center; gap: 10px; }
.bio-icon { color: #8B0000; }
.bio-text { display: flex; flex-direction: column; }
.bio-label { font-size: 0.8em; color: #6c757d; }
.bio-value { font-size: 1em; font-weight: 600; color: #212529; }
.stats-summary-card { position: relative; background-color: #fff; border-radius: 8px; box-shadow: 0 4px 15px rgba(0,0,0,0.07); padding: 25px; margin-bottom: 40px; border: 1px solid #e9ecef; }
.season-tag { position: absolute; top: -12px; left: 50%; transform: translateX(-50%); background-color: #8B0000; color: white; font-weight: bold; padding: 5px 15px; border-radius: 20px; font-size: 0.9em; }
.stats-grid { display: grid; grid-template-columns: repeat(6, 1fr); gap: 10px; text-align: center; }
.stat-item { padding: 10px 0; }
.stat-icon { height: 32px; width: 32px; margin: 0 auto 10px auto; color: #8B0000; }
.stat-icon.card-icon { height: 28px; width: 20px; border-radius: 3px; border: 1px solid rgba(0,0,0,0.2); }
.stat-icon.card-icon.yellow { background-color: #ffc107; }
.stat-icon.card-icon.red { background-color: #dc3545; }
.stat-value { font-size: 2em; font-weight: 700; color: #1a1a1a; display: block; line-height: 1; }
.stat-label { font-size: 0.8em; color: #6c757d; margin-top: 5px; }
.season-accordion-container h3 { text-align: center; border: none; color: #333; }
.season-accordion { border: 1px solid #dee2e6; border-radius: 5px; margin-bottom: 10px; overflow: hidden; }
.season-header { cursor: pointer; padding: 15px; background-color: #f8f9fa; display: flex; justify-content: space-between; align-items: center; font-weight: 600; }
.season-header:hover { background-color: #e9ecef; }
.season-arrow { transition: transform 0.3s ease; }
.season-accordion.active .season-arrow { transform: rotate(180deg); }
.season-arrow::before { content: '▼'; font-size: 0.8em; color: #6c757d; }
.season-content { display: none; padding: 15px; border-top: 1px solid #dee2e6; }
.tab-nav { margin-bottom: 15px; border-bottom: 1px solid #dee2e6; }
.tab-button { background: none; border: none; padding: 10px 15px; cursor: pointer; font-size: 1em; color: #6c757d; font-weight: 600; border-bottom: 3px solid transparent; }
.tab-button.active { color: #8B0000; border-bottom-color: #8B0000; }
.tab-panel { display: none; }
.tab-panel.active { display: block; }
.stats-table-season th, .matches-table-season th { font-size: 0.9em; padding: 8px; }
.stats-table-season td, .matches-table-season td { font-size: 0.9em; padding: 8px; }
.team-cell-with-logo { display: flex; align-items: center; gap: 10px; }
.team-cell-with-logo img { height: 20px; width: 20px; flex-shrink: 0; }
.team-cell-with-logo a { font-weight: normal; }
.team-logo-small.national-team-flag { border-radius: 50%; border: 1px solid #ccc; }
@media (max-width: 768px) {
  .player-profile-header-new { flex-direction: column; align-items: flex-start; }
  .player-name-new { font-size: 2em; text-align: left; }
  .player-bio { width: 100%; }
  .stats-grid { grid-template-columns: repeat(3, 1fr); }
  .stat-item { margin-bottom: 15px; }
}
@media (max-width: 480px) {
  .stats-grid { grid-template-columns: repeat(2, 1fr); }
}
.sub-tab-nav { display: flex; flex-wrap: wrap; gap: 5px; margin-bottom: 15px; padding-bottom: 10px; border-bottom: 1px solid #e9ecef; }
.sub-tab-button { background-color: #f1f1f1; border: 1px solid #ddd; border-radius: 20px; padding: 5px 12px; cursor: pointer; font-size: 0.85em; font-weight: 500; color: #333; transition: all 0.2s ease; }
.sub-tab-button:hover { background-color: #e0e0e0; }
.sub-tab-button.active { background-color: #8B0000; color: white; border-color: #8B0000; }
.sub-tab-panel { display: none; }
.sub-tab-panel.active { display: block; }
.card-icon-header { display: inline-block; width: 10px; height: 14px; border-radius: 2px; vertical-align: middle; }
.card-icon-header.yellow { background-color: #ffc107; border: 1px solid #c69500; }
.card-icon-header.red { background-color: #dc3545; border: 1px solid #a71d2a; }
.match-list-container { font-size: 0.9em; }
.match-list-header { display: grid; grid-template-columns: 1.5fr 5fr 1.2fr 0.8fr 0.8fr 1fr; gap: 10px; font-weight: bold; color: #6c757d; padding: 10px; border-bottom: 2px solid #dee2e6; margin-bottom: 5px; }
.match-list-header > div { text-align: center; } /* Regla general: Centra TODOS los títulos */
.match-list-header > div:first-child { text-align: left; } /* Excepción: Alinea a la izquierda SÓLO el PRIMER título (Fecha) */
.match-list-row { display: grid; grid-template-columns: 1.5fr 5fr 1.2fr 0.8fr 0.8fr 1fr; gap: 10px; align-items: center; padding: 12px 10px; border-bottom: 1px solid #e9ecef; transition: background-color 0.2s; }
.match-list-row.clickable-row { cursor: pointer; }
.match-list-row.clickable-row:hover { background-color: #f8f9fa; }
.cell-match { display: flex; align-items: center; justify-content: space-between; }
.team-home, .team-away { display: flex; align-items: center; gap: 8px; flex: 1; font-weight: 500; }
.team-home { justify-content: flex-end; text-align: right; }
.team-away { justify-content: flex-start; text-align: left; }
.team-home .team-logo-small, .team-away .team-logo-small { height: 20px; width: 20px; }
.match-score { background-color: #e9ecef; font-weight: bold; padding: 5px 10px; border-radius: 4px; margin: 0 10px; white-space: nowrap; }
.cell-goals, .cell-minutes, .cell-link { text-align: center; }
.cell-cards { text-align: center; display: flex; gap: 4px; justify-content: center; align-items: center; }
.card-icon-table { display: inline-block; width: 8px; height: 12px; border-radius: 1px; }
.card-icon-table.yellow { background-color: #ffc107; }
.card-icon-table.red { background-color: #dc3545; }
/* --- INICIO: ESTILOS PARA PÁGINA DE EQUIPO (V8) --- */

/* --- Cabecera con Logo --- */
.team-header-container-v5 { display: flex; align-items: center; gap: 25px; padding-bottom: 20px; border-bottom: 1px solid #e0e0e0; margin-bottom: 20px; }
.team-logo-main img { width: 90px; height: 90px; object-fit: contain; }
.team-header-info { flex-grow: 1; }
.team-name-v3 { font-size: 2.8em; color: #1a1a1a; margin: 0 0 15px 0; font-weight: 700; line-height: 1.1; text-align: left; padding: 0; border: none; }
.info-item { display: flex; align-items: center; gap: 10px; }
.info-icon svg { fill: #6c757d; width:20px; height:20px; }
.info-text { display: flex; flex-direction: column; }
.info-label { font-size: 0.7em; color: #6c757d; text-transform: uppercase; font-weight: 500; }
.info-value { font-size: 0.9em; font-weight: 600; color: #212529; }

/* --- Pestañas Principales (Roster/Schedule) --- */
.tab-nav-v4 { display: flex; gap: 10px; border-bottom: 1px solid #dee2e6; }
.tab-button-v4 { background: none; border: 1px solid transparent; border-bottom: none; padding: 8px 12px; cursor: pointer; font-size: 0.9em; color: #6c757d; font-weight: 600; margin-bottom: -1px; border-radius: 5px 5px 0 0; }
.tab-button-v4.active { color: #CC0000; border-color: #dee2e6; background-color: #fff; }

/* --- Contenedor de Filtros y Píldoras --- */
.filters-and-pills-container { background-color: #f7f7f7; margin-top: 25px; }
.filter-bar-v2 { margin-top: 0 !important; }

/* --- Píldoras de Competición --- */
.competition-pills-container { display: flex; flex-wrap: wrap; gap: 8px; padding: 0 15px 15px 15px; }
.competition-pill { background-color: #e9ecef; color: #495057; border: 1px solid #dee2e6; border-radius: 15px; padding: 6px 14px; font-size: 0.85em; font-weight: 500; cursor: pointer; transition: all 0.2s ease-in-out; }
.competition-pill:hover { background-color: #ced4da; }
.competition-pill.active { background-color: #8B0000; color: #fff; border-color: #8B0000; }

/* --- Tabla de Plantilla (Roster) --- */
.player-roster-table { width: 100%; border-collapse: collapse; font-size: 0.9em; margin-top: 20px; }
.player-roster-table th { padding: 10px; border-bottom: 2px solid #dee2e6; text-align: left; color: #6c757d; font-size: 0.8em; text-transform: uppercase; }
.player-roster-table th.sortable { cursor: pointer; }
.player-roster-table th.sortable:hover { color: #212529; }
.player-roster-table th.sort-asc::after, .player-roster-table th.sort-desc::after { content: ''; display: inline-block; width: 0; height: 0; border-left: 4px solid transparent; border-right: 4px solid transparent; margin-left: 5px; }
.player-roster-table th.sort-asc::after { border-bottom: 4px solid #212529; }
.player-roster-table th.sort-desc::after { border-top: 4px solid #212529; }
.player-roster-table td { padding: 12px 10px; border-bottom: 1px solid #e9ecef; }
.player-roster-table tbody tr { cursor: pointer; transition: background-color 0.2s ease-in-out; }
.player-roster-table tbody tr:hover { background-color: #f8f9fa; }
.player-roster-table tr:last-child td { border-bottom: none; }
.player-info-cell .player-name { color: #CC0000; font-weight: bold; }
.player-info-cell .player-position { font-size: 0.9em; color: #6c757d; }

/* --- Tabla de Calendario (Schedule) --- */
.schedule-match-table { width: 100%; border-collapse: collapse; font-size: 0.9em; margin-top: 20px; }
.schedule-match-table th { padding: 10px; border-bottom: 2px solid #dee2e6; color: #6c757d; font-size: 0.8em; text-transform: uppercase; text-align: left;}
.schedule-match-table td { padding: 12px 10px; border-bottom: 1px solid #e9ecef; vertical-align: middle; }
.schedule-match-table tbody tr { cursor: pointer; transition: background-color 0.2s ease-in-out; }
.schedule-match-table tbody tr:hover { background-color: #f8f9fa; }
.schedule-match-table tr:last-child td { border-bottom: none; }
.schedule-match-table .th-match { text-align: center; }
.schedule-match-table .td-home { text-align: right; }
.schedule-match-table .td-away { text-align: left; }
.schedule-match-table .td-score { text-align: center; width: 80px; }
.schedule-team-in-table { display: flex; align-items: center; gap: 8px; font-weight: 500; }
.td-home .schedule-team-in-table { justify-content: flex-end; }
.td-away .schedule-team-in-table { justify-content: flex-start; }
.schedule-score-in-table { display: inline-block; font-size: 1em; font-weight: 700; background-color: #f1f2f3; padding: 5px 10px; border-radius: 4px; white-space: nowrap; }

)"


writeLines(estilo_css, file.path(RUTA_ASSETS_COMPARTIDOS, "style.css"))


### 12.2. Save Functionality Script (script.js)
script_js <- r"(
let searchData = [];

document.addEventListener('DOMContentLoaded', function() {
  // INICIALIZADORES GLOBALES (PARA TODO EL SITIO)
  initializeSearch();
  initializeMobileMenu();
  initializePlayerProfileInteractions();
  
  // INICIALIZADOR ESPECÍFICO PARA LA PÁGINA DE EQUIPO
  // Se ejecuta solo si encuentra el contenedor de datos de la página de equipo.
  if (document.getElementById('team-page-data')) {
    initializeTeamProfilePage();
  }
});

function initializeMobileMenu() {
  const hamburger = document.getElementById('hamburger-icon');
  const nav = document.querySelector('.navbar');
  const closeBtn = document.getElementById('close-nav-btn');
  const dropdownBtn = document.querySelector('.dropdown .dropbtn');

  if (hamburger && nav && closeBtn) {
    hamburger.addEventListener('click', () => { nav.classList.add('mobile-active'); });
    closeBtn.addEventListener('click', () => { nav.classList.remove('mobile-active'); });
  }

  if (dropdownBtn) {
    dropdownBtn.addEventListener('click', (event) => {
      if (window.innerWidth <= 850) { // <-- Corregido para que coincida con el CSS
        event.preventDefault();
        const parentDropdown = dropdownBtn.parentElement;
        parentDropdown.classList.toggle('open');
      }
    });
  }
}

// --- FUNCIONES PARA PERFIL DE JUGADORA ---
function initializePlayerProfileInteractions() {
    const accordions = document.querySelectorAll('.season-accordion');
    if (!accordions.length) return;

    accordions.forEach(accordion => {
        const header = accordion.querySelector('.season-header');
        header.addEventListener('click', () => {
            accordion.classList.toggle('active');
            const content = accordion.querySelector('.season-content');
            content.style.display = accordion.classList.contains('active') ? 'block' : 'none';
        });
    });
    
    document.querySelectorAll('.tab-button').forEach(button => {
        button.addEventListener('click', (event) => {
            const clickedButton = event.currentTarget;
            const seasonId = clickedButton.dataset.seasonId;
            const tabTarget = clickedButton.dataset.tabTarget;
            switchTab(seasonId, tabTarget, clickedButton);
        });
    });

    document.querySelectorAll('.sub-tab-button').forEach(button => {
        button.addEventListener('click', (event) => {
            const clickedButton = event.currentTarget;
            const seasonId = clickedButton.dataset.seasonId;
            const subtabTarget = clickedButton.dataset.subtabTarget;
            switchSubTab(seasonId, subtabTarget, clickedButton);
        });
    });
}

function switchTab(seasonId, tabToShow, clickedButton) {
    const seasonContent = clickedButton.closest('.season-content');
    if (!seasonContent) return;
    seasonContent.querySelectorAll('.tab-panel').forEach(panel => panel.classList.remove('active'));
    seasonContent.querySelectorAll('.tab-button').forEach(button => button.classList.remove('active'));
    const panelToShow = seasonContent.querySelector(`#${tabToShow}-${seasonId}`);
    if (panelToShow) panelToShow.classList.add('active');
    clickedButton.classList.add('active');
}

function switchSubTab(seasonId, competitionId, clickedButton) {
    const parentTabPanel = clickedButton.closest('.tab-panel');
    if (!parentTabPanel) return;
    parentTabPanel.querySelectorAll('.sub-tab-panel').forEach(panel => panel.classList.remove('active'));
    parentTabPanel.querySelectorAll('.sub-tab-button').forEach(button => button.classList.remove('active'));
    const panelToShow = parentTabPanel.querySelector(`#matches-${seasonId}-${competitionId}`);
    if (panelToShow) panelToShow.classList.add('active');
    clickedButton.classList.add('active');
}

// --- FUNCIONES DE BÚSQUEDA Y UTILIDADES ---
function getSiteBasePath() {
  const path = window.location.pathname;
  const match = path.match(/^(.*\/)(mk|sq|es|en)\//);
  if (match && match[1]) { return match[1]; }
  return "/";
}

function getCurrentLanguageFromPath() {
  const path = window.location.pathname;
  const match = path.match(/\/(mk|sq|es|en)\//);
  if (match && match[1]) { return match[1]; }
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

  fetch(jsonUrl).then(response => response.json()).then(data => {
      searchData = data;
      if(searchInput) searchInput.disabled = false;
  }).catch(error => console.error('Error loading search data:', error));
  
  document.addEventListener('click', function(event) {
      const searchContainer = document.querySelector('.search-container');
      if (searchContainer && !searchContainer.contains(event.target)) {
        const suggestions = document.getElementById('search-suggestions');
        if(suggestions) suggestions.style.display = 'none';
      }
        const clickableRow = event.target.closest('.clickable-row, .player-roster-table tbody tr');
      if (clickableRow && clickableRow.dataset.href) { 
          if (event.target.closest('a')) return;
          window.location.href = clickableRow.dataset.href; 
      } else if (clickableRow && clickableRow.onclick) {
          if (event.target.closest('a')) return;
          clickableRow.onclick();
      }
  });
}

function generateLink(target_id) {
    const basePath = "."; // Rutas relativas desde la página actual
    const parts = target_id.split('-');
    const type = parts[0];
    const id = parts.slice(1).join('-');

    // Las carpetas deben coincidir con tus `nombres_carpetas_relativos`
    const paths = {
        'jugadora': 'igraci',
        'equipo': 'timovi',
        'arbitro': 'sudii',
        'стадион': 'stadioni', // Mantener el nombre interno consistente
        'menu-competicion': 'natprevaruvanja'
    };

    if (paths[type]) {
        return `${basePath}/${paths[type]}/${id}.html`;
    }
    return '#'; // Fallback
}

function handleSearchInput(event) {
    const input = event.target.value.toLowerCase().trim();
    const suggestionsContainer = document.getElementById('search-suggestions');
    
    if (event.key === 'Enter') {
        event.preventDefault();
        showSearchResults();
        return;
    }

    if (input.length < 2) {
        suggestionsContainer.style.display = 'none';
        return;
    }

    const matches = searchData.filter(item => 
        item.search_terms.toLowerCase().includes(input)
    ).slice(0, 10); // Limitar a 10 sugerencias

    if (matches.length > 0) {
        suggestionsContainer.innerHTML = '';
        matches.forEach(item => {
            const link = document.createElement('a');
            link.href = generateLink(item.target_id);
            
            // Resaltar la coincidencia
            const regex = new RegExp(`(${input.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&')})`, 'gi');
            const highlightedName = item.Име.replace(regex, '<strong>$1</strong>');
            
            link.innerHTML = `${highlightedName} <span class="search-result-type">(${item.Тип})</span>`;
            suggestionsContainer.appendChild(link);
        });
        suggestionsContainer.style.display = 'block';
    } else {
        suggestionsContainer.style.display = 'none';
    }
}

function showSearchResults() {
    const input = document.getElementById('search-input').value.toLowerCase().trim();
    const suggestionsContainer = document.getElementById('search-suggestions');
    const mainContent = document.getElementById('main-content');
    const body = document.body;

    suggestionsContainer.style.display = 'none';

    if (input.length < 2) {
        mainContent.innerHTML = `<h2>${body.dataset.searchPromptMsg || 'Please enter at least 2 characters'}</h2>`;
        return;
    }

    const matches = searchData.filter(item => 
        item.search_terms.toLowerCase().includes(input)
    );

    let resultsHtml = `<h2>${body.dataset.searchResultsTitle || 'Search results for'}: "${input}"</h2>`;

    if (matches.length > 0) {
        resultsHtml += '<div id="search-results-list"><ul>';
        matches.forEach(item => {
            resultsHtml += `
                <li>
                    <a href="${generateLink(item.target_id)}">
                        ${item.Име}
                    </a>
                    <span class="search-result-type">(${item.Тип})</span>
                </li>`;
        });
        resultsHtml += '</ul></div>';
    } else {
        resultsHtml += `<p>${body.dataset.noSearchResultsMsg || 'No results found.'}</p>`;
    }

    mainContent.innerHTML = resultsHtml;
}

function sortTable(tableId, columnIndex) { /* ... (código de búsqueda sin cambios) ... */ }

function showLetter(letter) {
  // 1. Ocultar todos los grupos de letras
  const letterGroups = document.querySelectorAll('.letter-group');
  letterGroups.forEach(group => {
    group.classList.remove('active');
  });

  // 2. Mostrar solo el grupo de la letra seleccionada
  const activeGroup = document.getElementById('group-' + letter);
  if (activeGroup) {
    activeGroup.classList.add('active');
  }

  // 3. Actualizar el estado visual de los botones de las letras
  const letterNavLinks = document.querySelectorAll('.letter-nav a');
  letterNavLinks.forEach(link => {
    link.classList.remove('active');
    if (link.getAttribute('data-letter') === letter) {
      link.classList.add('active');
    }
  });
}

function toggleDetails(elementId) { /* ... (código de búsqueda sin cambios) ... */ }


// ==============================================================================
// == INICIO: FUNCIONES PARA LA PÁGINA DE PERFIL DE EQUIPO (COPIA LITERAL)      ==
// ==============================================================================
function initializeTeamProfilePage() {
  const dataEl = document.getElementById('team-page-data');
  if (!dataEl) return;
  const pageData = JSON.parse(dataEl.textContent);
  const t = pageData.translations;

  let rosterSortColumn = 'dorsal_principal';
  let rosterSortDirection = 'asc';

  const rosterSeason = document.getElementById('roster-season-filter');
  const rosterCategory = document.getElementById('roster-category-filter');
  const rosterPillsContainer = document.getElementById('roster-competition-pills');
  const rosterTableContainer = document.getElementById('roster-table-container');
  
  const scheduleSeason = document.getElementById('schedule-season-filter');
  const scheduleCategory = document.getElementById('schedule-category-filter');
  const schedulePillsContainer = document.getElementById('schedule-competition-pills');
  const scheduleTableContainer = document.getElementById('schedule-table-container');

  document.querySelectorAll('.tab-button-v4').forEach(button => {
    button.addEventListener('click', (event) => {
      const targetPanelId = event.currentTarget.dataset.tabTarget;
      document.querySelectorAll('.tab-panel').forEach(p => p.classList.remove('active'));
      document.getElementById(targetPanelId).classList.add('active');
      document.querySelectorAll('.tab-button-v4').forEach(b => b.classList.remove('active'));
      event.currentTarget.classList.add('active');
    });
  });

  function updateRosterCategoryFilter() {
    const season = rosterSeason.value;
    const categories = [...new Set(pageData.roster_data.filter(p => p.competicion_temporada === season).map(p => JSON.stringify({ key: p.category_key })))].map(s => JSON.parse(s));
    const order = { "category_senior": 1, "category_youth": 2, "category_cadet": 3 };
    categories.sort((a, b) => (order[a.key] || 99) - (order[b.key] || 99));
    rosterCategory.innerHTML = '';
    categories.forEach(c => { rosterCategory.innerHTML += `<option value="${c.key}">${t[c.key] || c.key}</option>`; });
    updateRosterCompetitionFilter();
  }

  function updateRosterCompetitionFilter() {
    rosterPillsContainer.innerHTML = '';
    if(!rosterSeason.value || !rosterCategory.value) { updateRosterView(); return; }
    const season = rosterSeason.value; 
    const category = rosterCategory.value;
    const competitions = [...new Set(pageData.roster_data.filter(p => p.competicion_temporada === season && p.category_key === category).map(p => JSON.stringify({id: p.competicion_id, name: p.CompeticionLang || p.competicion_nombre})))].map(s => JSON.parse(s));
    competitions.forEach(c => { rosterPillsContainer.innerHTML += `<button class="competition-pill" data-competition-id="${c.id}">${c.name}</button>`; });
    const firstPill = rosterPillsContainer.querySelector('.competition-pill');
    if (firstPill) { firstPill.classList.add('active'); }
    updateRosterView();
  }

  function updateRosterView() {
    const activePill = rosterPillsContainer.querySelector('.competition-pill.active');
    if (!activePill) { rosterTableContainer.innerHTML = `<p style="text-align:center; padding: 20px;">${t.no_players_found}</p>`; return; }
    const season = rosterSeason.value;
    const category = rosterCategory.value;
    const competition = activePill.dataset.competitionId;
    let filteredData = pageData.roster_data.filter(p => p.competicion_temporada === season && p.category_key === category && p.competicion_id === competition);
    if (filteredData.length === 0) { rosterTableContainer.innerHTML = `<p style="text-align:center; padding: 20px;">${t.no_players_found}</p>`; return; }
    const direction = rosterSortDirection === 'asc' ? 1 : -1;
    filteredData.sort((a, b) => {
      let valA, valB;
      if (rosterSortColumn === 'dorsal_principal') { valA = parseInt(a.dorsal_principal) || 999; valB = parseInt(b.dorsal_principal) || 999;
      } else if (rosterSortColumn === 'PlayerName') { return a.PlayerName.localeCompare(b.PlayerName) * direction;
      } else { valA = parseInt(a[rosterSortColumn]) || 0; valB = parseInt(b[rosterSortColumn]) || 0; }
      return (valA - valB) * direction;
    });
    let html = `<table class="player-roster-table"><thead><tr> <th class="sortable" data-sort-key="dorsal_principal" style="width:5%;">${t.col_dorsal}</th> <th class="sortable" data-sort-key="PlayerName">${t.col_player}</th> <th class="sortable" data-sort-key="Played" style="width:8%;">${t.col_apps}</th> <th class="sortable" data-sort-key="Minutes" style="width:8%;">${t.col_mins}</th> <th class="sortable" data-sort-key="Goals" style="width:8%;">${t.col_goals}</th> <th class="sortable" data-sort-key="Yellows" style="width:12%;">${t.col_cards}</th> </tr></thead><tbody>`;
    filteredData.forEach(player => {
      const position = t[player.posicion_final_unificada] || '';
      const playerURL = `../igraci/${player.id}.html`;
      html += `<tr onclick="window.location.href='${playerURL}'"> <td>${player.dorsal_principal || '-'}</td> <td><div class="player-info-cell"><div class="player-name">${player.PlayerName}</div><div class="player-position">${position}</div></div></td> <td style="text-align:center;">${player.Played || 0}</td> <td style="text-align:center;">${player.Minutes || 0}</td> <td style="text-align:center;">${player.Goals || 0}</td> <td style="text-align:center;"><span class="card-icon-table yellow"></span> ${player.Yellows || 0}&nbsp;/&nbsp;<span class="card-icon-table red"></span> ${player.Reds || 0}</td> </tr>`;
    });
    html += '</tbody></table>';
    rosterTableContainer.innerHTML = html;
    const currentHeader = rosterTableContainer.querySelector(`[data-sort-key="${rosterSortColumn}"]`);
    if (currentHeader) { currentHeader.classList.add(rosterSortDirection === 'asc' ? 'sort-asc' : 'sort-desc'); }
  }

  function updateScheduleCategoryFilter() {
    const season = scheduleSeason.value;
    const categories = [...new Set(pageData.matches_data.filter(m => m.competicion_temporada === season).map(m => JSON.stringify({key: m.category_key, name: m.category_name})))].map(s => JSON.parse(s));
    const order = { "category_senior": 1, "category_youth": 2, "category_cadet": 3 };
    categories.sort((a, b) => (order[a.key] || 99) - (order[b.key] || 99));
    scheduleCategory.innerHTML = '';
    categories.forEach(c => { scheduleCategory.innerHTML += `<option value="${c.key}">${c.name}</option>`; });
    updateScheduleCompetitionFilter();
  }
  
  function updateScheduleCompetitionFilter() {
      schedulePillsContainer.innerHTML = '';
      if(!scheduleSeason.value || !scheduleCategory.value) { updateScheduleView(); return; }
      const season = scheduleSeason.value; 
      const category = scheduleCategory.value;
      const competitions = [...new Set(pageData.matches_data.filter(m => m.competicion_temporada === season && m.category_key === category).map(m => JSON.stringify({id: m.competicion_id, name: m.CompeticionLang})))].map(s => JSON.parse(s));
      competitions.forEach(c => { schedulePillsContainer.innerHTML += `<button class="competition-pill" data-competition-id="${c.id}">${c.name}</button>`; });
      const firstPill = schedulePillsContainer.querySelector('.competition-pill');
      if (firstPill) { firstPill.classList.add('active'); }
      updateScheduleView();
  }

  function updateScheduleView() {
    const activePill = schedulePillsContainer.querySelector('.competition-pill.active');
    if(!activePill) { scheduleTableContainer.innerHTML = `<p style="text-align:center; padding: 20px;">${t.no_matches_found}</p>`; return; }
    const season = scheduleSeason.value; 
    const category = scheduleCategory.value; 
    const competition = activePill.dataset.competitionId;
    let filteredMatches = pageData.matches_data.filter(m => m.competicion_temporada === season && m.category_key === category && m.competicion_id === competition);
    if (filteredMatches.length === 0) { scheduleTableContainer.innerHTML = `<p style="text-align:center; padding: 20px;">${t.no_matches_found}</p>`; return; }
    filteredMatches.sort((a, b) => new Date(b.fecha.split('.').reverse().join('-')) - new Date(a.fecha.split('.').reverse().join('-')));
    let html = `<div class="match-list-container">
    <div class="match-list-header">
        <div>${t.col_date}</div>
        <div>${t.col_match}</div>
        <div>${t.col_competition || 'Competition'}</div>
    </div>`;
filteredMatches.forEach(match => {
    const matchURL = `../natprevari/${match.id_partido}.html`;
    html += `<div class="match-list-row" onclick="window.location.href='${matchURL}'">
    <div class="cell-date">${match.fecha}</div>
        <div class="cell-match">
            <span class="team-home">${match.local_lang} <img src="${match.home_logo_url}" class="team-logo-small"></span>
            <span class="match-score">${match.goles_local} : ${match.goles_visitante}</span>
            <span class="team-away"><img src="${match.away_logo_url}" class="team-logo-small"> ${match.visitante_lang}</span>
        </div>
        <div class="cell-competition">${match.CompeticionLang}</div>
    </div>`;
});
html += '</div>';
scheduleTableContainer.innerHTML = html;
  }

  rosterSeason.addEventListener('change', updateRosterCategoryFilter);
  rosterCategory.addEventListener('change', updateRosterCompetitionFilter);
  rosterPillsContainer.addEventListener('click', function(e) { if (e.target.classList.contains('competition-pill')) { rosterPillsContainer.querySelectorAll('.competition-pill').forEach(pill => pill.classList.remove('active')); e.target.classList.add('active'); updateRosterView(); } });
  rosterTableContainer.addEventListener('click', function(e){ const header = e.target.closest('th.sortable'); if(!header) return; const sortKey = header.dataset.sortKey; if(rosterSortColumn === sortKey){ rosterSortDirection = rosterSortDirection === 'asc' ? 'desc' : 'asc'; } else { rosterSortColumn = sortKey; rosterSortDirection = 'asc'; } updateRosterView(); });
  scheduleSeason.addEventListener('change', updateScheduleCategoryFilter);
  scheduleCategory.addEventListener('change', updateScheduleCompetitionFilter);
  schedulePillsContainer.addEventListener('click', function(e) { if (e.target.classList.contains('competition-pill')) { schedulePillsContainer.querySelectorAll('.competition-pill').forEach(pill => pill.classList.remove('active')); e.target.classList.add('active'); updateScheduleView(); } });

  if(rosterSeason && rosterSeason.options.length > 0) updateRosterCategoryFilter();
  if(scheduleSeason && scheduleSeason.options.length > 0) updateScheduleCategoryFilter();
}
)"


writeLines(script_js, file.path(RUTA_ASSETS_COMPARTIDOS, "script.js"))
message("style.css and script.js files saved to the assets folder.")


# ============================================================================ #
# ==                INTERRUPTORES DE CONTROL DE GENERACIÓN                  ==
# ============================================================================ #
# Cambia estos valores a TRUE o FALSE para controlar qué partes del sitio se
# regeneran. Esto es útil para hacer pruebas rápidas en una sección.
# Para una construcción completa, todos deben estar en TRUE.

GENERAR_PAGINAS_ESTATICAS <- TRUE   # Incluye: Inicio, Archivo, Lista de Equipos/Jugadoras, Acerca de
GENERAR_PAGINAS_COMPETICION <- TRUE   # Todas las páginas de competiciones (menús y tablas)
GENERAR_PERFILES_PARTIDO <- TRUE      # Perfiles individuales para cada partido
GENERAR_PERFILES_JUGADORA <- TRUE     # Perfiles individuales para cada jugadora
GENERAR_PERFILES_EQUIPO <- TRUE       # Perfiles individuales para cada equipo
GENERAR_PERFILES_ARBITRO <- TRUE      # Perfiles individuales para cada árbitro
GENERAR_PERFILES_ESTADIO <- TRUE      # Perfiles individuales para cada estadio

# ============================================================================ #


#### 13. HTML PAGE GENERATION (OPTIMIZED ARCHITECTURE) ####

if (hubo_cambios) {
  
  # ============================================================================ #
  # ==      MAIN GENERATION LOOP: Iterates over each language and builds the site      ==
  # ============================================================================ #
  
  for (lang in IDIOMAS_SOPORTADOS) {
    
    idioma_actual <<- lang
    nombres_archivos_traducidos <- nombres_archivos_mk
    
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
    
    # --- 1. Jugadoras ---
    # Obtenemos tanto el nombre a mostrar en el idioma actual como el nombre latino "oficial" si existe.
    search_jugadoras_data_lang <- jugadoras_stats_df %>% 
      select(id, 
             DisplayName = !!sym(player_name_col), 
             LatinName = PlayerName_en, # Usamos 'en' como el nombre latino de referencia
             CyrillicName = PlayerName_mk)
    
    search_jugadoras <- search_jugadoras_data_lang %>%
      mutate(
        Тип = t("player_type"),
        target_id = paste0("jugadora-", id),
        search_terms = paste(
          tolower(DisplayName),
          tolower(LatinName),
          sapply(CyrillicName, generar_terminos_busqueda, USE.NAMES = FALSE)
        )
      ) %>%
      select(Име = DisplayName, Тип, target_id, search_terms)
    
    # --- 2. Entidades (Equipos, Árbitros, Estadios) ---
    # Hacemos lo mismo para las entidades
    entidades_maestro_lang_df <- entidades_maestro_df %>%
      select(original_name,
             current_lang_name = !!sym(entity_name_col),
             latin_name = translated_name_en) # 'en' como referencia latina
    
    # Función auxiliar para evitar repetir código
    generar_search_entidad <- function(df, nombres_filtro, tipo_entidad, id_prefix) {
      df %>%
        filter(original_name %in% nombres_filtro) %>%
        mutate(
          Тип = t(tipo_entidad),
          target_id = paste0(id_prefix, generar_id_seguro(original_name)),
          search_terms = paste(
            tolower(current_lang_name),
            tolower(latin_name),
            sapply(original_name, generar_terminos_busqueda, USE.NAMES = FALSE)
          )
        ) %>%
        select(Име = current_lang_name, Тип, target_id, search_terms)
    }
    
    search_equipos <- generar_search_entidad(entidades_maestro_lang_df, nombres_equipos, "team_type", "equipo-")
    search_arbitros <- generar_search_entidad(entidades_maestro_lang_df, nombres_arbitros, "referee_type", "arbitro-")
    search_estadios <- generar_search_entidad(entidades_maestro_lang_df, nombres_estadios, "stadium_type", "стадион-")
    
    # --- 3. Competiciones ---
    # Las competiciones son un caso especial, pero aplicamos la misma lógica
    search_competiciones <- competiciones_unicas_df %>% 
      mutate(
        Име = !!sym(comp_name_col),
        LatinName = !!sym("nombre_completo_en"), # 'en' como referencia latina
        Тип = t("competition_type"), 
        target_id = paste0("menu-competicion-", competicion_id), 
        search_terms = paste(
          tolower(Име),
          tolower(LatinName),
          sapply(nombre_completo_mk, generar_terminos_busqueda, USE.NAMES = FALSE)
        )
      ) %>% 
      select(Име, Тип, target_id, search_terms)
    
    # --- 4. Unir todo ---
    search_index_df_lang <- bind_rows(search_jugadoras, search_equipos, search_arbitros, search_competiciones, search_estadios) %>% 
      # Limpieza final para eliminar duplicados y espacios extra
      mutate(search_terms = sapply(str_split(search_terms, "\\s+"), function(x) paste(unique(x), collapse = " "))) %>%
      arrange(Име)
    
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
            filter(competicion_nombre == nombre_comp_mk, competicion_temporada == temporada_actual_str, jornada == ultima_jornada_jugada)
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
        comp_nombre_lang = comp_nombre_actual_lang 
      )
      
      list(resultados = bloque_resultados_html, clasificacion = tabla_clasificacion_html)
    }) %>% purrr::compact()
    
    contenido_portal_dinamico <- tagList(
      tags$h3(class="main-content-title", t("latest_results_title")),
      map(lista_componentes_html, "resultados"),
      
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
    
    # 13.1.2.bis. Generate new static and list pages
    message("   > Generating new list and static pages...")
    
    # --- GENERATE COMPETITION ARCHIVE PAGE ---
    path_to_archive_page <- file.path(RUTA_SALIDA_RAIZ, lang, "arhiva.html")
    message("     > Generating: ", basename(path_to_archive_page))
    
    competiciones_archivo <- competiciones_unicas_df %>%
      filter(competicion_id != "reprezentacija") %>%
      mutate(temporada_num = as.numeric(str_extract(competicion_temporada, "^\\d{2}"))) %>%
      arrange(desc(temporada_num)) %>%
      group_by(competicion_temporada)
    
    contenido_archivo <- tagList(
      crear_botones_navegacion(path_to_lang_root = "."),
      tags$h2(t("archive_page_title")),
      map(group_split(competiciones_archivo), function(df_temporada) {
        temporada <- df_temporada$competicion_temporada[1]
        tagList(
          tags$h3(paste(t("player_season"), temporada)),
          tags$ul(
            map(1:nrow(df_temporada), function(i) {
              comp <- df_temporada[i,]
              tags$li(
                tags$a(
                  href = file.path(nombres_carpetas_relativos$competiciones, paste0(comp$competicion_id, ".html")),
                  comp[[comp_name_col]]
                )
              )
            })
          )
        )
      })
    )
    save_html(crear_pagina_html(contenido_archivo, t("archive_page_title"), "..", script_contraseña_lang, "competitions"), file = path_to_archive_page)
    
    
    # --- GENERATE TEAMS LIST PAGE ---
    path_to_teams_page <- file.path(RUTA_SALIDA_RAIZ, lang, "klubovi.html")
    message("     > Generating: ", basename(path_to_teams_page))
    
    # CORRECCIÓN: Se añade rowwise() para aplicar la función get_national_team_iso()
    # a cada fila individualmente, evitando el error de vectorización.
    # También se simplifica la lógica del filtro.
    equipos_para_listar <- entidades_df_lang %>%
      filter(original_name %in% nombres_equipos) %>%
      rowwise() %>%
      filter(is.na(get_national_team_iso(original_name))) %>%
      ungroup() %>%
      arrange(current_lang_name)
    
    contenido_equipos <- tagList(
      crear_botones_navegacion(path_to_lang_root = "."),
      tags$h2(t("teams_list_title")),
      tags$div(
        class = "team-list",
        tags$ul(
          map(1:nrow(equipos_para_listar), function(i) {
            equipo <- equipos_para_listar[i,]
            tags$li(
              tags$a(
                href = file.path(nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(equipo$original_name), ".html")),
                equipo$current_lang_name
              )
            )
          })
        )
      )
    )
    save_html(crear_pagina_html(contenido_equipos, t("teams_list_title"), "..", script_contraseña_lang, "teams"), file = path_to_teams_page)
    
    
    # --- GENERATE PLAYERS LIST PAGE ---
    path_to_players_page <- file.path(RUTA_SALIDA_RAIZ, lang, "fudbalerki.html")
    message("     > Generating: ", basename(path_to_players_page))
    
    jugadoras_para_listar <- jugadoras_stats_df %>%
      filter(!str_starts(id, "player_gen_")) %>%
      filter(!is.na(!!sym(player_name_col)) & trimws(!!sym(player_name_col)) != "") %>%
      mutate(
        apellido = sapply(!!sym(player_name_col), extraer_apellido),
        letra_inicial = toupper(substr(apellido, 1, 1))
      )
    
    if (lang == 'mk') {
      # Special sorting for Macedonian alphabet
      jugadoras_para_listar <- jugadoras_para_listar %>% arrange(apellido, .locale = "mk")
      alfabeto_df <- jugadoras_para_listar %>% 
        distinct(letra_inicial) %>% 
        arrange(letra_inicial, .locale = "mk")
      alfabeto <- alfabeto_df$letra_inicial
    } else {
      jugadoras_para_listar <- jugadoras_para_listar %>% arrange(apellido)
      alfabeto <- sort(unique(jugadoras_para_listar$letra_inicial))
      alfabeto <- alfabeto[grepl("^[\\p{L}]$", alfabeto, perl = TRUE)]
    }
    
    grupos_por_letra <- split(jugadoras_para_listar, jugadoras_para_listar$letra_inicial)
    
    contenido_jugadoras <- tagList(
      crear_botones_navegacion(path_to_lang_root = "."),
      tags$h2(t("players_list_title")),
      tags$p(t("players_list_select_letter")),
      tags$div(
        class = "letter-nav",
        map(alfabeto, function(letra) {
          tags$a(
            href = "javascript:void(0);",
            onclick = paste0("showLetter('", letra, "')"),
            `data-letter` = letra,
            class = if(letra == alfabeto[1]) "active" else "",
            letra
          )
        })
      ),
      tags$div(
        id = "player-list-container",
        class = "player-list",
        map(alfabeto, function(letra) {
          grupo <- grupos_por_letra[[letra]]
          
          if (!is.null(grupo) && nrow(grupo) > 0) {
            
            tags$div(
              class = if(letra == alfabeto[1]) "letter-group active" else "letter-group",
              id = paste0("group-", letra),
              tags$h3(letra),
              tags$ul(
                map(1:nrow(grupo), function(i) {
                  jugadora <- grupo[i,]
                  tags$li(
                    tags$a(
                      href = file.path(nombres_carpetas_relativos$jugadoras, paste0(jugadora$id, ".html")),
                      jugadora[[player_name_col]]
                    )
                  )
                })
              )
            )
          } else {
            NULL
          }
        })
      )
    )
    save_html(crear_pagina_html(contenido_jugadoras, t("players_list_title"), "..", script_contraseña_lang, "players"), file = path_to_players_page)
    
    
    # --- GENERATE ABOUT PAGE ---
    path_to_about_page <- file.path(RUTA_SALIDA_RAIZ, lang, "za-zfudbalmk.html")
    message("     > Generating: ", basename(path_to_about_page))
    contenido_about <- tagList(
      crear_botones_navegacion(path_to_lang_root = "."),
      tags$h2(t("about_page_title")),
      tags$div(
        class = "about-content",
        HTML(t("about_page_content"))
      )
    )
    save_html(crear_pagina_html(contenido_about, t("about_page_title"), "..", script_contraseña_lang, "about"), file = path_to_about_page)
    
    
    # 13.1.3. Generate Competition Pages.
    if (GENERAR_PAGINAS_COMPETICION) {
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
          crear_pagina_html(contenido_menu_seleccion, comp_nombre_current_lang, "../..", script_contraseña_lang, current_page_id = "national_team"),
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
            filter(!is.na(jornada)) %>% # CORRECCIÓN: Añadido para evitar NAs.
            mutate(order_key = case_when(
              str_detect(jornada, "1/64") ~ 1,
              str_detect(jornada, "1/32") ~ 2,
              str_detect(jornada, "1/16") ~ 3,
              str_detect(jornada, "1/8") ~ 4,
              str_detect(jornada, "1/4") ~ 5,
              str_detect(jornada, "1/2") ~ 6,
              str_detect(jornada, "3/4") ~ 6.5,
              str_detect(jornada, "Ф$|ф$|финале") ~ 7,
              str_detect(jornada, "^\\d+$") ~ as.numeric(jornada),
              TRUE ~ 99 
            )) %>% 
            arrange(order_key) %>% 
            pull(jornada) 
        } else { c() }    
        contenido_partidos <- tagList(crear_botones_navegacion(path_to_lang_root = ".."), tags$h2(paste(t("schedule_title"), "-", comp_nombre_current_lang)), map(jornadas_comp, function(j) { partidos_jornada <- partidos_comp %>% filter(jornada == j) %>% arrange(local); header_text <- if(is_cup || is_friendly_comp) as.character(j) else paste(t("round_prefix"), j); get_logo_tag <- function(nombre_equipo_mk) { nombre_archivo_final <- paste0(generar_id_seguro(nombre_equipo_mk), ".png"); if (!file.exists(file.path(RUTA_LOGOS_DESTINO, nombre_archivo_final))) { nombre_archivo_final <- "NOLOGO.png" }; ruta_relativa_logo_html <- file.path("..", "..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, nombre_archivo_final); tags$img(class="team-logo", src = ruta_relativa_logo_html, alt = nombre_equipo_mk) }; tagList(tags$h3(class="jornada-header", header_text), map(1:nrow(partidos_jornada), function(k) { partido <- partidos_jornada[k,]; is_placeholder_match <- is.na(partido$id_partido); local_name <- entidades_df_lang$current_lang_name[match(partido$local, entidades_df_lang$original_name)]; visitante_name <- entidades_df_lang$current_lang_name[match(partido$visitante, entidades_df_lang$original_name)]; resultado_texto <- if (is_placeholder_match) " - " else { res_base <- paste(partido$goles_local, "-", partido$goles_visitante); if (!is.na(partido$penales_local)) res_base <- sprintf("%s (%s - %s)", res_base, partido$penales_local, partido$penales_visitante); if (isTRUE(partido$es_resultado_oficial)) res_base <- paste(res_base, "*"); res_base }; contenido_comun <- tagList(tags$span(class="equipo equipo-local", get_logo_tag(partido$local), tags$span(local_name)), tags$span(class="resultado", resultado_texto), tags$span(class="equipo equipo-visitante", tags$span(visitante_name), get_logo_tag(partido$visitante))); if (is_placeholder_match) tags$div(class = "partido-link-placeholder", contenido_comun) else tags$a(class = "partido-link", href = file.path("..", nombres_carpetas_relativos$partidos, paste0(partido$id_partido, ".html")), contenido_comun) })) }))
        nombre_archivo_partidos <- paste0(comp_id, "_", nombres_archivos_traducidos$partidos, ".html"); save_html(crear_pagina_html(contenido_partidos, paste(t("schedule_title"), "-", comp_nombre_current_lang), "../..", script_contraseña_lang), file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, nombre_archivo_partidos))
        lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href=nombre_archivo_partidos, class="menu-button", t("schedule_title"))
        if (!is_placeholder_only_comp) {
          tabla_goleadoras_comp <- stats_goleadoras_por_comp_df %>% filter(competicion_id == comp_id) %>% left_join(jugadoras_stats_df %>% select(id, !!player_name_col_sym), by = "id") %>% filter(!is.na(!!player_name_col_sym)) %>% select(Pos, id, PlayerName = !!player_name_col_sym, TeamNames_mk, Goals); headers_traducidos <- c(t("standings_pos"), t("player_type"), t("team_type"), t("stats_goals")); contenido_goleadoras <- tagList(crear_botones_navegacion(path_to_lang_root = ".."), tags$h2(paste(t("scorers_title"), "-", comp_nombre_current_lang)), tags$table(tags$thead(tags$tr(map(headers_traducidos, tags$th))), tags$tbody(map(1:nrow(tabla_goleadoras_comp), function(j){ g <- tabla_goleadoras_comp[j,]; tags$tr(tags$td(g$Pos), tags$td(tags$a(href=file.path("..", nombres_carpetas_relativos$jugadoras, paste0(g$id, ".html")), g$PlayerName)), tags$td({ teams_mk <- str_split(g$TeamNames_mk, " / ")[[1]]; team_tags <- list(); for (i in seq_along(teams_mk)) { team_name_mk <- teams_mk[i]; team_name <- entidades_df_lang %>% filter(original_name == team_name_mk) %>% pull(current_lang_name); nombre_archivo_final <- paste0(generar_id_seguro(team_name_mk), ".png"); if (!file.exists(file.path(RUTA_LOGOS_DESTINO, nombre_archivo_final))) { nombre_archivo_final <- "NOLOGO.png" }; ruta_relativa_logo_html <- file.path("..", "..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, nombre_archivo_final); team_element <- tags$span(class="team-cell", tags$img(class="team-logo", src = ruta_relativa_logo_html, alt = team_name), tags$a(href = file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(team_name_mk), ".html")), team_name)); team_tags <- append(team_tags, list(team_element)); if (i < length(teams_mk)) { team_tags <- append(team_tags, list(tags$span(style="margin: 0 5px;", "/"))) } }; tagList(team_tags) }), tags$td(g$Goals)) }))))
          nombre_archivo_goleadoras <- paste0(comp_id, "_", nombres_archivos_traducidos$goleadoras, ".html"); save_html(crear_pagina_html(contenido_goleadoras, paste(t("scorers_title"), "-", comp_nombre_current_lang), "../..", script_contraseña_lang), file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, nombre_archivo_goleadoras))
          lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href=nombre_archivo_goleadoras, class="menu-button", t("scorers_title"))
        }
        if (!is_cup && !is_friendly_comp && !is_placeholder_only_comp) {
          clasificacion_df_comp_raw <- stats_clasificacion_por_comp_df %>% filter(competicion_id == comp_id); clave_estilo_comp <- paste(comp_info$competicion_nombre, comp_info$competicion_temporada); contenido_tabla <- if (nrow(clasificacion_df_comp_raw) == 0) { tags$p(t("standings_no_data_message")) } else { clasificacion_df_comp_raw_lang <- clasificacion_df_comp_raw %>% left_join(entidades_df_lang, by = c("team" = "original_name")) %>% select(Pos, team_lang = current_lang_name, P, W, D, L, GF, GA, GD, Pts); nombres_neutros <- c("Pos", "team_lang", "P", "W", "D", "L", "GF", "GA", "GD", "Pts"); claves_traduccion <- c("standings_pos", "standings_team", "standings_p", "standings_w", "standings_d", "standings_l", "standings_gf", "standings_ga", "standings_gd", "standings_pts"); nombres_traducidos <- sapply(claves_traduccion, t, USE.NAMES = FALSE); mapa_nombres_col <- setNames(as.list(nombres_neutros), nombres_traducidos); clasificacion_df_comp <- clasificacion_df_comp_raw_lang %>% rename(!!!mapa_nombres_col); estilos_comp <- estilos_clasificacion_data[[clave_estilo_comp]]; tagList(tags$table(tags$thead(tags$tr(map(names(clasificacion_df_comp), tags$th))), tags$tbody(map(1:nrow(clasificacion_df_comp), function(j) { fila <- clasificacion_df_comp[j,]; nombre_equipo <- fila[[t("standings_team")]]; posicion_equipo <- fila[[t("standings_pos")]]; nombre_equipo_original <- clasificacion_df_comp_raw$team[j]; nombre_archivo_final <- paste0(generar_id_seguro(nombre_equipo_original), ".png"); if (!file.exists(file.path(RUTA_LOGOS_DESTINO, nombre_archivo_final))) { nombre_archivo_final <- "NOLOGO.png" }; ruta_relativa_logo_html <- file.path("..", "..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, nombre_archivo_final); regla_actual <- NULL; if (!is.null(estilos_comp)) { regla_match <- estilos_comp$reglas %>% filter(puesto == posicion_equipo); if (nrow(regla_match) > 0) { regla_actual <- regla_match[1,] } }; tags$tr(map(seq_along(fila), function(k) { cell_value <- fila[[k]]; col_name <- names(fila)[k]; if (col_name == t("standings_pos") && !is.null(regla_actual)) { tags$td(style = paste0("border-left: 5px solid ", regla_actual$color, "; font-weight: bold;"), cell_value) } else if (col_name == t("standings_team")) { tags$td(class = "team-cell", tags$img(class="team-logo", src = ruta_relativa_logo_html, alt = nombre_equipo), tags$a(href=file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(nombre_equipo_original), ".html")), cell_value)) } else { tags$td(cell_value) }})) }))), if (!is.null(estilos_comp) && length(estilos_comp$leyenda) > 0) { tags$div(class = "legend", map(estilos_comp$leyenda, function(item_leyenda) { tags$div(class = "legend-item", tags$span(class = "legend-color-box", style = paste0("background-color: ", item_leyenda$color, ";")), tags$span(t(item_leyenda$texto_key))) })) }) }; contenido_clasificacion <- tagList(crear_botones_navegacion(path_to_lang_root = ".."), tags$h2(paste(t("standings_title"), "-", comp_nombre_current_lang)), contenido_tabla); nombre_archivo_clasif <- paste0(comp_id, "_", nombres_archivos_traducidos$clasificacion, ".html"); save_html(crear_pagina_html(contenido_clasificacion, paste(t("standings_title"), "-", comp_nombre_current_lang), "../..", script_contraseña_lang), file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, nombre_archivo_clasif)); lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href=nombre_archivo_clasif, class="menu-button", t("standings_title"))
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
          if (nrow(tabla_porteras_comp_raw) > 0) { porteras_mas_50 <- tabla_porteras_comp_raw %>% 
            filter(group == "mas_50") %>% 
            arrange(
              GA90,       # Criterio 1: Menor coeficiente de goles en contra (ascendente)
              desc(CS),     # Criterio 2: Mayor número de porterías a cero (descendente)
              desc(Minutes) # Criterio 3: Mayor número de minutos jugados (descendente)
            ) %>% 
            mutate(Pos = row_number()); 
          
          porteras_menos_50 <- tabla_porteras_comp_raw %>% 
            filter(group == "menos_50", Minutes > 0) %>% 
            arrange(
              GA90,       # Criterio 1: Menor coeficiente de goles en contra (ascendente)
              desc(CS),     # Criterio 2: Mayor número de porterías a cero (descendente)
              desc(Minutes) # Criterio 3: Mayor número de minutos jugados (descendente)
            ) %>% 
            mutate(Pos = row_number());
          generar_tabla_html_porteras <- function(df, table_id) { if (is.null(df) || nrow(df) == 0) { return(tags$p(t("no_data_in_category")))}; tags$table(id = table_id, `data-sort-col` = "3", `data-sort-dir` = "asc", tags$thead(tags$tr(tags$th(t("standings_pos")), tags$th(t("player_type")), tags$th(t("team_type")), tags$th(class="sortable-header asc", onclick=sprintf("sortTable('%s', 3)", table_id), t("gk_ga_90")), tags$th(t("gk_ga")), tags$th(t("stats_minutes")), tags$th(class="sortable-header", onclick=sprintf("sortTable('%s', 6)", table_id), t("gk_cs")))), tags$tbody(map(1:nrow(df), function(j){ p <- df[j,]; nombre_equipo <- p$TeamName; nombre_equipo_mk <- p$TeamName_mk; nombre_archivo_final <- paste0(generar_id_seguro(nombre_equipo_mk), ".png"); if (!file.exists(file.path(RUTA_LOGOS_DESTINO, nombre_archivo_final))) { nombre_archivo_final <- "NOLOGO.png" }; ruta_relativa_logo_html <- file.path("..", "..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, nombre_archivo_final); tags$tr(tags$td(p$Pos), tags$td(tags$a(href=file.path("..", nombres_carpetas_relativos$jugadoras, paste0(p$id, ".html")), p$PlayerName)), tags$td(class = "team-cell", tags$img(class="team-logo", src = ruta_relativa_logo_html, alt = nombre_equipo), tags$a(href=file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(nombre_equipo_mk), ".html")), nombre_equipo)), tags$td(format(round(p$GA90, 2), nsmall = 2)), tags$td(p$GA), tags$td(p$Minutes), tags$td(p$CS)) })))}; contenido_porteras <- tagList(crear_botones_navegacion(path_to_lang_root = ".."), tags$h2(paste(t("goalkeepers_title"), "-", comp_nombre_current_lang)), tags$h3(t("gk_stats_header_over_50")), generar_tabla_html_porteras(porteras_mas_50, "tabla-porteras-mas-50"), tags$h3(t("gk_stats_header_under_50")), generar_tabla_html_porteras(porteras_menos_50, "tabla-porteras-menos-50")); nombre_archivo_porteras <- paste0(comp_id, "_golmanki.html"); save_html(crear_pagina_html(contenido_porteras, paste(t("goalkeepers_title"), "-", comp_nombre_current_lang), "../..", script_contraseña_lang), file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, nombre_archivo_porteras)); lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href=nombre_archivo_porteras, class="menu-button", t("goalkeepers_title")) }
          
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
          nombre_archivo_sanciones <- paste0(comp_id, "_", nombres_archivos_traducidos$sanciones, ".html"); save_html(crear_pagina_html(contenido_sanciones, paste(t("disciplinary_title"), "-", comp_nombre_current_lang), "../..", script_contraseña_lang), file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, nombre_archivo_sanciones)); lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href=nombre_archivo_sanciones, class="menu-button", t("disciplinary_title"))
        }
        contenido_menu_final <- tagList(crear_botones_navegacion(path_to_lang_root = ".."), tags$h2(comp_nombre_current_lang), tags$div(class="menu-container", lista_botones_menu))
        save_html(crear_pagina_html(contenido_menu_final, comp_nombre_current_lang, "../..", script_contraseña_lang, current_page_id = "competitions"), file = file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, paste0(comp_id, ".html")))
      }
    })
    }
    
    # 13.1.23. Generate Individual Profile Pages.
    if (GENERAR_PERFILES_PARTIDO) {
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
        partido_info$categoria 
      } else if(is_cup_match) {
        partido_info$jornada
      } else {
        paste(t("round_prefix"), partido_info$jornada)
      }
      nota_arbitro <- resumen_partido$nota_arbitro %||% NA_character_
      if (!is.na(nota_arbitro)) { nota_arbitro <- str_remove(nota_arbitro, "^[\\s:]*") }
      path_rel_competiciones <- file.path("..", nombres_carpetas_relativos$competiciones); path_rel_timovi <- file.path("..", nombres_carpetas_relativos$timovi); path_rel_jugadoras <- file.path("..", nombres_carpetas_relativos$jugadoras); path_rel_arbitros <- file.path("..", nombres_carpetas_relativos$arbitros); path_rel_estadios <- file.path("..", nombres_carpetas_relativos$estadios)
      crear_cabecera_alineacion <- function(nombre_equipo_mk, nombre_equipo_lang) {
        iso_code <- get_national_team_iso(nombre_equipo_mk)
        
        if (!is.na(iso_code)) {
          logo_src <- paste0("https://hatscripts.github.io/circle-flags/flags/", iso_code, ".svg")
          logo_class <- "match-page-crest national-team-flag"
        } else {
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
            is_stadium_excluded_for_national_match <- partido_info$es_partido_seleccion && (generar_id_seguro(estadio_info_mk$estadio) %in% stadium_ids_to_skip)
            
            if (!is_stadium_excluded_for_national_match) {
              estadio_element <- tags$a(href = file.path(path_rel_estadios, paste0(generar_id_seguro(estadio_info_mk$estadio), ".html")), estadio_name_lang)
            } else {
              estadio_element <- estadio_name_lang
            }
            estadio_element
          } else {
            t("match_unknown")
          }
        ),
        tags$h3(t("referees_title")),
        
        #### MODIFICACIÓN ROBUSTA PARA EL ERROR FATAL Y EL AVISO ####
        tags$ul(class = "sudii-lista", map(1:nrow(arbitros_partido_lang), function(a) {
          arb <- arbitros_partido_lang[a,]
          
          # 1. Comprobación ultra-segura para el rol del árbitro.
          rol_traducido <- "" # Valor por defecto.
          # Esta condición verifica que 'uloga' existe, es un vector de longitud 1,
          # no es NA y no es una cadena vacía, antes de intentar traducirlo.
          if (!is.null(arb$uloga) && length(arb$uloga) == 1 && !is.na(arb$uloga) && nchar(arb$uloga) > 0) {
            rol_traducido <- t(arb$uloga)
          }
          
          # 2. Comprobación ultra-segura para la ciudad.
          nombre_mostrado <- arb$current_lang_name # Valor por defecto.
          # Esta condición verifica que 'ciudad' existe, es de longitud 1,
          # no es NA y no es una cadena vacía, antes de usarla.
          if (!is.null(arb$ciudad) && length(arb$ciudad) == 1 && !is.na(arb$ciudad) && nchar(arb$ciudad) > 0) {
            nombre_mostrado <- paste0(arb$current_lang_name, " (", arb$ciudad, ")")
          }
          
          is_arb_excluded_for_national_match <- partido_info$es_partido_seleccion && (generar_id_seguro(arb$ime) %in% referee_ids_to_skip)
          
          ref_element <- if (!is_arb_excluded_for_national_match) {
            tags$a(href = file.path(path_rel_arbitros, paste0(generar_id_seguro(arb$ime), ".html")), nombre_mostrado)
          } else {
            nombre_mostrado
          }
          
          tags$li(paste0(rol_traducido, ": "), ref_element)
        })),
        #### FIN DE LA MODIFICACIÓN ####
        
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
    }
    
    
    # ==============================================================================
    # == INICIO DEL BLOQUE FINAL v5 DE PERFIL DE JUGADORA (REEMPLAZAR)             ==
    # ==============================================================================
    if (GENERAR_PERFILES_JUGADORA) {
    walk(1:nrow(jugadoras_stats_df), function(i) {
      jugadora <- jugadoras_stats_df[i,]; id_j <- jugadora$id;
      if (id_j %in% player_ids_to_skip) { return() }
      
      if (!full_rebuild_needed && !(id_j %in% affected_player_ids)) { return() }
      
      current_player_name <- jugadora[[player_name_col]]
      
      # --- 1. CONSTRUIR LA CABECERA DEL PERFIL ---
      info_items <- list()
      if (!is.na(jugadora$codigo_iso)) {
        info_items[[length(info_items) + 1]] <- tags$div(class="bio-item", tags$div(class="bio-icon", HTML('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" width="24" height="24"><path d="M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm-1 17.93c-3.95-.49-7-3.85-7-7.93 0-.62.08-1.21.21-1.79L8.35 12H11v7.93zM13 19.93V12h2.65l4.14 2.21c-.43 3.32-3.26 5.95-6.79 6.72zM13 4.07V10h2.65l4.14-2.21C19.37 4.56 16.54 2 13 2.07zM11 4.07c3.53 0 6.37 2.49 6.79 5.72L13 10H11V4.07zM4.26 8.21C4.08 7.43 4 6.64 4 5.86c0-1.03.24-2 .66-2.87l4.14 2.22H4.26zm.43 7.58c.2.6.46 1.17.78 1.7L9.61 14H4.69z"/></svg>')), tags$div(class="bio-text", tags$span(class="bio-label", t("player_nationality")), tags$span(class="bio-value", jugadora$nombre_macedonio %||% jugadora$nacionalidad)))
      }
      mapa_pos_traducida <- c("goalkeeper" = t("position_goalkeeper"), "defender" = t("position_defender"), "midfielder" = t("position_midfielder"), "forward" = t("position_forward"))
      if (!is.na(jugadora$posicion_final_unificada) && jugadora$posicion_final_unificada != "") {
        posicion_traducida <- recode(jugadora$posicion_final_unificada, !!!mapa_pos_traducida, .default = jugadora$posicion_final_unificada)
        info_items[[length(info_items) + 1]] <- tags$div(class="bio-item", tags$div(class="bio-icon", HTML('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" width="24" height="24"><path d="M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm0 18c-4.41 0-8-3.59-8-8s3.59-8 8-8 8 3.59 8 8-3.59 8-8 8zm-1-13h2v6h-2zm0 8h2v2h-2z"/></svg>')), tags$div(class="bio-text", tags$span(class="bio-label", t("player_position")), tags$span(class="bio-value", posicion_traducida)))
      }
      if (!is.na(jugadora$fecha_nacimiento)) {
        fecha_formateada <- format(as.Date(jugadora$fecha_nacimiento), format = "%d.%m.%Y")
        edad_texto <- if (!is.na(jugadora$edad)) paste0("(", jugadora$edad, " ", t("player_age_suffix"), ")") else ""
        info_items[[length(info_items) + 1]] <- tags$div(class="bio-item", tags$div(class="bio-icon", HTML('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" width="24" height="24"><path d="M19 3h-1V1h-2v2H8V1H6v2H5c-1.11 0-1.99.9-1.99 2L3 19c0 1.1.89 2 2 2h14c1.1 0 2-.9 2-2V5c0-1.1-.9-2-2-2zm0 16H5V8h14v11zM7 10h5v5H7z"/></svg>')), tags$div(class="bio-text", tags$span(class="bio-label", t("player_birth_date")), tags$span(class="bio-value", paste(fecha_formateada, edad_texto))))
      }
      ciudad_original_en <- jugadora$ciudad_nacimiento
      if (!is.na(ciudad_original_en) && ciudad_original_en != "") {
        ciudad_a_mostrar <- ciudad_original_en
        if (!is.null(mapa_ciudades_long_df)) {
          traduccion <- mapa_ciudades_long_df %>% filter(lang == idioma_actual, en == ciudad_original_en) %>% pull(translated_city)
          if (length(traduccion) > 0) { ciudad_a_mostrar <- traduccion[1] }
        }
        info_items[[length(info_items) + 1]] <- tags$div(class="bio-item", tags$div(class="bio-icon", HTML('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" width="24" height="24"><path d="M12 2C8.13 2 5 5.13 5 9c0 5.25 7 13 7 13s7-7.75 7-13c0-3.87-3.13-7-7-7zm0 9.5c-1.38 0-2.5-1.12-2.5-2.5s1.12-2.5 2.5-2.5 2.5 1.12 2.5 2.5-1.12 2.5-2.5 2.5z"/></svg>')), tags$div(class="bio-text", tags$span(class="bio-label", t("player_birth_place")), tags$span(class="bio-value", ciudad_a_mostrar)))
      }
      
      # --- 2. CONSTRUIR TARJETA DE ESTADÍSTICAS (LÓGICA CORREGIDA) ---
      player_career_data <- career_summary_jugadoras_df %>% filter(id == id_j)
      stats_summary_card_html <- NULL
      if (nrow(player_career_data) > 0) {
        latest_season_info <- player_career_data %>% 
          filter(!str_detect(competicion_nombre, "Пријателски|Бараж")) %>% 
          mutate(start_year = as.integer(substr(competicion_temporada, 1, 2))) %>% 
          arrange(desc(start_year)) %>% 
          slice(1)
        
        if (nrow(latest_season_info) > 0) {
          latest_season <- latest_season_info$competicion_temporada[1]
          
          # CORRECCIÓN 1: Se añade `Played` a la suma y se recalcula `SubOn`.
          latest_season_stats <- player_career_data %>% 
            filter(competicion_temporada == latest_season, !str_detect(competicion_nombre, "Пријателски|Бараж")) %>% 
            summarise(
              Played=sum(Played, na.rm=T), # <-- (A) AÑADIDO: Sumar los partidos jugados.
              Starter=sum(Starter, na.rm=T), 
              Goals=sum(Goals, na.rm=T), 
              Yellows=sum(Yellows, na.rm=T), 
              Reds=sum(Reds, na.rm=T)
            ) %>% 
            mutate(SubOn = Played - Starter) # <-- (B) CORREGIDO: SubOn ahora es Jugados - Titular.
          
          stats_summary_card_html <- tags$div(class="stats-summary-card", 
                                              tags$div(class="season-tag", latest_season), 
                                              tags$div(class="stats-grid", 
                                                       # CORRECCIÓN 2: Se usa `Played` en lugar de `CalledUp` para "Apps".
                                                       tags$div(class="stat-item", HTML('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="stat-icon"><path d="M17 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2"></path><circle cx="9" cy="7" r="4"></circle><path d="M23 21v-2a4 4 0 0 0-3-3.87"></path><path d="M16 3.13a4 4 0 0 1 0 7.75"></path></svg>'), 
                                                                tags$span(class="stat-value", latest_season_stats$Played), # <-- (C) CORREGIDO: Muestra `Played`.
                                                                tags$span(class="stat-label", t("player_apps"))
                                                       ), 
                                                       tags$div(class="stat-item", HTML('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="stat-icon"><circle cx="12" cy="12" r="10"></circle><polyline points="12 6 12 12 16 14"></polyline></svg>'), 
                                                                tags$span(class="stat-value", latest_season_stats$Starter), 
                                                                tags$span(class="stat-label", t("player_starter"))
                                                       ), 
                                                       tags$div(class="stat-item", HTML('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="stat-icon"><polyline points="15 3 21 3 21 9"></polyline><polyline points="9 21 3 21 3 15"></polyline><line x1="21" y1="3" x2="14" y2="10"></line><line x1="3" y1="21" x2="10" y2="14"></line></svg>'), 
                                                                tags$span(class="stat-value", latest_season_stats$SubOn), 
                                                                tags$span(class="stat-label", t("player_sub_on"))
                                                       ), 
                                                       tags$div(class="stat-item", HTML('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="stat-icon"><circle cx="12" cy="12" r="2"></circle><path d="M16.24 7.76l-2.12 2.12M12 2v2M7.76 7.76l2.12 2.12M2 12h2M7.76 16.24l2.12-2.12M12 22v-2M16.24 16.24l-2.12-2.12M22 12h-2"></path></svg>'), 
                                                                tags$span(class="stat-value", latest_season_stats$Goals), 
                                                                tags$span(class="stat-label", t("player_goals"))
                                                       ), 
                                                       tags$div(class="stat-item", HTML('<div class="stat-icon card-icon yellow"></div>'), 
                                                                tags$span(class="stat-value", latest_season_stats$Yellows), 
                                                                tags$span(class="stat-label", t("player_yellow_cards_short"))
                                                       ), 
                                                       tags$div(class="stat-item", HTML('<div class="stat-icon card-icon red"></div>'), 
                                                                tags$span(class="stat-value", latest_season_stats$Reds), 
                                                                tags$span(class="stat-label", t("player_red_cards_short"))
                                                       )
                                              )
          )
        }
      }
      
      # --- 3. CONSTRUIR EL ACORDEÓN DE ESTADÍSTICAS ---
      
      generar_logo_html <- function(nombre_equipo_mk) {
        iso_code <- get_national_team_iso(nombre_equipo_mk)
        if (!is.na(iso_code)) {
          tags$img(class="team-logo-small national-team-flag", src = paste0("https://hatscripts.github.io/circle-flags/flags/", iso_code, ".svg"))
        } else {
          logo_path <- paste0(generar_id_seguro(nombre_equipo_mk), ".png")
          if (!file.exists(file.path(RUTA_LOGOS_DESTINO, logo_path))) { logo_path <- "NOLOGO.png" }
          tags$img(class="team-logo-small", src=file.path("..", "..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, logo_path))
        }
      }
      
      career_accordion_html <- NULL
      if (nrow(player_career_data) > 0) {
        partidos_jugadora_details <- apariciones_df %>%
          filter(id == id_j) %>%
          left_join(
            partidos_df %>% select(id_partido, local, visitante, goles_local, goles_visitante, fecha, es_partido_seleccion, categoria),
            by = "id_partido"
          ) %>% # <-- ESTA ES LA LÍNEA CORREGIDA
          left_join(entidades_df_lang %>% select(original_name, local_lang=current_lang_name), by=c("local"="original_name")) %>%
          left_join(entidades_df_lang %>% select(original_name, visitante_lang=current_lang_name), by=c("visitante"="original_name"))
        national_team_data_player <- national_team_career_by_category_df %>% filter(id == id_j)
        seasons_club <- player_career_data %>% filter(equipo != "Македонија") %>% distinct(competicion_temporada) %>% mutate(type = "club")
        seasons_nat <- if(nrow(national_team_data_player) > 0) tibble(competicion_temporada = t("competition_reprezentacija"), type = "national") else tibble()
        all_seasons <- bind_rows(seasons_nat, seasons_club) %>% mutate(start_year = ifelse(type == "club", as.integer(substr(competicion_temporada, 1, 2)), 999)) %>% arrange(desc(start_year))
        
        career_accordion_html <- tags$div(class="season-accordion-container",
                                          tags$h3(t("player_stats_by_season")),
                                          map(1:nrow(all_seasons), function(s_idx) {
                                            season_info <- all_seasons[s_idx, ]; season_name <- season_info$competicion_temporada; season_type <- season_info$type; season_id_safe <- paste0(str_replace_all(tolower(season_name), "[^a-z0-9]", "-"), "-", s_idx)
                                            if(season_type == "national") {
                                              stats_tab_content <- tags$table(class="stats-table-season",
                                                                              tags$thead(tags$tr(tags$th(t("category_header")), tags$th(t("player_apps")), tags$th(t("player_starter")), tags$th(t("player_goals")), tags$th(HTML('<span class="card-icon-header yellow"></span>')), tags$th(HTML('<span class="card-icon-header red"></span>')), tags$th(t("player_mins")))),
                                                                              tags$tbody(map(1:nrow(national_team_data_player), function(k) {
                                                                                stage <- national_team_data_player[k, ]; tags$tr(tags$td(stage$categoria), tags$td(stage$Played), tags$td(stage$Starter), tags$td(stage$Goals), tags$td(stage$Yellows), tags$td(stage$Reds), tags$td(stage$Minutes))
                                                                              }))
                                              )
                                             
                                              matches_data_season <- partidos_jugadora_details %>% 
                                                filter(es_partido_seleccion == TRUE, equipo == "Македонија") %>%
                                                mutate(fecha_date = as.Date(fecha, format = "%d.%m.%Y")) %>%
                                                arrange(desc(fecha_date))
                                              categories_in_season <- matches_data_season %>% pull(categoria) %>% unique() %>% sort()
                                              matches_tab_content <- tagList(
                                                tags$div(class="sub-tab-nav", map(seq_along(categories_in_season), function(c_idx){ current_category <- categories_in_season[c_idx]; tags$button(class=if(c_idx==1)"sub-tab-button active" else "sub-tab-button", `data-season-id`=season_id_safe, `data-subtab-target`=current_category, current_category) })),
                                                map(seq_along(categories_in_season), function(c_idx){
                                                  current_category <- categories_in_season[c_idx]; matches_in_comp <- matches_data_season %>% filter(categoria == current_category)
                                                  tags$div(id=paste0("matches-", season_id_safe, "-", current_category), class=if(c_idx==1)"sub-tab-panel active" else "sub-tab-panel",
                                                           tags$div(class="match-list-container",
                                                                    tags$div(class="match-list-header", tags$div(t("team_header_date")), tags$div(t("match_header_match")), tags$div(t("category_header")), tags$div(t("player_goals")), tags$div(t("match_header_card")), tags$div(t("player_mins"))),
                                                                    map(1:nrow(matches_in_comp), function(p_idx){
                                                                      partido_row <- matches_in_comp[p_idx,]; goles_partido_jugadora <- goles_df_unificado %>% filter(id_partido == partido_row$id_partido, id == id_j); tarjetas_partido_jugadora <- tarjetas_df_unificado %>% filter(id_partido == partido_row$id_partido, id == id_j);
                                                                      tags$div(class="match-list-row clickable-row", `data-href`=file.path("..", nombres_carpetas_relativos$partidos, paste0(partido_row$id_partido, ".html")),
                                                                               tags$div(class="cell-date", partido_row$fecha),
                                                                               tags$div(class="cell-match", tags$span(class="team-home", partido_row$local_lang, generar_logo_html(partido_row$local)), tags$span(class="match-score", paste(partido_row$goles_local, ":", partido_row$goles_visitante)), tags$span(class="team-away", generar_logo_html(partido_row$visitante), partido_row$visitante_lang)),
                                                                               tags$div(class="cell-competition", partido_row$categoria),
                                                                               tags$div(class="cell-goals", if(nrow(goles_partido_jugadora)>0) nrow(goles_partido_jugadora) else "0"),
                                                                               tags$div(class="cell-cards", if(nrow(tarjetas_partido_jugadora)>0) tagList(if("Amarilla" %in% tarjetas_partido_jugadora$tipo) tags$span(class="card-icon-table yellow"), if("Roja" %in% tarjetas_partido_jugadora$tipo) tags$span(class="card-icon-table red"))),
                                                                               tags$div(class="cell-minutes", if(is.na(partido_row$minutos_jugados)) "0" else partido_row$minutos_jugados)
                                                                      )
                                                                    })
                                                           )
                                                  )
                                                })
                                              )
                                            } else {
                                              stats_data_season <- player_career_data %>% filter(competicion_temporada == season_name) %>% left_join(competiciones_unicas_df, by=c("competicion_nombre", "competicion_temporada")) %>% left_join(entidades_df_lang, by = c("equipo" = "original_name"))
                                              stats_tab_content <- tags$table(class="stats-table-season",
                                                                              tags$thead(tags$tr(tags$th(t("player_competition")), tags$th(t("team_type")), tags$th(t("player_apps")), tags$th(t("player_starter")), tags$th(t("player_goals")), tags$th(HTML('<span class="card-icon-header yellow"></span>')), tags$th(HTML('<span class="card-icon-header red"></span>')), tags$th(t("player_mins")))),
                                                                              tags$tbody(map(1:nrow(stats_data_season), function(k){
                                                                                stage <- stats_data_season[k, ]; comp_url <- file.path("..", nombres_carpetas_relativos$competiciones, paste0(stage$competicion_id, ".html")); team_url <- file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(stage$equipo), ".html"));
                                                                                tags$tr(tags$td(tags$a(href = comp_url, stage[[comp_name_col]])), tags$td(class="team-cell-with-logo", generar_logo_html(stage$equipo), tags$a(href=team_url, stage$current_lang_name)), tags$td(stage$Played), tags$td(stage$Starter), tags$td(stage$Goals), tags$td(stage$Yellows), tags$td(stage$Reds), tags$td(stage$Minutes))
                                                                              }))
                                              )
                                              # CORRECCIÓN AQUÍ: Asegurarse de que `partidos_jugadora_details` tiene las columnas necesarias antes de filtrar.
                                              matches_data_season <- partidos_jugadora_details %>% 
                                                left_join(competiciones_unicas_df, by=c("competicion_nombre", "competicion_temporada")) %>%
                                                filter(competicion_temporada == season_name)
                                              
                                              competitions_in_season <- matches_data_season %>% distinct(competicion_id, .keep_all = TRUE) %>% select(competicion_id, competicion_nombre_lang = !!sym(comp_name_col))
                                              matches_tab_content <- tagList(
                                                tags$div(class="sub-tab-nav", map(1:nrow(competitions_in_season), function(c_idx){ comp <- competitions_in_season[c_idx,]; tags$button(class=if(c_idx==1)"sub-tab-button active" else "sub-tab-button", `data-season-id`=season_id_safe, `data-subtab-target`=comp$competicion_id, comp$competicion_nombre_lang) })),
                                                map(1:nrow(competitions_in_season), function(c_idx){

                                                  comp <- competitions_in_season[c_idx,]; 
                                                  matches_in_comp <- matches_data_season %>% 
                                                    filter(competicion_id == comp$competicion_id) %>%
                                                    mutate(fecha_date = as.Date(fecha, format = "%d.%m.%Y")) %>%
                                                    arrange(desc(fecha_date))
                                                           tags$div(id=paste0("matches-", season_id_safe, "-", comp$competicion_id), class=if(c_idx==1)"sub-tab-panel active" else "sub-tab-panel",
                                                           tags$div(class="match-list-container",
                                                                    tags$div(class="match-list-header", tags$div(t("team_header_date")), tags$div(t("match_header_match")), tags$div(t("player_goals")), tags$div(t("match_header_card")), tags$div(t("player_mins"))),
                                                                    map(1:nrow(matches_in_comp), function(p_idx){
                                                                      partido_row <- matches_in_comp[p_idx,]; goles_partido_jugadora <- goles_df_unificado %>% filter(id_partido == partido_row$id_partido, id == id_j); tarjetas_partido_jugadora <- tarjetas_df_unificado %>% filter(id_partido == partido_row$id_partido, id == id_j);
                                                                      tags$div(class="match-list-row clickable-row", `data-href`=file.path("..", nombres_carpetas_relativos$partidos, paste0(partido_row$id_partido, ".html")),
                                                                               tags$div(class="cell-date", partido_row$fecha),
                                                                               tags$div(class="cell-match", tags$span(class="team-home", partido_row$local_lang, generar_logo_html(partido_row$local)), tags$span(class="match-score", paste(partido_row$goles_local, ":", partido_row$goles_visitante)), tags$span(class="team-away", generar_logo_html(partido_row$visitante), partido_row$visitante_lang)),
                                                                               tags$div(class="cell-goals", if(nrow(goles_partido_jugadora)>0) nrow(goles_partido_jugadora) else "0"),
                                                                               tags$div(class="cell-cards", if(nrow(tarjetas_partido_jugadora)>0) tagList(if("Amarilla" %in% tarjetas_partido_jugadora$tipo) tags$span(class="card-icon-table yellow"), if("Roja" %in% tarjetas_partido_jugadora$tipo) tags$span(class="card-icon-table red"))),
                                                                               tags$div(class="cell-minutes", if(is.na(partido_row$minutos_jugados)) "0" else partido_row$minutos_jugados)
                                                                      )
                                                                    })
                                                           )
                                                  )
                                                })
                                              )
                                            }
                                            tags$div(class="season-accordion",
                                                     tags$div(class="season-header", tags$span(class="season-header-title", season_name), tags$span(class="season-arrow")),
                                                     tags$div(class="season-content",
                                                              tags$div(class="tab-nav",
                                                                       tags$button(class="tab-button active", `data-season-id`=season_id_safe, `data-tab-target`="stats", t("tab_stats")),
                                                                       tags$button(class="tab-button", `data-season-id`=season_id_safe, `data-tab-target`="matches", t("tab_matches"))),
                                                              tags$div(id=paste0("stats-", season_id_safe), class="tab-panel active", stats_tab_content),
                                                              tags$div(id=paste0("matches-", season_id_safe), class="tab-panel", matches_tab_content)
                                                     )
                                            )
                                          })
        )
      }
      
      # --- 4. ENSAMBLAR LA PÁGINA FINAL ---
      
      contenido_jugadora <- tagList(
        crear_botones_navegacion(path_to_lang_root = ".."),
        tags$div(class = "player-profile-header-new", tags$div(class = "player-name-container", tags$h2(class = "player-name-new", current_player_name)), if(length(info_items) > 0) tags$div(class = "player-bio", info_items)),
        stats_summary_card_html,
        career_accordion_html
      )
      pagina_jugadora_final <- crear_pagina_html(
        contenido_jugadora, current_player_name, path_to_root_dir = "../..", 
        script_contraseña_lang
      )
      save_html(pagina_jugadora_final, file = file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$jugadoras, paste0(id_j, ".html")))
    })
    }
    # ============================================================================
    # == FIN DEL BLOQUE FINAL v5 DE PERFIL DE JUGADORA                           ==
    # ============================================================================
  
    
    
    
    # ================== REEMPLAZA TODO EL CONTENIDO DEL BLOQUE if (GENERAR_PERFILES_EQUIPO) CON ESTO ==================
    if (GENERAR_PERFILES_EQUIPO) {
      message("   > Generating team profiles (v8 - Literal Implementation)...")
      
      # --- Definimos las funciones auxiliares del script de pruebas UNA VEZ fuera del bucle ---
      find_main_stadium <- function(team_name_mk, seasons_df, estadios_df) {
        seasons_sorted <- seasons_df %>% mutate(start_year = as.integer(substr(competicion_temporada, 1, 2))) %>% arrange(desc(start_year)) %>% pull(competicion_temporada) %>% unique()
        for (season in seasons_sorted) {
          stadium <- estadios_df %>%
            filter(local == team_name_mk, competicion_temporada == season, !str_detect(competicion_nombre, "Пријателски|Бараж"), !is.na(estadio)) %>%
            count(estadio, sort = TRUE) %>% slice(1) %>% pull(estadio)
          if (length(stadium) > 0) return(stadium)
        }
        return(NA_character_)
      }
      
      # --- Inicia el bucle para cada equipo ---
      walk(unique(c(partidos_df$local, partidos_df$visitante)), function(team_mk) {
        if (team_mk %in% team_names_to_skip_mk) { return() }
        id_t <- generar_id_seguro(team_mk)
        if (!full_rebuild_needed && !(id_t %in% affected_team_ids)) { return() }
        
        # ============================================================================
        # == INICIO DE LA LÓGICA LITERAL DEL SCRIPT DE PRUEBAS                       ==
        # ============================================================================
        
        # --- 3.1. Preparar datos específicos del idioma ---
        current_team_name <- (entidades_df_lang %>% filter(original_name == team_mk))$current_lang_name[1]
        
        # --- 3.2. Preparación de datos avanzada ---
        stadium_principal_mk <- find_main_stadium(team_mk, partidos_df, estadios_df)
        stadium_principal_lang <- if (!is.na(stadium_principal_mk)) (entidades_df_lang %>% filter(original_name == stadium_principal_mk))$current_lang_name[1] else NA_character_
        
        logo_filename_principal <- paste0(generar_id_seguro(team_mk), ".png")
        if (!file.exists(file.path(RUTA_LOGOS_DESTINO, logo_filename_principal))) { logo_filename_principal <- "NOLOGO.png" }
        ruta_logo_principal <- file.path("..", "..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, logo_filename_principal)
        
        # -- LÓGICA DE DORSAL LITERAL DEL SCRIPT DE PRUEBAS --
        dorsal_principal_df <- apariciones_df %>%
          filter(equipo == team_mk, !is.na(dorsal)) %>%
          group_by(id, competicion_temporada, competicion_nombre) %>%
          count(dorsal, name = "freq", sort = TRUE) %>% slice(1) %>% ungroup() %>%
          select(id, competicion_temporada, competicion_nombre, dorsal_principal = dorsal)
        
        # -- LÓGICA DE ROSTER LITERAL DEL SCRIPT DE PRUEBAS --
        stats_jugadoras_con_categoria <- stats_jugadoras_por_equipo_temporada_df %>%
          filter(equipo == team_mk) %>%
          # ¡IMPORTANTE! La lógica de categoría del script de pruebas se basa en el nombre de la competición, no en la columna 'categoria'.
          mutate(category_key = case_when(
            str_detect(competicion_nombre, "Младинска") ~ "category_youth", 
            str_detect(competicion_nombre, "Кадетска") ~ "category_cadet", 
            TRUE ~ "category_senior"
          )) %>%
          left_join(dorsal_principal_df, by = c("id", "competicion_temporada", "competicion_nombre")) %>%
          # Unimos con jugadoras_stats_df para obtener el nombre traducido correcto y la posición
          left_join(jugadoras_stats_df %>% select(id, PlayerName = !!sym(player_name_col), posicion_final_unificada), by = "id") %>%
          # Unimos con competiciones_unicas_df para el id de competición y el nombre de competición traducido
          left_join(competiciones_unicas_df %>% select(competicion_id, competicion_nombre, competicion_temporada, CompeticionLang = !!sym(comp_name_col)), by = c("competicion_nombre", "competicion_temporada"))
        
        # -- LÓGICA DE CALENDARIO LITERAL DEL SCRIPT DE PRUEBAS --
        partidos_del_equipo_con_categoria <- partidos_df %>%
          filter(local == team_mk | visitante == team_mk, !is.na(id_partido)) %>% 
          mutate(
            category_key = case_when(
              str_detect(competicion_nombre, "Младинска") ~ "category_youth", 
              str_detect(competicion_nombre, "Кадетска") ~ "category_cadet", 
              TRUE ~ "category_senior"
            ),
            category_name = sapply(category_key, t)
          ) %>%
          left_join(entidades_df_lang %>% rename(local_lang = current_lang_name), by = c("local" = "original_name")) %>%
          left_join(entidades_df_lang %>% rename(visitante_lang = current_lang_name), by = c("visitante" = "original_name")) %>%
          left_join(competiciones_unicas_df %>% select(competicion_id, competicion_nombre, competicion_temporada, CompeticionLang = !!sym(comp_name_col)), by = c("competicion_nombre", "competicion_temporada")) %>%
          rowwise() %>%
          mutate(
            home_logo_url = {
              logo_filename <- paste0(generar_id_seguro(local), ".png")
              if (!file.exists(file.path(RUTA_LOGOS_DESTINO, logo_filename))) { logo_filename <- "NOLOGO.png" }
              file.path("..", "..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, logo_filename)
            },
            away_logo_url = {
              logo_filename <- paste0(generar_id_seguro(visitante), ".png")
              if (!file.exists(file.path(RUTA_LOGOS_DESTINO, logo_filename))) { logo_filename <- "NOLOGO.png" }
              file.path("..", "..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, logo_filename)
            }
          ) %>%
          ungroup()
        
        # --- 3.3. Incrustar datos como JSON (Literal) ---
        datos_para_js <- toJSON(list(
          roster_data = stats_jugadoras_con_categoria,
          matches_data = partidos_del_equipo_con_categoria,
          translations = list(
            position_goalkeeper = t("position_goalkeeper"), position_defender = t("position_defender"),
            position_midfielder = t("position_midfielder"), position_forward = t("position_forward"), col_dorsal = "#",
            col_player = t("team_roster_player"), col_apps = t("player_apps_short"), col_mins = t("player_mins_short"),
            col_goals = t("player_goals_short"), col_cards = t("team_roster_cards"), no_players_found = t("team_roster_no_players"),
            no_matches_found = t("team_schedule_no_matches"),
            col_date = t("match_date"), col_match = t("match_match"),
            col_competition = t("player_competition"),
            category_senior = t("category_senior"), category_youth = t("category_youth"), category_cadet = t("category_cadet")
          )
        ), auto_unbox = TRUE)
        script_datos_json <- tags$script(id = "team-page-data", type = "application/json", HTML(datos_para_js))
        
        # --- 3.4. Construcción del HTML de la página (Literal) ---
        team_header_final <- tags$div(class="team-header-container-v5",
                                      tags$div(class="team-logo-main", tags$img(src = ruta_logo_principal, alt = paste("Logo", current_team_name))),
                                      tags$div(class="team-header-info",
                                               tags$h2(class="team-name-v3", current_team_name),
                                               if (!is.na(stadium_principal_lang)) {
                                                 tags$div(class="info-item",
                                                          tags$div(class="info-icon", HTML('<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 -960 960 960" width="24"><path d="M480-480q33 0 56.5-23.5T560-560q0-33-23.5-56.5T480-640q-33 0-56.5 23.5T400-560q0 33 23.5 56.5T480-480Zm0 400Q319-217 239.5-334.5T160-552q0-150 96.5-239T480-880q127 0 223.5 89T800-552q0 100-79.5 217.5T480-80Z"/></svg>')),
                                                          tags$div(class="info-text", tags$span(class="info-label", toupper(t("match_stadium"))), tags$span(class="info-value", stadium_principal_lang))
                                                 )
                                               }
                                      )
        )
        # ¡CORRECCIÓN! Usando las clases de pestaña correctas para que coincida con el estilo de la jugadora.
        tab_navigation <- tags$div(class="tab-nav-v4",
                                   tags$button(class="tab-button-v4 active", `data-tab-target`="roster-panel", t("team_tab_roster")),
                                   tags$button(class="tab-button-v4", `data-tab-target`="schedule-panel", t("team_tab_schedule"))
        )
        available_seasons_roster <- if(nrow(stats_jugadoras_con_categoria) > 0) sort(unique(stats_jugadoras_con_categoria$competicion_temporada), decreasing = TRUE) else c()
        available_seasons_schedule <- if(nrow(partidos_del_equipo_con_categoria) > 0) sort(unique(partidos_del_equipo_con_categoria$competicion_temporada), decreasing = TRUE) else c()
        roster_panel <- tags$div(id="roster-panel", class="tab-panel active",
                                 tags$div(class="filters-and-pills-container",
                                          tags$div(class="filter-bar-v2", style = "margin-bottom: 0;",
                                                   tags$div(class="filter-group", tags$label(`for`="roster-season-filter", t("player_season")), tags$select(id="roster-season-filter", map(available_seasons_roster, ~tags$option(value = .x, .x)))),
                                                   tags$div(class="filter-group", tags$label(`for`="roster-category-filter", t("category_header")), tags$select(id="roster-category-filter"))
                                          ),
                                          tags$div(id="roster-competition-pills", class="competition-pills-container")
                                 ),
                                 tags$div(id="roster-table-container")
        )
        schedule_panel <- tags$div(id="schedule-panel", class="tab-panel",
                                   tags$div(class="filters-and-pills-container",
                                            tags$div(class="filter-bar-v2", style = "margin-bottom: 0;",
                                                     tags$div(class="filter-group", tags$label(`for`="schedule-season-filter", t("player_season")), tags$select(id="schedule-season-filter", map(available_seasons_schedule, ~tags$option(value = .x, .x)))),
                                                     tags$div(class="filter-group", tags$label(`for`="schedule-category-filter", t("category_header")), tags$select(id="schedule-category-filter"))
                                            ),
                                            tags$div(id="schedule-competition-pills", class="competition-pills-container")
                                   ),
                                   tags$div(id="schedule-table-container")
        )
        contenido_equipo <- tagList(team_header_final, tab_navigation, roster_panel, schedule_panel)
        
        # --- 4. Ensamblado final ---
        pagina_equipo_final <- crear_pagina_html(contenido_equipo, current_team_name, path_to_root_dir = "../..", script_contraseña_lang, current_page_id = "teams")
        pagina_equipo_final$children[[2]]$children <- tagAppendChildren(pagina_equipo_final$children[[2]]$children, script_datos_json)
        save_html(pagina_equipo_final, file = file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$timovi, paste0(id_t, ".html")))
        
        # ============================================================================
        # == FIN DE LA LÓGICA LITERAL                                                ==
        # ============================================================================
      })
    }
    
    
    if (GENERAR_PERFILES_ARBITRO) {
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
    }
      
    if (GENERAR_PERFILES_ESTADIO) {
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
    }
    
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
  
  saveRDS(partidos_actuales_ids, file = ruta_build_log)
  message(paste("\nBuild log updated successfully at:", ruta_build_log))
  
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