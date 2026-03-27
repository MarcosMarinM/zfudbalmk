#### 8. HELPER FUNCTIONS AND TEMPLATE DEFINITIONS ####

### 8.1. Text Formatting and Sanitization Functions

#' @title Normalize a name for search matching.
#' @description Applies a 4-step normalization pipeline: lowercase, Cyrillic-to-Latin
#' transliteration, diacritics removal, and digraph/double collapsing. The same
#' pipeline is replicated in JavaScript (normalizeForSearch) so that user input and
#' stored search terms converge to the same normalized form regardless of script.
#' @param nombre A string (Cyrillic, Latin, or mixed).
#' @return A single normalized lowercase ASCII string.
generar_terminos_busqueda <- function(nombre) {
  if (is.na(nombre) || nchar(trimws(nombre)) == 0) return("")
  
  # Step 1: Lowercase
  texto <- tolower(nombre)
  

  # Step 2: Cyrillic → Latin (1:1 map)
  map_cyr_to_lat <- c(
    '\u0430'='a', '\u0431'='b', '\u0432'='v', '\u0433'='g', '\u0434'='d',
    '\u0453'='gj', '\u0435'='e', '\u0436'='z', '\u0437'='z', '\u0455'='dz',
    '\u0438'='i', '\u0458'='j', '\u043a'='k', '\u043b'='l', '\u0459'='lj',
    '\u043c'='m', '\u043d'='n', '\u045a'='nj', '\u043e'='o', '\u043f'='p',
    '\u0440'='r', '\u0441'='s', '\u0442'='t', '\u045c'='kj', '\u0443'='u',
    '\u0444'='f', '\u0445'='h', '\u0446'='c', '\u0447'='c', '\u0448'='s',
    '\u045f'='dz'
  )
  texto <- str_replace_all(texto, map_cyr_to_lat)
  
  # Step 3: Remove diacritics
  map_diacritics <- c(
    '\u010d'='c', '\u0161'='s', '\u017e'='z', '\u0111'='d', '\u0107'='c',
    '\u01f5'='g', '\u1e31'='k', '\u0144'='n', '\u013a'='l', '\u00f1'='n',
    '\u00eb'='e', '\u00e7'='c', '\u00df'='s',
    '\u00fc'='u', '\u00f6'='o', '\u00e4'='a',
    '\u00e1'='a', '\u00e9'='e', '\u00ed'='i', '\u00f3'='o', '\u00fa'='u',
    '\u00e0'='a', '\u00e8'='e', '\u00ec'='i', '\u00f2'='o', '\u00f9'='u',
    '\u00e2'='a', '\u00ea'='e', '\u00ee'='i', '\u00f4'='o', '\u00fb'='u'
  )
  texto <- str_replace_all(texto, map_diacritics)
  
  # Step 4: Collapse digraphs and doubles (longest first)
  # Trigraphs
  texto <- str_replace_all(texto, fixed("dzh"), "z")
  texto <- str_replace_all(texto, fixed("sch"), "s")
  texto <- str_replace_all(texto, fixed("tch"), "c")
  texto <- str_replace_all(texto, fixed("dgh"), "g")
  # Digraphs (Macedonian, Albanian, international)
  digraphs <- c("xh"="z", "sh"="s", "ch"="c", "zh"="z", "gj"="g", "kj"="k",
                "nj"="n", "lj"="l", "dj"="d", "dz"="z", "dh"="d", "th"="t",
                "ah"="a", "ph"="f")
  for (dg in names(digraphs)) {
    texto <- str_replace_all(texto, fixed(dg), digraphs[[dg]])
  }
  # Double consonants
  doubles <- c("ll","ss","rr","tt","ff","nn","mm","pp","bb","dd","gg","cc","zz")
  for (dbl in doubles) {
    texto <- str_replace_all(texto, fixed(dbl), substr(dbl, 1, 1))
  }
  
  return(texto)
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
              tags$link(rel = "icon", type = "image/png", href = file.path(path_to_root_dir, "favicon.png")),
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

### 8.4. Shared Utility Functions (Consolidated from multiple files)

#' @title Format match minutes to handle added time (e.g., 451 → "45+1").
#' @param minutos A numeric vector of minutes.
#' @return A character vector with the formatted minutes.
formatear_minuto_partido <- function(minutos) {
  sapply(minutos, function(minuto) {
    if (is.na(minuto) || !is.numeric(minuto)) return(as.character(minuto))
    if (minuto > 140 && nchar(as.character(minuto)) >= 3) {
      minuto_str <- as.character(minuto)
      base <- substr(minuto_str, 1, 2)
      added <- substr(minuto_str, 3, nchar(minuto_str))
      return(paste0(base, "+", added))
    } else {
      return(as.character(minuto))
    }
  })
}

#' @title Get the relative file path to a club team's logo.
#' @description Always returns a local path (no national team flag logic).
#'   Falls back to "NOLOGO.png" if not found.
#' @param nombre_equipo_mk Team name in Macedonian (original_name).
#' @param path_to_root Relative path from the calling page to the docs/ root.
#' @return A string with the relative path to the logo file.
get_club_logo_path <- function(nombre_equipo_mk, path_to_root = "../..") {
  nombre_archivo <- paste0(generar_id_seguro(nombre_equipo_mk), ".png")
  if (!file.exists(file.path(RUTA_LOGOS_DESTINO, nombre_archivo))) {
    nombre_archivo <- "NOLOGO.png"
  }
  file.path(path_to_root, nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, nombre_archivo)
}

#' @title Get an HTML img tag for a team logo.
#' @description For national teams, uses flag SVG from hatscripts.github.io.
#'   For clubs, uses local logo file. Falls back to "NOLOGO.png".
#' @param nombre_equipo_mk Team name in Macedonian (original_name).
#' @param path_to_root Relative path from the calling page to the docs/ root.
#' @param css_class CSS class(es) for the img tag. "national-team-flag" is
#'   appended automatically for national teams.
#' @return An htmltools img tag.
get_logo_tag <- function(nombre_equipo_mk, path_to_root = "../..", css_class = "team-logo") {
  iso_code <- get_national_team_iso(nombre_equipo_mk)
  if (!is.na(iso_code)) {
    flag_url <- paste0("https://hatscripts.github.io/circle-flags/flags/", iso_code, ".svg")
    tags$img(class = paste(css_class, "national-team-flag"), src = flag_url, alt = nombre_equipo_mk)
  } else {
    tags$img(class = css_class, src = get_club_logo_path(nombre_equipo_mk, path_to_root), alt = nombre_equipo_mk)
  }
}

#' @title Order round/matchday names for cup and league competitions.
#' @param jornadas_vector Character vector of round names.
#' @param descendente If TRUE, order from most recent to earliest.
#' @return A character vector of ordered round names (NAs removed).
ordenar_jornadas <- function(jornadas_vector, descendente = FALSE) {
  df <- data.frame(jornada = jornadas_vector, stringsAsFactors = FALSE) |>
    filter(!is.na(jornada)) |>
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
    ))
  
  if (descendente) {
    df <- df |> arrange(desc(order_key))
  } else {
    df <- df |> arrange(order_key)
  }
  
  df |> pull(jornada)
}

#' @title Generate search index entries for a given entity type.
#' @param df Dataframe with columns: original_name, current_lang_name, latin_name.
#' @param nombres_filtro Character vector of names to include.
#' @param tipo_entidad Translation key for the entity type (e.g., "team_type").
#' @param id_prefix Prefix for the target_id (e.g., "equipo-").
#' @return A tibble with columns: Име, Тип, target_id, search_terms.
generar_search_entidad <- function(df, nombres_filtro, tipo_entidad, id_prefix) {
  df |>
    filter(original_name %in% nombres_filtro) |>
    mutate(
      Тип = t(tipo_entidad),
      target_id = paste0(id_prefix, generar_id_seguro(original_name)),
      search_terms = paste(
        sapply(current_lang_name, generar_terminos_busqueda, USE.NAMES = FALSE),
        sapply(latin_name, generar_terminos_busqueda, USE.NAMES = FALSE),
        sapply(original_name, generar_terminos_busqueda, USE.NAMES = FALSE)
      )
    ) |>
    select(Име = current_lang_name, Тип, target_id, search_terms)
}


### 8.5. Function to Generate Match Timeline

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
  
  # Columnas extra para el timeline visual
  cols_extra_vacias <- tibble(
    equipo_canonico_mk = character(), tipo_evento = character(),
    jugadora_id = character(), jugadora_nombre = character(),
    jugadora_sub_id = character(), jugadora_sub_nombre = character()
  )
  
  # Step 1: Goals
  goles_data_raw <- goles_df_unificado %>% filter(id_partido == id_p)
  
  if (!is.null(goles_data_raw) && nrow(goles_data_raw) > 0) {
    goles_data <- goles_data_raw %>%
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
        }),
        tipo_evento = if_else(tipo == "Autogol", "autogol", "gol")
      ) %>% 
      select(
        minuto, icono = tipo, texto_evento,
        equipo_canonico_mk = equipo_acreditado_canonico,
        tipo_evento, jugadora_id = id, jugadora_nombre = PlayerName
      ) %>% 
      mutate(
        icono = recode(icono, "Normal" = "⚽", "Autogol" = "⚽"),
        jugadora_sub_id = NA_character_, jugadora_sub_nombre = NA_character_
      )
    lista_eventos[[length(lista_eventos) + 1]] <- goles_eventos
  }
  
  # Step 2: Cards
  tarjetas_data_raw <- tarjetas_df_unificado %>% filter(id_partido == id_p)
  
  if (!is.null(tarjetas_data_raw) && nrow(tarjetas_data_raw) > 0) {
    tarjetas_data <- tarjetas_data_raw %>%
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
        icono = if_else(tipo == "Amarilla", "🟨", "🟥"),
        tipo_evento = if_else(tipo == "Amarilla", "tarjeta_amarilla", "tarjeta_roja")
      ) %>% 
      select(
        minuto, icono, texto_evento,
        equipo_canonico_mk = equipo_canonico, tipo_evento,
        jugadora_id = id, jugadora_nombre = PlayerName
      ) %>%
      mutate(jugadora_sub_id = NA_character_, jugadora_sub_nombre = NA_character_)
    lista_eventos[[length(lista_eventos) + 1]] <- tarjetas_eventos
  }
  
  # Step 3: Substitutions
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
      tibble(
        minuto = cambio$minuto, icono = "🔄", texto_evento = texto_final,
        equipo_canonico_mk = nombre_equipo_mk, tipo_evento = "cambio",
        jugadora_id = id_entra %||% NA_character_, jugadora_nombre = nombre_entra_lang,
        jugadora_sub_id = id_sale %||% NA_character_, jugadora_sub_nombre = nombre_sale_lang
      )
    })
  }
  alineacion_partido <- apariciones_df %>% filter(id_partido == id_p)
  cambios_local_eventos <- procesar_cambios(resumen_partido$cambios_local, info_partido_canonico$local, filter(alineacion_partido, equipo == info_partido_canonico$local))
  cambios_visitante_eventos <- procesar_cambios(resumen_partido$cambios_visitante, info_partido_canonico$visitante, filter(alineacion_partido, equipo == info_partido_canonico$visitante))
  
  lista_eventos <- c(lista_eventos, list(cambios_local_eventos), list(cambios_visitante_eventos))
  lista_eventos_compacta <- purrr::compact(lista_eventos)
  if (length(lista_eventos_compacta) == 0) {
    return(tibble(
      minuto = integer(), icono = character(), texto_evento = character(),
      equipo_canonico_mk = character(), tipo_evento = character(),
      jugadora_id = character(), jugadora_nombre = character(),
      jugadora_sub_id = character(), jugadora_sub_nombre = character()
    ))
  }
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
    
    logo_local_src <- get_club_logo_path(partido$local, path_to_root = "..")
    logo_visitante_src <- get_club_logo_path(partido$visitante, path_to_root = "..")
    
    ruta_partido_html <- if(!is.na(partido$id_partido)) file.path(nombres_carpetas_relativos$partidos, paste0(partido$id_partido, ".html")) else "#"
    
    fila_local <- tags$div(class="match-row",
                           tags$div(class="team-info", tags$img(src = logo_local_src, class="logo"), tags$span(class="name", local_name)),
                           if(!is.na(partido$id_partido)) tags$span(class="score", partido$goles_local) else NULL
    )
    fila_visitante <- tags$div(class="match-row",
                               tags$div(class="team-info", tags$img(src = logo_visitante_src, class="logo"), tags$span(class="name", visitante_name)),
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
                          logo_src <- get_club_logo_path(fila$team, path_to_root = "..")
                          
                          estilo_borde <- ""
                          if (!is.null(estilos_comp)) {
                            regla_match <- estilos_comp$reglas %>% filter(puesto == fila$Pos)
                            if (nrow(regla_match) > 0) {
                              estilo_borde <- paste0("border-left: 3px solid ", regla_match[1, "color"], ";")
                            }
                          }
                          
                          tags$tr(
                            tags$td(style = estilo_borde, class="pos-cell", fila$Pos),
                            tags$td(class="logo-cell", tags$img(class="mini-logo", src = logo_src)),
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


