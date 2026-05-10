#### 8. HELPER FUNCTIONS AND TEMPLATE DEFINITIONS ####

# Authoritative map for Macedonian Cyrillic to Latin transliteration with diacritics.
# This should be used for all display-level transliterations.
MAP_CYR_TO_LAT_DIACRITICS <- c(
  "\u0430" = "a", "\u0431" = "b", "\u0432" = "v", "\u0433" = "g", "\u0434" = "d", "\u0453" = "gj", "\u0435" = "e",
  "\u0436" = "\u017e", "\u0437" = "z", "\u0455" = "dz", "\u0438" = "i", "\u0458" = "j", "\u043a" = "k", "\u043b" = "l",
  "\u0459" = "lj", "\u043c" = "m", "\u043d" = "n", "\u045a" = "nj", "\u043e" = "o", "\u043f" = "p", "\u0440" = "r",
  "\u0441" = "s", "\u0442" = "t", "\u045c" = "kj", "\u0443" = "u", "\u0444" = "f", "\u0445" = "h", "\u0446" = "c",
  "\u0447" = "\u010d", "\u045f" = "d\u017e", "\u0448" = "\u0161"
)

### 8.1. Text Formatting and Sanitization Functions

#' @title Normalize a name for search matching.
#' @description Applies a 4-step normalization pipeline: lowercase, Cyrillic-to-Latin
#' transliteration, diacritics removal, and digraph/double collapsing. The same
#' pipeline is replicated in JavaScript (normalizeForSearch) so that user input and
#' stored search terms converge to the same normalized form regardless of script.
#' @param nombre A string (Cyrillic, Latin, or mixed).
#' @return A single normalized lowercase ASCII string.
generar_terminos_busqueda <- function(nombre) {
  if (is.na(nombre) || nchar(trimws(nombre)) == 0) {
    return("")
  }

  # Step 1: Lowercase
  texto <- tolower(nombre)


  # Step 2: Cyrillic -> Latin (1:1 map)
  map_cyr_to_lat <- c(
    "\u0430" = "a", "\u0431" = "b", "\u0432" = "v", "\u0433" = "g", "\u0434" = "d",
    "\u0453" = "gj", "\u0435" = "e", "\u0436" = "z", "\u0437" = "z", "\u0455" = "dz",
    "\u0438" = "i", "\u0458" = "j", "\u043a" = "k", "\u043b" = "l", "\u0459" = "lj",
    "\u043c" = "m", "\u043d" = "n", "\u045a" = "nj", "\u043e" = "o", "\u043f" = "p",
    "\u0440" = "r", "\u0441" = "s", "\u0442" = "t", "\u045c" = "kj", "\u0443" = "u",
    "\u0444" = "f", "\u0445" = "h", "\u0446" = "c", "\u0447" = "c", "\u0448" = "s",
    "\u045f" = "dz"
  )
  texto <- str_replace_all(texto, map_cyr_to_lat)

  # Step 3: Remove diacritics
  map_diacritics <- c(
    "\u010d" = "c", "\u0161" = "s", "\u017e" = "z", "\u0111" = "d", "\u0107" = "c",
    "\u01f5" = "g", "\u1e31" = "k", "\u0144" = "n", "\u013a" = "l", "\u00f1" = "n",
    "\u00eb" = "e", "\u00e7" = "c", "\u00df" = "s",
    "\u00fc" = "u", "\u00f6" = "o", "\u00e4" = "a",
    "\u00e1" = "a", "\u00e9" = "e", "\u00ed" = "i", "\u00f3" = "o", "\u00fa" = "u",
    "\u00e0" = "a", "\u00e8" = "e", "\u00ec" = "i", "\u00f2" = "o", "\u00f9" = "u",
    "\u00e2" = "a", "\u00ea" = "e", "\u00ee" = "i", "\u00f4" = "o", "\u00fb" = "u"
  )
  texto <- str_replace_all(texto, map_diacritics)

  # Step 4: Collapse digraphs and doubles (longest first)
  # Trigraphs
  texto <- str_replace_all(texto, fixed("dzh"), "z")
  texto <- str_replace_all(texto, fixed("sch"), "s")
  texto <- str_replace_all(texto, fixed("tch"), "c")
  texto <- str_replace_all(texto, fixed("dgh"), "g")
  # Digraphs (Macedonian, Albanian, international)
  digraphs <- c(
    "xh" = "z", "sh" = "s", "ch" = "c", "zh" = "z", "gj" = "g", "kj" = "k",
    "nj" = "n", "lj" = "l", "dj" = "d", "dz" = "z", "dh" = "d", "th" = "t",
    "ah" = "a", "ph" = "f"
  )
  for (dg in names(digraphs)) {
    texto <- str_replace_all(texto, fixed(dg), digraphs[[dg]])
  }
  # Double consonants
  doubles <- c("ll", "ss", "rr", "tt", "ff", "nn", "mm", "pp", "bb", "dd", "gg", "cc", "zz")
  for (dbl in doubles) {
    texto <- str_replace_all(texto, fixed(dbl), substr(dbl, 1, 1))
  }

  return(texto)
}


#' @title Create a safe identifier for use in URLs and filenames.
#' @description Transliterates, converts to lowercase, replaces spaces, and removes invalid characters.
#' @param nombre The original string.
#' @return A sanitized string.
mapa_nombres_latinos <- character()

generar_id_seguro <- function(nombre) {
  # 8.1.10. "Flat" transliteration map for simple and readable URL IDs.
  map_id_plain <- c(
    "\u0430" = "a", "\u0431" = "b", "\u0432" = "v", "\u0433" = "g", "\u0434" = "d", "\u0453" = "g", "\u0435" = "e",
    "\u0436" = "z", "\u0437" = "z", "\u0455" = "dz", "\u0438" = "i", "\u0458" = "j", "\u043a" = "k", "\u043b" = "l",
    "\u0459" = "lj", "\u043c" = "m", "\u043d" = "n", "\u045a" = "n", "\u043e" = "o", "\u043f" = "p", "\u0440" = "r",
    "\u0441" = "s", "\u0442" = "t", "\u045c" = "kj", "\u0443" = "u", "\u0444" = "f", "\u0445" = "h", "\u0446" = "c",
    "\u0447" = "c", "\u045f" = "dz", "\u0448" = "s"
  )
  nombre_latin <- nombre
  if (exists("mapa_nombres_latinos") && !is.null(mapa_nombres_latinos) && length(mapa_nombres_latinos) > 0) {
    nombre_clave <- tolower(str_squish(nombre))
    matched <- mapa_nombres_latinos[nombre_clave]
    if (!is.null(names(matched))) {
      names(matched) <- NULL
    }
    nombre_latin <- ifelse(
      !is.na(matched) & nzchar(trimws(matched)),
      tolower(matched),
      str_replace_all(tolower(nombre), map_id_plain)
    )
  } else {
    nombre_latin <- str_replace_all(tolower(nombre), map_id_plain)
  }

  map_latin_diacritics <- c(
    "\u00e0" = "a", "\u00e1" = "a", "\u00e2" = "a", "\u00e3" = "a", "\u00e4" = "a", "\u00e5" = "a", "\u0101" = "a", "\u0103" = "a", "\u0105" = "a",
    "\u00e7" = "c", "\u0107" = "c", "\u010d" = "c", "\u010f" = "d", "\u0111" = "d",
    "\u00e8" = "e", "\u00e9" = "e", "\u00ea" = "e", "\u00eb" = "e", "\u0113" = "e", "\u0117" = "e", "\u0119" = "e",
    "\u0123" = "g", "\u011f" = "g", "\u01f5" = "g",
    "\u0127" = "h",
    "\u00ec" = "i", "\u00ed" = "i", "\u00ee" = "i", "\u00ef" = "i", "\u012b" = "i", "\u012f" = "i",
    "\u0137" = "k",
    "\u013a" = "l", "\u013c" = "l", "\u013e" = "l", "\u0142" = "l",
    "\u00f1" = "n", "\u0144" = "n", "\u0148" = "n", "\u0146" = "n",
    "\u00f2" = "o", "\u00f3" = "o", "\u00f4" = "o", "\u00f5" = "o", "\u00f6" = "o", "\u00f8" = "o", "\u014d" = "o", "\u0151" = "o", "\u0153" = "oe",
    "\u0155" = "r", "\u0159" = "r",
    "\u015b" = "s", "\u0161" = "s", "\u015f" = "s", "\u0219" = "s",
    "\u0165" = "t", "\u0163" = "t", "\u021b" = "t",
    "\u00f9" = "u", "\u00fa" = "u", "\u00fb" = "u", "\u00fc" = "u", "\u016b" = "u", "\u016f" = "u", "\u0171" = "u",
    "\u0177" = "w",
    "\u00fd" = "y", "\u00ff" = "y",
    "\u017e" = "z", "\u017a" = "z", "\u017bc" = "z"
  )
  nombre_latin <- str_replace_all(nombre_latin, map_latin_diacritics)
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
  # Defensive guard: team_name_mk must be scalar non-NA character
  if (is.null(team_name_mk) || length(team_name_mk) != 1 || is.na(team_name_mk)) {
    return(NA_character_)
  }

  # 8.1.11. Special case for "\u041c\u0430\u043a\u0435\u0434\u043e\u043d\u0438\u0458\u0430".
  if (team_name_mk == "\u041c\u0430\u043a\u0435\u0434\u043e\u043d\u0438\u0458\u0430") {
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


#' @title Standard normalization for string matching in lookups.
#' @description Replaces NBSP with normal spaces, collapses internal whitespace, and lowercases the string.
#' @param x The string to normalize.
#' @return A normalized string.
normalize_for_join <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x_chr <- as.character(x)
  # Replace NBSP with a standard space
  x_clean <- stringr::str_replace_all(x_chr, "\u00A0", " ")
  # Collapse internal whitespace and lowercase
  tolower(stringr::str_squish(x_clean))
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
  # 1. Si no hay mapa de conversi\u00f3n, devolver los datos originales sin cambios.
  if (is.null(mapa_df) || nrow(mapa_df) == 0) {
    return(data_input)
  }
  # 2. Si la entrada de datos est\u00e1 vac\u00eda (NULL, 0 filas, 0 longitud), devolverla.
  if (is.null(data_input) || (is.data.frame(data_input) && nrow(data_input) == 0) || (!is.data.frame(data_input) && length(data_input) == 0)) {
    return(data_input)
  }

  # (normalize_for_join is now a global utility function in this file)

  # --- LOGIC BRANCHING ---
  if (is.data.frame(data_input)) {
    # --- CASO 1: La entrada es un DATAFRAME ---
    df <- data_input
    for (col_name in intersect(columnas, names(df))) {
      # Crear una columna temporal con el nombre normalizado para el cruce.
      df <- df %>%
        mutate(.join_col_lower = normalize_for_join(.data[[col_name]])) %>%
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
      .join_col_lower = normalize_for_join(original_value)
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
crear_selector_idioma <- function() {
  tags$div(
    class = "language-selector",
    style = "text-align: right; margin-bottom: 15px; font-size: 0.9em;",
    tagList(
      lapply(seq_along(IDIOMAS_SOPORTADOS), function(i) {
        lang_code <- IDIOMAS_SOPORTADOS[i]
        lang_name <- textos[[lang_code]][["lang_name"]] %||% lang_code

        tag_element <- tags$a(
          href = "#",
          class = "lang-btn",
          `data-lang` = lang_code,
          onclick = sprintf("switchLanguage('%s'); return false;", lang_code),
          paste0("[ ", lang_name, " ]")
        )

        if (i < length(IDIOMAS_SOPORTADOS)) {
          tagList(tag_element, " ")
        } else {
          tag_element
        }
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
#' @param script_password The script tag for password protection.
#' @param current_page_id A string identifying the current page for the navbar (e.g., "home", "players").
#' @return A complete htmltools `html` object.
crear_pagina_html <- function(contenido_principal, titulo_pagina, path_to_assets = ".", script_password = NULL, current_page_id = "home") {
  # In the flat architecture, assets are always at ./assets/ from root pages,
  # or ../assets/ from subfolders. path_to_assets handles this.

  # Build multi-language search data-attributes as JSON
  search_results_json <- toJSON(setNames(
    lapply(IDIOMAS_SOPORTADOS, function(l) textos[[l]][["search_results_for"]] %||% "Search results for"),
    IDIOMAS_SOPORTADOS
  ), auto_unbox = TRUE)
  no_search_results_json <- toJSON(setNames(
    lapply(IDIOMAS_SOPORTADOS, function(l) textos[[l]][["no_search_results"]] %||% "No results"),
    IDIOMAS_SOPORTADOS
  ), auto_unbox = TRUE)
  search_prompt_json <- toJSON(setNames(
    lapply(IDIOMAS_SOPORTADOS, function(l) textos[[l]][["search_prompt"]] %||% "Enter at least 2 characters"),
    IDIOMAS_SOPORTADOS
  ), auto_unbox = TRUE)

  tags$html(
    lang = IDIOMAS_SOPORTADOS[1],
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
      tags$title(titulo_pagina),
      tags$link(rel = "icon", type = "image/png", href = file.path(path_to_assets, "favicon.png")),
      tags$link(rel = "stylesheet", href = file.path(path_to_assets, nombres_carpetas_relativos$assets, "style.css")),
      script_password
    ),
    tags$body(
      `data-search-results-title` = search_results_json,
      `data-no-search-results-msg` = no_search_results_json,
      `data-search-prompt-msg` = search_prompt_json,
      tags$div(
        class = "container",
        crear_selector_idioma(),

        # --- Header with title and nav ---
        tags$div(
          class = "header-main",
          tags$h1(tags$a(href = file.path(path_to_assets, "index.html"), style = "color: inherit; text-decoration: none;", t("site_title"))),
          crear_barra_navegacion(path_to_root = path_to_assets, current_page_id = current_page_id)
        ),
        tags$div(
          class = "search-container",
          tags$form(
            action = "#", onsubmit = "showSearchResults(); return false;",
            tags$input(type = "text", id = "search-input", class = "search-input", placeholder = t("search_placeholder")),
            tags$button(type = "submit", class = "search-button", t_html("search_button"))
          ),
          tags$div(id = "search-suggestions")
        ),
        tags$div(
          id = "main-content",
          contenido_principal
        )
      ),
      tags$script(defer = NA, src = file.path(path_to_assets, nombres_carpetas_relativos$assets, "script.js"))
    )
  )
}


#' @title Create the main navigation bar.
#' @description Generates the full <nav> element with dropdowns and hamburger icon.
#' @param path_to_lang_root Relative path to the current language root (e.g., '.').
#' @param current_page_id An identifier for the current page to set the 'active' class.
#' @return An htmltools `nav` object.
crear_barra_navegacion <- function(path_to_root = ".", current_page_id = "home") {
  crear_item_menu <- function(id, texto_tag, href) {
    tags$li(tags$a(class = if (id == current_page_id) "active" else "", href = href, texto_tag))
  }

  normalizar_texto_nav <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    stringr::str_to_lower(stringr::str_squish(x))
  }

  inferir_categoria_nav <- function(categoria_raw, nombre_comp) {
    normalizar_categoria_competicion(categoria_raw, nombre_comp)
  }

  es_categoria_mladinska <- function(categoria_raw, nombre_comp, nombre_completo_mk = "") {
    categoria <- inferir_categoria_nav(categoria_raw, nombre_comp)
    juventud_categoria <- c(
      "\u041c\u043b\u0430\u0434\u0438\u043d\u0446\u0438", "\u041a\u0430\u0434\u0435\u0442\u0438", "\u041f\u0438\u043e\u043d\u0435\u0440\u0438",
      "\u041c\u043b\u0430\u0434\u0438 \u041f\u0438\u043e\u043d\u0435\u0440\u0438", "\u041f\u043e\u043c\u0430\u043b\u0438 \u043f\u0435\u0442\u043b\u0438\u045a\u0430",
      "\u041f\u0435\u0442\u043b\u0438\u045a\u0430", "\u0414\u0435\u0442\u0441\u043a\u0430 \u043b\u0438\u0433\u0430"
    )
    base <- categoria %in% juventud_categoria

    texto_combo <- normalizar_texto_nav(paste(categoria_raw, nombre_comp, nombre_completo_mk))
    extra <- str_detect(texto_combo, regex(
      "\u0444\u0444\u043c|ffm|\u043f\u043e\u043c\u0430\u043b\u0438[[:space:]/-]*\u043f\u0435\u0442\u043b\u0438\u045a\u0430|\u043f\u043e\u043c\u0430\u043b\u0438[[:space:]/-]*\u043f\u0435\u0442\u043b\u0438\u043d\u0458\u0430|\u043f\u0435\u0442\u043b\u0438\u045a\u0430|petlinja|petlinj\u00eb|\u0434\u0435\u0442\u0441\u043a\u0430|detska|kids|child|children|\u043c\u043b\u0430\u0434\u0438\u043d|mladin|junior|youth|juven|\u043f\u0438\u043e\u043d\u0435\u0440|pioner|\u043a\u0430\u0434\u0435\u0442|kadet|cadet",
      ignore_case = TRUE
    ))

    base | extra
  }

  temporada_actual <- obtener_temporada_actual()

  # Preparar competiciones actuales para navegaci\u00f3n:
  # - excluir siempre \u0411\u0430\u0440\u0430\u0436
  # - excluir variantes de \u041a\u0443\u043f + \u0415\u043b\u0438\u043c\u0438\u043d\u0430\u0446\u0438\u0458\u0430
  # - excluir amistosos
  competiciones_activas <- competiciones_unicas_df %>%
    filter(
      competicion_temporada == temporada_actual,
      !str_detect(competicion_nombre, regex("\u043f\u0440\u0438\u0458\u0430\u0442\u0435\u043b\u0441\u043a\u0438", ignore_case = TRUE)),
      !str_detect(competicion_nombre, regex("\\b\u0431\u0430\u0440\u0430\u0436\\b", ignore_case = TRUE)),
      !(str_detect(competicion_nombre, regex("(\u043a\u0443\u043f|kup)", ignore_case = TRUE)) &
          str_detect(competicion_nombre, regex("(\u0435\u043b\u0438\u043c\u0438\u043d\u0430\u0446\u0438\u0458\u0430|eliminacija)", ignore_case = TRUE)))
    ) %>%
    mutate(
      categoria = str_squish(coalesce(as.character(categoria), "")),
      categoria_norm = normalizar_texto_nav(categoria),
      categoria_nav = inferir_categoria_nav(categoria, competicion_nombre),
      is_youth_nav = es_categoria_mladinska(categoria, competicion_nombre, nombre_completo_mk)
    )

  # Helpers para obtener los enlaces de las competiciones (multi-lang)
  crear_link_comp <- function(df) {
    if (nrow(df) == 0) {
      return(NULL)
    }
    map(1:nrow(df), function(i) {
      c_item <- df[i, ]
      tags$a(
        href = file.path(path_to_root, nombres_carpetas_relativos$competiciones, paste0(c_item$competicion_id, ".html")),
        comp_name_spans(c_item)
      )
    })
  }

  # 0. Cubos Estrictos (Buckets)
  comps_senior <- competiciones_activas %>%
    filter(str_detect(categoria_norm, "\u0441\u0435\u043d\u0438\u043e\u0440|senior"), !is_youth_nav)

  # Buckets are now more robust to Latin/Cyrillic and casing
  df_prva <- comps_senior %>% filter(str_detect(competicion_nombre, "^(?i)(\u041f\u0440\u0432\u0430|Prva)"))
  df_vtora <- comps_senior %>% filter(str_detect(competicion_nombre, "^(?i)(\u0412\u0442\u043e\u0440\u0430|Vtora)"))
  df_kup_nacional <- comps_senior %>% filter(str_detect(competicion_nombre, "^(?i)(\u041a\u0443\u043f|Kup) (\u043d\u0430|na) \u0420\u041c"))
  df_treta <- comps_senior %>% filter(str_detect(competicion_nombre, "^(?i)(\u0422\u0440\u0435\u0442\u0430|Treta)"))

  # Copas regionales empiezan por "\u041a\u0443\u043f" pero no son la nacional
  df_copas_regionales <- comps_senior %>% filter(str_detect(competicion_nombre, "^(?i)(\u041a\u0443\u043f|Kup)") & !str_detect(competicion_nombre, "^(?i)(\u041a\u0443\u043f|Kup) (\u043d\u0430|na) \u0420\u041c"))

  # Ligas municipales: todo lo \u0421\u0435\u043d\u0438\u043e\u0440 (Senior) que no sea Primera, Segunda, Tercera ni Copa (nacional o regional)
  opstinski_comps_df <- comps_senior %>%
    anti_join(bind_rows(df_prva, df_vtora, df_kup_nacional, df_treta, df_copas_regionales), by = "competicion_id") %>%
    mutate(
      # Use mk name as the base sorting name (always stable)
      raw_nombre = .data[[paste0("nombre_completo_mk")]],
      # Grouping label: remove season and extra qualifiers
      base_name_group = str_replace(raw_nombre, "\\s*\\(.*\\)", "") %>%
        str_replace("\\s*\\d{2}/\\d{2}$", "") %>%
        str_replace("(?i)\u0441\u0443\u043f\u0435\u0440\\s*", "") %>%
        trimws()
    ) %>%
    filter(!is.na(raw_nombre), nchar(base_name_group) > 0) %>%
    filter(!inferir_categoria_nav(categoria, competicion_nombre) %in% c(
      "\u041c\u043b\u0430\u0434\u0438\u043d\u0446\u0438", "\u041a\u0430\u0434\u0435\u0442\u0438", "\u041f\u0438\u043e\u043d\u0435\u0440\u0438",
      "\u041c\u043b\u0430\u0434\u0438 \u041f\u0438\u043e\u043d\u0435\u0440\u0438", "\u041f\u043e\u043c\u0430\u043b\u0438 \u043f\u0435\u0442\u043b\u0438\u045a\u0430",
      "\u041f\u0435\u0442\u043b\u0438\u045a\u0430", "\u0414\u0435\u0442\u0441\u043a\u0430 \u043b\u0438\u0433\u0430"
    ))

  comp_dropdown_items <- list()

  # 1. \u041f\u0440\u0432\u0430 \u043b\u0438\u0433\u0430
  comp_dropdown_items <- append(comp_dropdown_items, crear_link_comp(df_prva))

  # 2. \u0412\u0442\u043e\u0440\u0430 \u043b\u0438\u0433\u0430
  comp_dropdown_items <- append(comp_dropdown_items, crear_link_comp(df_vtora))

  # 3. \u041a\u0443\u043f na RM
  comp_dropdown_items <- append(comp_dropdown_items, crear_link_comp(df_kup_nacional))

  # 4. \u0422\u0440\u0435\u0442\u0430 \u043b\u0438\u0433\u0430 (Submen\u00fa)
  items_treta <- crear_link_comp(df_treta)
  if (length(items_treta) > 0) {
    comp_dropdown_items[[length(comp_dropdown_items) + 1]] <- tags$div(
      class = "dropdown-submenu",
      tags$a(href = "javascript:void(0)", class = "sub-dropbtn", tagList(t_html("nav.treta_liga"), " \u25B8")),
      tags$div(class = "dropdown-submenu-content", items_treta)
    )
  }

  # 5. \u041e\u043f\u0448\u0442\u0438\u043d\u0441\u043a\u0438 \u043b\u0438\u0433\u0438 (Dynamic sorting and grouping)
  items_opstinski <- list()

  if (nrow(opstinski_comps_df) > 0) {
    # Get groups, sorted alphabetically by base name (e.g. OFL Bitola, OFL Skopje)
    cat_summary_opstinski <- opstinski_comps_df %>%
      group_by(base_name_group) %>%
      summarise(n = n(), .groups = "drop") %>%
      arrange(base_name_group)

    for (i in seq_len(nrow(cat_summary_opstinski))) {
      current_group <- cat_summary_opstinski$base_name_group[i]
      group_size <- cat_summary_opstinski$n[i]

      group_data <- opstinski_comps_df %>%
        filter(base_name_group == current_group) %>%
        # Calculate priority within group
        mutate(
          prio = case_when(
            str_detect(competicion_nombre, "(?i)\u0441\u0443\u043f\u0435\u0440") ~ 1,
            str_detect(competicion_nombre, "(?i)(\u043a\u0443\u043f|kup)") ~ 2,
            str_detect(competicion_nombre, "(?i)\u043f\u0440\u0432\u0430") ~ 3,
            str_detect(competicion_nombre, "(?i)\u0432\u0442\u043e\u0440\u0430") ~ 4,
            str_detect(competicion_nombre, "(?i)\u0442\u0440\u0435\u0442\u0430") ~ 5,
            TRUE ~ 6
          )
        ) %>%
        arrange(prio, raw_nombre)

      if (group_size > 1) {
        # Multiple levels (e.g., Bitola -> A1, A2, B)

        # Determine translation key for the group header
        group_key <- case_when(
          str_detect(current_group, "\u0421\u043a\u043e\u043f\u0458\u0435|Skopje") ~ "nav.ofl_skopje",
          str_detect(current_group, "\u0411\u0438\u0442\u043e\u043b\u0430|Bitola|Manastir|\u04101|\u04102") ~ "nav.ofl_bitola",
          str_detect(current_group, "\u041f\u0440\u0438\u043b\u0435\u043f|Prilep") ~ "nav.ofl_prilep",
          str_detect(current_group, "\u041a\u0443\u043c\u0430\u043d\u043e\u0432\u043e|Kumanovo") ~ "nav.ofl_kumanovo",
          str_detect(current_group, "\u0412\u0435\u043b\u0435\u0441|Veles") ~ "nav.ofl_veles",
          str_detect(current_group, "\u0422\u0435\u0442\u043e\u0432\u043e|Tetovo") ~ "nav.ofl_tetovo",
          str_detect(current_group, "\u0413\u043e\u0441\u0442\u0438\u0432\u0430\u0440|Gostivar") ~ "nav.ofl_gostivar",
          str_detect(current_group, "\u0421\u0442\u0440\u0443\u0433\u0430|Struga") ~ "nav.ofl_struga",
          TRUE ~ NA_character_
        )

        header_spans <- if (!is.na(group_key)) {
          t_html(group_key)
        } else {
          tagList(lapply(IDIOMAS_SOPORTADOS, function(l) tags$span(class = paste0("lang-", l), current_group)))
        }

        sub_links <- map(seq_len(nrow(group_data)), function(j) {
          c_item <- group_data[j, ]
          tags$a(
            href = file.path(path_to_root, nombres_carpetas_relativos$competiciones, paste0(c_item$competicion_id, ".html")),
            comp_name_spans(c_item)
          )
        })

        items_opstinski[[length(items_opstinski) + 1]] <- tags$div(
          class = "dropdown-submenu",
          tags$a(href = "javascript:void(0)", class = "sub-dropbtn", tagList(header_spans, " \u25B8")),
          tags$div(class = "dropdown-submenu-content", sub_links)
        )
      } else {
        # Single league link
        c_item <- group_data[1, ]
        items_opstinski[[length(items_opstinski) + 1]] <- tags$a(
          href = file.path(path_to_root, nombres_carpetas_relativos$competiciones, paste0(c_item$competicion_id, ".html")),
          comp_name_spans(c_item)
        )
      }
    }
  }

  if (length(items_opstinski) > 0) {
    comp_dropdown_items[[length(comp_dropdown_items) + 1]] <- tags$div(
      class = "dropdown-submenu",
      tags$a(href = "javascript:void(0)", class = "sub-dropbtn", tagList(t_html("nav.opstinski"), " \u25B8")),
      tags$div(class = "dropdown-submenu-content", items_opstinski)
    )
  }

  # 6. \u041c\u043b\u0430\u0434\u0438\u043d\u0441\u043a\u0438 \u043a\u0430\u0442\u0435\u0433\u043e\u0440\u0438\u0438 (Oldest to youngest)
  # Keep youth buckets even if source category is noisy (e.g., petlinja tagged as senior).
  mladinski_comps_df <- competiciones_activas %>%
    filter(
      is_youth_nav |
        (
          !str_detect(categoria_norm, "\u0441\u0435\u043d\u0438\u043e\u0440|senior") &
            categoria != "" &
            !str_detect(categoria_norm, "^\u0434\u0440\u0443\u0433\u0438$|^other$")
        )
    ) %>%
    mutate(
      categoria = if_else(is_youth_nav, categoria_nav, categoria),
      max_age = case_when(
        categoria == "\u041c\u043b\u0430\u0434\u0438\u043d\u0446\u0438" ~ 19,
        categoria == "\u041a\u0430\u0434\u0435\u0442\u0438" ~ 18,
        categoria == "\u041f\u0438\u043e\u043d\u0435\u0440\u0438" ~ 17,
        categoria == "\u041c\u043b\u0430\u0434\u0438 \u041f\u0438\u043e\u043d\u0435\u0440\u0438" ~ 16,
        categoria == "\u041f\u0435\u0442\u043b\u0438\u045a\u0430" ~ 15,
        categoria == "\u041f\u043e\u043c\u0430\u043b\u0438 \u043f\u0435\u0442\u043b\u0438\u045a\u0430" ~ 14,
        categoria == "\u0414\u0435\u0442\u0441\u043a\u0430 \u043b\u0438\u0433\u0430" ~ 13,
        TRUE ~ max_age
      )

    )

  items_mladinski <- list()

  if (nrow(mladinski_comps_df) > 0) {
    cat_summary <- mladinski_comps_df %>%
      group_by(categoria) %>%
      summarise(max_age_cat = max(max_age, na.rm = TRUE), .groups = "drop") %>%
      filter(is.finite(max_age_cat)) %>%
      arrange(desc(max_age_cat))

    for (i in seq_len(nrow(cat_summary))) {
      current_cat <- cat_summary$categoria[i]
      current_max_age <- cat_summary$max_age_cat[i]

      # Label for category level
      prefix <- t("nav.sub_prefix")
      age_label <- paste0(prefix, current_max_age)

      cat_key <- case_when(
        current_cat == "\u041c\u043b\u0430\u0434\u0438\u043d\u0446\u0438" ~ "nav.mladinci_cat_base",
        current_cat == "\u041a\u0430\u0434\u0435\u0442\u0438" ~ "nav.kadeti_cat_base",
        current_cat == "\u041f\u0438\u043e\u043d\u0435\u0440\u0438" ~ "nav.pioneri_cat_base",
        current_cat == "\u041c\u043b\u0430\u0434\u0438 \u041f\u0438\u043e\u043d\u0435\u0440\u0438" ~ "nav.ppioneri_cat_base",
        current_cat == "\u041f\u0435\u0442\u043b\u0438\u045a\u0430" ~ "nav.petlinja_cat_base",
        current_cat == "\u041f\u043e\u043c\u0430\u043b\u0438 \u043f\u0435\u0442\u043b\u0438\u045a\u0430" ~ "nav.pomali_petlinja_cat_base",
        current_cat == "\u0414\u0435\u0442\u0441\u043a\u0430 \u043b\u0438\u0433\u0430" ~ "nav.detski_cat_base",
        TRUE ~ NA_character_
      )

      # Multi-language category label construction (e.g., "Youth (U19)")
      full_group_label <- category_label_spans(current_cat, current_max_age)

      # All competitions in this category (Leagues + Cups)
      cat_comps <- mladinski_comps_df %>%
        filter(categoria == current_cat) %>%
        mutate(
          prio = case_when(
            str_detect(competicion_nombre, "(?i)\u0441\u0443\u043f\u0435\u0440") ~ 1,
            str_detect(competicion_nombre, "(?i)(\u043a\u0443\u043f|kup)") ~ 2,
            str_detect(competicion_nombre, "(?i)\u043f\u0440\u0432\u0430") ~ 3,
            str_detect(competicion_nombre, "(?i)\u0432\u0442\u043e\u0440\u0430") ~ 4,
            str_detect(competicion_nombre, "(?i)\u0442\u0440\u0435\u0442\u0430") ~ 5,
            TRUE ~ 6
          )
        ) %>%
        arrange(prio, competicion_nombre)

      comp_links <- map(seq_len(nrow(cat_comps)), function(j) {
        c_item <- cat_comps[j, ]
        tags$a(
          href = file.path(path_to_root, nombres_carpetas_relativos$competiciones, paste0(c_item$competicion_id, ".html")),
          comp_name_spans(c_item)
        )
      })

      items_mladinski[[length(items_mladinski) + 1]] <- tags$div(
        class = "dropdown-submenu",
        tags$a(href = "javascript:void(0)", class = "sub-dropbtn", tagList(full_group_label, " \u25B8")),
        tags$div(class = "dropdown-submenu-content", comp_links)
      )
    }
  }

  if (length(items_mladinski) > 0) {
    comp_dropdown_items[[length(comp_dropdown_items) + 1]] <- tags$div(
      class = "dropdown-submenu",
      tags$a(href = "javascript:void(0)", class = "sub-dropbtn", tagList(t_html("nav.mladinski"), " \u25B8")),
      tags$div(class = "dropdown-submenu-content", items_mladinski)
    )
  }

  # 7. \u0410\u0440\u0445\u0438\u0432\u0430
  comp_dropdown_items <- append(comp_dropdown_items, list(tags$hr(), tags$a(href = file.path(path_to_root, paste0(nombres_archivos_traducidos$archive, ".html")), t_html("menu_archive"))))

  tags$nav(
    class = "navbar",
    tags$a(
      href = "javascript:void(0);", class = "hamburger-icon", id = "hamburger-icon",
      tags$span(class = "bar"), tags$span(class = "bar"), tags$span(class = "bar")
    ),
    tags$ul(
      class = "nav-links",
      tags$li(class = "close-btn-li", tags$a(href = "javascript:void(0);", id = "close-nav-btn", "\u00d7")),
      crear_item_menu("home", t_html("menu_home"), file.path(path_to_root, "index.html")),
      tags$li(
        class = "dropdown",
        tags$a(
          href = "javascript:void(0)", class = if (current_page_id == "competitions") "dropbtn active" else "dropbtn",
          t_html("menu_competitions"), HTML(" &#9662;")
        ),
        tags$div(class = "dropdown-content", comp_dropdown_items)
      ),
      # National team page disabled for this deployment.
      crear_item_menu("players", t_html("menu_players"), file.path(path_to_root, paste0(nombres_archivos_traducidos$players, ".html"))),
      crear_item_menu("teams", t_html("menu_teams"), file.path(path_to_root, paste0(nombres_archivos_traducidos$teams, ".html"))),
      crear_item_menu("about", t_html("menu_about"), file.path(path_to_root, paste0(nombres_archivos_traducidos$about, ".html")))
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

#' @title Format match minutes to handle added time (e.g., 451 \u2192 "45+1").
#' @param minutos A numeric vector of minutes.
#' @return A character vector with the formatted minutes.
formatear_minuto_partido <- function(minutos, html = FALSE) {
  sapply(minutos, function(minuto) {
    if (is.na(minuto) || !is.numeric(minuto)) {
      if (html) {
        return(as.character(t_html("match_timeline_after_match")))
      }
      return(t("match_timeline_after_match"))
    }
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

#' @title Calculate a float value for sorting minutes including injury time.
#' @description Converts encoded minutes like 451 (45+1) to 45.001 for correct sorting.
#' @param minuto A single numeric or integer minute value.
#' @return A numeric value for sorting.
calcular_minuto_sort <- function(minuto) {
  if (is.na(minuto) || !is.numeric(minuto)) return(999) # Send NAs/invalid to end
  
  # Decode logic consistent with formatear_minuto_partido
  if (minuto > 140 && nchar(as.character(minuto)) >= 3) {
    minuto_str <- as.character(minuto)
    base <- as.numeric(substr(minuto_str, 1, 2))
    added <- as.numeric(substr(minuto_str, 3, nchar(minuto_str)))
    return(base + added / 1000)
  }
  return(as.numeric(minuto))
}

#' @title Get the relative file path to a club team's logo.
#' @description Always returns a local path (no national team flag logic).
#'   Falls back to "NOLOGO.webp" if not found.
#' @param nombre_equipo_mk Team name in Macedonian (original_name).
#' @param path_to_root Relative path from the calling page to the docs/ root.
#' @return A string with the relative path to the logo file.
get_club_logo_path <- function(nombre_equipo_mk, path_to_root = "..") {
  nombre_archivo <- paste0(generar_id_seguro(nombre_equipo_mk), ".webp")
  if (!file.exists(file.path(RUTA_LOGOS_DESTINO, nombre_archivo))) {
    nombre_archivo <- "NOLOGO.webp"
  }
  file.path(path_to_root, nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, nombre_archivo)
}

#' @title Get an HTML img tag for a team logo.
#' @description For national teams, uses flag SVG from hatscripts.github.io.
#'   For clubs, uses local logo file. Falls back to "NOLOGO.webp".
#' @param nombre_equipo_mk Team name in Macedonian (original_name).
#' @param path_to_root Relative path from the calling page to the docs/ root.
#' @param css_class CSS class(es) for the img tag. "national-team-flag" is
#'   appended automatically for national teams.
#' @return An htmltools img tag.
get_logo_tag <- function(nombre_equipo_mk, path_to_root = "..", css_class = "team-logo") {
  iso_code <- get_national_team_iso(nombre_equipo_mk)
  if (!is.na(iso_code)) {
    flag_url <- paste0("https://hatscripts.github.io/circle-flags/flags/", iso_code, ".svg")
    tags$img(class = paste(css_class, "national-team-flag"), src = flag_url, alt = nombre_equipo_mk)
  } else {
    tags$img(class = css_class, src = get_club_logo_path(nombre_equipo_mk, path_to_root), alt = nombre_equipo_mk)
  }
}

#' @title Normalize competition category labels from source data.
#' @description Uses only the category field (not competition name heuristics).
#' @param categoria_raw Raw category label.
#' @return Normalized Macedonian category label.
normalizar_categoria_competicion <- function(categoria_raw, nombre_comp = "") {
  categoria_raw_chr <- str_squish(coalesce(as.character(categoria_raw), ""))
  nombre_comp_chr <- str_squish(coalesce(as.character(nombre_comp), ""))
  texto_combo <- str_to_lower(paste(categoria_raw_chr, nombre_comp_chr))

  case_when(
    str_detect(texto_combo, "(\u043f\u043e\u043c\u0430\u043b\u0438|pomali).{0,10}(\u043f\u0435\u0442\u043b\u0438|petli)|(\u043f\u0435\u0442\u043b\u0438|petli).{0,10}(\u043f\u043e\u043c\u0430\u043b\u0438|pomali)") ~ "\u041f\u043e\u043c\u0430\u043b\u0438 \u043f\u0435\u0442\u043b\u0438\u045a\u0430", # U15
    str_detect(texto_combo, "\u043f\u0435\u0442\u043b\u0438\u045a\u0430|petlinja|petlinj\u00eb") ~ "\u041f\u0435\u0442\u043b\u0438\u045a\u0430",
    
    # U16
    str_detect(texto_combo, "(\u043c\u043b\u0430\u0434\u0438|\u043f\u043e\u043c\u043b\u0430\u0434\u0438|mladi|young).{0,10}(\u043f\u0438\u043e\u043d\u0435\u0440|pioner)") ~ "\u041c\u043b\u0430\u0434\u0438 \u041f\u0438\u043e\u043d\u0435\u0440\u0438",
    
    # U17
    str_detect(texto_combo, "\u043f\u0438\u043e\u043d\u0435\u0440|pioner|pioneer") ~ "\u041f\u0438\u043e\u043d\u0435\u0440\u0438",
    
    # U18
    str_detect(texto_combo, "\u043a\u0430\u0434\u0435\u0442|kadet|cadet") ~ "\u041a\u0430\u0434\u0435\u0442\u0438",
    
    # U19
    str_detect(texto_combo, "\u043c\u043b\u0430\u0434\u0438\u043d|mladin|youth|juven") ~ "\u041c\u043b\u0430\u0434\u0438\u043d\u0446\u0438",
    
    # U13
    str_detect(texto_combo, "\u0434\u0435\u0442\u0441\u043a\u0430|kids?|child") ~ "\u0414\u0435\u0442\u0441\u043a\u0430 \u043b\u0438\u0433\u0430",
    
    # Senior
    str_detect(texto_combo, "\u0441\u0435\u043d\u0438\u043e\u0440|senior") ~ "\u0421\u0435\u043d\u0438\u043e\u0440\u0438",
    
    TRUE ~ categoria_raw_chr
  )
}

#' @title Check if competition uses cup round naming.
#' @param nombre_competicion Competition name.
#' @return TRUE when it is a cup competition and not an elimination stage marker.
es_competicion_copa <- function(nombre_competicion) {
  nombre_norm <- str_to_lower(str_squish(coalesce(as.character(nombre_competicion), "")))
  str_detect(nombre_norm, "\u043a\u0443\u043f|kup") & !str_detect(nombre_norm, "\u0435\u043b\u0438\u043c\u0438\u043d\u0430\u0446|eliminac")
}

#' @title Build cup round translation keys from round match counts.
#' @description For numeric rounds (1,2,3...), infer knockout labels by number
#'   of matches in each round.
#' @param partidos_comp_df Competition matches dataframe with `jornada`.
#' @return Named character vector: names are raw rounds, values are translation keys.
crear_mapa_claves_ronda_copa <- function(partidos_comp_df) {
  if (is.null(partidos_comp_df) || nrow(partidos_comp_df) == 0 || !"jornada" %in% names(partidos_comp_df)) {
    return(setNames(character(0), character(0)))
  }

  resumen_rondas <- partidos_comp_df %>%
    transmute(jornada = str_squish(as.character(jornada))) %>%
    filter(!is.na(jornada), jornada != "") %>%
    count(jornada, name = "n_partidos") %>%
    mutate(order_idx = match(jornada, ordenar_jornadas(jornada))) %>%
    arrange(order_idx)

  if (nrow(resumen_rondas) == 0) {
    return(setNames(character(0), character(0)))
  }

  round_keys <- rep(NA_character_, nrow(resumen_rondas))
  is_numeric_round <- str_detect(resumen_rondas$jornada, "^\\d+$")

  for (i in seq_len(nrow(resumen_rondas))) {
    if (!is_numeric_round[i]) {
      next
    }

    n_matches <- resumen_rondas$n_partidos[i]
    has_later_single_match <- FALSE
    if (i < nrow(resumen_rondas)) {
      has_later_single_match <- any(resumen_rondas$n_partidos[(i + 1):nrow(resumen_rondas)] == 1, na.rm = TRUE)
    }

    if (n_matches == 1) {
      round_keys[i] <- if (has_later_single_match) "cup_round_1_2" else "cup_round_final"
      next
    }

    equipos_estimados <- n_matches * 2
    cuadro_objetivo <- 2^ceiling(log2(equipos_estimados))

    round_keys[i] <- case_when(
      cuadro_objetivo >= 32 ~ "cup_round_1_16",
      cuadro_objetivo >= 16 ~ "cup_round_1_8",
      cuadro_objetivo >= 8 ~ "cup_round_1_4",
      cuadro_objetivo >= 4 ~ "cup_round_1_2",
      TRUE ~ "cup_round_final"
    )
  }

  setNames(round_keys, resumen_rondas$jornada)
}

#' @title Convert cup round key into translated label.
#' @param clave_jornada Translation key for a cup round.
#' @param fallback_jornada Raw round text when no key is available.
#' @return A translated html label or raw text.
etiqueta_jornada_por_clave <- function(clave_jornada, fallback_jornada) {
  if (!is.null(clave_jornada) && length(clave_jornada) > 0 && !is.na(clave_jornada) && nzchar(clave_jornada)) {
    return(t_html(clave_jornada))
  }

  fallback_chr <- str_squish(coalesce(as.character(fallback_jornada), ""))
  if (!nzchar(fallback_chr)) {
    return("")
  }
  fallback_chr
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
      str_detect(jornada, "\u0424$|\u0444$|final") ~ 7,
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
#' @return A tibble with columns: Ime, Tip, target_id, search_terms.
generar_search_entidad <- function(df, nombres_filtro, tipo_entidad, id_prefix) {
  if (length(nombres_filtro) == 0) {
    return(tibble("Ime" = character(0), "Tip" = character(0), target_id = character(0), search_terms = character(0)))
  }
  df %>%
    filter(original_name %in% nombres_filtro) %>%
    rowwise() %>%
    mutate(
      "Ime" = as.character(entity_name_spans(original_name)),
      "Tip" = as.character(t_html(tipo_entidad)),
      target_id = paste0(id_prefix, generar_id_seguro(original_name)),
      search_terms = {
        name_mk <- original_name
        # Use pick() as recommended to replace cur_data()
        current_row <- pick(everything())
        trans_cols <- intersect(paste0("translated_name_", IDIOMAS_SOPORTADOS), names(current_row))

        terms <- generar_terminos_busqueda(name_mk)
        for (col in trans_cols) {
          val <- current_row[[col]][1]
          if (!is.na(val) && val != "" && val != name_mk) {
            terms <- paste(terms, generar_terminos_busqueda(val))
          }
        }
        terms
      }
    ) %>%
    ungroup() %>%
    mutate(across(everything(), as.character)) %>%
    select("Ime", "Tip", target_id, search_terms)
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

  info_partido_canonico <- partidos_df %>%
    filter(id_partido == id_p) %>%
    head(1)
  es_partido_seleccion <- info_partido_canonico$es_partido_seleccion[1]
  if (is.na(es_partido_seleccion)) es_partido_seleccion <- FALSE

  mapa_jugadora_a_equipo <- apariciones_df %>%
    filter(id_partido == id_p) %>%
    distinct(id, equipo_canonico = equipo)

  crear_link_condicional <- function(spans, ruta_base, id_seguro, nombre_equipo_original_mk) {
    debe_ser_enlazable <- !es_partido_seleccion || (es_partido_seleccion && nombre_equipo_original_mk == "\u041c\u0430\u043a\u0435\u0434\u043e\u043d\u0438\u0458\u0430")
    if (debe_ser_enlazable && !is.na(id_seguro) && nchar(id_seguro) > 0) {
      # Return a tagList that can be converted to char
      return(tags$a(href = paste0(ruta_base, "/", id_seguro, ".html"), spans))
    } else {
      return(spans)
    }
  }

  # Columnas extra para el timeline visual
  cols_extra_vacias <- tibble(
    equipo_canonico_mk = character(), tipo_evento = character(),
    jugadora_id = character(), jugadora_nombre = character(),
    jugadora_sub_id = character(), jugadora_sub_nombre = character()
  )

  safe_value <- function(value, fallback = NA_character_) {
    value_chr <- as.character(value)
    value_chr <- value_chr[!is.na(value_chr)]
    value_chr <- if (length(value_chr) > 0) str_squish(value_chr[1]) else NA_character_

    if (!is.na(value_chr) && nzchar(value_chr) && tolower(value_chr) != "na") {
      return(value_chr)
    }

    fallback_chr <- as.character(fallback)
    fallback_chr <- fallback_chr[!is.na(fallback_chr)]
    fallback_chr <- if (length(fallback_chr) > 0) str_squish(fallback_chr[1]) else NA_character_

    if (is.na(fallback_chr) || !nzchar(fallback_chr) || tolower(fallback_chr) == "na") {
      return(NA_character_)
    }

    fallback_chr
  }

  normalize_team_name <- function(x) {
    x <- as.character(x)
    x <- tolower(x)
    x <- str_replace_all(x, "[[:space:]]+", " ")
    x <- str_trim(x)
    ifelse(is.na(x) | x == "", NA_character_, x)
  }

  local_norm <- normalize_team_name(info_partido_canonico$local[1])
  visitante_norm <- normalize_team_name(info_partido_canonico$visitante[1])

  equipo_acreditado_lookup <- entidades_lang_df %>%
    select(original_name, starts_with("translated_name_")) %>%
    tidyr::pivot_longer(cols = starts_with("translated_name_"), names_to = "lang", values_to = "team_name") %>%
    filter(!is.na(team_name) & team_name != "") %>%
    mutate(team_name_norm = normalize_team_name(team_name)) %>%
    distinct(team_name_norm, original_name)

  canonical_team_name <- function(team_name) {
    team_name <- as.character(team_name)
    if (length(team_name) == 0) return(character(0))

    team_norm <- normalize_team_name(team_name)
    canonical <- rep(NA_character_, length(team_name))

    for (i in seq_along(team_norm)) {
      if (is.na(team_norm[i])) {
        canonical[i] <- NA_character_
      } else {
        match_row <- equipo_acreditado_lookup$original_name[equipo_acreditado_lookup$team_name_norm == team_norm[i]]
        canonical[i] <- if (length(match_row) > 0) match_row[1] else team_name[i]
      }
    }

    canonical
  }

  get_player_row_with_fallback <- function(player_id, fallback_name = NA_character_) {
    player_id_safe <- safe_value(player_id, fallback = NA_character_)
    player_row <- tibble()

    if (!is.na(player_id_safe) && "id" %in% names(jugadoras_lang_df) && nrow(jugadoras_lang_df) > 0) {
      player_row <- jugadoras_lang_df %>%
        filter(id == player_id_safe) %>%
        head(1)
    }

    if (nrow(player_row) == 0) {
      fallback_label <- safe_value(fallback_name, fallback = player_id_safe)
      if (is.na(fallback_label)) fallback_label <- "???"

      player_row <- tibble(id = safe_value(player_id_safe, fallback = fallback_label))
      for (lang in IDIOMAS_SOPORTADOS) {
        player_row[[paste0("PlayerName_", lang)]] <- fallback_label
      }
    }

    player_row
  }

  get_player_name_lang <- function(player_id, lang, fallback = NA_character_) {
    player_id_safe <- safe_value(player_id, fallback = NA_character_)
    col_name <- paste0("PlayerName_", lang)

    if (!is.na(player_id_safe) && "id" %in% names(jugadoras_lang_df) && col_name %in% names(jugadoras_lang_df) && nrow(jugadoras_lang_df) > 0) {
      player_rows <- jugadoras_lang_df[jugadoras_lang_df$id == player_id_safe, , drop = FALSE]
      if (nrow(player_rows) > 0) {
        player_name <- safe_value(player_rows[[col_name]][1], fallback = NA_character_)
        if (!is.na(player_name)) {
          return(player_name)
        }
      }
    }

    safe_value(fallback, fallback = player_id_safe)
  }

  # Step 1: Goals
  goles_data_raw <- goles_df_unificado %>% filter(id_partido == id_p)

  if (!is.null(goles_data_raw) && nrow(goles_data_raw) > 0) {
    goles_data <- goles_data_raw %>%
      left_join(mapa_jugadora_a_equipo, by = "id", relationship = "many-to-many") %>%
      mutate(
        equipo_acreditado_canonico = {
          equipo_acreditado_coalesce <- coalesce(
            equipo_canonico,
            if ("equipo" %in% names(.)) equipo else NA_character_,
            equipo_acreditado
          )
          equipo_acreditado_coalesce <- canonical_team_name(equipo_acreditado_coalesce)
          ea_norm <- normalize_team_name(equipo_acreditado_coalesce)
          local_raw_norm <- normalize_team_name(resumen_partido$partido_info$local)
          visitante_raw_norm <- normalize_team_name(resumen_partido$partido_info$visitante)
          case_when(
            equipo_acreditado_coalesce == resumen_partido$partido_info$local ~ info_partido_canonico$local,
            equipo_acreditado_coalesce == resumen_partido$partido_info$visitante ~ info_partido_canonico$visitante,
            ea_norm == local_raw_norm ~ info_partido_canonico$local,
            ea_norm == visitante_raw_norm ~ info_partido_canonico$visitante,
            ea_norm == local_norm ~ info_partido_canonico$local,
            ea_norm == visitante_norm ~ info_partido_canonico$visitante,
            TRUE ~ equipo_acreditado_coalesce
          )
        }
      )

    goles_eventos <- map_df(1:nrow(goles_data), function(idx) {
      gol <- goles_data[idx, ]

      # Prepare spans
      fallback_goal_name <- safe_value(gol$jugadora, fallback = gol$id)
      player_row <- get_player_row_with_fallback(gol$id, fallback_goal_name)
      j_spans <- player_name_spans_from_row(player_row)

      texto_evento_spans <- tagList(
        lapply(IDIOMAS_SOPORTADOS, function(lang) {
          key <- if (gol$tipo == "Autogol") "match_timeline_own_goal" else "match_timeline_goal"
          patt <- textos[[lang]][[key]] %||% key

          # Lookup localized values
          j_val <- get_player_name_lang(gol$id, lang, fallback = fallback_goal_name)
          ej_val <- safe_value(
            (entidades_lang_df %>% filter(original_name == gol$equipo_canonico) %>% pull(paste0("translated_name_", lang)))[1],
            fallback = gol$equipo_canonico
          )
          ea_val <- safe_value(
            (entidades_lang_df %>% filter(original_name == gol$equipo_acreditado_canonico) %>% pull(paste0("translated_name_", lang)))[1],
            fallback = gol$equipo_acreditado_canonico
          )

          link_j_loc <- as.character(crear_link_condicional(j_val, path_jugadoras, gol$id, gol$equipo_canonico))
          link_ej_loc <- as.character(crear_link_condicional(ej_val, path_timovi, generar_id_seguro(gol$equipo_canonico), gol$equipo_canonico))
          link_ea_loc <- as.character(crear_link_condicional(ea_val, path_timovi, generar_id_seguro(gol$equipo_acreditado_canonico), gol$equipo_acreditado_canonico))

          content <- if (gol$tipo == "Autogol") sprintf(patt, link_j_loc, link_ej_loc, link_ea_loc) else sprintf(patt, link_j_loc, link_ea_loc)
          tags$span(class = paste0("lang-", lang), HTML(content))
        })
      )

      tibble(
        minuto = gol$minuto,
        icono = if (gol$tipo == "Autogol") "\u26bd" else "\u26bd",
        texto_evento = as.character(texto_evento_spans),
        equipo_canonico_mk = gol$equipo_acreditado_canonico,
        tipo_evento = if (gol$tipo == "Autogol") "autogol" else "gol",
        jugadora_id = gol$id,
        jugadora_nombre = as.character(j_spans),
        jugadora_sub_id = NA_character_,
        jugadora_sub_nombre = NA_character_
      )
    })
    lista_eventos[[length(lista_eventos) + 1]] <- goles_eventos
  }

  # Step 2: Cards
  tarjetas_data_raw <- tarjetas_df_unificado %>% filter(id_partido == id_p)

  if (!is.null(tarjetas_data_raw) && nrow(tarjetas_data_raw) > 0) {
    tarjetas_data <- tarjetas_data_raw %>%
      left_join(mapa_jugadora_a_equipo, by = "id", relationship = "many-to-many") %>%
      left_join(jugadoras_lang_df, by = "id") %>%
      left_join(entidades_lang_df, by = c("equipo_canonico" = "original_name"))

    tarjetas_eventos <- map_df(1:nrow(tarjetas_data), function(idx) {
      card <- tarjetas_data[idx, ]

      # Prepare spans
      fallback_card_name <- safe_value(card$jugadora, fallback = card$id)
      player_row <- get_player_row_with_fallback(card$id, fallback_card_name)
      j_spans <- player_name_spans_from_row(player_row)
      e_spans <- entity_name_spans(card$equipo_canonico)

      texto_evento_spans <- tagList(
        lapply(IDIOMAS_SOPORTADOS, function(lang) {
          key <- "match_timeline_card"
          patt <- textos[[lang]][[key]] %||% key

          # Lookup localized values
          j_val <- get_player_name_lang(card$id, lang, fallback = fallback_card_name)
          e_val <- safe_value(
            (entidades_lang_df %>% filter(original_name == card$equipo_canonico) %>% pull(paste0("translated_name_", lang)))[1],
            fallback = card$equipo_canonico
          )

          link_j_loc <- as.character(crear_link_condicional(j_val, path_jugadoras, card$id, card$equipo_canonico))
          link_e_loc <- as.character(crear_link_condicional(e_val, path_timovi, generar_id_seguro(card$equipo_canonico), card$equipo_canonico))

          content <- sprintf(patt, link_j_loc, link_e_loc)
          tags$span(class = paste0("lang-", lang), HTML(content))
        })
      )

      tibble(
        minuto = card$minuto,
        icono = if (card$tipo == "Amarilla") as.character(tags$span(class = "card-yellow")) else as.character(tags$span(class = "card-red")),
        texto_evento = as.character(texto_evento_spans),
        equipo_canonico_mk = card$equipo_canonico,
        tipo_evento = if (card$tipo == "Amarilla") "tarjeta_amarilla" else "tarjeta_roja",
        jugadora_id = card$id,
        jugadora_nombre = as.character(j_spans),
        jugadora_sub_id = NA_character_,
        jugadora_sub_nombre = NA_character_
      )
    })
    lista_eventos[[length(lista_eventos) + 1]] <- tarjetas_eventos
  }

  # Step 3: Substitutions
  procesar_cambios <- function(cambios_df, nombre_equipo_mk, alineacion_equipo) {
    if (is.null(cambios_df) || nrow(cambios_df) == 0 || is.null(alineacion_equipo) || nrow(alineacion_equipo) == 0) {
      return(NULL)
    }
    map_df(1:nrow(cambios_df), function(i) {
      cambio <- cambios_df[i, ]
      match_info <- str_match(cambio$texto, "Entra (.*?) \\((\\d+)\\) por (.*?) \\((\\d+)\\)")
      if (is.na(match_info[1, 1])) {
        return(NULL)
      }
      nombre_entra_raw <- match_info[1, 2]
      dorsal_entra <- as.numeric(match_info[1, 3])
      nombre_sale_raw <- match_info[1, 4]
      dorsal_sale <- as.numeric(match_info[1, 5])

      id_entra <- (alineacion_equipo %>% filter(dorsal == dorsal_entra))$id[1]
      id_sale <- (alineacion_equipo %>% filter(dorsal == dorsal_sale))$id[1]

      # Prepare spans for names
      entra_spans <- player_name_spans_from_row(get_player_row_with_fallback(id_entra, nombre_entra_raw))
      sale_spans <- player_name_spans_from_row(get_player_row_with_fallback(id_sale, nombre_sale_raw))
      eq_spans <- entity_name_spans(nombre_equipo_mk)

      texto_evento_spans <- tagList(
        lapply(IDIOMAS_SOPORTADOS, function(lang) {
          key <- "match_timeline_substitution"
          patt <- textos[[lang]][[key]] %||% key

          # Lookup localized values
          entra_val <- get_player_name_lang(id_entra, lang, fallback = nombre_entra_raw)
          sale_val <- get_player_name_lang(id_sale, lang, fallback = nombre_sale_raw)
          eq_val <- safe_value(
            (entidades_lang_df %>% filter(original_name == nombre_equipo_mk) %>% pull(paste0("translated_name_", lang)))[1],
            fallback = nombre_equipo_mk
          )

          link_entra <- as.character(crear_link_condicional(entra_val, path_jugadoras, id_entra, nombre_equipo_mk))
          link_sale <- as.character(crear_link_condicional(sale_val, path_jugadoras, id_sale, nombre_equipo_mk))
          link_equipo <- as.character(crear_link_condicional(eq_val, path_timovi, generar_id_seguro(nombre_equipo_mk), nombre_equipo_mk))

          content <- sprintf(patt, link_equipo, link_entra, dorsal_entra, link_sale, dorsal_sale)
          tags$span(class = paste0("lang-", lang), HTML(content))
        })
      )

      tibble(
        minuto = cambio$minuto,
        icono = "\U0001F504",
        texto_evento = as.character(texto_evento_spans),
        equipo_canonico_mk = nombre_equipo_mk,
        tipo_evento = "cambio",
        jugadora_id = id_entra %||% NA_character_,
        jugadora_nombre = as.character(entra_spans),
        jugadora_sub_id = id_sale %||% NA_character_,
        jugadora_sub_nombre = as.character(sale_spans)
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
  bind_rows(lista_eventos_compacta) %>%
    filter(!is.na(minuto)) %>%
    mutate(
      equipo_canonico_norm = normalize_team_name(equipo_canonico_mk),
      es_local = case_when(
        !is.na(equipo_canonico_norm) & equipo_canonico_norm == local_norm ~ TRUE,
        !is.na(equipo_canonico_norm) & equipo_canonico_norm == visitante_norm ~ FALSE,
        TRUE ~ NA
      )
    ) %>%
    select(-equipo_canonico_norm) %>%
    mutate(minuto_sort = map_dbl(minuto, calcular_minuto_sort)) %>%
    arrange(minuto_sort) %>%
    select(-minuto_sort)
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
#' @param path_to_root Relative path to the assets (e.g., '.' for index, '..' for subfolders).
#' @return An htmltools `div` object with the complete component.
crear_bloque_resultados_competicion <- function(comp_info, partidos_jornada_df, comp_nombre_lang, entidades_df_lang, ancho_ficha_px, path_to_root = "..") {
  # Ruta a la p\u00e1gina de MEN\u00da de la competici\u00f3n (para el t\u00edtulo principal)
  ruta_competicion_completa <- file.path(nombres_carpetas_relativos$competiciones, paste0(comp_info$competicion_id, ".html"))

  is_cup_comp <- es_competicion_copa(comp_info$competicion_nombre)
  mapa_claves_ronda_copa <- if (is_cup_comp) {
    partidos_df %>%
      filter(
        competicion_nombre == comp_info$competicion_nombre,
        competicion_temporada == comp_info$competicion_temporada
      ) %>%
      crear_mapa_claves_ronda_copa()
  } else {
    setNames(character(0), character(0))
  }

  # \u00a1AQU\u00cd EST\u00c1 LA CORRECCI\u00d3N CLAVE!
  # Nueva ruta que apunta directamente a la p\u00e1gina de CALENDARIO (Raspored)
  ruta_raspored <- file.path(
    nombres_carpetas_relativos$competiciones,
    paste0(comp_info$competicion_id, "_", nombres_archivos_traducidos$partidos, ".html")
  )

  # Helper to create each individual match block (vertical layout)
  crear_item_partido_vertical <- function(partido) {
    local_name <- entidades_df_lang$current_lang_name[match(partido$local, entidades_df_lang$original_name)]
    visitante_name <- entidades_df_lang$current_lang_name[match(partido$visitante, entidades_df_lang$original_name)]

    logo_local_src <- get_club_logo_path(partido$local, path_to_root = path_to_root)
    logo_visitante_src <- get_club_logo_path(partido$visitante, path_to_root = path_to_root)

    is_cancelled <- isTRUE(partido$es_cancelado)
    is_official <- isTRUE(partido$es_resultado_oficial)
    # Cancelled-only matches are not clickable; official results (even if also cancelled) link to their profile
    has_link <- !is.na(partido$id_partido) && (!is_cancelled || is_official)
    ruta_partido_html <- if (has_link) file.path(nombres_carpetas_relativos$partidos, paste0(partido$id_partido, ".html")) else "javascript:void(0)"

    # Show scores for played matches and official results (with asterisk)
    show_score <- has_link && !is.na(partido$goles_local) && !is.na(partido$goles_visitante)
    score_suffix <- if (is_official) "*" else ""

    fila_local <- tags$div(
      class = "match-row",
      tags$div(class = "team-info", tags$img(src = logo_local_src, class = "logo"), tags$span(class = "name", entity_name_spans(partido$local))),
      if (show_score) tags$span(class = "score", paste0(partido$goles_local, score_suffix)) else NULL
    )
    fila_visitante <- tags$div(
      class = "match-row",
      tags$div(class = "team-info", tags$img(src = logo_visitante_src, class = "logo"), tags$span(class = "name", entity_name_spans(partido$visitante))),
      if (show_score) tags$span(class = "score", paste0(partido$goles_visitante, score_suffix)) else NULL
    )

    tags$a(
      class = case_when(
        is_cancelled && !is_official ~ "match-block cancelled",
        is.na(partido$id_partido) ~ "match-block placeholder",
        TRUE ~ "match-block"
      ),
      href = ruta_partido_html,
      style = paste0("width: ", ancho_ficha_px, "px;"),
      tags$div(
        class = "match-round",
        if (is_cup_comp) {
          clave_jornada_copa <- unname(mapa_claves_ronda_copa[str_squish(as.character(partido$jornada))])
          etiqueta_jornada_por_clave(clave_jornada_copa, partido$jornada)
        } else {
          tagList(t_html("round_prefix_short"), " ", partido$jornada)
        }
      ),
      tags$div(
        class = "match-content",
        fila_local,
        if (is_cancelled && !is_official) {
          tags$span(class = "score-separator-cancelled", t_html("match_cancelled"))
        } else if (is.na(partido$id_partido)) {
          tags$span(class = "score-separator-placeholder", "-")
        } else {
          tags$span(class = "score-separator", "-")
        },
        fila_visitante
      )
    )
  }

  # Main block structure
  tags$div(
    class = "competition-results-block",
    tags$div(
      class = "results-header",
      # El t\u00edtulo principal sigue apuntando a la ruta del MEN\u00da
      tags$a(
        href = ruta_competicion_completa, class = "competition-title-link",
        tags$h2(class = "competition-title", comp_name_spans(comp_info))
      ),
      # El enlace "\u0421\u0438\u0442\u0435 \u0440\u0435\u0437\u0443\u043b\u0442\u0430\u0442\u0438" ahora apunta a la nueva ruta del CALENDARIO
      tags$a(href = ruta_raspored, class = "results-link", tagList(t_html("all_results_link"), " >"))
    ),
    tags$hr(class = "separator"),
    tags$div(
      class = "results-scroll-container",
      tags$div(
        class = "results-row",
        map(1:nrow(partidos_jornada_df), ~ crear_item_partido_vertical(partidos_jornada_df[.x, ]))
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
#' @param path_to_root Relative path to the assets.
#' @return An htmltools `div` object for the entire column (header + table).
crear_tabla_clasificacion_portada <- function(comp_info, stats_clasificacion_por_comp_df, entidades_df_lang, abreviaturas_lang_df, estilos_data, comp_nombre_lang, path_to_root = "..") {
  clasificacion_comp <- stats_clasificacion_por_comp_df %>%
    filter(competicion_id == comp_info$competicion_id)

  if (nrow(clasificacion_comp) == 0) {
    return(NULL)
  }

  clasificacion_final_df <- clasificacion_comp %>%
    left_join(entidades_df_lang, by = c("team" = "original_name"))

  # Do not join the full abbreviations table here because it contains
  # multiple language rows per team, which would duplicate each standing row.
  # Abbreviations are rendered per-language later in the table body.

  clave_estilo_comp <- paste(comp_info$competicion_nombre, comp_info$competicion_temporada)
  estilos_comp <- estilos_data[[clave_estilo_comp]]

  # \u00a1NUEVO! Ruta a la p\u00e1gina de la TABLA COMPLETA de la competici\u00f3n.
  ruta_tabla_completa <- file.path(
    nombres_carpetas_relativos$competiciones,
    paste0(comp_info$competicion_id, "_", nombres_archivos_traducidos$clasificacion, ".html")
  )

  # El contenedor principal ahora incluye el t\u00edtulo y la tabla.
  tags$div(
    class = "mini-standings-container",
    # \u00a1NUEVO! T\u00edtulo clicable de la competici\u00f3n.
    tags$a(
      href = ruta_tabla_completa, class = "mini-standings-title-link",
      tags$h4(class = "mini-standings-title", comp_name_spans(comp_info))
    ),

    # La tabla existente, sin cambios.
    tags$table(
      class = "mini-standings-table",
      tags$thead(tags$tr(
        tags$th(t_html("mini_standings_pos")),
        tags$th(t_html("mini_standings_team"), colspan = "2"),
        tags$th(t_html("mini_standings_p")),
        tags$th(t_html("mini_standings_pts"))
      )),
      tags$tbody(
        map(1:nrow(clasificacion_final_df), function(i) {
          fila <- clasificacion_final_df[i, ]
          logo_src <- get_club_logo_path(fila$team, path_to_root = path_to_root)

          estilo_borde <- ""
          if (!is.null(estilos_comp)) {
            regla_match <- estilos_comp$reglas %>% filter(puesto == fila$Pos)
            if (nrow(regla_match) > 0) {
              estilo_borde <- paste0("border-left: 3px solid ", regla_match[1, "color"], ";")
            }
          }

          tags$tr(
            tags$td(style = estilo_borde, class = "pos-cell", fila$Pos),
            tags$td(class = "logo-cell", tags$img(class = "mini-logo", src = logo_src)),
            tags$td(class = "abbr-cell", tagList(lapply(IDIOMAS_SOPORTADOS, function(l) {
              abbr <- if (!is.null(abreviaturas_lang_df) && nrow(abreviaturas_lang_df) > 0) {
                abreviaturas_lang_df %>%
                  filter(original_mk == fila$team, lang == l) %>%
                  pull(abbreviation) %>%
                  first()
              } else {
                NULL
              }
              if (is.null(abbr) || is.na(abbr)) {
                abbr <- toupper(substr(get_entity_name(fila$team, l), 1, 3))
              }
              tags$span(class = paste0("lang-", l), abbr)
            }))),
            tags$td(class = "pj-cell", fila$P),
            tags$td(class = "pts-cell", fila$Pts)
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
  tryCatch(
    {
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
    },
    error = function(e) {
      warning(paste("Error reading or processing the translations file:", e$message))
    }
  )
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
  if (is.null(key) || is.na(key) || !is.character(key) || length(key) != 1) {
    warning("Invalid translation key: NA or non-scalar, returning empty string.")
    return("")
  }

  if (!exists("textos") || is.null(textos) || length(textos) == 0 || !is.list(textos)) {
    warning("Translation dictionary 'textos' is missing or invalid.")
    return(key)
  }

  if (!idioma_actual %in% names(textos) || is.null(textos[[idioma_actual]]) || !is.list(textos[[idioma_actual]])) {
    warning(paste("Current language not available in textos:", idioma_actual))
    if (IDIOMAS_SOPORTADOS[1] %in% names(textos) && !is.null(textos[[IDIOMAS_SOPORTADOS[1]]])) {
      traduccion_fallback <- textos[[IDIOMAS_SOPORTADOS[1]]][[key]]
      return(ifelse(is.null(traduccion_fallback), key, traduccion_fallback))
    }
    return(key)
  }

  traduccion <- textos[[idioma_actual]][[key]]
  if (is.null(traduccion)) {
    traduccion_fallback <- NULL
    if (IDIOMAS_SOPORTADOS[1] %in% names(textos) && !is.null(textos[[IDIOMAS_SOPORTADOS[1]]])) {
      traduccion_fallback <- textos[[IDIOMAS_SOPORTADOS[1]]][[key]]
    }
    if (is.null(traduccion_fallback)) {
      warning(paste("Translation key not found in any language:", key))
      return(key)
    }
    return(traduccion_fallback)
  }
  return(traduccion)
}

#' @title Generate multi-language HTML spans for a UI translation key.
#' @description For each supported language, outputs a <span class="lang-XX">text</span>.
#' CSS rules hide/show the correct language based on html[lang].
#' @param key The translation key.
#' @return An htmltools tagList with one span per language.
t_html <- function(key) {
  tagList(
    lapply(IDIOMAS_SOPORTADOS, function(lang) {
      texto <- textos[[lang]][[key]]
      if (is.null(texto)) texto <- key
      tags$span(class = paste0("lang-", lang), HTML(texto))
    })
  )
}

#' @title Generate multi-language HTML spans for an entity name.
#' @description Looks up the translated name from entidades_maestro_df for each language.
#' @param original_name_mk The original Macedonian name of the entity.
#' @return An htmltools tagList with one span per language.
entity_name_spans <- function(original_name_mk) {
  tagList(
    lapply(IDIOMAS_SOPORTADOS, function(lang) {
      col <- paste0("translated_name_", lang)
      # Try exact match first
      nombre <- entidades_maestro_df[[col]][entidades_maestro_df$original_name == original_name_mk]
      if (length(nombre) == 0 || is.na(nombre[1])) {
        # Try normalized match (collapse spaces, replace NBSP, lowercase)
        if (is.null(original_name_mk) || is.na(original_name_mk)) {
          nombre <- original_name_mk
        } else {
          norm_input <- tolower(str_squish(str_replace_all(as.character(original_name_mk), "\u00A0", " ")))
          normalized_names <- tolower(str_squish(str_replace_all(as.character(entidades_maestro_df$original_name), "\u00A0", " ")))
          idx <- match(norm_input, normalized_names)
          if (!is.na(idx) && length(idx) > 0) {
            candidate <- entidades_maestro_df[[col]][idx]
            if (!is.null(candidate) && !is.na(candidate) && nzchar(as.character(candidate))) {
              nombre <- candidate
            } else {
              nombre <- original_name_mk
            }
          } else {
            nombre <- original_name_mk
          }
        }
      }
      tags$span(class = paste0("lang-", lang), nombre[1])
    })
  )
}

#' @title Generate multi-language HTML spans for a player name.
#' @description Looks up the player name from jugadoras_stats_df for each language.
#' @param player_id The player ID.
#' @return An htmltools tagList with one span per language.
player_name_spans <- function(player_id) {
  # Ensure player_id is a scalar string/number
  target_id <- as.character(unlist(player_id)[1])

  tagList(
    lapply(IDIOMAS_SOPORTADOS, function(lang) {
      col <- paste0("PlayerName_", lang)

      # Use match() for safe and fast lookup
      idx <- match(target_id, jugadoras_stats_df$id)
      nombre <- if (!is.na(idx)) jugadoras_stats_df[[col]][idx] else NA

      if (is.null(nombre) || is.na(nombre)) nombre <- target_id
      tags$span(class = paste0("lang-", lang), nombre)
    })
  )
}

#' @title Generate multi-language HTML spans for a player name (vectorized lookup from df row).
#' @description Given a row from jugadoras_stats_df, generates multi-lang spans.
#' @param jugadora_row A single-row dataframe with PlayerName_XX columns.
#' @return An htmltools tagList with one span per language.
player_name_spans_from_row <- function(jugadora_row) {
  if (is.null(jugadora_row) || nrow(jugadora_row) == 0) {
    jugadora_row <- tibble(id = NA_character_)
  }

  id_fallback <- if ("id" %in% names(jugadora_row) && nrow(jugadora_row) > 0) {
    as.character(jugadora_row[["id"]][1])
  } else {
    NA_character_
  }
  id_fallback <- if (!is.na(id_fallback)) str_squish(id_fallback) else NA_character_

  tagList(
    lapply(IDIOMAS_SOPORTADOS, function(lang) {
      col <- paste0("PlayerName_", lang)
      nombre <- if (col %in% names(jugadora_row) && nrow(jugadora_row) > 0) {
        as.character(jugadora_row[[col]][1])
      } else {
        NA_character_
      }

      if (length(nombre) == 0) {
        nombre <- NA_character_
      }

      nombre <- if (!is.na(nombre)) str_squish(nombre) else NA_character_
      if (is.na(nombre) || !nzchar(nombre) || tolower(nombre) == "na") {
        nombre <- if (!is.na(id_fallback) && nzchar(id_fallback)) id_fallback else "???"
      }

      tags$span(class = paste0("lang-", lang), nombre)
    })
  )
}

#' @title Generate multi-language HTML spans for a competition name.
#' @description Uses competiciones_unicas_df columns nombre_completo_XX.
#' @param comp_row A single-row dataframe from competiciones_unicas_df.
#' @return An htmltools tagList with one span per language.
#' @title Generate multi-language HTML spans for a competition name.
#' @description Uses competiciones_unicas_df columns nombre_completo_XX.
#' @param comp_row A single-row dataframe from competiciones_unicas_df.
#' @param season_override Optional replacement for the season string (e.g., '24/25' instead of '25/26').
#' @return An htmltools tagList with one span per language.
comp_name_spans <- function(comp_row, season_override = NULL) {
  tagList(
    lapply(IDIOMAS_SOPORTADOS, function(lang) {
      col <- paste0("nombre_completo_", lang)
      nombre <- if (col %in% names(comp_row)) comp_row[[col]][1] else NA
      comp_name_base <- if ("competicion_nombre" %in% names(comp_row)) comp_row[["competicion_nombre"]][1] else "???"
      if (is.null(nombre) || (length(nombre) == 1 && is.na(nombre))) nombre <- comp_name_base

      # Handle season override (e.g. for Baraz in Archive)
      curr_season <- if ("competicion_temporada" %in% names(comp_row)) comp_row[["competicion_temporada"]][1] else ""
      if (!is.null(season_override) && !is.na(curr_season) && curr_season != "") {
        nombre <- stringr::str_replace(nombre, curr_season, season_override)
      }

      tags$span(class = paste0("lang-", lang), nombre)
    })
  )
}


#' @title Generate multi-language spans for a competition category label (e.g. "Youth (U19)").
#' @description Combines a category base name with an age prefix and age value across all languages.
#' @param current_cat The category name in Macedonian (e.g. "\u041c\u043b\u0430\u0434\u0438\u043d\u0446\u0438").
#' @param current_max_age The age value (e.g. 19).
#' @return An htmltools tagList with one span per language.
category_label_spans <- function(current_cat, current_max_age) {
  cat_key <- case_when(
    current_cat == "\u041c\u043b\u0430\u0434\u0438\u043d\u0446\u0438" ~ "nav.mladinci_cat_base",
    current_cat == "\u041a\u0430\u0434\u0435\u0442\u0438" ~ "nav.kadeti_cat_base",
    current_cat == "\u041f\u0438\u043e\u043d\u0435\u0440\u0438" ~ "nav.pioneri_cat_base",
    current_cat == "\u041c\u043b\u0430\u0434\u0438 \u041f\u0438\u043e\u043d\u0435\u0440\u0438" ~ "nav.ppioneri_cat_base",
    current_cat == "\u041f\u043e\u043c\u0430\u043b\u0438 \u043f\u0435\u0442\u043b\u0438\u045a\u0430" ~ "nav.pomali_petlinja_cat_base",
    current_cat == "\u041f\u0435\u0442\u043b\u0438\u045a\u0430" ~ "nav.petlinja_cat_base",
    current_cat == "\u0414\u0435\u0442\u0441\u043a\u0430 \u043b\u0438\u0433\u0430" ~ "nav.detski_cat_base",
    TRUE ~ NA_character_
  )

  tagList(
    lapply(IDIOMAS_SOPORTADOS, function(lang) {
      cat_name_lang <- if (!is.na(cat_key)) textos[[lang]][[cat_key]] %||% current_cat else current_cat
      prefix_lang <- textos[[lang]][["nav.sub_prefix"]] %||% "U"
      tags$span(class = paste0("lang-", lang), paste0(cat_name_lang, " (", prefix_lang, current_max_age, ")"))
    })
  )
}

#' @title Get the entity name for a specific language.
#' @description Simple lookup from entidades_maestro_df for a given language.
#' @param original_name_mk The Macedonian entity name.
#' @param lang Language code.
#' @return A single string with the translated name.
get_entity_name <- function(original_name_mk, lang) {
  col <- paste0("translated_name_", lang)
  nombre <- entidades_maestro_df[[col]][entidades_maestro_df$original_name == original_name_mk]
  if (length(nombre) == 0 || is.na(nombre[1])) {
    return(original_name_mk)
  }
  return(nombre[1])
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
  staff = "staff",
  logos = "logos"
)

nombres_archivos_traducidos <- list(
  partidos = "raspored",
  clasificacion = "tabela",
  goleadoras = "strelci",
  sanciones = "disciplinska",
  archive = "arhiva",
  about = "za-proektot",
  players = "fudbaleri",
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

# 8.6.7. Create the FLAT folder structure (no per-language subfolders).
# In the client-side i18n architecture, all HTML lives directly in docs/.
walk(
  nombres_carpetas_relativos[c("competiciones", "partidos", "jugadoras", "timovi", "arbitros", "estadios", "staff")],
  ~ dir.create(file.path(RUTA_SALIDA_RAIZ, .x), showWarnings = FALSE, recursive = TRUE)
)
message("Flat directory structure created in: ", RUTA_SALIDA_RAIZ)

# 8.6.8. Export translations.json for client-side i18n.
translations_json_path <- file.path(RUTA_ASSETS_COMPARTIDOS, "translations.json")
translations_json_content <- toJSON(textos, auto_unbox = TRUE, pretty = TRUE)
writeLines(translations_json_content, translations_json_path, useBytes = TRUE)
message("translations.json exported to: ", translations_json_path)

# 8.6.9. Copy shared assets (Logos).
copiar_logos_compartidos <- function() {
  ruta_logos_fuente <- "Logos"

  if (!dir.exists(ruta_logos_fuente)) {
    warning("The logos folder was not found. Logos will not be copied.")
    return(invisible(NULL))
  }

  archivos_logo_fuente <- list.files(ruta_logos_fuente, pattern = "\\.webp$", full.names = TRUE)
  if (length(archivos_logo_fuente) == 0) {
    warning("The logos folder exists but contains no .webp files.")
    return(invisible(NULL))
  }

  # Remove stale exports so slug changes are reflected immediately.
  archivos_logo_destino <- list.files(RUTA_LOGOS_DESTINO, pattern = "\\.webp$", full.names = TRUE)
  if (length(archivos_logo_destino) > 0) {
    unlink(archivos_logo_destino, force = TRUE)
  }

  walk(archivos_logo_fuente, function(ruta_completa_fuente) {
    nombre_archivo_original <- basename(ruta_completa_fuente)

    nombre_archivo_destino <- if (tolower(nombre_archivo_original) == "nologo.webp") {
      "NOLOGO.webp"
    } else {
      nombre_base_sin_ext <- tools::file_path_sans_ext(nombre_archivo_original)
      paste0(generar_id_seguro(nombre_base_sin_ext), ".webp")
    }

    ruta_completa_destino <- file.path(RUTA_LOGOS_DESTINO, nombre_archivo_destino)
    file.copy(from = ruta_completa_fuente, to = ruta_completa_destino, overwrite = TRUE)
  })

  message(paste(length(archivos_logo_fuente), " logos copied to the shared assets folder: ", RUTA_LOGOS_DESTINO))
  if (!file.exists(file.path(ruta_logos_fuente, "NOLOGO.webp"))) {
    warning("WARNING: The placeholder logo 'NOLOGO.webp' was not found.")
  }

  invisible(NULL)
}

copiar_logos_compartidos()
