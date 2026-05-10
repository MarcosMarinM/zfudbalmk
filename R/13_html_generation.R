#### 13. HTML PAGE GENERATION (OPTIMIZED ARCHITECTURE) ####

if (hubo_cambios) {
  # --- Parallelization and progress bar setup ---
  n_workers <- min(4, max(1, parallel::detectCores() - 1))
  plan(multisession, workers = n_workers)
  handlers(handler_cli(
    format = "{cli::pb_spin} [{cli::pb_current}/{cli::pb_total}] {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"
  ))
  pkgs_paralelos <- c("htmltools", "dplyr", "purrr", "stringr", "rlang", "jsonlite", "tidyr")
  message(sprintf("   > Parallelization enabled with %d workers.", n_workers))

  # ============================================================================ #
  # ==      MAIN GENERATION LOOP: Iterates over each language and builds the site      ==
  # ============================================================================ #
  nombres_archivos_mk <- nombres_archivos_traducidos

  render_team_names <- function(team_names_mk, path_to_root = "..") {
    team_names_mk <- as.character(team_names_mk)
    if (length(team_names_mk) == 0 || is.na(team_names_mk) || team_names_mk == "") {
      return(tagList(NULL))
    }
    teams_mk_list <- str_split(team_names_mk, "\\s*(?:/|;)\\s*")
    if (length(teams_mk_list) == 0) {
      return(tagList(NULL))
    }
    teams_mk <- teams_mk_list[[1]]
    if (is.null(teams_mk) || length(teams_mk) == 0) {
      return(tagList(NULL))
    }
    teams_mk <- teams_mk[teams_mk != ""]
    if (length(teams_mk) == 0) {
      return(tagList(NULL))
    }
    tagList(
      map(seq_along(teams_mk), function(i) {
        team_name_mk <- teams_mk[i]
        team_element <- tags$span(
          class = "team-cell",
          get_logo_tag(team_name_mk, path_to_root = path_to_root),
          tags$a(href = file.path(path_to_root, nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(team_name_mk), ".html")), entity_name_spans(team_name_mk))
        )
        if (i < length(teams_mk)) {
          tagList(team_element, tags$br())
        } else {
          team_element
        }
      })
    )
  }

  render_team_names_text <- function(teams_mk) {
    if (is.null(teams_mk) || length(teams_mk) == 0) return("")
    teams_mk <- unlist(strsplit(as.character(teams_mk), ";|/"))
    teams_mk <- trimws(teams_mk)
    teams_mk <- teams_mk[teams_mk != ""]
    if (length(teams_mk) == 0) {
      return(tagList(NULL))
    }
    tagList(
      map(seq_along(teams_mk), function(i) {
        team_name_mk <- teams_mk[i]
        team_element <- entity_name_spans(team_name_mk)
        if (i < length(teams_mk)) {
          tagList(team_element, tags$br())
        } else {
          team_element
        }
      })
    )
  }

  non_empty_or_na <- function(x) {
    value <- str_squish(as.character(x))
    value[is.na(value) | value == "" | tolower(value) == "na"] <- NA_character_
    value
  }

  build_goal_team_fallback_by_comp <- function() {
    empty_tbl <- tibble(
      competicion_id = character(),
      id = character(),
      TeamNames_fallback = character(),
      LastTeam_fallback = character()
    )

    if (!exists("goles_df_unificado") || is.null(goles_df_unificado) || nrow(goles_df_unificado) == 0) {
      return(empty_tbl)
    }

    team_cols <- c("equipo_jugadora", "equipo_acreditado", "equipo")
    present_team_cols <- team_cols[team_cols %in% names(goles_df_unificado)]
    if (length(present_team_cols) == 0) {
      return(empty_tbl)
    }

    goles_con_comp <- goles_df_unificado %>%
      left_join(
        partidos_df %>% select(id_partido, competicion_nombre, competicion_temporada),
        by = "id_partido"
      ) %>%
      left_join(
        competiciones_unicas_df %>% select(competicion_id, competicion_nombre, competicion_temporada),
        by = c("competicion_nombre", "competicion_temporada")
      ) %>%
      mutate(
        id = non_empty_or_na(id),
        team_candidate = non_empty_or_na(coalesce(!!!rlang::syms(present_team_cols)))
      ) %>%
      filter(!is.na(competicion_id), !is.na(id), !is.na(team_candidate))

    if (exists("mapa_conversiones_df") && !is.null(mapa_conversiones_df) && nrow(mapa_conversiones_df) > 0 && nrow(goles_con_comp) > 0) {
      goles_con_comp <- goles_con_comp %>%
        mutate(
          team_candidate = aplicar_conversiones(team_candidate, mapa_df = mapa_conversiones_df),
          team_candidate = non_empty_or_na(team_candidate)
        ) %>%
        filter(!is.na(team_candidate))
    }

    if (nrow(goles_con_comp) == 0) {
      return(empty_tbl)
    }

    goles_con_comp %>%
      group_by(competicion_id, id) %>%
      summarise(
        TeamNames_fallback = paste(unique(team_candidate), collapse = " / "),
        LastTeam_fallback = dplyr::last(team_candidate),
        .groups = "drop"
      )
  }

  has_translation_key <- function(key) {
    if (is.null(key) || is.na(key) || !nzchar(trimws(key))) {
      return(FALSE)
    }
    any(vapply(IDIOMAS_SOPORTADOS, function(lang_code) {
      if (!lang_code %in% names(textos) || is.null(textos[[lang_code]])) {
        return(FALSE)
      }
      !is.null(textos[[lang_code]][[key]])
    }, logical(1)))
  }

  render_role_label <- function(role_key, role_raw = NA_character_) {
    if (has_translation_key(role_key)) {
      return(t_html(role_key))
    }
    if (!is.null(role_raw) && !is.na(role_raw) && nzchar(trimws(role_raw))) {
      return(role_raw)
    }
    if (!is.null(role_key) && !is.na(role_key) && nzchar(trimws(role_key))) {
      return(role_key)
    }
    ""
  }

  save_page_html <- function(page, file) {
    # htmltools::renderTags drops <head> when the object is a complete <html>
    # document. Build the HTML manually in that case to preserve CSS/JS links.
    if (inherits(page, "shiny.tag") && identical(page$name, "html")) {
      html_attrs <- ""
      if (length(page$attribs)) {
        html_attrs <- paste0(" ", paste(
          sprintf('%s="%s"', names(page$attribs), vapply(page$attribs, htmltools::htmlEscape, character(1), attribute = TRUE)),
          collapse = " "
        ))
      }

      head_tag <- NULL
      body_tag <- NULL
      for (child in page$children) {
        if (!inherits(child, "shiny.tag")) next
        if (identical(child$name, "head")) head_tag <- child
        if (identical(child$name, "body")) body_tag <- child
      }

      head_html <- ""
      if (!is.null(head_tag) && length(head_tag$children)) {
        head_html <- paste0(
          "<head>\n",
          paste(vapply(head_tag$children, function(x) htmltools::renderTags(x)$html, character(1), USE.NAMES = FALSE), collapse = "\n"),
          "\n</head>\n"
        )
      }

      body_html <- ""
      if (!is.null(body_tag)) {
        body_attrs <- ""
        if (length(body_tag$attribs)) {
          body_attrs <- paste0(" ", paste(
            sprintf('%s="%s"', names(body_tag$attribs), vapply(body_tag$attribs, htmltools::htmlEscape, character(1), attribute = TRUE)),
            collapse = " "
          ))
        }
        body_html <- paste0(
          "<body", body_attrs, ">\n",
          paste(vapply(body_tag$children, function(x) htmltools::renderTags(x)$html, character(1), USE.NAMES = FALSE), collapse = "\n"),
          "\n</body>\n"
        )
      }

      rendered <- paste0("<!DOCTYPE html>\n<html", html_attrs, ">\n", head_html, body_html, "</html>")
    } else {
      rendered <- htmltools::renderTags(page)$html
      if (!grepl("^\\s*<!DOCTYPE\\s+html", rendered, ignore.case = TRUE)) {
        rendered <- paste0("<!DOCTYPE html>\n", rendered)
      }
    }

    writeLines(rendered, con = file, useBytes = TRUE)
  }

  # ============================================================================ #
  # ==      SINGLE-PASS GENERATION: All languages in one set of HTML files      ==
  # ============================================================================ #
  # In the flat SPA architecture, we generate ONE set of pages.
  # Language switching is handled client-side via CSS (lang-XX spans).
  # We set idioma_actual to 'mk' as the base language for R-side t() calls
  # that still exist (e.g., page titles for <title> tag).
  idioma_actual <<- IDIOMAS_SOPORTADOS[1]  # 'mk'
  lang <- idioma_actual
  player_name_col <- paste0("PlayerName_", lang)
  player_name_col_sym <- rlang::sym(player_name_col)
  comp_name_col <- paste0("nombre_completo_", lang)
  comp_name_col_sym <- rlang::sym(comp_name_col)
  entity_name_col <- paste0("translated_name_", lang)
  entity_name_col_sym <- rlang::sym(entity_name_col)

  # Fallback dataframe for legacy code referencing entidades_df_lang
  entidades_df_lang <- entidades_maestro_df %>%
    rename(current_lang_name = !!entity_name_col_sym)

  goal_team_fallback_by_comp <- build_goal_team_fallback_by_comp()

  nombres_archivos_traducidos <- nombres_archivos_mk

  message("\n--- Generating single multi-language site ---")

  # 13.1.1. Prepare language-agnostic data.
  message("   > Preparing data and scripts...")

    # For the SPA architecture, we need all languages available for spans
    jugadoras_lang_df <- jugadoras_stats_df %>%
      select(id, starts_with("PlayerName_")) %>%
      mutate(PlayerName = !!player_name_col_sym)
    jugadoras_all_lang_df <- jugadoras_lang_df # ALIAS for different script sections

    entidades_all_lang_df <- entidades_maestro_df %>%
      select(original_name, starts_with("translated_name_")) %>%
      mutate(current_lang_name = !!entity_name_col_sym)
    entidades_df_lang_all <- entidades_all_lang_df # ALIAS for different script sections

    # --- 1. Standardization Helper ---
    standardize_search_df <- function(df) {
      target_cols <- c("Ime", "Tip", "target_id", "search_terms")
      if (is.null(df) || nrow(df) == 0) {
        return(tibble("Ime" = character(0), "Tip" = character(0), target_id = character(0), search_terms = character(0)))
      }
      # Ensure all expected columns exist and are character
      for (col in target_cols) {
        if (!col %in% names(df)) df[[col]] <- NA_character_
      }
      df %>%
        mutate(across(all_of(target_cols), as.character)) %>%
        select(all_of(target_cols))
    }

    # --- 2. Jugadoras (Players) ---
    search_jugadoras <- jugadoras_lang_df %>%
      rowwise() %>%
      mutate(
        "Ime" = as.character(player_name_spans_from_row(pick(everything()))),
        "Tip" = as.character(t_html("player_type")),
        target_id = paste0("jugadora-", id),
        search_terms = paste(
          generar_terminos_busqueda(PlayerName_mk),
          generar_terminos_busqueda(PlayerName_sq),
          generar_terminos_busqueda(PlayerName_es),
          generar_terminos_busqueda(PlayerName_en)
        )
      ) %>%
      ungroup() %>%
      standardize_search_df()

    # --- 3. Entidades (Teams, Referees, Stadiums, Staff) ---
    nombres_equipos_search <- if (exists("team_names_to_skip_mk")) setdiff(nombres_equipos, team_names_to_skip_mk) else nombres_equipos
    nombres_arbitros_search <- if (exists("referee_ids_to_skip")) nombres_arbitros[!(generar_id_seguro(nombres_arbitros) %in% referee_ids_to_skip)] else nombres_arbitros
    nombres_estadios_search <- if (exists("stadium_ids_to_skip")) nombres_estadios[!(generar_id_seguro(nombres_estadios) %in% stadium_ids_to_skip)] else nombres_estadios
    
    search_equipos <- standardize_search_df(generar_search_entidad(entidades_df_lang_all, nombres_equipos_search, "team_type", "equipo-"))
    search_arbitros <- standardize_search_df(generar_search_entidad(entidades_df_lang_all, nombres_arbitros_search, "referee_type", "arbitro-"))
    search_estadios <- standardize_search_df(generar_search_entidad(entidades_df_lang_all, nombres_estadios_search, "stadium_type", "\u0441\u0442\u0430\u0434\u0438\u043e\u043d-"))
    
    nombres_staff_search <- if (exists("staff_df") && nrow(staff_df) > 0) unique(staff_df$nombre) else character(0)
    if (exists("staff_names_to_skip")) nombres_staff_search <- setdiff(nombres_staff_search, staff_names_to_skip)
    search_staff <- if (length(nombres_staff_search) > 0) {
      standardize_search_df(generar_search_entidad(entidades_df_lang_all, nombres_staff_search, "staff_type", "staff-"))
    } else {
      standardize_search_df(NULL)
    }

    # --- 3. Competiciones ---
    if (nrow(competiciones_unicas_df) > 0) {
      search_competiciones <- competiciones_unicas_df
      search_competiciones$"Ime" <- sapply(1:nrow(search_competiciones), function(i) {
        as.character(comp_name_spans(search_competiciones[i, ]))
      })
      search_competiciones <- search_competiciones %>%
        mutate(
          "Tip" = as.character(t_html("competition_type")),
          target_id = paste0("menu-competicion-", competicion_id),
          search_terms = paste(
            sapply(nombre_completo_mk, generar_terminos_busqueda, USE.NAMES = FALSE),
            sapply(nombre_completo_en, generar_terminos_busqueda, USE.NAMES = FALSE)
          )
        ) %>%
        standardize_search_df()
    } else {
      search_competiciones <- standardize_search_df(NULL)
    }

    # --- 4. Unir todo y guardar UNA sola vez ---
    search_index_df <- bind_rows(search_jugadoras, search_equipos, search_arbitros, search_competiciones, search_estadios, search_staff) %>%
      mutate(search_terms = sapply(str_split(search_terms, "\\s+"), function(x) paste(unique(x), collapse = " "))) %>%
      arrange("Ime")

    search_data_json <- toJSON(search_index_df, auto_unbox = TRUE)
    ruta_json_salida <- file.path(RUTA_ASSETS_COMPARTIDOS, "search_data.json")
    writeLines(search_data_json, ruta_json_salida, useBytes = TRUE)
    message("     > Unified search index saved to: ", basename(ruta_json_salida))

    if (PROTEGER_CON_CONTRASENA) {
      la_contrasena <- "secreto123"
      message("     > Password protection ENABLED.")
      script_password_lang <- tags$script(HTML(
        sprintf(
          "(function() { var p = '%s'; var s = sessionStorage; var d = document; if (s.getItem('zfudbalmk-password-ok') === p) return; var i; var m = '%s'; while (true) { i = prompt(m, ''); if (i === p) { s.setItem('zfudbalmk-password-ok', i); break; } if (i === null) { d.body.innerHTML = '<div style=\"text-align:center; padding: 50px; font-family: sans-serif;\"><h1>%s</h1><p>%s</p></div>'; throw new Error('Access denied'); } m = '%s'; } })();",
          la_contrasena, t("password_prompt"), t("access_denied_header"), t("access_denied_body"), t("password_wrong")
        )
      ))
    } else {
      message("     > Password protection DISABLED.")
      script_password_lang <- NULL
    }

    # 13.1.2. Generate Home Page (Portal) with Latest Results and Standings.
    message("   > Generating new dynamic index.html...")

    temporada_actual_str <- obtener_temporada_actual()
    message(paste("     > Current season determined as:", temporada_actual_str))

    competiciones_en_portada_mk <- c(
      "\u041f\u0440\u0432\u0430 \u041c\u0424\u041b", "\u0412\u0442\u043e\u0440\u0430 \u041c\u0424\u041b", "\u041c\u043b\u0430\u0434\u0438\u043d\u0441\u043a\u0430 \u0421\u0423\u041f\u0415\u0420 \u043b\u0438\u0433\u0430",
      "\u0422\u0440\u0435\u0442\u0430 \u043b\u0438\u0433\u0430 \u0418\u0421\u0422\u041e\u041a", "\u0422\u0440\u0435\u0442\u0430 \u043b\u0438\u0433\u0430 \u0417\u0410\u041f\u0410\u0414", "\u0422\u0440\u0435\u0442\u0430 \u043b\u0438\u0433\u0430 \u0421\u0415\u0412\u0415\u0420", 
      "\u0422\u0440\u0435\u0442\u0430 \u043b\u0438\u0433\u0430 \u0408\u0423\u0413", "\u0422\u0440\u0435\u0442\u0430 \u043b\u0438\u0433\u0430 \u0408\u0423\u0413\u041e\u0417\u0410\u041f\u0410\u0414"
    )

    equipos_en_portada_mk <- partidos_df %>%
      filter(competicion_temporada == temporada_actual_str, competicion_nombre %in% competiciones_en_portada_mk) %>%
      distinct(local, visitante) %>%
      pivot_longer(everything(), names_to = NULL, values_to = "equipo") %>%
      distinct(equipo) %>%
      pull(equipo)

    if (length(equipos_en_portada_mk) > 0) {
      nombres_traducidos <- (entidades_df_lang %>% filter(original_name %in% equipos_en_portada_mk))$current_lang_name
      longitud_max_nombre <- max(nchar(nombres_traducidos), na.rm = TRUE)
      ancho_ficha_calculado_px <- (longitud_max_nombre * 6.5) + 50
    } else {
      ancho_ficha_calculado_px <- 220
    }
    message(paste("     > Calculated match block width:", ancho_ficha_calculado_px, "px"))

    abreviaturas_actual_lang_df <- if (exists("mapa_abreviaturas_long_df") && !is.null(mapa_abreviaturas_long_df)) {
      mapa_abreviaturas_long_df
    } else {
      tibble(original_mk = character(), lang = character(), abbreviation = character())
    }

    # Canonical match subset used across legacy generation blocks.
    # NOTE: Do NOT filter out cancelled/official-result matches here.
    # They need to appear in competition hubs and team schedules.
    valid_partidos_df <- partidos_df %>%
      mutate(es_cancelado = if ("es_cancelado" %in% names(.)) coalesce(es_cancelado, FALSE) else FALSE) %>%
      filter(!is.na(id_partido))

    lista_componentes_html <- map(competiciones_en_portada_mk, function(nombre_comp_mk) {
      comp_info <- competiciones_unicas_df %>%
        filter(competicion_nombre == nombre_comp_mk, competicion_temporada == temporada_actual_str)
      if (nrow(comp_info) == 0) {
        return(NULL)
      }

      comp_nombre_actual_lang <- comp_info[[comp_name_col]][1]

      ultima_jornada_jugada_df <- valid_partidos_df %>%
        filter(
          competicion_nombre == nombre_comp_mk, 
          competicion_temporada == temporada_actual_str, 
          !is.na(id_partido),
          !is.na(goles_local),
          !(es_cancelado %in% TRUE)
        )

      bloque_resultados_html <- NULL
      if (nrow(ultima_jornada_jugada_df) > 0) {
        ultima_jornada_jugada <- max(as.numeric(ultima_jornada_jugada_df$jornada), na.rm = TRUE)
        if (!is.infinite(ultima_jornada_jugada) && !is.na(ultima_jornada_jugada)) {
          partidos_para_mostrar <- partidos_df %>%
            filter(competicion_nombre == nombre_comp_mk, competicion_temporada == temporada_actual_str, jornada == ultima_jornada_jugada)
          if (nrow(partidos_para_mostrar) > 0) {
            bloque_resultados_html <- crear_bloque_resultados_competicion(
              comp_info, partidos_para_mostrar, comp_nombre_actual_lang, entidades_df_lang, ancho_ficha_calculado_px, path_to_root = "."
            )
          }
        }
      }

      tabla_clasificacion_html <- crear_tabla_clasificacion_portada(
        comp_info, stats_clasificacion_por_comp_df, entidades_df_lang,
        abreviaturas_actual_lang_df, estilos_clasificacion_data,
        comp_nombre_lang = comp_nombre_actual_lang, path_to_root = "."
      )

      list(resultados = bloque_resultados_html, clasificacion = tabla_clasificacion_html)
    }) %>% purrr::compact()

    contenido_portal_dinamico <- tagList(
      tags$h3(class = "main-content-title", t_html("latest_results_title")),
      map(lista_componentes_html, "resultados"),
      tags$h3(class = "main-content-title standings-grid-title", t_html("standings_main_title")),
      tags$div(
        class = "standings-grid-container",
        map(lista_componentes_html, "clasificacion")
      )
    )

    pagina_portal_final <- crear_pagina_html(
      contenido_principal = contenido_portal_dinamico,
      titulo_pagina = t("site_title"),
      path_to_assets = ".",
      script_password = script_password_lang
    )
    save_page_html(pagina_portal_final, file = file.path(RUTA_SALIDA_RAIZ, "index.html"))

    # 13.1.2.bis. Generate new static and list pages
    message("   > Generating new list and static pages...")

    # --- GENERATE COMPETITION ARCHIVE PAGE ---
    path_to_archive_page <- file.path(RUTA_SALIDA_RAIZ, paste0(nombres_archivos_traducidos$archive, ".html"))
    message("     > Generating: ", basename(path_to_archive_page))

    competiciones_archivo <- competiciones_unicas_df %>%
      filter(competicion_id != "reprezentacija")

    archive_category_label <- function(cat_key) {
      t_key <- case_when(
        cat_key == "senior"          ~ "category_senior",
        cat_key == "mladinski"       ~ "nav.mladinci_cat_base",
        cat_key == "kadeti"          ~ "nav.kadeti_cat_base",
        cat_key == "pioneri"         ~ "nav.pioneri_cat_base",
        cat_key == "mladi_pioneri"    ~ "nav.ppioneri_cat_base",
        cat_key == "pomali_petlinja" ~ "nav.pomali_petlinja_cat_base",
        cat_key == "petlinja"        ~ "nav.petlinja_cat_base",
        TRUE                         ~ "category_other"
      )
      t_html(t_key)
    }

    competiciones_archivo_raw <- competiciones_archivo %>%
      mutate(
        nombre_sort_ref = coalesce(nombre_completo_mk, competicion_nombre),
        es_baraz = str_detect(competicion_nombre, regex("\\b\u0431\u0430\u0440\u0430\u0436\\b", ignore_case = TRUE)),
        competicion_temporada_show = competicion_temporada,
        temporada_num = as.numeric(str_extract(competicion_temporada_show, "^\\d{2}")),
        nombre_baraz_objetivo = if_else(
          es_baraz,
          str_remove(competicion_nombre, regex("^\\s*\u0431\u0430\u0440\u0430\u0436\\s+\u0437\u0430\\s+", ignore_case = TRUE)) %>%
            str_remove("\\s*\\d{2}/\\d{2}\\s*$") %>%
            str_squish(),
          NA_character_
        ),
        categoria_normalizada = normalizar_categoria_competicion(categoria, competicion_nombre),
        archive_category_key = case_when(
          categoria_normalizada == "\u041f\u043e\u043c\u0430\u043b\u0438 \u043f\u0435\u0442\u043b\u0438\u045a\u0430" ~ "pomali_petlinja",
          categoria_normalizada == "\u041f\u0435\u0442\u043b\u0438\u045a\u0430" ~ "petlinja",
          categoria_normalizada == "\u041c\u043b\u0430\u0434\u0438 \u041f\u0438\u043e\u043d\u0435\u0440\u0438" ~ "mladi_pioneri",
          categoria_normalizada == "\u041f\u0438\u043e\u043d\u0435\u0440\u0438" ~ "pioneri",
          categoria_normalizada == "\u041a\u0430\u0434\u0435\u0442\u0438" ~ "kadeti",
          categoria_normalizada == "\u041c\u043b\u0430\u0434\u0438\u043d\u0446\u0438" ~ "mladinski",
          TRUE ~ "senior"
        ),
        archive_category_order = case_when(
          archive_category_key == "senior" ~ 1,
          archive_category_key == "mladinski" ~ 2,
          archive_category_key == "kadeti" ~ 3,
          archive_category_key == "pioneri" ~ 4,
          archive_category_key == "mladi_pioneri" ~ 5,
          archive_category_key == "petlinja" ~ 6,
          archive_category_key == "pomali_petlinja" ~ 7,
          TRUE ~ 7
        ),
        competicion_order_within_cat = case_when(
          str_detect(nombre_sort_ref, "(?i)\u0421\u0443\u043f\u0435\u0440\u043b\u0438\u0433\u0430") ~ 1,
          str_detect(nombre_sort_ref, "(?i)\u041f\u0440\u0432\u0430") ~ 2,
          str_detect(nombre_sort_ref, "(?i)\u041a\u0443\u043f") & !str_detect(nombre_sort_ref, "(?i)\u041e\u0424\u0421") &
            !str_detect(nombre_sort_ref, "(?i)\u0415\u043b\u0438\u043c\u0438\u043d\u0430\u0446\u0438\u0458\u0430") ~ 3,
          str_detect(nombre_sort_ref, "(?i)\u041a\u0443\u043f") & !str_detect(nombre_sort_ref, "(?i)\u041e\u0424\u0421") &
            str_detect(nombre_sort_ref, "(?i)\u0415\u043b\u0438\u043c\u0438\u043d\u0430\u0446\u0438\u0458\u0430") ~ 3.1,
          str_detect(nombre_sort_ref, "(?i)\u0412\u0442\u043e\u0440\u0430") ~ 4,
          str_detect(nombre_sort_ref, "(?i)\u0422\u0440\u0435\u0442\u0430") ~ 5,
          str_detect(nombre_sort_ref, "(?i)\u041e\u0424\u041b") ~ 6,
          str_detect(nombre_sort_ref, "(?i)\u041a\u0443\u043f") & str_detect(nombre_sort_ref, "(?i)\u041e\u0424\u0421") ~ 8,
          TRUE ~ 7
        ),
        competicion_order_effective = case_when(
          es_baraz & str_detect(nombre_baraz_objetivo, "(?i)\u041f\u0440\u0432\u0430") ~ 2.2,
          es_baraz & str_detect(nombre_baraz_objetivo, "(?i)\u041a\u0443\u043f") ~ 3.2,
          es_baraz & str_detect(nombre_baraz_objetivo, "(?i)\u0412\u0442\u043e\u0440\u0430") ~ 4.2,
          es_baraz & str_detect(nombre_baraz_objetivo, "(?i)\u0422\u0440\u0435\u0442\u0430") ~ 5.2,
          es_baraz & str_detect(nombre_baraz_objetivo, "(?i)\u041e\u0424\u041b") ~ 6.2,
          es_baraz ~ competicion_order_within_cat + 0.2,
          TRUE ~ competicion_order_within_cat
        )
      ) %>%
      arrange(desc(temporada_num), archive_category_order, competicion_order_effective, nombre_sort_ref, .locale = "mk")

    temporadas_archivo <- competiciones_archivo_raw %>%
      group_by(competicion_temporada_show, temporada_num) %>%
      group_split()

    contenido_archivo <- tagList(
      crear_botones_navegacion("."),
      tags$h2(t_html("archive_page_title")),
      map(temporadas_archivo, function(df_temporada) {
        temporada <- df_temporada$competicion_temporada_show[1]

        categorias_temporada <- df_temporada %>%
          arrange(archive_category_order, competicion_order_effective, nombre_sort_ref, .locale = "mk") %>%
          group_split(archive_category_order, archive_category_key, .keep = TRUE)

        tags$div(
          class = "season-accordion",
          tags$div(
            class = "season-header",
            tags$h3(tagList(t_html("player_season"), " ", temporada))
          ),
          tags$div(
            class = "season-content",
            style = "display: none;",
            map(categorias_temporada, function(df_cat) {
              cat_key <- df_cat$archive_category_key[1]

              tags$div(
                class = "season-accordion archive-category-accordion",
                tags$div(
                  class = "season-header",
                  tags$h4(archive_category_label(cat_key))
                ),
                tags$div(
                  class = "season-content",
                  style = "display: none;",
                  tags$div(
                    class = "archive-season-container",
                    tags$ul(
                      map(1:nrow(df_cat), function(k) {
                        comp <- df_cat[k, ]

                        tags$li(
                          tags$a(
                            href = file.path(nombres_carpetas_relativos$competiciones, paste0(comp$competicion_id, ".html")),
                            comp_name_spans(comp)
                          )
                        )
                      })
                    )
                  )
                )
              )
            })
          )
        )
      })
    )
    save_page_html(crear_pagina_html(contenido_archivo, t("site_title"), ".", script_password_lang, "archive"), file = path_to_archive_page)


    # --- GENERATE TEAMS LIST PAGE ---
    path_to_teams_page <- file.path(RUTA_SALIDA_RAIZ, "klubovi.html")
    message("     > Generating: ", basename(path_to_teams_page))

    team_alphabet_blocks <- map(IDIOMAS_SOPORTADOS, function(curr_lang_code) {
      equipos_lang <- entidades_df_lang %>%
        filter(original_name %in% nombres_equipos) %>%
        rowwise() %>%
        filter(is.na(get_national_team_iso(original_name))) %>%
        ungroup() %>%
        mutate(
          display_name = trimws(current_lang_name),
          letra_inicial = toupper(substr(display_name, 1, 1))
        )
      
      equipos_lang <- equipos_lang %>%
        mutate(
          display_name = sapply(original_name, function(x) get_entity_name(x, curr_lang_code)),
          letra_inicial = toupper(substr(display_name, 1, 1))
        ) %>%
        arrange(display_name, .locale = curr_lang_code)

      alfabeto_lang <- equipos_lang %>%
        distinct(letra_inicial) %>%
        filter(grepl("^[\\p{L}]$", letra_inicial, perl = TRUE)) %>%
        arrange(letra_inicial, .locale = curr_lang_code) %>%
        pull(letra_inicial)
      
      grupos_lang <- split(equipos_lang, equipos_lang$letra_inicial)

      tags$div(
        class = paste0("lang-alphabet-block lang-", curr_lang_code),
        style = if(curr_lang_code == IDIOMAS_SOPORTADOS[1]) "" else "display: none;",
        tags$div(
          class = "letter-nav",
          map(alfabeto_lang, function(letra) {
            tags$a(
              href = "javascript:void(0);",
              onclick = sprintf("showLetter('%s', '%s')", letra, curr_lang_code),
              class = "letter-link",
              `data-letter` = letra,
              letra
            )
          })
        ),
        tags$div(
          class = "team-list",
          map(alfabeto_lang, function(letra) {
            grupo <- grupos_lang[[letra]]
            tags$div(
              id = paste0("group-", curr_lang_code, "-", letra),
              class = "letter-group",
              tags$h3(letra),
              tags$ul(
                map(1:nrow(grupo), function(i) {
                  equipo <- grupo[i, ]
                      tags$li(
                        tags$a(
                          href = file.path(nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(equipo$original_name), ".html")),
                          class = "team-list-link",
                          get_logo_tag(equipo$original_name, path_to_root = "."),
                          entity_name_spans(equipo$original_name)
                        )
                  )
                })
              )
            )
          })
        )
      )
    })

    contenido_equipos <- tagList(
      crear_botones_navegacion("."),
      tags$h2(t_html("teams_list_title")),
      tags$p(t_html("teams_list_select_letter")),
      team_alphabet_blocks
    )
    save_page_html(crear_pagina_html(contenido_equipos, t("site_title"), ".", script_password_lang, "teams"), file = path_to_teams_page)


    # --- GENERATE PLAYERS LIST PAGE ---
    path_to_players_page <- file.path(RUTA_SALIDA_RAIZ, paste0(nombres_archivos_traducidos$players, ".html"))
    message("     > Generating: ", basename(path_to_players_page))

    alphabet_blocks <- map(IDIOMAS_SOPORTADOS, function(curr_lang_code) {
      player_name_col_lang <- paste0("PlayerName_", curr_lang_code)
      
      players_lang <- jugadoras_stats_df %>%
        filter(!is.na(id) & trimws(as.character(id)) != "") %>%
        mutate(
          display_name = coalesce(!!sym(player_name_col_lang), PlayerName_mk),
          apellido = sapply(display_name, extraer_apellido),
          letra_inicial = toupper(substr(apellido, 1, 1))
        ) %>%
        filter(!is.na(display_name) & display_name != "")

      players_lang <- players_lang %>% arrange(apellido, .locale = curr_lang_code)
      
      alfabeto_lang <- players_lang %>%
        distinct(letra_inicial) %>%
        arrange(letra_inicial, .locale = curr_lang_code) %>%
        pull(letra_inicial)
      
      alfabeto_lang <- alfabeto_lang[grepl("^[\\p{L}]$", alfabeto_lang, perl = TRUE)]
      
      grupos_lang <- split(players_lang, players_lang$letra_inicial)

      tags$div(
        class = paste0("lang-alphabet-block lang-", curr_lang_code),
        style = if(curr_lang_code == IDIOMAS_SOPORTADOS[1]) "" else "display: none;",
        tags$div(
          class = "letter-nav",
          map(alfabeto_lang, function(letra) {
            tags$a(
              href = "javascript:void(0);",
              onclick = sprintf("showLetter('%s', '%s')", letra, curr_lang_code),
              class = "letter-link",
              `data-letter` = letra,
              letra
            )
          })
        ),
        tags$div(
          class = "player-list",
          map(alfabeto_lang, function(letra) {
            df_letra <- grupos_lang[[letra]]
            tags$div(
              id = paste0("group-", curr_lang_code, "-", letra),
              class = "letter-group",
              tags$h3(letra),
              tags$ul(
                map(1:nrow(df_letra), function(i) {
                  p <- df_letra[i, ]
                  tags$li(
                    tags$a(
                      href = file.path(nombres_carpetas_relativos$jugadoras, paste0(p$id, ".html")),
                      player_name_spans_from_row(p)
                    )
                  )
                })
              )
            )
          })
        )
      )
    })

    contenido_jugadoras <- tagList(
      crear_botones_navegacion("."),
      tags$h2(t_html("players_list_title")),
      tags$p(t_html("players_list_select_letter")),
      alphabet_blocks
    )
    save_page_html(crear_pagina_html(contenido_jugadoras, t("site_title"), ".", script_password_lang, "players"), file = path_to_players_page)


    # --- GENERATE ABOUT PAGE ---
    path_to_about_page <- file.path(RUTA_SALIDA_RAIZ, paste0(nombres_archivos_traducidos$about, ".html"))
    message("     > Generating: ", basename(path_to_about_page))
    contenido_about <- tagList(
      crear_botones_navegacion("."),
      tags$h2(t_html("about_page_title")),
      tags$div(
        class = "about-content",
        t_html("about_page_content")
      )
    )
    save_page_html(crear_pagina_html(contenido_about, t("site_title"), ".", script_password_lang, "about"), file = path_to_about_page)


    # 13.1.3. Generate Competition Pages.
    if (GENERAR_PAGINAS_COMPETICION) {
      message("   > Generating competition pages...")
      walk(1:nrow(competiciones_unicas_df), function(i) {
        comp_info <- competiciones_unicas_df[i, ]
        comp_id <- comp_info$competicion_id

        # 13.1.4. If the id is "reprezentacija", use the new logic.
        if (comp_id == "reprezentacija") {
          # 13.1.5. LOGIC FOR THE NATIONAL TEAM PAGE.
          if (!full_rebuild_needed && !(comp_id %in% affected_competition_ids)) {
            return()
          }

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

          # 13.1.9. 3. Generate the main (menu) page for "\u0420\u0435\u043f\u0440\u0435\u0437\u0435\u043d\u0442\u0430\u0446\u0438\u0458\u0430".
          contenido_menu_seleccion <- tagList(
            crear_botones_navegacion(".."),
            tags$h2(comp_name_spans(comp_info)),
            tags$div(class = "menu-container", lista_botones_menu_seleccion)
          )
          save_page_html(
            crear_pagina_html(contenido_menu_seleccion, t("site_title"), "..", script_password_lang, current_page_id = "national_team"),
            file = file.path(RUTA_SALIDA_RAIZ, nombres_carpetas_relativos$competiciones, paste0(comp_id, ".html"))
          )

          # 13.1.10. 4. Generate a page for EACH category.
          walk(categorias_seleccion, function(cat_actual) {
            # 13.1.11. Filter national team matches for this category and sort them.
            partidos_categoria <- partidos_df %>%
              filter(es_partido_seleccion == TRUE, categoria == cat_actual) %>%
              mutate(fecha_parsed = as.Date(fecha, format = "%d.%m.%Y")) %>%
              arrange(desc(fecha_parsed))

            # 13.1.12. Team logo tag \u2014 uses centralized get_logo_tag() from 08_functions.R

            # 13.1.15. Generate the list of matches (without grouping by matchday).
            contenido_lista_partidos <- tagList(
              crear_botones_navegacion(".."),
              tags$h2(tagList(comp_name_spans(comp_info), " - ", cat_actual)),
              map(1:nrow(partidos_categoria), function(k) {
                partido <- partidos_categoria[k, ]
                is_placeholder_match <- is.na(partido$id_partido)
                local_name <- entidades_df_lang$current_lang_name[match(partido$local, entidades_df_lang$original_name)]
                visitante_name <- entidades_df_lang$current_lang_name[match(partido$visitante, entidades_df_lang$original_name)]
                is_cancelled <- isTRUE(partido$es_cancelado) && !isTRUE(partido$es_resultado_oficial)
                resultado_texto <- if (is_cancelled) {
                  t_html("match_cancelled")
                } else if (is_placeholder_match) {
                  tags$span(" - ")
                } else {
                  res_base <- paste(partido$goles_local, "-", partido$goles_visitante)
                  if (!is_na(partido$penales_local)) res_base <- sprintf("%s (%s - %s)", res_base, partido$penales_local, partido$penales_visitante)
                  if (isTRUE(partido$es_resultado_oficial)) res_base <- paste(res_base, "*")
                  tags$span(res_base)
                }

                # 13.1.16. The visual content of the match (teams and result).
                contenido_comun <- tagList(
                  tags$span(class = "equipo equipo-local", get_logo_tag(partido$local), entity_name_spans(partido$local)),
                  tags$span(class = "resultado", resultado_texto),
                  tags$span(class = "equipo equipo-visitante", entity_name_spans(partido$visitante), get_logo_tag(partido$visitante))
                )

                # 13.1.17. Wrap everything in a tagList to add the date above the match link.
                tagList(
                  # 13.1.18. Add the match date here, using the 'fecha' column from the 'partido' object.
                  tags$p(
                    style = "text-align: center; margin-bottom: 2px; margin-top: 15px; font-size: 0.9em; color: #555;",
                    partido$fecha
                  ),
                  # 13.1.19. The original if/else block to create the link or placeholder.
                  if (is_placeholder_match || is_cancelled) {
                    tags$div(class = if (is_cancelled) "partido-link-cancelled" else "partido-link-placeholder", contenido_comun)
                  } else {
                    tags$a(class = "partido-link", href = file.path("..", nombres_carpetas_relativos$partidos, paste0(partido$id_partido, ".html")), contenido_comun)
                  }
                )
              })
            )

            # 13.1.20. Save the HTML file for the category.
            nombre_archivo_cat_final <- paste0(comp_id, "_", generar_id_seguro(cat_actual), ".html")
            save_page_html(
              crear_pagina_html(contenido_lista_partidos, t("site_title"), "..", script_password_lang),
              file.path(RUTA_SALIDA_RAIZ, nombres_carpetas_relativos$competiciones, nombre_archivo_cat_final)
            )
          })
        } else {
          # 13.1.21. ORIGINAL LOGIC FOR NORMAL COMPETITIONS.
          if (!full_rebuild_needed && !(comp_id %in% affected_competition_ids)) {
            return()
          }
          comp_nombre_current_lang <- comp_info[[comp_name_col]]
          is_cup <- es_competicion_copa(comp_info$competicion_nombre)
          is_friendly_comp <- str_detect(tolower(comp_info$competicion_nombre), "\u043f\u0440\u0438\u0458\u0430\u0442\u0435\u043b\u0441\u043a\u0438")
          player_name_col_sym <- rlang::sym(player_name_col)
          lista_botones_menu <- list()
          partidos_comp <- partidos_df %>% filter(competicion_nombre == comp_info$competicion_nombre, competicion_temporada == comp_info$competicion_temporada)
          mapa_claves_ronda_copa <- if (is_cup) crear_mapa_claves_ronda_copa(partidos_comp) else setNames(character(0), character(0))
          is_placeholder_only_comp <- all(is.na(partidos_comp$id_partido))
          jornadas_comp <- if (nrow(partidos_comp) > 0) {
            ordenar_jornadas(unique(partidos_comp$jornada))
          } else {
            c()
          }
          contenido_partidos <- tagList(
            crear_botones_navegacion(".."),
            tags$h2(tagList(t_html("schedule_title"), " - ", comp_name_spans(comp_info))),
            map(jornadas_comp, function(j) {
              partidos_jornada <- partidos_comp %>%
                filter(jornada == j) %>%
                arrange(local)
              clave_jornada_copa <- unname(mapa_claves_ronda_copa[str_squish(as.character(j))])
              header_text <- if (is_friendly_comp) {
                as.character(j)
              } else if (is_cup) {
                etiqueta_jornada_por_clave(clave_jornada_copa, j)
              } else {
                tagList(t_html("round_prefix"), " ", j)
              }
              tagList(
                tags$div(
                  class = "comp-hub-section-header",
                  style = "margin-top: 30px;",
                  tags$h3(class = "comp-hub-section-title", header_text)
                ),
                map(1:nrow(partidos_jornada), function(k) {
                  partido <- partidos_jornada[k, ]
                  is_placeholder_match <- is.na(partido$id_partido)
                  is_cancelled <- isTRUE(partido$es_cancelado) && !isTRUE(partido$es_resultado_oficial)

                  resultado_texto <- if (is_cancelled) {
                    t_html("match_cancelled")
                  } else if (is_placeholder_match) {
                    " - "
                  } else {
                    res_base <- paste(partido$goles_local, "-", partido$goles_visitante)
                    if (!is.na(partido$penales_local)) res_base <- sprintf("%s (%s - %s)", res_base, partido$penales_local, partido$penales_visitante)
                    if (isTRUE(partido$es_resultado_oficial)) res_base <- paste(res_base, "*")
                    res_base
                  }

                  lugar_partido_mk <- estadios_df %>%
                    filter(id_partido == partido$id_partido) %>%
                    pull(estadio)
                  lugar_partido_lang <- if (length(lugar_partido_mk) > 0 && !is.na(lugar_partido_mk[1])) {
                    (entidades_df_lang %>% filter(original_name == lugar_partido_mk[1]))$current_lang_name[1]
                  } else {
                    NA_character_
                  }

                  date_html <- if (!is.na(partido$fecha)) tags$div(class = "comp-hub-match-date", partido$fecha) else NULL
                  stadium_html <- if (!is.na(lugar_partido_lang)) tags$div(class = "comp-hub-match-stadium", t_html("match_stadium"), ": ", entity_name_spans(lugar_partido_mk[1])) else NULL

                  is_clickable <- !is_placeholder_match && !is_cancelled
                  wrapper_class <- paste0("comp-hub-match-row", if (is_clickable) " clickable" else "")

                  match_content <- tagList(
                    if (!is.null(date_html) || !is.null(stadium_html)) {
                      tags$div(class = "comp-hub-match-time", date_html, stadium_html)
                    },
                    tags$div(class = "comp-hub-match-teams",
                      tags$span(class = "comp-hub-team-home",
                        entity_name_spans(partido$local),
                        get_logo_tag(partido$local, path_to_root = "..", css_class = "comp-hub-team-logo")
                      ),
                      tags$span(class = "comp-hub-match-score", resultado_texto),
                      tags$span(class = "comp-hub-team-away",
                        get_logo_tag(partido$visitante, path_to_root = "..", css_class = "comp-hub-team-logo"),
                        entity_name_spans(partido$visitante)
                      )
                    )
                  )

                  if (is_clickable) {
                    tags$a(class = wrapper_class, href = file.path("..", nombres_carpetas_relativos$partidos, paste0(partido$id_partido, ".html")), match_content)
                  } else {
                    tags$div(class = wrapper_class, match_content)
                  }
                })
              )
            })
          )
          nombre_archivo_partidos <- paste0(comp_id, "_", nombres_archivos_traducidos$partidos, ".html")
          save_page_html(crear_pagina_html(contenido_partidos, t("site_title"), "..", script_password_lang), file.path(RUTA_SALIDA_RAIZ, nombres_carpetas_relativos$competiciones, nombre_archivo_partidos))
          lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href = nombre_archivo_partidos, class = "menu-button", t_html("schedule_title"))
          if (!is_placeholder_only_comp) {
            tabla_goleadoras_comp <- stats_goleadoras_por_comp_df %>%
              filter(competicion_id == comp_id) %>%
              left_join(jugadoras_stats_df %>% select(id, starts_with("PlayerName_")), by = "id") %>%
              left_join(goal_team_fallback_by_comp, by = c("competicion_id", "id")) %>%
              mutate(
                TeamNames_clean = non_empty_or_na(TeamNames_mk),
                LastTeam_clean = non_empty_or_na(LastTeam_mk),
                TeamNames_mk = coalesce(TeamNames_clean, LastTeam_clean, TeamNames_fallback)
              ) %>%
              filter(!is.na(PlayerName_mk)) %>%
              select(Pos, id, starts_with("PlayerName_"), TeamNames_mk, Goals)
            headers_traducidos <- list(
              t_html("standings_pos"), 
              t_html("player_type"), 
              t_html("team_type"), 
              t_html("stats_goals")
            )
            contenido_goleadoras <- tagList(crear_botones_navegacion(".."), tags$h2(tagList(t_html("scorers_title"), " - ", comp_name_spans(comp_info))), tags$table(tags$thead(tags$tr(map(headers_traducidos, tags$th))), tags$tbody(map(1:nrow(tabla_goleadoras_comp), function(j) {
              g <- tabla_goleadoras_comp[j, ]
              tags$tr(tags$td(g$Pos), tags$td(tags$a(href = file.path("..", nombres_carpetas_relativos$jugadoras, paste0(g$id, ".html")), player_name_spans_from_row(g))), tags$td(render_team_names(g$TeamNames_mk)), tags$td(g$Goals))
            }))))
            nombre_archivo_goleadoras <- paste0(comp_id, "_", nombres_archivos_traducidos$goleadoras, ".html")
            save_page_html(crear_pagina_html(contenido_goleadoras, t("site_title"), "..", script_password_lang), file.path(RUTA_SALIDA_RAIZ, nombres_carpetas_relativos$competiciones, nombre_archivo_goleadoras))
            lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href = nombre_archivo_goleadoras, class = "menu-button", t_html("scorers_title"))
          }
          if (!is_cup && !is_friendly_comp && !is_placeholder_only_comp) {
            clasificacion_df_comp_raw <- stats_clasificacion_por_comp_df %>% filter(competicion_id == comp_id)
            clave_estilo_comp <- paste(comp_info$competicion_nombre, comp_info$competicion_temporada)
            contenido_tabla <- if (nrow(clasificacion_df_comp_raw) == 0) {
              tags$p(t_html("standings_no_data_message"))
            } else {
              claves_traduccion_standings <- c("standings_pos", "standings_team", "standings_p", "standings_w", "standings_d", "standings_l", "standings_gf", "standings_ga", "standings_gd", "standings_pts")
              estilos_comp <- estilos_clasificacion_data[[clave_estilo_comp]]
              tagList(tags$table(tags$thead(tags$tr(map(map(claves_traduccion_standings, t_html), tags$th))), tags$tbody(map(1:nrow(clasificacion_df_comp_raw), function(j) {
                fila_raw <- clasificacion_df_comp_raw[j, ]
                nombre_equipo_original <- fila_raw$team
                ruta_relativa_logo_html <- get_club_logo_path(nombre_equipo_original)
                regla_actual <- NULL
                if (!is.null(estilos_comp)) {
                  regla_match <- estilos_comp$reglas %>% filter(puesto == fila_raw$Pos)
                  if (nrow(regla_match) > 0) {
                    regla_actual <- regla_match[1, ]
                  }
                }
                tags$tr(
                  tags$td(style = if (!is.null(regla_actual)) paste0("border-left: 5px solid ", regla_actual$color, "; font-weight: bold;") else NULL, fila_raw$Pos),
                  tags$td(class = "team-cell", tags$img(class = "team-logo", src = ruta_relativa_logo_html, alt = "Logo"), tags$a(href = file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(nombre_equipo_original), ".html")), entity_name_spans(nombre_equipo_original)), if (!is.na(fila_raw$puntos_deducidos) && fila_raw$puntos_deducidos > 0) tags$span(style = "color:#8B0000; font-weight:normal; font-size:0.85em;", paste0(" (\u2013", fila_raw$puntos_deducidos, ")")) else NULL),
                  tags$td(fila_raw$P), tags$td(fila_raw$W), tags$td(fila_raw$D), tags$td(fila_raw$L), tags$td(fila_raw$GF), tags$td(fila_raw$GA), tags$td(fila_raw$GD), tags$td(fila_raw$Pts)
                )
              }))), if (!is.null(estilos_comp) && length(estilos_comp$leyenda) > 0) {
                tags$div(class = "legend", map(estilos_comp$leyenda, function(item_leyenda) {
                  tags$div(class = "legend-item", tags$span(class = "legend-color-box", style = paste0("background-color: ", item_leyenda$color, ";")), tags$span(t_html(item_leyenda$texto_key)))
                }))
              })
            }
            contenido_clasificacion <- tagList(crear_botones_navegacion(".."), tags$h2(tagList(t_html("standings_title"), " - ", comp_name_spans(comp_info))), contenido_tabla)
            nombre_archivo_clasif <- paste0(comp_id, "_", nombres_archivos_traducidos$clasificacion, ".html")
            save_page_html(crear_pagina_html(contenido_clasificacion, t("site_title"), "..", script_password_lang), file.path(RUTA_SALIDA_RAIZ, nombres_carpetas_relativos$competiciones, nombre_archivo_clasif))
            lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href = nombre_archivo_clasif, class = "menu-button", t_html("standings_title"))
            # 1. Calcular minutos totales POSIBLES solo de partidos JUGADOS.
            minutos_totales_equipo_comp <- valid_partidos_df %>%
              filter(
                competicion_nombre == comp_info$competicion_nombre,
                competicion_temporada == comp_info$competicion_temporada,
                !is.na(id_partido) # \u00a1LA CLAVE! Solo partidos con acta (ID).
              ) %>%
              select(local, visitante, duracion_partido) %>%
              pivot_longer(cols = c(local, visitante), names_to = "tipo_equipo", values_to = "equipo") %>%
              group_by(equipo) %>%
              summarise(minutos_totales_posibles = sum(duracion_partido, na.rm = TRUE), .groups = "drop")

            # 2. Tabla unificada de porteras (sin split por % de minutos).
            # Una fila por jugadora+competici\u00f3n (como goleadoras), con TeamNames_mk "A / B".
            tabla_porteras_comp <- stats_porteras_por_comp_df %>%
              filter(competicion_id == comp_id) %>%
              left_join(jugadoras_stats_df %>% select(id, starts_with("PlayerName_")), by = "id") %>%
              left_join(goal_team_fallback_by_comp, by = c("competicion_id", "id")) %>%
              mutate(
                TeamNames_clean = non_empty_or_na(TeamNames_mk),
                LastTeam_clean = non_empty_or_na(LastTeam_mk),
                TeamNames_mk = coalesce(TeamNames_clean, LastTeam_clean, TeamNames_fallback)
              ) %>%
              filter(Minutes > 0, !is.na(PlayerName_mk)) %>%
              select(id, Pos, starts_with("PlayerName_"), TeamNames_mk, GA90, GA, Minutes, CS)
            if (nrow(tabla_porteras_comp) > 0) {
              generar_tabla_html_porteras <- function(df, table_id) {
                if (is.null(df) || nrow(df) == 0) {
                  return(tags$p(t_html("no_data_in_category")))
                }
                tags$table(id = table_id, `data-sort-col` = "6", `data-sort-dir` = "desc", tags$thead(tags$tr(tags$th(t_html("standings_pos")), tags$th(t_html("player_type")), tags$th(t_html("team_type")), tags$th(class = "sortable-header", onclick = sprintf("sortTable('%s', 3)", table_id), t_html("gk_ga_90")), tags$th(t_html("gk_ga")), tags$th(t_html("stats_minutes")), tags$th(class = "sortable-header desc", onclick = sprintf("sortTable('%s', 6)", table_id), t_html("gk_cs")))), tags$tbody(map(1:nrow(df), function(j) {
                  p <- df[j, ]
                  tags$tr(tags$td(p$Pos), tags$td(tags$a(href = file.path("..", nombres_carpetas_relativos$jugadoras, paste0(p$id, ".html")), player_name_spans_from_row(p))), tags$td(render_team_names(p$TeamNames_mk)), tags$td(format(round(p$GA90, 2), nsmall = 2)), tags$td(p$GA), tags$td(p$Minutes), tags$td(p$CS))
                })))
              }
              contenido_porteras <- tagList(crear_botones_navegacion(".."), tags$h2(tagList(t_html("goalkeepers_title"), " - ", comp_name_spans(comp_info))), generar_tabla_html_porteras(tabla_porteras_comp, "tabla-porteras"))
              nombre_archivo_porteras <- paste0(comp_id, "_golmanki.html")
              save_page_html(crear_pagina_html(contenido_porteras, t("site_title"), "..", script_password_lang), file.path(RUTA_SALIDA_RAIZ, nombres_carpetas_relativos$competiciones, nombre_archivo_porteras))
              lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href = nombre_archivo_porteras, class = "menu-button", t_html("goalkeepers_title"))
            }

            tabla_sanciones_comp <- stats_sanciones_por_comp_df %>%
              filter(competicion_id == comp_id) %>%
              left_join(jugadoras_stats_df %>% select(id, starts_with("PlayerName_")), by = "id") %>%
              left_join(goal_team_fallback_by_comp, by = c("competicion_id", "id")) %>%
              mutate(
                TeamNames_clean = non_empty_or_na(TeamNames_mk),
                LastTeam_clean = non_empty_or_na(LastTeam_mk),
                TeamNames_mk = coalesce(TeamNames_clean, LastTeam_clean, TeamNames_fallback)
              ) %>%
              filter(!is.na(PlayerName_mk)) %>%
              select(Pos, id, starts_with("PlayerName_"), TeamNames_mk, YellowCards, RedCards)
            contenido_sanciones <- tagList(crear_botones_navegacion(".."), tags$h2(tagList(t_html("disciplinary_title"), " - ", comp_name_spans(comp_info))), tags$table(tags$thead(tags$tr(map(tagList(t_html("standings_pos"), t_html("player_type"), t_html("team_type"), HTML("<span class='card-yellow'></span>"), HTML("<span class='card-red'></span>")), tags$th))), tags$tbody(if (nrow(tabla_sanciones_comp) > 0) {
              map(1:nrow(tabla_sanciones_comp), function(j) {
                s <- tabla_sanciones_comp[j, ]
                tags$tr(tags$td(s$Pos), tags$td(tags$a(href = file.path("..", nombres_carpetas_relativos$jugadoras, paste0(s$id, ".html")), player_name_spans_from_row(s))), tags$td(render_team_names(s$TeamNames_mk)), tags$td(s$YellowCards), tags$td(s$RedCards))
              })
            } else {
              tags$tr(tags$td(colspan = "5", t_html("disciplinary_no_cards_message")))
            })))
            nombre_archivo_sanciones <- paste0(comp_id, "_", nombres_archivos_traducidos$sanciones, ".html")
            save_page_html(crear_pagina_html(contenido_sanciones, t("site_title"), "..", script_password_lang), file.path(RUTA_SALIDA_RAIZ, nombres_carpetas_relativos$competiciones, nombre_archivo_sanciones))
            lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href = nombre_archivo_sanciones, class = "menu-button", t_html("disciplinary_title"))
          }

          # ========================================================================
          # == INICIO: NUEVO C\u00d3DIGO PARA GENERAR EL "HUB" DE LA COMPETICI\u00d3N (v2 - Corregido)
          # ========================================================================
          message(paste0("\u00a0 \u00a0 \u00a0 \u00a0 > Generating new Competition Hub for: ", comp_id))

          # --- 0. Definir nombres de archivo por adelantado para evitar errores ---
          nombre_archivo_goleadoras <- paste0(comp_id, "_", nombres_archivos_traducidos$goleadoras, ".html")
          nombre_archivo_sanciones <- paste0(comp_id, "_", nombres_archivos_traducidos$sanciones, ".html")
          nombre_archivo_porteras <- paste0(comp_id, "_golmanki.html")
          nombre_archivo_clasif <- paste0(comp_id, "_", nombres_archivos_traducidos$clasificacion, ".html")

          # --- 1. Logo helper \u2014 uses centralized get_club_logo_path() from 08_functions.R
          get_logo_path_relativo <- function(nombre_equipo_mk) get_club_logo_path(nombre_equipo_mk)

          # --- 2. Preparar datos de JORNADAS (Schedule) ---
          partidos_jugados <- partidos_comp %>% 
            filter(
              !is.na(id_partido),
              !is.na(goles_local),
              !(es_cancelado %in% TRUE)
            )
          jornada_por_defecto_raw <- NA_character_

          if (nrow(partidos_jugados) > 0) {
            jornadas_jugadas_desc <- ordenar_jornadas(unique(partidos_jugados$jornada), descendente = TRUE)
            if (length(jornadas_jugadas_desc) > 0) {
              jornada_por_defecto_raw <- jornadas_jugadas_desc[1]
            }
          }

          if (is.na(jornada_por_defecto_raw) && length(jornadas_comp) > 0) {
            jornada_por_defecto_raw <- jornadas_comp[1]
          }

          datos_jornadas_json <- map(jornadas_comp, function(j) {
            partidos_jornada <- partidos_comp %>%
              filter(jornada == j) %>%
              arrange(local)

            partidos_data <- map(1:nrow(partidos_jornada), function(k) {
              partido <- partidos_jornada[k, ]
              is_placeholder_match <- is.na(partido$id_partido)

              local_name <- entidades_df_lang$current_lang_name[match(partido$local, entidades_df_lang$original_name)]
              visitante_name <- entidades_df_lang$current_lang_name[match(partido$visitante, entidades_df_lang$original_name)]

              is_cancelled <- isTRUE(partido$es_cancelado)
              is_official <- isTRUE(partido$es_resultado_oficial)
              resultado_texto <- if (is_cancelled && !is_official) {
                t("match_cancelled")
              } else if (is_placeholder_match) {
                partido$hora %||% " - "
              } else {
                res_base <- paste(partido$goles_local, "-", partido$goles_visitante)
                if (!is.na(partido$penales_local)) res_base <- sprintf("%s (%s - %s)", res_base, partido$penales_local, partido$penales_visitante)
                if (is_official) res_base <- paste(res_base, "*")
                res_base
              }

              lugar_partido_mk <- estadio_info_mk <- estadios_df %>%
                filter(id_partido == partido$id_partido) %>%
                pull(estadio)
              lugar_partido_lang <- if (length(lugar_partido_mk) > 0) {
                (entidades_df_lang %>% filter(original_name == lugar_partido_mk[1]))$current_lang_name[1]
              } else {
                NA_character_
              }

              list(
                id_partido = partido$id_partido,
                local_lang = as.character(entity_name_spans(partido$local)),
                visitante_lang = as.character(entity_name_spans(partido$visitante)),
                local_logo_path = get_logo_path_relativo(partido$local),
                visitante_logo_path = get_logo_path_relativo(partido$visitante),
                resultado = resultado_texto,
                lugar_lang = if (length(lugar_partido_mk) > 0) as.character(entity_name_spans(lugar_partido_mk[1])) else NA_character_,
                fecha = partido$fecha %||% NA_character_,
                es_cancelado = isTRUE(partido$es_cancelado),
                es_resultado_oficial = isTRUE(partido$es_resultado_oficial)
              )
            })

            list(
              jornada_nombre = {
                clave_jornada_copa <- unname(mapa_claves_ronda_copa[str_squish(as.character(j))])
                if (is_friendly_comp) {
                  as.character(j)
                } else if (is_cup) {
                  as.character(etiqueta_jornada_por_clave(clave_jornada_copa, j))
                } else {
                  as.character(tagList(t_html("round_prefix"), " ", j))
                }
              },
              jornada_id_raw = j,
              partidos = partidos_data
            )
          })

          # --- 3. Preparar datos de ESTAD\u00cdSTICAS (Top 5) ---

          datos_top_goleadoras <- stats_goleadoras_por_comp_df %>%
            filter(competicion_id == comp_id) %>%
            left_join(jugadoras_stats_df %>% select(id, starts_with("PlayerName_")), by = "id") %>%
            left_join(goal_team_fallback_by_comp, by = c("competicion_id", "id")) %>%
            mutate(
              LastTeam_clean = non_empty_or_na(LastTeam_mk),
              LastTeam_mk = coalesce(LastTeam_clean, LastTeam_fallback, TeamNames_fallback)
            ) %>%
            filter(!is.na(PlayerName_mk)) %>%
            head(5) %>%
            rowwise() %>%
            mutate(
              PlayerName = as.character(player_name_spans_from_row(pick(everything()))),
              TeamName = as.character(render_team_names_text(LastTeam_mk)),
              id_jugadora = id, 
              id_equipo_mk = LastTeam_mk,
              link_jugadora = file.path("..", nombres_carpetas_relativos$jugadoras, paste0(id_jugadora, ".html")),
              link_equipo = file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(id_equipo_mk), ".html"))
            ) %>%
            select(Pos, PlayerName, TeamName, Goals, id_jugadora, id_equipo_mk, link_jugadora, link_equipo)

          datos_top_tarjetas <- stats_sanciones_por_comp_df %>%
            filter(competicion_id == comp_id) %>%
            left_join(jugadoras_stats_df %>% select(id, starts_with("PlayerName_")), by = "id") %>%
            left_join(goal_team_fallback_by_comp, by = c("competicion_id", "id")) %>%
            mutate(
              LastTeam_clean = non_empty_or_na(LastTeam_mk),
              LastTeam_mk = coalesce(LastTeam_clean, LastTeam_fallback, TeamNames_fallback)
            ) %>%
            filter(!is.na(PlayerName_mk)) %>%
            head(5) %>%
            rowwise() %>%
            mutate(
              PlayerName = as.character(player_name_spans_from_row(pick(everything()))),
              TeamName = as.character(render_team_names_text(LastTeam_mk)),
              id_jugadora = id,
              id_equipo_mk = LastTeam_mk,
              link_jugadora = file.path("..", nombres_carpetas_relativos$jugadoras, paste0(id_jugadora, ".html")),
              link_equipo = file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(id_equipo_mk), ".html"))
            ) %>%
            select(Pos, PlayerName, TeamName, YellowCards, RedCards, id_jugadora, id_equipo_mk, link_jugadora, link_equipo)

          minutos_totales_equipo_comp <- valid_partidos_df %>%
            filter(
              competicion_nombre == comp_info$competicion_nombre,
              competicion_temporada == comp_info$competicion_temporada,
              !is.na(id_partido)
            ) %>%
            select(local, visitante, duracion_partido) %>%
            pivot_longer(cols = c(local, visitante), names_to = "tipo_equipo", values_to = "equipo") %>%
            group_by(equipo) %>%
            summarise(minutos_totales_posibles = sum(duracion_partido, na.rm = TRUE), .groups = "drop")

          datos_top_porteras <- stats_porteras_por_comp_df %>%
            filter(competicion_id == comp_id, Minutes > 0) %>%
            left_join(jugadoras_stats_df %>% select(id, starts_with("PlayerName_")), by = "id") %>%
            left_join(goal_team_fallback_by_comp, by = c("competicion_id", "id")) %>%
            mutate(
              LastTeam_clean = non_empty_or_na(LastTeam_mk),
              LastTeam_mk = coalesce(LastTeam_clean, LastTeam_fallback, TeamNames_fallback)
            ) %>%
            filter(!is.na(PlayerName_mk)) %>%
            arrange(desc(CS), GA90, desc(Minutes)) %>%
            head(5) %>%
            rowwise() %>%
            mutate(
              PlayerName = as.character(player_name_spans_from_row(pick(everything()))),
              TeamName = as.character(render_team_names_text(LastTeam_mk)),
              id_jugadora = id,
              id_equipo_mk = LastTeam_mk,
              link_jugadora = file.path("..", nombres_carpetas_relativos$jugadoras, paste0(id_jugadora, ".html")),
              link_equipo = file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(id_equipo_mk), ".html"))
            ) %>%
            select(Pos, PlayerName, TeamName, GA90, CS, id_jugadora, id_equipo_mk, link_jugadora, link_equipo)

          # --- 4. Preparar datos de CLASIFICACI\u00d3N (HTML completo) ---
          bloque_html_clasificacion <- if (!is_cup && !is_friendly_comp && !is_placeholder_only_comp) {
            clasificacion_df_comp_raw <- stats_clasificacion_por_comp_df %>% filter(competicion_id == comp_id)
            clave_estilo_comp <- paste(comp_info$competicion_nombre, comp_info$competicion_temporada)

            contenido_tabla <- if (nrow(clasificacion_df_comp_raw) == 0) {
              tags$p(t_html("standings_no_data_message"))
            } else {
              claves_traduccion_standings <- c("standings_pos", "standings_team", "standings_p", "standings_w", "standings_d", "standings_l", "standings_gf", "standings_ga", "standings_gd", "standings_pts")
              estilos_comp <- estilos_clasificacion_data[[clave_estilo_comp]]
              tagList(tags$table(tags$thead(tags$tr(map(map(claves_traduccion_standings, t_html), tags$th))), tags$tbody(map(1:nrow(clasificacion_df_comp_raw), function(j) {
                fila_raw <- clasificacion_df_comp_raw[j, ]
                nombre_equipo_original <- fila_raw$team
                ruta_relativa_logo_html <- get_club_logo_path(nombre_equipo_original)
                regla_actual <- NULL
                if (!is.null(estilos_comp)) {
                  regla_match <- estilos_comp$reglas %>% filter(puesto == fila_raw$Pos)
                  if (nrow(regla_match) > 0) {
                    regla_actual <- regla_match[1, ]
                  }
                }
                tags$tr(
                  tags$td(style = if (!is.null(regla_actual)) paste0("border-left: 5px solid ", regla_actual$color, "; font-weight: bold;") else NULL, fila_raw$Pos),
                  tags$td(class = "team-cell", tags$img(class = "team-logo", src = ruta_relativa_logo_html, alt = "Logo"), tags$a(href = file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(nombre_equipo_original), ".html")), entity_name_spans(nombre_equipo_original)), if (!is.na(fila_raw$puntos_deducidos) && fila_raw$puntos_deducidos > 0) tags$span(style = "color:#8B0000; font-weight:normal; font-size:0.85em;", paste0(" (\u2013", fila_raw$puntos_deducidos, ")")) else NULL),
                  tags$td(fila_raw$P), tags$td(fila_raw$W), tags$td(fila_raw$D), tags$td(fila_raw$L), tags$td(fila_raw$GF), tags$td(fila_raw$GA), tags$td(fila_raw$GD), tags$td(fila_raw$Pts)
                )
              }))), if (!is.null(estilos_comp) && length(estilos_comp$leyenda) > 0) {
                tags$div(class = "legend", map(estilos_comp$leyenda, function(item_leyenda) {
                  tags$div(class = "legend-item", tags$span(class = "legend-color-box", style = paste0("background-color: ", item_leyenda$color, ";")), tags$span(t_html(item_leyenda$texto_key)))
                }))
              })
            }

            tagList(
              tags$div(
                class = "comp-hub-section-header",
                tags$a(
                  href = nombre_archivo_clasif, class = "comp-hub-title-link",
                  tags$h3(class = "comp-hub-section-title", t_html("standings_title"))
                )
              ),
              contenido_tabla
            )
          } else {
            NULL
          }

          # --- 5. Incrustar todo en un paquete JSON ---
          datos_completos_hub_json <- toJSON(list(
            jornadas_data = datos_jornadas_json,
            jornada_por_defecto_raw = jornada_por_defecto_raw,
            stats_goleadoras = datos_top_goleadoras,
            stats_tarjetas = datos_top_tarjetas,
            stats_porteras = datos_top_porteras,
            links = list(
              goleadoras = nombre_archivo_goleadoras,
              tarjetas = nombre_archivo_sanciones,
              porteras = nombre_archivo_porteras,
              clasificacion = if (!is.null(bloque_html_clasificacion)) nombre_archivo_clasif else NA_character_
            ),
            translations = list(
              scorers = as.character(t_html("scorers_title")),
              cards = as.character(t_html("disciplinary_title")),
              goalkeepers = as.character(t_html("goalkeepers_title")),
              see_all = as.character(t_html("see_full_list")),
              stadium = as.character(t_html("match_stadium")),
              match_cancelled = as.character(t_html("match_cancelled")),
              no_matches_in_round = as.character(t_html("match_timeline_no_events"))
            )
          ), auto_unbox = TRUE, na = "null")

          script_datos_hub_json <- tags$script(
            id = "competition-hub-data",
            type = "application/json",
            HTML(datos_completos_hub_json)
          )

          # --- 6. Construir el esqueleto HTML del Hub ---
          if (is_cup) {
            panel_goleadoras_copa <- tagList(
              tags$div(
                class = "comp-hub-section-header",
                tags$h3(class = "comp-hub-section-title", t_html("scorers_title")),
                tags$a(href = nombre_archivo_goleadoras, class = "comp-hub-see-all-link", tagList(t_html("all_results_link"), " >"))
              ),
              tags$div(id = "comp-hub-stats-content-goleadoras", class = "comp-hub-tab-panel active")
            )

            contenido_hub_final <- tagList(
              crear_botones_navegacion(".."),
              tags$h2(comp_name_spans(comp_info)),

              tags$div(
                class = "comp-hub-left-col",
                tags$div(
                  class = "comp-hub-section-header",
                  tags$a(
                    href = nombre_archivo_partidos, class = "comp-hub-title-link",
                    tags$h3(class = "comp-hub-section-title", t_html("schedule_title"))
                  ),
                  tags$a(href = nombre_archivo_partidos, class = "comp-hub-see-all-link", tagList(t_html("all_results_link"), " >"))
                ),
                tags$div(
                  class = "comp-hub-schedule-nav",
                  tags$button(id = "comp-hub-prev-round", class = "comp-hub-nav-arrow", "\u2039"),
                  tags$h4(id = "comp-hub-round-title", ""),
                  tags$button(id = "comp-hub-next-round", class = "comp-hub-nav-arrow", "\u203a")
                ),
                tags$div(id = "comp-hub-schedule-matches")
              ),

              tags$div(
                class = "comp-hub-right-col",
                panel_goleadoras_copa
              )
            )
          } else {
            contenido_hub_final <- tagList(
              crear_botones_navegacion(".."),
              tags$h2(comp_name_spans(comp_info)),

              tags$div(
                class = "comp-hub-container",
                tags$div(
                  class = "comp-hub-left-col",
                  tags$div(
                    class = "comp-hub-section-header",
                    tags$a(
                      href = nombre_archivo_partidos, class = "comp-hub-title-link",
                      tags$h3(class = "comp-hub-section-title", t_html("schedule_title"))
                    ),
                    tags$a(href = nombre_archivo_partidos, class = "comp-hub-see-all-link", tagList(t_html("all_results_link"), " >"))
                  ),
                  tags$div(
                    class = "comp-hub-schedule-nav",
                    tags$button(id = "comp-hub-prev-round", class = "comp-hub-nav-arrow", "\u2039"),
                    tags$h4(id = "comp-hub-round-title", ""),
                    tags$button(id = "comp-hub-next-round", class = "comp-hub-nav-arrow", "\u203a")
                  ),
                  tags$div(id = "comp-hub-schedule-matches")
                ),

                tags$div(
                  class = "comp-hub-right-col",
                  tags$nav(
                    class = "comp-hub-stats-tabs",
                    tags$button(class = "comp-hub-tab-btn active", "data-tab" = "goleadoras", t_html("scorers_title")),
                    tags$button(class = "comp-hub-tab-btn", "data-tab" = "tarjetas", t_html("disciplinary_title")),
                    tags$button(class = "comp-hub-tab-btn", "data-tab" = "porteras", t_html("goalkeepers_title"))
                  ),
                  # Contenedores para cada pesta\u00f1a
                  tags$div(id = "comp-hub-stats-content-goleadoras", class = "comp-hub-tab-panel active"),
                  tags$div(id = "comp-hub-stats-content-tarjetas", class = "comp-hub-tab-panel"),
                  tags$div(id = "comp-hub-stats-content-porteras", class = "comp-hub-tab-panel")
                )
              ),

              # Fila Inferior (Clasificaci\u00f3n)
              tags$div(
                class = "comp-hub-bottom-row",
                bloque_html_clasificacion # El HTML de la clasificaci\u00f3n se inserta directamente
              )
            )
          } # Fin del if/else (is_cup)


          # --- 7. Guardar la p\u00e1gina ---
          pagina_hub_final <- crear_pagina_html(
            contenido_hub_final,
            t("site_title"),
            "..",
            script_password_lang,
            current_page_id = "competitions"
          )

          # A\u00f1adir el JSON al body
          pagina_hub_final$children[[2]]$children <- tagAppendChildren(
            pagina_hub_final$children[[2]]$children,
            script_datos_hub_json
          )

          save_page_html(
            pagina_hub_final,
            file = file.path(RUTA_SALIDA_RAIZ, nombres_carpetas_relativos$competiciones, paste0(comp_id, ".html"))
          )

          # ========================================================================
          # == FIN: NUEVO C\u00d3DIGO PARA GENERAR EL "HUB" DE LA COMPETICI\u00d3N (v2 - Corregido)
          # ========================================================================
        }
      })
    }

    # 13.1.23. Generate Individual Profile Pages.
    if (GENERAR_PERFILES_PARTIDO) {
      indices_partidos <- which(
        !is.na(partidos_df$id_partido) &
          !(sapply(seq_len(nrow(partidos_df)), function(i) isTRUE(partidos_df$es_cancelado[i])) &
            !sapply(seq_len(nrow(partidos_df)), function(i) isTRUE(partidos_df$es_resultado_oficial[i]))) & # Skip cancelled-only, keep official results
          (full_rebuild_needed | partidos_df$id_partido %in% affected_match_ids)
      )
      n_partidos <- length(indices_partidos)
      message(sprintf("   > Generating %d match profiles in parallel...", n_partidos))
      # Pre-package per-match data to avoid exporting resultados_exitosos
      # (50+ MB) to every parallel worker
      resultados_por_id <- setNames(
        resultados_exitosos,
        vapply(resultados_exitosos, \(x) x$partido_info$id_partido, character(1))
      )
      datos_por_partido <- lapply(indices_partidos, function(i) {
        id_p <- partidos_df$id_partido[i]
        list(
          i = i,
          resumen = resultados_por_id[[id_p]]
        )
      })
      rm(resultados_por_id)
      if (n_partidos > 0) {
        with_progress({
          p_partidos <- progressor(steps = n_partidos)
          future_lapply(datos_por_partido, function(pkt) {
            i <- pkt$i
            resumen_partido <- pkt$resumen
            partido_info <- partidos_df[i, ]
            id_p <- partido_info$id_partido

            local_name_spans <- entity_name_spans(partido_info$local)
            visitante_name_spans <- entity_name_spans(partido_info$visitante)

            cronologia <- generar_cronologia_df(id_p, resumen_partido, entidades_all_lang_df, jugadoras_all_lang_df)
            arbitros_partido_mk <- if (exists("arbitros_df") && "id_partido" %in% names(arbitros_df)) {
              arbitros_df %>% filter(id_partido == id_p)
            } else {
              tibble()
            }
            arbitros_partido_lang <- if (nrow(arbitros_partido_mk) > 0 && exists("entidades_df_lang")) {
              arb_lang <- arbitros_partido_mk %>% left_join(entidades_df_lang, by = c("ime" = "original_name"))
              role_order <- c("referee_main", "referee_asst1", "referee_asst2", "referee_asst4", "var_referee", "var_assistant", "var_operator", "match_delegate", "match_kontrolor")
              arb_lang %>% mutate(role_order = match(uloga, role_order, nomatch = length(role_order) + 1)) %>%
                arrange(role_order, row_number()) %>%
                select(-role_order)
            } else {
              tibble()
            }

            staff_partido <- if (exists("staff_df") && "id_partido" %in% names(staff_df)) {
              staff_df %>% filter(id_partido == id_p)
            } else {
              tibble()
            }

            estadio_info_mk <- if (exists("estadios_df") && "id_partido" %in% names(estadios_df)) {
              estadios_df %>%
                filter(id_partido == id_p) %>%
                head(1)
            } else {
              tibble()
            }
            estadio_name_spans <- if (nrow(estadio_info_mk) > 0) entity_name_spans(estadio_info_mk$estadio) else t_html("match_unknown")

            goles_partido <- if (exists("goles_df_unificado") && "id_partido" %in% names(goles_df_unificado)) {
              goles_df_unificado %>%
                filter(id_partido == id_p) %>%
                left_join(jugadoras_lang_df, by = "id")
            } else {
              tibble()
            }

            normalize_team_name <- function(x) {
              x <- as.character(x)
              x <- stringr::str_to_lower(x)
              x <- stringr::str_replace_all(x, "[[:space:]]+", " ")
              x <- stringr::str_trim(x)
              x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
              x <- tolower(x)
              x <- stringr::str_replace_all(x, "[^[:alnum:] ]+", "")
              x <- stringr::str_replace_all(x, "[[:space:]]+", " ")
              x <- stringr::str_trim(x)
              x
            }

            if (nrow(goles_partido) > 0 && exists("entidades_maestro_df")) {
              equipo_lookup <- entidades_maestro_df %>%
                select(original_name, translated_name_mk, translated_name_sq, translated_name_es, translated_name_en) %>%
                tidyr::pivot_longer(cols = starts_with("translated_name"), names_to = "lang", values_to = "team_name") %>%
                filter(is.na(team_name) == FALSE, team_name != "") %>%
                mutate(team_name_norm = normalize_team_name(team_name)) %>%
                distinct(team_name_norm, original_name)

              goles_partido <- goles_partido %>%
                mutate(equipo_acreditado_norm = normalize_team_name(equipo_acreditado)) %>%
                left_join(equipo_lookup, by = c("equipo_acreditado_norm" = "team_name_norm")) %>%
                mutate(
                  equipo_acreditado_canonico = coalesce(original_name, equipo_acreditado),
                  equipo_acreditado_canonico = if_else(equipo_acreditado_canonico == "", NA_character_, equipo_acreditado_canonico),
                  equipo_acreditado_canonico_norm = normalize_team_name(equipo_acreditado_canonico)
                ) %>%
                select(-original_name)
            } else if (nrow(goles_partido) > 0) {
              goles_partido <- goles_partido %>%
                mutate(
                  equipo_acreditado_canonico = equipo_acreditado,
                  equipo_acreditado_canonico_norm = normalize_team_name(equipo_acreditado_canonico)
                )
            }

            if (!"equipo_acreditado_canonico" %in% names(goles_partido)) {
              goles_partido <- goles_partido %>% mutate(equipo_acreditado_canonico = NA_character_)
            }
            if (!"equipo_acreditado_canonico_norm" %in% names(goles_partido)) {
              goles_partido <- goles_partido %>% mutate(equipo_acreditado_canonico_norm = NA_character_)
            }

            tarjetas_partido <- if (exists("tarjetas_df_unificado") && "id_partido" %in% names(tarjetas_df_unificado)) {
              tarjetas_partido <- tarjetas_df_unificado %>%
                filter(id_partido == id_p) %>%
                left_join(jugadoras_lang_df, by = "id")
            } else {
              tibble()
            }
            penales_partido <- if (exists("penales_df_unificado") && "id_partido" %in% names(penales_df_unificado)) {
              penales_df_unificado %>%
                filter(id_partido == id_p) %>%
                left_join(jugadoras_lang_df, by = "id")
            } else {
              tibble()
            }
            partido_comp_info <- competiciones_unicas_df %>% filter(competicion_nombre == partido_info$competicion_nombre, competicion_temporada == partido_info$competicion_temporada)
            comp_name_spans_val <- comp_name_spans(partido_comp_info)
            is_cup_match <- es_competicion_copa(partido_info$competicion_nombre)
            mapa_claves_ronda_copa_match <- if (is_cup_match) {
              partidos_df %>%
                filter(
                  competicion_nombre == partido_info$competicion_nombre,
                  competicion_temporada == partido_info$competicion_temporada
                ) %>%
                crear_mapa_claves_ronda_copa()
            } else {
              setNames(character(0), character(0))
            }
            jornada_texto <- if (partido_info$es_partido_seleccion) {
              partido_info$categoria
            } else if (is_cup_match) {
              clave_jornada_copa <- unname(mapa_claves_ronda_copa_match[str_squish(as.character(partido_info$jornada))])
              etiqueta_jornada_por_clave(clave_jornada_copa, partido_info$jornada)
            } else {
              tagList(t_html("round_prefix"), " ", partido_info$jornada)
            }
            nota_arbitro <- resumen_partido$nota_arbitro %||% NA_character_
            if (!is.na(nota_arbitro)) {
              nota_arbitro <- str_remove(nota_arbitro, "^[\\s:]*")
              # Period before newline \u2192 period + space
              nota_arbitro <- gsub("\\.\\s*[\\r\\n]+\\s*", ". ", nota_arbitro, perl = TRUE)
              # Newline without preceding period \u2192 add period + space
              nota_arbitro <- gsub("\\s*[\\r\\n]+\\s*", ". ", nota_arbitro, perl = TRUE)
              nota_arbitro <- str_trim(nota_arbitro)
            }
            path_rel_competiciones <- file.path("..", nombres_carpetas_relativos$competiciones)
            path_rel_timovi <- file.path("..", nombres_carpetas_relativos$timovi)
            path_rel_jugadoras <- file.path("..", nombres_carpetas_relativos$jugadoras)
            path_rel_arbitros <- file.path("..", nombres_carpetas_relativos$arbitros)
            path_rel_estadios <- file.path("..", nombres_carpetas_relativos$estadios)
            path_rel_staff <- file.path("..", nombres_carpetas_relativos$staff)
            alineacion_partido_lang <- apariciones_df %>%
              filter(id_partido == id_p) %>%
              left_join(jugadoras_lang_df, by = "id")

            # --- Helper: render lineup for one team (new design) ---
            render_equipo_html_new <- function(df_equipo, goles_del_partido, tarjetas_del_partido, is_national_team_match, team_original_mk_name, duracion_partido) {
              if (is.null(df_equipo) || nrow(df_equipo) == 0) {
                return(tags$p(t_html("match_no_data")))
              }
              starters <- df_equipo %>% filter(tipo == "Titular")
              subs <- df_equipo %>%
                filter(tipo == "Suplente") %>%
                arrange(dorsal)

              crear_lista_jugadoras <- function(df_j, duracion_partido) {
                if (nrow(df_j) == 0) {
                  return(tags$p(style = "color:#777;", t_html("match_no_players")))
                }
                tags$ul(map(1:nrow(df_j), function(idx) {
                  row_j <- df_j[idx, ]
                  id <- row_j$id
                  dorsal <- row_j$dorsal
                  tipo <- row_j$tipo
                  es_portera <- row_j$es_portera
                  es_capitana <- row_j$es_capitana
                  min_entra <- row_j$min_entra
                  min_sale <- row_j$min_sale

                  eventos_html <- tagList()
                  goles_jugadora <- goles_del_partido %>% filter(id == !!id, tipo == "Normal")
                  if (nrow(goles_jugadora) > 0) {
                    walk(1:nrow(goles_jugadora), function(g) {
                      gol <- goles_jugadora[g, ]
                      eventos_html <<- tagAppendChild(eventos_html, tags$span(class = "player-event goal", HTML(paste0("\u26bd\ufe0e ", formatear_minuto_partido(gol$minuto, html = TRUE)))))
                    })
                  }
                  tarjetas_jugadora <- tarjetas_del_partido %>% filter(id == !!id)
                  if (nrow(tarjetas_jugadora) > 0) {
                    walk(1:nrow(tarjetas_jugadora), function(c) {
                      tarjeta <- tarjetas_jugadora[c, ]
                      card_span <- tags$span(class = if (tarjeta$tipo == "Amarilla") "card-yellow" else "card-red")
                      eventos_html <<- tagAppendChild(eventos_html, tags$span(class = "player-event", card_span, HTML(paste0("\ufe0e ", formatear_minuto_partido(tarjeta$minuto, html = TRUE)))))
                    })
                  }
                  if (!is.na(min_entra) && tipo == "Suplente") {
                    eventos_html <- tagAppendChild(eventos_html, tags$span(class = "player-event sub-in", HTML(paste0("\u2191", formatear_minuto_partido(min_entra, html = TRUE)))))
                  }
                  if (!is.na(min_sale) && min_sale > 0 && isTRUE(row_j$fue_sustituido)) {
                    eventos_html <- tagAppendChild(eventos_html, tags$span(class = "player-event sub-out", HTML(paste0("\u2193", formatear_minuto_partido(min_sale, html = TRUE)))))
                  }
                  icono_p <- if (isTRUE(es_portera)) "\U0001F9E4\ufe0f" else ""
                  icono_c <- if (isTRUE(es_capitana)) "(C)" else ""
                  should_be_clickable <- !is_national_team_match || (is_national_team_match && team_original_mk_name == "\u041c\u0430\u043a\u0435\u0434\u043e\u043d\u0438\u0458\u0430")
                  
                  player_spans <- player_name_spans_from_row(row_j)
                  player_element <- if (should_be_clickable) {
                    tags$a(href = file.path(path_rel_jugadoras, paste0(id, ".html")), player_spans)
                  } else {
                    player_spans
                  }
                  tags$li(paste0(dorsal, ". "), player_element, icono_p, icono_c, eventos_html)
                }))
              }
              tagList(
                tags$div(class = "mp-lineup-sub-header", t_html("match_starting_lineup")),
                crear_lista_jugadoras(starters, duracion_partido),
                tags$div(class = "mp-lineup-sub-header", t_html("match_substitutes")),
                crear_lista_jugadoras(subs, duracion_partido)
              )
            }

            # --- Helper: render penalties for one team ---
            render_penales_html <- function(df_equipo) {
              if (is.null(df_equipo) || nrow(df_equipo) == 0) {
                return(NULL)
              }
              tags$ul(map(1:nrow(df_equipo), function(idx) {
                row_eq <- df_equipo[idx, ]
                id <- row_eq$id
                dorsal <- row_eq$dorsal
                resultado_penal <- row_eq$resultado_penal
                
                player_spans <- if (is.na(id)) "NA" else player_name_spans_from_row(row_eq)
                
                tags$li(
                  if (resultado_penal == "Gol") "\u2705" else "\u274c", " ",
                  if (is.na(id)) "NA" else tags$a(href = file.path(path_rel_jugadoras, paste0(id, ".html")), player_spans),
                  paste0(" (", dorsal, ")")
                )
              }))
            }

            # --- Helper: render goals summary for one side ---
            render_goals_summary <- function(goles_df_side, align) {
              if (nrow(goles_df_side) == 0) {
                return(NULL)
              }
              goles_agrupados <- goles_df_side %>%
                group_by(id, tipo, across(starts_with("PlayerName_"))) %>%
                summarise(
                  primer_gol = min(map_dbl(minuto, calcular_minuto_sort), na.rm = TRUE),
                  minutos = paste0("(", paste0(sapply(minuto, formatear_minuto_partido, html = TRUE), collapse = ", "), ")"),
                  .groups = "drop"
                ) %>%
                arrange(primer_gol)
              tagList(map(1:nrow(goles_agrupados), function(g_idx) {
                gol <- goles_agrupados[g_idx, ]
                og_label <- if (gol$tipo == "Autogol") tags$span(class = "mp-og-label", t_html("own_goal_label")) else NULL
                should_link <- !partido_info$es_partido_seleccion || partido_info$local == "\u041c\u0430\u043a\u0435\u0434\u043e\u043d\u0438\u0458\u0430" || partido_info$visitante == "\u041c\u0430\u043a\u0435\u0434\u043e\u043d\u0438\u0458\u0430"
                
                player_spans <- player_name_spans_from_row(gol)
                player_el <- if (should_link && !is.na(gol$id)) {
                  tags$a(href = file.path(path_rel_jugadoras, paste0(gol$id, ".html")), tags$span(class = "mp-goal-player", player_spans))
                } else {
                  tags$span(class = "mp-goal-player", player_spans)
                }
                if (align == "home") {
                  tags$div(class = "mp-goal-row", player_el, og_label, tags$span(class = "mp-goal-minute", HTML(gol$minutos)))
                } else {
                  tags$div(class = "mp-goal-row", tags$span(class = "mp-goal-minute", HTML(gol$minutos)), player_el, og_label)
                }
              }))
            }

            # --- Helper: build visual timeline (REDESIGNED) ---
            render_timeline_visual <- function(cronologia, equipo_local_mk) {
              if (!exists("cronologia") || is.null(cronologia) || nrow(cronologia) == 0) {
                return(tags$p(class = "mp-timeline-no-events", t_html("match_timeline_no_events")))
              }
              tags$section(
                class = "mp-timeline-container",
                tags$div(
                  class = "mp-timeline-inner",
                  tags$div(class = "mp-timeline-line"),
                  tagList(map(1:nrow(cronologia), function(c) {
                    e <- cronologia[c, ]
                    es_local <- !is.na(e$equipo_canonico_mk) && e$equipo_canonico_mk == equipo_local_mk
                    minuto_fmt <- formatear_minuto_partido(e$minuto, html = TRUE)

                    # Player names - transliterated spans
                    player_name_spans <- if (!is.na(e$jugadora_nombre)) HTML(e$jugadora_nombre) else ""
                    sub_name_spans <- if (!is.na(e$jugadora_sub_nombre)) HTML(e$jugadora_sub_nombre) else NULL

                    should_link <- !partido_info$es_partido_seleccion || partido_info$local == "\u041c\u0430\u043a\u0435\u0434\u043e\u043d\u0438\u0458\u0430" || partido_info$visitante == "\u041c\u0430\u043a\u0435\u0434\u043e\u043d\u0438\u0458\u0430"
                    
                    player_el <- if (should_link && !is.na(e$jugadora_id) && nchar(e$jugadora_id) > 0) {
                      tags$a(href = file.path(path_rel_jugadoras, paste0(e$jugadora_id, ".html")), class = "player-name", player_name_spans)
                    } else {
                      tags$span(class = "player-name", player_name_spans)
                    }
                    
                    sub_el <- if (!is.null(sub_name_spans)) {
                      if (should_link && !is.na(e$jugadora_sub_id) && nchar(e$jugadora_sub_id) > 0) {
                        tags$a(href = file.path(path_rel_jugadoras, paste0(e$jugadora_sub_id, ".html")), class = "sub-name", sub_name_spans)
                      } else {
                        tags$span(class = "sub-name", sub_name_spans)
                      }
                    } else {
                      NULL
                    }

                    # Event icon logic
                    event_icon <- tags$div(class = "event-icon", HTML(e$icono))

                    # Build the event block content
                    event_block_content <- if (!is.null(e$tipo_evento) && !is.na(e$tipo_evento) && e$tipo_evento == "cambio") {
                      # Stacked for substitution
                      tagList(event_icon, tags$div(class = "player-stack", player_el, sub_el))
                    } else {
                      # Just player and icon
                      tagList(event_icon, player_el)
                    }

                    es_local_event <- isTRUE(e$es_local)

                    if (es_local_event) {
                      tags$div(
                        class = "mp-timeline-event home",
                        tags$div(class = "mp-event-content", event_block_content),
                        tags$div(class = "mp-minute", HTML(minuto_fmt)),
                        tags$div(class = "mp-event-spacer")
                      )
                    } else {
                      tags$div(
                        class = "mp-timeline-event visitor",
                        tags$div(class = "mp-event-spacer"),
                        tags$div(class = "mp-minute", HTML(minuto_fmt)),
                        tags$div(class = "mp-event-content", event_block_content)
                      )
                    }
                  }))
                )
              )
            }

            # --- Build stadium element ---
            estadio_element <- if (nrow(estadio_info_mk) > 0) {
              is_stadium_excluded <- partido_info$es_partido_seleccion && (generar_id_seguro(estadio_info_mk$estadio) %in% stadium_ids_to_skip)
              if (!is_stadium_excluded) {
                tags$a(href = file.path(path_rel_estadios, paste0(generar_id_seguro(estadio_info_mk$estadio), ".html")), estadio_name_spans)
              } else {
                estadio_name_spans
              }
            } else {
              t_html("match_unknown")
            }

            # --- Build referees list (Flexible and order-aware) ---
            # Define standard role sequence for display
            role_order <- c("referee_main", "referee_asst1", "referee_asst2", "referee_asst4", "var_referee", "var_assistant", "var_operator", "match_delegate", "match_kontrolor")
            
            referees_html <- if (nrow(arbitros_partido_lang) > 0) {
              # Sort based on role_order
              arbitros_ordenados <- arbitros_partido_lang %>%
                mutate(sort_priority = match(uloga, role_order)) %>%
                arrange(coalesce(sort_priority, 99))
                
              tags$ul(class = "mp-referees-list", map(1:nrow(arbitros_ordenados), function(a) {
                arb <- arbitros_ordenados[a, ]
                
                # Use tagList to join translated role and ref element, avoiding escaping
                rol_span <- render_role_label(arb$uloga, arb$uloga_raw %||% NA_character_)
                nombre_mostrado <- entity_name_spans(arb$ime)
                
                is_arb_excluded <- partido_info$es_partido_seleccion && (generar_id_seguro(arb$ime) %in% referee_ids_to_skip)
                ref_element <- if (!is_arb_excluded) {
                  tags$a(href = file.path(path_rel_arbitros, paste0(generar_id_seguro(arb$ime), ".html")), nombre_mostrado)
                } else {
                  nombre_mostrado
                }
                
                tags$li(tagList(rol_span, ": "), ref_element)
              }))
            } else {
              NULL
            }

            # --- Build staff list (two-column grid, mirroring lineups) ---
            staff_html <- if (nrow(staff_partido) > 0) {
              staff_partido_lang <- staff_partido %>%
                left_join(entidades_df_lang %>% select(original_name, staff_lang_name = current_lang_name), by = c("nombre" = "original_name")) %>%
                mutate(nombre_mostrado = coalesce(staff_lang_name, nombre))
              staff_local <- staff_partido_lang %>% filter(equipo == partido_info$local)
              staff_visitante <- staff_partido_lang %>% filter(equipo == partido_info$visitante)

              render_staff_equipo <- function(df_staff) {
                if (nrow(df_staff) == 0) {
                  return(tags$ul(class = "mp-staff-list"))
                }
                tags$ul(class = "mp-staff-list", map(1:nrow(df_staff), function(s) {
                  stf <- df_staff[s, ]
                  rol_traducido <- t_html(stf$rol)
                  staff_link <- tags$a(
                    href = file.path(path_rel_staff, paste0(generar_id_seguro(stf$nombre), ".html")),
                    entity_name_spans(stf$nombre)
                  )
                  tags$li(class = "staff-item", tags$span(class = "staff-role-label", tagList(rol_traducido, ":")), " ", staff_link)
                }))
              }

              tags$section(
                class = "mp-staff-section",
                tags$h3(class = "mp-section-title", t_html("staff_title")),
                tags$div(
                  class = "mp-staff-grid",
                  tags$div(
                    class = "mp-staff-col",
                    tags$div(
                      class = "mp-lineup-header",
                      get_logo_tag(partido_info$local, css_class = ""),
                      tags$h4(crear_enlace_equipo_condicional(partido_info$local, entity_name_spans(partido_info$local)))
                    ),
                    render_staff_equipo(staff_local)
                  ),
                  tags$div(
                    class = "mp-staff-col",
                    tags$div(
                      class = "mp-lineup-header",
                      get_logo_tag(partido_info$visitante, css_class = ""),
                      tags$h4(crear_enlace_equipo_condicional(partido_info$visitante, entity_name_spans(partido_info$visitante)))
                    ),
                    render_staff_equipo(staff_visitante)
                  )
                )
              )
            } else {
              NULL
            }

            # --- Goals summary data ---
            goles_resumen_local_name <- resumen_partido$partido_info$local
            goles_resumen_visitante_name <- resumen_partido$partido_info$visitante
            goles_resumen_local_norm <- normalize_team_name(goles_resumen_local_name)
            goles_resumen_visitante_norm <- normalize_team_name(goles_resumen_visitante_name)

            local_norm <- normalize_team_name(partido_info$local)
            visitante_norm <- normalize_team_name(partido_info$visitante)

            if (nrow(goles_partido) > 0) {
              if (!"equipo" %in% names(goles_partido)) {
                goles_partido <- goles_partido %>% mutate(equipo = NA_character_)
              }
              if (!"equipo_acreditado" %in% names(goles_partido)) {
                goles_partido <- goles_partido %>% mutate(equipo_acreditado = NA_character_)
              }
              goles_partido <- goles_partido %>% mutate(
                equipo_acreditado_norm = normalize_team_name(equipo_acreditado),
                equipo_norm = normalize_team_name(equipo),

                # Determinamos si la jugadora pertenece al equipo local
                is_player_local = case_when(
                  # 1. Coincidencias exactas originales
                  equipo == partido_info$local ~ TRUE,
                  equipo == partido_info$visitante ~ FALSE,
                  equipo_acreditado == partido_info$local ~ TRUE,
                  equipo_acreditado == partido_info$visitante ~ FALSE,
                  equipo_acreditado_canonico == partido_info$local ~ TRUE,
                  equipo_acreditado_canonico == partido_info$visitante ~ FALSE,

                  # 2. Coincidencias exactas normalizadas
                  !is.na(equipo_acreditado_canonico_norm) & equipo_acreditado_canonico_norm == local_norm ~ TRUE,
                  !is.na(equipo_acreditado_canonico_norm) & equipo_acreditado_canonico_norm == visitante_norm ~ FALSE,
                  !is.na(equipo_acreditado_norm) & equipo_acreditado_norm == local_norm ~ TRUE,
                  !is.na(equipo_acreditado_norm) & equipo_acreditado_norm == visitante_norm ~ FALSE,
                  !is.na(equipo_norm) & equipo_norm == local_norm ~ TRUE,
                  !is.na(equipo_norm) & equipo_norm == visitante_norm ~ FALSE,

                  # 3. Coincidencias con los nombres del resumen (fallback)
                  !is.na(equipo_acreditado_canonico_norm) & equipo_acreditado_canonico_norm == goles_resumen_local_norm ~ TRUE,
                  !is.na(equipo_acreditado_canonico_norm) & equipo_acreditado_canonico_norm == goles_resumen_visitante_norm ~ FALSE,
                  !is.na(equipo_acreditado_norm) & equipo_acreditado_norm == goles_resumen_local_norm ~ TRUE,
                  !is.na(equipo_acreditado_norm) & equipo_acreditado_norm == goles_resumen_visitante_norm ~ FALSE,
                  !is.na(equipo_norm) & equipo_norm == goles_resumen_local_norm ~ TRUE,
                  !is.na(equipo_norm) & equipo_norm == goles_resumen_visitante_norm ~ FALSE,

                  # 4. Fallback difuso (grepl)
                  grepl(local_norm, equipo_acreditado_canonico_norm, ignore.case = TRUE) ~ TRUE,
                  grepl(visitante_norm, equipo_acreditado_canonico_norm, ignore.case = TRUE) ~ FALSE,
                  grepl(local_norm, equipo_norm, ignore.case = TRUE) ~ TRUE,
                  grepl(visitante_norm, equipo_norm, ignore.case = TRUE) ~ FALSE,
                  TRUE ~ NA
                )
              )

              # 5. Fallback a prueba de balas: buscar a la jugadora en la alineaci\u00f3n del propio partido
              if (any(is.na(goles_partido$is_player_local)) && exists("alineacion_partido_lang")) {
                alineacion_simple <- alineacion_partido_lang %>%
                  select(id, equipo_alineacion = equipo) %>%
                  filter(!is.na(id)) %>%
                  distinct(id, .keep_all = TRUE)

                goles_partido <- goles_partido %>%
                  left_join(alineacion_simple, by = "id") %>%
                  mutate(
                    is_player_local = if_else(
                      is.na(is_player_local) & !is.na(equipo_alineacion),
                      equipo_alineacion == partido_info$local,
                      is_player_local
                    )
                  ) %>%
                  select(-equipo_alineacion)
              }

              # 6. Invertir atribuci\u00f3n si es Autogol (beneficia al contrario)
              goles_partido <- goles_partido %>% mutate(
                is_local_goal = if_else(!is.na(tipo) & tipo == "Autogol", !is_player_local, is_player_local)
              )

              goles_local_df <- goles_partido %>% filter(is_local_goal == TRUE)
              goles_visitante_df <- goles_partido %>% filter(is_local_goal == FALSE)

              # Si a\u00fan quedaran NAs inasignables (muy improbable con la capa de la alineaci\u00f3n)
              goles_na_df <- goles_partido %>% filter(is.na(is_local_goal))
              if (nrow(goles_na_df) > 0) {
                # Los sumamos a local sin forzar roturas aleatorias por la mitad
                goles_local_df <- bind_rows(goles_local_df, goles_na_df)
              }
            } else {
              goles_local_df <- tibble()
              goles_visitante_df <- tibble()
            }

            # --- MAIN CONTENT ---
            contenido_partido <- tagList(
              crear_botones_navegacion(".."),
              tags$div(
                class = "mp-container",

                # === COMPETITION HEADER ===
                tags$section(
                  class = "mp-competition-header",
                  tags$h2(tags$a(href = file.path(path_rel_competiciones, paste0(partido_comp_info$competicion_id, ".html")), comp_name_spans(partido_comp_info))),
                  tags$p(tagList(jornada_texto, " \u00b7 ", partido_info$fecha))
                ),

                # === SCOREBOARD ===
                tags$header(
                  class = "mp-scoreboard",
                  tags$div(
                    class = "mp-team mp-team-home",
                    tags$div(class = "mp-team-name", crear_enlace_equipo_condicional(partido_info$local, entity_name_spans(partido_info$local))),
                    tags$div(class = "mp-team-logo", get_logo_tag(partido_info$local, css_class = ""))
                  ),
                  tags$div(
                    class = "mp-score-container",
                    tags$div(class = "mp-score", paste(partido_info$goles_local, "-", partido_info$goles_visitante)),
                    tags$div(class = "mp-status", t_html("final_score")),
                    if (!is.na(partido_info$penales_local)) {
                      tags$div(class = "mp-penalties-note", tagList("(", t_html("penalties_short"), " ", paste0(partido_info$penales_local, "-", partido_info$penales_visitante, ")")))
                    },
                    if (isTRUE(partido_info$es_resultado_oficial)) {
                      tags$div(class = "mp-official-result", t_html("match_official_result"))
                    }
                  ),
                  tags$div(
                    class = "mp-team mp-team-away",
                    tags$div(class = "mp-team-logo", get_logo_tag(partido_info$visitante, css_class = "")),
                    tags$div(class = "mp-team-name", crear_enlace_equipo_condicional(partido_info$visitante, entity_name_spans(partido_info$visitante)))
                  )
                ),

                # === MATCH INFO (Goals Summary + Details) ===
                tags$section(
                  class = "mp-match-info",
                  # Goals summary box
                  tags$div(
                    class = "mp-info-box mp-goals-summary",
                    tags$div(class = "mp-goals-title", t_html("stats_goals")),
                    tags$div(
                      class = "mp-goals-grid",
                      tags$div(class = "mp-goals-home", render_goals_summary(goles_local_df, "home")),
                      tags$div(class = "mp-goals-away", render_goals_summary(goles_visitante_df, "away"))
                    )
                  ),
                  # Details box
                  tags$div(
                    class = "mp-info-box",
                    tags$div(
                      class = "mp-detail-row",
                      tags$span(paste0("\u23f1\ufe0f ", partido_info$fecha)),
                      tags$span(partido_info$hora)
                    ),
                    tags$div(
                      class = "mp-detail-row",
                      tags$span("\U0001F3DF\ufe0f ", estadio_element)
                    ),
                    tags$div(
                      class = "mp-detail-row",
                      tags$span(referees_html)
                    ),
                    if (!is.na(nota_arbitro) && nchar(nota_arbitro) > 0) {
                      tags$p(class = "mp-notes", nota_arbitro)
                    }
                  )
                ),

                # === VISUAL TIMELINE ===
                render_timeline_visual(cronologia, partido_info$local),

                # === LINEUPS ===
                tags$section(
                  class = "mp-lineups-section",
                  tags$h3(class = "mp-section-title", t_html("lineups_title")),
                  tags$div(
                    class = "mp-lineups-grid",
                    tags$div(
                      class = "mp-lineup",
                      tags$div(
                        class = "mp-lineup-header",
                        get_logo_tag(partido_info$local, css_class = ""),
                        tags$h4(crear_enlace_equipo_condicional(partido_info$local, entity_name_spans(partido_info$local)))
                      ),
                      render_equipo_html_new(
                        filter(alineacion_partido_lang, equipo == partido_info$local),
                        goles_partido, tarjetas_partido,
                        partido_info$es_partido_seleccion, partido_info$local,
                        partido_info$duracion_partido
                      )
                    ),
                    tags$div(
                      class = "mp-lineup",
                      tags$div(
                        class = "mp-lineup-header",
                        get_logo_tag(partido_info$visitante, css_class = ""),
                        tags$h4(crear_enlace_equipo_condicional(partido_info$visitante, entity_name_spans(partido_info$visitante)))
                      ),
                      render_equipo_html_new(
                        filter(alineacion_partido_lang, equipo == partido_info$visitante),
                        goles_partido, tarjetas_partido,
                        partido_info$es_partido_seleccion, partido_info$visitante,
                        partido_info$duracion_partido
                      )
                    )
                  )
                ),

                # === COACHING STAFF (below lineups, low prominence) ===
                staff_html,

                # === PENALTIES (if applicable) ===
                if (!is.na(partido_info$penales_local) && nrow(penales_partido) > 0) {
                  tags$section(
                    class = "mp-penales-section",
                    tags$h3(class = "mp-section-title", t_html("penalties_title")),
                    tags$div(
                      class = "mp-penales-grid",
                      tags$div(
                        tags$h4(entity_name_spans(partido_info$local)),
                        render_penales_html(filter(penales_partido, equipo == partido_info$local))
                      ),
                      tags$div(
                        tags$h4(entity_name_spans(partido_info$visitante)),
                        render_penales_html(filter(penales_partido, equipo == partido_info$visitante))
                      )
                    )
                  )
                }
              ),
              crear_botones_navegacion("..")
            )
            pagina_partido_final <- crear_pagina_html(contenido_partido, t("site_title"), path_to_assets = "..", script_password_lang)
            save_page_html(pagina_partido_final, file = file.path(RUTA_SALIDA_RAIZ, nombres_carpetas_relativos$partidos, paste0(id_p, ".html")))
            p_partidos()
          }, future.seed = TRUE, future.packages = pkgs_paralelos)
        })
      }
    }


    # ==============================================================================
    # == INICIO DEL BLOQUE FINAL v5 DE PERFIL DE JUGADORA (REEMPLAZAR)             ==
    # ==============================================================================
    if (GENERAR_PERFILES_JUGADORA) {
      indices_jugadoras <- which(
        !(jugadoras_stats_df$id %in% player_ids_to_skip) &
          (full_rebuild_needed | jugadoras_stats_df$id %in% affected_player_ids)
      )
      n_jugadoras <- length(indices_jugadoras)
      message(sprintf("   > Generating %d player profiles in parallel...", n_jugadoras))
      if (n_jugadoras > 0) {
        with_progress({
          p_jugadoras <- progressor(steps = n_jugadoras)
          future_lapply(indices_jugadoras, function(i) {
            jugadora <- jugadoras_stats_df[i, ]
            id_j <- jugadora$id

            current_player_name <- jugadora[[player_name_col]]

            # --- 1. CONSTRUIR LA CABECERA DEL PERFIL ---
            info_items <- list()
            if (!is.na(jugadora$codigo_iso)) {
              info_items[[length(info_items) + 1]] <- tags$div(class = "bio-item", tags$div(class = "bio-icon", HTML('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" width="24" height="24"><path d="M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm-1 17.93c-3.95-.49-7-3.85-7-7.93 0-.62.08-1.21.21-1.79L8.35 12H11v7.93zM13 19.93V12h2.65l4.14 2.21c-.43 3.32-3.26 5.95-6.79 6.72zM13 4.07V10h2.65l4.14-2.21C19.37 4.56 16.54 2 13 2.07zM11 4.07c3.53 0 6.37 2.49 6.79 5.72L13 10H11V4.07zM4.26 8.21C4.08 7.43 4 6.64 4 5.86c0-1.03.24-2 .66-2.87l4.14 2.22H4.26zm.43 7.58c.2.6.46 1.17.78 1.7L9.61 14H4.69z"/></svg>')), tags$div(class = "bio-text", tags$span(class = "bio-label", t_html("player_nationality")), tags$span(class = "bio-value", jugadora$nombre_macedonio %||% jugadora$nacionalidad)))
            }
            mapa_pos_traducida <- c("goalkeeper" = t_html("position_goalkeeper"), "defender" = t_html("position_defender"), "midfielder" = t_html("position_midfielder"), "forward" = t_html("position_forward"))
            if (!is.na(jugadora$posicion_final_unificada) && jugadora$posicion_final_unificada != "") {
              posicion_traducida <- tagList(lapply(IDIOMAS_SOPORTADOS, function(l) {
                key <- jugadora$posicion_final_unificada
                # The recode above actually put a tagList into the vector, which is weird in R
                # Better: do it manually
                pos_key <- paste0("position_", jugadora$posicion_final_unificada)
                tags$span(class = paste0("lang-", l), textos[[l]][[pos_key]] %||% jugadora$posicion_final_unificada)
              }))
              info_items[[length(info_items) + 1]] <- tags$div(class = "bio-item", tags$div(class = "bio-icon", HTML('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" width="24" height="24"><path d="M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm0 18c-4.41 0-8-3.59-8-8s3.59-8 8-8 8 3.59 8 8-3.59 8-8 8zm-1-13h2v6h-2zm0 8h2v2h-2z"/></svg>')), tags$div(class = "bio-text", tags$span(class = "bio-label", t_html("player_position")), tags$span(class = "bio-value", posicion_traducida)))
            }
            if (!is.na(jugadora$fecha_nacimiento)) {
              fecha_formateada <- format(as.Date(jugadora$fecha_nacimiento), format = "%d.%m.%Y")
              edad_texto <- if (!is.na(jugadora$edad)) tagList(" (", jugadora$edad, " ", t_html("player_age_suffix"), ")") else ""
              info_items[[length(info_items) + 1]] <- tags$div(class = "bio-item", tags$div(class = "bio-icon", HTML('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" width="24" height="24"><path d="M19 3h-1V1h-2v2H8V1H6v2H5c-1.11 0-1.99.9-1.99 2L3 19c0 1.1.89 2 2 2h14c1.1 0 2-.9 2-2V5c0-1.1-.9-2-2-2zm0 16H5V8h14v11zM7 10h5v5H7z"/></svg>')), tags$div(class = "bio-text", tags$span(class = "bio-label", t_html("player_birth_date")), tags$span(class = "bio-value", tagList(fecha_formateada, edad_texto))))
            }
            ciudad_original_en <- jugadora$ciudad_nacimiento
            if (!is.na(ciudad_original_en) && ciudad_original_en != "") {
              ciudad_a_mostrar <- ciudad_original_en
              if (!is.null(mapa_ciudades_long_df)) {
                traduccion <- mapa_ciudades_long_df %>%
                  filter(.data$lang == idioma_actual, en == ciudad_original_en) %>%
                  pull(translated_city)
                if (length(traduccion) > 0) {
                  ciudad_a_mostrar <- traduccion[1]
                }
              }
              info_items[[length(info_items) + 1]] <- tags$div(class = "bio-item", tags$div(class = "bio-icon", HTML('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" width="24" height="24"><path d="M12 2C8.13 2 5 5.13 5 9c0 5.25 7 13 7 13s7-7.75 7-13c0-3.87-3.13-7-7-7zm0 9.5c-1.38 0-2.5-1.12-2.5-2.5s1.12-2.5 2.5-2.5 2.5 1.12 2.5 2.5-1.12 2.5-2.5 2.5z"/></svg>')), tags$div(class = "bio-text", tags$span(class = "bio-label", t_html("player_birth_place")), tags$span(class = "bio-value", ciudad_a_mostrar)))
            }

            # --- 2. CONSTRUIR TARJETA DE ESTAD\u00cdSTICAS (L\u00d3GICA CORREGIDA) ---
            player_career_data <- career_summary_jugadoras_df %>% filter(id == id_j)
            if (!"equipo" %in% names(player_career_data)) player_career_data$equipo <- NA_character_
            player_career_data <- player_career_data %>%
              mutate(
                competicion_nombre = coalesce(as.character(competicion_nombre), ""),
                competicion_temporada = coalesce(as.character(competicion_temporada), ""),
                equipo = coalesce(as.character(equipo), "")
              )
            stats_summary_card_html <- NULL
            if (nrow(player_career_data) > 0) {
              latest_season_info <- player_career_data %>%
                filter(!str_detect(competicion_nombre, "\u041f\u0440\u0438\u0458\u0430\u0442\u0435\u043b\u0441\u043a\u0438|\u0411\u0430\u0440\u0430\u0436"), competicion_temporada != "") %>%
                mutate(start_year = as.integer(substr(competicion_temporada, 1, 2))) %>%
                arrange(desc(start_year)) %>%
                slice(1)

              if (nrow(latest_season_info) > 0) {
                latest_season <- latest_season_info$competicion_temporada[1]

                # CORRECCI\u00d3N 1: Se a\u00f1ade `Played` a la suma y se recalcula `SubOn`.
                latest_season_stats <- player_career_data %>%
                  filter(competicion_temporada == latest_season, !str_detect(competicion_nombre, "\u041f\u0440\u0438\u0458\u0430\u0442\u0435\u043b\u0441\u043a\u0438|\u0411\u0430\u0440\u0430\u0436")) %>%
                  summarise(
                    Played = sum(Played, na.rm = T), # <-- (A) A\u00d1ADIDO: Sumar los partidos jugados.
                    Starter = sum(Starter, na.rm = T),
                    Goals = sum(Goals, na.rm = T),
                    Yellows = sum(Yellows, na.rm = T),
                    Reds = sum(Reds, na.rm = T)
                  ) %>%
                  mutate(
                    Played = pmax(Played, Starter),
                    SubOn = pmax(0, Played - Starter) # <-- (B) CORREGIDO: impedir valores negativos en SubOn.
                  )

                stats_summary_card_html <- tags$div(
                  class = "stats-summary-card",
                  tags$div(class = "season-tag", latest_season),
                  tags$div(
                    class = "stats-grid",
                    # CORRECCI\u00d3N 2: Se usa `Played` en lugar de `CalledUp` para "Apps".
                    tags$div(
                      class = "stat-item", HTML('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="stat-icon"><path d="M17 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2"></path><circle cx="9" cy="7" r="4"></circle><path d="M23 21v-2a4 4 0 0 0-3-3.87"></path><path d="M16 3.13a4 4 0 0 1 0 7.75"></path></svg>'),
                      tags$span(class = "stat-value", latest_season_stats$Played),
                      tags$span(class = "stat-label", t_html("player_apps"))
                    ),
                    tags$div(
                      class = "stat-item", HTML('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="stat-icon"><circle cx="12" cy="12" r="10"></circle><polyline points="12 6 12 12 16 14"></polyline></svg>'),
                      tags$span(class = "stat-value", latest_season_stats$Starter),
                      tags$span(class = "stat-label", t_html("player_starter"))
                    ),
                    tags$div(
                      class = "stat-item", HTML('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="stat-icon"><polyline points="15 3 21 3 21 9"></polyline><polyline points="9 21 3 21 3 15"></polyline><line x1="21" y1="3" x2="14" y2="10"></line><line x1="3" y1="21" x2="10" y2="14"></line></svg>'),
                      tags$span(class = "stat-value", latest_season_stats$SubOn),
                      tags$span(class = "stat-label", t_html("player_sub_on"))
                    ),
                    tags$div(
                      class = "stat-item", HTML('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="stat-icon"><circle cx="12" cy="12" r="2"></circle><path d="M16.24 7.76l-2.12 2.12M12 2v2M7.76 7.76l2.12 2.12M2 12h2M7.76 16.24l2.12-2.12M12 22v-2M16.24 16.24l-2.12-2.12M22 12h-2"></path></svg>'),
                      tags$span(class = "stat-value", latest_season_stats$Goals),
                      tags$span(class = "stat-label", t_html("player_goals"))
                    ),
                    tags$div(
                      class = "stat-item", HTML('<div class="stat-icon card-icon yellow"></div>'),
                      tags$span(class = "stat-value", latest_season_stats$Yellows),
                      tags$span(class = "stat-label", t_html("player_yellow_cards_short"))
                    ),
                    tags$div(
                      class = "stat-item", HTML('<div class="stat-icon card-icon red"></div>'),
                      tags$span(class = "stat-value", latest_season_stats$Reds),
                      tags$span(class = "stat-label", t_html("player_red_cards_short"))
                    )
                  )
                )
              }
            }

            # --- 3. CONSTRUIR EL ACORDE\u00d3N DE ESTAD\u00cdSTICAS ---

            # Logo helper \u2014 uses centralized get_logo_tag() with "team-logo-small" class
            generar_logo_html <- function(nombre_equipo_mk) get_logo_tag(nombre_equipo_mk, css_class = "team-logo-small")

            career_accordion_html <- NULL
            if (nrow(player_career_data) > 0) {
              partidos_jugadora_details <- apariciones_df %>%
                filter(id == id_j) %>%
                left_join(
                  partidos_df %>% select(
                    id_partido,
                    local,
                    visitante,
                    goles_local,
                    goles_visitante,
                    fecha,
                    es_partido_seleccion,
                    categoria,
                    es_cancelado,
                    competicion_nombre_match = competicion_nombre,
                    competicion_temporada_match = competicion_temporada
                  ),
                  by = "id_partido"
                ) %>%
                mutate(
                  competicion_nombre = coalesce(competicion_nombre_match, competicion_nombre),
                  competicion_temporada = coalesce(competicion_temporada_match, competicion_temporada)
                ) %>%
                select(-competicion_nombre_match, -competicion_temporada_match) %>%
                left_join(entidades_all_lang_df, by = c("local" = "original_name")) %>%
                left_join(entidades_all_lang_df, by = c("visitante" = "original_name"))
              national_team_data_player <- national_team_career_by_category_df %>% filter(id == id_j)
              seasons_club <- player_career_data %>%
                filter(equipo != "\u041c\u0430\u043a\u0435\u0434\u043e\u043d\u0438\u0458\u0430", competicion_temporada != "") %>%
                distinct(competicion_temporada) %>%
                mutate(type = "club")
              seasons_nat <- if (nrow(national_team_data_player) > 0) tibble(competicion_temporada = as.character(t_html("competition_reprezentacija")), type = "national") else tibble()
              all_seasons <- bind_rows(seasons_nat, seasons_club) %>%
                mutate(start_year = if_else(type == "club", as.integer(substr(competicion_temporada, 1, 2)), 999L)) %>%
                arrange(desc(start_year))

              career_accordion_html <- tags$div(
                class = "season-accordion-container",
                tags$h3(t_html("player_stats_by_season")),
                map(1:nrow(all_seasons), function(s_idx) {
                  season_info <- all_seasons[s_idx, ]
                  season_name <- season_info$competicion_temporada
                  season_type <- season_info$type
                  season_id_safe <- paste0(str_replace_all(tolower(season_name), "[^a-z0-9]", "-"), "-", s_idx)
                  if (season_type == "national") {
                    stats_tab_content <- tags$table(
                      class = "stats-table-season",
                      tags$thead(tags$tr(map(tagList(t_html("category_header"), t_html("player_apps"), t_html("player_starter"), t_html("player_goals"), HTML('<span class="card-icon-header yellow"></span>'), HTML('<span class="card-icon-header red"></span>'), t_html("player_mins")), tags$th))),
                      tags$tbody(map(1:nrow(national_team_data_player), function(k) {
                        stage <- national_team_data_player[k, ]
                        tags$tr(tags$td(stage$categoria), tags$td(stage$Played), tags$td(stage$Starter), tags$td(stage$Goals), tags$td(stage$Yellows), tags$td(stage$Reds), tags$td(stage$Minutes))
                      }))
                    )

                    matches_data_season <- partidos_jugadora_details %>%
                      filter(es_partido_seleccion == TRUE, equipo == "\u041c\u0430\u043a\u0435\u0434\u043e\u043d\u0438\u0458\u0430") %>%
                      mutate(fecha_date = as.Date(fecha, format = "%d.%m.%Y")) %>%
                      arrange(desc(fecha_date))
                    categories_in_season <- matches_data_season %>%
                      pull(categoria) %>%
                      unique() %>%
                      sort()
                    matches_tab_content <- tagList(
                      tags$div(class = "sub-tab-nav", map(seq_along(categories_in_season), function(c_idx) {
                        current_category <- categories_in_season[c_idx]
                        tags$button(class = if (c_idx == 1) "sub-tab-button active" else "sub-tab-button", `data-season-id` = season_id_safe, `data-subtab-target` = current_category, current_category)
                      })),
                      map(seq_along(categories_in_season), function(c_idx) {
                        current_category <- categories_in_season[c_idx]
                        matches_in_comp <- matches_data_season %>% filter(categoria == current_category)
                        tags$div(
                          id = paste0("matches-", season_id_safe, "-", current_category), class = if (c_idx == 1) "sub-tab-panel active" else "sub-tab-panel",
                          tags$div(
                            class = "match-list-container",
                            tags$div(class = "match-list-header", tags$div(t_html("team_header_date")), tags$div(t_html("match_header_match")), tags$div(t_html("category_header")), tags$div(t_html("player_goals")), tags$div(t_html("match_header_card")), tags$div(t_html("player_mins"))),
                            map(seq_len(nrow(matches_in_comp)), function(p_idx) {
                              partido_row <- matches_in_comp[p_idx, ]
                              partido_id_val <- if ("id_partido" %in% names(partido_row)) partido_row$id_partido else NA
                              goles_partido_jugadora <- if (!is.na(partido_id_val) && length(partido_id_val) == 1) {
                                goles_df_unificado %>% filter(id_partido == partido_id_val, id == id_j)
                              } else {
                                tibble()
                              }
                              tarjetas_partido_jugadora <- if (!is.na(partido_id_val) && length(partido_id_val) == 1) {
                                tarjetas_df_unificado %>% filter(id_partido == partido_id_val, id == id_j)
                              } else {
                                tibble()
                              }
                              is_cancelled <- isTRUE(partido_row$es_cancelado) && !isTRUE(partido_row$es_resultado_oficial)
                              row_class <- if (is_cancelled) "match-list-row cancelled" else "match-list-row clickable-row"
                              match_href <- if (!is.na(partido_id_val) && partido_id_val != "" && !is_cancelled) file.path("..", nombres_carpetas_relativos$partidos, paste0(partido_id_val, ".html")) else "javascript:void(0)"
                              score_text <- if (is_cancelled) t_html("match_cancelled") else tags$span(paste(partido_row$goles_local, ":", partido_row$goles_visitante))

                              tags$div(
                                class = row_class, `data-href` = match_href,
                                tags$div(class = "cell-date", partido_row$fecha),
                                tags$div(class = "cell-match", tags$span(class = "team-home", entity_name_spans(partido_row$local), generar_logo_html(partido_row$local)), tags$span(class = "match-score", score_text), tags$span(class = "team-away", generar_logo_html(partido_row$visitante), entity_name_spans(partido_row$visitante))),
                                tags$div(class = "cell-competition", t_html(partido_row$categoria)),
                                tags$div(class = "cell-goals", if (nrow(goles_partido_jugadora) > 0) nrow(goles_partido_jugadora) else "0"),
                                tags$div(class = "cell-cards", if (nrow(tarjetas_partido_jugadora) > 0) tagList(if ("Amarilla" %in% tarjetas_partido_jugadora$tipo) tags$span(class = "card-icon-table yellow"), if ("Roja" %in% tarjetas_partido_jugadora$tipo) tags$span(class = "card-icon-table red"))),
                                tags$div(class = "cell-minutes", if (is.na(partido_row$minutos_jugados)) "0" else partido_row$minutos_jugados)
                              )
                            })
                          )
                        )
                      })
                    )
                  } else {
                    stats_data_season <- player_career_data %>%
                      filter(competicion_temporada == season_name) %>%
                      left_join(competiciones_unicas_df %>% select(-any_of("categoria")), by = c("competicion_nombre", "competicion_temporada")) %>%
                      left_join(entidades_all_lang_df, by = c("equipo" = "original_name"))
                    if (!"equipo" %in% names(stats_data_season)) stats_data_season$equipo <- NA_character_
                    stats_data_season <- stats_data_season %>%
                      mutate(
                        competicion_nombre = coalesce(as.character(competicion_nombre), ""),
                        competicion_temporada = coalesce(as.character(competicion_temporada), ""),
                        equipo = coalesce(as.character(equipo), ""),
                        current_lang_name = coalesce(as.character(current_lang_name), equipo)
                      )
                    stats_tab_content <- tags$table(
                      class = "stats-table-season",
                      tags$thead(tags$tr(map(tagList(t_html("player_competition"), t_html("team_type"), t_html("player_apps"), t_html("player_starter"), t_html("player_goals"), HTML('<span class="card-icon-header yellow"></span>'), HTML('<span class="card-icon-header red"></span>'), t_html("player_mins")), tags$th))),
                      tags$tbody(map(1:nrow(stats_data_season), function(k) {
                        stage <- stats_data_season[k, ]
                        comp_url <- file.path("..", nombres_carpetas_relativos$competiciones, paste0(stage$competicion_id, ".html"))
                        team_url <- file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(stage$equipo), ".html"))
                        tags$tr(tags$td(tags$a(href = comp_url, comp_name_spans(stage))), tags$td(class = "team-cell-with-logo", generar_logo_html(stage$equipo), tags$a(href = team_url, entity_name_spans(stage$equipo))), tags$td(stage$Played), tags$td(stage$Starter), tags$td(stage$Goals), tags$td(stage$Yellows), tags$td(stage$Reds), tags$td(stage$Minutes))
                      }))
                    )
                    # CORRECCI\u00d3N AQU\u00cd: Asegurarse de que `partidos_jugadora_details` tiene las columnas necesarias antes de filtrar.
                    matches_data_season <- partidos_jugadora_details %>%
                      left_join(competiciones_unicas_df %>% select(-any_of("categoria")), by = c("competicion_nombre", "competicion_temporada")) %>%
                      filter(competicion_temporada == season_name)

                    competitions_in_season <- matches_data_season %>%
                      distinct(competicion_id, .keep_all = TRUE) %>%
                      filter(!is.na(competicion_id), competicion_id != "") %>%
                      select(competicion_id, competicion_nombre_lang = !!sym(comp_name_col))
                    matches_tab_content <- if (nrow(competitions_in_season) > 0) {
                      tagList(
                        tags$div(class = "sub-tab-nav", map(seq_len(nrow(competitions_in_season)), function(c_idx) {
                          comp <- competitions_in_season[c_idx, ]
                          comp_row <- matches_data_season %>% filter(competicion_id == comp$competicion_id) %>% slice(1)
                          tags$button(class = if (c_idx == 1) "sub-tab-button active" else "sub-tab-button", `data-season-id` = season_id_safe, `data-subtab-target` = comp$competicion_id, comp_name_spans(comp_row))
                        })),
                        map(seq_len(nrow(competitions_in_season)), function(c_idx) {
                          comp <- competitions_in_season[c_idx, ]
                          matches_in_comp <- matches_data_season %>%
                            filter(competicion_id == comp$competicion_id) %>%
                            mutate(fecha_date = as.Date(fecha, format = "%d.%m.%Y")) %>%
                            arrange(desc(fecha_date))
                          tags$div(
                            id = paste0("matches-", season_id_safe, "-", comp$competicion_id), class = if (c_idx == 1) "sub-tab-panel active" else "sub-tab-panel",
                            tags$div(
                              class = "match-list-container",
                              tags$div(class = "match-list-header", tags$div(t_html("team_header_date")), tags$div(t_html("match_header_match")), tags$div(t_html("player_goals")), tags$div(t_html("match_header_card")), tags$div(t_html("player_mins"))),
                              map(seq_len(nrow(matches_in_comp)), function(p_idx) {
                                partido_row <- matches_in_comp[p_idx, ]
                                partido_id_val <- if ("id_partido" %in% names(partido_row)) partido_row$id_partido else NA
                                if (length(partido_id_val) != 1) partido_id_val <- NA

                                goles_partido_jugadora <- if (!is.na(partido_id_val) && !is.null(partido_id_val)) {
                                  goles_df_unificado %>% filter(id_partido == partido_id_val, id == id_j)
                                } else {
                                  tibble()
                                }

                                tarjetas_partido_jugadora <- if (!is.na(partido_id_val) && !is.null(partido_id_val)) {
                                  tarjetas_df_unificado %>% filter(id_partido == partido_id_val, id == id_j)
                                } else {
                                  tibble()
                                }

                                target_href <- if (!is.na(partido_id_val) && nzchar(as.character(partido_id_val))) {
                                  file.path("..", nombres_carpetas_relativos$partidos, paste0(partido_id_val, ".html"))
                                } else {
                                  "#"
                                }

                                tags$div(
                                  class = "match-list-row clickable-row", `data-href` = target_href,
                                  tags$div(class = "cell-date", partido_row$fecha),
                                  tags$div(class = "cell-match", tags$span(class = "team-home", entity_name_spans(partido_row$local), generar_logo_html(partido_row$local)), tags$span(class = "match-score", tags$span(paste(partido_row$goles_local, ":", partido_row$goles_visitante))), tags$span(class = "team-away", generar_logo_html(partido_row$visitante), entity_name_spans(partido_row$visitante))),
                                  tags$div(class = "cell-goals", if (nrow(goles_partido_jugadora) > 0) nrow(goles_partido_jugadora) else "0"),
                                  tags$div(class = "cell-cards", if (nrow(tarjetas_partido_jugadora) > 0) tagList(if ("Amarilla" %in% tarjetas_partido_jugadora$tipo) tags$span(class = "card-icon-table yellow"), if ("Roja" %in% tarjetas_partido_jugadora$tipo) tags$span(class = "card-icon-table red"))),
                                  tags$div(class = "cell-minutes", if (is.na(partido_row$minutos_jugados)) "0" else partido_row$minutos_jugados)
                                )
                              })
                            )
                          )
                        })
                      )
                    } else {
                      tags$p(style = "text-align:center;", t_html("player_no_matches"))
                    }
                  }
                  tags$div(
                    class = "season-accordion",
                    tags$div(class = "season-header", tags$span(class = "season-header-title", season_name), tags$span(class = "season-arrow")),
                    tags$div(
                      class = "season-content",
                      tags$div(
                        class = "tab-nav",
                        tags$button(class = "tab-button active", `data-season-id` = season_id_safe, `data-tab-target` = "stats", t_html("tab_stats")),
                        tags$button(class = "tab-button", `data-season-id` = season_id_safe, `data-tab-target` = "matches", t_html("tab_matches"))
                      ),
                      tags$div(id = paste0("stats-", season_id_safe), class = "tab-panel active", stats_tab_content),
                      tags$div(id = paste0("matches-", season_id_safe), class = "tab-panel", matches_tab_content)
                    )
                  )
                })
              )
            }

            # --- 4. ENSAMBLAR LA P\u00c1GINA FINAL ---

            contenido_jugadora <- tagList(
              crear_botones_navegacion(".."),
              tags$div(class = "player-profile-header-new", tags$div(class = "player-name-container", tags$h2(class = "player-name-new", player_name_spans(id_j))), if (length(info_items) > 0) tags$div(class = "player-bio", info_items)),
              stats_summary_card_html,
              career_accordion_html
            )
            pagina_jugadora_final <- crear_pagina_html(
              contenido_jugadora, t("site_title"),
              path_to_assets = "..",
              script_password_lang
            )
            save_page_html(pagina_jugadora_final, file = file.path(RUTA_SALIDA_RAIZ, nombres_carpetas_relativos$jugadoras, paste0(id_j, ".html")))
            p_jugadoras()
          }, future.seed = TRUE, future.packages = pkgs_paralelos)
        })
      }
    }
    # ============================================================================
    # == FIN DEL BLOQUE FINAL v5 DE PERFIL DE JUGADORA                           ==
    # ============================================================================


    # ================== REEMPLAZA TODO EL CONTENIDO DEL BLOQUE if (GENERAR_PERFILES_EQUIPO) CON ESTO ==================
    if (GENERAR_PERFILES_EQUIPO) {
      message("   > Generating team profiles (v8 - Literal Implementation)...")

      # --- Definimos las funciones auxiliares del script de pruebas UNA VEZ fuera del bucle ---
      find_main_stadium <- function(team_name_mk, seasons_df, estadios_df) {
        category_priority <- c(
          "\u0421\u0435\u043d\u0438\u043e\u0440\u0438", "\u041c\u043b\u0430\u0434\u0438\u043d\u0446\u0438", "\u041a\u0430\u0434\u0435\u0442\u0438", "\u041c\u043b\u0430\u0434\u0438 \u041f\u0438\u043e\u043d\u0435\u0440\u0438", "\u041f\u0438\u043e\u043d\u0435\u0440\u0438", "\u041f\u043e\u043c\u0430\u043b\u0438 \u043f\u0435\u0442\u043b\u0438\u045a\u0430", "\u041f\u0435\u0442\u043b\u0438\u045a\u0430"
        )

        stadium_from_category <- function(category_filter) {
          candidates <- estadios_df %>%
            filter(
              local == team_name_mk,
              !str_detect(competicion_nombre, "\u041f\u0440\u0438\u0458\u0430\u0442\u0435\u043b\u0441\u043a\u0438|\u0411\u0430\u0440\u0430\u0436"),
              !is.na(estadio)
            )
          if ("categoria" %in% names(candidates)) {
            candidates <- candidates %>%
              filter(!is.na(categoria), categoria == category_filter)
          } else {
            candidates <- candidates %>% filter(FALSE)
          }
          candidates %>%
            count(estadio, sort = TRUE) %>%
            slice(1) %>%
            pull(estadio)
        }

        for (category in category_priority) {
          stadium <- stadium_from_category(category)
          if (length(stadium) > 0) {
            return(stadium)
          }
        }

        seasons_sorted <- seasons_df %>%
          mutate(start_year = as.integer(substr(competicion_temporada, 1, 2))) %>%
          arrange(desc(start_year)) %>%
          pull(competicion_temporada) %>%
          unique()
        for (season in seasons_sorted) {
          stadium <- estadios_df %>%
            filter(local == team_name_mk, competicion_temporada == season, !str_detect(competicion_nombre, "\u041f\u0440\u0438\u0458\u0430\u0442\u0435\u043b\u0441\u043a\u0438|\u0411\u0430\u0440\u0430\u0436"), !is.na(estadio)) %>%
            count(estadio, sort = TRUE) %>%
            slice(1) %>%
            pull(estadio)
          if (length(stadium) > 0) {
            return(stadium)
          }
        }
        return(NA_character_)
      }

      # --- Inicia el bucle para cada equipo ---
      todos_equipos <- unique(c(partidos_df$local, partidos_df$visitante))
      equipos_a_generar <- todos_equipos[
        !(todos_equipos %in% team_names_to_skip_mk) &
          (full_rebuild_needed | generar_id_seguro(todos_equipos) %in% affected_team_ids)
      ]
      n_equipos <- length(equipos_a_generar)
      message(sprintf("   > Generating %d team profiles in parallel...", n_equipos))
      if (n_equipos > 0) {
        with_progress({
          p_equipos <- progressor(steps = n_equipos)
          future_lapply(equipos_a_generar, function(team_mk) {
            id_t <- generar_id_seguro(team_mk)

            # ============================================================================
            # == INICIO DE LA L\u00d3GICA LITERAL DEL SCRIPT DE PRUEBAS                       ==
            # ============================================================================

            # --- 3.1. Preparar datos espec\u00edficos del idioma ---
            team_name_spans_val <- entity_name_spans(team_mk)

            # --- 3.2. Preparaci\u00f3n de datos avanzada ---
            stadium_principal_mk <- find_main_stadium(team_mk, valid_partidos_df, estadios_df)
            stadium_principal_spans <- if (!is.na(stadium_principal_mk)) entity_name_spans(stadium_principal_mk) else NULL

            ruta_logo_principal <- get_club_logo_path(team_mk)

            # -- L\u00d3GICA DE DORSAL LITERAL DEL SCRIPT DE PRUEBAS --
            dorsal_principal_df <- apariciones_df %>%
              filter(equipo == team_mk, !is.na(dorsal)) %>%
              group_by(id, competicion_temporada, competicion_nombre) %>%
              count(dorsal, name = "freq", sort = TRUE) %>%
              slice(1) %>%
              ungroup() %>%
              select(id, competicion_temporada, competicion_nombre, dorsal_principal = dorsal)

            comp_category_map <- valid_partidos_df %>%
              filter(!is.na(categoria), competicion_temporada != "") %>%
              distinct(competicion_nombre, competicion_temporada, categoria)

            # -- L\u00d3GICA DE ROSTER LITERAL DEL SCRIPT DE PRUEBAS --
            stats_jugadoras_con_categoria <- stats_jugadoras_por_equipo_temporada_df %>%
              filter(equipo == team_mk) %>%
              left_join(comp_category_map, by = c("competicion_nombre", "competicion_temporada")) %>%
              mutate(category_key = case_when(
                !is.na(categoria) & str_detect(categoria, regex("\u043f\u043e\u043c\u0430\u043b\u0438\\s*\u043f\u0435\u0442\u043b\u0438\u045a\u0430|\u043f\u043e\u043c\u0430\u043b\u0438\\s*\u043f\u0435\u0442\u043b\u0438\u043d\u0458\u0430|pomali\\s*petlinj", ignore_case = TRUE)) ~ "category_pomali_petlinja",
                !is.na(categoria) & str_detect(categoria, regex("\u043f\u0435\u0442\u043b\u0438\u045a\u0430|petlinja|petlinj\u00eb|kids|child|children|(^|\\s)\u0434\u0435\u0442\u0441\u043a\u0430(\\s|$)|(^|\\s)detska(\\s|$)", ignore_case = TRUE)) ~ "category_petlinja",
                !is.na(categoria) & str_detect(categoria, regex("\u043c\u043b\u0430\u0434\u0438.*\u043f\u0438\u043e\u043d\u0435\u0440|mladi.*pioner", ignore_case = TRUE)) ~ "category_mladi_pioneri",
                !is.na(categoria) & str_detect(categoria, regex("\u043f\u0438\u043e\u043d\u0435\u0440|pioner|pioneer", ignore_case = TRUE)) ~ "category_pioneri",
                !is.na(categoria) & str_detect(categoria, regex("\u043c\u043b\u0430\u0434\u0438\u043d|mladin|youth", ignore_case = TRUE)) ~ "category_youth",
                !is.na(categoria) & str_detect(categoria, regex("\u043a\u0430\u0434\u0435\u0442|kadet|cadet", ignore_case = TRUE)) ~ "category_cadet",
                !is.na(categoria) & categoria != "" ~ "category_senior",
                str_detect(competicion_nombre, regex("\u043f\u043e\u043c\u0430\u043b\u0438\\s*\u043f\u0435\u0442\u043b\u0438\u045a\u0430|\u043f\u043e\u043c\u0430\u043b\u0438\\s*\u043f\u0435\u0442\u043b\u0438\u043d\u0458\u0430|pomali\\s*petlinj", ignore_case = TRUE)) ~ "category_pomali_petlinja",
                str_detect(competicion_nombre, regex("\u043f\u0435\u0442\u043b\u0438\u045a\u0430|petlinja|petlinj\u00eb|kids|child|children|(^|\\s)\u0434\u0435\u0442\u0441\u043a\u0430(\\s|$)|(^|\\s)detska(\\s|$)", ignore_case = TRUE)) ~ "category_petlinja",
                str_detect(competicion_nombre, "(?i)\u041f\u043e\u043c\u043b\u0430\u0434.*\u041f\u0438\u043e\u043d\u0435\u0440|\u041c\u043b\u0430\u0434\u0438 \u041f\u0438\u043e\u043d\u0435\u0440\u0438|Mladi Pioneri|Mladi Pioner") ~ "category_mladi_pioneri",
                str_detect(competicion_nombre, "(?i)\u041f\u0438\u043e\u043d\u0435\u0440|Pioner|Pioneer") ~ "category_pioneri",
                str_detect(competicion_nombre, "(?i)\u041c\u043b\u0430\u0434\u0438\u043d\u0441\u043a\u0430|Mladinska|Mladin") ~ "category_youth",
                str_detect(competicion_nombre, "(?i)\u041a\u0430\u0434\u0435\u0442\u0441\u043a\u0430|Kadetksa|Kadet") ~ "category_cadet",
                TRUE ~ "category_senior"
              )) %>%
              left_join(dorsal_principal_df, by = c("id", "competicion_temporada", "competicion_nombre")) %>%
              # Unimos con jugadoras_stats_df para obtener el nombre traducido correcto y la posici\u00f3n
              left_join(jugadoras_stats_df %>% select(id, starts_with("PlayerName_"), posicion_final_unificada), by = "id") %>%
              left_join(competiciones_unicas_df %>% select(-any_of("categoria")), by = c("competicion_nombre", "competicion_temporada")) %>%
              rowwise() %>%
              mutate(CompeticionLang = as.character(comp_name_spans(pick(everything())))) %>%
              ungroup()

            # -- L\u00d3GICA DE CALENDARIO LITERAL DEL SCRIPT DE PRUEBAS --
            partidos_del_equipo_con_categoria <- valid_partidos_df %>%
              filter(local == team_mk | visitante == team_mk, !is.na(id_partido)) %>%
              mutate(
                category_key = case_when(
                  !is.na(categoria) & str_detect(categoria, regex("\u043f\u043e\u043c\u0430\u043b\u0438\\s*\u043f\u0435\u0442\u043b\u0438\u045a\u0430|\u043f\u043e\u043c\u0430\u043b\u0438\\s*\u043f\u0435\u0442\u043b\u0438\u043d\u0458\u0430|pomali\\s*petlinj", ignore_case = TRUE)) ~ "category_pomali_petlinja",
                  !is.na(categoria) & str_detect(categoria, regex("\u043f\u0435\u0442\u043b\u0438\u045a\u0430|petlinja|petlinj\u00eb|kids|child|children|(^|\\s)\u0434\u0435\u0442\u0441\u043a\u0430(\\s|$)|(^|\\s)detska(\\s|$)", ignore_case = TRUE)) ~ "category_petlinja",
                  !is.na(categoria) & str_detect(categoria, regex("\u043c\u043b\u0430\u0434\u0438.*\u043f\u0438\u043e\u043d\u0435\u0440|mladi.*pioner", ignore_case = TRUE)) ~ "category_mladi_pioneri",
                  !is.na(categoria) & str_detect(categoria, regex("\u043f\u0438\u043e\u043d\u0435\u0440\u0438|pioneri|pioneer", ignore_case = TRUE)) ~ "category_pioneri",
                  !is.na(categoria) & str_detect(categoria, regex("\u043c\u043b\u0430\u0434\u0438\u043d|mladin|youth", ignore_case = TRUE)) ~ "category_youth",
                  !is.na(categoria) & str_detect(categoria, regex("\u043a\u0430\u0434\u0435\u0442|kadet|cadet", ignore_case = TRUE)) ~ "category_cadet",
                  !is.na(categoria) & categoria != "" ~ "category_senior",
                  str_detect(competicion_nombre, regex("\u043f\u043e\u043c\u0430\u043b\u0438\\s*\u043f\u0435\u0442\u043b\u0438\u045a\u0430|\u043f\u043e\u043c\u0430\u043b\u0438\\s*\u043f\u0435\u0442\u043b\u0438\u043d\u0458\u0430|pomali\\s*petlinj", ignore_case = TRUE)) ~ "category_pomali_petlinja",
                  str_detect(competicion_nombre, regex("\u043f\u0435\u0442\u043b\u0438\u045a\u0430|petlinja|petlinj\u00eb|kids|child|children|(^|\\s)\u0434\u0435\u0442\u0441\u043a\u0430(\\s|$)|(^|\\s)detska(\\s|$)", ignore_case = TRUE)) ~ "category_petlinja",
                  str_detect(competicion_nombre, "(?i)\u041f\u043e\u043c\u043b\u0430\u0434.*\u041f\u0438\u043e\u043d\u0435\u0440|\u041c\u043b\u0430\u0434\u0438 \u041f\u0438\u043e\u043d\u0435\u0440\u0438|Mladi Pioneri|Mladi Pioner") ~ "category_mladi_pioneri",
                  str_detect(competicion_nombre, "(?i)\u041f\u0438\u043e\u043d\u0435\u0440|Pioner|Pioneer") ~ "category_pioneri",
                  str_detect(competicion_nombre, "(?i)\u041c\u043b\u0430\u0434\u0438\u043d\u0441\u043a\u0430|Mladinska|Mladin") ~ "category_youth",
                  str_detect(competicion_nombre, "(?i)\u041a\u0430\u0434\u0435\u0442\u0441\u043a\u0430|Kadetksa|Kadet") ~ "category_cadet",
                  TRUE ~ "category_senior"
                ),
                category_name = sapply(category_key, function(k) as.character(t_html(k)))
              ) %>%
              left_join(entidades_all_lang_df, by = c("local" = "original_name")) %>%
              mutate(local_lang = map_chr(local, ~as.character(entity_name_spans(.x)))) %>%
              left_join(entidades_all_lang_df, by = c("visitante" = "original_name")) %>%
              mutate(visitante_lang = map_chr(visitante, ~as.character(entity_name_spans(.x)))) %>%
              left_join(competiciones_unicas_df %>% select(-any_of("categoria")), by = c("competicion_nombre", "competicion_temporada")) %>%
              rowwise() %>%
              mutate(CompeticionLang = as.character(comp_name_spans(pick(everything())))) %>%
              ungroup() %>%
              rowwise() %>%
              mutate(
                home_logo_url = get_club_logo_path(local),
                away_logo_url = get_club_logo_path(visitante)
              ) %>%
              ungroup()

            translations_list <- list()
            for (lang in IDIOMAS_SOPORTADOS) {
              translations_list[[lang]] <- list(
                position_goalkeeper = textos[[lang]][["position_goalkeeper"]],
                position_defender = textos[[lang]][["position_defender"]],
                position_midfielder = textos[[lang]][["position_midfielder"]],
                position_forward = textos[[lang]][["position_forward"]],
                col_dorsal = "#",
                col_player = textos[[lang]][["team_roster_player"]],
                col_apps = textos[[lang]][["player_apps_short"]],
                col_mins = textos[[lang]][["player_mins_short"]],
                col_goals = textos[[lang]][["player_goals_short"]],
                col_cards = textos[[lang]][["team_roster_cards"]],
                no_players_found = textos[[lang]][["team_roster_no_players"]],
                no_matches_found = textos[[lang]][["team_schedule_no_matches"]],
                col_date = textos[[lang]][["match_date"]],
                col_match = textos[[lang]][["match_match"]],
                col_competition = textos[[lang]][["player_competition"]],
                category_senior = textos[[lang]][["category_senior"]],
                category_youth = textos[[lang]][["category_youth"]],
                category_cadet = textos[[lang]][["category_cadet"]],
                category_pioneri = textos[[lang]][["category_pioneri"]],
                category_mladi_pioneri = textos[[lang]][["category_mladi_pioneri"]],
                category_pomali_petlinja = textos[[lang]][["category_pomali_petlinja"]],
                category_petlinja = textos[[lang]][["category_petlinja"]],
                match_cancelled = textos[[lang]][["match_cancelled"]]
              )
            }

            # Ensure there are no duplicated categoria columns (categoria.x / categoria.y)
            if (exists("stats_jugadoras_con_categoria") && !is.null(stats_jugadoras_con_categoria) && nrow(stats_jugadoras_con_categoria) > 0) {
              if ("categoria.x" %in% names(stats_jugadoras_con_categoria) || "categoria.y" %in% names(stats_jugadoras_con_categoria) || !"categoria" %in% names(stats_jugadoras_con_categoria)) {
                stats_jugadoras_con_categoria <- stats_jugadoras_con_categoria %>%
                  mutate(
                    categoria = coalesce(
                      if ("categoria" %in% names(.)) as.character(.data$categoria) else NA_character_,
                      if ("categoria.x" %in% names(.)) as.character(.data$`categoria.x`) else NA_character_,
                      if ("categoria.y" %in% names(.)) as.character(.data$`categoria.y`) else NA_character_
                    )
                  ) %>%
                  select(-any_of(c("categoria.x", "categoria.y")))
              }
            }
            if (exists("partidos_del_equipo_con_categoria") && !is.null(partidos_del_equipo_con_categoria) && nrow(partidos_del_equipo_con_categoria) > 0) {
              if ("categoria.x" %in% names(partidos_del_equipo_con_categoria) || "categoria.y" %in% names(partidos_del_equipo_con_categoria) || !"categoria" %in% names(partidos_del_equipo_con_categoria)) {
                partidos_del_equipo_con_categoria <- partidos_del_equipo_con_categoria %>%
                  mutate(
                    categoria = coalesce(
                      if ("categoria" %in% names(.)) as.character(.data$categoria) else NA_character_,
                      if ("categoria.x" %in% names(.)) as.character(.data$`categoria.x`) else NA_character_,
                      if ("categoria.y" %in% names(.)) as.character(.data$`categoria.y`) else NA_character_
                    )
                  ) %>%
                  select(-any_of(c("categoria.x", "categoria.y")))
              }
            }

            datos_para_js <- toJSON(list(
              roster_data = stats_jugadoras_con_categoria,
              matches_data = partidos_del_equipo_con_categoria,
              translations = translations_list
            ), auto_unbox = TRUE)
            script_datos_json <- tags$script(id = "team-page-data", type = "application/json", HTML(datos_para_js))

            # --- 3.4. Construcci\u00f3n del HTML de la p\u00e1gina (Literal) ---
            team_header_final <- tags$div(
              class = "team-header-container-v5",
              tags$div(class = "team-logo-main", tags$img(src = ruta_logo_principal, alt = "Logo")),
              tags$div(
                class = "team-header-info",
                tags$h2(class = "team-name-v3", team_name_spans_val),
                if (!is.null(stadium_principal_spans)) {
                  tags$div(
                    class = "info-item",
                    tags$div(class = "info-icon", HTML('<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 -960 960 960" width="24"><path d="M480-480q33 0 56.5-23.5T560-560q0-33-23.5-56.5T480-640q-33 0-56.5 23.5T400-560q0 33 23.5 56.5T480-480Zm0 400Q319-217 239.5-334.5T160-552q0-150 96.5-239T480-880q127 0 223.5 89T800-552q0 100-79.5 217.5T480-80Z"/></svg>')),
                    tags$div(class = "info-text", tags$span(class = "info-label", t_html("match_stadium")), tags$span(class = "info-value", stadium_principal_spans))
                  )
                }
              )
            )
            # \u00a1CORRECCI\u00d3N! Usando las clases de pesta\u00f1a correctas para que coincida con el estilo de la jugadora.
            tab_navigation <- tags$div(
              class = "tab-nav-v4",
              tags$button(class = "tab-button-v4 active", `data-tab-target` = "roster-panel", t_html("team_tab_roster")),
              tags$button(class = "tab-button-v4", `data-tab-target` = "schedule-panel", t_html("team_tab_schedule"))
            )
            available_seasons_roster <- if (nrow(stats_jugadoras_con_categoria) > 0) sort(unique(stats_jugadoras_con_categoria$competicion_temporada), decreasing = TRUE) else c()
            available_seasons_schedule <- if (nrow(partidos_del_equipo_con_categoria) > 0) sort(unique(partidos_del_equipo_con_categoria$competicion_temporada), decreasing = TRUE) else c()
            roster_panel <- tags$div(
              id = "roster-panel", class = "tab-panel active",
              tags$div(
                class = "filters-and-pills-container",
                tags$div(
                  class = "filter-bar-v2", style = "margin-bottom: 0;",
                  tags$div(class = "filter-group", tags$label(`for` = "roster-season-filter", t_html("player_season")), tags$select(id = "roster-season-filter", map(available_seasons_roster, ~ tags$option(value = .x, .x)))),
                  tags$div(class = "filter-group", tags$label(`for` = "roster-category-filter", t_html("category_header")), tags$select(id = "roster-category-filter"))
                ),
                tags$div(id = "roster-competition-pills", class = "competition-pills-container")
              ),
              tags$div(id = "roster-table-container")
            )
            schedule_panel <- tags$div(
              id = "schedule-panel", class = "tab-panel",
              tags$div(
                class = "filters-and-pills-container",
                tags$div(
                  class = "filter-bar-v2", style = "margin-bottom: 0;",
                  tags$div(class = "filter-group", tags$label(`for` = "schedule-season-filter", t_html("player_season")), tags$select(id = "schedule-season-filter", map(available_seasons_schedule, ~ tags$option(value = .x, .x)))),
                  tags$div(class = "filter-group", tags$label(`for` = "schedule-category-filter", t_html("category_header")), tags$select(id = "schedule-category-filter"))
                ),
                tags$div(id = "schedule-competition-pills", class = "competition-pills-container")
              ),
              tags$div(id = "schedule-table-container")
            )
            contenido_equipo <- tagList(team_header_final, tab_navigation, roster_panel, schedule_panel)

            # --- 4. Ensamblado final ---
            pagina_equipo_final <- crear_pagina_html(contenido_equipo, t("site_title"), path_to_assets = "..", script_password_lang, current_page_id = "teams")
            pagina_equipo_final$children[[2]]$children <- tagAppendChildren(pagina_equipo_final$children[[2]]$children, script_datos_json)
            save_page_html(pagina_equipo_final, file = file.path(RUTA_SALIDA_RAIZ, nombres_carpetas_relativos$timovi, paste0(id_t, ".html")))
            p_equipos()

            # ============================================================================
            # == FIN DE LA L\u00d3GICA LITERAL                                                ==
            # ============================================================================
          }, future.seed = TRUE, future.packages = pkgs_paralelos)
        })
      }
    }


    if (GENERAR_PERFILES_ARBITRO) {
      todos_arbitros <- unique(arbitros_df$ime)
      arbitros_a_generar <- todos_arbitros[
        !(generar_id_seguro(todos_arbitros) %in% referee_ids_to_skip) &
          (full_rebuild_needed | generar_id_seguro(todos_arbitros) %in% affected_referee_ids)
      ]
      n_arbitros <- length(arbitros_a_generar)
      message(sprintf("   > Generating %d referee profiles in parallel...", n_arbitros))
      if (n_arbitros > 0) {
        with_progress({
          p_arbitros <- progressor(steps = n_arbitros)
          future_lapply(arbitros_a_generar, function(arb_mk) {
            id_a <- generar_id_seguro(arb_mk)

            arb_name_spans <- entity_name_spans(arb_mk)
            temporadas_summary <- stats_arbitros_por_temporada_df %>%
              filter(ime == arb_mk) %>%
              left_join(competiciones_unicas_df %>% select(-any_of("categoria")), by = c("competicion_nombre", "competicion_temporada")) %>%
              select(competicion_temporada, competicion_nombre, !!sym(comp_name_col), num_matches)

            path_rel_partidos <- file.path("..", nombres_carpetas_relativos$partidos)

            tbody_content <- if (nrow(temporadas_summary) > 0) {
              map(1:nrow(temporadas_summary), function(j) {
                stage <- temporadas_summary[j, ]
                details_id <- paste0("details-arbitro-", id_a, "-", j)
                comp_row_ref <- competiciones_unicas_df %>% filter(competicion_nombre == stage$competicion_nombre, competicion_temporada == stage$competicion_temporada) %>% slice(1)
                nombre_competicion_mostrado <- comp_name_spans(comp_row_ref)

                historial_stage_mk <- arbitros_df %>%
                  filter(ime == arb_mk) %>%
                  left_join(valid_partidos_df, by = "id_partido") %>%
                  filter(competicion_temporada == stage$competicion_temporada, competicion_nombre == stage$competicion_nombre) %>%
                  mutate(fecha_date = as.Date(fecha, format = "%d.%m.%Y")) %>%
                  arrange(desc(fecha_date))

                historial_stage <- historial_stage_mk %>%
                  left_join(entidades_all_lang_df, by = c("local" = "original_name")) %>%
                  left_join(entidades_all_lang_df, by = c("visitante" = "original_name"))

                tabla_detalles <- tags$table(
                  class = "sp-table",
                  tags$thead(tags$tr(map(tagList(t_html("team_header_date"), t_html("round_prefix"), t_html("match_header_match"), t_html("referee_header_role")), tags$th))),
                  tags$tbody(map(1:nrow(historial_stage), function(p_idx) {
                    partido <- historial_stage[p_idx, ]
                    is_cancelled <- isTRUE(partido$es_cancelado)
                    is_official <- isTRUE(partido$es_resultado_oficial)
                    resultado_txt <- if (is_cancelled && !is_official) {
                      t_html("match_cancelled")
                    } else {
                      res <- paste(partido$goles_local, "-", partido$goles_visitante)
                      if (is_official) res <- paste(res, "*")
                      res
                    }
                    has_link <- !is_cancelled || is_official
                    match_href <- if (has_link) file.path(path_rel_partidos, paste0(partido$id_partido, ".html")) else "javascript:void(0)"
                    tags$tr(
                      class = if (is_cancelled && !is_official) "sp-match-row-cancelled" else "sp-clickable-row",
                      onclick = if (has_link) paste0("window.location='", match_href, "'") else "",
                      tags$td(partido$fecha),
                      tags$td(partido$jornada),
                      tags$td(
                        tags$span(
                          class = "sp-match-cell",
                          get_logo_tag(partido$local, css_class = "sp-table-logo"),
                          tags$span(class = "sp-team-name", entity_name_spans(partido$local)),
                          tags$span(class = "sp-result", resultado_txt),
                          tags$span(class = "sp-team-name", entity_name_spans(partido$visitante)),
                          get_logo_tag(partido$visitante, css_class = "sp-table-logo")
                        )
                      ),
                      tags$td(render_role_label(partido$uloga, partido$uloga_raw %||% NA_character_))
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
              tags$tr(tags$td(colspan = "3", t_html("player_no_matches")))
            }

            ciudad_arbitra <- (arbitros_df %>% filter(ime == arb_mk) %>% slice(1))$ciudad

            titulo_perfil_arbitra <- tags$h2(
              arb_name_spans,
              if (!is.na(ciudad_arbitra)) {
                tags$span(style = "font-size: 0.6em; color: #555; vertical-align: middle; font-weight: normal;", paste0("(", ciudad_arbitra, ")"))
              }
            )

            contenido_arbitro <- tagList(
              crear_botones_navegacion(".."),
              tags$div(
                class = "sp-container",
                tags$div(class = "sp-header", titulo_perfil_arbitra),
                tags$section(
                  class = "sp-history-section",
                  tags$h3(class = "sp-section-title", t_html("referee_history_by_competition")),
                  tags$table(
                    class = "sp-table",
                    tags$thead(tags$tr(tags$th(t_html("player_season")), tags$th(t_html("player_competition")), tags$th(t_html("referee_header_matches")))),
                    tags$tbody(tbody_content)
                  )
                )
              )
            )

            pagina_arbitro_final <- crear_pagina_html(
              contenido_principal = contenido_arbitro,
              titulo_pagina = t("site_title"),
              path_to_assets = "..",
              script_password = script_password_lang
            )
            save_page_html(pagina_arbitro_final, file = file.path(RUTA_SALIDA_RAIZ, nombres_carpetas_relativos$arbitros, paste0(id_a, ".html")))
            p_arbitros()
          }, future.seed = TRUE, future.packages = pkgs_paralelos)
        })
      }
    }

    if (GENERAR_PERFILES_ESTADIO) {
      todos_estadios <- unique(na.omit(estadios_df$estadio))
      estadios_a_generar <- todos_estadios[
        !(generar_id_seguro(todos_estadios) %in% stadium_ids_to_skip) &
          (full_rebuild_needed | generar_id_seguro(todos_estadios) %in% affected_stadium_ids)
      ]
      n_estadios <- length(estadios_a_generar)
      message(sprintf("   > Generating %d stadium profiles in parallel...", n_estadios))
      if (n_estadios > 0) {
        with_progress({
          p_estadios <- progressor(steps = n_estadios)
          future_lapply(estadios_a_generar, function(est_mk) {
            id_e <- generar_id_seguro(est_mk)
            est_name_spans <- entity_name_spans(est_mk)
            historial_mk <- estadios_df %>%
              filter(estadio == est_mk) %>%
              left_join(
                valid_partidos_df %>% select(id_partido, goles_local, goles_visitante, jornada, es_cancelado, es_resultado_oficial),
                by = "id_partido"
              ) %>%
              mutate(fecha_date = as.Date(fecha, format = "%d.%m.%Y")) %>%
              arrange(desc(fecha_date))
            historial <- historial_mk %>%
              left_join(entidades_all_lang_df, by = c("local" = "original_name")) %>%
              left_join(entidades_all_lang_df, by = c("visitante" = "original_name")) %>%
              left_join(competiciones_unicas_df %>% select(-any_of("categoria")), by = c("competicion_nombre", "competicion_temporada")) %>%
              filter(!is.na(id_partido))
            path_rel_partidos <- file.path("..", nombres_carpetas_relativos$partidos)

            contenido_estadio <- tagList(
              crear_botones_navegacion(".."),
              tags$div(
                class = "sp-container",

                # Header
                tags$div(
                  class = "sp-header",
                  tags$h2(est_name_spans)
                ),

                # Match history
                tags$section(
                  class = "sp-history-section",
                  tags$h3(class = "sp-section-title", t_html("stadium_match_history")),
                  tags$table(
                    class = "sp-table",
                    tags$thead(tags$tr(map(tagList(t_html("team_header_date"), t_html("player_competition"), t_html("round_prefix"), t_html("match_header_match")), tags$th))),
                    tags$tbody(
                      if (nrow(historial) > 0) {
                        map(1:nrow(historial), function(p_idx) {
                          partido <- historial[p_idx, ]
                          nombre_comp <- partido[[comp_name_col]]
                          if (is.null(nombre_comp) || is.na(nombre_comp) || trimws(as.character(nombre_comp)) == "" || tolower(trimws(as.character(nombre_comp))) == "na") {
                            nombre_comp <- partido$competicion_nombre %||% ""
                          }
                          is_cancelled <- isTRUE(partido$es_cancelado)
                          is_official <- isTRUE(partido$es_resultado_oficial)
                          resultado_txt <- if (is_cancelled && !is_official) {
                            t_html("match_cancelled")
                          } else {
                            res_txt <- paste(partido$goles_local, "-", partido$goles_visitante)
                            if (is_official) res_txt <- paste(res_txt, "*")
                            tags$span(res_txt)
                          }
                          has_link <- !is_cancelled || is_official
                          match_href <- if (has_link) file.path(path_rel_partidos, paste0(partido$id_partido, ".html")) else "javascript:void(0)"
                          tags$tr(
                            class = if (is_cancelled && !is_official) "sp-match-row-cancelled" else "sp-clickable-row",
                            onclick = if (has_link) paste0("window.location='", match_href, "'") else "",
                            tags$td(partido$fecha),
                            tags$td(nombre_comp),
                            tags$td(partido$jornada),
                            tags$td(
                              tags$span(
                                class = "sp-match-cell",
                                get_logo_tag(partido$local, css_class = "sp-table-logo"),
                                tags$span(class = "sp-team-name", entity_name_spans(partido$local)),
                                tags$span(class = "sp-result", resultado_txt),
                                tags$span(class = "sp-team-name", entity_name_spans(partido$visitante)),
                                get_logo_tag(partido$visitante, css_class = "sp-table-logo")
                              )
                            )
                          )
                        })
                      } else {
                        tags$tr(tags$td(colspan = "4", t_html("player_no_matches")))
                      }
                    )
                  )
                )
              ),
              crear_botones_navegacion("..")
            )
            pagina_estadio_final <- crear_pagina_html(contenido_estadio, t("site_title"), path_to_assets = "..", script_password_lang)
            save_page_html(pagina_estadio_final, file = file.path(RUTA_SALIDA_RAIZ, nombres_carpetas_relativos$estadios, paste0(id_e, ".html")))
            p_estadios()
          }, future.seed = TRUE, future.packages = pkgs_paralelos)
        })
      }
    }

    # ================== STAFF PROFILES ==================
    if (GENERAR_PERFILES_STAFF && exists("staff_df") && nrow(staff_df) > 0) {
      staff_con_perfil <- staff_df
      todos_staff <- unique(staff_con_perfil$nombre)

      # Determine which staff to (re)generate
      staff_ids_to_skip <- if (exists("staff_names_to_skip") && length(staff_names_to_skip) > 0) generar_id_seguro(staff_names_to_skip) else character(0)
      if (!exists("affected_staff_ids")) affected_staff_ids <- character(0)
      staff_a_generar <- todos_staff[
        !(generar_id_seguro(todos_staff) %in% staff_ids_to_skip) &
          (full_rebuild_needed | generar_id_seguro(todos_staff) %in% affected_staff_ids)
      ]
      n_staff <- length(staff_a_generar)
      message(sprintf("   > Generating %d staff profiles in parallel...", n_staff))

      if (n_staff > 0) {
        with_progress({
          p_staff <- progressor(steps = n_staff)
          future_lapply(staff_a_generar, function(staff_mk) {
            id_s <- generar_id_seguro(staff_mk)

            staff_name_spans <- entity_name_spans(staff_mk)

            temporadas_summary <- stats_staff_por_temporada_df %>%
              filter(nombre == staff_mk) %>%
              left_join(competiciones_unicas_df %>% select(-any_of("categoria")), by = c("competicion_nombre", "competicion_temporada")) %>%
              select(competicion_temporada, competicion_nombre, !!sym(comp_name_col), num_matches)

            path_rel_partidos <- file.path("..", nombres_carpetas_relativos$partidos)

            tbody_content <- if (nrow(temporadas_summary) > 0) {
              map(1:nrow(temporadas_summary), function(j) {
                stage <- temporadas_summary[j, ]
                details_id <- paste0("details-staff-", id_s, "-", j)
                comp_row_ref <- competiciones_unicas_df %>% filter(competicion_nombre == stage$competicion_nombre, competicion_temporada == stage$competicion_temporada) %>% slice(1)
                nombre_competicion_mostrado <- comp_name_spans(comp_row_ref)

                historial_stage_mk <- staff_con_perfil %>%
                  filter(nombre == staff_mk) %>%
                  left_join(valid_partidos_df, by = "id_partido") %>%
                  filter(
                    competicion_temporada == stage$competicion_temporada,
                    competicion_nombre == stage$competicion_nombre
                  ) %>%
                  mutate(fecha_date = as.Date(fecha, format = "%d.%m.%Y")) %>%
                  arrange(desc(fecha_date))

                historial_stage <- historial_stage_mk %>%
                  left_join(entidades_all_lang_df, by = c("local" = "original_name")) %>%
                  left_join(entidades_all_lang_df, by = c("visitante" = "original_name"))

                if ("equipo" %in% names(historial_stage)) {
                  historial_stage <- historial_stage %>%
                    left_join(entidades_df_lang %>% select(original_name, team_name = current_lang_name),
                      by = c("equipo" = "original_name")
                    )
                } else {
                  historial_stage <- historial_stage %>% mutate(team_name = NA_character_)
                }

                tabla_detalles <- tags$table(
                  class = "sp-table",
                  tags$thead(tags$tr(
                    tags$th(t_html("team_header_date")),
                    tags$th(t_html("staff_header_team")),
                    tags$th(t_html("match_header_match")),
                    tags$th(t_html("staff_header_role"))
                  )),
                  tags$tbody(map(1:nrow(historial_stage), function(p_idx) {
                    partido <- historial_stage[p_idx, ]
                    is_cancelled <- isTRUE(partido$es_cancelado)
                    is_official <- isTRUE(partido$es_resultado_oficial)
                    resultado_txt <- if (is_cancelled && !is_official) {
                      t_html("match_cancelled")
                    } else {
                      res <- paste(partido$goles_local, "-", partido$goles_visitante)
                      if (is_official) res <- paste(res, "*")
                      res
                    }
                    has_link <- !is_cancelled || is_official
                    match_href <- if (has_link) file.path(path_rel_partidos, paste0(partido$id_partido, ".html")) else "javascript:void(0)"
                    rol_traducido <- t(partido$rol)
                    team_display <- if (!is.na(partido$team_name)) partido$team_name else ""
                    tags$tr(
                      class = if (is_cancelled && !is_official) "sp-match-row-cancelled" else "sp-clickable-row",
                      onclick = if (has_link) paste0("window.location='", match_href, "'") else "",
                      tags$td(partido$fecha),
                      tags$td(if (length(partido$equipo) > 0 && !is.na(partido$equipo)) entity_name_spans(partido$equipo) else ""),
                      tags$td(
                        tags$span(
                          class = "sp-match-cell",
                          get_logo_tag(partido$local, css_class = "sp-table-logo"),
                          tags$span(class = "sp-team-name", entity_name_spans(partido$local)),
                          tags$span(class = "sp-result", resultado_txt),
                          tags$span(class = "sp-team-name", entity_name_spans(partido$visitante)),
                          get_logo_tag(partido$visitante, css_class = "sp-table-logo")
                        )
                      ),
                      tags$td(t_html(partido$rol))
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
              tags$tr(tags$td(colspan = "3", t_html("player_no_matches")))
            }

            contenido_staff <- tagList(
              crear_botones_navegacion(".."),
              tags$div(
                class = "sp-container",
                tags$div(class = "sp-header", tags$h2(staff_name_spans)),
                tags$section(
                  class = "sp-history-section",
                  tags$h3(class = "sp-section-title", t_html("staff_history_by_competition")),
                  tags$table(
                    class = "sp-table",
                    tags$thead(tags$tr(
                      tags$th(t_html("player_season")),
                      tags$th(t_html("player_competition")),
                      tags$th(t_html("staff_header_matches"))
                    )),
                    tags$tbody(tbody_content)
                  )
                )
              )
            )

            pagina_staff_final <- crear_pagina_html(
              contenido_principal = contenido_staff,
              titulo_pagina = t("site_title"),
              path_to_assets = "..",
              script_password = script_password_lang
            )
            save_page_html(pagina_staff_final, file = file.path(RUTA_SALIDA_RAIZ, nombres_carpetas_relativos$staff, paste0(id_s, ".html")))
            p_staff()
          }, future.seed = TRUE, future.packages = pkgs_paralelos)
        })
      }
    }
  # --- (Redirect page no longer needed in flat architecture) ---

  # --- Restore sequential execution ---
  plan(sequential)
}
