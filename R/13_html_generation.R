#### 13. HTML PAGE GENERATION (OPTIMIZED ARCHITECTURE) ####

if (hubo_cambios) {
  
  # --- Parallelization and progress bar setup ---
  n_workers <- max(1, parallel::detectCores() - 1)
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
          sapply(DisplayName, generar_terminos_busqueda, USE.NAMES = FALSE),
          sapply(LatinName, generar_terminos_busqueda, USE.NAMES = FALSE),
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
    
    # Search entity generation — uses centralized generar_search_entidad() from 08_functions.R
    search_equipos <- generar_search_entidad(entidades_maestro_lang_df, nombres_equipos, "team_type", "equipo-")
    search_arbitros <- generar_search_entidad(entidades_maestro_lang_df, nombres_arbitros, "referee_type", "arbitro-")
    search_estadios <- generar_search_entidad(entidades_maestro_lang_df, nombres_estadios, "stadium_type", "стадион-")
    nombres_staff_search <- if (exists("staff_df") && nrow(staff_df) > 0) unique(staff_df$nombre) else character(0)
    search_staff <- if (length(nombres_staff_search) > 0) generar_search_entidad(entidades_maestro_lang_df, nombres_staff_search, "staff_type", "staff-") else tibble()
    
    # --- 3. Competiciones ---
    # Las competiciones son un caso especial, pero aplicamos la misma lógica
    search_competiciones <- competiciones_unicas_df %>% 
      mutate(
        Име = !!sym(comp_name_col),
        LatinName = !!sym("nombre_completo_en"), # 'en' como referencia latina
        Тип = t("competition_type"), 
        target_id = paste0("menu-competicion-", competicion_id), 
        search_terms = paste(
          sapply(Име, generar_terminos_busqueda, USE.NAMES = FALSE),
          sapply(LatinName, generar_terminos_busqueda, USE.NAMES = FALSE),
          sapply(nombre_completo_mk, generar_terminos_busqueda, USE.NAMES = FALSE)
        )
      ) %>% 
      select(Име, Тип, target_id, search_terms)
    
    # --- 4. Unir todo ---
    search_index_df_lang <- bind_rows(search_jugadoras, search_equipos, search_arbitros, search_competiciones, search_estadios, search_staff) %>% 
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
            
            # 13.1.12. Team logo tag — uses centralized get_logo_tag() from 08_functions.R
            
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
            ordenar_jornadas(unique(partidos_comp$jornada))
          } else { c() }    
          contenido_partidos <- tagList(crear_botones_navegacion(path_to_lang_root = ".."), tags$h2(paste(t("schedule_title"), "-", comp_nombre_current_lang)), map(jornadas_comp, function(j) { partidos_jornada <- partidos_comp %>% filter(jornada == j) %>% arrange(local); header_text <- if(is_cup || is_friendly_comp) as.character(j) else paste(t("round_prefix"), j);  tagList(tags$h3(class="jornada-header", header_text), map(1:nrow(partidos_jornada), function(k) { partido <- partidos_jornada[k,]; is_placeholder_match <- is.na(partido$id_partido); local_name <- entidades_df_lang$current_lang_name[match(partido$local, entidades_df_lang$original_name)]; visitante_name <- entidades_df_lang$current_lang_name[match(partido$visitante, entidades_df_lang$original_name)]; resultado_texto <- if (is_placeholder_match) " - " else { res_base <- paste(partido$goles_local, "-", partido$goles_visitante); if (!is.na(partido$penales_local)) res_base <- sprintf("%s (%s - %s)", res_base, partido$penales_local, partido$penales_visitante); if (isTRUE(partido$es_resultado_oficial)) res_base <- paste(res_base, "*"); res_base }; contenido_comun <- tagList(tags$span(class="equipo equipo-local", get_logo_tag(partido$local), tags$span(local_name)), tags$span(class="resultado", resultado_texto), tags$span(class="equipo equipo-visitante", tags$span(visitante_name), get_logo_tag(partido$visitante))); if (is_placeholder_match) tags$div(class = "partido-link-placeholder", contenido_comun) else tags$a(class = "partido-link", href = file.path("..", nombres_carpetas_relativos$partidos, paste0(partido$id_partido, ".html")), contenido_comun) })) }))
          nombre_archivo_partidos <- paste0(comp_id, "_", nombres_archivos_traducidos$partidos, ".html"); save_html(crear_pagina_html(contenido_partidos, paste(t("schedule_title"), "-", comp_nombre_current_lang), "../..", script_contraseña_lang), file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, nombre_archivo_partidos))
          lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href=nombre_archivo_partidos, class="menu-button", t("schedule_title"))
          if (!is_placeholder_only_comp) {
            tabla_goleadoras_comp <- stats_goleadoras_por_comp_df %>% filter(competicion_id == comp_id) %>% left_join(jugadoras_stats_df %>% select(id, !!player_name_col_sym), by = "id") %>% filter(!is.na(!!player_name_col_sym)) %>% select(Pos, id, PlayerName = !!player_name_col_sym, TeamNames_mk, Goals); headers_traducidos <- c(t("standings_pos"), t("player_type"), t("team_type"), t("stats_goals")); contenido_goleadoras <- tagList(crear_botones_navegacion(path_to_lang_root = ".."), tags$h2(paste(t("scorers_title"), "-", comp_nombre_current_lang)), tags$table(tags$thead(tags$tr(map(headers_traducidos, tags$th))), tags$tbody(map(1:nrow(tabla_goleadoras_comp), function(j){ g <- tabla_goleadoras_comp[j,]; tags$tr(tags$td(g$Pos), tags$td(tags$a(href=file.path("..", nombres_carpetas_relativos$jugadoras, paste0(g$id, ".html")), g$PlayerName)), tags$td({ teams_mk <- str_split(g$TeamNames_mk, " / ")[[1]]; team_tags <- list(); for (i in seq_along(teams_mk)) { team_name_mk <- teams_mk[i]; team_name <- entidades_df_lang %>% filter(original_name == team_name_mk) %>% pull(current_lang_name); team_element <- tags$span(class="team-cell", get_logo_tag(team_name_mk), tags$a(href = file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(team_name_mk), ".html")), team_name)); team_tags <- append(team_tags, list(team_element)); if (i < length(teams_mk)) { team_tags <- append(team_tags, list(tags$span(style="margin: 0 5px;", "/"))) } }; tagList(team_tags) }), tags$td(g$Goals)) }))))
            nombre_archivo_goleadoras <- paste0(comp_id, "_", nombres_archivos_traducidos$goleadoras, ".html"); save_html(crear_pagina_html(contenido_goleadoras, paste(t("scorers_title"), "-", comp_nombre_current_lang), "../..", script_contraseña_lang), file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, nombre_archivo_goleadoras))
            lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href=nombre_archivo_goleadoras, class="menu-button", t("scorers_title"))
          }
          if (!is_cup && !is_friendly_comp && !is_placeholder_only_comp) {
            clasificacion_df_comp_raw <- stats_clasificacion_por_comp_df %>% filter(competicion_id == comp_id); clave_estilo_comp <- paste(comp_info$competicion_nombre, comp_info$competicion_temporada); contenido_tabla <- if (nrow(clasificacion_df_comp_raw) == 0) { tags$p(t("standings_no_data_message")) } else { clasificacion_df_comp_raw_lang <- clasificacion_df_comp_raw %>% left_join(entidades_df_lang, by = c("team" = "original_name")) %>% select(Pos, team_lang = current_lang_name, P, W, D, L, GF, GA, GD, Pts); nombres_neutros <- c("Pos", "team_lang", "P", "W", "D", "L", "GF", "GA", "GD", "Pts"); claves_traduccion <- c("standings_pos", "standings_team", "standings_p", "standings_w", "standings_d", "standings_l", "standings_gf", "standings_ga", "standings_gd", "standings_pts"); nombres_traducidos <- sapply(claves_traduccion, t, USE.NAMES = FALSE); mapa_nombres_col <- setNames(as.list(nombres_neutros), nombres_traducidos); clasificacion_df_comp <- clasificacion_df_comp_raw_lang %>% rename(!!!mapa_nombres_col); estilos_comp <- estilos_clasificacion_data[[clave_estilo_comp]]; tagList(tags$table(tags$thead(tags$tr(map(names(clasificacion_df_comp), tags$th))), tags$tbody(map(1:nrow(clasificacion_df_comp), function(j) { fila <- clasificacion_df_comp[j,]; nombre_equipo <- fila[[t("standings_team")]]; posicion_equipo <- fila[[t("standings_pos")]]; nombre_equipo_original <- clasificacion_df_comp_raw$team[j]; ruta_relativa_logo_html <- get_club_logo_path(nombre_equipo_original); regla_actual <- NULL; if (!is.null(estilos_comp)) { regla_match <- estilos_comp$reglas %>% filter(puesto == posicion_equipo); if (nrow(regla_match) > 0) { regla_actual <- regla_match[1,] } }; tags$tr(map(seq_along(fila), function(k) { cell_value <- fila[[k]]; col_name <- names(fila)[k]; if (col_name == t("standings_pos") && !is.null(regla_actual)) { tags$td(style = paste0("border-left: 5px solid ", regla_actual$color, "; font-weight: bold;"), cell_value) } else if (col_name == t("standings_team")) { deduccion <- clasificacion_df_comp_raw$puntos_deducidos[j]; nota_sancion <- if (!is.na(deduccion) && deduccion > 0) tags$span(style="color:#8B0000; font-weight:normal; font-size:0.85em;", paste0(" (\u2013", deduccion, ")")) else NULL; tags$td(class = "team-cell", tags$img(class="team-logo", src = ruta_relativa_logo_html, alt = nombre_equipo), tags$a(href=file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(nombre_equipo_original), ".html")), cell_value), nota_sancion) } else { tags$td(cell_value) }})) }))), if (!is.null(estilos_comp) && length(estilos_comp$leyenda) > 0) { tags$div(class = "legend", map(estilos_comp$leyenda, function(item_leyenda) { tags$div(class = "legend-item", tags$span(class = "legend-color-box", style = paste0("background-color: ", item_leyenda$color, ";")), tags$span(t(item_leyenda$texto_key))) })) }) }; contenido_clasificacion <- tagList(crear_botones_navegacion(path_to_lang_root = ".."), tags$h2(paste(t("standings_title"), "-", comp_nombre_current_lang)), contenido_tabla); nombre_archivo_clasif <- paste0(comp_id, "_", nombres_archivos_traducidos$clasificacion, ".html"); save_html(crear_pagina_html(contenido_clasificacion, paste(t("standings_title"), "-", comp_nombre_current_lang), "../..", script_contraseña_lang), file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, nombre_archivo_clasif)); lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href=nombre_archivo_clasif, class="menu-button", t("standings_title"))
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
            
            # 2. Tabla unificada de porteras (sin split por % de minutos).
            # Una fila por jugadora+competición (como goleadoras), con TeamNames_mk "A / B".
            tabla_porteras_comp <- stats_porteras_por_comp_df %>% filter(competicion_id == comp_id) %>% left_join(jugadoras_stats_df %>% select(id, !!player_name_col_sym), by = "id") %>% filter(Minutes > 0, !is.na(!!player_name_col_sym)) %>% arrange(desc(CS), GA90, desc(Minutes)) %>% mutate(Pos = row_number()) %>% select(id, Pos, PlayerName = !!player_name_col_sym, TeamNames_mk, GA90, GA, Minutes, CS)
            if (nrow(tabla_porteras_comp) > 0) {
            generar_tabla_html_porteras <- function(df, table_id) { if (is.null(df) || nrow(df) == 0) { return(tags$p(t("no_data_in_category")))}; tags$table(id = table_id, `data-sort-col` = "6", `data-sort-dir` = "desc", tags$thead(tags$tr(tags$th(t("standings_pos")), tags$th(t("player_type")), tags$th(t("team_type")), tags$th(class="sortable-header", onclick=sprintf("sortTable('%s', 3)", table_id), t("gk_ga_90")), tags$th(t("gk_ga")), tags$th(t("stats_minutes")), tags$th(class="sortable-header desc", onclick=sprintf("sortTable('%s', 6)", table_id), t("gk_cs")))), tags$tbody(map(1:nrow(df), function(j){ p <- df[j,]; tags$tr(tags$td(p$Pos), tags$td(tags$a(href=file.path("..", nombres_carpetas_relativos$jugadoras, paste0(p$id, ".html")), p$PlayerName)), tags$td({ teams_mk <- str_split(p$TeamNames_mk, " / ")[[1]]; team_tags <- list(); for (i in seq_along(teams_mk)) { team_name_mk <- teams_mk[i]; team_name <- entidades_df_lang %>% filter(original_name == team_name_mk) %>% pull(current_lang_name); team_element <- tags$span(class="team-cell", get_logo_tag(team_name_mk), tags$a(href = file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(team_name_mk), ".html")), team_name)); team_tags <- append(team_tags, list(team_element)); if (i < length(teams_mk)) { team_tags <- append(team_tags, list(tags$span(style="margin: 0 5px;", "/"))) } }; tagList(team_tags) }), tags$td(format(round(p$GA90, 2), nsmall = 2)), tags$td(p$GA), tags$td(p$Minutes), tags$td(p$CS)) })))}; contenido_porteras <- tagList(crear_botones_navegacion(path_to_lang_root = ".."), tags$h2(paste(t("goalkeepers_title"), "-", comp_nombre_current_lang)), generar_tabla_html_porteras(tabla_porteras_comp, "tabla-porteras")); nombre_archivo_porteras <- paste0(comp_id, "_golmanki.html"); save_html(crear_pagina_html(contenido_porteras, paste(t("goalkeepers_title"), "-", comp_nombre_current_lang), "../..", script_contraseña_lang), file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, nombre_archivo_porteras)); lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href=nombre_archivo_porteras, class="menu-button", t("goalkeepers_title")) }
            
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
            #   if (nrow(tabla_final_defensas) > 0) { contenido_defensas <- tagList(crear_botones_navegacion(path_to_lang_root = ".."), tags$h2(paste(t("defensive_trio_title"), "-", comp_nombre_current_lang)), tags$p(style="text-align:center; font-style:italic; color:#555;", t("defensive_trio_subtitle")), tags$table(class = "main-summary-table", tags$thead(tags$tr(tags$th(t("standings_pos")), tags$th(t("defensive_trio_header_trio")), tags$th(t("team_type")), tags$th(t("defensive_trio_header_minutes")), tags$th(t("defensive_trio_header_ga")), tags$th(t("defensive_trio_header_ga90")))), tags$tbody(pmap(tabla_final_defensas, function(...) { fila <- list(...); nombre_equipo <- fila$TeamName; nombre_equipo_mk <- fila$TeamName_mk; tags$tr(tags$td(fila$Pos), tags$td(fila$TrioNames), tags$td(class="team-cell", get_logo_tag(nombre_equipo_mk), tags$a(href=file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(nombre_equipo_mk), ".html")), onclick="event.stopPropagation();", nombre_equipo)), tags$td(round(fila$MinutesTogether)), tags$td(fila$GA_Together), tags$td(format(round(fila$GA90_Together, 2), nsmall = 2))) }))))
            #   nombre_archivo_defensas <- paste0(comp_id, "_defanzivno_trio.html"); save_html(crear_pagina_html(contenido_defensas, paste(t("defensive_trio_title"), "-", comp_nombre_current_lang), "../..", script_contraseña_lang), file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, nombre_archivo_defensas)); lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href=nombre_archivo_defensas, class="menu-button", t("defensive_trio_title")) }
            # }
            
            tabla_sanciones_comp <- stats_sanciones_por_comp_df %>% filter(competicion_id == comp_id) %>% left_join(jugadoras_stats_df %>% select(id, !!player_name_col_sym), by = "id") %>% filter(!is.na(!!player_name_col_sym)) %>% select(Pos, id, PlayerName = !!player_name_col_sym, TeamNames_mk, YellowCards, RedCards); contenido_sanciones <- tagList(crear_botones_navegacion(path_to_lang_root = ".."), tags$h2(paste(t("disciplinary_title"), "-", comp_nombre_current_lang)), tags$table(tags$thead(tags$tr(tags$th(t("standings_pos")), tags$th(t("player_type")), tags$th(t("team_type")), tags$th(HTML("<span class='card-yellow'></span>")), tags$th(HTML("<span class='card-red'></span>")))), tags$tbody(if(nrow(tabla_sanciones_comp) > 0) { map(1:nrow(tabla_sanciones_comp), function(j) { s <- tabla_sanciones_comp[j,]; tags$tr(tags$td(s$Pos), tags$td(tags$a(href=file.path("..", nombres_carpetas_relativos$jugadoras, paste0(s$id, ".html")), s$PlayerName)), tags$td({ teams_mk <- str_split(s$TeamNames_mk, " / ")[[1]]; team_tags <- list(); for (i in seq_along(teams_mk)) { team_name_mk <- teams_mk[i]; team_name <- entidades_df_lang %>% filter(original_name == team_name_mk) %>% pull(current_lang_name); team_element <- tags$span(class="team-cell", get_logo_tag(team_name_mk), tags$a(href = file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(team_name_mk), ".html")), team_name)); team_tags <- append(team_tags, list(team_element)); if (i < length(teams_mk)) { team_tags <- append(team_tags, list(tags$span(style="margin: 0 5px;", "/"))) }}; tagList(team_tags) }), tags$td(s$YellowCards), tags$td(s$RedCards)) })} else { tags$tr(tags$td(colspan="5", t("disciplinary_no_cards_message"))) })))
            nombre_archivo_sanciones <- paste0(comp_id, "_", nombres_archivos_traducidos$sanciones, ".html"); save_html(crear_pagina_html(contenido_sanciones, paste(t("disciplinary_title"), "-", comp_nombre_current_lang), "../..", script_contraseña_lang), file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, nombre_archivo_sanciones)); lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href=nombre_archivo_sanciones, class="menu-button", t("disciplinary_title"))
          }
          
          # ========================================================================
          # == INICIO: NUEVO CÓDIGO PARA GENERAR EL "HUB" DE LA COMPETICIÓN (v2 - Corregido)
          # ========================================================================
          message(paste0("        > Generating new Competition Hub for: ", comp_id))
          
          # --- 0. Definir nombres de archivo por adelantado para evitar errores ---
          # Esto garantiza que las variables existan, incluso si la lógica anterior las omitió (p.ej., en una Copa)
          nombre_archivo_goleadoras <- paste0(comp_id, "_", nombres_archivos_traducidos$goleadoras, ".html")
          nombre_archivo_sanciones <- paste0(comp_id, "_", nombres_archivos_traducidos$sanciones, ".html")
          nombre_archivo_porteras <- paste0(comp_id, "_golmanki.html") # Nombre hardcodeado del script original
          nombre_archivo_clasif <- paste0(comp_id, "_", nombres_archivos_traducidos$clasificacion, ".html")
          
          # --- 1. Logo helper — uses centralized get_club_logo_path() from 08_functions.R
          get_logo_path_relativo <- function(nombre_equipo_mk) get_club_logo_path(nombre_equipo_mk)
          
          # --- 2. Preparar datos de JORNADAS (Schedule) ---
          partidos_jugados <- partidos_comp %>% filter(!is.na(id_partido))
          jornada_por_defecto_raw <- NA_character_ # La jornada 'raw' (ej: "1", "1/2")
          
          if (nrow(partidos_jugados) > 0) {
            jornadas_jugadas_desc <- ordenar_jornadas(unique(partidos_jugados$jornada), descendente = TRUE)
            if (length(jornadas_jugadas_desc) > 0) {
              jornada_por_defecto_raw <- jornadas_jugadas_desc[1]
            }
          }
          
          # Si AÚN no hay jornada por defecto (ej. no se ha jugado ningún partido),
          # simplemente coger la primera jornada del calendario (ej. "1")
          if (is.na(jornada_por_defecto_raw) && length(jornadas_comp) > 0) {
            jornada_por_defecto_raw <- jornadas_comp[1]
          }
          
          # Formatear TODAS las jornadas para el JSON
          datos_jornadas_json <- map(jornadas_comp, function(j) {
            partidos_jornada <- partidos_comp %>% filter(jornada == j) %>% arrange(local)
            
            partidos_data <- map(1:nrow(partidos_jornada), function(k) {
              partido <- partidos_jornada[k,]
              is_placeholder_match <- is.na(partido$id_partido)
              
              local_name <- entidades_df_lang$current_lang_name[match(partido$local, entidades_df_lang$original_name)]
              visitante_name <- entidades_df_lang$current_lang_name[match(partido$visitante, entidades_df_lang$original_name)]
              
              resultado_texto <- if (is_placeholder_match) {
                partido$hora %||% " - "
              } else {
                res_base <- paste(partido$goles_local, "-", partido$goles_visitante)
                if (!is.na(partido$penales_local)) res_base <- sprintf("%s (%s - %s)", res_base, partido$penales_local, partido$penales_visitante)
                if (isTRUE(partido$es_resultado_oficial)) res_base <- paste(res_base, "*")
                res_base
              }
              
              # Obtener estadio (lugar)
              lugar_partido_mk <- estadio_info_mk <- estadios_df %>% filter(id_partido == partido$id_partido) %>% pull(estadio)
              lugar_partido_lang <- if(length(lugar_partido_mk) > 0) {
                (entidades_df_lang %>% filter(original_name == lugar_partido_mk[1]))$current_lang_name[1]
              } else {
                NA_character_
              }
              
              list(
                id_partido = partido$id_partido,
                local_lang = local_name,
                visitante_lang = visitante_name,
                local_logo_path = get_logo_path_relativo(partido$local),
                visitante_logo_path = get_logo_path_relativo(partido$visitante),
                resultado = resultado_texto,
                lugar_lang = lugar_partido_lang %||% NA_character_,
                fecha = partido$fecha %||% NA_character_
              )
            })
            
            list(
              jornada_nombre = if(is_cup || is_friendly_comp) as.character(j) else paste(t("round_prefix"), j),
              jornada_id_raw = j,
              partidos = partidos_data
            )
          })
          
          # --- 3. Preparar datos de ESTADÍSTICAS (Top 5) ---
          
          # Goleadoras
          datos_top_goleadoras <- stats_goleadoras_por_comp_df %>%
            filter(competicion_id == comp_id) %>%
            left_join(jugadoras_lang_df, by = "id") %>%
            # Use LastTeam_mk (single team) for the hub top 5 so translation always resolves
            left_join(entidades_df_lang, by = c("LastTeam_mk" = "original_name")) %>%
            filter(!is.na(PlayerName)) %>%
            select(Pos, PlayerName, TeamName = current_lang_name, Goals, id_jugadora = id, id_equipo_mk = LastTeam_mk) %>%
            head(5) %>%
            # Añadir rutas de enlace para el JS
            mutate(
              link_jugadora = file.path("..", nombres_carpetas_relativos$jugadoras, paste0(id_jugadora, ".html")),
              link_equipo = file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(id_equipo_mk), ".html"))
            )
          
          # Tarjetas
          datos_top_tarjetas <- stats_sanciones_por_comp_df %>%
            filter(competicion_id == comp_id) %>%
            left_join(jugadoras_lang_df, by = "id") %>%
            left_join(entidades_df_lang, by = c("LastTeam_mk" = "original_name")) %>%
            filter(!is.na(PlayerName)) %>%
            select(Pos, PlayerName, TeamName = current_lang_name, YellowCards, RedCards, id_jugadora = id, id_equipo_mk = LastTeam_mk) %>%
            head(5) %>%
            mutate(
              link_jugadora = file.path("..", nombres_carpetas_relativos$jugadoras, paste0(id_jugadora, ".html")),
              link_equipo = file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(id_equipo_mk), ".html"))
            )
          
          # Calcular los minutos totales posibles por equipo en esta competición
          minutos_totales_equipo_comp <- partidos_df %>%
            filter(
              competicion_nombre == comp_info$competicion_nombre,
              competicion_temporada == comp_info$competicion_temporada,
              !is.na(id_partido) # Solo partidos con acta
            ) %>%
            select(local, visitante, duracion_partido) %>%
            pivot_longer(cols = c(local, visitante), names_to = "tipo_equipo", values_to = "equipo") %>%
            group_by(equipo) %>%
            summarise(minutos_totales_posibles = sum(duracion_partido, na.rm = TRUE), .groups = 'drop')
          
          # Porteras (top-5 en hub) — usa LastTeam_mk como goleadoras
          datos_top_porteras <- stats_porteras_por_comp_df %>%
            filter(competicion_id == comp_id, Minutes > 0) %>%
            left_join(jugadoras_lang_df, by = "id") %>%
            left_join(entidades_df_lang, by = c("LastTeam_mk" = "original_name")) %>%
            filter(!is.na(PlayerName)) %>%
            arrange(desc(CS), GA90, desc(Minutes)) %>%
            mutate(Pos = row_number()) %>%
            select(Pos, PlayerName, TeamName = current_lang_name, GA90, CS, id_jugadora = id, id_equipo_mk = LastTeam_mk) %>%
            head(5) %>%
            mutate(
              link_jugadora = file.path("..", nombres_carpetas_relativos$jugadoras, paste0(id_jugadora, ".html")),
              link_equipo = file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(id_equipo_mk), ".html"))
            )
          
          # --- 4. Preparar datos de CLASIFICACIÓN (HTML completo) ---
          # Reutilizamos la lógica exacta que ya tenías para la página de 'Tabela'
          # para mantener la coherencia (incluyendo los estilos de colores).
          bloque_html_clasificacion <- if (!is_cup && !is_friendly_comp && !is_placeholder_only_comp) {
            
            clasificacion_df_comp_raw <- stats_clasificacion_por_comp_df %>% filter(competicion_id == comp_id)
            clave_estilo_comp <- paste(comp_info$competicion_nombre, comp_info$competicion_temporada)
            
            contenido_tabla <- if (nrow(clasificacion_df_comp_raw) == 0) {
              tags$p(t("standings_no_data_message"))
            } else {
              clasificacion_df_comp_raw_lang <- clasificacion_df_comp_raw %>%
                left_join(entidades_df_lang, by = c("team" = "original_name")) %>%
                select(Pos, team_lang = current_lang_name, P, W, D, L, GF, GA, GD, Pts)
              
              nombres_neutros <- c("Pos", "team_lang", "P", "W", "D", "L", "GF", "GA", "GD", "Pts")
              claves_traduccion <- c("standings_pos", "standings_team", "standings_p", "standings_w", "standings_d", "standings_l", "standings_gf", "standings_ga", "standings_gd", "standings_pts")
              nombres_traducidos <- sapply(claves_traduccion, t, USE.NAMES = FALSE)
              mapa_nombres_col <- setNames(as.list(nombres_neutros), nombres_traducidos)
              
              clasificacion_df_comp <- clasificacion_df_comp_raw_lang %>% rename(!!!mapa_nombres_col)
              
              estilos_comp <- estilos_clasificacion_data[[clave_estilo_comp]]
              
              tagList(
                tags$table(
                  tags$thead(tags$tr(map(names(clasificacion_df_comp), tags$th))),
                  tags$tbody(map(1:nrow(clasificacion_df_comp), function(j) {
                    fila <- clasificacion_df_comp[j,]
                    nombre_equipo <- fila[[t("standings_team")]]
                    posicion_equipo <- fila[[t("standings_pos")]]
                    nombre_equipo_original <- clasificacion_df_comp_raw$team[j]
                    
                    # Usamos la nueva función helper para la ruta del logo
                    ruta_relativa_logo_html <- get_logo_path_relativo(nombre_equipo_original)
                    
                    regla_actual <- NULL
                    if (!is.null(estilos_comp)) {
                      regla_match <- estilos_comp$reglas %>% filter(puesto == posicion_equipo)
                      if (nrow(regla_match) > 0) {
                        regla_actual <- regla_match[1,]
                      }
                    }
                    
                    tags$tr(map(seq_along(fila), function(k) {
                      cell_value <- fila[[k]]
                      col_name <- names(fila)[k]
                      if (col_name == t("standings_pos") && !is.null(regla_actual)) {
                        tags$td(style = paste0("border-left: 5px solid ", regla_actual$color, "; font-weight: bold;"), cell_value)
                      } else if (col_name == t("standings_team")) {
                        deduccion <- clasificacion_df_comp_raw$puntos_deducidos[j]
                        nota_sancion <- if (!is.na(deduccion) && deduccion > 0) tags$span(style="color:#8B0000; font-weight:normal; font-size:0.85em;", paste0(" (\u2013", deduccion, ")")) else NULL
                        tags$td(
                          class = "team-cell",
                          tags$img(class="team-logo", src = ruta_relativa_logo_html, alt = nombre_equipo),
                          tags$a(href=file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(nombre_equipo_original), ".html")), cell_value),
                          nota_sancion
                        )
                      } else {
                        tags$td(cell_value)
                      }
                    }))
                  }))
                ),
                if (!is.null(estilos_comp) && length(estilos_comp$leyenda) > 0) {
                  tags$div(
                    class = "legend",
                    map(estilos_comp$leyenda, function(item_leyenda) {
                      tags$div(
                        class = "legend-item",
                        tags$span(class = "legend-color-box", style = paste0("background-color: ", item_leyenda$color, ";")),
                        tags$span(t(item_leyenda$texto_key))
                      )
                    })
                  )
                }
              )
            }
            
            # Devolver el bloque completo con su título
            tagList(
              tags$div(class = "comp-hub-section-header",
                       # El título H3 ahora está DENTRO del enlace
                       tags$a(href = nombre_archivo_clasif, class="comp-hub-title-link",
                              tags$h3(class="comp-hub-section-title", t("standings_title"))
                       )
                       # No hay enlace "see all" a la derecha
              ),
              contenido_tabla
            )
            
          } else {
            NULL # No mostrar bloque de clasificación para Copas o Amistosos
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
              clasificacion = if(!is.null(bloque_html_clasificacion)) nombre_archivo_clasif else NA_character_
            ),
            translations = list(
              scorers = t("scorers_title"),
              cards = t("disciplinary_title"),
              goalkeepers = t("goalkeepers_title"),
              # *** CORRECCIÓN DE TRADUCCIÓN AQUÍ ***
              see_all = t("see_full_list"),
              stadium = t("match_stadium"),
              no_matches_in_round = t("match_timeline_no_events") # Reutilizamos esta traducción
            )
          ), auto_unbox = TRUE, na = "null") # Asegurarse que los NA se conviertan en null
          
          script_datos_hub_json <- tags$script(
            id = "competition-hub-data",
            type = "application/json",
            HTML(datos_completos_hub_json)
          )
          
          # --- 6. Construir el esqueleto HTML del Hub ---
          # ¡NUEVA LÓGICA! Usar un diseño diferente para Ligas vs Copas
          if (is_cup) {
            # --- 6a. Esqueleto SIMPLIFICADO para Copas ---
            # (Solo Jornadas y Goleadoras, como pediste)
            
            # Preparar el panel de Goleadoras (ya calculado en Sección 3)
            panel_goleadoras_copa <- tagList(
              tags$div(class = "comp-hub-section-header",
                       tags$h3(class="comp-hub-section-title", t("scorers_title")),
                       # *** CORRECCIÓN DE TRADUCCIÓN AQUÍ ***
                       tags$a(href = nombre_archivo_goleadoras, class="comp-hub-see-all-link", paste0(t("all_results_link"), " >"))
              ),
              # El JS llenará esta lista
              tags$div(id = "comp-hub-stats-content-goleadoras", class = "comp-hub-tab-panel active")
            )
            
            contenido_hub_final <- tagList(
              crear_botones_navegacion(path_to_lang_root = ".."),
              tags$h2(comp_nombre_current_lang),
              
              # Contenedor principal (Grid) - 2 columnas
              tags$div(class = "comp-hub-left-col",
                       tags$div(class = "comp-hub-section-header",
                                # El título H3 ahora está DENTRO del enlace
                                tags$a(href = nombre_archivo_partidos, class="comp-hub-title-link",
                                       tags$h3(class="comp-hub-section-title", t("schedule_title"))
                                ),
                                # El enlace "see all" se mantiene
                                tags$a(href = nombre_archivo_partidos, class="comp-hub-see-all-link", paste0(t("all_results_link"), " >"))
                       ),
                       tags$div(class = "comp-hub-schedule-nav",
                                tags$button(id = "comp-hub-prev-round", class = "comp-hub-nav-arrow", "‹"),
                                tags$h4(id = "comp-hub-round-title", ""),
                                tags$button(id = "comp-hub-next-round", class = "comp-hub-nav-arrow", "›")
                       ),
                       tags$div(id = "comp-hub-schedule-matches") # JS llenará esto
              ),
              
              # Columna Derecha (Solo Goleadoras)
              tags$div(class = "comp-hub-right-col",
                       panel_goleadoras_copa
              )
            )
            # No hay .comp-hub-bottom-row para Copas
            
            
          } else {
            # --- 6b. Esqueleto COMPLETO para Ligas (el que ya teníamos) ---
            
            contenido_hub_final <- tagList(
              crear_botones_navegacion(path_to_lang_root = ".."),
              tags$h2(comp_nombre_current_lang),
              
              # Contenedor principal (Grid)
              tags$div(class = "comp-hub-container",
                       # Columna Izquierda (Jornadas)
                       tags$div(class = "comp-hub-left-col",
                                tags$div(class = "comp-hub-section-header",
                                         # El título H3 ahora está DENTRO del enlace
                                         tags$a(href = nombre_archivo_partidos, class="comp-hub-title-link",
                                                tags$h3(class="comp-hub-section-title", t("schedule_title"))
                                         ),
                                         # El enlace "see all" se mantiene
                                         tags$a(href = nombre_archivo_partidos, class="comp-hub-see-all-link", paste0(t("all_results_link"), " >"))
                                ),
                                tags$div(class = "comp-hub-schedule-nav",
                                         tags$button(id = "comp-hub-prev-round", class = "comp-hub-nav-arrow", "‹"),
                                         tags$h4(id = "comp-hub-round-title", ""),
                                         tags$button(id = "comp-hub-next-round", class = "comp-hub-nav-arrow", "›")
                                ),
                                tags$div(id = "comp-hub-schedule-matches") # JS llenará esto
                       ),
                       
                       # Columna Derecha (Estadísticas con Pestañas)
                       tags$div(class = "comp-hub-right-col",
                                tags$nav(class = "comp-hub-stats-tabs",
                                         tags$button(class = "comp-hub-tab-btn active", "data-tab" = "goleadoras", t("scorers_title")),
                                         # *** CORRECCIÓN DEL TYPO AQUÍ ***
                                         tags$button(class = "comp-hub-tab-btn", "data-tab" = "tarjetas", t("disciplinary_title")),
                                         tags$button(class = "comp-hub-tab-btn", "data-tab" = "porteras", t("goalkeepers_title"))
                                ),
                                # Contenedores para cada pestaña
                                tags$div(id = "comp-hub-stats-content-goleadoras", class = "comp-hub-tab-panel active"),
                                tags$div(id = "comp-hub-stats-content-tarjetas", class = "comp-hub-tab-panel"),
                                tags$div(id = "comp-hub-stats-content-porteras", class = "comp-hub-tab-panel")
                       )
              ),
              
              # Fila Inferior (Clasificación)
              tags$div(class = "comp-hub-bottom-row",
                       bloque_html_clasificacion # El HTML de la clasificación se inserta directamente
              )
            )
          } # Fin del if/else (is_cup)
          
          
          # --- 7. Guardar la página ---
          pagina_hub_final <- crear_pagina_html(
            contenido_hub_final,
            comp_nombre_current_lang,
            "../..",
            script_contraseña_lang,
            current_page_id = "competitions"
          )
          
          # Añadir el JSON al body
          pagina_hub_final$children[[2]]$children <- tagAppendChildren(
            pagina_hub_final$children[[2]]$children,
            script_datos_hub_json
          )
          
          save_html(
            pagina_hub_final,
            file = file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, paste0(comp_id, ".html"))
          )
          
          # ========================================================================
          # == FIN: NUEVO CÓDIGO PARA GENERAR EL "HUB" DE LA COMPETICIÓN (v2 - Corregido)
          # ========================================================================
          
        }
      })
    }
    
    # 13.1.23. Generate Individual Profile Pages.
    if (GENERAR_PERFILES_PARTIDO) {
      indices_partidos <- which(
        !is.na(partidos_df$id_partido) &
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
      if (n_partidos > 0) with_progress({
      p_partidos <- progressor(steps = n_partidos)
      future_lapply(datos_por_partido, function(pkt) {
        i <- pkt$i; resumen_partido <- pkt$resumen
        partido_info <- partidos_df[i,]; id_p <- partido_info$id_partido
        
        local_name <- (entidades_df_lang %>% filter(original_name == partido_info$local))$current_lang_name[1]
        visitante_name <- (entidades_df_lang %>% filter(original_name == partido_info$visitante))$current_lang_name[1]
        
        cronologia <- generar_cronologia_df(id_p, resumen_partido, entidades_df_lang, jugadoras_lang_df)
        arbitros_partido_mk <- arbitros_df %>% filter(id_partido == id_p); arbitros_partido_lang <- arbitros_partido_mk %>% left_join(entidades_df_lang, by = c("ime" = "original_name"))
        staff_partido <- if (exists("staff_df") && nrow(staff_df) > 0) staff_df %>% filter(id_partido == id_p) else tibble()
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
        if (!is.na(nota_arbitro)) { nota_arbitro <- str_remove(nota_arbitro, "^[\\s:]*"); nota_arbitro <- gsub("[\\r\\n]+", " ", nota_arbitro) }
        path_rel_competiciones <- file.path("..", nombres_carpetas_relativos$competiciones); path_rel_timovi <- file.path("..", nombres_carpetas_relativos$timovi); path_rel_jugadoras <- file.path("..", nombres_carpetas_relativos$jugadoras); path_rel_arbitros <- file.path("..", nombres_carpetas_relativos$arbitros); path_rel_estadios <- file.path("..", nombres_carpetas_relativos$estadios)
        alineacion_partido_lang <- apariciones_df %>% filter(id_partido == id_p) %>% left_join(jugadoras_lang_df, by="id")
        
        # --- Helper: render lineup for one team (new design) ---
        render_equipo_html_new <- function(df_equipo, goles_del_partido, tarjetas_del_partido, is_national_team_match, team_original_mk_name, duracion_partido) {
          if (is.null(df_equipo) || nrow(df_equipo) == 0) { return(tags$p(t("match_no_data"))) }
          starters <- df_equipo %>% filter(tipo == "Titular")
          subs <- df_equipo %>% filter(tipo == "Suplente")
          
          crear_lista_jugadoras <- function(df_j, duracion_partido) { 
            if (nrow(df_j) == 0) { return(tags$p(style = "color:#777;", t("match_no_players"))) }
            tags$ul(pmap(df_j, function(id, PlayerName, dorsal, tipo, es_portera, es_capitana, min_entra, min_sale, minutos_jugados, ...) {
              eventos_html <- tagList()
              goles_jugadora <- goles_del_partido %>% filter(id == !!id, tipo == "Normal")
              if (nrow(goles_jugadora) > 0) { walk(1:nrow(goles_jugadora), function(g) { gol <- goles_jugadora[g,]; eventos_html <<- tagAppendChild(eventos_html, tags$span(class = "player-event goal", HTML(paste0("⚽︎ ", formatear_minuto_partido(gol$minuto))))) }) }
              tarjetas_jugadora <- tarjetas_del_partido %>% filter(id == !!id)
              if (nrow(tarjetas_jugadora) > 0) { walk(1:nrow(tarjetas_jugadora), function(c) { tarjeta <- tarjetas_jugadora[c,]; card_span <- tags$span(class = if (tarjeta$tipo == "Amarilla") "card-yellow" else "card-red"); eventos_html <<- tagAppendChild(eventos_html, tags$span(class = "player-event", card_span, HTML(paste0("︎ ", formatear_minuto_partido(tarjeta$minuto))))) }) }
              if (!is.na(min_entra) && tipo == "Suplente") { eventos_html <- tagAppendChild(eventos_html, tags$span(class = "player-event sub-in", paste0("↑", formatear_minuto_partido(min_entra)))) }
              if (!is.na(min_sale) && min_sale < duracion_partido && !is.na(minutos_jugados) && minutos_jugados > 0) {
                eventos_html <- tagAppendChild(eventos_html, tags$span(class = "player-event sub-out", paste0("↓", formatear_minuto_partido(min_sale))))
              }
              icono_p <- if (isTRUE(es_portera)) "🧤" else ""
              icono_c <- if (isTRUE(es_capitana)) "(C)" else ""
              should_be_clickable <- !is_national_team_match || (is_national_team_match && team_original_mk_name == "Македонија")
              player_element <- if (should_be_clickable) { tags$a(href = file.path(path_rel_jugadoras, paste0(id, ".html")), PlayerName) } else { PlayerName }
              tags$li(paste0(dorsal, ". "), player_element, icono_p, icono_c, eventos_html)
            }))
          }
          tagList(
            tags$div(class = "mp-lineup-sub-header", t("match_starting_lineup")),
            crear_lista_jugadoras(starters, duracion_partido),
            tags$div(class = "mp-lineup-sub-header", t("match_substitutes")),
            crear_lista_jugadoras(subs, duracion_partido)
          )
        }
        
        # --- Helper: render penalties for one team ---
        render_penales_html <- function(df_equipo) {
          if(is.null(df_equipo) || nrow(df_equipo) == 0) { return(NULL) }
          tags$ul(pmap(df_equipo, function(PlayerName, id, dorsal, resultado_penal, ...) {
            tags$li(
              if(resultado_penal=="Gol") "✅" else "❌", " ",
              if(is.na(PlayerName)) "NA" else tags$a(href=file.path(path_rel_jugadoras, paste0(id, ".html")), PlayerName),
              paste0(" (", dorsal, ")")
            )
          }))
        }
        
        # --- Helper: render goals summary for one side ---
        render_goals_summary <- function(goles_df_side, align) {
          if (nrow(goles_df_side) == 0) return(NULL)
          goles_agrupados <- goles_df_side %>%
            group_by(id, PlayerName, tipo) %>%
            summarise(
              primer_gol = min(minuto, na.rm = TRUE),
              minutos = paste0("(", paste0(sapply(minuto, formatear_minuto_partido), collapse = ", "), ")"),
              .groups = "drop"
            ) %>%
            arrange(primer_gol)
          tagList(map(1:nrow(goles_agrupados), function(g) {
            gol <- goles_agrupados[g,]
            og_label <- if (gol$tipo == "Autogol") tags$span(class = "mp-og-label", t("own_goal_label")) else NULL
            should_link <- !partido_info$es_partido_seleccion || partido_info$local == "Македонија" || partido_info$visitante == "Македонија"
            player_el <- if (should_link && !is.na(gol$id)) {
              tags$a(href = file.path(path_rel_jugadoras, paste0(gol$id, ".html")), tags$span(class = "mp-goal-player", gol$PlayerName))
            } else {
              tags$span(class = "mp-goal-player", gol$PlayerName)
            }
            if (align == "home") {
              tags$div(class = "mp-goal-row", player_el, og_label, tags$span(class = "mp-goal-minute", gol$minutos))
            } else {
              tags$div(class = "mp-goal-row", tags$span(class = "mp-goal-minute", gol$minutos), player_el, og_label)
            }
          }))
        }
        
        # --- Helper: build visual timeline ---
        render_timeline_visual <- function(cronologia, equipo_local_mk) {
          if (!exists("cronologia") || is.null(cronologia) || nrow(cronologia) == 0) {
            return(tags$p(class = "mp-timeline-no-events", t("match_timeline_no_events")))
          }
          tags$section(class = "mp-timeline-container",
            tags$div(class = "mp-timeline-line"),
            tagList(map(1:nrow(cronologia), function(c) {
              e <- cronologia[c,]
              es_local <- !is.na(e$equipo_canonico_mk) && e$equipo_canonico_mk == equipo_local_mk
              minuto_fmt <- formatear_minuto_partido(e$minuto)
              
              # Content for the event — player names as links
              should_link <- !partido_info$es_partido_seleccion || partido_info$local == "Македонија" || partido_info$visitante == "Македонија"
              player_el <- if (should_link && !is.na(e$jugadora_id) && nchar(e$jugadora_id) > 0) {
                tags$a(href = file.path(path_rel_jugadoras, paste0(e$jugadora_id, ".html")),
                       class = "player", e$jugadora_nombre %||% "")
              } else {
                tags$span(class = "player", e$jugadora_nombre %||% "")
              }
              sub_el <- if (!is.na(e$jugadora_sub_nombre)) {
                if (should_link && !is.na(e$jugadora_sub_id) && nchar(e$jugadora_sub_id) > 0) {
                  tags$a(href = file.path(path_rel_jugadoras, paste0(e$jugadora_sub_id, ".html")),
                         class = "mp-player-sub", e$jugadora_sub_nombre)
                } else {
                  tags$span(class = "mp-player-sub", e$jugadora_sub_nombre)
                }
              } else NULL
              
              event_content <- tagList(
                tags$span(class = "icon", e$icono),
                player_el,
                sub_el
              )
              
              if (es_local) {
                tags$div(class = "mp-timeline-event",
                  tags$div(class = "mp-event-top", event_content),
                  tags$div(class = "mp-minute", minuto_fmt),
                  tags$div(class = "mp-event-bottom")
                )
              } else {
                tags$div(class = "mp-timeline-event",
                  tags$div(class = "mp-event-top"),
                  tags$div(class = "mp-minute", minuto_fmt),
                  tags$div(class = "mp-event-bottom", event_content)
                )
              }
            }))
          )
        }
        
        # --- Build stadium element ---
        estadio_element <- if (nrow(estadio_info_mk) > 0) {
          is_stadium_excluded <- partido_info$es_partido_seleccion && (generar_id_seguro(estadio_info_mk$estadio) %in% stadium_ids_to_skip)
          if (!is_stadium_excluded) {
            tags$a(href = file.path(path_rel_estadios, paste0(generar_id_seguro(estadio_info_mk$estadio), ".html")), estadio_name_lang)
          } else {
            estadio_name_lang
          }
        } else {
          t("match_unknown")
        }
        
        # --- Build referees list ---
        referees_html <- if (nrow(arbitros_partido_lang) > 0) {
          tags$ul(class = "mp-referees-list", map(1:nrow(arbitros_partido_lang), function(a) {
            arb <- arbitros_partido_lang[a,]
            rol_traducido <- ""
            if (!is.null(arb$uloga) && length(arb$uloga) == 1 && !is.na(arb$uloga) && nchar(arb$uloga) > 0) {
              rol_traducido <- t(arb$uloga)
            }
            nombre_mostrado <- arb$current_lang_name
            if (!is.null(arb$ciudad) && length(arb$ciudad) == 1 && !is.na(arb$ciudad) && nchar(arb$ciudad) > 0) {
              nombre_mostrado <- paste0(arb$current_lang_name, " (", arb$ciudad, ")")
            }
            is_arb_excluded <- partido_info$es_partido_seleccion && (generar_id_seguro(arb$ime) %in% referee_ids_to_skip)
            ref_element <- if (!is_arb_excluded) {
              tags$a(href = file.path(path_rel_arbitros, paste0(generar_id_seguro(arb$ime), ".html")), nombre_mostrado)
            } else {
              nombre_mostrado
            }
            tags$li(paste0(rol_traducido, ": "), ref_element)
          }))
        } else { NULL }
        
        # --- Build staff list ---
        staff_html <- if (nrow(staff_partido) > 0) {
          staff_local <- staff_partido %>% filter(equipo == partido_info$local)
          staff_visitante <- staff_partido %>% filter(equipo == partido_info$visitante)

          render_staff_equipo <- function(df_staff) {
            if (nrow(df_staff) == 0) return(NULL)
            tags$ul(class = "mp-staff-list", map(1:nrow(df_staff), function(s) {
              stf <- df_staff[s,]
              rol_traducido <- t(stf$rol)
              tags$li(paste0(rol_traducido, ": ", stf$nombre))
            }))
          }

          tagList(
            tags$div(class = "mp-staff-section",
              tags$div(class = "mp-detail-row", tags$span(tags$b(t("staff_title")))),
              if (nrow(staff_local) > 0) tagList(
                tags$div(class = "mp-staff-subtitle", local_name),
                render_staff_equipo(staff_local)
              ),
              if (nrow(staff_visitante) > 0) tagList(
                tags$div(class = "mp-staff-subtitle", visitante_name),
                render_staff_equipo(staff_visitante)
              )
            )
          )
        } else { NULL }

        # --- Goals summary data ---
        goles_resumen_local_name <- resumen_partido$partido_info$local
        goles_local_df <- goles_partido %>% filter(equipo_acreditado == goles_resumen_local_name)
        goles_visitante_df <- goles_partido %>% filter(equipo_acreditado != goles_resumen_local_name)
        
        # --- MAIN CONTENT ---
        contenido_partido <- tagList(
          crear_botones_navegacion(path_to_lang_root = ".."),
          
          tags$div(class = "mp-container",
            
            # === COMPETITION HEADER ===
            tags$section(class = "mp-competition-header",
              tags$h2(tags$a(href = file.path(path_rel_competiciones, paste0(partido_comp_info$competicion_id, ".html")), comp_nombre_current_lang)),
              tags$p(paste0(jornada_texto, " · ", partido_info$fecha))
            ),
            
            # === SCOREBOARD ===
            tags$header(class = "mp-scoreboard",
              tags$div(class = "mp-team mp-team-home",
                tags$div(class = "mp-team-name", crear_enlace_equipo_condicional(partido_info$local, local_name)),
                tags$div(class = "mp-team-logo", get_logo_tag(partido_info$local, css_class = ""))
              ),
              tags$div(class = "mp-score-container",
                tags$div(class = "mp-score", paste(partido_info$goles_local, "-", partido_info$goles_visitante)),
                tags$div(class = "mp-status", t("final_score")),
                if (!is.na(partido_info$penales_local)) {
                  tags$div(class = "mp-penalties-note", paste0("(", t("penalties_short"), " ", partido_info$penales_local, "-", partido_info$penales_visitante, ")"))
                },
                if (isTRUE(partido_info$es_resultado_oficial)) {
                  tags$div(class = "mp-official-result", t("match_official_result"))
                }
              ),
              tags$div(class = "mp-team mp-team-away",
                tags$div(class = "mp-team-logo", get_logo_tag(partido_info$visitante, css_class = "")),
                tags$div(class = "mp-team-name", crear_enlace_equipo_condicional(partido_info$visitante, visitante_name))
              )
            ),
            
            # === MATCH INFO (Goals Summary + Details) ===
            tags$section(class = "mp-match-info",
              # Goals summary box
              tags$div(class = "mp-info-box mp-goals-summary",
                tags$div(class = "mp-goals-title", t("stats_goals")),
                tags$div(class = "mp-goals-grid",
                  tags$div(class = "mp-goals-home", render_goals_summary(goles_local_df, "home")),
                  tags$div(class = "mp-goals-away", render_goals_summary(goles_visitante_df, "away"))
                )
              ),
              # Details box
              tags$div(class = "mp-info-box",
                tags$div(class = "mp-detail-row",
                  tags$span(paste0("⏱️ ", partido_info$fecha)),
                  tags$span(partido_info$hora)
                ),
                tags$div(class = "mp-detail-row",
                  tags$span("🏟️ ", estadio_element)
                ),
                tags$div(class = "mp-detail-row",
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
            tags$section(class = "mp-lineups-section",
              tags$h3(class = "mp-section-title", t("lineups_title")),
              tags$div(class = "mp-lineups-grid",
                tags$div(class = "mp-lineup",
                  tags$div(class = "mp-lineup-header",
                    get_logo_tag(partido_info$local, css_class = ""),
                    tags$h4(crear_enlace_equipo_condicional(partido_info$local, local_name))
                  ),
                  render_equipo_html_new(
                    filter(alineacion_partido_lang, equipo == partido_info$local),
                    goles_partido, tarjetas_partido,
                    partido_info$es_partido_seleccion, partido_info$local,
                    partido_info$duracion_partido
                  )
                ),
                tags$div(class = "mp-lineup",
                  tags$div(class = "mp-lineup-header",
                    get_logo_tag(partido_info$visitante, css_class = ""),
                    tags$h4(crear_enlace_equipo_condicional(partido_info$visitante, visitante_name))
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
              tags$section(class = "mp-penales-section",
                tags$h3(class = "mp-section-title", t("penalties_title")),
                tags$div(class = "mp-penales-grid",
                  tags$div(
                    tags$h4(local_name),
                    render_penales_html(filter(penales_partido, equipo == partido_info$local))
                  ),
                  tags$div(
                    tags$h4(visitante_name),
                    render_penales_html(filter(penales_partido, equipo == partido_info$visitante))
                  )
                )
              )
            }
          ),
          
          crear_botones_navegacion(path_to_lang_root = "..")
        )
        pagina_partido_final <- crear_pagina_html(contenido_partido, paste(local_name, "vs", visitante_name), path_to_root_dir = "../..", script_contraseña_lang)
        save_html(pagina_partido_final, file = file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$partidos, paste0(id_p, ".html")))
        p_partidos()
      }, future.seed = TRUE, future.packages = pkgs_paralelos)
      })
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
      if (n_jugadoras > 0) with_progress({
      p_jugadoras <- progressor(steps = n_jugadoras)
      future_lapply(indices_jugadoras, function(i) {
        jugadora <- jugadoras_stats_df[i,]; id_j <- jugadora$id;
        
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
        
        # Logo helper — uses centralized get_logo_tag() with "team-logo-small" class
        generar_logo_html <- function(nombre_equipo_mk) get_logo_tag(nombre_equipo_mk, css_class = "team-logo-small")
        
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
        p_jugadoras()
      }, future.seed = TRUE, future.packages = pkgs_paralelos)
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
      todos_equipos <- unique(c(partidos_df$local, partidos_df$visitante))
      equipos_a_generar <- todos_equipos[
        !(todos_equipos %in% team_names_to_skip_mk) &
        (full_rebuild_needed | generar_id_seguro(todos_equipos) %in% affected_team_ids)
      ]
      n_equipos <- length(equipos_a_generar)
      message(sprintf("   > Generating %d team profiles in parallel...", n_equipos))
      if (n_equipos > 0) with_progress({
      p_equipos <- progressor(steps = n_equipos)
      future_lapply(equipos_a_generar, function(team_mk) {
        id_t <- generar_id_seguro(team_mk)
        
        # ============================================================================
        # == INICIO DE LA LÓGICA LITERAL DEL SCRIPT DE PRUEBAS                       ==
        # ============================================================================
        
        # --- 3.1. Preparar datos específicos del idioma ---
        current_team_name <- (entidades_df_lang %>% filter(original_name == team_mk))$current_lang_name[1]
        
        # --- 3.2. Preparación de datos avanzada ---
        stadium_principal_mk <- find_main_stadium(team_mk, partidos_df, estadios_df)
        stadium_principal_lang <- if (!is.na(stadium_principal_mk)) (entidades_df_lang %>% filter(original_name == stadium_principal_mk))$current_lang_name[1] else NA_character_
        
        ruta_logo_principal <- get_club_logo_path(team_mk)
        
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
            home_logo_url = get_club_logo_path(local),
            away_logo_url = get_club_logo_path(visitante)
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
        p_equipos()
        
        # ============================================================================
        # == FIN DE LA LÓGICA LITERAL                                                ==
        # ============================================================================
      }, future.seed = TRUE, future.packages = pkgs_paralelos)
      })
    }
    
    
    if (GENERAR_PERFILES_ARBITRO) {
      todos_arbitros <- unique(arbitros_df$ime)
      arbitros_a_generar <- todos_arbitros[
        !(generar_id_seguro(todos_arbitros) %in% referee_ids_to_skip) &
        (full_rebuild_needed | generar_id_seguro(todos_arbitros) %in% affected_referee_ids)
      ]
      n_arbitros <- length(arbitros_a_generar)
      message(sprintf("   > Generating %d referee profiles in parallel...", n_arbitros))
      if (n_arbitros > 0) with_progress({
      p_arbitros <- progressor(steps = n_arbitros)
      future_lapply(arbitros_a_generar, function(arb_mk) {
        id_a <- generar_id_seguro(arb_mk)
        
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
            
            tabla_detalles <- tags$table(class = "sp-table",
              tags$thead(tags$tr(
                tags$th(t("team_header_date")), 
                tags$th(t("round_prefix")), 
                tags$th(t("match_header_match")), 
                tags$th(t("referee_header_role"))
              )),
              tags$tbody(map(1:nrow(historial_stage), function(p_idx) {
                partido <- historial_stage[p_idx,]
                resultado_txt <- paste(partido$goles_local, "-", partido$goles_visitante)
                match_href <- file.path(path_rel_partidos, paste0(partido$id_partido, ".html"))
                tags$tr(
                  class = "sp-clickable-row",
                  onclick = paste0("window.location='", match_href, "'"),
                  tags$td(partido$fecha),
                  tags$td(partido$jornada),
                  tags$td(
                    tags$span(class = "sp-match-cell",
                      get_logo_tag(partido$local, css_class = "sp-table-logo"),
                      tags$span(class = "sp-team-name", partido$home_name),
                      tags$span(class = "sp-result", resultado_txt),
                      tags$span(class = "sp-team-name", partido$away_name),
                      get_logo_tag(partido$visitante, css_class = "sp-table-logo")
                    )
                  ),
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
          tags$div(class = "sp-container",
            tags$div(class = "sp-header", titulo_perfil_arbitra),
            tags$section(class = "sp-history-section",
              tags$h3(class = "sp-section-title", t("referee_history_by_competition")),
              tags$table(class = "sp-table",
                tags$thead(tags$tr(tags$th(t("player_season")), tags$th(t("player_competition")), tags$th(t("referee_header_matches")))),
                tags$tbody(tbody_content)
              )
            )
          )
        )
        
        pagina_arbitro_final <- crear_pagina_html(
          contenido_principal = contenido_arbitro,
          titulo_pagina = current_arb_name,
          path_to_root_dir = "../..",
          script_contraseña = script_contraseña_lang
        )
        save_html(pagina_arbitro_final, file = file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$arbitros, paste0(id_a, ".html")))
        p_arbitros()
      }, future.seed = TRUE, future.packages = pkgs_paralelos)
      })
    }
    
    if (GENERAR_PERFILES_ESTADIO) {
      todos_estadios <- unique(na.omit(estadios_df$estadio))
      estadios_a_generar <- todos_estadios[
        !(generar_id_seguro(todos_estadios) %in% stadium_ids_to_skip) &
        (full_rebuild_needed | generar_id_seguro(todos_estadios) %in% affected_stadium_ids)
      ]
      n_estadios <- length(estadios_a_generar)
      message(sprintf("   > Generating %d stadium profiles in parallel...", n_estadios))
      if (n_estadios > 0) with_progress({
      p_estadios <- progressor(steps = n_estadios)
      future_lapply(estadios_a_generar, function(est_mk) {
        id_e <- generar_id_seguro(est_mk)
        current_est_name <- entidades_df_lang %>% filter(original_name == est_mk) %>% pull(current_lang_name)
        historial_mk <- estadios_df %>%
          filter(estadio == est_mk) %>%
          left_join(
            partidos_df %>% select(id_partido, goles_local, goles_visitante, jornada),
            by = "id_partido"
          ) %>%
          mutate(fecha_date = as.Date(fecha, format = "%d.%m.%Y")) %>%
          arrange(desc(fecha_date))
        historial <- historial_mk %>%
          left_join(entidades_df_lang %>% select(original_name, home_name = current_lang_name), by = c("local" = "original_name")) %>%
          left_join(entidades_df_lang %>% select(original_name, away_name = current_lang_name), by = c("visitante" = "original_name")) %>%
          left_join(competiciones_unicas_df %>% select(competicion_nombre, competicion_temporada, !!sym(comp_name_col)), by = c("competicion_nombre", "competicion_temporada"))
        path_rel_partidos <- file.path("..", nombres_carpetas_relativos$partidos)
        
        contenido_estadio <- tagList(
          crear_botones_navegacion(path_to_lang_root = ".."),
          
          tags$div(class = "sp-container",
            
            # Header
            tags$div(class = "sp-header",
              tags$h2(current_est_name)
            ),
            
            # Match history
            tags$section(class = "sp-history-section",
              tags$h3(class = "sp-section-title", t("stadium_match_history")),
              tags$table(class = "sp-table",
                tags$thead(tags$tr(
                  tags$th(t("team_header_date")),
                  tags$th(t("player_competition")),
                  tags$th(t("round_prefix")),
                  tags$th(t("match_header_match"))
                )),
                tags$tbody(
                  if (nrow(historial) > 0) {
                    map(1:nrow(historial), function(p_idx) {
                      partido <- historial[p_idx, ]
                      nombre_comp <- partido[[comp_name_col]]
                      resultado_txt <- paste(partido$goles_local, "-", partido$goles_visitante)
                      match_href <- file.path(path_rel_partidos, paste0(partido$id_partido, ".html"))
                      tags$tr(
                        class = "sp-clickable-row",
                        onclick = paste0("window.location='", match_href, "'"),
                        tags$td(partido$fecha),
                        tags$td(nombre_comp),
                        tags$td(partido$jornada),
                        tags$td(
                          tags$span(class = "sp-match-cell",
                            get_logo_tag(partido$local, css_class = "sp-table-logo"),
                            tags$span(class = "sp-team-name", partido$home_name),
                            tags$span(class = "sp-result", resultado_txt),
                            tags$span(class = "sp-team-name", partido$away_name),
                            get_logo_tag(partido$visitante, css_class = "sp-table-logo")
                          )
                        )
                      )
                    })
                  } else {
                    tags$tr(tags$td(colspan = "4", t("player_no_matches")))
                  }
                )
              )
            )
          ),
          
          crear_botones_navegacion(path_to_lang_root = "..")
        )
        pagina_estadio_final <- crear_pagina_html(contenido_estadio, current_est_name, path_to_root_dir = "../..", script_contraseña_lang)
        save_html(pagina_estadio_final, file = file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$estadios, paste0(id_e, ".html")))
        p_estadios()
      }, future.seed = TRUE, future.packages = pkgs_paralelos)
      })
    }
    
    # ================== STAFF PROFILES ==================
    if (GENERAR_PERFILES_STAFF && exists("staff_df") && nrow(staff_df) > 0) {
      staff_con_perfil <- staff_df
      todos_staff <- unique(staff_con_perfil$nombre)
      
      # Determine which staff to (re)generate
      staff_ids_to_skip <- character(0) # No exclusions for staff currently
      if (!exists("affected_staff_ids")) affected_staff_ids <- character(0)
      staff_a_generar <- todos_staff[
        !(generar_id_seguro(todos_staff) %in% staff_ids_to_skip) &
        (full_rebuild_needed | generar_id_seguro(todos_staff) %in% affected_staff_ids)
      ]
      n_staff <- length(staff_a_generar)
      message(sprintf("   > Generating %d staff profiles in parallel...", n_staff))
      
      if (n_staff > 0) with_progress({
        p_staff <- progressor(steps = n_staff)
        future_lapply(staff_a_generar, function(staff_mk) {
          id_s <- generar_id_seguro(staff_mk)
          
          current_staff_name <- (entidades_df_lang %>% filter(original_name == staff_mk))$current_lang_name[1]
          if (is.na(current_staff_name)) current_staff_name <- staff_mk
          
          temporadas_summary <- stats_staff_por_temporada_df %>%
            filter(nombre == staff_mk) %>%
            left_join(competiciones_unicas_df, by = c("competicion_nombre", "competicion_temporada")) %>%
            select(competicion_temporada, competicion_nombre, !!sym(comp_name_col), num_matches)
          
          path_rel_partidos <- file.path("..", nombres_carpetas_relativos$partidos)
          
          tbody_content <- if (nrow(temporadas_summary) > 0) {
            map(1:nrow(temporadas_summary), function(j) {
              stage <- temporadas_summary[j,]
              details_id <- paste0("details-staff-", id_s, "-", j)
              nombre_competicion_mostrado <- stage[[comp_name_col]]
              
              historial_stage_mk <- staff_con_perfil %>%
                filter(nombre == staff_mk) %>%
                left_join(partidos_df, by = "id_partido") %>%
                filter(competicion_temporada == stage$competicion_temporada,
                       competicion_nombre == stage$competicion_nombre) %>%
                mutate(fecha_date = as.Date(fecha, format = "%d.%m.%Y")) %>%
                arrange(desc(fecha_date))
              
              historial_stage <- historial_stage_mk %>%
                left_join(entidades_df_lang %>% select(original_name, home_name = current_lang_name),
                          by = c("local" = "original_name")) %>%
                left_join(entidades_df_lang %>% select(original_name, away_name = current_lang_name),
                          by = c("visitante" = "original_name")) %>%
                left_join(entidades_df_lang %>% select(original_name, team_name = current_lang_name),
                          by = c("equipo" = "original_name"))
              
              tabla_detalles <- tags$table(class = "sp-table",
                tags$thead(tags$tr(
                  tags$th(t("team_header_date")),
                  tags$th(t("staff_header_team")),
                  tags$th(t("match_header_match")),
                  tags$th(t("staff_header_role"))
                )),
                tags$tbody(map(1:nrow(historial_stage), function(p_idx) {
                  partido <- historial_stage[p_idx,]
                  resultado_txt <- paste(partido$goles_local, "-", partido$goles_visitante)
                  match_href <- file.path(path_rel_partidos, paste0(partido$id_partido, ".html"))
                  rol_traducido <- t(partido$rol)
                  team_display <- if (!is.na(partido$team_name)) partido$team_name else ""
                  tags$tr(
                    class = "sp-clickable-row",
                    onclick = paste0("window.location='", match_href, "'"),
                    tags$td(partido$fecha),
                    tags$td(team_display),
                    tags$td(
                      tags$span(class = "sp-match-cell",
                        get_logo_tag(partido$local, css_class = "sp-table-logo"),
                        tags$span(class = "sp-team-name", partido$home_name),
                        tags$span(class = "sp-result", resultado_txt),
                        tags$span(class = "sp-team-name", partido$away_name),
                        get_logo_tag(partido$visitante, css_class = "sp-table-logo")
                      )
                    ),
                    tags$td(rol_traducido)
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
            tags$tr(tags$td(colspan = "3", t("player_no_matches")))
          }
          
          contenido_staff <- tagList(
            crear_botones_navegacion(path_to_lang_root = ".."),
            tags$div(class = "sp-container",
              tags$div(class = "sp-header", tags$h2(current_staff_name)),
              tags$section(class = "sp-history-section",
                tags$h3(class = "sp-section-title", t("staff_history_by_competition")),
                tags$table(class = "sp-table",
                  tags$thead(tags$tr(
                    tags$th(t("player_season")),
                    tags$th(t("player_competition")),
                    tags$th(t("staff_header_matches"))
                  )),
                  tags$tbody(tbody_content)
                )
              )
            )
          )
          
          pagina_staff_final <- crear_pagina_html(
            contenido_principal = contenido_staff,
            titulo_pagina = current_staff_name,
            path_to_root_dir = "../..",
            script_contraseña = script_contraseña_lang
          )
          save_html(pagina_staff_final, file = file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$staff, paste0(id_s, ".html")))
          p_staff()
        }, future.seed = TRUE, future.packages = pkgs_paralelos)
      })
    }
    
  } # 13.1.44. End of the main language loop.
  
  # 13.1.45. Create the redirect page at the site root.
  message("\nCreating redirect file at the site root...")
  redirect_html_content <- c('<!DOCTYPE html>', '<html>', '<head>', '<title>Redirecting...</title>', '<meta charset="utf-8">', paste0('<meta http-equiv="refresh" content="0; url=', IDIOMAS_SOPORTADOS[1], '/index.html">'), '</head>', '<body>', '<p>If you are not redirected automatically, follow this <a href="', IDIOMAS_SOPORTADOS[1], '/index.html">link</a>.</p>', '</body>', '</html>')
  writeLines(redirect_html_content, file.path(RUTA_SALIDA_RAIZ, "index.html"))
  
  # --- Restore sequential execution ---
  plan(sequential)
  
}

