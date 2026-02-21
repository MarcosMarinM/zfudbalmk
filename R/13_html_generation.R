#### 13. HTML PAGE GENERATION (OPTIMIZED ARCHITECTURE) ####

if (hubo_cambios) {
  
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
            tabla_porteras_comp_raw <- stats_porteras_por_comp_df %>% filter(competicion_id == comp_id) %>% left_join(jugadoras_stats_df %>% select(id, !!player_name_col_sym), by = "id") %>% left_join(entidades_df_lang, by = c("TeamName_mk" = "original_name")) %>% left_join(minutos_totales_equipo_comp, by = c("TeamName_mk" = "equipo")) %>% mutate(pct_minutos = if_else(!is.na(minutos_totales_posibles) & minutos_totales_posibles > 0, (Minutes / minutos_totales_posibles) * 100, 0), group = if_else(pct_minutos >= 75, "mas_50", "menos_50")) %>% select(id, PlayerName = !!player_name_col_sym, TeamName = current_lang_name, TeamName_mk, GA90, GA, Minutes, CS, group)
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
          
          # --- 1. Definir helper de logo ---
          # Esta función helper obtiene la ruta relativa al logo DESDE la página del hub.
          get_logo_path_relativo <- function(nombre_equipo_mk) {
            nombre_archivo_final <- paste0(generar_id_seguro(nombre_equipo_mk), ".png")
            if (!file.exists(file.path(RUTA_LOGOS_DESTINO, nombre_archivo_final))) {
              nombre_archivo_final <- "NOLOGO.png"
            }
            # La ruta debe ser relativa desde '.../natprevaruvanja/comp.html' -> '../../assets/logos/...'
            file.path("..", "..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, nombre_archivo_final)
          }
          
          # --- 2. Preparar datos de JORNADAS (Schedule) ---
          partidos_jugados <- partidos_comp %>% filter(!is.na(id_partido))
          jornada_por_defecto_raw <- NA_character_ # La jornada 'raw' (ej: "1", "1/2")
          
          if (nrow(partidos_jugados) > 0) {
            # Usar la misma lógica de ordenación que la página de calendario
            jornadas_jugadas_ordenadas <- data.frame(jornada = unique(partidos_jugados$jornada)) %>%
              filter(!is.na(jornada)) %>%
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
              arrange(desc(order_key)) # Ordenamos de la más reciente a la más antigua
            
            if (nrow(jornadas_jugadas_ordenadas) > 0) {
              jornada_por_defecto_raw <- jornadas_jugadas_ordenadas$jornada[1]
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
            # Corrección para obtener el nombre del equipo en el idioma correcto
            left_join(entidades_df_lang, by = c("TeamNames_mk" = "original_name")) %>%
            filter(!is.na(PlayerName)) %>%
            select(Pos, PlayerName, TeamName = current_lang_name, Goals, id_jugadora = id, id_equipo_mk = TeamNames_mk) %>%
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
            left_join(entidades_df_lang, by = c("TeamNames_mk" = "original_name")) %>%
            filter(!is.na(PlayerName)) %>%
            select(Pos, PlayerName, TeamName = current_lang_name, YellowCards, RedCards, id_jugadora = id, id_equipo_mk = TeamNames_mk) %>%
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
          
          # Porteras (La nueva pestaña que pediste)
          datos_top_porteras <- stats_porteras_por_comp_df %>%
            filter(competicion_id == comp_id) %>% # 1. Filtrar por competición
            
            # 2. Unir con los minutos totales para calcular el %
            left_join(minutos_totales_equipo_comp, by = c("TeamName_mk" = "equipo")) %>%
            
            # 3. Calcular el porcentaje de minutos jugados
            mutate(pct_minutos = if_else(!is.na(minutos_totales_posibles) & minutos_totales_posibles > 0, (Minutes / minutos_totales_posibles) * 100, 0)) %>%
            
            # 4. *** ¡EL FILTRO CLAVE PEDIDO POR EL USUARIO! ***
            #    Usamos >= 50 para replicar la lógica original de la página completa.
            filter(pct_minutos >= 75) %>% 
            
            # 5. Continuar con el resto de la lógica...
            left_join(jugadoras_lang_df, by = "id") %>%
            left_join(entidades_df_lang, by = c("TeamName_mk" = "original_name")) %>%
            filter(!is.na(PlayerName)) %>%
            # Ordenar por GA90 (asc), luego CS (desc), luego Minutos (desc)
            arrange(GA90, desc(CS), desc(Minutes)) %>%
            mutate(Pos = row_number()) %>%
            select(Pos, PlayerName, TeamName = current_lang_name, GA90, CS, id_jugadora = id, id_equipo_mk = TeamName_mk) %>%
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
                        tags$td(
                          class = "team-cell",
                          tags$img(class="team-logo", src = ruta_relativa_logo_html, alt = nombre_equipo),
                          tags$a(href=file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(nombre_equipo_original), ".html")), cell_value)
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
        render_equipo_html <- function(df_equipo, goles_del_partido, tarjetas_del_partido, is_national_team_match, team_original_mk_name, duracion_partido) { # <-- CAMBIO 1: Añadir duracion_partido
          if (is.null(df_equipo) || nrow(df_equipo) == 0) { return(tags$p(t("match_no_data"))) }
          starters <- df_equipo %>% filter(tipo == "Titular")
          subs <- df_equipo %>% filter(tipo == "Suplente")
          
          crear_lista_jugadoras <- function(df_j, duracion_partido) { 
            if (nrow(df_j) == 0) { return(tags$p(style = "color:#777;", t("match_no_players"))) }
            tags$ul(pmap(df_j, function(id, PlayerName, dorsal, tipo, es_portera, es_capitana, min_entra, min_sale, minutos_jugados, ...) {
              eventos_html <- tagList()
              goles_jugadora <- goles_del_partido %>% filter(id == !!id, tipo == "Normal")
              if (nrow(goles_jugadora) > 0) { walk(1:nrow(goles_jugadora), function(g) { gol <- goles_jugadora[g,]; eventos_html <<- tagAppendChild(eventos_html, tags$span(class = "player-event goal", HTML(paste0("⚽︎ ", formatear_minuto_partido(gol$minuto), "'")))) }) }
              tarjetas_jugadora <- tarjetas_del_partido %>% filter(id == !!id)
              if (nrow(tarjetas_jugadora) > 0) { walk(1:nrow(tarjetas_jugadora), function(c) { tarjeta <- tarjetas_jugadora[c,]; card_span <- tags$span(class = if (tarjeta$tipo == "Amarilla") "card-yellow" else "card-red"); eventos_html <<- tagAppendChild(eventos_html, tags$span(class = "player-event", card_span, HTML(paste0("︎ ", formatear_minuto_partido(tarjeta$minuto), "'")))) }) }
              if (!is.na(min_entra) && tipo == "Suplente") { eventos_html <- tagAppendChild(eventos_html, tags$span(class = "player-event sub-in", paste0("↑", min_entra, "'"))) }
              
              if (!is.na(min_sale) && min_sale < duracion_partido && !is.na(minutos_jugados) && minutos_jugados > 0) { # <-- CAMBIO 3: Usar duracion_partido en lugar de 90
                eventos_html <- tagAppendChild(eventos_html, tags$span(class = "player-event sub-out", paste0("↓", min_sale, "'")))
              }
              # -------------------------------------------
              
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
          tagList(tags$h4(t("match_starting_lineup")), crear_lista_jugadoras(starters, duracion_partido), tags$h4(t("match_substitutes")), crear_lista_jugadoras(subs, duracion_partido)) # <-- CAMBIO 4: Pasar duracion_partido a las llamadas
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
                              partido_info$local,
                              partido_info$duracion_partido
                            )
                   ), 
                   tags$div(class = "columna-alineacion", 
                            crear_cabecera_alineacion(partido_info$visitante, visitante_name), 
                            render_equipo_html(
                              filter(alineacion_partido_lang, equipo == partido_info$visitante), 
                              goles_partido, 
                              tarjetas_partido,
                              partido_info$es_partido_seleccion,
                              partido_info$visitante,
                              partido_info$duracion_partido
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

