################################################################################
##                                                                            ##
##    SCRIPT INDEPENDIENTE PARA GENERAR/ACTUALIZAR CALENDARIOS DESDE EXCEL    ##
##                                                                            ##
################################################################################


## -------------------------------------------------------------------------- ##
##  0. CONFIGURACIÓN INICIAL
## -------------------------------------------------------------------------- ##

message("===================================================================")
message("==  Iniciando script de generación de calendarios (standalone)   ==")
message("===================================================================")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, dplyr, stringr, purrr, htmltools, tidyr, jsonlite)

## -------------------------------------------------------------------------- ##
##  1. CARGA DE CONFIGURACIONES COMPARTIDAS
## -------------------------------------------------------------------------- ##

### 1.1. Cargar Traducciones y Función `t()` ----
ruta_traducciones <- "translations.txt"
textos <- list()
IDIOMAS_SOPORTADOS <- character(0)

if (file.exists(ruta_traducciones)) {
  tryCatch({
    traducciones_df <- read.csv(ruta_traducciones, stringsAsFactors = FALSE, encoding = "UTF-8")
    IDIOMAS_SOPORTADOS <- names(traducciones_df)[-1]
    textos <- traducciones_df %>%
      pivot_longer(cols = -key, names_to = "lang", values_to = "translation") %>%
      split(.$lang) %>%
      map(~ setNames(as.list(.$translation), .$key))
    message(paste("Traducciones cargadas para:", paste(IDIOMAS_SOPORTADOS, collapse = ", ")))
  }, error = function(e) { warning("Error al cargar traducciones.") })
} else {
  warning("Archivo 'translations.txt' no encontrado. La funcionalidad multiidioma será limitada.")
}
if (length(IDIOMAS_SOPORTADOS) == 0) IDIOMAS_SOPORTADOS <- c("mk")
idioma_actual <- IDIOMAS_SOPORTADOS[1]
t <- function(key) {
  traduccion <- textos[[idioma_actual]][[key]]
  if (is.null(traduccion)) {
    traduccion_fallback <- textos[[IDIOMAS_SOPORTADOS[1]]][[key]]
    return(traduccion_fallback %||% key)
  }
  return(traduccion)
}

### 1.2. Cargar Correcciones y Traducciones de Nombres ----
mapa_correcciones <- NULL
if (file.exists("conversions.txt")) {
  tryCatch({
    conversiones_df <- read.csv("conversions.txt", stringsAsFactors = FALSE, encoding = "UTF-8")
    mapa_correcciones <- setNames(conversiones_df$corregido, conversiones_df$original)
    message("Archivo de correcciones 'conversions.txt' cargado.")
  }, error = function(e) { warning("Error al cargar conversions.txt.") })
} else {
  message("AVISO: 'conversions.txt' no encontrado.")
}

mapa_nombres_entidades_long <- NULL
if (file.exists("entity_corrections.txt")) {
  tryCatch({
    df <- read.csv("entity_corrections.txt", stringsAsFactors = FALSE, encoding = "UTF-8", check.names = FALSE)
    key_col_name <- names(df)[1]
    mapa_nombres_entidades_long <- df %>%
      pivot_longer(cols = -all_of(key_col_name), names_to = "lang", values_to = "translated_name") %>%
      rename(original_mk = !!sym(key_col_name)) %>%
      mutate(lang = str_remove(lang, "latin_|translation_"))
    message("Correcciones/Traducciones de nombres de entidades cargadas.")
  }, error = function(e) { warning("Error al cargar entity_corrections.txt") })
}

mapa_traducciones_comp_long <- NULL
if (file.exists("competition_translations.txt")) {
  tryCatch({
    df <- read.csv("competition_translations.txt", stringsAsFactors = FALSE, encoding = "UTF-8", check.names = FALSE)
    key_col_name <- names(df)[1]
    mapa_traducciones_comp_long <- df %>%
      pivot_longer(cols = -all_of(key_col_name), names_to = "lang", values_to = "translated_name") %>%
      rename(original_mk = !!sym(key_col_name)) %>%
      mutate(lang = str_remove(lang, "latin_|translation_"))
    message("Traducciones de competiciones cargadas.")
  }, error = function(e) { warning("Error al cargar competition_translations.txt") })
}


### 1.3. Funciones y Rutas Auxiliares Completas ----

generar_id_seguro <- function(nombre) {
  map_id_plain<-c('а'='a','б'='b','в'='v','г'='g','д'='d','ѓ'='g','е'='e','ж'='z','з'='z','ѕ'='dz','и'='i','ј'='j','к'='k','л'='l','љ'='lj','м'='m','н'='n','њ'='n','о'='o','п'='p','р'='r','с'='s','т'='t','ќ'='kj','у'='u','ф'='f','х'='h','ц'='c','ч'='c','џ'='dz','ш'='s')
  nombre_latin <- str_replace_all(tolower(nombre), map_id_plain)
  id_sanitizada <- str_replace_all(nombre_latin, "[\\s/]+", "_") %>% str_replace_all("[^a-z0-9_\\-]+", "") %>% str_replace_all("_{2,}", "_") %>% str_replace_all("^_|_+$", "")
  return(id_sanitizada)
}

generar_terminos_busqueda <- function(nombre) {
  if (is.na(nombre) || nchar(trimws(nombre)) == 0) return("")
  nombre_lower <- tolower(nombre)
  map_base <- c('а'='a','б'='b','в'='v','г'='g','д'='d','з'='z','и'='i','к'='k','м'='m','н'='n','о'='o','п'='p','р'='r','с'='s','т'='t','ф'='f','х'='h')
  mapa_variaciones <- list('ч'=c('č','ch','c'),'ш'=c('š','sh','s'),'ж'=c('ž','zh','z'),'ѓ'=c('ǵ','gj','đ','g'),'ќ'=c('ḱ','kj','ć','q'),'њ'=c('ń','nj','ñ'),'љ'=c('ĺ','lj'),'у'=c('u','y'),'л'='l','е'='e','ц'=c('c','ts'),'ѕ'='dz','џ'=c('dž','j'),'ј'=c('j','y'))
  versions <- c(nombre_lower, stringr::str_replace_all(nombre_lower, map_base))
  for (char_cyrillic in names(mapa_variaciones)) {
    variants <- mapa_variaciones[[char_cyrillic]]
    expanded_versions <- c()
    for (variant in variants) { expanded_versions <- c(expanded_versions, stringr::str_replace_all(versions, char_cyrillic, variant)) }
    versions <- unique(c(versions, expanded_versions))
  }
  map_ascii_simplification <- c('č'='c','š'='s','ž'='z','đ'='d','ć'='c','ǵ'='g','ḱ'='k','ń'='n','ĺ'='l','ñ'='n')
  simplified_versions <- stringr::str_replace_all(versions, map_ascii_simplification)
  return(paste(unique(c(versions, simplified_versions)), collapse = " "))
}

crear_selector_idioma <- function(idioma_pagina_actual) {
  tags$div(class = "language-selector", style = "text-align: right; margin-bottom: 15px; font-size: 0.9em;",
           tagList(lapply(seq_along(IDIOMAS_SOPORTADOS), function(i) {
             lang_code <- IDIOMAS_SOPORTADOS[i]
             lang_name <- textos[[lang_code]][["lang_name"]] %||% lang_code
             tag_element <- if (lang_code == idioma_pagina_actual) {
               tags$span(style = "font-weight: bold; color: #333;", paste0("[ ", lang_name, " ]"))
             } else {
               js_onclick <- sprintf("window.location.href = window.location.href.replace('/%s/', '/%s/'); return false;", idioma_pagina_actual, lang_code)
               tags$a(href = "#", onclick = js_onclick, paste0("[ ", lang_name, " ]"))
             }
             if (i < length(IDIOMAS_SOPORTADOS)) { tagList(tag_element, " ") } else { tag_element }
           })
           )
  )
}

crear_botones_navegacion <- function(path_to_lang_root = ".") {
  tags$div(class = "nav-buttons",
           tags$a(t("back_button"), href = "#", onclick = "history.back(); return false;", class = "back-link"),
           tags$a(t("home_button"), href = file.path(path_to_lang_root, "index.html"), class = "back-link")
  )
}

crear_pagina_html <- function(contenido_principal, titulo_pagina, path_to_root_dir = ".") {
  tags$html(lang = idioma_actual,
            tags$head(tags$meta(charset="UTF-8"), tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"), tags$title(titulo_pagina),
                      tags$link(rel = "stylesheet", href = file.path(path_to_root_dir, nombres_carpetas_relativos$assets, "style.css"))),
            tags$body(`data-search-results-title`=t("search_results_for"), `data-no-search-results-msg`=t("no_search_results"), `data-search-prompt-msg`=t("search_prompt"), `data-search-placeholder`=t("search_placeholder"),
                      tags$div(class="container",
                               crear_selector_idioma(idioma_pagina_actual = idioma_actual),
                               tags$h1(tags$a(href=file.path(path_to_root_dir, idioma_actual, "index.html"), style="color: inherit; text-decoration: none;", t("site_title"))),
                               tags$div(class="search-container",
                                        tags$form(action="#", onsubmit="showSearchResults(); return false;",
                                                  tags$input(type="text", id="search-input", class="search-input", placeholder=t("search_placeholder"), onkeyup="handleSearchInput(event)"),
                                                  tags$button(type="submit", class="search-button", t("search_button"))),
                                        tags$div(id = "search-suggestions")),
                               tags$div(id = "main-content", contenido_principal)),
                      tags$script(defer = NA, src = file.path(path_to_root_dir, nombres_carpetas_relativos$assets, "script.js"))))
}


nombres_carpetas_relativos <- list(assets="assets", competiciones="natprevaruvanja", partidos="natprevari", logos="logos")
nombres_archivos_mk <- list(partidos="raspored")
RUTA_SALIDA_RAIZ <- "docs"
RUTA_ASSETS_DESTINO <- file.path(RUTA_SALIDA_RAIZ, nombres_carpetas_relativos$assets)
RUTA_LOGOS_DESTINO <- file.path(RUTA_ASSETS_DESTINO, nombres_carpetas_relativos$logos)

dir.create(RUTA_LOGOS_DESTINO, showWarnings = FALSE, recursive = TRUE)
for (lang in IDIOMAS_SOPORTADOS) {
  dir.create(file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones), showWarnings = FALSE, recursive = TRUE)
}
if (dir.exists("Logos")) {
  walk(list.files("Logos", pattern = "\\.png$", full.names = TRUE), function(ruta_fuente) {
    nombre_base <- tools::file_path_sans_ext(basename(ruta_fuente))
    nombre_destino <- if(basename(ruta_fuente) == "NOLOGO.png") "NOLOGO.png" else paste0(generar_id_seguro(nombre_base), ".png")
    file.copy(from = ruta_fuente, to = file.path(RUTA_LOGOS_DESTINO, nombre_destino), overwrite = TRUE)
  })
  message("Escudos de logos actualizados en la carpeta de assets.")
}


## -------------------------------------------------------------------------- ##
##  2. LÓGICA PRINCIPAL
## -------------------------------------------------------------------------- ##

### 2.1. Procesar todos los calendarios y crear dataframes maestros ----
ruta_calendarios <- "Calendarios"
archivos_calendario <- list.files(path = ruta_calendarios, pattern = "\\.xlsx?$", full.names = TRUE)
if (length(archivos_calendario) == 0) stop("No se encontraron archivos de calendario en la carpeta 'Calendarios'.")

lista_calendarios_df <- map(archivos_calendario, function(ruta_cal) {
  nombre_archivo_raw <- tools::file_path_sans_ext(basename(ruta_cal))
  match_temporada <- str_match(nombre_archivo_raw, "^(.+?)\\s(\\d{2})_(\\d{2})$")
  if (is.na(match_temporada[1, 1])) {
    warning(paste0("SALTANDO: El archivo '", basename(ruta_cal), "' no sigue el formato 'NOMBRE COMPETICION AA_AA.xlsx'."))
    return(NULL)
  }
  
  df_excel <- read_excel(ruta_cal, col_types = "text")[, c(1, 4)]
  names(df_excel) <- c("jornada", "partido_str")
  df_procesado <- df_excel %>%
    fill(jornada, .direction = "down") %>%
    filter(!is.na(partido_str), str_detect(partido_str, "\\s-\\s")) %>%
    separate(partido_str, into = c("local", "visitante"), sep = "\\s-\\s", extra = "merge") %>%
    mutate(across(c(local, visitante), ~str_trim(.)))
  
  if (!is.null(mapa_correcciones)) {
    df_procesado <- df_procesado %>%
      mutate(
        local = recode(local, !!!mapa_correcciones),
        visitante = recode(visitante, !!!mapa_correcciones)
      )
  }
  
  df_procesado %>% mutate(
    competicion_nombre = str_trim(match_temporada[1, 2]),
    competicion_temporada = paste0(match_temporada[1, 3], "/", match_temporada[1, 4])
  )
}) %>% compact()

if(length(lista_calendarios_df) == 0) stop("No se pudo procesar ningún archivo de calendario válido.")

calendarios_completos_df <- bind_rows(lista_calendarios_df)
equipos_unicos_df <- tibble(original_name = unique(c(calendarios_completos_df$local, calendarios_completos_df$visitante)))
competiciones_unicas_df <- calendarios_completos_df %>%
  distinct(competicion_nombre, competicion_temporada) %>%
  mutate(nombre_comp_completo = paste(competicion_nombre, competicion_temporada))

# ======================================================================== #
# ==        ¡AQUÍ ESTÁ LA CORRECCIÓN DE LA LÓGICA DE NOMBRES!           == #
# ======================================================================== #
# 1. Crear el dataframe maestro de entidades con el nombre original.
entidades_maestro_df <- equipos_unicos_df

# 2. Añadir la columna de nombres en macedonio, que es simplemente el nombre original.
entidades_maestro_df$mk <- entidades_maestro_df$original_name

# 3. Iterar sobre los OTROS idiomas para añadir las traducciones/transliteraciones.
map_transliteration_entity <- c('а'='a','б'='b','в'='v','г'='g','д'='d','ѓ'='gj','е'='e','ж'='ž','з'='z','ѕ'='dz','и'='i','ј'='j','к'='k','л'='l','љ'='lj','м'='m','н'='n','њ'='nj','о'='o','п'='p','р'='r','с'='s','т'='t','ќ'='kj','у'='u','ф'='f','х'='h','ц'='c','ч'='č','џ'='dž','ш'='š')
for (lang_code in setdiff(IDIOMAS_SOPORTADOS, "mk")) {
  # Unir con traducciones explícitas de entity_corrections.txt
  traducciones_lang <- mapa_nombres_entidades_long %>% 
    filter(lang == lang_code) %>%
    select(original_mk, !!sym(lang_code) := translated_name)
  
  entidades_maestro_df <- entidades_maestro_df %>%
    left_join(traducciones_lang, by = c("original_name" = "original_mk"))
  
  # Aplicar transliteración como fallback SOLO si la traducción es NA.
  entidades_maestro_df[[lang_code]] <- coalesce(
    entidades_maestro_df[[lang_code]], 
    str_to_title(str_replace_all(tolower(entidades_maestro_df$original_name), map_transliteration_entity))
  )
}

### 2.2. Detectar partidos que YA tienen acta (para no duplicarlos) ----
partidos_con_acta_df <- tibble()
if (file.exists("actas_cache.rds")) {
  message("Cargando caché de actas existentes para evitar duplicados...")
  partidos_con_acta_df <- map_dfr(readRDS("actas_cache.rds"), "partido_info")
}

### 2.3. Bucle de Generación por Idioma ----
for (lang in IDIOMAS_SOPORTADOS) {
  idioma_actual <<- lang
  message(paste("\n--- Generando páginas para el idioma:", toupper(lang), "---"))
  
  # Generar el índice de búsqueda para este idioma
  search_equipos <- entidades_maestro_df %>%
    mutate(Тип = t("team_type"), target_id = paste0("equipo-", generar_id_seguro(original_name)), search_terms = sapply(original_name, generar_terminos_busqueda, USE.NAMES=FALSE)) %>%
    select(Име = !!sym(lang), Тип, target_id, search_terms)
  
  search_competiciones <- competiciones_unicas_df %>%
    mutate(
      translated_name = if (lang == "mk") {
        nombre_comp_completo
      } else {
        map_chr(nombre_comp_completo, ~ (mapa_traducciones_comp_long %>% filter(original_mk == .x, lang == !!idioma_actual) %>% pull(translated_name))[1] %||% .x)
      },
      Тип = t("competition_type"), target_id = paste0("menu-competicion-", generar_id_seguro(nombre_comp_completo)), search_terms = sapply(nombre_comp_completo, generar_terminos_busqueda, USE.NAMES=FALSE)) %>%
    select(Име = translated_name, Тип, target_id, search_terms)
  
  search_index_df_lang <- bind_rows(search_equipos, search_competiciones) %>% arrange(Име)
  jsonlite::write_json(search_index_df_lang, file.path(RUTA_ASSETS_DESTINO, paste0("search_data_calendars_", lang, ".json")), auto_unbox = TRUE)
  
  for (i in 1:nrow(competiciones_unicas_df)) {
    comp_info <- competiciones_unicas_df[i, ]
    message(paste("   -> Procesando competición:", comp_info$nombre_comp_completo))
    
    partidos_calendario_comp <- calendarios_completos_df %>% filter(nombre_comp_completo == comp_info$nombre_comp_completo)
    partidos_acta_comp <- partidos_con_acta_df %>% filter(competicion_nombre == comp_info$competicion_nombre, competicion_temporada == comp_info$competicion_temporada) %>% mutate(es_de_calendario = FALSE)
    
    partidos_a_renderizar_df <- partidos_calendario_comp %>%
      anti_join(partidos_acta_comp, by = c("local", "visitante")) %>%
      mutate(id_partido = paste0("cal_", generar_id_seguro(paste(local, visitante, jornada))), es_de_calendario = TRUE) %>%
      bind_rows(partidos_acta_comp)
    
    if (nrow(partidos_a_renderizar_df) == 0) {
      message("      > No hay partidos que mostrar (quizás todos tienen acta).")
      next
    }
    
    competicion_id <- generar_id_seguro(comp_info$nombre_comp_completo)
    is_cup <- str_detect(tolower(comp_info$competicion_nombre), "куп")
    jornadas_comp <- unique(partidos_a_renderizar_df$jornada)
    
    # ======================================================================== #
    # ==        ¡AQUÍ ESTÁ LA CORRECCIÓN DEL NOMBRE DE LA COMPETICIÓN!      == #
    # ======================================================================== #
    nombre_comp_traducido <- if (lang == "mk") {
      comp_info$nombre_comp_completo
    } else {
      (mapa_traducciones_comp_long %>% filter(original_mk == comp_info$nombre_comp_completo, lang == !!idioma_actual) %>% pull(translated_name))[1] %||% comp_info$nombre_comp_completo
    }
    
    # A. GENERACIÓN DE LA PÁGINA DEL CALENDARIO (_raspored)
    contenido_partidos_html <- tagList(
      crear_botones_navegacion(path_to_lang_root = ".."),
      tags$h2(paste(t("schedule_title"), "-", nombre_comp_traducido)),
      map(jornadas_comp, function(j) {
        partidos_jornada <- partidos_a_renderizar_df %>% filter(jornada == j)
        tagList(tags$h3(class="jornada-header", if(is_cup) as.character(j) else paste(t("round_prefix"),j)),
                map(1:nrow(partidos_jornada), function(k) {
                  partido <- partidos_jornada[k,]
                  local_name_lang <- entidades_maestro_df %>% filter(original_name == partido$local) %>% pull(!!sym(lang))
                  visitante_name_lang <- entidades_maestro_df %>% filter(original_name == partido$visitante) %>% pull(!!sym(lang))
                  
                  if (partido$es_de_calendario) {
                    tags$div(class="partido-link", tags$span(class="equipo equipo-local", get_logo_tag(partido$local), tags$span(local_name_lang)), tags$span(class="resultado","-:-"), tags$span(class="equipo equipo-visitante", tags$span(visitante_name_lang), get_logo_tag(partido$visitante)))
                  } else {
                    resultado_texto <- paste(partido$goles_local, "-", partido$goles_visitante)
                    if (!is.na(partido$penales_local)) resultado_texto <- sprintf("%s (%s - %s)", resultado_texto, partido$penales_local, partido$penales_visitante)
                    if (isTRUE(partido$es_resultado_oficial)) resultado_texto <- paste(resultado_texto, "*")
                    tags$a(class="partido-link", href=file.path("..",nombres_carpetas_relativos$partidos,paste0(partido$id_partido,".html")), tags$span(class="equipo equipo-local",get_logo_tag(partido$local),tags$span(local_name_lang)), tags$span(class="resultado",resultado_texto), tags$span(class="equipo equipo-visitante",tags$span(visitante_name_lang), get_logo_tag(partido$visitante)))
                  }
                }))
      })
    )
    
    ruta_salida_calendario <- file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, paste0(competicion_id, "_", nombres_archivos_mk$partidos, ".html"))
    save_html(crear_pagina_html(contenido_partidos_html, paste(t("schedule_title"), "-", nombre_comp_traducido), "../.."), file = ruta_salida_calendario)
    
    # B. GENERACIÓN DE LA PÁGINA DE MENÚ DE LA COMPETICIÓN
    contenido_menu_html <- tagList(
      crear_botones_navegacion(path_to_lang_root = ".."),
      tags$h2(nombre_comp_traducido),
      tags$div(class="menu-container", tags$a(href=basename(ruta_salida_calendario), class="menu-button", t("schedule_title")))
    )
    
    ruta_salida_menu <- file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, paste0(competicion_id, ".html"))
    save_html(crear_pagina_html(contenido_menu_html, nombre_comp_traducido, "../.."), file = ruta_salida_menu)
  }
}

message("\n===================================================================")
message("==      Proceso de generación de calendarios finalizado.         ==")
message("== Revisa las páginas de menú y calendario en la carpeta 'docs'. ==")
message("===================================================================")