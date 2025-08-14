################################################################################
##                                                                            ##
##           SCRIPT DE GENERACIÓN DE INFORME HTML - CONTINUACIÓN              ##
##                                                                            ##
################################################################################


## -------------------------------------------------------------------------- ##
##  6. CONFIGURACIÓN INICIAL Y DEL ENTORNO
## -------------------------------------------------------------------------- ##

### 6.1. Carga de paquetes ----

# Carga de paquetes necesarios para la generación del informe HTML.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, tidyr, purrr, htmltools, stringr, jsonlite, readxl
)

## -------------------------------------------------------------------------- ##
##  7. DEFINICIÓN DE FUNCIONES AUXILIARES Y PLANTILLAS
## -------------------------------------------------------------------------- ##

### 7.1. Funciones de formato de texto y sanitización ----

#' Genera un conjunto exhaustivo pero optimizado de términos de búsqueda.
#'
#' Esta función utiliza una estrategia de "expansión lineal" para evitar la
#' explosión combinatoria. En lugar de crear permutaciones de permutaciones,
#' genera un nuevo término por cada regla de sustitución aplicada al nombre original.
#' Esto mantiene la riqueza de variantes de búsqueda deseadas por el usuario
#' sin que el tamaño del índice de búsqueda crezca de forma exponencial.
#'
#' @param nombre Un string con el nombre en cirílico.
#' @return Un string con todos los términos de búsqueda únicos y optimizados separados por espacios.
generar_terminos_busqueda <- function(nombre) {
  if (is.na(nombre) || nchar(trimws(nombre)) == 0) return("")
  
  nombre_lower <- tolower(nombre)
  
  # Contenedor para todos los términos generados
  all_terms <- c(nombre_lower)
  
  # --- Listas de variantes del usuario ---
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
  
  # --- Proceso de Generación Lineal ---
  
  # 1. Añadir la transliteración base
  all_terms <- c(all_terms, str_replace_all(nombre_lower, map_base))
  
  # 2. Expansión lineal: Por cada regla de `mapa_variaciones`, crear un nuevo
  #    término a partir del nombre ORIGINAL.
  for (char_cyrillic in names(mapa_variaciones)) {
    # Optimización: solo procesar si el carácter existe en el nombre
    if (str_detect(nombre_lower, fixed(char_cyrillic))) {
      for (variant in mapa_variaciones[[char_cyrillic]]) {
        new_term <- str_replace_all(nombre_lower, fixed(char_cyrillic), variant)
        all_terms <- c(all_terms, new_term)
      }
    }
  }
  
  # 3. Aplicar simplificación final a todos los términos generados
  map_ascii_simplification <- c(
    'č'='c', 'š'='s', 'ž'='z', 'đ'='d', 'ć'='c', 'ǵ'='g',
    'ḱ'='k', 'ń'='n', 'ĺ'='l', 'ñ'='n', 'ë'='e', 'ç'='c', 'q'='k', 'x'='z', 'ß'='s'
  )
  
  simplified_terms <- str_replace_all(all_terms, map_ascii_simplification)
  
  # 4. Combinar, eliminar duplicados y devolver
  final_terms <- unique(c(all_terms, simplified_terms))
  return(paste(final_terms, collapse = " "))
}


#' Crea un identificador seguro para usar en URLs y nombres de archivo.
#' Translitera, convierte a minúsculas, reemplaza espacios y elimina caracteres no válidos.
#' @param nombre El string original.
#' @return Un string sanitizado.
generar_id_seguro <- function(nombre) {
  # Mapa de transliteración "plana" para IDs de URL simples y legibles.
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

#' Obtiene el código ISO 3166-1 alpha-2 para un equipo nacional.
#'
#' @param team_name_mk Nombre del equipo en macedonio.
#' @return El código ISO (ej. "mk", "gb") o NA si no es una selección nacional conocida.
get_national_team_iso <- function(team_name_mk) {
  # Caso especial para "Македонија"
  if (team_name_mk == "Македонија") {
    return("mk")
  }
  
  # Buscar en el mapeo de nacionalidades
  if (!is.null(mapeo_completo_df) && nrow(mapeo_completo_df) > 0) {
    match_row <- mapeo_completo_df %>%
      filter(nombre_macedonio == team_name_mk) %>%
      head(1) # Tomar la primera coincidencia
    
    if (nrow(match_row) > 0 && !is.na(match_row$codigo_iso)) {
      return(tolower(match_row$codigo_iso)) # Devolver en minúsculas para la URL de la bandera
    }
  }
  return(NA_character_) # No es una selección nacional o no se encontró el código ISO
}

### 7.2. Funciones de manipulación de datos ----

#' Aplica un mapa de conversiones (diccionario) a columnas de un dataframe.
#' @param df El dataframe a modificar.
#' @param columnas Vector de nombres de columnas a las que aplicar el mapa.
#' @param mapa_conversiones Un vector nombrado (original=corregido).
#' @return El dataframe modificado.
aplicar_conversiones <- function(df, columnas, mapa_conversiones) {
  if (is.null(mapa_conversiones) || nrow(df) == 0) return(df)
  df %>%
    mutate(across(any_of(columnas), ~ recode(.x, !!!mapa_conversiones)))
}

#' Reordena nombres de formato "Apellido, Nombre" a "Nombre Apellido".
#' @param nombres Un vector de strings con nombres.
#' @return Un vector con los nombres reordenados.
reordenar_nombre_jugadora <- function(nombres) {
  sapply(nombres, function(nombre) {
    if (is.na(nombre)) return(NA_character_)
    palabras <- str_split(nombre, "\\s+")[[1]]
    if (length(palabras) >= 2) {
      primer_nombre <- palabras[length(palabras)]
      apellido <- paste(palabras[-length(palabras)], collapse = " ")
      return(paste(primer_nombre, apellido))
    } else {
      return(nombre)
    }
  }, USE.NAMES = FALSE)
}


### 7.3. Funciones de generación de componentes HTML ----

#' Crea el selector de idioma.
#' Genera enlaces para cambiar entre los idiomas soportados.
#' @param idioma_pagina_actual El código del idioma de la página actual (e.g., "mk").
#' @return Un objeto `div` de htmltools con los enlaces de idioma.
crear_selector_idioma <- function(idioma_pagina_actual) {
  tags$div(
    class = "language-selector",
    style = "text-align: right; margin-bottom: 15px; font-size: 0.9em;",
    tagList(
      lapply(seq_along(IDIOMAS_SOPORTADOS), function(i) {
        lang_code <- IDIOMAS_SOPORTADOS[i]
        
        # Obtenemos el nombre del idioma desde el diccionario del idioma actual para la visualización
        # Usamos el diccionario del idioma de destino para el nombre del idioma
        lang_name <- textos[[lang_code]][["lang_name"]] %||% lang_code
        
        tag_element <- if (lang_code == idioma_pagina_actual) {
          tags$span(style = "font-weight: bold; color: #333;", paste0("[ ", lang_name, " ]"))
        } else {
          # El JS reemplaza dinámicamente el código de idioma en la URL actual.
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


#' Crea los botones de navegación estándar (Atrás, Inicio).
#' @param path_to_lang_root Ruta relativa a la raíz del idioma actual (e.g., '..').
#' @return Un objeto `div` de htmltools.
crear_botones_navegacion <- function(path_to_lang_root = ".") {
  tags$div(class = "nav-buttons",
           tags$a(t("back_button"), href = "#", onclick = "history.back(); return false;", class = "back-link"),
           tags$a(t("home_button"), href = file.path(path_to_lang_root, "index.html"), class = "back-link")
  )
}

#' Crea la estructura base de una página HTML (plantilla).
#' @param contenido_principal El contenido principal de la página (un objeto de htmltools).
#' @param titulo_pagina El título que aparecerá en la pestaña del navegador (ya traducido).
#' @param path_to_root_dir Ruta relativa al directorio raíz 'docs/'.
#' @param search_data_json El string JSON con los datos de búsqueda.
#' @param script_contraseña El tag de script para la protección por contraseña.
#' @return Un objeto `html` de htmltools completo.
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
              # Se elimina el script que incrustaba el JSON aquí.
              tags$script(defer = NA, src = file.path(path_to_root_dir, nombres_carpetas_relativos$assets, "script.js"))
            )
  )
}



### 7.4. Función para generar cronología de partidos ----

#' Genera un dataframe con la cronología de eventos de un partido (goles, tarjetas, cambios).
#' @param id_p El ID del partido.
#' @param resumen_partido La lista de resultados para un partido, generada por `procesar_acta`.
#' @param entidades_lang_df El dataframe de entidades ya filtrado para el idioma actual.
#' @param jugadoras_lang_df El dataframe de jugadoras ya filtrado para el idioma actual.
#' @return Un dataframe ordenado por minuto con los eventos del partido.
generar_cronologia_df <- function(id_p, resumen_partido, entidades_lang_df, jugadoras_lang_df) {
  lista_eventos <- list()
  path_jugadoras <- file.path("..", nombres_carpetas_relativos$jugadoras)
  path_timovi <- file.path("..", nombres_carpetas_relativos$timovi)
  
  # Obtener la bandera que indica si es un partido de la selección.
  # La extraemos del dataframe `partidos_df` que ya tiene esta lógica.
  es_partido_seleccion <- (partidos_df %>% filter(id_partido == id_p) %>% pull(es_partido_seleccion))[1]
  if (is.na(es_partido_seleccion)) {
    es_partido_seleccion <- FALSE
  }
  
  # --- INICIO DE LA MODIFICACIÓN CLAVE ---
  # Función auxiliar interna para generar enlaces condicionales.
  # Decide si una entidad (jugadora o equipo) debe tener un enlace.
  crear_link_condicional <- function(nombre_a_mostrar, ruta_base, id_seguro, nombre_equipo_original_mk) {
    # Un enlace se crea SI:
    # 1. NO es un partido de la selección.
    # O
    # 2. ES un partido de la selección Y la entidad pertenece a "Македонија".
    debe_ser_enlazable <- !es_partido_seleccion || (es_partido_seleccion && nombre_equipo_original_mk == "Македонија")
    
    if (debe_ser_enlazable && !is.na(id_seguro) && nchar(id_seguro) > 0) {
      return(sprintf("<a href='%s/%s.html'>%s</a>", ruta_base, id_seguro, nombre_a_mostrar))
    } else {
      # Si no, simplemente se devuelve el nombre como texto plano.
      return(nombre_a_mostrar)
    }
  }
  # --- FIN DE LA MODIFICACIÓN CLAVE ---
  
  # 1. Goles
  goles_data <- goles_df_unificado %>% 
    filter(id_partido == id_p) %>%
    left_join(jugadoras_lang_df, by = "id")
  
  if (!is.null(goles_data) && nrow(goles_data) > 0) {
    goles_eventos <- goles_data %>% 
      mutate(
        equipo_jugadora_lang = entidades_df_lang$current_lang_name[match(equipo_jugadora, entidades_df_lang$original_name)],
        equipo_acreditado_lang = entidades_df_lang$current_lang_name[match(equipo_acreditado, entidades_df_lang$original_name)],
        texto_evento = pmap_chr(list(tipo, id, PlayerName, equipo_jugadora, equipo_jugadora_lang, equipo_acreditado, equipo_acreditado_lang), function(t, id_j, j_lang, ej_mk, ej_lang, ea_mk, ea_lang) {
          
          # Lógica condicional para jugadora y equipos
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
  
  # 2. Tarjetas
  tarjetas_data <- tarjetas_df_unificado %>% 
    filter(id_partido == id_p) %>%
    left_join(jugadoras_lang_df, by = "id") %>%
    left_join(entidades_df_lang, by = c("equipo" = "original_name"))
  
  if (!is.null(tarjetas_data) && nrow(tarjetas_data) > 0) {
    tarjetas_eventos <- tarjetas_data %>%
      mutate(
        texto_evento = pmap_chr(list(id, PlayerName, equipo, current_lang_name), function(id_j, j_lang, e_mk, e_lang) {
          
          # Lógica condicional para jugadora y equipo
          link_jugadora <- crear_link_condicional(j_lang, path_jugadoras, id_j, e_mk)
          link_equipo <- crear_link_condicional(e_lang, path_timovi, generar_id_seguro(e_mk), e_mk)
          
          sprintf(t("match_timeline_card"), link_jugadora, link_equipo)
        }),
        icono = if_else(tipo == "Amarilla", "🟨", "🟥")
      ) %>% select(minuto, icono, texto_evento)
    lista_eventos[[length(lista_eventos) + 1]] <- tarjetas_eventos
  }
  
  # 3. Sustituciones
  procesar_cambios <- function(cambios_df, nombre_equipo_mk, alineacion_equipo) {
    if (is.null(cambios_df) || nrow(cambios_df) == 0 || is.null(alineacion_equipo) || nrow(alineacion_equipo) == 0) return(NULL)
    map_dfr(1:nrow(cambios_df), function(i) {
      cambio <- cambios_df[i,]
      match_info <- str_match(cambio$texto, "Entra (.*?) \\((\\d+)\\) por (.*?) \\((\\d+)\\)")
      if (is.na(match_info[1,1])) return(NULL)
      nombre_entra_raw <- match_info[1, 2]; dorsal_entra <- as.numeric(match_info[1, 3])
      nombre_sale_raw <- match_info[1, 4]; dorsal_sale <- as.numeric(match_info[1, 5])
      
      # Unión más robusta para encontrar IDs
      id_entra <- (alineacion_equipo %>% filter(dorsal == dorsal_entra))$id[1]
      id_sale <- (alineacion_equipo %>% filter(dorsal == dorsal_sale))$id[1]
      
      nombre_entra_lang <- (jugadoras_lang_df %>% filter(id == id_entra))$PlayerName[1] %||% nombre_entra_raw
      nombre_sale_lang <- (jugadoras_lang_df %>% filter(id == id_sale))$PlayerName[1] %||% nombre_sale_raw
      nombre_equipo_lang <- (entidades_df_lang %>% filter(original_name == nombre_equipo_mk))$current_lang_name[1]
      
      # Lógica condicional para jugadoras y equipo
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


#' Crea un enlace para un equipo solo si no es una selección nacional.
#' @param nombre_equipo_mk El nombre original del equipo en macedonio.
#' @param nombre_equipo_lang El nombre del equipo en el idioma actual.
#' @param path_to_root Ruta relativa a la carpeta de equipos (ej. "..").
#' @return Un tag `<a>` de htmltools si es un club, o un `<span>` si es una selección.
crear_enlace_equipo_condicional <- function(nombre_equipo_mk, nombre_equipo_lang, path_to_root = "..") {
  # Usamos la función existente para determinar si es una selección nacional.
  es_seleccion_nacional <- !is.na(get_national_team_iso(nombre_equipo_mk))
  
  if (es_seleccion_nacional) {
    # Si es una selección, devuelve el nombre como texto plano (dentro de un span por consistencia).
    return(tags$span(nombre_equipo_lang))
  } else {
    # Si es un club, crea el hipervínculo a su página de perfil.
    ruta_perfil_equipo <- file.path(path_to_root, nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(nombre_equipo_mk), ".html"))
    return(tags$a(href = ruta_perfil_equipo, nombre_equipo_lang))
  }
}

# ============================================================================ #
# ==           INTERRUPTOR DE PROTECCIÓN POR CONTRASEÑA          == #
# ============================================================================ #
# Cambia a TRUE para generar el sitio web con una contraseña de acceso.
# Cambia a FALSE para generar un sitio web de acceso público y sin contraseña.
PROTEGER_CON_CONTRASENA <- FALSE 
# ============================================================================ #

###  6.2. Gestión de idiomas y traducciones ----

# --- Carga de traducciones desde archivo externo ---
ruta_traducciones <- "translations.txt"
textos <- list()
IDIOMAS_SOPORTADOS <- character(0)

if (!file.exists(ruta_traducciones)) {
  warning("El archivo 'translations.txt' no se encontró. La funcionalidad multiidioma estará limitada.")
} else {
  tryCatch({
    # Leer el archivo CSV asegurando la codificación UTF-8
    traducciones_df <- read.csv(ruta_traducciones, stringsAsFactors = FALSE, encoding = "UTF-8")
    
    # Los idiomas soportados son los nombres de las columnas, excepto la primera ('key')
    IDIOMAS_SOPORTADOS <- names(traducciones_df)[-1]
    
    # Transformar el dataframe de formato ancho a largo
    traducciones_long_df <- traducciones_df %>%
      pivot_longer(
        cols = -key,
        names_to = "lang",
        values_to = "translation"
      )
    
    # Crear la lista anidada `textos` a partir del dataframe largo
    textos <- traducciones_long_df %>%
      split(.$lang) %>%
      map(~ setNames(as.list(.$translation), .$key))
    
    message(paste("Traducciones cargadas exitosamente para los idiomas:", paste(IDIOMAS_SOPORTADOS, collapse = ", ")))
    
  }, error = function(e) {
    warning(paste("Error al leer o procesar el archivo de traducciones:", e$message))
  })
}

# Si no se cargaron idiomas, se establece un fallback mínimo para que el script no falle
if (length(IDIOMAS_SOPORTADOS) == 0) {
  IDIOMAS_SOPORTADOS <- c("mk") # Idioma por defecto
}

# Variable global para el idioma actual y función auxiliar para traducciones.
# Esto se establecerá dentro del bucle de generación principal.
idioma_actual <- IDIOMAS_SOPORTADOS[1]

#' Función auxiliar para obtener texto traducido.
#'
#' Accede al diccionario 'textos' y devuelve la cadena para el idioma actual.
#' Si una clave no existe para un idioma, devuelve la clave misma como fallback.
#'
#' @param key La clave de la cadena de texto a traducir.
#' @return El string traducido.
t <- function(key) {
  traduccion <- textos[[idioma_actual]][[key]]
  if (is.null(traduccion)) {
    # Fallback: intentar obtener del idioma por defecto
    traduccion_fallback <- textos[[IDIOMAS_SOPORTADOS[1]]][[key]]
    if (is.null(traduccion_fallback)) {
      warning(paste("Clave de traducción no encontrada en ningún idioma:", key))
      return(key) # Devolver la clave si no se encuentra en ningún sitio
    }
    return(traduccion_fallback)
  }
  return(traduccion)
}

###  6.3. Definición de rutas y creación de directorios multilingües ----

# --- Definición de nombres de carpetas y archivos base ---
# Se definen los nombres en macedonio para mantener consistencia interna.
# Estos serán los nombres de las carpetas DENTRO de cada directorio de idioma.
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

# --- Creación de la estructura de directorios ---
# Directorio raíz para toda la salida del sitio web.
RUTA_SALIDA_RAIZ <- "docs"
dir.create(RUTA_SALIDA_RAIZ, showWarnings = FALSE, recursive = TRUE)

# Directorio para assets compartidos (CSS, JS, Logos).
RUTA_ASSETS_COMPARTIDOS <- file.path(RUTA_SALIDA_RAIZ, nombres_carpetas_relativos$assets)
RUTA_LOGOS_DESTINO <- file.path(RUTA_ASSETS_COMPARTIDOS, nombres_carpetas_relativos$logos)
dir.create(RUTA_ASSETS_COMPARTIDOS, showWarnings = FALSE, recursive = TRUE)
dir.create(RUTA_LOGOS_DESTINO, showWarnings = FALSE, recursive = TRUE)

# Bucle para crear la estructura de carpetas para cada idioma soportado.
for (lang in IDIOMAS_SOPORTADOS) {
  ruta_base_lang <- file.path(RUTA_SALIDA_RAIZ, lang)
  
  # Se crean las carpetas específicas del idioma (competiciones, partidos, etc.).
  walk(nombres_carpetas_relativos[c("competiciones", "partidos", "jugadoras", "timovi", "arbitros", "estadios")], 
       ~ dir.create(file.path(ruta_base_lang, .x), showWarnings = FALSE, recursive = TRUE))
}
message("Estructura de directorios multilingüe creada en: ", RUTA_SALIDA_RAIZ)

# --- Copia de assets compartidos (Logos) ---
ruta_logos_fuente <- "Logos"
if (dir.exists(ruta_logos_fuente)) {
  archivos_logo_fuente <- list.files(ruta_logos_fuente, pattern = "\\.png$", full.names = TRUE)
  
  if (length(archivos_logo_fuente) > 0) {
    # Se itera sobre cada logo, se sanitiza su nombre y se copia a la carpeta compartida de 'assets'.
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
    
    message(paste(length(archivos_logo_fuente), " escudos copiados a la carpeta de assets compartidos: ", RUTA_LOGOS_DESTINO))
    if (!file.exists(file.path(ruta_logos_fuente, "NOLOGO.png"))) {
      warning("AVISO: No se encontró el escudo placeholder 'NOLOGO.png'.")
    }
  } else {
    warning("La carpeta de logos existe pero no contiene archivos .png.")
  }
} else {
  warning("La carpeta de logos no se encontró. No se copiarán los escudos.")
}





## -------------------------------------------------------------------------- ##
##  8. CARGA DE DATOS EXTERNOS (CONFIGURACIÓN Y MAPEADOS)
## -------------------------------------------------------------------------- ##

message("Започнување со генерирање на HTML извештајот...")

#### 8.1. Cargar estilos de clasificación ----
message("Вчитување на сопствени стилови за табели...")

#' Parsea un archivo de texto para definir estilos de color en las tablas de clasificación.
#' @param ruta_archivo Ruta al archivo de estilos.
#' @return Una lista anidada por competición con las reglas de estilo y la leyenda.
parsear_estilos_clasificacion <- function(ruta_archivo) {
  if (!file.exists(ruta_archivo)) {
    warning(paste("Датотеката за стилови на табели не е пронајдена во:", ruta_archivo))
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
    message("Стиловите за табели се вчитани успешно.")
    return(lista_estilos)
  }, error = function(e) {
    warning("Грешка при вчитување на датотеката за стилови на табели."); message("Грешката е: ", e$message)
    return(list())
  })
}
ruta_estilos_clasificacion <- "estilos_clasificacion.txt"
estilos_clasificacion_data <- parsear_estilos_clasificacion(ruta_estilos_clasificacion)


### 8.2. Cargar mapeo de nacionalidades ----
message("Вчитување на мапирања за националности...")

ruta_mapeo_iso <- "nacionalidades_mapeo.txt"
ruta_traduccion_mk <- "nacionalidades_traduccion.txt"
mapeo_completo_df <- NULL

if (file.exists(ruta_mapeo_iso) && file.exists(ruta_traduccion_mk)) {
  tryCatch({
    mapeo_iso_df <- read.csv(ruta_mapeo_iso, stringsAsFactors = FALSE)
    traduccion_mk_df <- read.csv(ruta_traduccion_mk, stringsAsFactors = FALSE, encoding = "UTF-8")
    mapeo_completo_df <- merge(mapeo_iso_df, traduccion_mk_df, by = "nombre_ingles", all = TRUE)
    mapeo_completo_df$clave_lower <- tolower(trimws(mapeo_completo_df$nombre_ingles))
    message("Мапирањата за националности се вчитани успешно.")
  }, error = function(e) {
    warning("Грешка при вчитување на мапирање на националности. Функционалноста на знамињата ќе биде оневозможена.")
    message("Грешката е: ", e$message)
  })
} else {
  warning("Датотеки за мапирање на националности не се пронајдени. Функционалноста на знамињата ќе биде оневозможена.")
}


### 8.3. Cargar correcciones de nombres ----
message("Вчитување на датотека за корекции на имиња...")

ruta_conversiones <- "conversions.txt"
mapa_conversiones <- NULL
if (file.exists(ruta_conversiones)) {
  tryCatch({
    conversiones_df <- read.csv(ruta_conversiones, stringsAsFactors = FALSE, encoding = "UTF-8")
    mapa_conversiones <- setNames(conversiones_df$corregido, conversiones_df$original)
    message("Датотеката за корекции е вчитана.")
  }, error = function(e) {
    warning("Грешка при вчитување на conversions.txt. Нема да се применат корекции.")
  })
} else {
  message("Датотеката conversions.txt не е пронајдена. Се продолжува без корекции.")
}

### 8.4. Cargar traducciones y correcciones de nombres dinámicas ----
message("Вчитување на динамични преводи/корекции на имиња...")

# Función genérica para cargar un archivo de traducción/corrección y convertirlo a un formato largo
cargar_mapa_traduccion <- function(ruta_archivo) {
  if (!file.exists(ruta_archivo)) {
    message(paste("Датотеката", basename(ruta_archivo), "не е пронајдена."))
    return(NULL)
  }
  tryCatch({
    df <- read.csv(ruta_archivo, stringsAsFactors = FALSE, encoding = "UTF-8", check.names = FALSE)
    
    # 1. Se identifica el nombre de la primera columna (la clave, ej: 'mk').
    # 2. Se pivota usando ese nombre de columna dinámicamente.
    # 3. Se renombra la columna clave a 'original_mk' para consistencia interna.
    key_col_name <- names(df)[1]
    
    df %>%
      pivot_longer(
        cols = -all_of(key_col_name),
        names_to = "lang",
        values_to = "translated_name"
      ) %>%
      rename(original_mk = !!sym(key_col_name)) %>%
      # Se elimina cualquier prefijo innecesario de los códigos de idioma
      mutate(lang = str_remove(lang, "latin_|translation_"))
    
  }, error = function(e) {
    warning(paste("Грешка при вчитување на", basename(ruta_archivo), ":", e$message))
    return(NULL)
  })
}

mapa_nombres_jugadoras_long <- cargar_mapa_traduccion("name_corrections.txt")
mapa_nombres_entidades_long <- cargar_mapa_traduccion("entity_corrections.txt")
mapa_nombres_competiciones_long <- cargar_mapa_traduccion("competition_translations.txt")

### 8.5. Cargar correcciones de nombres de entidades (equipos, árbitros) ----
message("Вчитување на датотека за корекции на имиња на ентитети...")

ruta_correcciones_entidades <- "entity_corrections.txt"
mapa_correcciones_entidades <- NULL
if (file.exists(ruta_correcciones_entidades)) {
  tryCatch({
    correcciones_df <- read.csv(ruta_correcciones_entidades, stringsAsFactors = FALSE, encoding = "UTF-8")
    mapa_correcciones_entidades <- setNames(correcciones_df$latin_sq, correcciones_df$original_mk)
    message("Датотеката за корекции на имиња на ентитети е вчитана.")
  }, error = function(e) {
    warning("Грешка при вчитување на entity_corrections.txt.")
  })
} else {
  message("Датотеката entity_corrections.txt не е пронајдена. Ќе се користи стандардна транслитерација.")
}

### 8.6. Cargar traducciones de competiciones ----
message("Вчитување на преводи за натпреварувања...")

ruta_traducciones_comp <- "competition_translations.txt"
mapa_traducciones_comp <- NULL
if (file.exists(ruta_traducciones_comp)) {
  tryCatch({
    traducciones_comp_df <- read.csv(ruta_traducciones_comp, stringsAsFactors = FALSE, encoding = "UTF-8")
    mapa_traducciones_comp <- setNames(traducciones_comp_df$translation_sq, traducciones_comp_df$original_mk)
    message("Преводите за натпреварувања се вчитани.")
  }, error = function(e) {
    warning("Грешка при вчитување на competition_translations.txt.")
  })
} else {
  message("Датотеката competition_translations.txt не е пронајдена. Ќе се користат оригиналните имиња.")
}

### 8.7. Cargar calendarios desde archivos Excel ----
message("Вчитување на календари од Excel датотеки...")

#' Carga partidos de calendarios futuros desde archivos Excel.
#'
#' Escanea la carpeta 'Calendarios', extrae la competición y temporada del
#' nombre del archivo, lee los partidos, y los formatea en un dataframe
#' compatible con `partidos_df`.
#'
#' @param ruta_carpeta_calendarios Ruta a la carpeta que contiene los Excels.
#' @return Un dataframe con los partidos "placeholder".
cargar_calendarios_excel <- function(ruta_carpeta_calendarios = "Calendarios") {
  if (!dir.exists(ruta_carpeta_calendarios)) {
    message("Директориумот 'Calendarios' не е пронајден. Се прескокнува вчитувањето на идни натпревари.")
    return(tibble())
  }
  
  archivos_excel <- list.files(
    path = ruta_carpeta_calendarios, 
    pattern = "\\.xlsx?$", 
    full.names = TRUE, 
    recursive = TRUE
  )
  
  if (length(archivos_excel) == 0) {
    message("Не се пронајдени Excel датотеки во директориумот 'Calendarios'.")
    return(tibble())
  }
  
  map_dfr(archivos_excel, function(ruta_archivo) {
    nombre_base <- tools::file_path_sans_ext(basename(ruta_archivo))
    
    # Extraer nombre de competición y temporada del nombre del archivo
    match_nombre <- str_match(nombre_base, "^(.*?)\\s+(\\d{2}_\\d{2})$")
    
    if (is.na(match_nombre[1, 1])) {
      warning(paste("Името на датотеката", basename(ruta_archivo), "не е во очекуваниот формат ('Име на натпреварување ГГ_ГГ'). Се прескокнува."))
      return(NULL)
    }
    
    comp_nombre <- str_trim(match_nombre[1, 2])
    comp_temporada <- str_replace(match_nombre[1, 3], "_", "/")
    
    message(paste("   > Вчитување на календар за:", comp_nombre, comp_temporada))
    
    # Leer el archivo Excel
    df_excel <- tryCatch({
      read_excel(ruta_archivo)
    }, error = function(e) {
      warning(paste("Грешка при читање на Excel датотеката:", ruta_archivo, "-", e$message))
      return(NULL)
    })
    
    if (is.null(df_excel) || ncol(df_excel) < 4) return(NULL)
    
    # Asignar nombres estándar a las columnas esperadas
    names(df_excel)[1:4] <- c("jornada", "fecha_hora", "lugar", "partido_raw")
    
    df_excel %>%
      filter(!is.na(partido_raw)) %>%
      mutate(
        # Convertir la jornada a texto para asegurar la consistencia
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


### 8.8. Cargar mapeo de unificación de IDs ----
message("Вчитување на мапирање за унификација на ID...")

ruta_unificacion_id <- "id_unification.txt"
mapa_unificacion_id_df <- NULL
if (file.exists(ruta_unificacion_id)) {
  tryCatch({
    mapa_unificacion_id_df <- read.csv(ruta_unificacion_id, stringsAsFactors = FALSE, encoding = "UTF-8", colClasses = "character")
    message(paste("Датотеката за унификација на ID е вчитана со", nrow(mapa_unificacion_id_df), "правила."))
  }, error = function(e) {
    warning("Грешка при вчитување на id_unification.txt. Нема да се примени унификација на ID.")
  })
} else {
  message("Датотеката id_unification.txt не е пронајдена. Се продолжува без унификација на ID.")
}


# Pegue este bloque al final de la sección 8, por ejemplo, como ### 8.9.

### 8.9. Cargar traducciones de países (NUEVO) ----
message("Вчитување на преводи за имиња на држави...")

ruta_traducciones_paises <- "country_translations.txt"
mapa_traducciones_paises_df <- NULL
if (file.exists(ruta_traducciones_paises)) {
  tryCatch({
    mapa_traducciones_paises_df <- read.csv(
      ruta_traducciones_paises, 
      stringsAsFactors = FALSE, 
      encoding = "UTF-8",
      check.names = FALSE # Importante para manejar columnas como 'translation_es'
    )
    message(paste("Преводите за држави се вчитани успешно со", nrow(mapa_traducciones_paises_df), "записи."))
  }, error = function(e) {
    warning("Грешка при вчитување на country_translations.txt. Имињата на државите може да не се преведат правилно.")
  })
} else {
  message("Датотеката country_translations.txt не е пронајдена. Ќе се користи стандардна транслитерација за имињата на државите.")
}

## -------------------------------------------------------------------------- ##
##  9. PROCESAMIENTO Y TRANSFORMACIÓN DE DATOS PRINCIPALES
## -------------------------------------------------------------------------- ##

### 9.0. Fusión de datos reales con calendarios futuros ----
message("Комбинирање на реални податоци со идни календари...")

# 1. Derivar el dataframe de partidos reales desde el caché
partidos_df_reales <- map_dfr(resultados_exitosos, "partido_info")

# 2. Cargar los partidos "placeholder" desde los archivos Excel
partidos_df_placeholders <- cargar_calendarios_excel()

# 3. Lógica de sustitución: solo mantener placeholders para partidos que aún no se han jugado
if (nrow(partidos_df_placeholders) > 0 && nrow(partidos_df_reales) > 0) {
  # Crear una clave única para identificar cada partido
  partidos_df_reales <- partidos_df_reales %>%
    mutate(match_key = paste(local, visitante, competicion_nombre, competicion_temporada))
  
  partidos_df_placeholders <- partidos_df_placeholders %>%
    mutate(match_key = paste(local, visitante, competicion_nombre, competicion_temporada))
  
  # Filtrar los placeholders para quitar los que ya tienen un acta real
  placeholders_a_mantener <- partidos_df_placeholders %>%
    anti_join(partidos_df_reales, by = "match_key")
  
  # Unificar los dos dataframes
  partidos_df <- bind_rows(
    partidos_df_reales %>% select(-match_key),
    placeholders_a_mantener %>% select(-match_key)
  )
  
} else if (nrow(partidos_df_placeholders) > 0) {
  partidos_df <- partidos_df_placeholders
} else {
  partidos_df <- partidos_df_reales
}

### 9.0.1. Asignar Duración de Partido por Competición ----
message("9.0.1. Асигнирање на времетраење на натпреварот според натпреварување...")
partidos_df <- partidos_df %>%
  mutate(
    duracion_partido = case_when(
      str_detect(tolower(competicion_nombre), "младинска") ~ 80, # Partidos de 80 mins
      str_detect(tolower(competicion_nombre), "кадетска")  ~ 60, # Partidos de 60 mins
      TRUE                                                  ~ 90  # Default para el resto
    )
  )

message("Резиме на времетраење на натпревари:")
print(
  partidos_df %>%
    count(competicion_nombre, duracion_partido) %>%
    as.data.frame()
)

### 9.0.2. Aplicar correcciones iniciales a nombres de equipos ----
message("9.0.2. Примена на првични корекции на имиња на тимови...")
if (!is.null(mapa_conversiones)) {
  # Aplicar correcciones a 'partidos_df' (equipos, estadios) ANTES de cualquier otro procesamiento.
  # Esto asegura que los nombres de los calendarios Excel también se corrijan.
  partidos_df <- aplicar_conversiones(partidos_df, c("local", "visitante"), mapa_conversiones)
}

### 9.1. APLICAR UNIFICACIÓN DE IDs MAESTRA (NUEVO ORDEN) ----
# CAMBIO CLAVE: Este bloque se ejecuta ANTES de cualquier otra corrección de nombres.
if (!is.null(mapa_unificacion_id_df) && nrow(mapa_unificacion_id_df) > 0) {
  message("Aplicando reglas de unificación de ID maestras...")
  
  # Primero reordenamos los nombres canónicos ANTES de crear el mapa.
  mapa_unificacion_id_df$nombre_canonico <- reordenar_nombre_jugadora(mapa_unificacion_id_df$nombre_canonico)
  
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
    # Se renombra 'id_jugadora' a 'id' en los dataframes de eventos para usar una sola función
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
  
  message("Unificación de ID completada.")
}



### 9.1.5. Aplicar correcciones y reordenar nombres ----
message("Примена на корекции и преуредување на имиња...")

if (is.null(attr(resultados_exitosos, "nombres_procesados"))) {
  
  # SE UNIFICAN LAS DOS OPERACIONES EN UN SOLO BUCLE 'MAP' PARA GARANTIZAR LOS CAMBIOS
  resultados_exitosos <- map(resultados_exitosos, function(res) {
    if (is.null(res)) return(NULL)
    
    # --- PASO 1: APLICAR CORRECCIONES DE conversions.txt ---
    if (!is.null(mapa_conversiones)) {
      # Corregir nombres de entidades (equipos, estadios, árbitros)
      res$partido_info <- aplicar_conversiones(res$partido_info, c("local", "visitante"), mapa_conversiones)
      res$estadio <- recode(res$estadio, !!!mapa_conversiones)
      res$arbitro_principal_nombre <- recode(res$arbitro_principal_nombre, !!!mapa_conversiones)
      res$arbitro_asist_1_nombre <- recode(res$arbitro_asist_1_nombre, !!!mapa_conversiones)
      res$arbitro_asist_2_nombre <- recode(res$arbitro_asist_2_nombre, !!!mapa_conversiones)
      
      # Corregir NOMBRES DE JUGADORAS y equipos asociados a eventos
      if (nrow(res$alineacion_local) > 0) res$alineacion_local <- aplicar_conversiones(res$alineacion_local, "nombre", mapa_conversiones)
      if (nrow(res$alineacion_visitante) > 0) res$alineacion_visitante <- aplicar_conversiones(res$alineacion_visitante, "nombre", mapa_conversiones)
      if (nrow(res$goles) > 0) res$goles <- aplicar_conversiones(res$goles, c("jugadora", "equipo_jugadora", "equipo_acreditado"), mapa_conversiones)
      if (nrow(res$tarjetas) > 0) res$tarjetas <- aplicar_conversiones(res$tarjetas, c("jugadora", "equipo"), mapa_conversiones)
      if (!is.null(res$penales) && nrow(res$penales) > 0) {
        res$penales <- aplicar_conversiones(res$penales, c("jugadora", "equipo"), mapa_conversiones)
      }
    }
    
    # --- PASO 2: REORDENAR NOMBRES DE JUGADORAS ---
    # Se aplica DESPUÉS de las correcciones para que el formato sea consistente.
    # Esta lógica se aplica a los nombres que vienen del acta y NO fueron sobreescritos por id_unification.txt
    if (nrow(res$alineacion_local) > 0) res$alineacion_local$nombre <- reordenar_nombre_jugadora(res$alineacion_local$nombre)
    if (nrow(res$alineacion_visitante) > 0) res$alineacion_visitante$nombre <- reordenar_nombre_jugadora(res$alineacion_visitante$nombre)
    if (nrow(res$goles) > 0) res$goles$jugadora <- reordenar_nombre_jugadora(res$goles$jugadora)
    if (nrow(res$tarjetas) > 0) res$tarjetas$jugadora <- reordenar_nombre_jugadora(res$tarjetas$jugadora)
    if (!is.null(res$penales) && nrow(res$penales) > 0) res$penales$jugadora <- reordenar_nombre_jugadora(res$penales$jugadora)
    
    # MUY IMPORTANTE: Devolver el objeto 'res' completamente modificado
    return(res)
  })
  
  # Se añade el atributo al objeto de datos para marcarlo como procesado.
  attr(resultados_exitosos, "nombres_procesados") <- TRUE
  message("Имињата се преуредени и корегирани.")
  
} else {
  message("Корекцијата и преуредувањето на имињата веќе се извршени врз овие податоци. Се прескокнува овој чекор.")
}

### 9.2. Consolidar y unificar datos de jugadoras ----

# Se crea el dataframe RAW sin la información de competición, que se añadirá después.
apariciones_df_raw <- map_dfr(resultados_exitosos, ~bind_rows(
  .x$alineacion_local %>% mutate(id_partido = .x$partido_info$id_partido, equipo = .x$partido_info$local),
  .x$alineacion_visitante %>% mutate(id_partido = .x$partido_info$id_partido, equipo = .x$partido_info$visitante)
)) %>% 
  mutate(nombre = str_squish(nombre)) %>%
  # Se une con el `partidos_df` YA CORREGIDO para obtener los nombres de competición correctos.
  left_join(
    partidos_df %>% select(id_partido, competicion_nombre, competicion_temporada), 
    by = "id_partido"
  )

minutos_df_raw <- map_dfr(resultados_exitosos, function(res) {
  if(is.null(res)) return(NULL)
  id_p <- res$partido_info$id_partido
  
  # Obtenemos la duración correcta para este partido desde el dataframe modificado
  duracion <- (partidos_df %>% filter(id_partido == id_p) %>% pull(duracion_partido))[1]
  # Si por alguna razón no se encuentra, usamos 90 como fallback seguro.
  if (length(duracion) == 0 || is.na(duracion)) { duracion <- 90 }
  
  calcular_minutos_equipo <- function(alineacion, cambios, duracion_partido) {
    if(is.null(alineacion) || nrow(alineacion) == 0) return(NULL)
    # Usamos duracion_partido en lugar de '90'
    jugadoras_con_minutos <- alineacion %>% mutate(min_entra = if_else(tipo == "Titular", 0, NA_real_), min_sale = if_else(tipo == "Titular", duracion_partido, 0))
    if (!is.null(cambios) && nrow(cambios) > 0) {
      cambios_procesados <- cambios %>% mutate(d_entra = as.numeric(str_match(texto, "Entra .*?\\((\\d+)\\)")[, 2]), d_sale  = as.numeric(str_match(texto, "por .*?\\((\\d+)\\)")[, 2])) %>% select(minuto, d_entra, d_sale) %>% filter(!is.na(d_entra) & !is.na(d_sale))
      for (i in 1:nrow(cambios_procesados)) {
        cambio <- cambios_procesados[i, ]; jugadoras_con_minutos <- jugadoras_con_minutos %>% mutate(min_sale = if_else(dorsal == cambio$d_sale, as.numeric(cambio$minuto), min_sale), min_entra = if_else(dorsal == cambio$d_entra, as.numeric(cambio$minuto), min_entra))
      }
    }
    # Usamos duracion_partido aquí también para los suplentes no sustituidos
    jugadoras_con_minutos %>% mutate(min_sale = if_else(!is.na(min_entra) & tipo == "Suplente" & min_sale == 0, duracion_partido, min_sale), minutos_jugados = if_else(is.na(min_entra), 0, min_sale - min_entra)) %>% mutate(minutos_jugados = pmax(0, minutos_jugados))
  }
  
  # Pasamos la duración correcta a la función
  min_local <- calcular_minutos_equipo(res$alineacion_local, res$cambios_local, duracion)
  min_visitante <- calcular_minutos_equipo(res$alineacion_visitante, res$cambios_visitante, duracion)
  
  bind_rows(min_local, min_visitante) %>% mutate(id_partido = id_p)
})

# Lógica crucial para crear un ID canónico (maestro) para cada jugadora,
# resolviendo inconsistencias de IDs o nombres en las actas.
preferred_id_map <- apariciones_df_raw %>% filter(!is.na(nombre), !is.na(id), str_detect(id, "^\\d{5,6}$")) %>% count(nombre, id, name = "frequency") %>% group_by(nombre) %>% filter(frequency == max(frequency)) %>% slice(1) %>% ungroup() %>% select(nombre, canonical_id = id)
id_mapping <- apariciones_df_raw %>% filter(!is.na(nombre) & nchar(trimws(nombre)) > 2) %>% distinct(nombre) %>% left_join(preferred_id_map, by = "nombre") %>% mutate(final_id = if_else(!is.na(canonical_id), as.character(canonical_id), paste0("player_gen_", generar_id_seguro(nombre)))) %>% select(nombre, canonical_id = final_id)

# Se crea el dataframe final de apariciones, uniendo los minutos y el ID canónico.
apariciones_df <- apariciones_df_raw %>% left_join(minutos_df_raw %>% select(id_partido, nombre, dorsal, tipo, min_entra, min_sale, minutos_jugados), by = c("id_partido", "nombre", "dorsal", "tipo")) %>% select(-id) %>% left_join(id_mapping, by = "nombre") %>% rename(id = canonical_id) %>% select(id, id_partido, nombre, dorsal, tipo, equipo, es_portera, es_capitana, competicion_nombre, competicion_temporada, everything())


### 9.3. Unificar datos de eventos (goles y tarjetas) ----

# Se procesan los dataframes de goles y tarjetas para asignar el ID canónico a cada jugadora.
goles_raw_df <- map_dfr(resultados_exitosos, "goles")
if (nrow(goles_raw_df) > 0) { 
  goles_df_unificado <- goles_raw_df %>% 
    mutate(jugadora = str_squish(jugadora)) %>% # Ya reordenada y corregida de 9.1
    left_join(id_mapping, by = c("jugadora" = "nombre")) %>% 
    select(-any_of(c("id", "id_jugadora"))) %>% 
    rename(id = canonical_id)
} else { goles_df_unificado <- tibble(id_partido = character(), jugadora = character(), equipo_jugadora = character(), equipo_acreditado = character(), minuto = integer(), dorsal = integer(), tipo = character(), id = character()) }

tarjetas_raw_df <- map_dfr(resultados_exitosos, "tarjetas")
if(nrow(tarjetas_raw_df) > 0) { 
  tarjetas_df_unificado <- tarjetas_raw_df %>% 
    mutate(jugadora = str_squish(jugadora)) %>% # Ya reordenada y corregida de 9.1
    left_join(id_mapping, by = c("jugadora" = "nombre")) %>% 
    select(-any_of(c("id", "id_jugadora"))) %>% 
    rename(id = canonical_id)
} else { tarjetas_df_unificado <- tibble(jugadora = character(), equipo = character(), dorsal = integer(), minuto = integer(), tipo = character(), motivo = character(), id_partido = character(), id = character()) }

penales_raw_df <- map_dfr(resultados_exitosos, "penales")
if(nrow(penales_raw_df) > 0) {
  ## MODIFICACIÓN: CORRECCIÓN DEL RENAME Y LA LÓGICA DE UNIÓN.
  penales_df_unificado <- penales_raw_df %>%
    mutate(
      jugadora = str_squish(jugadora) # 'jugadora' ya viene reordenada y corregida de la sección 9.1.
    ) %>%
    left_join(id_mapping, by = c("jugadora" = "nombre")) %>%
    select(-any_of(c("id", "id_jugadora"))) %>%
    rename(id = canonical_id) # Esta línea faltaba y causaba el error.
} else { 
  penales_df_unificado <- tibble(jugadora = character(), dorsal = integer(), equipo = character(), resultado_penal = character(), id_partido = character(), id = character()) 
}


### 9.4. Procesar y traducir datos demográficos ----
message("Процесирање и преведување на позиции и демографски податоци на фудбалерки...")

# Diccionario para unificar posiciones a claves neutras.
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

### 9.4.5. Reasignación de partidos de la selección nacional ----
message("9.4.5. Релоцирање на натпревари на репрезентацијата...")

# Se identifican los partidos de la selección y se les asigna una competición y temporada ficticias.
# Esto los agrupará bajo una única entidad virtual.
partidos_df <- partidos_df %>%
  mutate(
    es_partido_seleccion = (local == "Македонија" | visitante == "Македонија"),
    competicion_nombre = if_else(es_partido_seleccion, "Репрезентација", competicion_nombre),
    competicion_temporada = if_else(es_partido_seleccion, "Сите", competicion_temporada)
  )

### 9.5. Identificar y ordenar competiciones ----
message("Идентификување, преведување и подредување на уникатни натпреварувања...")

if (exists("partidos_df") && nrow(partidos_df) > 0) {
  
  # NUEVO: Calcular el año numérico más reciente entre las competiciones reales.
  # Esto se usará para posicionar correctamente "Репрезентација".
  max_real_season_numeric <- partidos_df %>%
    filter(competicion_nombre != "Репрезентација") %>%
    distinct(competicion_temporada) %>%
    mutate(
      start_year = as.integer(str_extract(competicion_temporada, "^\\d{2,4}")),
      # Convertir '23' a '2023' para comparación correcta
      sort_year = if_else(nchar(as.character(start_year)) == 2, 2000 + start_year, start_year)
    ) %>%
    pull(sort_year) %>%
    max(na.rm = TRUE) # Asegurarse de manejar NA si la lista está vacía
  
  # Si no hay temporadas reales (ej. dataframe vacío), establecemos un valor por defecto seguro.
  if (is.infinite(max_real_season_numeric)) max_real_season_numeric <- 2000 
  
  # 1. Se procesan todas las competiciones reales, EXCLUYENDO la de "Репрезентација".
  competiciones_base_df <- partidos_df %>%
    filter(competicion_nombre != "Репрезентација") %>%
    distinct(competicion_nombre, competicion_temporada) %>%
    mutate(
      competicion_id = generar_id_seguro(paste(competicion_nombre, competicion_temporada)),
      nombre_lower = tolower(competicion_nombre),
      # Calcular el año de inicio numérico para la ordenación
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
  
  # 2. Se crea manualmente la entrada para la pseudo-competición "Репрезентација".
  # Asignarle un 'sort_year' intermedio.
  competicion_seleccion_df <- tibble(
    competicion_nombre = "Репрезентација",
    competicion_temporada = "Сите", 
    competicion_id = "reprezentacija", 
    nombre_lower = "репрезентација",
    importancia_score = 0, 
    baraz_modifier = 0,
    final_score = 0,
    # La sitúa numéricamente justo por encima de la temporada más reciente real.
    sort_year = max_real_season_numeric + 0.5 
  )
  
  # 3. Se combinan las competiciones reales con la de la selección.
  # La columna `orden_primario` se crea AQUÍ para asegurar que todas las columnas necesarias existan.
  competiciones_combinadas_df <- bind_rows(competiciones_base_df, competicion_seleccion_df) %>%
    mutate(
      orden_primario = case_when(
        # CORRECCIÓN CLAVE: Usar `sort_year` para la comparación numérica
        sort_year == max_real_season_numeric ~ 1, # Temporada más reciente: la primera.
        competicion_id == "reprezentacija"   ~ 2, # Selección: justo después de la temporada más reciente.
        TRUE                                 ~ 3  # Todas las demás temporadas (más antiguas): las últimas.
      )
    )
  
  # --- Lógica de Traducción Robusta para Competiciones (sin cambios) ---
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
  
  # Modificar la función arrange()
  competiciones_unicas_df <- competiciones_unicas_df %>%
    arrange(orden_primario,              # 1. Prioridad: ¿Temporada actual, Selección, o temporada antigua?
            final_score,                 # 2. Prioridad: Importancia de la competición (Copa, Primera, Segunda...)
            desc(competicion_temporada), # 3. Prioridad: Temporadas más recientes primero (para las de 'orden_primario = 3')
            nombre_completo_mk)          # 4. Prioridad: Desempate alfabético por nombre.
  
} else {
  competiciones_unicas_df <- tibble()
}


### 10.2. Generar datos de árbitros y estadios ----
# Se reestructura la creación de arbitros_df para usar los campos separados y
# se aplica la reordenación de nombres.
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
  mutate(ime = reordenar_nombre_jugadora(ime_raw)) %>%
  select(id_partido, ime, ciudad, uloga)

estadios_df <- map_dfr(resultados_exitosos, ~if(is.null(.x)||is.null(.x$estadio)) NULL else data.frame(id_partido=.x$partido_info$id_partido,estadio=.x$estadio)) %>% left_join(partidos_df,by="id_partido")

### 10.3. Crear dataframe maestro de entidades (equipos, árbitros, estadios) ----
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

# Se corrigen los fallbacks para la generación de nombres de entidades.
for (lang_code in setdiff(IDIOMAS_SOPORTADOS, "mk")) {
  target_col <- paste0("translated_name_", lang_code)
  
  # Se asegura de que la columna de destino exista.
  if (!target_col %in% names(entidades_maestro_df)) {
    entidades_maestro_df[[target_col]] <- NA_character_
  }
  
  entidades_maestro_df <- entidades_maestro_df %>%
    mutate(!!target_col := coalesce(
      # 1. Intenta usar la traducción manual si existe.
      .data[[target_col]],
      # 2. Si no, aplica la transliteración automática como fallback.
      str_replace_all(tolower(original_name), map_transliteration_entity) %>% str_to_title()
    ))
}


### INICIO DE LA MEJORA (VERSIÓN CORREGIDA): Integrar traducciones de países ###
message("10.3.1. Интегрирање на преводи за држави во главниот датафрејм на ентитетите...")

if (!is.null(mapa_traducciones_paises_df) && nrow(mapa_traducciones_paises_df) > 0) {
  
  # Preparar el dataframe de traducciones de países para la unión
  paises_para_unir <- mapa_traducciones_paises_df %>%
    # CORRECCIÓN CLAVE: Se renombra 'original_mk' a 'original_name' para que coincida con la columna de unión del dataframe maestro.
    rename(original_name = original_mk) %>%
    # Ahora se renombran las columnas de traducción para evitar conflictos.
    rename_with(~ paste0("country_", .), .cols = -original_name)
  
  # Unir el dataframe maestro de entidades con las traducciones de países
  # Este left_join ahora funcionará porque ambos dataframes tienen una columna 'original_name'.
  entidades_maestro_df <- entidades_maestro_df %>%
    left_join(paises_para_unir, by = "original_name")
  
  # Bucle para actualizar cada columna de idioma, dando prioridad a la traducción del país
  for (lang_code in setdiff(IDIOMAS_SOPORTADOS, "mk")) {
    col_entidad <- paste0("translated_name_", lang_code)
    col_pais <- paste0("country_translation_", lang_code) # Corregido para que coincida con el prefijo
    
    # Solo proceder si ambas columnas existen
    if (col_entidad %in% names(entidades_maestro_df) && col_pais %in% names(entidades_maestro_df)) {
      entidades_maestro_df <- entidades_maestro_df %>%
        mutate(
          !!col_entidad := coalesce(.data[[col_pais]], .data[[col_entidad]])
        )
    }
  }
  
  # Limpiar las columnas auxiliares de "country_"
  entidades_maestro_df <- entidades_maestro_df %>%
    select(-starts_with("country_"))
  
  message("Преводите за држави се успешно интегрирани.")
}
### FIN DE LA MEJORA (VERSIÓN CORREGIDA) ###

### 9.6. Determinar el alcance de los cambios para la generación incremental ----
message("Проверка на промени за инкрементално генерирање...")

# Cargar la información de cambios guardada por el Script 1
ruta_cache_info <- "cache_info.rds"
if (!file.exists(ruta_cache_info)) {
  stop("Не е пронајдена датотеката со информации за кеш (cache_info.rds). Ве молиме, прво извршете го Скрипт 1.")
}
info_cambios <- readRDS(ruta_cache_info)

# Determinar si hubo cambios en los PDF o si se cargaron calendarios nuevos
hubo_cambios_pdf <- info_cambios$hubo_cambios
hubo_cambios_excel <- exists("partidos_df_placeholders") && nrow(partidos_df_placeholders) > 0
hubo_cambios <- hubo_cambios_pdf || hubo_cambios_excel

# Variable de control para la reconstrucción completa
full_rebuild_needed <- FALSE

# Si no hubo ningún tipo de cambio, se puede omitir la regeneración de archivos.
if (!hubo_cambios) {
  message("Не се пронајдени промени во записниците или календарите. Нема потреба од регенерирање на HTML-датотеките.")
} else {
  # Si se eliminaron archivos PDF, es necesaria una reconstrucción completa para asegurar la consistencia.
  if (length(info_cambios$archivos_eliminados_nombres) > 0) {
    message("Детектирани се избришани записници. Ќе се изврши целосна реконструкција на сајтот.")
    full_rebuild_needed <- TRUE
  } else {
    message("Детектирани се нови или изменети записници/календари. Ќе се изврши инкрементално ажурирање.")
    
    # Inicializar conjuntos de entidades afectadas
    affected_competition_ids <- character(0)
    affected_match_ids <- character(0)
    affected_player_ids <- character(0)
    affected_team_ids <- character(0)
    affected_referee_ids <- character(0)
    affected_stadium_ids <- character(0)
    
    # --- Paso 1: Identificar entidades afectadas por actas PDF nuevas/modificadas ---
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
    
    # --- Paso 2: Añadir competiciones de calendarios Excel a la lista de afectadas ---
    if (exists("partidos_df_placeholders") && nrow(partidos_df_placeholders) > 0) {
      ids_placeholders <- partidos_df_placeholders %>%
        left_join(competiciones_unicas_df, by = c("competicion_nombre", "competicion_temporada")) %>%
        pull(competicion_id)
      
      # Añadir los IDs de las competiciones placeholder al conjunto de afectadas
      affected_competition_ids <- unique(c(affected_competition_ids, na.omit(ids_placeholders)))
    }
    
    # Mensaje final de resumen de cambios
    message(paste("Идентификувани се", length(affected_competition_ids), "натпреварувања,",
                  length(affected_match_ids), "натпревари, и",
                  length(affected_player_ids), "фудбалерки за ажурирање."))
  }
}

## -------------------------------------------------------------------------- ##
##  10. CREACIÓN DE DATASETS AGREGADOS PARA PERFILES Y ESTADÍSTICAS
## -------------------------------------------------------------------------- ##
message("Централизирано пресметување на сите статистики...")

### 10.1. Generar estadísticas globales de jugadoras ----
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
  
  # Se corrigen los fallbacks para la generación de nombres.
  for (lang_code in setdiff(IDIOMAS_SOPORTADOS, "mk")) {
    target_col <- paste0("PlayerName_", lang_code)
    
    # Se asegura de que la columna de destino exista antes de la mutación.
    if (!target_col %in% names(jugadoras_stats_temp)) {
      jugadoras_stats_temp[[target_col]] <- NA_character_
    }
    
    jugadoras_stats_temp <- jugadoras_stats_temp %>%
      mutate(!!target_col := coalesce(
        # 1. Intenta usar la traducción manual si existe.
        .data[[target_col]],
        # 2. Si no, aplica la transliteración automática como fallback.
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




### 10.4. Calcular estadísticas por competición (Clasificación, Goleadoras, Sanciones) ----
stats_clasificacion_por_comp_df <- competiciones_unicas_df %>%
  filter(competicion_id != "reprezentacija", !str_detect(tolower(competicion_nombre), "куп")) %>%
  group_by(competicion_id, competicion_nombre, competicion_temporada) %>%
  reframe({
    grupo_actual <- cur_group()
    partidos_comp_raw <- partidos_df %>% 
      filter(competicion_nombre == grupo_actual$competicion_nombre, 
             competicion_temporada == grupo_actual$competicion_temporada)
    
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

### 10.5. Calcular estadísticas de porteras por competición ----
porteras_apariciones_df <- apariciones_df %>%
  filter(es_portera == TRUE, !is.na(id), minutos_jugados > 0) %>%
  select(id, id_partido, equipo, competicion_nombre, competicion_temporada, min_entra, min_sale, minutos_jugados)

goles_recibidos_df <- goles_df_unificado %>%
  left_join(partidos_df %>% select(id_partido, local, visitante), by = "id_partido") %>%
  mutate(equipo_que_recibio_gol = if_else(equipo_acreditado == local, visitante, local)) %>%
  select(id_partido, equipo_conceded = equipo_que_recibio_gol, minuto_gol = minuto)

# Paso 1: Calcular los Goles Encajados (GA) de forma explícita y robusta
stats_ga <- porteras_apariciones_df %>%
  left_join(goles_recibidos_df, by = c("id_partido", "equipo" = "equipo_conceded"), relationship = "many-to-many") %>%
  filter(!is.na(minuto_gol) & minuto_gol >= min_entra & minuto_gol <= min_sale) %>%
  group_by(id, competicion_nombre, competicion_temporada, TeamName_mk = equipo) %>%
  summarise(GA = n(), .groups = 'drop')

# Paso 2: Calcular las Porterías a Cero (CS) de forma explícita y robusta
stats_cs <- porteras_apariciones_df %>%
  # Unimos con partidos_df para obtener la duración de cada partido
  left_join(partidos_df %>% select(id_partido, duracion_partido, local, visitante, goles_local, goles_visitante), by = "id_partido") %>%
  # Una CS requiere jugar el partido completo (minutos >= duración)
  filter(minutos_jugados >= duracion_partido) %>%
  mutate(goles_recibidos_partido = if_else(local == equipo, goles_visitante, goles_local)) %>%
  filter(goles_recibidos_partido == 0) %>%
  group_by(id, competicion_nombre, competicion_temporada, TeamName_mk = equipo) %>%
  summarise(CS = n(), .groups = 'drop')

# Paso 3: Calcular los minutos totales
stats_minutos <- porteras_apariciones_df %>%
  group_by(id, competicion_nombre, competicion_temporada, TeamName_mk = equipo) %>%
  summarise(Minutes = sum(minutos_jugados, na.rm = TRUE), .groups = 'drop')

# Paso 4: Unir todas las estadísticas en un dataframe final
stats_porteras_por_comp_df <- stats_minutos %>%
  full_join(stats_ga, by = c("id", "competicion_nombre", "competicion_temporada", "TeamName_mk")) %>%
  full_join(stats_cs, by = c("id", "competicion_nombre", "competicion_temporada", "TeamName_mk")) %>%
  mutate(across(c(GA, CS), ~replace_na(., 0))) %>%
  mutate(GA90 = if_else(Minutes > 0, (GA / Minutes) * 90, 0)) %>%
  left_join(competiciones_unicas_df %>% filter(competicion_id != "reprezentacija") %>% select(competicion_id, competicion_nombre, competicion_temporada), by = c("competicion_nombre", "competicion_temporada"))

### 10.6. Calcular estadísticas de tríos defensivos por competición ----
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

# Paso 1: Calcular los minutos totales que cada trío jugó junto
stats_minutes_trios <- trio_minutos_partido_df %>%
  group_by(trio_key, competicion_nombre, competicion_temporada, TeamName_mk = equipo) %>%
  summarise(MinutesTogether = sum(minutos_compartidos, na.rm = TRUE), .groups = 'drop')

# Paso 2: Calcular los goles encajados por cada trío mientras jugaban juntos
stats_ga_trios <- trio_minutos_partido_df %>%
  left_join(goles_recibidos_df, by = c("id_partido", "equipo" = "equipo_conceded"), relationship = "many-to-many") %>%
  filter(!is.na(minuto_gol) & minuto_gol >= start_shared & minuto_gol <= end_shared) %>%
  group_by(trio_key, competicion_nombre, competicion_temporada, TeamName_mk = equipo) %>%
  summarise(GA_Together = n(), .groups = 'drop')

# Paso 3: Unir las estadísticas usando left_join para prevenir NAs en trio_key
# Se parte de los tríos que tienen minutos y se les añaden los goles (si los hay).
stats_trios_defensivos_df <- stats_minutes_trios %>%
  left_join(stats_ga_trios, by = c("trio_key", "competicion_nombre", "competicion_temporada", "TeamName_mk")) %>%
  filter(!is.na(trio_key)) %>% # Filtro de seguridad extra, aunque left_join debería prevenirlo
  mutate(GA_Together = replace_na(GA_Together, 0)) %>%
  mutate(GA90_Together = if_else(MinutesTogether > 0, (GA_Together / MinutesTogether) * 90, 0)) %>%
  left_join(competiciones_unicas_df %>% filter(competicion_id != "reprezentacija") %>% select(competicion_id, competicion_nombre, competicion_temporada), by = c("competicion_nombre", "competicion_temporada"))

### 10.6.5. Calcular resúmenes de carrera para la Selección Nacional por jugadora ----
message("10.6.5. Пресметување резиме на кариера за репрезентацијата по фудбалерка...")

national_team_career_summary_df <- apariciones_df %>%
  filter(!is.na(id)) %>%
  # Unir con partidos_df para identificar partidos de la selección
  left_join(partidos_df %>% select(id_partido, es_partido_seleccion, local, visitante), by = "id_partido") %>%
  # Filtrar por partidos de la selección donde la jugadora es de Macedonia
  filter(es_partido_seleccion == TRUE, equipo == "Македонија") %>%
  # Agrupar por jugadora, asignando valores fijos para la pseudocompetición
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
  # Unir con goles de la selección para esta jugadora
  left_join(
    goles_df_unificado %>%
      filter(!is.na(id), tipo == "Normal") %>%
      left_join(partidos_df %>% select(id_partido, es_partido_seleccion), by = "id_partido") %>%
      filter(es_partido_seleccion == TRUE, equipo_jugadora == "Македонија") %>%
      group_by(id) %>%
      summarise(Goals = n(), .groups = 'drop'),
    by = "id"
  ) %>%
  # Unir con tarjetas de la selección para esta jugadora
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

# Opcional: imprimir un resumen para verificar
message("   > Resumen de carreras de la selección nacional procesado. Filas: ", nrow(national_team_career_summary_df))


### 10.7. Calcular resúmenes de carrera por jugadora ----
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

### 10.8. Calcular resúmenes de perfiles de equipos ----
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

### 10.9. Calcular resúmenes de perfiles de árbitros ----
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

### 10.9.5. Identificación de entidades a excluir de la generación de páginas individuales. ----
message("10.9.5. Идентификување на ентитети за исклучување од генерирање поединечни страници...")

# Obtener IDs de partidos de la selección nacional
ids_partidos_seleccion <- partidos_df %>%
  filter(es_partido_seleccion == TRUE) %>%
  pull(id_partido) %>%
  unique()

### INICIO DE LA MODIFICACIÓN ###
# Equipos a excluir de la generación de páginas individuales:
# Son todas las selecciones que han jugado contra Macedonia.
equipos_en_partidos_seleccion <- c(
  (partidos_df %>% filter(id_partido %in% ids_partidos_seleccion) %>% pull(local)),
  (partidos_df %>% filter(id_partido %in% ids_partidos_seleccion) %>% pull(visitante))
)
team_names_to_skip_mk <- unique(equipos_en_partidos_seleccion[equipos_en_partidos_seleccion != "Македонија"])
### FIN DE LA MODIFICACIÓN ###

# Jugadoras a excluir de la generación de páginas individuales:
# Aparecen en un partido de la selección Y no juegan en el equipo "Македонија" en ese partido.
player_ids_to_skip <- apariciones_df %>%
  filter(id_partido %in% ids_partidos_seleccion, # La jugadora jugó en un partido de la selección nacional
         equipo != "Македонија") %>%             # Y su equipo NO era "Македонија"
  pull(id) %>%
  unique()

# Árbitros que participaron en partidos de la selección
# Se excluyen si arbitraron un partido de la selección.
referee_ids_to_skip <- arbitros_df %>%
  filter(id_partido %in% ids_partidos_seleccion) %>%
  pull(ime) %>%
  unique() %>%
  generar_id_seguro() # Convertir a ID seguro para la comparación en el loop

# Estadios donde se jugaron partidos de la selección
# Se excluyen si fueron sede de un partido de la selección.
stadium_ids_to_skip <- estadios_df %>%
  filter(id_partido %in% ids_partidos_seleccion) %>%
  pull(estadio) %>%
  unique() %>%
  na.omit() %>% # Asegurarse de que no haya NA en los nombres de estadios
  generar_id_seguro() # Convertir a ID seguro para la comparación en el loop

message(paste("   >", length(team_names_to_skip_mk), "selecciones nacionales extranjeras serán excluidas de los perfiles de equipo."))
message(paste("   >", length(player_ids_to_skip), "jugadoras no-macedonias en partidos de la selección serán excluidas de los perfiles individuales."))
message(paste("   >", length(referee_ids_to_skip), "árbitros de partidos de la selección serán excluidos de los perfiles individuales."))
message(paste("   >", length(stadium_ids_to_skip), "estadios de partidos de la selección serán excluidos de los perfiles individuales."))

## -------------------------------------------------------------------------- ##
##  12. EXTERNALIZACIÓN DE ASSETS (CSS Y JAVASCRIPT)
## -------------------------------------------------------------------------- ##

### 12.1. Guardar hoja de estilos (style.css) ----

# Se define todo el CSS como un string y se escribe en un archivo externo.
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
/* --- INICIO DEL BLOQUE CORREGIDO --- */
/* Estilo común para los contenedores de equipo en ambos tipos de enlaces (real y placeholder) */
.partido-link span.equipo, .partido-link-placeholder span.equipo { 
  flex: 1 1 40%; /* Ocupa el 40% del espacio, permitiendo encoger/crecer */
  display: flex; 
  align-items: center; 
  font-weight: bold; 
}
/* Estilo para el equipo local, alineado a la derecha */
.partido-link span.equipo-local, .partido-link-placeholder span.equipo-local { 
  justify-content: flex-end; /* Alinea contenido (logo+texto) a la derecha */
}
/* Estilo para el equipo visitante, alineado a la izquierda */
.partido-link span.equipo-visitante, .partido-link-placeholder span.equipo-visitante { 
  justify-content: flex-start; /* Alinea contenido (logo+texto) a la izquierda */
}
/* Estilo para el resultado, ocupa el 12% central fijo */
.partido-link span.resultado, .partido-link-placeholder span.resultado { 
  flex: 0 0 12%; /* No crece, no encoge, base del 12% */
  font-size: 1.2em; 
  font-weight: bold; 
  text-align: center; 
}
/* --- FIN DEL BLOQUE CORREGIDO --- */
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
.team-logo.national-team-flag {
  border-radius: 50%;
  border: 1px solid #ccc; /* Un borde sutil para las banderas */
}
.team-cell { display: flex; align-items: center; }
.team-cell .team-logo { margin-right: 12px; }
.partido-link .team-logo { height: 28px; width: 28px; }
.partido-link .equipo-local .team-logo { margin-right: 10px; }
.partido-link .equipo-visitante .team-logo { margin-left: 10px; }
.alineacion-header { text-align: center; margin-bottom: 20px; }
.alineacion-header .match-page-crest, .team-page-crest {
  width: 120px;
  height: 120px;
  object-fit: contain;
  margin: 10px auto;
  display: block;
}
.alineacion-header h3 {
  border-bottom: none;
  padding-bottom: 0;
  margin-bottom: 5px;
}
.alineacion-header h3 a { color: #8B0000; }
)"

writeLines(estilo_css, file.path(RUTA_ASSETS_COMPARTIDOS, "style.css"))


### 12.2. Guardar script de funcionalidades (script.js) ----
script_js <- r"(
let searchData = [];

document.addEventListener('DOMContentLoaded', initializeSearch);

// Se hace la lógica de rutas más robusta para que funcione
// tanto en el servidor local de `servr` como en GitHub Pages.
function getSiteBasePath() {
  const path = window.location.pathname;
  // Encuentra la parte de la ruta antes de la primera carpeta de idioma.
  // Ej: de "/repo/mk/page.html" extrae "/repo/"
  const match = path.match(/^(.*\/)(mk|sq|es|en)\//);
  if (match && match[1]) {
    return match[1];
  }
  // Fallback para la raíz (ej. `servr` en local)
  return "/";
}

function getCurrentLanguageFromPath() {
  const path = window.location.pathname;
  // Busca el código de idioma de 2 letras en la ruta.
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

// ... (El resto del JS, handleSearchInput, showSearchResults, etc., permanece igual)
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
message("Archivos style.css y script.js guardados en la carpeta assets.")


## -------------------------------------------------------------------------- ##
##  13. GENERACIÓN DE PÁGINAS HTML (ARQUITECTURA OPTIMIZADA)
## -------------------------------------------------------------------------- ##

if (hubo_cambios) {
  
  # ============================================================================ #
  # ==  BUCLE PRINCIPAL DE GENERACIÓN: Itera sobre cada idioma y genera el sitio==
  # ============================================================================ #
  
  for (lang in IDIOMAS_SOPORTADOS) {
    
    idioma_actual <<- lang
    message(paste("\n--- Generando páginas para el idioma:", toupper(lang), "---"))
    
    # --- 1. Preparación de datos y scripts específicos del idioma ---
    message("   > Preparando datos y scripts para '", lang, "'...")
    
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
    message("     > Índice de búsqueda guardado en: ", basename(ruta_json_salida))
    
    if (PROTEGER_CON_CONTRASENA) {
      la_contrasena <- "secreto123"
      message("     > Protección por contraseña ACTIVADA.")
      script_contraseña_lang <- tags$script(HTML(
        sprintf(
          "(function() { var p = '%s'; var s = sessionStorage; var d = document; if (s.getItem('zfudbalmk-password-ok') === p) return; var i; var m = '%s'; while (true) { i = prompt(m, ''); if (i === p) { s.setItem('zfudbalmk-password-ok', i); break; } if (i === null) { d.body.innerHTML = '<div style=\"text-align:center; padding: 50px; font-family: sans-serif;\"><h1>%s</h1><p>%s</p></div>'; throw new Error('Access denied'); } m = '%s'; } })();",
          la_contrasena, t("password_prompt"), t("access_denied_header"), t("access_denied_body"), t("password_wrong")
        )
      ))
    } else {
      message("     > Protección por contraseña DESACTIVADA.")
      script_contraseña_lang <- NULL
    }
    
    # --- 2. Página de inicio (portal) ---
    message("   > Generando index.html...")
    contenido_portal <- tags$div(
      id = "portal", tags$h2(t("portal_title")),
      tags$div(class = "portal-container",
               if (nrow(competiciones_unicas_df) > 0) {
                 map(1:nrow(competiciones_unicas_df), function(i) {
                   comp <- competiciones_unicas_df[i,]; comp_name <- comp[[comp_name_col]]
                   tags$a(href = file.path(nombres_carpetas_relativos$competiciones, paste0(comp$competicion_id, ".html")), class = "portal-button", comp_name)
                 })
               } else { tags$p(t("no_competitions_found")) }
      )
    )
    pagina_portal_final <- crear_pagina_html(
      contenido_principal = contenido_portal, titulo_pagina = t("site_title"), path_to_root_dir = "..",
      script_contraseña = script_contraseña_lang
    )
    save_html(pagina_portal_final, file = file.path(RUTA_SALIDA_RAIZ, lang, "index.html"))
    
    # --- 3. Páginas de competiciones ---
    message("   > Generando páginas de competiciones...")
    walk(1:nrow(competiciones_unicas_df), function(i) {
      comp_info <- competiciones_unicas_df[i,]; comp_id <- comp_info$competicion_id
      
      # Si el id es "reprezentacija", se usa la nueva lógica.
      if (comp_id == "reprezentacija") {
        
        # LÓGICA PARA LA PÁGINA DE LA SELECCIÓN NACIONAL
        if (!full_rebuild_needed && !(comp_id %in% affected_competition_ids)) { return() }
        
        comp_nombre_current_lang <- comp_info[[comp_name_col]]
        
        # 1. Encontrar todas las categorías únicas para los partidos de la selección.
        categorias_seleccion <- partidos_df %>%
          filter(es_partido_seleccion == TRUE) %>%
          distinct(categoria) %>%
          filter(!is.na(categoria)) %>%
          arrange(categoria) %>%
          pull(categoria)
        
        # 2. Crear los botones del menú, uno por cada categoría.
        lista_botones_menu_seleccion <- map(categorias_seleccion, function(cat) {
          # El nombre del archivo HTML se basa en la categoría sanitizada.
          nombre_archivo_cat <- paste0(comp_id, "_", generar_id_seguro(cat), ".html")
          tags$a(href = nombre_archivo_cat, class = "menu-button", cat)
        })
        
        # 3. Generar la página principal (menú) para "Репрезентација".
        contenido_menu_seleccion <- tagList(
          crear_botones_navegacion(path_to_lang_root = ".."),
          tags$h2(comp_nombre_current_lang),
          tags$div(class = "menu-container", lista_botones_menu_seleccion)
        )
        save_html(
          crear_pagina_html(contenido_menu_seleccion, comp_nombre_current_lang, "../..", script_contraseña_lang),
          file = file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, paste0(comp_id, ".html"))
        )
        
        # 4. Generar una página para CADA categoría.
        walk(categorias_seleccion, function(cat_actual) {
          
          # Filtrar partidos de la selección para esta categoría y ordenarlos.
          partidos_categoria <- partidos_df %>%
            filter(es_partido_seleccion == TRUE, categoria == cat_actual) %>%
            mutate(fecha_parsed = as.Date(fecha, format = "%d.%m.%Y")) %>%
            arrange(desc(fecha_parsed))
          
          # Función para crear el logo de un equipo.
          get_logo_tag <- function(nombre_equipo_mk) { 
            iso_code <- get_national_team_iso(nombre_equipo_mk)
            if (!is.na(iso_code)) {
              # Es una selección nacional, usar URL de bandera
              flag_url <- paste0("https://hatscripts.github.io/circle-flags/flags/", iso_code, ".svg")
              tags$img(class="team-logo national-team-flag", src = flag_url, alt = nombre_equipo_mk)
            } else {
              # Es un equipo de club, usar ruta de logo local
              nombre_archivo_final <- paste0(generar_id_seguro(nombre_equipo_mk), ".png")
              if (!file.exists(file.path(RUTA_LOGOS_DESTINO, nombre_archivo_final))) { nombre_archivo_final <- "NOLOGO.png" }
              ruta_relativa_logo_html <- file.path("..", "..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, nombre_archivo_final)
              tags$img(class="team-logo", src = ruta_relativa_logo_html, alt = nombre_equipo_mk) 
            }
          }
          
          # Generar la lista de partidos (sin agrupación por jornada).
          contenido_lista_partidos <- tagList(
            crear_botones_navegacion(path_to_lang_root = ".."), 
            tags$h2(paste(comp_nombre_current_lang, "-", cat_actual)),
            
            ### INICIO DE LA MODIFICACIÓN ###
            map(1:nrow(partidos_categoria), function(k) {
              partido <- partidos_categoria[k,]; is_placeholder_match <- is.na(partido$id_partido)
              local_name <- entidades_df_lang$current_lang_name[match(partido$local, entidades_df_lang$original_name)]
              visitante_name <- entidades_df_lang$current_lang_name[match(partido$visitante, entidades_df_lang$original_name)]
              resultado_texto <- if (is_placeholder_match) " - " else { res_base <- paste(partido$goles_local, "-", partido$goles_visitante); if (!is.na(partido$penales_local)) res_base <- sprintf("%s (%s - %s)", res_base, partido$penales_local, partido$penales_visitante); if (isTRUE(partido$es_resultado_oficial)) res_base <- paste(res_base, "*"); res_base }
              
              # El contenido visual del partido (equipos y resultado)
              contenido_comun <- tagList(
                tags$span(class="equipo equipo-local", get_logo_tag(partido$local), tags$span(local_name)), 
                tags$span(class="resultado", resultado_texto), 
                tags$span(class="equipo equipo-visitante", tags$span(visitante_name), get_logo_tag(partido$visitante))
              )
              
              # Se envuelve todo en un tagList para añadir la fecha encima del enlace del partido.
              tagList(
                # Se añade la fecha del partido aquí, usando la columna 'fecha' del objeto 'partido'.
                tags$p(
                  style = "text-align: center; margin-bottom: 2px; margin-top: 15px; font-size: 0.9em; color: #555;", 
                  partido$fecha
                ),
                # El bloque if/else original para crear el enlace o el placeholder.
                if (is_placeholder_match) {
                  tags$div(class = "partido-link-placeholder", contenido_comun)
                } else {
                  tags$a(class = "partido-link", href = file.path("..", nombres_carpetas_relativos$partidos, paste0(partido$id_partido, ".html")), contenido_comun)
                }
              )
            })
          )
          
          # Guardar el archivo HTML para la categoría.
          nombre_archivo_cat_final <- paste0(comp_id, "_", generar_id_seguro(cat_actual), ".html")
          titulo_pagina_cat <- paste(t("category_page_title"), "-", comp_nombre_current_lang, "-", cat_actual)
          save_html(
            crear_pagina_html(contenido_lista_partidos, titulo_pagina_cat, "../..", script_contraseña_lang),
            file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, nombre_archivo_cat_final)
          )
        })
        
      } else {
        
        # LÓGICA ORIGINAL PARA LAS COMPETICIONES NORMALES
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
              str_detect(jornada, "3/4") ~ 6.5, # <-- LÍNEA AÑADIDA: Reconoce el partido por el 3er puesto
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
          minutos_totales_equipo_comp <- partidos_df %>% filter(competicion_nombre == comp_info$competicion_nombre, competicion_temporada == comp_info$competicion_temporada) %>% select(local, visitante, duracion_partido) %>% pivot_longer(cols = c(local, visitante), names_to = "tipo_equipo", values_to = "equipo") %>% group_by(equipo) %>% summarise(minutos_totales_posibles = sum(duracion_partido, na.rm = TRUE), .groups = 'drop'); tabla_porteras_comp_raw <- stats_porteras_por_comp_df %>% filter(competicion_id == comp_id) %>% left_join(jugadoras_stats_df %>% select(id, !!player_name_col_sym), by = "id") %>% left_join(entidades_df_lang, by = c("TeamName_mk" = "original_name")) %>% left_join(minutos_totales_equipo_comp, by = c("TeamName_mk" = "equipo")) %>% mutate(pct_minutos = if_else(!is.na(minutos_totales_posibles) & minutos_totales_posibles > 0, (Minutes / minutos_totales_posibles) * 100, 0), group = if_else(pct_minutos >= 50, "mas_50", "menos_50")) %>% select(id, PlayerName = !!player_name_col_sym, TeamName = current_lang_name, TeamName_mk, GA90, GA, Minutes, CS, group)
          if (nrow(tabla_porteras_comp_raw) > 0) { porteras_mas_50 <- tabla_porteras_comp_raw %>% filter(group == "mas_50") %>% arrange(GA90, desc(CS)) %>% mutate(Pos = row_number()); porteras_menos_50 <- tabla_porteras_comp_raw %>% filter(group == "menos_50", Minutes > 0) %>% arrange(GA90, desc(CS)) %>% mutate(Pos = row_number()); generar_tabla_html_porteras <- function(df, table_id) { if (is.null(df) || nrow(df) == 0) { return(tags$p(t("no_data_in_category")))}; tags$table(id = table_id, `data-sort-col` = "3", `data-sort-dir` = "asc", tags$thead(tags$tr(tags$th(t("standings_pos")), tags$th(t("player_type")), tags$th(t("team_type")), tags$th(class="sortable-header asc", onclick=sprintf("sortTable('%s', 3)", table_id), t("gk_ga_90")), tags$th(t("gk_ga")), tags$th(t("stats_minutes")), tags$th(class="sortable-header", onclick=sprintf("sortTable('%s', 6)", table_id), t("gk_cs")))), tags$tbody(map(1:nrow(df), function(j){ p <- df[j,]; nombre_equipo <- p$TeamName; nombre_equipo_mk <- p$TeamName_mk; nombre_archivo_final <- paste0(generar_id_seguro(nombre_equipo_mk), ".png"); if (!file.exists(file.path(RUTA_LOGOS_DESTINO, nombre_archivo_final))) { nombre_archivo_final <- "NOLOGO.png" }; ruta_relativa_logo_html <- file.path("..", "..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, nombre_archivo_final); tags$tr(tags$td(p$Pos), tags$td(tags$a(href=file.path("..", nombres_carpetas_relativos$jugadoras, paste0(p$id, ".html")), p$PlayerName)), tags$td(class = "team-cell", tags$img(class="team-logo", src = ruta_relativa_logo_html, alt = nombre_equipo), tags$a(href=file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(nombre_equipo_mk), ".html")), nombre_equipo)), tags$td(format(round(p$GA90, 2), nsmall = 2)), tags$td(p$GA), tags$td(p$Minutes), tags$td(p$CS)) })))}; contenido_porteras <- tagList(crear_botones_navegacion(path_to_lang_root = ".."), tags$h2(paste(t("goalkeepers_title"), "-", comp_nombre_current_lang)), tags$h3(t("gk_stats_header_over_50")), generar_tabla_html_porteras(porteras_mas_50, "tabla-porteras-mas-50"), tags$h3(t("gk_stats_header_under_50")), generar_tabla_html_porteras(porteras_menos_50, "tabla-porteras-menos-50")); nombre_archivo_porteras <- paste0(comp_id, "_golmanki.html"); save_html(crear_pagina_html(contenido_porteras, paste(t("goalkeepers_title"), "-", comp_nombre_current_lang), "../..", script_contraseña_lang), file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, nombre_archivo_porteras)); lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href=nombre_archivo_porteras, class="menu-button", t("goalkeepers_title")) }
          if (str_detect(comp_info$competicion_nombre, "Прва|Втора")) {
            partidos_en_comp <- partidos_df %>% filter(competicion_nombre == comp_info$competicion_nombre, competicion_temporada == comp_info$competicion_temporada); minutos_totales_equipo_comp <- bind_rows(partidos_en_comp %>% count(TeamName_mk = local), partidos_en_comp %>% count(TeamName_mk = visitante)) %>% group_by(TeamName_mk) %>% summarise(P = sum(n), .groups = 'drop') %>% mutate(minutos_totales_posibles = P * 90) %>% select(TeamName_mk, minutos_totales_posibles)
            tabla_final_defensas <- stats_trios_defensivos_df %>% filter(competicion_id == comp_id) %>% left_join(minutos_totales_equipo_comp, by = "TeamName_mk") %>% filter(!is.na(minutos_totales_posibles), MinutesTogether >= (minutos_totales_posibles * 0.5)) %>% group_by(TeamName_mk) %>% arrange(GA90_Together, GA_Together, desc(MinutesTogether)) %>% slice_head(n = 1) %>% ungroup() %>% left_join(entidades_df_lang, by = c("TeamName_mk" = "original_name")) %>% mutate(TeamName = current_lang_name) %>% filter(!is.na(trio_key)) %>% rowwise() %>% mutate(TrioNames = { ids_del_trio <- strsplit(trio_key, "-")[[1]]; nombres_encontrados <- jugadoras_stats_df %>% filter(id %in% ids_del_trio) %>% select(id, PlayerName = !!player_name_col_sym); nombres_ordenados <- sapply(ids_del_trio, function(id_actual) { nombre <- (nombres_encontrados %>% filter(id == id_actual) %>% pull(PlayerName))[1]; if (is.na(nombre)) id_actual else nombre }); paste(nombres_ordenados, collapse = " - ") }) %>% ungroup() %>% arrange(GA90_Together, GA_Together, desc(MinutesTogether)) %>% mutate(Pos = row_number()) %>% select(Pos, TrioNames, TeamName, TeamName_mk, MinutesTogether, GA_Together, GA90_Together, trio_key)
            if (nrow(tabla_final_defensas) > 0) { contenido_defensas <- tagList(crear_botones_navegacion(path_to_lang_root = ".."), tags$h2(paste(t("defensive_trio_title"), "-", comp_nombre_current_lang)), tags$p(style="text-align:center; font-style:italic; color:#555;", t("defensive_trio_subtitle")), tags$table(class = "main-summary-table", tags$thead(tags$tr(tags$th(t("standings_pos")), tags$th(t("defensive_trio_header_trio")), tags$th(t("team_type")), tags$th(t("defensive_trio_header_minutes")), tags$th(t("defensive_trio_header_ga")), tags$th(t("defensive_trio_header_ga90")))), tags$tbody(pmap(tabla_final_defensas, function(...) { fila <- list(...); nombre_equipo <- fila$TeamName; nombre_equipo_mk <- fila$TeamName_mk; nombre_archivo_final <- paste0(generar_id_seguro(nombre_equipo_mk), ".png"); if (!file.exists(file.path(RUTA_LOGOS_DESTINO, nombre_archivo_final))) { nombre_archivo_final <- "NOLOGO.png" }; ruta_relativa_logo_html <- file.path("..", "..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, nombre_archivo_final); tags$tr(tags$td(fila$Pos), tags$td(fila$TrioNames), tags$td(class="team-cell", tags$img(class="team-logo", src = ruta_relativa_logo_html, alt = nombre_equipo), tags$a(href=file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(nombre_equipo_mk), ".html")), onclick="event.stopPropagation();", nombre_equipo)), tags$td(round(fila$MinutesTogether)), tags$td(fila$GA_Together), tags$td(format(round(fila$GA90_Together, 2), nsmall = 2))) }))))
            nombre_archivo_defensas <- paste0(comp_id, "_defanzivno_trio.html"); save_html(crear_pagina_html(contenido_defensas, paste(t("defensive_trio_title"), "-", comp_nombre_current_lang), "../..", script_contraseña_lang), file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, nombre_archivo_defensas)); lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href=nombre_archivo_defensas, class="menu-button", t("defensive_trio_title")) }
          }
          tabla_sanciones_comp <- stats_sanciones_por_comp_df %>% filter(competicion_id == comp_id) %>% left_join(jugadoras_stats_df %>% select(id, !!player_name_col_sym), by = "id") %>% filter(!is.na(!!player_name_col_sym)) %>% select(Pos, id, PlayerName = !!player_name_col_sym, TeamNames_mk, YellowCards, RedCards); contenido_sanciones <- tagList(crear_botones_navegacion(path_to_lang_root = ".."), tags$h2(paste(t("disciplinary_title"), "-", comp_nombre_current_lang)), tags$table(tags$thead(tags$tr(tags$th(t("standings_pos")), tags$th(t("player_type")), tags$th(t("team_type")), tags$th(HTML("<span class='card-yellow'></span>")), tags$th(HTML("<span class='card-red'></span>")))), tags$tbody(if(nrow(tabla_sanciones_comp) > 0) { map(1:nrow(tabla_sanciones_comp), function(j) { s <- tabla_sanciones_comp[j,]; tags$tr(tags$td(s$Pos), tags$td(tags$a(href=file.path("..", nombres_carpetas_relativos$jugadoras, paste0(s$id, ".html")), s$PlayerName)), tags$td({ teams_mk <- str_split(s$TeamNames_mk, " / ")[[1]]; team_tags <- list(); for (i in seq_along(teams_mk)) { team_name_mk <- teams_mk[i]; team_name <- entidades_df_lang %>% filter(original_name == team_name_mk) %>% pull(current_lang_name); nombre_archivo_final <- paste0(generar_id_seguro(team_name_mk), ".png"); if (!file.exists(file.path(RUTA_LOGOS_DESTINO, nombre_archivo_final))) { nombre_archivo_final <- "NOLOGO.png" }; ruta_relativa_logo_html <- file.path("..", "..", nombres_carpetas_relativos$assets, nombres_carpetas_relativos$logos, nombre_archivo_final); team_element <- tags$span(class="team-cell", tags$img(class="team-logo", src = ruta_relativa_logo_html, alt = team_name), tags$a(href = file.path("..", nombres_carpetas_relativos$timovi, paste0(generar_id_seguro(team_name_mk), ".html")), team_name)); team_tags <- append(team_tags, list(team_element)); if (i < length(teams_mk)) { team_tags <- append(team_tags, list(tags$span(style="margin: 0 5px;", "/"))) }}; tagList(team_tags) }), tags$td(s$YellowCards), tags$td(s$RedCards)) })} else { tags$tr(tags$td(colspan="5", t("disciplinary_no_cards_message"))) })))
          nombre_archivo_sanciones <- paste0(comp_id, "_", nombres_archivos_mk$sanciones, ".html"); save_html(crear_pagina_html(contenido_sanciones, paste(t("disciplinary_title"), "-", comp_nombre_current_lang), "../..", script_contraseña_lang), file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, nombre_archivo_sanciones)); lista_botones_menu[[length(lista_botones_menu) + 1]] <- tags$a(href=nombre_archivo_sanciones, class="menu-button", t("disciplinary_title"))
        }
        contenido_menu_final <- tagList(crear_botones_navegacion(path_to_lang_root = ".."), tags$h2(comp_nombre_current_lang), tags$div(class="menu-container", lista_botones_menu))
        save_html(crear_pagina_html(contenido_menu_final, comp_nombre_current_lang, "../..", script_contraseña_lang), file = file.path(RUTA_SALIDA_RAIZ, lang, nombres_carpetas_relativos$competiciones, paste0(comp_id, ".html")))
      }
    })
    
    # --- 4. Páginas de perfiles individuales ---
    message("   > Generando perfiles individuales (partidos, jugadoras, etc.)...")
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
        # Si es partido de la selección, usar la categoría del partido.
        partido_info$categoria 
      } else if(is_cup_match) { # Si es partido de copa
        partido_info$jornada
      } else { # Para ligas normales
        paste(t("round_prefix"), partido_info$jornada)
      }
      nota_arbitro <- resumen_partido$nota_arbitro; if (!is.na(nota_arbitro)) { nota_arbitro <- str_remove(nota_arbitro, "^[\\s:]*") }
      path_rel_competiciones <- file.path("..", nombres_carpetas_relativos$competiciones); path_rel_timovi <- file.path("..", nombres_carpetas_relativos$timovi); path_rel_jugadoras <- file.path("..", nombres_carpetas_relativos$jugadoras); path_rel_arbitros <- file.path("..", nombres_carpetas_relativos$arbitros); path_rel_estadios <- file.path("..", nombres_carpetas_relativos$estadios)
      crear_cabecera_alineacion <- function(nombre_equipo_mk, nombre_equipo_lang) {
        iso_code <- get_national_team_iso(nombre_equipo_mk)
        
        if (!is.na(iso_code)) {
          # Es una selección nacional, usar URL de bandera
          logo_src <- paste0("https://hatscripts.github.io/circle-flags/flags/", iso_code, ".svg")
          logo_class <- "match-page-crest national-team-flag"
        } else {
          # Es un equipo de club, usar ruta de logo local
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
      render_equipo_html <- function(df_equipo, goles_del_partido, tarjetas_del_partido, is_national_team_match, team_original_mk_name) { # Eliminamos `player_ids_to_skip` ya que la lógica ha cambiado
        if (is.null(df_equipo) || nrow(df_equipo) == 0) { return(tags$p(t("match_no_data"))) }
        starters <- df_equipo %>% filter(tipo == "Titular")
        subs <- df_equipo %>% filter(tipo == "Suplente")
        
        # Inner function to create the player list items (li tags)
        crear_lista_jugadoras <- function(df_j) {
          if (nrow(df_j) == 0) { return(tags$p(style = "color:#777;", t("match_no_players"))) }
          tags$ul(pmap(df_j, function(id, PlayerName, dorsal, tipo, es_portera, es_capitana, min_entra, min_sale, minutos_jugados, ...) {
            eventos_html <- tagList()
            goles_jugadora <- goles_del_partido %>% filter(id == !!id, tipo == "Normal")
            if (nrow(goles_jugadora) > 0) { walk(1:nrow(goles_jugadora), function(g) { gol <- goles_jugadora[g,]; eventos_html <<- tagAppendChild(eventos_html, tags$span(class = "player-event goal", HTML(paste0("⚽︎ ", formatear_minuto_partido(gol$minuto), "'")))) }) }
            tarjetas_jugadora <- tarjetas_del_partido %>% filter(id == !!id) # Corregido de tarjetas_del_tempo
            if (nrow(tarjetas_jugadora) > 0) { walk(1:nrow(tarjetas_jugadora), function(c) { tarjeta <- tarjetas_jugadora[c,]; card_span <- tags$span(class = if (tarjeta$tipo == "Amarilla") "card-yellow" else "card-red"); eventos_html <<- tagAppendChild(eventos_html, tags$span(class = "player-event", card_span, HTML(paste0("︎ ", formatear_minuto_partido(tarjeta$minuto), "'")))) }) }
            if (!is.na(min_entra) && tipo == "Suplente") { eventos_html <- tagAppendChild(eventos_html, tags$span(class = "player-event sub-in", paste0("↑", min_entra, "'"))) }
            if (!is.na(min_sale) && min_sale < 90 && !is.na(minutos_jugados) && minutos_jugados > 0) { eventos_html <- tagAppendChild(eventos_html, tags$span(class = "player-event sub-out", paste0("↓", min_sale, "'"))) }
            icono_p <- if (isTRUE(es_portera)) "🧤" else ""
            icono_c <- if (isTRUE(es_capitana)) "(C)" else ""
            
            # Lógica condicional para los enlaces de jugadoras
            # Sólo si NO es un partido de la selección NACIONAL Y el equipo actual NO es "Македонија"
            # O, de forma más concisa: si no es un partido de la selección O el equipo actual es "Македонија"
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
            # Lógica condicional para el enlace del estadio
            # Sólo si es partido de selección Y el estadio está en la lista de excluidos, no se genera enlace.
            # O, de forma más concisa: si no es partido de selección O el estadio NO está en la lista de excluidos.
            is_stadium_excluded_for_national_match <- partido_info$es_partido_seleccion && (generar_id_seguro(estadio_info_mk$estadio) %in% stadium_ids_to_skip)
            
            if (!is_stadium_excluded_for_national_match) {
              estadio_element <- tags$a(href = file.path(path_rel_estadios, paste0(generar_id_seguro(estadio_info_mk$estadio), ".html")), estadio_name_lang)
            } else {
              estadio_element <- estadio_name_lang # Texto plano
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
          
          # Lógica condicional para enlaces de árbitros
          # Sólo si es partido de selección Y el árbitro está en la lista de excluidos, no se genera enlace.
          # O, de forma más concisa: si no es partido de selección O el árbitro NO está en la lista de excluidos.
          is_arb_excluded_for_national_match <- partido_info$es_partido_seleccion && (generar_id_seguro(arb$ime) %in% referee_ids_to_skip)
          
          if (!is_arb_excluded_for_national_match) {
            ref_element <- tags$a(href = file.path(path_rel_arbitros, paste0(generar_id_seguro(arb$ime), ".html")), nombre_mostrado)
          } else {
            ref_element <- nombre_mostrado # Texto plano
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
                            partido_info$es_partido_seleccion, # Nuevo: indica si es partido de selección
                            partido_info$local                  # Nuevo: nombre original del equipo local
                          )
                 ), 
                 tags$div(class = "columna-alineacion", 
                          crear_cabecera_alineacion(partido_info$visitante, visitante_name), 
                          render_equipo_html(
                            filter(alineacion_partido_lang, equipo == partido_info$visitante), 
                            goles_partido, 
                            tarjetas_partido,
                            partido_info$es_partido_seleccion, # Nuevo: indica si es partido de selección
                            partido_info$visitante             # Nuevo: nombre original del equipo visitante
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
      
      # Preparar rutas relativas
      path_rel_timovi <- file.path("..", nombres_carpetas_relativos$timovi)
      path_rel_partidos <- file.path("..", nombres_carpetas_relativos$partidos)
      
      # Lista para acumular las filas HTML de la tabla de carrera
      lista_filas_carrera <- list()
      
      # --- PASO 1: Procesar la carrera en la SELECCIÓN NACIONAL ---
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
                  ### LÍNEA MODIFICADA ###
                  # Se reemplaza tags$a por la nueva función, que devolverá texto plano.
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
      
      # --- PASO 2: Procesar la carrera en CLUBES ---
      player_career_clubs_df <- career_summary_jugadoras_df %>% 
        # ==================== ESTA ES LA LÍNEA CORREGIDA ====================
      filter(id == id_j, equipo != "Македонија") %>%
        # ====================================================================
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
      
      # --- PASO 3: Construir la tabla final ---
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
      
      # Cabecera del perfil
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
      
      # Contenido final y guardado del archivo
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
      # Lógica para la imagen del escudo/bandera en el perfil del equipo
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
        tags$img(class=team_logo_class, src=team_logo_src, alt=paste("Escudo de", current_team_name)), 
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
      
      # NUEVA LÓGICA DE EXCLUSIÓN: Si el estadio está en la lista de exclusion, saltar.
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
    
    # --- BLOQUE REINTEGRADO TERMINA AQUÍ ---
    
  } # Fin del bucle principal de idiomas
  
  # --- 5. Creación de la página de redirección en la raíz ---
  message("\nCreando el archivo de redirección en la raíz del sitio...")
  redirect_html_content <- c('<!DOCTYPE html>', '<html>', '<head>', '<title>Redirecting...</title>', '<meta charset="utf-8">', paste0('<meta http-equiv="refresh" content="0; url=', IDIOMAS_SOPORTADOS[1], '/index.html">'), '</head>', '<body>', '<p>If you are not redirected automatically, follow this <a href="', IDIOMAS_SOPORTADOS[1], '/index.html">link</a>.</p>', '</body>', '</html>')
  writeLines(redirect_html_content, file.path(RUTA_SALIDA_RAIZ, "index.html"))
  
}

## -------------------------------------------------------------------------- ##
##  14. FINALIZACIÓN DEL PROCESO
## -------------------------------------------------------------------------- ##
if (hubo_cambios) {
  # Se vuelve a establecer el idioma por defecto al finalizar
  idioma_actual <<- IDIOMAS_SOPORTADOS[1] 
  message(paste("\n", t("final_process_success")));
  if (full_rebuild_needed) { 
    message(t("final_full_rebuild")) 
  } else { 
    message(t("final_incremental_update")) 
  }
  message("El proceso generó primero las páginas maestras y luego las tradujo.")
  message(paste(t("final_site_location"), RUTA_SALIDA_RAIZ));
  message(t("final_navigate_prompt"))
} else {
  message("\nNo se detectaron cambios. El sitio web ya está actualizado.")
}