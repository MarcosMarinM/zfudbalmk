# =========================================================================
# SCRIPT DE GENERACIÓN DE INFORME HTML (VERSIÓN CORREGIDA Y MACEDONIZADA)
# =========================================================================

# -------------------------------------------------------------------------
# PASO 6: INSTALAR Y CARGAR PAQUETES (SIN CAMBIOS)
# -------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, tidyr, purrr, htmltools, stringr, jsonlite
)

# =========================================================================
# NUEVO PASO 6.5: DEFINIR RUTAS Y CREAR ESTRUCTURA DE DIRECTORIOS
# =========================================================================

# Nombres de carpetas y archivos en macedonio para consistencia
nombres_carpetas_mk <- list(
  base = "docs", 
  assets = "assets", 
  competiciones = "natprevaruvanja", 
  partidos = "natprevari", 
  jugadoras = "igraci", 
  equipos = "timovi", 
  arbitros = "sudii", 
  estadios = "stadioni"
)

nombres_archivos_mk <- list(
  partidos = "raspored", 
  clasificacion = "tabela", 
  goleadoras = "strelci", 
  sanciones = "disciplinska"
)

# Se usan los nombres de la lista para definir las rutas dinámicamente
RUTA_BASE_SALIDA <- nombres_carpetas_mk$base
RUTA_ASSETS <- file.path(RUTA_BASE_SALIDA, nombres_carpetas_mk$assets)
RUTA_COMPETICIONES <- file.path(RUTA_BASE_SALIDA, nombres_carpetas_mk$competiciones)
RUTA_PARTIDOS <- file.path(RUTA_BASE_SALIDA, nombres_carpetas_mk$partidos)
RUTA_JUGADORAS <- file.path(RUTA_BASE_SALIDA, nombres_carpetas_mk$jugadoras)
RUTA_EQUIPOS <- file.path(RUTA_BASE_SALIDA, nombres_carpetas_mk$equipos)
RUTA_ARBITROS <- file.path(RUTA_BASE_SALIDA, nombres_carpetas_mk$arbitros)
RUTA_ESTADIOS <- file.path(RUTA_BASE_SALIDA, nombres_carpetas_mk$estadios)

# Crear todos los directorios de una vez.
walk(c(RUTA_BASE_SALIDA, RUTA_ASSETS, RUTA_COMPETICIONES, RUTA_PARTIDOS, 
       RUTA_JUGADORAS, RUTA_EQUIPOS, RUTA_ARBITROS, RUTA_ESTADIOS), 
     dir.create, showWarnings = FALSE, recursive = TRUE)

message("Estructura de directorios creada en: ", RUTA_BASE_SALIDA)


# =========================================================================
# FUNCIONES AUXILIARES
# =========================================================================
generar_terminos_busqueda <- function(nombre) {
  nombre_lower <- tolower(nombre)
  versions <- c(nombre_lower)
  map_base <- c('а'='a', 'б'='b', 'в'='v', 'г'='g', 'д'='d', 'ѓ'='gj', 'е'='e', 'ж'='z', 'з'='z', 'ѕ'='dz', 'и'='i', 'ј'='j', 'к'='k', 'л'='l', 'љ'='lj', 'м'='m', 'н'='n', 'њ'='n', 'о'='o', 'п'='p', 'р'='r', 'с'='s', 'т'='t', 'ќ'='kj', 'у'='u', 'ф'='f', 'х'='h', 'ц'='c', 'ч'='c', 'џ'='dz', 'ш'='s')
  map_diacritic <- c('а'='a', 'б'='b', 'в'='v', 'г'='g', 'д'='d', 'ѓ'='đ', 'е'='e', 'ж'='ž', 'з'='z', 'ѕ'='dz', 'и'='i', 'ј'='j', 'к'='k', 'л'='ll', 'љ'='lj', 'м'='m', 'н'='n', 'њ'='nj', 'о'='o', 'п'='p', 'р'='r', 'с'='s', 'т'='t', 'ќ'='ć', 'у'='u', 'ф'='f', 'х'='h', 'ц'='c', 'ч'='č', 'џ'='dž', 'ш'='š')
  map_digraph <- c('а'='a', 'б'='b', 'в'='v', 'г'='g', 'д'='d', 'ѓ'='g', 'е'='e', 'ж'='zh', 'з'='z', 'ѕ'='dz', 'и'='i', 'ј'='j', 'к'='k', 'л'='l', 'љ'='lj', 'м'='m', 'н'='n', 'њ'='nj', 'о'='o', 'п'='p', 'р'='r', 'с'='s', 'т'='t', 'ќ'='kj', 'у'='u', 'ф'='f', 'х'='h', 'ц'='c', 'ч'='ch', 'џ'='dzh', 'ш'='sh')
  map_alternate <- c('а'='a', 'б'='b', 'в'='v', 'г'='g', 'д'='d', 'ѓ'='dj', 'е'='ë', 'ж'='z', 'з'='z', 'ѕ'='z', 'и'='i', 'ј'='j', 'к'='k', 'л'='ll', 'љ'='l', 'м'='m', 'н'='n', 'њ'='n', 'о'='o', 'п'='p', 'р'='r', 'с'='s', 'т'='t', 'ќ'='c', 'у'='y', 'ф'='f', 'х'='h', 'ц'='ts', 'ч'='ç', 'џ'='xh', 'ш'='sh')
  versions <- c(versions, str_replace_all(nombre_lower, map_base), str_replace_all(nombre_lower, map_diacritic), str_replace_all(nombre_lower, map_digraph), str_replace_all(nombre_lower, map_alternate), str_replace_all(nombre_lower, c('ќ' = 'ḱ')), str_replace_all(nombre_lower, c('њ' = 'ń')), str_replace_all(nombre_lower, c('њ' = 'ñ')))
  map_norm_diacritics <- c('š'='s', 'č'='c', 'ž'='z', 'đ'='dj', 'ć'='c', 'ń'='n', 'ñ'='n', 'ḱ'='k', 'ë'='e', 'ç'='c')
  versions <- c(versions, str_replace_all(nombre_lower, map_norm_diacritics))
  return(paste(unique(versions), collapse = " "))
}
generar_id_seguro <- function(nombre) {
  # Mapa de transliteración de cirílico a latino
  map_id <- c(
    'а'='a', 'б'='b', 'в'='v', 'г'='g', 'д'='d', 'ѓ'='gj', 'е'='e', 
    'ж'='z', 'з'='z', 'ѕ'='dz', 'и'='i', 'ј'='j', 'к'='k', 'л'='l', 
    'љ'='lj', 'м'='m', 'н'='n', 'њ'='nj', 'о'='o', 'п'='p', 'р'='r', 
    'с'='s', 'т'='t', 'ќ'='kj', 'у'='u', 'ф'='f', 'х'='h', 'ц'='c', 
    'ч'='c', 'џ'='dz', 'ш'='s')
  
  # 1. Convertir a minúsculas y transliterar
  nombre_latin <- str_replace_all(tolower(nombre), map_id)
  
  # 2. Reemplazar espacios y slashes con guiones bajos
  id_sanitizada <- str_replace_all(nombre_latin, "[\\s/]+", "_")
  id_sanitizada <- str_replace_all(id_sanitizada, "[^a-z0-9_\\-]+", "")
  
  # 4. Limpiar guiones bajos duplicados o en los extremos
  id_sanitizada <- str_replace_all(id_sanitizada, "_{2,}", "_")
  id_sanitizada <- str_replace_all(id_sanitizada, "^_+|_+$", "")
  
  return(id_sanitizada)
}
crear_botones_navegacion <- function(ruta_relativa_assets = ".") {
  tags$div(class = "nav-buttons",
           tags$a("← Назад", href = "#", onclick = "history.back(); return false;", class = "back-link"),
           tags$a("🏠 Почетна", href = file.path(ruta_relativa_assets, "index.html"), class = "back-link")
  )
}

# --- NUEVA FUNCIÓN AÑADIDA PARA CORREGIR EL ERROR ---
generar_cronologia_df <- function(id_p, resumen_partido) {
  lista_eventos <- list()
  
  # 1. Goles
  goles_data <- resumen_partido$goles
  if (!is.null(goles_data) && nrow(goles_data) > 0) {
    
    if (!"equipo_acreditado" %in% names(goles_data) && "equipo" %in% names(goles_data)) {
      goles_data <- goles_data %>% rename(equipo_acreditado = equipo)
    }
    
    goles_con_ids <- goles_data %>%
      left_join(apariciones_df %>% distinct(nombre, id), by = c("jugadora" = "nombre"))
    
    goles_eventos <- goles_con_ids %>%
      mutate(
        icono = "⚽",
        texto_evento = case_when(
          tipo == "Autogol" ~ paste0("Автогол на ", sprintf("<a href='../%s/%s.html'>%s</a>", nombres_carpetas_mk$jugadoras, id, jugadora), " (", equipo_jugadora, ") во корист на ", equipo_acreditado),
          TRUE ~ paste0("Гол на ", sprintf("<a href='../%s/%s.html'>%s</a>", nombres_carpetas_mk$jugadoras, id, jugadora), " (", equipo_acreditado, ")")
        )
      ) %>%
      select(minuto, icono, texto_evento)
    lista_eventos[[length(lista_eventos) + 1]] <- goles_eventos
  }
  
  # 2. Tarjetas
  tarjetas_data <- resumen_partido$tarjetas
  if (!is.null(tarjetas_data) && nrow(tarjetas_data) > 0) {
    tarjetas_eventos <- tarjetas_data %>%
      mutate(
        icono = if_else(tipo == "Amarilla", "🟨", "🟥"),
        texto_evento = paste0(
          "Картон за ",
          sprintf("<a href='../%s/%s.html'>%s</a>", nombres_carpetas_mk$jugadoras, id_jugadora, jugadora),
          " (",
          sprintf("<a href='../%s/%s.html'>%s</a>", nombres_carpetas_mk$equipos, generar_id_seguro(equipo), equipo),
          ")"
        )
      ) %>%
      select(minuto, icono, texto_evento)
    lista_eventos[[length(lista_eventos) + 1]] <- tarjetas_eventos
  }
  
  # 3. Sustituciones
  procesar_cambios <- function(cambios_df, nombre_equipo, alineacion_equipo) {
    if (is.null(cambios_df) || nrow(cambios_df) == 0 || is.null(alineacion_equipo) || nrow(alineacion_equipo) == 0) return(NULL)
    
    map_dfr(1:nrow(cambios_df), function(i) {
      cambio <- cambios_df[i,]
      match_info <- str_match(cambio$texto, "Entra (.*?) \\((\\d+)\\) por (.*?) \\((\\d+)\\)")
      if (is.na(match_info[1,1])) return(NULL)
      
      nombre_entra <- match_info[1, 2]; dorsal_entra <- match_info[1, 3]
      nombre_sale <- match_info[1, 4]; dorsal_sale <- match_info[1, 5]
      
      # --- INICIO DE LA CORRECCIÓN: Búsqueda robusta de IDs ---
      info_entra <- filter(alineacion_equipo, nombre == nombre_entra, dorsal == as.numeric(dorsal_entra))
      id_entra <- if (nrow(info_entra) > 0) info_entra$id[1] else NA_character_
      
      info_sale <- filter(alineacion_equipo, nombre == nombre_sale, dorsal == as.numeric(dorsal_sale))
      id_sale <- if (nrow(info_sale) > 0) info_sale$id[1] else NA_character_
      # --- FIN DE LA CORRECCIÓN ---
      
      texto_entra_link <- if (!is.na(id_entra)) sprintf("<a href='../%s/%s.html'>%s</a>", nombres_carpetas_mk$jugadoras, id_entra, nombre_entra) else nombre_entra
      texto_sale_link <- if (!is.na(id_sale)) sprintf("<a href='../%s/%s.html'>%s</a>", nombres_carpetas_mk$jugadoras, id_sale, nombre_sale) else nombre_sale
      
      texto_final <- paste0(
        "Измена за ", sprintf("<a href='../%s/%s.html'>%s</a>", nombres_carpetas_mk$equipos, generar_id_seguro(nombre_equipo), nombre_equipo),
        ": влегува ", texto_entra_link, " (", dorsal_entra, ") на местото на ", texto_sale_link, " (", dorsal_sale, ")"
      )
      
      tibble(minuto = cambio$minuto, icono = "🔄", texto_evento = texto_final)
    })
  }
  
  # Unimos las alineaciones para tener un mapa ID-Nombre-Dorsal completo para este partido
  alineacion_completa <- bind_rows(
    resumen_partido$alineacion_local %>%
      mutate(equipo = resumen_partido$partido_info$local) %>%
      select(-id) %>% 
      left_join(id_mapping, by="nombre") %>% 
      rename(id=canonical_id),
    resumen_partido$alineacion_visitante %>%
      mutate(equipo = resumen_partido$partido_info$visitante) %>%
      select(-id) %>% 
      left_join(id_mapping, by="nombre") %>% 
      rename(id=canonical_id)
  )
  
  cambios_local_eventos <- procesar_cambios(resumen_partido$cambios_local, resumen_partido$partido_info$local, filter(alineacion_completa, equipo == resumen_partido$partido_info$local))
  cambios_visitante_eventos <- procesar_cambios(resumen_partido$cambios_visitante, resumen_partido$partido_info$visitante, filter(alineacion_completa, equipo == resumen_partido$partido_info$visitante))
  
  lista_eventos[[length(lista_eventos) + 1]] <- cambios_local_eventos
  lista_eventos[[length(lista_eventos) + 1]] <- cambios_visitante_eventos
  
  # Combinar y ordenar
  if (length(lista_eventos) == 0) {
    return(tibble(minuto = integer(), icono = character(), texto_evento = character()))
  }
  
  cronologia_final_df <- bind_rows(lista_eventos) %>%
    filter(!is.na(minuto)) %>%
    arrange(minuto)
  
  return(cronologia_final_df)
}

# =========================================================================
# NUEVA FUNCIÓN: PLANTILLA HTML (CON MODIFICACIÓN)
# =========================================================================
crear_pagina_html <- function(contenido_principal, titulo_pagina = "Фудбалски портал МК", ruta_relativa_assets = ".", search_data_json, script_contraseña) {
  tags$html(lang = "mk",
            tags$head(
              tags$meta(charset="UTF-8"),
              tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
              tags$title(titulo_pagina),
              tags$link(rel = "stylesheet", href = file.path(ruta_relativa_assets, nombres_carpetas_mk$assets, "style.css")),
              script_contraseña
            ),
            tags$body(
              tags$div(class = "container",
                       tags$h1(tags$a(href = file.path(ruta_relativa_assets, "index.html"), style = "color: inherit; text-decoration: none;", "Фудбалски портал МК")),
                       tags$div(class = "search-container",
                                tags$form(action = "#", onsubmit = "showSearchResults(); return false;",
                                          tags$input(type = "text", id = "search-input", class = "search-input", placeholder = "Пребарај фудбалерка, тим, судија, стадион...", onkeyup = "handleSearchInput(event)"),
                                          tags$button(type = "submit", class = "search-button", "Пребарај")
                                ),
                                tags$div(id = "search-suggestions")
                       ),
                       ### MODIFICACIÓN INICIO: Añadir un div para el contenido principal ###
                       # Este 'div' nos permite reemplazar fácilmente el contenido con los resultados de la búsqueda.
                       tags$div(id = "main-content",
                                contenido_principal
                       )
                       ### MODIFICACIÓN FIN ###
              ),
              tags$script(type = "application/json", id = "search-data-json", HTML(search_data_json)),
              tags$script(defer = NA, src = file.path(ruta_relativa_assets, nombres_carpetas_mk$assets, "script.js"))
            )
  )
}


message("Започнување со генерирање на HTML извештајот...")

# =========================================================================
# NUEVO PASO 6.8: CARGAR ESTILOS DE CLASIFICACIÓN
# =========================================================================
message("Вчитување на сопствени стилови за табели...")

# --- Función para parsear el archivo de estilos de clasificación ---
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
      if (linea == "" || startsWith(linea, "#")) next # Ignorar líneas vacías o comentarios
      
      # Detectar cabecera de competición
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
          texto <- trimws(partes[3])
          
          if (!is.na(puesto) && nchar(color) > 1 && nchar(texto) > 0) {
            # Añadir regla de estilo para la fila de la tabla
            regla_actual <- data.frame(puesto = puesto, color = color, texto = texto, stringsAsFactors = FALSE)
            lista_estilos[[competicion_actual]]$reglas <- rbind(lista_estilos[[competicion_actual]]$reglas, regla_actual)
            
            # Añadir a la leyenda (evitando duplicados)
            if (!any(sapply(lista_estilos[[competicion_actual]]$leyenda, function(l) l$color == color && l$texto == texto))) {
              lista_estilos[[competicion_actual]]$leyenda[[length(lista_estilos[[competicion_actual]]$leyenda) + 1]] <- list(color = color, texto = texto)
            }
          }
        }
      }
    }
    message("Стиловите за табели се вчитани успешно.")
    return(lista_estilos)
  }, error = function(e) {
    warning("Грешка при вчитување на датотеката за стилови на табели. Нема да се применат сопствени стилови.")
    message("Грешката е: ", e$message)
    return(list())
  })
}

# Llamada a la función para cargar los datos
ruta_estilos_clasificacion <- "estilos_clasificacion.txt"
estilos_clasificacion_data <- parsear_estilos_clasificacion(ruta_estilos_clasificacion)


# =========================================================================
# PASO 6.9: CARGAR MAPEADORES EXTERNOS
# =========================================================================
message("Вчитување на мапирања за националности...")

# Usamos rutas relativas. Los archivos deben estar en el mismo directorio que el script.
ruta_mapeo_iso <- "nacionalidades_mapeo.txt"
ruta_traduccion_mk <- "nacionalidades_traduccion.txt"
mapeo_completo_df <- NULL

if (file.exists(ruta_mapeo_iso) && file.exists(ruta_traduccion_mk)) {
  tryCatch({
    # CAMBIO: Usamos read.csv que es más robusto para este formato.
    mapeo_iso_df <- read.csv(ruta_mapeo_iso, stringsAsFactors = FALSE)
    traduccion_mk_df <- read.csv(ruta_traduccion_mk, stringsAsFactors = FALSE, encoding = "UTF-8")
    
    # Unirlos en un único dataframe de mapeo
    mapeo_completo_df <- merge(mapeo_iso_df, traduccion_mk_df, by = "nombre_ingles", all = TRUE)
    
    # Crear una columna de clave en minúsculas para una unión robusta
    mapeo_completo_df$clave_lower <- tolower(trimws(mapeo_completo_df$nombre_ingles))
    
    message("Мапирањата за националности се вчитани успешно.")
  }, error = function(e) {
    warning("Грешка при вчитување на датотеките за мапирање на националности. Функционалноста на знамињата ќе биде оневозможена.")
    # Imprimir el error original para un mejor diagnóstico
    message("Грешката е: ", e$message)
  })
} else {
  warning("Една или повеќе датотеки за мапирање на националности не се пронајдени. Функционалноста на знамињата ќе биде оневозможена.")
}

# -------------------------------------------------------------------------
# PASO 7: PREPARACIÓN DE DATOS
# -------------------------------------------------------------------------

# =========================================================================
# NUEVO PASO 7.0: APLICAR CORRECCIONES Y REORDENAR NOMBRES
# =========================================================================
message("Aplicando correcciones y reordenando nombres...")

# --- Función para aplicar el mapa de conversiones a un dataframe ---
aplicar_conversiones <- function(df, columnas, mapa_conversiones) {
  if (is.null(mapa_conversiones) || nrow(df) == 0) return(df)
  # Usamos across con any_of para aplicar la función solo a las columnas que existan
  df %>%
    mutate(across(any_of(columnas), ~ recode(.x, !!!mapa_conversiones)))
}

# --- Función para reordenar el nombre de una jugadora (Apellido, Nombre -> Nombre Apellido) ---
reordenar_nombre_jugadora <- function(nombres) {
  sapply(nombres, function(nombre) {
    if (is.na(nombre)) return(NA_character_)
    palabras <- str_split(nombre, "\\s+")[[1]]
    if (length(palabras) >= 2) {
      # Mueve la última palabra al principio
      primer_nombre <- palabras[length(palabras)]
      apellido <- paste(palabras[-length(palabras)], collapse = " ")
      return(paste(primer_nombre, apellido))
    } else {
      # Si solo hay una palabra, la devuelve tal cual
      return(nombre)
    }
  }, USE.NAMES = FALSE)
}

# 1. Cargar el archivo de conversiones
ruta_conversiones <- "conversions.txt"
mapa_conversiones <- NULL
if (file.exists(ruta_conversiones)) {
  tryCatch({
    conversiones_df <- read.csv(ruta_conversiones, stringsAsFactors = FALSE, encoding = "UTF-8")
    # Crear un vector nombrado que sirva como mapa/diccionario para recode
    mapa_conversiones <- setNames(conversiones_df$corregido, conversiones_df$original)
    message("Archivo de conversiones cargado y procesado.")
  }, error = function(e) {
    warning("Error al leer el archivo conversions.txt. No se aplicarán correcciones.")
  })
} else {
  message("Archivo conversions.txt no encontrado. Se continuará sin aplicar correcciones de nombres.")
}

# 2. Aplicar correcciones a todos los dataframes relevantes
if (!is.null(mapa_conversiones)) {
  # Corregir nombres de equipos y estadios en partidos_df
  partidos_df <- aplicar_conversiones(partidos_df, c("local", "visitante", "estadio"), mapa_conversiones)
  
  # Corregir nombres en los datos de resultados exitosos (fuente de todos los demás dataframes)
  resultados_exitosos <- map(resultados_exitosos, function(res) {
    if (is.null(res)) return(NULL)
    
    # Info del partido
    res$partido_info <- aplicar_conversiones(res$partido_info, c("local", "visitante"), mapa_conversiones)
    res$estadio <- recode(res$estadio, !!!mapa_conversiones)
    res$arbitro_principal <- recode(res$arbitro_principal, !!!mapa_conversiones)
    res$arbitro_asist_1 <- recode(res$arbitro_asist_1, !!!mapa_conversiones)
    res$arbitro_asist_2 <- recode(res$arbitro_asist_2, !!!mapa_conversiones)
    
    # Alineaciones
    res$alineacion_local <- aplicar_conversiones(res$alineacion_local, c("nombre"), mapa_conversiones)
    res$alineacion_visitante <- aplicar_conversiones(res$alineacion_visitante, c("nombre"), mapa_conversiones)
    
    # Goles
    res$goles <- aplicar_conversiones(res$goles, c("jugadora", "equipo_jugadora", "equipo_acreditado"), mapa_conversiones)
    
    # Tarjetas
    res$tarjetas <- aplicar_conversiones(res$tarjetas, c("jugadora", "equipo"), mapa_conversiones)
    
    return(res)
  })
}

# 3. Reordenar nombres de jugadoras (APELLIDO, NOMBRE -> NOMBRE APELLIDO)
resultados_exitosos <- map(resultados_exitosos, function(res) {
  if (is.null(res)) return(NULL)
  
  if (nrow(res$alineacion_local) > 0) {
    res$alineacion_local$nombre <- reordenar_nombre_jugadora(res$alineacion_local$nombre)
  }
  if (nrow(res$alineacion_visitante) > 0) {
    res$alineacion_visitante$nombre <- reordenar_nombre_jugadora(res$alineacion_visitante$nombre)
  }
  if (nrow(res$goles) > 0) {
    res$goles$jugadora <- reordenar_nombre_jugadora(res$goles$jugadora)
  }
  if (nrow(res$tarjetas) > 0) {
    res$tarjetas$jugadora <- reordenar_nombre_jugadora(res$tarjetas$jugadora)
  }
  
  return(res)
})


# --- 7.1: Cargar y unificar datos brutos ---
apariciones_df_raw <- map_dfr(resultados_exitosos, ~bind_rows(
  .x$alineacion_local %>% mutate(id_partido = .x$partido_info$id_partido, equipo = .x$partido_info$local, competicion_nombre = .x$partido_info$competicion_nombre, competicion_temporada = .x$partido_info$competicion_temporada),
  .x$alineacion_visitante %>% mutate(id_partido = .x$partido_info$id_partido, equipo = .x$partido_info$visitante, competicion_nombre = .x$partido_info$competicion_nombre, competicion_temporada = .x$partido_info$competicion_temporada)
)) %>% mutate(nombre = str_squish(nombre))

minutos_df_raw <- map_dfr(resultados_exitosos, function(res) {
  if(is.null(res)) return(NULL); id_p <- res$partido_info$id_partido
  calcular_minutos_equipo <- function(alineacion, cambios) {
    if(is.null(alineacion) || nrow(alineacion) == 0) return(NULL)
    jugadoras_con_minutos <- alineacion %>% mutate(min_entra = if_else(tipo == "Titular", 0, NA_real_), min_sale = if_else(tipo == "Titular", 90, 0))
    if (!is.null(cambios) && nrow(cambios) > 0) {
      cambios_procesados <- cambios %>%
        mutate(d_entra = as.numeric(str_match(texto, "Entra .*?\\((\\d+)\\)")[, 2]), d_sale  = as.numeric(str_match(texto, "por .*?\\((\\d+)\\)")[, 2])) %>%
        select(minuto, d_entra, d_sale) %>% filter(!is.na(d_entra) & !is.na(d_sale))
      for (i in 1:nrow(cambios_procesados)) {
        cambio <- cambios_procesados[i, ]
        jugadoras_con_minutos <- jugadoras_con_minutos %>% mutate(min_sale = if_else(dorsal == cambio$d_sale, as.numeric(cambio$minuto), min_sale), min_entra = if_else(dorsal == cambio$d_entra, as.numeric(cambio$minuto), min_entra))
      }
    }
    jugadoras_con_minutos %>% mutate(min_sale = if_else(!is.na(min_entra) & tipo == "Suplente" & min_sale == 0, 90, min_sale), minutos_jugados = if_else(is.na(min_entra), 0, min_sale - min_entra)) %>% mutate(minutos_jugados = pmax(0, minutos_jugados))
  }
  min_local <- calcular_minutos_equipo(res$alineacion_local, res$cambios_local); min_visitante <- calcular_minutos_equipo(res$alineacion_visitante, res$cambios_visitante)
  bind_rows(min_local, min_visitante) %>% mutate(id_partido = id_p)
})


preferred_id_map <- apariciones_df_raw %>% filter(!is.na(nombre), !is.na(id), str_detect(id, "^\\d{5,6}$")) %>% count(nombre, id, name = "frequency") %>% group_by(nombre) %>% filter(frequency == max(frequency)) %>% slice(1) %>% ungroup() %>% select(nombre, canonical_id = id)
id_mapping <- apariciones_df_raw %>% filter(!is.na(nombre) & nchar(trimws(nombre)) > 2) %>% distinct(nombre) %>% left_join(preferred_id_map, by = "nombre") %>% mutate(final_id = if_else(!is.na(canonical_id), as.character(canonical_id), paste0("player_gen_", generar_id_seguro(nombre)))) %>% select(nombre, canonical_id = final_id)

apariciones_df <- apariciones_df_raw %>% left_join(minutos_df_raw %>% select(id_partido, nombre, dorsal, tipo, min_entra, min_sale, minutos_jugados), by = c("id_partido", "nombre", "dorsal", "tipo")) %>% select(-id) %>% left_join(id_mapping, by = "nombre") %>% rename(id = canonical_id) %>% select(id, id_partido, nombre, dorsal, tipo, equipo, es_portera, es_capitana, competicion_nombre, competicion_temporada, everything())

goles_raw_df <- map_dfr(resultados_exitosos, "goles")
if (nrow(goles_raw_df) > 0) { goles_df_unificado <- goles_raw_df %>% mutate(jugadora = str_squish(jugadora)) %>% left_join(id_mapping, by = c("jugadora" = "nombre")) %>% select(-any_of(c("id", "id_jugadora"))) %>% rename(id = canonical_id)
} else { goles_df_unificado <- tibble(id_partido = character(), jugadora = character(), equipo_jugadora = character(), equipo_acreditado = character(), minuto = integer(), dorsal = integer(), tipo = character(), id = character()) }

tarjetas_raw_df <- map_dfr(resultados_exitosos, "tarjetas")
if(nrow(tarjetas_raw_df) > 0) { tarjetas_df_unificado <- tarjetas_raw_df %>% mutate(jugadora = str_squish(jugadora)) %>% left_join(id_mapping, by = c("jugadora" = "nombre")) %>% select(-any_of(c("id", "id_jugadora"))) %>% rename(id = canonical_id)
} else { tarjetas_df_unificado <- tibble(jugadora = character(), equipo = character(), dorsal = integer(), minuto = integer(), tipo = character(), motivo = character(), id_partido = character(), id = character()) }

# --- 7.1.5: Procesar y traducir posiciones y datos demográficos ---
message("Процесирање и преведување на позиции и демографски податоци на фудбалерки...")

# 1. Crear un único DICCIONARIO de traducción de posiciones.
traduccion_posicion_mk <- c(
  "GK" = "Голманка", "Portera" = "Голманка",
  "DL" = "Одбрана", "DC" = "Одбрана", "DR" = "Одбрана", "DM" = "Одбрана",
  "WBL" = "Одбрана", "WBR" = "Одбрана", "Defensa" = "Одбрана",
  "ML" = "Среден ред", "MC" = "Среден ред", "MR" = "Среден ред", "AMC" = "Среден ред",
  "Centrocampista" = "Среден ред",
  "AML" = "Напад", "AMR" = "Напад", "SC" = "Напад", "Delantera" = "Напад"
)

# 2. Procesar el dataframe 'posiciones_df' para mantener todos los datos.
#    Esta lógica es ahora mucho más simple y directa.
posiciones_procesadas_df <- posiciones_df %>%
  # Usamos el diccionario para crear una nueva columna traducida.
  mutate(posicion_mk = recode(posicion, !!!traduccion_posicion_mk, .default = NA_character_)) %>%
  
  # Quitamos cualquier fila que не se haya podido traducir
  filter(!is.na(posicion_mk)) %>%
  
  # Agrupamos por jugadora para manejar los casos de múltiples posiciones
  group_by(id) %>%
  summarise(
    # Obtenemos las posiciones únicas y las unimos con "/"
    posicion_final_mk = paste(unique(posicion_mk), collapse = " / "),
    # Conservamos los datos demográficos (tomamos el primero, asumiendo que son consistentes)
    nacionalidad = first(nacionalidad),
    fecha_nacimiento = first(fecha_nacimiento),
    ciudad_nacimiento = first(ciudad_nacimiento),
    .groups = 'drop'
  )

# --- 7.2: Identificar competiciones únicas ---
message("Идентификување и подредување на уникатни натпреварувања...")
if (exists("partidos_df") && nrow(partidos_df) > 0) {
  competiciones_unicas_df <- partidos_df %>%
    distinct(competicion_nombre, competicion_temporada) %>%
    mutate(
      nombre_completo = paste(competicion_nombre, competicion_temporada),
      competicion_id = generar_id_seguro(nombre_completo), # <-- LLAMADA CRUCIAL A LA FUNCIÓN CORREGIDA
      nombre_lower = tolower(competicion_nombre)
    ) %>%
    mutate(
      importancia_score = case_when(
        str_detect(nombre_lower, "куп") ~ 1,
        str_detect(nombre_lower, "прва") ~ 2,
        str_detect(nombre_lower, "втора") ~ 3,
        str_detect(nombre_lower, "трета") ~ 4,
        str_detect(nombre_lower, "младинска") ~ 5,
        str_detect(nombre_lower, "кадетска") ~ 6,
        TRUE ~ 7
      ),
      baraz_modifier = if_else(str_detect(nombre_lower, "бараж"), 0.5, 0),
      final_score = importancia_score + baraz_modifier
    ) %>%
    arrange(
      desc(competicion_temporada),
      final_score,
      nombre_completo
    )
} else {
  competiciones_unicas_df <- tibble(competicion_nombre=character(), competicion_temporada=character(), competicion_id=character(), nombre_completo=character())
}


# --- 7.3: Preparar datos globales para perfiles ---
if (!exists("apariciones_df") || nrow(apariciones_df) == 0) {
  jugadoras_stats_df <- data.frame(id=character(), Фудбалерка=character(), Тим=character(), Повикана=integer(), Одиграни_натпревари=integer(), Почетен_состав=integer(), Минути=numeric(), Голови=numeric(), Жолти=integer(), Црвени=integer(), stringsAsFactors = FALSE)
} else {
  stats_generales <- apariciones_df %>% filter(!is.na(id)) %>% group_by(id) %>% summarise(Фудбалерка=first(nombre),Тим=last(equipo),Повикана=n_distinct(id_partido),Почетен_состав=sum(tipo=="Titular",na.rm=T),Минути=sum(minutos_jugados,na.rm=T),Одиграни_натпревари=sum(minutos_jugados>0,na.rm=T),.groups='drop')
  goles_por_jugadora_global <- goles_df_unificado %>% filter(!is.na(id), tipo == "Normal") %>% group_by(id) %>% summarise(Голови = n(), .groups = 'drop')
  tarjetas_por_jugadora_global <- tarjetas_df_unificado %>% filter(!is.na(id)) %>% group_by(id) %>% summarise(Жолти=sum(tipo=="Amarilla",na.rm=T),Црвени=sum(tipo=="Roja",na.rm=T),.groups='drop')
  jugadoras_stats_temp <- stats_generales %>% 
    left_join(goles_por_jugadora_global, by="id") %>% 
    left_join(tarjetas_por_jugadora_global, by="id") %>%
    left_join(posiciones_procesadas_df, by = "id") %>%
    mutate(
      Голови=replace_na(Голови,0), 
      Жолти=replace_na(Жолти,0), 
      Црвени=replace_na(Црвени,0),
      edad = if_else(!is.na(fecha_nacimiento), floor(as.numeric(difftime(Sys.Date(), fecha_nacimiento, units = "days")) / 365.25), NA_integer_),
      clave_lower = tolower(trimws(nacionalidad)) # Clave para la unión
    )
  
  # Unir con el mapeo completo si existe
  if (!is.null(mapeo_completo_df)) {
    jugadoras_stats_df <- jugadoras_stats_temp %>%
      left_join(mapeo_completo_df, by = "clave_lower")
  } else {
    # Si el mapeo falla, crea las columnas como NA para evitar errores posteriores
    jugadoras_stats_df <- jugadoras_stats_temp %>%
      mutate(codigo_iso = NA_character_, nombre_macedonio = NA_character_)
  }
  
  jugadoras_stats_df <- jugadoras_stats_df %>%
    # Seleccionamos todas las columnas necesarias, incluyendo las nuevas del mapeo
    select(id, Фудбалерка, Тим, posicion_final_mk, nacionalidad, edad, codigo_iso, nombre_macedonio, Повикана, Одиграни_натпревари, Почетен_состав, Минути, Голови, Жолти, Црвени) %>% 
    arrange(desc(Голови), desc(Минути))
}
arbitros_df <- map_dfr(resultados_exitosos, ~if(is.null(.x)||is.null(.x$arbitro_principal)) NULL else data.frame(id_partido=.x$partido_info$id_partido,arbitro_principal=.x$arbitro_principal,arbitro_asist_1=.x$arbitro_asist_1,arbitro_asist_2=.x$arbitro_asist_2)) %>% pivot_longer(cols=starts_with("arbitro_"),names_to="uloga",values_to="ime",values_drop_na=T) %>% mutate(uloga=case_when(uloga=="arbitro_principal"~"Главен судија",uloga=="arbitro_asist_1"~"1-ви помошник",uloga=="arbitro_asist_2"~"2-ри помошник",T~uloga))
estadios_df <- map_dfr(resultados_exitosos, ~if(is.null(.x)||is.null(.x$estadio)) NULL else data.frame(id_partido=.x$partido_info$id_partido,estadio=.x$estadio)) %>% left_join(partidos_df,by="id_partido")

# --- 7.4: Crear índice de búsqueda unificado (con soporte multi-alfabeto) ---
message("Креирање на индекс за пребарување со поддршка за латиница...")

search_jugadoras <- jugadoras_stats_df %>% 
  select(Име = Фудбалерка, id) %>% 
  mutate(
    Тип = "Фудбалерка", 
    target_id = paste0("jugadora-", id),
    search_terms = sapply(Име, generar_terminos_busqueda, USE.NAMES = FALSE)
  ) %>% select(Име, Тип, target_id, search_terms)

search_equipos <- data.frame(Име = unique(c(partidos_df$local,partidos_df$visitante))) %>% 
  mutate(
    Тип = "Тим", 
    target_id = paste0("equipo-", generar_id_seguro(Име)),
    search_terms = sapply(Име, generar_terminos_busqueda, USE.NAMES = FALSE)
  )

search_arbitros <- data.frame(Име = unique(arbitros_df$ime)) %>% 
  mutate(
    Тип = "Судија", 
    target_id = paste0("arbitro-", generar_id_seguro(Име)),
    search_terms = sapply(Име, generar_terminos_busqueda, USE.NAMES = FALSE)
  )

search_competiciones <- competiciones_unicas_df %>% 
  mutate(
    Име = nombre_completo, 
    Тип = "Натпреварување", 
    target_id = paste0("menu-competicion-", competicion_id),
    search_terms = sapply(Име, generar_terminos_busqueda, USE.NAMES = FALSE)
  ) %>% select(Име, Тип, target_id, search_terms)

search_estadios <- data.frame(Име = unique(na.omit(estadios_df$estadio))) %>%
  mutate(
    Тип = "Стадион",
    target_id = paste0("стадион-", generar_id_seguro(Име)),
    search_terms = sapply(Име, generar_terminos_busqueda, USE.NAMES = FALSE)
  )

search_index_df <- bind_rows(search_jugadoras, search_equipos, search_arbitros, search_competiciones, search_estadios) %>% 
  arrange(Име)

search_data_json <- toJSON(search_index_df, auto_unbox = TRUE)



# =========================================================================
# PASO 8 y 9: EXTERNALIZACIÓN Y GENERACIÓN DE PÁGINAS HTML
# =========================================================================

# --- 8.1: Guardar CSS y JS en archivos externos ---
estilo_css <- r"(
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; line-height: 1.6; background-color: #f8f9fa; color: #212529; margin: 0; }
.container { max-width: 900px; margin: 20px auto; padding: 20px; background-color: #ffffff; border-radius: 8px; box-shadow: 0 0 15px rgba(0,0,0,0.05); }
.page { display: none; } #portal { display: block; }
h1, h2, h3 { color: #003366; border-bottom: 2px solid #dee2e6; padding-bottom: 10px; }
h1 { font-size: 2.5em; text-align: center; } h2 { font-size: 1.8em; margin-top: 40px; } h3 { font-size: 1.5em; }
a { color: #0056b3; text-decoration: none; font-weight: bold; } a:hover { text-decoration: underline; }
table { width: 100%; border-collapse: collapse; margin-top: 20px; }
th, td { padding: 12px; border: 1px solid #dee2e6; text-align: left; } th { background-color: #f2f2f2; }
.summary-row { cursor: pointer; } .summary-row:hover { background-color: #f0f8ff; }
.details-row { display: none; } .details-row > td { padding: 0; }
.details-content { padding: 20px; background-color: #fdfdfd; border-top: 2px solid #003366; }
.details-content h4 { font-size: 1.3em; color: #004488; margin-top: 10px; border-bottom: 1px solid #e0e0e0; padding-bottom: 5px;}
.back-link, .menu-button, .portal-button { display: inline-block; margin-top: 20px; padding: 10px 15px; background-color: #6c757d; color: white !important; border-radius: 5px; font-weight: bold; text-decoration: none; text-align: center;}
.back-link:hover, .menu-button:hover, .portal-button:hover { background-color: #5a6268; text-decoration: none; }
.menu-container, .portal-container { text-align: center; padding: 20px 0; display: flex; flex-wrap: wrap; justify-content: center; gap: 15px; }
.menu-button { padding: 15px 30px; font-size: 1.1em; background-color: #003366; color: white !important; } .menu-button:hover { background-color: #002244; }
.portal-button { width: 80%; padding: 20px; font-size: 1.3em; background-color: #004488; } .portal-button:hover { background-color: #003366; }
.sortable-header { cursor: pointer; user-select: none; } .sortable-header::after { content: ' '; display: inline-block; margin-left: 5px; }
.sortable-header.asc::after { content: '▲'; } .sortable-header.desc::after { content: '▼'; }
.partido-link { display: flex; justify-content: space-between; align-items: center; padding: 15px; margin: 10px 0; background-color: #e9ecef; border-radius: 5px; transition: background-color 0.2s; }
.partido-link:hover { background-color: #ced4da; } .partido-link span.equipo { flex: 1; }
.partido-link span.equipo-local { text-align: right; margin-right: 15px; } .partido-link span.equipo-visitante { text-align: left; margin-left: 15px; }
.partido-link span.resultado { font-size: 1.2em; font-weight: bold; text-align: center; }
.jornada-header { background-color: #003366; color: white; padding: 10px; border-radius: 5px; margin-top: 30px; }
.timeline { list-style: none; padding-left: 0; } .timeline li { padding: 8px 0; border-bottom: 1px dotted #ccc; display: flex; align-items: center; }
.timeline .icon { margin-right: 10px; font-size: 1.2em; width: 24px; text-align: center; }
.alineaciones-container { display: flex; gap: 30px; align-items: flex-start; } .columna-alineacion { flex: 1; }
.columna-alineacion h3 a { color: #003366; } .columna-alineacion h4 { margin-top: 15px; margin-bottom: 10px; font-size: 1.2em; color: #111; border-bottom: 1px solid #ccc; padding-bottom: 5px; }
.columna-alineacion ul { list-style: none; padding: 0; margin: 0 0 20px 0; } .columna-alineacion li { padding: 6px 3px; border-bottom: 1px solid #f0f0f0; }
.player-event { margin-left: 8px; font-size: 0.9em; color: #444; vertical-align: middle; } .player-event.goal { font-weight: bold; }
.sub-in { color: #28a745; font-style: italic; vertical-align: middle; } .sub-out { color: #dc3545; font-style: italic; vertical-align: middle; }
.card-yellow, .card-red { display: inline-block; width: 12px; height: 16px; border: 1px solid #777; border-radius: 2px; vertical-align: middle; margin-left: 4px; }
.card-yellow { background-color: #ffc107; } .card-red { background-color: #dc3545; }
.search-container { position: relative; margin: 25px 0; }
.search-container form { display: flex; }
.search-input { flex-grow: 1; font-size: 1.1em; padding: 12px; border: 1px solid #ccc; border-radius: 5px 0 0 5px; }
.search-button { font-size: 1.1em; padding: 12px 20px; border: 1px solid #003366; background-color: #003366; color: white; cursor: pointer; border-radius: 0 5px 5px 0; }
#search-suggestions { display: none; position: absolute; top: 100%; left: 0; right: 0; background-color: white; border: 1px solid #ccc; border-top: none; z-index: 1000; max-height: 300px; overflow-y: auto; box-shadow: 0 4px 8px rgba(0,0,0,0.1); }
#search-suggestions a { display: block; padding: 12px; color: #333; text-decoration: none; border-bottom: 1px solid #f0f0f0; }
#search-suggestions a:last-child { border-bottom: none; }
#search-suggestions a:hover { background-color: #f2f2f2; }
#search-suggestions a strong { color: #003366; }
#search-results-list ul { list-style-type: none; padding: 0; }
#search-results-list li { margin-bottom: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 4px; }
#search-results-list a { font-size: 1.2em; text-decoration: none; }
#search-results-list a:hover { text-decoration: underline; }
.search-result-type { font-size: 0.85em; color: #6c757d; margin-left: 8px; }
.clickable-row { cursor: pointer; }
.clickable-row:hover { background-color: #f0f8ff; }

/* === INICIO ESTILOS NUEVOS PARA LEYENDA DE CLASIFICACIÓN === */
.legend { margin-top: 20px; padding: 10px; text-align: left; font-size: 0.9em; }
.legend-item { display: inline-flex; align-items: center; margin-right: 20px; margin-bottom: 5px; }
.legend-color-box { width: 15px; height: 15px; border: 1px solid #ccc; margin-right: 8px; flex-shrink: 0; }
/* === FIN ESTILOS NUEVOS === */

)"
writeLines(estilo_css, file.path(RUTA_ASSETS, "style.css"))





script_js <- r"(
let searchData = [];

document.addEventListener('DOMContentLoaded', initializeSearch);

function initializeSearch() {
  const searchDataElement = document.getElementById('search-data-json');
  if (searchDataElement) {
    try { 
      searchData = JSON.parse(searchDataElement.textContent); 
    } catch (e) { 
      console.error('Error parsing search data JSON:', e); 
    }
  }
  
  // Ocultar sugerencias si se hace clic fuera del buscador
  document.addEventListener('click', function(event) {
    const searchContainer = document.querySelector('.search-container');
    if (searchContainer && !searchContainer.contains(event.target)) {
      const suggestions = document.getElementById('search-suggestions');
      if(suggestions) suggestions.style.display = 'none';
    }
  });

  // ================== NUEVO MANEJADOR DE CLICS (UBICACIÓN CORRECTA) ==================
  // Delegación de eventos para manejar clics en filas de tabla clicables.
  // Lo ponemos aquí para que se active una vez que la página ha cargado.
  document.addEventListener('click', function(event) {
    // 'closest' sube por el DOM para encontrar el ancestro más cercano que coincida
    const clickableRow = event.target.closest('.clickable-row');
    
    if (clickableRow && clickableRow.dataset.href) {
      window.location.href = clickableRow.dataset.href;
    }
  });
  // =================================================================================
}

function toggleDetails(elementId) {
  const detailsRow = document.getElementById(elementId);
  if (detailsRow) {
    detailsRow.style.display = (detailsRow.style.display === 'table-row') ? 'none' : 'table-row';
  }
}

function getBasePath() {
  const path = window.location.pathname;
  if (path.endsWith('.html') && !path.endsWith('index.html')) {
    const segments = path.split('/');
    if (segments.length > 2) return '..';
  }
  return '.';
}

function generateLink(target_id) {
  const basePath = getBasePath();
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
    case 'menu': 
      folder = 'natprevaruvanja';
      id = id.replace('competicion-', '');
      break;
    default: return `${basePath}/index.html`;
  }
  return `${basePath}/${folder}/${id}.html`;
}

function handleSearchInput(event) {
  if (event.key === 'Enter') {
    event.preventDefault();
    showSearchResults();
    return;
  }
  const input = document.getElementById('search-input');
  const suggestionsContainer = document.getElementById('search-suggestions');
  const query = input.value.trim().toLowerCase();
  
  if (query.length < 2) {
    suggestionsContainer.innerHTML = '';
    suggestionsContainer.style.display = 'none';
    return;
  }
  const searchTokens = query.split(' ').filter(t => t.length > 0);
  const filteredResults = searchData.filter(item => {
    return searchTokens.every(token => item.search_terms.includes(token));
  });
  const top5 = filteredResults.slice(0, 5);
  
  if (top5.length === 0) {
    suggestionsContainer.innerHTML = '';
    suggestionsContainer.style.display = 'none';
    return;
  }
  suggestionsContainer.innerHTML = top5.map(item => `<a href='${generateLink(item.target_id)}'><strong>${item.Име}</strong> <span class='search-result-type'>(${item.Тип})</span></a>`).join('');
  suggestionsContainer.style.display = 'block';
}

function showSearchResults() {
  const input = document.getElementById('search-input');
  const suggestionsContainer = document.getElementById('search-suggestions');
  const mainContent = document.getElementById('main-content');
  
  if (!input || !mainContent) return;
  
  suggestionsContainer.style.display = 'none';
  const query = input.value.trim().toLowerCase();
  const originalQuery = input.value.trim();
  if (query.length < 2) {
    mainContent.innerHTML = `<h2>Резултати од пребарувањето</h2><p>Ве молиме внесете најмалку 2 карактери за да пребарувате.</p>${crear_botones_navegacion('..').outerHTML}`;
    return;
  }
  const searchTokens = query.split(' ').filter(t => t.length > 0);
  const results = searchData.filter(item => {
    return searchTokens.every(token => item.search_terms.includes(token));
  });
  let resultsHtml = `<h2>Резултати од пребарувањето за: "${originalQuery}"</h2>`;
  
  if (results.length > 0) {
    resultsHtml += '<div id="search-results-list"><ul>';
    results.forEach(item => {
      resultsHtml += `<li><a href="${generateLink(item.target_id)}">${item.Име}<span class="search-result-type">(${item.Тип})</span></a></li>`;
    });
    resultsHtml += '</ul></div>';
  } else {
    resultsHtml += `<p>Нема пронајдени резултати за "${originalQuery}".</p>`;
  }
  
  resultsHtml += `<div class="nav-buttons"><a href="${window.location.pathname}" class="back-link">← Врати се на претходната страница</a></div>`;
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
writeLines(script_js, file.path(RUTA_ASSETS, "script.js"))
message("Archivos style.css y script.js guardados en la carpeta assets.")

# --- 8.2: Definir el script de la contraseña (sin cambios) ---
script_contraseña <- tags$script(HTML(
  "(function() { var contraseñaCorrecta = 'FuckYouFFM'; var contraseñaIngresada = sessionStorage.getItem('zfudbalmk-password-ok'); if (contraseñaIngresada === contraseñaCorrecta) { return; } var input; var promptMessage = 'За да пристапите до извештајот, внесете ја лозинката:'; while (true) { input = prompt(promptMessage, ''); if (input === contraseñaCorrecta) { sessionStorage.setItem('zfudbalmk-password-ok', input); break; } if (input === null) { document.body.innerHTML = '<div style=\"text-align:center; padding: 50px; font-family: sans-serif;\"><h1>Пристапот е одбиен</h1><p>Процесот е откажан од страна на корисникот.</p></div>'; throw new Error('Access denied by user.'); } promptMessage = 'Погрешна лозинка. Обидете се повторно:'; } })();"
))

#######------------------------------------######
####### PASO 9: GENERACIÓN DE PÁGINAS HTML ######
#######------------------------------------######

# --- 9.1: Generación de la página del Portal (index.html) ---
message("Generando el archivo principal index.html (portal)...")
contenido_portal <- tags$div(
  id = "portal",
  tags$h2("Портал на натпреварувања"),
  tags$div(class = "portal-container",
           if (nrow(competiciones_unicas_df) > 0) {
             map(1:nrow(competiciones_unicas_df), function(i) {
               comp <- competiciones_unicas_df[i,]
               ### CAMBIO ###
               # Enlace apunta a la carpeta correcta en macedonio
               tags$a(href = file.path(nombres_carpetas_mk$competiciones, paste0(comp$competicion_id, ".html")),
                      class="portal-button", comp$nombre_completo)
             })
           } else { tags$p("Не се пронајдени натпреварувања.") }
  )
)
pagina_portal_final <- crear_pagina_html(
  contenido_principal = contenido_portal,
  titulo_pagina = "Фудбалски портал МК",
  ruta_relativa_assets = ".",
  search_data_json = search_data_json,
  script_contraseña = script_contraseña
)
save_html(pagina_portal_final, file = file.path(RUTA_BASE_SALIDA, "index.html"))

# --- 9.2: Generación de páginas por competición en un bucle ---
message("Генерирање на страници за секое натпреварување...")
walk(1:nrow(competiciones_unicas_df), function(i) {
  
  comp_info <- competiciones_unicas_df[i,]
  comp_id <- comp_info$competicion_id
  comp_nombre <- comp_info$nombre_completo
  is_cup <- str_detect(tolower(comp_info$competicion_nombre), "куп")
  
  # Filtramos los datos específicos para esta competición
  partidos_comp <- partidos_df %>% filter(competicion_nombre == comp_info$competicion_nombre, competicion_temporada == comp_info$competicion_temporada)
  ids_partidos_comp <- partidos_comp$id_partido
  goles_comp <- goles_df_unificado %>% filter(id_partido %in% ids_partidos_comp)
  tarjetas_comp <- tarjetas_df_unificado %>% filter(id_partido %in% ids_partidos_comp)
  apariciones_comp <- apariciones_df %>% filter(id_partido %in% ids_partidos_comp)
  
  # ==========================================================
  # 1. PÁGINA DE MENÚ DE LA COMPETICIÓN (Versión inicial sin el botón de porteras)
  # ==========================================================
  contenido_menu_botones_inicial <- if (is_cup) {
    tags$div(class="menu-container",
             tags$a(href=paste0(comp_id, "_", nombres_archivos_mk$partidos, ".html"), class="menu-button", "Распоред"),
             tags$a(href=paste0(comp_id, "_", nombres_archivos_mk$goleadoras, ".html"), class="menu-button", "Стрелци"),
             tags$a(href=paste0(comp_id, "_", nombres_archivos_mk$sanciones, ".html"), class="menu-button", "Дисциплинска"))
  } else {
    tags$div(class="menu-container",
             tags$a(href=paste0(comp_id, "_", nombres_archivos_mk$partidos, ".html"), class="menu-button", "Распоред"),
             tags$a(href=paste0(comp_id, "_", nombres_archivos_mk$clasificacion, ".html"), class="menu-button", "Табела"),
             tags$a(href=paste0(comp_id, "_", nombres_archivos_mk$goleadoras, ".html"), class="menu-button", "Стрелци"),
             tags$a(href=paste0(comp_id, "_", nombres_archivos_mk$sanciones, ".html"), class="menu-button", "Дисциплинска"))
  }
  
  contenido_menu_completo_inicial <- tagList(
    crear_botones_navegacion(ruta_relativa_assets = ".."),
    tags$h2(comp_nombre),
    contenido_menu_botones_inicial
  )
  
  pagina_menu_final_inicial <- crear_pagina_html(
    contenido_principal = contenido_menu_completo_inicial,
    titulo_pagina = comp_nombre, 
    ruta_relativa_assets = "..",
    search_data_json = search_data_json, 
    script_contraseña = script_contraseña
  )
  save_html(pagina_menu_final_inicial, file = file.path(RUTA_COMPETICIONES, paste0(comp_id, ".html")))
  
  # ==========================================================
  # 2. PÁGINA DE PARTIDOS/CALENDARIO 
  # ==========================================================
  jornadas_comp <- if (nrow(partidos_comp) > 0) {
    jornadas_unicas_df <- data.frame(jornada = unique(partidos_comp$jornada)) %>%
      mutate(order_key = case_when(str_detect(jornada, "1/16") ~ 1, str_detect(jornada, "1/8") ~ 2, str_detect(jornada, "1/4") ~ 3, str_detect(jornada, "1/2") ~ 4, str_detect(jornada, "Ф$|ф$|финале") ~ 5, !is_cup ~ as.numeric(suppressWarnings(jornada)), TRUE ~ 99)) %>%
      arrange(order_key)
    jornadas_unicas_df$jornada
  } else { c() }
  
  contenido_partidos <- tagList(
    crear_botones_navegacion(ruta_relativa_assets = ".."),
    tags$h2(paste("Распоред -", comp_nombre)),
    map(jornadas_comp, function(j) {
      partidos_jornada <- partidos_comp %>% filter(jornada == j) %>% arrange(local)
      header_text <- if(is_cup) as.character(j) else paste("Коло", j)
      tagList(
        tags$h3(class="jornada-header", header_text),
        map(1:nrow(partidos_jornada), function(k) {
          partido <- partidos_jornada[k,]
          resultado_texto <- paste(partido$goles_local,"-",partido$goles_visitante)
          if (isTRUE(partido$es_resultado_oficial)) {
            resultado_texto <- paste(resultado_texto, "*")
          }
          tags$a(class="partido-link", 
                 href=file.path("..", nombres_carpetas_mk$partidos, paste0(partido$id_partido, ".html")),
                 tags$span(class="equipo equipo-local", partido$local),
                 tags$span(class="resultado", resultado_texto),
                 tags$span(class="equipo equipo-visitante", partido$visitante))
        })
      )
    })
  )
  
  pagina_partidos_final <- crear_pagina_html(contenido_partidos, paste("Распоред -", comp_nombre), "..", search_data_json, script_contraseña)
  save_html(pagina_partidos_final, file.path(RUTA_COMPETICIONES, paste0(comp_id, "_", nombres_archivos_mk$partidos, ".html")))
  
  
  # ==========================================================
  # 3. PÁGINA DE CLASIFICACIÓN (si es liga) - (BLOQUE MODIFICADO)
  # ==========================================================
  if (!is_cup) {
    calcular_clasificacion <- function(partidos) { if (is.null(partidos) || nrow(partidos) == 0) return(data.frame(Порака = "Нема обработени валидни натпревари.")); locales <- partidos %>% select(equipo = local, GF = goles_local, GC = goles_visitante); visitantes <- partidos %>% select(equipo = visitante, GF = goles_visitante, GC = goles_local); resultados_por_equipo <- bind_rows(locales, visitantes) %>% mutate(Pts = case_when(GF > GC ~ 3, GF < GC ~ 0, TRUE ~ 1), resultado = case_when(GF > GC ~ "Поб", GF < GC ~ "Пор", TRUE ~ "Нер")); clasificacion <- resultados_por_equipo %>% group_by(Тим = equipo) %>% summarise(Н = n(), Бод. = sum(Pts), Поб = sum(resultado == "Поб"), Нер = sum(resultado == "Нер"), Пор = sum(resultado == "Пор"), ДГ = sum(GF), ПГ = sum(GC), .groups = 'drop') %>% mutate(ГР = ДГ - ПГ) %>% arrange(desc(Бод.), desc(ГР), desc(ДГ)) %>% mutate(Поз. = row_number()) %>% select(Поз., Тим, Н, Поб, Нер, Пор, ДГ, ПГ, ГР, Бод.); return(clasificacion)}
    clasificacion_df_comp <- calcular_clasificacion(partidos_comp)
    
    # --- INICIO DE LA LÓGICA DE ESTILOS PERSONALIZADOS ---
    estilos_comp <- estilos_clasificacion_data[[comp_nombre]]
    
    contenido_clasificacion <- tagList(
      crear_botones_navegacion(".."),
      tags$h2(paste("Табела -", comp_nombre)),
      tags$table(tags$thead(tags$tr(map(names(clasificacion_df_comp), tags$th))),
                 tags$tbody(map(1:nrow(clasificacion_df_comp), function(j) {
                   fila <- clasificacion_df_comp[j,]
                   
                   # Buscar si hay una regla para esta posición
                   regla_actual <- NULL
                   if (!is.null(estilos_comp)) {
                     regla_match <- estilos_comp$reglas %>% filter(puesto == fila$Поз.)
                     if (nrow(regla_match) > 0) {
                       regla_actual <- regla_match[1,]
                     }
                   }
                   
                   tags$tr(map(1:ncol(fila), function(k) {
                     cell_value <- fila[[k]]
                     
                     # Aplicar estilo solo a la primera celda (posición)
                     if (k == 1 && !is.null(regla_actual)) {
                       tags$td(style = paste0("border-left: 5px solid ", regla_actual$color, "; font-weight: bold;"), cell_value)
                     } else if (k == 2) { # Celda del equipo (con enlace)
                       tags$td(tags$a(href=file.path("..", nombres_carpetas_mk$equipos, paste0(generar_id_seguro(cell_value), ".html")), cell_value))
                     } else { # Otras celdas
                       tags$td(cell_value)
                     }
                   }))
                 }))),
      # Generar la leyenda si hay estilos definidos
      if (!is.null(estilos_comp) && length(estilos_comp$leyenda) > 0) {
        tags$div(class = "legend",
                 map(estilos_comp$leyenda, function(item_leyenda) {
                   tags$div(class = "legend-item",
                            tags$span(class = "legend-color-box", style = paste0("background-color: ", item_leyenda$color, ";")),
                            tags$span(item_leyenda$texto)
                   )
                 })
        )
      }
    )
    # --- FIN DE LA LÓGICA DE ESTILOS PERSONALIZADOS ---
    
    pagina_clasificacion_final <- crear_pagina_html(contenido_clasificacion, paste("Табела -", comp_nombre), "..", search_data_json, script_contraseña)
    save_html(pagina_clasificacion_final, file = file.path(RUTA_COMPETICIONES, paste0(comp_id, "_", nombres_archivos_mk$clasificacion, ".html")))
  }
  
  # ==========================================================
  # SECCIÓN: PÁGINA DE PORTERAS (GOLMANKI)
  # ==========================================================
  
  # Inicializamos las variables para evitar errores si no hay porteras
  porteras_mas_50 <- NULL
  porteras_menos_50 <- NULL
  
  # Filtramos solo las apariciones de porteras para esta competición
  porteras_comp_df <- apariciones_comp %>%
    filter(es_portera == TRUE, !is.na(id)) %>%
    select(id, nombre, equipo, id_partido, minutos_jugados, min_entra, min_sale)
  
  # Calculamos los minutos totales posibles para cada equipo
  minutos_totales_equipo <- partidos_comp %>%
    group_by(equipo = local) %>% summarise(n_partidos = n()) %>%
    bind_rows(partidos_comp %>% group_by(equipo = visitante) %>% summarise(n_partidos = n())) %>%
    group_by(equipo) %>%
    summarise(minutos_totales_posibles = sum(n_partidos) * 90)
  
  # Solo procedemos si hay datos de porteras y de minutos de equipos
  if (nrow(porteras_comp_df) > 0 && nrow(minutos_totales_equipo) > 0) {
    
    lista_filas_porteras <- list()
    
    for (portera_id in unique(porteras_comp_df$id)) {
      info_portera <- porteras_comp_df %>% filter(id == portera_id) %>% head(1)
      partidos_jugados_portera <- porteras_comp_df %>% filter(id == portera_id)
      goles_recibidos_total <- 0
      porterias_a_cero <- 0
      
      for (row in 1:nrow(partidos_jugados_portera)) {
        partido_actual <- partidos_jugados_portera[row, ]
        equipo_portera <- partido_actual$equipo
        id_partido_actual <- partido_actual$id_partido
        
        # Cálculo de goles recibidos (sin cambios)
        goles_recibidos_partido <- goles_comp %>%
          filter(id_partido == id_partido_actual, equipo_acreditado != equipo_portera)
        
        if (nrow(goles_recibidos_partido) > 0) {
          goles_mientras_jugaba <- goles_recibidos_partido %>%
            filter(minuto >= partido_actual$min_entra & minuto <= partido_actual$min_sale)
          goles_recibidos_total <- goles_recibidos_total + nrow(goles_mientras_jugaba)
        }
        
        # Lógica de porterías a cero
        goles_recibidos_equipo_partido <- partidos_comp %>%
          filter(id_partido == id_partido_actual) %>%
          mutate(goles_recibidos = if_else(local == equipo_portera, goles_visitante, goles_local)) %>%
          pull(goles_recibidos)
        
        if (goles_recibidos_equipo_partido == 0 && partido_actual$minutos_jugados > 0) {
          
          resumen_partido <- purrr::keep(resultados_exitosos, ~.x$partido_info$id_partido == id_partido_actual)[[1]]
          
          # ============================ INICIO DE LA CORRECCIÓN ============================
          # Seleccionamos las sustituciones SÓLO del equipo de la portera
          info_partido_base <- resumen_partido$partido_info
          es_local <- (equipo_portera == info_partido_base$local)
          
          cambios_equipo_portera <- if (es_local) resumen_partido$cambios_local else resumen_partido$cambios_visitante
          
          dorsales_cambios <- c()
          if (!is.null(cambios_equipo_portera) && nrow(cambios_equipo_portera) > 0) {
            dorsales_cambios <- unique(na.omit(c(as.numeric(str_match(cambios_equipo_portera$texto, "Entra .*?\\((\\d+)\\)")[, 2]), as.numeric(str_match(cambios_equipo_portera$texto, "por .*?\\((\\d+)\\)")[, 2]))))
          }
          # ============================ FIN DE LA CORRECCIÓN ============================
          
          dorsal_portera_partido <- apariciones_comp %>%
            filter(id_partido == id_partido_actual, id == portera_id) %>% pull(dorsal) %>% head(1)
          
          if (!(dorsal_portera_partido %in% dorsales_cambios)) {
            porterias_a_cero <- porterias_a_cero + 1
          }
        }
      }
      
      lista_filas_porteras[[length(lista_filas_porteras) + 1]] <- tibble(
        id = portera_id, Фудбалерка = info_portera$nombre, Тим = info_portera$equipo,
        Минути = sum(partidos_jugados_portera$minutos_jugados), ПГ = goles_recibidos_total, ЧМ = porterias_a_cero
      )
    }
    
    # El resto del código de esta sección no necesita cambios
    if (length(lista_filas_porteras) > 0) {
      tabla_porteras_comp <- bind_rows(lista_filas_porteras) %>%
        mutate(`ПГ/90` = if_else(Минути > 0, (ПГ / Минути) * 90, 0)) %>%
        left_join(minutos_totales_equipo, by = c("Тим" = "equipo")) %>%
        mutate(процент_минути = if_else(!is.na(minutos_totales_posibles) & minutos_totales_posibles > 0, (Минути / minutos_totales_posibles) * 100, 0),
               grupo = if_else(процент_минути >= 50, "mas_50", "menos_50")) %>%
        select(id, Фудбалерка, Тим, `ПГ/90`, ПГ, Минути, ЧМ, grupo)
      
      porteras_mas_50 <- tabla_porteras_comp %>%
        filter(grupo == "mas_50") %>% 
        arrange(`ПГ/90`, desc(ЧМ)) %>% 
        mutate(Поз. = row_number())
      
      porteras_menos_50 <- tabla_porteras_comp %>%
        filter(grupo == "menos_50", Минути > 0) %>% 
        arrange(`ПГ/90`, desc(ЧМ)) %>% 
        mutate(Поз. = row_number())
      
      # Función auxiliar para generar el HTML de la tabla
      generar_tabla_html_porteras <- function(df, table_id) {
        if (is.null(df) || nrow(df) == 0) return(tags$p("Нема податоци во оваа категорија."))
        tags$table(id = table_id, `data-sort-col` = "3", `data-sort-dir` = "asc",
                   tags$thead(tags$tr(tags$th("Поз."), tags$th("Фудбалерка"), tags$th("Тим"),
                                      tags$th(class="sortable-header asc", onclick=sprintf("sortTable('%s', 3)", table_id), "ПГ/90"),
                                      tags$th("ПГ"), tags$th("Минути"),
                                      tags$th(class="sortable-header", onclick=sprintf("sortTable('%s', 6)", table_id), "ЧМ"))),
                   tags$tbody(map(1:nrow(df), function(j){
                     p <- df[j,]; tags$tr(tags$td(p$Поз.),
                                          tags$td(tags$a(href=file.path("..", nombres_carpetas_mk$jugadoras, paste0(p$id, ".html")), p$Фудбалерка)),
                                          tags$td(tags$a(href=file.path("..", nombres_carpetas_mk$equipos, paste0(generar_id_seguro(p$Тим), ".html")), p$Тим)),
                                          tags$td(format(round(p$`ПГ/90`, 2), nsmall = 2)), tags$td(p$ПГ),
                                          tags$td(p$Минути), tags$td(p$ЧМ))
                   }))
        )
      }
      
      # Creamos el contenido de la página de porteras
      contenido_porteras <- tagList(
        crear_botones_navegacion(".."),
        tags$h2(paste("Статистика на голманки -", comp_nombre)),
        tags$h3("Фудбалерки со 50% или повеќе одиграни минути"),
        generar_tabla_html_porteras(porteras_mas_50, "tabla-porteras-mas-50"),
        tags$h3("Фудбалерки со помалку од 50% одиграни минути"),
        generar_tabla_html_porteras(porteras_menos_50, "tabla-porteras-menos-50")
      )
      pagina_porteras_final <- crear_pagina_html(contenido_porteras, paste("Голманки -", comp_nombre), "..", search_data_json, script_contraseña)
      save_html(pagina_porteras_final, file.path(RUTA_COMPETICIONES, paste0(comp_id, "_golmanki.html")))
      
      # Y AHORA, SOBREESCRIBIMOS EL MENÚ PARA AÑADIR EL NUEVO BOTÓN
      contenido_menu_botones_final <- if (is_cup) {
        tags$div(class="menu-container",
                 tags$a(href=paste0(comp_id, "_", nombres_archivos_mk$partidos, ".html"), class="menu-button", "Распоред"),
                 tags$a(href=paste0(comp_id, "_", nombres_archivos_mk$goleadoras, ".html"), class="menu-button", "Стрелци"),
                 tags$a(href=paste0(comp_id, "_golmanki.html"), class="menu-button", "Голманки"), # BOTÓN AÑADIDO
                 tags$a(href=paste0(comp_id, "_", nombres_archivos_mk$sanciones, ".html"), class="menu-button", "Дисциплинска"))
      } else {
        tags$div(class="menu-container",
                 tags$a(href=paste0(comp_id, "_", nombres_archivos_mk$partidos, ".html"), class="menu-button", "Распоред"),
                 tags$a(href=paste0(comp_id, "_", nombres_archivos_mk$clasificacion, ".html"), class="menu-button", "Табела"),
                 tags$a(href=paste0(comp_id, "_", nombres_archivos_mk$goleadoras, ".html"), class="menu-button", "Стрелци"),
                 tags$a(href=paste0(comp_id, "_golmanki.html"), class="menu-button", "Голманки"), # BOTÓN AÑADIDO
                 tags$a(href=paste0(comp_id, "_", nombres_archivos_mk$sanciones, ".html"), class="menu-button", "Дисциплинска"))
      }
      contenido_menu_completo_final <- tagList(
        crear_botones_navegacion(ruta_relativa_assets = ".."),
        tags$h2(comp_nombre),
        contenido_menu_botones_final)
      pagina_menu_final_actualizada <- crear_pagina_html(contenido_menu_completo_final, comp_nombre, "..", search_data_json, script_contraseña)
      save_html(pagina_menu_final_actualizada, file = file.path(RUTA_COMPETICIONES, paste0(comp_id, ".html")))
    }
  }
  
  
  # ==========================================================
  # 4. PÁGINA DE GOLEADORAS
  # ==========================================================
  
  # PASO 1: Calcular el total de goles por jugadora (lógica sin cambios)
  goles_por_jugadora_comp <- goles_comp %>% 
    filter(!is.na(id), tipo == "Normal") %>% 
    group_by(id) %>% 
    summarise(Голови = n(), .groups = 'drop')
  
  # PASO 2: Agregar la información de la jugadora, concatenando los equipos (lógica sin cambios)
  jugadoras_info_comp <- apariciones_comp %>%
    filter(!is.na(id)) %>%
    group_by(id) %>%
    summarise(
      Фудбалерка = first(nombre),
      Тим = paste(unique(equipo), collapse = " / "),
      .groups = 'drop'
    )
  
  # PASO 3: Unir la información agregada (lógica sin cambios)
  tabla_goleadoras_comp <- goles_por_jugadora_comp %>%
    left_join(jugadoras_info_comp, by = "id") %>%
    filter(!is.na(Фудбалерка)) %>%
    arrange(desc(Голови)) %>%
    mutate(Поз. = min_rank(desc(Голови))) %>%
    select(Поз., id, Фудбалерка, Тим, Голови)
  
  
  contenido_goleadoras <- tagList(
    crear_botones_navegacion(".."),
    tags$h2(paste("Листа на стрелци -", comp_nombre)),
    tags$table(
      tags$thead(tags$tr(map(names(tabla_goleadoras_comp %>% select(-id)), tags$th))),
      tags$tbody(map(1:nrow(tabla_goleadoras_comp), function(j){
        g <- tabla_goleadoras_comp[j,]
        tags$tr(
          tags$td(g$Поз.),
          tags$td(tags$a(href=file.path("..", nombres_carpetas_mk$jugadoras, paste0(g$id, ".html")), g$Фудбалерка)),
          
          # ============================ INICIO DE LA CORRECCIÓN ============================
          # Esta celda ahora maneja múltiples equipos, creando un enlace para cada uno.
          tags$td({
            # Dividimos la cadena de equipos en un vector
            teams <- str_split(g$Тим, " / ")[[1]]
            
            # Creamos una lista para almacenar las etiquetas HTML (enlaces y separadores)
            team_tags <- list()
            
            # Iteramos sobre cada nombre de equipo
            for (i in seq_along(teams)) {
              team_name <- teams[i]
              
              # Creamos un enlace <a> para el equipo actual
              team_link <- tags$a(
                href = file.path("..", nombres_carpetas_mk$equipos, paste0(generar_id_seguro(team_name), ".html")),
                team_name
              )
              
              # Añadimos el enlace a nuestra lista
              team_tags <- append(team_tags, list(team_link))
              
              # Si no es el último equipo de la lista, añadimos el separador
              if (i < length(teams)) {
                team_tags <- append(team_tags, list(" / "))
              }
            }
            # Devolvemos una lista de etiquetas que htmltools renderizará correctamente
            tagList(team_tags)
          }),
          # ============================ FIN DE LA CORRECCIÓN ============================
          
          tags$td(g$Голови)
        )
      }))
    )
  )
  pagina_goleadoras_final <- crear_pagina_html(contenido_goleadoras, paste("Стрелци -", comp_nombre), "..", search_data_json, script_contraseña)
  save_html(pagina_goleadoras_final, file.path(RUTA_COMPETICIONES, paste0(comp_id, "_", nombres_archivos_mk$goleadoras, ".html")))
  
  # ==========================================================
  # 5. PÁGINA DE SANCIONES
  # ==========================================================
  tarjetas_por_jugadora_comp <- tarjetas_comp %>% filter(!is.na(id)) %>% group_by(id) %>% summarise(Жолти=sum(tipo=="Amarilla",na.rm=T),Црвени=sum(tipo=="Roja",na.rm=T),.groups='drop')
  tabla_sanciones_comp <- tarjetas_por_jugadora_comp %>% left_join(jugadoras_info_comp, by = "id") %>% filter(!is.na(Фудбалерка), Жолти > 0 | Црвени > 0) %>% arrange(desc(Црвени), desc(Жолти)) %>% mutate(Поз. = min_rank(desc(Црвени * 1000 + Жолти))) %>% select(Поз., id, Фудбалерка, Тим, Жолти, Црвени)
  
  contenido_sanciones <- tagList(
    crear_botones_navegacion(".."),
    tags$h2(paste("Дисциплинска евиденција -", comp_nombre)),
    tags$table(
      tags$thead(tags$tr(tags$th("Поз."), tags$th("Фудбалерка"), tags$th("Тим"), tags$th(HTML("<span class='card-yellow'></span>")), tags$th(HTML("<span class='card-red'></span>")))),
      tags$tbody(if(nrow(tabla_sanciones_comp) > 0) {
        map(1:nrow(tabla_sanciones_comp), function(j) {
          s <- tabla_sanciones_comp[j,]
          tags$tr(
            tags$td(s$Поз.),
            tags$td(tags$a(href=file.path("..", nombres_carpetas_mk$jugadoras, paste0(s$id, ".html")), s$Фудбалерка)),
            tags$td(tags$a(href=file.path("..", nombres_carpetas_mk$equipos, paste0(generar_id_seguro(s$Тим), ".html")), s$Тим)),
            tags$td(s$Жолти),
            tags$td(s$Црвени)
          )
        })
      } else {
        tags$tr(tags$td(colspan="5", "Нема регистрирани картони."))
      })
    )
  )
  pagina_sanciones_final <- crear_pagina_html(contenido_sanciones, paste("Дисциплинска -", comp_nombre), "..", search_data_json, script_contraseña)
  save_html(pagina_sanciones_final, file.path(RUTA_COMPETICIONES, paste0(comp_id, "_", nombres_archivos_mk$sanciones, ".html")))
  
  # ====================================================================
  # 6. PÁGINA DE TRÍOS DEFENSIVOS (VERSIÓN 2.0 - REESCRITA)
  # ====================================================================
  message(paste("Computando tríos defensivos para:", comp_nombre))
  
  # PASO 1: Calcular los minutos totales posibles por equipo
  minutos_totales_equipo_comp <- partidos_comp %>%
    group_by(equipo = local) %>% summarise(n_partidos = n()) %>%
    bind_rows(partidos_comp %>% group_by(equipo = visitante) %>% summarise(n_partidos = n())) %>%
    group_by(equipo) %>%
    summarise(minutos_totales_posibles = sum(n_partidos) * 90, .groups = 'drop')
  
  # PASO 2: Identificar todas las apariciones de defensas
  defensas_comp_df <- apariciones_comp %>%
    left_join(posiciones_procesadas_df, by = "id") %>%
    filter(str_detect(posicion_final_mk, "Одбрана"), !is.na(min_entra), minutos_jugados > 0) %>%
    select(id_partido, equipo, id, nombre, min_entra, min_sale)
  
  # PASO 3: Calcular minutos compartidos por cada trío en cada partido
  trio_minutos_partido <- defensas_comp_df %>%
    group_by(id_partido, equipo) %>%
    filter(n() >= 3) %>%
    group_modify(~ {
      combn(.x$id, 3, simplify = FALSE) %>%
        map_dfr(function(trio_ids) {
          jugadoras_trio <- .x %>% filter(id %in% trio_ids)
          minutos_compartidos <- max(0, min(jugadoras_trio$min_sale) - max(jugadoras_trio$min_entra))
          tibble(trio_key = paste(sort(trio_ids), collapse = "-"),
                 minutos_compartidos = minutos_compartidos,
                 start_shared = max(jugadoras_trio$min_entra),
                 end_shared = min(jugadoras_trio$min_sale))
        })
    }) %>%
    ungroup() %>%
    filter(minutos_compartidos > 0)
  
  # Solo continuar si hay datos
  if(nrow(trio_minutos_partido) > 0) {
    
    # PASO 4: Agregar minutos totales y APLICAR EL FILTRO DEL 50%
    trios_con_minutos_totales <- trio_minutos_partido %>%
      group_by(equipo, trio_key) %>%
      summarise(minutos_totales_juntas = sum(minutos_compartidos), .groups = 'drop') %>%
      left_join(minutos_totales_equipo_comp, by = "equipo") %>%
      mutate(pct_minutos_jugados = minutos_totales_juntas / minutos_totales_posibles) %>%
      filter(pct_minutos_jugados >= 0.5)
    
    # PASO 5: Seleccionar el mejor trío de los que superaron el filtro
    mejores_trios_filtrados <- trios_con_minutos_totales %>%
      group_by(equipo) %>%
      slice_max(order_by = minutos_totales_juntas, n = 1, with_ties = FALSE) %>%
      ungroup()
    
    # Solo continuar si algún trío superó el filtro
    if(nrow(mejores_trios_filtrados) > 0) {
      
      
      # PASO 6: CALCULAR GOLES RECIBIDOS (MÉTODO CORRECTO)
      
      # 1. Preparamos un dataframe que, para cada gol, sepa quién lo marcó Y QUIÉN LO RECIBIÓ.
      #    Usamos 'partidos_comp' para obtener la información del rival.
      goles_con_rival_df <- goles_comp %>%
        left_join(partidos_comp %>% select(id_partido, local, visitante), by = "id_partido") %>%
        mutate(
          equipo_que_recibio_gol = case_when(
            # Si el equipo que marcó es el local, el que recibió fue el visitante
            equipo_acreditado == local ~ visitante,
            # Si el equipo que marcó es el visitante, el que recibió fue el local
            equipo_acreditado == visitante ~ local,
            # En caso de autogol, el equipo que recibió es el acreditado con el gol
            tipo == "Autogol" ~ equipo_acreditado,
            TRUE ~ NA_character_
          )
        ) %>%
        filter(!is.na(equipo_que_recibio_gol)) %>%
        select(id_partido, equipo_conceded = equipo_que_recibio_gol, minuto_gol = minuto)
      
      # 2. Ahora, el cálculo de goles es el mismo que antes, pero con el dataframe correcto.
      goles_calculados <- pmap_dfr(mejores_trios_filtrados, function(...) {
        trio_actual <- list(...)
        intervalos_del_trio <- trio_minutos_partido %>%
          filter(trio_key == trio_actual$trio_key, equipo == trio_actual$equipo)
        
        goles_en_intervalos <- pmap_dbl(intervalos_del_trio, function(...) {
          intervalo_actual <- list(...)
          goles_contados <- goles_con_rival_df %>%
            filter(id_partido == intervalo_actual$id_partido,
                   equipo_conceded == intervalo_actual$equipo,
                   minuto_gol >= intervalo_actual$start_shared,
                   minuto_gol <= intervalo_actual$end_shared) %>%
            nrow()
          return(goles_contados)
        })
        
        tibble(
          equipo = trio_actual$equipo,
          trio_key = trio_actual$trio_key,
          goles_recibidos_juntas = sum(goles_en_intervalos)
        )
      })
      
      # PASO 7: Preparar la tabla final para el HTML
      id_nombre_map <- apariciones_comp %>% distinct(id, nombre)
      
      # ============================ INICIO DE LA CORRECCIÓN ============================
      
      tabla_final_defensas <- mejores_trios_filtrados %>%
        left_join(goles_calculados, by = c("equipo", "trio_key")) %>%
        mutate(goles_recibidos_juntas = replace_na(goles_recibidos_juntas, 0)) %>%
        # 1. CREAMOS LAS COLUMNAS CON SUS NOMBRES FINALES PRIMERO
        mutate(
          `Клуб` = equipo,
          `Минути заедно` = minutos_totales_juntas,
          `Примени голови` = goles_recibidos_juntas,
          `Примени/90мин` = if_else(minutos_totales_juntas > 0, (goles_recibidos_juntas / minutos_totales_juntas) * 90, 0)
        ) %>%
        rowwise() %>%
        mutate(
          `Трио` = paste(sapply(strsplit(trio_key, "-")[[1]], function(id) id_nombre_map$nombre[id_nombre_map$id == id][1]), collapse = " - ")
        ) %>%
        ungroup() %>%
        # 2. AHORA QUE LAS COLUMNAS EXISTEN, PODEMOS ORDENAR POR ELLAS
        arrange(`Примени/90мин`, `Примени голови`) %>%
        # 3. FINALMENTE, SELECCIONAMOS LAS COLUMNAS EN EL ORDEN DESEADO
        mutate(`Поз.` = row_number()) %>%
        select(`Поз.`, `Трио`, `Клуб`, `Минути заедно`, `Примени голови`, `Примени/90мин`, trio_key)
      
      
      # PASO 8: Generar el HTML con la tabla desplegable (VERSIÓN FINAL)
      contenido_defensas <- tagList(
        crear_botones_navegacion(".."),
        tags$h2(paste("Најдобро дефанзивно трио -", comp_nombre)),
        tags$p(style="text-align:center; font-style:italic; color:#555;", 
               "Дефанзивното трио од секој тим кое одиграло најмалку 50 % од можните минути заедно."),
        tags$table(class = "main-summary-table",
                   tags$thead(tags$tr(map(names(tabla_final_defensas %>% select(-trio_key)), tags$th))),
                   tags$tbody(
                     # Iteramos sobre cada fila del dataframe de la tabla final
                     pmap(tabla_final_defensas, function(...) {
                       fila <- list(...)
                       details_id <- paste0("details-trio-", fila$Поз.) # Usar la posición como ID único
                       
                       partidos_del_trio <- trio_minutos_partido %>%
                         filter(trio_key == fila$trio_key) %>%
                         left_join(partidos_comp, by = "id_partido") %>%
                         select(id_partido, local, visitante, goles_local, goles_visitante, minutos_compartidos)
                       
                       tagList(
                         tags$tr(class = "summary-row", onclick = sprintf("toggleDetails('%s')", details_id),
                                 tags$td(fila$Поз.),
                                 tags$td(fila$Трио),
                                 tags$td(tags$a(href=file.path("..", nombres_carpetas_mk$equipos, paste0(generar_id_seguro(fila$Клуб), ".html")), 
                                                onclick="event.stopPropagation();", fila$Клуб)),
                                 tags$td(round(fila$`Минути заедно`)),
                                 tags$td(fila$`Примени голови`),
                                 tags$td(format(round(fila$`Примени/90мин`, 2), nsmall = 2))
                         ),
                         tags$tr(id = details_id, class = "details-row",
                                 tags$td(colspan = "6", 
                                         tags$div(class = "details-content",
                                                  tags$h4("Одиграни натпревари заедно"),
                                                  tags$table(class="details-table",
                                                             tags$thead(tags$tr(tags$th("Натпревар"), tags$th("Резултат"), tags$th("Минути заедно"))),
                                                             tags$tbody(
                                                               map(1:nrow(partidos_del_trio), function(p_idx) {
                                                                 partido <- partidos_del_trio[p_idx,]
                                                                 enlace_partido <- file.path("..", nombres_carpetas_mk$partidos, paste0(partido$id_partido, ".html"))
                                                                 tags$tr(class = "clickable-row", `data-href` = enlace_partido,
                                                                         tags$td(paste(partido$local, "vs", partido$visitante)),
                                                                         tags$td(paste(partido$goles_local, "-", partido$goles_visitante)),
                                                                         tags$td(round(partido$minutos_compartidos))
                                                                 )
                                                               })
                                                             )
                                                  )
                                         )
                                 )
                         )
                       )
                     })
                   )
        )
      )
      
      # Guardar la página HTML y actualizar el menú (esta parte no cambia)
      nombre_archivo_defensas <- paste0(comp_id, "_defanzivno_trio.html")
      save_html(crear_pagina_html(contenido_defensas, paste("Дефанзивно трио -", comp_nombre), "..", search_data_json, script_contraseña),
                file.path(RUTA_COMPETICIONES, nombre_archivo_defensas))
      
      
      # CORRECCIÓN: Volvemos a generar el menú de la competición para añadir el nuevo botón.
      # Esta es una forma robusta de hacerlo, sin manipular los tags directamente.
      nuevo_boton_defensa <- tags$a(href=nombre_archivo_defensas, class="menu-button", "Дефанзивно трио")
      
      botones_menu <- if (is_cup) {
        tagList(
          tags$a(href=paste0(comp_id, "_", nombres_archivos_mk$partidos, ".html"), class="menu-button", "Распоред"),
          tags$a(href=paste0(comp_id, "_", nombres_archivos_mk$goleadoras, ".html"), class="menu-button", "Стрелци"),
          # Aquí puedes decidir el orden de los botones
          nuevo_boton_defensa,
          tags$a(href=paste0(comp_id, "_golmanki.html"), class="menu-button", "Голманки"),
          tags$a(href=paste0(comp_id, "_", nombres_archivos_mk$sanciones, ".html"), class="menu-button", "Дисциплинска")
        )
      } else {
        tagList(
          tags$a(href=paste0(comp_id, "_", nombres_archivos_mk$partidos, ".html"), class="menu-button", "Распоред"),
          tags$a(href=paste0(comp_id, "_", nombres_archivos_mk$clasificacion, ".html"), class="menu-button", "Табела"),
          tags$a(href=paste0(comp_id, "_", nombres_archivos_mk$goleadoras, ".html"), class="menu-button", "Стрелци"),
          nuevo_boton_defensa,
          tags$a(href=paste0(comp_id, "_golmanki.html"), class="menu-button", "Голманки"),
          tags$a(href=paste0(comp_id, "_", nombres_archivos_mk$sanciones, ".html"), class="menu-button", "Дисциплинска")
        )
      }
      
      contenido_menu_final <- tagList(
        crear_botones_navegacion(ruta_relativa_assets = ".."),
        tags$h2(comp_nombre),
        tags$div(class="menu-container", botones_menu)
      )
      
      save_html(crear_pagina_html(contenido_menu_final, comp_nombre, "..", search_data_json, script_contraseña), 
                file = file.path(RUTA_COMPETICIONES, paste0(comp_id, ".html")))
    }
    
  }
})




# --- 9.3: Generación de páginas globales (perfiles) ---
message("Генерирање на страници за секој натпревар, фудбалерка, тим, судија и стадион...")

# Enriquecemos partidos_df con el competicion_id para poder generar los enlaces
partidos_df_enriquecido <- partidos_df %>%
  left_join(
    competiciones_unicas_df %>% select(competicion_nombre, competicion_temporada, competicion_id),
    by = c("competicion_nombre", "competicion_temporada")
  )

# ====================================================================
# Bucle para PARTIDOS 
# ====================================================================
walk(1:nrow(partidos_df_enriquecido), function(i) {
  
  partido <- partidos_df_enriquecido[i, ]
  id_p <- partido$id_partido
  
  # Recopilación de todos los datos necesarios para la página del partido
  resumen_partido <- purrr::keep(resultados_exitosos, ~.x$partido_info$id_partido == id_p)[[1]]
  cronologia <- generar_cronologia_df(id_p, resumen_partido) 
  arbitros_partido <- arbitros_df %>% filter(id_partido == id_p)
  estadio_info <- estadios_df %>% filter(id_partido == id_p) %>% head(1)
  goles_partido <- goles_df_unificado %>% filter(id_partido == id_p)
  tarjetas_partido <- tarjetas_df_unificado %>% filter(id_partido == id_p)
  alineacion_partido <- apariciones_df %>% filter(id_partido == id_p)
  
  ### NUEVO BLOQUE DE LÓGICA: Determinar el texto de la jornada/ronda ###
  is_cup_match <- str_detect(tolower(partido$competicion_nombre), "куп")
  jornada_texto <- if(is_cup_match) partido$jornada else paste("Коло", partido$jornada)
  
  # Función interna para renderizar las alineaciones (con la lógica de eventos actualizada)
  render_equipo_html <- function(df_equipo, goles_del_partido, tarjetas_del_partido) {
    if (is.null(df_equipo) || nrow(df_equipo) == 0) return(tags$p("Нема податоци."))
    starters <- df_equipo %>% filter(tipo == "Titular"); subs <- df_equipo %>% filter(tipo == "Suplente")
    
    crear_lista_jugadoras <- function(df_j) {
      if (nrow(df_j) == 0) return(tags$p(style = "color:#777;", "Нема."))
      
      # --- INICIO DE LA CORRECCIÓN: Usar pmap con argumentos nombrados ---
      tags$ul(
        pmap(df_j, function(id, nombre, dorsal, tipo, equipo, es_portera, es_capitana, min_entra, min_sale, minutos_jugados, ...) {
          eventos_html <- tagList()
          
          # Goles
          goles_jugadora <- goles_del_partido %>% filter(id == !!id, tipo == "Normal")
          if (nrow(goles_jugadora) > 0) {
            walk(1:nrow(goles_jugadora), function(g) {
              gol <- goles_jugadora[g,]
              eventos_html <<- tagAppendChild(eventos_html, tags$span(class = "player-event goal", HTML(paste0("⚽︎ ", formatear_minuto_partido(gol$minuto), "'"))))
            })
          }
          
          # Tarjetas
          tarjetas_jugadora <- tarjetas_del_partido %>% filter(id == !!id)
          if (nrow(tarjetas_jugadora) > 0) {
            walk(1:nrow(tarjetas_jugadora), function(c) {
              tarjeta <- tarjetas_jugadora[c,]
              card_span <- tags$span(class = if (tarjeta$tipo == "Amarilla") "card-yellow" else "card-red")
              eventos_html <<- tagAppendChild(eventos_html, tags$span(class = "player-event", card_span, HTML(paste0("︎ ", formatear_minuto_partido(tarjeta$minuto), "'"))))
            })
          }
          
          # Sustituciones
          if (!is.na(min_entra) && tipo == "Suplente") {
            eventos_html <- tagAppendChild(eventos_html, tags$span(class = "player-event sub-in", paste0("↑", min_entra, "'")))
          }
          if (!is.na(min_sale) && min_sale < 90 && !is.na(minutos_jugados) && minutos_jugados > 0) {
            eventos_html <- tagAppendChild(eventos_html, tags$span(class = "player-event sub-out", paste0("↓", min_sale, "'")))
          }
          
          icono_p <- if (isTRUE(es_portera)) "🧤" else ""
          icono_c <- if (isTRUE(es_capitana)) "(C)" else ""
          
          tags$li(paste0(dorsal, ". "), tags$a(href = file.path("..", nombres_carpetas_mk$jugadoras, paste0(id, ".html")), nombre), icono_p, icono_c, eventos_html)
        })
      )
      # --- FIN DE LA CORRECCIÓN ---
    }
    tagList(tags$h4("Почетен состав"), crear_lista_jugadoras(starters), tags$h4("Резерви"), crear_lista_jugadoras(subs))
  }
  
  # Creación del contenido HTML de la página del partido
  contenido_partido <- tagList(
    crear_botones_navegacion(".."),
    tags$h2(paste(partido$local, "vs", partido$visitante)),
    
    tags$p(style = "text-align:center; font-size: 1.1em; color: #555; margin-top: -15px; margin-bottom: 20px;",
           tags$a(href = file.path("..", nombres_carpetas_mk$competiciones, paste0(partido$competicion_id, ".html")),
                  paste(partido$competicion_nombre, partido$competicion_temporada)),
           " - ",
           jornada_texto
    ),
    
    tags$h3({
      resultado_texto <- paste("Конечен резултат:", partido$goles_local, "-", partido$goles_visitante)
      if (isTRUE(partido$es_resultado_oficial)) {
        resultado_texto <- paste(resultado_texto, "(службен резултат)")
      }
      resultado_texto
    }),
    tags$p(paste0("Датум: ", partido$fecha, " | Време: ", partido$hora, " | Стадион: "), if (nrow(estadio_info) > 0) tags$a(href = file.path("..", nombres_carpetas_mk$estadios, paste0(generar_id_seguro(estadio_info$estadio), ".html")), estadio_info$estadio) else "Непознат"),
    tags$h3("Судии"),
    tags$ul(class = "sudii-lista", map(1:nrow(arbitros_partido), function(a) { arb <- arbitros_partido[a, ]; tags$li(paste0(arb$uloga, ": "), tags$a(href = file.path("..", nombres_carpetas_mk$arbitros, paste0(generar_id_seguro(arb$ime), ".html")), arb$ime)) })),
    tags$h3("Состави"),
    tags$div(class = "alineaciones-container",
             tags$div(class = "columna-alineacion", tags$h3(style = "text-align:center;border:none;", tags$a(href = file.path("..", nombres_carpetas_mk$equipos, paste0(generar_id_seguro(partido$local), ".html")), partido$local)), render_equipo_html(filter(alineacion_partido, equipo == partido$local), goles_partido, tarjetas_partido)),
             tags$div(class = "columna-alineacion", tags$h3(style = "text-align:center;border:none;", tags$a(href = file.path("..", nombres_carpetas_mk$equipos, paste0(generar_id_seguro(partido$visitante), ".html")), partido$visitante)), render_equipo_html(filter(alineacion_partido, equipo == partido$visitante), goles_partido, tarjetas_partido))),
    tags$h3("Хронологија"),
    tags$ul(class = "timeline", if (exists("cronologia") && nrow(cronologia) > 0) { map(1:nrow(cronologia), function(c) { e <- cronologia[c, ]; tags$li(HTML(paste0("<span class='icon'>", e$icono, "</span>")), paste0(formatear_minuto_partido(e$minuto), "' - "), HTML(e$texto_evento)) }) } else { tags$li("Нема регистрирани настани.") }),
    crear_botones_navegacion("..")
  )
  
  # Generación final de la página HTML del partido
  pagina_partido_final <- crear_pagina_html(contenido_partido, paste(partido$local, "vs", partido$visitante), "..", search_data_json, script_contraseña)
  save_html(pagina_partido_final, file = file.path(RUTA_PARTIDOS, paste0(id_p, ".html")))
})


# Bucle para JUGADORAS
walk(1:nrow(jugadoras_stats_df), function(i) {
  jugadora <- jugadoras_stats_df[i,]; id_j <- jugadora$id
  player_career_summary <- apariciones_df %>% filter(id == id_j) %>% group_by(competicion_temporada, competicion_nombre, equipo) %>% summarise(Повикана = n_distinct(id_partido), Одиграни = sum(minutos_jugados > 0, na.rm = TRUE), Почетен_состав = sum(tipo == "Titular", na.rm = TRUE), Минути = sum(minutos_jugados, na.rm = TRUE), .groups = 'drop')
  player_goals_summary <- goles_df_unificado %>% filter(id == id_j, tipo == "Normal") %>% left_join(partidos_df %>% select(id_partido, competicion_temporada, competicion_nombre), by = "id_partido") %>% group_by(competicion_temporada, competicion_nombre, equipo_jugadora) %>% summarise(Голови = n(), .groups = 'drop') %>% rename(equipo = equipo_jugadora)
  player_cards_summary <- tarjetas_df_unificado %>% filter(id == id_j) %>% left_join(partidos_df %>% select(id_partido, competicion_temporada, competicion_nombre), by = "id_partido") %>% group_by(competicion_temporada, competicion_nombre, equipo) %>% summarise(Жолти = sum(tipo == "Amarilla", na.rm = TRUE), Црвени = sum(tipo == "Roja", na.rm = TRUE), .groups = 'drop')
  player_career_final <- player_career_summary %>% left_join(player_goals_summary, by = c("competicion_temporada", "competicion_nombre", "equipo")) %>% left_join(player_cards_summary, by = c("competicion_temporada", "competicion_nombre", "equipo")) %>% mutate(Голови = replace_na(Голови, 0), Жолти = replace_na(Жолти, 0), Црвени = replace_na(Црвени, 0)) %>% arrange(desc(competicion_temporada))
  
  contenido_jugadora <- tagList(
    crear_botones_navegacion(".."),
    
    tags$h2(style = "display: flex; align-items: center; gap: 15px;",
            jugadora$Фудбалерка,
            
            # Lógica para la bandera (utiliza las columnas unidas desde los archivos de texto)
            if (!is.na(jugadora$codigo_iso)) {
              # Si no hay traducción macedonia disponible, usa el nombre original como fallback
              texto_emergente <- if_else(!is.na(jugadora$nombre_macedonio), jugadora$nombre_macedonio, jugadora$nacionalidad)
              
              url_bandera <- paste0("https://kapowaz.github.io/square-flags/flags/", jugadora$codigo_iso, ".svg")
              tags$img(
                src = url_bandera,
                alt = texto_emergente,
                title = texto_emergente,
                style = "height: 0.9em; width: auto; border: 1px solid #ccc;"
              )
            },
            
            # Mostramos la edad
            if (!is.na(jugadora$edad)) {
              tags$span(style = "font-size: 0.6em; color: #555; vertical-align: middle; font-weight: normal;", 
                        paste0(jugadora$edad, " год.")
              )
            },
            
            # Mostramos la posición (como antes)
            if (!is.na(jugadora$posicion_final_mk)) {
              tags$span(style = "font-size: 0.6em; color: #555; vertical-align: middle; font-weight: normal;", 
                        jugadora$posicion_final_mk)
            }
    ),
    
    tags$h3("Резиме на кариера"),
    tags$table(class="career-summary-table", tags$thead(tags$tr(tags$th("Сезона"), tags$th("Тим"), tags$th("Натпреварување"), tags$th("Наст."), tags$th("Гол."), tags$th("Мин."))),
               tags$tbody(map(1:nrow(player_career_final), function(j) {
                 stage <- player_career_final[j,]; details_id <- paste0("details-", id_j, "-", j)
                 partidos_stage <- apariciones_df %>% filter(id == id_j, competicion_temporada == stage$competicion_temporada, competicion_nombre == stage$competicion_nombre, equipo == stage$equipo) %>% left_join(partidos_df, by="id_partido")
                 goles_stage <- goles_df_unificado %>% filter(id == id_j, id_partido %in% partidos_stage$id_partido)
                 tarjetas_stage <- tarjetas_df_unificado %>% filter(id == id_j, id_partido %in% partidos_stage$id_partido)
                 tagList(
                   tags$tr(class="summary-row", onclick=sprintf("toggleDetails('%s')", details_id),
                           tags$td(stage$competicion_temporada),
                           tags$td(tags$a(href=file.path("..", nombres_carpetas_mk$equipos, paste0(generar_id_seguro(stage$equipo), ".html")), onclick="event.stopPropagation();", stage$equipo)),
                           tags$td(stage$competicion_nombre), tags$td(stage$Одиграни), tags$td(stage$Голови), tags$td(stage$Минути)),
                   tags$tr(id=details_id, class="details-row", tags$td(colspan="6", tags$div(class="details-content",
                                                                                             tags$h4("Детална статистика"), tags$table(tags$tbody(tags$tr(tags$td("Тим"), tags$td(stage$equipo)), tags$tr(tags$td("Повикана"), tags$td(stage$Повикана)), tags$tr(tags$td("Одиграни"), tags$td(stage$Одиграни)), tags$tr(tags$td("Почетен состав"), tags$td(stage$Почетен_состав)), tags$tr(tags$td("Минути"), tags$td(stage$Минути)), tags$tr(tags$td("Голови"), tags$td(stage$Голови)), tags$tr(tags$td("Жолти картони"), tags$td(stage$Жолти)), tags$tr(tags$td("Црвени картони"), tags$td(stage$Црвени)))),
                                                                                             tags$h4("Список на натпревари"), tags$table(tags$thead(tags$tr(tags$th("Коло"),tags$th("Натпревар"),tags$th("Резултат"),tags$th("Статус"), tags$th("Мин."))), tags$tbody(if(nrow(partidos_stage)>0) { map(1:nrow(partidos_stage),function(p_idx){ partido_row <- partidos_stage[p_idx,]; status_partido <- if (partido_row$tipo == "Titular") "Почетен состав" else if (!is.na(partido_row$minutos_jugados) && partido_row$minutos_jugados > 0) "Резерва (одигра)" else "Повикана"; tags$tr(tags$td(partido_row$jornada), tags$td(tags$a(href=file.path("..", nombres_carpetas_mk$partidos, paste0(partido_row$id_partido, ".html")),paste(partido_row$local,"vs",partido_row$visitante))), tags$td(paste(partido_row$goles_local,"-",partido_row$goles_visitante)), tags$td(status_partido), tags$td(if(is.na(partido_row$minutos_jugados)) 0 else partido_row$minutos_jugados)) }) } else { tags$tr(tags$td(colspan="5","Нема одиграни натпревари.")) })),
                                                                                             tags$h4("Список на голови"), tags$table(tags$thead(tags$tr(tags$th("Коло"), tags$th("Натпревар"), tags$th("Минута"))), tags$tbody(if(nrow(goles_stage)>0){ map(1:nrow(goles_stage), function(g_idx){ goal_row <- goles_stage[g_idx,]; g_partido<-filter(partidos_df, id_partido==goal_row$id_partido); tags$tr(tags$td(g_partido$jornada), tags$td(tags$a(href=file.path("..", nombres_carpetas_mk$partidos, paste0(goal_row$id_partido, ".html")),paste(g_partido$local,"vs",g_partido$visitante))), tags$td(formatear_minuto_partido(goal_row$minuto)))}) } else { tags$tr(tags$td(colspan="3","Нема постигнато голови.")) })),
                                                                                             tags$h4("Список на картони"), tags$table(tags$thead(tags$tr(tags$th("Коло"),tags$th("Натпревар"),tags$th("Картон"),tags$th("Минута"),tags$th("Причина"))), tags$tbody(if(nrow(tarjetas_stage)>0){ map(1:nrow(tarjetas_stage),function(t_idx){ card_row <- tarjetas_stage[t_idx,]; t_partido<-filter(partidos_df, id_partido==card_row$id_partido); icon<-if(card_row$tipo=="Amarilla")tags$span(class="card-yellow")else tags$span(class="card-red");tags$tr(tags$td(t_partido$jornada),tags$td(tags$a(href=file.path("..", nombres_carpetas_mk$partidos, paste0(card_row$id_partido, ".html")),paste(t_partido$local,"vs",t_partido$visitante))),tags$td(icon),tags$td(formatear_minuto_partido(card_row$minuto)),tags$td(card_row$motivo))}) } else { tags$tr(tags$td(colspan="5","Нема добиено картони.")) })))
                   )))
               }))
    )
  )
  pagina_jugadora_final <- crear_pagina_html(contenido_jugadora, jugadora$Фудбалерка, "..", search_data_json, script_contraseña)
  save_html(pagina_jugadora_final, file = file.path(RUTA_JUGADORAS, paste0(id_j, ".html")))
})

# Bucle para EQUIPOS
walk(unique(c(partidos_df$local, partidos_df$visitante)), function(team) {
  id_t <- generar_id_seguro(team)
  historial_equipo <- partidos_df %>% filter(local == team | visitante == team) %>% mutate(fecha_date = as.Date(fecha, format = "%d.%m.%Y"))
  temporadas_summary <- historial_equipo %>% group_by(competicion_temporada, competicion_nombre) %>% summarise(last_match_date = max(fecha_date, na.rm = TRUE), .groups = 'drop') %>% arrange(desc(last_match_date))
  
  contenido_equipo <- tagList(
    crear_botones_navegacion(".."),
    tags$h2(team), tags$h3("Историја по натпреварувања"),
    tags$table(class="team-career-summary", tags$thead(tags$tr(tags$th("Сезона"), tags$th("Натпреварување"))),
               tags$tbody(map(1:nrow(temporadas_summary), function(j) {
                 stage <- temporadas_summary[j,]; details_id <- paste0("details-", id_t, "-", j)
                 historial_stage <- historial_equipo %>% filter(competicion_temporada == stage$competicion_temporada, competicion_nombre == stage$competicion_nombre) %>% arrange(fecha_date)
                 ids_partidos_stage <- historial_stage$id_partido
                 stats_jugadoras_stage <- apariciones_df %>% filter(id_partido %in% ids_partidos_stage, equipo == team) %>% group_by(id, nombre) %>% summarise(Повикана = n_distinct(id_partido), Одиграни = sum(minutos_jugados > 0, na.rm = TRUE), Минути = sum(minutos_jugados, na.rm = TRUE), .groups = 'drop')
                 goles_stage <- goles_df_unificado %>% filter(id_partido %in% ids_partidos_stage, equipo_jugadora == team) %>% group_by(id) %>% summarise(Голови = n(), .groups = 'drop')
                 tarjetas_stage <- tarjetas_df_unificado %>% filter(id_partido %in% ids_partidos_stage, equipo == team) %>% group_by(id) %>% summarise(Жолти = sum(tipo == "Amarilla", na.rm = TRUE), Црвени = sum(tipo == "Roja", na.rm = TRUE), .groups = 'drop')
                 stats_final_stage <- stats_jugadoras_stage %>% left_join(goles_stage, by = "id") %>% left_join(tarjetas_stage, by = "id") %>% mutate(across(c(Голови, Жолти, Црвени), ~replace_na(., 0))) %>% select(id, Фудбалерка = nombre, Повикана, Одиграни, Минути, Голови, Жолти, Црвени) %>% arrange(desc(Минути))
                 headers_stats <- c("Фудбалерка", "Пов", "Одиг", "Мин", "Гол", "Ж", "Ц")
                 tagList(
                   tags$tr(class="summary-row", onclick=sprintf("toggleDetails('%s')", details_id), tags$td(stage$competicion_temporada), tags$td(stage$competicion_nombre)),
                   tags$tr(id = details_id, class="details-row", tags$td(colspan="2", tags$div(class="details-content",
                                                                                               tags$h4("Статистика на фудбалерки"), tags$table(tags$thead(tags$tr(map(headers_stats, tags$th))), tags$tbody(if(nrow(stats_final_stage) > 0) { map(1:nrow(stats_final_stage), function(p_idx) { p <- stats_final_stage[p_idx,]; tags$tr(tags$td(tags$a(href=file.path("..", nombres_carpetas_mk$jugadoras, paste0(p$id, ".html")), p$Фудбалерка)), tags$td(p$Повикана), tags$td(p$Одиграни), tags$td(p$Минути), tags$td(p$Голови), tags$td(p$Жолти), tags$td(p$Црвени)) }) } else { tags$tr(tags$td(colspan=length(headers_stats), "Нема податоци за фудбалерки.")) })),
                                                                                               tags$h4("Список на натпревари"), tags$table(tags$thead(tags$tr(tags$th("Коло"), tags$th("Датум"), tags$th("Домаќин"), tags$th("Гостин"), tags$th("Резултат"))), tags$tbody(map(1:nrow(historial_stage), function(p_idx) { partido <- historial_stage[p_idx,]; tags$tr(tags$td(partido$jornada), tags$td(partido$fecha), tags$td(partido$local), tags$td(partido$visitante), tags$td(tags$a(href=file.path("..", nombres_carpetas_mk$partidos, paste0(partido$id_partido, ".html")), paste(partido$goles_local, "-", partido$goles_visitante)))) })))
                   )))
                 )
               })))
  )
  pagina_equipo_final <- crear_pagina_html(contenido_equipo, team, "..", search_data_json, script_contraseña)
  save_html(pagina_equipo_final, file = file.path(RUTA_EQUIPOS, paste0(id_t, ".html")))
})

# Bucle para ARBITROS
walk(unique(arbitros_df$ime), function(arb) {
  id_a <- generar_id_seguro(arb)
  historial_arbitro <- arbitros_df %>% filter(ime == arb) %>% left_join(partidos_df, by = "id_partido") %>% mutate(fecha_date = as.Date(fecha, format = "%d.%m.%Y"))
  temporadas_summary <- historial_arbitro %>% group_by(competicion_temporada, competicion_nombre) %>% summarise(last_match_date = max(fecha_date, na.rm = TRUE), num_matches = n(), .groups = 'drop') %>% arrange(desc(last_match_date))
  
  contenido_arbitro <- tagList(
    crear_botones_navegacion(".."),
    tags$h2(arb), tags$h3("Историја по натпреварувања"),
    tags$table(tags$thead(tags$tr(tags$th("Сезона"), tags$th("Натпреварување"), tags$th("Натпревари"))),
               tags$tbody(if (nrow(temporadas_summary) > 0) {
                 map(1:nrow(temporadas_summary), function(j) {
                   stage <- temporadas_summary[j,]; details_id <- paste0("details-arbitro-", id_a, "-", j)
                   historial_stage <- historial_arbitro %>% filter(competicion_temporada == stage$competicion_temporada, competicion_nombre == stage$competicion_nombre) %>% arrange(desc(fecha_date))
                   tagList(
                     tags$tr(class = "summary-row", onclick = sprintf("toggleDetails('%s')", details_id), tags$td(stage$competicion_temporada), tags$td(stage$competicion_nombre), tags$td(stage$num_matches)),
                     tags$tr(id = details_id, class = "details-row", tags$td(colspan = "3", tags$div(class = "details-content",
                                                                                                     ### CAMBIO ###
                                                                                                     tags$table(tags$thead(tags$tr(tags$th("Датум"), tags$th("Коло"), tags$th("Натпревар"), tags$th("Резултат"), tags$th("Улога"))),
                                                                                                                tags$tbody(map(1:nrow(historial_stage), function(p_idx) {
                                                                                                                  partido <- historial_stage[p_idx,]; tags$tr(tags$td(partido$fecha), tags$td(partido$jornada), tags$td(tags$a(href=file.path("..", nombres_carpetas_mk$partidos, paste0(partido$id_partido, ".html")), paste(partido$local, "vs", partido$visitante))), tags$td(paste(partido$goles_local, "-", partido$goles_visitante)), tags$td(partido$uloga))
                                                                                                                })))
                     )))
                   )
                 })
               } else { tags$tr(tags$td(colspan="3", "Нема делегирани натпревари.")) }))
  )
  pagina_arbitro_final <- crear_pagina_html(contenido_arbitro, arb, "..", search_data_json, script_contraseña)
  ### CAMBIO ###
  save_html(pagina_arbitro_final, file = file.path(RUTA_ARBITROS, paste0(id_a, ".html")))
})

# Bucle para ESTADIOS
walk(unique(na.omit(estadios_df$estadio)), function(est) {
  id_e <- generar_id_seguro(est)
  historial <- estadios_df %>% filter(estadio == est) %>% mutate(fecha_date = as.Date(fecha, format = "%d.%m.%Y")) %>% arrange(desc(fecha_date))
  
  contenido_estadio <- tagList(
    crear_botones_navegacion(".."),
    tags$h2(est), tags$h3("Историја на натпревари"),
    tags$table(tags$thead(tags$tr(tags$th("Датум"), tags$th("Сезона"), tags$th("Натпреварување"), tags$th("Коло"), tags$th("Натпревар"), tags$th("Резултат"))),
               tags$tbody(if (nrow(historial) > 0) {
                 map(1:nrow(historial), function(p_idx) {
                   ### CAMBIO ###
                   partido <- historial[p_idx, ]; tags$tr(tags$td(partido$fecha), tags$td(partido$competicion_temporada), tags$td(partido$competicion_nombre), tags$td(partido$jornada), tags$td(tags$a(href=file.path("..", nombres_carpetas_mk$partidos, paste0(partido$id_partido, ".html")), paste(partido$local, "vs", partido$visitante))), tags$td(paste(partido$goles_local, "-", partido$goles_visitante)))
                 })
               } else { tags$tr(tags$td(colspan = "6", "Нема одиграни натпревари на овој стадион.")) }))
  )
  pagina_estadio_final <- crear_pagina_html(contenido_estadio, est, "..", search_data_json, script_contraseña)
  ### CAMBIO ###
  save_html(pagina_estadio_final, file = file.path(RUTA_ESTADIOS, paste0(id_e, ".html")))
})

# =========================================================================
# MENSAJE FINAL
# =========================================================================
message(paste("\nPROCESO COMPLETADO CON ÉXITO!"))
message(paste("Se ha creado un sitio web estático en la carpeta:", RUTA_BASE_SALIDA))
message("Abre el archivo 'index.html' de esa carpeta para empezar a navegar.")