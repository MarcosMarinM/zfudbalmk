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

