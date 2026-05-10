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
ruta_estilos_clasificacion <- if (file.exists("estilos_clasificacion.txt")) "estilos_clasificacion.txt" else "dictionaries/estilos_clasificacion.txt"
estilos_clasificacion_data <- parsear_estilos_clasificacion(ruta_estilos_clasificacion)


### 9.1b. Load comps_ffm.xlsx \u2014 Competition Category Master (Source of Truth)
# "\u041a\u0440\u0430\u0442\u043a\u043e Ime" (col 3) = raw name WITH season e.g. "\u041f\u0440\u0432\u0430 \u041c\u0424\u041b 25/26".
# "\u041a\u0430\u0442\u0435\u0433\u043e\u0440\u0438\u0458\u0430" (col 16) = authoritative age category (SOURCE OF TRUTH for this fix).
# 10_data_processing.R builds lookup key as:
#   tolower(str_squish(paste(competicion_nombre, competicion_temporada)))
# which equals tolower(str_squish("\u041a\u0440\u0430\u0442\u043a\u043e Ime")) since that already has the season.
# STRATEGY:
#   Primary:    \u041a\u0440\u0430\u0442\u043a\u043e Ime WITH season       \u2192 "\u041f\u0440\u0432\u0430 \u041c\u0424\u041b 25/26"   (main match path)
#   Long-year:  same but "2025/26" format    \u2192 "\u041f\u0440\u0432\u0430 \u041c\u0424\u041b 2025/26" (scrape.R may return 4-digit)
#   Secondary:  \u041a\u0440\u0430\u0442\u043a\u043e Ime WITHOUT season    \u2192 "\u041f\u0440\u0432\u0430 \u041c\u0424\u041b"         (fallback)
message("Loading comps_ffm.xlsx - competition category master...")
df_comps_ffm <- NULL
mapa_categorias_competiciones <- tibble(
  competicion_nombre  = character(),
  categoria           = character(),
  "vid_natprevaruvanje" = character(),
  min_age             = numeric(),
  max_age             = numeric()
)

ruta_comps_ffm <- if (file.exists("comps_ffm.xlsx")) "comps_ffm.xlsx" else "dictionaries/comps_ffm.xlsx"
if (file.exists(ruta_comps_ffm)) {
  tryCatch({
    df_comps_ffm <- read_excel(ruta_comps_ffm)

    # Use COLUMN INDICES \u2014 robust across locales; no Cyrillic regex needed.
    # Verified layout of comps_ffm.xlsx:
    #   [3]  \u041a\u0440\u0430\u0442\u043a\u043e Ime           (short name WITH season)
    #   [10] \u0412\u0438\u0434 \u043d\u0430 natprevaruvanje (competition type)
    #   [16] \u041a\u0430\u0442\u0435\u0433\u043e\u0440\u0438\u0458\u0430           (age category)
    #   [22] Minimum age
    #   [23] Maximum age
    n_cols         <- ncol(df_comps_ffm)
    kratko_ime_col <- if (n_cols >= 3)  names(df_comps_ffm)[3]  else NA_character_
    vid_col        <- if (n_cols >= 10) names(df_comps_ffm)[10] else NA_character_
    kategorija_col <- if (n_cols >= 16) names(df_comps_ffm)[16] else NA_character_
    min_age_col    <- if (n_cols >= 22) names(df_comps_ffm)[22] else NA_character_
    max_age_col    <- if (n_cols >= 23) names(df_comps_ffm)[23] else NA_character_

    if (anyNA(c(kratko_ime_col, kategorija_col))) {
      warning("comps_ffm.xlsx: required columns (3 and 16) not found. Category mapping skipped.")
    } else {

      raw_map <- df_comps_ffm %>%
        transmute(
          kratko_ime = trimws(as.character(.data[[kratko_ime_col]])),
          categoria  = trimws(as.character(.data[[kategorija_col]])),
          vid        = if (!is.na(vid_col))
                         trimws(as.character(.data[[vid_col]]))
                       else NA_character_,
          min_age    = if (!is.na(min_age_col))
                         suppressWarnings(as.numeric(.data[[min_age_col]]))
                       else NA_real_,
          max_age    = if (!is.na(max_age_col))
                         suppressWarnings(as.numeric(.data[[max_age_col]]))
                       else NA_real_
        ) %>%
        filter(!is.na(kratko_ime) & kratko_ime != "",
               !is.na(categoria)  & categoria  != "" & categoria != "NA")

      # Helper: strip the trailing season suffix (long patterns BEFORE short ones)
      strip_season_suffix <- function(x) {
        x %>%
          str_remove("\\s+\\d{4}-\\d{4}\\s*$") %>%  # "2011-2012" (before 2-digit)
          str_remove("\\s+\\d{4}/\\d{4}\\s*$") %>%  # "2025/2026"
          str_remove("\\s+\\d{4}/\\d{2}\\s*$") %>%  # "2025/26"
          str_remove("\\s+\\d{2}/\\d{2}\\s*$") %>%  # "25/26"
          str_squish()
      }

      # === PRIMARY: \u041a\u0440\u0430\u0442\u043a\u043e Ime WITH season (e.g. "\u041f\u0440\u0432\u0430 \u041c\u0424\u041b 25/26") ===
      primary_rows <- raw_map %>%
        transmute(
          competicion_nombre  = kratko_ime,
          categoria           = categoria,
          "vid_natprevaruvanje" = vid,
          min_age             = min_age,
          max_age             = max_age
        )

      # === LONG-YEAR rows: cover "\u041f\u0440\u0432\u0430 \u041c\u0424\u041b 2025/26" in case scrape.R returns 4-digit year ===
      long_year_rows <- primary_rows %>%
        mutate(
          short_s = str_extract(competicion_nombre, "(?<=\\s)\\d{2}/\\d{2}\\s*$"),
          short_s = trimws(short_s),
          long_s  = if_else(
            !is.na(short_s) & nchar(short_s) > 0,
            paste0("20", substr(short_s, 1, 2), "/", substr(short_s, 4, 5)),
            NA_character_
          ),
          # mapply allows per-row substitution; no fixed=TRUE so $ is a regex anchor
          competicion_nombre = mapply(function(nom, ss, ls) {
            if (is.na(ss) || is.na(ls) || nchar(ss) == 0) return(nom)
            sub(paste0(ss, "\\s*$"), ls, nom, perl = TRUE)
          }, competicion_nombre, short_s, long_s, SIMPLIFY = TRUE, USE.NAMES = FALSE)
        ) %>%
        filter(!is.na(short_s) & nchar(short_s) > 0) %>%
        select(-short_s, -long_s)

      # === SECONDARY: \u041a\u0440\u0430\u0442\u043a\u043e Ime WITHOUT season (fallback) ===
      secondary_rows <- raw_map %>%
        mutate(base_name = strip_season_suffix(kratko_ime)) %>%
        filter(base_name != kratko_ime) %>%
        transmute(
          competicion_nombre  = base_name,
          categoria           = categoria,
          "vid_natprevaruvanje" = vid,
          min_age             = min_age,
          max_age             = max_age
        )

      mapa_categorias_competiciones <- bind_rows(primary_rows, long_year_rows, secondary_rows) %>%
        filter(!is.na(competicion_nombre) & competicion_nombre != "") %>%
        distinct(competicion_nombre, .keep_all = TRUE)

      message(paste(
        "comps_ffm.xlsx loaded:", nrow(raw_map), "raw rows |",
        nrow(primary_rows), "primary (with season) |",
        nrow(long_year_rows), "long-year variants |",
        nrow(secondary_rows), "secondary (no season) |",
        "Total:", nrow(mapa_categorias_competiciones), "mapping entries"
      ))
    }
  }, error = function(e) {
    warning("Error loading comps_ffm.xlsx. Competition categories will not be assigned.")
    message("Error: ", e$message)
  })
} else {
  warning("comps_ffm.xlsx not found. Competition categories will not be assigned.")
}


### 9.2. Load Nationality Mappings
message("Loading nationality mappings...")

ruta_mapeo_iso <- if (file.exists("nacionalidades_mapeo.txt")) "nacionalidades_mapeo.txt" else "dictionaries/nacionalidades_mapeo.txt"
ruta_traduccion_mk <- if (file.exists("nacionalidades_traduccion.txt")) "nacionalidades_traduccion.txt" else "dictionaries/nacionalidades_traduccion.txt"
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

ruta_conversiones <- if (file.exists("conversions.txt")) "conversions.txt" else "dictionaries/conversions.txt"
# El mapa ahora se guarda como un dataframe para la nueva funci\u00f3n `aplicar_conversiones`.
mapa_conversiones_df <- NULL 
if (file.exists(ruta_conversiones)) {
  tryCatch({
    conversiones_df <- read.csv(ruta_conversiones, stringsAsFactors = FALSE, encoding = "UTF-8")
    
    # CLAVE: Crear un dataframe de mapeo que es case-insensitive y permite duplicados en la entrada.
    mapa_conversiones_df <- conversiones_df %>%
      # Convertir la columna 'original' a min\u00fasculas y normalizar espacios para el cruce.
      mutate(
        original_lower = normalize_for_join(original),
        corregido = trimws(corregido)
      ) %>%
      # Seleccionar solo las columnas necesarias.
      select(original_lower, corregido) %>%
      # Asegurarse de que el mapa en s\u00ed no tenga filas duplicadas.
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

mapa_nombres_jugadoras_long <- cargar_mapa_traduccion(if (file.exists("name_corrections.txt")) "name_corrections.txt" else "dictionaries/name_corrections.txt")
mapa_nombres_entidades_long <- cargar_mapa_traduccion(if (file.exists("entity_corrections.txt")) "entity_corrections.txt" else "dictionaries/entity_corrections.txt")
mapa_nombres_competiciones_long <- cargar_mapa_traduccion(if (file.exists("competitions.txt")) "competitions.txt" else "dictionaries/competitions.txt")

### 9.6.2. Build competition name normalization from comps_ffm and competitions.txt
message("Building competition name normalization map from comps_ffm and competitions.txt...")
mapa_competiciones_nombres <- tibble(original = character(), normalized = character())

# 1) Auto map from comps_ffm \u041a\u0440\u0430\u0442\u043a\u043e Ime -> normalized (TitleCase)
if (exists("df_comps_ffm") && !is.null(df_comps_ffm) && "\u041a\u0440\u0430\u0442\u043a\u043e Ime" %in% names(df_comps_ffm)) {
  mapa_competiciones_nombres <- df_comps_ffm %>%
    transmute(
      original = trimws(as.character(.data$"\u041a\u0440\u0430\u0442\u043a\u043e Ime")),
      normalized = trimws(str_to_title(str_to_lower(as.character(.data$"\u041a\u0440\u0430\u0442\u043a\u043e Ime"))))
    ) %>%
    filter(!is.na(original) & original != "") %>%
    distinct()
  message(paste("Auto-mapped", nrow(mapa_competiciones_nombres), "competitions from comps_ffm."))
} else {
  message("No comps_ffm mapping available for competitions.")
}

# 2) Override/add with explicit rules from competitions.txt
ruta_competiciones_map <- "competitions.txt"
if (file.exists(ruta_competiciones_map)) {
  tryCatch({
    competitions_df_raw <- read.csv(
      ruta_competiciones_map,
      stringsAsFactors = FALSE,
      encoding = "UTF-8",
      check.names = FALSE
    )
    competitions_df <- data.frame(
      original = competitions_df_raw[[1]],
      normalized = competitions_df_raw[[2]],
      stringsAsFactors = FALSE
    )

    competitions_df <- competitions_df %>%
      mutate(
        original = trimws(gsub('(^\"|\"$)', '', original)),
        normalized = trimws(gsub('(^\"|\"$)', '', normalized))
      ) %>%
      filter(!is.na(original) & original != "") %>%
      distinct(original, normalized)

    mapa_competiciones_nombres <- bind_rows(competitions_df, mapa_competiciones_nombres) %>%
      distinct(original, .keep_all = TRUE)

    message(paste("Loaded", nrow(competitions_df), "competition name rules from competitions.txt.",
                  "Effective mapping now", nrow(mapa_competiciones_nombres), "entries."))
  }, error = function(e) {
    warning("Error loading competitions.txt. Competition name normalization from file will be skipped."); message("Error was: ", e$message)
  })
} else {
  message("competitions.txt file not found. Using only auto mapping from comps_ffm (if any).")
}

# Build a lookup vector for original competition names to normalized competition names.
# This allows category assignment to use the original raw source name for matching.
mapa_competiciones_nombres_lookup <- if (nrow(mapa_competiciones_nombres) > 0) {
  nombres <- mapa_competiciones_nombres %>%
    # Use a narrower regex that only targets slashes for season suffixes (YY/YY or YYYY/YY or YYYY/YYYY)
    # AND strip any group label parentheses for dictionary matching.
    mutate(original_clean = stringr::str_to_lower(trimws(gsub("\\s*\\([^)]*\\)\\s*$", "", str_replace(original, "\\s*(?:\\d{4}/\\d{4}|\\d{2}/\\d{2}|\\d{4}/\\d{2})\\s*$", "")))))
  setNames(nombres$normalized, nombres$original_clean)
} else {
  character()
}

mapear_nombre_competicion_original <- function(name) {
  name_chr <- as.character(name)
  if (length(name_chr) == 0 || length(mapa_competiciones_nombres_lookup) == 0) {
    return(name_chr)
  }
  # Refined regex: strip only season formats with slashes at the end and group label parentheses.
  key <- stringr::str_to_lower(trimws(gsub("\\s*\\([^)]*\\)\\s*$", "", str_replace(name_chr, "\\s*(?:\\d{4}/\\d{4}|\\d{2}/\\d{2}|\\d{4}/\\d{2})\\s*$", ""))))
  mapped <- unname(mapa_competiciones_nombres_lookup[key])
  mapped[is.na(mapped)] <- name_chr[is.na(mapped)]
  mapped[is.na(mapped)] <- name_chr[is.na(mapped)]
  mapped
}

# Load optional competitions disambiguation file (defines groups for identically-named competitions)
ruta_competiciones_desambiguacion <- "competitions_disambiguation.txt"
desambiguacion_competiciones_df <- NULL
if (file.exists(ruta_competiciones_desambiguacion)) {
  tryCatch({
    desamb_raw <- read.delim(
      ruta_competiciones_desambiguacion,
      header = TRUE,
      sep = "	",
      stringsAsFactors = FALSE,
      encoding = "UTF-8",
      comment.char = "#",
      na.strings = c("", "NA")
    )

    names(desamb_raw) <- tolower(names(desamb_raw))
    if (!"original" %in% names(desamb_raw) && "original_name" %in% names(desamb_raw)) {
      desamb_raw <- dplyr::rename(desamb_raw, original = original_name)
    }
    if (!"group" %in% names(desamb_raw) && "grupo" %in% names(desamb_raw)) {
      desamb_raw <- dplyr::rename(desamb_raw, group = grupo)
    }
    if (!"teams" %in% names(desamb_raw) && "equipos" %in% names(desamb_raw)) {
      desamb_raw <- dplyr::rename(desamb_raw, teams = equipos)
    }
    if (!"season" %in% names(desamb_raw)) {
      desamb_raw$season <- NA_character_
    }

    desamb_raw <- desamb_raw %>%
      mutate(
        original = trimws(as.character(original)),
        season = trimws(as.character(season)),
        group = trimws(as.character(group)),
        teams = trimws(as.character(teams))
      ) %>%
      filter(!is.na(original) & original != "")

    if (nrow(desamb_raw) > 0) {
      desamb_raw <- desamb_raw %>%
        mutate(
            original_clean = tolower(trimws(str_replace(original, "\\s*(?:\\d{4}-\\d{4}|\\d{2}/\\d{2})\\s*$", ""))),
            teams_list = stringr::str_split(teams, "\\|")
        )
      desambiguacion_competiciones_df <- desamb_raw
      message(paste("Loaded competitions disambiguation:", nrow(desambiguacion_competiciones_df), "rows."))
    } else {
      message("competitions_disambiguation.txt present but no valid rows found.")
    }
  }, error = function(e) {
    warning("Error loading competitions_disambiguation.txt. Competition disambiguation disabled.")
  })
} else {
  message("competitions_disambiguation.txt not found; competition disambiguation disabled.")
}

### 9.5. Load Entity Name Corrections (Teams, Referees)
message("Loading entity name corrections file...")

ruta_correcciones_entidades <- if (file.exists("entity_corrections.txt")) "entity_corrections.txt" else "dictionaries/entity_corrections.txt"
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

ruta_traducciones_comp <- if (file.exists("competition_translations.txt")) "competition_translations.txt" else "dictionaries/competition_translations.txt"
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
      # Seleccionar las columnas finales, asegurando que 'fecha' y 'hora' est\u00e1n presentes
      select(competicion_nombre, competicion_temporada, jornada, fecha, hora, local, visitante)
  })
}


### 9.8. Load ID Unification Mappings
message("Loading ID unification mappings...")

ruta_unificacion_id <- if (file.exists("id_unification.txt")) "id_unification.txt" else "dictionaries/id_unification.txt"
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

ruta_traducciones_paises <- if (file.exists("country_translations.txt")) "country_translations.txt" else "dictionaries/country_translations.txt"
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

ruta_abreviaturas <- if (file.exists("abbreviations.txt")) "abbreviations.txt" else "dictionaries/abbreviations.txt"
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



### 9.10.1. Load Club Latin Name Corrections
message("Loading club latin name corrections from clubs_latin.txt...")

ruta_clubs_latin <- if (file.exists("clubs_latin.txt")) "clubs_latin.txt" else "dictionaries/clubs_latin.txt"
mapa_clubs_latin <- NULL
if (file.exists(ruta_clubs_latin)) {
  tryCatch({
    clubs_latin_df <- read.csv(
      ruta_clubs_latin,
      stringsAsFactors = FALSE,
      encoding = "UTF-8",
      header = FALSE,
      col.names = c("original", "normalized"),
      check.names = FALSE
    ) %>%
      mutate(
        original = trimws(original),
        normalized = trimws(normalized)
      ) %>%
      filter(!is.na(original) & original != "") %>%
      distinct(original, normalized)

    mapa_clubs_latin <- clubs_latin_df

    clubs_slug_map <- clubs_latin_df %>%
      mutate(
        original = tolower(str_squish(original)),
        normalized = str_trim(normalized)
      ) %>%
      filter(!is.na(original), original != "", !is.na(normalized), normalized != "") %>%
      {
        setNames(.$normalized, .$original)
      }

    combined_map <- c(mapa_nombres_latinos, clubs_slug_map)
    combined_map <- combined_map[!is.na(names(combined_map)) & names(combined_map) != ""]
    combined_map <- combined_map[!is.na(combined_map) & trimws(combined_map) != ""]
    mapa_nombres_latinos <- combined_map[!duplicated(names(combined_map), fromLast = TRUE)]
    message(paste("Loaded", nrow(mapa_clubs_latin), "club latin correction rows."))

    # Re-export logos after loading clubs_latin so logo filenames follow
    # the same slug rules as team URLs.
    if (exists("copiar_logos_compartidos") && is.function(copiar_logos_compartidos)) {
      copiar_logos_compartidos()
      message("Logos re-exported using clubs_latin.txt mapping.")
    }
  }, error = function(e) {
    warning("Error loading clubs_latin.txt. Club latin corrections will be skipped."); message("Error was: ", e$message)
  })
} else {
  message("clubs_latin.txt file not found. Club latin corrections will be skipped.")
}

### 9.11. Load Player Role Overrides
message("Loading player role overrides...")

ruta_roles_forzados <- if (file.exists("player_role_overrides.txt")) "player_role_overrides.txt" else "dictionaries/player_role_overrides.txt"
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

### 9.12. Load Homonym Disambiguation
message("Loading homonym disambiguation rules...")

ruta_desambiguacion <- if (file.exists("desambiguacion.txt")) "desambiguacion.txt" else "dictionaries/desambiguacion.txt"
desambiguacion_df <- NULL
if (file.exists(ruta_desambiguacion)) {
  tryCatch({
    desambiguacion_df <- read.csv(
      ruta_desambiguacion,
      stringsAsFactors = FALSE,
      encoding = "UTF-8",
      colClasses = "character"
    )
    if (nrow(desambiguacion_df) > 0) {
      message(paste("Disambiguation rules loaded with", nrow(desambiguacion_df), "entries."))
    } else {
      message("Disambiguation file is empty. No disambiguation will be applied.")
      desambiguacion_df <- NULL
    }
  }, error = function(e) {
    warning("Error loading desambiguacion.txt. No disambiguation will be applied.")
  })
} else {
  message("desambiguacion.txt not found. Continuing without disambiguation.")
}

### 9.13. Load City Translations
message("Loading city name translations...")

ruta_traducciones_ciudades <- if (file.exists("gradovi.txt")) "gradovi.txt" else "dictionaries/gradovi.txt"
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



### 9.14. Build mapa_nombres_latinos for URL generation
message("Building global mapa_nombres_latinos for URL slugs...")
mapa_nombres_latinos_env <- new.env(hash = TRUE)

if (exists("mapa_clubs_latin") && !is.null(mapa_clubs_latin)) {
  for (i in seq_len(nrow(mapa_clubs_latin))) {
    k <- tolower(stringr::str_squish(mapa_clubs_latin$original[i]))
    mapa_nombres_latinos_env[[k]] <- as.character(mapa_clubs_latin$normalized[i])
  }
}

ruta_nc <- if (file.exists("name_corrections.txt")) "name_corrections.txt" else "dictionaries/name_corrections.txt"
if (file.exists(ruta_nc)) {
  nc_df <- read.csv(ruta_nc, stringsAsFactors = FALSE, encoding = "UTF-8")
  if ("mk" %in% names(nc_df)) {
    for (i in seq_len(nrow(nc_df))) {
      latin_val <- NA_character_
      if ("en" %in% names(nc_df) && !is.na(nc_df$en[i]) && nzchar(trimws(nc_df$en[i]))) {
        latin_val <- nc_df$en[i]
      } else if ("sq" %in% names(nc_df) && !is.na(nc_df$sq[i]) && nzchar(trimws(nc_df$sq[i]))) {
        latin_val <- nc_df$sq[i]
      }
      if (!is.na(nc_df$mk[i]) && nzchar(trimws(nc_df$mk[i])) && !is.na(latin_val)) {
        k <- tolower(stringr::str_squish(nc_df$mk[i]))
        mapa_nombres_latinos_env[[k]] <- as.character(latin_val)
      }
    }
  }
}

mapa_nombres_latinos <<- unlist(as.list(mapa_nombres_latinos_env))
message(paste("Built mapa_nombres_latinos with", length(mapa_nombres_latinos), "entries."))
