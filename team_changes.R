################################################################################
##                                                                            ##
##      MARKET MOVEMENT DETECTION SCRIPT (CONFIGURABLE & CACHE-BASED)       ##
##                                                                            ##
################################################################################


#### 1. PARAMETERS TO MODIFY ####
#
# English comments for shareability, as requested.
#
# Instructions:
# 1. Set the season strings for comparison.
# 2. Define which leagues and how many matchdays to check in the PAST season.
# 3. Define which leagues and which specific matchdays to check in the NEW season.

# -- General Settings --
PAST_SEASON   <- "24/25"
NEW_SEASON    <- "25/26"

# -- Analysis Scope for the PAST Season --
PAST_SEASON_LEAGUES <- c(
  "Прва ЖФЛ",
  "Втора ЖФЛ",
  "Младинска Женска Фудбалска Лига"
)
# How many of the LAST matchdays from the past season should be analyzed?
# (e.g., 2 means the last two, 5 means the last five)
PAST_SEASON_MATCHDAYS_TO_ANALYZE <- 5

# -- Analysis Scope for the NEW Season --
NEW_SEASON_LEAGUES <- c(
  "Прва ЖФЛ"
)
# Which specific matchdays from the new season should be analyzed?
# (e.g., 1 means only the first, 1:3 means the first three, c(1, 4) means first and fourth)
NEW_SEASON_MATCHDAYS_TO_ANALYZE <- 4


#### 2. SCRIPT INITIALIZATION ####

# 2.1. Load necessary packages.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, purrr, stringr)

# 2.2. Load processed data from the cache.
message("Loading processed data from cache...")
ruta_cache_resultados <- "actas_cache.rds"
if (!file.exists(ruta_cache_resultados)) {
  stop(paste("Cache file '", ruta_cache_resultados, "' not found.\nPlease run the main website generation script (Script 2) first."))
}
resultados_exitosos <- readRDS(ruta_cache_resultados)
message("Cache data loaded successfully.")


#### 3. DATA CONSOLIDATION ####

message("Consolidating data for analysis...")
partidos_df <- purrr::map_dfr(resultados_exitosos, "partido_info")
apariciones_df <- purrr::map_dfr(resultados_exitosos, ~{
  info <- .x$partido_info
  bind_rows(
    .x$alineacion_local %>% mutate(id_partido = info$id_partido, equipo = info$local),
    .x$alineacion_visitante %>% mutate(id_partido = info$id_partido, equipo = info$visitante)
  )
})


#### 4. ANALYSIS EXECUTION ####

# 4.1. Filter match IDs based on the parameters defined in Section 1.
message("Filtering matches from the selected periods...")

# Past Season: Last N matchdays of each specified league.
ids_partidos_temporada_anterior <- PAST_SEASON_LEAGUES %>%
  map_dfr(~{
    partidos_liga_temp <- partidos_df %>%
      filter(competicion_nombre == .x, competicion_temporada == PAST_SEASON)
    if (nrow(partidos_liga_temp) == 0) return(NULL)
    
    max_jornada <- suppressWarnings(as.numeric(partidos_liga_temp$jornada)) %>%
      max(na.rm = TRUE)
    if (is.infinite(max_jornada)) return(NULL)
    
    # Calculate the range of matchdays to analyze based on the parameter
    jornadas_objetivo <- (max_jornada - PAST_SEASON_MATCHDAYS_TO_ANALYZE + 1):max_jornada
    
    partidos_liga_temp %>%
      filter(as.numeric(jornada) %in% jornadas_objetivo) %>%
      select(id_partido)
  }) %>%
  pull(id_partido)

# New Season: Specific matchdays of specified leagues.
ids_partidos_temporada_nueva <- partidos_df %>%
  filter(competicion_nombre %in% NEW_SEASON_LEAGUES, 
         competicion_temporada == NEW_SEASON,
         as.numeric(jornada) %in% NEW_SEASON_MATCHDAYS_TO_ANALYZE) %>%
  pull(id_partido)

# 4.2. Create player tables for each period and detect changes.
message("Analyzing player movements between seasons...")

# Table of players from the past season period (last team they played for)
jugadoras_temporada_anterior <- apariciones_df %>%
  filter(id_partido %in% ids_partidos_temporada_anterior, !is.na(id)) %>%
  distinct(id, nombre, equipo) %>%
  group_by(id, nombre) %>%
  summarise(equipo_anterior = last(equipo), .groups = 'drop')

# Table of players from the new season period
jugadoras_temporada_nueva <- apariciones_df %>%
  filter(id_partido %in% ids_partidos_temporada_nueva, !is.na(id)) %>%
  distinct(id, nombre, equipo) %>%
  rename(equipo_nuevo = equipo)

# Join both tables and find the differences
fichajes_detectados <- jugadoras_temporada_anterior %>%
  inner_join(jugadoras_temporada_nueva, by = c("id", "nombre")) %>%
  filter(equipo_anterior != equipo_nuevo) %>%
  select(id, nombre, equipo_anterior, equipo_nuevo) %>%
  arrange(nombre)


#### 5. OUTPUT GENERATION ####

ruta_salida_txt <- file.path(Sys.getenv("HOME"), "Downloads", "teamchanges.txt")

# 5.1. Create the text content.
lineas_de_texto <- c(
  "===========================================================",
  paste("MARKET MOVEMENTS DETECTED BETWEEN", PAST_SEASON, "AND", NEW_SEASON),
  "===========================================================\n",
  "Players who have changed teams based on the analyzed match reports:\n"
)

if (nrow(fichajes_detectados) > 0) {
  lineas_fichajes <- pmap_chr(fichajes_detectados, function(id, nombre, equipo_anterior, equipo_nuevo) {
    sprintf("  - ID: %-7s | %-25s | %s -> %s", id, nombre, equipo_anterior, equipo_nuevo)
  })
  lineas_de_texto <- c(lineas_de_texto, lineas_fichajes)
} else {
  lineas_de_texto <- c(lineas_de_texto, "  No team changes were detected in the analyzed dataset.")
}

# 5.2. Write the file.
tryCatch({
  con <- file(ruta_salida_txt, open = "wt", encoding = "UTF-8")
  writeLines(lineas_de_texto, con)
  close(con)
  message(paste("\nPROCESS COMPLETED SUCCESSFULLY!"))
  message(paste("Report generated at:", ruta_salida_txt))
}, error = function(e) {
  message("\nError writing the output file.")
  message(paste("Original error:", e$message))
})