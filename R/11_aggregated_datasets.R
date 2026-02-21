#### 11. CREATION OF AGGREGATED DATASETS FOR PROFILES AND STATISTICS ####
message("Centralized calculation of all statistics...")

### 11.1. Generate Global Player Statistics
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
  
  # 11.1.1. Correct the fallbacks for name generation.
  for (lang_code in setdiff(IDIOMAS_SOPORTADOS, "mk")) {
    target_col <- paste0("PlayerName_", lang_code)
    
    # 11.1.2. Ensure the target column exists before mutation.
    if (!target_col %in% names(jugadoras_stats_temp)) {
      jugadoras_stats_temp[[target_col]] <- NA_character_
    }
    
    jugadoras_stats_temp <- jugadoras_stats_temp %>%
      mutate(!!target_col := coalesce(
        # 11.1.3. 1. Try to use the manual translation if it exists.
        .data[[target_col]],
        # 11.1.4. 2. If not, apply automatic transliteration as a fallback.
        str_replace_all(tolower(PlayerName_mk), map_transliteration_player) %>% str_to_title()
      ))
  }
  
  if (!is.null(mapeo_completo_df)) {
    jugadoras_stats_df <- jugadoras_stats_temp %>% left_join(mapeo_completo_df, by = "clave_lower")
  } else {
    jugadoras_stats_df <- jugadoras_stats_temp %>% mutate(codigo_iso = NA_character_, nombre_macedonio = NA_character_)
  }
  
  jugadoras_stats_df <- jugadoras_stats_df %>%
    select(
      id, starts_with("PlayerName_"), Team, 
      posicion_final_unificada, 
      fecha_nacimiento,           
      ciudad_nacimiento,         
      nacionalidad, edad, codigo_iso, nombre_macedonio, 
      CalledUp, Played, Starter, Minutes, Goals, Yellows, Reds
    ) %>% 
    arrange(desc(Goals), desc(Minutes))
}

### 11.2. Calculate Statistics by Competition (Standings, Scorers, Sanctions)
stats_clasificacion_por_comp_df <- competiciones_unicas_df %>%
  filter(competicion_id != "reprezentacija", !str_detect(tolower(competicion_nombre), "куп")) %>%
  group_by(competicion_id, competicion_nombre, competicion_temporada) %>%
  reframe({
    grupo_actual <- cur_group()
    partidos_comp_raw <- partidos_df %>% 
      filter(
        competicion_nombre == grupo_actual$competicion_nombre, 
        competicion_temporada == grupo_actual$competicion_temporada,
        !is.na(id_partido)
      )
    
    # --- LA CORRECCIÓN ESTÁ AQUÍ ---
    # En lugar de usar return(), usamos una estructura if/else completa.
    # Si no hay partidos, el bloque devuelve un tibble vacío.
    # Si hay partidos, el bloque else se ejecuta y devuelve la tabla de clasificación.
    if (nrow(partidos_comp_raw) == 0) {
      
      tibble() # Devuelve un tibble vacío para este grupo
      
    } else {
      
      # Todo el código que calcula la clasificación va aquí dentro del 'else'
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
    }
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

### 11.3. Calculate Goalkeeper Statistics by Competition
porteras_apariciones_df <- apariciones_df %>%
  filter(es_portera == TRUE, !is.na(id), minutos_jugados > 0) %>%
  select(id, id_partido, equipo, competicion_nombre, competicion_temporada, min_entra, min_sale, minutos_jugados)

goles_recibidos_df <- goles_df_unificado %>%
  left_join(partidos_df %>% select(id_partido, local, visitante), by = "id_partido") %>%
  mutate(equipo_que_recibio_gol = if_else(equipo_acreditado == local, visitante, local)) %>%
  select(id_partido, equipo_conceded = equipo_que_recibio_gol, minuto_gol = minuto)

# 11.3.1. Step 1: Calculate Goals Against (GA) explicitly and robustly.
stats_ga <- porteras_apariciones_df %>%
  left_join(goles_recibidos_df, by = c("id_partido", "equipo" = "equipo_conceded"), relationship = "many-to-many") %>%
  
  # PASO ADICIONAL: Normalizar el minuto del gol para la comparación.
  # Esto convierte 903 -> 90, 452 -> 45, etc., solo para este cálculo.
  mutate(
    minuto_gol_normalizado = case_when(
      minuto_gol > 900 ~ 90,
      minuto_gol > 450 & minuto_gol < 900 ~ 45, # Para el añadido de la primera parte
      TRUE ~ minuto_gol
    )
  ) %>%
  
  # FILTRO CORREGIDO: Usamos la nueva columna normalizada para la comparación.
  filter(!is.na(minuto_gol) & minuto_gol_normalizado >= min_entra & minuto_gol_normalizado <= min_sale) %>%
  
  group_by(id, competicion_nombre, competicion_temporada, TeamName_mk = equipo) %>%
  summarise(GA = n(), .groups = 'drop')

# 11.3.2. Step 2: Calculate Clean Sheets (CS) explicitly and robustly.
stats_cs <- porteras_apariciones_df %>%
  # 11.3.3. Join with partidos_df to get the duration of each match.
  left_join(partidos_df %>% select(id_partido, duracion_partido, local, visitante, goles_local, goles_visitante), by = "id_partido") %>%
  # 11.3.4. A CS requires playing the full match (minutes >= duration).
  filter(minutos_jugados >= duracion_partido) %>%
  mutate(goles_recibidos_partido = if_else(local == equipo, goles_visitante, goles_local)) %>%
  filter(goles_recibidos_partido == 0) %>%
  group_by(id, competicion_nombre, competicion_temporada, TeamName_mk = equipo) %>%
  summarise(CS = n(), .groups = 'drop')

# 11.3.5. Step 3: Calculate total minutes.
stats_minutos <- porteras_apariciones_df %>%
  group_by(id, competicion_nombre, competicion_temporada, TeamName_mk = equipo) %>%
  summarise(Minutes = sum(minutos_jugados, na.rm = TRUE), .groups = 'drop')

# 11.3.6. Step 4: Join all statistics into a final dataframe.
stats_porteras_por_comp_df <- stats_minutos %>%
  full_join(stats_ga, by = c("id", "competicion_nombre", "competicion_temporada", "TeamName_mk")) %>%
  full_join(stats_cs, by = c("id", "competicion_nombre", "competicion_temporada", "TeamName_mk")) %>%
  mutate(across(c(GA, CS), ~replace_na(., 0))) %>%
  mutate(GA90 = if_else(Minutes > 0, (GA / Minutes) * 90, 0)) %>%
  left_join(competiciones_unicas_df %>% filter(competicion_id != "reprezentacija") %>% select(competicion_id, competicion_nombre, competicion_temporada), by = c("competicion_nombre", "competicion_temporada"))

### 11.4. Calculate Defensive Trio Statistics by Competition
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

# 11.4.1. Step 1: Calculate the total minutes each trio played together.
stats_minutes_trios <- trio_minutos_partido_df %>%
  group_by(trio_key, competicion_nombre, competicion_temporada, TeamName_mk = equipo) %>%
  summarise(MinutesTogether = sum(minutos_compartidos, na.rm = TRUE), .groups = 'drop')

# 11.4.2. Step 2: Calculate goals conceded by each trio while they were playing together.
stats_ga_trios <- trio_minutos_partido_df %>%
  left_join(goles_recibidos_df, by = c("id_partido", "equipo" = "equipo_conceded"), relationship = "many-to-many") %>%
  filter(!is.na(minuto_gol) & minuto_gol >= start_shared & minuto_gol <= end_shared) %>%
  group_by(trio_key, competicion_nombre, competicion_temporada, TeamName_mk = equipo) %>%
  summarise(GA_Together = n(), .groups = 'drop')

# 11.4.3. Step 3: Join the statistics using left_join to prevent NAs in trio_key.
# 11.4.4. Start from trios that have minutes and add goals to them (if any).
stats_trios_defensivos_df <- stats_minutes_trios %>%
  left_join(stats_ga_trios, by = c("trio_key", "competicion_nombre", "competicion_temporada", "TeamName_mk")) %>%
  filter(!is.na(trio_key)) %>% # 11.4.5. Extra safety filter, although left_join should prevent this.
  mutate(GA_Together = replace_na(GA_Together, 0)) %>%
  mutate(GA90_Together = if_else(MinutesTogether > 0, (GA_Together / MinutesTogether) * 90, 0)) %>%
  left_join(competiciones_unicas_df %>% filter(competicion_id != "reprezentacija") %>% select(competicion_id, competicion_nombre, competicion_temporada), by = c("competicion_nombre", "competicion_temporada"))

### 11.5. Calculate National Team Career Summaries per Player
message("Calculating career summaries for the National Team per player...")

national_team_career_summary_df <- apariciones_df %>%
  filter(!is.na(id)) %>%
  # 11.5.1. Join with partidos_df to identify national team matches.
  left_join(partidos_df %>% select(id_partido, es_partido_seleccion, local, visitante), by = "id_partido") %>%
  # 11.5.2. Filter for national team matches where the player is from Macedonia.
  filter(es_partido_seleccion == TRUE, equipo == "Македонија") %>%
  # 11.5.3. Group by player, assigning fixed values for the pseudo-competition.
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
  # 11.5.4. Join with national team goals for this player.
  left_join(
    goles_df_unificado %>%
      filter(!is.na(id), tipo == "Normal") %>%
      left_join(partidos_df %>% select(id_partido, es_partido_seleccion), by = "id_partido") %>%
      filter(es_partido_seleccion == TRUE, equipo_jugadora == "Македонија") %>%
      group_by(id) %>%
      summarise(Goals = n(), .groups = 'drop'),
    by = "id"
  ) %>%
  # 11.5.5. Join with national team cards for this player.
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

# 11.5.6. Optional: print a summary to verify.
message("   > National team career summaries processed. Rows: ", nrow(national_team_career_summary_df))

message("Calculating career summaries for the National Team by category...")

national_team_career_by_category_df <- apariciones_df %>%
  filter(!is.na(id)) %>%
  left_join(partidos_df %>% select(id_partido, es_partido_seleccion, categoria), by = "id_partido") %>%
  filter(es_partido_seleccion == TRUE, equipo == "Македонија", !is.na(categoria)) %>%
  group_by(id, categoria) %>%
  summarise(
    CalledUp = n_distinct(id_partido),
    Played = sum(minutos_jugados > 0, na.rm=TRUE),
    Starter = sum(tipo=="Titular", na.rm=TRUE),
    Minutes = sum(minutos_jugados, na.rm=TRUE),
    .groups = 'drop'
  ) %>%
  left_join(
    goles_df_unificado %>%
      filter(!is.na(id), tipo == "Normal") %>%
      left_join(partidos_df %>% select(id_partido, es_partido_seleccion, categoria), by = "id_partido") %>%
      filter(es_partido_seleccion == TRUE, equipo_jugadora == "Македонија", !is.na(categoria)) %>%
      group_by(id, categoria) %>%
      summarise(Goals = n(), .groups = 'drop'),
    by = c("id", "categoria")
  ) %>%
  left_join(
    tarjetas_df_unificado %>%
      filter(!is.na(id)) %>%
      left_join(partidos_df %>% select(id_partido, es_partido_seleccion, categoria), by = "id_partido") %>%
      filter(es_partido_seleccion == TRUE, equipo == "Македонија", !is.na(categoria)) %>%
      group_by(id, categoria) %>%
      summarise(Yellows = sum(tipo == "Amarilla", na.rm=T), Reds = sum(tipo == "Roja", na.rm=T), .groups = 'drop'),
    by = c("id", "categoria")
  ) %>%
  mutate(across(c(CalledUp, Played, Starter, Minutes, Goals, Yellows, Reds), ~replace_na(., 0)))

### 11.6. Calculate Career Summaries per Player
career_summary_jugadoras_df <- apariciones_df %>%
  filter(!is.na(id)) %>%
  group_by(id, competicion_temporada, competicion_nombre, equipo) %>%
  summarise(
    CalledUp = n_distinct(id_partido), Played = sum(minutos_jugados > 0, na.rm=TRUE),
    Starter = sum(tipo=="Titular", na.rm=TRUE), Minutes = sum(minutos_jugados, na.rm=TRUE),
    .groups = 'drop'
  ) %>%
  full_join(
    {
      # CORRECCIÓN: Renombrar 'equipo' a 'equipo_canonico' AL CREAR el mapa.
      mapa_partido_jugadora_a_equipo <- apariciones_df %>% 
        distinct(id, id_partido, equipo_canonico = equipo)
      
      goles_df_unificado %>% 
        filter(!is.na(id)) %>%
        left_join(mapa_partido_jugadora_a_equipo, by = c("id", "id_partido"), relationship = "many-to-many") %>%
        left_join(partidos_df %>% select(id_partido, competicion_temporada, competicion_nombre), by="id_partido") %>%
        filter(!is.na(equipo_canonico)) %>% 
        group_by(id, competicion_temporada, competicion_nombre, equipo = equipo_canonico) %>%
        summarise(Goals = sum(tipo == "Normal"), .groups = 'drop')
    },
    by = c("id", "competicion_temporada", "competicion_nombre", "equipo")
  ) %>%
  full_join(
    {
      # CORRECCIÓN: Aplicar el mismo renombrado aquí.
      mapa_partido_jugadora_a_equipo <- apariciones_df %>% 
        distinct(id, id_partido, equipo_canonico = equipo)
      
      tarjetas_df_unificado %>% 
        filter(!is.na(id)) %>%
        left_join(mapa_partido_jugadora_a_equipo, by = c("id", "id_partido"), relationship = "many-to-many") %>%
        left_join(partidos_df %>% select(id_partido, competicion_temporada, competicion_nombre), by="id_partido") %>%
        filter(!is.na(equipo_canonico)) %>%
        group_by(id, competicion_temporada, competicion_nombre, equipo = equipo_canonico) %>%
        summarise(Yellows = sum(tipo == "Amarilla", na.rm=T), Reds = sum(tipo == "Roja", na.rm=T), .groups = 'drop')
    },
    by = c("id", "competicion_temporada", "competicion_nombre", "equipo")
  ) %>%
  mutate(across(c(CalledUp, Played, Starter, Minutes, Goals, Yellows, Reds), ~replace_na(., 0))) %>%
  arrange(id, desc(competicion_temporada))

### 11.7. Calculate Team Profile Summaries
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

### 11.7. Calculate Team Profile Summaries (VERSIÓN ALINEADA CON SCRIPT DE PRUEBAS)
stats_jugadoras_por_equipo_temporada_df <- apariciones_df %>%
  group_by(id, equipo, competicion_nombre, competicion_temporada) %>%
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

### 11.8. Calculate Referee Profile Summaries
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

### 11.9. Identify Entities to Exclude from Individual Page Generation
message("Identifying entities to exclude from individual page generation...")

# 11.9.1. Get IDs of national team matches.
ids_partidos_seleccion <- partidos_df %>%
  filter(es_partido_seleccion == TRUE) %>%
  pull(id_partido) %>%
  unique()

# 11.9.2. Teams to exclude from individual page generation:
# 11.9.3. These are all national teams that have played against Macedonia.
equipos_en_partidos_seleccion <- c(
  (partidos_df %>% filter(id_partido %in% ids_partidos_seleccion) %>% pull(local)),
  (partidos_df %>% filter(id_partido %in% ids_partidos_seleccion) %>% pull(visitante))
)
team_names_to_skip_mk <- unique(equipos_en_partidos_seleccion[equipos_en_partidos_seleccion != "Македонија"])

# 11.9.4. Players to exclude from individual page generation:
# 11.9.5. They appear in a national team match AND do not play for "Македонија" in that match.
player_ids_to_skip <- apariciones_df %>%
  filter(id_partido %in% ids_partidos_seleccion, # The player played in a national team match
         equipo != "Македонија") %>%             # AND their team was NOT "Македонија"
  pull(id) %>%
  unique()

# 11.9.6. Referees who participated in national team matches.
# 11.9.7. They are excluded if they refereed a national team match.
referee_ids_to_skip <- arbitros_df %>%
  filter(id_partido %in% ids_partidos_seleccion) %>%
  pull(ime) %>%
  unique() %>%
  generar_id_seguro() # 11.9.8. Convert to safe ID for comparison in the loop.

# 11.9.9. Stadiums where national team matches were played.
# 11.9.10. They are excluded if they hosted a national team match.
stadium_ids_to_skip <- estadios_df %>%
  filter(id_partido %in% ids_partidos_seleccion) %>%
  pull(estadio) %>%
  unique() %>%
  na.omit() %>% # 11.9.11. Ensure there are no NAs in stadium names.
  generar_id_seguro() # 11.9.12. Convert to safe ID for comparison in the loop.

message(paste("   >", length(team_names_to_skip_mk), "foreign national teams will be excluded from team profiles."))
message(paste("   >", length(player_ids_to_skip), "non-Macedonian players in national team matches will be excluded from individual profiles."))
message(paste("   >", length(referee_ids_to_skip), "referees from national team matches will be excluded from individual profiles."))
message(paste("   >", length(stadium_ids_to_skip), "stadiums from national team matches will be excluded from individual profiles."))


# ==============================================================================
# ==      AÑADIR ESTE BLOQUE PARA CREAR LA INSTANTÁNEA DE DATOS PARA PRUEBAS      ==
# ==============================================================================
message(">>>>>> CREANDO INSTANTÁNEA DE DATOS PARA PRUEBAS (entorno_procesado.RData)...")

# Lista de todos los objetos que necesitamos para generar las páginas
objetos_a_guardar <- c(
  # Dataframes principales de datos
  "partidos_df", "apariciones_df", "goles_df_unificado", "tarjetas_df_unificado",
  "arbitros_df", "estadios_df",
  
  # Dataframes de estadísticas agregadas
  "jugadoras_stats_df", "stats_clasificacion_por_comp_df", 
  "stats_goleadoras_por_comp_df", "stats_sanciones_por_comp_df",
  "stats_porteras_por_comp_df", "stats_trios_defensivos_df",
  "career_summary_jugadoras_df", "stats_equipos_por_temporada_df",
  "stats_jugadoras_por_equipo_temporada_df", "stats_arbitros_por_temporada_df",
  "national_team_career_summary_df", "national_team_career_by_category_df",
  
  # Dataframes de configuración y mapeo
  "entidades_maestro_df", "competiciones_unicas_df", "mapeo_completo_df",
  
  # Variables y listas de configuración
  "textos", "IDIOMAS_SOPORTADOS", "nombres_carpetas_relativos", 
  "estilos_clasificacion_data", "nombres_archivos_traducidos",
  
  # Variables de exclusión
  "player_ids_to_skip", "team_names_to_skip_mk", "referee_ids_to_skip", "stadium_ids_to_skip"
)

# Guardar solo los objetos existentes en el entorno
save(
  list = intersect(objetos_a_guardar, ls()), 
  file = "entorno_procesado.RData"
)

message(">>>>>> Instantánea guardada. Puedes detener el script si solo querías esto.")
# ==============================================================================

# Crear el historial de partidos enriquecido que necesita la página de equipo
message("Creating enriched match history for team pages...")
historial_partidos_con_categoria <- partidos_df %>%
  filter(!is.na(id_partido)) %>%
  left_join(competiciones_unicas_df, by = c("competicion_nombre", "competicion_temporada")) %>%
  mutate(
    categoria = case_when(
      str_detect(competicion_nombre, "Младинска") ~ "Младинки",
      str_detect(competicion_nombre, "Кадетска") ~ "Кадетки",
      TRUE ~ "Сениорки"
    ),
    category_key = case_when(
      categoria == "Младинки" ~ "category_youth",
      categoria == "Кадетки" ~ "category_cadet",
      TRUE ~ "category_senior"
    ),
    home_logo_url = paste0("../../assets/logos/", generar_id_seguro(local), ".png"),
    away_logo_url = paste0("../../assets/logos/", generar_id_seguro(visitante), ".png")
  )

