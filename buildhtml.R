# =========================================================================
# SCRIPT DE GENERACIÓN DE INFORME HTML (VERSIÓN MULTI-COMPETICIÓN FINAL CORREGIDA)
# =========================================================================

# -------------------------------------------------------------------------
# PASO 6: INSTALAR Y CARGAR PAQUETES
# -------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, tidyr, purrr, htmltools, stringr, jsonlite
)

message("Започнување со генерирање на HTML извештајот...")

# -------------------------------------------------------------------------
# PASO 7: PREPARACIÓN DE DATOS
# -------------------------------------------------------------------------

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

generar_id_seguro <- function(nombre) {
  id_sanitizada <- tolower(nombre)
  id_sanitizada <- gsub("[\\s/]+", "_", id_sanitizada)
  id_sanitizada <- gsub("[^\\p{L}0-9_\\-]+", "", id_sanitizada, perl = TRUE)
  id_sanitizada <- gsub("^_+|_+$", "", id_sanitizada)
  return(id_sanitizada)
}

preferred_id_map <- apariciones_df_raw %>% filter(!is.na(nombre), !is.na(id), str_detect(id, "^\\d{5,6}$")) %>% count(nombre, id, name = "frequency") %>% group_by(nombre) %>% filter(frequency == max(frequency)) %>% slice(1) %>% ungroup() %>% select(nombre, canonical_id = id)
id_mapping <- apariciones_df_raw %>% filter(!is.na(nombre) & nchar(trimws(nombre)) > 2) %>% distinct(nombre) %>% left_join(preferred_id_map, by = "nombre") %>% mutate(final_id = if_else(!is.na(canonical_id), as.character(canonical_id), paste0("player_gen_", generar_id_seguro(nombre)))) %>% select(nombre, canonical_id = final_id)

apariciones_df <- apariciones_df_raw %>% left_join(minutos_df_raw %>% select(id_partido, nombre, dorsal, tipo, min_entra, min_sale, minutos_jugados), by = c("id_partido", "nombre", "dorsal", "tipo")) %>% select(-id) %>% left_join(id_mapping, by = "nombre") %>% rename(id = canonical_id) %>% select(id, id_partido, nombre, dorsal, tipo, equipo, es_portera, es_capitana, competicion_nombre, competicion_temporada, everything())

goles_raw_df <- map_dfr(resultados_exitosos, "goles")
if (nrow(goles_raw_df) > 0) { goles_df_unificado <- goles_raw_df %>% mutate(jugadora = str_squish(jugadora)) %>% left_join(id_mapping, by = c("jugadora" = "nombre")) %>% select(-any_of(c("id", "id_jugadora"))) %>% rename(id = canonical_id)
} else { goles_df_unificado <- tibble(id_partido = character(), jugadora = character(), equipo_jugadora = character(), equipo = character(), minuto = integer(), dorsal = integer(), tipo = character(), id = character()) }

tarjetas_raw_df <- map_dfr(resultados_exitosos, "tarjetas")
if(nrow(tarjetas_raw_df) > 0) { tarjetas_df_unificado <- tarjetas_raw_df %>% mutate(jugadora = str_squish(jugadora)) %>% left_join(id_mapping, by = c("jugadora" = "nombre")) %>% select(-any_of(c("id", "id_jugadora"))) %>% rename(id = canonical_id)
} else { tarjetas_df_unificado <- tibble(jugadora = character(), equipo = character(), dorsal = integer(), minuto = integer(), tipo = character(), motivo = character(), id_partido = character(), id = character()) }


# --- 7.2: Identificar competiciones únicas ---
message("Идентификување на уникатни натпреварувања...")
if (exists("partidos_df") && nrow(partidos_df) > 0) {
  competiciones_unicas_df <- partidos_df %>%
    distinct(competicion_nombre, competicion_temporada) %>%
    mutate(
      competicion_id = paste0(generar_id_seguro(competicion_nombre), "_", generar_id_seguro(competicion_temporada)),
      nombre_completo = paste(competicion_nombre, competicion_temporada)
    ) %>%
    arrange(nombre_completo)
} else {
  competiciones_unicas_df <- tibble(competicion_nombre=character(), competicion_temporada=character(), competicion_id=character(), nombre_completo=character())
}

# --- 7.3: Preparar datos globales para perfiles ---
if (!exists("apariciones_df") || nrow(apariciones_df) == 0) {
  jugadoras_stats_df <- data.frame(id=character(), Играч=character(), Тим=character(), Повикана=integer(), Одиграни_натпревари=integer(), Почетен_состав=integer(), Минути=numeric(), Голови=numeric(), Жолти=integer(), Црвени=integer(), stringsAsFactors = FALSE)
} else {
  stats_generales <- apariciones_df %>% filter(!is.na(id)) %>% group_by(id) %>% summarise(Играч=first(nombre),Тим=last(equipo),Повикана=n_distinct(id_partido),Почетен_состав=sum(tipo=="Titular",na.rm=T),Минути=sum(minutos_jugados,na.rm=T),Одиграни_натпревари=sum(minutos_jugados>0,na.rm=T),.groups='drop')
  goles_por_jugadora_global <- goles_df_unificado %>% filter(!is.na(id), tipo == "Normal") %>% group_by(id) %>% summarise(Голови = n(), .groups = 'drop')
  tarjetas_por_jugadora_global <- tarjetas_df_unificado %>% filter(!is.na(id)) %>% group_by(id) %>% summarise(Жолти=sum(tipo=="Amarilla",na.rm=T),Црвени=sum(tipo=="Roja",na.rm=T),.groups='drop')
  jugadoras_stats_df <- stats_generales %>% left_join(goles_por_jugadora_global, by="id") %>% left_join(tarjetas_por_jugadora_global, by="id") %>% mutate(Голови=replace_na(Голови,0), Жолти=replace_na(Жолти,0), Црвени=replace_na(Црвени,0)) %>% select(id, Играч, Тим, Повикана, Одиграни_натпревари, Почетен_состав, Минути, Голови, Жолти, Црвени) %>% arrange(desc(Голови), desc(Минути))
}
arbitros_df <- map_dfr(resultados_exitosos, ~if(is.null(.x)||is.null(.x$arbitro_principal)) NULL else data.frame(id_partido=.x$partido_info$id_partido,arbitro_principal=.x$arbitro_principal,arbitro_asist_1=.x$arbitro_asist_1,arbitro_asist_2=.x$arbitro_asist_2)) %>% pivot_longer(cols=starts_with("arbitro_"),names_to="uloga",values_to="ime",values_drop_na=T) %>% mutate(uloga=case_when(uloga=="arbitro_principal"~"Главен судија",uloga=="arbitro_asist_1"~"1-ви помошник",uloga=="arbitro_asist_2"~"2-ри помошник",T~uloga))
estadios_df <- map_dfr(resultados_exitosos, ~if(is.null(.x)||is.null(.x$estadio)) NULL else data.frame(id_partido=.x$partido_info$id_partido,estadio=.x$estadio)) %>% left_join(partidos_df,by="id_partido")

# --- 7.4: Crear índice de búsqueda unificado ---
message("Креирање на индекс за пребарување...")
search_jugadoras <- jugadoras_stats_df %>% select(Име = Играч, id) %>% mutate(Тип = "Играч", target_id = paste0("jugadora-", id)) %>% select(Име, Тип, target_id)
search_equipos <- data.frame(Име = unique(c(partidos_df$local,partidos_df$visitante))) %>% mutate(Тип = "Тим", target_id = paste0("equipo-", generar_id_seguro(Име)))
search_arbitros <- data.frame(Име = unique(arbitros_df$ime)) %>% mutate(Тип = "Судија", target_id = paste0("arbitro-", generar_id_seguro(Име)))
search_competiciones <- competiciones_unicas_df %>% mutate(Име = nombre_completo, Тип = "Натпреварување", target_id = paste0("menu-competicion-", competicion_id)) %>% select(Име, Тип, target_id)
search_index_df <- bind_rows(search_jugadoras, search_equipos, search_arbitros, search_competiciones) %>% arrange(Име)
search_data_json <- toJSON(search_index_df, auto_unbox = TRUE)

# -------------------------------------------------------------------------
# PASO 8 y 9: CSS, JS y Generación de HTML
# -------------------------------------------------------------------------

# --- 8.1: CSS ---
estilo_css <- paste(
  "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; line-height: 1.6; background-color: #f8f9fa; color: #212529; margin: 0; }",
  ".container { max-width: 900px; margin: 20px auto; padding: 20px; background-color: #ffffff; border-radius: 8px; box-shadow: 0 0 15px rgba(0,0,0,0.05); }",
  ".page { display: none; } #portal { display: block; }",
  "h1, h2, h3 { color: #003366; border-bottom: 2px solid #dee2e6; padding-bottom: 10px; }",
  "h1 { font-size: 2.5em; text-align: center; } h2 { font-size: 1.8em; margin-top: 40px; } h3 { font-size: 1.5em; }",
  "a { color: #0056b3; text-decoration: none; font-weight: bold; } a:hover { text-decoration: underline; }",
  "table { width: 100%; border-collapse: collapse; margin-top: 20px; }",
  "th, td { padding: 12px; border: 1px solid #dee2e6; text-align: left; } th { background-color: #f2f2f2; }",
  ".summary-row { cursor: pointer; } .summary-row:hover { background-color: #f0f8ff; }",
  ".details-row { display: none; } .details-row > td { padding: 0; }",
  ".details-content { padding: 20px; background-color: #fdfdfd; border-top: 2px solid #003366; }",
  ".details-content h4 { font-size: 1.3em; color: #004488; margin-top: 10px; border-bottom: 1px solid #e0e0e0; padding-bottom: 5px;}",
  ".back-link, .menu-button, .portal-button { display: inline-block; margin-top: 20px; padding: 10px 15px; background-color: #6c757d; color: white !important; border-radius: 5px; font-weight: bold; text-decoration: none; text-align: center;}",
  ".back-link:hover, .menu-button:hover, .portal-button:hover { background-color: #5a6268; text-decoration: none; }",
  ".menu-container, .portal-container { text-align: center; padding: 20px 0; display: flex; flex-wrap: wrap; justify-content: center; gap: 15px; }",
  ".menu-button { padding: 15px 30px; font-size: 1.1em; background-color: #003366; color: white !important; } .menu-button:hover { background-color: #002244; }",
  ".portal-button { width: 80%; padding: 20px; font-size: 1.3em; background-color: #004488; } .portal-button:hover { background-color: #003366; }",
  ".sortable-header { cursor: pointer; user-select: none; } .sortable-header::after { content: ' '; display: inline-block; margin-left: 5px; }",
  ".sortable-header.asc::after { content: '▲'; } .sortable-header.desc::after { content: '▼'; }",
  ".partido-link { display: flex; justify-content: space-between; align-items: center; padding: 15px; margin: 10px 0; background-color: #e9ecef; border-radius: 5px; transition: background-color 0.2s; }",
  ".partido-link:hover { background-color: #ced4da; } .partido-link span.equipo { flex: 1; }",
  ".partido-link span.equipo-local { text-align: right; margin-right: 15px; } .partido-link span.equipo-visitante { text-align: left; margin-left: 15px; }",
  ".partido-link span.resultado { font-size: 1.2em; font-weight: bold; text-align: center; }",
  ".jornada-header { background-color: #003366; color: white; padding: 10px; border-radius: 5px; margin-top: 30px; }",
  ".timeline { list-style: none; padding-left: 0; } .timeline li { padding: 8px 0; border-bottom: 1px dotted #ccc; display: flex; align-items: center; }",
  ".timeline .icon { margin-right: 10px; font-size: 1.2em; width: 24px; text-align: center; }",
  ".alineaciones-container { display: flex; gap: 30px; align-items: flex-start; } .columna-alineacion { flex: 1; }",
  ".columna-alineacion h3 a { color: #003366; } .columna-alineacion h4 { margin-top: 15px; margin-bottom: 10px; font-size: 1.2em; color: #111; border-bottom: 1px solid #ccc; padding-bottom: 5px; }",
  ".columna-alineacion ul { list-style: none; padding: 0; margin: 0 0 20px 0; } .columna-alineacion li { padding: 6px 3px; border-bottom: 1px solid #f0f0f0; }",
  ".player-event { margin-left: 8px; font-size: 0.9em; color: #444; vertical-align: middle; } .player-event.goal { font-weight: bold; }",
  ".sub-in { color: #28a745; font-style: italic; vertical-align: middle; } .sub-out { color: #dc3545; font-style: italic; vertical-align: middle; }",
  ".card-yellow, .card-red { display: inline-block; width: 12px; height: 16px; border: 1px solid #777; border-radius: 2px; vertical-align: middle; margin-left: 4px; }",
  ".card-yellow { background-color: #ffc107; } .card-red { background-color: #dc3545; }",
  ".search-container { position: relative; margin: 25px 0; }",
  ".search-container form { display: flex; }",
  ".search-input { flex-grow: 1; font-size: 1.1em; padding: 12px; border: 1px solid #ccc; border-radius: 5px 0 0 5px; }",
  ".search-button { font-size: 1.1em; padding: 12px 20px; border: 1px solid #003366; background-color: #003366; color: white; cursor: pointer; border-radius: 0 5px 5px 0; }",
  "#search-suggestions { display: none; position: absolute; top: 100%; left: 0; right: 0; background-color: white; border: 1px solid #ccc; border-top: none; z-index: 1000; max-height: 300px; overflow-y: auto; box-shadow: 0 4px 8px rgba(0,0,0,0.1); }",
  "#search-suggestions a { display: block; padding: 12px; color: #333; text-decoration: none; border-bottom: 1px solid #f0f0f0; }",
  "#search-suggestions a:last-child { border-bottom: none; }",
  "#search-suggestions a:hover { background-color: #f2f2f2; }",
  "#search-suggestions a strong { color: #003366; }",
  "#search-results-list ul { list-style-type: none; padding: 0; }",
  "#search-results-list li { margin-bottom: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 4px; }",
  "#search-results-list a { font-size: 1.2em; text-decoration: none; }",
  "#search-results-list a:hover { text-decoration: underline; }",
  ".search-result-type { font-size: 0.85em; color: #6c757d; margin-left: 8px; }",
  collapse = "\n"
)

# --- 8.2: JavaScript ---
script_js <- paste(
  "let searchData = [];",
  "document.addEventListener('DOMContentLoaded', initializeSearch);",
  "function initializeSearch() {",
  "  const searchDataElement = document.getElementById('search-data-json');",
  "  if (searchDataElement) {",
  "    try { searchData = JSON.parse(searchDataElement.textContent); } catch (e) { console.error('Error parsing search data JSON:', e); }",
  "  }",
  "  document.addEventListener('click', function(event) {",
  "    const searchContainer = document.querySelector('.search-container');",
  "    if (searchContainer && !searchContainer.contains(event.target)) {",
  "      const suggestions = document.getElementById('search-suggestions');",
  "      if(suggestions) suggestions.style.display = 'none';",
  "    }",
  "  });",
  "}",
  "function toggleDetails(elementId) {",
  "  const detailsRow = document.getElementById(elementId);",
  "  if (detailsRow) {",
  "    if (detailsRow.style.display === 'table-row') {",
  "      detailsRow.style.display = 'none';",
  "    } else {",
  "      detailsRow.style.display = 'table-row';",
  "    }",
  "  }",
  "}",
  "function handleSearchInput(event) {",
  "  if (event.key === 'Enter') { event.preventDefault(); showSearchResults(); return; }",
  "  const input = document.getElementById('search-input');",
  "  const suggestionsContainer = document.getElementById('search-suggestions');",
  "  const query = input.value.trim().toLowerCase();",
  "  if (query.length < 2) { suggestionsContainer.innerHTML = ''; suggestionsContainer.style.display = 'none'; return; }",
  "  const filteredResults = searchData.filter(item => item.Име.toLowerCase().includes(query));",
  "  const top5 = filteredResults.slice(0, 5);",
  "  if (top5.length === 0) { suggestionsContainer.innerHTML = ''; suggestionsContainer.style.display = 'none'; return; }",
  "  suggestionsContainer.innerHTML = top5.map(item => `<a href='#' onclick=\\\"mostrarPagina('${item.target_id}'); document.getElementById('search-suggestions').style.display='none'; return false;\\\"><strong>${item.Име}</strong> <span class='search-result-type'>(${item.Тип})</span></a>`).join('');",
  "  suggestionsContainer.style.display = 'block';",
  "}",
  "function showSearchResults() {",
  "  const input = document.getElementById('search-input');",
  "  const suggestionsContainer = document.getElementById('search-suggestions');",
  "  const resultsList = document.getElementById('search-results-list');",
  "  const resultsTitle = document.getElementById('search-results-title');",
  "  const query = input.value.trim().toLowerCase();",
  "  if(suggestionsContainer) suggestionsContainer.style.display = 'none';",
  "  if (query.length === 0) {",
  "    resultsList.innerHTML = '<p>Ве молиме внесете термин за пребарување.</p>';",
  "    resultsTitle.innerText = 'Резултати од пребарувањето';",
  "    mostrarPagina('search-results'); return;",
  "  }",
  "  const filteredResults = searchData.filter(item => item.Име.toLowerCase().includes(query));",
  "  resultsTitle.innerText = `Резултати за: \\\"${input.value}\\\" (${filteredResults.length} пронајдени)`;",
  "  if (filteredResults.length === 0) { resultsList.innerHTML = '<p>Нема пронајдени резултати.</p>'; }",
  "  else { resultsList.innerHTML = '<ul>' + filteredResults.map(item =>`<li><a href='#' onclick=\\\"mostrarPagina('${item.target_id}')\\\">${item.Име} <span class='search-result-type'>(${item.Тип})</span></a></li>`).join('') + '</ul>'; }",
  "  mostrarPagina('search-results');",
  "}",
  "function mostrarPagina(id) {",
  "  document.querySelectorAll('.page').forEach(p => p.style.display = 'none');",
  "  const a = document.getElementById(id);",
  "  if (a) { a.style.display = 'block'; window.scrollTo(0, 0); } else { document.getElementById('portal').style.display = 'block'; }",
  "}",
  "function sortTable(tableId, columnIndex) {",
  "  const table = document.getElementById(tableId); if(!table) return;",
  "  const tbody = table.querySelector('tbody'); const rows = Array.from(tbody.querySelectorAll('tr')); const header = table.querySelectorAll('th')[columnIndex];",
  "  let currentDir = table.dataset.sortDir || 'desc'; let newDir = 'asc';",
  "  if (table.dataset.sortCol == columnIndex) { newDir = currentDir === 'asc' ? 'desc' : 'asc'; }",
  "  table.dataset.sortCol = columnIndex; table.dataset.sortDir = newDir;",
  "  rows.sort((a, b) => {",
  "    const valA = a.children[columnIndex].innerText; const valB = b.children[columnIndex].innerText;",
  "    const numA = parseFloat(valA); const numB = parseFloat(valB); let comparison = 0;",
  "    if (!isNaN(numA) && !isNaN(numB)) { comparison = numA - numB; } else { comparison = valA.localeCompare(valB, 'mk', { sensitivity: 'base' }); }",
  "    return newDir === 'asc' ? comparison : -comparison;",
  "  });",
  "  tbody.innerHTML = ''; rows.forEach(row => tbody.appendChild(row));",
  "  table.querySelectorAll('th').forEach(th => th.classList.remove('asc', 'desc'));",
  "  if(header) header.classList.add(newDir);",
  "}",
  "function filtrarPartidosJugadora(idJugadora) {",
  "  const contenedorFiltros = document.getElementById('filtros-historial-' + idJugadora); if (!contenedorFiltros) return;",
  "  const filtrosSeleccionados = Array.from(contenedorFiltros.querySelectorAll('input[type=\\\"checkbox\\\"]:checked')).map(cb => cb.value);",
  "  const tbody = document.getElementById('historial-tbody-' + idJugadora); if (!tbody) return;",
  "  const filas = tbody.querySelectorAll('tr');",
  "  filas.forEach(fila => {",
  "    const statusFila = fila.children[3] ? fila.children[3].innerText : '';",
  "    if (filtrosSeleccionados.length === 0 || filtrosSeleccionados.includes(statusFila)) { fila.style.display = ''; } else { fila.style.display = 'none'; }",
  "  });",
  "}",
  collapse = "\n"
)

# --- 9.1: Página del Portal (Página de inicio) ---
pagina_portal <- tags$div(
  id = "portal", class = "page",
  tags$h2("Портал на натпреварувања"),
  tags$div(class = "portal-container",
           if (nrow(competiciones_unicas_df) > 0) {
             map(1:nrow(competiciones_unicas_df), function(i) {
               comp <- competiciones_unicas_df[i,]
               tags$a(href="#", onclick=sprintf("mostrarPagina('menu-competicion-%s')", comp$competicion_id),
                      class="portal-button", comp$nombre_completo)
             })
           } else { tags$p("Не се пронајдени натпреварувања.") }
  )
)

# --- 9.2: Generación de páginas por competición en un bucle ---
message("Генерирање на страници за секое натпреварување...")
paginas_por_competicion <- map(1:nrow(competiciones_unicas_df), function(i) {
  comp_info <- competiciones_unicas_df[i,]
  comp_id <- comp_info$competicion_id
  comp_nombre <- comp_info$nombre_completo
  partidos_comp <- partidos_df %>% filter(competicion_nombre == comp_info$competicion_nombre, competicion_temporada == comp_info$competicion_temporada)
  ids_partidos_comp <- partidos_comp$id_partido
  goles_comp <- goles_df_unificado %>% filter(id_partido %in% ids_partidos_comp)
  tarjetas_comp <- tarjetas_df_unificado %>% filter(id_partido %in% ids_partidos_comp)
  apariciones_comp <- apariciones_df %>% filter(id_partido %in% ids_partidos_comp)
  calcular_clasificacion <- function(partidos) { if (is.null(partidos) || nrow(partidos) == 0) return(data.frame(Порака = "Нема обработени валидни натпревари.")); locales <- partidos %>% select(equipo = local, GF = goles_local, GC = goles_visitante); visitantes <- partidos %>% select(equipo = visitante, GF = goles_visitante, GC = goles_local); resultados_por_equipo <- bind_rows(locales, visitantes) %>% mutate(Pts = case_when(GF > GC ~ 3, GF < GC ~ 0, TRUE ~ 1), resultado = case_when(GF > GC ~ "Поб", GF < GC ~ "Пор", TRUE ~ "Нер")); clasificacion <- resultados_por_equipo %>% group_by(Тим = equipo) %>% summarise(Н = n(), Бод. = sum(Pts), Поб = sum(resultado == "Поб"), Нер = sum(resultado == "Нер"), Пор = sum(resultado == "Пор"), ДГ = sum(GF), ПГ = sum(GC), .groups = 'drop') %>% mutate(ГР = ДГ - ПГ) %>% arrange(desc(Бод.), desc(ГР), desc(ДГ)) %>% mutate(Поз. = row_number()) %>% select(Поз., Тим, Н, Поб, Нер, Пор, ДГ, ПГ, ГР, Бод.); return(clasificacion)}
  clasificacion_df_comp <- calcular_clasificacion(partidos_comp)
  goles_por_jugadora_comp <- goles_comp %>% filter(!is.na(id), tipo == "Normal") %>% group_by(id) %>% summarise(Голови = n(), .groups = 'drop')
  jugadoras_info_comp <- apariciones_comp %>% distinct(id, Играч = nombre, Тим = equipo)
  tabla_goleadoras_comp <- goles_por_jugadora_comp %>% left_join(jugadoras_info_comp, by="id") %>% filter(!is.na(Играч)) %>% arrange(desc(Голови)) %>% mutate(Поз. = min_rank(desc(Голови))) %>% select(Поз., id, Играч, Тим, Голови)
  tarjetas_por_jugadora_comp <- tarjetas_comp %>% filter(!is.na(id)) %>% group_by(id) %>% summarise(Жолти=sum(tipo=="Amarilla",na.rm=T),Црвени=sum(tipo=="Roja",na.rm=T),.groups='drop')
  tabla_sanciones_comp <- tarjetas_por_jugadora_comp %>% left_join(jugadoras_info_comp, by = "id") %>% filter(!is.na(Играч), Жолти > 0 | Црвени > 0) %>% arrange(desc(Црвени), desc(Жолти)) %>% mutate(Поз. = min_rank(desc(Црвени * 1000 + Жолти))) %>% select(Поз., id, Играч, Тим, Жолти, Црвени)
  
  pagina_menu <- tags$div(id=paste0("menu-competicion-", comp_id), class="page", tags$a("← Назад кон порталот", href="#", onclick="mostrarPagina('portal')", class="back-link"), tags$h2(comp_nombre), tags$div(class="menu-container", tags$a(href="#", onclick=sprintf("mostrarPagina('partidos-%s')", comp_id), class="menu-button", "Распоред"), tags$a(href="#", onclick=sprintf("mostrarPagina('clasificacion-%s')", comp_id), class="menu-button", "Табела"), tags$a(href="#", onclick=sprintf("mostrarPagina('goleadoras-%s')", comp_id), class="menu-button", "Стрелци"), tags$a(href="#", onclick=sprintf("mostrarPagina('sanciones-%s')", comp_id), class="menu-button", "Дисциплинска")))
  jornadas_comp <- if (nrow(partidos_comp) > 0) sort(unique(partidos_comp$jornada)) else c()
  lista_partidos_html <- map(jornadas_comp, function(j) { partidos_jornada <- partidos_comp %>% filter(jornada == j) %>% arrange(local); tagList(tags$h3(class="jornada-header",paste("Коло",j)), map(1:nrow(partidos_jornada), function(i) { partido <- partidos_jornada[i,]; tags$a(class="partido-link", href="#", onclick=sprintf("mostrarPagina('partido-%s')", partido$id_partido), tags$span(class="equipo equipo-local", partido$local), tags$span(class="resultado", paste(partido$goles_local,"-",partido$goles_visitante)), tags$span(class="equipo equipo-visitante", partido$visitante)) })) })
  pagina_partidos <- tags$div(id=paste0("partidos-", comp_id), class="page", tags$a("← Назад кон менито",href="#",onclick=sprintf("mostrarPagina('menu-competicion-%s')", comp_id),class="back-link"),tags$h2(paste("Распоред -", comp_nombre)),lista_partidos_html)
  pagina_clasificacion <- tags$div(id=paste0("clasificacion-", comp_id), class="page", tags$a("← Назад кон менито",href="#",onclick=sprintf("mostrarPagina('menu-competicion-%s')", comp_id),class="back-link"), tags$h2(paste("Табела -", comp_nombre)), tags$table(tags$thead(tags$tr(map(names(clasificacion_df_comp),tags$th))),tags$tbody(map(1:nrow(clasificacion_df_comp),function(i){tr<-clasificacion_df_comp[i,];tags$tr(map(tr,function(cell){if(is.character(cell)&&cell%in%clasificacion_df_comp$Тим)tags$td(tags$a(href="#",onclick=sprintf("mostrarPagina('equipo-%s')",generar_id_seguro(cell)),cell))else tags$td(cell)}))}))))
  pagina_goleadoras <- tags$div(id=paste0("goleadoras-", comp_id), class="page", tags$a("← Назад кон менито",href="#",onclick=sprintf("mostrarPagina('menu-competicion-%s')", comp_id),class="back-link"), tags$h2(paste("Листа на стрелци -", comp_nombre)), tags$table(tags$thead(tags$tr(map(names(tabla_goleadoras_comp%>%select(-id)),tags$th))),tags$tbody(map(1:nrow(tabla_goleadoras_comp),function(i){g<-tabla_goleadoras_comp[i,];tags$tr(tags$td(g$Поз.),tags$td(tags$a(href="#",onclick=sprintf("mostrarPagina('jugadora-%s')",g$id),g$Играч)),tags$td(tags$a(href="#",onclick=sprintf("mostrarPagina('equipo-%s')",generar_id_seguro(g$Тим)),g$Тим)),tags$td(g$Голови))}))))
  pagina_sanciones <- tags$div(id=paste0("sanciones-", comp_id), class="page", tags$a("← Назад кон менито",href="#",onclick=sprintf("mostrarPagina('menu-competicion-%s')", comp_id),class="back-link"), tags$h2(paste("Дисциплинска евиденција -", comp_nombre)), tags$table(tags$thead(tags$tr(tags$th("Поз."), tags$th("Играч"), tags$th("Тим"), tags$th(HTML("<span class='card-yellow'></span>")), tags$th(HTML("<span class='card-red'></span>")))), tags$tbody(if(nrow(tabla_sanciones_comp)>0) { map(1:nrow(tabla_sanciones_comp), function(i) {s<-tabla_sanciones_comp[i,];tags$tr(tags$td(s$Поз.),tags$td(tags$a(href="#",onclick=sprintf("mostrarPagina('jugadora-%s')",s$id),s$Играч)),tags$td(tags$a(href="#",onclick=sprintf("mostrarPagina('equipo-%s')",generar_id_seguro(s$Тим)),s$Тим)),tags$td(s$Жолти),tags$td(s$Црвени))})} else {tags$tr(tags$td(colspan="5","Нема регистрирани картони."))}) ))
  tagList(pagina_menu, pagina_partidos, pagina_clasificacion, pagina_goleadoras, pagina_sanciones)
})

# --- 9.3: Generación de páginas globales (perfiles) ---
message("Генерирање на глобални страници (натпревари, играчи, тимови, судии)...")

generar_cronologia_df <- function(id_p, resumen_partido) {
  goles_partido <- filter(goles_df_unificado, id_partido == id_p); tarjetas_partido <- filter(tarjetas_df_unificado, id_partido == id_p)
  cambios_partido <- bind_rows(resumen_partido$cambios_local%>%mutate(equipo=resumen_partido$partido_info$local), resumen_partido$cambios_visitante%>%mutate(equipo=resumen_partido$partido_info$visitante))
  alineacion_partido <- bind_rows(resumen_partido$alineacion_local%>%mutate(equipo=resumen_partido$partido_info$local),resumen_partido$alineacion_visitante%>%mutate(equipo=resumen_partido$partido_info$visitante)) %>% left_join(id_mapping,by="nombre") %>% select(-id) %>% rename(id=canonical_id)
  goles_html<-goles_partido%>%mutate(tipo_evento="goal",icono="⚽",texto_evento=pmap_chr(list(jugadora,id,equipo_jugadora,tipo),function(j,p_id,t,type){link<-if(!is.na(p_id))as.character(tags$a(href="#",onclick=sprintf("mostrarPagina('jugadora-%s')",p_id),j))else j;if(type=="Autogol")paste0("(",t,") Автогол на ",link)else paste0("(",t,") Гол на ",link)}))
  tarjetas_html<-tarjetas_partido%>%mutate(tipo_evento="card",icono=if_else(tipo=="Amarilla","<span class='card-yellow'></span>","<span class='card-red'></span>"),texto_evento=pmap_chr(list(jugadora,id,equipo,tipo,motivo),function(j,p_id,t,type,m){link<-if(!is.na(p_id))as.character(tags$a(href="#",onclick=sprintf("mostrarPagina('jugadora-%s')",p_id),j))else j;tipo_mk<-if(type=="Amarilla")"Жолт" else "Црвен";paste0("(",t,") ",tipo_mk," картон за ",link," ('",m,"')")}))
  cambios_html<-cambios_partido%>%mutate(d_e=as.numeric(str_match(texto,"Entra .*?\\((\\d+)\\)")[,2]),d_s=as.numeric(str_match(texto,"por .*?\\((\\d+)\\)")[,2]))%>%left_join(select(alineacion_partido,id,nombre,dorsal,equipo),by=c("equipo","d_e"="dorsal"))%>%rename(id_e=id,n_e=nombre)%>%left_join(select(alineacion_partido,id,nombre,dorsal,equipo),by=c("equipo","d_s"="dorsal"),suffix=c("_e","_s"))%>%rename(id_s=id,n_s=nombre)%>%mutate(tipo_evento="sub",icono="🔄",texto_evento=pmap_chr(list(equipo,id_e,n_e,d_e,id_s,n_s,d_s),function(t,id1,nm1,dr1,id2,nm2,dr2){l1<-if(!is.na(id1))as.character(tags$a(href="#",onclick=sprintf("mostrarPagina('jugadora-%s')",id1),nm1))else"Непознат";l2<-if(!is.na(id2))as.character(tags$a(href="#",onclick=sprintf("mostrarPagina('jugadora-%s')",id2),nm2))else"Непознат";paste0("(",t,") Влегува ",l1," (",dr1,") на местото на ",l2," (",dr2,")")}))
  bind_rows(goles_html%>%select(minuto,texto_evento,tipo_evento,icono),tarjetas_html%>%select(minuto,texto_evento,tipo_evento,icono),cambios_html%>%select(minuto,texto_evento,tipo_evento,icono)) %>% arrange(minuto,case_when(tipo_evento=="goal"~1,tipo_evento=="card"~2,tipo_evento=="sub"~3,T~4))
}

paginas_partidos_html <- map(1:nrow(partidos_df), function(i) {
  partido <- partidos_df[i, ]; id_p <- partido$id_partido; resumen_partido <- purrr::keep(resultados_exitosos, ~.x$partido_info$id_partido == id_p)[[1]]; cronologia <- generar_cronologia_df(id_p, resumen_partido)
  arbitros_partido <- arbitros_df %>% filter(id_partido == id_p); estadio_info <- estadios_df %>% filter(id_partido == id_p) %>% head(1)
  goles_partido <- goles_df_unificado %>% filter(id_partido == id_p); tarjetas_partido <- tarjetas_df_unificado %>% filter(id_partido == id_p); alineacion_partido <- apariciones_df %>% filter(id_partido == id_p)
  render_equipo_html <- function(df_equipo, goles_del_partido, tarjetas_del_partido) {
    if (is.null(df_equipo) || nrow(df_equipo) == 0) return(tags$p("Нема податоци."))
    starters <- df_equipo %>% filter(tipo == "Titular"); subs <- df_equipo %>% filter(tipo == "Suplente")
    crear_lista_jugadoras <- function(df_j) {
      if (nrow(df_j) == 0) return(tags$p(style = "color:#777;", "Нема."))
      tags$ul(map(1:nrow(df_j), function(j) {
        jugadora <- df_j[j, ]; eventos_html <- tagList()
        goles_jugadora <- goles_del_partido %>% filter(id == jugadora['id'], tipo == "Normal"); if (nrow(goles_jugadora) > 0) walk(1:nrow(goles_jugadora), function(g) { gol <- goles_jugadora[g, ]; eventos_html <<- tagAppendChild(eventos_html, tags$span(class = "player-event goal", HTML(paste0("⚽ ", gol$minuto, "'")))) })
        tarjetas_jugadora <- tarjetas_del_partido %>% filter(id == jugadora['id']); if (nrow(tarjetas_jugadora) > 0) walk(1:nrow(tarjetas_jugadora), function(c) { tarjeta <- tarjetas_jugadora[c, ]; icono_emoji <- if (tarjeta$tipo == "Amarilla") "🟨" else "🟥"; eventos_html <<- tagAppendChild(eventos_html, tags$span(class = "player-event", HTML(paste0(icono_emoji, " ", tarjeta$minuto, "'")))) })
        if (!is.na(jugadora['min_entra']) && jugadora['tipo'] == "Suplente") eventos_html <- tagAppendChild(eventos_html, tags$span(class = "player-event sub-in", paste0("↑", jugadora['min_entra'], "'")))
        if (!is.na(jugadora['min_sale']) && jugadora['min_sale'] < 90 && !is.na(jugadora['minutos_jugados']) && jugadora['minutos_jugados'] > 0) eventos_html <- tagAppendChild(eventos_html, tags$span(class = "player-event sub-out", paste0("↓", jugadora['min_sale'], "'")))
        icono_p <- if (isTRUE(jugadora$es_portera)) "🧤" else ""; icono_c <- if (isTRUE(jugadora$es_capitana)) "(C)" else ""
        tags$li(paste0(jugadora$dorsal, ". "), tags$a(href = "#", onclick = sprintf("mostrarPagina('jugadora-%s')", jugadora$id), jugadora$nombre), icono_p, icono_c, eventos_html)
      }))
    }
    tagList(tags$h4("Почетен состав"), crear_lista_jugadoras(starters), tags$h4("Резерви"), crear_lista_jugadoras(subs))
  }
  tags$div(id = paste0("partido-", id_p), class = "page", tags$a("← Назад", href = "#", onclick = "history.back()", class = "back-link"), tags$h2(paste(partido$local, "vs", partido$visitante)), tags$h3(paste("Конечен резултат:", partido$goles_local, "-", partido$goles_visitante)), tags$p(paste0("Датум: ", partido$fecha, " | Време: ", partido$hora, " | Стадион: "), if (nrow(estadio_info) > 0) tags$a(href = "#", onclick = sprintf("mostrarPagina('стадион-%s')", generar_id_seguro(estadio_info$estadio)), estadio_info$estadio) else "Непознат"), tags$h3("Судии"), tags$ul(class = "sudii-lista", map(1:nrow(arbitros_partido), function(a) { arb <- arbitros_partido[a, ]; tags$li(paste0(arb$uloga, ": "), tags$a(href = "#", onclick = sprintf("mostrarPagina('arbitro-%s')", generar_id_seguro(arb$ime)), arb$ime)) })), tags$h3("Состави"), tags$div(class = "alineaciones-container", tags$div(class = "columna-alineacion", tags$h3(style = "text-align:center;border:none;", tags$a(href = "#", onclick = sprintf("mostrarPagina('equipo-%s')", generar_id_seguro(partido$local)), partido$local)), render_equipo_html(filter(alineacion_partido, equipo == partido$local), goles_partido, tarjetas_partido)), tags$div(class = "columna-alineacion", tags$h3(style = "text-align:center;border:none;", tags$a(href = "#", onclick = sprintf("mostrarPagina('equipo-%s')", generar_id_seguro(partido$visitante)), partido$visitante)), render_equipo_html(filter(alineacion_partido, equipo == partido$visitante), goles_partido, tarjetas_partido))), tags$h3("Хронологија"), tags$ul(class = "timeline", if (nrow(cronologia) > 0) { map(1:nrow(cronologia), function(c) { e <- cronologia[c, ]; tags$li(HTML(paste0("<span class='icon'>", e$icono, "</span>")), paste0(e$minuto, "' - "), HTML(e$texto_evento)) }) } else { tags$li("Нема регистрирани настани.") }), tags$a("← Назад", href = "#", onclick = "history.back()", class = "back-link"))
})

paginas_jugadoras_html <- map(1:nrow(jugadoras_stats_df), function(i) {
  jugadora <- jugadoras_stats_df[i,]
  id_j <- jugadora$id
  player_career_summary <- apariciones_df %>%
    filter(id == id_j) %>%
    group_by(competicion_temporada, competicion_nombre, equipo) %>%
    summarise(
      Повикана = n_distinct(id_partido),
      Одиграни = sum(minutos_jugados > 0, na.rm = TRUE),
      Почетен_состав = sum(tipo == "Titular", na.rm = TRUE),
      Минути = sum(minutos_jugados, na.rm = TRUE),
      .groups = 'drop'
    )
  player_goals_summary <- goles_df_unificado %>%
    filter(id == id_j, tipo == "Normal") %>%
    left_join(partidos_df %>% select(id_partido, competicion_temporada, competicion_nombre), by = "id_partido") %>%
    group_by(competicion_temporada, competicion_nombre, equipo_jugadora) %>%
    summarise(Голови = n(), .groups = 'drop') %>%
    rename(equipo = equipo_jugadora)
  player_cards_summary <- tarjetas_df_unificado %>%
    filter(id == id_j) %>%
    left_join(partidos_df %>% select(id_partido, competicion_temporada, competicion_nombre), by = "id_partido") %>%
    group_by(competicion_temporada, competicion_nombre, equipo) %>%
    summarise(
      Жолти = sum(tipo == "Amarilla", na.rm = TRUE),
      Црвени = sum(tipo == "Roja", na.rm = TRUE),
      .groups = 'drop'
    )
  player_career_final <- player_career_summary %>%
    left_join(player_goals_summary, by = c("competicion_temporada", "competicion_nombre", "equipo")) %>%
    left_join(player_cards_summary, by = c("competicion_temporada", "competicion_nombre", "equipo")) %>%
    mutate(
      Голови = replace_na(Голови, 0),
      Жолти = replace_na(Жолти, 0),
      Црвени = replace_na(Црвени, 0)
    ) %>%
    arrange(desc(competicion_temporada))
  
  tags$div(id=paste0("jugadora-",id_j), class="page",
           tags$a("← Назад", href="#", onclick="history.back()", class="back-link"),
           tags$h2(jugadora$Играч),
           tags$h3("Резиме на кариера"),
           tags$table(class="career-summary-table",
                      tags$thead(
                        tags$tr(
                          tags$th("Сезона"), tags$th("Тим"), tags$th("Натпреварување"),
                          tags$th("Наст."), tags$th("Гол."), tags$th("Мин.")
                        )
                      ),
                      tags$tbody(
                        map(1:nrow(player_career_final), function(j) {
                          stage <- player_career_final[j,]
                          details_id <- paste0("details-", id_j, "-", j)
                          partidos_stage <- apariciones_df %>% filter(id == id_j, competicion_temporada == stage$competicion_temporada, competicion_nombre == stage$competicion_nombre, equipo == stage$equipo) %>% left_join(partidos_df, by="id_partido")
                          goles_stage <- goles_df_unificado %>% filter(id == id_j, id_partido %in% partidos_stage$id_partido)
                          tarjetas_stage <- tarjetas_df_unificado %>% filter(id == id_j, id_partido %in% partidos_stage$id_partido)
                          
                          tagList(
                            tags$tr(class="summary-row", onclick=sprintf("toggleDetails('%s')", details_id),
                                    tags$td(stage$competicion_temporada),
                                    tags$td(tags$a(href="#", onclick=sprintf("mostrarPagina('equipo-%s'); event.stopPropagation();", generar_id_seguro(stage$equipo)), stage$equipo)),
                                    tags$td(stage$competicion_nombre),
                                    tags$td(stage$Одиграни),
                                    tags$td(stage$Голови),
                                    tags$td(stage$Минути)
                            ),
                            tags$tr(id=details_id, class="details-row",
                                    tags$td(colspan="6",
                                            tags$div(class="details-content",
                                                     tags$h4("Детална статистика"),
                                                     tags$table(
                                                       tags$tbody(
                                                         tags$tr(tags$td("Тим"), tags$td(stage$equipo)),
                                                         tags$tr(tags$td("Повикана"), tags$td(stage$Повикана)),
                                                         tags$tr(tags$td("Одиграни"), tags$td(stage$Одиграни)),
                                                         tags$tr(tags$td("Почетен состав"), tags$td(stage$Почетен_состав)),
                                                         tags$tr(tags$td("Минути"), tags$td(stage$Минути)),
                                                         tags$tr(tags$td("Голови"), tags$td(stage$Голови)),
                                                         tags$tr(tags$td("Жолти картони"), tags$td(stage$Жолти)),
                                                         tags$tr(tags$td("Црвени картони"), tags$td(stage$Црвени))
                                                       )
                                                     ),
                                                     tags$h4("Список на натпревари"),
                                                     tags$table(tags$thead(tags$tr(tags$th("Коло"),tags$th("Натпревар"),tags$th("Резултат"),tags$th("Статус"), tags$th("Мин."))),
                                                                tags$tbody(if(nrow(partidos_stage)>0) { map(1:nrow(partidos_stage),function(p_idx){ partido_row <- partidos_stage[p_idx,]; status_partido <- if (partido_row$tipo == "Titular") "Почетен состав" else if (!is.na(partido_row$minutos_jugados) && partido_row$minutos_jugados > 0) "Резерва (одигра)" else "Повикана"; tags$tr(tags$td(partido_row$jornada), tags$td(tags$a(href="#",onclick=sprintf("mostrarPagina('partido-%s')",partido_row$id_partido.x),paste(partido_row$local,"vs",partido_row$visitante))), tags$td(paste(partido_row$goles_local,"-",partido_row$goles_visitante)), tags$td(status_partido), tags$td(if(is.na(partido_row$minutos_jugados)) 0 else partido_row$minutos_jugados)) }) } else { tags$tr(tags$td(colspan="5","Нема одиграни натпревари.")) })),
                                                     tags$h4("Список на голови"),
                                                     tags$table(tags$thead(tags$tr(tags$th("Коло"), tags$th("Натпревар"), tags$th("Минута"))),
                                                                tags$tbody(if(nrow(goles_stage)>0){ map(1:nrow(goles_stage), function(g_idx){ goal_row <- goles_stage[g_idx,]; g_partido<-filter(partidos_df, id_partido==goal_row$id_partido); tags$tr(tags$td(g_partido$jornada), tags$td(tags$a(href="#",onclick=sprintf("mostrarPagina('partido-%s')",goal_row$id_partido),paste(g_partido$local,"vs",g_partido$visitante))), tags$td(goal_row$minuto))}) } else { tags$tr(tags$td(colspan="3","Нема постигнато голови.")) })),
                                                     tags$h4("Список на картони"),
                                                     tags$table(tags$thead(tags$tr(tags$th("Коло"),tags$th("Натпревар"),tags$th("Картон"),tags$th("Минута"),tags$th("Причина"))),
                                                                tags$tbody(if(nrow(tarjetas_stage)>0){ map(1:nrow(tarjetas_stage),function(t_idx){ card_row <- tarjetas_stage[t_idx,]; t_partido<-filter(partidos_df, id_partido==card_row$id_partido); icon<-if(card_row$tipo=="Amarilla")tags$span(class="card-yellow")else tags$span(class="card-red");tags$tr(tags$td(t_partido$jornada),tags$td(tags$a(href="#",onclick=sprintf("mostrarPagina('partido-%s')",card_row$id_partido),paste(t_partido$local,"vs",t_partido$visitante))),tags$td(icon),tags$td(card_row$minuto),tags$td(card_row$motivo))}) } else { tags$tr(tags$td(colspan="5","Нема добиено картони.")) }))
                                            )
                                    )
                            )
                          )
                        })
                      )
           ),
           tags$a("← Назад", href="#", onclick="history.back()", class="back-link")
  )
})

paginas_equipos_html <- map(unique(c(partidos_df$local,partidos_df$visitante)),function(team){id_t<-generar_id_seguro(team);historial<-partidos_df%>%filter(local==team|visitante==team)%>%arrange(jornada);stats<-jugadoras_stats_df%>%filter(Тим==team)%>%select(id,Играч,Повикана,Одиграни_натпревари,Голови,Жолти,Црвени,Минути)%>%arrange(desc(Минути));headers<-c("Играч","Пов","Одиг","Гол","Ж","Ц","Мин");tags$div(id=paste0("equipo-",id_t),class="page",tags$a("← Назад",href="#",onclick="mostrarPagina('portal')",class="back-link"),tags$h2(team),tags$h3("Статистика на играчи"),tags$table(id=paste0("stats-",id_t),tags$thead(tags$tr(map(seq_along(headers),function(i){tags$th(class="sortable-header",onclick=sprintf("sortTable('%s',%d)",paste0("stats-",id_t),i-1),headers[i])}))),tags$tbody(if(nrow(stats)>0){map(1:nrow(stats),function(j){p<-stats[j,];tags$tr(tags$td(tags$a(href="#",onclick=sprintf("mostrarPagina('jugadora-%s')",p$id),p$Играч)),tags$td(p$Повикана),tags$td(p$Одиграни_натпревари),tags$td(p$Голови),tags$td(p$Жолти),tags$td(p$Црвени),tags$td(p$Минути))})}else tags$tr(tags$td(colspan=length(headers),"Нема податоци.")))),tags$h3("Историја на натпревари"),tags$table(tags$thead(tags$tr(tags$th("Коло"),tags$th("Домаќин"),tags$th("Гостин"),tags$th("Резултат"))),tags$tbody(if(nrow(historial)>0){map(1:nrow(historial),function(p){partido<-historial[p,];tags$tr(tags$td(partido$jornada),tags$td(partido$local),tags$td(partido$visitante),tags$td(tags$a(href="#",onclick=sprintf("mostrarPagina('partido-%s')",partido$id_partido),paste(partido$goles_local,"-",partido$goles_visitante))))})}else tags$tr(tags$td(colspan="4","Нема одиграни натпревари.")))),tags$a("← Назад",href="#",onclick="mostrarPagina('portal')",class="back-link"))})

paginas_arbitros_html <- map(unique(arbitros_df$ime),function(arb){id_a<-generar_id_seguro(arb);historial<-arbitros_df%>%filter(ime==arb)%>%left_join(partidos_df,by="id_partido")%>%arrange(jornada);tags$div(id=paste0("arbitro-",id_a),class="page",tags$a("← Назад",href="#",onclick="mostrarPagina('portal')",class="back-link"),tags$h2(arb),tags$h3("Историја"),tags$table(tags$thead(tags$tr(tags$th("Коло"),tags$th("Натпревар"),tags$th("Резултат"),tags$th("Улога"))),tags$tbody(if(nrow(historial)>0){map(1:nrow(historial),function(p){partido<-historial[p,];tags$tr(tags$td(partido$jornada),tags$td(tags$a(href="#",onclick=sprintf("mostrarPagina('partido-%s')",partido$id_partido),paste(partido$local,"vs",partido$visitante))),tags$td(paste(partido$goles_local,"-",partido$goles_visitante)),tags$td(partido$uloga))})}else tags$tr(tags$td(colspan="4","Нема делегирани натпревари.")))),tags$a("← Назад",href="#",onclick="mostrarPagina('portal')",class="back-link"))})

paginas_estadios_html <- map(unique(na.omit(estadios_df$estadio)),function(est){id_e<-generar_id_seguro(est);historial<-estadios_df%>%filter(estadio==est)%>%arrange(jornada);tags$div(id=paste0("стадион-",id_e),class="page",tags$a("← Назад",href="#",onclick="mostrarPagina('portal')",class="back-link"),tags$h2(est),tags$h3("Историја"),tags$table(tags$thead(tags$tr(tags$th("Коло"),tags$th("Натпревар"),tags$th("Резултат"))),tags$tbody(if(nrow(historial)>0){map(1:nrow(historial),function(p){partido<-historial[p,];tags$tr(tags$td(partido$jornada),tags$td(tags$a(href="#",onclick=sprintf("mostrarPagina('partido-%s')",partido$id_partido),paste(partido$local,"vs",partido$visitante))),tags$td(paste(partido$goles_local,"-",partido$goles_visitante)))})}else tags$tr(tags$td(colspan="3","Нема одиграни натпревари.")))),tags$a("← Назад",href="#",onclick="mostrarPagina('portal')",class="back-link"))})

# -------------------------------------------------------------------------
# PASO 10: CONSTRUIR Y GUARDAR EL ARCHIVO HTML FINAL
# -------------------------------------------------------------------------
message("Составување на финалната HTML датотека...")
pagina_completa <- tags$html(lang = "mk",
                             tags$head(
                               tags$meta(charset="UTF-8"), 
                               tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
                               tags$title("Фудбалски портал МК"), 
                               tags$style(HTML(estilo_css))
                             ),
                             tags$body(
                               tags$div(class = "container",
                                        tags$h1("Фудбалски портал МК"),
                                        tags$div(class = "search-container",
                                                 tags$form(action = "#", onsubmit = "showSearchResults(); return false;",
                                                           tags$input(type = "text", id = "search-input", class = "search-input", placeholder = "Пребарај играч, тим, судија, натпреварување...", onkeyup = "handleSearchInput(event)"),
                                                           tags$button(type = "submit", class = "search-button", "Пребарај")
                                                 ),
                                                 tags$div(id = "search-suggestions")
                                        ),
                                        pagina_portal,
                                        paginas_por_competicion,
                                        tags$div(id = "search-results", class = "page", tags$a("← Назад кон порталот", href="#", onclick="mostrarPagina('portal')", class="back-link"), tags$h2(id = "search-results-title"), tags$div(id = "search-results-list")),
                                        paginas_partidos_html, 
                                        paginas_jugadoras_html, 
                                        paginas_equipos_html,
                                        paginas_arbitros_html, 
                                        paginas_estadios_html
                               ),
                               tags$script(type = "application/json", id = "search-data-json", HTML(search_data_json)),
                               tags$script(HTML(script_js))
                             )
)

ruta_salida_html <- "index.html"
save_html(pagina_completa, file = ruta_salida_html)

message(paste("\nHTML ИЗВЕШТАЈОТ Е УСПЕШНО КРЕИРАН!"))
message(paste("Проверете ја излезната датотека во:", ruta_salida_html))