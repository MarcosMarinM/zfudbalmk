# =========================================================================
# SCRIPT DE GENERACIÓN DE INFORME HTML (VERSIÓN REFACTORIZADA - MULTI-PÁGINA)
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
RUTA_BASE_SALIDA <- "informe_web"
RUTA_ASSETS <- file.path(RUTA_BASE_SALIDA, "assets")
RUTA_COMPETICIONES <- file.path(RUTA_BASE_SALIDA, "competiciones")
RUTA_PARTIDOS <- file.path(RUTA_BASE_SALIDA, "partidos")
RUTA_JUGADORAS <- file.path(RUTA_BASE_SALIDA, "jugadoras")
RUTA_EQUIPOS <- file.path(RUTA_BASE_SALIDA, "equipos")
RUTA_ARBITROS <- file.path(RUTA_BASE_SALIDA, "arbitros")
RUTA_ESTADIOS <- file.path(RUTA_BASE_SALIDA, "estadios")

# Crear todos los directorios de una vez. showWarnings = FALSE evita avisos si ya existen.
walk(c(RUTA_BASE_SALIDA, RUTA_ASSETS, RUTA_COMPETICIONES, RUTA_PARTIDOS, 
       RUTA_JUGADORAS, RUTA_EQUIPOS, RUTA_ARBITROS, RUTA_ESTADIOS), 
     dir.create, showWarnings = FALSE, recursive = TRUE)

message("Estructura de directorios creada en: ", RUTA_BASE_SALIDA)


# =========================================================================
# FUNCIONES AUXILIARES (CON MODIFICACIONES)
# =========================================================================
# --- SIN CAMBIOS en estas dos funciones ---
generar_terminos_busqueda <- function(nombre) {
  nombre_lower <- tolower(nombre)
  versions <- c(nombre_lower)
  map_base <- c('а'='a', 'б'='b', 'в'='v', 'г'='g', 'д'='d', 'ѓ'='g', 'е'='e', 'ж'='z', 'з'='z', 'ѕ'='dz', 'и'='i', 'ј'='j', 'к'='k', 'л'='l', 'љ'='l', 'м'='m', 'н'='n', 'њ'='n', 'о'='o', 'п'='p', 'р'='r', 'с'='s', 'т'='t', 'ќ'='k', 'у'='u', 'ф'='f', 'х'='h', 'ц'='c', 'ч'='c', 'џ'='dz', 'ш'='s')
  map_diacritic <- c('а'='a', 'б'='b', 'в'='v', 'г'='g', 'д'='d', 'ѓ'='đ', 'е'='e', 'ж'='ž', 'з'='z', 'ѕ'='dz', 'и'='i', 'ј'='j', 'к'='k', 'л'='ll', 'љ'='lj', 'м'='m', 'н'='n', 'њ'='nj', 'о'='o', 'п'='p', 'р'='r', 'с'='s', 'т'='t', 'ќ'='ć', 'у'='u', 'ф'='f', 'х'='h', 'ц'='c', 'ч'='č', 'џ'='dž', 'ш'='š')
  map_digraph <- c('а'='a', 'б'='b', 'в'='v', 'г'='g', 'д'='d', 'ѓ'='g', 'е'='e', 'ж'='zh', 'з'='z', 'ѕ'='dz', 'и'='i', 'ј'='j', 'к'='k', 'л'='l', 'љ'='lj', 'м'='m', 'н'='n', 'њ'='nj', 'о'='o', 'п'='p', 'р'='r', 'с'='s', 'т'='t', 'ќ'='kj', 'у'='u', 'ф'='f', 'х'='h', 'ц'='c', 'ч'='ch', 'џ'='dzh', 'ш'='sh')
  map_alternate <- c('а'='a', 'б'='b', 'в'='v', 'г'='g', 'д'='d', 'ѓ'='dj', 'е'='ë', 'ж'='z', 'з'='z', 'ѕ'='z', 'и'='i', 'ј'='j', 'к'='k', 'л'='ll', 'љ'='l', 'м'='m', 'н'='n', 'њ'='n', 'о'='o', 'п'='p', 'р'='r', 'с'='s', 'т'='t', 'ќ'='c', 'у'='y', 'ф'='f', 'х'='h', 'ц'='ts', 'ч'='ç', 'џ'='xh', 'ш'='sh')
  versions <- c(versions, str_replace_all(nombre_lower, map_base), str_replace_all(nombre_lower, map_diacritic), str_replace_all(nombre_lower, map_digraph), str_replace_all(nombre_lower, map_alternate), str_replace_all(nombre_lower, c('ќ' = 'ḱ')), str_replace_all(nombre_lower, c('њ' = 'ń')), str_replace_all(nombre_lower, c('њ' = 'ñ')))
  map_norm_diacritics <- c('š'='s', 'č'='c', 'ž'='z', 'đ'='dj', 'ć'='c', 'ń'='n', 'ñ'='n', 'ḱ'='k', 'ë'='e', 'ç'='c')
  versions <- c(versions, str_replace_all(nombre_lower, map_norm_diacritics))
  return(paste(unique(versions), collapse = " "))
}
generar_id_seguro <- function(nombre) {
  map_id <- c('а'='a', 'б'='b', 'в'='v', 'г'='g', 'д'='d', 'ѓ'='g', 'е'='e', 'ж'='z', 'з'='z', 'ѕ'='dz', 'и'='i', 'ј'='j', 'к'='k', 'л'='l', 'љ'='l', 'м'='m', 'н'='n', 'њ'='n', 'о'='o', 'п'='p', 'р'='r', 'с'='s', 'т'='t', 'ќ'='k', 'у'='u', 'ф'='f', 'х'='h', 'ц'='c', 'ч'='c', 'џ'='dz', 'ш'='s')
  nombre_latin <- str_replace_all(tolower(nombre), map_id)
  id_sanitizada <- gsub("[\\s/]+", "_", nombre_latin)
  id_sanitizada <- gsub("[^a-z0-9_\\-]+", "", id_sanitizada)
  id_sanitizada <- gsub("_{2,}", "_", id_sanitizada)
  id_sanitizada <- gsub("^_+|_+$", "", id_sanitizada)
  return(id_sanitizada)
}

# --- MODIFICADA para usar rutas de archivo relativas ---
crear_botones_navegacion <- function(ruta_relativa_assets = ".") {
  # "." si estamos en la raíz (index.html).
  # ".." si estamos en una subcarpeta (ej: /jugadoras/ficha.html).
  tags$div(class = "nav-buttons",
           tags$a("← Назад", href = "#", onclick = "history.back(); return false;", class = "back-link"),
           tags$a("🏠 Почетна", href = file.path(ruta_relativa_assets, "index.html"), class = "back-link")
  )
}

# =========================================================================
# NUEVA FUNCIÓN: PLANTILLA HTML
# =========================================================================
crear_pagina_html <- function(contenido_principal, titulo_pagina = "Фудбалски портал МК", ruta_relativa_assets = ".", search_data_json, script_contraseña) {
  # Esta función crea la estructura base de cualquier página HTML.
  # contenido_principal: El objeto de htmltools específico para esa página.
  # titulo_pagina: El título que aparecerá en la pestaña del navegador.
  # ruta_relativa_assets: La ruta para encontrar la carpeta 'assets'.
  
  tags$html(lang = "mk",
            tags$head(
              tags$meta(charset="UTF-8"),
              tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
              tags$title(titulo_pagina),
              tags$link(rel = "stylesheet", href = file.path(ruta_relativa_assets, "assets/style.css")),
              # El script de la contraseña se pone en el head para que se ejecute antes de mostrar nada
              script_contraseña
            ),
            tags$body(
              tags$div(class = "container",
                       # Cabecera estándar con título y búsqueda
                       tags$h1(tags$a(href = file.path(ruta_relativa_assets, "index.html"), style = "color: inherit; text-decoration: none;", "Фудбалски портал МК")),
                       tags$div(class = "search-container",
                                tags$form(action = "#", onsubmit = "showSearchResults(); return false;",
                                          tags$input(type = "text", id = "search-input", class = "search-input", placeholder = "Пребарај фудбалерка, тим, судија, стадион...", onkeyup = "handleSearchInput(event)"),
                                          tags$button(type = "submit", class = "search-button", "Пребарај")
                                ),
                                tags$div(id = "search-suggestions")
                       ),
                       # Contenido principal específico de la página
                       contenido_principal
              ),
              # Los datos de búsqueda y el script principal se añaden al final del body
              tags$script(type = "application/json", id = "search-data-json", HTML(search_data_json)),
              tags$script(defer = NA, src = file.path(ruta_relativa_assets, "assets/script.js"))
            )
  )
}


message("Започнување со генерирање на HTML извештајот...")

# -------------------------------------------------------------------------
# PASO 7: PREPARACIÓN DE DATOS (SIN CAMBIOS)
# -------------------------------------------------------------------------
# Se asume que TODO el código del PASO 7 del script original se ejecuta aquí.
# Este paso es crucial y debe estar presente. Simplemente no se muestra aquí
# para mayor claridad, ya que su lógica interna no ha cambiado.
#
# Al final de este paso, tenemos los dataframes y variables listos:
# - partidos_df, goles_df_unificado, tarjetas_df_unificado, apariciones_df
# - competiciones_unicas_df, jugadoras_stats_df, arbitros_df, estadios_df
# - search_index_df, search_data_json
# -------------------------------------------------------------------------
# (Asegúrate de que el código original del PASO 7 esté aquí en tu script)


# =========================================================================
# PASO 8 y 9: EXTERNALIZACIÓN Y GENERACIÓN DE PÁGINAS HTML
# =========================================================================

# --- 8.1: Guardar CSS y JS en archivos externos (VERSIÓN CORREGIDA) ---
# Usamos r"()" para definir cadenas de texto multi-línea y evitar el límite de caracteres de R.
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
.timeline .icon { margin-right: 10px; font-s
ize: 1.2em; width: 24px; text-align: center; }
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
)"
writeLines(estilo_css, file.path(RUTA_ASSETS, "style.css"))

script_js <- r"(
let searchData = [];
document.addEventListener('DOMContentLoaded', initializeSearch);
function initializeSearch() {
  const searchDataElement = document.getElementById('search-data-json');
  if (searchDataElement) {
    try { searchData = JSON.parse(searchDataElement.textContent); } catch (e) { console.error('Error parsing search data JSON:', e); }
  }
  document.addEventListener('click', function(event) {
    const searchContainer = document.querySelector('.search-container');
    if (searchContainer && !searchContainer.contains(event.target)) {
      const suggestions = document.getElementById('search-suggestions');
      if(suggestions) suggestions.style.display = 'none';
    }
  });
}
function toggleDetails(elementId) {
  const detailsRow = document.getElementById(elementId);
  if (detailsRow) {
    if (detailsRow.style.display === 'table-row') {
      detailsRow.style.display = 'none';
    } else {
      detailsRow.style.display = 'table-row';
    }
  }
}
function getBasePath() {
  const path = window.location.pathname;
  // Si la URL termina en .html, estamos en una subpágina.
  if (path.endsWith('.html') && !path.endsWith('index.html')) {
      const segments = path.split('/');
      // Si hay más de 2 segmentos (ej. /repo/subfolder/page.html), estamos en una subcarpeta
      if (segments.length > 2) return '..';
  }
  // Si no, estamos en la raíz (index.html o la carpeta principal)
  return '.';
}
function handleSearchInput(event) {
  if (event.key === 'Enter') { event.preventDefault(); return; }
  const input = document.getElementById('search-input');
  const suggestionsContainer = document.getElementById('search-suggestions');
  const query = input.value.trim().toLowerCase();
  if (query.length < 2) { suggestionsContainer.innerHTML = ''; suggestionsContainer.style.display = 'none'; return; }
  const searchTokens = query.split(' ').filter(t => t.length > 0);
  if (searchTokens.length === 0) { suggestionsContainer.innerHTML = ''; suggestionsContainer.style.display = 'none'; return; }
  const filteredResults = searchData.filter(item => {
    return searchTokens.every(token => item.search_terms.includes(token));
  });
  const top5 = filteredResults.slice(0, 5);
  if (top5.length === 0) { suggestionsContainer.innerHTML = ''; suggestionsContainer.style.display = 'none'; return; }
  suggestionsContainer.innerHTML = top5.map(item => `<a href='${generateLink(item.target_id)}'><strong>${item.Име}</strong> <span class='search-result-type'>(${item.Тип})</span></a>`).join('');
  suggestionsContainer.style.display = 'block';
}
function generateLink(target_id) {
  const basePath = getBasePath();
  const parts = target_id.split('-');
  const type = parts[0];
  const id_parts = parts.slice(1);
  
  // Maneja casos donde el ID contiene guiones, como en 'menu-competicion-[...]'
  let id = id_parts.join('-'); 
  
  let folder;
  switch(type) {
    case 'jugadora': folder = 'jugadoras'; break;
    case 'equipo': folder = 'equipos'; break;
    case 'arbitro': folder = 'arbitros'; break;
    case 'стадион': folder = 'estadios'; break;
    case 'menu': 
      folder = 'competiciones';
      // Para competiciones, el target_id es 'menu-competicion-[id]', así que quitamos 'competicion-'
      id = id.replace('competicion-', '');
      break;
    default: return `${basePath}/index.html`;
  }
  return `${basePath}/${folder}/${id}.html`;
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
               # ENLACE MODIFICADO: Apunta a un archivo HTML
               tags$a(href = file.path("competiciones", paste0(comp$competicion_id, ".html")),
                      class="portal-button", comp$nombre_completo)
             })
           } else { tags$p("Не се пронајдени натпреварувања.") }
  )
)

pagina_portal_final <- crear_pagina_html(
  contenido_principal = contenido_portal,
  titulo_pagina = "Фудбалски портал МК",
  ruta_relativa_assets = ".", # En la raíz, la ruta es "."
  search_data_json = search_data_json,
  script_contraseña = script_contraseña
)
save_html(pagina_portal_final, file = file.path(RUTA_BASE_SALIDA, "index.html"))

# --- 9.2: Generación de páginas por competición en un bucle ---
message("Генерирање на страници за секое натпреварување...")
walk(1:nrow(competiciones_unicas_df), function(i) {
  
  # Lógica de preparación de datos (sin cambios, necesaria en cada iteración)
  comp_info <- competiciones_unicas_df[i,]
  comp_id <- comp_info$competicion_id
  comp_nombre <- comp_info$nombre_completo
  partidos_comp <- partidos_df %>% filter(competicion_nombre == comp_info$competicion_nombre, competicion_temporada == comp_info$competicion_temporada)
  ids_partidos_comp <- partidos_comp$id_partido
  goles_comp <- goles_df_unificado %>% filter(id_partido %in% ids_partidos_comp)
  tarjetas_comp <- tarjetas_df_unificado %>% filter(id_partido %in% ids_partidos_comp)
  apariciones_comp <- apariciones_df %>% filter(id_partido %in% ids_partidos_comp)
  is_cup <- str_detect(tolower(comp_info$competicion_nombre), "куп")
  
  # ==========================================================
  # 1. PÁGINA DE MENÚ DE LA COMPETICIÓN ([comp_id].html)
  # ==========================================================
  # Definimos los botones del menú con enlaces a los archivos HTML que VAMOS a crear
  contenido_menu_botones <- if (is_cup) {
    tags$div(class="menu-container",
             tags$a(href=paste0(comp_id, "_partidos.html"), class="menu-button", "Распоред"),
             tags$a(href=paste0(comp_id, "_goleadoras.html"), class="menu-button", "Стрелци"),
             tags$a(href=paste0(comp_id, "_sanciones.html"), class="menu-button", "Дисциплинска"))
  } else {
    tags$div(class="menu-container",
             tags$a(href=paste0(comp_id, "_partidos.html"), class="menu-button", "Распоред"),
             tags$a(href=paste0(comp_id, "_clasificacion.html"), class="menu-button", "Табела"),
             tags$a(href=paste0(comp_id, "_goleadoras.html"), class="menu-button", "Стрелци"),
             tags$a(href=paste0(comp_id, "_sanciones.html"), class="menu-button", "Дисциплинска"))
  }
  
  contenido_menu_completo <- tagList(
    crear_botones_navegacion(ruta_relativa_assets = ".."),
    tags$h2(comp_nombre),
    contenido_menu_botones
  )
  
  pagina_menu_final <- crear_pagina_html(
    contenido_principal = contenido_menu_completo,
    titulo_pagina = comp_nombre, 
    ruta_relativa_assets = "..",
    search_data_json = search_data_json, 
    script_contraseña = script_contraseña
  )
  save_html(pagina_menu_final, file = file.path(RUTA_COMPETICIONES, paste0(comp_id, ".html")))
  
  # ==========================================================
  # 2. PÁGINA DE PARTIDOS/CALENDARIO ([comp_id]_partidos.html)
  # ==========================================================
  jornadas_comp <- if (nrow(partidos_comp) > 0) {
    jornadas_unicas_df <- data.frame(jornada = unique(partidos_comp$jornada)) %>%
      mutate(order_key = case_when(str_detect(jornada, "1/16") ~ 1, str_detect(jornada, "1/8") ~ 2, str_detect(jornada, "1/4") ~ 3, str_detect(jornada, "1/2") ~ 4, str_detect(jornada, "Ф$|ф$|финале") ~ 5, !is_cup ~ as.numeric(jornada), TRUE ~ 99)) %>%
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
          tags$a(class="partido-link", 
                 # ENLACE MODIFICADO: Apunta al archivo del partido
                 href=file.path("..", "partidos", paste0(partido$id_partido, ".html")),
                 tags$span(class="equipo equipo-local", partido$local),
                 tags$span(class="resultado", paste(partido$goles_local,"-",partido$goles_visitante)),
                 tags$span(class="equipo equipo-visitante", partido$visitante))
        })
      )
    })
  )
  
  pagina_partidos_final <- crear_pagina_html(contenido_partidos, paste("Распоред -", comp_nombre), "..", search_data_json, script_contraseña)
  save_html(pagina_partidos_final, file.path(RUTA_COMPETICIONES, paste0(comp_id, "_partidos.html")))
  
  
  # ==========================================================
  # 3. PÁGINA DE CLASIFICACIÓN (si es liga) ([comp_id]_clasificacion.html)
  # ==========================================================
  if (!is_cup) {
    calcular_clasificacion <- function(partidos) { if (is.null(partidos) || nrow(partidos) == 0) return(data.frame(Порака = "Нема обработени валидни натпревари.")); locales <- partidos %>% select(equipo = local, GF = goles_local, GC = goles_visitante); visitantes <- partidos %>% select(equipo = visitante, GF = goles_visitante, GC = goles_local); resultados_por_equipo <- bind_rows(locales, visitantes) %>% mutate(Pts = case_when(GF > GC ~ 3, GF < GC ~ 0, TRUE ~ 1), resultado = case_when(GF > GC ~ "Поб", GF < GC ~ "Пор", TRUE ~ "Нер")); clasificacion <- resultados_por_equipo %>% group_by(Тим = equipo) %>% summarise(Н = n(), Бод. = sum(Pts), Поб = sum(resultado == "Поб"), Нер = sum(resultado == "Нер"), Пор = sum(resultado == "Пор"), ДГ = sum(GF), ПГ = sum(GC), .groups = 'drop') %>% mutate(ГР = ДГ - ПГ) %>% arrange(desc(Бод.), desc(ГР), desc(ДГ)) %>% mutate(Поз. = row_number()) %>% select(Поз., Тим, Н, Поб, Нер, Пор, ДГ, ПГ, ГР, Бод.); return(clasificacion)}
    clasificacion_df_comp <- calcular_clasificacion(partidos_comp)
    
    contenido_clasificacion <- tagList(
      crear_botones_navegacion(".."),
      tags$h2(paste("Табела -", comp_nombre)),
      tags$table(tags$thead(tags$tr(map(names(clasificacion_df_comp), tags$th))),
                 tags$tbody(map(1:nrow(clasificacion_df_comp), function(j) {
                   tr <- clasificacion_df_comp[j,]
                   tags$tr(map(tr, function(cell) {
                     if(is.character(cell) && cell %in% clasificacion_df_comp$Тим) {
                       # ENLACE MODIFICADO: Apunta al archivo del equipo
                       tags$td(tags$a(href=file.path("..", "equipos", paste0(generar_id_seguro(cell), ".html")), cell))
                     } else { tags$td(cell) }
                   }))
                 })))
    )
    pagina_clasificacion_final <- crear_pagina_html(contenido_clasificacion, paste("Табела -", comp_nombre), "..", search_data_json, script_contraseña)
    save_html(pagina_clasificacion_final, file = file.path(RUTA_COMPETICIONES, paste0(comp_id, "_clasificacion.html")))
  }
  
  # ==========================================================
  # 4. PÁGINA DE GOLEADORAS ([comp_id]_goleadoras.html)
  # ==========================================================
  goles_por_jugadora_comp <- goles_comp %>% filter(!is.na(id), tipo == "Normal") %>% group_by(id) %>% summarise(Голови = n(), .groups = 'drop')
  jugadoras_info_comp <- apariciones_comp %>% distinct(id, Фудбалерка = nombre, Тим = equipo)
  tabla_goleadoras_comp <- goles_por_jugadora_comp %>% left_join(jugadoras_info_comp, by="id") %>% filter(!is.na(Фудбалерка)) %>% arrange(desc(Голови)) %>% mutate(Поз. = min_rank(desc(Голови))) %>% select(Поз., id, Фудбалерка, Тим, Голови)
  
  contenido_goleadoras <- tagList(
    crear_botones_navegacion(".."),
    tags$h2(paste("Листа на стрелци -", comp_nombre)),
    tags$table(
      tags$thead(tags$tr(map(names(tabla_goleadoras_comp %>% select(-id)), tags$th))),
      tags$tbody(map(1:nrow(tabla_goleadoras_comp), function(j){
        g <- tabla_goleadoras_comp[j,]
        tags$tr(
          tags$td(g$Поз.),
          # ENLACE MODIFICADO: Apunta al archivo de la jugadora
          tags$td(tags$a(href=file.path("..", "jugadoras", paste0(g$id, ".html")), g$Фудбалерка)),
          # ENLACE MODIFICADO: Apunta al archivo del equipo
          tags$td(tags$a(href=file.path("..", "equipos", paste0(generar_id_seguro(g$Тим), ".html")), g$Тим)),
          tags$td(g$Голови)
        )
      }))
    )
  )
  pagina_goleadoras_final <- crear_pagina_html(contenido_goleadoras, paste("Стрелци -", comp_nombre), "..", search_data_json, script_contraseña)
  save_html(pagina_goleadoras_final, file.path(RUTA_COMPETICIONES, paste0(comp_id, "_goleadoras.html")))
  
  # ==========================================================
  # 5. PÁGINA DE SANCIONES ([comp_id]_sanciones.html)
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
            # ENLACE MODIFICADO
            tags$td(tags$a(href=file.path("..", "jugadoras", paste0(s$id, ".html")), s$Фудбалерка)),
            # ENLACE MODIFICADO
            tags$td(tags$a(href=file.path("..", "equipos", paste0(generar_id_seguro(s$Тим), ".html")), s$Тим)),
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
  save_html(pagina_sanciones_final, file.path(RUTA_COMPETICIONES, paste0(comp_id, "_sanciones.html")))
  
}) # Fin del bucle walk para competiciones

# --- 9.3: Generación de páginas globales (perfiles) ---
message("Генерирање на страници за секој натпревар, фудбалерка, тим, судија и стадион...")

# Bucle para PARTIDOS
walk(1:nrow(partidos_df), function(i) {
  partido <- partidos_df[i, ]; id_p <- partido$id_partido
  resumen_partido <- purrr::keep(resultados_exitosos, ~.x$partido_info$id_partido == id_p)[[1]]
  cronologia <- generar_cronologia_df(id_p, resumen_partido) # Esta función debe ser actualizada
  arbitros_partido <- arbitros_df %>% filter(id_partido == id_p)
  estadio_info <- estadios_df %>% filter(id_partido == id_p) %>% head(1)
  goles_partido <- goles_df_unificado %>% filter(id_partido == id_p)
  tarjetas_partido <- tarjetas_df_unificado %>% filter(id_partido == id_p)
  alineacion_partido <- apariciones_df %>% filter(id_partido == id_p)
  
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
        tags$li(paste0(jugadora$dorsal, ". "), tags$a(href = file.path("..", "jugadoras", paste0(jugadora$id, ".html")), jugadora$nombre), icono_p, icono_c, eventos_html)
      }))
    }
    tagList(tags$h4("Почетен состав"), crear_lista_jugadoras(starters), tags$h4("Резерви"), crear_lista_jugadoras(subs))
  }
  
  contenido_partido <- tagList(
    crear_botones_navegacion(".."),
    tags$h2(paste(partido$local, "vs", partido$visitante)), 
    tags$h3(paste("Конечен резултат:", partido$goles_local, "-", partido$goles_visitante)), 
    tags$p(paste0("Датум: ", partido$fecha, " | Време: ", partido$hora, " | Стадион: "), if (nrow(estadio_info) > 0) tags$a(href = file.path("..", "estadios", paste0(generar_id_seguro(estadio_info$estadio), ".html")), estadio_info$estadio) else "Непознат"), 
    tags$h3("Судии"), 
    tags$ul(class = "sudii-lista", map(1:nrow(arbitros_partido), function(a) { arb <- arbitros_partido[a, ]; tags$li(paste0(arb$uloga, ": "), tags$a(href = file.path("..", "arbitros", paste0(generar_id_seguro(arb$ime), ".html")), arb$ime)) })), 
    tags$h3("Состави"), 
    tags$div(class = "alineaciones-container", 
             tags$div(class = "columna-alineacion", tags$h3(style = "text-align:center;border:none;", tags$a(href = file.path("..", "equipos", paste0(generar_id_seguro(partido$local), ".html")), partido$local)), render_equipo_html(filter(alineacion_partido, equipo == partido$local), goles_partido, tarjetas_partido)), 
             tags$div(class = "columna-alineacion", tags$h3(style = "text-align:center;border:none;", tags$a(href = file.path("..", "equipos", paste0(generar_id_seguro(partido$visitante), ".html")), partido$visitante)), render_equipo_html(filter(alineacion_partido, equipo == partido$visitante), goles_partido, tarjetas_partido))), 
    tags$h3("Хронологија"), 
    tags$ul(class = "timeline", if (nrow(cronologia) > 0) { map(1:nrow(cronologia), function(c) { e <- cronologia[c, ]; tags$li(HTML(paste0("<span class='icon'>", e$icono, "</span>")), paste0(e$minuto, "' - "), HTML(e$texto_evento)) }) } else { tags$li("Нема регистрирани настани.") }), 
    crear_botones_navegacion("..")
  )
  
  pagina_partido_final <- crear_pagina_html(contenido_partido, paste(partido$local, "vs", partido$visitante), "..", search_data_json, script_contraseña)
  save_html(pagina_partido_final, file = file.path(RUTA_PARTIDOS, paste0(id_p, ".html")))
})

# Bucle para JUGADORAS
walk(1:nrow(jugadoras_stats_df), function(i) {
  jugadora <- jugadoras_stats_df[i,]
  id_j <- jugadora$id
  # Lógica de datos de la jugadora (sin cambios)
  player_career_summary <- apariciones_df %>% filter(id == id_j) %>% group_by(competicion_temporada, competicion_nombre, equipo) %>% summarise(Повикана = n_distinct(id_partido), Одиграни = sum(minutos_jugados > 0, na.rm = TRUE), Почетен_состав = sum(tipo == "Titular", na.rm = TRUE), Минути = sum(minutos_jugados, na.rm = TRUE), .groups = 'drop')
  player_goals_summary <- goles_df_unificado %>% filter(id == id_j, tipo == "Normal") %>% left_join(partidos_df %>% select(id_partido, competicion_temporada, competicion_nombre), by = "id_partido") %>% group_by(competicion_temporada, competicion_nombre, equipo_jugadora) %>% summarise(Голови = n(), .groups = 'drop') %>% rename(equipo = equipo_jugadora)
  player_cards_summary <- tarjetas_df_unificado %>% filter(id == id_j) %>% left_join(partidos_df %>% select(id_partido, competicion_temporada, competicion_nombre), by = "id_partido") %>% group_by(competicion_temporada, competicion_nombre, equipo) %>% summarise(Жолти = sum(tipo == "Amarilla", na.rm = TRUE), Црвени = sum(tipo == "Roja", na.rm = TRUE), .groups = 'drop')
  player_career_final <- player_career_summary %>% left_join(player_goals_summary, by = c("competicion_temporada", "competicion_nombre", "equipo")) %>% left_join(player_cards_summary, by = c("competicion_temporada", "competicion_nombre", "equipo")) %>% mutate(Голови = replace_na(Голови, 0), Жолти = replace_na(Жолти, 0), Црвени = replace_na(Црвени, 0)) %>% arrange(desc(competicion_temporada))
  
  contenido_jugadora <- tagList(
    crear_botones_navegacion(".."),
    tags$h2(jugadora$Фудбалерка),
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
                           tags$td(tags$a(href=file.path("..", "equipos", paste0(generar_id_seguro(stage$equipo), ".html")), onclick="event.stopPropagation();", stage$equipo)),
                           tags$td(stage$competicion_nombre), tags$td(stage$Одиграни), tags$td(stage$Голови), tags$td(stage$Минути)),
                   tags$tr(id=details_id, class="details-row", tags$td(colspan="6", tags$div(class="details-content",
                                                                                             tags$h4("Детална статистика"), tags$table(tags$tbody(tags$tr(tags$td("Тим"), tags$td(stage$equipo)), tags$tr(tags$td("Повикана"), tags$td(stage$Повикана)), tags$tr(tags$td("Одиграни"), tags$td(stage$Одиграни)), tags$tr(tags$td("Почетен состав"), tags$td(stage$Почетен_состав)), tags$tr(tags$td("Минути"), tags$td(stage$Минути)), tags$tr(tags$td("Голови"), tags$td(stage$Голови)), tags$tr(tags$td("Жолти картони"), tags$td(stage$Жолти)), tags$tr(tags$td("Црвени картони"), tags$td(stage$Црвени)))),
                                                                                             tags$h4("Список на натпревари"), tags$table(tags$thead(tags$tr(tags$th("Коло"),tags$th("Натпревар"),tags$th("Резултат"),tags$th("Статус"), tags$th("Мин."))), tags$tbody(if(nrow(partidos_stage)>0) { map(1:nrow(partidos_stage),function(p_idx){ partido_row <- partidos_stage[p_idx,]; status_partido <- if (partido_row$tipo == "Titular") "Почетен состав" else if (!is.na(partido_row$minutos_jugados) && partido_row$minutos_jugados > 0) "Резерва (одигра)" else "Повикана"; tags$tr(tags$td(partido_row$jornada), tags$td(tags$a(href=file.path("..", "partidos", paste0(partido_row$id_partido, ".html")),paste(partido_row$local,"vs",partido_row$visitante))), tags$td(paste(partido_row$goles_local,"-",partido_row$goles_visitante)), tags$td(status_partido), tags$td(if(is.na(partido_row$minutos_jugados)) 0 else partido_row$minutos_jugados)) }) } else { tags$tr(tags$td(colspan="5","Нема одиграни натпревари.")) })),
                                                                                             tags$h4("Список на голови"), tags$table(tags$thead(tags$tr(tags$th("Коло"), tags$th("Натпревар"), tags$th("Минута"))), tags$tbody(if(nrow(goles_stage)>0){ map(1:nrow(goles_stage), function(g_idx){ goal_row <- goles_stage[g_idx,]; g_partido<-filter(partidos_df, id_partido==goal_row$id_partido); tags$tr(tags$td(g_partido$jornada), tags$td(tags$a(href=file.path("..", "partidos", paste0(goal_row$id_partido, ".html")),paste(g_partido$local,"vs",g_partido$visitante))), tags$td(goal_row$minuto))}) } else { tags$tr(tags$td(colspan="3","Нема постигнато голови.")) })),
                                                                                             tags$h4("Список на картони"), tags$table(tags$thead(tags$tr(tags$th("Коло"),tags$th("Натпревар"),tags$th("Картон"),tags$th("Минута"),tags$th("Причина"))), tags$tbody(if(nrow(tarjetas_stage)>0){ map(1:nrow(tarjetas_stage),function(t_idx){ card_row <- tarjetas_stage[t_idx,]; t_partido<-filter(partidos_df, id_partido==card_row$id_partido); icon<-if(card_row$tipo=="Amarilla")tags$span(class="card-yellow")else tags$span(class="card-red");tags$tr(tags$td(t_partido$jornada),tags$td(tags$a(href=file.path("..", "partidos", paste0(card_row$id_partido, ".html")),paste(t_partido$local,"vs",t_partido$visitante))),tags$td(icon),tags$td(card_row$minuto),tags$td(card_row$motivo))}) } else { tags$tr(tags$td(colspan="5","Нема добиено картони.")) })))
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
                                                                                               tags$h4("Статистика на фудбалерки"), tags$table(tags$thead(tags$tr(map(headers_stats, tags$th))), tags$tbody(if(nrow(stats_final_stage) > 0) { map(1:nrow(stats_final_stage), function(p_idx) { p <- stats_final_stage[p_idx,]; tags$tr(tags$td(tags$a(href=file.path("..", "jugadoras", paste0(p$id, ".html")), p$Фудбалерка)), tags$td(p$Повикана), tags$td(p$Одиграни), tags$td(p$Минути), tags$td(p$Голови), tags$td(p$Жолти), tags$td(p$Црвени)) }) } else { tags$tr(tags$td(colspan=length(headers_stats), "Нема податоци за фудбалерки.")) })),
                                                                                               tags$h4("Список на натпревари"), tags$table(tags$thead(tags$tr(tags$th("Коло"), tags$th("Датум"), tags$th("Домаќин"), tags$th("Гостин"), tags$th("Резултат"))), tags$tbody(map(1:nrow(historial_stage), function(p_idx) { partido <- historial_stage[p_idx,]; tags$tr(tags$td(partido$jornada), tags$td(partido$fecha), tags$td(partido$local), tags$td(partido$visitante), tags$td(tags$a(href=file.path("..", "partidos", paste0(partido$id_partido, ".html")), paste(partido$goles_local, "-", partido$goles_visitante)))) })))
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
                                                                                                     tags$table(tags$thead(tags$tr(tags$th("Датум"), tags$th("Коло"), tags$th("Натпревар"), tags$th("Резултат"), tags$th("Улога"))),
                                                                                                                tags$tbody(map(1:nrow(historial_stage), function(p_idx) {
                                                                                                                  partido <- historial_stage[p_idx,]; tags$tr(tags$td(partido$fecha), tags$td(partido$jornada), tags$td(tags$a(href=file.path("..", "partidos", paste0(partido$id_partido, ".html")), paste(partido$local, "vs", partido$visitante))), tags$td(paste(partido$goles_local, "-", partido$goles_visitante)), tags$td(partido$uloga))
                                                                                                                })))
                     )))
                   )
                 })
               } else { tags$tr(tags$td(colspan="3", "Нема делегирани натпревари.")) }))
  )
  pagina_arbitro_final <- crear_pagina_html(contenido_arbitro, arb, "..", search_data_json, script_contraseña)
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
                   partido <- historial[p_idx, ]; tags$tr(tags$td(partido$fecha), tags$td(partido$competicion_temporada), tags$td(partido$competicion_nombre), tags$td(partido$jornada), tags$td(tags$a(href=file.path("..", "partidos", paste0(partido$id_partido, ".html")), paste(partido$local, "vs", partido$visitante))), tags$td(paste(partido$goles_local, "-", partido$goles_visitante)))
                 })
               } else { tags$tr(tags$td(colspan = "6", "Нема одиграни натпревари на овој стадион.")) }))
  )
  pagina_estadio_final <- crear_pagina_html(contenido_estadio, est, "..", search_data_json, script_contraseña)
  save_html(pagina_estadio_final, file = file.path(RUTA_ESTADIOS, paste0(id_e, ".html")))
})

# =========================================================================
# MENSAJE FINAL
# =========================================================================
message(paste("\nPROCESO COMPLETADO CON ÉXITO!"))
message(paste("Se ha creado un sitio web estático en la carpeta:", RUTA_BASE_SALIDA))
message("Abre el archivo 'index.html' de esa carpeta para empezar a navegar.")