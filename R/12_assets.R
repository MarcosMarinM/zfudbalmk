#### 12. EXTERNALIZATION OF ASSETS (CSS AND JAVASCRIPT) ####

### 12.1. Save Stylesheet (style.css)
# 12.1.1. The entire CSS is defined as a string and written to an external file.
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
.partido-link span.equipo, .partido-link-placeholder span.equipo { flex: 1 1 40%; display: flex; align-items: center; font-weight: bold; }
.partido-link span.equipo-local, .partido-link-placeholder span.equipo-local { justify-content: flex-end; }
.partido-link span.equipo-visitante, .partido-link-placeholder span.equipo-visitante { justify-content: flex-start; }
.partido-link span.resultado, .partido-link-placeholder span.resultado { flex: 0 0 12%; font-size: 1.2em; font-weight: bold; text-align: center; }
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
.clickable-row:hover { background-color: #f8f9fa; }
.legend { margin-top: 20px; padding: 10px; text-align: left; font-size: 0.9em; }
.legend-item { display: inline-flex; align-items: center; margin-right: 20px; margin-bottom: 5px; }
.legend-color-box { width: 15px; height: 15px; border: 1px solid #ccc; margin-right: 8px; flex-shrink: 0; }
.team-logo { height: 24px; width: 24px; object-fit: contain; }
.team-logo.national-team-flag { border-radius: 50%; border: 1px solid #ccc; }
.team-cell { display: flex; align-items: center; }
.team-cell .team-logo { margin-right: 12px; }
.partido-link .team-logo { height: 28px; width: 28px; }
.partido-link .equipo-local .team-logo { margin-right: 10px; }
.partido-link .equipo-visitante .team-logo { margin-left: 10px; }
.alineacion-header { text-align: center; margin-bottom: 20px; }
.alineacion-header .match-page-crest, .team-page-crest { width: 120px; height: 120px; object-fit: contain; margin: 10px auto; display: block; }
.alineacion-header h3 { border-bottom: none; padding-bottom: 0; margin-bottom: 5px; }
.alineacion-header h3 a { color: #8B0000; }
.header-main { display: flex; justify-content: space-between; align-items: center; border-bottom: 2px solid #dee2e6; padding-bottom: 10px; }
.header-main h1 { border-bottom: none; padding-bottom: 0; margin: 0; text-align: left; }
.main-content-title { text-align: left; font-size: 1.5em; color: #333; margin-bottom: 20px; }
.competition-results-block { margin-bottom: 50px; }
.results-header { display: flex; justify-content: space-between; align-items: flex-end; }
.competition-title-link { text-decoration: none; }
.competition-title { color: #CC0000; font-size: 2em; margin: 0; padding: 0; border: none; }
.results-link { color: #6c757d; font-weight: bold; text-decoration: none; font-size: 0.9em; }
.results-link:hover { color: #333; }
.separator { border: 0; border-top: 2px solid #CC0000; margin: 5px 0 15px 0; }
.results-scroll-container { overflow-x: auto; overflow-y: hidden; white-space: nowrap; padding-bottom: 15px; }
.results-row { display: flex; flex-wrap: nowrap; gap: 15px; }
.match-block { display: inline-flex; flex-direction: column; text-decoration: none; color: #333; background-color: #f8f9fa; border-radius: 5px; padding: 10px; transition: background-color 0.2s ease; border: 1px solid #e9ecef; flex-shrink: 0; }.match-block:not(.placeholder):hover { background-color: #e9ecef; }
.match-round { font-size: 0.8em; color: #6c757d; text-align: center; margin-bottom: 8px; }
.match-content { display: flex; flex-direction: column; align-items: center; }
.match-row { display: flex; justify-content: space-between; align-items: center; width: 100%; padding: 4px 0; }
.team-info { display: flex; align-items: center; }
.logo { width: 20px; height: 20px; object-fit: contain; margin-right: 8px; }
.name { font-weight: bold; font-size: 0.95em; white-space: normal; }
.score { font-weight: bold; font-size: 1.1em; color: #CC0000; }
.score-separator, .score-separator-placeholder { font-size: 1.1em; color: #CC0000; line-height: 0.5; }
.score-separator-placeholder { visibility: hidden; }
.results-scroll-container::-webkit-scrollbar { height: 8px; }
.results-scroll-container::-webkit-scrollbar-track { background: #f1f1f1; border-radius: 4px; }
.results-scroll-container::-webkit-scrollbar-thumb { background: #ccc; border-radius: 4px; }
.results-scroll-container::-webkit-scrollbar-thumb:hover { background: #aaa; }
.standings-grid-container { display: grid; grid-template-columns: repeat(4, 1fr); gap: 20px; margin-top: 20px; }
.mini-standings-container { background-color: #fff; border: 1px solid #e9ecef; border-radius: 5px; padding: 10px; }
.mini-standings-table { width: 100%; border-collapse: collapse; font-size: 0.85em; }
.mini-standings-table th { text-align: center; color: #6c757d; font-weight: bold; padding-bottom: 8px; border-bottom: 1px solid #dee2e6;}
.mini-standings-table td { text-align: center; padding: 6px 2px; border-bottom: 1px solid #f1f1f1; }
.mini-standings-table tr:last-child td { border-bottom: none; }
.mini-logo { width: 16px; height: 16px; object-fit: contain; vertical-align: middle; }
.pos-cell { font-weight: bold; text-align: left !important; padding-left: 5px !important; }
.logo-cell { width: 20px; padding-right: 4px !important; padding-left: 0 !important; }
.abbr-cell { text-align: left !important; font-weight: bold; }
.pts-cell { font-weight: bold; }
@media (max-width: 900px) { .standings-grid-container { grid-template-columns: repeat(2, 1fr); } }
@media (max-width: 500px) { .standings-grid-container { grid-template-columns: 1fr; } }
.standings-grid-title { margin-top: 40px !important; }
.mini-standings-title-link { text-decoration: none; color: inherit; }
.mini-standings-title { font-size: 1em; text-align: center; margin: 0 0 10px 0; padding-bottom: 8px; border-bottom: 1px solid #dee2e6; font-weight: bold; color: #333; }
.mini-standings-title-link:hover .mini-standings-title { color: #CC0000; }
.navbar { background-color: transparent; position: relative; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; }
.nav-links { list-style-type: none; margin: 0; padding: 0; display: flex; background-color: #333; border-radius: 5px; }
.nav-links li a, .dropbtn { display: block; color: white; text-align: center; padding: 10px 14px; text-decoration: none; transition: background-color 0.3s; border-radius: 5px; }
.nav-links li > a:hover, .dropdown:hover .dropbtn { background-color: #CC0000; }
.nav-links li a.active { background-color: #8B0000; font-weight: bold; }
.dropdown { position: relative; }
.dropdown-content { display: none; position: absolute; background-color: #8B0000; min-width: 200px; box-shadow: 0px 8px 16px 0px rgba(0,0,0,0.2); z-index: 1001; border-radius: 5px; }
.dropdown-content a { color: #CC0100; font-weight: bold; padding: 12px 16px; text-decoration: none; display: block; text-align: left; }
.dropdown-content a:hover { background-color: #CC0100; }
.dropdown:hover .dropdown-content { display: block; }
.dropdown-content hr { border: 0; border-top: 1px solid #ddd; margin: 5px 0; }
.hamburger-icon { display: none; }
.close-btn-li { display: none; }
@media screen and (max-width: 850px) {
  .header-main { border-bottom: none; }
  .navbar { background-color: transparent; }
  .nav-links { display: none; flex-direction: column; position: fixed; top: 0; left: 0; width: 100%; height: 100%; background-color: rgba(20, 20, 20, 0.98); justify-content: center; align-items: center; z-index: 2000; border-radius: 0; }
  .navbar.mobile-active .nav-links { display: flex; }
  .nav-links li { margin: 15px 0; }
  .nav-links li a { font-size: 1.5em; background-color: transparent; }
  .dropdown-content { position: static; display: none; background-color: transparent; box-shadow: none; padding-left: 20px; border: none; }
  .dropdown:hover .dropdown-content { display: none; }
  .dropdown.open .dropdown-content { display: block; }
  .dropdown .dropbtn { display: flex; align-items: center; justify-content: center; }
  .hamburger-icon { display: block; cursor: pointer; padding: 10px; z-index: 2001; }
  .hamburger-icon .bar { display: block; width: 25px; height: 3px; margin: 5px auto; background-color: #333; transition: all 0.3s ease-in-out; }
  .close-btn-li { display: block; position: absolute; top: 20px; right: 25px; }
  #close-nav-btn { font-size: 3em; font-weight: bold; color: white; }
}
.letter-nav { text-align: center; margin-bottom: 20px; padding: 10px; background: #f1f1f1; border-radius: 5px; }
.letter-nav a { display: inline-block; padding: 5px 10px; margin: 2px; background-color: #fff; border: 1px solid #ccc; border-radius: 3px; color: #333; text-decoration: none; font-weight: bold; }
.letter-nav a:hover { background-color: #CC0000; color: white; }
.letter-nav a.active { background-color: #8B0000; color: white; border-color: #8B0000; }
.letter-group { display: none; }
.letter-group.active { display: block; }
.player-list ul, .team-list ul { list-style: none; padding: 0; columns: 3; column-gap: 20px; }
.player-list li, .team-list li { padding: 5px 0; }
@media (max-width: 768px) { .player-list ul, .team-list ul { columns: 2; } }
@media (max-width: 480px) { .player-list ul, .team-list ul { columns: 1; } }
.player-profile-header-new { display: flex; align-items: center; background-color: #f8f9fa; border-radius: 8px; padding: 20px; gap: 30px; margin-bottom: 30px; border: 1px solid #e9ecef; flex-wrap: wrap; }
.player-name-container { flex: 1 1 40%; }
.player-name-new { font-size: 2.5em; color: #1a1a1a; margin: 0; padding: 0; border: none; font-weight: 700; line-height: 1.2; }
.player-bio { flex: 1 1 50%; display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 10px; }
.bio-item { display: flex; align-items: center; gap: 10px; }
.bio-icon { color: #8B0000; }
.bio-text { display: flex; flex-direction: column; }
.bio-label { font-size: 0.8em; color: #6c757d; }
.bio-value { font-size: 1em; font-weight: 600; color: #212529; }
.stats-summary-card { position: relative; background-color: #fff; border-radius: 8px; box-shadow: 0 4px 15px rgba(0,0,0,0.07); padding: 25px; margin-bottom: 40px; border: 1px solid #e9ecef; }
.season-tag { position: absolute; top: -12px; left: 50%; transform: translateX(-50%); background-color: #8B0000; color: white; font-weight: bold; padding: 5px 15px; border-radius: 20px; font-size: 0.9em; }
.stats-grid { display: grid; grid-template-columns: repeat(6, 1fr); gap: 10px; text-align: center; }
.stat-item { padding: 10px 0; }
.stat-icon { height: 32px; width: 32px; margin: 0 auto 10px auto; color: #8B0000; }
.stat-icon.card-icon { height: 28px; width: 20px; border-radius: 3px; border: 1px solid rgba(0,0,0,0.2); }
.stat-icon.card-icon.yellow { background-color: #ffc107; }
.stat-icon.card-icon.red { background-color: #dc3545; }
.stat-value { font-size: 2em; font-weight: 700; color: #1a1a1a; display: block; line-height: 1; }
.stat-label { font-size: 0.8em; color: #6c757d; margin-top: 5px; }
.season-accordion-container h3 { text-align: center; border: none; color: #333; }
.season-accordion { border: 1px solid #dee2e6; border-radius: 5px; margin-bottom: 10px; overflow: hidden; }
.season-header { cursor: pointer; padding: 15px; background-color: #f8f9fa; display: flex; justify-content: space-between; align-items: center; font-weight: 600; }
.season-header:hover { background-color: #e9ecef; }
.season-arrow { transition: transform 0.3s ease; }
.season-accordion.active .season-arrow { transform: rotate(180deg); }
.season-arrow::before { content: '▼'; font-size: 0.8em; color: #6c757d; }
.season-content { display: none; padding: 15px; border-top: 1px solid #dee2e6; }
.tab-nav { margin-bottom: 15px; border-bottom: 1px solid #dee2e6; }
.tab-button { background: none; border: none; padding: 10px 15px; cursor: pointer; font-size: 1em; color: #6c757d; font-weight: 600; border-bottom: 3px solid transparent; }
.tab-button.active { color: #8B0000; border-bottom-color: #8B0000; }
.tab-panel { display: none; }
.tab-panel.active { display: block; }
.stats-table-season th, .matches-table-season th { font-size: 0.9em; padding: 8px; }
.stats-table-season td, .matches-table-season td { font-size: 0.9em; padding: 8px; }
.team-cell-with-logo { display: flex; align-items: center; gap: 10px; }
.team-cell-with-logo img { height: 20px; width: 20px; flex-shrink: 0; }
.team-cell-with-logo a { font-weight: normal; }
.team-logo-small.national-team-flag { border-radius: 50%; border: 1px solid #ccc; }
@media (max-width: 768px) {
  .player-profile-header-new { flex-direction: column; align-items: flex-start; }
  .player-name-new { font-size: 2em; text-align: left; }
  .player-bio { width: 100%; }
  .stats-grid { grid-template-columns: repeat(3, 1fr); }
  .stat-item { margin-bottom: 15px; }
}
@media (max-width: 480px) {
  .stats-grid { grid-template-columns: repeat(2, 1fr); }
}
.sub-tab-nav { display: flex; flex-wrap: wrap; gap: 5px; margin-bottom: 15px; padding-bottom: 10px; border-bottom: 1px solid #e9ecef; }
.sub-tab-button { background-color: #f1f1f1; border: 1px solid #ddd; border-radius: 20px; padding: 5px 12px; cursor: pointer; font-size: 0.85em; font-weight: 500; color: #333; transition: all 0.2s ease; }
.sub-tab-button:hover { background-color: #e0e0e0; }
.sub-tab-button.active { background-color: #8B0000; color: white; border-color: #8B0000; }
.sub-tab-panel { display: none; }
.sub-tab-panel.active { display: block; }
.card-icon-header { display: inline-block; width: 10px; height: 14px; border-radius: 2px; vertical-align: middle; }
.card-icon-header.yellow { background-color: #ffc107; border: 1px solid #c69500; }
.card-icon-header.red { background-color: #dc3545; border: 1px solid #a71d2a; }
.match-list-container { font-size: 0.9em; }
.match-list-header { display: grid; grid-template-columns: 1.5fr 5fr 1.2fr 0.8fr 0.8fr 1fr; gap: 10px; font-weight: bold; color: #6c757d; padding: 10px; border-bottom: 2px solid #dee2e6; margin-bottom: 5px; }
.match-list-header > div { text-align: center; } /* Regla general: Centra TODOS los títulos */
.match-list-header > div:first-child { text-align: left; } /* Excepción: Alinea a la izquierda SÓLO el PRIMER título (Fecha) */
.match-list-row { display: grid; grid-template-columns: 1.5fr 5fr 1.2fr 0.8fr 0.8fr 1fr; gap: 10px; align-items: center; padding: 12px 10px; border-bottom: 1px solid #e9ecef; transition: background-color 0.2s; }
.match-list-row.clickable-row { cursor: pointer; }
.match-list-row.clickable-row:hover { background-color: #f8f9fa; }
.cell-match { display: flex; align-items: center; justify-content: space-between; }
.team-home, .team-away { display: flex; align-items: center; gap: 8px; flex: 1; font-weight: 500; }
.team-home { justify-content: flex-end; text-align: right; }
.team-away { justify-content: flex-start; text-align: left; }
.team-home .team-logo-small, .team-away .team-logo-small { height: 20px; width: 20px; }
.match-score { background-color: #e9ecef; font-weight: bold; padding: 5px 10px; border-radius: 4px; margin: 0 10px; white-space: nowrap; }
.cell-goals, .cell-minutes, .cell-link { text-align: center; }
.cell-cards { text-align: center; display: flex; gap: 4px; justify-content: center; align-items: center; }
.card-icon-table { display: inline-block; width: 8px; height: 12px; border-radius: 1px; }
.card-icon-table.yellow { background-color: #ffc107; }
.card-icon-table.red { background-color: #dc3545; }
/* --- INICIO: ESTILOS PARA PÁGINA DE EQUIPO (V8) --- */

/* --- Cabecera con Logo --- */
.team-header-container-v5 { display: flex; align-items: center; gap: 25px; padding-bottom: 20px; border-bottom: 1px solid #e0e0e0; margin-bottom: 20px; }
.team-logo-main img { width: 90px; height: 90px; object-fit: contain; }
.team-header-info { flex-grow: 1; }
.team-name-v3 { font-size: 2.8em; color: #1a1a1a; margin: 0 0 15px 0; font-weight: 700; line-height: 1.1; text-align: left; padding: 0; border: none; }
.info-item { display: flex; align-items: center; gap: 10px; }
.info-icon svg { fill: #6c757d; width:20px; height:20px; }
.info-text { display: flex; flex-direction: column; }
.info-label { font-size: 0.7em; color: #6c757d; text-transform: uppercase; font-weight: 500; }
.info-value { font-size: 0.9em; font-weight: 600; color: #212529; }

/* --- Pestañas Principales (Roster/Schedule) --- */
.tab-nav-v4 { display: flex; gap: 10px; border-bottom: 1px solid #dee2e6; }
.tab-button-v4 { background: none; border: 1px solid transparent; border-bottom: none; padding: 8px 12px; cursor: pointer; font-size: 0.9em; color: #6c757d; font-weight: 600; margin-bottom: -1px; border-radius: 5px 5px 0 0; }
.tab-button-v4.active { color: #CC0000; border-color: #dee2e6; background-color: #fff; }

/* --- Contenedor de Filtros y Píldoras --- */
.filters-and-pills-container { background-color: #f7f7f7; margin-top: 25px; }
.filter-bar-v2 { margin-top: 0 !important; }

/* --- Píldoras de Competición --- */
.competition-pills-container { display: flex; flex-wrap: wrap; gap: 8px; padding: 0 15px 15px 15px; }
.competition-pill { background-color: #e9ecef; color: #495057; border: 1px solid #dee2e6; border-radius: 15px; padding: 6px 14px; font-size: 0.85em; font-weight: 500; cursor: pointer; transition: all 0.2s ease-in-out; }
.competition-pill:hover { background-color: #ced4da; }
.competition-pill.active { background-color: #8B0000; color: #fff; border-color: #8B0000; }

/* --- Tabla de Plantilla (Roster) --- */
.player-roster-table { width: 100%; border-collapse: collapse; font-size: 0.9em; margin-top: 20px; }
.player-roster-table th { padding: 10px; border-bottom: 2px solid #dee2e6; text-align: left; color: #6c757d; font-size: 0.8em; text-transform: uppercase; }
.player-roster-table th.sortable { cursor: pointer; }
.player-roster-table th.sortable:hover { color: #212529; }
.player-roster-table th.sort-asc::after, .player-roster-table th.sort-desc::after { content: ''; display: inline-block; width: 0; height: 0; border-left: 4px solid transparent; border-right: 4px solid transparent; margin-left: 5px; }
.player-roster-table th.sort-asc::after { border-bottom: 4px solid #212529; }
.player-roster-table th.sort-desc::after { border-top: 4px solid #212529; }
.player-roster-table td { padding: 12px 10px; border-bottom: 1px solid #e9ecef; }
.player-roster-table tbody tr { cursor: pointer; transition: background-color 0.2s ease-in-out; }
.player-roster-table tbody tr:hover { background-color: #f8f9fa; }
.player-roster-table tr:last-child td { border-bottom: none; }
.player-info-cell .player-name { color: #CC0000; font-weight: bold; }
.player-info-cell .player-position { font-size: 0.9em; color: #6c757d; }

/* --- Tabla de Calendario (Schedule) --- */
.schedule-match-table { width: 100%; border-collapse: collapse; font-size: 0.9em; margin-top: 20px; }
.schedule-match-table th { padding: 10px; border-bottom: 2px solid #dee2e6; color: #6c757d; font-size: 0.8em; text-transform: uppercase; text-align: left;}
.schedule-match-table td { padding: 12px 10px; border-bottom: 1px solid #e9ecef; vertical-align: middle; }
.schedule-match-table tbody tr { cursor: pointer; transition: background-color 0.2s ease-in-out; }
.schedule-match-table tbody tr:hover { background-color: #f8f9fa; }
.schedule-match-table tr:last-child td { border-bottom: none; }
.schedule-match-table .th-match { text-align: center; }
.schedule-match-table .td-home { text-align: right; }
.schedule-match-table .td-away { text-align: left; }
.schedule-match-table .td-score { text-align: center; width: 80px; }
.schedule-team-in-table { display: flex; align-items: center; gap: 8px; font-weight: 500; }
.td-home .schedule-team-in-table { justify-content: flex-end; }
.td-away .schedule-team-in-table { justify-content: flex-start; }
.schedule-score-in-table { display: inline-block; font-size: 1em; font-weight: 700; background-color: #f1f2f3; padding: 5px 10px; border-radius: 4px; white-space: nowrap; }



/* --- 1. Contenedor Principal (Grid) --- */
.comp-hub-container {
  display: grid;
  grid-template-columns: 2fr 1fr; /* Columna izquierda más ancha */
  gap: 30px;
  margin-top: 25px;
  margin-bottom: 40px;
}

.comp-hub-left-col, .comp-hub-right-col, .comp-hub-bottom-row {
  background-color: #ffffff;
  border: 1px solid #e9ecef;
  border-radius: 8px;
  padding: 20px;
}

/* --- 2. Encabezados de Sección --- */
.comp-hub-section-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  border-bottom: 2px solid #dee2e6;
  padding-bottom: 10px;
  margin-bottom: 15px;
}

.comp-hub-section-title {
  margin: 0;
  padding: 0;
  border: none;
  color: #333;
  font-size: 1.5em;
}

.comp-hub-see-all-link {
  font-size: 0.9em;
  font-weight: bold;
  color: #6c757d;
  text-decoration: none;
}
.comp-hub-see-all-link:hover {
  color: #CC0000;
}


/* --- 3. Columna Izquierda (Jornadas/Schedule) --- */
.comp-hub-schedule-nav {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 15px;
  background-color: #f8f9fa;
  border-radius: 5px;
  padding: 5px;
}

#comp-hub-round-title {
  margin: 0;
  padding: 0;
  font-size: 1.1em;
  color: #CC0000;
  text-align: center;
  flex-grow: 1;
}

.comp-hub-nav-arrow {
  background-color: #e9ecef;
  border: 1px solid #dee2e6;
  color: #333;
  font-size: 1.5em;
  font-weight: bold;
  border-radius: 5px;
  cursor: pointer;
  width: 35px;
  height: 35px;
  line-height: 35px;
  text-align: center;
  padding: 0;
}
.comp-hub-nav-arrow:hover {
  background-color: #ced4da;
}
.comp-hub-nav-arrow:disabled {
  background-color: #f8f9fa;
  color: #adb5bd;
  cursor: not-allowed;
}

.comp-hub-match-row {
  display: block;
  text-decoration: none;
  color: #212529;
  padding: 15px;
  border-bottom: 1px solid #f1f1f1;
  transition: background-color 0.2s;
}
.comp-hub-match-row.clickable:hover {
  background-color: #f8f9fa;
}
.comp-hub-match-row:last-child {
  border-bottom: none;
}

.comp-hub-match-teams {
  display: flex;
  align-items: center;
  justify-content: space-between;
  font-size: 1.1em;
  font-weight: 500;
}

.comp-hub-team-home, .comp-hub-team-away {
  display: flex;
  align-items: center;
  gap: 10px;
  flex: 1;
}
.comp-hub-team-home { justify-content: flex-end; text-align: right; }
.comp-hub-team-away { justify-content: flex-start; text-align: left; }

.comp-hub-team-logo {
  width: 24px;
  height: 24px;
  object-fit: contain;
}

.comp-hub-match-score {
  font-size: 1.1em;
  font-weight: 700;
  color: #CC0000;
  background-color: #f1f2f3;
  padding: 5px 12px;
  border-radius: 4px;
  margin: 0 15px;
  min-width: 60px;
  text-align: center;
}

.comp-hub-match-time {
  font-size: 0.85em;
  color: #6c757d;
  text-align: center;
  margin-bottom: 8px;
  display: flex;
  justify-content: space-between;
  flex-wrap: wrap;
}
.comp-hub-match-date { padding-right: 10px; }
.comp-hub-match-stadium { flex-grow: 1; text-align: right; }

.comp-hub-no-matches, .comp-hub-no-stats {
  padding: 20px;
  text-align: center;
  color: #6c757d;
  font-style: italic;
}

/* --- 4. Columna Derecha (Estadísticas) --- */
.comp-hub-right-col {
  padding: 0; /* El padding lo tendrán los contenedores internos */
  overflow: hidden; /* Para que los bordes redondeados afecten a los hijos */
}

.comp-hub-stats-tabs {
  display: flex;
  background-color: #f8f9fa;
  border-bottom: 1px solid #dee2e6;
}

.comp-hub-tab-btn {
  flex: 1;
  background: none;
  border: none;
  border-bottom: 3px solid transparent;
  padding: 12px 5px;
  font-size: 0.9em;
  font-weight: 600;
  color: #6c757d;
  cursor: pointer;
  transition: all 0.2s;
  text-align: center;
}
.comp-hub-tab-btn:hover {
  color: #333;
}
.comp-hub-tab-btn.active {
  color: #CC0000;
  border-bottom-color: #CC0000;
}

.comp-hub-tab-panel {
  display: none;
  padding: 15px;
}
.comp-hub-tab-panel.active {
  display: block;
}

.comp-hub-stats-list {
  display: flex;
  flex-direction: column;
  gap: 12px;
}

.comp-hub-stat-row {
  display: grid;
  grid-template-columns: 20px 1fr auto;
  gap: 10px;
  align-items: center;
  font-size: 0.95em;
}

.comp-hub-stat-row .stat-pos {
  font-weight: bold;
  color: #6c757d;
  text-align: right;
}

.comp-hub-stat-row .stat-player-team {
  display: flex;
  flex-direction: column;
}
.comp-hub-stat-row .stat-player {
  font-weight: bold;
  color: #CC0000;
  text-decoration: none;
}
.comp-hub-stat-row .stat-player:hover { text-decoration: underline; }

.comp-hub-stat-row .stat-team {
  font-size: 0.9em;
  color: #6c757d;
  text-decoration: none;
}
.comp-hub-stat-row .stat-team:hover { text-decoration: underline; }

.comp-hub-stat-row .stat-value {
  font-weight: 700;
  font-size: 1.1em;
  text-align: right;
}
.comp-hub-stat-row .stat-value.cards {
  font-size: 0.9em;
  font-weight: 500;
  white-space: nowrap;
}

.stats-see-all {
  display: block;
  text-align: right;
  margin-top: 15px;
  font-size: 0.9em;
}


/* --- 5. Fila Inferior (Clasificación) --- */
.comp-hub-bottom-row {
  grid-column: 1 / -1; /* Ocupa todo el ancho */
  padding: 20px;
}
/* La tabla de clasificación ya tiene estilos globales, así que no necesita más */


/* --- 6. Media Queries (Responsividad) --- */
@media (max-width: 850px) {
  .comp-hub-container {
    grid-template-columns: 1fr; /* Apilar columnas en móvil */
  }
  
  .comp-hub-match-teams {
    font-size: 1em;
  }
  .comp-hub-match-score {
    margin: 0 10px;
    font-size: 1em;
  }
  
  .comp-hub-tab-btn {
    font-size: 0.8em;
  }
}
/* --- 7. Estilos para Títulos Clicables (Añadido) --- */
.comp-hub-title-link {
  text-decoration: none;
  color: inherit; /* Hereda el color normal del título */
}
.comp-hub-title-link:hover .comp-hub-section-title {
  color: #CC0000; /* Hace que el título se vuelva rojo al pasar el ratón */
  text-decoration: underline;
}

/* Ocultar el enlace "see all" en la sección de clasificación (ya que el título es el enlace) */
.comp-hub-bottom-row .comp-hub-see-all-link {
  display: none;
}

)"


writeLines(estilo_css, file.path(RUTA_ASSETS_COMPARTIDOS, "style.css"))


### 12.2. Save Functionality Script (script.js)
script_js <- r"(
let searchData = [];

document.addEventListener('DOMContentLoaded', function() {
  // INICIALIZADORES GLOBALES (PARA TODO EL SITIO)
  initializeSearch();
  initializeMobileMenu();
  initializePlayerProfileInteractions();
 
  // INICIALIZADOR ESPECÍFICO PARA LA PÁGINA DE EQUIPO
  if (document.getElementById('team-page-data')) {
    initializeTeamProfilePage();
  }
  
  // ¡NUEVO! INICIALIZADOR PARA EL HUB DE COMPETICIÓN
  if (document.getElementById('competition-hub-data')) {
    initializeCompetitionHub();
  }
});

function initializeMobileMenu() {
  const hamburger = document.getElementById('hamburger-icon');
  const nav = document.querySelector('.navbar');
  const closeBtn = document.getElementById('close-nav-btn');
  const dropdownBtn = document.querySelector('.dropdown .dropbtn');

  if (hamburger && nav && closeBtn) {
    hamburger.addEventListener('click', () => { nav.classList.add('mobile-active'); });
    closeBtn.addEventListener('click', () => { nav.classList.remove('mobile-active'); });
  }

  if (dropdownBtn) {
    dropdownBtn.addEventListener('click', (event) => {
      if (window.innerWidth <= 850) { // <-- Corregido para que coincida con el CSS
        event.preventDefault();
        const parentDropdown = dropdownBtn.parentElement;
        parentDropdown.classList.toggle('open');
      }
    });
  }
}

// --- FUNCIONES PARA PERFIL DE JUGADORA ---
function initializePlayerProfileInteractions() {
    const accordions = document.querySelectorAll('.season-accordion');
    if (!accordions.length) return;

    accordions.forEach(accordion => {
        const header = accordion.querySelector('.season-header');
        header.addEventListener('click', () => {
            accordion.classList.toggle('active');
            const content = accordion.querySelector('.season-content');
            content.style.display = accordion.classList.contains('active') ? 'block' : 'none';
        });
    });
    
    document.querySelectorAll('.tab-button').forEach(button => {
        button.addEventListener('click', (event) => {
            const clickedButton = event.currentTarget;
            const seasonId = clickedButton.dataset.seasonId;
            const tabTarget = clickedButton.dataset.tabTarget;
            switchTab(seasonId, tabTarget, clickedButton);
        });
    });

    document.querySelectorAll('.sub-tab-button').forEach(button => {
        button.addEventListener('click', (event) => {
            const clickedButton = event.currentTarget;
            const seasonId = clickedButton.dataset.seasonId;
            const subtabTarget = clickedButton.dataset.subtabTarget;
            switchSubTab(seasonId, subtabTarget, clickedButton);
        });
    });
}

function switchTab(seasonId, tabToShow, clickedButton) {
    const seasonContent = clickedButton.closest('.season-content');
    if (!seasonContent) return;
    seasonContent.querySelectorAll('.tab-panel').forEach(panel => panel.classList.remove('active'));
    seasonContent.querySelectorAll('.tab-button').forEach(button => button.classList.remove('active'));
    const panelToShow = seasonContent.querySelector(`#${tabToShow}-${seasonId}`);
    if (panelToShow) panelToShow.classList.add('active');
    clickedButton.classList.add('active');
}

function switchSubTab(seasonId, competitionId, clickedButton) {
    const parentTabPanel = clickedButton.closest('.tab-panel');
    if (!parentTabPanel) return;
    parentTabPanel.querySelectorAll('.sub-tab-panel').forEach(panel => panel.classList.remove('active'));
    parentTabPanel.querySelectorAll('.sub-tab-button').forEach(button => button.classList.remove('active'));
    const panelToShow = parentTabPanel.querySelector(`#matches-${seasonId}-${competitionId}`);
    if (panelToShow) panelToShow.classList.add('active');
    clickedButton.classList.add('active');
}

// --- FUNCIONES DE BÚSQUEDA Y UTILIDADES ---
function getSiteBasePath() {
  const path = window.location.pathname;
  const match = path.match(/^(.*\/)(mk|sq|es|en)\//);
  if (match && match[1]) { return match[1]; }
  return "/";
}

function getCurrentLanguageFromPath() {
  const path = window.location.pathname;
  const match = path.match(/\/(mk|sq|es|en)\//);
  if (match && match[1]) { return match[1]; }
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

  fetch(jsonUrl).then(response => response.json()).then(data => {
      searchData = data;
      if(searchInput) searchInput.disabled = false;
  }).catch(error => console.error('Error loading search data:', error));
  
  document.addEventListener('click', function(event) {
      const searchContainer = document.querySelector('.search-container');
      if (searchContainer && !searchContainer.contains(event.target)) {
        const suggestions = document.getElementById('search-suggestions');
        if(suggestions) suggestions.style.display = 'none';
      }
        const clickableRow = event.target.closest('.clickable-row, .player-roster-table tbody tr');
      if (clickableRow && clickableRow.dataset.href) { 
          if (event.target.closest('a')) return;
          window.location.href = clickableRow.dataset.href; 
      } else if (clickableRow && clickableRow.onclick) {
          if (event.target.closest('a')) return;
          clickableRow.onclick();
      }
  });
}

/**
 * Encuentra la ruta raíz del idioma actual de forma robusta.
 * @returns {string} Una ruta absoluta desde la raíz del dominio, ej: "/docs/mk/"
 */
function getLanguageRootPath() {
    const path = window.location.pathname;
    // Busca el primer segmento de idioma en la URL (ej. /mk/, /sq/)
    const match = path.match(/\/(mk|sq|es|en)\//);
    
    if (match && match.index !== -1) {
        // Devuelve la parte de la URL hasta el final del directorio del idioma
        // ej. de "/docs/mk/igraci/id.html" extrae "/docs/mk/"
        return path.substring(0, match.index + match[0].length);
    }
    
    // Fallback si la URL no tiene el formato esperado (muy improbable)
    // Asume que el idioma está en la raíz.
    const lang = document.documentElement.lang || 'mk';
    return `/${lang}/`;
}

/**
 * Genera un enlace de URL correcto y absoluto para un elemento del índice de búsqueda.
 * @param {string} target_id - El ID del objetivo, ej: "jugadora-12345"
 * @returns {string} La URL completa y funcional.
 */
function generateLink(target_id) {
    // ¡LA CORRECCIÓN CLAVE! Usamos la ruta absoluta en lugar de "."
    const basePath = getLanguageRootPath(); 
    const parts = target_id.split('-');
    const type = parts[0];
    const id = parts.slice(1).join('-');

    const paths = {
        'jugadora': 'igraci',
        'equipo': 'timovi',
        'arbitro': 'sudii',
        'стадион': 'stadioni',
        'menu-competicion': 'natprevaruvanja'
    };

    if (paths[type]) {
        // basePath ya termina con "/", así que no añadimos una extra.
        return `${basePath}${paths[type]}/${id}.html`;
    }
    return '#'; // Fallback
}

function handleSearchInput(event) {
    const input = event.target.value.toLowerCase().trim();
    const suggestionsContainer = document.getElementById('search-suggestions');
    
    if (event.key === 'Enter') {
        event.preventDefault();
        showSearchResults();
        return;
    }

    if (input.length < 2) {
        suggestionsContainer.style.display = 'none';
        return;
    }

    const matches = searchData.filter(item => 
        item.search_terms.toLowerCase().includes(input)
    ).slice(0, 10); // Limitar a 10 sugerencias

    if (matches.length > 0) {
        suggestionsContainer.innerHTML = '';
        matches.forEach(item => {
            const link = document.createElement('a');
            link.href = generateLink(item.target_id);
            
            // Resaltar la coincidencia
            const regex = new RegExp(`(${input.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&')})`, 'gi');
            const highlightedName = item.Име.replace(regex, '<strong>$1</strong>');
            
            link.innerHTML = `${highlightedName} <span class="search-result-type">(${item.Тип})</span>`;
            suggestionsContainer.appendChild(link);
        });
        suggestionsContainer.style.display = 'block';
    } else {
        suggestionsContainer.style.display = 'none';
    }
}

function showSearchResults() {
    const input = document.getElementById('search-input').value.toLowerCase().trim();
    const suggestionsContainer = document.getElementById('search-suggestions');
    const mainContent = document.getElementById('main-content');
    const body = document.body;

    suggestionsContainer.style.display = 'none';

    if (input.length < 2) {
        mainContent.innerHTML = `<h2>${body.dataset.searchPromptMsg || 'Please enter at least 2 characters'}</h2>`;
        return;
    }

    const matches = searchData.filter(item => 
        item.search_terms.toLowerCase().includes(input)
    );

    let resultsHtml = `<h2>${body.dataset.searchResultsTitle || 'Search results for'}: "${input}"</h2>`;

    if (matches.length > 0) {
        resultsHtml += '<div id="search-results-list"><ul>';
        matches.forEach(item => {
            resultsHtml += `
                <li>
                    <a href="${generateLink(item.target_id)}">
                        ${item.Име}
                    </a>
                    <span class="search-result-type">(${item.Тип})</span>
                </li>`;
        });
        resultsHtml += '</ul></div>';
    } else {
        resultsHtml += `<p>${body.dataset.noSearchResultsMsg || 'No results found.'}</p>`;
    }

    mainContent.innerHTML = resultsHtml;
}

function sortTable(tableId, columnIndex) { /* ... (código de búsqueda sin cambios) ... */ }

function showLetter(letter) {
  // 1. Ocultar todos los grupos de letras
  const letterGroups = document.querySelectorAll('.letter-group');
  letterGroups.forEach(group => {
    group.classList.remove('active');
  });

  // 2. Mostrar solo el grupo de la letra seleccionada
  const activeGroup = document.getElementById('group-' + letter);
  if (activeGroup) {
    activeGroup.classList.add('active');
  }

  // 3. Actualizar el estado visual de los botones de las letras
  const letterNavLinks = document.querySelectorAll('.letter-nav a');
  letterNavLinks.forEach(link => {
    link.classList.remove('active');
    if (link.getAttribute('data-letter') === letter) {
      link.classList.add('active');
    }
  });
}

function toggleDetails(elementId) { /* ... (código de búsqueda sin cambios) ... */ }


// ==============================================================================
// == INICIO: FUNCIONES PARA LA PÁGINA DE PERFIL DE EQUIPO (COPIA LITERAL)      ==
// ==============================================================================
function initializeTeamProfilePage() {
  const dataEl = document.getElementById('team-page-data');
  if (!dataEl) return;
  const pageData = JSON.parse(dataEl.textContent);
  const t = pageData.translations;

  let rosterSortColumn = 'dorsal_principal';
  let rosterSortDirection = 'asc';

  const rosterSeason = document.getElementById('roster-season-filter');
  const rosterCategory = document.getElementById('roster-category-filter');
  const rosterPillsContainer = document.getElementById('roster-competition-pills');
  const rosterTableContainer = document.getElementById('roster-table-container');
  
  const scheduleSeason = document.getElementById('schedule-season-filter');
  const scheduleCategory = document.getElementById('schedule-category-filter');
  const schedulePillsContainer = document.getElementById('schedule-competition-pills');
  const scheduleTableContainer = document.getElementById('schedule-table-container');

  document.querySelectorAll('.tab-button-v4').forEach(button => {
    button.addEventListener('click', (event) => {
      const targetPanelId = event.currentTarget.dataset.tabTarget;
      document.querySelectorAll('.tab-panel').forEach(p => p.classList.remove('active'));
      document.getElementById(targetPanelId).classList.add('active');
      document.querySelectorAll('.tab-button-v4').forEach(b => b.classList.remove('active'));
      event.currentTarget.classList.add('active');
    });
  });

  function updateRosterCategoryFilter() {
    const season = rosterSeason.value;
    const categories = [...new Set(pageData.roster_data.filter(p => p.competicion_temporada === season).map(p => JSON.stringify({ key: p.category_key })))].map(s => JSON.parse(s));
    const order = { "category_senior": 1, "category_youth": 2, "category_cadet": 3 };
    categories.sort((a, b) => (order[a.key] || 99) - (order[b.key] || 99));
    rosterCategory.innerHTML = '';
    categories.forEach(c => { rosterCategory.innerHTML += `<option value="${c.key}">${t[c.key] || c.key}</option>`; });
    updateRosterCompetitionFilter();
  }

  function updateRosterCompetitionFilter() {
    rosterPillsContainer.innerHTML = '';
    if(!rosterSeason.value || !rosterCategory.value) { updateRosterView(); return; }
    const season = rosterSeason.value; 
    const category = rosterCategory.value;
    const competitions = [...new Set(pageData.roster_data.filter(p => p.competicion_temporada === season && p.category_key === category).map(p => JSON.stringify({id: p.competicion_id, name: p.CompeticionLang || p.competicion_nombre})))].map(s => JSON.parse(s));
    competitions.forEach(c => { rosterPillsContainer.innerHTML += `<button class="competition-pill" data-competition-id="${c.id}">${c.name}</button>`; });
    const firstPill = rosterPillsContainer.querySelector('.competition-pill');
    if (firstPill) { firstPill.classList.add('active'); }
    updateRosterView();
  }

  function updateRosterView() {
    const activePill = rosterPillsContainer.querySelector('.competition-pill.active');
    if (!activePill) { rosterTableContainer.innerHTML = `<p style="text-align:center; padding: 20px;">${t.no_players_found}</p>`; return; }
    const season = rosterSeason.value;
    const category = rosterCategory.value;
    const competition = activePill.dataset.competitionId;
    let filteredData = pageData.roster_data.filter(p => p.competicion_temporada === season && p.category_key === category && p.competicion_id === competition);
    if (filteredData.length === 0) { rosterTableContainer.innerHTML = `<p style="text-align:center; padding: 20px;">${t.no_players_found}</p>`; return; }
    const direction = rosterSortDirection === 'asc' ? 1 : -1;
    filteredData.sort((a, b) => {
      let valA, valB;
      if (rosterSortColumn === 'dorsal_principal') { valA = parseInt(a.dorsal_principal) || 999; valB = parseInt(b.dorsal_principal) || 999;
      } else if (rosterSortColumn === 'PlayerName') { return a.PlayerName.localeCompare(b.PlayerName) * direction;
      } else { valA = parseInt(a[rosterSortColumn]) || 0; valB = parseInt(b[rosterSortColumn]) || 0; }
      return (valA - valB) * direction;
    });
    let html = `<table class="player-roster-table"><thead><tr> <th class="sortable" data-sort-key="dorsal_principal" style="width:5%;">${t.col_dorsal}</th> <th class="sortable" data-sort-key="PlayerName">${t.col_player}</th> <th class="sortable" data-sort-key="Played" style="width:8%;">${t.col_apps}</th> <th class="sortable" data-sort-key="Minutes" style="width:8%;">${t.col_mins}</th> <th class="sortable" data-sort-key="Goals" style="width:8%;">${t.col_goals}</th> <th class="sortable" data-sort-key="Yellows" style="width:12%;">${t.col_cards}</th> </tr></thead><tbody>`;
    filteredData.forEach(player => {
      const position = t[player.posicion_final_unificada] || '';
      const playerURL = `../igraci/${player.id}.html`;
      html += `<tr onclick="window.location.href='${playerURL}'"> <td>${player.dorsal_principal || '-'}</td> <td><div class="player-info-cell"><div class="player-name">${player.PlayerName}</div><div class="player-position">${position}</div></div></td> <td style="text-align:center;">${player.Played || 0}</td> <td style="text-align:center;">${player.Minutes || 0}</td> <td style="text-align:center;">${player.Goals || 0}</td> <td style="text-align:center;"><span class="card-icon-table yellow"></span> ${player.Yellows || 0}&nbsp;/&nbsp;<span class="card-icon-table red"></span> ${player.Reds || 0}</td> </tr>`;
    });
    html += '</tbody></table>';
    rosterTableContainer.innerHTML = html;
    const currentHeader = rosterTableContainer.querySelector(`[data-sort-key="${rosterSortColumn}"]`);
    if (currentHeader) { currentHeader.classList.add(rosterSortDirection === 'asc' ? 'sort-asc' : 'sort-desc'); }
  }

  function updateScheduleCategoryFilter() {
    const season = scheduleSeason.value;
    const categories = [...new Set(pageData.matches_data.filter(m => m.competicion_temporada === season).map(m => JSON.stringify({key: m.category_key, name: m.category_name})))].map(s => JSON.parse(s));
    const order = { "category_senior": 1, "category_youth": 2, "category_cadet": 3 };
    categories.sort((a, b) => (order[a.key] || 99) - (order[b.key] || 99));
    scheduleCategory.innerHTML = '';
    categories.forEach(c => { scheduleCategory.innerHTML += `<option value="${c.key}">${c.name}</option>`; });
    updateScheduleCompetitionFilter();
  }
  
  function updateScheduleCompetitionFilter() {
      schedulePillsContainer.innerHTML = '';
      if(!scheduleSeason.value || !scheduleCategory.value) { updateScheduleView(); return; }
      const season = scheduleSeason.value; 
      const category = scheduleCategory.value;
      const competitions = [...new Set(pageData.matches_data.filter(m => m.competicion_temporada === season && m.category_key === category).map(m => JSON.stringify({id: m.competicion_id, name: m.CompeticionLang})))].map(s => JSON.parse(s));
      competitions.forEach(c => { schedulePillsContainer.innerHTML += `<button class="competition-pill" data-competition-id="${c.id}">${c.name}</button>`; });
      const firstPill = schedulePillsContainer.querySelector('.competition-pill');
      if (firstPill) { firstPill.classList.add('active'); }
      updateScheduleView();
  }

  function updateScheduleView() {
    const activePill = schedulePillsContainer.querySelector('.competition-pill.active');
    if(!activePill) { scheduleTableContainer.innerHTML = `<p style="text-align:center; padding: 20px;">${t.no_matches_found}</p>`; return; }
    const season = scheduleSeason.value; 
    const category = scheduleCategory.value; 
    const competition = activePill.dataset.competitionId;
    let filteredMatches = pageData.matches_data.filter(m => m.competicion_temporada === season && m.category_key === category && m.competicion_id === competition);
    if (filteredMatches.length === 0) { scheduleTableContainer.innerHTML = `<p style="text-align:center; padding: 20px;">${t.no_matches_found}</p>`; return; }
    filteredMatches.sort((a, b) => new Date(b.fecha.split('.').reverse().join('-')) - new Date(a.fecha.split('.').reverse().join('-')));
    let html = `<div class="match-list-container">
    <div class="match-list-header">
        <div>${t.col_date}</div>
        <div>${t.col_match}</div>
        <div>${t.col_competition || 'Competition'}</div>
    </div>`;
filteredMatches.forEach(match => {
    const matchURL = `../natprevari/${match.id_partido}.html`;
    html += `<div class="match-list-row" onclick="window.location.href='${matchURL}'">
    <div class="cell-date">${match.fecha}</div>
        <div class="cell-match">
            <span class="team-home">${match.local_lang} <img src="${match.home_logo_url}" class="team-logo-small"></span>
            <span class="match-score">${match.goles_local} : ${match.goles_visitante}</span>
            <span class="team-away"><img src="${match.away_logo_url}" class="team-logo-small"> ${match.visitante_lang}</span>
        </div>
        <div class="cell-competition">${match.CompeticionLang}</div>
    </div>`;
});
html += '</div>';
scheduleTableContainer.innerHTML = html;
  }

  rosterSeason.addEventListener('change', updateRosterCategoryFilter);
  rosterCategory.addEventListener('change', updateRosterCompetitionFilter);
  rosterPillsContainer.addEventListener('click', function(e) { if (e.target.classList.contains('competition-pill')) { rosterPillsContainer.querySelectorAll('.competition-pill').forEach(pill => pill.classList.remove('active')); e.target.classList.add('active'); updateRosterView(); } });
  rosterTableContainer.addEventListener('click', function(e){ const header = e.target.closest('th.sortable'); if(!header) return; const sortKey = header.dataset.sortKey; if(rosterSortColumn === sortKey){ rosterSortDirection = rosterSortDirection === 'asc' ? 'desc' : 'asc'; } else { rosterSortColumn = sortKey; rosterSortDirection = 'asc'; } updateRosterView(); });
  scheduleSeason.addEventListener('change', updateScheduleCategoryFilter);
  scheduleCategory.addEventListener('change', updateScheduleCompetitionFilter);
  schedulePillsContainer.addEventListener('click', function(e) { if (e.target.classList.contains('competition-pill')) { schedulePillsContainer.querySelectorAll('.competition-pill').forEach(pill => pill.classList.remove('active')); e.target.classList.add('active'); updateScheduleView(); } });

  if(rosterSeason && rosterSeason.options.length > 0) updateRosterCategoryFilter();
  if(scheduleSeason && scheduleSeason.options.length > 0) updateScheduleCategoryFilter();
}

// ==============================================================================
// == INICIO: FUNCIONES PARA EL NUEVO "HUB" DE COMPETICIÓN (v1)
// ==============================================================================

function initializeCompetitionHub() {
  const dataEl = document.getElementById('competition-hub-data');
  if (!dataEl) return;

  const hubData = JSON.parse(dataEl.textContent);
  const t = hubData.translations;
  let currentRoundIndex = 0;

  // --- Elementos del DOM ---
  const roundTitleEl = document.getElementById('comp-hub-round-title');
  const matchesContainerEl = document.getElementById('comp-hub-schedule-matches');
  const prevRoundBtn = document.getElementById('comp-hub-prev-round');
  const nextRoundBtn = document.getElementById('comp-hub-next-round');
  
  const statsTabsContainer = document.querySelector('.comp-hub-stats-tabs');
  const statsTabButtons = document.querySelectorAll('.comp-hub-tab-btn');
  const statsPanels = {
    goleadoras: document.getElementById('comp-hub-stats-content-goleadoras'),
    tarjetas: document.getElementById('comp-hub-stats-content-tarjetas'),
    porteras: document.getElementById('comp-hub-stats-content-porteras')
  };

  // --- 1. Función para renderizar una Jornada (Schedule) ---
  function renderScheduleRound(roundIndex) {
    const roundData = hubData.jornadas_data[roundIndex];
    if (!roundData) return;

    // Actualizar título y estado de flechas
    roundTitleEl.textContent = roundData.jornada_nombre;
    prevRoundBtn.disabled = (roundIndex === 0);
    nextRoundBtn.disabled = (roundIndex === hubData.jornadas_data.length - 1);
    
    // Limpiar y llenar partidos
    matchesContainerEl.innerHTML = '';
    if (roundData.partidos.length === 0) {
      matchesContainerEl.innerHTML = `<p class="comp-hub-no-matches">${t.no_matches_in_round || 'No matches for this round.'}</p>`;
      return;
    }

    roundData.partidos.forEach(partido => {
      const matchLink = partido.id_partido 
        ? `../natprevari/${partido.id_partido}.html` 
        : '#';
      const isClickable = partido.id_partido ? 'clickable' : '';
      const stadiumInfo = partido.lugar_lang ? `<div class="comp-hub-match-stadium">${t.stadium || 'Stadium'}: ${partido.lugar_lang}</div>` : '';
      const dateInfo = partido.fecha ? `<div class="comp-hub-match-date">${partido.fecha}</div>` : '';

      const matchHtml = `
        <a class="comp-hub-match-row ${isClickable}" href="${matchLink}">
          <div class="comp-hub-match-time">
            ${dateInfo}
            ${stadiumInfo}
          </div>
          <div class="comp-hub-match-teams">
            <span class="comp-hub-team-home">
              ${partido.local_lang} <img src="${partido.local_logo_path}" class="comp-hub-team-logo">
            </span>
            <span class="comp-hub-match-score">${partido.resultado}</span>
            <span class="comp-hub-team-away">
              <img src="${partido.visitante_logo_path}" class="comp-hub-team-logo"> ${partido.visitante_lang}
            </span>
          </div>
        </a>
      `;
      matchesContainerEl.innerHTML += matchHtml;
    });
  }

  // --- 2. Función para renderizar tablas de Estadísticas (Top 5) ---
  function renderStatsTable(tabName) {
    let data, container, html;
    
    switch (tabName) {
      case 'goleadoras':
        data = hubData.stats_goleadoras;
        container = statsPanels.goleadoras;
        html = `<div class="comp-hub-stats-list">`;
        if (data.length > 0) {
          data.forEach(item => {
            html += `
              <div class="comp-hub-stat-row">
                <span class="stat-pos">${item.Pos}</span>
                <div class="stat-player-team">
                  <a class="stat-player" href="${item.link_jugadora}">${item.PlayerName}</a>
                  <a class="stat-team" href="${item.link_equipo}">${item.TeamName}</a>
                </div>
                <span class="stat-value">${item.Goals}</span>
              </div>`;
          });
          html += `</div><a class="comp-hub-see-all-link stats-see-all" href="${hubData.links.goleadoras}">${t.see_all} ></a>`;
        } else {
          html = `<p class="comp-hub-no-stats">${t.no_matches_in_round}</p>`; // Reutilizar traducción
        }
        container.innerHTML = html;
        break;

      case 'tarjetas':
        data = hubData.stats_tarjetas;
        container = statsPanels.tarjetas;
        html = `<div class="comp-hub-stats-list">`;
        if (data.length > 0) {
          data.forEach(item => {
            html += `
              <div class="comp-hub-stat-row">
                <span class="stat-pos">${item.Pos}</span>
                <div class="stat-player-team">
                  <a class="stat-player" href="${item.link_jugadora}">${item.PlayerName}</a>
                  <a class="stat-team" href="${item.link_equipo}">${item.TeamName}</a>
                </div>
                <span class="stat-value cards">
                  <span class="card-icon-table yellow"></span> ${item.YellowCards}
                  <span class="card-icon-table red"></span> ${item.RedCards}
                </span>
              </div>`;
          });
          html += `</div><a class="comp-hub-see-all-link stats-see-all" href="${hubData.links.tarjetas}">${t.see_all} ></a>`;
        } else {
          html = `<p class="comp-hub-no-stats">${t.no_matches_in_round}</p>`;
        }
        container.innerHTML = html;
        break;

      case 'porteras':
        data = hubData.stats_porteras;
        container = statsPanels.porteras;
        html = `<div class="comp-hub-stats-list">`;
         if (data.length > 0) {
          data.forEach(item => {
            html += `
              <div class="comp-hub-stat-row">
                <span class="stat-pos">${item.Pos}</span>
                <div class="stat-player-team">
                  <a class="stat-player" href="${item.link_jugadora}">${item.PlayerName}</a>
                  <a class="stat-team" href="${item.link_equipo}">${item.TeamName}</a>
                </div>
                <span class="stat-value">${parseFloat(item.GA90).toFixed(2)}</span>
              </div>`;
          });
          html += `</div><a class="comp-hub-see-all-link stats-see-all" href="${hubData.links.porteras}">${t.see_all} ></a>`;
        } else {
          html = `<p class="comp-hub-no-stats">${t.no_matches_in_round}</p>`;
        }
        container.innerHTML = html;
        break;
    }
  }

  // --- 3. Event Listeners ---
  
  // Navegación de Jornadas
  prevRoundBtn.addEventListener('click', () => {
    if (currentRoundIndex > 0) {
      currentRoundIndex--;
      renderScheduleRound(currentRoundIndex);
    }
  });

  nextRoundBtn.addEventListener('click', () => {
    if (currentRoundIndex < hubData.jornadas_data.length - 1) {
      currentRoundIndex++;
      renderScheduleRound(currentRoundIndex);
    }
  });

  // Pestañas de Estadísticas (SÓLO SI EXISTEN)
if (statsTabsContainer) { // <--- AÑADE ESTA LÍNEA
  statsTabsContainer.addEventListener('click', (e) => {
    const targetButton = e.target.closest('.comp-hub-tab-btn');
    if (!targetButton) return;

    const tabName = targetButton.dataset.tab;

    // Quitar 'active' a todos
    statsTabButtons.forEach(btn => btn.classList.remove('active'));
    Object.values(statsPanels).forEach(panel => panel.classList.remove('active'));

    // Poner 'active' al clickado
    targetButton.classList.add('active');
    if (statsPanels[tabName]) {
      statsPanels[tabName].classList.add('active');
    }

    // Renderizar (o re-renderizar) el contenido de la pestaña
    renderStatsTable(tabName);
  });
} // <--- Y AÑADE ESTA LLAVE DE CIERRE


  // --- 4. Inicialización ---
  // Encontrar la jornada por defecto
  const defaultRoundRaw = hubData.jornada_por_defecto_raw;
  let initialIndex = 0;
  if (defaultRoundRaw) {
    const foundIndex = hubData.jornadas_data.findIndex(j => j.jornada_id_raw === defaultRoundRaw);
    if (foundIndex !== -1) {
      initialIndex = foundIndex;
    }
  }
  currentRoundIndex = initialIndex;
  
  // Renderizar estado inicial
  renderScheduleRound(currentRoundIndex);
  renderStatsTable('goleadoras'); // Renderizar la primera pestaña por defecto
}

)"


writeLines(script_js, file.path(RUTA_ASSETS_COMPARTIDOS, "script.js"))
message("style.css and script.js files saved to the assets folder.")


# ============================================================================ #
# ==                INTERRUPTORES DE CONTROL DE GENERACIÓN                  ==
# ============================================================================ #
# Cambia estos valores a TRUE o FALSE para controlar qué partes del sitio se
# regeneran. Esto es útil para hacer pruebas rápidas en una sección.
# Para una construcción completa, todos deben estar en TRUE.

GENERAR_PAGINAS_ESTATICAS <- TRUE   # Incluye: Inicio, Archivo, Lista de Equipos/Jugadoras, Acerca de
GENERAR_PAGINAS_COMPETICION <- TRUE   # Todas las páginas de competiciones (menús y tablas)
GENERAR_PERFILES_PARTIDO <- TRUE      # Perfiles individuales para cada partido
GENERAR_PERFILES_JUGADORA <- TRUE    # Perfiles individuales para cada jugadora
GENERAR_PERFILES_EQUIPO <- TRUE       # Perfiles individuales para cada equipo
GENERAR_PERFILES_ARBITRO <- TRUE      # Perfiles individuales para cada árbitro
GENERAR_PERFILES_ESTADIO <- TRUE      # Perfiles individuales para cada estadio

# ============================================================================ #


