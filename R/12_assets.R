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
.sortable-header.asc::after { content: '\25B2'; } .sortable-header.desc::after { content: '\25BC'; }
.partido-link { display: flex; align-items: center; padding: 15px; margin: 10px 0; background-color: #e9ecef; border-radius: 5px; }
.partido-link { transition: background-color 0.2s; }
.partido-link-placeholder { display: flex; align-items: center; padding: 15px; margin: 10px 0; background-color: #e9ecef; border-radius: 5px; cursor: default; }
.partido-link:hover { background-color: #ced4da; }
.partido-link span.equipo, .partido-link-placeholder span.equipo, .partido-link-cancelled span.equipo { flex: 1; display: flex; align-items: center; font-weight: bold; }
.partido-link span.equipo-local, .partido-link-placeholder span.equipo-local, .partido-link-cancelled span.equipo-local { justify-content: flex-end; flex-direction: row-reverse; }
.partido-link span.equipo-visitante, .partido-link-placeholder span.equipo-visitante, .partido-link-cancelled span.equipo-visitante { justify-content: flex-start; flex-direction: row-reverse; }
.partido-link span.resultado, .partido-link-placeholder span.resultado, .partido-link-cancelled span.resultado { flex: 0 0 12%; font-size: 1.2em; font-weight: bold; text-align: center; }
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
.team-logo { width: 24px; height: 24px; object-fit: contain; flex-shrink: 0; }
.team-logo.national-team-flag { border-radius: 50%; border: 1px solid #ccc; }
.team-cell { display: flex; align-items: center; }
.team-cell .team-logo { margin-right: 12px; }
.partido-link .team-logo { max-height: 28px; max-width: 28px; height: auto; width: auto; }
.partido-link .equipo-local .team-logo, .partido-link-placeholder .equipo-local .team-logo, .partido-link-cancelled .equipo-local .team-logo { margin-left: 10px; }
.partido-link .equipo-visitante .team-logo, .partido-link-placeholder .equipo-visitante .team-logo, .partido-link-cancelled .equipo-visitante .team-logo { margin-right: 10px; }
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
.team-info { display: flex; align-items: center; overflow: hidden; flex: 1; }
.logo { width: 20px; height: 20px; object-fit: contain; margin-right: 8px; }
.name { font-weight: bold; font-size: 0.95em; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.score { font-weight: bold; font-size: 1.1em; color: #CC0000; }
.score-separator, .score-separator-placeholder, .score-separator-cancelled { font-size: 1.1em; color: #CC0000; line-height: 0.5; }
.score-separator-placeholder { visibility: hidden; }
.score-separator-cancelled { font-style: italic; font-size: 0.8em; color: #777; margin: 10px 0; }
.match-block.cancelled { opacity: 0.7; pointer-events: none; cursor: default; }
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
.nav-links li a { display: block; color: white; text-align: center; padding: 10px 14px; text-decoration: none; transition: background-color 0.3s; border-radius: 5px; }
.dropbtn { display: block; color: white; text-align: center; padding: 10px 14px; text-decoration: none; transition: background-color 0.3s; border-radius: 5px; width: 100%; box-sizing: border-box; }
.nav-links li > a:hover, .dropdown:hover .dropbtn { background-color: #CC0000; }
.nav-links li a.active { background-color: #8B0000; font-weight: bold; }
.dropdown { position: relative; }
.dropdown-content { display: none; position: absolute; background-color: #8B0000; min-width: 200px; box-shadow: 0px 8px 16px 0px rgba(0,0,0,0.2); z-index: 1001; border-radius: 5px; }
.dropdown-content a { color: white !important; font-weight: bold; padding: 12px 16px; text-decoration: none; display: block; text-align: left !important; }
.dropdown-content a:hover { background-color: #CC0100; }
.dropdown:hover .dropdown-content { display: block; }
.dropdown-content hr { border: 0; border-top: 1px solid #ddd; margin: 5px 0; }
.partido-link-placeholder, .partido-link-cancelled { display: flex; text-decoration: none; padding: 15px; border-bottom: 1px solid #eee; background-color: #f9f9f9; color: #777; align-items: center; justify-content: center; gap: 15px; }
.partido-link-cancelled { background-color: #f2f2f2; font-style: italic; opacity: 0.7; cursor: default; }
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
  .dropdown-submenu-content { position: static; display: none; padding-left: 20px; background-color: transparent; box-shadow: none; border: none; }
  .dropdown-submenu.open .dropdown-submenu-content { display: block; }
}
.dropdown-submenu { position: relative; }
.dropdown-submenu .dropdown-submenu-content { display: none; position: absolute; left: 100%; top: 0; background-color: #8B0000; min-width: 200px; box-shadow: 0px 8px 16px 0px rgba(0,0,0,0.2); border-radius: 0 5px 5px 5px; z-index: 1002; }
.dropdown-submenu:hover > .dropdown-submenu-content { display: block; }
.dropdown-submenu .sub-dropbtn { color: white !important; cursor: default; display: flex !important; justify-content: flex-start !important; align-items: center; text-align: left !important; padding: 12px 16px; width: 100%; box-sizing: border-box; }
.dropdown-submenu .sub-dropbtn:hover { background-color: #CC0100; }
.archive-season-container { margin-left: 15px; margin-bottom: 30px; }
.archive-season-container ul { list-style: disc; padding-left: 20px; }
.archive-season-container li { margin-bottom: 8px; }
.letter-nav { text-align: center; margin-bottom: 20px; padding: 10px; background: #f1f1f1; border-radius: 5px; }
.letter-nav a { display: inline-block; padding: 5px 10px; margin: 2px; background-color: #fff; border: 1px solid #ccc; border-radius: 3px; color: #333; text-decoration: none; font-weight: bold; }
.letter-nav a:hover { background-color: #CC0000; color: white; }
.letter-nav a.active { background-color: #8B0000; color: white; border-color: #8B0000; }
.letter-group { display: none; }
.letter-group.active { display: block; }
.player-list ul, .team-list ul, .players-alphabetical-list ul { list-style: none; padding: 0; columns: 3; column-gap: 20px; }
.player-list li, .team-list li, .players-alphabetical-list li { padding: 5px 0; }
.team-list-link { display: flex; align-items: center; gap: 8px; }
@media (max-width: 768px) { .player-list ul, .team-list ul, .players-alphabetical-list ul { columns: 2; } }
@media (max-width: 480px) { .player-list ul, .team-list ul, .players-alphabetical-list ul { columns: 1; } }
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
.season-arrow::before { content: '\25bc'; font-size: 0.8em; color: #6c757d; }
.season-content { display: none; padding: 15px; border-top: 1px solid #dee2e6; }
.tab-nav { margin-bottom: 15px; border-bottom: 1px solid #dee2e6; }
.tab-button { background: none; border: none; padding: 10px 15px; cursor: pointer; font-size: 1em; color: #6c757d; font-weight: 600; border-bottom: 3px solid transparent; }
.tab-button.active { color: #8B0000; border-bottom-color: #8B0000; }
.tab-panel { display: none; }
.tab-panel.active { display: block; }
.stats-table-season th, .matches-table-season th { font-size: 0.9em; padding: 8px; }
.stats-table-season td, .matches-table-season td { font-size: 0.9em; padding: 8px; }
.team-cell-with-logo { display: flex; align-items: center; gap: 10px; }
.team-cell-with-logo img { max-height: 20px; max-width: 20px; height: auto; width: auto; flex-shrink: 0; }
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
.match-list-header > div { text-align: center; } /* Regla general: Centra TODOS los t\u00edtulos */
.match-list-header > div:first-child { text-align: left; } /* Excepci\u00f3n: Alinea a la izquierda S\u00d3LO el PRIMER t\u00edtulo (Fecha) */
.match-list-row { display: grid; grid-template-columns: 1.5fr 5fr 1.2fr 0.8fr 0.8fr 1fr; gap: 10px; align-items: center; padding: 12px 10px; border-bottom: 1px solid #e9ecef; transition: background-color 0.2s; }
.match-list-row.clickable-row { cursor: pointer; }
.match-list-row.clickable-row:hover { background-color: #f8f9fa; }
.match-list-row.cancelled { opacity: 0.6; font-style: italic; cursor: default !important; }
.cell-match { display: flex; align-items: center; justify-content: space-between; }
.team-home, .team-away { display: flex; align-items: center; gap: 8px; flex: 1; font-weight: 500; }
.team-home { justify-content: flex-end; text-align: right; }
.team-away { justify-content: flex-start; text-align: left; }
.team-home .team-logo-small, .team-away .team-logo-small { max-height: 20px; max-width: 20px; height: auto; width: auto; }
.match-score { background-color: #e9ecef; font-weight: bold; padding: 5px 10px; border-radius: 4px; margin: 0 10px; white-space: nowrap; }
.cell-goals, .cell-minutes, .cell-link { text-align: center; }
.cell-cards { text-align: center; display: flex; gap: 4px; justify-content: center; align-items: center; }
.card-icon-table { display: inline-block; width: 8px; height: 12px; border-radius: 1px; }
.card-icon-table.yellow { background-color: #ffc107; }
.card-icon-table.red { background-color: #dc3545; }
/* --- INICIO: ESTILOS PARA P\u00c1GINA DE EQUIPO (V8) --- */

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

/* --- Pesta\u00f1as Principales (Roster/Schedule) --- */
.tab-nav-v4 { display: flex; gap: 10px; border-bottom: 1px solid #dee2e6; }
.tab-button-v4 { background: none; border: 1px solid transparent; border-bottom: none; padding: 8px 12px; cursor: pointer; font-size: 0.9em; color: #6c757d; font-weight: 600; margin-bottom: -1px; border-radius: 5px 5px 0 0; }
.tab-button-v4.active { color: #CC0000; border-color: #dee2e6; background-color: #fff; }

/* --- Contenedor de Filtros y P\u00edldoras --- */
.filters-and-pills-container { background-color: #f7f7f7; margin-top: 25px; }
.filter-bar-v2 { margin-top: 0 !important; }

/* --- P\u00edldoras de Competici\u00f3n --- */
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
  grid-template-columns: 2fr 1fr; /* Columna izquierda m\u00e1s ancha */
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

/* --- 2. Encabezados de Secci\u00f3n --- */
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

/* --- 4. Columna Derecha (Estad\u00edsticas) --- */
.comp-hub-right-col {
  padding: 0; /* El padding lo tendr\u00e1n los contenedores internos */
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


/* --- 5. Fila Inferior (Clasificaci\u00f3n) --- */
.comp-hub-bottom-row {
  grid-column: 1 / -1; /* Ocupa todo el ancho */
  padding: 20px;
}
/* La tabla de clasificaci\u00f3n ya tiene estilos globales, as\u00ed que no necesita m\u00e1s */


/* --- 6. Media Queries (Responsividad) --- */
@media (max-width: 850px) {
  .comp-hub-container {
    grid-template-columns: 1fr; /* Apilar columnas en m\u00f3vil */
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
/* --- 7. Estilos para T\u00edtulos Clicables (A\u00f1adido) --- */
.comp-hub-title-link {
  text-decoration: none;
  color: inherit; /* Hereda el color normal del t\u00edtulo */
}
.comp-hub-title-link:hover .comp-hub-section-title {
  color: #CC0000; /* Hace que el t\u00edtulo se vuelva rojo al pasar el rat\u00f3n */
  text-decoration: underline;
}

/* Ocultar el enlace "see all" en la secci\u00f3n de clasificaci\u00f3n (ya que el t\u00edtulo es el enlace) */
.comp-hub-bottom-row .comp-hub-see-all-link {
  display: none;
}

/* ============================== */
/* --- MATCH PAGE REDESIGN ---    */
/* ============================== */
.mp-container { max-width: 1000px; margin: 0 auto; }

/* Scoreboard */
.mp-scoreboard { display: flex; justify-content: center; align-items: center; margin-bottom: 40px; }
.mp-team { display: flex; align-items: center; flex: 1; }
.mp-team-home { justify-content: flex-end; }
.mp-team-away { justify-content: flex-start; }
.mp-team-name { background-color: #8B0000; color: white; padding: 15px 25px; font-weight: 700; font-size: 14px; letter-spacing: 0.5px; }
.mp-team-name a { color: white; text-decoration: none; }
.mp-team-name span { color: white; }
.mp-team-logo { margin: 0 20px; }
.mp-team-logo img { width: 50px; height: 50px; object-fit: contain; }
.mp-score-container { display: flex; flex-direction: column; align-items: center; background-color: #f4f4f4; padding: 10px 30px; border-radius: 8px; }
.mp-score { font-size: 32px; font-weight: 800; color: #111; }
.mp-status { font-size: 12px; font-weight: 700; margin-top: 5px; color: #111; }
.mp-penalties-note { font-size: 11px; color: #666; }
.mp-official-result { color: #8B0000; font-weight: bold; font-size: 11px; margin-top: 3px; }

/* Match Info Grid */
.mp-match-info { display: grid; grid-template-columns: 1fr 1fr; gap: 20px; margin-bottom: 40px; }
.mp-info-box { background-color: #f4f4f4; border-radius: 4px; padding: 20px; }

/* Goals Summary */
.mp-goals-summary { display: flex; flex-direction: column; }
.mp-goals-title { font-size: 13px; font-weight: 800; text-align: center; margin-bottom: 15px; text-transform: uppercase; color: #666; letter-spacing: 0.5px; }
.mp-goals-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 0; height: 100%; }
.mp-goals-home { text-align: right; border-right: 1px solid #d0d0d0; padding-right: 20px; }
.mp-goals-away { text-align: left; padding-left: 20px; }
.mp-goal-row { font-size: 14px; margin-bottom: 8px; }
.mp-goal-row a { color: inherit; text-decoration: none; }
.mp-goal-row a:hover { text-decoration: underline; }
.mp-goal-player { font-weight: 600; }
.mp-goals-home .mp-goal-minute { margin-left: 8px; font-weight: 700; }
.mp-goals-away .mp-goal-minute { margin-right: 8px; font-weight: 700; }
.mp-og-label { font-size: 11px; color: #8B0000; font-weight: 700; margin-left: 4px; }

/* Details Box */
.mp-detail-row { display: flex; justify-content: space-between; padding: 10px 0; border-bottom: 1px solid #e0e0e0; font-size: 14px; font-weight: 500; }
.mp-detail-row:last-child { border-bottom: none; }
.mp-detail-row a { color: inherit; text-decoration: none; }
.mp-detail-row a:hover { text-decoration: underline; }

/* Referees subsection inside details */
.mp-referees-list { padding: 0; margin: 0; list-style: none; column-count: 2; column-gap: 20px; }
.mp-referees-list li { padding: 5px 0; font-size: 13px; break-inside: avoid; }
.mp-referees-list li a { color: inherit; text-decoration: none; }
.mp-referees-list li a:hover { text-decoration: underline; }

/* Staff subsection inside details */
.mp-staff-section { border-top: 2px solid #eaeaea; padding-top: 30px; }
.mp-staff-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 40px; }
.mp-staff-list { padding: 0; margin: 0; list-style: none; }
.mp-staff-list li { padding: 8px 0; border-bottom: 1px solid #eaeaea; font-size: 14px; display: flex; align-items: center; gap: 5px; }
.mp-staff-list li a { color: inherit; text-decoration: none; }
.mp-staff-list li a:hover { text-decoration: underline; }

/* Competition Header */
.mp-competition-header { text-align: center; margin-bottom: 40px; }
.mp-competition-header h2 { font-size: 20px; font-weight: 700; border-bottom: none; margin-top: 0; padding-bottom: 0; }
.mp-competition-header h2 a { color: #8B0000; text-decoration: none; }
.mp-competition-header p { color: #666; font-size: 14px; }

/* Timeline (Visual Redesign v2 - Split Horizontal) */
.mp-timeline-container { position: relative; padding: 60px 20px; border-bottom: 2px solid #eee; margin-bottom: 40px; overflow-x: auto; background: #fff; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.03); }
.mp-timeline-inner { position: relative; display: flex; align-items: center; justify-content: flex-start; gap: 0; min-width: max-content; padding: 0 50px; }
.mp-timeline-line { position: absolute; top: 50%; left: 0; right: 0; height: 3px; background-color: #8B0000; transform: translateY(-50%); z-index: 1; }
.mp-timeline-event { display: flex; flex-direction: column; align-items: center; justify-content: center; min-width: 140px; z-index: 2; position: relative; }
.mp-event-content, .mp-event-spacer { height: 80px; width: 100%; display: flex; flex-direction: column; align-items: center; justify-content: center; }
.mp-event-content { font-size: 13px; font-weight: 700; text-align: center; }
.mp-event-content a { color: #111; text-decoration: none; }
.mp-event-content a:hover { text-decoration: underline; color: #8B0000; }

.mp-timeline-event.home .mp-event-content { justify-content: flex-end; padding-bottom: 15px; }
.mp-timeline-event.visitor .mp-event-content { justify-content: flex-start; padding-top: 15px; }

.event-icon { font-size: 1.4em; margin-bottom: 2px; }
.player-name { font-size: 14px; font-weight: 800; display: block; color: #111; }
.player-stack { display: flex; flex-direction: column; align-items: center; line-height: 1.1; }
.sub-name { font-size: 12px; font-weight: 400; color: #666; margin-top: 2px; }

.mp-minute { background-color: #fff; font-weight: 900; font-size: 20px; padding: 2px 10px; z-index: 3; color: #333; border: 3px solid #8B0000; border-radius: 8px; min-width: 40px; text-align: center; line-height: 1; }
.mp-timeline-no-events { text-align: center; color: #999; font-size: 15px; padding: 30px 0; font-style: italic; }

/* Lineups */
.mp-lineups-section { border-top: 2px solid #eaeaea; padding-top: 30px; }
.mp-section-title { text-align: center; font-size: 22px; margin-bottom: 30px; border-bottom: none; padding-bottom: 0; color: #111; }
.mp-lineups-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 40px; }
.mp-lineup-header { display: flex; align-items: center; gap: 15px; margin-bottom: 15px; padding-bottom: 5px; border-bottom: 2px solid #8B0000; }
.mp-lineup-header img { width: 40px; height: 40px; object-fit: contain; }
.mp-lineup-header h4 { font-size: 18px; margin: 0; }
.mp-lineup-header h4 a { color: #8B0000; text-decoration: none; }
.mp-lineup ul { list-style: none; padding: 0; margin: 0; }
.mp-lineup li { padding: 8px 0; border-bottom: 1px solid #eaeaea; font-size: 15px; }
.mp-lineup li a { color: inherit; text-decoration: none; }
.mp-lineup li a:hover { text-decoration: underline; }
.mp-lineup .mp-lineup-sub-header { margin-top: 20px; font-size: 13px; font-weight: 800; color: #666; text-transform: uppercase; letter-spacing: 0.5px; padding-bottom: 8px; border-bottom: 2px solid #8B0000; display: inline-block; }

/* Officials Notes */
.mp-notes { white-space: pre-wrap; background-color: #f9f9f9; border-left: 3px solid #ccc; padding: 10px; margin-bottom: 30px; }

/* Penalties */
.mp-penales-section { border-top: 2px solid #eaeaea; padding-top: 30px; margin-top: 10px; }
.mp-penales-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 40px; }
.mp-penales-grid h4 { font-size: 18px; margin-bottom: 15px; }
.mp-penales-grid ul { list-style: none; padding: 0; }
.mp-penales-grid li { padding: 8px 0; border-bottom: 1px solid #eaeaea; font-size: 15px; }
.mp-penales-grid li a { color: inherit; text-decoration: none; }
.mp-penales-grid li a:hover { text-decoration: underline; }

/* Responsive - Match pages */
@media (max-width: 768px) {
  .mp-scoreboard { flex-direction: column; gap: 15px; }
  .mp-team { justify-content: center !important; }
  .mp-match-info { grid-template-columns: 1fr; }
  .mp-lineups-grid, .mp-penales-grid, .mp-staff-grid { grid-template-columns: 1fr; }
  .mp-timeline-container { justify-content: flex-start; gap: 10px; padding: 40px 10px; }
  .mp-timeline-event { min-width: 80px; }
}

/* ============================== */
/* --- STADIUM PAGE REDESIGN ---  */
/* ============================== */
.sp-container { max-width: 1000px; margin: 0 auto; }

/* Header */
.sp-header { text-align: center; margin-bottom: 30px; padding-bottom: 20px; border-bottom: 2px solid #eaeaea; }
.sp-header h2 { font-size: 1.8em; color: #8B0000; border-bottom: none; padding-bottom: 0; margin: 0 0 5px 0; }
.sp-header .sp-subtitle { font-size: 14px; color: #666; }

/* Match history table */
.sp-history-section { border-top: 2px solid #eaeaea; padding-top: 25px; }
.sp-section-title { text-align: center; font-size: 20px; margin-bottom: 20px; border-bottom: none; padding-bottom: 0; color: #111; }
.sp-table { width: 100%; border-collapse: collapse; font-size: 14px; }
.sp-table th { background-color: #8B0000; color: white; padding: 10px 12px; text-align: left; font-weight: 600; font-size: 13px; text-transform: uppercase; letter-spacing: 0.3px; border: none; }
.sp-table td { padding: 10px 12px; border-bottom: 1px solid #eaeaea; vertical-align: middle; }
.sp-table tr:hover { background-color: #f8f9fa; }
.sp-table a { color: inherit; text-decoration: none; }
.sp-table a:hover { text-decoration: underline; color: #8B0000; }
.sp-clickable-row { cursor: pointer; }
.sp-clickable-row:hover { background-color: #f0e8e8; }
.sp-match-cell { display: inline-flex; align-items: center; gap: 8px; }
.sp-table-logo { width: 20px; height: 20px; object-fit: contain; flex-shrink: 0; }
.sp-team-name { font-weight: 600; white-space: nowrap; }
.sp-result { font-weight: 700; text-align: center; margin: 0 4px; }

/* Accordion rows (referee detail toggle) */
.sp-table .summary-row { cursor: pointer; }
.sp-table .summary-row:hover { background-color: #f0e8e8; }
.sp-table .summary-row td:first-child::before { content: '\25B6 '; font-size: 0.8em; color: #8B0000; }
.sp-table .summary-row.active td:first-child::before { content: '\25BC '; }
.sp-table .details-row { display: none; }
.sp-table .details-row > td { padding: 0; border-bottom: 2px solid #8B0000; }
.sp-table .details-row .details-content { padding: 15px 20px; background-color: #fafafa; }
.sp-table .details-row .sp-table { margin: 0; }
.sp-table .details-row .sp-table th { background-color: #555; }

/* Responsive - Stadium pages */
@media (max-width: 768px) {
  .sp-table { font-size: 13px; }
  .sp-table th, .sp-table td { padding: 8px 6px; }
  .sp-team-name { font-size: 12px; }
}

/* ============================================================
   MULTI-LANGUAGE SPA SYSTEM
   Each element can contain multiple .lang-XX spans.
   Only the span matching html[lang] is shown.
   ============================================================ */
.lang-mk, .lang-sq, .lang-es, .lang-en { display: none; }
html[lang="mk"] .lang-mk { display: inline; }
html[lang="sq"] .lang-sq { display: inline; }
html[lang="es"] .lang-es { display: inline; }
html[lang="en"] .lang-en { display: inline; }

/* Language selector buttons */
.lang-btn { text-decoration: none; color: #555; cursor: pointer; }
.lang-btn.active { font-weight: bold; color: #333; text-decoration: underline; }

)"


writeLines(estilo_css, file.path(RUTA_ASSETS_COMPARTIDOS, "style.css"))


### 12.2. Save Functionality Script (script.js)
script_js <- r"(
// ==========================================================================
// VARIABLES GLOBALES Y MULTILING\u00dcISMO
// ==========================================================================
let searchData = [];
let currentLang = 'mk';

// Detect language: URL param > localStorage > default
(function() {
  const urlParams = new URLSearchParams(window.location.search);
  const paramLang = urlParams.get('lang');
  const storedLang = localStorage.getItem('zfudbalmk-lang');
  currentLang = paramLang || storedLang || 'mk';
})();

document.addEventListener('DOMContentLoaded', function() {
  // 1. Apply language immediately
  switchLanguage(currentLang, !new URLSearchParams(window.location.search).has('lang'));

  // 2. Initialize components
  initializeSearch();
  initializeMobileMenu();
  initializePlayerProfileInteractions();

  if (document.getElementById('team-page-data')) initializeTeamProfilePage();
  if (document.getElementById('competition-hub-data')) initializeCompetitionHub();
});

/**
 * Switches language dynamically across the entire page.
 */
function switchLanguage(langCode, save = true) {
  currentLang = langCode;
  if(save) localStorage.setItem('zfudbalmk-lang', langCode);

  // 1. CSS magic: toggle visibility of lang-XX spans
  document.documentElement.lang = langCode;

  // 2. Update language selector buttons
  document.querySelectorAll('.lang-btn').forEach(btn => {
    btn.classList.toggle('active', btn.dataset.lang === langCode);
  });

  // 3. Update non-HTML attributes (placeholders)
  try {
    const searchPromptRaw = document.body.dataset.searchPromptMsg;
    if (searchPromptRaw) {
      const searchPrompt = JSON.parse(searchPromptRaw);
      const searchInput = document.getElementById('search-input');
      if (searchInput && searchPrompt[langCode]) {
        searchInput.placeholder = searchPrompt[langCode];
      }
    }
  } catch(e) { console.error('Error parsing dataset JSON', e); }

  // 4. Sync alphabetical list blocks (teams/players pages)
  updateAlphabetBlocksForLanguage(langCode);

  // 5. Update URL with ?lang= parameter
  const url = new URL(window.location);
  url.searchParams.set('lang', langCode);
  window.history.replaceState({}, '', url);

  // 6. If search is open, repaint results
  const searchInput = document.getElementById('search-input');
  if (searchInput && searchInput.value.trim().length >= 2) {
    showSearchResults();
  }

  // 7. Re-render dynamic content (Team Profiles and Competition Hubs)
  // We use custom events or global functions to trigger re-renders
  if (document.getElementById('team-page-data')) {
    if (typeof updateRosterView === 'function') updateRosterView();
    if (typeof updateScheduleView === 'function') updateScheduleView();
  }
  if (document.getElementById('competition-hub-data')) {
    if (typeof renderScheduleRound === 'function') renderScheduleRound(window.currentRoundIndex || 0);
    if (typeof renderStatsTable === 'function') {
        const activeTab = document.querySelector('.comp-hub-tab-btn.active');
        if (activeTab) renderStatsTable(activeTab.dataset.tab);
    }
  }
}

function updateAlphabetBlocksForLanguage(langCode) {
  const blocks = document.querySelectorAll('.lang-alphabet-block');
  if (!blocks.length) return;

  blocks.forEach(block => {
    const isTargetLang = block.classList.contains(`lang-${langCode}`);
    block.style.display = isTargetLang ? '' : 'none';
  });

  activateDefaultAlphabetLetter(langCode);
}

function activateDefaultAlphabetLetter(langCode) {
  const block = document.querySelector(`.lang-alphabet-block.lang-${langCode}`);
  if (!block) return;

  const preferredLetter = langCode === 'mk' ? '\u0410' : 'A';
  const preferredLink = block.querySelector(`.letter-nav a[data-letter="${preferredLetter}"]`);
  const firstLink = block.querySelector('.letter-nav a[data-letter]');
  const targetLetter = preferredLink
    ? preferredLetter
    : (firstLink ? firstLink.getAttribute('data-letter') : null);

  if (targetLetter) {
    showLetter(targetLetter, langCode);
  }
}

function getBasePath() {
  const isSubfolder = /\/(natprevaruvanja|natprevari|igraci|timovi|sudii|stadioni|staff)\//.test(window.location.pathname);
  return isSubfolder ? '../' : './';
}

function generateLink(target_id) {
    const basePath = getBasePath();
    const parts = target_id.split('-');
    const type = parts[0];
    const id = parts.slice(1).join('-');
    const paths = {
        'jugadora': 'igraci', 'equipo': 'timovi', 'arbitro': 'sudii',
        '\u0441\u0442\u0430\u0434\u0438\u043e\u043d': 'stadioni', 'staff': 'staff', 'menu-competicion': 'natprevaruvanja'
    };
    if (paths[type]) return `${basePath}${paths[type]}/${id}.html`;
    return '#';
}

// ==========================================================================
// BUSCADOR UNIVERSAL
// ==========================================================================
function normalizeForSearch(text) {
  if (!text) return '';
  let s = text.toLowerCase();
  const cyrMap = {'\u0430':'a','\u0431':'b','\u0432':'v','\u0433':'g','\u0434':'d','\u0453':'gj','\u0435':'e','\u0436':'z','\u0437':'z','\u0455':'dz','\u0438':'i','\u0458':'j','\u043a':'k','\u043b':'l','\u0459':'lj','\u043c':'m','\u043d':'n','\u045a':'nj','\u043e':'o','\u043f':'p','\u0440':'r','\u0441':'s','\u0442':'t','\u045c':'kj','\u0443':'u','\u0444':'f','\u0445':'h','\u0446':'c','\u0447':'c','\u0448':'s','\u045f':'dz'};
  s = s.replace(/[\u0430-\u045f]/g, ch => cyrMap[ch] || ch);
  const diaMap = {'\u010d':'c','\u0161':'s','\u017e':'z','\u0111':'d','\u0107':'c','\u01f5':'g','\u1e31':'k','\u0144':'n','\u013a':'l','\u00f1':'n','\u00eb':'e','\u00e7':'c','\u00df':'s','\u00fc':'u','\u00f6':'o','\u00e4':'a','\u00e1':'a','\u00e9':'e','\u00ed':'i','\u00f3':'o','\u00fa':'u','\u00e0':'a','\u00e8':'e','\u00ec':'i','\u00f2':'o','\u00f9':'u','\u00e2':'a','\u00ea':'e','\u00ee':'i','\u00f4':'o','\u00fb':'u'};
  s = s.replace(/[\u00df-\u01f5\u1e31]/g, ch => diaMap[ch] || ch);
  s = s.replace(/dzh/g,'z').replace(/sch/g,'s').replace(/tch/g,'c').replace(/dgh/g,'g');
  s = s.replace(/xh/g,'z').replace(/sh/g,'s').replace(/ch/g,'c').replace(/zh/g,'z');
  s = s.replace(/gj/g,'g').replace(/kj/g,'k').replace(/nj/g,'n').replace(/lj/g,'l');
  s = s.replace(/dj/g,'d').replace(/dz/g,'z').replace(/dh/g,'d').replace(/th/g,'t');
  s = s.replace(/ah/g,'a').replace(/ph/g,'f');
  s = s.replace(/ll/g,'l').replace(/ss/g,'s').replace(/rr/g,'r').replace(/tt/g,'t');
  s = s.replace(/ff/g,'f').replace(/nn/g,'n').replace(/mm/g,'m').replace(/pp/g,'p');
  s = s.replace(/bb/g,'b').replace(/dd/g,'d').replace(/gg/g,'g').replace(/cc/g,'c').replace(/zz/g,'z');
  return s;
}

function initializeSearch() {
  const basePath = getBasePath();
  const jsonUrl = `${basePath}assets/search_data.json`;
  const searchInput = document.getElementById('search-input');

  fetch(jsonUrl).then(r => r.json()).then(data => {
      searchData = data;
      searchData.forEach(item => { item._n = normalizeForSearch(item.search_terms); });
      if(searchInput) searchInput.disabled = false;
  }).catch(err => console.error('Error loading search data:', err));

  if (searchInput) {
    searchInput.addEventListener('keyup', function(event) { handleSearchInput(event); });
  }

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
      }
  });
}

function handleSearchInput(event) {
    const rawInput = event.target.value.trim();
    const suggestionsContainer = document.getElementById('search-suggestions');
    if (event.key === 'Enter') { event.preventDefault(); showSearchResults(); return; }
    if (rawInput.length < 2) { suggestionsContainer.style.display = 'none'; return; }
    const normalizedInput = normalizeForSearch(rawInput);
    const matches = searchData.filter(item => item._n.includes(normalizedInput)).slice(0, 10);
    if (matches.length > 0) {
        suggestionsContainer.innerHTML = '';
        matches.forEach(item => {
            const link = document.createElement('a');
            link.href = generateLink(item.target_id);
            link.innerHTML = `${item.Ime} <span class="search-result-type">(${item.Tip})</span>`;
            suggestionsContainer.appendChild(link);
        });
        suggestionsContainer.style.display = 'block';
    } else { suggestionsContainer.style.display = 'none'; }
}

function showSearchResults() {
    const rawInput = document.getElementById('search-input').value.trim();
    const suggestionsContainer = document.getElementById('search-suggestions');
    const mainContent = document.getElementById('main-content');
    const body = document.body;
    suggestionsContainer.style.display = 'none';
    const titleObj = JSON.parse(body.dataset.searchResultsTitle || '{"mk":"Search"}');
    const noResultsObj = JSON.parse(body.dataset.noSearchResultsMsg || '{"mk":"No results"}');
    const promptObj = JSON.parse(body.dataset.searchPromptMsg || '{"mk":"Type more"}');
    if (rawInput.length < 2) { mainContent.innerHTML = `<h2>${promptObj[currentLang] || 'Please enter at least 2 characters'}</h2>`; return; }
    const normalizedInput = normalizeForSearch(rawInput);
    const matches = searchData.filter(item => item._n.includes(normalizedInput));
    let resultsHtml = `<h2>${titleObj[currentLang] || 'Search results for'}: "${rawInput}"</h2>`;
    if (matches.length > 0) {
        resultsHtml += '<div id="search-results-list"><ul>';
        matches.forEach(item => { resultsHtml += `<li><a href="${generateLink(item.target_id)}">${item.Ime}</a> <span class="search-result-type">(${item.Tip})</span></li>`; });
        resultsHtml += '</ul></div>';
    } else { resultsHtml += `<p>${noResultsObj[currentLang] || 'No results found.'}</p>`; }
    mainContent.innerHTML = resultsHtml;
}

// ==========================================================================
// UTILIDADES UI
// ==========================================================================
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
      if (window.innerWidth <= 850) { event.preventDefault(); dropdownBtn.parentElement.classList.toggle('open'); }
    });
  }
  document.querySelectorAll('.sub-dropbtn').forEach(btn => {
    btn.addEventListener('click', (event) => {
      if (window.innerWidth <= 850) { event.preventDefault(); event.stopPropagation(); btn.parentElement.classList.toggle('open'); }
    });
  });
}

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
            const seasonContent = clickedButton.closest('.season-content');
            if (!seasonContent) return;
            seasonContent.querySelectorAll('.tab-panel').forEach(panel => panel.classList.remove('active'));
            seasonContent.querySelectorAll('.tab-button').forEach(btn => btn.classList.remove('active'));
            const panelToShow = seasonContent.querySelector(`#${tabTarget}-${seasonId}`);
            if (panelToShow) panelToShow.classList.add('active');
            clickedButton.classList.add('active');
        });
    });

    document.querySelectorAll('.sub-tab-button').forEach(button => {
        button.addEventListener('click', (event) => {
            const clickedButton = event.currentTarget;
            const seasonId = clickedButton.dataset.seasonId;
            const subtabTarget = clickedButton.dataset.subtabTarget;
            const seasonContent = clickedButton.closest('.season-content');
            if (!seasonContent) return;
            seasonContent.querySelectorAll('.sub-tab-panel').forEach(panel => panel.classList.remove('active'));
            seasonContent.querySelectorAll('.sub-tab-button').forEach(btn => btn.classList.remove('active'));
            // In 13_html_generation.R it's id = paste0("matches-", season_id_safe, "-", target)
            const panelToShow = seasonContent.querySelector(`#matches-${seasonId}-${subtabTarget}`);
            if (panelToShow) panelToShow.classList.add('active');
            clickedButton.classList.add('active');
        });
    });
}

function showLetter(letter, lang = currentLang) {
  const selectedLang = lang || currentLang || 'mk';
  const activeBlock = document.querySelector(`.lang-alphabet-block.lang-${selectedLang}`);

  const groupsScope = activeBlock || document;
  groupsScope.querySelectorAll('.letter-group').forEach(group => group.classList.remove('active'));

  let activeGroup = document.getElementById(`group-${selectedLang}-${letter}`);
  if (!activeGroup && !activeBlock) {
    activeGroup = document.getElementById(`group-${letter}`);
  }

  if (activeGroup && (!activeBlock || activeBlock.contains(activeGroup))) {
    activeGroup.classList.add('active');
  }

  const linksScope = activeBlock || document;
  linksScope.querySelectorAll('.letter-nav a').forEach(link => {
    const shouldActivate = link.getAttribute('data-letter') === letter;
    link.classList.toggle('active', shouldActivate);
  });
}

function toggleDetails(elementId) {
  var row = document.getElementById(elementId);
  if (!row) return;
  var isVisible = row.style.display === 'table-row';
  row.style.display = isVisible ? 'none' : 'table-row';
  var summaryRow = row.previousElementSibling;
  if (summaryRow) summaryRow.classList.toggle('active', !isVisible);
}

function sortTable(tableId, columnIndex) {
  var table = document.getElementById(tableId);
  if (!table) return;
  var tbody = table.getElementsByTagName("tbody")[0];
  var rows = Array.from(tbody.getElementsByTagName("tr"));
  var ths = table.getElementsByTagName("th");
  var currentSortCol = table.getAttribute("data-sort-col");
  var currentSortDir = table.getAttribute("data-sort-dir");
  var direction = (currentSortCol == columnIndex && currentSortDir === "asc") ? "desc" : "asc";
  rows.sort(function(a, b) {
    var valA = a.getElementsByTagName("td")[columnIndex].textContent.trim() || "0";
    var valB = b.getElementsByTagName("td")[columnIndex].textContent.trim() || "0";
    var numA = parseFloat(valA); var numB = parseFloat(valB);
    if (!isNaN(numA) && !isNaN(numB)) return direction === "asc" ? numA - numB : numB - numA;
    return direction === "asc" ? valA.localeCompare(valB) : valB.localeCompare(valA);
  });
  rows.forEach(function(row) { tbody.appendChild(row); });
  table.setAttribute("data-sort-col", columnIndex);
  table.setAttribute("data-sort-dir", direction);
  for (var i = 0; i < ths.length; i++) { ths[i].classList.remove("asc", "desc"); if (i == columnIndex) ths[i].classList.add(direction); }
}

// ==============================================================================
// RENDERIZADO DINAMICO (Team y Competition Hub)
// ==============================================================================
function initializeTeamProfilePage() {
  const dataEl = document.getElementById('team-page-data');
  if (!dataEl) return;
  const pageData = JSON.parse(dataEl.textContent);
  const translationsByLang = pageData.translations || {};
  const getTeamTranslations = () => translationsByLang[currentLang] || translationsByLang.mk || translationsByLang.en || {};
  const getCompetitionId = (item) => {
    const explicitId = item.competicion_id || item['competicion_id.x'] || item['competicion_id.y'];
    if (explicitId) return String(explicitId);
    const compName = (item.competicion_nombre || '').trim();
    const season = (item.competicion_temporada || '').trim();
    if (compName && season) return `${compName}__${season}`;
    if (compName) return compName;
    return null;
  };
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

  if (rosterSeason) rosterSeason.addEventListener('change', updateRosterCategoryFilter);
  if (rosterCategory) rosterCategory.addEventListener('change', updateRosterCompetitionFilter);
  if (rosterPillsContainer) rosterPillsContainer.addEventListener('click', e => {
    const pill = e.target.closest('.competition-pill');
    if (pill) {
      rosterPillsContainer.querySelectorAll('.competition-pill').forEach(p => p.classList.remove('active'));
      pill.classList.add('active');
      updateRosterView();
    }
  });

  if (scheduleSeason) scheduleSeason.addEventListener('change', updateScheduleCategoryFilter);
  if (scheduleCategory) scheduleCategory.addEventListener('change', updateScheduleCompetitionFilter);
  if (schedulePillsContainer) schedulePillsContainer.addEventListener('click', e => {
    const pill = e.target.closest('.competition-pill');
    if (pill) {
      schedulePillsContainer.querySelectorAll('.competition-pill').forEach(p => p.classList.remove('active'));
      pill.classList.add('active');
      updateScheduleView();
    }
  });

  function updateRosterCategoryFilter() {
    const t = getTeamTranslations();
    const season = rosterSeason.value;
    const categories = [...new Set(pageData.roster_data.filter(p => p.competicion_temporada === season).map(p => JSON.stringify({ key: p.category_key })))].map(s => JSON.parse(s));
    const order = {"category_senior":1,"category_youth":2,"category_cadet":3,"category_pioneri":4,"category_mladi_pioneri":5,"category_petlinja":6,"category_pomali_petlinja":7};
    categories.sort((a, b) => (order[a.key] || 99) - (order[b.key] || 99));
    rosterCategory.innerHTML = '';
    categories.forEach(c => { rosterCategory.innerHTML += `<option value="${c.key}">${t[c.key] || c.key}</option>`; });
    updateRosterCompetitionFilter();
  }

  function updateRosterCompetitionFilter() {
    rosterPillsContainer.innerHTML = '';
    if(!rosterSeason.value || !rosterCategory.value) { updateRosterView(); return; }
    const season = rosterSeason.value; const category = rosterCategory.value;
    const competitions = [...new Set(pageData.roster_data
      .filter(p => p.competicion_temporada === season && p.category_key === category)
      .map(p => JSON.stringify({id: getCompetitionId(p), name: p.CompeticionLang || p.competicion_nombre})))
    ].map(s => JSON.parse(s)).filter(c => !!c.id);
    competitions.forEach(c => { rosterPillsContainer.innerHTML += `<button class="competition-pill" data-competition-id="${c.id}">${c.name}</button>`; });
    const firstPill = rosterPillsContainer.querySelector('.competition-pill');
    if (firstPill) firstPill.classList.add('active');
    updateRosterView();
  }

  function updateRosterView() {
    const t = getTeamTranslations();
    const activePill = rosterPillsContainer.querySelector('.competition-pill.active');
    if (!activePill) { rosterTableContainer.innerHTML = `<p style="text-align:center; padding: 20px;">${t.no_players_found}</p>`; return; }
    const season = rosterSeason.value; const category = rosterCategory.value; const competition = activePill.dataset.competitionId;
    let filteredData = pageData.roster_data.filter(p => p.competicion_temporada === season && p.category_key === category && getCompetitionId(p) === competition);
    if (filteredData.length === 0) { rosterTableContainer.innerHTML = `<p style="text-align:center; padding: 20px;">${t.no_players_found}</p>`; return; }
    const direction = rosterSortDirection === 'asc' ? 1 : -1;
    filteredData.sort((a, b) => {
      let valA, valB;
      if (rosterSortColumn === 'dorsal_principal') { valA = parseInt(a.dorsal_principal) || 999; valB = parseInt(b.dorsal_principal) || 999;
      } else if (rosterSortColumn === 'PlayerName') {
        const nameA = a[`PlayerName_${currentLang}`] || a.PlayerName_mk || '';
        const nameB = b[`PlayerName_${currentLang}`] || b.PlayerName_mk || '';
        return nameA.localeCompare(nameB, currentLang) * direction;
      } else { valA = parseInt(a[rosterSortColumn]) || 0; valB = parseInt(b[rosterSortColumn]) || 0; }
      return (valA - valB) * direction;
    });
    let html = `<table class="player-roster-table"><thead><tr>
      <th class="sortable" data-sort-key="dorsal_principal" style="width:5%;">${t.col_dorsal}</th>
      <th class="sortable" data-sort-key="PlayerName">${t.col_player}</th>
      <th class="sortable" data-sort-key="Played" style="width:8%;">${t.col_apps}</th>
      <th class="sortable" data-sort-key="Minutes" style="width:8%;">${t.col_mins}</th>
      <th class="sortable" data-sort-key="Goals" style="width:8%;">${t.col_goals}</th>
      <th class="sortable" data-sort-key="Yellows" style="width:12%;">${t.col_cards}</th>
    </tr></thead><tbody>`;
    filteredData.forEach(player => {
      const position = t[player.posicion_final_unificada] || player.posicion_final_unificada || '';
      const playerName = player[`PlayerName_${currentLang}`] || player.PlayerName_mk || '';
      const playerURL = `${getBasePath()}igraci/${player.id}.html`;
      html += `<tr onclick="window.location.href='${playerURL}'">
        <td>${player.dorsal_principal || '-'}</td>
        <td><div class="player-info-cell"><div class="player-name">${playerName}</div><div class="player-position">${position}</div></div></td>
        <td style="text-align:center;">${player.Played || 0}</td><td style="text-align:center;">${player.Minutes || 0}</td>
        <td style="text-align:center;">${player.Goals || 0}</td>
        <td style="text-align:center;"><span class="card-icon-table yellow"></span> ${player.Yellows || 0}&nbsp;/&nbsp;<span class="card-icon-table red"></span> ${player.Reds || 0}</td>
      </tr>`;
    });
    html += '</tbody></table>';
    rosterTableContainer.innerHTML = html;
    const currentHeader = rosterTableContainer.querySelector(`[data-sort-key="${rosterSortColumn}"]`);
    if (currentHeader) currentHeader.classList.add(rosterSortDirection === 'asc' ? 'sort-asc' : 'sort-desc');
  }

  function updateScheduleCategoryFilter() {
    const t = getTeamTranslations();
    const season = scheduleSeason.value;
    const categories = [...new Set(pageData.matches_data.filter(m => m.competicion_temporada === season).map(m => JSON.stringify({key: m.category_key, name: m.category_name})))].map(s => JSON.parse(s));
    const order = {"category_senior":1,"category_youth":2,"category_cadet":3,"category_pioneri":4,"category_mladi_pioneri":5,"category_petlinja":6,"category_pomali_petlinja":7};
    categories.sort((a, b) => (order[a.key] || 99) - (order[b.key] || 99));
    scheduleCategory.innerHTML = '';
    categories.forEach(c => { scheduleCategory.innerHTML += `<option value="${c.key}">${t[c.key] || c.name}</option>`; });
    updateScheduleCompetitionFilter();
  }

  function updateScheduleCompetitionFilter() {
      schedulePillsContainer.innerHTML = '';
      if(!scheduleSeason.value || !scheduleCategory.value) { updateScheduleView(); return; }
      const season = scheduleSeason.value; const category = scheduleCategory.value;
      const competitions = [...new Set(pageData.matches_data
        .filter(m => m.competicion_temporada === season && m.category_key === category)
        .map(m => JSON.stringify({id: getCompetitionId(m), name: m.CompeticionLang})))
      ].map(s => JSON.parse(s)).filter(c => !!c.id);
      competitions.forEach(c => { schedulePillsContainer.innerHTML += `<button class="competition-pill" data-competition-id="${c.id}">${c.name}</button>`; });
      const firstPill = schedulePillsContainer.querySelector('.competition-pill');
      if (firstPill) firstPill.classList.add('active');
      updateScheduleView();
  }

  function updateScheduleView() {
    const t = getTeamTranslations();
    const activePill = schedulePillsContainer.querySelector('.competition-pill.active');
    if(!activePill) { scheduleTableContainer.innerHTML = `<p style="text-align:center; padding: 20px;">${t.no_matches_found}</p>`; return; }
    const season = scheduleSeason.value; const category = scheduleCategory.value; const competition = activePill.dataset.competitionId;
    let filteredMatches = pageData.matches_data.filter(m => m.competicion_temporada === season && m.category_key === category && getCompetitionId(m) === competition);
    if (filteredMatches.length === 0) { scheduleTableContainer.innerHTML = `<p style="text-align:center; padding: 20px;">${t.no_matches_found}</p>`; return; }
    filteredMatches.sort((a, b) => new Date(b.fecha.split('.').reverse().join('-')) - new Date(a.fecha.split('.').reverse().join('-')));
    let html = `<div class="match-list-container"><div class="match-list-header"><div>${t.col_date}</div><div>${t.col_match}</div><div>${t.col_competition || 'Competition'}</div></div>`;
    filteredMatches.forEach(match => {
        const isCancelled = !!match.es_cancelado;
        const isOfficial = !!match.es_resultado_oficial;
        const hasLink = match.id_partido && (!isCancelled || isOfficial);
        const matchURL = hasLink ? `${getBasePath()}natprevari/${match.id_partido}.html` : '#';
        const rowClass = (isCancelled && !isOfficial) ? 'match-list-row cancelled' : 'match-list-row clickable-row';
        const scoreSuffix = isOfficial ? '*' : '';
        const scoreText = (isCancelled && !isOfficial) ? t.match_cancelled : `${match.goles_local}${scoreSuffix} : ${match.goles_visitante}${scoreSuffix}`;
        html += `<div class="${rowClass}" onclick="${matchURL !== '#' ? `window.location.href='${matchURL}'` : ''}">
          <div class="cell-date">${match.fecha}</div>
          <div class="cell-match"><span class="team-home">${match.local_lang} <img src="${match.home_logo_url}" class="team-logo-small"></span><span class="match-score">${scoreText}</span><span class="team-away"><img src="${match.away_logo_url}" class="team-logo-small"> ${match.visitante_lang}</span></div>
          <div class="cell-competition">${match.CompeticionLang}</div></div>`;
    });
    html += '</div>';
    scheduleTableContainer.innerHTML = html;
  }

  if(rosterSeason && rosterSeason.options.length > 0) updateRosterCategoryFilter();
  if(scheduleSeason && scheduleSeason.options.length > 0) updateScheduleCategoryFilter();

  // Expose global functions for switchLanguage
  window.updateRosterView = updateRosterView;
  window.updateScheduleView = updateScheduleView;
}

function initializeCompetitionHub() {
  const dataEl = document.getElementById('competition-hub-data');
  if (!dataEl) return;
  const hubData = JSON.parse(dataEl.textContent);
  const t = hubData.translations;
  let currentRoundIndex = 0;
  const roundTitleEl = document.getElementById('comp-hub-round-title');
  const matchesContainerEl = document.getElementById('comp-hub-schedule-matches');
  const prevRoundBtn = document.getElementById('comp-hub-prev-round');
  const nextRoundBtn = document.getElementById('comp-hub-next-round');
  const statsTabsContainer = document.querySelector('.comp-hub-stats-tabs');
  const statsTabButtons = document.querySelectorAll('.comp-hub-tab-btn');
  const statsPanels = { goleadoras: document.getElementById('comp-hub-stats-content-goleadoras'), tarjetas: document.getElementById('comp-hub-stats-content-tarjetas'), porteras: document.getElementById('comp-hub-stats-content-porteras') };

  function renderScheduleRound(roundIndex) {
    const roundData = hubData.jornadas_data[roundIndex];
    if (!roundData) return;
    roundTitleEl.innerHTML = roundData.jornada_nombre;
    prevRoundBtn.disabled = (roundIndex === 0);
    nextRoundBtn.disabled = (roundIndex === hubData.jornadas_data.length - 1);
    matchesContainerEl.innerHTML = '';
    if (roundData.partidos.length === 0) { matchesContainerEl.innerHTML = `<p class="comp-hub-no-matches">${t.no_matches_in_round}</p>`; return; }
    roundData.partidos.forEach(partido => {
      const isCancelled = !!partido.es_cancelado;
      const isOfficial = !!partido.es_resultado_oficial;
      const hasLink = partido.id_partido && (!isCancelled || isOfficial);
      const matchLink = hasLink ? `${getBasePath()}natprevari/${partido.id_partido}.html` : 'javascript:void(0)';
      const wrapperTag = hasLink ? 'a' : 'div';
      const hrefAttr = hasLink ? `href="${matchLink}"` : '';
      const isClickable = hasLink ? 'clickable' : '';
      const scoreSuffix = isOfficial ? '*' : '';
      const scoreText = (isCancelled && !isOfficial) ? t.match_cancelled : (partido.resultado ? partido.resultado : '-');
      const stadiumInfo = partido.lugar_lang ? `<div class="comp-hub-match-stadium">${t.stadium}: ${partido.lugar_lang}</div>` : '';
      const dateInfo = partido.fecha ? `<div class="comp-hub-match-date">${partido.fecha}</div>` : '';
      matchesContainerEl.innerHTML += `<${wrapperTag} class="comp-hub-match-row ${isClickable}" ${hrefAttr}><div class="comp-hub-match-time">${dateInfo} ${stadiumInfo}</div><div class="comp-hub-match-teams"><span class="comp-hub-team-home">${partido.local_lang} <img src="${partido.local_logo_path}" class="comp-hub-team-logo"></span><span class="comp-hub-match-score">${scoreText}</span><span class="comp-hub-team-away"><img src="${partido.visitante_logo_path}" class="comp-hub-team-logo"> ${partido.visitante_lang}</span></div></${wrapperTag}>`;
    });
  }

  function renderStatsTable(tabName) {
    let data, container, html;
    switch (tabName) {
      case 'goleadoras': data = hubData.stats_goleadoras; container = statsPanels.goleadoras;
        html = `<div class="comp-hub-stats-list">`;
        if (data.length > 0) { data.forEach(item => { html += `<div class="comp-hub-stat-row"><span class="stat-pos">${item.Pos}</span><div class="stat-player-team"><a class="stat-player" href="${item.link_jugadora}">${item.PlayerName}</a><a class="stat-team" href="${item.link_equipo}">${item.TeamName}</a></div><span class="stat-value">${item.Goals}</span></div>`; }); html += `</div><a class="comp-hub-see-all-link stats-see-all" href="${hubData.links.goleadoras}">${t.see_all} ></a>`; }
        else html = `<p class="comp-hub-no-stats">${t.no_matches_in_round}</p>`;
        container.innerHTML = html; break;
      case 'tarjetas': data = hubData.stats_tarjetas; container = statsPanels.tarjetas;
        html = `<div class="comp-hub-stats-list">`;
        if (data.length > 0) { data.forEach(item => { html += `<div class="comp-hub-stat-row"><span class="stat-pos">${item.Pos}</span><div class="stat-player-team"><a class="stat-player" href="${item.link_jugadora}">${item.PlayerName}</a><a class="stat-team" href="${item.link_equipo}">${item.TeamName}</a></div><span class="stat-value cards"><span class="card-icon-table yellow"></span> ${item.YellowCards} <span class="card-icon-table red"></span> ${item.RedCards}</span></div>`; }); html += `</div><a class="comp-hub-see-all-link stats-see-all" href="${hubData.links.tarjetas}">${t.see_all} ></a>`; }
        else html = `<p class="comp-hub-no-stats">${t.no_matches_in_round}</p>`;
        container.innerHTML = html; break;
      case 'porteras': data = hubData.stats_porteras; container = statsPanels.porteras;
        html = `<div class="comp-hub-stats-list">`;
        if (data.length > 0) { data.forEach(item => { html += `<div class="comp-hub-stat-row"><span class="stat-pos">${item.Pos}</span><div class="stat-player-team"><a class="stat-player" href="${item.link_jugadora}">${item.PlayerName}</a><a class="stat-team" href="${item.link_equipo}">${item.TeamName}</a></div><span class="stat-value">${parseFloat(item.GA90).toFixed(2)}</span></div>`; }); html += `</div><a class="comp-hub-see-all-link stats-see-all" href="${hubData.links.porteras}">${t.see_all} ></a>`; }
        else html = `<p class="comp-hub-no-stats">${t.no_matches_in_round}</p>`;
        container.innerHTML = html; break;
    }
  }

  prevRoundBtn.addEventListener('click', () => { if (currentRoundIndex > 0) { currentRoundIndex--; window.currentRoundIndex = currentRoundIndex; renderScheduleRound(currentRoundIndex); } });
  nextRoundBtn.addEventListener('click', () => { if (currentRoundIndex < hubData.jornadas_data.length - 1) { currentRoundIndex++; window.currentRoundIndex = currentRoundIndex; renderScheduleRound(currentRoundIndex); } });
  if (statsTabsContainer) {
    statsTabsContainer.addEventListener('click', (e) => {
      const targetButton = e.target.closest('.comp-hub-tab-btn');
      if (!targetButton) return;
      const tabName = targetButton.dataset.tab;
      statsTabButtons.forEach(btn => btn.classList.remove('active'));
      Object.values(statsPanels).forEach(panel => panel.classList.remove('active'));
      targetButton.classList.add('active');
      if (statsPanels[tabName]) statsPanels[tabName].classList.add('active');
      renderStatsTable(tabName);
    });
  }
  const defaultRoundRaw = hubData.jornada_por_defecto_raw;
  let initialIndex = 0;
  if (defaultRoundRaw) { const foundIndex = hubData.jornadas_data.findIndex(j => j.jornada_id_raw === defaultRoundRaw); if (foundIndex !== -1) initialIndex = foundIndex; }
  currentRoundIndex = initialIndex;
  window.currentRoundIndex = currentRoundIndex; // Store globally for switchLanguage
  window.renderScheduleRound = renderScheduleRound;
  window.renderStatsTable = renderStatsTable;
  renderScheduleRound(currentRoundIndex);
  renderStatsTable('goleadoras');
}

)"

writeLines(script_js, file.path(RUTA_ASSETS_COMPARTIDOS, "script.js"))
message("style.css and script.js files saved to the assets folder.")

### 12.3. Save Favicon (if exists)
if (file.exists("favicon.png")) {
  file.copy("favicon.png", file.path(RUTA_SALIDA_RAIZ, "favicon.png"), overwrite = TRUE)
  message("favicon.png copied to the root of the web folder.")
}


# ============================================================================ #
# ==                INTERRUPTORES DE CONTROL DE GENERACI\u00d3N                  ==
# ============================================================================ #
# Cambia estos valores a TRUE o FALSE para controlar qu\u00e9 partes del sitio se
# regeneran. Esto es \u00fatil para hacer pruebas r\u00e1pidas en una secci\u00f3n.
# Para una construcci\u00f3n completa, todos deben estar en TRUE.

GENERAR_PAGINAS_ESTATICAS <- TRUE # Incluye: Inicio, Archivo, Lista de Equipos/Jugadoras, Acerca de
GENERAR_PAGINAS_COMPETICION <- TRUE # Todas las p\u00e1ginas de competiciones (men\u00fas y tablas)
GENERAR_PERFILES_PARTIDO <- TRUE # Perfiles individuales para cada partido
GENERAR_PERFILES_JUGADORA <- TRUE # Perfiles individuales para cada jugadora
GENERAR_PERFILES_EQUIPO <- TRUE # Perfiles individuales para cada equipo
GENERAR_PERFILES_ARBITRO <- TRUE # Perfiles individuales para cada \u00e1rbitro
GENERAR_PERFILES_ESTADIO <- TRUE # Perfiles individuales para cada estadio
GENERAR_PERFILES_STAFF <- TRUE # Perfiles individuales para cada miembro de staff

# ============================================================================ #
