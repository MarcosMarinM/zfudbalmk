
let searchData = [];

document.addEventListener('DOMContentLoaded', function() {
  // INICIALIZADORES GLOBALES (PARA TODO EL SITIO)
  initializeSearch();
  initializeMobileMenu();
  initializePlayerProfileInteractions();
  
  // INICIALIZADOR ESPECÍFICO PARA LA PÁGINA DE EQUIPO
  // Se ejecuta solo si encuentra el contenedor de datos de la página de equipo.
  if (document.getElementById('team-page-data')) {
    initializeTeamProfilePage();
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

