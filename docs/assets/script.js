
let searchData = [];

document.addEventListener('DOMContentLoaded', initializeSearch);

// CORRECCIÓN: Simplifica la obtención de la ruta base del idioma.
function getLangBasePath() {
  const path = window.location.pathname;
  const lang = document.documentElement.lang || 'mk';
  // Encuentra la parte de la ruta hasta la carpeta del idioma (e.g., /docs/mk/)
  const langRootIndex = path.indexOf(`/${lang}/`);
  if (langRootIndex > -1) {
    return path.substring(0, langRootIndex + lang.length + 2);
  }
  // Fallback para la página de redirección en la raíz
  return `./${lang}/`;
}

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
  document.addEventListener('click', function(event) {
    const clickableRow = event.target.closest('.clickable-row');
    if (clickableRow && clickableRow.dataset.href) { window.location.href = clickableRow.dataset.href; }
  });
}

function toggleDetails(elementId) {
  const detailsRow = document.getElementById(elementId);
  if (detailsRow) { detailsRow.style.display = (detailsRow.style.display === 'table-row') ? 'none' : 'table-row'; }
}

// CORRECCIÓN: Usa la nueva función para construir los enlaces de búsqueda.
function generateLink(target_id) {
  const basePath = getLangBasePath();
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
    case 'menu': folder = 'natprevaruvanja'; id = id.replace('competicion-', ''); break;
    default: return `${basePath}index.html`;
  }
  return `${basePath}${folder}/${id}.html`;
}

function handleSearchInput(event) {
  if (event.key === 'Enter') { event.preventDefault(); showSearchResults(); return; }
  const input = document.getElementById('search-input');
  const suggestionsContainer = document.getElementById('search-suggestions');
  const query = input.value.trim().toLowerCase();
  if (query.length < 2) { suggestionsContainer.innerHTML = ''; suggestionsContainer.style.display = 'none'; return; }
  const searchTokens = query.split(' ').filter(t => t.length > 0);
  const filteredResults = searchData.filter(item => searchTokens.every(token => item.search_terms.includes(token)));
  const top5 = filteredResults.slice(0, 5);
  if (top5.length === 0) { suggestionsContainer.innerHTML = ''; suggestionsContainer.style.display = 'none'; return; }
  suggestionsContainer.innerHTML = top5.map(item => `<a href='${generateLink(item.target_id)}'><strong>${item.Име}</strong> <span class='search-result-type'>(${item.Тип})</span></a>`).join('');
  suggestionsContainer.style.display = 'block';
}

function showSearchResults() {
  const input = document.getElementById('search-input');
  const suggestionsContainer = document.getElementById('search-suggestions');
  const mainContent = document.getElementById('main-content');
  const body = document.body;
  if (!input || !mainContent) return;
  suggestionsContainer.style.display = 'none';
  const query = input.value.trim().toLowerCase();
  const originalQuery = input.value.trim();
  const basePath = getLangBasePath();
  
  if (query.length < 2) {
    mainContent.innerHTML = `<h2>${body.dataset.searchResultsTitle || 'Search Results'}</h2><p>${body.dataset.searchPromptMsg || 'Please enter at least 2 characters.'}</p><div class="nav-buttons"><a href="${basePath}index.html" class="back-link">← Back</a></div>`;
    return;
  }
  
  const searchTokens = query.split(' ').filter(t => t.length > 0);
  const results = searchData.filter(item => searchTokens.every(token => item.search_terms.includes(token)));
  
  let resultsHtml = `<h2>${body.dataset.searchResultsTitle || 'Search Results for'}: "${originalQuery}"</h2>`;
  if (results.length > 0) {
    resultsHtml += '<div id="search-results-list"><ul>';
    results.forEach(item => { resultsHtml += `<li><a href="${generateLink(item.target_id)}">${item.Име}<span class="search-result-type">(${item.Тип})</span></a></li>`; });
    resultsHtml += '</ul></div>';
  } else {
    resultsHtml += `<p>${body.dataset.noSearchResultsMsg || 'No results found for'} "${originalQuery}".</p>`;
  }
  resultsHtml += `<div class="nav-buttons"><a href="#" onclick="history.back(); return false;" class="back-link">← Back</a></div>`;
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

