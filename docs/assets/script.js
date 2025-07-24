
let searchData = [];

document.addEventListener('DOMContentLoaded', initializeSearch);

function initializeSearch() {
  const searchDataElement = document.getElementById('search-data-json');
  if (searchDataElement) {
    try { 
      searchData = JSON.parse(searchDataElement.textContent); 
    } catch (e) { 
      console.error('Error parsing search data JSON:', e); 
    }
  }
  
  // Ocultar sugerencias si se hace clic fuera del buscador
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
    detailsRow.style.display = (detailsRow.style.display === 'table-row') ? 'none' : 'table-row';
  }
}

function getBasePath() {
  const path = window.location.pathname;
  if (path.endsWith('.html') && !path.endsWith('index.html')) {
    const segments = path.split('/');
    if (segments.length > 2) return '..';
  }
  return '.';
}

function generateLink(target_id) {
  const basePath = getBasePath();
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
    case 'menu': 
      folder = 'natprevaruvanja';
      id = id.replace('competicion-', '');
      break;
    default: return `${basePath}/index.html`;
  }
  return `${basePath}/${folder}/${id}.html`;
}

// MODIFICACIÓN: La función de sugerencias ahora también gestiona la tecla 'Enter'
function handleSearchInput(event) {
  // Si el usuario presiona Enter, ejecuta la búsqueda completa
  if (event.key === 'Enter') {
    event.preventDefault();
    showSearchResults();
    return;
  }

  const input = document.getElementById('search-input');
  const suggestionsContainer = document.getElementById('search-suggestions');
  const query = input.value.trim().toLowerCase();
  
  if (query.length < 2) {
    suggestionsContainer.innerHTML = '';
    suggestionsContainer.style.display = 'none';
    return;
  }

  const searchTokens = query.split(' ').filter(t => t.length > 0);
  const filteredResults = searchData.filter(item => {
    return searchTokens.every(token => item.search_terms.includes(token));
  });

  const top5 = filteredResults.slice(0, 5);
  
  if (top5.length === 0) {
    suggestionsContainer.innerHTML = '';
    suggestionsContainer.style.display = 'none';
    return;
  }

  suggestionsContainer.innerHTML = top5.map(item => `<a href='${generateLink(item.target_id)}'><strong>${item.Име}</strong> <span class='search-result-type'>(${item.Тип})</span></a>`).join('');
  suggestionsContainer.style.display = 'block';
}

// NUEVA FUNCIÓN: Muestra una página con todos los resultados de la búsqueda
function showSearchResults() {
  const input = document.getElementById('search-input');
  const suggestionsContainer = document.getElementById('search-suggestions');
  const mainContent = document.getElementById('main-content');
  
  if (!input || !mainContent) return; // Salida segura si los elementos no existen
  
  suggestionsContainer.style.display = 'none'; // Siempre ocultar sugerencias
  const query = input.value.trim().toLowerCase();
  const originalQuery = input.value.trim();

  if (query.length < 2) {
    mainContent.innerHTML = `<h2>Резултати од пребарувањето</h2>
                             <p>Ве молиме внесете најмалку 2 карактери за да пребарувате.</p>
                             ${crear_botones_navegacion('..').outerHTML}`;
    return;
  }

  const searchTokens = query.split(' ').filter(t => t.length > 0);
  const results = searchData.filter(item => {
    return searchTokens.every(token => item.search_terms.includes(token));
  });

  let resultsHtml = `<h2>Резултати од пребарувањето за: "${originalQuery}"</h2>`;
  
  if (results.length > 0) {
    resultsHtml += '<div id="search-results-list"><ul>';
    results.forEach(item => {
      resultsHtml += `
        <li>
          <a href="${generateLink(item.target_id)}">
            ${item.Име}
            <span class="search-result-type">(${item.Тип})</span>
          </a>
        </li>`;
    });
    resultsHtml += '</ul></div>';
  } else {
    resultsHtml += `<p>Нема пронајдени резултати за "${originalQuery}".</p>`;
  }
  
  resultsHtml += `<div class="nav-buttons">
                    <a href="${window.location.pathname}" class="back-link">← Врати се на претходната страница</a>
                  </div>`;
  
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

