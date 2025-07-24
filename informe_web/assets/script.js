
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

