# consultar.R
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) stop("Usa: Rscript consultar.R <ID_PARTIDO>")

id_chr <- args[1]
if (!file.exists("actas_cache.rds")) stop("No se encuentra actas_cache.rds")

cache <- readRDS("actas_cache.rds")

if (!id_chr %in% names(cache)) {
  cat(sprintf("\n[!] El ID %s no existe en la caché.\n", id_chr))
} else {
  item <- cache[[id_chr]]
  cat(sprintf("\n=== DATOS DEL PARTIDO %s ===\n", id_chr))
  cat(sprintf("Equipos: %s vs %s\n", item$equipo_local_cyr, item$equipo_visitante_cyr))
  cat(sprintf("FECHA: %s\n", as.character(item$fecha_hora_partido)))
  
  cat("\n--- ÁRBITROS (INFORME ORIGINAL) ---\n")
  cat(sprintf("Principal: %s\n", item$arbitro_principal_nombre))
  cat(sprintf("Asistentes: %s, %s\n", item$arbitro_asist_1_nombre, item$arbitro_asist_2_nombre))
  cat(sprintf("Delegado: %s\n", item$delegado))
  
  cat("\n--- OFICIALES (DELEGIRANJE ENRIQUECIDO) ---\n")
  if (is.null(item$oficiales_delegacion)) {
    cat("Campo inexistente (NULL)\n")
  } else if (nrow(item$oficiales_delegacion) == 0) {
    cat("Consulta realizada: SIN DATOS (Tabla vacía)\n")
  } else {
    print(item$oficiales_delegacion)
  }
  cat("===========================================\n")
}
