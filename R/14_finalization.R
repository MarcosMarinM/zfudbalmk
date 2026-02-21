#### 14. PROCESS FINALIZATION ####
if (hubo_cambios) {
  # 14.1.1. Reset to the default language upon completion.
  idioma_actual <<- IDIOMAS_SOPORTADOS[1] 
  
  # >>> INICIO DEL CÓDIGO AÑADIDO (Paso 3) <<<
  saveRDS(competiciones_actuales_ids, file = ruta_comp_log)
  message(paste("\nCompetition log updated successfully at:", ruta_comp_log))
  # >>> FIN DEL CÓDIGO AÑADIDO (Paso 3) <<<
  
  saveRDS(partidos_actuales_ids, file = ruta_build_log)
  message(paste("\nBuild log updated successfully at:", ruta_build_log))
  
  message(paste("\n", t("final_process_success")));
  if (full_rebuild_needed) { 
    message(t("final_full_rebuild")) 
  } else { 
    message(t("final_incremental_update")) 
  }
  message("The process first generated the master pages and then translated them.")
  message(paste(t("final_site_location"), RUTA_SALIDA_RAIZ));
  message(t("final_navigate_prompt"))
} else {
  message("\nNo changes were detected. The website is already up to date.")
}