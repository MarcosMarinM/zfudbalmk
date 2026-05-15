source("R/07_setup.R", encoding = "UTF-8")
source("R/08_functions.R", encoding = "UTF-8")
source("R/09_data_loading.R", encoding = "UTF-8")
source("R/10_data_processing.R", encoding = "UTF-8")
source("R/11_aggregated_datasets.R", encoding = "UTF-8")

# Check Yllza Maksuti entries
cat("\n\n=== Yllza Maksuti entries in jugadoras_stats_df ===\n")
yllza <- jugadoras_stats_df %>% filter(grepl("yllza|maksuti", id, ignore.case = TRUE) | grepl("Yllza|Maksuti", PlayerName_mk))
print(yllza %>% select(id, PlayerName_mk, posicion_final_unificada, fecha_nacimiento, nacionalidad, ciudad_nacimiento, CalledUp, Goals), width = 200)

cat("\n\n=== Bio data rows in posiciones_procesadas_df for Yllza ===\n")
bio_y <- posiciones_procesadas_df %>% filter(grepl("yllza|maksuti", id, ignore.case = TRUE))
print(bio_y, width = 200)

# Check how many bio rows were expanded
cat("\n\n=== Reconciliation stats ===\n")
cat("Total bio records:", nrow(posiciones_procesadas_df), "\n")
cat("Unique bio IDs:", n_distinct(posiciones_procesadas_df$id), "\n")

# Count players where bio data is present in stats
bio_filled <- jugadoras_stats_df %>% filter(!is.na(posicion_final_unificada) | !is.na(fecha_nacimiento))
cat("\nPlayers with bio data in stats:", nrow(bio_filled), "out of", nrow(jugadoras_stats_df), "\n")
