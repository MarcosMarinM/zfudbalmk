# =========================================================================
# SCRIPT INDEPENDIENTE PARA LIMPIAR Y DEDUPLICAR ARCHIVO EXCEL DE JUGADORAS
# =========================================================================

# -------------------------------------------------------------------------
# PASO 0: INSTALAR Y CARGAR PAQUETES NECESARIOS
# -------------------------------------------------------------------------
# Este script necesita 'readxl' para leer, 'dplyr' para manipular, y
# 'writexl' para guardar el nuevo archivo Excel.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, dplyr, writexl)

# -------------------------------------------------------------------------
# PASO 1: DEFINIR RUTAS DE ARCHIVOS (CONFIGURACIÓN)
# -------------------------------------------------------------------------
# Cambia estas rutas para que apunten a tus archivos.
ruta_archivo_entrada <- "/Users/marcosmarinm/Documents/жфМ/zfudbalmk/20238-mkd-players-since-2023.xls"
ruta_archivo_salida  <- "/Users/marcosmarinm/Documents/жфМ/zfudbalmk/igraci.xlsx" # Guardamos como .xlsx que es más moderno

# -------------------------------------------------------------------------
# PASO 2: CARGAR Y VALIDAR EL ARCHIVO EXCEL
# -------------------------------------------------------------------------
message(paste("Cargando archivo:", basename(ruta_archivo_entrada)))

# Comprobar si el archivo de entrada existe antes de intentar leerlo
if (!file.exists(ruta_archivo_entrada)) {
  stop("¡Error! El archivo de entrada no se encuentra en la ruta especificada. Revisa la variable 'ruta_archivo_entrada'.")
}

# Leer el archivo Excel en un dataframe
datos_excel <- read_excel(ruta_archivo_entrada)
n_inicial <- nrow(datos_excel)
message(paste("Se han cargado", n_inicial, "filas."))

# Validar que las columnas necesarias existen
columnas_requeridas <- c("FM ID", "FA ID Number")
if (!all(columnas_requeridas %in% names(datos_excel))) {
  stop("¡Error! El archivo Excel no contiene las columnas necesarias: 'FM ID' y/o 'FA ID Number'.")
}

# -------------------------------------------------------------------------
# PASO 3: LÓGICA DE DEDUPLICACIÓN
# -------------------------------------------------------------------------
message("Iniciando proceso de deduplicación...")

# La lógica se implementa en una cadena de 'dplyr' para mayor claridad.
datos_limpios <- datos_excel %>%
  
  # A. Añadimos un número de fila original para poder aplicar la regla de "quedarse con el primero"
  mutate(orden_original = row_number()) %>%
  
  # B. Agrupamos por la columna que define los duplicados ('FM ID')
  group_by(`FM ID`) %>%
  
  # C. Creamos una columna de 'prioridad' para cada fila dentro de un grupo duplicado.
  #    - Prioridad 1 (la mejor): Cumple la regla del 'FA ID Number'.
  #    - Prioridad 2 (peor): No cumple la regla.
  mutate(
    # La expresión regular "^[0-9]{6}$" significa:
    # ^ : desde el principio de la cadena
    # [0-9]{6} : exactamente 6 caracteres que deben ser dígitos
    # $ : hasta el final de la cadena
    prioridad = case_when(
      !is.na(`FA ID Number`) & grepl("^[0-9]{6}$", `FA ID Number`) ~ 1,
      TRUE ~ 2 # Para todas las demás filas
    )
  ) %>%
  
  # D. Ordenamos las filas dentro de cada grupo.
  #    Primero por nuestra 'prioridad' (las filas que cumplen la regla suben a la cima).
  #    Luego, por 'orden_original' para resolver empates (la primera que apareció se queda arriba).
  arrange(prioridad, orden_original) %>%
  
  # E. Nos quedamos ÚNICAMENTE con la primera fila de cada grupo.
  #    Gracias al arrange anterior, esta será la mejor fila posible según las reglas.
  slice_head(n = 1) %>%
  
  # F. Desagrupamos para volver a tener un dataframe normal
  ungroup() %>%
  
  # G. Eliminamos las columnas auxiliares que creamos
  select(-orden_original, -prioridad)

n_final <- nrow(datos_limpios)
n_eliminados <- n_inicial - n_final
message(paste("Proceso completado. Se han eliminado", n_eliminados, "filas duplicadas."))

# -------------------------------------------------------------------------
# PASO 4: GUARDAR EL ARCHIVO LIMPIO
# -------------------------------------------------------------------------
message(paste("Guardando el archivo limpio con", n_final, "filas en:", ruta_archivo_salida))

tryCatch({
  write_xlsx(datos_limpios, path = ruta_archivo_salida)
  message("\n¡ÉXITO! El archivo ha sido guardado correctamente.")
}, error = function(e) {
  message("\n¡ERROR! No se pudo guardar el archivo. Asegúrate de tener permisos de escritura y de que el archivo no esté abierto en otro programa.")
  message("Mensaje de error original: ", e$message)
})