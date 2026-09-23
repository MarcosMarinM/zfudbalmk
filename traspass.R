################################################################################
# traspass.R - Traspasos de la temporada 25/26 al inicio de la 26/27
#
# Compara las plantillas del FINAL de la 25/26 (últimos partidos con alineación)
# con las del INICIO de la 26/27 (primeras 2 jornadas de la Prva).
#
# Para cada club de la Prva 26/27 se listan las jugadoras "nuevas" (que NO
# estaban en su plantilla a final de la 25/26), buscando su equipo previo en la
# 25/26 (Prva, Vtora) y su ID de federación en igracki.xlsx.
################################################################################
.libPaths(c(".rlib", .libPaths()))
suppressMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr); library(readxl)
})

#### 1. Cargar caché y extraer apariciones ####
cat("Cargando actas_cache.rds...\n")
cache <- readRDS("actas_cache.rds")

apariciones <- list()
for (nm in names(cache)) {
  it <- cache[[nm]]
  pi <- it$partido_info
  if (is.null(pi) || length(pi$competicion_nombre) == 0 || is.na(pi$competicion_nombre)) next
  for (sn in c("alineacion_local", "alineacion_visitante")) {
    df <- it[[sn]]
    if (!is.data.frame(df) || nrow(df) == 0) next
    equipo <- if (sn == "alineacion_local") pi$local else pi$visitante
    apariciones[[length(apariciones) + 1]] <- tibble(
      id_partido    = as.character(pi$id_partido),
      temporada     = pi$competicion_temporada,
      competicion   = pi$competicion_nombre,
      jornada       = as.integer(pi$jornada),
      fecha         = as.character(pi$fecha),
      equipo        = equipo,
      nombre        = df$nombre,
      nombre_latin  = if ("nombre_latin" %in% names(df)) df$nombre_latin else NA_character_
    )
  }
}
ap <- bind_rows(apariciones)
ap <- ap %>%
  filter(!is.na(nombre), nzchar(trimws(nombre)), nombre != "NaN") %>%
  mutate(nombre = str_squish(nombre))

#### 2. Claves de identidad de la jugadora ####
deaccent <- function(x) sapply(x, function(z) if (is.na(z)) NA_character_ else iconv(z, from = "UTF-8", to = "ASCII//TRANSLIT"), USE.NAMES = FALSE)
ap <- ap %>% mutate(
  nombre_clave = stringr::str_to_lower(gsub("[^[:alpha:] ]", "", nombre)),
  latin_key    = ifelse(is.na(nombre_latin) | nombre_latin == "" | nombre_latin == "NaN",
                        NA_character_,
                        stringr::str_to_lower(gsub("[^[:alpha:] ]", "", deaccent(nombre_latin))))
)

#### 3. Ventanas de tiempo ####
# FINAL 25/26: últimas jornadas (que tengan alineaciones) de Prva y de Vtora.
ultimas_con_lineup <- function(ap, comp, temp, n = 3) {
  ap %>%
    filter(competicion == comp, temporada == temp) %>%
    distinct(jornada) %>% arrange(jornada) %>% tail(n) %>% pull(jornada)
}
prva26_final_js <- ultimas_con_lineup(ap, "Прва ЖФЛ", "25/26", 3)
cat("  Prva 25/26 últimas jornadas con alineación:", prva26_final_js, "\n")

# INICIO 26/27: primeras 2 jornadas de Prva.
prva27_inicio_js <- ap %>%
  filter(competicion == "Прва ЖФЛ", temporada == "26/27") %>%
  distinct(jornada) %>% arrange(jornada) %>% head(2) %>% pull(jornada)
cat("  Prva 26/27 primeras jornadas:", prva27_inicio_js, "\n")

# Prva: últimas 3 jornadas con alineación (= final real de la 25/26).
# Vtora: TODAS las jornadas con alineación (la 25/26 de Vtora terminó en j14-15
#        sin alineaciones en caché; el ascenso Konzuli->Pelister sale de aquí).
final25_prva <- ap %>% filter(temporada == "25/26", competicion == "Прва ЖФЛ", jornada %in% prva26_final_js)
final25_vtora <- ap %>% filter(temporada == "25/26", competicion == "Втора ЖФЛ")
final25 <- bind_rows(final25_prva, final25_vtora)

inicio27 <- ap %>%
  filter(competicion == "Прва ЖФЛ", temporada == "26/27", jornada %in% prva27_inicio_js)

#### 4. Continuidad de clubes ####
# Pelister Pro 2026 jugó en Vtora 25/26 (tras el Konzuli/baraj). Se busca su
# plantilla previa en cualquiera de sus denominaciones de la 25/26.
continuidad <- c(
  "ЖФК ПЕЛИСТЕР ПРО 2026 Битола" = "ЖФК КОНЗУЛИ ЈУНАЈТЕД"
)
plantilla_previa_club <- list()
clubes27 <- sort(unique(inicio27$equipo))
for (club in clubes27) {
  plantilla_previa_club[[club]] <- final25 %>% filter(equipo == club)
  if (nrow(plantilla_previa_club[[club]]) == 0 && club %in% names(continuidad)) {
    plantilla_previa_club[[club]] <- final25 %>% filter(equipo == unname(continuidad[[club]]))
  }
}

#### 5. ID de federación desde igracki.xlsx ####
cat("Cargando igracki.xlsx...\n")
bio <- readxl::read_excel("igracki.xlsx")
latin_to_cyrillic <- function(nombres) {
  md <- c("zh"="ж","sh"="ш","ch"="ч","nj"="њ","lj"="љ","kj"="ќ","gj"="ѓ","dz"="ѕ","dj"="ѓ")
  ms <- c("a"="а","b"="б","c"="ц","d"="д","e"="е","f"="ф","g"="г","h"="х","i"="и","j"="ј","k"="к","l"="л","m"="м","n"="н","o"="о","p"="п","r"="р","s"="с","t"="т","u"="у","v"="в","z"="з","w"="в","q"="к","x"="кс","y"="ј",
    "č"="ч","š"="ш","ž"="ж","đ"="ѓ","ć"="ќ","ç"="ч")
  sapply(nombres, function(nombre){ if(is.na(nombre)||nchar(trimws(nombre))==0) return(nombre)
    nl<-tolower(nombre); nc<-str_replace_all(str_replace_all(nl,md),ms)
    w<-str_split(nc," ")[[1]]; paste0(toupper(substr(w,1,1)),substr(w,2,nchar(w)),collapse=" ")},
    USE.NAMES=FALSE)
}
ig_full    <- trimws(paste(bio$`First Name`, bio$`Second Name`))
ig_rev     <- trimws(paste(bio$`Second Name`, bio$`First Name`))
ig_cyr      <- latin_to_cyrillic(ig_full)
ig_cyr_rev  <- latin_to_cyrillic(ig_rev)
ig_lat_plain <- stringr::str_to_lower(gsub("[^[:alpha:] ]","", deaccent(ig_full)))
ig_lat_rev   <- stringr::str_to_lower(gsub("[^[:alpha:] ]","", deaccent(ig_rev)))
ig_cyr_norm  <- stringr::str_to_lower(gsub("[^[:alpha:] ]","", ig_cyr))
ig_cyr_rev_norm <- stringr::str_to_lower(gsub("[^[:alpha:] ]","", ig_cyr_rev))

make_lookup <- function(keys, ids) {
  l <- setNames(ids, keys); l <- l[!is.na(names(l)) & names(l) != ""]
  l[!duplicated(names(l), fromLast = TRUE)]
}
lookup_lat      <- make_lookup(ig_lat_plain, bio$`FA ID Number`)
lookup_lat_rev  <- make_lookup(ig_lat_rev,   bio$`FA ID Number`)
lookup_cyr      <- make_lookup(ig_cyr_norm,  bio$`FA ID Number`)
lookup_cyr_rev  <- make_lookup(ig_cyr_rev_norm, bio$`FA ID Number`)

buscar_id <- function(clave_cyr, clave_lat) {
  if (!is.na(clave_cyr)) {
    v <- lookup_cyr[clave_cyr]; if (!is.na(v)) return(v)
    v <- lookup_cyr_rev[clave_cyr]; if (!is.na(v)) return(v)
  }
  if (!is.na(clave_lat)) {
    v <- lookup_lat[clave_lat]; if (!is.na(v)) return(v)
    v <- lookup_lat_rev[clave_lat]; if (!is.na(v)) return(v)
  }
  NA_character_
}

#### 6. Calcular altas (traspasos) por club de Prva 26/27 ####
# 'previo' = el último equipo donde la jugadora apareció en la 25/26 (Prva o
# Vtora). Para clubes continuistas (misma denominación) esa jugadora no es un
# traspaso y se omite. Para ascendidos (p.ej. Konzuli -> Pelister) declaramos
# el club previo de la 25/26 con su nombre de aquella temporada.
cat("Calculando traspasos...\n")

# Equipos previos por jugadora (última aparición de la 25/26 ordenada por fecha)
prev_eq_por_jugadora <- final25 %>%
  arrange(fecha) %>%
  group_by(nombre_clave) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(nombre_clave, previo = equipo, previo_competicion = competicion) %>%
  distinct(nombre_clave, .keep_all = TRUE)

altas_rows <- list()
for (club in clubes27) {
  # nombres previos del club en 25/26 (continuidad inclusive)
  club_prev_names <- unique(c(club, if (club %in% names(continuidad)) unname(continuidad[[club]]) else character(0)))

  # si la jugadora estuvo en este mismo club (u homónimo previo) en la 25/26,
  # no es un traspaso; la dejamos fuera salvo que sea un cambio de denominación
  # tipo Vtora -> Prva (ascenso) donde queremos registrarla igualmente con previo.
  nuevas <- inicio27 %>% filter(equipo == club) %>% distinct(nombre_clave, .keep_all = TRUE)
  for (i in seq_len(nrow(nuevas))) {
    clk <- nuevas$nombre_clave[i]
    clat <- nuevas$latin_key[i]

    prev <- prev_eq_por_jugadora %>% filter(nombre_clave == clk) %>% pull(previo)
    prev_comp <- prev_eq_por_jugadora %>% filter(nombre_clave == clk) %>% pull(previo_competicion)

    if (length(prev) == 0 || is.na(prev[1])) {
      # no jugó en 25/26 -> debut
      # (salvo que sea continuidad directa y no tenga aparición previa: déjalo)
      prev_display <- "Sin club en 25/26 (debut/nueva incorporación)"
    } else {
      # Traspaso real dentro de la misma liga (Prva<->Prva) o desde Vtora.
      # Se registra SIEMPRE que la jugadora NO fuera del propio club continuista
      # en Prva (misma división).
      prev_display <- paste(prev, collapse = " / ")
    }

    # Omitir jugadoras que ya estaban en este mismo club en la 25/26 en la misma
    # división (continuidad real de plantilla), salvo que vengan de la Vtora
    # (es decir, jugadoras del mismo club pero que el club cambió de division).
    estuvo_en_club <- any(club_prev_names %in% prev)
    vino_de_vtora <- length(prev) > 0 && any(!is.na(prev_comp) & prev_comp == "Втора ЖФЛ")

    # Regla: se considera alta/traspaso si la jugadora viene de OTRO club o del
    # mismo club pero desde Segunda (ascenso) o sin club.
    es_alta <- (!estuvo_en_club) || vino_de_vtora
    if (!es_alta) {
      # estuvo en el mismo club en Prva -> se queda, no es traspaso
      if (!vino_de_vtora) next
    }

    id <- buscar_id(clk, clat)
    altas_rows[[length(altas_rows) + 1]] <- data.frame(
      jugadora_cyr = nuevas$nombre[i],
      jugadora_lat = ifelse(is.na(nuevas$nombre_latin[i]), "", nuevas$nombre_latin[i]),
      equipo_previo = prev_display,
      equipo_nuevo  = club,
      id_federacion = ifelse(is.na(id), "", as.character(id)),
      stringsAsFactors = FALSE
    )
  }
}
altas <- bind_rows(altas_rows)

#### 7. Escribir .txt ####
cat("Generando traspass.txt...\n")
out <- c(
  "====================================================================",
  " TRASPASOS - FÚTBOL FEMENINO MACEDONIO",
  " Comparativa: FINAL temporada 25/26 (últimos partidos con alineación)",
  "   vs INICIO temporada 26/27 (primeras 2 jornadas de la Prva)",
  " Se incluyen jugadoras procedentes de la Vtora (Segunda) 25/26.",
  " ID = número de federación según igracki.xlsx (blanco si no consta).",
  "====================================================================",
  ""
)
for (club in clubes27) {
  d <- altas %>% filter(equipo_nuevo == club)
  out <- c(out, sprintf("####  %s  ####", club))
  if (nrow(d) == 0) {
    out <- c(out, "   (sin altas detectadas)", "")
    next
  }
  for (i in seq_len(nrow(d))) {
    nm <- if (nzchar(d$jugadora_lat[i])) paste0(d$jugadora_cyr[i], " (", d$jugadora_lat[i], ")")
          else d$jugadora_cyr[i]
    out <- c(out, sprintf("   - %-44s | previo: %-34s | ID: %s", nm, d$equipo_previo[i], d$id_federacion[i]))
  }
  out <- c(out, "")
}
writeLines(out, "traspass.txt", useBytes = TRUE)

#### 8. CSV para revisión ####
write.csv(altas, "traspass.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("Escrito traspass.txt (", nrow(altas), "filas). Cobertura de IDs: ",
    sum(nzchar(altas$id_federacion)), "/", nrow(altas), " (",
    round(100*sum(nzchar(altas$id_federacion))/nrow(altas)), "% ).\n")