# =============================================================================
# Enfermedades raras en México - Egresos hospitalarios SSA/IMSS-Bienestar
# Sub-análisis geográfico usando ENTIDAD DE RESIDENCIA HABITUAL del paciente
# =============================================================================
#
# QUÉ ES ESTE SCRIPT
# ------------------
# La base "Sectorial" de egresos (ya usada en enfermedades_raras_principal_unificado.R y
# en analisis geografico adicional.R) NO trae residencia del paciente: su
# variable ENTIDAD es en realidad CEDOCVE = entidad de la UNIDAD MÉDICA
# (lugar de atención), no de residencia.
#
# La base separada que solo cubre Secretaría de Salud / IMSS-Bienestar
# ("ssa_egresos_AAAA.zip", carpeta Datos/egresos) SÍ trae la residencia
# habitual del paciente, confirmado en el descriptor oficial
# ("ssa_Descriptores_Bases_de_Datos_EGRESOS_2022 (1).xlsx", hoja "Matriz SAEH"):
#
#     ENTIDAD  ->  "Entidad de residencia habitual"      (catálogo CATENTIDADRES)
#     MUNIC    ->  "Municipio de residencia habitual"    (catálogo CATMPORES)
#     LOC      ->  "Localidad de residencia habitual"    (catálogo CATLOCALIDADRES)
#
# Además, se verificó directamente en los datos que esta base es UNA SOLA red
# institucional a lo largo de todo el periodo: en 2014-2023 el CLUES de cada
# registro trae el código de institución "SSA"; a partir de 2024 el mismo tipo
# de unidades aparece recodificado como "IMB" (IMSS-Bienestar), por la reforma
# INSABI -> IMSS-Bienestar. Es decir, esta base SÍ es continua 2014-2025 (no
# tiene la caída de 2024 que sí aparece en la base Sectorial, donde "SSA" e
# "IMB" se cuentan como instituciones separadas). Esto confirma la hipótesis:
# esta base SÍ trae "el total sin la caída", al menos para la red de unidades
# de Secretaría de Salud / IMSS-Bienestar.
#
# LIMITACIÓN A TENER PRESENTE: esta base cubre SOLO la red SSA/IMSS-Bienestar,
# que en la base Sectorial representa entre ~42% y ~52% del total de egresos
# 2014-2023 (ver tabla ya entregada). Por lo tanto este sub-análisis describe
# la distribución geográfica de la atención en ESA red, no del sistema
# completo (IMSS régimen ordinario, ISSSTE, PEMEX, privados, etc. no están
# aquí). Pendiente: calcular ese mismo porcentaje pero solo dentro del
# subconjunto de egresos por enfermedades raras (no población general) para
# saber si esta limitación es mayor o menor específicamente en raras.
#
# NOTA SOBRE CALIDAD DE DATOS DEL CATÁLOGO (ya resuelta)
# --------------------------------------------------------
# Se encontró que 22 de 1,738 códigos en cie_que_son_raras$causa traían un
# daga(+)/asterisco(*)/coma sobrante al final (ej. "M310+", "A543+", "A180,"),
# lo que rompía el match exacto contra AFECPRIN4/CAUSA_DEF. Esto YA SE
# CORRIGIÓ: el catálogo ahora vive en "cie10 raras_corregido.xlsx" (ya no en
# el .dta original), sin esos símbolos. La limpieza de la sección 1 de este
# script es por tanto defensiva/no-op con el catálogo actual - no hace daño
# dejarla, por si en algún momento se vuelve a una copia sin corregir.
#
# También se encontró que, de los códigos limpios, 89 son de solo 3 caracteres
# (nivel de categoría CIE-10, ej. "I10", "E10", "C53"). Se probó una
# corrección por prefijo (tratar esos 3 caracteres como toda la categoría) y
# SE DESCARTÓ: infla los conteos con enfermedades comunes, no raras (I10 =
# hipertensión esencial, E10 = diabetes tipo 1, C53/C56/C61 = cánceres
# comunes de cérvix/ovario/próstata, entre otros). Este script, correctamente,
# NO expande esos 89 códigos por prefijo - los deja sin match, igual que el
# comportamiento del script principal.
#
# REQUISITOS ANTES DE CORRER ESTE SCRIPT (correr en este orden, misma sesión R)
#   1. enfermedades_raras_principal_unificado.R          (crea cie_que_son_raras,
#                                               codigos_raras_cuadrantes (ya limpio,
#                                               ya con la exclusión de baja
#                                               especificidad aplicada),
#                                               pop_estado, estados_validos,
#                                               gadm_bridge, mx_states)
#   2. analisis geografico adicional.R        (crea mx_states_sf, nb, lw)
#
# ATAJO: si ya corrieron los dos scripts anteriores al menos una vez (y
# guardaron su sesión), no hace falta volver a correrlos - basta con, en una
# sesión nueva de R:
#   (la ruta sale de config_rutas.R; con setwd() a la carpeta del paquete basta)
#   load(file.path(datos_path, "analisis", "sesion_base_raras.RData"))
#   load(file.path(datos_path, "analisis", "sesion_geografico_extendido.RData"))
#   library(data.table); library(dplyr); library(sf); library(spdep); library(ggplot2); library(openxlsx)
#
# CONSUMO DE RECURSOS - AVISO
# ----------------------------
# Este script lee 12 archivos de texto (2014-2025), de entre ~350 y ~560 MB
# SIN comprimir cada uno (varios millones de filas). Para mantenerlo ligero:
#   - Se extrae un solo archivo a la vez a un directorio temporal (nunca los
#     12 a la vez).
#   - fread() solo selecciona ~7 columnas de las ~45-50 que trae cada año
#     (no se cargan todas).
#   - Se libera memoria (rm + gc) después de cada año.
# Aun así, calculen entre 10 y 25 minutos de cómputo total y un pico de RAM
# de aprox. 2-3 GB. Si prefieren una versión más ligera (por ejemplo, solo
# 2019-2024, o usando data.table::fread(cmd=) sin descomprimir a disco),
# avísenme y lo ajustamos antes de que corran esto.
#
# =============================================================================

library(data.table)
library(dplyr)
library(sf)
library(spdep)
library(ggplot2)
library(openxlsx)

# Ruta de datos (menor 30, 19-oct-2026): se define UNA sola vez en config_rutas.R, en la
# carpeta del paquete. Si datos_path ya existe en la sesion se respeta; si no, se lee del
# archivo de configuracion (hay que haber hecho setwd() a la carpeta del paquete).
if (!exists("datos_path", inherits = TRUE)) {
  if (file.exists("config_rutas.R")) {
    source("config_rutas.R", encoding = "UTF-8")
  } else {
    # Sin setwd() al paquete: usar ER_DATOS o la ruta del equipo de corrida (E:/...). Para otra
    # maquina, ajustar config_rutas.R (y hacer setwd() a la carpeta del paquete).
    datos_path <- Sys.getenv("ER_DATOS", unset = "E:/jp/enfermedades raras/Datos")
    message("AVISO: no encontre config_rutas.R en ", getwd(), "; uso datos_path = ", datos_path)
  }
}
.ruta_ses <- function(x) file.path(datos_path, "analisis", x)
.dp <- datos_path
if (!exists("mx_states")) load(.ruta_ses("sesion_base_raras.RData"))
if (!exists("mx_states_sf")) load(.ruta_ses("sesion_geografico_extendido.RData"))
datos_path <- .dp   # por si una sesion vieja guardo otra ruta
egresos_path <- file.path(datos_path, "egresos")

stopifnot(
  exists("cie_que_son_raras"), exists("codigos_raras_cuadrantes"),
  exists("pop_estado"), exists("estados_validos"),
  exists("gadm_bridge"), exists("mx_states"),
  exists("mx_states_sf"), exists("lw")
)

egresos_path <- file.path(datos_path, "egresos")   # ya definido en el script maestro
tmp_ssa <- tempdir()

# -----------------------------------------------------------------------------
# 1. Catálogo de códigos "raras" - LIMPIO (corrige daga/asterisco/coma sobrante;
#    ya NO se hace match por prefijo para los códigos de 3 caracteres - ver
#    corrección 24-ago abajo)
# -----------------------------------------------------------------------------

raw_codes   <- cie_que_son_raras$causa
clean_codes <- toupper(trimws(gsub("[+*,]", "", raw_codes)))
clean_codes <- unique(clean_codes[clean_codes != "" & !is.na(clean_codes)])

codigos_raras_4 <- clean_codes[nchar(clean_codes) == 4]
codigos_raras_3 <- clean_codes[nchar(clean_codes) == 3]  # se conservan solo para el cat() informativo

cat("Códigos originales:         ", length(raw_codes), "\n")
cat("Códigos únicos limpios:     ", length(clean_codes), "\n")
cat("  - de 4 caracteres (match exacto):  ", length(codigos_raras_4), "\n")
cat("  - de 3 caracteres (NO se usan - ver nota de calidad de datos arriba): ", length(codigos_raras_3), "\n\n")

# CORRECCIÓN 24-ago: esta función SÍ hacía match por prefijo
# (substr(codigo,1,3) %in% codigos_raras_3), lo mismo que se probó y se
# descartó en la base Sectorial (infló el conteo de "raras" en 174,022 casos
# solo en 2014, dominado por enfermedades comunes: I10 hipertensión, E10
# diabetes tipo 1, C53/C56/C61 cánceres de cérvix/ovario/próstata, K72, D24).
# Se corrige para usar SOLO match exacto de 4 caracteres, igual que
# `codigos_raras`/`egresos_raras` en enfermedades_raras_principal_unificado.R - los 89
# códigos de 3 caracteres quedan sin match, sin expandirse a su categoría.
match_raras <- function(codigo) {
  codigo <- toupper(trimws(codigo))
  codigo %in% codigos_raras_4
}

# CORRECCIÓN 24-ago: en vez de reconstruir la matriz de cuadrantes aquí (con
# una limpieza ligeramente distinta a la del script maestro - y sin la
# exclusión de códigos de baja especificidad K746/C509/C349 agregada ahí),
# se reutiliza directamente `codigos_raras_cuadrantes` tal como lo deja
# enfermedades_raras_principal_unificado.R (ya limpio, ya sin match por prefijo, y ya
# con la exclusión aplicada). Esto viene cargado desde sesion_base_raras.RData
# - no hace falta reconstruirlo, y evita que las dos versiones se desincronicen
# si en el futuro se ajusta la limpieza o la exclusión en el script maestro.
matriz_clean <- setDT(copy(codigos_raras_cuadrantes))
matriz_clean <- matriz_clean[!duplicated(matriz_clean$cie10)]

asignar_cuadrante <- function(afecprin) {
  afecprin <- toupper(trimws(afecprin))
  matriz_clean[match(afecprin, cie10), .(cuadrante, nombre_es)]
}

# -----------------------------------------------------------------------------
# 2. Mapa de archivos dentro de cada zip (el nombre y formato cambian por año)
# -----------------------------------------------------------------------------

year_meta_ssa <- list(
  list(yr = 2014, zip = "ssa_egresos_2014.zip", file = "EGRESO.csv"),
  list(yr = 2015, zip = "ssa_egresos_2015.zip", file = "SAEH_2015/EGRESO_2015.csv"),
  list(yr = 2016, zip = "ssa_egresos_2016.zip", file = "SAEH_2016/EGRESOS_2016.csv"),
  list(yr = 2017, zip = "ssa_egresos_2017.zip", file = "EGRESOS_2017.csv"),
  list(yr = 2018, zip = "ssa_egresos_2018.zip", file = "EGRESO_2018.csv"),
  list(yr = 2019, zip = "ssa_egresos_2019.zip", file = "Egresos2019/EGRESOS_2019.txt"),
  list(yr = 2020, zip = "ssa_egresos_2020.zip", file = "EGRESOS.txt"),
  list(yr = 2021, zip = "ssa_egresos_2021.zip", file = "EGRESOS.txt"),
  list(yr = 2022, zip = "ssa_egresos_2022.zip", file = "EGRESOS.txt"),
  list(yr = 2023, zip = "ssa_egresos_2023.zip", file = "Registros/Egresos.txt"),
  list(yr = 2024, zip = "ssa_egresos_2024.zip", file = "Registros/Egresos.txt"),
  list(yr = 2025, zip = "ssa_egresos_2025.zip", file = "Registros/Egresos.txt")
)

# Para comparar 2014-2024 con el resto del análisis (defunciones llega solo a
# 2024), por default se usan los primeros 11; cambien a year_meta_ssa completo
# si quieren incluir 2025.
years_a_usar <- year_meta_ssa[1:11]

# -----------------------------------------------------------------------------
# 3. Cargar un año de la base SSA/IMSS-Bienestar (residencia)
# -----------------------------------------------------------------------------

load_ssa_egreso <- function(m) {
  fp <- file.path(tmp_ssa, basename(m$file))
  if (!file.exists(fp)) {
    unzip(file.path(egresos_path, m$zip), files = m$file, exdir = tmp_ssa, junkpaths = TRUE)
  }
  
  # fread detecta el separador (coma o "|") automáticamente
  hdr    <- names(fread(fp, nrows = 0))
  hdr_up <- toupper(hdr)
  
  necesarias <- c("CLUES", "ENTIDAD", "MUNIC", "AFECPRIN", "SEXO", "DIAS_ESTA", "MOTEGRE")
  idx <- match(necesarias, hdr_up)
  if (any(is.na(idx))) {
    stop(m$yr, ": faltan columnas -> ", paste(necesarias[is.na(idx)], collapse = ", "))
  }
  sel <- hdr[idx]
  
  # CORRECCION 14-oct-2026: una parada anticipada de fread (campos de mas) ahora es error.
  dt <- withCallingHandlers(
    fread(fp, select = sel, fill = Inf, colClasses = "character",
          showProgress = FALSE, encoding = "UTF-8"),
    warning = function(w) {
      if (grepl("Stopped early|Se detuvo anticipadamente", conditionMessage(w)))
        stop("fread se detuvo antes de terminar ", m$file, ": ", conditionMessage(w), call. = FALSE)
    })
  setnames(dt, sel, necesarias)
  dt[, ANIO := m$yr]
  
  dt <- dt[MOTEGRE != "5"]                                   # excluir defunción intrahospitalaria
  dt[, ENT_RESID := sprintf("%02d", suppressWarnings(as.integer(ENTIDAD)))]
  dt[, INSTITUCION := substr(CLUES, 3, 5)]                   # para verificar continuidad SSA/IMB
  
  message(m$yr, ": ", format(nrow(dt), big.mark = ","), " filas tras filtro (vivos)")
  
  file.remove(fp)                                            # liberar disco temporal
  gc()
  dt
}

egresos_ssa_list <- lapply(years_a_usar, load_ssa_egreso)
egresos_ssa       <- rbindlist(egresos_ssa_list, fill = TRUE, use.names = TRUE)
rm(egresos_ssa_list); gc()

# -----------------------------------------------------------------------------
# 4. Verificación de continuidad institucional (SSA -> IMSS-Bienestar)
# -----------------------------------------------------------------------------

cat("=== Distribución institucional por año (verificar que no hay caída en 2024) ===\n")
print(egresos_ssa[, .N, by = .(ANIO, INSTITUCION)][order(ANIO, -N)])

# -----------------------------------------------------------------------------
# 5. Filtrar por enfermedades raras (match exacto 4 + prefijo 3, ver sección 1)
# -----------------------------------------------------------------------------

egresos_ssa[, es_rara := match_raras(AFECPRIN)]
egresos_ssa_raras <- egresos_ssa[es_rara == TRUE]

cat("Egresos SSA/IMSS-Bienestar (vivos), 2014-2024: ", format(nrow(egresos_ssa), big.mark = ","), "\n")
cat("De los cuales, por enfermedades raras:         ", format(nrow(egresos_ssa_raras), big.mark = ","), "\n")
cat("Porcentaje:                                    ", round(nrow(egresos_ssa_raras) / nrow(egresos_ssa) * 100, 2), "%\n\n")

egresos_ssa_raras[, .N, by = ANIO][order(ANIO)]

# -----------------------------------------------------------------------------
# 6. Tasas por entidad de RESIDENCIA (ya no "lugar de atención")
# -----------------------------------------------------------------------------

raras_estado_ssa <- egresos_ssa_raras[ENT_RESID %in% estados_validos,
                                      .(egresos_raras = .N), by = ENT_RESID]
total_estado_ssa <- egresos_ssa[ENT_RESID %in% estados_validos,
                                .(total_egresos = .N), by = ENT_RESID]

tasas_estado_egr_ssa <- Reduce(merge, list(raras_estado_ssa, total_estado_ssa, pop_estado)) |> setDT()
tasas_estado_egr_ssa[, `:=`(
  tasa_raras = egresos_raras / poblacion * 1e5,
  pct_raras  = egresos_raras / total_egresos * 100
)]
setorder(tasas_estado_egr_ssa, -tasa_raras)

mx_map_ssa <- merge(mx_states, gadm_bridge, by = "NAME_1") |>
  merge(tasas_estado_egr_ssa, by = "ENT_RESID") |> st_as_sf()

ggplot(mx_map_ssa) +
  geom_sf(aes(fill = tasa_raras), color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "plasma", name = "Egresos por\n100,000 hab.") +
  labs(
    title    = "Egresos hospitalarios por enfermedades raras, por entidad de RESIDENCIA",
    subtitle = "Red Secretaría de Salud / IMSS-Bienestar, 2014-2024",
    caption  = "Fuente: base SSA/IMSS-Bienestar de egresos hospitalarios (ENTIDAD = residencia habitual)."
  ) +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(datos_path, "analisis", "mapa_egresos_ssa_residencia.jpeg"),
       width = 8, height = 6, dpi = 300)

# -----------------------------------------------------------------------------
# 7. Disgregación por cuadrante (usa el catálogo limpio de la sección 1)
# -----------------------------------------------------------------------------

cuad_asignado <- asignar_cuadrante(egresos_ssa_raras$AFECPRIN)
egresos_ssa_raras[, `:=`(cuadrante = cuad_asignado$cuadrante, nombre_es = cuad_asignado$nombre_es)]

egresos_ssa_raras_cuad <- egresos_ssa_raras[!is.na(cuadrante)]

cat("Egresos raras con cuadrante asignado: ",
    format(nrow(egresos_ssa_raras_cuad), big.mark = ","),
    " de ", format(nrow(egresos_ssa_raras), big.mark = ","), "\n\n")

# Tasas estado x cuadrante (rellenando combinaciones en 0 con CJ)
conteo_estado_cuad_ssa <- egresos_ssa_raras_cuad[ENT_RESID %in% estados_validos,
                                                 .(egresos = .N), by = .(ENT_RESID, cuadrante)]
combos_ssa <- CJ(ENT_RESID = estados_validos,
                 cuadrante = sort(unique(egresos_ssa_raras_cuad$cuadrante)))
tasas_estado_egr_ssa_cuad <- merge(combos_ssa, conteo_estado_cuad_ssa,
                                   by = c("ENT_RESID", "cuadrante"), all.x = TRUE)
tasas_estado_egr_ssa_cuad[is.na(egresos), egresos := 0]
tasas_estado_egr_ssa_cuad <- merge(tasas_estado_egr_ssa_cuad, pop_estado, by = "ENT_RESID")
tasas_estado_egr_ssa_cuad[, tasa_raras := egresos / poblacion * 1e5]

# -----------------------------------------------------------------------------
# 8. Índice de Moran global (mismo lw que en analisis geografico adicional.R)
# -----------------------------------------------------------------------------

vec_tasa_ssa <- tasas_estado_egr_ssa$tasa_raras[match(mx_states_sf$ENT_RESID, tasas_estado_egr_ssa$ENT_RESID)]

moran_global_ssa <- moran.test(vec_tasa_ssa, lw, zero.policy = TRUE)
print(moran_global_ssa)

# -----------------------------------------------------------------------------
# 9. LISA (clusters locales) para la tasa total, por RESIDENCIA
# -----------------------------------------------------------------------------

lisa_ssa <- localmoran(vec_tasa_ssa, lw, zero.policy = TRUE)

z_tasa_ssa <- scale(vec_tasa_ssa)[, 1]
z_lag_ssa  <- lag.listw(lw, z_tasa_ssa, zero.policy = TRUE)

cluster_ssa <- rep("No significativo", length(z_tasa_ssa))
sig_ssa <- lisa_ssa[, "Pr(z != E(Ii))"] < 0.05
cluster_ssa[sig_ssa & z_tasa_ssa > 0 & z_lag_ssa > 0] <- "Alto-Alto"
cluster_ssa[sig_ssa & z_tasa_ssa < 0 & z_lag_ssa < 0] <- "Bajo-Bajo"
cluster_ssa[sig_ssa & z_tasa_ssa > 0 & z_lag_ssa < 0] <- "Alto-Bajo"
cluster_ssa[sig_ssa & z_tasa_ssa < 0 & z_lag_ssa > 0] <- "Bajo-Alto"

lisa_clusters_ssa <- data.table(
  ENT_RESID = mx_states_sf$ENT_RESID,
  NAME_1    = mx_states_sf$NAME_1,
  tasa_raras = vec_tasa_ssa,
  Ii        = lisa_ssa[, "Ii"],
  p_valor   = lisa_ssa[, "Pr(z != E(Ii))"],
  cluster   = cluster_ssa
)
setorder(lisa_clusters_ssa, -tasa_raras)
print(lisa_clusters_ssa[cluster != "No significativo"])

mx_lisa_ssa <- mx_states_sf
mx_lisa_ssa$cluster <- cluster_ssa

ggplot(mx_lisa_ssa) +
  geom_sf(aes(fill = cluster), color = "white", linewidth = 0.3) +
  scale_fill_manual(values = c(
    "Alto-Alto" = "#d7191c", "Bajo-Bajo" = "#2c7bb6",
    "Alto-Bajo" = "#fdae61", "Bajo-Alto" = "#abd9e9",
    "No significativo" = "grey85"
  ), name = "Cluster LISA") +
  labs(
    title    = "Clusters espaciales (LISA) - Egresos por enfermedades raras",
    subtitle = "Por entidad de RESIDENCIA - red SSA/IMSS-Bienestar, 2014-2024"
  ) +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(datos_path, "analisis", "mapa_lisa_egresos_ssa_residencia.jpeg"),
       width = 8, height = 6, dpi = 300)

# -----------------------------------------------------------------------------
# 10. Moran's I por cuadrante (residencia)
# -----------------------------------------------------------------------------

moran_por_cuadrante_ssa <- rbindlist(lapply(sort(unique(tasas_estado_egr_ssa_cuad$cuadrante)), function(cdr) {
  sub <- tasas_estado_egr_ssa_cuad[cuadrante == cdr]
  vec <- sub$tasa_raras[match(mx_states_sf$ENT_RESID, sub$ENT_RESID)]
  vec[is.na(vec)] <- 0
  if (sd(vec) == 0) {
    return(data.table(cuadrante = cdr, moran_I = NA, p_valor = NA, nota = "sin variación (todo ceros o constante)"))
  }
  mt <- moran.test(vec, lw, zero.policy = TRUE)
  data.table(cuadrante = cdr, moran_I = mt$estimate[["Moran I statistic"]],
             p_valor = mt$p.value, nota = NA_character_)
}))
print(moran_por_cuadrante_ssa)

# -----------------------------------------------------------------------------
# 11. Comparación de cobertura institucional (para contexto / decisión)
# -----------------------------------------------------------------------------

# % que representa esta red (SSA/IMSS-Bienestar) dentro del total Sectorial,
# YA CALCULADO previamente para población general (no específico de raras):
#   2014 46.3% | 2015 47.2% | 2016 52.3% | 2017 47.4% | 2018 46.6% | 2019 45.5%
#   2020 44.6% | 2021 43.5% | 2022 41.8% | 2023 42.2% | 2024 13.8%* (*aparente
#   caída en Sectorial por reclasificación institucional, ver nota arriba)
# Pendiente: repetir este mismo cálculo pero solo sobre egresos_raras de la
# base Sectorial (no sobre el total), para saber si la proporción SSA/IMB es
# distinta específicamente en enfermedades raras. Lo dejamos pendiente de
# confirmar si aún lo quieren dado que ahora ya tenemos esta base con
# residencia como alternativa más completa.

# -----------------------------------------------------------------------------
# 12. Exportar resultados
# -----------------------------------------------------------------------------

wb <- createWorkbook()

addWorksheet(wb, "tasas_estado_egr_ssa")
writeData(wb, "tasas_estado_egr_ssa", tasas_estado_egr_ssa)

addWorksheet(wb, "tasas_estado_cuad_ssa")
writeData(wb, "tasas_estado_cuad_ssa", tasas_estado_egr_ssa_cuad)

addWorksheet(wb, "moran_global_ssa")
writeData(wb, "moran_global_ssa", data.frame(
  moran_I = moran_global_ssa$estimate[["Moran I statistic"]],
  p_valor = moran_global_ssa$p.value
))

addWorksheet(wb, "moran_por_cuadrante_ssa")
writeData(wb, "moran_por_cuadrante_ssa", moran_por_cuadrante_ssa)

addWorksheet(wb, "lisa_clusters_ssa")
writeData(wb, "lisa_clusters_ssa", lisa_clusters_ssa)

addWorksheet(wb, "cobertura_institucional")
writeData(wb, "cobertura_institucional", egresos_ssa[, .N, by = .(ANIO, INSTITUCION)][order(ANIO, -N)])

saveWorkbook(wb, file.path(datos_path, "analisis", "analisis_geografico_ssa_residencia.xlsx"), overwrite = TRUE)

cat("\nListo. Archivos generados en Datos/analisis/:\n")
cat(" - mapa_egresos_ssa_residencia.jpeg\n")
cat(" - mapa_lisa_egresos_ssa_residencia.jpeg\n")
cat(" - analisis_geografico_ssa_residencia.xlsx\n")