# =============================================================================
# Enfermedades raras en México — Extensión: distribución geográfica
# Complementa enfermedades_raras_principal_unificado.R con:
#   A) Mapas de mortalidad por cuadrante (residencia del paciente)
#   B) Mapas de egresos por cuadrante (entidad de la unidad médica — NO
#      residencia; ver nota metodológica más abajo)
#   C) Índice de Moran global (mortalidad total y por cuadrante)
#   D) Clusters LISA (Moran local) para identificar zonas de agrupamiento
#      alto-alto / bajo-bajo estadísticamente significativas
#
# REQUISITO: correr este script EN LA MISMA SESIÓN, después de haber corrido
# completo enfermedades_raras_principal_unificado.R (reutiliza sus objetos: mx_states,
# gadm_bridge, pop_estado, estados_validos, defunciones_raras_cuadrantes,
# egresos_raras_cuadrantes, codigos_raras_cuadrantes, datos_path).
#
# NOTA (24-ago): enfermedades_raras_principal_unificado.R separó `egresos_raras` (catálogo
# completo, ya no tiene columna `cuadrante`) de `egresos_raras_cuadrantes`
# (solo códigos con cuadrante asignado en la matriz, con columna `cuadrante`),
# igual que ya existía para defunciones. Esta sección B usa la segunda.
#
# ATAJO: si ya corriste enfermedades_raras_principal_unificado.R al menos una vez y
# guardó su sesión (paso 15 de ese script), no hace falta volver a correrlo
# completo — basta con:
#   load(file.path(datos_path, "analisis", "sesion_base_raras.RData"))
# (ajusta datos_path a tu ruta si es una sesión nueva de R).
#
# NOTA METODOLÓGICA IMPORTANTE (egresos):
# La base "Sectorial" de egresos hospitalarios NO trae un campo de residencia
# habitual del paciente — solo CEDOCVE ("Estado de la Unidad Médica", según el
# diccionario de datos SAEH-DGIS), es decir, dónde está el hospital. Los mapas
# de egresos de esta sección muestran por tanto "lugar de atención", no
# "lugar de residencia". Para enfermedades raras esto tiende a sobrerrepresentar
# entidades con hospitales de referencia (CDMX, Jalisco, Nuevo León) frente a
# la distribución real de pacientes. Los mapas de mortalidad SÍ usan residencia
# (ENT_RESID, campo estándar de INEGI) y no tienen este problema.
# =============================================================================

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
if (!exists("defunciones_raras_cuadrantes") || !exists("mx_states")) load(.ruta_ses("sesion_base_raras.RData"))
if (!exists("defunciones_raras_cuadrantes_ampliado") && file.exists(.ruta_ses("sesion_ampliado.RData"))) load(.ruta_ses("sesion_ampliado.RData"))
datos_path <- .dp   # por si una sesion vieja guardo otra ruta
egresos_path <- file.path(datos_path, "egresos")

stopifnot(
  exists("defunciones_raras_cuadrantes"),
  exists("egresos_raras_cuadrantes"),
  exists("mx_states"),
  exists("gadm_bridge"),
  exists("pop_estado"),
  exists("estados_validos"),
  exists("codigos_raras_cuadrantes")
)

if (!requireNamespace("spdep", quietly = TRUE)) stop("Falta el paquete spdep: instalalo antes con install.packages(\"spdep\") (o corre 00_prerrequisitos.R).")
library(spdep)
library(sf)
library(dplyr)
library(data.table)
library(ggplot2)
library(openxlsx)

cuadrantes_validos <- sort(unique(codigos_raras_cuadrantes$cuadrante))

# -----------------------------------------------------------------------------
# A. Mortalidad por cuadrante y estado de residencia (acumulado 2014–2024)
# -----------------------------------------------------------------------------

grid_def <- CJ(ENT_RESID = estados_validos, cuadrante = cuadrantes_validos)

raras_estado_cuad <- defunciones_raras_cuadrantes[
  ENT_RESID %in% estados_validos,
  .(muertes = .N), by = .(ENT_RESID, cuadrante)
]

tasas_estado_cuad <- merge(grid_def, raras_estado_cuad, by = c("ENT_RESID", "cuadrante"), all.x = TRUE)
tasas_estado_cuad[is.na(muertes), muertes := 0L]
tasas_estado_cuad <- merge(tasas_estado_cuad, pop_estado, by = "ENT_RESID")
tasas_estado_cuad[, tasa := muertes / poblacion * 1e5]

mx_map_cuad <- merge(mx_states, gadm_bridge, by = "NAME_1") |>
  merge(tasas_estado_cuad, by = "ENT_RESID", allow.cartesian = TRUE) |> st_as_sf()

ggplot(mx_map_cuad) +
  geom_sf(aes(fill = tasa), color = "white", linewidth = 0.2) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "Tasa por\n100,000 hab.") +
  facet_wrap(~cuadrante) +
  labs(
    title    = "Tasa de mortalidad por enfermedades raras, por cuadrante",
    subtitle = "Por estado de residencia, México 2014–2024 (acumulado)"
  ) +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), strip.text = element_text(face = "bold"))

ggsave(file.path(datos_path, "analisis/mapa_mortalidad_cuadrante.jpeg"), width = 9, height = 7, dpi = 150)

# -----------------------------------------------------------------------------
# B. Egresos por cuadrante y entidad de la unidad médica (acumulado 2014–2024)
#    (lugar de atención, no residencia — ver nota al inicio del script)
# -----------------------------------------------------------------------------

grid_egr <- CJ(ENTIDAD = estados_validos, cuadrante = cuadrantes_validos)

raras_estado_egr_cuad <- egresos_raras_cuadrantes[
  ENTIDAD %in% estados_validos,
  .(egresos = .N), by = .(ENTIDAD, cuadrante)
]

pop_estado_egr <- pop_estado
names(pop_estado_egr)[1] <- "ENTIDAD"

tasas_estado_egr_cuad <- merge(grid_egr, raras_estado_egr_cuad, by = c("ENTIDAD", "cuadrante"), all.x = TRUE)
tasas_estado_egr_cuad[is.na(egresos), egresos := 0L]
tasas_estado_egr_cuad <- merge(tasas_estado_egr_cuad, pop_estado_egr, by = "ENTIDAD")
tasas_estado_egr_cuad[, tasa := egresos / poblacion * 1e5]

gadm_bridge_egr <- gadm_bridge
names(gadm_bridge_egr)[2] <- "ENTIDAD"

mx_map_egr_cuad <- merge(mx_states, gadm_bridge_egr, by = "NAME_1") |>
  merge(tasas_estado_egr_cuad, by = "ENTIDAD", allow.cartesian = TRUE) |> st_as_sf()

ggplot(mx_map_egr_cuad) +
  geom_sf(aes(fill = tasa), color = "white", linewidth = 0.2) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "Tasa por\n100,000 hab.") +
  facet_wrap(~cuadrante) +
  labs(
    title    = "Tasa de egresos por enfermedades raras, por cuadrante",
    subtitle = "Por entidad de la unidad médica (lugar de atención, NO residencia)\nMéxico 2014–2024 (acumulado, excluye defunciones intrahospitalarias)"
  ) +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), strip.text = element_text(face = "bold"))

ggsave(file.path(datos_path, "analisis/mapa_egresos_cuadrante.jpeg"), width = 9, height = 7, dpi = 150)

# -----------------------------------------------------------------------------
# C. Estructura de vecindad espacial (para Moran's I y LISA)
# -----------------------------------------------------------------------------

# El shapefile de GADM (gadm41_MEX_1.json) trae algunos polígonos con
# geometría inválida (vértices duplicados) que el motor esférico s2 de sf
# rechaza al calcular vecindad/contigüidad (error típico: "Loop N is not
# valid: Edge 0 is degenerate"). Se desactiva s2 (motor plano GEOS, suficiente
# para contigüidad a nivel estatal) y se reparan las geometrías antes de
# construir los vecinos.
sf::sf_use_s2(FALSE)

mx_states_sf <- merge(mx_states, gadm_bridge, by = "NAME_1") |>
  st_as_sf() |>
  st_make_valid() |>
  arrange(ENT_RESID)

nb <- poly2nb(mx_states_sf, queen = TRUE)

# Revisar si algún estado quedó sin vecinos (isla topológica en el shapefile).
n_sin_vecinos <- sum(card(nb) == 0)
if (n_sin_vecinos > 0) {
  message(n_sin_vecinos, " estado(s) sin vecinos por contigüidad. Considera knn2nb() ",
          "como alternativa si esto afecta el resultado.")
}

lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# -----------------------------------------------------------------------------
# D. Índice de Moran global — mortalidad total y por cuadrante
# -----------------------------------------------------------------------------

# tasas_estado viene del script principal (agregado, sin cuadrante)
tasa_mort_estado <- tasas_estado[match(mx_states_sf$ENT_RESID, ENT_RESID)]
stopifnot(!anyNA(tasa_mort_estado$tasa_raras))

moran_result <- moran.test(tasa_mort_estado$tasa_raras, lw, zero.policy = TRUE)
print(moran_result)

moran_por_cuadrante <- rbindlist(lapply(cuadrantes_validos, function(cdr) {
  df <- tasas_estado_cuad[cuadrante == cdr][match(mx_states_sf$ENT_RESID, ENT_RESID)]
  mt <- moran.test(df$tasa, lw, zero.policy = TRUE)
  data.table(
    cuadrante = cdr,
    moran_I   = unname(mt$estimate["Moran I statistic"]),
    p_value   = mt$p.value
  )
}))
print(moran_por_cuadrante)

# -----------------------------------------------------------------------------
# E. Clusters LISA (Moran local) — mortalidad total
# -----------------------------------------------------------------------------

local_moran <- localmoran(tasa_mort_estado$tasa_raras, lw, zero.policy = TRUE)
p_col <- grep("^Pr", colnames(local_moran), value = TRUE)[1]

mx_states_sf$tasa    <- tasa_mort_estado$tasa_raras
mx_states_sf$Ii      <- local_moran[, "Ii"]
mx_states_sf$p_value <- local_moran[, p_col]

tasa_z     <- as.numeric(scale(mx_states_sf$tasa))
lag_tasa_z <- lag.listw(lw, tasa_z, zero.policy = TRUE)
sig        <- mx_states_sf$p_value < 0.05

mx_states_sf$cluster <- "No significativo"
mx_states_sf$cluster[sig & tasa_z > 0 & lag_tasa_z > 0] <- "Alto-Alto"
mx_states_sf$cluster[sig & tasa_z < 0 & lag_tasa_z < 0] <- "Bajo-Bajo"
mx_states_sf$cluster[sig & tasa_z > 0 & lag_tasa_z < 0] <- "Alto-Bajo"
mx_states_sf$cluster[sig & tasa_z < 0 & lag_tasa_z > 0] <- "Bajo-Alto"

ggplot(mx_states_sf) +
  geom_sf(aes(fill = cluster), color = "white", linewidth = 0.3) +
  scale_fill_manual(values = c(
    "Alto-Alto"         = "#d73027",
    "Bajo-Bajo"         = "#4575b4",
    "Alto-Bajo"         = "#fc8d59",
    "Bajo-Alto"         = "#91bfdb",
    "No significativo"  = "grey90"
  ), name = "Cluster LISA (p<0.05)") +
  labs(
    title    = "Clusters espaciales (LISA) de mortalidad por enfermedades raras",
    subtitle = paste0(
      "Índice de Moran global = ", round(unname(moran_result$estimate["Moran I statistic"]), 3),
      "  (p = ", format.pval(moran_result$p.value, digits = 2), ")\n",
      "Por estado de residencia, México 2014–2024 (acumulado)"
    )
  ) +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(datos_path, "analisis/mapa_lisa_mortalidad.jpeg"), width = 8, height = 6, dpi = 150)

# -----------------------------------------------------------------------------
# F. Exportar resultados numéricos
# -----------------------------------------------------------------------------

wb <- createWorkbook()

addWorksheet(wb, "tasas_cuadrante_def")
writeData(wb, "tasas_cuadrante_def", tasas_estado_cuad)

addWorksheet(wb, "tasas_cuadrante_egr")
writeData(wb, "tasas_cuadrante_egr", tasas_estado_egr_cuad)

addWorksheet(wb, "moran_global")
writeData(wb, "moran_global", data.table(
  medida  = "mortalidad_total",
  moran_I = unname(moran_result$estimate["Moran I statistic"]),
  p_value = moran_result$p.value
))

addWorksheet(wb, "moran_por_cuadrante")
writeData(wb, "moran_por_cuadrante", moran_por_cuadrante)

addWorksheet(wb, "lisa_clusters")
writeData(wb, "lisa_clusters",
          st_drop_geometry(mx_states_sf[, c("ENT_RESID", "tasa", "Ii", "p_value", "cluster")]))

saveWorkbook(wb, file.path(datos_path, "analisis_geografico_cuadrante_moran.xlsx"), overwrite = TRUE)

cat("\nListo. Nuevos archivos en Datos/analisis/:\n",
    "  - mapa_mortalidad_cuadrante.jpeg\n",
    "  - mapa_egresos_cuadrante.jpeg\n",
    "  - mapa_lisa_mortalidad.jpeg\n",
    "Y en Datos/: analisis_geografico_cuadrante_moran.xlsx\n")

# -----------------------------------------------------------------------------
# G. Guardar sesión — para no tener que volver a correr este script cada vez
#    que se quiera correr analisis_geografico_extendido_parte2.R o
#    analisis_geografico_ssa_residencia.R
# -----------------------------------------------------------------------------

objetos_para_guardar_geo <- c(
  "mx_states_sf", "nb", "lw", "cuadrantes_validos",
  "tasas_estado_cuad", "tasas_estado_egr_cuad",
  "moran_result", "moran_por_cuadrante"
)

save(list = objetos_para_guardar_geo,
     file = file.path(datos_path, "analisis", "sesion_geografico_extendido.RData"))

cat("\nSesión geográfica guardada en Datos/analisis/sesion_geografico_extendido.RData\n",
    "En la próxima corrida, en vez de correr este script de nuevo, basta con:\n",
    "  load(file.path(datos_path, \"analisis\", \"sesion_base_raras.RData\"))\n",
    "  load(file.path(datos_path, \"analisis\", \"sesion_geografico_extendido.RData\"))\n",
    "y luego correr directamente parte2 o el script de residencia SSA.\n")

# -----------------------------------------------------------------------------
# H. AMPLIADO (adicional) — Moran's I para cuadrantes 2, 5 y 9 con el catálogo
#    ampliado (corrección de los 5 códigos "pelón" de 3 caracteres: Fibrosis
#    Quística, Anemia Falciforme, Hemofilia A, Hemofilia B, Huntington). NO
#    sustituye ni modifica ningún objeto de las secciones A-G (mx_states_sf,
#    nb, lw, moran_result, moran_por_cuadrante, etc. quedan intactos).
#
#    Solo se recalculan cuadrantes 2, 5 y 9: son los únicos que pueden diferir
#    del original (1,3,4,6,7,8 no tienen ningún código "pelón" de alta
#    confianza, así que son idénticos por construcción — recalcularlos sería
#    trabajo redundante, sin ningún cambio posible en el resultado).
#
#    REQUISITO: correr esto en la misma sesión que ya corrió las secciones A-G
#    de este script (reutiliza mx_states_sf, lw, pop_estado, estados_validos,
#    moran_por_cuadrante), y que además tenga cargados los objetos "_ampliado"
#    generados por enfermedades_raras_principal_unificado.R sección 3bis/15bis — ya sea
#    porque se corrió ese script completo en esta sesión, o cargando:
#      load(file.path(datos_path, "analisis", "sesion_ampliado.RData"))
# -----------------------------------------------------------------------------

stopifnot(exists("defunciones_raras_cuadrantes_ampliado"), exists("moran_por_cuadrante"))

cuadrantes_afectados_ampliado <- c(2, 5, 9)

grid_def_ampliado <- CJ(ENT_RESID = estados_validos, cuadrante = cuadrantes_afectados_ampliado)

raras_estado_cuad_ampliado <- defunciones_raras_cuadrantes_ampliado[
  ENT_RESID %in% estados_validos & cuadrante %in% cuadrantes_afectados_ampliado,
  .(muertes = .N), by = .(ENT_RESID, cuadrante)
]

tasas_estado_cuad_ampliado <- merge(grid_def_ampliado, raras_estado_cuad_ampliado,
                                    by = c("ENT_RESID", "cuadrante"), all.x = TRUE)
tasas_estado_cuad_ampliado[is.na(muertes), muertes := 0L]
tasas_estado_cuad_ampliado <- merge(tasas_estado_cuad_ampliado, pop_estado, by = "ENT_RESID")
tasas_estado_cuad_ampliado[, tasa := muertes / poblacion * 1e5]

moran_por_cuadrante_ampliado <- rbindlist(lapply(cuadrantes_afectados_ampliado, function(cdr) {
  df <- tasas_estado_cuad_ampliado[cuadrante == cdr][match(mx_states_sf$ENT_RESID, ENT_RESID)]
  mt <- moran.test(df$tasa, lw, zero.policy = TRUE)
  data.table(
    cuadrante = cdr,
    moran_I   = unname(mt$estimate["Moran I statistic"]),
    p_value   = mt$p.value
  )
}))

# --- Contraste directo original vs. ampliado (cambio en I y en significancia) ---
comparacion_moran_ampliado <- merge(
  moran_por_cuadrante[cuadrante %in% cuadrantes_afectados_ampliado],
  moran_por_cuadrante_ampliado,
  by = "cuadrante", suffixes = c("_original", "_ampliado")
)
comparacion_moran_ampliado[, `:=`(
  diferencia_I         = moran_I_ampliado - moran_I_original,
  cambia_significancia = (p_value_original < 0.05) != (p_value_ampliado < 0.05)
)]
cat("\n--- AMPLIADO: Moran's I original vs. ampliado (cuadrantes 2/5/9) ---\n")
print(comparacion_moran_ampliado)

# --- Exportar ---
wb_geo_ampliado <- createWorkbook()

addWorksheet(wb_geo_ampliado, "tasas_cuadrante_def_ampliado")
writeData(wb_geo_ampliado, "tasas_cuadrante_def_ampliado", tasas_estado_cuad_ampliado)

addWorksheet(wb_geo_ampliado, "moran_comparacion")
writeData(wb_geo_ampliado, "moran_comparacion", comparacion_moran_ampliado)

saveWorkbook(wb_geo_ampliado, file.path(datos_path, "analisis_geografico_ampliado_moran.xlsx"), overwrite = TRUE)

cat("\nListo. Comparación de Moran's I (original vs. ampliado, cuadrantes 2/5/9) en:\n",
    "  Datos/analisis_geografico_ampliado_moran.xlsx\n")