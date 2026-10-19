# =============================================================================
# Enfermedades raras en México - Extensión parte 2
#   G) LISA para los cuadrantes con Moran's I global significativo en
#      mortalidad (cuadrante 3 y cuadrante 9)
#   H) Egresos: mismo tratamiento espacial que mortalidad - Moran's I global,
#      LISA global, y Moran's I por cuadrante (lugar de atención, recordatorio:
#      NO es residencia del paciente)
#
# REQUISITO: correr EN LA MISMA SESIÓN, después de analisis geografico adicional.R
# (que a su vez corre después de enfermedades_raras_principal_unificado.R). Reutiliza:
# mx_states_sf, lw, cuadrantes_validos, tasas_estado_cuad, tasas_estado_egr_cuad,
# tasas_estado_egr, datos_path.
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
if (!exists("mx_states")) load(.ruta_ses("sesion_base_raras.RData"))
if (!exists("mx_states_sf")) load(.ruta_ses("sesion_geografico_extendido.RData"))
datos_path <- .dp   # por si una sesion vieja guardo otra ruta
egresos_path <- file.path(datos_path, "egresos")

stopifnot(
  exists("mx_states_sf"), exists("lw"), exists("cuadrantes_validos"),
  exists("tasas_estado_cuad"), exists("tasas_estado_egr_cuad"),
  exists("tasas_estado_egr")
)

library(spdep); library(sf); library(data.table); library(ggplot2); library(openxlsx)

# -----------------------------------------------------------------------------
# Función auxiliar: LISA para un cuadrante específico (reutilizable)
# -----------------------------------------------------------------------------

lisa_cuadrante <- function(cdr, tasas_cuad_dt, id_col, nombre_archivo, titulo) {
  sub <- tasas_cuad_dt[cuadrante == cdr]
  df  <- sub[match(mx_states_sf$ENT_RESID, sub[[id_col]])]
  stopifnot(!anyNA(df$tasa))
  
  lm    <- localmoran(df$tasa, lw, zero.policy = TRUE)
  p_col <- grep("^Pr", colnames(lm), value = TRUE)[1]
  
  sf_cdr <- mx_states_sf
  sf_cdr$tasa    <- df$tasa
  sf_cdr$p_value <- lm[, p_col]
  
  tasa_z     <- as.numeric(scale(sf_cdr$tasa))
  lag_tasa_z <- lag.listw(lw, tasa_z, zero.policy = TRUE)
  sig        <- sf_cdr$p_value < 0.05
  
  sf_cdr$cluster <- "No significativo"
  sf_cdr$cluster[sig & tasa_z > 0 & lag_tasa_z > 0] <- "Alto-Alto"
  sf_cdr$cluster[sig & tasa_z < 0 & lag_tasa_z < 0] <- "Bajo-Bajo"
  sf_cdr$cluster[sig & tasa_z > 0 & lag_tasa_z < 0] <- "Alto-Bajo"
  sf_cdr$cluster[sig & tasa_z < 0 & lag_tasa_z > 0] <- "Bajo-Alto"
  
  mt <- moran.test(df$tasa, lw, zero.policy = TRUE)
  
  p <- ggplot(sf_cdr) +
    geom_sf(aes(fill = cluster), color = "white", linewidth = 0.3) +
    scale_fill_manual(values = c(
      "Alto-Alto"        = "#d73027",
      "Bajo-Bajo"        = "#4575b4",
      "Alto-Bajo"        = "#fc8d59",
      "Bajo-Alto"        = "#91bfdb",
      "No significativo" = "grey90"
    ), name = "Cluster LISA (p<0.05)") +
    labs(
      title    = titulo,
      subtitle = paste0(
        "Índice de Moran = ", round(unname(mt$estimate["Moran I statistic"]), 3),
        "  (p = ", format.pval(mt$p.value, digits = 2), ")"
      )
    ) +
    theme_void(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
  
  ggsave(file.path(datos_path, "analisis", nombre_archivo), plot = p, width = 8, height = 6, dpi = 150)
  
  sf_cdr
}

nombre_estado <- setNames(mx_states_sf$NAME_1, mx_states_sf$ENT_RESID)

# -----------------------------------------------------------------------------
# G. LISA - mortalidad, cuadrante 3 (tamiz neonatal + sintomático/paliativo)
#    y cuadrante 9 (manifestación tardía + sintomático/paliativo)
# -----------------------------------------------------------------------------

lisa_cuad3 <- lisa_cuadrante(
  3, tasas_estado_cuad, "ENT_RESID",
  "mapa_lisa_mortalidad_cuadrante3.jpeg",
  "Clusters LISA - mortalidad, cuadrante 3\n(tamiz neonatal disponible + sintomático/paliativo)"
)

lisa_cuad9 <- lisa_cuadrante(
  9, tasas_estado_cuad, "ENT_RESID",
  "mapa_lisa_mortalidad_cuadrante9.jpeg",
  "Clusters LISA - mortalidad, cuadrante 9\n(manifestación tardía + sintomático/paliativo)"
)

cat("\nCuadrante 3 - estados con cluster significativo:\n")
print(data.frame(
  estado  = nombre_estado[lisa_cuad3$ENT_RESID],
  cluster = lisa_cuad3$cluster
)[lisa_cuad3$cluster != "No significativo", ])

cat("\nCuadrante 9 - estados con cluster significativo:\n")
print(data.frame(
  estado  = nombre_estado[lisa_cuad9$ENT_RESID],
  cluster = lisa_cuad9$cluster
)[lisa_cuad9$cluster != "No significativo", ])

# -----------------------------------------------------------------------------
# H.1 Egresos - Moran's I global (lugar de atención, NO residencia)
# -----------------------------------------------------------------------------

tasa_egr_estado <- tasas_estado_egr[match(mx_states_sf$ENT_RESID, ENTIDAD)]
stopifnot(!anyNA(tasa_egr_estado$tasa_raras))

moran_result_egr <- moran.test(tasa_egr_estado$tasa_raras, lw, zero.policy = TRUE)
print(moran_result_egr)

# -----------------------------------------------------------------------------
# H.2 Egresos - LISA global
# -----------------------------------------------------------------------------

local_moran_egr <- localmoran(tasa_egr_estado$tasa_raras, lw, zero.policy = TRUE)
p_col_egr <- grep("^Pr", colnames(local_moran_egr), value = TRUE)[1]

mx_states_sf_egr <- mx_states_sf
mx_states_sf_egr$tasa    <- tasa_egr_estado$tasa_raras
mx_states_sf_egr$p_value <- local_moran_egr[, p_col_egr]

tasa_z_egr     <- as.numeric(scale(mx_states_sf_egr$tasa))
lag_tasa_z_egr <- lag.listw(lw, tasa_z_egr, zero.policy = TRUE)
sig_egr        <- mx_states_sf_egr$p_value < 0.05

mx_states_sf_egr$cluster <- "No significativo"
mx_states_sf_egr$cluster[sig_egr & tasa_z_egr > 0 & lag_tasa_z_egr > 0] <- "Alto-Alto"
mx_states_sf_egr$cluster[sig_egr & tasa_z_egr < 0 & lag_tasa_z_egr < 0] <- "Bajo-Bajo"
mx_states_sf_egr$cluster[sig_egr & tasa_z_egr > 0 & lag_tasa_z_egr < 0] <- "Alto-Bajo"
mx_states_sf_egr$cluster[sig_egr & tasa_z_egr < 0 & lag_tasa_z_egr > 0] <- "Bajo-Alto"

ggplot(mx_states_sf_egr) +
  geom_sf(aes(fill = cluster), color = "white", linewidth = 0.3) +
  scale_fill_manual(values = c(
    "Alto-Alto"        = "#d73027",
    "Bajo-Bajo"        = "#4575b4",
    "Alto-Bajo"        = "#fc8d59",
    "Bajo-Alto"        = "#91bfdb",
    "No significativo" = "grey90"
  ), name = "Cluster LISA (p<0.05)") +
  labs(
    title    = "Clusters espaciales (LISA) de egresos por enfermedades raras",
    subtitle = paste0(
      "Índice de Moran global = ", round(unname(moran_result_egr$estimate["Moran I statistic"]), 3),
      "  (p = ", format.pval(moran_result_egr$p.value, digits = 2), ")\n",
      "Por entidad de la unidad médica (lugar de atención, NO residencia)"
    )
  ) +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(datos_path, "analisis/mapa_lisa_egresos.jpeg"), width = 8, height = 6, dpi = 150)

# -----------------------------------------------------------------------------
# H.3 Egresos - Moran's I por cuadrante
# -----------------------------------------------------------------------------

moran_por_cuadrante_egr <- rbindlist(lapply(cuadrantes_validos, function(cdr) {
  sub <- tasas_estado_egr_cuad[cuadrante == cdr]
  df  <- sub[match(mx_states_sf$ENT_RESID, sub$ENTIDAD)]
  if (anyNA(df$tasa)) return(NULL)
  mt <- moran.test(df$tasa, lw, zero.policy = TRUE)
  data.table(
    cuadrante = cdr,
    moran_I   = unname(mt$estimate["Moran I statistic"]),
    p_value   = mt$p.value
  )
}))
print(moran_por_cuadrante_egr)

# -----------------------------------------------------------------------------
# I. Exportar todo a un segundo archivo (no pisa analisis_geografico_cuadrante_moran.xlsx)
# -----------------------------------------------------------------------------

wb2 <- createWorkbook()

addWorksheet(wb2, "lisa_cuadrante3_def")
writeData(wb2, "lisa_cuadrante3_def",
          st_drop_geometry(lisa_cuad3[, c("ENT_RESID", "NAME_1", "tasa", "p_value", "cluster")]))

addWorksheet(wb2, "lisa_cuadrante9_def")
writeData(wb2, "lisa_cuadrante9_def",
          st_drop_geometry(lisa_cuad9[, c("ENT_RESID", "NAME_1", "tasa", "p_value", "cluster")]))

addWorksheet(wb2, "moran_global_egresos")
writeData(wb2, "moran_global_egresos", data.table(
  medida  = "egresos_total",
  moran_I = unname(moran_result_egr$estimate["Moran I statistic"]),
  p_value = moran_result_egr$p.value
))

addWorksheet(wb2, "lisa_egresos_total")
writeData(wb2, "lisa_egresos_total",
          st_drop_geometry(mx_states_sf_egr[, c("ENT_RESID", "NAME_1", "tasa", "p_value", "cluster")]))

addWorksheet(wb2, "moran_cuadrante_egresos")
writeData(wb2, "moran_cuadrante_egresos", moran_por_cuadrante_egr)

saveWorkbook(wb2, file.path(datos_path, "analisis_geografico_extension2.xlsx"), overwrite = TRUE)

cat("\nListo. Nuevos archivos en Datos/analisis/:\n",
    "  - mapa_lisa_mortalidad_cuadrante3.jpeg\n",
    "  - mapa_lisa_mortalidad_cuadrante9.jpeg\n",
    "  - mapa_lisa_egresos.jpeg\n",
    "Y en Datos/: analisis_geografico_extension2.xlsx\n")
