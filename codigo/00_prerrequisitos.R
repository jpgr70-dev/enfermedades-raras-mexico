# =============================================================================
# 00_prerrequisitos.R -- revisa que esten instalados los paquetes de TODO el paquete
# de replicacion y la ruta de datos. Solo informa: no instala nada por su cuenta.
# Correr una vez antes de empezar (con setwd() a la carpeta del paquete).
# =============================================================================
paquetes <- c(
  # todos los scripts
  "data.table", "openxlsx", "ggplot2", "dplyr",
  # A (prevalencia)
  "xml2",
  # B1 (principal)
  "haven", "foreign", "sf", "ggrepel",
  # B2-B4 (geografico)
  "spdep",
  # C (potencialmente raros)
  "brglm2", "fixest", "marginaleffects", "MASS"
)
faltan <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan)) {
  cat("FALTAN paquetes:", paste(faltan, collapse = ", "), "\nInstalar con:\n  install.packages(c(",
      paste0('"', faltan, '"', collapse = ", "), "))\n")
} else cat("Paquetes: OK\n")
cat("R", R.version.string, "\n")

if (!file.exists("config_rutas.R")) stop("setwd() a la carpeta del paquete (donde esta config_rutas.R).")
source("config_rutas.R", encoding = "UTF-8")
cat("datos_path =", datos_path, "\n")
necesarios <- c("egresos", "analisis", "0_Pob_Mitad_1950_2070.xlsx", "cie10 raras_corregido.xlsx",
                "13023_2021_1737_MOESM1_ESM.xlsx", "es_product9_prev.xml",
                "es_product9_ages.xml", "orphanet catalogo.dta", file.path("analisis", "matriz_clasificacion_v01.csv"),
                "WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx",
                "defunciones_generales_base_datos_2010_2014_dbf.zip",
                "defunciones_generales_base_datos_2015_2019_dbf.zip")
for (a in necesarios) cat(sprintf("  %-70s %s\n", a, if (file.exists(file.path(datos_path, a))) "ok" else "FALTA"))
orph <- list.files(datos_path, pattern = "^ORPHA_ICD10_mapping.*\\.xml$", recursive = TRUE, ignore.case = TRUE)
cat(sprintf("  %-70s %s\n", "ORPHA_ICD10_mapping*.xml (en cualquier subcarpeta; script C)", if (length(orph)) "ok" else "FALTA (copiarlo de Orphanet_Nomenclature_Pack_ES a Datos/)"))
cat("  defunciones 2020-2024 (zip por anio):",
    sum(file.exists(file.path(datos_path, paste0("defunciones_base_datos_", 2020:2024, "_dbf.zip")))), "de 5\n")
cat("  egresos (carpeta):", length(list.files(file.path(datos_path, "egresos"))), "archivos\n")
cat("\nGADM (mapas): se descarga una vez a datos_path/gadm41_MEX_1.json; sin internet, copiar ese archivo a mano.\n")
