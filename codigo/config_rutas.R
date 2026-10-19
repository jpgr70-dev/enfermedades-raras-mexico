# =============================================================================
# config_rutas.R -- UNICO lugar donde se define la ruta de los datos.
# Todos los scripts del paquete la leen de aqui (menor 30, 19-oct-2026).
#
# Como usarlo: abrir R, hacer setwd() a la carpeta del paquete (la que contiene este
# archivo) y correr el script que toque. Para otra maquina, cambiar SOLO la ruta de
# abajo, o definir la variable de entorno ER_DATOS antes de abrir R.
#
# `datos_path` debe contener: egresos/, defunciones_*_dbf.zip, 0_Pob_Mitad_1950_2070.xlsx,
# cie10 raras_corregido.xlsx, 13023_2021_1737_MOESM1_ESM.xlsx, es_product9_*.xml,
# WPP2024_*.xlsx, orphanet catalogo.dta, ORPHA_ICD10_mapping*.xml (copiar a Datos/ desde Orphanet_Nomenclature_Pack_ES) y analisis/
# (con matriz_clasificacion_v01.csv). Las salidas de las sesiones van a datos_path/analisis/.
# =============================================================================
datos_path   <- Sys.getenv("ER_DATOS", unset = "E:/jp/enfermedades raras/Datos")
egresos_path <- file.path(datos_path, "egresos")
stopifnot("datos_path no existe: ajustalo en config_rutas.R" = dir.exists(datos_path))
dir.create(file.path(datos_path, "analisis"), showWarnings = FALSE, recursive = TRUE)
