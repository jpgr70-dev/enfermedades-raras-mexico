# =============================================================================
# tabla 2 referencia AN PQ 17Oct2026.R
#
# Columna de referencia "capitulos A-N + P + Q" (escenario principal E6b) para
# la Tabla 2 del manuscrito: N, mortalidad, estancia, sexo, grupo de edad e
# institucion, para tres grupos:
#   - Potencialmente raros
#   - Referencia principal (E6b): egresos NO potencialmente raros de capitulos
#     CIE-10 A-N + P + Q (sin O embarazo/parto, sin R-Z)
#   - Resto, deciles 9-10 (la referencia anterior, para comparar)
# Es un conteo agregado sobre el checkpoint "datos_clasificados" del script C:
# no ajusta modelos. Tarda pocos minutos (cargar el checkpoint ocupa varios GB).
# Corre en el equipo Windows. No modifica ni el script C ni sus checkpoints.
# =============================================================================

library(data.table)
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
dir_checkpoints <- file.path(datos_path, "analisis", "checkpoints_potencialmente_raros")
dir_salida      <- file.path(datos_path, "analisis")
cat10_zip       <- file.path(datos_path, "egresos", "ssa_Catalogos_Egresos_2023_2025.zip")

ruta <- file.path(dir_checkpoints, "datos_clasificados.rds")
stopifnot("Falta el checkpoint datos_clasificados.rds: corre primero el script C" = file.exists(ruta))
datos_chk <- readRDS(ruta)
e <- datos_chk$egresos_completos
rm(datos_chk); gc()

faltan <- setdiff(c("cat3", "SEXO", "EDAD_num", "DIAS_ESTA_num", "defuncion",
                    "potencialmente_raro", "decil_original", "CLUES"), names(e))
stopifnot("Faltan columnas en egresos_completos" = length(faltan) == 0)

# --- Institucion (mismo join y misma agrupacion que la seccion 9 del script C) ---
if (!"institucion_4cat" %in% names(e)) {
  tmp <- tempdir()
  nombres_zip <- unzip(cat10_zip, list = TRUE)$Name
  clues_csv <- nombres_zip[grepl("^CATCLUES\\.csv$", basename(nombres_zip), ignore.case = TRUE)]
  stopifnot("No se encontro CATCLUES.csv en el zip de catalogos" = length(clues_csv) == 1)
  unzip(cat10_zip, files = clues_csv, exdir = tmp, junkpaths = TRUE)
  cat_clues <- fread(file.path(tmp, basename(clues_csv)), sep = "|", encoding = "Latin-1",
                     colClasses = "character")
  cat_clues[, `NOMBRE DE LA INSTITUCION` := enc2utf8(`NOMBRE DE LA INSTITUCION`)]
  inst_clues <- unique(cat_clues[, .(CLUES, institucion = `NOMBRE DE LA INSTITUCION`)])
  stopifnot("CLUES duplicado con institucion distinta" = !anyDuplicated(inst_clues$CLUES))
  e[inst_clues, institucion := i.institucion, on = "CLUES"]
  e[is.na(institucion), institucion := "No encontrado en catalogo"]
  e[, institucion_4cat := fcase(
    institucion == "INSTITUTO MEXICANO DEL SEGURO SOCIAL", "IMSS",
    institucion %in% c("SECRETARIA DE SALUD", "SERVICIOS DE SALUD IMSS BIENESTAR"), "Secretaría de Salud",
    institucion == "INSTITUTO DE SEGURIDAD Y SERVICIOS SOCIALES PARA LOS TRABAJADORES DEL ESTADO", "ISSSTE",
    default = "Otras")]
  rm(cat_clues, inst_clues)
}

# --- Grupos -------------------------------------------------------------------
caps_E6b <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","P","Q")
e[, letra_cap := substr(cat3, 1, 1)]
e[, grupo_ref := fcase(
  potencialmente_raro, "Potencialmente raros",
  letra_cap %in% caps_E6b, "Referencia A-N+P+Q (E6b)",
  default = NA_character_)]
e[, grupo_9_10 := fcase(
  potencialmente_raro, "Potencialmente raros",
  decil_original %in% c(9, 10), "Resto, deciles 9-10",
  default = NA_character_)]

e[, grupo_edad := as.character(cut(EDAD_num, breaks = c(-Inf, 17, 34, 49, 64, 79, Inf),
  labels = c("0-17", "18-34", "35-49", "50-64", "65-79", "80+")))]
e[is.na(grupo_edad), grupo_edad := "(edad no registrada)"]
e[, sexo_lbl := fcase(SEXO == "2", "Femenino", SEXO == "1", "Masculino",
                      default = "Otro/no especificado")]

describir <- function(dt, var_grupo) {
  d <- dt[!is.na(get(var_grupo))]
  setnames(d, var_grupo, "grupo", skip_absent = TRUE)
  res <- list()
  res$resumen <- d[, .(n_egresos = .N, n_defunciones = sum(defuncion),
                       mortalidad_pct = round(100 * mean(defuncion), 3),
                       mediana_dias = median(DIAS_ESTA_num, na.rm = TRUE),
                       p25_dias = quantile(DIAS_ESTA_num, 0.25, na.rm = TRUE),
                       p75_dias = quantile(DIAS_ESTA_num, 0.75, na.rm = TRUE),
                       edad_mediana = median(EDAD_num, na.rm = TRUE)), by = grupo]
  pct <- function(v) { x <- d[, .N, by = .(grupo, categoria = get(v))]
                       x[, pct := round(100 * N / sum(N), 1), by = grupo]
                       setnames(x, "N", "n"); x[order(grupo, categoria)] }
  res$sexo <- pct("sexo_lbl"); res$edad <- pct("grupo_edad"); res$institucion <- pct("institucion_4cat")
  res
}
r_ref  <- describir(e[, .(potencialmente_raro, grupo_ref, cat3, defuncion, DIAS_ESTA_num, EDAD_num,
                          sexo_lbl, grupo_edad, institucion_4cat)], "grupo_ref")
r_9_10 <- describir(e[, .(potencialmente_raro, grupo_9_10, defuncion, DIAS_ESTA_num, EDAD_num,
                          sexo_lbl, grupo_edad, institucion_4cat)], "grupo_9_10")

cat("\n--- Referencia A-N+P+Q: resumen ---\n");  print(r_ref$resumen)
cat("\n--- sexo ---\n");        print(r_ref$sexo)
cat("\n--- grupo de edad ---\n"); print(r_ref$edad)
cat("\n--- institucion ---\n");   print(r_ref$institucion)

wb <- createWorkbook()
hojas <- list(ref_AN_PQ_resumen = r_ref$resumen, ref_AN_PQ_sexo = r_ref$sexo,
              ref_AN_PQ_edad = r_ref$edad, ref_AN_PQ_institucion = r_ref$institucion,
              ref_9_10_resumen = r_9_10$resumen, ref_9_10_sexo = r_9_10$sexo,
              ref_9_10_edad = r_9_10$edad, ref_9_10_institucion = r_9_10$institucion)
for (n in names(hojas)) { addWorksheet(wb, n); writeData(wb, n, hojas[[n]]) }
saveWorkbook(wb, file.path(dir_salida, "tabla2_referencia_AN_PQ 17Oct2026.xlsx"), overwrite = TRUE)
cat("\nGuardado:", file.path(dir_salida, "tabla2_referencia_AN_PQ 17Oct2026.xlsx"), "\n")
