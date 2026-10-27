# =============================================================================
# Cobertura de la red SSA/IMSS-Bienestar dentro de los egresos de la base Sectorial
# 24-oct-2026. Resuelve la cifra "42% a 52%" de Metodos (hasta ahora escrita a mano en un
# comentario de analisis_geografico_ssa_residencia.R) y la recalculada por Andres (43.9% en
# 2021 a 53.1% en 2016, egresos vivos en ambas bases).
#
# Para cada anio 2014-2023 y 2024 calcula, para el total de egresos y solo para los de
# enfermedades raras (catalogo de 4 caracteres, sin K746, C509 ni C349):
#   a) Sectorial vivos: total y los de unidades SSA o IMB (caracteres 3 a 5 de la CLUES)
#      -> % Sectorial (SSA+IMB) / Sectorial
#   b) Base SSA (la de residencia) vivos
#      -> % base SSA / Sectorial  (es la cobertura que importa para el sub-analisis)
#   c) los mismos conteos incluyendo defunciones intrahospitalarias (MOTEGRE = 5),
#      para poder reproducir la serie original del comentario.
# Lee un anio a la vez y solo 3 columnas: poca RAM. Tiempo estimado: 10-20 min.
# Salida: Datos/analisis/cobertura_ssa/cobertura_ssa_sobre_sectorial 24Oct2026.xlsx
# =============================================================================
library(data.table); library(openxlsx)

if (!exists("datos_path", inherits = TRUE)) {
  if (file.exists("config_rutas.R")) {
    source("config_rutas.R", encoding = "UTF-8")
  } else {
    datos_path <- Sys.getenv("ER_DATOS", unset = "")
    if (!nzchar(datos_path)) stop("No encontre config_rutas.R en ", getwd(), " y ER_DATOS no esta definida: haz setwd() a la carpeta codigo/ o define ER_DATOS.")
  }
}
egresos_path <- file.path(datos_path, "egresos")
dir_salida   <- file.path(datos_path, "analisis", "cobertura_ssa")
dir.create(dir_salida, showWarnings = FALSE, recursive = TRUE)
tmp <- file.path(tempdir(), "cob"); dir.create(tmp, showWarnings = FALSE)

# Catalogo de raras a 4 caracteres, igual que el script geografico (sin K746, C509, C349)
cat_raras <- read.xlsx(file.path(datos_path, "cie10 raras_corregido.xlsx"), sheet = 1)
cod <- toupper(trimws(gsub("[+*,]", "", cat_raras$causa)))
cod <- unique(cod[nchar(cod) == 4 & !is.na(cod)])
cod <- setdiff(cod, c("K746", "C509", "C349"))
cat("Codigos raros de 4 caracteres:", length(cod), "\n")

meta_sect <- list(
  list(yr=2014, zip="sectorial_egresos_2014.zip", file="SECTOR2014.csv",             sep=","),
  list(yr=2015, zip="sectorial_egresos_2015.zip", file="2015_Sectorial.csv",         sep=","),
  list(yr=2016, zip="sectorial_egresos_2016.zip", file="2016_Sectorial.csv",         sep=","),
  list(yr=2017, zip="sectorial_egresos_2017.zip", file="SECTORIAL_2017.csv",         sep="|"),
  list(yr=2018, zip="sectorial_egresos_2018.zip", file="Egresos_Sectorial_2018.csv", sep="|"),
  list(yr=2019, zip="sectorial_egresos_2019.zip", file="Egresos_Sectorial_2019.csv", sep="|"),
  list(yr=2020, zip="sectorial_egresos_2020.zip", file="Egresos_Sectorial_2020.csv", sep="|"),
  list(yr=2021, zip="sectorial_egresos_2021.zip", file="Egresos_Sectorial_2021.txt", sep="|"),
  list(yr=2022, zip="sectorial_egresos_2022.zip", file="Egresos_Sectorial_2022.txt", sep="|"),
  list(yr=2023, zip="sectorial_egresos_2023.zip", file="Egresos_Sectorial_2023.txt", sep="|"),
  list(yr=2024, zip="sectorial_egresos_2024.zip", file="Egresos_Sectorial_2024.txt", sep="|"))
meta_ssa <- list(
  list(yr=2014, zip="ssa_egresos_2014.zip", file="EGRESO.csv"),
  list(yr=2015, zip="ssa_egresos_2015.zip", file="SAEH_2015/EGRESO_2015.csv"),
  list(yr=2016, zip="ssa_egresos_2016.zip", file="SAEH_2016/EGRESOS_2016.csv"),
  list(yr=2017, zip="ssa_egresos_2017.zip", file="EGRESOS_2017.csv"),
  list(yr=2018, zip="ssa_egresos_2018.zip", file="EGRESO_2018.csv"),
  list(yr=2019, zip="ssa_egresos_2019.zip", file="Egresos2019/EGRESOS_2019.txt"),
  list(yr=2020, zip="ssa_egresos_2020.zip", file="EGRESOS.txt"),
  list(yr=2021, zip="ssa_egresos_2021.zip", file="EGRESOS.txt"),
  list(yr=2022, zip="ssa_egresos_2022.zip", file="EGRESOS.txt"),
  list(yr=2023, zip="ssa_egresos_2023.zip", file="Registros/Egresos.txt"),
  list(yr=2024, zip="ssa_egresos_2024.zip", file="Registros/Egresos.txt"))

leer <- function(zip, file, sep, columnas, yr) {
  fp <- file.path(tmp, basename(file))
  unzip(file.path(egresos_path, zip), files = file, exdir = tmp, junkpaths = TRUE)
  on.exit(suppressWarnings(try(file.remove(fp), silent = TRUE)), add = TRUE)
  hdr <- if (is.null(sep)) names(fread(fp, nrows = 0)) else names(fread(fp, sep = sep, nrows = 0))
  hup <- toupper(hdr)
  sel <- sapply(columnas, function(opts) { i <- match(toupper(opts), hup); hdr[i[!is.na(i)][1]] })
  stopifnot("faltan columnas" = !anyNA(sel))
  args <- list(fp, select = unname(sel), fill = Inf,
               colClasses = "character", showProgress = FALSE, encoding = "UTF-8")
  if (!is.null(sep)) args$sep <- sep
  dt <- withCallingHandlers(do.call(fread, args), warning = function(w) {
    if (grepl("Stopped early|Se detuvo anticipadamente", conditionMessage(w)))
      stop("fread se detuvo antes de terminar ", file, call. = FALSE) })
  setnames(dt, unname(sel), names(sel))
  dt
}

res <- list()
for (i in seq_along(meta_sect)) {
  ms <- meta_sect[[i]]; mb <- meta_ssa[[i]]
  # Sectorial
  s <- leer(ms$zip, ms$file, ms$sep, list(CLUES="CLUES", MOTEGRE=c("MOTEGRE","MOTIVO_EGRESO"), DX=c("AFECPRIN4","AFECPRIN")), ms$yr)
  s[, `:=`(vivo = MOTEGRE != "5", red = substr(CLUES, 3, 5) %in% c("SSA", "IMB"),
           rara = toupper(trimws(DX)) %in% cod)]
  # Base SSA (residencia)
  b <- leer(mb$zip, mb$file, NULL, list(CLUES="CLUES", MOTEGRE="MOTEGRE", DX="AFECPRIN"), mb$yr)
  b[, `:=`(vivo = MOTEGRE != "5", rara = toupper(trimws(DX)) %in% cod)]
  res[[i]] <- data.table(
    anio = ms$yr,
    sect_todos = nrow(s), sect_vivos = s[vivo == TRUE, .N],
    sect_red_todos = s[red == TRUE, .N], sect_red_vivos = s[red == TRUE & vivo == TRUE, .N],
    ssa_todos = nrow(b), ssa_vivos = b[vivo == TRUE, .N],
    sect_raras_vivos = s[rara & vivo == TRUE, .N],
    sect_raras_red_vivos = s[rara & vivo == TRUE & red == TRUE, .N],
    ssa_raras_vivos = b[rara & vivo == TRUE, .N],
    sect_raras_todos = s[rara == TRUE, .N], sect_raras_red_todos = s[rara & red == TRUE, .N])
  message(ms$yr, " listo")
  rm(s, b); gc()
}
r <- rbindlist(res)
r[, `:=`(
  pct_sect_red_todos     = round(100 * sect_red_todos / sect_todos, 1),
  pct_sect_red_vivos     = round(100 * sect_red_vivos / sect_vivos, 1),
  pct_ssa_sobre_sect_vivos = round(100 * ssa_vivos / sect_vivos, 1),
  pct_ssa_sobre_sect_todos = round(100 * ssa_todos / sect_todos, 1),
  raras_pct_sect_red_vivos = round(100 * sect_raras_red_vivos / sect_raras_vivos, 1),
  raras_pct_ssa_sobre_sect_vivos = round(100 * ssa_raras_vivos / sect_raras_vivos, 1))]
print(r[, .(anio, pct_sect_red_todos, pct_sect_red_vivos, pct_ssa_sobre_sect_vivos,
            raras_pct_sect_red_vivos, raras_pct_ssa_sobre_sect_vivos)])
cat("\nRango 2014-2023 (egresos vivos, base SSA / Sectorial): ",
    range(r[anio <= 2023, pct_ssa_sobre_sect_vivos]), "\n")
cat("Rango 2014-2023 (egresos vivos, SSA+IMB dentro de Sectorial): ",
    range(r[anio <= 2023, pct_sect_red_vivos]), "\n")
write.xlsx(list(cobertura = r), file.path(dir_salida, "cobertura_ssa_sobre_sectorial 24Oct2026.xlsx"))
cat("\nGuardado en", file.path(dir_salida, "cobertura_ssa_sobre_sectorial 24Oct2026.xlsx"), "\n")
