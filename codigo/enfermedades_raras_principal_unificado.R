# =============================================================================
# SCRIPT PRINCIPAL UNIFICADO (24-sep-2026)
# Base: "enfermedades raras.R" (version del equipo Windows: incluye las tres
# correcciones de ago-2026, el ejercicio AMPLIADO 3bis/15bis, las tablas de
# manuscrito -> tablas_manuscrito_v2.xlsx y el guardado de sesion que usan los
# scripts geograficos). Se le agrego la seccion 15ter (mapas 2014 vs. 2024,
# Figuras 1 y 2 del manuscrito), tomada sin cambios de
# analisis_enfermedades_raras_corregido.R. Convertido de Latin-1/CRLF a UTF-8.
# Ver METODOLOGIA_prevalencia_y_egresos_ER_Mexico.md para el orden de corrida.
#
# 3-oct-2026: los nombres de objeto ya no llevan "ñ" (total_por_año -> total_por_anio,
# etc.) para que el script corra aunque R o RStudio lea el archivo con otra
# codificación. Aun así, ábrelo como UTF-8 para que los textos de las gráficas
# conserven acentos.
#
# CORRECCIÓN 3-oct-2026 (denominadores): todas las tasas usan ahora una sola
# fuente de población, CONAPO (0_Pob_Mitad_1950_2070.xlsx), leída una vez en la
# nueva sección 1b. Se eliminaron: la población del Banco Mundial (sección 4),
# las estimaciones CONAPO 2019 escritas a mano (sección 5), el total nacional
# 130,861,007 escrito a mano (secciones 6 y 13) y las proporciones fijas por
# sexo 48.9%/51.1% (secciones 7, 13, 15.3 y 15.4). Ahora la población por sexo
# viene de CONAPO por entidad y año. `pop_estado` (que usan los scripts B2 y B4)
# es la población media anual 2014–2024 de CONAPO por entidad. La versión
# anterior está en respaldo/enfermedades_raras_principal_unificado_antes_CONAPO_2026-10-03.R.
# 4-oct-2026 (mapas): todos los ggsave de mapas (theme_void) llevan bg = "white"
# (sin esto el JPEG sale con fondo negro) y los dos mapas 2014 vs. 2024 de la
# sección 15ter (Figuras 1 y 2) ya no llevan título ni subtítulo: el pie de figura
# lo da el documento. Versión anterior en respaldo/..._antes_bg_blanco_2026-10-04.R.
# =============================================================================
# =============================================================================
# Enfermedades raras en México, 2014–2024
# Mortalidad (defunciones) y morbilidad (egresos hospitalarios)
# =============================================================================

library(haven)
library(foreign)
library(data.table)
library(ggplot2)
library(sf)
library(dplyr)
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

# -----------------------------------------------------------------------------
# 1. Importar catálogos de enfermedades raras
# -----------------------------------------------------------------------------

## Actualización 24-ago: el catálogo de enfermedades raras ya no vive en el
## .dta original — ahora está en "cie10 raras_corregido.xlsx" (mismas 1,738
## filas / 1,732 códigos únicos en `causa`, ya sin los 22 símbolos sobrantes
## de daga/asterisco/coma; siguen sin match por prefijo los 89 códigos de 3
## caracteres — ver hallazgo en metodologia/analisis-geografico-defunciones-egresos.md).
## Línea anterior, por si se necesita volver al .dta:
##   cie_que_son_raras <- read_dta(file.path(datos_path, "cie que son rarasdta.dta"))
cie_que_son_raras <- read.xlsx(file.path(datos_path, "cie10 raras_corregido.xlsx"), sheet = 1)
# EXCLUSIÓN 12-oct-2026 (decisión de JP): K746, C509 y C349 se excluyen también del
# catálogo GENERAL (defunciones_raras, egresos_raras, Figuras 1-2, tasas nacionales y
# Moran total), no solo del catálogo por cuadrante. Motivo: son códigos de sitio/causa
# no especificada que, en 2024, aportaban 30,447 de 112,983 defunciones (27%) del
# catálogo general. Ver la exclusión original por cuadrante en la sección 3 de este script.
codigos_excluir_general <- c("K746", "C509", "C349")
cie_que_son_raras <- cie_que_son_raras[!(cie_que_son_raras$causa %in% codigos_excluir_general), ]
message("Catálogo general sin K746/C509/C349: ", nrow(cie_que_son_raras), " filas")
orphanet_catalogo <- read_dta(file.path(datos_path, "orphanet catalogo.dta"))
matriz_clasificacion_v0.1 <- read.csv(file.path(datos_path,"/analisis/matriz_clasificacion_v01.csv"))
matriz_clasificacion_v0.1 <- matriz_clasificacion_v0.1 %>%
  mutate(cie10 = substr(gsub("\\.", "", cie10), 1, 4))
matriz_clasificacion_v0.1 <- matriz_clasificacion_v0.1[!duplicated(matriz_clasificacion_v0.1$cie10), 
                                                       c("nombre_es", "cie10", "eje_deteccion", "eje_tratabilidad", "cuadrante", "confianza")]
# -----------------------------------------------------------------------------
# 1b. Población CONAPO: denominador único de todas las tasas
# -----------------------------------------------------------------------------
# Proyecciones de población a mitad de año, CONAPO. Columnas confirmadas contra
# el archivo real: RENGLON, AÑO, ENTIDAD, CVE_GEO, EDAD, SEXO, POBLACION.
# CVE_GEO == 0 es el total nacional; 1-32 son las entidades, en el mismo orden
# que gadm_bridge. SEXO viene como texto: "Hombres" / "Mujeres". Una fila por
# edad simple, por lo que hay que sumar.

pob_conapo <- openxlsx::read.xlsx(file.path(datos_path, "0_Pob_Mitad_1950_2070.xlsx"))
setDT(pob_conapo)
# Renombrado por posición (evita problemas de encoding con "AÑO")
setnames(pob_conapo, old = names(pob_conapo),
         new = c("RENGLON", "ANIO_POB", "ENTIDAD_NOMBRE", "CVE_GEO", "EDAD", "SEXO", "POBLACION"))
stopifnot(all(unique(pob_conapo$SEXO) %in% c("Hombres", "Mujeres")))
pob_conapo[, SEXO_COD := fifelse(SEXO == "Hombres", 1L, 2L)]

anios_estudio <- 2014:2024
n_anios <- length(anios_estudio)

# Por entidad, año y sexo
pob_estado_sexo_anio <- pob_conapo[CVE_GEO != 0 & ANIO_POB %in% anios_estudio,
                                   .(pob_sexo = sum(POBLACION, na.rm = TRUE)),
                                   by = .(ANIO_POB, CVE_GEO, SEXO = SEXO_COD)]
pob_estado_sexo_anio[, ENT_RESID := sprintf("%02d", CVE_GEO)]

# Por entidad y año
pob_estado_anio <- pob_estado_sexo_anio[, .(poblacion = sum(pob_sexo)),
                                        by = .(ANIO_POB, CVE_GEO, ENT_RESID)]

# Nacional por año (suma de entidades) y por año y sexo
poblacion <- pob_estado_anio[, .(poblacion = sum(poblacion)), by = .(ANIO_FUENTE = ANIO_POB)]
pob_nac_sexo <- pob_estado_sexo_anio[, .(pob_sexo = sum(pob_sexo)),
                                     by = .(ANIO_FUENTE = ANIO_POB, SEXO)]

# Chequeos contra las cifras nacionales confirmadas del archivo real
chk_2014 <- poblacion[ANIO_FUENTE == 2014, poblacion]
chk_2024 <- poblacion[ANIO_FUENTE == 2024, poblacion]
cat("Poblacion nacional CONAPO 2014:", format(chk_2014, big.mark = ","), "(esperado ~121,048,604)\n")
cat("Poblacion nacional CONAPO 2024:", format(chk_2024, big.mark = ","), "(esperado ~132,274,416)\n")
stopifnot(
  uniqueN(pob_estado_anio$ENT_RESID) == 32,
  nrow(poblacion) == n_anios,
  abs(chk_2014 - 121048604) < 1000,
  abs(chk_2024 - 132274416) < 1000,
  # el total nacional del archivo (CVE_GEO == 0) debe coincidir con la suma de entidades
  abs(pob_conapo[CVE_GEO == 0 & ANIO_POB == 2024, sum(POBLACION)] - chk_2024) < 1000
)

# Población media anual 2014–2024 por entidad: denominador de las tasas
# acumuladas por entidad. Conserva el nombre `pop_estado` porque lo usan los
# scripts B2 (analisis geografico adicional.R) y B4 (analisis_geografico_ssa_residencia.R).
# tasa acumulada = casos 2014–2024 / población media * 1e5
# tasa media anual = tasa acumulada / n_anios
pop_estado <- as.data.frame(pob_estado_anio[, .(poblacion = mean(poblacion)), by = ENT_RESID])

# Población 2014 y 2024 por entidad (secciones 6, 7, 13 y 15ter)
pob_2014 <- pob_estado_anio[ANIO_POB == 2014, .(ENT_RESID, poblacion)]
pob_2024 <- pob_estado_anio[ANIO_POB == 2024, .(ENT_RESID, poblacion)]
pop_2024 <- pob_2024
pob_sexo_2024 <- pob_estado_sexo_anio[ANIO_POB == 2024, .(ENT_RESID, SEXO, pob_sexo)]

# -----------------------------------------------------------------------------
# 2. Cargar defunciones 2014–2024
# -----------------------------------------------------------------------------

tmp <- tempdir()

# -- 2014 (zip multi-año 2010-2014) --
zip_1014 <- file.path(datos_path, "defunciones_generales_base_datos_2010_2014_dbf.zip")

read_defun <- function(zip_file, dbf_path, year) {
  unzip(zip_file, files = dbf_path, exdir = tmp, junkpaths = FALSE)
  df <- read.dbf(file.path(tmp, dbf_path), as.is = TRUE)
  df$ANIO_FUENTE <- year
  message(year, ": ", nrow(df), " rows, ", ncol(df), " cols")
  df
}

d2014 <- read_defun(zip_1014, "defunciones_base_datos_2014_dbf/DEFUN14.dbf", 2014)

# -- 2015–2019 (zip multi-año 2015-2019) --
zip_1519 <- file.path(datos_path, "defunciones_generales_base_datos_2015_2019_dbf.zip")
years_1519 <- list(
  list(path = "defunciones_base_datos_2015/DEFUN15.DBF", year = 2015),
  list(path = "defunciones_base_datos_2016/DEFUN16.dbf", year = 2016),
  list(path = "defunciones_base_datos_2017/DEFUN17.dbf", year = 2017),
  list(path = "defunciones_base_datos_2018/DEFUN18.DBF", year = 2018),
  list(path = "defunciones_base_datos_2019/DEFUN19.DBF", year = 2019)
)
list_1519 <- lapply(years_1519, function(x) read_defun(zip_1519, x$path, x$year))

# -- 2020–2024 (zips individuales; nombres de archivo varían en mayúsculas) --
year_files <- list(
  `2020` = "defun20.dbf",
  `2021` = "defun21.dbf",
  `2022` = "DEFUN22.dbf",
  `2023` = "DEFUN23.dbf",
  `2024` = "DEFUN24.dbf"
)

list_2024 <- lapply(names(year_files), function(yr) {
  zip <- file.path(datos_path, paste0("defunciones_base_datos_", yr, "_dbf.zip"))
  dbf <- year_files[[yr]]
  unzip(zip, files = dbf, exdir = tmp)
  df  <- read.dbf(file.path(tmp, dbf), as.is = TRUE)
  df$ANIO_FUENTE <- as.integer(yr)
  message(yr, ": ", nrow(df), " rows, ", ncol(df), " cols")
  df
})

# -- Combinar todos los años (fill = TRUE maneja columnas distintas entre años) --
defunciones <- rbindlist(c(list(d2014), list_1519, list_2024),
                         fill = TRUE, use.names = TRUE)

# -----------------------------------------------------------------------------
# 3. Filtrar muertes por enfermedades raras
# -----------------------------------------------------------------------------

codigos_raras_cuadrantes <- matriz_clasificacion_v0.1

# EXCLUSIÓN 24-ago (decisión tomada en la conversación "Generar código para R"
# de este proyecto): 3 códigos de la matriz de cuadrantes son de baja
# especificidad — a nivel de "sitio/causa no especificada" cubren un volumen
# enorme de casos comunes que no corresponden realmente a la enfermedad rara
# que se buscaba clasificar (mismo tipo de riesgo que el match por prefijo de
# los códigos de 3 caracteres, pero aquí con códigos de 4 caracteres legítimos
# en la matriz):
#   - K746: cirrosis hepática, no especificada
#   - C509: cáncer de mama, sitio no especificado
#   - C349: cáncer de bronquio/pulmón, sitio no especificado
# Se excluyen aquí, a nivel de fuente (`codigos_raras_cuadrantes`), para que
# TODO lo que depende de la clasificación por cuadrante (defunciones_raras_cuadrantes,
# egresos_raras_cuadrantes, tasas_cuadrantes, tasas_egresos_cuadrantes, top10_def,
# top10_egr, y los tres scripts de extensión geográfica que consumen estos
# objetos) refleje la exclusión de forma consistente, sin tener que duplicar
# el cálculo al final del script. Lista confirmada como definitiva por JP —
# no se identificaron más candidatos de baja especificidad.
# NOTA: se usa forma explícita con "," y `codigos_raras_cuadrantes$cie10`
# (no `!cie10 %in% ...` a secas) porque en este punto del script
# `codigos_raras_cuadrantes` es un data.frame normal, no un data.table —
# `df[i]` sin coma indexaría COLUMNAS, no filas (ver nota de calidad de datos
# sobre esta trampa de sintaxis en metodologia/analisis-geografico-defunciones-egresos.md).
codigos_excluir <- c("K746", "C509", "C349")
codigos_raras_cuadrantes <- codigos_raras_cuadrantes[!codigos_raras_cuadrantes$cie10 %in% codigos_excluir, ]

# -----------------------------------------------------------------------------
# 3bis. AMPLIADO (adicional — NO sustituye ni modifica codigos_raras_cuadrantes)
# -----------------------------------------------------------------------------
# Corrección de 5 códigos "pelón" de 3 caracteres (E84, D57, D66, D67, G10) que,
# tras la transformación de limpieza de arriba (substr(gsub(...),1,4)), quedan
# en exactamente 3 caracteres y por tanto NUNCA hacen match con los datos
# administrativos mexicanos (CAUSA_DEF/AFECPRIN4 siempre de 4 caracteres) —
# ver hallazgo "Revisión sistemática de los 78 códigos 'pelón' de 3
# caracteres" en metodologia/analisis-geografico-defunciones-egresos.md.
# Afecta a: Fibrosis Quística y Anemia Falciforme (cuadrante 2), Hemofilia A
# y Hemofilia B (cuadrante 5), Huntington (cuadrante 9) — las 5 son
# `confianza = "alta"`, es decir, no es un problema de las filas ya excluidas
# por baja especificidad.
#
# A petición de JP (25-ago) esto se implementa como una aproximación APARTE:
# se crea `codigos_raras_cuadrantes_ampliado` (y toda la cadena de objetos
# "_ampliado" que sigue) sin tocar `codigos_raras_cuadrantes` ni nada que ya
# dependa de él, para poder contrastar ambas versiones al final (sección 15bis)
# y evaluar la implicación geográfica (analisis geografico adicional.R, sección H).
#
# Subcódigos elegidos según el análisis de riesgo de ambigüedad ya documentado:
#   - E84 (Fibrosis Quística, cuadrante 2)  -> E840, E841, E848
#     (se excluye E849 "no especificada", mismo criterio que la exclusión de
#      K746/C509/C349 de arriba: un código catch-all de baja especificidad)
#   - D57 (Anemia Falciforme, cuadrante 2)  -> D570, D571
#     (se excluye D572: Orphanet la comparte con 6 entidades del cuadrante 6;
#      se excluyen D573 "rasgo" -portador, no enfermo- y D578 "otras", ambos
#      de especificidad insuficiente)
#   - D66 (Hemofilia A, cuadrante 5)        -> D66X (sin subdivisión OMS real;
#      México rellena con "X" — mismo patrón que R99X/I64X)
#   - D67 (Hemofilia B, cuadrante 5)        -> D67X (ídem)
#   - G10 (Huntington, cuadrante 9)         -> G10X (ídem)
codigos_pelon_a_reemplazar <- c("E84", "D57", "D66", "D67", "G10")

reemplazos_ampliado <- data.frame(
  cie10_original = c("E84", "E84", "E84", "D57", "D57", "D66", "D67", "G10"),
  cie10          = c("E840", "E841", "E848", "D570", "D571", "D66X", "D67X", "G10X")
)

filas_originales_pelon <- codigos_raras_cuadrantes[
  codigos_raras_cuadrantes$cie10 %in% codigos_pelon_a_reemplazar,
  c("nombre_es", "cie10", "eje_deteccion", "eje_tratabilidad", "cuadrante", "confianza")
]

# NOTA (corregido tras error de JP al correrlo): al hacer merge con by.x/by.y
# de distinto nombre, R NO genera una columna "cie10.y" — como el nombre de la
# columna "cie10" de `reemplazos_ampliado` (el código nuevo de 4 caracteres)
# coincide con el nombre del by.y de `filas_originales_pelon` (el código viejo
# de 3 caracteres), R simplemente conserva una sola columna "cie10" (la de
# `reemplazos_ampliado`, el código nuevo) sin sufijo — no hay colisión real.
# El código viejo de 3 caracteres queda en "cie10_original".
filas_nuevas_ampliado <- merge(
  reemplazos_ampliado,
  filas_originales_pelon,
  by.x = "cie10_original", by.y = "cie10"
)
filas_nuevas_ampliado <- filas_nuevas_ampliado[, c("nombre_es", "cie10", "eje_deteccion", "eje_tratabilidad", "cuadrante", "confianza")]

# CORRECCION 19-oct-2026 (menor 30, doble conteo de E84.8): la matriz de cuadrantes ya
# trae E848 con otra condicion (sindrome fibrosis quistica-gastritis-anemia megaloblastica,
# cuadrante 6, confianza baja). Al agregar E848 como fibrosis quistica (cuadrante 2) sin
# quitar la fila previa, cada egreso/defuncion con E848 quedaba contado dos veces en el
# merge. Ahora los subcodigos nuevos REEMPLAZAN cualquier fila previa con el mismo codigo
# (en el ampliado, E848 queda solo como fibrosis quistica) y se verifica que no haya
# codigos repetidos. `codigos_raras_cuadrantes` (el principal) no se modifica.
codigos_ya_en_matriz <- intersect(filas_nuevas_ampliado$cie10, codigos_raras_cuadrantes$cie10)
cat("Subcodigos nuevos que ya estaban en la matriz (se reemplazan): ",
    paste(codigos_ya_en_matriz, collapse = ", "), "\n", sep = "")
codigos_raras_cuadrantes_ampliado <- rbind(
  codigos_raras_cuadrantes[!codigos_raras_cuadrantes$cie10 %in%
                             c(codigos_pelon_a_reemplazar, filas_nuevas_ampliado$cie10), ],
  filas_nuevas_ampliado[, names(codigos_raras_cuadrantes)]
)
stopifnot("codigos repetidos en codigos_raras_cuadrantes_ampliado (doble conteo)" =
            !anyDuplicated(codigos_raras_cuadrantes_ampliado$cie10))

cat("Códigos ampliado: ", nrow(codigos_raras_cuadrantes_ampliado),
    " filas (original: ", nrow(codigos_raras_cuadrantes),
    "; -5 códigos pelón, +8 subcódigos nuevos, -", length(codigos_ya_en_matriz),
    " fila(s) previa(s) reemplazada(s))\n", sep = "")

defunciones_raras_cuadrantes <- defunciones[defunciones$CAUSA_DEF %in% codigos_raras_cuadrantes$cie10, ]

defunciones_raras_cuadrantes <- merge(
  defunciones[defunciones$CAUSA_DEF %in% codigos_raras_cuadrantes$cie10, ],
  codigos_raras_cuadrantes,
  by.x = "CAUSA_DEF",
  by.y = "cie10",
  all.x = TRUE
)

# AMPLIADO — objeto paralelo, mismo patrón de merge que arriba, no sobrescribe
# defunciones_raras_cuadrantes.
defunciones_raras_cuadrantes_ampliado <- merge(
  defunciones[defunciones$CAUSA_DEF %in% codigos_raras_cuadrantes_ampliado$cie10, ],
  codigos_raras_cuadrantes_ampliado,
  by.x = "CAUSA_DEF",
  by.y = "cie10",
  all.x = TRUE
)

codigos_raras     <- cie_que_son_raras$causa
defunciones_raras <- defunciones[CAUSA_DEF %in% codigos_raras]

# Resumen general
cat("Total muertes 2014-2024:    ", format(nrow(defunciones),       big.mark = ","), "\n")
cat("Muertes por E. raras:       ", format(nrow(defunciones_raras), big.mark = ","), "\n")
cat("Porcentaje:                 ", round(nrow(defunciones_raras) / nrow(defunciones) * 100, 2), "%\n")

# Muertes por año
defunciones_raras[, .N, by = ANIO_FUENTE][order(ANIO_FUENTE)]

defunciones_raras_cuadrantes[, .N, by = .(ANIO_FUENTE, cuadrante)][order(ANIO_FUENTE, cuadrante)]

# Códigos con más muertes
defunciones_raras[, .N, by = CAUSA_DEF][order(-N)][1:15]

# -----------------------------------------------------------------------------
# 4. Tasas de mortalidad por 100,000 habitantes (nivel nacional)
# -----------------------------------------------------------------------------

# Población nacional por año: objeto `poblacion` de la sección 1b (CONAPO).
# (Antes: Banco Mundial vía API; se eliminó para usar una sola fuente.)

# Calcular tasas
total_por_anio <- defunciones[, .(total_muertes = .N), by = ANIO_FUENTE]
raras_por_anio <- defunciones_raras[, .(muertes_raras = .N), by = ANIO_FUENTE]

raras_anio_cuadrantes <- defunciones_raras_cuadrantes[, .(muertes_cuadrantes = .N), by = .(ANIO_FUENTE, cuadrante)][order(ANIO_FUENTE, cuadrante)]

tasas <- Reduce(merge, list(total_por_anio, raras_por_anio, poblacion)) |> setDT()
tasas[, `:=`(
  tasa_total = total_muertes / poblacion * 1e5,
  tasa_raras = muertes_raras / poblacion * 1e5,
  pct_raras  = muertes_raras / total_muertes * 100
)][order(ANIO_FUENTE)]

# Tasas por cuadrante por separado
tasas_cuadrantes <- merge(raras_anio_cuadrantes, poblacion, by = "ANIO_FUENTE")
tasas_cuadrantes[, tasa_cuadrantes := muertes_cuadrantes / poblacion * 1e5]

# AMPLIADO — mismo cálculo, objeto paralelo, usando defunciones_raras_cuadrantes_ampliado
raras_anio_cuadrantes_ampliado <- defunciones_raras_cuadrantes_ampliado[, .(muertes_cuadrantes = .N), by = .(ANIO_FUENTE, cuadrante)][order(ANIO_FUENTE, cuadrante)]

tasas_cuadrantes_ampliado <- merge(raras_anio_cuadrantes_ampliado, poblacion, by = "ANIO_FUENTE")
tasas_cuadrantes_ampliado[, tasa_cuadrantes := muertes_cuadrantes / poblacion * 1e5]

wb <- createWorkbook()

addWorksheet(wb, "tasas_generales")
writeData(wb, "tasas_generales", tasas)

addWorksheet(wb, "tasas_cuadrantes")
writeData(wb, "tasas_cuadrantes", tasas_cuadrantes)

saveWorkbook(wb, file.path(datos_path, "tasas_mortalidad_raras.xlsx"), overwrite = TRUE)

# Gráfica de tasas nacionales
tasas_long <- rbind(
  tasas[, .(ANIO_FUENTE, tasa = tasa_raras, tipo = "Enfermedades raras")],
  tasas[, .(ANIO_FUENTE, tasa = tasa_total, tipo = "Total")]
)

p1_tasa_mortalidad_nacional <- ggplot(tasas_long, aes(x = ANIO_FUENTE, y = tasa, color = tipo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 2014:2024) +
  labs(
    title = "Tasa de mortalidad por 100,000 habitantes, México 2014–2024",
    x = NULL, y = "Muertes por 100,000 hab.",
    color = NULL
  ) +
  facet_wrap(~tipo, scales = "free_y") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
p1_tasa_mortalidad_nacional
ggsave(file.path(datos_path, "analisis", "tasa mortalidad nacional.jpeg"),
       plot = p1_tasa_mortalidad_nacional, width = 10, height = 5, dpi = 300)

p2_tasa_mortalidad_cuadrante <- ggplot(tasas_cuadrantes, aes(x = ANIO_FUENTE, y = tasa_cuadrantes, color = cuadrante)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 2014:2024) +
  labs(
    title = "Tasa de mortalidad por enfermedades raras por cuadrante\nMéxico 2014–2024",
    x = NULL, y = "Muertes por 100,000 hab.",
    color = "Cuadrante"
  ) +
  facet_wrap(~cuadrante, scales = "free_y") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
p2_tasa_mortalidad_cuadrante
ggsave(file.path(datos_path, "analisis", "tasa mortalidad cuadrante.jpeg"),
       plot = p2_tasa_mortalidad_cuadrante, width = 12, height = 8, dpi = 300)

library(ggrepel)

p3_tasa_mortalidad_cuadrante_2024 <- tasas_cuadrantes[ANIO_FUENTE == 2024] |>
  ggplot(aes(x = cuadrante, y = tasa_cuadrantes, fill = cuadrante)) +
  geom_col() +
  geom_text(aes(label = round(tasa_cuadrantes, 2)), vjust = -0.5, size = 3.5) +
  labs(
    title = "Tasa de mortalidad por enfermedades raras por cuadrante\nMéxico 2024",
    x = "Cuadrante", y = "Muertes por 100,000 hab.",
    fill = "Cuadrante"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
p3_tasa_mortalidad_cuadrante_2024
ggsave(file.path(datos_path, "analisis", "tasa mortalidad cuadrante 2024.jpeg"),
       plot = p3_tasa_mortalidad_cuadrante_2024, width = 8, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# 5. Mapa estatal (acumulado 2014–2024)
# -----------------------------------------------------------------------------

# Shapefile de estados de México (GADM)
# Menor 30: se descarga UNA vez a Datos/ y despues se lee del disco (la corrida ya no
# depende de internet si el archivo existe; si no hay internet, copiar el .json a mano).
gadm_url   <- "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_MEX_1.json"
gadm_local <- file.path(datos_path, "gadm41_MEX_1.json")
if (!file.exists(gadm_local)) {
  message("Descargando GADM 4.1 (estados de Mexico) a ", gadm_local)
  utils::download.file(gadm_url, gadm_local, mode = "wb", quiet = TRUE)
}
mx_states <- st_read(gadm_local, quiet = TRUE)

# Puente entre claves INEGI y nombres GADM
gadm_bridge <- data.frame(
  NAME_1 = c("Aguascalientes","BajaCalifornia","BajaCaliforniaSur",
             "Campeche","Coahuila","Colima","Chiapas","Chihuahua",
             "DistritoFederal","Durango","Guanajuato","Guerrero",
             "Hidalgo","Jalisco","M\u00e9xico","Michoac\u00e1n","Morelos","Nayarit",
             "NuevoLe\u00f3n","Oaxaca","Puebla","Quer\u00e9taro","QuintanaRoo",
             "SanLuisPotos\u00ed","Sinaloa","Sonora","Tabasco","Tamaulipas",
             "Tlaxcala","Veracruz","Yucat\u00e1n","Zacatecas"),
  ENT_RESID = sprintf("%02d", 1:32)
)
# CORRECCION 14-oct-2026: nombres con acento escritos con escapes \u (inmunes a la
# codificacion del archivo). Con codificacion equivocada el merge por NAME_1 perdia
# 6 estados (Mexico, Michoacan, Nuevo Leon, Queretaro, San Luis Potosi, Yucatan).
stopifnot("gadm_bridge no coincide con los 32 NAME_1 de GADM" =
            all(gadm_bridge$NAME_1 %in% mx_states$NAME_1) && nrow(mx_states) == 32)

# Población por entidad: `pop_estado` de la sección 1b (CONAPO, media anual 2014–2024).

estados_validos <- sprintf("%02d", 1:32)

raras_estado <- defunciones_raras[ENT_RESID %in% estados_validos,
                                  .(muertes_raras = .N), by = ENT_RESID]
total_estado <- defunciones[ENT_RESID %in% estados_validos,
                            .(total_muertes = .N), by = ENT_RESID]

tasas_estado <- Reduce(merge, list(raras_estado, total_estado, pop_estado)) |> setDT()
tasas_estado[, `:=`(
  tasa_raras       = muertes_raras / poblacion * 1e5,   # acumulada 2014–2024
  tasa_raras_anual = muertes_raras / poblacion * 1e5 / n_anios,
  pct_raras        = muertes_raras / total_muertes * 100
)]

mx_map <- merge(mx_states, gadm_bridge, by = "NAME_1") |>
  merge(tasas_estado, by = "ENT_RESID")

p4_mapa_mortalidad_acumulado <- ggplot(mx_map) +
  geom_sf(aes(fill = tasa_raras), color = "white", linewidth = 0.3) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       name = "Tasa por\n100,000 hab.") +
  labs(
    title    = "Tasa de mortalidad por enfermedades raras",
    subtitle = "Por estado de residencia, México 2014–2024 (acumulado)"
  ) +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
p4_mapa_mortalidad_acumulado
ggsave(file.path(datos_path, "analisis", "mapa mortalidad acumulado 2014-2024.jpeg"),
       plot = p4_mapa_mortalidad_acumulado, width = 8, height = 6, dpi = 300, bg = "white")

# -----------------------------------------------------------------------------
# 6. Mapa estatal – solo 2024
# -----------------------------------------------------------------------------

# pop_2024: población CONAPO 2024 por entidad (sección 1b)

raras_2024 <- defunciones_raras[ANIO_FUENTE == 2024 & ENT_RESID %in% estados_validos,
                                .(muertes_raras = .N), by = ENT_RESID]
total_2024 <- defunciones[ANIO_FUENTE == 2024 & ENT_RESID %in% estados_validos,
                          .(total_muertes = .N), by = ENT_RESID]

tasas_2024 <- Reduce(merge, list(raras_2024, total_2024, pop_2024)) |> setDT()
tasas_2024[, tasa_raras := muertes_raras / poblacion * 1e5]

mx_map_2024 <- merge(mx_states, gadm_bridge, by = "NAME_1") |>
  merge(tasas_2024, by = "ENT_RESID")

p5_mapa_mortalidad_2024 <- ggplot(mx_map_2024) +
  geom_sf(aes(fill = tasa_raras), color = "white", linewidth = 0.3) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       name = "Tasa por\n100,000 hab.") +
  labs(
    title    = "Tasa de mortalidad por enfermedades raras, 2024",
    subtitle = "Por estado de residencia"
  ) +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
p5_mapa_mortalidad_2024
ggsave(file.path(datos_path, "analisis", "mapa mortalidad 2024.jpeg"),
       plot = p5_mapa_mortalidad_2024, width = 8, height = 6, dpi = 300, bg = "white")

# -----------------------------------------------------------------------------
# 7. Mapas por sexo – 2024
# -----------------------------------------------------------------------------

# SEXO: 1 = Hombre, 2 = Mujer, 9 = No especificado (excluido)
raras_sexo_2024 <- defunciones_raras[
  ANIO_FUENTE == 2024 & ENT_RESID %in% estados_validos & SEXO %in% c(1, 2),
  .(muertes_raras = .N), by = .(ENT_RESID, SEXO)
]

# Población por entidad y sexo, CONAPO 2024 (sección 1b)
pop_sexo <- copy(pob_sexo_2024)
raras_sexo_2024[, SEXO := as.integer(SEXO)]

tasas_sexo <- merge(raras_sexo_2024,
                    pop_sexo[, .(ENT_RESID, SEXO, pob_sexo)],
                    by = c("ENT_RESID", "SEXO"))
tasas_sexo[, `:=`(
  tasa_raras  = muertes_raras / pob_sexo * 1e5,
  sexo_label  = ifelse(SEXO == 1, "Hombres", "Mujeres")
)]

mx_sexo_map <- merge(mx_states, gadm_bridge, by = "NAME_1") |>
  merge(tasas_sexo, by = "ENT_RESID", allow.cartesian = TRUE) |> st_as_sf()

lims <- range(tasas_sexo$tasa_raras)

p6_mapa_mortalidad_sexo_2024 <- ggplot(mx_sexo_map) +
  geom_sf(aes(fill = tasa_raras), color = "white", linewidth = 0.2) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       limits = lims,
                       name = "Tasa por\n100,000 hab.") +
  facet_wrap(~sexo_label) +
  labs(
    title    = "Tasa de mortalidad por enfermedades raras, 2024",
    subtitle = "Por sexo y estado de residencia"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title  = element_text(face = "bold"),
    strip.text  = element_text(face = "bold", size = 12)
  )
p6_mapa_mortalidad_sexo_2024
ggsave(file.path(datos_path, "analisis", "mapa mortalidad 2024 sexo.jpeg"),
       plot = p6_mapa_mortalidad_sexo_2024, width = 10, height = 6, dpi = 300, bg = "white")

# -----------------------------------------------------------------------------
# 8. Mapa de razón hombre/mujer – 2024
# -----------------------------------------------------------------------------

ratio_estado <- dcast(tasas_sexo, ENT_RESID ~ SEXO, value.var = "tasa_raras")
setnames(ratio_estado, c("ENT_RESID", "tasa_H", "tasa_M"))
ratio_estado[, ratio_HM := tasa_H / tasa_M]

mx_ratio_map <- merge(mx_states, gadm_bridge, by = "NAME_1") |>
  merge(ratio_estado, by = "ENT_RESID") |> st_as_sf()

p7_mapa_razon_mortalidad_sexo <- ggplot(mx_ratio_map) +
  geom_sf(aes(fill = ratio_HM), color = "white", linewidth = 0.3) +
  scale_fill_gradient2(
    low      = "#4575b4",   # azul  = exceso femenino
    mid      = "#ffffbf",   # amarillo = paridad
    high     = "#d73027",   # rojo  = exceso masculino
    midpoint = 1,
    name     = "Razón\nH/M",
    limits   = c(0.85, 1.20),
    oob      = scales::squish
  ) +
  labs(
    title    = "Razón de mortalidad por enfermedades raras hombre/mujer, 2024",
    subtitle = "Rojo = exceso masculino · Azul = exceso femenino · Amarillo = paridad"
  ) +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
p7_mapa_razon_mortalidad_sexo
ggsave(file.path(datos_path, "analisis", "mapa razon mortalidad sexo.jpeg"),
       plot = p7_mapa_razon_mortalidad_sexo, width = 8, height = 6, dpi = 300, bg = "white")

gc()
# =============================================================================
# EGRESOS HOSPITALARIOS
# =============================================================================

egresos_path <- file.path(datos_path, "egresos")

# Metadatos: nombre del zip, archivo interno y separador por año
# Nota: CEDOCVE = clave de entidad (estado); MOTEGRE/MOTIVO_EGRESO = motivo de egreso
# MOTEGRE == "5" = defunción intrahospitalaria (se excluye)
year_meta_egr <- list(
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
  list(yr=2024, zip="sectorial_egresos_2024.zip", file="Egresos_Sectorial_2024.txt", sep="|")
)

# -----------------------------------------------------------------------------
# 9. Cargar egresos 2014–2024
# -----------------------------------------------------------------------------

load_egreso <- function(m) {
  fp <- file.path(tmp, m$file)
  if (!file.exists(fp))
    unzip(file.path(egresos_path, m$zip), files = m$file, exdir = tmp)
  
  hdr    <- names(fread(fp, sep = m$sep, nrows = 0))
  hdr_up <- toupper(hdr)
  
  col_map <- list(
    AFECPRIN4 = "AFECPRIN4",
    SEXO      = "SEXO",
    ENTIDAD   = "CEDOCVE",
    DIAS_ESTA = "DIAS_ESTA",
    MOTEGRE   = c("MOTEGRE", "MOTIVO_EGRESO")
  )
  
  sel <- sapply(col_map, function(opts) {
    idx <- match(toupper(opts), hdr_up)
    hdr[idx[!is.na(idx)][1]]
  })
  sel <- sel[!is.na(sel)]
  
  # CORRECCION 14-oct-2026: el archivo 2015_Sectorial.csv tiene 10 campos en las primeras
  # 5,242,876 lineas y 11 (coma final) en las 1,017,280 restantes (registros de la SSA).
  # Con fill = TRUE fread se detenia en la linea 5,242,877 y descartaba ~1.0 millon de
  # egresos de 2015, sin error. fill = Inf lee el archivo completo; ademas, cualquier
  # parada anticipada se convierte en error.
  dt <- withCallingHandlers(
    fread(fp, sep = m$sep, select = unname(sel),
          fill = if (m$yr == 2015) Inf else TRUE,
          colClasses = "character", showProgress = FALSE),
    warning = function(w) {
      if (grepl("Stopped early|Se detuvo anticipadamente", conditionMessage(w)))
        stop("fread se detuvo antes de terminar ", m$file, ": ", conditionMessage(w), call. = FALSE)
    })
  setnames(dt, unname(sel), names(sel)[match(unname(sel), sel)])
  dt[, ANIO := m$yr]
  
  # Filtrar y agregar ANTES de retornar
  dt <- dt[MOTEGRE != "5"]  # excluir defunciones intrahospitalarias
  dt[, ENTIDAD := sprintf("%02d", as.integer(ENTIDAD))]
  
  message(m$yr, ": ", format(nrow(dt), big.mark = ","), " rows tras filtro")
  gc()
  dt
}

egresos_list <- lapply(year_meta_egr, load_egreso)
egresos_vivos <- rbindlist(egresos_list, fill = TRUE, use.names = TRUE)
rm(egresos_list); gc()
# -----------------------------------------------------------------------------
# 10. Filtrar egresos por enfermedades raras
# -----------------------------------------------------------------------------

egresos_raras <- egresos_vivos[AFECPRIN4 %in% codigos_raras]

cat("Egresos por E. raras:  ", format(nrow(egresos_raras), big.mark=","), "\n")
cat("Porcentaje:            ", round(nrow(egresos_raras)/nrow(egresos_vivos)*100, 2), "%\n\n")

egresos_raras[, .N, by=ANIO][order(ANIO)]

# -----------------------------------------------------------------------------
# 11. Tasas de egresos por 100,000 habitantes (nivel nacional)
# -----------------------------------------------------------------------------

# Merge egresos con cuadrantes — OBJETO APARTE (no sobrescribe egresos_raras),
# igual que defunciones_raras / defunciones_raras_cuadrantes más arriba.
# Antes esta línea reasignaba `egresos_raras`, con lo que todo lo que sigue en
# el script (tendencia nacional, mapa estatal acumulado, mapas por sexo,
# comparación top 25 — ninguno "por cuadrante" en su título) terminaba
# corriendo sobre el subconjunto de códigos con cuadrante asignado en
# matriz_clasificacion_v01.csv, no sobre el catálogo completo de raras
# (codigos_raras) como sí ocurre en la mortalidad. Ver nota en el resumen
# del proyecto (metodologia/analisis-geografico-defunciones-egresos.md).
egresos_raras_cuadrantes <- egresos_vivos[AFECPRIN4 %in% codigos_raras_cuadrantes$cie10][
  codigos_raras_cuadrantes, on = .(AFECPRIN4 = cie10), nomatch = 0
]

# AMPLIADO — objeto paralelo, mismo patrón, no sobrescribe egresos_raras_cuadrantes.
egresos_raras_cuadrantes_ampliado <- egresos_vivos[AFECPRIN4 %in% codigos_raras_cuadrantes_ampliado$cie10][
  codigos_raras_cuadrantes_ampliado, on = .(AFECPRIN4 = cie10), nomatch = 0
]

# Contar egresos por año y cuadrante
egresos_anio_cuadrantes <- egresos_raras_cuadrantes[, .(egresos_cuadrantes = .N),
                                                   by = .(ANIO, cuadrante)][order(ANIO, cuadrante)]

# Merge con población (ajusta el nombre del año si es distinto)
tasas_egresos_cuadrantes <- merge(egresos_anio_cuadrantes, poblacion,
                                  by.x = "ANIO", by.y = "ANIO_FUENTE")
tasas_egresos_cuadrantes[, tasa_egresos := egresos_cuadrantes / poblacion * 1e5]

# AMPLIADO — mismo cálculo, objeto paralelo
egresos_anio_cuadrantes_ampliado <- egresos_raras_cuadrantes_ampliado[, .(egresos_cuadrantes = .N),
                                                                     by = .(ANIO, cuadrante)][order(ANIO, cuadrante)]

tasas_egresos_cuadrantes_ampliado <- merge(egresos_anio_cuadrantes_ampliado, poblacion,
                                           by.x = "ANIO", by.y = "ANIO_FUENTE")
tasas_egresos_cuadrantes_ampliado[, tasa_egresos := egresos_cuadrantes / poblacion * 1e5]

# Figura por cuadrante (paneles separados)
p8_tasa_egresos_cuadrante <- ggplot(tasas_egresos_cuadrantes, aes(x = ANIO, y = tasa_egresos, color = cuadrante)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 2014:2024) +
  labs(
    title = "Tasa de egresos hospitalarios por enfermedades raras por cuadrante\nMéxico 2014–2024",
    x = NULL, y = "Egresos por 100,000 hab.",
    color = "Cuadrante"
  ) +
  facet_wrap(~cuadrante, scales = "free_y") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
p8_tasa_egresos_cuadrante
ggsave(file.path(datos_path, "analisis", "tasa egresos cuadrante.jpeg"),
       plot = p8_tasa_egresos_cuadrante, width = 12, height = 8, dpi = 300)

# Reusar tabla `poblacion` obtenida del Banco Mundial en sección 4
total_egr_anio <- egresos_vivos[, .(total_egresos = .N), by=.(ANIO_FUENTE = ANIO)]
raras_egr_anio <- egresos_raras[, .(egresos_raras = .N), by=.(ANIO_FUENTE = ANIO)]

tasas_egr <- Reduce(merge, list(total_egr_anio, raras_egr_anio, poblacion)) |> setDT()
tasas_egr[, `:=`(
  tasa_total = total_egresos / poblacion * 1e5,
  tasa_raras = egresos_raras / poblacion * 1e5,
  pct_raras  = egresos_raras / total_egresos * 100
)][order(ANIO_FUENTE)]

# Gráfica
tasas_egr_long <- rbind(
  tasas_egr[, .(ANIO_FUENTE, tasa = tasa_raras, tipo = "Enfermedades raras")],
  tasas_egr[, .(ANIO_FUENTE, tasa = tasa_total, tipo = "Total")]
)

p9_tasa_egresos_nacional <- ggplot(tasas_egr_long, aes(x = ANIO_FUENTE, y = tasa, color = tipo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 2014:2024) +
  labs(
    title    = "Tasa de egresos hospitalarios por 100,000 habitantes, México 2014–2024",
    subtitle = "Excluye egresos por defunción (MOTEGRE=5)",
    x = NULL, y = "Egresos por 100,000 hab.", color = NULL
  ) +
  facet_wrap(~tipo, scales = "free_y") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
p9_tasa_egresos_nacional
ggsave(file.path(datos_path, "analisis", "tasa egresos nacional.jpeg"),
       plot = p9_tasa_egresos_nacional, width = 10, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# 12. Mapa estatal de egresos – acumulado 2014–2024
# -----------------------------------------------------------------------------

# Reusar pop_estado y gadm_bridge de sección 5
# Nota: en egresos la variable de estado es ENTIDAD (= CEDOCVE)
gadm_bridge_egr <- gadm_bridge
names(gadm_bridge_egr)[2] <- "ENTIDAD"   # renombrar para el join

raras_estado_egr <- egresos_raras[ENTIDAD %in% estados_validos,
                                  .(egresos_raras = .N), by=ENTIDAD]
total_estado_egr <- egresos_vivos[ENTIDAD %in% estados_validos,
                                  .(total_egresos = .N), by=ENTIDAD]

pop_estado_egr <- pop_estado
names(pop_estado_egr)[1] <- "ENTIDAD"

tasas_estado_egr <- Reduce(merge, list(raras_estado_egr, total_estado_egr, pop_estado_egr)) |> setDT()
tasas_estado_egr[, `:=`(
  tasa_raras       = egresos_raras / poblacion * 1e5,   # acumulada 2014–2024
  tasa_raras_anual = egresos_raras / poblacion * 1e5 / n_anios
)]

mx_map_egr <- merge(mx_states, gadm_bridge_egr, by="NAME_1") |>
  merge(tasas_estado_egr, by="ENTIDAD")

p10_mapa_egresos_acumulado <- ggplot(mx_map_egr) +
  geom_sf(aes(fill = tasa_raras), color="white", linewidth=0.3) +
  scale_fill_distiller(palette="YlOrRd", direction=1,
                       name="Tasa por\n100,000 hab.") +
  labs(title    = "Tasa de egresos por enfermedades raras",
       subtitle = "Por entidad de la unidad médica (CEDOCVE), México 2014–2024 (excluye defunciones)") +
  theme_void(base_size=12) +
  theme(plot.title=element_text(face="bold"))
p10_mapa_egresos_acumulado
ggsave(file.path(datos_path, "analisis", "mapa egresos acumulado 2014-2024.jpeg"),
       plot = p10_mapa_egresos_acumulado, width = 8, height = 6, dpi = 300, bg = "white")

# -----------------------------------------------------------------------------
# 13. Mapas de egresos por sexo – 2024
# -----------------------------------------------------------------------------

# Población CONAPO 2024 por entidad (sección 1b)

raras_sexo_egr <- egresos_raras[
  ANIO == 2024 & ENTIDAD %in% estados_validos & SEXO %in% c("1","2"),
  .(egresos_raras = .N), by=.(ENTIDAD, SEXO)
]

pop_sexo_egr <- copy(pob_sexo_2024)
setnames(pop_sexo_egr, "ENT_RESID", "ENTIDAD")
pop_sexo_egr[, SEXO := as.character(SEXO)]   # en egresos SEXO es texto "1"/"2"

tasas_sexo_egr <- merge(raras_sexo_egr,
                        pop_sexo_egr[, .(ENTIDAD, SEXO, pob_sexo)],
                        by=c("ENTIDAD","SEXO"))
tasas_sexo_egr[, `:=`(
  tasa_raras = egresos_raras / pob_sexo * 1e5,
  sexo_label = ifelse(SEXO=="1", "Hombres", "Mujeres")
)]

mx_sexo_egr <- merge(mx_states, gadm_bridge_egr, by="NAME_1") |>
  merge(tasas_sexo_egr, by="ENTIDAD", allow.cartesian=TRUE) |> st_as_sf()

lims_egr <- range(tasas_sexo_egr$tasa_raras)

p11_mapa_egresos_sexo_2024 <- ggplot(mx_sexo_egr) +
  geom_sf(aes(fill = tasa_raras), color="white", linewidth=0.2) +
  scale_fill_distiller(palette="YlOrRd", direction=1,
                       limits=lims_egr, name="Tasa por\n100,000 hab.") +
  facet_wrap(~sexo_label) +
  labs(title    = "Tasa de egresos por enfermedades raras, 2024",
       subtitle = "Por sexo y entidad de la unidad médica (excluye defunciones)") +
  theme_void(base_size=12) +
  theme(plot.title = element_text(face="bold"),
        strip.text = element_text(face="bold", size=12))
p11_mapa_egresos_sexo_2024
ggsave(file.path(datos_path, "analisis", "mapa egresos 2024 sexo.jpeg"),
       plot = p11_mapa_egresos_sexo_2024, width = 10, height = 6, dpi = 300, bg = "white")

# -----------------------------------------------------------------------------
# 14. Comparación de códigos: egresos vs. defunciones
# -----------------------------------------------------------------------------

# Una sola etiqueta de nombre por código CIE (la más corta disponible)
nombres <- as.data.table(orphanet_catalogo)[
  , .(nombre = nombre[which.min(nchar(nombre))]), by = causa
]

# Conteos por código en cada fuente
egr_cod <- egresos_raras[,     .(egresos = .N), by=.(causa = AFECPRIN4)]
def_cod <- defunciones_raras[, .(muertes = .N), by=.(causa = CAUSA_DEF)]

# Unión y tasa de letalidad proxy (muertes / total de eventos)
comp <- merge(egr_cod, def_cod, by="causa", all=TRUE)
comp <- merge(comp, nombres, by="causa", all.x=TRUE)
comp[is.na(egresos), egresos := 0L]
comp[is.na(muertes), muertes := 0L]
comp[, `:=`(
  total     = egresos + muertes,
  cfr_proxy = round(muertes / (egresos + muertes) * 100, 1)
)]
comp[is.na(nombre), nombre := causa]

# Top 25 por volumen total para el gráfico
top25 <- comp[order(-total)][1:25]
top25[, etiqueta := ifelse(nchar(nombre) > 45,
                           paste0(substr(nombre,1,43), "…"), nombre)]
top25[, etiqueta := paste0(causa, " – ", etiqueta)]
top25[, etiqueta := reorder(etiqueta, egresos)]

tot_egr <- nrow(egresos_raras)
tot_def <- nrow(defunciones_raras)

top25[, pct_egr := egresos / tot_egr * 100]
top25[, pct_def := muertes / tot_def * 100]

top25_long <- melt(top25, id.vars=c("etiqueta","cfr_proxy"),
                   measure.vars=c("pct_egr","pct_def"),
                   variable.name="fuente", value.name="pct")
top25_long[, fuente := ifelse(fuente=="pct_egr", "Egresos vivos", "Defunciones")]

p12_comparacion_top25 <- ggplot(top25_long, aes(x=pct, y=etiqueta, color=fuente, shape=fuente)) +
  geom_line(aes(group=etiqueta), color="grey70", linewidth=0.6) +
  geom_point(size=3) +
  scale_color_manual(values=c("Egresos vivos"="#2166ac", "Defunciones"="#d73027")) +
  scale_shape_manual(values=c("Egresos vivos"=16, "Defunciones"=17)) +
  labs(
    title    = "Peso relativo de los principales códigos de enfermedades raras",
    subtitle = "Top 25 por volumen total · Azul = egresos vivos · Rojo = defunciones",
    x = "% del total de eventos por fuente", y = NULL, color=NULL, shape=NULL
  ) +
  theme_minimal(base_size=11) +
  theme(legend.position="top",
        axis.text.y=element_text(size=8))
p12_comparacion_top25
ggsave(file.path(datos_path, "analisis", "comparacion egresos defunciones top25.jpeg"),
       plot = p12_comparacion_top25, width = 9, height = 10, dpi = 300)

# --- Top 10 defunciones por cuadrante ---
# CORRECCIÓN: antes agrupaba por `cuadrante` sobre `defunciones_raras`, que no
# tiene esa columna (solo la tiene `defunciones_raras_cuadrantes`, que ya
# viene fusionada con la matriz). Con datos limpios esto habría marcado error
# ("object 'cuadrante' not found"); si corrió antes fue probablemente porque
# quedó una variable `cuadrante` suelta de otra corrida en la sesión de R.
top10_def <- defunciones_raras_cuadrantes[, .(defunciones = .N), by = .(cuadrante, CAUSA_DEF)]
setorder(top10_def, cuadrante, -defunciones)
top10_def <- top10_def[, .SD[1:10], by = cuadrante]

# Pegar nombre
top10_def <- merge(top10_def,
                   codigos_raras_cuadrantes[, c("cie10", "nombre_es")],  # ajusta nombres
                   by.x = "CAUSA_DEF", by.y = "cie10",
                   all.x = TRUE)
setorder(top10_def, cuadrante, -defunciones)

# --- Top 10 egresos por cuadrante ---
# Misma corrección: usar egresos_raras_cuadrantes (con columna cuadrante),
# no egresos_raras (la versión general, sin cuadrante, desde el fix de la
# sección 10-11).
top10_egr <- egresos_raras_cuadrantes[, .(egresos = .N), by = .(cuadrante, AFECPRIN4)]
setorder(top10_egr, cuadrante, -egresos)
top10_egr <- top10_egr[, .SD[1:10], by = cuadrante]

top10_egr <- merge(top10_egr,
                   codigos_raras_cuadrantes[, c("cie10", "nombre_es")],  # ajusta nombres
                   by.x = "AFECPRIN4", by.y = "cie10",
                   all.x = TRUE)
setorder(top10_egr, cuadrante, -egresos)# --- Exportar ---
wb <- createWorkbook()

for (cdr in unique(top10_def$cuadrante)) {
  addWorksheet(wb, paste0("Def_", cdr))
  writeData(wb, paste0("Def_", cdr), top10_def[cuadrante == cdr])
  
  addWorksheet(wb, paste0("Egr_", cdr))
  writeData(wb, paste0("Egr_", cdr), top10_egr[cuadrante == cdr])
}

saveWorkbook(wb, file.path(datos_path, "top10_causas_cuadrantes.xlsx"), overwrite = TRUE)

# -----------------------------------------------------------------------------
# 15. Tablas de resultados para el manuscrito
# -----------------------------------------------------------------------------
# Consolidado de lo que se armó en la conversación "Generar código para R" de
# este proyecto (tablas_manuscrito.xlsx / _v2.xlsx), integrado aquí con la
# exclusión de códigos de baja especificidad ya aplicada arriba (sección 3) y
# reutilizando los objetos ya corregidos de este script (tasas, tasas_cuadrantes,
# tasas_egr, tasas_egresos_cuadrantes, top10_def, top10_egr) — no hace falta
# recalcularlos, ya reflejan la exclusión porque `codigos_raras_cuadrantes` se
# filtró en la fuente.

# --- 15.1 Resumen general defunciones y egresos ---
# A petición de JP se reportan AMBAS cifras lado a lado, no solo una:
#   - "catálogo general" (codigos_raras / defunciones_raras / egresos_raras):
#     el catálogo completo de enfermedades raras, sin restringir a lo que
#     tiene cuadrante asignado en matriz_clasificacion_v01.csv.
#   - "restringido a cuadrante" (defunciones_raras_cuadrantes / egresos_raras_cuadrantes):
#     solo los códigos que además tienen cuadrante asignado, ya con la
#     exclusión de K746/C509/C349 aplicada.
# Si matriz_clasificacion_v01.csv no cubre el 100% del catálogo general, la
# cifra restringida a cuadrante será menor — es justo lo que se quiere poder
# ver de un vistazo, no solo asumir.
resumen_general <- data.frame(
  indicador = c(
    "Total defunciones 2014-2024",
    "Defunciones por ER — catálogo general",
    "% defunciones por ER — catálogo general",
    "Defunciones por ER — restringido a cuadrante (excl. baja especificidad)",
    "% defunciones por ER — restringido a cuadrante",
    "Total egresos vivos 2014-2024",
    "Egresos por ER — catálogo general",
    "% egresos por ER — catálogo general",
    "Egresos por ER — restringido a cuadrante (excl. baja especificidad)",
    "% egresos por ER — restringido a cuadrante"
  ),
  valor = c(
    nrow(defunciones),
    nrow(defunciones_raras),
    round(nrow(defunciones_raras) / nrow(defunciones) * 100, 2),
    nrow(defunciones_raras_cuadrantes),
    round(nrow(defunciones_raras_cuadrantes) / nrow(defunciones) * 100, 2),
    nrow(egresos_vivos),
    nrow(egresos_raras),
    round(nrow(egresos_raras) / nrow(egresos_vivos) * 100, 2),
    nrow(egresos_raras_cuadrantes),
    round(nrow(egresos_raras_cuadrantes) / nrow(egresos_vivos) * 100, 2)
  )
)

# --- 15.2 Tasas nacionales por año (defunciones y egresos) ---
# tasas, tasas_cuadrantes ya existen de la sección 4
# tasas_egr, tasas_egresos_cuadrantes ya existen de la sección 11

# --- 15.3 Tasas por sexo a nivel nacional — defunciones ---
raras_sexo_nac <- defunciones_raras[
  SEXO %in% c(1, 2), .(muertes_raras = .N), by = .(ANIO_FUENTE, SEXO)
]
total_sexo_nac <- defunciones[
  SEXO %in% c(1, 2), .(total_muertes = .N), by = .(ANIO_FUENTE, SEXO)
]
pop_sexo_nac <- copy(pob_nac_sexo)   # CONAPO por año y sexo (sección 1b)
raras_sexo_nac[, SEXO := as.integer(SEXO)]
total_sexo_nac[, SEXO := as.integer(SEXO)]
tasas_sexo_nac <- Reduce(merge, list(raras_sexo_nac, total_sexo_nac,
                                     pop_sexo_nac[, .(ANIO_FUENTE, SEXO, pob_sexo)])) |> setDT()
tasas_sexo_nac[, `:=`(
  tasa_raras = muertes_raras / pob_sexo * 1e5,
  tasa_total = total_muertes / pob_sexo * 1e5,
  pct_raras  = muertes_raras / total_muertes * 100,
  sexo_label = ifelse(SEXO == 1, "Hombres", "Mujeres")
)][order(ANIO_FUENTE, SEXO)]

# --- 15.4 Tasas por sexo a nivel nacional — egresos ---
# CORRECCIÓN vs. la versión de "Generar código para R": ahí este bloque usaba
# `egresos_raras` (que en ese script se sobreescribía con el subconjunto de
# cuadrantes — el mismo bug que ya corregimos en este script). Aquí se usa
# `egresos_raras_cuadrantes`, que es el objeto correcto para todo lo que va
# desglosado por cuadrante o filtrado por la exclusión de baja especificidad.
raras_sexo_egr_nac <- egresos_raras_cuadrantes[
  SEXO %in% c("1", "2"), .(egresos_raras = .N), by = .(ANIO_FUENTE = ANIO, SEXO)
]
total_sexo_egr_nac <- egresos_vivos[
  SEXO %in% c("1", "2"), .(total_egresos = .N), by = .(ANIO_FUENTE = ANIO, SEXO)
]
pop_sexo_egr_nac <- copy(pob_nac_sexo)   # CONAPO por año y sexo (sección 1b)
pop_sexo_egr_nac[, SEXO := as.character(SEXO)]
tasas_sexo_egr_nac <- Reduce(merge, list(raras_sexo_egr_nac, total_sexo_egr_nac,
                                         pop_sexo_egr_nac[, .(ANIO_FUENTE, SEXO, pob_sexo)])) |> setDT()
tasas_sexo_egr_nac[, `:=`(
  tasa_raras = egresos_raras / pob_sexo * 1e5,
  tasa_total = total_egresos / pob_sexo * 1e5,
  pct_raras  = egresos_raras / total_egresos * 100,
  sexo_label = ifelse(SEXO == "1", "Hombres", "Mujeres")
)][order(ANIO_FUENTE, SEXO)]

# --- 15.5 Exportar todo a un solo Excel con pestañas ---
# Incluye también Def_X/Egr_X (top 10 por cuadrante, ya calculados arriba en
# la sección 14) — mismo contenido que top10_causas_cuadrantes.xlsx, mantenido
# aquí también para que tablas_manuscrito_v2.xlsx sea un paquete autocontenido.
wb_manuscrito <- createWorkbook()

addWorksheet(wb_manuscrito, "resumen_general")
writeData(wb_manuscrito, "resumen_general", resumen_general)

addWorksheet(wb_manuscrito, "mort_tasas_anuales")
writeData(wb_manuscrito, "mort_tasas_anuales", tasas)

addWorksheet(wb_manuscrito, "mort_tasas_cuadrantes")
writeData(wb_manuscrito, "mort_tasas_cuadrantes", tasas_cuadrantes)

addWorksheet(wb_manuscrito, "mort_tasas_sexo_nac")
writeData(wb_manuscrito, "mort_tasas_sexo_nac", tasas_sexo_nac)

addWorksheet(wb_manuscrito, "egr_tasas_anuales")
writeData(wb_manuscrito, "egr_tasas_anuales", tasas_egr)

addWorksheet(wb_manuscrito, "egr_tasas_cuadrantes")
writeData(wb_manuscrito, "egr_tasas_cuadrantes", tasas_egresos_cuadrantes)

addWorksheet(wb_manuscrito, "egr_tasas_sexo_nac")
writeData(wb_manuscrito, "egr_tasas_sexo_nac", tasas_sexo_egr_nac)

for (cdr in unique(top10_def$cuadrante)) {
  addWorksheet(wb_manuscrito, paste0("Def_", cdr))
  writeData(wb_manuscrito, paste0("Def_", cdr), top10_def[cuadrante == cdr])
  
  addWorksheet(wb_manuscrito, paste0("Egr_", cdr))
  writeData(wb_manuscrito, paste0("Egr_", cdr), top10_egr[cuadrante == cdr])
}

saveWorkbook(wb_manuscrito, file.path(datos_path, "tablas_manuscrito_v2.xlsx"), overwrite = TRUE)
message("Tablas exportadas a Datos/tablas_manuscrito_v2.xlsx")

# -----------------------------------------------------------------------------
# 15bis. AMPLIADO (adicional) — tabla de contraste original vs. ampliado,
#        cuadrantes 2/5/9 (los únicos que pueden diferir; 1,3,4,6,7,8 son
#        idénticos entre original y ampliado por construcción — no se les
#        tocó ningún código, así que recalcularlos sería trabajo redundante).
# -----------------------------------------------------------------------------

cuadrantes_afectados_ampliado <- c(2, 5, 9)

# --- Defunciones y egresos: total acumulado 2014-2024 por cuadrante ---
contraste_def <- merge(
  defunciones_raras_cuadrantes[cuadrante %in% cuadrantes_afectados_ampliado, .(defunciones_original = .N), by = cuadrante],
  defunciones_raras_cuadrantes_ampliado[cuadrante %in% cuadrantes_afectados_ampliado, .(defunciones_ampliado = .N), by = cuadrante],
  by = "cuadrante", all = TRUE
)
contraste_def[, `:=`(
  diferencia = defunciones_ampliado - defunciones_original,
  pct_cambio = round((defunciones_ampliado - defunciones_original) / defunciones_original * 100, 1)
)]
setorder(contraste_def, cuadrante)

contraste_egr <- merge(
  egresos_raras_cuadrantes[cuadrante %in% cuadrantes_afectados_ampliado, .(egresos_original = .N), by = cuadrante],
  egresos_raras_cuadrantes_ampliado[cuadrante %in% cuadrantes_afectados_ampliado, .(egresos_ampliado = .N), by = cuadrante],
  by = "cuadrante", all = TRUE
)
contraste_egr[, `:=`(
  diferencia = egresos_ampliado - egresos_original,
  pct_cambio = round((egresos_ampliado - egresos_original) / egresos_original * 100, 1)
)]
setorder(contraste_egr, cuadrante)

# --- Tasas del último año disponible (2024) por cuadrante ---
contraste_tasa_def <- merge(
  tasas_cuadrantes[ANIO_FUENTE == 2024 & cuadrante %in% cuadrantes_afectados_ampliado, .(cuadrante, tasa_original = tasa_cuadrantes)],
  tasas_cuadrantes_ampliado[ANIO_FUENTE == 2024 & cuadrante %in% cuadrantes_afectados_ampliado, .(cuadrante, tasa_ampliado = tasa_cuadrantes)],
  by = "cuadrante"
)
contraste_tasa_def[, diferencia_tasa := tasa_ampliado - tasa_original]
setorder(contraste_tasa_def, cuadrante)

contraste_tasa_egr <- merge(
  tasas_egresos_cuadrantes[ANIO == 2024 & cuadrante %in% cuadrantes_afectados_ampliado, .(cuadrante, tasa_original = tasa_egresos)],
  tasas_egresos_cuadrantes_ampliado[ANIO == 2024 & cuadrante %in% cuadrantes_afectados_ampliado, .(cuadrante, tasa_ampliado = tasa_egresos)],
  by = "cuadrante"
)
contraste_tasa_egr[, diferencia_tasa := tasa_ampliado - tasa_original]
setorder(contraste_tasa_egr, cuadrante)

# --- Desglose: cuántos casos aporta cada uno de los 8 subcódigos nuevos ---
# (antes de este ajuste, TODOS estos renglones eran 0 en cualquier resultado
# por cuadrante — los 5 códigos "pelón" originales nunca hacían match)
codigos_nuevos_ampliado <- c("E840", "E841", "E848", "D570", "D571", "D66X", "D67X", "G10X")

desglose_def_nuevo <- defunciones_raras_cuadrantes_ampliado[
  CAUSA_DEF %in% codigos_nuevos_ampliado, .(defunciones = .N), by = .(cuadrante, CAUSA_DEF, nombre_es)
]
setorder(desglose_def_nuevo, cuadrante, -defunciones)

desglose_egr_nuevo <- egresos_raras_cuadrantes_ampliado[
  AFECPRIN4 %in% codigos_nuevos_ampliado, .(egresos = .N), by = .(cuadrante, AFECPRIN4, nombre_es)
]
setorder(desglose_egr_nuevo, cuadrante, -egresos)

cat("\n--- AMPLIADO: contraste original vs. ampliado (cuadrantes 2/5/9) ---\n")
print(contraste_def)
print(contraste_egr)
cat("\nDesglose de defunciones nuevas (antes 0 en todos los casos):\n")
print(desglose_def_nuevo)
cat("\nDesglose de egresos nuevos (antes 0 en todos los casos):\n")
print(desglose_egr_nuevo)

# --- Exportar contraste a un Excel APARTE (no modifica tablas_manuscrito_v2.xlsx) ---
wb_ampliado <- createWorkbook()

addWorksheet(wb_ampliado, "contraste_defunciones")
writeData(wb_ampliado, "contraste_defunciones", contraste_def)

addWorksheet(wb_ampliado, "contraste_egresos")
writeData(wb_ampliado, "contraste_egresos", contraste_egr)

addWorksheet(wb_ampliado, "contraste_tasa_def_2024")
writeData(wb_ampliado, "contraste_tasa_def_2024", contraste_tasa_def)

addWorksheet(wb_ampliado, "contraste_tasa_egr_2024")
writeData(wb_ampliado, "contraste_tasa_egr_2024", contraste_tasa_egr)

addWorksheet(wb_ampliado, "desglose_defunciones_nuevas")
writeData(wb_ampliado, "desglose_defunciones_nuevas", desglose_def_nuevo)

addWorksheet(wb_ampliado, "desglose_egresos_nuevos")
writeData(wb_ampliado, "desglose_egresos_nuevos", desglose_egr_nuevo)

saveWorkbook(wb_ampliado, file.path(datos_path, "contraste_ampliado_5_enfermedades.xlsx"), overwrite = TRUE)
message("Tabla de contraste exportada a Datos/contraste_ampliado_5_enfermedades.xlsx")

# =============================================================================
# 15ter. Heterogeneidad estatal 2014 vs. 2024 (Figuras 1 y 2 del manuscrito)
#        Integrada el 24-sep-2026 desde analisis_enfermedades_raras_corregido.R
# =============================================================================
# JP pidió mapas que muestren heterogeneidad de tasas de egresos y
# defunciones por estado, y si se mantiene en el tiempo (2014 vs. 2024) --
# usando la definición por catálogo CIE-10 (Orphanet + Blazsik), no deciles.
#
# Figura a = egresos (2014 | 2024, un panel cada uno, misma escala de color)
# Figura b = defunciones (2014 | 2024, un panel cada uno, misma escala)
#
# Para que 2014 y 2024 sean comparables con la MISMA fuente de población
# (el mapa de 2024 ya existente usa proporciones fijas 2019 reescaladas;
# aquí usamos población CONAPO real por entidad y año, más precisa).

# -----------------------------------------------------------------------------
# 15ter.1 Población CONAPO por entidad y año
# -----------------------------------------------------------------------------

# pob_conapo, pob_estado_anio, pob_2014 y pob_2024 se leen y verifican una sola vez
# en la sección 1b; aquí solo se reutilizan.

# -----------------------------------------------------------------------------
# 15ter.2 Figura b: defunciones por estado, 2014 vs. 2024 (residencia real)
# -----------------------------------------------------------------------------

def_2014 <- defunciones_raras[ANIO_FUENTE == 2014 & ENT_RESID %in% estados_validos,
                               .(muertes_raras = .N), by = ENT_RESID]
tot_def_2014 <- defunciones[ANIO_FUENTE == 2014 & ENT_RESID %in% estados_validos,
                             .(total_muertes = .N), by = ENT_RESID]
tasas_def_2014 <- Reduce(merge, list(def_2014, tot_def_2014, pob_2014)) |> setDT()
tasas_def_2014[, `:=`(tasa_raras = muertes_raras / poblacion * 1e5, ANIO_MAPA = 2014L)]

def_2024b <- defunciones_raras[ANIO_FUENTE == 2024 & ENT_RESID %in% estados_validos,
                                .(muertes_raras = .N), by = ENT_RESID]
tot_def_2024b <- defunciones[ANIO_FUENTE == 2024 & ENT_RESID %in% estados_validos,
                              .(total_muertes = .N), by = ENT_RESID]
tasas_def_2024b <- Reduce(merge, list(def_2024b, tot_def_2024b, pob_2024)) |> setDT()
tasas_def_2024b[, `:=`(tasa_raras = muertes_raras / poblacion * 1e5, ANIO_MAPA = 2024L)]

tasas_def_1424 <- rbind(tasas_def_2014, tasas_def_2024b, fill = TRUE)
tasas_def_1424[, ANIO_MAPA_lbl := factor(ANIO_MAPA, levels = c(2014, 2024))]

mx_map_def_1424 <- merge(mx_states, gadm_bridge, by = "NAME_1") |>
  merge(tasas_def_1424, by = "ENT_RESID", allow.cartesian = TRUE) |> st_as_sf()

lims_def_1424 <- range(tasas_def_1424$tasa_raras)

fig_b_defunciones_1424 <- ggplot(mx_map_def_1424) +
  geom_sf(aes(fill = tasa_raras), color = "white", linewidth = 0.2) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       limits = lims_def_1424, name = "Tasa por\n100,000 hab.") +
  facet_wrap(~ANIO_MAPA_lbl) +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 12))
fig_b_defunciones_1424
ggsave(file.path(datos_path, "analisis", "mapa_defunciones_2014_2024_comparacion.jpeg"),
       fig_b_defunciones_1424, width = 11, height = 6, dpi = 300, bg = "white")

# -----------------------------------------------------------------------------
# 15ter.3 Figura a: egresos por estado, 2014 vs. 2024 (lugar de atención)
# -----------------------------------------------------------------------------

pob_2014_egr <- copy(pob_2014); setnames(pob_2014_egr, "ENT_RESID", "ENTIDAD")
pob_2024_egr_15 <- copy(pob_2024); setnames(pob_2024_egr_15, "ENT_RESID", "ENTIDAD")

egr_2014 <- egresos_raras[ANIO == 2014 & ENTIDAD %in% estados_validos,
                           .(egresos_raras_n = .N), by = ENTIDAD]
tot_egr_2014 <- egresos_vivos[ANIO == 2014 & ENTIDAD %in% estados_validos,
                               .(total_egresos = .N), by = ENTIDAD]
tasas_egr_2014 <- Reduce(merge, list(egr_2014, tot_egr_2014, pob_2014_egr)) |> setDT()
tasas_egr_2014[, `:=`(tasa_raras = egresos_raras_n / poblacion * 1e5, ANIO_MAPA = 2014L)]

egr_2024b <- egresos_raras[ANIO == 2024 & ENTIDAD %in% estados_validos,
                            .(egresos_raras_n = .N), by = ENTIDAD]
tot_egr_2024b <- egresos_vivos[ANIO == 2024 & ENTIDAD %in% estados_validos,
                                .(total_egresos = .N), by = ENTIDAD]
tasas_egr_2024b <- Reduce(merge, list(egr_2024b, tot_egr_2024b, pob_2024_egr_15)) |> setDT()
tasas_egr_2024b[, `:=`(tasa_raras = egresos_raras_n / poblacion * 1e5, ANIO_MAPA = 2024L)]

tasas_egr_1424 <- rbind(tasas_egr_2014, tasas_egr_2024b, fill = TRUE)
tasas_egr_1424[, ANIO_MAPA_lbl := factor(ANIO_MAPA, levels = c(2014, 2024))]

mx_map_egr_1424 <- merge(mx_states, gadm_bridge_egr, by = "NAME_1") |>
  merge(tasas_egr_1424, by = "ENTIDAD", allow.cartesian = TRUE) |> st_as_sf()

lims_egr_1424 <- range(tasas_egr_1424$tasa_raras)

fig_a_egresos_1424 <- ggplot(mx_map_egr_1424) +
  geom_sf(aes(fill = tasa_raras), color = "white", linewidth = 0.2) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       limits = lims_egr_1424, name = "Tasa por\n100,000 hab.") +
  facet_wrap(~ANIO_MAPA_lbl) +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 12))
fig_a_egresos_1424
ggsave(file.path(datos_path, "analisis", "mapa_egresos_2014_2024_comparacion.jpeg"),
       fig_a_egresos_1424, width = 11, height = 6, dpi = 300, bg = "white")

cat("\nListo: mapa_egresos_2014_2024_comparacion.jpeg y mapa_defunciones_2014_2024_comparacion.jpeg guardados en Datos/analisis/\n")

# -----------------------------------------------------------------------------
# 16. Guardar sesión — para no tener que volver a correr todo este script
#     cada vez que se quiera correr analisis geografico adicional.R,
#     analisis_geografico_extendido_parte2.R o analisis_geografico_ssa_residencia.R
# -----------------------------------------------------------------------------
# Solo se guardan las tablas ya filtradas/agregadas que esos scripts usan
# (NO se guardan `defunciones` ni `egresos_vivos` completos — son los objetos
# más pesados de esta sesión, con millones de filas cada uno, y ningún script
# de extensión los necesita; solo usan las versiones ya filtradas por
# enfermedades raras). Esto mantiene el archivo .RData ligero.

objetos_para_guardar <- c(
  # (menor 30) datos_path y egresos_path ya NO se guardan en la sesion: los scripts
  # B2-B4 los toman de config_rutas.R, no de la ruta de quien corrio B1.
  "cie_que_son_raras", "matriz_clasificacion_v0.1",
  "codigos_raras", "codigos_raras_cuadrantes", "codigos_excluir",
  "mx_states", "gadm_bridge",
  "pop_estado", "estados_validos",
  "defunciones_raras_cuadrantes",
  "egresos_raras", "egresos_raras_cuadrantes",
  "tasas_estado", "tasas_estado_egr",
  "resumen_general", "tasas", "tasas_cuadrantes",
  "tasas_egr", "tasas_egresos_cuadrantes",
  "tasas_sexo_nac", "tasas_sexo_egr_nac",
  "top10_def", "top10_egr"
)

save(list = objetos_para_guardar,
     file = file.path(datos_path, "analisis", "sesion_base_raras.RData"))

cat("\nSesión guardada en Datos/analisis/sesion_base_raras.RData\n",
    "En la próxima corrida, en vez de correr todo este script de nuevo, basta con:\n",
    "  load(file.path(datos_path, \"analisis\", \"sesion_base_raras.RData\"))\n",
    "y luego correr directamente analisis geografico adicional.R\n")

# AMPLIADO — guardado en un .RData APARTE (no se agrega nada a
# objetos_para_guardar ni se toca sesion_base_raras.RData). Necesario para que
# analisis geografico adicional.R (sección H) pueda calcular la implicación
# geográfica sin tener que volver a correr todo este script.
objetos_para_guardar_ampliado <- c(
  "codigos_raras_cuadrantes_ampliado",
  "defunciones_raras_cuadrantes_ampliado", "egresos_raras_cuadrantes_ampliado",
  "tasas_cuadrantes_ampliado", "tasas_egresos_cuadrantes_ampliado",
  "contraste_def", "contraste_egr", "contraste_tasa_def", "contraste_tasa_egr",
  "desglose_def_nuevo", "desglose_egr_nuevo"
)

save(list = objetos_para_guardar_ampliado,
     file = file.path(datos_path, "analisis", "sesion_ampliado.RData"))

cat("\nSesión ampliado guardada en Datos/analisis/sesion_ampliado.RData (aparte de sesion_base_raras.RData)\n")