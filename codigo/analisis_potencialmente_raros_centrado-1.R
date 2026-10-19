# =============================================================================
# [18-oct-2026: CEDOCVE normalizada a 2 digitos (32 entidades); ver seccion tras la carga]
# Análisis CENTRADO en la nueva agrupación "Potencialmente raros" vs. "Resto"
# (por decil nacional, desagregado 1-10) — México, egresos Sectorial 2014-2024
#
# QUÉ ES ESTE SCRIPT Y POR QUÉ ES DISTINTO DE analisis_deciles_frecuencia.R:
# Es una versión MÁS LIGERA, centrada solo en la agrupación de 3 niveles que
# rediseñamos el 13-sep (potencialmente_raro / decil_original) y en el
# contenido nuevo pedido el 14-sep: mortalidad y días de estancia por decil
# desagregado (estilo Fig. 2a/2b de Tröster et al. 2023), la corrección de la
# regresión de mortalidad, la consulta pendiente del 29.7%, y un modelo nuevo
# de propensión a "potencialmente raro". AUTOCONTENIDO: no depende de correr
# antes analisis_deciles_frecuencia.R.
#
# AHORRO DE RECURSOS DELIBERADO respecto al script principal: este análisis NO
# necesita el dataset de DEFUNCIONES (certificados de defunción, DBF) ni la
# POBLACIÓN CONAPO -- todo lo pedido (mortalidad hospitalaria, días de
# estancia, el modelo de propensión, la consulta del 29.7%) se construye
# únicamente a partir de egresos_completos (Sectorial). Nos ahorramos la
# lectura de los 11 archivos DBF de defunciones, que en el script principal
# es un costo de I/O comparable al de los propios egresos -- aproximadamente
# la mitad del tiempo de carga total.
#
# CONSUMO DE RECURSOS -- AVISO:
#  - Carga de los 11 años de egresos Sectorial: varios minutos, igual que en
#    el script principal (mismo costo, es el mismo dataset).
#  - El modelo de mortalidad corregido (sección 5) y el modelo de propensión
#    (sección 7) se corren sobre el acumulado 2014-2024 (~60 millones de
#    egresos) -- son los pasos más pesados de este script.
#  - El modelo de propensión requiere el paquete `fixest` (no viene en la
#    instalación base de R) -- instalar con install.packages("fixest") si
#    hace falta. Se eligió fixest en vez de glm() porque absorbe efectos
#    fijos (aquí, por estado) de forma algorítmica en vez de expandir una
#    columna de variables indicadoras por cada estado, lo cual es mucho más
#    rápido y usa mucha menos memoria a esta escala (60 millones de filas).
#  - La corrección de la regresión de mortalidad (sección 5) usa el paquete
#    `brglm2` (bias-reduction / equivalente a la corrección de Firth) --
#    instalar con install.packages("brglm2") si hace falta. Se eligió
#    brglm2 en vez de logistf (el paquete "clásico" de Firth) porque su
#    algoritmo (mínimos cuadrados iterativamente reponderados, igual que
#    glm()) escala mucho mejor a decenas de millones de filas -- logistf no
#    está pensado para este volumen y probablemente no terminaría en un
#    tiempo razonable.
#  - Para los promedios marginales (AME) del modelo de propensión se usa
#    `marginaleffects` -- instalar con install.packages("marginaleffects")
#    si hace falta.
# =============================================================================

library(data.table)
library(openxlsx)
library(ggplot2)
library(brglm2)   # movido aquí (antes solo se cargaba en la sección 7) porque la
                   # sección 6b ahora también ajusta un modelo con corrección tipo
                   # Firth (ver aviso de recursos abajo, 15-sep)

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
tmp <- tempdir()
egresos_path <- file.path(datos_path, "egresos")
cat10_zip <- file.path(egresos_path, "ssa_Catalogos_Egresos_2023_2025.zip")
# (movidos aquí arriba, antes solo se definían dentro de las secciones 2 y 4 --
#  ahora se necesitan también fuera de ellas, ver CHECKPOINTS más abajo, 15-sep)

# -----------------------------------------------------------------------------
# CHECKPOINTS (15-sep, a petición de JP -- conexión intermitente con la
# máquina donde corre esto, y el modelo de mortalidad tarda 10-15 min, así
# que una corrida cortada a medias perdía todo el avance). Los pasos más
# pesados (cargar los 11 años de egresos + toda la clasificación, y cada
# modelo de regresión) guardan su resultado en disco al terminar. Si la
# sesión se corta, basta con volver a correr el script COMPLETO desde el
# principio: cada paso ya terminado se detecta y se carga desde su
# checkpoint en vez de recalcularse -- solo se rehace el trabajo que de
# verdad no se había completado.
#
# Para forzar que un paso se recalcule desde cero (por ejemplo, si cambias
# el código de ese paso), borra el .rds correspondiente dentro de
# Datos/analisis/checkpoints_potencialmente_raros/ (o la carpeta completa,
# para reiniciar todo el script desde cero).
# -----------------------------------------------------------------------------

dir_checkpoints <- file.path(datos_path, "analisis", "checkpoints_potencialmente_raros")
if (!dir.exists(dir_checkpoints)) dir.create(dir_checkpoints, recursive = TRUE)

ruta_checkpoint <- function(nombre) file.path(dir_checkpoints, paste0(nombre, ".rds"))
hay_checkpoint   <- function(nombre) file.exists(ruta_checkpoint(nombre))

cargar_checkpoint <- function(nombre) {
  cat("Checkpoint encontrado ('", nombre, "') -- se reutiliza, no se recalcula este paso.\n", sep = "")
  readRDS(ruta_checkpoint(nombre))
}

guardar_checkpoint <- function(objeto, nombre) {
  saveRDS(objeto, ruta_checkpoint(nombre))
  cat("Checkpoint guardado: ", nombre, ".rds\n", sep = "")
}

asignar_decil <- function(x, n = 10) {
  r <- frank(x, ties.method = "average")
  pmin(n, ceiling(r / length(r) * n))
}
# NOTA: función idéntica, carácter por carácter, a la del script principal
# (analisis_deciles_frecuencia.R, sección 6) -- ver ahí la nota metodológica
# completa (Tröster et al. 2023). x = frecuencia de la categoría (n_categoria,
# repetida por registro); n = número de deciles (10).

if (hay_checkpoint("datos_clasificados")) {

  datos_chk              <- cargar_checkpoint("datos_clasificados")
  egresos_completos      <- datos_chk$egresos_completos
  nombre_cat3_completo   <- datos_chk$nombre_cat3_completo
  codigos_raras_cat3     <- datos_chk$codigos_raras_cat3
  niveles_grupo_tier_ext <- datos_chk$niveles_grupo_tier_ext
  rm(datos_chk)

} else {

# -----------------------------------------------------------------------------
# 1. Catálogo propio de enfermedades raras (idéntico al script principal)
# -----------------------------------------------------------------------------

cie_que_son_raras <- read.xlsx(file.path(datos_path, "cie10 raras_corregido.xlsx"), sheet = 1)
codigos_raras <- cie_que_son_raras$causa
codigos_raras_cat3 <- unique(substr(toupper(codigos_raras), 1, 3))

# -----------------------------------------------------------------------------
# 2. Egresos hospitalarios 2014-2024 (Sectorial) -- idéntico al script
#    principal, MÁS la columna CEDOCVE (estado de la unidad médica -- OJO,
#    ver aviso abajo) para el modelo de propensión con efectos fijos por
#    estado que pidió JP.
#
#    AVISO METODOLÓGICO (ya documentado en el proyecto, sección de análisis
#    geográfico): CEDOCVE es la entidad DE LA UNIDAD MÉDICA (dónde está el
#    hospital), NO la entidad de residencia del paciente -- Sectorial no
#    trae residencia del paciente en ningún campo. El modelo de propensión
#    de la sección 7 debe leerse con esa salvedad: los efectos fijos
#    controlan por "en qué estado se ubica el hospital", no por "dónde vive
#    el paciente".
# -----------------------------------------------------------------------------

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

load_egreso_completo <- function(m) {
  fp <- file.path(tmp, m$file)
  if (!file.exists(fp))
    unzip(file.path(egresos_path, m$zip), files = m$file, exdir = tmp)

  hdr    <- names(fread(fp, sep = m$sep, nrows = 0))
  hdr_up <- toupper(hdr)

  col_map <- list(
    AFECPRIN4 = "AFECPRIN4",
    DIAS_ESTA = "DIAS_ESTA",
    MOTEGRE   = c("MOTEGRE", "MOTIVO_EGRESO"),
    SEXO      = "SEXO",
    EDAD      = c("EDAD1", "EDAD"),
    CLUES     = "CLUES",
    CEDOCVE   = "CEDOCVE"
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

  message(m$yr, ": ", format(nrow(dt), big.mark = ","),
          " rows (columnas: ", paste(names(sel), collapse = ", "), ")")
  gc()
  dt
}

egresos_completos_list <- lapply(year_meta_egr, load_egreso_completo)
egresos_completos <- rbindlist(egresos_completos_list, fill = TRUE, use.names = TRUE)
rm(egresos_completos_list); gc()

n_sin_cedocve <- egresos_completos[is.na(CEDOCVE) | CEDOCVE == "", .N]
if (n_sin_cedocve > 0) {
  cat("\nAVISO: ", format(n_sin_cedocve, big.mark = ","), " egresos (",
      round(n_sin_cedocve / nrow(egresos_completos) * 100, 2),
      "%) no traen CEDOCVE -- se excluirán del modelo de propensión (sección 7),",
      " pero se conservan en todo lo demás (mortalidad, días de estancia, etc.).\n", sep = "")
}

cat("\nTotal egresos cargados (incl. defunciones intrahospitalarias):",
    format(nrow(egresos_completos), big.mark = ","), "\n")

egresos_vivos <- egresos_completos[MOTEGRE != "5"]

# -----------------------------------------------------------------------------
# 3. Categoría de 3 caracteres + decil ORIGINAL (nacional, sin acotar) --
#    equivalente a las secciones 5-6 del script principal. Este es el decil
#    que se usa para desagregar "el resto" en 1-10.
# -----------------------------------------------------------------------------

egresos_completos[, cat3 := substr(toupper(AFECPRIN4), 1, 3)]
egresos_vivos[, cat3 := substr(toupper(AFECPRIN4), 1, 3)]

freq_egr <- egresos_vivos[, .N, by = cat3]
setnames(freq_egr, "N", "n_categoria")
egresos_vivos[freq_egr, n_categoria := i.n_categoria, on = "cat3"]
egresos_vivos[, decil := asignar_decil(n_categoria)]

decil_original_por_cat3 <- unique(egresos_vivos[, .(cat3, decil)])
stopifnot("decil (original) no es constante dentro de una misma categoría cat3" =
            !anyDuplicated(decil_original_por_cat3$cat3))

# -----------------------------------------------------------------------------
# 4. Universo ACOTADO (capítulos XVIII-XXII fuera, sección equivalente a la
#    10 del script principal) + las 3 fuentes de "rara conocida" (catálogo
#    propio, Orphanet, Blazsik et al. 2021, sección 11/11a del script
#    principal) -- necesarias para definir "potencialmente raros"
#    (decil_sens2 == 1).
# -----------------------------------------------------------------------------

capitulos_excluir <- c("R", "S", "T", "U", "V", "W", "X", "Y", "Z")
egresos_vivos[, cap_excluir := substr(cat3, 1, 1) %in% capitulos_excluir]
egresos_sens <- egresos_vivos[cap_excluir == FALSE]

freq_egr_sens <- egresos_sens[, .N, by = cat3]
setnames(freq_egr_sens, "N", "n_categoria_sens")
egresos_sens[freq_egr_sens, n_categoria_sens := i.n_categoria_sens, on = "cat3"]
egresos_sens[, decil_sens := asignar_decil(n_categoria_sens)]

# Catálogo CIE-10 oficial de la SSA (nombres) -- mismo archivo/ruta que el
# script principal (ssa_Catalogos_Egresos_2023_2025.zip -> CAT_CIE_10_2021.csv)
cat10_contenido <- unzip(cat10_zip, list = TRUE)$Name
cat10_csv <- cat10_contenido[grepl("CAT_CIE_10_2021\\.csv$", cat10_contenido, ignore.case = TRUE)]
stopifnot("No se encontró CAT_CIE_10_2021.csv dentro de ssa_Catalogos_Egresos_2023_2025.zip" =
            length(cat10_csv) == 1)
unzip(cat10_zip, files = cat10_csv, exdir = tmp, junkpaths = TRUE)
cie10_catalogo <- fread(file.path(tmp, basename(cat10_csv)), sep = "|",
                         encoding = "Latin-1", colClasses = "character")
cie10_catalogo[, NOMBRE := enc2utf8(NOMBRE)]

nombre_cat3 <- cie10_catalogo[nchar(CLAVE) == 3, .(cat3 = CLAVE, nombre_cie10 = NOMBRE)]
nombre_cat3X <- cie10_catalogo[nchar(CLAVE) == 4 & endsWith(CLAVE, "X"),
                                .(cat3 = substr(CLAVE, 1, 3), nombre_cie10 = NOMBRE)]
nombre_cat3_completo <- rbindlist(list(nombre_cat3, nombre_cat3X))
nombre_cat3_completo <- unique(nombre_cat3_completo, by = "cat3")

# Orphanet (mapeo oficial CIE-10)
library(xml2)
orphanet_candidatos <- list.files(datos_path, pattern = "^ORPHA_ICD10_mapping.*\\.xml$",
                                   recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
stopifnot("No se encontró ORPHA_ICD10_mapping...xml dentro de la carpeta Datos" =
            length(orphanet_candidatos) >= 1)
orphanet_doc <- read_xml(orphanet_candidatos[1])
orphanet_disorders <- xml_find_all(orphanet_doc, ".//Disorder")
orphanet_mapa <- rbindlist(lapply(orphanet_disorders, function(d) {
  refs <- xml_find_all(d, "./ExternalReferenceList/ExternalReference")
  if (length(refs) == 0) return(NULL)
  data.table(icd10 = xml_text(xml_find_first(refs, "./Reference")))
}))
orphanet_mapa[, cat3 := substr(toupper(gsub(".", "", icd10, fixed = TRUE)), 1, 3)]
orphanet_cat3 <- unique(orphanet_mapa$cat3)

# Blazsik et al. 2021
blazsik_cat <- read.xlsx(file.path(datos_path, "13023_2021_1737_MOESM1_ESM.xlsx"), sheet = "RD_ICD_codes")
blazsik_cat3 <- unique(substr(toupper(blazsik_cat$ICD10code), 1, 3))

fuentes_rara_conocida <- union(union(codigos_raras_cat3, orphanet_cat3), blazsik_cat3)
cats_universo_egr <- unique(egresos_sens$cat3)
descartables_egr <- setdiff(cats_universo_egr, fuentes_rara_conocida)

egresos_sens2 <- egresos_sens[!cat3 %in% descartables_egr]
freq_egr_sens2 <- egresos_sens2[, .N, by = cat3]
setnames(freq_egr_sens2, "N", "n_categoria_sens2")
egresos_sens2[freq_egr_sens2, n_categoria_sens2 := i.n_categoria_sens2, on = "cat3"]
egresos_sens2[, decil_sens2 := asignar_decil(n_categoria_sens2)]

# -----------------------------------------------------------------------------
# 5. CLASIFICACIÓN CENTRAL: "Potencialmente raros" + "Resto" desagregado por
#    decil NACIONAL 1-10 (en vez de colapsar el resto en solo 2 categorías,
#    como en analisis_deciles_frecuencia.R sección 13). Esta es la agrupación
#    de 11 niveles que pidió JP el 14-sep para las gráficas estilo Fig. 2a-b
#    de Tröster et al. 2023.
#
#    "Potencialmente raros" = decil_sens2 == 1 (idéntico al script principal,
#    conjunto fijo de categorías CIE-10 del universo acotado).
#    "Resto" = todo lo demás, SIN restricción -- etiquetado por su decil
#    ORIGINAL nacional (1 a 10, sección 3 de este script). En la práctica el
#    decil 1 del "resto" queda con pocas categorías (la mayoría de lo más
#    raro por frecuencia nacional ya cayó en "potencialmente raros"), pero no
#    está vacío por diseño -- ver aviso más abajo tras calcularlo.
# -----------------------------------------------------------------------------

cat3_potencialmente_raros <- unique(egresos_sens2[decil_sens2 == 1, cat3])
cat("\nCategorías cat3 en 'Potencialmente raros':", length(cat3_potencialmente_raros), "\n")

egresos_completos[, potencialmente_raro := cat3 %in% cat3_potencialmente_raros]
egresos_completos[decil_original_por_cat3, decil_original := i.decil, on = "cat3"]

n_sin_decil_original <- egresos_completos[is.na(decil_original), .N]
if (n_sin_decil_original > 0) {
  egresos_completos[is.na(decil_original), decil_original := 1]
}

egresos_completos[, grupo_tier_ext := fifelse(
  potencialmente_raro,
  "Potencialmente raros (acotado)",
  paste0("Resto -- decil ", decil_original, " nacional")
)]

# Orden explícito para que las gráficas/tablas salgan siempre en el mismo
# orden: Potencialmente raros primero, luego deciles 1 a 10 del resto.
niveles_grupo_tier_ext <- c("Potencialmente raros (acotado)",
                            paste0("Resto -- decil ", 1:10, " nacional"))
egresos_completos[, grupo_tier_ext := factor(grupo_tier_ext, levels = niveles_grupo_tier_ext)]

egresos_completos[, EDAD_num := suppressWarnings(as.numeric(EDAD))]
egresos_completos[, DIAS_ESTA_num := suppressWarnings(as.numeric(DIAS_ESTA))]
egresos_completos[, defuncion := MOTEGRE == "5"]

guardar_checkpoint(
  list(
    egresos_completos      = egresos_completos,
    nombre_cat3_completo   = nombre_cat3_completo,
    codigos_raras_cat3     = codigos_raras_cat3,
    niveles_grupo_tier_ext = niveles_grupo_tier_ext
  ),
  "datos_clasificados"
)

}  # fin del else (hay_checkpoint("datos_clasificados"))

# CORRECCION 18-oct-2026 (comentario de Andres sobre las "41 categorias de CEDOCVE"):
# CEDOCVE traia las claves de 9 entidades con dos formatos ("1" y "01", ..., "9" y
# "09"), de modo que el modelo de propension estimaba 41 efectos fijos y 41 clusteres
# en lugar de 32. Se normaliza a dos digitos (idempotente: funciona igual si
# egresos_completos viene del checkpoint). Los checkpoints del modelo de propension
# cambiaron de nombre (sufijo _cve32) para que se reestime con la clave corregida.
egresos_completos[!is.na(CEDOCVE) & CEDOCVE != "",
                  CEDOCVE := sprintf("%02d", suppressWarnings(as.integer(CEDOCVE)))]
cat("\nCategorias distintas de CEDOCVE tras normalizar:",
    uniqueN(egresos_completos[!is.na(CEDOCVE) & CEDOCVE != "", CEDOCVE]), "(esperado: 32)\n")
print(sort(unique(egresos_completos$CEDOCVE), na.last = TRUE))
stopifnot("CEDOCVE no quedo en 32 claves (01-32)" =
            setequal(unique(egresos_completos[!is.na(CEDOCVE) & CEDOCVE != "", CEDOCVE]), sprintf("%02d", 1:32)))

# Lo siguiente se recalcula SIEMPRE (es prácticamente instantáneo), tanto si
# egresos_completos se acaba de construir como si se cargó de un checkpoint.
tabla_n_grupo_ext <- egresos_completos[, .N, by = grupo_tier_ext][order(grupo_tier_ext)]
cat("\n--- Egresos por grupo (Potencialmente raros + resto por decil nacional) ---\n")
print(tabla_n_grupo_ext)
n_decil1_resto <- tabla_n_grupo_ext[grupo_tier_ext == "Resto -- decil 1 nacional", N]
if (length(n_decil1_resto) == 0 || n_decil1_resto == 0) {
  cat("\nAVISO: 'Resto -- decil 1 nacional' quedó vacío -- todas las categorías del decil 1",
      "nacional cayeron dentro de 'Potencialmente raros'. No es un error; se documenta",
      "para que quede claro en la gráfica/tabla por qué ese punto no aparece.\n")
}

# grupo_tier_3niv (colapso a 3 niveles para el Cuadro 2 / sección 7) se crea
# aquí -- antes vivía dentro de la sección 7, pero se necesita más arriba
# porque la muestra compartida de los modelos (ver más abajo) se recorta de
# egresos_completos DESPUÉS de que esta columna ya exista.
egresos_completos[, grupo_tier_3niv := fifelse(
  potencialmente_raro, "Potencialmente raros (acotado)",
  fifelse(decil_original == 10, "Resto -- decil 10 nacional (más comunes)",
          "Resto -- deciles 1-9 nacional")
)]

# -----------------------------------------------------------------------------
# MUESTRA ALEATORIA COMPARTIDA para los modelos de regresión (18-sep, a
# petición de JP): el ajuste con brglm2 sobre el universo completo (~60
# millones de filas) saturó los 448 GB de RAM de su máquina y empezó a
# intercambiar a disco sin avanzar en más de 2 horas -- el problema no es
# falta de RAM (448 GB ya es muchísimo), sino que el algoritmo de brglm2
# (recalcula valores de influencia/leverage en cada iteración) no escala bien
# a este volumen sin importar el equipo.
#
# Los modelos de Fig. 2a, Fig. 2b y el Cuadro 2 (sección 7) se ajustan SOLO
# sobre esta muestra. Todo lo demás (conteos por grupo, mortalidad/estancia
# CRUDA por grupo, el diagnóstico de decil 2, el 29.7%) se queda sobre el
# universo COMPLETO, sin muestrear -- el muestreo es solo para los pasos de
# regresión que no cabían en memoria.
#
# Tamaño FIJO (no depende de la RAM de la máquina, a propósito): con 3
# millones de filas los errores estándar ya son muy inferiores a lo
# necesario para distinguir un OR de 1 -- el universo completo no aporta
# precisión adicional relevante para estos modelos específicos, solo
# consumo de memoria. MISMA muestra (semilla fija) para los tres modelos,
# para que Fig. 2a, Fig. 2b y el Cuadro 2 sean directamente comparables
# entre sí (cualquier diferencia refleja la especificación del modelo, no
# qué filas le tocaron a cada uno).
n_muestra_modelos <- 3000000
set.seed(20250918)
idx_universo_modelos <- egresos_completos[, which(!is.na(EDAD_num) & SEXO %in% c("1", "2"))]
idx_muestra_modelos <- sample(idx_universo_modelos, min(n_muestra_modelos, length(idx_universo_modelos)))
datos_modelo_muestra <- egresos_completos[idx_muestra_modelos]
cat("\nMuestra aleatoria para los modelos de regresión (Fig. 2a/2b, Cuadro 2): ",
    format(nrow(datos_modelo_muestra), big.mark = ","), " de ",
    format(length(idx_universo_modelos), big.mark = ","), " egresos elegibles.\n", sep = "")
rm(idx_universo_modelos, idx_muestra_modelos)

# -----------------------------------------------------------------------------
# 6. MORTALIDAD y DÍAS DE ESTANCIA por grupo -- por año (para el detalle) y
#    agregado 2014-2024 (para el eje X de las gráficas estilo Fig. 2a-b).
# -----------------------------------------------------------------------------

mortalidad_grupo_ext_anio <- egresos_completos[, .(
  n_egresos      = .N,
  n_defunciones  = sum(defuncion),
  mortalidad_pct = round(sum(defuncion) / .N * 100, 3)
), by = .(ANIO, grupo_tier_ext)][order(ANIO, grupo_tier_ext)]

mortalidad_grupo_ext_total <- egresos_completos[, .(
  n_egresos      = .N,
  n_defunciones  = sum(defuncion),
  mortalidad_pct = round(sum(defuncion) / .N * 100, 3)
), by = grupo_tier_ext][order(grupo_tier_ext)]

dias_estancia_grupo_ext_anio <- egresos_completos[!is.na(DIAS_ESTA_num), .(
  n_egresos    = .N,
  mediana_dias = median(DIAS_ESTA_num),
  p25_dias     = quantile(DIAS_ESTA_num, 0.25),
  p75_dias     = quantile(DIAS_ESTA_num, 0.75),
  media_dias   = round(mean(DIAS_ESTA_num), 2)
), by = .(ANIO, grupo_tier_ext)][order(ANIO, grupo_tier_ext)]

dias_estancia_grupo_ext_total <- egresos_completos[!is.na(DIAS_ESTA_num), .(
  n_egresos    = .N,
  mediana_dias = median(DIAS_ESTA_num),
  p25_dias     = quantile(DIAS_ESTA_num, 0.25),
  p75_dias     = quantile(DIAS_ESTA_num, 0.75),
  media_dias   = round(mean(DIAS_ESTA_num), 2)
), by = grupo_tier_ext][order(grupo_tier_ext)]

cat("\n--- Mortalidad y días de estancia por grupo, acumulado 2014-2024 ---\n")
print(mortalidad_grupo_ext_total)
print(dias_estancia_grupo_ext_total)

# -----------------------------------------------------------------------------
# 6b. GRÁFICAS estilo Fig. 2a (mortalidad) y Fig. 2b (días de estancia) de
#     Tröster et al. 2023 -- REDISEÑADAS el 15-sep a petición de JP, tras
#     revisar la primera versión (que graficaba mortalidad %/mediana de días
#     CRUDAS, con la categoría "Potencialmente raros" aislada de la línea):
#
#       - Fig. 2a ahora grafica el OR de mortalidad hospitalaria de CADA
#         categoría (los 10 deciles del resto + potencialmente raros) frente
#         a una regresión logística ÚNICA con las 11 categorías como
#         predictor, corrección tipo Firth (brglm2, igual que la sección 7,
#         necesaria por el mismo problema de cuasi-separación) y ajuste por
#         edad, sexo y año -- decisión de JP (15-sep).
#       - Fig. 2b grafica exp(β) de una regresión lineal sobre log(días de
#         estancia) con la MISMA especificación de predictores (JP: "para el
#         B es exp(beta) porque los días de estancia se usaron en la
#         regresión como log(días)", igual que Tröster et al., que transforman
#         LOS con logaritmo natural). No hace falta corrección de sesgo aquí
#         porque el desenlace es continuo (no hay cuasi-separación posible).
#       - Categoría de referencia en AMBOS modelos: "Resto -- decil 10
#         nacional" (el decil más común) -- decisión de JP (15-sep), para que
#         el OR/exp(β) suba a medida que la categoría es más rara y se
#         acerque a 1 en el extremo más frecuente, igual que el diseño de
#         dosis-respuesta de Tröster et al.
#       - Al venir las 11 categorías del MISMO modelo, "Potencialmente raros"
#         ya no es un punto aislado: se grafica dentro de la misma línea que
#         los deciles, un punto más de la secuencia (JP: "incluye el punto de
#         potencialmente raras dentro de la línea").
#       - Se quitó el título/subtítulo de la gráfica (JP: "retira el
#         título") -- en un manuscrito el número y la leyenda de la figura
#         van como texto aparte, no incrustados en la imagen.
#       - Eje Y en escala logarítmica (estándar para gráficas de OR/razones) y
#         línea punteada en y=1 (sin efecto) para referencia visual.
#
#     AVISO DE RECURSOS (actualizado 18-sep): el ajuste original sobre el
#     universo completo (~60 millones de egresos) saturó los 448 GB de RAM
#     de la máquina de JP y se quedó intercambiando a disco sin avanzar en
#     más de 2 horas -- brglm2 recalcula valores de influencia/leverage en
#     cada iteración y ese paso no escala bien a este volumen. Por eso este
#     modelo (y el de la sección 7) ahora se ajustan sobre una MUESTRA
#     aleatoria fija de 3 millones de filas (`datos_modelo_muestra`, definida
#     arriba, justo después de construir grupo_tier_3niv) en vez del universo
#     completo -- con ese tamaño de muestra la precisión estadística es más
#     que suficiente, y el consumo de memoria deja de depender de qué tan
#     grande sea la máquina.
#
#     AVISO METODOLÓGICO (pendiente de tu confirmación): para el modelo de
#     Fig. 2b se excluyen los egresos con DIAS_ESTA == 0 (alta el mismo día),
#     porque log(0) no está definido -- no se usa log(días + 1). Si prefieres
#     mantenerlos con esa transformación alterna, dímelo y lo ajusto.
# -----------------------------------------------------------------------------

datos_modelo_fig2 <- copy(datos_modelo_muestra)  # copy(): evita modificar por
                                                  # referencia el objeto compartido

# --- Referencia combinada deciles 9+10 (15-sep, a petición de JP) -----------
# "Resto -- decil 10 nacional" por sí solo tiene mortalidad casi nula (144
# defunciones en 6,708,977 egresos = 0.002%), así que usarlo solo como
# referencia produce OR/exp(beta) ajustados de 2-4 órdenes de magnitud
# (cientos a miles, IC95 hasta >10,000) frente al resto de las categorías --
# resultado correcto (Firth evita que se vaya a infinito por la
# cuasi-separación) pero muy difícil de interpretar/comunicar en la figura.
# Para EVITAR ESTO, en Fig. 2a/2b (y solo en estas dos figuras) combinamos
# deciles 9 y 10 en una sola categoría de referencia con mortalidad menos
# extrema. No se toca nada más: las tablas descriptivas de la sección 6 y el
# modelo de la sección 7 (Cuadro 2) siguen usando los 10 deciles del "resto"
# por separado / su propio esquema de referencia (deciles 1-9 colapsados).
niveles_grupo_tier_ext_ref910 <- c(
  "Potencialmente raros (acotado)",
  paste0("Resto -- decil ", 1:8, " nacional"),
  "Resto -- deciles 9-10 nacional"
)
datos_modelo_fig2[, grupo_tier_ext_ref910 := fifelse(
  grupo_tier_ext %in% c("Resto -- decil 9 nacional", "Resto -- decil 10 nacional"),
  "Resto -- deciles 9-10 nacional",
  as.character(grupo_tier_ext)
)]
datos_modelo_fig2[, grupo_tier_ext_ref910 := relevel(
  factor(grupo_tier_ext_ref910, levels = niveles_grupo_tier_ext_ref910),
  ref = "Resto -- deciles 9-10 nacional"
)]
datos_modelo_fig2[, SEXO_f := factor(SEXO, levels = c("2", "1"), labels = c("Femenino", "Masculino"))]

extraer_tabla_efecto <- function(modelo, prefijo_var, nombre_valor,
                                  nombre_referencia, niveles) {
  coefs <- summary(modelo)$coefficients
  tabla <- data.table(
    variable  = rownames(coefs),
    valor     = exp(coefs[, "Estimate"]),
    IC95_lo   = exp(coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"]),
    IC95_hi   = exp(coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"])
  )
  tabla <- tabla[grepl(paste0("^", prefijo_var), variable)]
  tabla[, grupo_tier_ext := sub(paste0("^", prefijo_var), "", variable)]
  tabla <- tabla[, .(grupo_tier_ext, valor, IC95_lo, IC95_hi)]
  # Fila de referencia: valor = 1 por definición, sin IC.
  tabla <- rbind(tabla, data.table(
    grupo_tier_ext = nombre_referencia, valor = 1, IC95_lo = NA_real_, IC95_hi = NA_real_
  ))
  tabla[, grupo_tier_ext := factor(grupo_tier_ext, levels = niveles)]
  setorder(tabla, grupo_tier_ext)
  setnames(tabla, "valor", nombre_valor)
  tabla[]
}

# --- Fig. 2a: OR de mortalidad hospitalaria (Firth, ref. deciles 9-10 nacional) ---
# Este es el paso que tarda 10-15 min (checkpoint independiente: si se corta
# la sesión durante ESTE modelo, al reiniciar el script no hace falta repetir
# la carga/clasificación de arriba, y si ya había terminado, tampoco hace
# falta repetir el modelo en sí).
if (hay_checkpoint("fig2a_or_ref910")) {
  or_fig2a <- cargar_checkpoint("fig2a_or_ref910")
} else {
  modelo_fig2a_mortalidad <- glm(
    defuncion ~ grupo_tier_ext_ref910 + EDAD_num + SEXO_f + factor(ANIO),
    data = datos_modelo_fig2, family = binomial(),
    method = "brglmFit", type = "AS_mean"
  )
  or_fig2a <- extraer_tabla_efecto(modelo_fig2a_mortalidad, "grupo_tier_ext_ref910", "OR",
                                    "Resto -- deciles 9-10 nacional", niveles_grupo_tier_ext_ref910)
  guardar_checkpoint(or_fig2a, "fig2a_or_ref910")
}

cat("\n--- Fig. 2a: OR de mortalidad hospitalaria por categoría (ajustado, ref.: deciles 9-10 nacional) ---\n")
print(or_fig2a)

# --- Fig. 2b: exp(β) de log(días de estancia) (ref. deciles 9-10 nacional) --
if (hay_checkpoint("fig2b_expbeta_ref910")) {
  expb_fig2b <- cargar_checkpoint("fig2b_expbeta_ref910")
} else {
  datos_modelo_fig2b <- datos_modelo_fig2[!is.na(DIAS_ESTA_num) & DIAS_ESTA_num > 0]
  modelo_fig2b_estancia <- lm(
    log(DIAS_ESTA_num) ~ grupo_tier_ext_ref910 + EDAD_num + SEXO_f + factor(ANIO),
    data = datos_modelo_fig2b
  )
  expb_fig2b <- extraer_tabla_efecto(modelo_fig2b_estancia, "grupo_tier_ext_ref910", "exp_beta",
                                      "Resto -- deciles 9-10 nacional", niveles_grupo_tier_ext_ref910)
  guardar_checkpoint(expb_fig2b, "fig2b_expbeta_ref910")
  rm(datos_modelo_fig2b)
}

cat("\n--- Fig. 2b: exp(β) de días de estancia (log), por categoría (ajustado, ref.: deciles 9-10 nacional) ---\n")
print(expb_fig2b)

rm(datos_modelo_fig2); gc()

# --- Construcción de las gráficas --------------------------------------------

tema_fig2 <- theme_minimal(base_size = 12) +
  theme(legend.position = "none", panel.grid.minor = element_blank())

preparar_datos_fig2 <- function(dt_efecto) {
  d <- copy(dt_efecto)
  # "Resto -- deciles 9-10 nacional" (la referencia combinada) se ubica en
  # x = 9.5, entre donde estarían los deciles 9 y 10 por separado.
  d[, decil_x := suppressWarnings(fifelse(
    grupo_tier_ext == "Potencialmente raros (acotado)", 0,
    fifelse(grupo_tier_ext == "Resto -- deciles 9-10 nacional", 9.5,
            as.numeric(gsub("\\D", "", as.character(grupo_tier_ext))))
  ))]
  setorder(d, decil_x)
  d
}

graficar_fig2_or <- function(dt_efecto, col_y, etiqueta_y) {
  d <- preparar_datos_fig2(dt_efecto)
  d_tendencia <- d[decil_x >= 1]  # el ajuste lineal solo tiene sentido sobre los
                                  # deciles (escala ordinal); "Potencialmente
                                  # raros" no tiene un valor numérico de decil.
  ggplot(d, aes(x = decil_x, y = .data[[col_y]])) +
    geom_hline(yintercept = 1, linetype = "dotted", color = "gray50") +
    geom_smooth(data = d_tendencia, method = "lm", se = TRUE, color = "#2980b9",
                linewidth = 0.8, linetype = "dashed") +
    geom_line(color = "#34495e", linewidth = 0.8) +
    geom_pointrange(aes(ymin = IC95_lo, ymax = IC95_hi), color = "#34495e",
                     size = 0.5, na.rm = TRUE) +
    scale_x_continuous(
      breaks = c(0:8, 9.5),
      labels = c("Potencialmente\nraros", as.character(1:8), "9-10\n(referencia)"),
      limits = c(-0.3, 10.3)
    ) +
    scale_y_log10() +
    labs(x = "Decil nacional (resto) / Potencialmente raros -- referencia: deciles 9-10 nacional",
         y = etiqueta_y) +
    tema_fig2
}

grafica_fig2a_mortalidad <- graficar_fig2_or(
  or_fig2a, "OR",
  etiqueta_y = "OR de mortalidad hospitalaria\n(ajustado por edad, sexo y año; ref.: deciles 9-10 nacional)"
)

grafica_fig2b_estancia <- graficar_fig2_or(
  expb_fig2b, "exp_beta",
  etiqueta_y = "exp(β) de días de estancia, escala log\n(ajustado por edad, sexo y año; ref.: deciles 9-10 nacional)"
)

dir_graficas <- file.path(datos_path, "analisis")
ggsave(file.path(dir_graficas, "figura2a_mortalidad_por_decil.jpeg"),
       grafica_fig2a_mortalidad, width = 8, height = 5, dpi = 150)
ggsave(file.path(dir_graficas, "figura2b_dias_estancia_por_decil.jpeg"),
       grafica_fig2b_estancia, width = 8, height = 5, dpi = 150)
message("Gráficas guardadas en Datos/analisis/: figura2a_mortalidad_por_decil.jpeg, figura2b_dias_estancia_por_decil.jpeg")

# -----------------------------------------------------------------------------
# 7. REGRESIÓN DE MORTALIDAD CORREGIDA (14-sep, a petición de JP) -- el
#    modelo glm() del script principal (sección 14b) daba un OR degenerado
#    (<0.01, IC no estimable) para "Resto -- decil 10 nacional" por
#    cuasi-separación (solo 144 defunciones en 6.7 millones de egresos). Se
#    reemplaza por un ajuste con reducción de sesgo tipo Firth (brglm2,
#    método "AS_mean" -- la reducción de sesgo de la media, que es
#    exactamente la corrección de Firth para regresión logística), que da un
#    OR estable en vez de un punto degenerado. Grupo de referencia: "Resto --
#    deciles 1-9 nacional" colapsado (para que el modelo sea comparable al
#    Cuadro 2 ya entregado) -- se recalcula aquí ese colapso 1-9 solo para
#    este modelo, sin perder el detalle por decil de las tablas de arriba.
# -----------------------------------------------------------------------------

if (hay_checkpoint("mortalidad_or_corregido")) {
  mortalidad_or_corregido <- cargar_checkpoint("mortalidad_or_corregido")
} else {
  datos_modelo_mort <- copy(datos_modelo_muestra)  # copy(): evita modificar por
                                                    # referencia el objeto compartido
  datos_modelo_mort[, grupo_tier_3niv := relevel(factor(grupo_tier_3niv), ref = "Resto -- deciles 1-9 nacional")]
  datos_modelo_mort[, SEXO_f := factor(SEXO, levels = c("2", "1"), labels = c("Femenino", "Masculino"))]

  modelo_mortalidad_firth <- glm(
    defuncion ~ grupo_tier_3niv + EDAD_num + SEXO_f + factor(ANIO),
    data = datos_modelo_mort, family = binomial(),
    method = "brglmFit", type = "AS_mean"
  )

  coefs_firth <- summary(modelo_mortalidad_firth)$coefficients
  or_tabla_firth <- data.table(
    variable = rownames(coefs_firth),
    OR       = round(exp(coefs_firth[, "Estimate"]), 3),
    IC95_lo  = round(exp(coefs_firth[, "Estimate"] - 1.96 * coefs_firth[, "Std. Error"]), 3),
    IC95_hi  = round(exp(coefs_firth[, "Estimate"] + 1.96 * coefs_firth[, "Std. Error"]), 3),
    p_valor  = signif(coefs_firth[, "Pr(>|z|)"], 3)
  )
  mortalidad_or_corregido <- or_tabla_firth[grepl("^grupo_tier_3niv", variable)]
  guardar_checkpoint(mortalidad_or_corregido, "mortalidad_or_corregido")

  rm(datos_modelo_mort); gc()
}

cat("\n--- Mortalidad hospitalaria: OR corregido (brglm2, tipo Firth) ---\n")
cat("Referencia: 'Resto -- deciles 1-9 nacional'\n")
print(mortalidad_or_corregido)

# datos_modelo_muestra ya no hace falta (los tres modelos que la usan --
# Fig. 2a, Fig. 2b y este de arriba -- ya terminaron); se libera para dejar
# memoria disponible para lo que sigue (institución, modelo de propensión).
if (exists("datos_modelo_muestra")) { rm(datos_modelo_muestra); gc() }

# -----------------------------------------------------------------------------
# 8. CONSULTA PENDIENTE (13-sep): ¿qué hay en el 29.7% de "Resto -- deciles
#    1-9 nacional" que SÍ coincide con nuestro catálogo propio de raras?
#    Listado completo (cat3, nombre CIE-10, frecuencia dentro de ese grupo),
#    para revisión manual -- mismo patrón que la lista "genuino" de la
#    sección 10b del script principal.
# -----------------------------------------------------------------------------

egresos_completos[, es_de_catalogo_propio := cat3 %in% codigos_raras_cat3]

catalogo_en_resto_1_9 <- egresos_completos[
  grupo_tier_3niv == "Resto -- deciles 1-9 nacional" & es_de_catalogo_propio == TRUE,
  .N, by = cat3
][order(-N)]
setnames(catalogo_en_resto_1_9, "N", "n_egresos")
catalogo_en_resto_1_9 <- merge(catalogo_en_resto_1_9, nombre_cat3_completo, by = "cat3", all.x = TRUE)
catalogo_en_resto_1_9[is.na(nombre_cie10), nombre_cie10 := "(sin descripción en catálogo CIE-10 SSA)"]
catalogo_en_resto_1_9 <- catalogo_en_resto_1_9[order(-n_egresos), .(cat3, nombre_cie10, n_egresos)]

cat("\n--- Categorías de 'Resto -- deciles 1-9 nacional' que SÍ están en el catálogo propio ---\n")
cat(nrow(catalogo_en_resto_1_9), "categorías distintas. Top 15 por volumen:\n")
print(head(catalogo_en_resto_1_9, 15))

# -----------------------------------------------------------------------------
# 8b. DIAGNÓSTICO (15-sep, a petición de JP): "Resto -- decil 2 nacional" salió
#     con una mortalidad hospitalaria llamativamente alta en la Fig. 2a (muy
#     por encima de los deciles vecinos) -- se desglosa por cat3 para ver qué
#     categorías CIE-10 la componen. Se generaliza a CUALQUIER decil (no solo
#     el 2) para que sirva de diagnóstico reutilizable si aparece otro pico
#     inesperado en una corrida futura.
# -----------------------------------------------------------------------------

diagnostico_por_cat3 <- function(grupo) {
  d <- egresos_completos[
    grupo_tier_ext == grupo,
    .(n_egresos = .N, n_defunciones = sum(defuncion)),
    by = cat3
  ]
  d[, mortalidad_pct := round(n_defunciones / n_egresos * 100, 3)]
  d <- merge(d, nombre_cat3_completo, by = "cat3", all.x = TRUE)
  d[is.na(nombre_cie10), nombre_cie10 := "(sin descripción en catálogo CIE-10 SSA)"]
  d[, .(cat3, nombre_cie10, n_egresos, n_defunciones, mortalidad_pct)]
}

diagnostico_decil2 <- diagnostico_por_cat3("Resto -- decil 2 nacional")

cat("\n--- Diagnóstico 'Resto -- decil 2 nacional': categorías cat3 con más DEFUNCIONES (volumen absoluto) ---\n")
print(head(diagnostico_decil2[order(-n_defunciones)], 15))
cat("\n--- Mismo grupo, categorías con mayor TASA de mortalidad (ojo con n_egresos chico) ---\n")
print(head(diagnostico_decil2[order(-mortalidad_pct)], 15))

# -----------------------------------------------------------------------------
# 8c. DIAGNÓSTICO POR AÑO (16-sep, a petición de JP): dentro de uno o más
#     cat3 dados, ¿el volumen/mortalidad se concentra en algún año en
#     particular? Motivado por J96 (insuficiencia respiratoria) y R57
#     (choque) dentro del listado de "Resto -- decil 2 nacional" -- ambos son
#     códigos inespecíficos que suelen usarse como causa inmediata/asociada
#     en pacientes críticos, y JP sospecha que su peso podría concentrarse en
#     2020-2021 (efecto composicional de la pandemia) en vez de ser un
#     patrón estructural del decil. Se generaliza a CUALQUIER conjunto de
#     códigos cat3 (no solo J96/R57) para que sirva como diagnóstico
#     reutilizable ante cualquier otro código que llame la atención después.
# -----------------------------------------------------------------------------

diagnostico_por_cat3_anio <- function(codigos) {
  d <- egresos_completos[
    cat3 %in% codigos,
    .(n_egresos = .N, n_defunciones = sum(defuncion)),
    by = .(cat3, ANIO)
  ]
  d[, mortalidad_pct := round(n_defunciones / n_egresos * 100, 3)]
  d <- merge(d, nombre_cat3_completo, by = "cat3", all.x = TRUE)
  d[is.na(nombre_cie10), nombre_cie10 := "(sin descripción en catálogo CIE-10 SSA)"]
  setorder(d, cat3, ANIO)
  d[, .(cat3, nombre_cie10, ANIO, n_egresos, n_defunciones, mortalidad_pct)]
}

diagnostico_j96_r57_anio <- diagnostico_por_cat3_anio(c("J96", "R57"))

cat("\n--- Diagnóstico por año: J96 (insuficiencia respiratoria) y R57 (choque) ---\n")
cat("    (sospecha de JP: pico de mortalidad concentrado en años de pandemia)\n")
print(diagnostico_j96_r57_anio)

# -----------------------------------------------------------------------------
# 9. INSTITUCIÓN DE ATENCIÓN (CATCLUES) -- mismo join que el script
#    principal (sección 15c), agrupado luego en 4 categorías (IMSS / SSA /
#    ISSSTE / Otras) para el modelo de propensión, decisión de JP (14-sep).
# -----------------------------------------------------------------------------

cat10_contenido_clues <- unzip(cat10_zip, list = TRUE)$Name
clues_csv <- cat10_contenido_clues[grepl("^CATCLUES\\.csv$", basename(cat10_contenido_clues), ignore.case = TRUE)]
stopifnot("No se encontró CATCLUES.csv dentro de ssa_Catalogos_Egresos_2023_2025.zip" =
            length(clues_csv) == 1)
unzip(cat10_zip, files = clues_csv, exdir = tmp, junkpaths = TRUE)
catalogo_clues <- fread(file.path(tmp, basename(clues_csv)), sep = "|", encoding = "Latin-1", colClasses = "character")
catalogo_clues[, `NOMBRE DE LA INSTITUCION` := enc2utf8(`NOMBRE DE LA INSTITUCION`)]

institucion_por_clues <- unique(catalogo_clues[, .(CLUES, institucion = `NOMBRE DE LA INSTITUCION`)])
stopifnot("CLUES duplicado con institución distinta en el catálogo CATCLUES" =
            !anyDuplicated(institucion_por_clues$CLUES))

egresos_completos[institucion_por_clues, institucion := i.institucion, on = "CLUES"]
egresos_completos[is.na(institucion), institucion := "No encontrado en catálogo"]

# AVISO (14-sep, verificado contra el catálogo CATCLUES y contra los CLUES
# reales de los archivos crudos): "SERVICIOS DE SALUD IMSS BIENESTAR" se
# agrupa AQUÍ con "Secretaría de Salud", NO con "Otras". Son unidades de los
# Servicios Estatales de Salud transferidas al organismo federal IMSS-
# Bienestar a partir de 2023 -- su CLUES cambió (la CLUES vieja, con prefijo
# "SSA", queda en el catálogo marcada "FUERA DE OPERACION"), pero es la MISMA
# unidad física. Verificado directamente contra los datos: para una muestra
# de CLUES de Baja California, el archivo de 2020 usa el CLUES viejo (6,635
# registros, 0 con el nuevo) y el de 2024 usa el CLUES nuevo (8,900
# registros, 0 con el viejo). Sin este agrupamiento, estas unidades
# brincarían de "Secretaría de Salud" a una categoría nueva justo cuando
# cambió su CLUES (2023), creando un salto artificial en el panel 2014-2024
# que no refleja ningún cambio real en quién atiende -- decisión de JP.
#
# "INSTITUTO MEXICANO DEL SEGURO SOCIAL BIENESTAR" es DISTINTO -- es el
# programa rural histórico del IMSS (ex IMSS-COPLAMAR/IMSS-Solidaridad,
# nunca fue de Servicios de Salud estatales, su CLUES nunca cambió) -- se
# queda dentro de "Otras", decisión de JP (14-sep).
egresos_completos[, institucion_4cat := fcase(
  institucion == "INSTITUTO MEXICANO DEL SEGURO SOCIAL", "IMSS",
  institucion %in% c("SECRETARIA DE SALUD", "SERVICIOS DE SALUD IMSS BIENESTAR"), "Secretaría de Salud",
  institucion == "INSTITUTO DE SEGURIDAD Y SERVICIOS SOCIALES PARA LOS TRABAJADORES DEL ESTADO", "ISSSTE",
  default = "Otras"
)]
egresos_completos[, institucion_4cat := relevel(factor(institucion_4cat), ref = "IMSS")]

# -----------------------------------------------------------------------------
# 9b. TABLA DESCRIPTIVA (Tabla 1 del manuscrito, 16-sep, a petición de JP):
#     sexo, grupo de edad e institución de atención, por los MISMOS 3 grupos
#     que la referencia combinada de Fig. 2a/2b -- "Potencialmente raros
#     (acotado)" / "Resto -- deciles 1-8 nacional" / "Resto -- deciles 9-10
#     nacional" -- para que la tabla y las figuras describan exactamente el
#     mismo agrupamiento.
#
#     OJO: el análisis anterior (deciles_frecuencia_paper_suizo.xlsx) ya
#     tenía hojas tabla1_sexo/tabla1_grupo_edad/tabla1_institucion, pero con
#     "decil 10 nacional" SOLO como tercer grupo (no combinado con decil 9) --
#     no se pudieron reaproveechar tal cual porque esas hojas ya vienen
#     colapsadas a nivel de grupo: decil 9 quedó mezclado dentro de "Resto --
#     deciles 1-9 nacional" sin desglose por decil individual, así que no hay
#     forma de "separar" el decil 9 de ahí para recombinarlo con el 10. Por
#     eso se recalculan aquí -- es un conteo agregado barato sobre los datos
#     ya clasificados en memoria (nada de ajustar modelos), no debe
#     confundirse con el paso costoso de la sección 10.
# -----------------------------------------------------------------------------

egresos_completos[, grupo_tabla1 := fifelse(
  potencialmente_raro, "Potencialmente raros (acotado)",
  fifelse(decil_original %in% c(9, 10), "Resto -- deciles 9-10 nacional",
          "Resto -- deciles 1-8 nacional")
)]
niveles_grupo_tabla1 <- c("Potencialmente raros (acotado)",
                          "Resto -- deciles 1-8 nacional",
                          "Resto -- deciles 9-10 nacional")
egresos_completos[, grupo_tabla1 := factor(grupo_tabla1, levels = niveles_grupo_tabla1)]

egresos_completos[, grupo_edad := as.character(cut(
  EDAD_num, breaks = c(-Inf, 17, 34, 49, 64, 79, Inf),
  labels = c("0-17", "18-34", "35-49", "50-64", "65-79", "80+")
))]
egresos_completos[is.na(grupo_edad), grupo_edad := "(edad no registrada)"]
egresos_completos[, grupo_edad := factor(
  grupo_edad, levels = c("0-17", "18-34", "35-49", "50-64", "65-79", "80+", "(edad no registrada)")
)]

egresos_completos[, sexo_lbl := fcase(
  SEXO == "2", "Femenino",
  SEXO == "1", "Masculino",
  default = "Otro/no especificado"
)]

tabla1_n_mort_estancia <- egresos_completos[, .(
  n_egresos     = .N,
  n_defunciones = sum(defuncion),
  mediana_dias  = as.numeric(median(DIAS_ESTA_num, na.rm = TRUE)),
  p25_dias      = as.numeric(quantile(DIAS_ESTA_num, 0.25, na.rm = TRUE)),
  p75_dias      = as.numeric(quantile(DIAS_ESTA_num, 0.75, na.rm = TRUE))
), by = grupo_tabla1]
tabla1_n_mort_estancia[, mortalidad_pct := round(n_defunciones / n_egresos * 100, 3)]
setorder(tabla1_n_mort_estancia, grupo_tabla1)
tabla1_n_mort_estancia <- tabla1_n_mort_estancia[, .(
  grupo_tabla1, n_egresos, n_defunciones, mortalidad_pct, mediana_dias, p25_dias, p75_dias
)]

tabla1_sexo_3grupos <- egresos_completos[, .N, by = .(grupo_tabla1, sexo_lbl)]
tabla1_sexo_3grupos[, pct := round(N / sum(N) * 100, 1), by = grupo_tabla1]
setnames(tabla1_sexo_3grupos, "N", "n")
setorder(tabla1_sexo_3grupos, grupo_tabla1, -n)

tabla1_edad_3grupos <- egresos_completos[, .N, by = .(grupo_tabla1, grupo_edad)]
tabla1_edad_3grupos[, pct := round(N / sum(N) * 100, 1), by = grupo_tabla1]
setnames(tabla1_edad_3grupos, "N", "n")
setorder(tabla1_edad_3grupos, grupo_tabla1, grupo_edad)

tabla1_institucion_3grupos <- egresos_completos[, .N, by = .(grupo_tabla1, institucion)]
tabla1_institucion_3grupos[, pct := round(N / sum(N) * 100, 1), by = grupo_tabla1]
setnames(tabla1_institucion_3grupos, "N", "n")
setorder(tabla1_institucion_3grupos, grupo_tabla1, -n)

cat("\n--- Tabla descriptiva (3 grupos, referencia 9-10 combinada): N, mortalidad, días de estancia ---\n")
print(tabla1_n_mort_estancia)
cat("\n--- Tabla descriptiva: sexo ---\n")
print(tabla1_sexo_3grupos)
cat("\n--- Tabla descriptiva: grupo de edad ---\n")
print(tabla1_edad_3grupos)
cat("\n--- Tabla descriptiva: institución de atención ---\n")
print(tabla1_institucion_3grupos)

# -----------------------------------------------------------------------------
# 10. MODELO DE PROPENSIÓN A "POTENCIALMENTE RARO" (14-sep, a petición de JP)
#
#     OJO -- ENCUADRE DEL MODELO (JP lo confirmó explícitamente, 14-sep): al
#     igual que Tröster et al. no interpretan su "FB-RDx" (decil 1 por
#     frecuencia) como "tener una enfermedad rara" sino como pertenecer a la
#     categoría de diagnóstico-menos-frecuente, este modelo NO debe leerse
#     como "qué predice tener una enfermedad rara" -- la variable dependiente
#     es una CLASIFICACIÓN derivada únicamente de la frecuencia del código
#     CIE-10 principal (potencialmente_raro = decil_sens2==1, sección 5), no
#     un diagnóstico clínico confirmado. Lo que el modelo sí puede responder:
#     dado el sistema de salud mexicano tal como está documentado en estos
#     datos, ¿qué características del egreso (edad, sexo, institución,
#     estado del hospital, año) se asocian con que ESE egreso caiga en la
#     categoría "potencialmente raro"? -- una pregunta de equidad/acceso a la
#     clasificación, no de epidemiología clínica.
#
#     Especificación (confirmada con JP):
#       potencialmente_raro ~ EDAD_num + SEXO_f + institucion_4cat +
#                              factor(ANIO) | CEDOCVE (efectos fijos)
#       Errores estándar clusterizados por CEDOCVE (mismo nivel que los
#       efectos fijos -- la opción más simple y defendible; si prefieres
#       clusterizar por CLUES/hospital en vez de por estado, dímelo y lo
#       ajusto, sería un cluster más fino y probablemente errores un poco
#       más grandes/conservadores).
#
#     fixest::feglm() en vez de glm(): con ~60 millones de filas y ~32
#     categorías de estado, expandir el estado como variables indicadoras
#     dentro de glm() (como hace factor()) es mucho más lento y usa mucha
#     más memoria que la absorción algorítmica de efectos fijos de fixest
#     (que nunca construye esas columnas). Es la misma razón por la que el
#     modelo de mortalidad de la sección 7 ya era el paso más pesado del
#     script principal sin ni siquiera tener estado en la ecuación.
# -----------------------------------------------------------------------------

# install.packages("fixest") si hace falta -- no viene preinstalado.
library(fixest)
library(marginaleffects)

datos_modelo_prop <- egresos_completos[
  !is.na(EDAD_num) & SEXO %in% c("1", "2") & !is.na(CEDOCVE) & CEDOCVE != ""
]
datos_modelo_prop[, SEXO_f := factor(SEXO, levels = c("2", "1"), labels = c("Femenino", "Masculino"))]
datos_modelo_prop[, potencialmente_raro_num := as.integer(potencialmente_raro)]

# CHECKPOINTS SEPARADOS (18-sep, a raíz de que el paso del AME falló DESPUÉS
# de que el modelo principal ya había terminado -- con un solo checkpoint
# para los dos, un error en el AME obligaba a repetir también el ajuste
# principal, que ya había tardado buen rato y sí había funcionado). Ahora
# cada uno se guarda por separado: si el AME falla, al reiniciar el script
# NO hace falta volver a ajustar el modelo principal (feglm con EF
# absorbidos), solo se repite el paso que de verdad falló.
if (hay_checkpoint("modelo_propension_coefs_cve32")) {
  modelo_propension_coefs <- cargar_checkpoint("modelo_propension_coefs_cve32")
} else {

  modelo_propension <- feglm(
    potencialmente_raro_num ~ EDAD_num + SEXO_f + institucion_4cat + factor(ANIO) | CEDOCVE,
    data = datos_modelo_prop, family = "logit",
    cluster = ~CEDOCVE
  )

  cat("\n--- Modelo de propensión a 'potencialmente raro' (fixest::feglm, EF por estado) ---\n")
  print(summary(modelo_propension))

  coefs_prop <- as.data.table(coeftable(modelo_propension), keep.rownames = "variable")
  modelo_propension_coefs <- coefs_prop[, .(
    variable,
    OR      = round(exp(Estimate), 3),
    IC95_lo = round(exp(Estimate - 1.96 * `Std. Error`), 3),
    IC95_hi = round(exp(Estimate + 1.96 * `Std. Error`), 3),
    p_valor = signif(`Pr(>|z|)`, 3)
  )]
  guardar_checkpoint(modelo_propension_coefs, "modelo_propension_coefs_cve32")
  rm(modelo_propension)
}

if (hay_checkpoint("modelo_propension_ame_muestra3M")) {
  modelo_propension_ame <- cargar_checkpoint("modelo_propension_ame_muestra3M")
} else {

  # Promedios marginales (AME) -- pedido explícito de JP (14-sep): con 60
  # millones de observaciones casi cualquier efecto sale "significativo" por
  # el tamaño de muestra: el AME da la magnitud del efecto en la escala de
  # probabilidad (puntos porcentuales), que es más interpretable que el OR
  # cuando el tamaño de muestra ya no es el problema.
  #
  # AJUSTE 2 (18-sep): mi primer intento de corrección (pasar
  # `vcov = vcov(modelo_propension)`) estaba mal y no iba a funcionar --
  # confirmado contra la documentación real del paquete tras el segundo error
  # idéntico que reportaste. La razón de fondo: los 41 efectos fijos de
  # CEDOCVE NUNCA se estiman como coeficientes individuales (por eso feglm()
  # es eficiente en memoria) -- así que `vcov(modelo_propension)` sencillamente
  # no contiene, y no puede contener, la incertidumbre de esos 41 efectos. No
  # hay ninguna matriz que se le pueda pasar a `avg_slopes()` para suplir una
  # información que el modelo nunca calculó.
  #
  # La solución real: para el AME (y SOLO para el AME -- la tabla de
  # coeficientes de arriba, con clustering correcto, no cambia) se ajusta un
  # segundo modelo, idéntico en especificación, pero SIN absorber CEDOCVE
  # como efecto fijo -- se incluye como variable indicadora explícita
  # (factor(CEDOCVE), 41 categorías). Así TODOS los parámetros (incluido el
  # estado) quedan con su propio coeficiente y su entrada correspondiente en
  # la matriz de varianzas-covarianzas, y avg_slopes() ya puede propagar la
  # incertidumbre completa sin toparse con el error.
  #
  # AVISO DE RECURSOS (actualizado 16-sep): originalmente pensamos que este
  # segundo ajuste sería manejable sobre el universo completo, porque solo
  # añade ~40 columnas indicadoras (sin la corrección de Firth de brglm2, que
  # era la parte realmente cara en Fig. 2a). Resultó ser INCORRECTO: en la
  # corrida real la memoria llegó a 99% (444/448 GB) con CPU y Disk casi en
  # reposo (11% CPU, actividad de disco de solo KB/s) -- un patrón que no es
  # ni cómputo activo ni intercambio a disco normal, sino un proceso
  # efectivamente atascado. La causa más probable: `avg_slopes()` (método
  # delta) construye matrices de gradiente de dimensión filas x parámetros
  # (~60,000,000 x ~50) para propagar la incertidumbre, lo cual no escala con
  # más RAM disponible -- el mismo patrón que ya habíamos visto con brglm2 en
  # Fig. 2a. Por eso, igual que ahí, el AME (solo el AME -- el modelo de
  # coeficientes de arriba, con absorción de efectos fijos, sí es manejable
  # sobre el universo completo y no se toca) se calcula sobre una muestra
  # aleatoria de tamaño fijo en vez del universo completo.
  n_muestra_ame <- 3000000
  set.seed(20250918)
  idx_muestra_ame <- sample(nrow(datos_modelo_prop), min(n_muestra_ame, nrow(datos_modelo_prop)))
  datos_modelo_prop_muestra_ame <- datos_modelo_prop[idx_muestra_ame]
  cat("\nMuestra aleatoria para el paso de AME del modelo de propensión: ",
      nrow(datos_modelo_prop_muestra_ame), " de ", nrow(datos_modelo_prop), " egresos.\n", sep = "")

  modelo_propension_ame_base <- feglm(
    potencialmente_raro_num ~ EDAD_num + SEXO_f + institucion_4cat + factor(ANIO) + factor(CEDOCVE),
    data = datos_modelo_prop_muestra_ame, family = "logit",
    cluster = ~CEDOCVE
  )
  ame_propension <- avg_slopes(modelo_propension_ame_base, variables = c("EDAD_num", "SEXO_f", "institucion_4cat"))
  rm(modelo_propension_ame_base, datos_modelo_prop_muestra_ame, idx_muestra_ame)
  modelo_propension_ame <- as.data.table(ame_propension)[, .(
    term, contrast,
    AME_pp   = round(estimate * 100, 4),
    IC95_lo_pp = round(conf.low * 100, 4),
    IC95_hi_pp = round(conf.high * 100, 4),
    p_valor  = signif(p.value, 3)
  )]
  guardar_checkpoint(modelo_propension_ame, "modelo_propension_ame_muestra3M")
}


# -----------------------------------------------------------------------------
# 10c. PROPENSION CON EDAD EN SPLINE NATURAL (16-oct-2026; comentario M9 de Andres)
#      Mismo modelo que el de arriba pero con la edad como spline natural (df = 4,
#      nudos en los cuartiles). Los nudos se fijan de antemano (no dependen de
#      los datos nuevos) para que predict() y avg_slopes() funcionen igual con
#      fixest. Los checkpoints son NUEVOS ("..._spline_..."): los del modelo
#      con edad lineal no se tocan. Pasos pesados: el ajuste con efectos fijos
#      sobre el universo completo y el AME sobre la muestra de 3 millones
#      (misma semilla que el AME lineal, por lo que la muestra es la misma).
#      Los coeficientes de los 4 terminos del spline no se interpretan solos:
#      se reporta la prueba de Wald conjunta de la edad y el AME de la edad.
# -----------------------------------------------------------------------------
library(splines)
nudos_edad   <- quantile(datos_modelo_prop$EDAD_num, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
limites_edad <- range(datos_modelo_prop$EDAD_num, na.rm = TRUE)
cat("\nNudos del spline de edad (cuartiles):", paste(nudos_edad, collapse = ", "),
    "| limites:", paste(limites_edad, collapse = " - "), "\n")

if (hay_checkpoint("modelo_propension_spline_coefs_cve32") && hay_checkpoint("modelo_propension_spline_wald_cve32")) {
  modelo_propension_spline_coefs <- cargar_checkpoint("modelo_propension_spline_coefs_cve32")
  modelo_propension_spline_wald  <- cargar_checkpoint("modelo_propension_spline_wald_cve32")
} else {
  modelo_prop_spline <- feglm(
    potencialmente_raro_num ~ splines::ns(EDAD_num, knots = nudos_edad, Boundary.knots = limites_edad) +
      SEXO_f + institucion_4cat + factor(ANIO) | CEDOCVE,
    data = datos_modelo_prop, family = "logit",
    cluster = ~CEDOCVE
  )
  cat("\n--- Propension con edad en spline (fixest::feglm, EF por estado) ---\n")
  print(summary(modelo_prop_spline))

  coefs_prop_sp <- as.data.table(coeftable(modelo_prop_spline), keep.rownames = "variable")
  coefs_prop_sp[, variable := sub("^splines::ns\\(.*\\)([0-9]+)$", "edad_spline_\\1", variable)]
  modelo_propension_spline_coefs <- coefs_prop_sp[, .(
    variable,
    OR      = round(exp(Estimate), 3),
    IC95_lo = round(exp(Estimate - 1.96 * `Std. Error`), 3),
    IC95_hi = round(exp(Estimate + 1.96 * `Std. Error`), 3),
    p_valor = signif(`Pr(>|z|)`, 3)
  )]
  modelo_propension_spline_wald <- tryCatch({
    w <- fixest::wald(modelo_prop_spline, keep = "EDAD_num", print = FALSE)
    data.table(prueba = "Wald conjunta de los 4 terminos de edad",
               estadistico = unname(w$stat), gl = unname(w$df1), p_valor = signif(unname(w$p), 3))
  }, error = function(e) {
    data.table(prueba = paste("Wald fallo:", conditionMessage(e)),
               estadistico = NA_real_, gl = NA_real_, p_valor = NA_real_)
  })
  guardar_checkpoint(modelo_propension_spline_coefs, "modelo_propension_spline_coefs_cve32")
  guardar_checkpoint(modelo_propension_spline_wald,  "modelo_propension_spline_wald_cve32")
  rm(modelo_prop_spline); gc()
}

if (hay_checkpoint("modelo_propension_spline_ame_muestra3M")) {
  modelo_propension_spline_ame <- cargar_checkpoint("modelo_propension_spline_ame_muestra3M")
} else {
  set.seed(20250918)
  idx_ame_sp <- sample(nrow(datos_modelo_prop), min(3000000, nrow(datos_modelo_prop)))
  datos_ame_sp <- datos_modelo_prop[idx_ame_sp]
  modelo_ame_sp_base <- feglm(
    potencialmente_raro_num ~ splines::ns(EDAD_num, knots = nudos_edad, Boundary.knots = limites_edad) +
      SEXO_f + institucion_4cat + factor(ANIO) + factor(CEDOCVE),
    data = datos_ame_sp, family = "logit",
    cluster = ~CEDOCVE
  )
  ame_sp <- avg_slopes(modelo_ame_sp_base, variables = c("EDAD_num", "SEXO_f", "institucion_4cat"))
  rm(modelo_ame_sp_base, datos_ame_sp, idx_ame_sp)
  modelo_propension_spline_ame <- as.data.table(ame_sp)[, .(
    term, contrast,
    AME_pp     = round(estimate * 100, 4),
    IC95_lo_pp = round(conf.low * 100, 4),
    IC95_hi_pp = round(conf.high * 100, 4),
    p_valor    = signif(p.value, 3)
  )]
  guardar_checkpoint(modelo_propension_spline_ame, "modelo_propension_spline_ame_muestra3M")
}
cat("\n--- Propension con spline: Wald conjunta de la edad ---\n"); print(modelo_propension_spline_wald)
cat("\n--- Propension con spline: AME (puntos porcentuales) ---\n"); print(modelo_propension_spline_ame)


# -----------------------------------------------------------------------------
# 10d. IC DEL AME DE LA EDAD (SPLINE) POR SIMULACION PARAMETRICA (16-oct-2026)
#      avg_slopes() no entrego IC ni p para la edad con el spline. Aqui se
#      estima el AME de la edad y la probabilidad ajustada por edad con IC por
#      simulacion: se ajusta de nuevo el modelo de propension (misma muestra de
#      3 millones, semilla 20250918, EF de estado como indicadoras) y se
#      simulan B = 1000 vectores de coeficientes de N(beta, V), con V la matriz
#      de varianzas agrupada por entidad. No es un bootstrap no parametrico
#      (re-ajustar el modelo 1000 veces no es viable); el IC es el percentil
#      2.5-97.5 de las B simulaciones. Los calculos usan una submuestra de
#      300,000 egresos de esa muestra (el AME puntual cambia en el 3er decimal).
#      CAVEAT: con ~58 parametros y ~41 grupos, V agrupada es de rango
#      incompleto; por eso se reporta tambien el IC con V sin agrupar
#      (sensibilidad), que es mas angosto.
#      AME de la edad = promedio de [P(edad + 0.5) - P(edad - 0.5)] (diferencia
#      central, por ano), en puntos porcentuales.
# -----------------------------------------------------------------------------
if (hay_checkpoint("modelo_propension_spline_boot_cve32")) {
  boot_prop <- cargar_checkpoint("modelo_propension_spline_boot_cve32")
  modelo_prop_spline_ame_edad  <- boot_prop$ame_edad
  modelo_prop_spline_prob_edad <- boot_prop$prob_edad
  rm(boot_prop)
} else {
  stopifnot(requireNamespace("MASS", quietly = TRUE))
  B_sim <- 1000
  set.seed(20250918)
  idx_b <- sample(nrow(datos_modelo_prop), min(3000000, nrow(datos_modelo_prop)))
  dat_b <- datos_modelo_prop[idx_b]
  rm(idx_b)
  m_b <- feglm(
    potencialmente_raro_num ~ splines::ns(EDAD_num, knots = nudos_edad, Boundary.knots = limites_edad) +
      SEXO_f + institucion_4cat + factor(ANIO) + factor(CEDOCVE),
    data = dat_b, family = "logit", cluster = ~CEDOCVE
  )
  b_hat  <- coef(m_b)
  V_clus <- vcov(m_b)                       # agrupada por CEDOCVE
  V_iid  <- vcov(m_b, vcov = "iid")         # sensibilidad (sin agrupar)
  nom    <- names(b_hat)

  set.seed(20250919)
  sub_b <- sample(nrow(dat_b), 300000)
  dat_s <- dat_b[sub_b]
  # matriz de diseno tomada del propio modelo (mismas columnas que los coeficientes)
  X_full <- model.matrix(m_b, type = "rhs")
  stopifnot(nrow(X_full) == nrow(dat_b))
  X <- X_full[sub_b, , drop = FALSE]
  rm(X_full, dat_b); gc()
  norm_nom <- function(x) gsub("[[:space:]]|splines::", "", x)
  pos <- match(norm_nom(nom), norm_nom(colnames(X)))
  if (anyNA(pos)) {
    cat("\nNombres de coeficientes del modelo:\n");  print(nom)
    cat("\nNombres de columnas de la matriz de diseno:\n"); print(colnames(X))
    stop("No coinciden los nombres de coeficientes y columnas de la matriz de diseno (ver arriba).")
  }
  X <- as.matrix(X[, pos, drop = FALSE])
  col_ns <- grep("ns\\(EDAD_num", norm_nom(nom))
  stopifnot(length(col_ns) == 4)

  base_ns <- function(edad) {
    unclass(splines::ns(edad, knots = nudos_edad, Boundary.knots = limites_edad))
  }
  con_edad <- function(edad_vec) { Xn <- X; Xn[, col_ns] <- base_ns(edad_vec); Xn }
  edad0 <- dat_s$EDAD_num
  X_mas <- con_edad(edad0 + 0.5)
  X_men <- con_edad(edad0 - 0.5)

  simular <- function(V, etiqueta) {
    Bd <- MASS::mvrnorm(B_sim, mu = b_hat, Sigma = V)           # B x k
    ame_pt <- mean(plogis(drop(X_mas %*% b_hat)) - plogis(drop(X_men %*% b_hat))) * 100
    ame_d  <- numeric(B_sim)
    pr_ed  <- seq(0, 90, by = 10)
    pr_pt  <- vapply(pr_ed, function(a) mean(plogis(drop(con_edad(rep(a, nrow(X))) %*% b_hat))) * 100, numeric(1))
    pr_d   <- matrix(NA_real_, B_sim, length(pr_ed))
    Xa     <- lapply(pr_ed, function(a) con_edad(rep(a, nrow(X))))
    paso <- 50
    for (i0 in seq(1, B_sim, by = paso)) {
      ii <- i0:min(B_sim, i0 + paso - 1)
      Bt <- t(Bd[ii, , drop = FALSE])                           # k x paso
      ame_d[ii] <- colMeans(plogis(X_mas %*% Bt) - plogis(X_men %*% Bt)) * 100
      for (j in seq_along(pr_ed)) pr_d[ii, j] <- colMeans(plogis(Xa[[j]] %*% Bt)) * 100
    }
    list(
      ame = data.table(version_V = etiqueta, AME_pp_por_anio = round(ame_pt, 4),
                       IC95_lo_pp = round(unname(quantile(ame_d, 0.025)), 4),
                       IC95_hi_pp = round(unname(quantile(ame_d, 0.975)), 4),
                       prop_draws_neg = mean(ame_d < 0)),
      prob = data.table(version_V = etiqueta, edad = pr_ed, prob_ajustada_pp = round(pr_pt, 3),
                        IC95_lo_pp = round(apply(pr_d, 2, quantile, 0.025), 3),
                        IC95_hi_pp = round(apply(pr_d, 2, quantile, 0.975), 3))
    )
  }
  r1 <- simular(V_clus, "V agrupada por entidad (principal)")
  r2 <- simular(V_iid,  "V sin agrupar (sensibilidad)")
  modelo_prop_spline_ame_edad  <- rbind(r1$ame,  r2$ame)
  modelo_prop_spline_prob_edad <- rbind(r1$prob, r2$prob)
  guardar_checkpoint(list(ame_edad = modelo_prop_spline_ame_edad, prob_edad = modelo_prop_spline_prob_edad),
                     "modelo_propension_spline_boot_cve32")
  rm(m_b, X, X_mas, X_men, dat_s); gc()
}
cat("\n--- AME de la edad (spline) con IC por simulacion ---\n");  print(modelo_prop_spline_ame_edad)
cat("\n--- Probabilidad ajustada (pp) por edad ---\n");            print(modelo_prop_spline_prob_edad)

rm(datos_modelo_prop); gc()

cat("\n--- Promedios marginales (AME), en puntos porcentuales de probabilidad ---\n")
print(modelo_propension_ame)

# -----------------------------------------------------------------------------
# 11. EXPORTAR A EXCEL -- centrado únicamente en este análisis (no repite las
#     32 hojas del script principal).
# -----------------------------------------------------------------------------

wb <- createWorkbook()

hojas <- list(
  grupo_n_total              = tabla_n_grupo_ext,
  mortalidad_grupo_ext_anio  = mortalidad_grupo_ext_anio,
  mortalidad_grupo_ext_total = mortalidad_grupo_ext_total,
  estancia_grupo_ext_anio    = dias_estancia_grupo_ext_anio,
  estancia_grupo_ext_total   = dias_estancia_grupo_ext_total,
  mortalidad_or_corregido    = mortalidad_or_corregido,
  fig2a_or_ajustado          = or_fig2a,
  fig2b_expbeta_ajustado     = expb_fig2b,
  catalogo_en_resto_1_9      = catalogo_en_resto_1_9,
  diagnostico_decil2_cat3    = diagnostico_decil2,
  diagnostico_j96_r57_anio   = diagnostico_j96_r57_anio,
  tabla1_n_mort_estancia     = tabla1_n_mort_estancia,
  tabla1_sexo_3grupos        = tabla1_sexo_3grupos,
  tabla1_edad_3grupos        = tabla1_edad_3grupos,
  tabla1_institucion_3grupos = tabla1_institucion_3grupos,
  modelo_propension_coefs    = modelo_propension_coefs,
  modelo_propension_AME      = modelo_propension_ame,
  prop_spline_coefs          = modelo_propension_spline_coefs,
  prop_spline_wald_edad      = modelo_propension_spline_wald,
  prop_spline_AME            = modelo_propension_spline_ame,
  prop_spline_AME_edad_sim   = modelo_prop_spline_ame_edad,
  prop_spline_prob_por_edad  = modelo_prop_spline_prob_edad
)

for (nombre in names(hojas)) {
  addWorksheet(wb, nombre)
  writeData(wb, nombre, hojas[[nombre]])
}

stopifnot("Algún nombre de hoja excede 31 caracteres (límite de Excel)" =
            all(nchar(names(hojas)) <= 31))
stopifnot("Nombres de hoja duplicados" = !anyDuplicated(names(hojas)))

saveWorkbook(wb, file.path(dir_graficas, "potencialmente_raros_centrado.xlsx"), overwrite = TRUE)
cat("\nExcel guardado en Datos/analisis/potencialmente_raros_centrado.xlsx (",
    length(hojas), "hojas)\n")
