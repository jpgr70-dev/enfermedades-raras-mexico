# =============================================================================
# VERSION 17-oct-2026 (respuesta a M1, M2 y M3 de Andres Castaneda). Cambios
# respecto a la version del 11-sep-2026:
#  1. M1 (umbral): el descarte de 5/10 000 se aplica POR REGISTRO, no por
#     condicion (decision de JP, 17-oct): un registro por encima del umbral se
#     salta y la condicion puede entrar con otro registro/nivel de la
#     jerarquia. Se agrega el diagnostico de su efecto (hojas
#     Diagnostico_umbral y Condiciones_sobre_umbral).
#  2. M1 (idioma): "Categoria" se busca con y sin tilde (el XML trae
#     "Categoria (con tilde)"; hoy no cambia el resultado porque todas esas entidades
#     tambien tienen DisorderGroup == "Grupo de trastornos").
#  3. M2: los datos de estudios mexicanos ya NO se re-escalan por la razon
#     Mexico/mundo (se midieron en poblacion mexicana). Se EXCLUYEN los
#     overrides de las polidactilias postaxiales A y B (ORPHA:93334/93335):
#     el estudio de origen es de prevalencia al nacer (decision de JP).
#     Quedan los overrides de fenilcetonuria, hemofilia A y B y el de
#     hipotiroidismo congenito (este ultimo es "categoria": solo entra en el
#     escenario que incluye grupos y categorias).
#  4. M3: hoja "Sensibilidad": principal (ajuste Mexico/mundo), sin ajuste
#     (replica literal de Wakap), ajuste Mexico/Europa para condiciones con
#     dato europeo (ONU, Europe, Location code 908), punto medio de clase y
#     escenario con grupos y categorias; ademas el escalamiento del rango de
#     Wakap (3,585 enfermedades) al numero de condiciones de esta estimacion.
# =============================================================================
# =============================================================================
# Enfermedades raras en Mexico, 2014-2024 (poblacion de referencia: 2026)
# Prevalencia acumulada estimada -- replica de Nguengang Wakap et al. 2020
# (Eur J Hum Genet) aplicada a Mexico, con extension propia de ajuste por
# estructura de edad.
#
# NOTA: este script se diseno originalmente en Python (ver
# prevalencia_acumulada_mexico_2026.xlsx) y despues se porto linea-a-linea
# a R. La version en R YA SE CORRIO Y SE VALIDO (11-sep-2026): su resultado
# (prevalencia_acumulada_mexico_2026_desdeR.xlsx) coincide con el de Python
# hasta redondeo en la hoja "Resumen_nacional". Los pasos mas fragiles siguen
# siendo el parseo de los dos archivos de poblacion (ver FUENTES abajo).
#
# FUENTES:
# - es_product9_prev.xml   (Orphadata, prevalencia por enfermedad, 6,728 entidades)
# - es_product9_ages.xml   (Orphadata, edad promedio de inicio, 7,790 entidades)
# - matriz_clasificacion_v01.csv (clasificacion propia de 9 cuadrantes, columna orphacode)
# - 0_Pob_Mitad_1950_2070.xlsx (CONAPO, poblacion Mexico por edad simple)
# - WPP2024_SA1_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx (ONU) -- OJO:
#   este archivo NO trae el agregado "World" (solo grupos regionales/economicos);
#   para el total mundial se necesita el archivo completo por pais
#   (WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx, ~220MB) y filtrar
#   Location code == 900. Ademas: el archivo de la ONU tiene un bug de
#   metadata (el <dimension> del XML interno del xlsx queda desactualizado)
#   que hace que openxlsx/readxl con range/dimension automatico SOLO LEAN LAS
#   PRIMERAS FILAS -- si al leer con read.xlsx() obtienes muy pocas filas,
#   prueba leyendo con `openxlsx::read.xlsx(..., cols=1:110)` fijando cols/rows
#   explicitamente en vez de dejar que autodetecte el rango, o usa readxl.
#
# SUPUESTOS PRINCIPALES (ver hoja "Metodologia_supuestos" del Excel para el
# detalle completo -- aqui solo un resumen):
# 1. Seleccion de prevalencia por enfermedad: jerarquia geografica Mundial >
#    Europa (UE+Rusia+Turquia+Islandia) > EE.UU. (solo si la clase no supera
#    5/10 000). Solo PrevalenceType == "Prevalencia puntual".
# 2. Solo 4 clases de prevalencia (hasta 1-5/10 000, el umbral UE de
#    enfermedad rara) -- se EXCLUYEN por completo "6-9/10 000" y ">1/1000"
#    (no se acotan, se descartan, igual que hace el paper original).
# 3. ValMoy>0 con calidad "Value and class": min=max=ValMoy. Solo clase
#    ("Class only"): min/max = limites de la clase.
# 4. Enfermedades con unico dato "Caso(s)/Familia(s)": tasa = (casos+familias)
#    / poblacion mundial * 100000, aplicada a nivel de cada enfermedad (no
#    como bloque agregado ciego) para permitir el desglose por cuadrante.
# 5. Ajuste demografico propio (NO esta en el paper original ni en la replica
#    de Sudafrica, Orphanet J Rare Dis 2026): para enfermedades con UNA sola
#    categoria de edad de inicio, se reescala la tasa cruda por (fraccion de
#    poblacion MX en esa banda, 2026) / (fraccion de poblacion mundial en esa
#    banda, 2026). Bandas: Neonatal/Lactancia/Prenatal->0-1, Infancia->2-11,
#    Adolescencia->12-18, Edad adulta->19-65, Senectud->66+ (rangos de la
#    replica de Sudafrica). "Cualquier edad"/"Sin datos"/multiples categorias
#    -> sin ajustar (razon=1).
# 6. DOBLE CONTEO: se excluyen del calculo principal las entidades con
#    DisorderGroup=="Grupo de trastornos" o DisorderType=="Categoria"
#    (Orphadata las define como "coleccion de entidades clinicas", no una
#    enfermedad individual). Se investigo si el archivo "linearisation"
#    (product7) resolvia esto de forma mas rigurosa -- NO aplica: clasifica
#    por especialidad medica (33 categorias), no por jerarquia
#    enfermedad/subtipo. Quedan en el detalle marcadas, no se suman en el
#    escenario principal.
# 7. Sin filtro por PrevalenceValidationStatus (se usa todo, igual que el paper).
# 8. Ano de referencia: 2026 para poblacion de Mexico y del mundo.
#
# VALIDACION: el resultado en Python dio 4.80%-7.78% de la poblacion mexicana
# (6,454,342-10,458,957 personas), escenario principal (excluyendo grupos/
# categorias, con ajuste demografico). Benchmarks: paper original 3.5%-5.9%
# (UE/mundial, datos 2018); replica Sudafrica 2026: media 4.8%, rango 3.7%-5.9%.
#
# ACTUALIZACION 2026-09-11: se agrego un paso 7b que prioriza datos
# especificos de Mexico (verificados contra su articulo fuente) sobre la
# jerarquia Mundial/Europa/EE.UU. para 6 enfermedades. Con este cambio el
# escenario principal pasa a 4.85%-7.83% (6,524,596-10,529,211 personas).
# Ver el paso 7b para el detalle y la justificacion.
# =============================================================================

library(xml2)
library(data.table)
library(dplyr)
library(openxlsx)

# AJUSTA esta ruta a la carpeta Datos en la maquina donde corras esto.
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
# 1. Parsear es_product9_prev.xml
# -----------------------------------------------------------------------------
prev_xml <- read_xml(file.path(datos_path, "es_product9_prev.xml"))
disorders_prev <- xml_find_all(prev_xml, ".//Disorder")

message("Parseando product9_prev.xml (", length(disorders_prev), " entidades)...")

disease_meta <- data.table(
  orphacode = as.integer(xml_text(xml_find_first(disorders_prev, "./OrphaCode"))),
  nombre    = xml_text(xml_find_first(disorders_prev, "./Name")),
  tipo      = xml_text(xml_find_first(disorders_prev, "./DisorderType/Name")),
  grupo     = xml_text(xml_find_first(disorders_prev, "./DisorderGroup/Name"))
)

# Registros de prevalencia (uno por <Prevalence>, varias por enfermedad)
prev_records_list <- vector("list", length(disorders_prev))
for (i in seq_along(disorders_prev)) {
  d <- disorders_prev[[i]]
  oc <- as.integer(xml_text(xml_find_first(d, "./OrphaCode")))
  plist <- xml_find_first(d, "./PrevalenceList")
  if (is.na(plist)) next
  precs <- xml_find_all(plist, "./Prevalence")
  if (length(precs) == 0) next
  prev_records_list[[i]] <- data.table(
    orphacode = oc,
    prevalence_type = xml_text(xml_find_first(precs, "./PrevalenceType/Name")),
    qualification   = xml_text(xml_find_first(precs, "./PrevalenceQualification/Name")),
    prevalence_class = xml_text(xml_find_first(precs, "./PrevalenceClass/Name")),
    valmoy          = suppressWarnings(as.numeric(xml_text(xml_find_first(precs, "./ValMoy")))),
    geographic      = xml_text(xml_find_first(precs, "./PrevalenceGeographic/Name")),
    validation_status = xml_text(xml_find_first(precs, "./PrevalenceValidationStatus/Name"))
  )
}
prev_records <- rbindlist(prev_records_list, fill = TRUE)
message("Total registros de prevalencia: ", nrow(prev_records))

# -----------------------------------------------------------------------------
# 2. Parsear es_product9_ages.xml (edad de inicio)
# -----------------------------------------------------------------------------
ages_xml <- read_xml(file.path(datos_path, "es_product9_ages.xml"))
disorders_ages <- xml_find_all(ages_xml, ".//Disorder")
message("Parseando product9_ages.xml (", length(disorders_ages), " entidades)...")

onset_list <- vector("list", length(disorders_ages))
for (i in seq_along(disorders_ages)) {
  d <- disorders_ages[[i]]
  oc <- as.integer(xml_text(xml_find_first(d, "./OrphaCode")))
  al <- xml_find_first(d, "./AverageAgeOfOnsetList")
  cats <- if (!is.na(al)) xml_text(xml_find_all(al, "./AverageAgeOfOnset/Name")) else character(0)
  onset_list[[i]] <- data.table(orphacode = oc, categoria_edad = if (length(cats)) cats else NA_character_)
}
onset_dt <- rbindlist(onset_list, fill = TRUE)
# resumen por enfermedad: numero de categorias y la unica categoria si n==1
onset_summary <- onset_dt[!is.na(categoria_edad), .(
  n_categorias = .N,
  categoria_unica = if (.N == 1) categoria_edad[1] else NA_character_
), by = orphacode]
# enfermedades sin ninguna categoria (0 filas en onset_dt tras el filtro NA)
sin_dato_edad <- setdiff(unique(onset_dt$orphacode), onset_summary$orphacode)

# -----------------------------------------------------------------------------
# 3. Poblacion de Mexico 2026 (CONAPO), por edad simple
# -----------------------------------------------------------------------------
pob_mx_raw <- as.data.table(read.xlsx(file.path(datos_path, "0_Pob_Mitad_1950_2070.xlsx"), sheet = 1))
setnames(pob_mx_raw, iconv(names(pob_mx_raw), from = "UTF-8", to = "ASCII//TRANSLIT"))
pob_mx_raw[, entidad_ascii := toupper(iconv(ENTIDAD, from = "UTF-8", to = "ASCII//TRANSLIT"))]
pob_mx_2026 <- pob_mx_raw[entidad_ascii == "REPUBLICA MEXICANA" & ANO == 2026,
                          .(pob = sum(POBLACION)), by = EDAD]
setnames(pob_mx_2026, "EDAD", "edad")
mx_total <- sum(pob_mx_2026$pob)
message("Poblacion Mexico 2026: ", format(mx_total, big.mark=","))

# -----------------------------------------------------------------------------
# 4. Poblacion mundial 2026 (ONU WPP), por edad simple
#    Usa el archivo COMPLETO por pais (no el "SA1" de agregados especiales,
#    que no trae "World"), y filtra Location code == 900. Ver nota al inicio
#    sobre el bug de <dimension> -- si falla, prueba forzando cols/rows.
# -----------------------------------------------------------------------------
wpp_path <- file.path(datos_path, "WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx")
# CORREGIDO 2026-09-11: el archivo NO tiene una hoja combinada "Estimates and
# Medium variant" (ese nombre era un supuesto incorrecto). Las hojas estan
# separadas: "Estimates" (historico, hasta 2023) y "Medium variant"
# (proyeccion, 2024 en adelante). Como el ano de referencia es 2026 (un ano de
# proyeccion), se usa la hoja "Medium variant". El header sigue en la fila 17
# en ambas hojas (verificado). Si mas adelante usas un ano <=2023, cambia a
# sheet = "Estimates".
wpp_raw <- as.data.table(read.xlsx(wpp_path, sheet = "Medium variant", startRow = 17))
# columnas esperadas: Index, Variant, "Region, subregion, country or area *",
# Notes, "Location code", ISO3, ISO2, SDMX, Type, "Parent code", Year, luego
# 101 columnas de edad (0..99, "100+")
# CORREGIDO 2026-09-11: openxlsx::read.xlsx() sanea los nombres de columna
# (espacios/puntuacion -> "."), asi que "Location code" llega como
# "Location.code". Se busca solo por "Location" para no depender del
# separador exacto. Si algo mas falla aqui, corre `print(names(wpp_raw))`
# para ver los nombres reales.
message("Columnas de wpp_raw: ", paste(names(wpp_raw), collapse = " | "))
loc_col <- grep("Location", names(wpp_raw), value = TRUE)[1]
year_col <- grep("^Year$", names(wpp_raw), value = TRUE)[1]
stopifnot(!is.na(loc_col), !is.na(year_col))
age_cols <- names(wpp_raw)[which(names(wpp_raw) == year_col) + (1:101)]  # 0..99 + "100+"

wpp_world_2026 <- wpp_raw[get(loc_col) == 900 & get(year_col) == 2026]
stopifnot(nrow(wpp_world_2026) == 1)
pob_mundo_2026 <- data.table(
  edad = c(0:99, "100+"),
  pob  = as.numeric(unlist(wpp_world_2026[, ..age_cols])) * 1000  # el archivo viene en miles
)
world_total <- sum(pob_mundo_2026$pob)
message("Poblacion mundial 2026: ", format(world_total, big.mark=","))

# Europa (ONU, Location code 908), solo para el escenario de sensibilidad M3
wpp_europe_2026 <- wpp_raw[get(loc_col) == 908 & get(year_col) == 2026]
stopifnot("No se encontro Europa (Location code 908) en el archivo de la ONU" = nrow(wpp_europe_2026) == 1)
pob_europa_2026 <- data.table(
  edad = c(0:99, "100+"),
  pob  = as.numeric(unlist(wpp_europe_2026[, ..age_cols])) * 1000
)
europe_total <- sum(pob_europa_2026$pob)
message("Poblacion Europa 2026: ", format(europe_total, big.mark=","))

# -----------------------------------------------------------------------------
# 5. Razones de ajuste demografico por banda de edad (Mexico / Mundo, 2026)
# -----------------------------------------------------------------------------
mx_band_frac <- function(lo, hi) sum(pob_mx_2026[edad >= lo & edad <= hi, pob]) / mx_total
world_band_frac <- function(lo, hi) {
  edades_num <- suppressWarnings(as.numeric(pob_mundo_2026$edad))
  sum(pob_mundo_2026[!is.na(edades_num) & edades_num >= lo & edades_num <= hi, pob]) / world_total
}
bandas <- list(
  Neonatal_Infancia_0_1 = c(0, 1),
  Ninez_2_11            = c(2, 11),
  Adolescencia_12_18    = c(12, 18),
  Adulto_19_65          = c(19, 65)
)
band_ratios <- rbindlist(lapply(names(bandas), function(nm) {
  lo <- bandas[[nm]][1]; hi <- bandas[[nm]][2]
  mf <- mx_band_frac(lo, hi); wf <- world_band_frac(lo, hi)
  data.table(banda = nm, mx_frac = mf, world_frac = wf, ratio = mf / wf)
}))
# Vejez 66+ (incluye "100+" del lado mundial)
mx_frac_vejez <- sum(pob_mx_2026[edad >= 66, pob]) / mx_total
edades_num <- suppressWarnings(as.numeric(pob_mundo_2026$edad))
world_frac_vejez <- sum(pob_mundo_2026[is.na(edades_num) | edades_num >= 66, pob]) / world_total
band_ratios <- rbind(band_ratios, data.table(
  banda = "Vejez_66_mas", mx_frac = mx_frac_vejez, world_frac = world_frac_vejez,
  ratio = mx_frac_vejez / world_frac_vejez
))
print(band_ratios)

# Razones Mexico/Europa por banda (sensibilidad M3: los datos de Orphanet son
# en su mayoria europeos, asi que la razon relevante para esas condiciones
# podria ser Mexico/Europa y no Mexico/mundo)
edades_eu <- suppressWarnings(as.numeric(pob_europa_2026$edad))
eu_band_frac <- function(lo, hi) sum(pob_europa_2026[!is.na(edades_eu) & edades_eu >= lo & edades_eu <= hi, pob]) / europe_total
band_ratios_eu <- rbindlist(lapply(names(bandas), function(nm) {
  lo <- bandas[[nm]][1]; hi <- bandas[[nm]][2]
  data.table(banda = nm, ratio_eu = mx_band_frac(lo, hi) / eu_band_frac(lo, hi))
}))
eu_frac_vejez <- sum(pob_europa_2026[is.na(edades_eu) | edades_eu >= 66, pob]) / europe_total
band_ratios_eu <- rbind(band_ratios_eu, data.table(banda = "Vejez_66_mas", ratio_eu = mx_frac_vejez / eu_frac_vejez))
print(band_ratios_eu)

onset_to_band <- c(
  "Prenatal" = "Neonatal_Infancia_0_1", "Neonatal" = "Neonatal_Infancia_0_1",
  "Lactancia" = "Neonatal_Infancia_0_1", "Infancia" = "Ninez_2_11",
  "Adolescencia" = "Adolescencia_12_18", "Edad adulta" = "Adulto_19_65",
  "Senectud" = "Vejez_66_mas"
)

# -----------------------------------------------------------------------------
# 6. Clasificacion y seleccion de prevalencia por enfermedad
# -----------------------------------------------------------------------------
class_bounds <- list(
  "<1 / 1 000 000"  = c(0, 0.1),
  "1-9 / 1 000 000" = c(0.1, 0.9),
  "1-9 / 100 000"   = c(1, 9),
  "1-5 / 10 000"    = c(10, 50)
  # "6-9 / 10 000" y ">1 / 1000" EXCLUIDAS -- superan el umbral UE de rara.
)
eu_ru_tr_is <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Republic",
  "Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Ireland",
  "Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland",
  "Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom",
  "Russian Federation","Turkey","Iceland")

tier_of <- function(geo) {
  fifelse(geo == "Mundial", 1L,
  fifelse(geo == "Europa" | geo %in% eu_ru_tr_is, 2L,
  fifelse(geo == "United States", 3L, NA_integer_)))
}

record_value <- function(qualification, valmoy, clase) {
  # devuelve list(min=, max=) o NULL si no es utilizable
  if (!is.na(qualification) && qualification == "Value and class" && !is.na(valmoy) && valmoy > 0) {
    if (valmoy > 50) return(NULL)  # supera el umbral de rara -- excluir
    return(c(valmoy, valmoy))
  }
  if (is.na(clase) || !(clase %in% names(class_bounds))) return(NULL)
  class_bounds[[clase]]
}

selected <- list()
for (oc in unique(prev_records$orphacode)) {
  recs <- prev_records[orphacode == oc]
  point_recs <- recs[prevalence_type == "Prevalencia puntual"]
  chosen <- NULL; chosen_tier <- NA_integer_

  if (nrow(point_recs) > 0) {
    point_recs[, tier := tier_of(geographic)]
    for (t in c(1L, 2L, 3L)) {
      cands <- point_recs[tier == t]
      if (nrow(cands) == 0) next
      vals <- list()
      for (i in seq_len(nrow(cands))) {
        v <- record_value(cands$qualification[i], cands$valmoy[i], cands$prevalence_class[i])
        if (!is.null(v)) vals[[length(vals) + 1]] <- v
      }
      if (length(vals) > 0) {
        mins <- sapply(vals, `[`, 1); maxs <- sapply(vals, `[`, 2)
        chosen <- c(mean(mins), mean(maxs))
        chosen_tier <- t
        break
      }
    }
  }

  if (!is.null(chosen)) {
    selected[[as.character(oc)]] <- list(orphacode = oc, metodo = "prevalencia_puntual",
      tier = chosen_tier, min_per100k = chosen[1], max_per100k = chosen[2])
    next
  }

  cf_recs <- recs[prevalence_type == "Caso(s)/Familia(s)"]
  if (nrow(cf_recs) > 0) {
    casos <- sum(cf_recs[qualification == "Caso(s)", valmoy], na.rm = TRUE)
    familias <- sum(cf_recs[qualification == "Familia(s)", valmoy], na.rm = TRUE)
    tasa <- (casos + familias) / world_total * 100000
    selected[[as.character(oc)]] <- list(orphacode = oc, metodo = "caso_familia",
      tier = NA_integer_, min_per100k = tasa, max_per100k = tasa)
    next
  }

  selected[[as.character(oc)]] <- list(orphacode = oc, metodo = "excluido",
    tier = NA_integer_, min_per100k = NA_real_, max_per100k = NA_real_)
}
selected_dt <- rbindlist(selected)
message("Enfermedades con dato utilizable: ", selected_dt[!is.na(min_per100k), .N])

# -----------------------------------------------------------------------------
# 6b. DIAGNOSTICO DEL UMBRAL (M1, 17-oct-2026). El descarte de 5/10 000 se
#     aplica por REGISTRO (ver record_value): un registro puntual cuyo valor
#     supera 5/10 000 (ValMoy > 50 por 100 mil) o cuya clase es "6-9/10 000"
#     o ">1/1000" se salta; la condicion entra con otro registro del mismo
#     nivel, con un nivel inferior de la jerarquia, o por casos/familias. Aqui
#     se documenta cuanto importa eso.
# -----------------------------------------------------------------------------
es_sobre_umbral <- function(q, v, cl) {
  (!is.na(q) & q == "Value and class" & !is.na(v) & v > 50) |
  (!is.na(cl) & cl %in% c("6-9 / 10 000", ">1 / 1000"))
}
pr_puntual <- prev_records[prevalence_type == "Prevalencia puntual"]
pr_puntual[, `:=`(tier = tier_of(geographic),
                  sobre_umbral = es_sobre_umbral(qualification, valmoy, prevalence_class))]
umbral_dt <- pr_puntual[, .(n_reg_puntuales = .N, n_reg_sobre_umbral = sum(sobre_umbral)), by = orphacode]
# registros sobre el umbral en un nivel SUPERIOR al nivel finalmente usado
tier_elegido <- selected_dt[, .(orphacode, tier_elegido = tier)]
sup <- merge(pr_puntual[sobre_umbral == TRUE & !is.na(tier)], tier_elegido, by = "orphacode")
sup <- sup[!is.na(tier_elegido) & tier < tier_elegido]
umbral_dt[, entra_por_nivel_inferior := orphacode %in% unique(sup$orphacode)]
rm(pr_puntual, tier_elegido, sup)

# -----------------------------------------------------------------------------
# 7. Ajuste demografico + union con metadatos, cuadrante
# -----------------------------------------------------------------------------
selected_dt <- merge(selected_dt, disease_meta, by = "orphacode", all.x = TRUE)
selected_dt <- merge(selected_dt, onset_summary[, .(orphacode, n_categorias, categoria_unica)],
                      by = "orphacode", all.x = TRUE)
selected_dt[is.na(n_categorias), n_categorias := 0L]

selected_dt[, banda := onset_to_band[categoria_unica]]
selected_dt <- merge(selected_dt, band_ratios[, .(banda, ratio)], by = "banda", all.x = TRUE)
selected_dt[is.na(ratio), ratio := 1.0]  # sin ajustar: multiples categorias, "cualquier edad", sin dato
# Sensibilidad M3: razon Mexico/Europa para condiciones cuyo dato viene de Europa (tier 2)
selected_dt <- merge(selected_dt, band_ratios_eu, by = "banda", all.x = TRUE)
selected_dt[, ratio_eu_uso := fifelse(!is.na(tier) & tier == 2L & !is.na(ratio_eu), ratio_eu, ratio)]

# M1 (idioma): el XML trae "Categoria" con tilde; se aceptan ambas grafias
selected_dt[, es_grupo_o_categoria := (!is.na(grupo) & grupo == "Grupo de trastornos") |
                                      (!is.na(tipo) & tipo %in% c("Categoria", "Categor\u00eda"))]

selected_dt[, `:=`(
  min_per100k_ajustado = min_per100k * ratio,
  max_per100k_ajustado = max_per100k * ratio
)]
selected_dt[, `:=`(
  casos_mx_min_sin_ajustar = min_per100k / 100000 * mx_total,
  casos_mx_max_sin_ajustar = max_per100k / 100000 * mx_total,
  casos_mx_min_ajustado    = min_per100k_ajustado / 100000 * mx_total,
  casos_mx_max_ajustado    = max_per100k_ajustado / 100000 * mx_total
)]

# -----------------------------------------------------------------------------
# 7b. AJUSTE MANUAL: datos especificos de Mexico priorizados sobre la
#     jerarquia Mundial/Europa/EE.UU. (decision de JP, 2026-09-11)
#
# JP proporciono los 14 articulos fuente (mas 2 sustitutos) citados por
# Orphanet para los 16 registros geograficos="Mexico" en es_product9_prev.xml.
# Se verificaron los ValMoy de Orphanet contra el articulo original. En 6 de
# 16 el dato mexicano es de buena calidad (estudio nacional o multinacional
# con desglose por pais, cifra trazable al articulo) y JP autorizo priorizarlo
# sobre el nivel que normalmente usa el paper (Mundial > Europa > EE.UU.).
# Ver hoja "Metodologia_supuestos" del Excel Python, seccion "DATOS MEXICO
# PRIORIZADOS", para el detalle completo -- incluyendo el caso de Sirenomelia
# (ORPHA:3169), que se reviso pero se dejo FUERA de este ajuste por la
# magnitud del cambio (236x vs. el valor Mundial actual) -- pendiente de
# decision aparte.
#
# tier == 9 identifica estas filas como override manual (los tiers 1/2/3 del
# resto del script siguen significando Mundial/Europa/EE.UU. como antes).
# La columna fuente_override queda NA para todas las demas filas.

selected_dt[, fuente_override := NA_character_]

mexico_overrides <- data.table(
  # 17-oct-2026: se EXCLUYEN los overrides de polidactilia postaxial A y B
  # (ORPHA:93334 y 93335): el estudio de origen (RYVEMCE) es de prevalencia al
  # nacer, que la regla general de este script no usa (decision de JP).
  orphacode = c(716, 442, 98879, 98878),
  mx_valmoy = c(3.5999, 43.0, 0.29, 2.2),
  mx_fuente = c(
    "32668217[PMID] (Hillert et al. 2020, BIOPKU)",
    "18782254[PMID] (Rendon-Macias et al. 2008, IMSS, screening nacional 2000-2004)",
    "21649801[PMID] (Stonebraker et al. 2011, encuesta WFH)",
    "19845775[PMID] (Stonebraker et al. 2010, encuesta WFH)"
  )
)

for (i in seq_len(nrow(mexico_overrides))) {
  oc <- mexico_overrides$orphacode[i]
  val <- mexico_overrides$mx_valmoy[i]
  fuente <- mexico_overrides$mx_fuente[i]
  row_idx <- which(selected_dt$orphacode == oc)
  if (length(row_idx) == 0) {
    warning(sprintf("ORPHA:%s no encontrada en selected_dt -- revisar antes de confiar en el resultado", oc))
    next
  }
  # 17-oct-2026 (M2): el dato mexicano NO se re-escala por la razon Mexico/mundo:
  # se midio en poblacion mexicana. Razon = 1 en ambos ajustes.
  ratio <- 1.0
  selected_dt[row_idx, `:=`(ratio = 1.0, ratio_eu_uso = 1.0)]
  selected_dt[row_idx, `:=`(
    metodo = "mexico_override",
    tier = 9L,
    fuente_override = fuente,
    min_per100k = val,
    max_per100k = val,
    min_per100k_ajustado = val * ratio,
    max_per100k_ajustado = val * ratio,
    casos_mx_min_sin_ajustar = val / 100000 * mx_total,
    casos_mx_max_sin_ajustar = val / 100000 * mx_total,
    casos_mx_min_ajustado    = (val * ratio) / 100000 * mx_total,
    casos_mx_max_ajustado    = (val * ratio) / 100000 * mx_total
  )]
}
rm(mexico_overrides)

# Casos con el ajuste Mexico/Europa (sensibilidad M3)
selected_dt[, `:=`(
  casos_mx_min_europa = min_per100k * ratio_eu_uso / 100000 * mx_total,
  casos_mx_max_europa = max_per100k * ratio_eu_uso / 100000 * mx_total
)]


# Cuadrante (matriz propia, cruce EXACTO por orphacode -- no requiere CIE-10)
matriz <- fread(file.path(dirname(datos_path), "Datos/analisis/matriz_clasificacion_v01.csv"),
                 encoding = "UTF-8")
# (ajusta la ruta de arriba si tu matriz esta en otro lugar; normalmente
#  "<datos_path>/analisis/matriz_clasificacion_v01.csv")
matriz_path_alt <- file.path(datos_path, "analisis", "matriz_clasificacion_v01.csv")
if (file.exists(matriz_path_alt)) matriz <- fread(matriz_path_alt, encoding = "UTF-8")
selected_dt <- merge(selected_dt, matriz[, .(orphacode, cuadrante, nombre_matriz = nombre_es)],
                      by = "orphacode", all.x = TRUE)

# -----------------------------------------------------------------------------
# 8. Totales y desglose
# -----------------------------------------------------------------------------
usable_all  <- selected_dt[!is.na(min_per100k)]
usable_leaf <- usable_all[es_grupo_o_categoria == FALSE]

resumen <- data.table(
  escenario = c("Todas utilizables (incl. grupos/categorias)",
                "Excluyendo grupos/categorias (ESCENARIO PRINCIPAL)",
                "Excluyendo grupos/categorias -- SIN ajuste demografico"),
  n = c(nrow(usable_all), nrow(usable_leaf), nrow(usable_leaf)),
  casos_min = c(sum(usable_all$casos_mx_min_ajustado), sum(usable_leaf$casos_mx_min_ajustado),
                sum(usable_leaf$casos_mx_min_sin_ajustar)),
  casos_max = c(sum(usable_all$casos_mx_max_ajustado), sum(usable_leaf$casos_mx_max_ajustado),
                sum(usable_leaf$casos_mx_max_sin_ajustar))
)
resumen[, `:=`(pct_min = casos_min / mx_total * 100, pct_max = casos_max / mx_total * 100)]
print(resumen)

# -----------------------------------------------------------------------------
# 8b. SENSIBILIDAD (M3, 17-oct-2026). Todos los escenarios excluyen grupos y
#     categorias salvo el ultimo. El "punto medio" usa (min + max) / 2 de cada
#     condicion (valor puntual: min = max; clase: punto medio de la clase), como
#     en la replica de Sudafrica (Malherbe et al.).
# -----------------------------------------------------------------------------
suma <- function(dt, a, b) c(min = sum(dt[[a]]), max = sum(dt[[b]]))
esc <- list(
  "Principal: ajuste Mexico/mundo (datos mexicanos sin ajustar)" = suma(usable_leaf, "casos_mx_min_ajustado", "casos_mx_max_ajustado"),
  "Sin ajuste demografico (replica literal de Nguengang Wakap)"   = suma(usable_leaf, "casos_mx_min_sin_ajustar", "casos_mx_max_sin_ajustar"),
  "Ajuste Mexico/Europa para condiciones con dato europeo"         = suma(usable_leaf, "casos_mx_min_europa", "casos_mx_max_europa"),
  "Incluye grupos y categorias (con ajuste Mexico/mundo)"          = suma(usable_all,  "casos_mx_min_ajustado", "casos_mx_max_ajustado")
)
sensibilidad <- data.table(
  escenario = names(esc),
  n = c(nrow(usable_leaf), nrow(usable_leaf), nrow(usable_leaf), nrow(usable_all)),
  casos_min = sapply(esc, `[`, "min"), casos_max = sapply(esc, `[`, "max")
)
sensibilidad[, `:=`(pct_min = casos_min / mx_total * 100, pct_max = casos_max / mx_total * 100,
                    millones_min = casos_min / 1e6, millones_max = casos_max / 1e6)]
punto_medio <- function(dt, a, b) sum((dt[[a]] + dt[[b]]) / 2)
medio <- data.table(
  escenario = c("Punto medio de clase, con ajuste Mexico/mundo", "Punto medio de clase, sin ajuste",
                "Punto medio de clase, ajuste Mexico/Europa"),
  casos = c(punto_medio(usable_leaf, "casos_mx_min_ajustado", "casos_mx_max_ajustado"),
            punto_medio(usable_leaf, "casos_mx_min_sin_ajustar", "casos_mx_max_sin_ajustar"),
            punto_medio(usable_leaf, "casos_mx_min_europa", "casos_mx_max_europa"))
)
medio[, `:=`(pct = casos / mx_total * 100, millones = casos / 1e6)]
# Promedio empirico por clase (17-oct-2026): las condiciones de solo clase aportan,
# en lugar del limite o punto medio de su clase, el promedio de los valores puntuales
# de las condiciones con estimacion puntual de Orphanet cuyo valor cae en esa misma
# clase; asi pesan mas las clases donde las enfermedades observadas son mas prevalentes.
# Las demas condiciones (valor puntual, casos en familia, datos mexicanos) aportan su valor.
empirico <- function(dt) {
  d <- copy(dt)
  es_clase <- d$metodo == "prevalencia_puntual" & d$max_per100k > d$min_per100k + 1e-12
  v <- d[metodo == "prevalencia_puntual" & !es_clase, max_per100k]
  d[, m_emp := (min_per100k + max_per100k) / 2]
  cl <- unique(d[es_clase, .(min_per100k, max_per100k)])
  for (i in seq_len(nrow(cl))) {
    lo <- cl$min_per100k[i]; hi <- cl$max_per100k[i]
    vv <- v[v >= lo - 1e-12 & v <= hi + 1e-12]
    if (length(vv) > 0) d[es_clase & min_per100k == lo & max_per100k == hi, m_emp := mean(vv)]
  }
  d[, razon_aj := fifelse(casos_mx_max_sin_ajustar > 0, casos_mx_max_ajustado / casos_mx_max_sin_ajustar, 1)]
  d[, `:=`(c_sin = fifelse(es_clase, m_emp * mx_total / 1e5, (casos_mx_min_sin_ajustar + casos_mx_max_sin_ajustar) / 2),
           c_aj  = fifelse(es_clase, m_emp * mx_total / 1e5 * razon_aj, (casos_mx_min_ajustado + casos_mx_max_ajustado) / 2))]
  c(con_ajuste = sum(d$c_aj), sin_ajuste = sum(d$c_sin), n_clase = sum(es_clase))
}
emp <- empirico(usable_leaf)
promedio_empirico <- data.table(
  escenario = c("Promedio empirico por clase, con ajuste Mexico/mundo", "Promedio empirico por clase, sin ajuste"),
  casos = c(emp["con_ajuste"], emp["sin_ajuste"]), n_solo_clase = emp["n_clase"]
)
promedio_empirico[, `:=`(pct = casos / mx_total * 100, millones = casos / 1e6)]
print(promedio_empirico)
# Escalamiento del rango de Wakap (3.5%-5.9% con 3,585 enfermedades) al numero de
# condiciones de esta estimacion: explica por que se supera el techo de 5.9%
n_wakap <- 3585L
escala_wakap <- data.table(
  n_wakap = n_wakap, n_estimacion = nrow(usable_leaf),
  rango_wakap_min = 3.5, rango_wakap_max = 5.9,
  rango_escalado_min = 3.5 * nrow(usable_leaf) / n_wakap,
  rango_escalado_max = 5.9 * nrow(usable_leaf) / n_wakap
)
# Aporte de valores puntuales vs. clases (Vicente et al., 2021: efecto de sumar los
# limites de clase). Solo condiciones con prevalencia puntual de Orphanet.
usable_pp <- usable_leaf[metodo == "prevalencia_puntual"]
usable_pp[, base := fifelse(min_per100k == max_per100k, "Valor puntual", "Solo clase")]
aporte_clase <- usable_pp[, .(n = .N, pct_min = sum(casos_mx_min_sin_ajustar) / mx_total * 100,
                              pct_max = sum(casos_mx_max_sin_ajustar) / mx_total * 100), by = base]
print(sensibilidad); print(medio); print(escala_wakap); print(aporte_clase)

# Diagnostico del umbral (M1)
dg <- merge(usable_leaf[, .(orphacode, nombre, tier, metodo, casos_mx_min_sin_ajustar, casos_mx_max_sin_ajustar)],
            umbral_dt, by = "orphacode", all.x = TRUE)
dg[is.na(n_reg_sobre_umbral), n_reg_sobre_umbral := 0L]
excluidas_umbral <- selected_dt[metodo == "excluido" & es_grupo_o_categoria == FALSE, .(orphacode, nombre)]
excluidas_umbral <- merge(excluidas_umbral, umbral_dt, by = "orphacode")[n_reg_sobre_umbral > 0]
diag_umbral <- data.table(
  concepto = c("Condiciones incluidas con al menos un registro puntual sobre el umbral (5/10 000)",
               "  su aporte al rango, sin ajuste (casos minimo)",
               "  su aporte al rango, sin ajuste (casos maximo)",
               "Condiciones que entran por un nivel inferior tras descartar un registro sobre el umbral en un nivel superior",
               "  su aporte, sin ajuste (casos maximo)",
               "Condiciones excluidas por completo (sin otro registro utilizable) con algun registro sobre el umbral"),
  valor = c(dg[n_reg_sobre_umbral > 0, .N],
            dg[n_reg_sobre_umbral > 0, sum(casos_mx_min_sin_ajustar)],
            dg[n_reg_sobre_umbral > 0, sum(casos_mx_max_sin_ajustar)],
            dg[entra_por_nivel_inferior == TRUE, .N],
            dg[entra_por_nivel_inferior == TRUE, sum(casos_mx_max_sin_ajustar)],
            nrow(excluidas_umbral))
)
print(diag_umbral)
cond_sobre_umbral <- dg[n_reg_sobre_umbral > 0][order(-casos_mx_max_sin_ajustar)]

# CORREGIDO 2026-09-11: fread() detecto la columna "cuadrante" del CSV como
# integer (son digitos puros: 1,2,3...), pero fifelse() exige que "yes" y "no"
# sean del mismo tipo -- de ahi el error "'no' es integer pero 'yes' es
# character". Se fuerza cuadrante a character antes de comparar.
por_cuadrante <- usable_leaf[, .(
  n = .N, casos_min = sum(casos_mx_min_ajustado), casos_max = sum(casos_mx_max_ajustado)
), by = .(cuadrante = fifelse(is.na(cuadrante), "Sin cuadrante asignado", as.character(cuadrante)))]
por_cuadrante[, `:=`(pct_min = casos_min/mx_total*100, pct_max = casos_max/mx_total*100)]
print(por_cuadrante[order(cuadrante)])

# -----------------------------------------------------------------------------
# 9. Exportar
# -----------------------------------------------------------------------------
wb <- createWorkbook()
addWorksheet(wb, "Resumen_nacional"); writeData(wb, "Resumen_nacional", resumen)
addWorksheet(wb, "Sensibilidad"); writeData(wb, "Sensibilidad", sensibilidad)
writeData(wb, "Sensibilidad", medio, startRow = nrow(sensibilidad) + 4)
writeData(wb, "Sensibilidad", promedio_empirico, startRow = nrow(sensibilidad) + nrow(medio) + 7)
writeData(wb, "Sensibilidad", escala_wakap, startRow = nrow(sensibilidad) + nrow(medio) + nrow(promedio_empirico) + 10)
writeData(wb, "Sensibilidad", aporte_clase, startRow = nrow(sensibilidad) + nrow(medio) + nrow(promedio_empirico) + 14)
addWorksheet(wb, "Diagnostico_umbral"); writeData(wb, "Diagnostico_umbral", diag_umbral)
writeData(wb, "Diagnostico_umbral", excluidas_umbral, startRow = nrow(diag_umbral) + 4)
addWorksheet(wb, "Condiciones_sobre_umbral"); writeData(wb, "Condiciones_sobre_umbral", cond_sobre_umbral)
addWorksheet(wb, "Por_cuadrante"); writeData(wb, "Por_cuadrante", por_cuadrante[order(cuadrante)])
addWorksheet(wb, "Detalle_enfermedades"); writeData(wb, "Detalle_enfermedades", selected_dt)
saveWorkbook(wb, file.path(datos_path, "analisis", "prevalencia_acumulada_mexico_2026_desdeR 17Oct2026.xlsx"),
             overwrite = TRUE)
message("Listo. Resultados en prevalencia_acumulada_mexico_2026_desdeR 17Oct2026.xlsx (hojas Resumen_nacional, Sensibilidad, Diagnostico_umbral...).")
