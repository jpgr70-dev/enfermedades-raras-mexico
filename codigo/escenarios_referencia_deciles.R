# =============================================================================
# ESCENARIOS DE GRUPO DE REFERENCIA PARA EL MODELO DE DECILES (comentario M8 de
# Andrés, 11-oct-2026). Script NUEVO y AUTOCONTENIDO: no modifica
# analisis_potencialmente_raros_centrado-1.R ni sus checkpoints existentes.
#
# QUÉ HACE
#  Reutiliza el checkpoint "datos_clasificados" del script C (no vuelve a leer
#  ni a clasificar los egresos) y reconstruye EXACTAMENTE la misma muestra de
#  3,000,000 egresos (semilla 20250918). Sobre esa muestra ajusta la mortalidad
#  hospitalaria (logística con reducción de sesgo tipo Firth, brglm2) y los
#  días de estancia (lm sobre log) de "Potencialmente raros" contra distintos
#  grupos de referencia:
#    E1  deciles 9-10 del resto        (principal de C; desde 23-oct-2026 se
#                                       ESTIMA aqui, restringido a raros + deciles 9-10)
#    E2  deciles 1-8 del resto         (la comparación cruda de Andrés: 4.3 vs 4.1%)
#    E3  todo el resto, deciles 1-10   (más cercana a Tröster et al.)
#    E4  = E3 sin el capítulo XV (O00-O99, embarazo, parto y puerperio)
#    E5  = E4 con edad en spline natural (df = 4), como Tröster
#    E6  referencia = enfermedades comparables: todo el resto SIN los capítulos
#        O, P, Q (embarazo/parto; perinatales; congénitas) ni R-Z (síntomas,
#        lesiones, causas externas, factores de contacto), con edad en spline.
#        Es decir, solo capítulos A-N. (Agregado 13-oct-2026.)
#    E6b = sensibilidad de E6: conserva P y Q (excluye solo O y R-Z), spline.
#  NOTA (corrección 13-oct-2026): capitulos_excluir del script C (R-Z) define
#  el universo de los "potencialmente raros", pero NO restringe el grupo de
#  referencia: éste incluye capítulos O y R-Z (p. ej. R57, R68 en el decil 2).
#  Por eso E3-E5 comparan contra una referencia más heterogénea; E6 lo corrige.
#
#  Además, sobre TODOS los egresos (sin muestrear), produce:
#   - mortalidad cruda, % mujeres y % de 18-34 años por grupo de referencia
#   - los códigos CIE-10 (3 caracteres) que dominan los deciles 9-10
#   - los códigos del decil 2 con más defunciones y mayor letalidad (para
#     identificar qué produce el OR cercano a 25 de la Figura 3)
#   - categorías que solo aparecen en defunciones (comentario M6: caen en el
#     decil 1 por construcción)
#
# COSTO / ADVERTENCIA
#  Corre en el equipo Windows. Cargar el checkpoint de egresos ocupa varios GB
#  de RAM. Cada modelo logístico Firth sobre 3 millones de filas tarda del
#  orden de 10-15 min (el del script C con 11 niveles tardaba eso; estos son
#  binarios y deberían ser algo más rápidos): 4 modelos nuevos ~ 40-60 min.
#  Cada modelo guarda su propio checkpoint (esc_*.rds): si la sesión se corta,
#  se vuelve a correr el script completo y solo se rehace lo pendiente.
#  VERSIÓN 23-oct-2026 (corrección de edad): se ajustan los SIETE escenarios (E1 a E6b) desde
#  cero, con checkpoints esc_* nuevos en checkpoints_edadcorr (~1-1.5 h en total). E1 se
#  estima restringido (potencialmente raros + deciles 9-10) y ahora tiene estancia.
#  Requiere haber corrido antes el script C corregido (datos_clasificados en checkpoints_edadcorr).
#  Escribe a un xlsx NUEVO con fecha en la carpeta edadcorr; los anteriores quedan intactos.
#  NO SE HA EJECUTADO tras la corrección (solo revisada la sintaxis): si algo falla, revisar
#  primero los nombres de columnas del bloque "VERIFICACIÓN DE COLUMNAS".
# =============================================================================

library(data.table)
library(openxlsx)
library(brglm2)
library(splines)

# Ruta de datos: se define una sola vez en config_rutas.R (carpeta del paquete); si datos_path ya
# existe en la sesion se respeta; si no, la variable de entorno ER_DATOS.
if (!exists("datos_path", inherits = TRUE)) {
  if (file.exists("config_rutas.R")) {
    source("config_rutas.R", encoding = "UTF-8")
  } else {
    datos_path <- Sys.getenv("ER_DATOS", unset = "")
    if (!nzchar(datos_path)) stop("No encontre config_rutas.R en ", getwd(), " y ER_DATOS no esta definida: haz setwd() a la carpeta codigo/ o define ER_DATOS.")
  }
}
# CORRECCION 23-oct-2026 (edad): lee los datos clasificados del script C
# corregido (checkpoints_edadcorr) y escribe en la carpeta aparte. Los checkpoints esc_* se
# guardan tambien ahi, asi que ninguno se reutiliza de la corrida con la edad sin corregir.
dir_checkpoints <- file.path(datos_path, "analisis", "checkpoints_edadcorr")
dir_salida      <- file.path(datos_path, "analisis", "edadcorr")
if (!dir.exists(dir_salida)) dir.create(dir_salida, recursive = TRUE)

ruta_chk <- function(nombre) file.path(dir_checkpoints, paste0(nombre, ".rds"))
hay_chk  <- function(nombre) file.exists(ruta_chk(nombre))

# -----------------------------------------------------------------------------
# 1. Cargar datos clasificados (checkpoint del script C)
# -----------------------------------------------------------------------------
stopifnot("Falta el checkpoint datos_clasificados.rds: corre primero el script C" =
            hay_chk("datos_clasificados"))
datos_chk         <- readRDS(ruta_chk("datos_clasificados"))
egresos_completos <- datos_chk$egresos_completos
nombres_cat3      <- tryCatch(as.data.table(datos_chk$nombre_cat3_completo),
                              error = function(e) NULL)
rm(datos_chk); gc()

# --- VERIFICACIÓN DE COLUMNAS ------------------------------------------------
columnas_necesarias <- c("cat3", "SEXO", "ANIO", "EDAD_num", "DIAS_ESTA_num",
                         "defuncion", "potencialmente_raro", "decil_original")
faltan <- setdiff(columnas_necesarias, names(egresos_completos))
stopifnot("Faltan columnas en egresos_completos" = length(faltan) == 0)

# -----------------------------------------------------------------------------
# 2. Misma muestra de 3,000,000 egresos que usa el script C (sección 5)
# -----------------------------------------------------------------------------
n_muestra_modelos <- 3000000
set.seed(20250918)
idx_universo <- egresos_completos[, which(!is.na(EDAD_num) & SEXO %in% c("1", "2"))]
idx_muestra  <- sample(idx_universo, min(n_muestra_modelos, length(idx_universo)))
datos_muestra <- egresos_completos[idx_muestra,
                  .(cat3, SEXO, ANIO, EDAD_num, DIAS_ESTA_num, defuncion,
                    potencialmente_raro, decil_original)]
rm(idx_universo, idx_muestra); gc()
datos_muestra[, SEXO_f := factor(SEXO, levels = c("2", "1"),
                                 labels = c("Femenino", "Masculino"))]
datos_muestra[, cap_XV := substr(cat3, 1, 1) == "O"]
datos_muestra[, letra_cap := substr(cat3, 1, 1)]
# E6: solo capítulos de enfermedad A-N; E6b: A-N + P + Q
caps_E6  <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N")
caps_E6b <- c(caps_E6, "P", "Q")
cat("Muestra:", format(nrow(datos_muestra), big.mark = ","), "egresos\n")

# -----------------------------------------------------------------------------
# 3. Modelos por escenario
# -----------------------------------------------------------------------------
extraer <- function(m, nombre_coef = "grupoPotencialmente raros", exponenciar = TRUE) {
  cf <- summary(m)$coefficients
  est <- cf[nombre_coef, "Estimate"]; se <- cf[nombre_coef, "Std. Error"]
  pcol <- grep("^Pr\\(", colnames(cf), value = TRUE)
  data.table(valor = exp(est), lo = exp(est - 1.96 * se), hi = exp(est + 1.96 * se),
             p = cf[nombre_coef, pcol])
}

ajustar_escenario <- function(id, descripcion, datos, spline_edad = FALSE) {
  nombre_chk <- paste0("esc_", id)
  if (hay_chk(nombre_chk)) {
    cat("Checkpoint encontrado:", nombre_chk, "\n"); return(readRDS(ruta_chk(nombre_chk)))
  }
  cat("\n=== Ajustando", id, "-", descripcion, "===\n"); t0 <- Sys.time()
  datos <- copy(datos)
  datos[, grupo := factor(potencialmente_raro, levels = c(FALSE, TRUE),
                          labels = c("Referencia", "Potencialmente raros"))]
  f_edad <- if (spline_edad) "ns(EDAD_num, df = 4)" else "EDAD_num"
  rhs    <- paste("grupo +", f_edad, "+ SEXO_f + factor(ANIO)")

  m_mort <- glm(as.formula(paste("defuncion ~", rhs)), data = datos,
                family = binomial(), method = "brglmFit", type = "AS_mean")
  r_mort <- extraer(m_mort); rm(m_mort); gc()

  d1 <- datos[!is.na(DIAS_ESTA_num) & DIAS_ESTA_num > 0]
  m_e1 <- lm(as.formula(paste("log(DIAS_ESTA_num) ~", rhs)), data = d1)
  r_e1 <- extraer(m_e1); rm(m_e1, d1); gc()

  d0 <- datos[!is.na(DIAS_ESTA_num) & DIAS_ESTA_num >= 0]   # conserva estancias de 0 días
  m_e0 <- lm(as.formula(paste("log(DIAS_ESTA_num + 1) ~", rhs)), data = d0)
  r_e0 <- extraer(m_e0); rm(m_e0, d0); gc()

  res <- data.table(
    escenario = id, descripcion = descripcion,
    n_modelo = nrow(datos),
    n_raros = datos[grupo == "Potencialmente raros", .N],
    n_referencia = datos[grupo == "Referencia", .N],
    defunciones_raros = datos[grupo == "Potencialmente raros", sum(defuncion)],
    defunciones_referencia = datos[grupo == "Referencia", sum(defuncion)],
    OR_mortalidad = r_mort$valor, OR_lo = r_mort$lo, OR_hi = r_mort$hi, OR_p = r_mort$p,
    expbeta_estancia_sin_ceros = r_e1$valor, est1_lo = r_e1$lo, est1_hi = r_e1$hi,
    expbeta_estancia_con_ceros_log_mas1 = r_e0$valor, est0_lo = r_e0$lo, est0_hi = r_e0$hi,
    minutos = round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
  )
  saveRDS(res, ruta_chk(nombre_chk)); cat("Checkpoint guardado:", nombre_chk, "\n")
  res
}

resto <- !datos_muestra$potencialmente_raro
res_list <- list()

# E1: referencia deciles 9-10 del resto. CORRECCION 23-oct-2026 (decision de JP): se estima
# RESTRINGIDO a los potencialmente raros y los deciles 9-10, igual que los demas escenarios y
# como dice Metodos (antes se leia del checkpoint fig2a_or_ref910 de C, que es el modelo con
# toda la muestra y todos los deciles). Incluye estancia, con y sin ceros.
res_list[["E1"]] <- ajustar_escenario("E1", "Referencia deciles 9-10 del resto",
  datos_muestra[potencialmente_raro | (resto & decil_original >= 9)])
res_list[["E2"]] <- ajustar_escenario("E2", "Referencia deciles 1-8 del resto",
  datos_muestra[potencialmente_raro | (resto & decil_original <= 8)])
res_list[["E3"]] <- ajustar_escenario("E3", "Referencia todo el resto (deciles 1-10)",
  datos_muestra)
res_list[["E4"]] <- ajustar_escenario("E4", "E3 sin capitulo XV (embarazo, parto, puerperio)",
  datos_muestra[cap_XV == FALSE])
res_list[["E5"]] <- ajustar_escenario("E5", "E4 con edad en spline natural (df = 4)",
  datos_muestra[cap_XV == FALSE], spline_edad = TRUE)
res_list[["E6"]] <- ajustar_escenario("E6",
  "Referencia solo capitulos A-N (sin O, P, Q, R-Z); edad spline",
  datos_muestra[letra_cap %in% caps_E6], spline_edad = TRUE)
res_list[["E6b"]] <- ajustar_escenario("E6b",
  "Referencia capitulos A-N + P + Q (sin O, R-Z); edad spline",
  datos_muestra[letra_cap %in% caps_E6b], spline_edad = TRUE)
resumen_escenarios <- rbindlist(res_list, use.names = TRUE, fill = TRUE)
print(resumen_escenarios)
rm(datos_muestra); gc()

# -----------------------------------------------------------------------------
# 4. Descriptivos sobre TODOS los egresos (sin muestrear)
# -----------------------------------------------------------------------------
resumir <- function(dt, etiqueta) {
  dt[, .(grupo = etiqueta, n_egresos = .N, defunciones = sum(defuncion),
         mortalidad_pct = round(100 * mean(defuncion), 2),
         pct_mujeres = round(100 * mean(SEXO == "2"), 1),
         pct_18_34 = round(100 * mean(EDAD_num >= 18 & EDAD_num <= 34, na.rm = TRUE), 1))]
}
e <- egresos_completos
mortalidad_cruda <- rbindlist(list(
  resumir(e[potencialmente_raro == TRUE], "Potencialmente raros"),
  resumir(e[potencialmente_raro == FALSE & decil_original <= 8], "Resto deciles 1-8"),
  resumir(e[potencialmente_raro == FALSE & decil_original >= 9], "Resto deciles 9-10"),
  resumir(e[potencialmente_raro == FALSE & decil_original >= 2], "Resto deciles 2-10"),
  resumir(e[potencialmente_raro == FALSE], "Todo el resto (1-10)"),
  resumir(e[potencialmente_raro == FALSE & substr(cat3, 1, 1) != "O"], "Todo el resto sin capitulo XV"),
  resumir(e[potencialmente_raro == FALSE & substr(cat3, 1, 1) %in% caps_E6], "E6: resto capitulos A-N"),
  resumir(e[potencialmente_raro == FALSE & substr(cat3, 1, 1) %in% caps_E6b], "E6b: resto capitulos A-N + P + Q")
))
print(mortalidad_cruda)

con_nombre <- function(tab) {
  if (!is.null(nombres_cat3) && "cat3" %in% names(nombres_cat3)) {
    n2 <- unique(nombres_cat3, by = "cat3")
    tab <- merge(tab, n2, by = "cat3", all.x = TRUE, sort = FALSE)
  }
  tab
}

# Códigos que dominan los deciles 9-10
comp_9_10 <- e[potencialmente_raro == FALSE & decil_original >= 9,
               .(n_egresos = .N, defunciones = sum(defuncion),
                 mortalidad_pct = round(100 * mean(defuncion), 3),
                 pct_mujeres = round(100 * mean(SEXO == "2"), 1))
               , by = cat3][order(-n_egresos)]
comp_9_10[, pct_del_grupo := round(100 * n_egresos / sum(n_egresos), 1)]
comp_9_10 <- con_nombre(head(comp_9_10, 30))
print(comp_9_10)

# Decil 2: qué códigos aportan defunciones / letalidad
dec2 <- e[potencialmente_raro == FALSE & decil_original == 2,
          .(n_egresos = .N, defunciones = sum(defuncion),
            mortalidad_pct = round(100 * mean(defuncion), 2)), by = cat3]
dec2_por_defunciones <- con_nombre(head(dec2[order(-defunciones)], 25))
dec2_por_letalidad   <- con_nombre(head(dec2[n_egresos >= 1000][order(-mortalidad_pct)], 25))
print(dec2_por_defunciones); print(dec2_por_letalidad)

# Categorías que solo aparecen en defunciones (comentario M6)
solo_def <- e[, .(n_egresos = .N, vivos = sum(!defuncion), defunciones = sum(defuncion)),
              by = cat3][vivos == 0]
cat("\nCategorías (3 caracteres) con SOLO registros de defunción:", nrow(solo_def),
    "| defunciones en ellas:", solo_def[, sum(defunciones)], "\n")
solo_def <- con_nombre(solo_def[order(-defunciones)])

# Mortalidad cruda y estancia por capítulo (letra) en el resto vs. raros
por_capitulo <- e[, .(n_egresos = .N, defunciones = sum(defuncion),
                      mortalidad_pct = round(100 * mean(defuncion), 2),
                      raros_pct = round(100 * mean(potencialmente_raro), 1)),
                  by = .(letra = substr(cat3, 1, 1))][order(letra)]
print(por_capitulo)

# Mortalidad cruda por decil (para la Figura 3)
mort_por_decil <- e[potencialmente_raro == FALSE,
                    .(n_egresos = .N, defunciones = sum(defuncion),
                      mortalidad_pct = round(100 * mean(defuncion), 3)),
                    by = decil_original][order(decil_original)]

# -----------------------------------------------------------------------------
# 5. Guardar
# -----------------------------------------------------------------------------
write.xlsx(list(
  resumen_escenarios        = resumen_escenarios,
  mortalidad_cruda          = mortalidad_cruda,
  mortalidad_por_decil      = mort_por_decil,
  composicion_deciles_9_10  = comp_9_10,
  decil2_por_defunciones    = dec2_por_defunciones,
  decil2_por_letalidad      = dec2_por_letalidad,
  categorias_solo_defuncion = solo_def,
  por_capitulo              = por_capitulo
), file.path(dir_salida, "escenarios_referencia_deciles 23Oct2026.xlsx"))
cat("\nListo: ", file.path(dir_salida, "escenarios_referencia_deciles 23Oct2026.xlsx"), "\n")
