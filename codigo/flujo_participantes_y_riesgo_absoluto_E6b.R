# =============================================================================
# flujo_participantes_y_riesgo_absoluto_E6b.R
# (1) Flujo de egresos (RECORD 13 / 13.1)  (2) Riesgo absoluto, diferencia de riesgos
#     y razon de riesgos de mortalidad intrahospitalaria, escenario principal E6b
#     (STROBE 16c), por estandarizacion marginal (g-computation) del modelo E6b.
# Requiere el checkpoint datos_clasificados (script C corregido). Corre en Windows.
# Tiempo estimado: 5-15 min (un ajuste brglm2 sobre ~2.9 M de egresos).
# Salida: flujo_participantes_riesgo_E6b <fecha>.xlsx en datos_path/analisis/edadcorr
# =============================================================================
library(data.table); library(brglm2); library(splines); library(openxlsx); library(MASS)
# Ruta de datos: se define UNA sola vez en config_rutas.R (haz setwd() a la carpeta codigo/)
# o con la variable de entorno ER_DATOS. No hay ruta por omision.
if (!exists("datos_path", inherits = TRUE)) {
  if (file.exists("config_rutas.R")) source("config_rutas.R", encoding = "UTF-8") else {
    datos_path <- Sys.getenv("ER_DATOS", unset = "")
    if (!nzchar(datos_path)) stop("No encontre config_rutas.R en ", getwd(), " y ER_DATOS no esta definida.")
  }
}
setwd(file.path(datos_path, "analisis"))
dir_chk <- file.path(datos_path, "analisis", "checkpoints_edadcorr")
d <- readRDS(file.path(dir_chk, "datos_clasificados.rds"))$egresos_completos
setDT(d)

# ---------------- 1. FLUJO -------------------------------------------------
sexo_ok <- d$SEXO %in% c("1", "2"); edad_ok <- !is.na(d$EDAD_num)
fl <- list()
add <- function(etapa, n, nr = NA) fl[[length(fl) + 1]] <<- data.table(etapa = etapa, n = n, potencialmente_raros = nr)
add("Egresos Sectorial 2014-2024 (incl. defunciones)", nrow(d), d[, sum(potencialmente_raro)])
add("  Excluidos: sexo no 1/2", sum(!sexo_ok), d[!sexo_ok, sum(potencialmente_raro)])
add("  Excluidos: edad faltante tras correccion (de los de sexo valido)", sum(sexo_ok & !edad_ok),
    d[sexo_ok & !edad_ok, sum(potencialmente_raro)])
elig <- sexo_ok & edad_ok
add("Egresos con edad y sexo validos (universo de modelos)", sum(elig), d[elig, sum(potencialmente_raro)])
set.seed(20250918)
idx <- sample(which(elig), min(3e6, sum(elig)))
m <- d[idx, .(cat3, SEXO, ANIO, EDAD_num, DIAS_ESTA_num, defuncion, potencialmente_raro, decil_original)]
add("Muestra aleatoria para modelos (seed 20250918)", nrow(m), m[, sum(potencialmente_raro)])
m[, letra := substr(cat3, 1, 1)]
caps <- c(LETTERS[1:14], "P", "Q")                    # A-N + P + Q
add("  No incluidos en E6b (capitulos O, R-Z fuera de la comparacion)", m[!letra %in% caps, .N],
    m[!letra %in% caps, sum(potencialmente_raro)])
e <- m[letra %in% caps]
add("Muestra del escenario principal E6b", nrow(e), e[, sum(potencialmente_raro)])
add("  Grupo de referencia (A-N, P, Q no raros)", e[potencialmente_raro == FALSE, .N])
add("  Defunciones intrahospitalarias en E6b", e[, sum(defuncion)], e[potencialmente_raro == TRUE, sum(defuncion)])
flujo <- rbindlist(fl)
flujo_anio <- d[, .(egresos = .N, potencialmente_raros = sum(potencialmente_raro),
                    sin_edad_o_sexo = sum(!(SEXO %in% c("1","2") & !is.na(EDAD_num)))), by = ANIO][order(ANIO)]
print(flujo); print(flujo_anio)
rm(d); gc()

# ---------------- 2. RIESGO ABSOLUTO E6b -----------------------------------
e[, grupo := factor(potencialmente_raro, c(FALSE, TRUE), c("Referencia", "Potencialmente raros"))]
e[, SEXO_f := factor(SEXO, c("2", "1"), c("Femenino", "Masculino"))]
mod <- glm(defuncion ~ grupo + ns(EDAD_num, df = 4) + SEXO_f + factor(ANIO), data = e,
           family = binomial(), method = "brglmFit", type = "AS_mean")
# Patrones de covariables (edad x sexo x anio) con su frecuencia
pat <- e[, .(n = .N, n_raros = sum(potencialmente_raro)), by = .(EDAD_num, SEXO_f, ANIO)]
mm <- function(g) {
  nd <- copy(pat); nd[, grupo := factor(g, levels = levels(e$grupo))]
  model.matrix(delete.response(terms(mod)), model.frame(delete.response(terms(mod)), nd, xlev = mod$xlevels))
}
X1 <- mm("Potencialmente raros"); X0 <- mm("Referencia")
est <- function(b, w) {
  p1 <- plogis(X1 %*% b); p0 <- plogis(X0 %*% b)
  r1 <- sum(w * p1) / sum(w); r0 <- sum(w * p0) / sum(w)
  c(r1 = r1, r0 = r0, dif_pp = 100 * (r1 - r0), RR = r1 / r0)
}
b <- coef(mod); V <- vcov(mod)
set.seed(1)
draws <- mvrnorm(2000, b, V)
res <- list()
for (nm in c("Poblacion estandar: todo E6b", "Poblacion estandar: potencialmente raros")) {
  w <- if (nm == "Poblacion estandar: todo E6b") pat$n else pat$n_raros
  pt <- est(b, w); dr <- t(apply(draws, 1, est, w = w))
  ci <- apply(dr, 2, quantile, c(.025, .975))
  res[[nm]] <- data.table(estandarizacion = nm,
    riesgo_raros_pct = 100 * pt["r1"], lo1 = 100 * ci[1, "r1"], hi1 = 100 * ci[2, "r1"],
    riesgo_ref_pct = 100 * pt["r0"], lo0 = 100 * ci[1, "r0"], hi0 = 100 * ci[2, "r0"],
    dif_riesgo_pp = pt["dif_pp"], dif_lo = ci[1, "dif_pp"], dif_hi = ci[2, "dif_pp"],
    RR = pt["RR"], RR_lo = ci[1, "RR"], RR_hi = ci[2, "RR"])
}
riesgo <- rbindlist(res)
crudo <- e[, .(n = .N, defunciones = sum(defuncion), mortalidad_cruda_pct = 100 * mean(defuncion)), by = grupo]
cat("\nOR del modelo:", exp(coef(mod)["grupoPotencialmente raros"]), "\n"); print(crudo); print(riesgo)

out <- file.path("edadcorr", paste0("flujo_participantes_riesgo_E6b ", format(Sys.Date(), "%d%b%Y"), ".xlsx"))
write.xlsx(list(flujo = flujo, flujo_por_anio = flujo_anio, mortalidad_cruda = crudo,
                riesgo_E6b = riesgo), out)
cat("Guardado:", out, "\n")
