# =============================================================================
# FIGURAS 3 Y 4 DEL MANUSCRITO: FOREST PLOT DE ESCENARIOS (16-oct-2026)
# Figura 3: OR de mortalidad hospitalaria de "Potencialmente raros" segun el
#           grupo de referencia (E1-E6b).
# Figura 4: razon de estancia (exp(beta)) por escenario, sin ceros y con ceros
#           (log(dias + 1)). E1 no tiene estimacion de estancia en el xlsx.
# Lee SOLO el xlsx de escenarios (segundos, sin datos crudos, sin modelos).
# Escribe dos .jpeg (fondo blanco, sin titulo; el pie va en el documento) y un
# .xlsx con la tabla de cada figura.
# Correr en el equipo Windows, sesion nueva de R:
#   source("E:/jp/enfermedades raras/Datos/analisis/figuras 3 y 4 forest plot escenarios 16Oct2026.R",
#          encoding = "UTF-8", echo = TRUE)
# NO HE PODIDO EJECUTARLO (sin R disponible al escribirlo): si falla, pegar el error.
# =============================================================================
library(openxlsx)
library(ggplot2)

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
dir_salida <- file.path(datos_path, "analisis")
archivo    <- file.path(dir_salida, "escenarios_referencia_deciles 13Oct2026.xlsx")
stopifnot(file.exists(archivo))

res <- read.xlsx(archivo, sheet = "resumen_escenarios")
stopifnot(all(c("escenario", "OR_mortalidad", "OR_lo", "OR_hi",
                "expbeta_estancia_sin_ceros", "est1_lo", "est1_hi",
                "expbeta_estancia_con_ceros_log_mas1", "est0_lo", "est0_hi") %in% names(res)))

etiquetas <- c(
  E1  = "E1  Deciles 9\u201310 del resto",
  E2  = "E2  Deciles 1\u20138 del resto",
  E3  = "E3  Todo el resto",
  E4  = "E4  Sin cap\u00edtulo XV",
  E5  = "E5  Sin cap\u00edtulo XV, edad con spline",
  E6  = "E6  Cap\u00edtulos A\u2013N",
  E6b = "E6b  Cap\u00edtulos A\u2013N, P y Q (principal)"
)
res <- res[res$escenario %in% names(etiquetas), ]
res$etiqueta  <- factor(etiquetas[res$escenario], levels = rev(etiquetas))   # E1 arriba
res$principal <- res$escenario == "E6b"

azul  <- "#003D79"   # azul CIPPS-CISIDAT (principal)
gris  <- "#6B7280"   # escenarios de sensibilidad
fmt   <- function(x) formatC(x, format = "f", digits = 2)
txt   <- function(est, lo, hi) paste0(fmt(est), " (", fmt(lo), "\u2013", fmt(hi), ")")

tema <- theme_minimal(base_size = 11, base_family = "sans") +
  theme(panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#E5E7EB", linewidth = 0.3),
        axis.title.y       = element_blank(),
        axis.text.y        = element_text(color = "#111827"),
        axis.text.x        = element_text(color = "#374151"),
        axis.title.x       = element_text(color = "#374151", margin = margin(t = 8)),
        legend.position    = "bottom",
        plot.background    = element_rect(fill = "white", color = NA),
        panel.background   = element_rect(fill = "white", color = NA),
        plot.margin        = margin(10, 20, 10, 10))

# ---- Figura 3: mortalidad ---------------------------------------------------
f3 <- res
f3$texto <- txt(f3$OR_mortalidad, f3$OR_lo, f3$OR_hi)
g3 <- ggplot(f3, aes(x = OR_mortalidad, y = etiqueta, color = principal)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#9CA3AF", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = OR_lo, xmax = OR_hi), height = 0.18, linewidth = 0.6) +
  geom_point(aes(size = principal), shape = 15) +
  geom_text(aes(x = 12, label = texto), hjust = 0, size = 3.3, color = "#111827") +
  scale_color_manual(values = c(`FALSE` = gris, `TRUE` = azul), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 2.6, `TRUE` = 3.6), guide = "none") +
  scale_x_log10(breaks = c(0.8, 1, 1.25, 2, 4, 8), limits = c(0.7, 30),
                labels = c("0.8", "1", "1.25", "2", "4", "8")) +
  labs(x = "Raz\u00f3n de momios de mortalidad hospitalaria (escala logar\u00edtmica; IC 95%)") +
  tema
ggsave(file.path(dir_salida, "figura3_forest_mortalidad_escenarios.jpeg"),
       g3, width = 9, height = 4.2, dpi = 300, bg = "white")

# ---- Figura 4: estancia -----------------------------------------------------
f4 <- rbind(
  data.frame(escenario = res$escenario, etiqueta = res$etiqueta, principal = res$principal,
             medida = "Sin ceros (principal)",
             est = res$expbeta_estancia_sin_ceros, lo = res$est1_lo, hi = res$est1_hi),
  data.frame(escenario = res$escenario, etiqueta = res$etiqueta, principal = res$principal,
             medida = "Con ceros, log(d\u00edas + 1)",
             est = res$expbeta_estancia_con_ceros_log_mas1, lo = res$est0_lo, hi = res$est0_hi))
f4 <- f4[!is.na(f4$est), ]
f4$medida <- factor(f4$medida, levels = c("Sin ceros (principal)", "Con ceros, log(d\u00edas + 1)"))
f4$texto  <- ifelse(f4$medida == "Sin ceros (principal)", txt(f4$est, f4$lo, f4$hi), "")
dodge <- position_dodge(width = 0.55)
g4 <- ggplot(f4, aes(x = est, y = etiqueta, shape = medida, fill = medida)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#9CA3AF", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.15, linewidth = 0.5,
                 color = azul, position = dodge) +
  geom_point(size = 2.8, color = azul, position = dodge) +
  geom_text(data = f4[f4$medida == "Sin ceros (principal)", ],
            mapping = aes(x = 1.62, y = etiqueta, label = texto),
            hjust = 0, size = 3.3, color = "#111827",
            inherit.aes = FALSE, show.legend = FALSE) +
  scale_shape_manual(values = c(21, 24), name = NULL) +
  scale_fill_manual(values = c(azul, "white"), name = NULL) +
  scale_x_continuous(breaks = c(1, 1.1, 1.2, 1.3, 1.4, 1.5), limits = c(1, 1.95)) +
  labs(x = "Raz\u00f3n de estancia hospitalaria (exp(\u03b2); IC 95%)") +
  tema
ggsave(file.path(dir_salida, "figura4_forest_estancia_escenarios.jpeg"),
       g4, width = 9, height = 4.6, dpi = 300, bg = "white")

# ---- Tabla de cada figura ---------------------------------------------------
wb <- createWorkbook()
addWorksheet(wb, "figura3_mortalidad")
writeData(wb, "figura3_mortalidad",
          f3[, c("escenario", "etiqueta", "OR_mortalidad", "OR_lo", "OR_hi")])
addWorksheet(wb, "figura4_estancia")
writeData(wb, "figura4_estancia", f4[, c("escenario", "etiqueta", "medida", "est", "lo", "hi")])
saveWorkbook(wb, file.path(dir_salida, "figuras_3_4_escenarios 16Oct2026.xlsx"), overwrite = TRUE)

cat("\nFiguras guardadas en", dir_salida, "\n")
