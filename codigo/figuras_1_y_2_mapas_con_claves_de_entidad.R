# =============================================================================
# FIGURAS 1 Y 2 CON CLAVES DE ENTIDAD (18-oct-2026; opcion de idioma 31-oct-2026)
# Comentario 16 de Andres: las figuras no identifican las entidades.
# Regenera las dos figuras (egresos y defunciones, 2014 | 2024, misma escala de
# color) con la abreviatura de cada entidad sobre el mapa y, al pie, la clave
# INEGI (01-32) con el nombre completo.
#  - Egresos: egresos_raras (catalogo general, entidad de la unidad medica).
#  - Defunciones: usa `defunciones_raras` (catalogo general, residencia habitual)
#    si existe en la sesion; si no, usa defunciones_raras_cuadrantes (la sesion
#    guardada no incluye la primera; ~0.6% menos defunciones: 835,777 vs 841,177)
#    y lo avisa en consola.
# Lee la sesion guardada por B1 y el archivo de poblacion de CONAPO (poblacion
# por entidad y anio, igual que la seccion 15ter de B1).
# Correr en una sesion nueva de R (Windows):
#   source("figuras_1_y_2_mapas_con_claves_de_entidad.R",
#          encoding = "UTF-8", echo = TRUE)
# NO PROBADO (sin acceso a los datos al escribirlo): si falla, pegar el error.
# =============================================================================
library(data.table); library(openxlsx); library(sf); library(ggplot2)
idioma <- if (exists("idioma", inherits = TRUE)) idioma else Sys.getenv("ER_IDIOMA", unset = "es")
stopifnot(idioma %in% c("es", "en"))
sufijo <- if (idioma == "en") "_en" else ""
# IDIOMA (31-oct-2026): por omision espanol; para ingles definir antes de correr
#   idioma <- "en"   (o la variable de entorno ER_IDIOMA=en). Los archivos llevan sufijo _en.
#   Ejemplo (Windows): datos_path <- "<ruta de los datos>"; idioma <- "en"
#   source("figuras_1_y_2_mapas_con_claves_de_entidad.R", encoding = "UTF-8", echo = TRUE)
# Recomendado: install.packages("ggrepel") para que las etiquetas no se encimen.
# Ruta de datos (menor 30, 19-oct-2026): se define UNA sola vez en config_rutas.R, en la
# carpeta del paquete. Si datos_path ya existe en la sesion se respeta; si no, se lee del
# archivo de configuracion (hay que haber hecho setwd() a la carpeta del paquete).
if (!exists("datos_path", inherits = TRUE)) {
  if (file.exists("config_rutas.R")) {
    source("config_rutas.R", encoding = "UTF-8")
  } else {
    # Sin setwd() al paquete: usar la variable de entorno ER_DATOS. Para otra
    # maquina, ajustar config_rutas.R (y hacer setwd() a la carpeta del paquete).
    datos_path <- Sys.getenv("ER_DATOS", unset = "")
    if (!nzchar(datos_path)) stop("No encontre config_rutas.R en ", getwd(), " y ER_DATOS no esta definida: haz setwd() a la carpeta codigo/ o define ER_DATOS.")
  }
}
load(file.path(datos_path, "analisis", "sesion_base_raras.RData"))

nombres <- c("Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila","Colima",
  "Chiapas","Chihuahua","Ciudad de M\u00e9xico","Durango","Guanajuato","Guerrero","Hidalgo","Jalisco",
  "Estado de M\u00e9xico","Michoac\u00e1n","Morelos","Nayarit","Nuevo Le\u00f3n","Oaxaca","Puebla",
  "Quer\u00e9taro","Quintana Roo","San Luis Potos\u00ed","Sinaloa","Sonora","Tabasco","Tamaulipas",
  "Tlaxcala","Veracruz","Yucat\u00e1n","Zacatecas")
# Abreviaturas (orden de la clave INEGI 01-32)
abrev <- c("Ags.","BC","BCS","Camp.","Coah.","Col.","Chis.","Chih.","CDMX","Dgo.","Gto.","Gro.",
           "Hgo.","Jal.","Mex.","Mich.","Mor.","Nay.","NL","Oax.","Pue.","Qro.","Q. Roo","SLP",
           "Sin.","Son.","Tab.","Tamps.","Tlax.","Ver.","Yuc.","Zac.")
if (idioma == "en") {   # nombres en ingles solo donde difieren; el resto son nombres oficiales
  nombres[nombres == "Ciudad de M\u00e9xico"] <- "Mexico City"
  nombres[nombres == "Estado de M\u00e9xico"] <- "State of Mexico"
}
cat_ent <- data.table(ENT = sprintf("%02d", 1:32), entidad = nombres, abrev = abrev)

# Poblacion CONAPO por entidad y anio
pob <- openxlsx::read.xlsx(file.path(datos_path, "0_Pob_Mitad_1950_2070.xlsx")); setDT(pob)
setnames(pob, names(pob), c("RENGLON","ANIO_POB","ENTIDAD_NOMBRE","CVE_GEO","EDAD","SEXO","POBLACION"))
pob <- pob[CVE_GEO != 0 & ANIO_POB %in% c(2014, 2024),
           .(poblacion = sum(POBLACION, na.rm = TRUE)), by = .(anio = ANIO_POB, ENT = sprintf("%02d", CVE_GEO))]
stopifnot(abs(pob[anio == 2014, sum(poblacion)] - 121048604) < 1000,
          abs(pob[anio == 2024, sum(poblacion)] - 132274416) < 1000)

tasas <- function(dt, col_ent, col_anio) {
  x <- dt[get(col_anio) %in% c(2014, 2024) & get(col_ent) %in% estados_validos,
          .(casos = .N), by = .(anio = get(col_anio), ENT = get(col_ent))]
  x <- merge(x, pob, by = c("anio", "ENT"))
  x[, tasa := casos / poblacion * 1e5]
  x
}
if (exists("defunciones_raras")) {
  def_base <- defunciones_raras
} else {
  def_base <- defunciones_raras_cuadrantes
  message("AVISO: defunciones_raras no esta en la sesion; se usa defunciones_raras_cuadrantes (~0.6% menos defunciones).")
}
t_egr <- tasas(egresos_raras, "ENTIDAD", "ANIO")
t_def <- tasas(def_base, "ENT_RESID", "ANIO_FUENTE")
cat("Totales de control (casos):\n"); print(t_egr[, .(egresos = sum(casos)), by = anio]); print(t_def[, .(defunciones = sum(casos)), by = anio])

# Geometria: estados GADM + puente a clave INEGI; punto de etiqueta dentro del poligono
mapa_base <- merge(mx_states, as.data.frame(gadm_bridge), by = "NAME_1")
mapa_base$ENT <- mapa_base$ENT_RESID
stopifnot(nrow(mapa_base) == 32)
pts <- suppressWarnings(st_point_on_surface(mapa_base))
xy <- st_coordinates(pts)
lab_base <- data.table(ENT = mapa_base$ENT, X = xy[, 1], Y = xy[, 2])
lab_base <- merge(lab_base, cat_ent, by = "ENT")

# Clave al pie: "01 Aguascalientes (Ags.)", repartida en 4 columnas
clave <- paste0(cat_ent$ENT, " ", cat_ent$entidad, " (", cat_ent$abrev, ")")
clave_txt <- paste(vapply(split(clave, rep(1:4, each = 8)), paste, character(1), collapse = "\n"), collapse = "\n\n")
clave_cols <- vapply(split(clave, rep(1:4, each = 8)), paste, character(1), collapse = "\n")

hacer_figura <- function(t, titulo_leyenda) {
  lims <- range(t$tasa)
  d <- merge(as.data.frame(mapa_base)[, c("ENT", "geometry")], as.data.frame(t), by = "ENT") |> st_as_sf()
  d$anio_f <- factor(d$anio, levels = c(2014, 2024))
  lab <- merge(lab_base, t[, .(ENT, anio, tasa)], by = "ENT", allow.cartesian = TRUE)
  lab[, anio_f := factor(anio, levels = c(2014, 2024))]
  usa_repel <- requireNamespace("ggrepel", quietly = TRUE)
  capa_lab <- if (usa_repel) {
    ggrepel::geom_text_repel(data = lab, aes(X, Y, label = abrev), size = 2.4, fontface = "bold",
                             colour = "black", bg.color = "white", bg.r = 0.12,
                             segment.size = 0.2, segment.color = "grey35", min.segment.length = 0.25,
                             box.padding = 0.12, point.padding = 0, force = 1.2, max.overlaps = Inf, seed = 1)
  } else {
    message("AVISO: sin ggrepel las etiquetas pueden encimarse (install.packages('ggrepel')).")
    geom_text(data = lab, aes(X, Y, label = abrev), size = 2.3, fontface = "bold")
  }
  ggplot(d) +
    geom_sf(aes(fill = tasa), color = "white", linewidth = 0.2) +
    capa_lab +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, limits = lims, name = titulo_leyenda) +
    facet_wrap(~anio_f) +
        theme_void(base_size = 12) +
    theme(strip.text = element_text(face = "bold", size = 12),
          plot.margin = margin(5, 5, 5, 5))
}
guardar_figura <- function(p, archivo) {
  # Mapa arriba y clave de entidades en 4 columnas abajo (grid: viene con R)
  gg_clave <- ggplot() + xlim(0, 4) + ylim(0, 1) + theme_void() +
    lapply(1:4, function(i) annotate("text", x = i - 0.97, y = 0.97, label = clave_cols[i],
                                     hjust = 0, vjust = 1, size = 3, lineheight = 1.05))
  dir.create(dirname(archivo), recursive = TRUE, showWarnings = FALSE)
  jpeg(archivo, width = 11, height = 6.4, units = "in", res = 300, quality = 95, bg = "white")
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 1, heights = grid::unit(c(3.1, 1), "null"))))
  print(p, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(gg_clave, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
  invisible(dev.off())
}

tit_ley <- if (idioma == "en") "Rate per\n100,000 inhabitants" else "Tasa por\n100,000 hab."
f1 <- hacer_figura(t_egr, tit_ley)
f2 <- hacer_figura(t_def, tit_ley)
dir_sal <- file.path(datos_path, "analisis")
guardar_figura(f1, file.path(dir_sal, paste0("mapa_egresos_2014_2024_comparacion", sufijo, " 31Oct2026.jpeg")))
guardar_figura(f2, file.path(dir_sal, paste0("mapa_defunciones_2014_2024_comparacion", sufijo, " 31Oct2026.jpeg")))
cat("\nListo: mapas de egresos y defunciones (sufijo \"", sufijo, "\") en ", dir_sal, "\n", sep = "")
