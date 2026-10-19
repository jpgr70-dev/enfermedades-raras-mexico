# =============================================================================
# TASAS POR ENTIDAD 2014 Y 2024 (16-oct-2026): para actualizar el parrafo de entidades
# con tasas mas altas y bajas del marco (Figuras 1 y 2). Ligero: solo lee la sesion
# guardada por B1 (sesion_base_raras.RData) y el archivo de poblacion de CONAPO.
#  - Egresos: egresos_raras (catalogo general, lugar de la unidad medica), igual que
#    el mapa de la Figura 1.
#  - Defunciones: defunciones_raras_cuadrantes (definicion restringida por cuadrante,
#    ~0.6% menos defunciones que el catalogo general: 835,777 vs. 841,177), porque la
#    sesion guardada no incluye `defunciones_raras`. Sirve para el orden de las
#    entidades; si dos entidades quedan muy cerca, confirmar con la Figura 2.
# Correr en una sesion nueva de R (Windows):
#   source("E:/jp/enfermedades raras/Datos/analisis/tasas por entidad 2014 y 2024 16Oct2026.R",
#          encoding = "UTF-8", echo = TRUE)
# NO PROBADO (sin R disponible al escribirlo): si falla, pegar el error.
# =============================================================================
library(data.table); library(openxlsx)
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
load(file.path(datos_path, "analisis", "sesion_base_raras.RData"))

nombres <- c("Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila","Colima",
  "Chiapas","Chihuahua","Ciudad de M\u00e9xico","Durango","Guanajuato","Guerrero","Hidalgo","Jalisco",
  "Estado de M\u00e9xico","Michoac\u00e1n","Morelos","Nayarit","Nuevo Le\u00f3n","Oaxaca","Puebla",
  "Quer\u00e9taro","Quintana Roo","San Luis Potos\u00ed","Sinaloa","Sonora","Tabasco","Tamaulipas",
  "Tlaxcala","Veracruz","Yucat\u00e1n","Zacatecas")
cat_ent <- data.table(ENT = sprintf("%02d", 1:32), entidad = nombres)

# Poblacion CONAPO por entidad y anio (misma logica que la seccion 1b de B1)
pob <- openxlsx::read.xlsx(file.path(datos_path, "0_Pob_Mitad_1950_2070.xlsx")); setDT(pob)
setnames(pob, names(pob), c("RENGLON","ANIO_POB","ENTIDAD_NOMBRE","CVE_GEO","EDAD","SEXO","POBLACION"))
pob <- pob[CVE_GEO != 0 & ANIO_POB %in% c(2014, 2024),
           .(poblacion = sum(POBLACION, na.rm = TRUE)), by = .(anio = ANIO_POB, ENT = sprintf("%02d", CVE_GEO))]
stopifnot(abs(pob[anio == 2014, sum(poblacion)] - 121048604) < 1000,
          abs(pob[anio == 2024, sum(poblacion)] - 132274416) < 1000)

hacer <- function(dt, col_ent, col_anio) {
  x <- dt[get(col_anio) %in% c(2014, 2024) & get(col_ent) %in% estados_validos,
          .(casos = .N), by = .(anio = get(col_anio), ENT = get(col_ent))]
  x <- merge(x, pob, by = c("anio", "ENT")); x <- merge(x, cat_ent, by = "ENT")
  x[, tasa_100mil := round(casos / poblacion * 1e5, 1)]
  x[, rango := frank(-tasa_100mil, ties.method = "min"), by = anio]
  x[order(anio, rango), .(anio, rango, ENT, entidad, casos, poblacion, tasa_100mil)]
}
egr <- hacer(egresos_raras, "ENTIDAD", "ANIO")
def <- hacer(defunciones_raras_cuadrantes, "ENT_RESID", "ANIO_FUENTE")
cat("\nTotales de control (suma de entidades):\n"); print(egr[, .(casos = sum(casos)), by = anio]); print(def[, .(casos = sum(casos)), by = anio])
for (a in c(2014, 2024)) {
  cat("\n== EGRESOS", a, "- primeros 6 y ultimos 5\n"); print(rbind(egr[anio == a][1:6], tail(egr[anio == a], 5)))
  cat("\n== DEFUNCIONES", a, "- primeros 6 y ultimos 5\n"); print(rbind(def[anio == a][1:6], tail(def[anio == a], 5)))
}
wb <- createWorkbook()
addWorksheet(wb, "egresos_2014_2024"); writeData(wb, "egresos_2014_2024", egr)
addWorksheet(wb, "defunciones_2014_2024"); writeData(wb, "defunciones_2014_2024", def)
saveWorkbook(wb, file.path(datos_path, "analisis", "tasas_por_entidad_2014_2024 16Oct2026.xlsx"), overwrite = TRUE)
cat("\nGuardado: Datos/analisis/tasas_por_entidad_2014_2024 16Oct2026.xlsx\n")
