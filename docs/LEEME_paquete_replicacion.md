# Paquete de replicación — «Situación de las enfermedades raras en México»

Versión 26-oct-2026 (actualiza la versión del 19-oct, que respondía al comentario menor 30 de Andrés; agrega la corrección de edad, los escenarios y la cobertura). Reemplaza las secciones 0 y 5 de `METODOLOGIA_prevalencia_y_egresos_ER_Mexico.md` (versión 25-sep, desactualizada).

## 1. Antes de correr

1. Copiar todo el paquete a una carpeta y abrir R en ella: `setwd("<carpeta del paquete>")`.
2. Editar **solo** `config_rutas.R` (variable `datos_path`; también sirve la variable de entorno `ER_DATOS`). Ningún otro script lleva la ruta fija ni hay ruta por omisión: si no encuentran `config_rutas.R` y no está definida `ER_DATOS`, se detienen con un mensaje.
3. Correr `00_prerrequisitos.R`: lista los paquetes de R que faltan (con el `install.packages(...)` listo para pegar) y los archivos de `datos_path` que faltan. No instala nada por su cuenta.
4. Internet: **solo** la primera corrida de B1 lo necesita, para bajar los límites de entidades (GADM 4.1) a `datos_path/gadm41_MEX_1.json`. Después se lee del disco. Sin internet, copiar ese archivo a mano. Ya no se usa el API del Banco Mundial (se eliminó el 3-oct: todas las tasas usan CONAPO).

## 2. Orden de corrida y qué produce cada script

| # | Script | Reproduce | Depende de |
|---|---|---|---|
| A | `prevalencia_acumulada_mexico_2026.R` | Tabla 1 (prevalencia) | independiente |
| B1 | `enfermedades_raras_principal_unificado.R` | tasas nacionales y por cuadrante; mapas; tablas; guarda `analisis/sesion_base_raras.RData` y `sesion_ampliado.RData` | bases crudas (lento: 11 años de defunciones y egresos) |
| B2 | `analisis geografico adicional.R` | Moran global y por cuadrante; LISA; guarda `sesion_geografico_extendido.RData` | sesiones de B1 (las carga solo si faltan) |
| B3 | `analisis_geografico_extendido_parte2.R` | LISA cuadrante 3; Moran/LISA de egresos | sesiones de B1 y B2 (las carga solo) |
| B4 | `analisis_geografico_ssa_residencia.R` | sub-análisis SSA/IMSS-Bienestar por residencia | sesiones de B1 y B2 + `ssa_egresos_AAAA.zip` |
| C | `analisis_potencialmente_raros_centrado.R` | Tablas 2 y 3, modelos de mortalidad y estancia, Figuras 2a-2b; **con la corrección de edad** (horas/días/meses = 0; 999 = 0 si Z38 o P00–P96, si no NA; > 120 = NA); checkpoints en `analisis/checkpoints_edadcorr`, salidas en `analisis/edadcorr` | independiente (muy pesado: ~60 millones de egresos; los modelos usan una muestra de tamaño y semilla fijos) |
| — | `escenarios_referencia_deciles.R` | escenarios E1 (referencia restringida + deciles 9–10) a E6b; xlsx de escenarios | checkpoints de C (`datos_clasificados`); ~1 a 1.5 h |
| — | `tabla2_referencia_AN_PQ.R` | columna de referencia A–N+P+Q de la Tabla 2 | checkpoint de C |
| — | `tasas por entidad 2014 y 2024 16Oct2026.R` | tasas por entidad (texto de Figuras 1–2) | sesión de B1 |
| — | `figuras 1 y 2 mapas con claves de entidad 18Oct2026.R` | Figuras 1 y 2 | sesión de B1 |
| — | `figuras_3_y_4_forest_plot_escenarios.R` | Figuras 3 y 4 (razón de momios y de estancia por escenario) | xlsx de escenarios |
| — | `cobertura_SSA_IMB_sobre_Sectorial.R` | cobertura de la base SSA/IMSS-Bienestar sobre la base Sectorial, por año, total y enfermedades raras (44.0%–53.1% total; 37.7%–46.9% raras, 2014-2023) | independiente (bases crudas de egresos) |

Orden: A, C y la cobertura son independientes; la línea geográfica es **B1 → B2 → B3 → B4**; los scripts «—» van después de B1 o C según la columna «Depende de» (los escenarios van después de C, y las figuras 3 y 4 después de los escenarios).

## 3. Estado de verificación (qué se ha corrido con datos)

- B1, B2, B3, B4: corridos completos con datos el 14–15-oct-2026 (B1 ya incluye la sección de mapas 2014 vs. 2024; la observación de que no se había corrido tras agregarla quedó superada). Los cambios de este paquete del 19-oct a B1–B4 son de entorno (rutas, carga de sesiones, GADM en caché) y la corrección de E848 abajo; **conviene una corrida de B1 con la versión nueva para confirmar** que el ampliado ya no duplica (el script lo comprueba con `stopifnot`).
- C, tabla 2 de referencia, escenarios, figuras 3–4 y cobertura: corridos con datos por JP el 23–24-oct-2026 con la corrección de edad (edad con 0.13% de valores faltantes). Figuras 1–2 (19-oct): generadas con datos. Los scripts de tasas por entidad y figuras 1–2 no se han vuelto a correr con las modificaciones de entorno; verificados solo con `parse()`.

## 4. Correcciones de este paquete (19-oct-2026)

- **Doble conteo de E84.8 (ejercicio «ampliado», sección 3bis de B1).** La matriz de cuadrantes ya traía `E848` como otra condición (síndrome fibrosis quística–gastritis–anemia megaloblástica, cuadrante 6, confianza baja), y el ejercicio lo volvía a agregar como fibrosis quística (cuadrante 2) sin quitar la fila previa; cada egreso o defunción con E848 se contaba dos veces en el merge. Ahora los subcódigos nuevos reemplazan cualquier fila previa con el mismo código (E848 queda solo como fibrosis quística en el ampliado), se imprime cuáles se reemplazaron y el script se detiene si queda un código repetido. El análisis principal (`codigos_raras_cuadrantes`) no cambia, y el ejercicio ampliado no se cita en el manuscrito.
- **`spdep`:** B2 ya no lo instala solo; avisa y detiene la corrida si falta.
- **`datos_path` en las sesiones guardadas:** B1 ya no guarda `datos_path` ni `egresos_path` en el `.RData`; B2–B4 los toman de `config_rutas.R` (antes heredaban la ruta `E:/jp/...` de quien corrió B1).
- **Prerrequisitos citados:** B2–B4 y B1 apuntan ahora a los nombres vigentes de los scripts (antes citaban `analisis_enfermedades_raras.R` y `analisis_geografico_extendido.R`, que ya no se usan).
- **Mapa base:** se usa GADM 4.1 (descarga única a `datos_path`); la mención de `mexicoHigh.json` en la guía anterior era incorrecta.

## 5. Pendiente

- Archivo `ORPHA_ICD10_mapping*.xml` (de `Orphanet_Nomenclature_Pack_ES`): el script C lo busca dentro de `datos_path` (recursivo); `00_prerrequisitos.R` avisa si no está.
- Quien replique en otra máquina debe correr primero `00_prerrequisitos.R`; si algo falla, enviar el mensaje de error y la salida de ese script.

## 6. Corrección de edad (23-oct-2026) y cobertura (24-oct-2026)

- **Edad.** El script C corrige la edad de los egresos antes de clasificar: unidades de horas, días o meses se tratan como 0 años; el código 999 se trata como 0 si el diagnóstico es Z38 o P00–P96 y como faltante en los demás casos; edades > 120 son faltantes (0.13% en total). Los resultados de modelos y escenarios del manuscrito corresponden a esta versión.
- **Escenarios.** `escenarios_referencia_deciles.R` calcula los escenarios E1 a E6b (E1: referencia restringida a enfermedades raras más deciles 9–10); la razón de momios principal es 1.10 (IC95% 1.07–1.14) y el rango sin los deciles 9–10 es 0.99–1.28.
- **Cobertura.** `cobertura_SSA_IMB_sobre_Sectorial.R` reemplaza la cifra «42% a 52%» escrita a mano: la base SSA/IMSS-Bienestar de egresos vivos equivale al 44.0%–53.1% de la base Sectorial (37.7%–46.9% en enfermedades raras).
- **Memoria.** El ajuste de los modelos sobre el universo completo (≈60 millones de filas) saturó 448 GB de RAM; por eso se usa una muestra aleatoria de tamaño y semilla fijos.
