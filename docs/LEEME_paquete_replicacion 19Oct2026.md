# Paquete de replicación — «Situación de las enfermedades raras en México»

Versión 19-oct-2026 (responde al comentario menor 30 de Andrés). Reemplaza las secciones 0 y 5 de `METODOLOGIA_prevalencia_y_egresos_ER_Mexico.md` (versión 25-sep, desactualizada).

## 1. Antes de correr

1. Copiar todo el paquete a una carpeta y abrir R en ella: `setwd("<carpeta del paquete>")`.
2. Editar **solo** `config_rutas.R` (variable `datos_path`; también sirve la variable de entorno `ER_DATOS`). Ningún otro script lleva la ruta fija: si no encuentran `config_rutas.R`, usan la ruta del equipo de corrida y lo avisan.
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
| C | `analisis_potencialmente_raros_centrado-1.R` | Tabla 2/3, Figuras 3–4 (modelos); checkpoints en `analisis/checkpoints_potencialmente_raros` | independiente |
| — | `tabla 2 referencia AN PQ 17Oct2026.R` | columna de referencia A–N+P+Q de la Tabla 2 | checkpoint de C |
| — | `tasas por entidad 2014 y 2024 16Oct2026.R` | tasas por entidad (texto de Figuras 1–2) | sesión de B1 |
| — | `figuras 1 y 2 mapas con claves de entidad 18Oct2026.R` | Figuras 1 y 2 | sesión de B1 |
| — | `figuras 3 y 4 forest plot escenarios 16Oct2026.R` | Figuras 3 y 4 | xlsx de escenarios de C |

Orden: A y C son independientes; la línea geográfica es **B1 → B2 → B3 → B4**; los scripts «—» van después de B1 o C según la columna «Depende de».

## 3. Estado de verificación (qué se ha corrido con datos)

- B1, B2, B3, B4: corridos completos con datos el 14–15-oct-2026 (B1 ya incluye la sección de mapas 2014 vs. 2024; la observación de que no se había corrido tras agregarla quedó superada). Los cambios de este paquete del 19-oct a B1–B4 son de entorno (rutas, carga de sesiones, GADM en caché) y la corrección de E848 abajo; **conviene una corrida de B1 con la versión nueva para confirmar** que el ampliado ya no duplica (el script lo comprueba con `stopifnot`).
- C: corrido con datos por JP el 18–19-oct-2026 (con claves de entidad de 32 categorías). Figuras 1–2 (19-oct) y 3–4 (16-oct): generadas con datos. Los demás scripts «—» no se han vuelto a correr con estas modificaciones de entorno; verificados solo con `parse()`.

## 4. Correcciones de este paquete (19-oct-2026)

- **Doble conteo de E84.8 (ejercicio «ampliado», sección 3bis de B1).** La matriz de cuadrantes ya traía `E848` como otra condición (síndrome fibrosis quística–gastritis–anemia megaloblástica, cuadrante 6, confianza baja), y el ejercicio lo volvía a agregar como fibrosis quística (cuadrante 2) sin quitar la fila previa; cada egreso o defunción con E848 se contaba dos veces en el merge. Ahora los subcódigos nuevos reemplazan cualquier fila previa con el mismo código (E848 queda solo como fibrosis quística en el ampliado), se imprime cuáles se reemplazaron y el script se detiene si queda un código repetido. El análisis principal (`codigos_raras_cuadrantes`) no cambia, y el ejercicio ampliado no se cita en el manuscrito.
- **`spdep`:** B2 ya no lo instala solo; avisa y detiene la corrida si falta.
- **`datos_path` en las sesiones guardadas:** B1 ya no guarda `datos_path` ni `egresos_path` en el `.RData`; B2–B4 los toman de `config_rutas.R` (antes heredaban la ruta `E:/jp/...` de quien corrió B1).
- **Prerrequisitos citados:** B2–B4 y B1 apuntan ahora a los nombres vigentes de los scripts (antes citaban `analisis_enfermedades_raras.R` y `analisis_geografico_extendido.R`, que ya no se usan).
- **Mapa base:** se usa GADM 4.1 (descarga única a `datos_path`); la mención de `mexicoHigh.json` en la guía anterior era incorrecta.

## 5. Pendiente

- Archivo `ORPHA_ICD10_mapping*.xml` (de `Orphanet_Nomenclature_Pack_ES`): el script C lo busca dentro de `datos_path` (recursivo); `00_prerrequisitos.R` avisa si no está.
- Quien replique en otra máquina debe correr primero `00_prerrequisitos.R`; si algo falla, enviar el mensaje de error y la salida de ese script.
