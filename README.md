# Enfermedades raras en México: código de análisis

Código de replicación del estudio **«Magnitud y distribución de las enfermedades raras en México: prevalencia estimada y carga hospitalaria, 2014-2024»** (CIPPS–CISIDAT). Repositorio: https://github.com/jpgr70-dev/enfermedades-raras-mexico. El estudio estima la prevalencia puntual acumulada de las enfermedades raras a partir de Orphanet, describe la distribución geográfica de los egresos hospitalarios y las defunciones por condiciones potencialmente raras (2014-2024) y compara la mortalidad hospitalaria y la estancia de esos egresos con el resto.

*English summary.* R code to replicate the estimation of cumulative point prevalence of rare diseases in Mexico from Orphanet (adapted from Nguengang Wakap et al., 2020), the spatial analysis of hospital discharges and deaths with ICD-10 codes that potentially correspond to rare diseases (2014-2024), and the models of in-hospital mortality and length of stay. Raw data are public and are not redistributed here; see `datos/README.md`.

## Contenido

| Carpeta | Qué contiene |
|---|---|
| `codigo/` | Scripts en R (y un script en Python para el diagrama de flujo) y `config_rutas.R` (única ruta de datos) |
| `catalogo/` | `cie10 raras_corregido.xlsx`: catálogo de códigos CIE-10 potencialmente raros (mapeo de Orphanet y catálogo complementario de Blazsik et al. 2021; 1,732 códigos únicos, 1,729 tras excluir K74.6, C50.9 y C34.9) |
| `docs/` | Guía metodológica y LEEME del paquete (orden de corrida, qué produce cada script) |
| `datos/` | Instrucciones para obtener los datos públicos (no se incluyen) |

## Cómo correrlo

1. Obtener los datos (ver `datos/README.md`) y copiarlos a una carpeta de trabajo.
2. Abrir R en `codigo/` (`setwd("codigo")`) y editar **solo** `config_rutas.R`, o definir la variable de entorno `ER_DATOS` con la ruta de los datos.
3. Correr `00_prerrequisitos.R`: lista los paquetes de R y los archivos que faltan. No instala nada.
4. Correr los scripts en el orden de `docs/LEEME_paquete_replicacion.md`: A (prevalencia) y C (potencialmente raros, con la edad corregida) son independientes; la línea geográfica es B1 → B2 → B3 → B4; los scripts de escenarios, tablas y figuras se corren después de C o de B1; `cobertura_SSA_IMB_sobre_Sectorial.R` es independiente.

La ruta de los datos no tiene valor por omisión: hay que definirla en `config_rutas.R` o en la variable de entorno `ER_DATOS`. El script C procesa unos 60 millones de egresos y es la corrida más pesada: los modelos se ajustan sobre una muestra aleatoria de tamaño fijo y semilla fija (el ajuste sobre el universo completo saturó 448 GB de RAM sin avanzar), por lo que el resultado no depende de la memoria del equipo; los conteos y las tasas crudas usan el universo completo.

B1 es la corrida pesada (11 años de defunciones y egresos) y la única que necesita internet, solo la primera vez, para bajar los límites de entidades (GADM 4.1).

## Estado de verificación

El flujo de participantes y el riesgo absoluto del escenario principal (`flujo_participantes_y_riesgo_absoluto_E6b.R`) se corrieron el 1 de noviembre de 2026 sobre el checkpoint del script C. Los análisis completos (B1 a B4) se corrieron con datos entre el 14 y el 19 de octubre de 2026; C, la tabla 2, los escenarios, las figuras 3 y 4 y la cobertura SSA/IMSS-Bienestar se corrieron entre el 23 y el 24 de octubre de 2026 con la corrección de edad. El 19 de octubre se corrió B1 completo con la versión de este repositorio (≈9 min, sin errores): la sesión base es idéntica a la de la corrida anterior y el ejercicio ampliado difiere solo por la corrección del doble conteo de E84.8 (cuadrante 6: −637 defunciones y −232 egresos, 2014-2024). Los cambios de entorno de B2 a B4 (rutas, carga de sesiones) se verificaron únicamente con `parse()`; su corrida completa con esta versión queda pendiente.

## Cómo citar

Versión archivada v1.0.0 en Zenodo: https://doi.org/10.5281/zenodo.23983409. La referencia del artículo se agregará cuando esté aceptado. Ver también `CITATION.cff` (GitHub muestra el botón «Cite this repository» a partir de ese archivo).

## Software availability (English)

- **Project name:** enfermedades-raras-mexico
- **Home page:** https://github.com/jpgr70-dev/enfermedades-raras-mexico
- **Archived version:** Zenodo, v1.0.0, https://doi.org/10.5281/zenodo.23983409
- **Operating system:** platform independent; the analyses were run on Windows
- **Programming language:** R (packages listed by `codigo/00_prerrequisitos.R`; the flow diagram of the Supplementary figure is drawn with Python/matplotlib, `codigo/figura_S1_flujo_egresos.py`)
- **Other requirements:** the public data listed in `datos/README.md`; the script of the main model works on about 60 million discharges and needs a computer with ample memory
- **License:** MIT (code) and CC BY 4.0 (documentation in `docs/`)
- **Restrictions for non-academic use:** none from this repository; the data keep the terms of use of their sources

## Licencia

Código: MIT (`LICENSE`). Documentación (`docs/`): CC BY 4.0. Los datos de Orphanet, de Blazsik et al. (2021), de DGIS, INEGI, CONAPO y Naciones Unidas conservan sus licencias y términos de uso originales.
