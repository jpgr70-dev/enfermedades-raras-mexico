# Enfermedades raras en México: código de análisis

Código de replicación del estudio **«Magnitud y distribución de las enfermedades raras en México: prevalencia estimada y carga hospitalaria, 2014-2024»** (CIPPS–CISIDAT). El estudio estima la prevalencia puntual acumulada de las enfermedades raras a partir de Orphanet, describe la distribución geográfica de los egresos hospitalarios y las defunciones por condiciones potencialmente raras (2014-2024) y compara la mortalidad hospitalaria y la estancia de esos egresos con el resto.

*English summary.* R code to replicate the estimation of cumulative point prevalence of rare diseases in Mexico from Orphanet (adapted from Nguengang Wakap et al., 2020), the spatial analysis of hospital discharges and deaths with ICD-10 codes that potentially correspond to rare diseases (2014-2024), and the models of in-hospital mortality and length of stay. Raw data are public and are not redistributed here; see `datos/README.md`.

## Contenido

| Carpeta | Qué contiene |
|---|---|
| `codigo/` | Scripts en R y `config_rutas.R` (única ruta de datos) |
| `catalogo/` | `cie10 raras_corregido.xlsx`: catálogo de códigos CIE-10 potencialmente raros (mapeo de Orphanet y catálogo complementario de Blazsik et al. 2021; 1,732 códigos únicos, 1,729 tras excluir K74.6, C50.9 y C34.9) |
| `docs/` | Guía metodológica y LEEME del paquete (orden de corrida, qué produce cada script) |
| `datos/` | Instrucciones para obtener los datos públicos (no se incluyen) |

## Cómo correrlo

1. Obtener los datos (ver `datos/README.md`) y copiarlos a una carpeta de trabajo.
2. Abrir R en `codigo/` (`setwd("codigo")`) y editar **solo** `config_rutas.R`, o definir la variable de entorno `ER_DATOS` con la ruta de los datos.
3. Correr `00_prerrequisitos.R`: lista los paquetes de R y los archivos que faltan. No instala nada.
4. Correr los scripts en el orden de `docs/LEEME_paquete_replicacion 19Oct2026.md`: A (prevalencia) y C (potencialmente raros) son independientes; la línea geográfica es B1 → B2 → B3 → B4; los demás scripts producen tablas y figuras a partir de las sesiones guardadas.

B1 es la corrida pesada (11 años de defunciones y egresos) y la única que necesita internet, solo la primera vez, para bajar los límites de entidades (GADM 4.1).

## Estado de verificación

Los análisis completos se corrieron con datos entre el 14 y el 19 de octubre de 2026. Los cambios de entorno posteriores (rutas, carga de sesiones, GADM en caché, corrección del doble conteo de E84.8) se verificaron con `parse()`; la corrida completa de B1 con la versión nueva está en curso de confirmación.

## Cómo citar

Pendiente: se agregará el DOI de la versión archivada en Zenodo y la referencia del artículo. Mientras tanto, ver `CITATION.cff`.

## Licencia

Código: MIT (`LICENSE`). Documentación (`docs/`): CC BY 4.0. Los datos de Orphanet, de Blazsik et al. (2021), de DGIS, INEGI, CONAPO y Naciones Unidas conservan sus licencias y términos de uso originales.
