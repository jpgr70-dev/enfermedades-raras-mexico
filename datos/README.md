# Datos

Los datos crudos no se incluyen: son públicos, pesados y conservan los términos de uso de cada fuente. Para replicar, descargarlos y copiarlos a una carpeta de trabajo y poner su ruta en `codigo/config_rutas.R` (`datos_path`) o en la variable de entorno `ER_DATOS` (no hay ruta por omisión). `00_prerrequisitos.R` revisa qué falta.

| Archivo o carpeta en `datos_path` | Fuente | Notas |
|---|---|---|
| `egresos/` | Secretaría de Salud, Dirección General de Información en Salud (DGIS): egresos hospitalarios, base Sectorial y base de Servicios de Salud/IMSS-Bienestar, 2014-2024 (SAEH) | Datos abiertos de la DGIS |
| `defunciones_*_dbf.zip` | INEGI: Estadísticas de Defunciones Registradas (EDR), 2014-2024 | Datos abiertos del INEGI |
| `0_Pob_Mitad_1950_2070.xlsx` | CONAPO: Proyecciones de la Población de México y de las Entidades Federativas, 2020-2070 (población a mitad de año) | |
| `WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx` | Naciones Unidas, World Population Prospects 2024 (variante media) | |
| `es_product9_prev.xml`, `es_product9_ages.xml`, `orphanet catalogo.dta`, `ORPHA_ICD10_mapping*.xml` | Orphanet / Orphadata (productos de acceso libre, en español) | Licencia CC BY 4.0 (https://www.orphadata.com); citar a Orphanet |
| `13023_2021_1737_MOESM1_ESM.xlsx` | Material suplementario de Blazsik et al. 2021, *Orphanet J Rare Dis* 16:105 | Acceso abierto; citar el artículo |
| `cie10 raras_corregido.xlsx` | Incluido en `catalogo/` | Derivado de las dos fuentes anteriores |
| `analisis/matriz_clasificacion_v01.csv` | Producto intermedio del proyecto | No se redistribuye en este repositorio |
| `gadm41_MEX_1.json` | GADM 4.1, límites de entidades de México (https://gadm.org) | Se descarga solo en la primera corrida de B1. **No se redistribuye**: la licencia de GADM no lo permite |

Antes de publicar, verificar los términos de uso vigentes de cada fuente.
