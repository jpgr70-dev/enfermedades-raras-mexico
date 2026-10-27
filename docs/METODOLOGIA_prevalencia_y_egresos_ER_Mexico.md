# Enfermedades raras en México — guía metodológica de los análisis del manuscrito

**Para:** colaboradores del manuscrito *"Situación de las enfermedades raras en México"* (versión `…18Oct2026.docx`).
**Propósito:** explicar qué hace cada script, en qué orden correrlos, qué tabla o figura del paper genera cada uno y qué decisiones metodológicas se tomaron (y por qué). Los scripts muestran el *cómo*; este documento explica el *por qué*.
**Versión:** 19-oct-2026. Sustituye a la versión del 4-oct-2026 y a `METODOLOGIA_prevalencia_y_egresos_ER_Mexico.md` del 25-sep. Las instrucciones prácticas de corrida están en `LEEME_paquete_replicacion.md` (carpeta del paquete).

---

## 0. Paquete de replicación: script → sección del manuscrito

Todos los scripts están en la carpeta **`Paquete Andres/`**, en UTF-8 y con saltos de línea LF. Con ellos se reproducen las tablas, figuras y cifras de resultados del manuscrito.

| # | Script | Reproduce en el manuscrito | Depende de | Produce |
|---|---|---|---|---|
| A | `prevalencia_acumulada_mexico_2026.R` | **Tabla 1** (prevalencia puntual) y sensibilidades (sin ajuste, México/Europa, con grupos y categorías, punto medio, promedio empírico por clase, diagnóstico del umbral) | Independiente | `prevalencia_acumulada_mexico_2026_desdeR…xlsx` |
| B1 | `enfermedades_raras_principal_unificado.R` | Tasas nacionales y por cuadrante; mapas; tablas de manuscrito | Independiente (lee las bases crudas; es el paso lento) | `tablas_manuscrito_v2.xlsx`, `top10_causas_cuadrantes.xlsx`, `contraste_ampliado_5_enfermedades.xlsx`, mapas; guarda `sesion_base_raras.RData` y `sesion_ampliado.RData` |
| B2 | `analisis geografico adicional.R` | Moran's I global y por cuadrante (mortalidad y egresos Sectorial); LISA | Sesiones de B1 (las carga solo si faltan) | `analisis_geografico_cuadrante_moran.xlsx`, `analisis_geografico_ampliado_moran.xlsx`; guarda `sesion_geografico_extendido.RData` |
| B3 | `analisis_geografico_extendido_parte2.R` | LISA del cuadrante 3 en mortalidad; Moran/LISA de egresos Sectorial | Sesiones de B1 y B2 (las carga solo) | `analisis_geografico_extension2.xlsx`, `mapa_lisa_*.jpeg` |
| B4 | `analisis_geografico_ssa_residencia.R` | Sub-análisis SSA/IMSS-Bienestar por residencia | Sesiones de B1 y B2 + `ssa_egresos_AAAA.zip` | `analisis_geografico_ssa_residencia.xlsx` y mapas |
| C | `analisis_potencialmente_raros_centrado.R` | **Tabla 2, Tabla 3 y escenarios de referencia** (deciles de frecuencia, Firth, AME, modelo de propensión) | Independiente | `potencialmente_raros_centrado.xlsx`, `escenarios_referencia_deciles…xlsx`, checkpoints en `analisis/checkpoints_potencialmente_raros` |
| — | `tabla2_referencia_AN_PQ.R` | Columna «Referencia principal, capítulos A–N, P y Q» (E6b) de la Tabla 2 | Checkpoint de C | `tabla2_referencia_AN_PQ 23Oct2026.xlsx` |
| — | `tasas por entidad 2014 y 2024 16Oct2026.R` | Tasas por entidad 2014 y 2024 (texto de Figuras 1 y 2) | Sesión de B1 | `tasas_por_entidad_2014_2024 16Oct2026.xlsx` |
| — | `figuras 1 y 2 mapas con claves de entidad 18Oct2026.R` | **Figuras 1 y 2** (mapas 2014 vs. 2024 con claves de entidad) | Sesión de B1 | `mapa_egresos_…18Oct2026.jpeg`, `mapa_defunciones_…18Oct2026.jpeg` |
| — | `figuras_3_y_4_forest_plot_escenarios.R` | **Figuras 3 y 4** (forest plot de escenarios) | xlsx de escenarios de C | dos `.jpeg` y un `.xlsx` |

**Orden de corrida:** A y C son autocontenidos y pueden correrse en cualquier momento. La línea geográfica va en orden **B1 → B2 → B3 → B4**. Los scripts de figuras y de tasas por entidad se corren después de B1 (o de C). B1 es el paso lento, porque lee los 11 años de defunciones y de egresos; B2 a B4 cargan las sesiones `.RData`, así que no hace falta repetir B1 cada vez.

**Antes de correr nada:** `setwd()` a la carpeta del paquete, ajustar `datos_path` **solo** en `config_rutas.R` y correr `00_prerrequisitos.R`, que lista los paquetes de R y los archivos que faltan sin instalar nada por su cuenta. Internet solo se usa en la primera corrida de B1, para bajar los límites de entidades (GADM 4.1) a `datos_path/gadm41_MEX_1.json`; después se lee del disco.

**Sobre el script principal unificado (B1):** es la versión del equipo Windows (con las correcciones de agosto, el ejercicio «ampliado», las tablas de manuscrito y el guardado de sesión que usan B2 a B4) más la sección de mapas 2014 vs. 2024 (sección 15ter). B1 ya no usa nombres de objeto con «ñ» (desde el 3-oct).

**Estado de verificación:** B1 a B4 se corrieron completos con datos el 14–15-oct-2026 (B1 ya incluía la sección de mapas). C se corrió el 18–19-oct (claves de entidad de 32 categorías). Los cambios de entorno del 19-oct (rutas, carga de sesiones, GADM en caché, corrección de E848) se verificaron solo con `parse()`; **falta una corrida de B1 con la versión nueva** (la hará Andrés al probar el paquete).

**Corrección de denominadores (3-oct-2026, script B1):** todas las tasas usan una sola fuente de población, CONAPO (`0_Pob_Mitad_1950_2070.xlsx`), leída y verificada en la sección 1b. Antes convivían cuatro fuentes: Banco Mundial (tasas nacionales), estimaciones CONAPO 2019 escritas a mano (tasas por entidad), un total nacional de 130,861,007 escrito a mano (mapas 2024) y proporciones fijas por sexo de 48.9% y 51.1%. `pop_estado`, que usan B2 y B4, es la población media anual 2014–2024 de CONAPO por entidad. Las tasas nacionales bajaron cerca de 1% (CONAPO 2024 = 132,274,416 frente a 130,861,007) y los índices de Moran se movieron poco.

**No incluidos en el paquete (no se usan en el manuscrito):** `disbayes prevalencia.R` (enfoque bayesiano descartado por costo computacional), `identificar causas que podrian no ser raras.R`, `analisis_deciles_frecuencia(-1).R` (antecedente del script C), `egresos.R` y versiones anteriores del script principal.

---

## 1. Prevalencia puntual acumulada (Orphanet, población de referencia 2026)

### 1.1 Enfoque
Réplica de **Nguengang Wakap et al. 2020** (*Eur J Hum Genet* 28:165–73) aplicada a México, con una extensión propia: el ajuste por estructura de edad, que no está en el paper original ni en la réplica de Sudáfrica (Malherbe et al. 2026).

### 1.2 Pasos
El Excel de resultados trae hojas de supuestos y sensibilidad. Resumen:

1. **Universo:** 6,728 entidades con registro de prevalencia en Orphadata (`es_product9_prev.xml`) y 7,790 con edad de inicio (`es_product9_ages.xml`). Solo se usan registros con `PrevalenceType = "Prevalencia puntual"`, validados o no, como en el paper. De las 6,728, **4,852 tienen un dato utilizable** y 1,876 no; de las 4,852 se excluyen 149 grupos y categorías, de modo que **la estimación principal incluye 4,703 condiciones**.
2. **Selección del dato de prevalencia puntual**, en orden jerárquico: Mundial → Europa (UE, Rusia, Turquía, Islandia) → EE.UU. Si hay varios registros válidos en el mismo nivel, se promedian (decisión propia; el paper no especifica este caso).
3. **Clases de prevalencia y umbral por registro (decisión de JP, M1, 18-oct):** el umbral de enfermedad rara (5/10,000) se aplica **por registro**: se descarta el registro que lo supera, no la enfermedad completa. Solo se usan las cuatro clases hasta el umbral (<1/1,000,000; 1–9/1,000,000; 1–9/100,000; 1–5/10,000); se excluyen «6–9/10,000» y «>1/1,000», como en el paper. El script A incluye una hoja de diagnóstico del umbral (31 condiciones incluidas con algún registro sobre el umbral aportan 0.32–0.54 pp; 12 excluidas).
4. **Mínimo y máximo:** si hay valor puntual (`ValMoy > 0`), el mínimo y el máximo son ese valor. Si solo hay clase, se toman los límites inferior y superior de la clase (a diferencia de Wakap, que fija la clase <1/1,000,000 en 1/1,000,000 en ambos extremos; aquí el mínimo es 0).
5. **Condiciones con solo casos o familias reportados (142):** se suman y se dividen entre la población mundial.
6. **Excluidas por falta de dato utilizable (1,876):** solo incidencia anual (168), solo prevalencia al nacer o de por vida (19) y sin clase numérica utilizable (1,689).
7. **Ajuste demográfico (extensión propia; estimación principal por decisión de JP, M3):** la tasa de cada condición se multiplica por (fracción de población de México en la banda de edad de inicio) / (fracción mundial en esa banda), con datos de 2026. Bandas, tomadas de la réplica de Sudáfrica: 0–1, 2–11, 12–18, 19–65 y 66 años o más. Se ajustan las condiciones con una única categoría de edad de inicio: **1,627 de las 4,703** (el 3,059 de versiones anteriores contaba las 6,728 entidades). Las que dicen «cualquier edad», no tienen dato o tienen varias categorías quedan sin ajuste. El escenario sin ajuste va como sensibilidad.
8. **Poblaciones:** México 2026 = 134,407,258 (CONAPO, `0_Pob_Mitad_1950_2070.xlsx`) y mundo 2026 = 8,300,678,396 (ONU WPP2024, variante media).
9. **Clasificación por cuadrante:** cada enfermedad se cruza por `orphacode` con `matriz_clasificacion_v01.csv`, la matriz 3×3 de ventana de detección × tratabilidad. **El manuscrito ya no usa los cuadrantes (M4, 12-oct); se conservan en el Marco de acción como tipología para ordenar los retos.** La edad de inicio de Orphanet es el estratificador del manuscrito.
10. **Agregación:** los rangos mínimo y máximo se suman a nivel nacional y por condición.

### 1.3 Decisiones metodológicas
| Decisión | Justificación |
|---|---|
| **Se priorizó el dato de México sobre la jerarquía Mundial/Europa/EE.UU. en 4 de los 16 registros con geografía México** (Fenilcetonuria ORPHA:716, Hipotiroidismo congénito 442, Hemofilia A y B 98878/98879) | En cada caso se verificó el valor contra su artículo fuente original (PDF en `Articulos Mexico/`). Hipotiroidismo congénito es una «Categoría» en Orphanet, así que solo influye en el escenario que incluye grupos y categorías. |
| **Las polidactilias postaxiales A y B (93334, 93335) se excluyen de los datos mexicanos (M2, 18-oct)** | Son prevalencia al nacimiento, no puntual. |
| **Los otros 12 registros con geografía México se dejaron sin cambio** | Por ejemplo, Esporotricosis (ORPHA:826) es un estudio regional de zona hiperendémica etiquetado por error como nacional (PMID 25526781). |
| **Sirenomelia (ORPHA:3169) se dejó fuera del ajuste** | El dato de México es 236 veces el valor mundial; queda pendiente de una decisión aparte. |
| **Se excluyen del escenario principal las entidades con DisorderGroup = «Grupo de trastornos» o DisorderType = «Categoría» (149)** | Son colecciones de enfermedades, no entidades clínicas individuales, y sumarlas duplicaría casos. Quedan marcadas en el detalle por condición. |
| **El escenario con ajuste es el principal; el escenario sin ajuste es sensibilidad (M3)** | El ajuste apenas mueve el total (−0.005 pp en el mínimo, −0.012 pp en el máximo): sirve para mostrar que la estimación es estable frente al perfil demográfico. |

### 1.4 Resultado de referencia (para verificar una corrida)
Corrida del 17-oct-2026 (`prevalencia_acumulada_mexico_2026_desdeR 17Oct2026.xlsx`), N = 4,703:

| Escenario | Casos | % población |
|---|---|---|
| **Principal: sin grupos ni categorías, con ajuste demográfico** | **6,449,128 – 10,453,743** | **4.80% – 7.78%** |
| Sin ajuste demográfico (réplica literal del paper) | 6.45 – 10.47 M | 4.80% – 7.79% |
| Ajuste México/Europa (aproximado con «More developed: Europe», WPP2024) | 6.69 – 10.92 M | 4.97% – 8.12% |
| Incluye grupos y categorías | — | 5.96% – 9.60% |
| Punto medio de clase (con o sin ajuste) | 8.45 M | 6.29% |
| Promedio empírico por clase | 7.88 M | 5.87% |

Los valores puntuales (620 condiciones) aportan 4.09 pp; las condiciones con solo clase (3,942) aportan 0.71–3.69 pp.

**Por qué superamos el 3.5%–5.9% de Wakap:** no es el ajuste. Wakap usó 3,585 enfermedades con prevalencia y aquí hay 4,703 (+31%; escalado, 4.6%–7.7%), y trata distinto el extremo inferior de la clase más baja.

**Historial de cifras (para no confundirse con documentos viejos):** la versión del 16-sep reportaba N = 4,854 (que es el N del escenario con grupos y categorías) junto a los resultados del principal; el 24-sep se corrigió a 4,705 (4.85%–7.83%) y el 18-oct, tras M1 y M2, a 4,703 (4.80%–7.78%). Ninguna cifra anterior debe usarse.

### 1.4b Limitaciones propias de esta línea
- **Transferibilidad:** la mayoría de las prevalencias viene de literatura europea o «mundial», no de estudios en México. Puede no aplicar a enfermedades con efecto fundador o con variación étnica marcada.
- **Posible doble conteo residual:** aun sin grupos ni categorías, podrían quedar pares enfermedad–subtipo contados dos veces. `en_product7.xml` (linearisation) se revisó y no sirve (clasifica por especialidad médica).
- **Es un techo, no un valor esperado:** el mínimo se fija en 0 para la clase más baja y se descartan registros sobre el umbral por registro (ver 1.2, paso 3).
- **Estudios con datos administrativos y registros** reportan cifras menores (Véneto 0.3%, Inglaterra 1.5%, Australia Occidental, Hong Kong y Tasmania 1.5%–2.0%); la discusión del manuscrito las contrasta con esta estimación.

### 1.5 Notas técnicas
- El análisis se diseñó primero en Python y después se portó a R; la versión en R se validó contra la de Python.
- Paquetes de R: `xml2`, `data.table`, `dplyr`, `openxlsx`.
- Particularidades del archivo ONU WPP2024 (la lectura de población es el paso más frágil del script):
  - El archivo por grupos regionales no trae el agregado «World». Para el total mundial se usa el archivo completo por país, filtrando `Location code == 900`.
  - No existe una hoja combinada «Estimates and Medium variant»: las estimaciones van en «Estimates» (hasta 2023) y las proyecciones en «Medium variant» (2024 en adelante).
  - Un error en los metadatos del xlsx hace que la detección automática del rango lea solo las primeras filas. Hay que fijar `cols`/`rows` o usar `readxl`.
  - `openxlsx::read.xlsx()` sanea los nombres de columna: espacios y puntuación se convierten en «.».

---

## 2. Distribución geográfica de egresos y defunciones (definición por catálogo CIE-10)

### 2.1 Identificación de condiciones potencialmente raras
- **Catálogo:** mapeo oficial CIE-10 de Orphanet más el catálogo complementario de **Blazsik et al. 2021** (*Orphanet J Rare Dis* 16:105), en `Datos/cie10 raras_corregido.xlsx`: 1,738 filas, 1,732 códigos únicos (sin los símbolos †/*/, que rompían el match) y **1,729 tras excluir K746, C509 y C349**.
- **Coincidencia exacta a 4 caracteres** contra el diagnóstico principal del egreso (`AFECPRIN4`) o la causa básica de defunción (`CAUSA_DEF`), tras quitar puntos y truncar a 4 caracteres.
- **Se descartó el match por prefijo de 3 caracteres:** los 89 códigos de 3 caracteres del catálogo quedan sin match porque expandirlos metía enfermedades comunes (I10, E10, C53, C61, etc.).
- **Matriz de cuadrantes:** `matriz_clasificacion_v01.csv` (misma matriz que en la sección 1); solo se usa en el Marco.

### 2.2 Decisiones sobre códigos
| Decisión | Justificación |
|---|---|
| **K746, C509 y C349 se excluyen** (catálogo por cuadrante desde el 24-ago; catálogo general desde el 12-oct) | Códigos de baja especificidad que sobrerrepresentaban cánceres y cirrosis comunes (en 2024 sumaban 30,447 de 112,983 defunciones del catálogo general, 27%). Decisión definitiva. |
| **Se conserva G560 (túnel del carpo, «NON RARE IN EUROPE» en Orphanet)** | La etiqueta se refiere a Europa y no prueba que sea común en México; se discute en limitaciones. |
| **Principio general** | Los códigos ambiguos de baja especificidad se excluyen con justificación explícita, y los casos límite se discuten en limitaciones. |

### 2.3 Variable geográfica y bases
- **Defunciones:** entidad de residencia habitual (`ENT_RESID`, INEGI).
- **Egresos, dos fuentes en paralelo:**
  - (a) **Base Sectorial completa:** registra la entidad de la unidad médica (`CEDOCVE`), no la residencia. Puede sobrerrepresentar estados con hospitales de referencia (CDMX fija la escala de color de la Figura 1).
  - (b) **Sub-análisis SSA/IMSS-Bienestar:** sí registra la residencia habitual, pero cubre solo esa red (alrededor del 42% de los egresos por enfermedades raras).
  - Desde 2024 las unidades que antes aparecían como «SSA» se registran como «IMB» (IMSS-Bienestar). Se tratan como la misma red; si no, aparece una caída falsa en 2024. En la base Sectorial, los Servicios de Salud IMSS-Bienestar se agrupan con Secretaría de Salud, y el programa IMSS-Bienestar rural (ex-COPLAMAR) queda en «Otras».
- **Claves de entidad:** en la base Sectorial `CEDOCVE` traía 41 categorías (claves «1»–«9» y «01»–«09» a la vez). Todos los scripts rellenan a dos dígitos; el script C lo comprueba con `stopifnot` (32 claves).
- **Egresos vivos:** se excluye `MOTEGRE = 5` (defunción).
- **Agrupamiento espacial:** índice global de Moran; si es significativo, LISA. Matriz de contigüidad sobre las 32 entidades de **GADM 4.1** (`gadm41_MEX_1.json`, descarga única).
- **Heterogeneidad de tasas por entidad:** parte de la variación entre entidades refleja diferencias en la especificidad de codificación (el catálogo incluye subcódigos de 4 caracteres de cánceres frecuentes, como C50x, C54x, C90x, C91x, C44x y K80x). Está documentado como limitación del manuscrito; no se hizo análisis de sensibilidad que los excluya.

### 2.4 Correcciones incorporadas en el script principal (B1)
1. Lee el catálogo desde `cie10 raras_corregido.xlsx` (ya no del `.dta`).
2. Excluye K746/C509/C349 en el origen y en el catálogo general.
3. Separa `egresos_raras` (catálogo completo) de `egresos_raras_cuadrantes` (solo códigos con cuadrante asignado).
4. **Ejercicio «ampliado» (19-oct):** corrige el doble conteo de E84.8 (ver sección 4, punto 3).

### 2.5 Resultados de referencia (corrida B1–B4 del 14–15-oct-2026, denominadores CONAPO)
Fuentes: `Datos/analisis_geografico_cuadrante_moran.xlsx` (hojas `moran_global`, `moran_por_cuadrante`, `lisa_clusters`), `Datos/analisis_geografico_extension2.xlsx` (egresos Sectorial y LISA del cuadrante 3) y `Datos/analisis/analisis_geografico_ssa_residencia.xlsx` (SSA/residencia). En el manuscrito solo se usa el nivel global.
- **Moran global (van en el manuscrito):** mortalidad I = 0.003 (p = 0.39); egresos Sectorial I = 0.007 (p = 0.37); egresos SSA/residencia I = −0.033 (p = 0.50). Sin agrupamiento espacial global; el LISA localiza solo clústeres puntuales, que se presentan como exploratorios.
- **Por cuadrante (solo Marco; provisionales):** mortalidad, cuadrante 3: I = 0.233 (p = 0.018) y cuadrante 9: I = 0.070 (p = 0.19); los demás no son significativos. Egresos SSA/residencia: cuadrante 4, I = 0.186 (p = 0.024); cuadrante 8, I = 0.302 (p = 0.003); cuadrante 5 en el límite (p = 0.092). Egresos Sectorial: ningún cuadrante significativo.
- Las cifras del 3-oct (mortalidad I = 0.037, egresos I = 0.008, SSA I = −0.113) quedaron superadas por la exclusión de K746/C509/C349 del catálogo general (12-oct), que cambió los índices globales.

**Dos series de conteos en `tablas_manuscrito_v2.xlsx`:** las hojas anuales usan el catálogo general; las hojas por cuadrante suman la serie restringida a códigos con cuadrante asignado y sin K746/C509/C349. Hay que indicar en cada tabla o texto cuál se reporta.

**Figuras 1 y 2:** tasas de egresos y defunciones 2014 vs. 2024 por entidad, con claves de entidad (script `figuras 1 y 2 mapas con claves de entidad 18Oct2026.R`). Defunciones: se usa `defunciones_raras` si existe en la sesión y, si no, `defunciones_raras_cuadrantes` (~0.6% menos).

---

## 3. Perfil de egresos «potencialmente raros» (definición por frecuencia, Tröster et al. 2023)

### 3.1 Enfoque
Basado en **Tröster et al. 2023** (*BMC Med Res Methodol* 23:143), que define diagnósticos raros a partir de su frecuencia.

### 3.2 Pasos
1. Se normalizan los códigos a mayúsculas (`toupper()`); había unos 1,900 registros con minúsculas.
2. **Cada código CIE-10 se agrupa a 3 caracteres** y se ordena por su número de egresos acumulados 2014–2024.
3. Los egresos se dividen en **deciles nacionales de frecuencia por registro**, del 1 (menos frecuente) al 10 (más frecuente).
4. Se contrasta con el catálogo Orphanet + Blazsik para construir la categoría **«potencialmente raros»**. Las otras categorías son «Resto, deciles 1–8» y «Referencia, deciles 9–10». Además se evalúan escenarios alternativos de referencia (E1–E6b; la referencia principal E6b son los egresos no potencialmente raros de los capítulos A–N, P y Q, sin O ni R–Z), que alimentan las Figuras 3 y 4.
5. **Mortalidad hospitalaria y días de estancia** por categoría y por decil, ajustados por edad, sexo y año, con regresión con reducción de sesgo tipo Firth.
6. **Modelo de propensión a «potencialmente raro»:** regresión logística con edad, sexo, institución (IMSS, SSA, ISSSTE, Otras), efectos fijos por año y por entidad de la unidad médica (32 claves), errores estándar agrupados por entidad y efectos marginales promedio (AME).
7. **Muestra:** los modelos se estiman sobre una muestra aleatoria de **3,000,000** egresos con institución válida (**semilla 20250918**).

### 3.3 Decisiones metodológicas
| Decisión | Justificación |
|---|---|
| **Agrupar a 3 caracteres, no a 4 como el paper original** | Una enfermedad con varios subcódigos no parece artificialmente más rara por repartir su volumen. |
| **Deciles por registro, no por paciente** | No hay identificador único de paciente. Tampoco se restringe a mayores de 18 años. |
| **Deciles calculados una vez sobre el acumulado 2014–2024** | Mismo criterio que la clasificación por cuadrante. |
| **El script es autocontenido y no carga defunciones** | Ahorra memoria y tiempo. |

### 3.4 Advertencias
- Un solo código dominante puede ocupar un decil completo (O80, parto único espontáneo, en el decil 10). Es un rasgo del método.
- «Raro por frecuencia de registro» no equivale a «enfermedad rara clínicamente reconocida». El decil 1 incluye enfermedades comunes que rara vez son diagnóstico principal de hospitalización.

---

## 4. Advertencias transversales (para interpretar y para limitaciones)

1. **Los cuadrantes 3, 6 y 9 tienen confiabilidad reducida.** Más del 99% de sus filas en `matriz_clasificacion_v01.csv` son asignaciones automáticas sin curación manual (`confianza = "baja"`). Los cuadrantes 1, 2, 4, 5 y 8 están curados al 100%, pero tienen pocos códigos. Todo resultado de los cuadrantes 3, 6 o 9 es provisional.
2. **Los conteos basados en CIE-10 son «marcadores», no casos confirmados.** Según el mapeo oficial de Orphanet, solo alrededor del 26% de los códigos usados tienen relación «Exacta»; la mayoría comparte código con otras condiciones.
3. **Ejercicio «ampliado» (no está en el paper ni en el manuscrito).** Agrega subcódigos para Fibrosis quística, Anemia falciforme, Hemofilia A y B y Huntington, que con el catálogo actual quedan en cero casos. **Doble conteo de E84.8 (corregido el 19-oct):** `E848` ya estaba en la matriz como otra condición (síndrome fibrosis quística–gastritis–anemia megaloblástica, cuadrante 6, confianza baja) y el ejercicio lo volvía a agregar como fibrosis quística (cuadrante 2); cada egreso o defunción con E848 se contaba dos veces. Ahora los subcódigos nuevos reemplazan cualquier fila previa con el mismo código, el script imprime cuáles reemplazó y se detiene si queda un código repetido. El análisis principal no cambia. Falta confirmar con una corrida de B1.
4. **Se descartó la estimación bayesiana multiestado (`disbayes`)** por costo computacional (una sola condición, fibrosis quística, tomó demasiado tiempo). Los egresos y defunciones describen magnitud y distribución, no estiman prevalencia.
5. **Dos definiciones de «raro».** El Marco usa el catálogo completo (≈7.5% de los egresos); el manuscrito usa «potencialmente raros» (definición por frecuencia). No son intercambiables.

---

## 5. Requisitos para reproducir
- **R** con `data.table`, `dplyr`, `openxlsx`, `xml2`, `haven`, `foreign`, `sf`, `ggplot2`, `ggrepel`, `spdep`, `brglm2`, `fixest`, `marginaleffects` y `MASS` (`00_prerrequisitos.R` los revisa).
- **Rutas:** una sola, en `config_rutas.R` (`datos_path`, o la variable de entorno `ER_DATOS`). El equipo de corrida usa `E:/jp/enfermedades raras/Datos`.
- **Archivos en `datos_path`:** `egresos/`, las bases de defunciones por año, `0_Pob_Mitad_1950_2070.xlsx`, `cie10 raras_corregido.xlsx`, `13023_2021_1737_MOESM1_ESM.xlsx`, `es_product9_*.xml`, `WPP2024_*.xlsx`, `orphanet catalogo.dta`, `analisis/matriz_clasificacion_v01.csv` y `ORPHA_ICD10_mapping*.xml` (de `Orphanet_Nomenclature_Pack_ES`; el script C lo busca dentro de `datos_path`).
- **Memoria:** los egresos Sectorial suman alrededor de 60 millones de registros. Los scripts filtran y agregan dentro de la función de carga anual antes de consolidar con `rbindlist`. Se recomienda una sesión limpia y varios GB de RAM libres.

## 6. Referencias clave
- Nguengang Wakap S, et al. *Eur J Hum Genet.* 2020;28(2):165–73. doi:10.1038/s41431-019-0508-0
- Vicente E, Pruneda A, Ardanaz E. *Eur J Hum Genet.* 2021;29(6):1032–3. doi:10.1038/s41431-020-00763-z; réplica de Rath A, et al. *Eur J Hum Genet.* 2021;29(6):1034–5. doi:10.1038/s41431-020-00764-y
- Tröster TS, et al. *BMC Med Res Methodol.* 2023;23:143. doi:10.1186/s12874-023-01972-y
- Blazsik RM, et al. *Orphanet J Rare Dis.* 2021;16:105. doi:10.1186/s13023-021-01737-0
- Malherbe HL, et al. *Orphanet J Rare Dis.* 2026;21. doi:10.1186/s13023-026-04444-w
