# ISDM — Índice Sintético de Desigualdad Migratoria (Argentina y Chile, 2015–2024)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18734971.svg)](https://doi.org/10.5281/zenodo.18734971)

**Autora:** Natalia Debandi · **Proyecto:** MIGMOBS Latam

Código del artículo «Brechas de acceso a derechos entre población migrante y no migrante en Argentina y Chile (2015-2024)», número especial de *Comparative Migration Studies*. El ISDM resume las brechas entre población migrante y no migrante en tres dimensiones (laboral, pobreza y acceso a derechos sociales), dentro de cada país y año, y compara esas brechas entre Argentina y Chile.

## Cómo correrlo

Los scripts se corren **en orden, del 00 al 10**. Cada uno lee lo que produjo el anterior. Todos empiezan con `rm(list = ls())`, así que se pueden correr uno por uno a mano en RStudio. `correr_todo.R` corre del 01 al 10 seguidos, cada uno en un proceso aparte, con logs en `logs/`.

| Paso | Script | Lee | Escribe |
|---|---|---|---|
| 00 | `00_contexto_ARG_CHL.Rmd` | Banco Mundial (WDI, por internet), `Data/UNU_CRIS_adjusted.csv`, `Data/migration_imputed_RIKS_dec2021.csv` | `output/contexto/` (Gráficos 1, A1 y A2) |
| 01 | `01_procesamiento_EPH.R` | microdatos EPH 2016–2024 | `Data/EPH_indiv_2016_2024_proc.csv`, `Data/EPH_hogar_2016_2024_proc.csv` |
| 02 | `02_procesamiento_CASEN.R` | microdatos CASEN 2015, 2017, 2020, 2022, 2024 | `Data/Casen_unificado_2015_2024_proc.csv` |
| 03 | `03_indicadores_EPH.R` | paso 01 | `Data/indicadores_eph_2016_2024.csv` |
| 04 | `04_indicadores_CASEN.R` | paso 02 | `Data/indicadores_casen_2015_2024.csv` |
| 05 | `05_indicadores_unificados.R` | pasos 03 y 04, `Data/indicadores_ENE_CH.xlsx` | `Data/indicadores_unificados_arg_chile_2015_2024.csv` |
| 06 | `06_construccion_brechas.Rmd` | paso 05 | `Data/indicadores_con_brechas_arg_chile_2015_2024.csv` (migrantes totales) y `..._solocasen_...` (migrantes recientes) |
| 07 | `07_ISDM.R` (usa `R/isdm_funciones.R`) | paso 06 | `output/isdm/` |
| 08 | `08_composicion_y_edad.R` | pasos 01 y 02 | `output/isdm/composicion_*`, `actividad_estandarizada_edad*` |
| 09 | `09_graficos_articulo.R` | paso 06 | `output/articulo/` (Gráficos 2 a 6) |
| 10 | `10_calidad_muestral.R` | pasos 01 y 02 | `output/calidad/calidad_muestral.csv` |

Rutas de los microdatos: `c:/Data/EPH` y `c:/Data/Casen`, o las variables de entorno `ISDM_EPH_DIR` e `ISDM_CASEN_DIR`.

## Qué sale y dónde está en el artículo

| Artículo | Archivo |
|---|---|
| Tablas 1, 2 y 3 (brechas por indicador, pp) | `output/isdm/brechas_por_indicador.csv` |
| Tabla 4 (ISDM) | `output/isdm/isdm_resumen_pais.csv` |
| Gráfico 5 y nota 6 (serie anual) | `output/isdm/isdm_anual.csv` |
| Gráfico 6 (subíndices) | `output/isdm/subindices_resumen.csv` |
| Tabla A2 (sensibilidad del indicador de salud) | `output/isdm/sensibilidad_salud.csv` |
| Tabla A3 (composición) | `output/isdm/composicion_poblaciones.csv` |
| Tabla A4 (actividad estandarizada por edad) | `output/isdm/actividad_estandarizada_edad.csv` |
| Gráficos 2 a 6 | `output/articulo/` |
| Gráficos 1, A1 y A2 | `output/contexto/` |

## Decisiones metodológicas principales

- **Grupos.** Argentina: migrante = nacido en otro país (CH15); Chile: nacionalidad extranjera (r1a). Migrante reciente: vivía en otro país cinco años antes (CH16 en la EPH, r2 en la CASEN; en la CASEN 2024, llegada entre 2020 y 2024).
- **Indicadores.** Laboral: tasa de actividad, desempleo, informalidad asalariada y cuentapropismo (15 años y más en los dos países). Pobreza: pobreza e indigencia (EPH con PONDIH, como el INDEC). Social: asistencia escolar 6–17, asistencia a educación superior 18–29 y cobertura de salud (afiliación a algún sistema; solo Chile, porque en Argentina la cobertura pública es universal).
- **Fuentes en Chile.** Actividad y desempleo de los migrantes totales, de la ENE (indicadores publicados por el INE, 2017–2024). Todo lo demás, de la CASEN. La informalidad asalariada se calcula con la CASEN: un asalariado no afiliado a un sistema previsional cuenta como «no cotiza». La pobreza 2024 se toma con la metodología anterior (`pobreza_2013`), comparable con 2015–2022.
- **Años.** 2020 se excluye en los dos países. En Chile, las tasas de cada grupo se interpolan linealmente entre ondas de la CASEN, sin usar 2020 como punto de apoyo.
- **Brecha.** Δ = s × (tasa no migrante − tasa migrante), con s = +1 si un valor alto es bueno y −1 si es malo. Positivo: mejor la población no migrante.
- **Estandarización.** d = Δ / √(p̄(1−p̄)), con p̄ el promedio de las tasas de los dos grupos (diferencia de proporciones estandarizada). El cero es la paridad.
- **Agregación.** Subíndice = promedio de las d de la dimensión; ISDM = promedio de los tres subíndices. Absoluto (con |d|) y direccional (con signo), en d y en pp. El valor del período es el promedio de los años.

La documentación completa (ejemplos numéricos, problemas detectados y su corrección) está en `ISDM_metodologia_trazabilidad.docx`, en la carpeta del artículo.

## Datos

Los microdatos no están en el repositorio. Se descargan de:

| Datos | Fuente |
|---|---|
| EPH 2016–2024 | INDEC, [indec.gob.ar](https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos) |
| CASEN 2015, 2017, 2020, 2022, 2024 | Ministerio de Desarrollo Social y Familia de Chile, [observatorio.ministeriodesarrollosocial.gob.cl](https://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen) |
| ENE (indicadores publicados) | INE Chile; cargados en `Data/indicadores_ENE_CH.xlsx` |
| Stock migratorio | UNU-CRIS / RIKS |

Los microdatos procesados (`Data/*_proc.csv`) tampoco se suben, porque son registros individuales y pesan casi 1 GB; se regeneran con los pasos 01 y 02.

Sí están en el repositorio todos los resultados agregados, de modo que las cifras del artículo se pueden verificar sin correr nada:

- `Data/indicadores_*.csv`: indicadores por país, año y grupo (no migrante, migrante, migrante reciente), y las brechas.
- `Data/indicadores_ENE_CH.xlsx`: indicadores de la ENE tomados de las publicaciones del INE.
- `output/isdm/`: brechas estandarizadas, ISDM anual y resumen por país, subíndices, sensibilidad, composición y tasa de actividad estandarizada por edad.
- `output/articulo/` y `output/contexto/`: los gráficos del artículo.
- `output/calidad/`: tamaños muestrales y calidad de las estimaciones.

## Versiones

- `v1.0.0` y `v1.1.0`: versiones publicadas en Zenodo (cálculo original del índice).
- `v1.2.0-pre-orden`: estado del repositorio antes de ordenarlo (23/11/2026), con los scripts originales y las versiones intermedias de la revisión de noviembre de 2026.
- `v2.0.0`: cadena final numerada 00 a 10, la que produce las cifras de la versión revisada del artículo (noviembre de 2026). Corrige la estandarización del índice, la población en edad de trabajar, el ponderador de pobreza, el indicador de salud, la interpolación, la informalidad y el cuentapropismo en Chile y el corte de pobreza de la CASEN 2024.

## Licencia

CC BY 4.0 (ver `LICENSE`).

## Cómo citar

Debandi, N. (2026). *ISDM: Índice Sintético de Desigualdad Migratoria (Argentina y Chile, 2015-2024)* (v2.0.0) [Software]. Zenodo. https://doi.org/10.5281/zenodo.18734971

El DOI de arriba remite al registro en Zenodo, donde figuran todas las versiones. Los metadatos de cita están en `CITATION.cff` (botón «Cite this repository» de GitHub).
