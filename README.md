# Migmobs Nat — Índice Sintético de Desigualdad Migratoria (ISDM)

**Autora:** Natalia Debandi
**Proyecto:** MIGMOBS Latam
**Período de análisis:** 2015–2024
**Países:** Argentina y Chile

---

## Descripción general

Este repositorio contiene los scripts, reportes y datos procesados para la construcción del **Índice Sintético de Desigualdad Migratoria (ISDM)**: un índice comparativo que cuantifica las brechas socioeconómicas entre personas migrantes y nativas en Argentina y Chile, a lo largo del tiempo. El ISDM agrega indicadores de mercado laboral, pobreza, educación y salud en un único valor que permite comparar la integración migratoria entre países y períodos.

---

## Estructura del repositorio

```
Migmobs_nat/
├── Scripts de procesamiento de datos (1_procesamiento_*.R)
├── Scripts de construcción de indicadores (1_indicadores_*.R)
├── Reportes de análisis (.Rmd / .html)
├── Data/                  ← Datos procesados
├── graficos/              ← Gráficos de brechas por indicador
├── output/                ← Gráficos contextuales
├── output_context/        ← Gráficos para la sección de contexto
├── output_graficos/       ← Figuras del ISDM (todos los migrantes)
├── output_graficos_recientes/ ← Figuras del ISDM (migrantes recientes)
├── Documentacion/         ← Documentación metodológica de fuentes
├── FMR_Latam/             ← Documento FMR Latam
├── v1_nat/                ← Versión 1 de scripts (archivo histórico)
└── V2_nat/                ← Versión 2 de scripts (archivo histórico)
```

---

## Archivos en la raíz

### Scripts de procesamiento y construcción de indicadores

| Archivo | Descripción |
|---|---|
| `1_procesamiento_casen_2201_v2.R` | Procesa y homogeneiza los datos de la Encuesta CASEN (Chile, ondas 2015–2024). Genera variables comparables con la EPH argentina. |
| `1_procesamiento_EPH_2201_v2.R` | Procesa y homogeneiza los datos de la Encuesta Permanente de Hogares —EPH— (Argentina, trimestres 2016–2024). |
| `1_indicadores_casen_2201_v3.R` | Calcula indicadores socioeconómicos a partir de los datos procesados de CASEN. |
| `1_indicadores_EPH_2201_v3.R` | Calcula indicadores socioeconómicos a partir de los datos procesados de EPH. |
| `1_indicadores_unificados.R` | Unifica los indicadores de Argentina y Chile en una sola base comparativa. |
| `1_calidad_indicadores_2201_v1.R` | Evalúa la calidad estadística de los indicadores calculados (tamaños muestrales, coeficientes de variación, etc.). |

### Reportes en R Markdown

| Archivo | Descripción | Salida |
|---|---|---|
| `1_contexto_ARG_CHL_v3.Rmd` | Análisis de contexto: migración en Argentina y Chile. Incluye series de tiempo de stocks migratorios, indicadores macroeconómicos (PIB, Gini) y comparaciones estructurales. | `1_contexto_ARG_CHL_v3.html` |
| `1_contexto_normativo.Rmd` | Análisis del marco normativo migratorio en Argentina y Chile. Incluye timeline de eventos legislativos clave. | `1_contexto_normativo.html` |
| `1_migracion_ARG_CHL.Rmd` | Reporte sobre flujos y stocks de migración en ambos países. | *(sin .html en raíz)* |
| `2_construccion_ISDM.Rmd` | **Reporte central.** Describe la construcción metodológica del ISDM: selección de indicadores, normalización y agregación. Análisis comparativo Argentina–Chile 2015–2024. | `2_construccion_ISDM.html` |
| `2_EDA_indicadores.Rmd` | Análisis exploratorio de los indicadores antes de construir el índice. | *(sin .html en raíz)* |
| `2_ISDM_jerarquico_migrantes.Rmd` | ISDM con agregación jerárquica por dimensiones. Compara **todos los migrantes** vs. nativos en Argentina y Chile. | `2_ISDM_jerarquico_migrantes.html` |
| `2_ISDM_jerarquico_migrantes_recientes.Rmd` | ISDM con agregación jerárquica por dimensiones. Compara **migrantes recientes** (llegados en los últimos 5 años) vs. nativos. | `2_ISDM_jerarquico_migrantes_recientes.html` |

### Otros archivos

| Archivo | Descripción |
|---|---|
| `Migmobs_nat.Rproj` | Archivo de proyecto de RStudio. |
| `1_contexto_normativo_files/` | Archivos de soporte generados automáticamente por el reporte `1_contexto_normativo.Rmd`. |

---

## Carpetas de datos

### `Data/`

Contiene los datos procesados y construidos a lo largo del flujo de trabajo:

| Archivo | Descripción |
|---|---|
| `Casen_unificado_2015_2024_proc.csv` | Base CASEN procesada, ondas 2015–2024. |
| `EPH_indiv_2016_2024_proc.csv` | Base EPH individual procesada, 2016–2024. |
| `EPH_hogar_2016_2024_proc.csv` | Base EPH hogar procesada, 2016–2024. |
| `indicadores_unificados_arg_chile_2015_2024.csv` | Indicadores socioeconómicos unificados Argentina–Chile. |
| `indicadores_con_brechas_arg_chile_2015_2024.csv` | Indicadores con brechas migrante/nativo calculadas. |
| `indicadores_con_brechas_CASEN_arg_chile_2015_2024.csv` | Brechas calculadas a partir de datos CASEN específicamente. |
| `Indicadores_Casen_2015_2024.csv` | Indicadores CASEN por año. |
| `indicadores_eph_2016_2024.csv` | Indicadores EPH por trimestre/año. |
| `calidad_indicadores.csv` / `.rds` | Métricas de calidad estadística de los indicadores. |
| `indicadores_ENE_CH.xlsx` | Indicadores del mercado laboral chileno (ENE). |
| `relevamiento_normativo_migraciones_ARG_CHL.csv` | Relevamiento de eventos normativos en materia migratoria. |
| `migration_imputed_RIKS_dec2021.csv` | Stock migratorio estimado (fuente externa RIKS/UNU). |
| `UNU_CRIS_adjusted.csv` | Datos ajustados de migración (UNU-CRIS). |
| `v2/` | Versión 2 de datos intermedios procesados. |

---

## Carpetas de salida gráfica

| Carpeta | Contenido |
|---|---|
| `output/` | Gráficos de contexto macroeconómico (PIB, Gini, analfabetismo) y normativo (timeline). |
| `output_context/` | Versión final de gráficos para la sección de contexto, incluyendo stock migratorio. |
| `output_graficos/` | Figuras del ISDM para **todos los migrantes** (brechas por dimensión, evolución temporal, comparación entre países). |
| `output_graficos_recientes/` | Figuras del ISDM para **migrantes recientes** (mismas figuras, subgrupo reciente). |
| `graficos/` | Gráficos individuales de brechas por indicador (series de tiempo, distribuciones estandarizadas). |

---

## Carpetas de documentación e histórico

| Carpeta | Contenido |
|---|---|
| `Documentacion/` | Documentación metodológica de las fuentes de datos: manual de conceptos EPH y diseño de registro EPH. |
| `FMR_Latam/` | Documento de trabajo del proyecto FMR Latam (Free Movement/Migration Report). |
| `v1_nat/` | Versión 1 del proyecto: scripts exploratorios iniciales, análisis temáticos (educación, pobreza, trabajo) y versiones preliminares del índice. Conservado como archivo histórico. |
| `V2_nat/` | Versión 2 del proyecto: análisis de indicadores y primeras versiones del ISDM jerárquico. Conservado como archivo histórico. |
| `backup/` | Respaldo de versiones anteriores de scripts. |
| `_OLD JOACO/` | Scripts previos de una etapa anterior del proyecto. |

---

## Flujo de trabajo

```
Datos crudos (CASEN / EPH)
        ↓
1_procesamiento_*.R       ← Limpieza y homogeneización
        ↓
1_indicadores_*.R         ← Cálculo de indicadores por país
        ↓
1_indicadores_unificados.R ← Unificación Argentina–Chile
        ↓
1_calidad_indicadores.R   ← Evaluación de calidad
        ↓
2_construccion_ISDM.Rmd   ← Construcción del índice
        ↓
2_ISDM_jerarquico_*.Rmd   ← Análisis final con agregación jerárquica
```

---

## Fuentes de datos principales

- **CASEN** — Encuesta de Caracterización Socioeconómica Nacional (Chile)
- **EPH** — Encuesta Permanente de Hogares, INDEC (Argentina)
- **ENE** — Encuesta Nacional de Empleo (Chile)
- **UNU-RIKS / UNU-CRIS** — Estimaciones de stock migratorio internacional
