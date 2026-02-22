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
ISDM-argentina-chile/
├── Scripts de procesamiento de datos (1_procesamiento_*.R)
├── Scripts de construcción de indicadores (1_indicadores_*.R)
├── Reportes de análisis (.Rmd / .html)
├── README.md
└── Migmobs_nat.Rproj
```

> **Nota:** Los datos, gráficos y carpetas de salida no se incluyen en este repositorio (ver sección [Datos](#datos)).

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

---

## Datos

> ⚠️ **Los datasets no están disponibles en este repositorio** por su tamaño y por las condiciones de uso de las fuentes originales.

Los datos pueden **reconstruirse íntegramente** ejecutando los scripts de procesamiento sobre los microdatos originales. A continuación se indican las fuentes y dónde descargarlos:

| Dataset | Fuente | Descarga |
|---|---|---|
| EPH — Encuesta Permanente de Hogares (Argentina, 2016–2024) | INDEC | [indec.gob.ar](https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos) |
| CASEN — Encuesta de Caracterización Socioeconómica Nacional (Chile, 2015–2024) | Ministerio de Desarrollo Social de Chile | [observatorio.ministeriodesarrollosocial.gob.cl](https://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen) |
| Estimaciones de stock migratorio bilateral | UNU-CRIS / RIKS | [UNU-CRIS](https://cris.unu.edu) |

Los datos procesados (bases homogeneizadas e indicadores construidos) pueden **solicitarse directamente** a la autora escribiendo a: **nataliadebandi@gmail.com**

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
- **UNU-RIKS / UNU-CRIS** — Estimaciones de stock migratorio bilateral internacional
