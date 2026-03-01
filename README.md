# Migmobs Nat — Índice Sintético de Desigualdad Migratoria (ISDM)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18734971.svg)](https://doi.org/10.5281/zenodo.18734971)
**Autora:** Natalia Debandi
**Proyecto:** MIGMOBS Latam
**Período de análisis:** 2015–2024
**Países:** Argentina y Chile

---

## Descripción general

Este repositorio contiene los scripts, reportes y datos procesados para la construcción del **Índice Sintético de Desigualdad Migratoria (ISDM)**: un índice comparativo que cuantifica las brechas socioeconómicas entre personas migrantes y nativas en Argentina y Chile a lo largo del tiempo. El ISDM agrega indicadores de mercado laboral, pobreza, educación y salud en un único valor que permite comparar la integración migratoria entre países y períodos.

---

## Estructura del repositorio

```
Migmobs_nat/
├── 0_*.Rmd              ← Reportes de contexto
├── 1_1_*.R              ← Procesamiento de microdatos
├── 1_2_*.R              ← Construcción de indicadores
├── 1_3_*.R / 1_4_*.R    ← Calidad e integración
├── 1_5_*.Rmd            ← EDA de indicadores
├── 1_6_*.Rmd            ← Construcción de brechas e interpolación
├── 2_*.Rmd              ← Análisis ISDM
├── README.md
└── Migmobs_nat.Rproj
```

> **Nota:** Los datos, gráficos y carpetas de salida no se incluyen en este repositorio (ver sección [Datos](#datos)).

---

## Archivos en la raíz

### Scripts de procesamiento (prefijo `1_1_`)

| Archivo | Descripción |
|---|---|
| `1_1_procesamiento_casen.R` | Procesa y homogeneiza los datos de la Encuesta CASEN (Chile, ondas 2015, 2017, **2020**, 2022 y 2024). Incorpora el año 2020 con mapeo específico de variables (`lugar_nac`, `r2`, `o32`, etc.). Genera variables comparables con la EPH argentina. |
| `1_1_procesamiento_EPH.R` | Procesa y homogeneiza los datos de la Encuesta Permanente de Hogares —EPH— (Argentina, trimestres 2016–2024). |

### Scripts de indicadores y calidad (prefijos `1_2_` a `1_4_`)

| Archivo | Descripción |
|---|---|
| `1_2_indicadores_casen.R` | Calcula indicadores socioeconómicos a partir de los datos procesados de CASEN. |
| `1_2_indicadores_EPH.R` | Calcula indicadores socioeconómicos a partir de los datos procesados de EPH. |
| `1_3_calidad_indicadores.R` | Evalúa la calidad estadística de los indicadores calculados (tamaños muestrales, coeficientes de variación, tamaño muestral efectivo Kish). |
| `1_4_indicadores_unificados.R` | Unifica los indicadores de Argentina y Chile en una sola base comparativa (`indicadores_unificados_arg_chile_2015_2024.csv`). |

### Reportes de contexto (prefijo `0_`)

| Archivo | Descripción | Salida |
|---|---|---|
| `0_1_contexto_ARG_CHL.Rmd` | Análisis de contexto: migración en Argentina y Chile. Incluye series de tiempo de stocks migratorios, indicadores macroeconómicos (PIB, Gini) y comparaciones estructurales. | `.html` |
| `0_2_contexto_normativo.Rmd` | Análisis del marco normativo migratorio en Argentina y Chile. Incluye timeline de eventos legislativos clave. | `.html` |
| `0_2_migracion_ARG_CHL.Rmd` | Reporte sobre flujos y stocks de migración en ambos países. | `.html` |

### Reportes de análisis de indicadores (prefijos `1_5_` y `1_6_`)

| Archivo | Descripción | Salida |
|---|---|---|
| `1_5_EDA_indicadores.Rmd` | Análisis exploratorio de los indicadores. Incluye diagnóstico de calidad muestral (semáforo OK / CUIDADO / NO USAR), visualización de cobertura y análisis de brechas por indicador. | `.html` |
| `1_6_construccion_brechas.Rmd` | **Paso central de preparación de datos.** A partir de `indicadores_unificados_arg_chile_2015_2024.csv`: (1) unifica fuentes de Chile priorizando ENE cuando está disponible; (2) aplica **interpolación lineal sobre los valores originales** por indicador × grupo para los años inter-CASEN; (3) calcula brechas (nativos − migrantes); (4) genera **dos CSVs**: uno con ENE para indicadores laborales (`indicadores_con_brechas_arg_chile_2015_2024.csv`) y uno usando exclusivamente CASEN para todos los indicadores (`indicadores_con_brechas_solocasen_arg_chile_2015_2024.csv`). | `.html` |

### Reportes ISDM (prefijo `2_`)

| Archivo | Descripción | Salida |
|---|---|---|
| `2_1_ISDM_jerarquico_migrantes.Rmd` | ISDM con agregación jerárquica por dimensiones. Compara **todos los migrantes** vs. nativos. **Excluye 2020** en ambos países. Usa `indicadores_con_brechas_solocasen_arg_chile_2015_2024.csv`. Incluye pruebas de robustez (Shapiro-Wilk, Cohen's d, permutation test). | `.html` |
| `2_2_ISDM_jerarquico_migrantes_con2020.Rmd` | Versión del ISDM jerárquico (todos los migrantes) **incluyendo el año 2020**. Permite evaluar la robustez de los resultados ante la inclusión del año pandémico. | `.html` |
| `2_3_ISDM_jerarquico_migrantes_recientes.Rmd` | ISDM con agregación jerárquica. Compara **migrantes recientes** (llegados en los últimos 5 años) vs. nativos. Usa `indicadores_con_brechas_solocasen_arg_chile_2015_2024.csv` (fuente única CASEN para Chile). Incluye los mismos tests de robustez. | `.html` |

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
| CASEN — Encuesta de Caracterización Socioeconómica Nacional (Chile, 2015–2024, incluye 2020) | Ministerio de Desarrollo Social de Chile | [observatorio.ministeriodesarrollosocial.gob.cl](https://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen) |
| Estimaciones de stock migratorio bilateral | UNU-CRIS / RIKS | [UNU-CRIS](https://cris.unu.edu) |

Los datos procesados (bases homogeneizadas e indicadores construidos) pueden **solicitarse directamente** a la autora escribiendo a: **nataliadebandi@gmail.com**

---

## Flujo de trabajo

```
Microdatos crudos (CASEN 2015/2017/2020/2022/2024 · EPH 2016–2024)
        ↓
1_1_procesamiento_*.R         ← Limpieza, homogeneización y comparabilidad
        ↓
1_2_indicadores_*.R           ← Cálculo de indicadores por país
        ↓
1_3_calidad_indicadores.R     ← Evaluación de calidad muestral
        ↓
1_4_indicadores_unificados.R  ← Unificación Argentina–Chile
        ↓
1_5_EDA_indicadores.Rmd       ← Análisis exploratorio
        ↓
1_6_construccion_brechas.Rmd  ← Interpolación lineal + brechas + CSVs de salida
        ↓
2_*_ISDM_jerarquico_*.Rmd     ← Construcción y análisis del ISDM
```

---

## Decisiones metodológicas clave

### Incorporación de CASEN 2020

La encuesta CASEN 2020 ("en pandemia") utiliza una nomenclatura de variables diferente a otros años. Las equivalencias clave son:

| Variable estándar | CASEN 2020 | Nota |
|---|---|---|
| `r1a` (nacionalidad) | `lugar_nac` | Binaria: 0 = Chile, 1 = extranjero |
| `r1b` (residencia hace 5 años) | `r2` | Valor 4 = otro país (en otros años es 3) |
| `o29` (cotización previsional) | `o32` | Igual que 2022/2024 |
| `s12` (sistema de salud) | `s13` | Codificación propia: 1=FONASA, 2=FF.AA., 3=ISAPRE |
| `educ` | `educc` | Igual que 2024 |

**Decisión sobre FONASA 2020:** la encuesta no desglosa los grupos A/B/C/D de FONASA, por lo que todos los afiliados a FONASA en 2020 se clasifican como "salud pública gratuita".

### Tratamiento del año 2020 en el ISDM

El año 2020 se **excluye** del análisis principal (`2_1_ISDM_jerarquico_migrantes.Rmd`) por las siguientes razones:

- **Argentina (EPH):** la encuesta fue suspendida en el primer semestre y migró a modalidad telefónica en el segundo, generando una discontinuidad metodológica.
- **Chile (CASEN):** la encuesta de 2020 coincide con el período de pandemia, lo que distorsiona los indicadores laborales y de pobreza.

El archivo `2_2_ISDM_jerarquico_migrantes_con2020.Rmd` conserva 2020 como análisis de robustez.

### Interpolación lineal para años inter-CASEN

La CASEN se aplica cada dos o tres años (2015, 2017, 2020, 2022, 2024), generando años sin datos para Chile. Para mantener series temporales comparables con Argentina (EPH anual), se aplica **interpolación lineal** (`zoo::na.approx`) **sobre los valores originales del indicador por grupo migratorio**, antes de calcular brechas. Esto ocurre en `1_6_construccion_brechas.Rmd`.

Las filas interpoladas quedan identificadas con `fuente = "CASEN_interpolado"` en los datasets.

### Dos versiones del dataset de brechas

`1_6_construccion_brechas.Rmd` genera dos CSVs con diferente tratamiento de fuentes para Chile:

| CSV | Fuente laboral Chile | Uso |
|---|---|---|
| `indicadores_con_brechas_arg_chile_2015_2024.csv` | ENE (anual) | Serie laboral más densa |
| `indicadores_con_brechas_solocasen_arg_chile_2015_2024.csv` | CASEN (interpolado) | Fuente única, mayor consistencia interna |

Los reportes ISDM (`2_*`) utilizan la versión solo CASEN para garantizar comparabilidad metodológica entre dimensiones.

---

## Fuentes de datos principales

- **CASEN** — Encuesta de Caracterización Socioeconómica Nacional (Chile)
- **EPH** — Encuesta Permanente de Hogares, INDEC (Argentina)
- **ENE** — Encuesta Nacional de Empleo (Chile)
- **UNU-RIKS / UNU-CRIS** — Estimaciones de stock migratorio bilateral internacional

---

## Cómo citar / How to cite

**APA:**
Debandi, N. (2025). Índice Sintético de Desigualdad Migratoria (ISDM):
Argentina y Chile (v1.0.0). Zenodo. https://doi.org/10.5281/zenodo.18734971

**BibTeX:**
```bibtex
@software{debandi_2025_ISDM,
  author    = {Debandi, Natalia},
  title     = {ISDM-argentina-chile},
  year      = {2025},
  publisher = {Zenodo},
  version   = {v1.0.0},
  doi       = {10.5281/zenodo.18734971},
  url       = {https://doi.org/10.5281/zenodo.18734971}
}
```
