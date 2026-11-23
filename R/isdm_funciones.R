# =============================================================================
# Funciones comunes del ISDM (las usan 07_ISDM.R y 09_graficos_articulo.R)
# =============================================================================
# - clasificacion_isdm(): indicadores, dimensión y valencia, según la
#   especificación de salud (cobertura, publico, publico_invertido, sin_salud).
# - cargar_brechas(): brechas de migrantes totales (CSV con ENE) y recientes (CSV
#   solo CASEN), apiladas sin perder filas (P6).
# - preparar_isdm(): aplica la valencia y estandariza. Por defecto, d = brecha /
#   sqrt(p(1-p)), con p el promedio de las tasas de los dos grupos (P1, P15).
#   escala = "de_brechas" y centrar = TRUE reproducen las escalas anteriores, solo
#   para documentar el cambio.
# - calcular_isdm(): subíndices por dimensión, ISDM anual y promedio del período.
#
# Detalle de cada decisión: ISDM_metodologia_trazabilidad.docx (carpeta del artículo,
# VERSION_FINAL), códigos de problema P1 a P22.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

# -----------------------------------------------------------------------------
# Clasificación de indicadores según la especificación de salud
# -----------------------------------------------------------------------------
# salud = "cobertura"         -> I9_cobertura_salud (Chile; Argentina sin indicador). Principal.
#         "publico"           -> I9_sistema_salud_publico, valencia positiva (versión original).
#         "publico_invertido" -> I9_sistema_salud_publico, valencia negativa (cota).
#         "sin_salud"         -> sin indicador de salud.
clasificacion_isdm <- function(salud = "cobertura") {
  base <- tribble(
    ~indicador,                     ~dimension, ~tipo,
    "I1_tasa_actividad",            "Labor",    "positivo",
    "I2_tasa_desempleo",            "Labor",    "negativo",
    "I3_informalidad_asalariada",   "Labor",    "negativo",
    "I4_cuentapropismo",            "Labor",    "negativo",
    "I5_pobreza",                   "Poverty",  "negativo",
    "I6_pobreza_extrema",           "Poverty",  "negativo",
    "I7_asistencia_escolar_6_17",   "Social",   "positivo",
    "I8_asistencia_superior_18_29", "Social",   "positivo"
  )
  salud_fila <- switch(
    salud,
    cobertura         = tibble(indicador = "I9_cobertura_salud",       dimension = "Social", tipo = "positivo"),
    publico           = tibble(indicador = "I9_sistema_salud_publico", dimension = "Social", tipo = "positivo"),
    publico_invertido = tibble(indicador = "I9_sistema_salud_publico", dimension = "Social", tipo = "negativo"),
    sin_salud         = NULL,
    stop("Especificación de salud desconocida: ", salud)
  )
  bind_rows(base, salud_fila)
}

# -----------------------------------------------------------------------------
# Datos largos: una fila por país x año x indicador x grupo migrante
# -----------------------------------------------------------------------------
# Totales: brecha_nat_mig del CSV con ENE. Recientes: brecha_nat_rec del CSV
# solo CASEN. Se apilan por separado (no con left_join), para no perder las
# filas de recientes que no existen en el CSV con ENE (problema P6).
cargar_brechas <- function(
    archivo_ene = "Data/indicadores_con_brechas_arg_chile_2015_2024.csv",
    archivo_sc  = "Data/indicadores_con_brechas_solocasen_arg_chile_2015_2024.csv") {
  tot <- read_csv(archivo_ene, show_col_types = FALSE) %>%
    transmute(pais, fuente, anio, indicador, grupo = "Migrants",
              tasa_no_migrante = nativos, tasa_grupo = migrantes, brecha = brecha_nat_mig)
  rec <- read_csv(archivo_sc, show_col_types = FALSE) %>%
    transmute(pais, fuente, anio, indicador, grupo = "Recent migrants",
              tasa_no_migrante = nativos, tasa_grupo = migrantes_recientes, brecha = brecha_nat_rec)
  bind_rows(tot, rec) %>% filter(!is.na(brecha))
}

# -----------------------------------------------------------------------------
# Normalización direccional y estandarización
# -----------------------------------------------------------------------------
# brecha_normalizada > 0  -> mejor la población no migrante
# brecha_normalizada < 0  -> mejor la población migrante
#
# escala = "dispersion" (PRINCIPAL, decisión del 22/11/2026):
#   cada brecha se divide por la dispersión individual del indicador,
#   sqrt(p * (1 - p)), con p = promedio de las tasas de los dos grupos
#   comparados en ese país y año. Es una diferencia de proporciones
#   estandarizada (tipo d de Cohen). El cero es la paridad y la escala no
#   depende de cuántos países o años entren en el cálculo, por lo que funciona
#   también para indicadores observados en un solo país (cobertura de salud).
# escala = "de_brechas": brecha / DE de las brechas del indicador (por grupo,
#   ambos países, todos los años). Con centrar = TRUE se reproduce el cálculo
#   original (brecha - media) / DE, solo para documentar la diferencia.
preparar_isdm <- function(datos, salud = "cobertura", escala = "dispersion",
                          centrar = FALSE, excluir_anios = 2020) {
  clasif <- clasificacion_isdm(salud)
  d <- datos %>%
    filter(!anio %in% excluir_anios) %>%
    inner_join(clasif, by = "indicador") %>%
    mutate(brecha_normalizada = if_else(tipo == "negativo", -brecha, brecha))
  if (escala == "dispersion") {
    d %>%
      mutate(
        p_media   = ((tasa_no_migrante + tasa_grupo) / 2) / 100,
        disp_ind  = sqrt(p_media * (1 - p_media)) * 100,
        brecha_z  = brecha_normalizada / disp_ind
      )
  } else if (escala == "de_brechas") {
    d %>%
      group_by(indicador, grupo) %>%
      mutate(
        media_ind = mean(brecha_normalizada, na.rm = TRUE),
        de_ind    = sd(brecha_normalizada,   na.rm = TRUE),
        brecha_z  = if (centrar) (brecha_normalizada - media_ind) / de_ind
                    else brecha_normalizada / de_ind
      ) %>%
      ungroup()
  } else {
    stop("Escala desconocida: ", escala)
  }
}

# -----------------------------------------------------------------------------
# Agregación jerárquica: indicadores -> dimensión -> ISDM
# -----------------------------------------------------------------------------
calcular_isdm <- function(datos_prep) {
  sub <- datos_prep %>%
    group_by(pais, anio, grupo, dimension) %>%
    summarise(
      sub_abs_pp = mean(abs(brecha_normalizada), na.rm = TRUE),
      sub_dir_pp = mean(brecha_normalizada,      na.rm = TRUE),
      sub_abs_de = mean(abs(brecha_z),           na.rm = TRUE),   # unidades estandarizadas (d)
      sub_dir_de = mean(brecha_z,                na.rm = TRUE),
      n_ind      = n(),
      .groups = "drop"
    )
  anual <- sub %>%
    group_by(pais, anio, grupo) %>%
    summarise(
      isdm_abs_pp = mean(sub_abs_pp),
      isdm_dir_pp = mean(sub_dir_pp),
      isdm_abs_de = mean(sub_abs_de),
      isdm_dir_de = mean(sub_dir_de),
      n_dim       = n(),
      .groups = "drop"
    )
  resumen <- anual %>%
    group_by(pais, grupo) %>%
    summarise(across(starts_with("isdm_"), ~ mean(.x, na.rm = TRUE)),
              n_anios = n(), .groups = "drop")
  sub_resumen <- sub %>%
    group_by(pais, grupo, dimension) %>%
    summarise(across(starts_with("sub_"), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  list(subindices = sub, anual = anual, resumen = resumen, subindices_resumen = sub_resumen)
}
