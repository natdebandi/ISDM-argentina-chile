# =============================================================================
# DIAGNÓSTICO DE CALIDAD (N, N_EFF, IC95%) PARA INDICADORES MIGMOBS NAT
# =============================================================================
# Propósito:
#   - Recalcular, en un script separado, el diagnóstico de precisión de cada indicador
#   - Mantener intacto el pipeline existente de construcción de "indicadores_*"
#   - Generar un archivo "calidad_indicadores.csv" con:
#       indicador, pais, fuente, anio, grupo_migratorio,
#       n_total, n_con_peso, n_eff_kish,
#       porcentaje, porcentaje_lwr, porcentaje_upr
#
# Basado en los scripts de cálculo de indicadores:
#   - indicadores_EPH_2201_v2.R (Argentina / EPH)
#   - indicadores_casen_2201_v2.R (Chile / CASEN)
# =============================================================================

rm(list = ls())

library(tidyverse)
library(readr)

# -----------------------------
# Helpers
# -----------------------------

kish_neff <- function(w) {
  w <- w[!is.na(w) & w > 0]
  if (length(w) == 0) return(NA_real_)
  (sum(w)^2) / sum(w^2)
}

make_long_3grupos <- function(df, pais, fuente) {
  bind_rows(
    df %>% filter(.grupo_nat) %>% mutate(grupo_migratorio = "nativos"),
    df %>% filter(.grupo_mig) %>% mutate(grupo_migratorio = "migrantes"),
    df %>% filter(.grupo_rec) %>% mutate(grupo_migratorio = "migrantes_recientes")
  ) %>%
    select(-starts_with(".grupo_"))
}


make_grupo_casen <- function(migrante, migrante_reciente) {
  dplyr::case_when(
    migrante == "Nativos" ~ "nativos",
    migrante == "Migrantes" & migrante_reciente == "Si" ~ "migrantes_recientes",
    migrante == "Migrantes" ~ "migrantes",
    TRUE ~ NA_character_
  )
}

# Calcula proporción ponderada + IC95% usando aproximación normal con n_eff (Kish)
# Nota: es un diagnóstico de precisión (útil para submuestras chicas).
calc_calidad_prop <- function(df,
                              indicador_id,
                              pais,
                              fuente,
                              weight_var,
                              y_var,
                              universo_label,
                              year_var = "anio",
                              group_var = "grupo_migratorio") {

  df %>%
    filter(!is.na(.data[[group_var]])) %>%
    group_by(.data[[year_var]], .data[[group_var]]) %>%
    summarise(
      n_total = n(),
      n_con_peso = sum(!is.na(.data[[weight_var]]) & .data[[weight_var]] > 0),
      w_den = sum(.data[[weight_var]][!is.na(.data[[y_var]]) & .data[[weight_var]] > 0], na.rm = TRUE),
      w_num = sum(.data[[weight_var]][!is.na(.data[[y_var]]) & .data[[weight_var]] > 0] *
                    .data[[y_var]][!is.na(.data[[y_var]]) & .data[[weight_var]] > 0], na.rm = TRUE),
      p = w_num / w_den,
      n_eff_kish = kish_neff(.data[[weight_var]][!is.na(.data[[y_var]]) & .data[[weight_var]] > 0]),
      .groups = "drop"
    ) %>%
    mutate(
      se = sqrt(p * (1 - p) / n_eff_kish),
      lwr = pmax(p - 1.96 * se, 0),
      upr = pmin(p + 1.96 * se, 1),
      porcentaje = p * 100,
      porcentaje_lwr = lwr * 100,
      porcentaje_upr = upr * 100,
      anio = as.numeric(.data[[year_var]]),
      indicador = indicador_id,
      pais = pais,
      fuente = fuente,
      universo = universo_label
    ) %>%
    select(indicador, pais, fuente, universo, anio,
           grupo_migratorio = .data[[group_var]],
           n_total, n_con_peso, n_eff_kish,
           porcentaje, porcentaje_lwr, porcentaje_upr)
}

# -----------------------------
# 1) Cargar microdatos procesados (mismos insumos que tus scripts)
# -----------------------------
# Ajustá rutas si es necesario
EPH_indiv_unificado <- read_csv("Data/EPH_indiv_2016_2024_proc.csv", show_col_types = FALSE)
casen <- read_csv("Data/Casen_unificado_2015_2024_proc.csv", show_col_types = FALSE)

# -----------------------------
# 2) Calcular calidad por indicador (EPH / CASEN)
# -----------------------------
calidad <- tibble()

# =========================
# EPH (Argentina) - ponderación: "PONDERA"
# =========================

# I1 Actividad
calidad <- bind_rows(
  calidad,
  EPH_indiv_unificado %>%
    filter(!is.na(estado_actividad), !is.na(migrante), !is.na(ponderacion)) %>%
    mutate(
      .grupo_nat = migrante == 0,
      .grupo_mig = migrante == 1,  # TOTAL
      .grupo_rec = migrante == 1 & migrante_reciente == "Si",
      y = ifelse(estado_actividad %in% c("Ocupado", "Desocupado"), 1, 0)
    ) %>%
    make_long_3grupos() %>%
    calc_calidad_prop("I1_tasa_actividad", "Argentina", "EPH",
                      weight_var = "ponderacion", y_var = "y",
                      universo_label = "PET (estado_actividad no NA)",
                      group_var = "grupo_migratorio")
)

# I2 Desempleo (PEA)
calidad <- bind_rows(
  calidad,
  EPH_indiv_unificado %>%
    filter(estado_actividad %in% c("Ocupado", "Desocupado") & !is.na(migrante)) %>%
    mutate(
      .grupo_nat = migrante == 0,
      .grupo_mig = migrante == 1,  # TOTAL
      .grupo_rec = migrante == 1 & migrante_reciente == "Si",
      y = ifelse(estado_actividad == "Desocupado", 1, 0)
    ) %>%
    make_long_3grupos() %>%
    calc_calidad_prop(
      "I2_tasa_desempleo", "Argentina", "EPH",
      weight_var = "PONDERA", y_var = "y",
      universo_label = "PEA (ocupado/desocupado)",
      group_var = "grupo_migratorio"
    )
)

# I3 Informalidad asalariada
calidad <- bind_rows(
  calidad,
  EPH_indiv_unificado %>%
    filter(!is.na(formalidad_empleo_asalariada) & !is.na(migrante)) %>%
    mutate(
        .grupo_nat = migrante == 0,
        .grupo_mig = migrante == 1,  # TOTAL
        .grupo_rec = migrante == 1 & migrante_reciente == "Si",
        y = ifelse(formalidad_empleo_asalariada == "Asalariado informal", 1, 0)
    ) %>%
    make_long_3grupos() %>%
    calc_calidad_prop("I3_informalidad_asalariada", "Argentina", "EPH",
                      weight_var = "PONDERA", y_var = "y",
                      universo_label = "Asalariados (formalidad_empleo_asalariada no NA)",
                      group_var = "grupo_migratorio"
    )
)

# I4 Informalidad total
calidad <- bind_rows(
  calidad,
  EPH_indiv_unificado %>%
    filter(!is.na(formalidad_empleo_total) & !is.na(migrante)) %>%
    mutate(
      .grupo_nat = migrante == 0,
      .grupo_mig = migrante == 1,  # TOTAL
      .grupo_rec = migrante == 1 & migrante_reciente == "Si",
      y = ifelse(formalidad_empleo_total == "Informal", 1, 0)
    ) %>%
    make_long_3grupos() %>%
    calc_calidad_prop("I4_informalidad_total", "Argentina", "EPH",
                      weight_var = "PONDERA", y_var = "y",
                      universo_label = "Ocupados (formalidad_empleo_total no NA)",
                      group_var = "grupo_migratorio")
)

# I5 Cuentapropismo
calidad <- bind_rows(
  calidad,
  EPH_indiv_unificado %>%
    filter(!is.na(categoria_informalidad) & !is.na(migrante)) %>%
    mutate(
      .grupo_nat = migrante == 0,
      .grupo_mig = migrante == 1,  # TOTAL
      .grupo_rec = migrante == 1 & migrante_reciente == "Si",
      y = ifelse(categoria_informalidad == "Cuentapropista", 1, 0)
    ) %>%
    make_long_3grupos() %>%
    calc_calidad_prop("I5_cuentapropismo", "Argentina", "EPH",
                      weight_var = "PONDERA", y_var = "y",
                      universo_label = "Ocupados (categoria_informalidad no NA)",
                      group_var = "grupo_migratorio")
)

# I6 Pobreza
calidad <- bind_rows(
  calidad,
  EPH_indiv_unificado %>%
    filter(!is.na(situacion) & !is.na(migrante)) %>%
    mutate(
      .grupo_nat = migrante == 0,
      .grupo_mig = migrante == 1,  # TOTAL
      .grupo_rec = migrante == 1 & migrante_reciente == "Si",
      y = ifelse(situacion %in% c("pobre", "indigente"), 1, 0)
    ) %>%
    make_long_3grupos() %>%
    calc_calidad_prop("I6_pobreza", "Argentina", "EPH",
                      weight_var = "PONDERA", y_var = "y",
                      universo_label = "Total (situacion no NA)",
                      group_var = "grupo_migratorio")
)
# I7 Pobreza extrema
calidad <- bind_rows(
  calidad,
  EPH_indiv_unificado %>%
    filter(!is.na(situacion) & !is.na(migrante)) %>%
    mutate(
      .grupo_nat = migrante == 0,
      .grupo_mig = migrante == 1,  # TOTAL
      .grupo_rec = migrante == 1 & migrante_reciente == "Si",
      y = ifelse(situacion == "indigente", 1, 0)
    ) %>%
    make_long_3grupos() %>%
    calc_calidad_prop("I7_pobreza_extrema", "Argentina", "EPH",
                      weight_var = "PONDERA", y_var = "y",
                      universo_label = "Total (situacion no NA)",
                      group_var = "grupo_migratorio")
)


# I8 Asistencia escolar 6-17
calidad <- bind_rows(
  calidad,
  EPH_indiv_unificado %>%
    filter(edad >= 6 & edad <= 17 & !is.na(migrante) & !is.na(asistencia_educacion)) %>%
    mutate(
      .grupo_nat = migrante == 0,
      .grupo_mig = migrante == 1,  # TOTAL
      .grupo_rec = migrante == 1 & migrante_reciente == "Si",
      y = ifelse(asistencia_educacion == "Asiste actualmente", 1, 0)
    ) %>%
    make_long_3grupos() %>%
    calc_calidad_prop("I8_asistencia_escolar_6_17", "Argentina", "EPH",
                      weight_var = "PONDERA", y_var = "y",
                      universo_label = "Edad 6-17 (asistencia_educacion no NA)",
                      group_var = "grupo_migratorio")
)

# I9 Asistencia superior 18-29
calidad <- bind_rows(
  calidad,
  EPH_indiv_unificado %>%
    filter(edad >= 18 & edad <= 29 & !is.na(migrante)) %>%
    mutate(
      .grupo_nat = migrante == 0,
      .grupo_mig = migrante == 1,  # TOTAL
      .grupo_rec = migrante == 1 & migrante_reciente == "Si",
      y = ifelse(asistencia_educacion == "Asiste actualmente" &
                   nivel_educativo_asiste %in% c("Terciario", "Universitario", "Postgrado"),
                 1, 0)
    ) %>%
    make_long_3grupos() %>%
    calc_calidad_prop("I9_asistencia_superior_18_29", "Argentina", "EPH",
                      weight_var = "PONDERA", y_var = "y",
                      universo_label = "Edad 18-29 (asistencia actual + nivel superior)",
                      group_var = "grupo_migratorio")
)

# I10 Sistema público
calidad <- bind_rows(
  calidad,
  EPH_indiv_unificado %>%
    filter(!is.na(usa_sistema_publico) & !is.na(migrante)) %>%
    mutate(
      .grupo_nat = migrante == 0,
      .grupo_mig = migrante == 1,  # TOTAL
      .grupo_rec = migrante == 1 & migrante_reciente == "Si",
      y = ifelse(usa_sistema_publico == 1, 1, 0)
    ) %>%
    make_long_3grupos() %>%
    calc_calidad_prop("I10_sistema_salud_publico", "Argentina", "EPH",
                      weight_var = "PONDERA", y_var = "y",
                      universo_label = "Total (usa_sistema_publico no NA)",
                      group_var = "grupo_migratorio")
)

# =========================
# CASEN (Chile) - ponderación: "expr"
# =========================

# I1 Actividad (edad >=15)
calidad <- bind_rows(
  calidad,
  casen %>%
    filter(edad >= 15, !is.na(migrante), !is.na(expr)) %>%
    mutate(
      # flags de pertenencia a grupos (NO excluyentes)
      .grupo_nat = migrante == "Nativos",
      .grupo_mig = migrante == "Migrantes",  # TOTAL
      .grupo_rec = migrante == "Migrantes" & migrante_reciente == "Si",
      y = ifelse(estado_actividad %in% c("Ocupado", "Desocupado"), 1, 0)
    ) %>%
    make_long_3grupos() %>%
    calc_calidad_prop("I1_tasa_actividad", "Chile", "CASEN",
                      weight_var = "expr", y_var = "y",
                      universo_label = "Edad >=15",
                      group_var = "grupo_migratorio")
)

# I2 Desempleo (PEA)
calidad <- bind_rows(
  calidad,
  casen %>%
    filter(estado_actividad %in% c("Ocupado", "Desocupado") & !is.na(migrante)) %>%
    mutate(
      .grupo_nat = migrante == "Nativos",
      .grupo_mig = migrante == "Migrantes",  # TOTAL
      .grupo_rec = migrante == "Migrantes" & migrante_reciente == "Si",     
      y = ifelse(estado_actividad == "Desocupado", 1, 0)
    ) %>%
    make_long_3grupos() %>%
    calc_calidad_prop("I2_tasa_desempleo", "Chile", "CASEN",
                      weight_var = "expr", y_var = "y",
                      universo_label = "PEA (ocupado/desocupado)",
                      group_var = "grupo_migratorio")
)

# I3 Informalidad asalariada
calidad <- bind_rows(
  calidad,
  casen %>%
    filter(!is.na(formalidad_empleo_asalariada) & !is.na(migrante)) %>%
    mutate(
      .grupo_nat = migrante == "Nativos",
      .grupo_mig = migrante == "Migrantes",  # TOTAL
      .grupo_rec = migrante == "Migrantes" & migrante_reciente == "Si",     
      y = ifelse(formalidad_empleo_asalariada == "Asalariado informal", 1, 0)
    ) %>%
    make_long_3grupos() %>%
    calc_calidad_prop("I3_informalidad_asalariada", "Chile", "CASEN",
                      weight_var = "expr", y_var = "y",
                      universo_label = "Asalariados (formalidad_empleo_asalariada no NA)",
                      group_var = "grupo_migratorio")
)

# I4 Informalidad total
calidad <- bind_rows(
  calidad,
  casen %>%
    filter(!is.na(formalidad_empleo_total) & !is.na(migrante)) %>%
    mutate(
      .grupo_nat = migrante == "Nativos",
      .grupo_mig = migrante == "Migrantes",  # TOTAL
      .grupo_rec = migrante == "Migrantes" & migrante_reciente == "Si",     
      y = ifelse(formalidad_empleo_total == "Informal", 1, 0)
    ) %>%
    make_long_3grupos() %>%
    calc_calidad_prop("I4_informalidad_total", "Chile", "CASEN",
                      weight_var = "expr", y_var = "y",
                      universo_label = "Ocupados (formalidad_empleo_total no NA)",
                      group_var = "grupo_migratorio")
)

# I5 Cuentapropismo
calidad <- bind_rows(
  calidad,
  casen %>%
    filter(!is.na(categoria_informalidad) & !is.na(migrante)) %>%
    mutate(
      .grupo_nat = migrante == "Nativos",
      .grupo_mig = migrante == "Migrantes",  # TOTAL
      .grupo_rec = migrante == "Migrantes" & migrante_reciente == "Si",     
      y = ifelse(categoria_informalidad == "Cuentapropista", 1, 0)
    ) %>%
    make_long_3grupos() %>%
    calc_calidad_prop("I5_cuentapropismo", "Chile", "CASEN",
                      weight_var = "expr", y_var = "y",
                      universo_label = "Ocupados (categoria_informalidad no NA)",
                      group_var = "grupo_migratorio")
)

# I6 Pobreza
calidad <- bind_rows(
  calidad,
  casen %>%
    filter(!is.na(pobreza) & !is.na(migrante)) %>%
    mutate(
      .grupo_nat = migrante == "Nativos",
      .grupo_mig = migrante == "Migrantes",  # TOTAL
      .grupo_rec = migrante == "Migrantes" & migrante_reciente == "Si",     
      y = ifelse(pobreza %in% c("Pobres", "Pobres extremos"), 1, 0)
    ) %>%
    make_long_3grupos() %>%
    calc_calidad_prop("I6_pobreza", "Chile", "CASEN",
                      weight_var = "expr", y_var = "y",
                      universo_label = "Total (pobreza no NA)",
                      group_var = "grupo_migratorio")
)

# I7 Pobreza extrema
calidad <- bind_rows(
  calidad,
  casen %>%
    filter(!is.na(pobreza) & !is.na(migrante)) %>%
    mutate(
      .grupo_nat = migrante == "Nativos",
      .grupo_mig = migrante == "Migrantes",  # TOTAL
      .grupo_rec = migrante == "Migrantes" & migrante_reciente == "Si",     
      y = ifelse(pobreza == "Pobres extremos", 1, 0)
    ) %>%
    make_long_3grupos() %>%
    calc_calidad_prop("I7_pobreza_extrema", "Chile", "CASEN",
                      weight_var = "expr", y_var = "y",
                      universo_label = "Total (pobreza no NA)",
                      group_var = "grupo_migratorio")
)

# I8 Asistencia escolar 6-17
calidad <- bind_rows(
  calidad,
  casen %>%
    filter(edad >= 6 & edad <= 17 & !is.na(migrante) & !is.na(asiste)) %>%
    mutate(
      .grupo_nat = migrante == "Nativos",
      .grupo_mig = migrante == "Migrantes",  # TOTAL
      .grupo_rec = migrante == "Migrantes" & migrante_reciente == "Si",     
      y = ifelse(asiste == "Asiste", 1, 0)
    ) %>%
    make_long_3grupos() %>%
    calc_calidad_prop("I8_asistencia_escolar_6_17", "Chile", "CASEN",
                      weight_var = "expr", y_var = "y",
                      universo_label = "Edad 6-17 (asiste no NA)",
                      group_var = "grupo_migratorio")
)

# I9 Asistencia superior 18-29
calidad <- bind_rows(
  calidad,
  casen %>%
    filter(edad >= 18 & edad <= 29 & !is.na(migrante)) %>%
    mutate(
      .grupo_nat = migrante == "Nativos",
      .grupo_mig = migrante == "Migrantes",  # TOTAL
      .grupo_rec = migrante == "Migrantes" & migrante_reciente == "Si",     
      y = ifelse(asiste == "Asiste" & nivel_asiste %in% c(12, 13, 14, 15, 16, 17), 1, 0)
    ) %>%
    make_long_3grupos() %>%
    calc_calidad_prop("I9_asistencia_superior_18_29", "Chile", "CASEN",
                      weight_var = "expr", y_var = "y",
                      universo_label = "Edad 18-29 (asiste + nivel_asiste superior)",
                      group_var = "grupo_migratorio")
)

# I10 Sistema público
calidad <- bind_rows(
  calidad,
  casen %>%
    filter(!is.na(sistema_salud) & !is.na(migrante)) %>%
    mutate(
      .grupo_nat = migrante == "Nativos",
      .grupo_mig = migrante == "Migrantes",  # TOTAL
      .grupo_rec = migrante == "Migrantes" & migrante_reciente == "Si",     
      y = ifelse(sistema_salud == "Accede a salud pública gratuita", 1, 0)
    ) %>%
    make_long_3grupos() %>%
    calc_calidad_prop("I10_sistema_salud_publico", "Chile", "CASEN",
                      weight_var = "expr", y_var = "y",
                      universo_label = "Total (sistema_salud no NA)",
                      group_var = "grupo_migratorio")
)

# -----------------------------
# 3) Guardar outputs
# -----------------------------
#dir.create("Data", showWarnings = FALSE)

write_csv(calidad, "Data/calidad_indicadores.csv")
saveRDS(calidad, "Data/calidad_indicadores.rds")

message("✓ Diagnóstico de calidad guardado:")
message("  - Data/calidad_indicadores.csv")
message("  - Data/calidad_indicadores.rds")
message("Total filas: ", nrow(calidad))



calidad_eval <- calidad %>%
  mutate(
    calidad_muestra = case_when(
      n_con_peso >= 100 & n_eff_kish >= 50 ~ "OK",
      n_con_peso >= 30  & n_eff_kish >= 30 ~ "CUIDADO",
      TRUE                                ~ "NO USAR"
    )
  )


diagnostico <- calidad_eval %>%
  select(pais, fuente, indicador, anio, grupo_migratorio,
         n_con_peso, n_eff_kish, calidad_muestra) %>%
  arrange(pais, indicador, anio, grupo_migratorio)

print(diagnostico, n = 200)

atencion<-diagnostico %>%
  filter(calidad_muestra != "OK")


print(atencion)

calidad_eval %>%
  select(pais, fuente, indicador, anio, grupo_migratorio, n_con_peso) %>%
  pivot_wider(names_from = grupo_migratorio, values_from = n_con_peso) %>%
  mutate(ok = migrantes_recientes <= migrantes) %>%
  filter(!ok)
