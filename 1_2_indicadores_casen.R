# =============================================================================
# SCRIPT DE PROCESAMIENTO DE DATOS CASEN 
# =============================================================================
# Descripción: Procesamiento y homogeneización de datos de la Encuesta CASEN
#              (Caracterización Socioeconómica Nacional) de Chile
#              para los años 2015, 2017, 2022 y 2024
# Autor: Natalia Debandi - con el apoyo inicial de Joaquín Zajac
# Fecha: Enero 2026
# Mejoras: Variables de formalidad comparables con EPH Argentina
# =============================================================================

rm(list = ls())

library(tidyverse)
library(readr)
library(forcats)
library(openxlsx)
library(dplyr)

# Operador auxiliar "not in"
`%nin%` <- Negate(`%in%`)

casen <- read_csv("Data/Casen_unificado_2015_2024_proc.csv", show_col_types = FALSE)

indicadores_csv <- data.frame()

message("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
message("CALCULANDO INDICADORES CASEN (migrantes = total)")
message("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

# -----------------------------------------------------------------------------
# I1. TASA DE ACTIVIDAD
# -----------------------------------------------------------------------------
message("   → I1. Tasa de actividad...")

base_i1 <- casen %>%
  filter(edad >= 15 & !is.na(migrante)) %>%
  mutate(activo = ifelse(estado_actividad %in% c("Ocupado", "Desocupado"), 1, 0))

i1_nat <- base_i1 %>%
  filter(migrante == "Nativos") %>%
  group_by(anio) %>%
  summarise(activos = sum(expr[activo == 1], na.rm = TRUE),
            poblacion = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(nativos = (activos / poblacion) * 100) %>%
  select(anio, nativos)

i1_mig <- base_i1 %>%
  filter(migrante == "Migrantes") %>%
  group_by(anio) %>%
  summarise(activos = sum(expr[activo == 1], na.rm = TRUE),
            poblacion = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(migrantes = (activos / poblacion) * 100) %>%
  select(anio, migrantes)

i1_rec <- base_i1 %>%
  filter(migrante == "Migrantes", migrante_reciente == "Si") %>%
  group_by(anio) %>%
  summarise(activos = sum(expr[activo == 1], na.rm = TRUE),
            poblacion = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(migrantes_recientes = (activos / poblacion) * 100) %>%
  select(anio, migrantes_recientes)

tasa_actividad_casen <- i1_nat %>%
  full_join(i1_mig, by = "anio") %>%
  full_join(i1_rec, by = "anio") %>%
  mutate(indicador = "I1_tasa_actividad", pais = "Chile", anio = as.numeric(anio)) %>%
  select(indicador, anio, pais, nativos, migrantes, migrantes_recientes)

indicadores_csv <- bind_rows(indicadores_csv, tasa_actividad_casen)
message("      ✓ Tasa de actividad calculada\n")

# -----------------------------------------------------------------------------
# I2. TASA DE DESEMPLEO
# -----------------------------------------------------------------------------
message("   → I2. Tasa de desempleo...")

base_i2 <- casen %>%
  filter(estado_actividad %in% c("Ocupado", "Desocupado") & !is.na(migrante)) %>%
  mutate(desocupado = ifelse(estado_actividad == "Desocupado", 1, 0))

i2_nat <- base_i2 %>%
  filter(migrante == "Nativos") %>%
  group_by(anio) %>%
  summarise(desocupados = sum(expr[desocupado == 1], na.rm = TRUE),
            pea = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(nativos = (desocupados / pea) * 100) %>%
  select(anio, nativos)

i2_mig <- base_i2 %>%
  filter(migrante == "Migrantes") %>%
  group_by(anio) %>%
  summarise(desocupados = sum(expr[desocupado == 1], na.rm = TRUE),
            pea = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(migrantes = (desocupados / pea) * 100) %>%
  select(anio, migrantes)

i2_rec <- base_i2 %>%
  filter(migrante == "Migrantes", migrante_reciente == "Si") %>%
  group_by(anio) %>%
  summarise(desocupados = sum(expr[desocupado == 1], na.rm = TRUE),
            pea = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(migrantes_recientes = (desocupados / pea) * 100) %>%
  select(anio, migrantes_recientes)

tasa_desempleo_casen <- i2_nat %>%
  full_join(i2_mig, by = "anio") %>%
  full_join(i2_rec, by = "anio") %>%
  mutate(indicador = "I2_tasa_desempleo", pais = "Chile", anio = as.numeric(anio)) %>%
  select(indicador, anio, pais, nativos, migrantes, migrantes_recientes)

indicadores_csv <- bind_rows(indicadores_csv, tasa_desempleo_casen)
message("      ✓ Tasa de desempleo calculada\n")


# -----------------------------------------------------------------------------
# I3. INFORMALIDAD ASALARIADA
# -----------------------------------------------------------------------------
message("   → I3. Informalidad asalariada...")

base_i3 <- casen %>%
  filter(!is.na(formalidad_empleo_asalariada) & !is.na(migrante)) %>%
  mutate(informal = ifelse(formalidad_empleo_asalariada == "Asalariado informal", 1, 0))

i3_nat <- base_i3 %>%
  filter(migrante == "Nativos") %>%
  group_by(anio) %>%
  summarise(informales = sum(expr[informal == 1], na.rm = TRUE),
            total_asalariados = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(nativos = (informales / total_asalariados) * 100) %>%
  select(anio, nativos)

i3_mig <- base_i3 %>%
  filter(migrante == "Migrantes") %>%
  group_by(anio) %>%
  summarise(informales = sum(expr[informal == 1], na.rm = TRUE),
            total_asalariados = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(migrantes = (informales / total_asalariados) * 100) %>%
  select(anio, migrantes)

i3_rec <- base_i3 %>%
  filter(migrante == "Migrantes", migrante_reciente == "Si") %>%
  group_by(anio) %>%
  summarise(informales = sum(expr[informal == 1], na.rm = TRUE),
            total_asalariados = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(migrantes_recientes = (informales / total_asalariados) * 100) %>%
  select(anio, migrantes_recientes)

informalidad_asalariada_casen <- i3_nat %>%
  full_join(i3_mig, by = "anio") %>%
  full_join(i3_rec, by = "anio") %>%
  mutate(indicador = "I3_informalidad_asalariada", pais = "Chile", anio = as.numeric(anio)) %>%
  select(indicador, anio, pais, nativos, migrantes, migrantes_recientes)

indicadores_csv <- bind_rows(indicadores_csv, informalidad_asalariada_casen)
message("      ✓ Informalidad asalariada calculada\n")


# -----------------------------------------------------------------------------
# I4. INFORMALIDAD TOTAL
# -----------------------------------------------------------------------------
message("   → I4. Informalidad total...")

base_i4 <- casen %>%
  filter(!is.na(formalidad_empleo_total) & !is.na(migrante)) %>%
  mutate(informal = ifelse(formalidad_empleo_total == "Informal", 1, 0))

i4_nat <- base_i4 %>%
  filter(migrante == "Nativos") %>%
  group_by(anio) %>%
  summarise(informales = sum(expr[informal == 1], na.rm = TRUE),
            total_ocupados = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(nativos = (informales / total_ocupados) * 100) %>%
  select(anio, nativos)

i4_mig <- base_i4 %>%
  filter(migrante == "Migrantes") %>%
  group_by(anio) %>%
  summarise(informales = sum(expr[informal == 1], na.rm = TRUE),
            total_ocupados = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(migrantes = (informales / total_ocupados) * 100) %>%
  select(anio, migrantes)

i4_rec <- base_i4 %>%
  filter(migrante == "Migrantes", migrante_reciente == "Si") %>%
  group_by(anio) %>%
  summarise(informales = sum(expr[informal == 1], na.rm = TRUE),
            total_ocupados = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(migrantes_recientes = (informales / total_ocupados) * 100) %>%
  select(anio, migrantes_recientes)

informalidad_total_casen <- i4_nat %>%
  full_join(i4_mig, by = "anio") %>%
  full_join(i4_rec, by = "anio") %>%
  mutate(indicador = "I4_informalidad_total", pais = "Chile", anio = as.numeric(anio)) %>%
  select(indicador, anio, pais, nativos, migrantes, migrantes_recientes)

indicadores_csv <- bind_rows(indicadores_csv, informalidad_total_casen)
message("      ✓ Informalidad total calculada\n")


# -----------------------------------------------------------------------------
# I5. CUENTAPROPISMO
# -----------------------------------------------------------------------------
message("   → I5. Cuentapropismo...")

base_i5 <- casen %>%
  filter(!is.na(categoria_informalidad) & !is.na(migrante)) %>%
  mutate(cuentapropia = ifelse(categoria_informalidad == "Cuentapropista", 1, 0))

i5_nat <- base_i5 %>%
  filter(migrante == "Nativos") %>%
  group_by(anio) %>%
  summarise(cuentapropistas = sum(expr[cuentapropia == 1], na.rm = TRUE),
            total_ocupados = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(nativos = (cuentapropistas / total_ocupados) * 100) %>%
  select(anio, nativos)

i5_mig <- base_i5 %>%
  filter(migrante == "Migrantes") %>%
  group_by(anio) %>%
  summarise(cuentapropistas = sum(expr[cuentapropia == 1], na.rm = TRUE),
            total_ocupados = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(migrantes = (cuentapropistas / total_ocupados) * 100) %>%
  select(anio, migrantes)

i5_rec <- base_i5 %>%
  filter(migrante == "Migrantes", migrante_reciente == "Si") %>%
  group_by(anio) %>%
  summarise(cuentapropistas = sum(expr[cuentapropia == 1], na.rm = TRUE),
            total_ocupados = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(migrantes_recientes = (cuentapropistas / total_ocupados) * 100) %>%
  select(anio, migrantes_recientes)

cuentapropismo_casen <- i5_nat %>%
  full_join(i5_mig, by = "anio") %>%
  full_join(i5_rec, by = "anio") %>%
  mutate(indicador = "I5_cuentapropismo", pais = "Chile", anio = as.numeric(anio)) %>%
  select(indicador, anio, pais, nativos, migrantes, migrantes_recientes)

indicadores_csv <- bind_rows(indicadores_csv, cuentapropismo_casen)
message("      ✓ Cuentapropismo calculado\n")


# -----------------------------------------------------------------------------
# I6. POBREZA POR INGRESOS
# -----------------------------------------------------------------------------
message("   → I6. Pobreza por ingresos...")

base_i6 <- casen %>%
  filter(!is.na(pobreza) & !is.na(migrante)) %>%
  mutate(pobre = ifelse(pobreza %in% c("Pobres", "Pobres extremos"), 1, 0))

i6_nat <- base_i6 %>%
  filter(migrante == "Nativos") %>%
  group_by(anio) %>%
  summarise(pobres = sum(expr[pobre == 1], na.rm = TRUE),
            total = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(nativos = (pobres / total) * 100) %>%
  select(anio, nativos)

i6_mig <- base_i6 %>%
  filter(migrante == "Migrantes") %>%
  group_by(anio) %>%
  summarise(pobres = sum(expr[pobre == 1], na.rm = TRUE),
            total = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(migrantes = (pobres / total) * 100) %>%
  select(anio, migrantes)

i6_rec <- base_i6 %>%
  filter(migrante == "Migrantes", migrante_reciente == "Si") %>%
  group_by(anio) %>%
  summarise(pobres = sum(expr[pobre == 1], na.rm = TRUE),
            total = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(migrantes_recientes = (pobres / total) * 100) %>%
  select(anio, migrantes_recientes)

pobreza_casen <- i6_nat %>%
  full_join(i6_mig, by = "anio") %>%
  full_join(i6_rec, by = "anio") %>%
  mutate(indicador = "I6_pobreza", pais = "Chile", anio = as.numeric(anio)) %>%
  select(indicador, anio, pais, nativos, migrantes, migrantes_recientes)

indicadores_csv <- bind_rows(indicadores_csv, pobreza_casen)
message("      ✓ Pobreza por ingresos calculada\n")


# -----------------------------------------------------------------------------
# I7. POBREZA EXTREMA
# -----------------------------------------------------------------------------
message("   → I7. Pobreza extrema...")

base_i7 <- casen %>%
  filter(!is.na(pobreza) & !is.na(migrante)) %>%
  mutate(pobre_extremo = ifelse(pobreza == "Pobres extremos", 1, 0))

i7_nat <- base_i7 %>%
  filter(migrante == "Nativos") %>%
  group_by(anio) %>%
  summarise(pobres_extremos = sum(expr[pobre_extremo == 1], na.rm = TRUE),
            total = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(nativos = (pobres_extremos / total) * 100) %>%
  select(anio, nativos)

i7_mig <- base_i7 %>%
  filter(migrante == "Migrantes") %>%
  group_by(anio) %>%
  summarise(pobres_extremos = sum(expr[pobre_extremo == 1], na.rm = TRUE),
            total = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(migrantes = (pobres_extremos / total) * 100) %>%
  select(anio, migrantes)

i7_rec <- base_i7 %>%
  filter(migrante == "Migrantes", migrante_reciente == "Si") %>%
  group_by(anio) %>%
  summarise(pobres_extremos = sum(expr[pobre_extremo == 1], na.rm = TRUE),
            total = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(migrantes_recientes = (pobres_extremos / total) * 100) %>%
  select(anio, migrantes_recientes)

pobreza_extrema_casen <- i7_nat %>%
  full_join(i7_mig, by = "anio") %>%
  full_join(i7_rec, by = "anio") %>%
  mutate(indicador = "I7_pobreza_extrema", pais = "Chile", anio = as.numeric(anio)) %>%
  select(indicador, anio, pais, nativos, migrantes, migrantes_recientes)

indicadores_csv <- bind_rows(indicadores_csv, pobreza_extrema_casen)
message("      ✓ Pobreza extrema calculada\n")


# -----------------------------------------------------------------------------
# I8. ASISTENCIA ESCOLAR 6-17
# -----------------------------------------------------------------------------
message("   → I8. Tasa de asistencia escolar 6-17 años...")

base_i8 <- casen %>%
  filter(edad >= 6 & edad <= 17 & !is.na(migrante) & !is.na(asiste)) %>%
  mutate(asiste_escuela = ifelse(asiste == "Asiste", 1, 0))

i8_nat <- base_i8 %>%
  filter(migrante == "Nativos") %>%
  group_by(anio) %>%
  summarise(asisten = sum(expr[asiste_escuela == 1], na.rm = TRUE),
            total = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(nativos = (asisten / total) * 100) %>%
  select(anio, nativos)

i8_mig <- base_i8 %>%
  filter(migrante == "Migrantes") %>%
  group_by(anio) %>%
  summarise(asisten = sum(expr[asiste_escuela == 1], na.rm = TRUE),
            total = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(migrantes = (asisten / total) * 100) %>%
  select(anio, migrantes)

i8_rec <- base_i8 %>%
  filter(migrante == "Migrantes", migrante_reciente == "Si") %>%
  group_by(anio) %>%
  summarise(asisten = sum(expr[asiste_escuela == 1], na.rm = TRUE),
            total = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(migrantes_recientes = (asisten / total) * 100) %>%
  select(anio, migrantes_recientes)

asistencia_escolar_casen <- i8_nat %>%
  full_join(i8_mig, by = "anio") %>%
  full_join(i8_rec, by = "anio") %>%
  mutate(indicador = "I8_asistencia_escolar_6_17", pais = "Chile", anio = as.numeric(anio)) %>%
  select(indicador, anio, pais, nativos, migrantes, migrantes_recientes)

indicadores_csv <- bind_rows(indicadores_csv, asistencia_escolar_casen)
message("      ✓ Asistencia escolar 6-17 años calculada\n")


# -----------------------------------------------------------------------------
# I9. ASISTENCIA SUPERIOR 18-29
# -----------------------------------------------------------------------------
message("   → I9. Asistencia a educación superior 18-29 años...")

base_i9 <- casen %>%
  filter(edad >= 18 & edad <= 29 & !is.na(migrante)) %>%
  mutate(asiste_superior = ifelse(asiste == "Asiste" & nivel_asiste %in% c(12,13,14,15,16,17), 1, 0))

i9_nat <- base_i9 %>%
  filter(migrante == "Nativos") %>%
  group_by(anio) %>%
  summarise(asisten = sum(expr[asiste_superior == 1], na.rm = TRUE),
            total = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(nativos = (asisten / total) * 100) %>%
  select(anio, nativos)

i9_mig <- base_i9 %>%
  filter(migrante == "Migrantes") %>%
  group_by(anio) %>%
  summarise(asisten = sum(expr[asiste_superior == 1], na.rm = TRUE),
            total = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(migrantes = (asisten / total) * 100) %>%
  select(anio, migrantes)

i9_rec <- base_i9 %>%
  filter(migrante == "Migrantes", migrante_reciente == "Si") %>%
  group_by(anio) %>%
  summarise(asisten = sum(expr[asiste_superior == 1], na.rm = TRUE),
            total = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(migrantes_recientes = (asisten / total) * 100) %>%
  select(anio, migrantes_recientes)

asistencia_superior_casen <- i9_nat %>%
  full_join(i9_mig, by = "anio") %>%
  full_join(i9_rec, by = "anio") %>%
  mutate(indicador = "I9_asistencia_superior_18_29", pais = "Chile", anio = as.numeric(anio)) %>%
  select(indicador, anio, pais, nativos, migrantes, migrantes_recientes)

indicadores_csv <- bind_rows(indicadores_csv, asistencia_superior_casen)
message("      ✓ Asistencia superior 18-29 años calculada\n")


# -----------------------------------------------------------------------------
# I10. SISTEMA PÚBLICO DE SALUD
# -----------------------------------------------------------------------------
message("   → I10. Dependencia del sistema público de salud...")

base_i10 <- casen %>%
  filter(!is.na(sistema_salud) & !is.na(migrante)) %>%
  mutate(usa_publico = ifelse(sistema_salud == "Accede a salud pública gratuita", 1, 0))

i10_nat <- base_i10 %>%
  filter(migrante == "Nativos") %>%
  group_by(anio) %>%
  summarise(usan_publico = sum(expr[usa_publico == 1], na.rm = TRUE),
            total = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(nativos = (usan_publico / total) * 100) %>%
  select(anio, nativos)

i10_mig <- base_i10 %>%
  filter(migrante == "Migrantes") %>%
  group_by(anio) %>%
  summarise(usan_publico = sum(expr[usa_publico == 1], na.rm = TRUE),
            total = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(migrantes = (usan_publico / total) * 100) %>%
  select(anio, migrantes)

i10_rec <- base_i10 %>%
  filter(migrante == "Migrantes", migrante_reciente == "Si") %>%
  group_by(anio) %>%
  summarise(usan_publico = sum(expr[usa_publico == 1], na.rm = TRUE),
            total = sum(expr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(migrantes_recientes = (usan_publico / total) * 100) %>%
  select(anio, migrantes_recientes)

sistema_salud_publico_casen <- i10_nat %>%
  full_join(i10_mig, by = "anio") %>%
  full_join(i10_rec, by = "anio") %>%
  mutate(indicador = "I10_sistema_salud_publico", pais = "Chile", anio = as.numeric(anio)) %>%
  select(indicador, anio, pais, nativos, migrantes, migrantes_recientes)

indicadores_csv <- bind_rows(indicadores_csv, sistema_salud_publico_casen)
message("      ✓ Dependencia del sistema público de salud calculada\n")


message("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
message("INDICADORES CASEN COMPLETADOS (migrantes=total)")
message("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

write_csv(indicadores_csv, "Data/Indicadores_Casen_2015_2024.csv")


