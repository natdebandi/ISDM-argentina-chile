# =============================================================================
# SCRIPT DE PROCESAMIENTO DE DATOS EPH - VERSIÓN SIMPLIFICADA
# =============================================================================
# Descripción: Procesamiento y homogeneización de datos de la Encuesta 
#              Permanente de Hogares (EPH) del INDEC
# Autor: Natalia Debandi
# Fecha: Enero 2026
# Versión: Simplificada - sin funciones, procesamiento lineal
# =============================================================================

rm(list = ls())

library(tidyverse)
library(haven)
library(readr)
library(eph)  # Librería para calcular pobreza

# Operador auxiliar "not in"
`%nin%` <- Negate(`%in%`)


#leer Data/EPH_indiv_2016_2024_proc.csv
EPH_indiv_unificado <- read_csv("Data/EPH_indiv_2016_2024_proc.csv", show_col_types = FALSE)


# =============================================================================
# CÁLCULO DE INDICADORES EPH
# =============================================================================

#borrar todo menos EPH_indiv_unificado
rm(list = setdiff(ls(), "EPH_indiv_unificado"))

message("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
message("CALCULANDO INDICADORES EPH")
message("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")


# Dataframe para almacenar todos los indicadores
indicadores_eph <- data.frame()

# -----------------------------------------------------------------------------
# I1. TASA DE ACTIVIDAD
# -----------------------------------------------------------------------------

message("   → I1. Tasa de actividad...")


base_i1 <- EPH_indiv_unificado %>%
  filter(!is.na(estado_actividad) & !is.na(migrante)) %>%  # PET
  mutate(
    activo = ifelse(estado_actividad %in% c("Ocupado", "Desocupado"), 1, 0)
  )

# 1) Nativos
i1_nat <- base_i1 %>%
  filter(migrante == 0) %>%
  group_by(anio) %>%
  summarise(
    activos = sum(ponderacion[activo == 1], na.rm = TRUE),
    poblacion = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(nativos = (activos / poblacion) * 100) %>%
  select(anio, nativos)

# 2) Migrantes TOTALES (incluye recientes)
i1_mig_tot <- base_i1 %>%
  filter(migrante == 1) %>%
  group_by(anio) %>%
  summarise(
    activos = sum(ponderacion[activo == 1], na.rm = TRUE),
    poblacion = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(migrantes = (activos / poblacion) * 100) %>%
  select(anio, migrantes)

# 3) Migrantes RECIENTES (subconjunto de migrantes)
i1_mig_rec <- base_i1 %>%
  filter(migrante == 1, migrante_reciente == "Si") %>%
  group_by(anio) %>%
  summarise(
    activos = sum(ponderacion[activo == 1], na.rm = TRUE),
    poblacion = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(migrantes_recientes = (activos / poblacion) * 100) %>%
  select(anio, migrantes_recientes)

# Unir los 3
tasa_actividad_eph <- i1_nat %>%
  full_join(i1_mig_tot, by = "anio") %>%
  full_join(i1_mig_rec, by = "anio") %>%
  mutate(
    indicador = "I1_tasa_actividad",
    pais = "Argentina",
    anio = as.numeric(anio)
  ) %>%
  select(indicador, anio, pais, nativos, migrantes, migrantes_recientes)

indicadores_eph <- bind_rows(indicadores_eph, tasa_actividad_eph)
message("      ✓ Tasa de actividad calculada\n")



# -----------------------------------------------------------------------------
# I2. TASA DE DESEMPLEO
# -----------------------------------------------------------------------------

message("   → I2. Tasa de desempleo...")

base_i2 <- EPH_indiv_unificado %>%
  filter(estado_actividad %in% c("Ocupado", "Desocupado") & !is.na(migrante)) %>%
  mutate(
    desocupado = ifelse(estado_actividad == "Desocupado", 1, 0)
  )

# 1) Nativos
i2_nat <- base_i2 %>%
  filter(migrante == 0) %>%
  group_by(anio) %>%
  summarise(
    desocupados = sum(ponderacion[desocupado == 1], na.rm = TRUE),
    pea = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(nativos = (desocupados / pea) * 100)%>%
  select(anio, nativos)

# 2) Migrantes TOTALES (incluye recientes)
i2_mig_tot <- base_i2 %>%
  filter(migrante == 1) %>%
  group_by(anio) %>%
  summarise(
    desocupados = sum(ponderacion[desocupado == 1], na.rm = TRUE),
    pea = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(migrantes = (desocupados / pea) * 100) %>%
  select(anio, migrantes)

# 3) Migrantes RECIENTES (subconjunto de migrantes)
i2_mig_rec <- base_i2 %>%
  filter(migrante == 1, migrante_reciente == "Si") %>%
  group_by(anio) %>%
  summarise(
    desocupados = sum(ponderacion[desocupado == 1], na.rm = TRUE),
    pea = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(migrantes_recientes =(desocupados / pea) * 100) %>%
  select(anio, migrantes_recientes)

# Unir los 3
tasa_desempleo_eph <- i2_nat %>%
  full_join(i2_mig_tot, by = "anio") %>%
  full_join(i2_mig_rec, by = "anio") %>%
  mutate(
    indicador = "I2_tasa_desempleo",
    pais = "Argentina",
    anio = as.numeric(anio)
  ) %>%
  select(indicador, anio, pais, nativos, migrantes, migrantes_recientes)



indicadores_eph <- bind_rows(indicadores_eph, tasa_desempleo_eph)
message("      ✓ Tasa de desempleo calculada\n")

# -----------------------------------------------------------------------------
# I3. INFORMALIDAD ASALARIADA
# -----------------------------------------------------------------------------

message("   → I3. Informalidad asalariada...")

base_i3 <- EPH_indiv_unificado %>%
  filter(!is.na(formalidad_empleo_asalariada) & !is.na(migrante)) %>%
  mutate(informal = ifelse(formalidad_empleo_asalariada == "Asalariado informal", 1, 0))

i3_nat <- base_i3 %>%
  filter(migrante == 0) %>%
  group_by(anio) %>%
  summarise(
    informales = sum(ponderacion[informal == 1], na.rm = TRUE),
    total_asalariados = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(nativos = (informales / total_asalariados) * 100) %>%
  select(anio, nativos)

i3_mig <- base_i3 %>%
  filter(migrante == 1) %>%
  group_by(anio) %>%
  summarise(
    informales = sum(ponderacion[informal == 1], na.rm = TRUE),
    total_asalariados = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(migrantes = (informales / total_asalariados) * 100) %>%
  select(anio, migrantes)

i3_rec <- base_i3 %>%
  filter(migrante == 1, migrante_reciente == "Si") %>%
  group_by(anio) %>%
  summarise(
    informales = sum(ponderacion[informal == 1], na.rm = TRUE),
    total_asalariados = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(migrantes_recientes = (informales / total_asalariados) * 100) %>%
  select(anio, migrantes_recientes)

informalidad_asalariada_eph <- i3_nat %>%
  full_join(i3_mig, by = "anio") %>%
  full_join(i3_rec, by = "anio") %>%
  mutate(indicador = "I3_informalidad_asalariada", pais = "Argentina", anio = as.numeric(anio)) %>%
  select(indicador, anio, pais, nativos, migrantes, migrantes_recientes)

indicadores_eph <- bind_rows(indicadores_eph, informalidad_asalariada_eph)
message("      ✓ Informalidad asalariada calculada\n")


# -----------------------------------------------------------------------------
# I4. INFORMALIDAD TOTAL
# -----------------------------------------------------------------------------

message("   → I4. Informalidad total...")

base_i4 <- EPH_indiv_unificado %>%
  filter(!is.na(formalidad_empleo_total) & !is.na(migrante)) %>%
  mutate(informal = ifelse(formalidad_empleo_total == "Informal", 1, 0))

i4_nat <- base_i4 %>%
  filter(migrante == 0) %>%
  group_by(anio) %>%
  summarise(
    informales = sum(ponderacion[informal == 1], na.rm = TRUE),
    total_ocupados = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(nativos = (informales / total_ocupados) * 100) %>%
  select(anio, nativos)

i4_mig <- base_i4 %>%
  filter(migrante == 1) %>%
  group_by(anio) %>%
  summarise(
    informales = sum(ponderacion[informal == 1], na.rm = TRUE),
    total_ocupados = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(migrantes = (informales / total_ocupados) * 100) %>%
  select(anio, migrantes)

i4_rec <- base_i4 %>%
  filter(migrante == 1, migrante_reciente == "Si") %>%
  group_by(anio) %>%
  summarise(
    informales = sum(ponderacion[informal == 1], na.rm = TRUE),
    total_ocupados = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(migrantes_recientes = (informales / total_ocupados) * 100) %>%
  select(anio, migrantes_recientes)

informalidad_total_eph <- i4_nat %>%
  full_join(i4_mig, by = "anio") %>%
  full_join(i4_rec, by = "anio") %>%
  mutate(indicador = "I4_informalidad_total", pais = "Argentina", anio = as.numeric(anio)) %>%
  select(indicador, anio, pais, nativos, migrantes, migrantes_recientes)

indicadores_eph <- bind_rows(indicadores_eph, informalidad_total_eph)
message("      ✓ Informalidad total calculada\n")

# -----------------------------------------------------------------------------
# I5. CUENTAPROPISMO
# -----------------------------------------------------------------------------

message("   → I5. Cuentapropismo...")

base_i5 <- EPH_indiv_unificado %>%
  filter(!is.na(categoria_informalidad) & !is.na(migrante)) %>%
  mutate(cuentapropia = ifelse(categoria_informalidad == "Cuentapropista", 1, 0))

i5_nat <- base_i5 %>%
  filter(migrante == 0) %>%
  group_by(anio) %>%
  summarise(
    cuentapropistas = sum(ponderacion[cuentapropia == 1], na.rm = TRUE),
    total_ocupados = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(nativos = (cuentapropistas / total_ocupados) * 100) %>%
  select(anio, nativos)

i5_mig <- base_i5 %>%
  filter(migrante == 1) %>%
  group_by(anio) %>%
  summarise(
    cuentapropistas = sum(ponderacion[cuentapropia == 1], na.rm = TRUE),
    total_ocupados = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(migrantes = (cuentapropistas / total_ocupados) * 100) %>%
  select(anio, migrantes)

i5_rec <- base_i5 %>%
  filter(migrante == 1, migrante_reciente == "Si") %>%
  group_by(anio) %>%
  summarise(
    cuentapropistas = sum(ponderacion[cuentapropia == 1], na.rm = TRUE),
    total_ocupados = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(migrantes_recientes = (cuentapropistas / total_ocupados) * 100) %>%
  select(anio, migrantes_recientes)

cuentapropismo_eph <- i5_nat %>%
  full_join(i5_mig, by = "anio") %>%
  full_join(i5_rec, by = "anio") %>%
  mutate(indicador = "I5_cuentapropismo", pais = "Argentina", anio = as.numeric(anio)) %>%
  select(indicador, anio, pais, nativos, migrantes, migrantes_recientes)

indicadores_eph <- bind_rows(indicadores_eph, cuentapropismo_eph)
message("      ✓ Cuentapropismo calculado\n")


# -----------------------------------------------------------------------------
# I6. POBREZA POR INGRESOS
# -----------------------------------------------------------------------------

message("   → I6. Pobreza por ingresos...")

base_i6 <- EPH_indiv_unificado %>%
  filter(!is.na(situacion) & !is.na(migrante)) %>%
  mutate(pobre = ifelse(situacion %in% c("pobre", "indigente"), 1, 0))

i6_nat <- base_i6 %>%
  filter(migrante == 0) %>%
  group_by(anio) %>%
  summarise(
    pobres = sum(ponderacion[pobre == 1], na.rm = TRUE),
    total = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(nativos = (pobres / total) * 100) %>%
  select(anio, nativos)

i6_mig <- base_i6 %>%
  filter(migrante == 1) %>%
  group_by(anio) %>%
  summarise(
    pobres = sum(ponderacion[pobre == 1], na.rm = TRUE),
    total = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(migrantes = (pobres / total) * 100) %>%
  select(anio, migrantes)

i6_rec <- base_i6 %>%
  filter(migrante == 1, migrante_reciente == "Si") %>%
  group_by(anio) %>%
  summarise(
    pobres = sum(ponderacion[pobre == 1], na.rm = TRUE),
    total = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(migrantes_recientes = (pobres / total) * 100) %>%
  select(anio, migrantes_recientes)

pobreza_eph <- i6_nat %>%
  full_join(i6_mig, by = "anio") %>%
  full_join(i6_rec, by = "anio") %>%
  mutate(indicador = "I6_pobreza", pais = "Argentina", anio = as.numeric(anio)) %>%
  select(indicador, anio, pais, nativos, migrantes, migrantes_recientes)

indicadores_eph <- bind_rows(indicadores_eph, pobreza_eph)
message("      ✓ Pobreza por ingresos calculada\n")


# -----------------------------------------------------------------------------
# I7. POBREZA EXTREMA
# -----------------------------------------------------------------------------

message("   → I7. Pobreza extrema...")

base_i7 <- EPH_indiv_unificado %>%
  filter(!is.na(situacion) & !is.na(migrante)) %>%
  mutate(pobre_extremo = ifelse(situacion == "indigente", 1, 0))

i7_nat <- base_i7 %>%
  filter(migrante == 0) %>%
  group_by(anio) %>%
  summarise(
    pobres_extremos = sum(ponderacion[pobre_extremo == 1], na.rm = TRUE),
    total = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(nativos = (pobres_extremos / total) * 100) %>%
  select(anio, nativos)

i7_mig <- base_i7 %>%
  filter(migrante == 1) %>%
  group_by(anio) %>%
  summarise(
    pobres_extremos = sum(ponderacion[pobre_extremo == 1], na.rm = TRUE),
    total = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(migrantes = (pobres_extremos / total) * 100) %>%
  select(anio, migrantes)

i7_rec <- base_i7 %>%
  filter(migrante == 1, migrante_reciente == "Si") %>%
  group_by(anio) %>%
  summarise(
    pobres_extremos = sum(ponderacion[pobre_extremo == 1], na.rm = TRUE),
    total = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(migrantes_recientes = (pobres_extremos / total) * 100) %>%
  select(anio, migrantes_recientes)

pobreza_extrema_eph <- i7_nat %>%
  full_join(i7_mig, by = "anio") %>%
  full_join(i7_rec, by = "anio") %>%
  mutate(indicador = "I7_pobreza_extrema", pais = "Argentina", anio = as.numeric(anio)) %>%
  select(indicador, anio, pais, nativos, migrantes, migrantes_recientes)

indicadores_eph <- bind_rows(indicadores_eph, pobreza_extrema_eph)
message("      ✓ Pobreza extrema calculada\n")


# -----------------------------------------------------------------------------
# I8. TASA DE ASISTENCIA ESCOLAR (6-17 años)
# -----------------------------------------------------------------------------

message("   → I8. Tasa de asistencia escolar 6-17 años...")

base_i8 <- EPH_indiv_unificado %>%
  filter(edad >= 6 & edad <= 17 & !is.na(migrante) & !is.na(asistencia_educacion)) %>%
  mutate(asiste_escuela = ifelse(asistencia_educacion == "Asiste actualmente", 1, 0))

i8_nat <- base_i8 %>%
  filter(migrante == 0) %>%
  group_by(anio) %>%
  summarise(
    asisten = sum(ponderacion[asiste_escuela == 1], na.rm = TRUE),
    total = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(nativos = (asisten / total) * 100) %>%
  select(anio, nativos)

i8_mig <- base_i8 %>%
  filter(migrante == 1) %>%
  group_by(anio) %>%
  summarise(
    asisten = sum(ponderacion[asiste_escuela == 1], na.rm = TRUE),
    total = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(migrantes = (asisten / total) * 100) %>%
  select(anio, migrantes)

i8_rec <- base_i8 %>%
  filter(migrante == 1, migrante_reciente == "Si") %>%
  group_by(anio) %>%
  summarise(
    asisten = sum(ponderacion[asiste_escuela == 1], na.rm = TRUE),
    total = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(migrantes_recientes = (asisten / total) * 100) %>%
  select(anio, migrantes_recientes)

asistencia_escolar_eph <- i8_nat %>%
  full_join(i8_mig, by = "anio") %>%
  full_join(i8_rec, by = "anio") %>%
  mutate(indicador = "I8_asistencia_escolar_6_17", pais = "Argentina", anio = as.numeric(anio)) %>%
  select(indicador, anio, pais, nativos, migrantes, migrantes_recientes)

indicadores_eph <- bind_rows(indicadores_eph, asistencia_escolar_eph)
message("      ✓ Asistencia escolar 6-17 años calculada\n")


# -----------------------------------------------------------------------------
# I9. ASISTENCIA A EDUCACIÓN SUPERIOR (18-29 años)
# -----------------------------------------------------------------------------

message("   → I9. Asistencia a educación superior 18-29 años...")

base_i9 <- EPH_indiv_unificado %>%
  filter(edad >= 18 & edad <= 29 & !is.na(migrante)) %>%
  mutate(
    asiste_superior = ifelse(
      asistencia_educacion == "Asiste actualmente" &
        nivel_educativo_asiste %in% c("Terciario", "Universitario", "Postgrado"),
      1, 0
    )
  )

i9_nat <- base_i9 %>%
  filter(migrante == 0) %>%
  group_by(anio) %>%
  summarise(
    asisten = sum(ponderacion[asiste_superior == 1], na.rm = TRUE),
    total = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(nativos = (asisten / total) * 100) %>%
  select(anio, nativos)

i9_mig <- base_i9 %>%
  filter(migrante == 1) %>%
  group_by(anio) %>%
  summarise(
    asisten = sum(ponderacion[asiste_superior == 1], na.rm = TRUE),
    total = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(migrantes = (asisten / total) * 100) %>%
  select(anio, migrantes)

i9_rec <- base_i9 %>%
  filter(migrante == 1, migrante_reciente == "Si") %>%
  group_by(anio) %>%
  summarise(
    asisten = sum(ponderacion[asiste_superior == 1], na.rm = TRUE),
    total = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(migrantes_recientes = (asisten / total) * 100) %>%
  select(anio, migrantes_recientes)

asistencia_superior_eph <- i9_nat %>%
  full_join(i9_mig, by = "anio") %>%
  full_join(i9_rec, by = "anio") %>%
  mutate(indicador = "I9_asistencia_superior_18_29", pais = "Argentina", anio = as.numeric(anio)) %>%
  select(indicador, anio, pais, nativos, migrantes, migrantes_recientes)

indicadores_eph <- bind_rows(indicadores_eph, asistencia_superior_eph)
message("      ✓ Asistencia superior 18-29 años calculada\n")


# -----------------------------------------------------------------------------
# 4. AGREGAR INDICADOR I10 (al final, después de I9)
# -----------------------------------------------------------------------------

message("   → I10. Dependencia del sistema público de salud...")

base_i10 <- EPH_indiv_unificado %>%
  filter(!is.na(usa_sistema_publico) & !is.na(migrante)) %>%
  mutate(usa_publico = ifelse(usa_sistema_publico == 1, 1, 0))

i10_nat <- base_i10 %>%
  filter(migrante == 0) %>%
  group_by(anio) %>%
  summarise(
    usan_publico = sum(ponderacion[usa_publico == 1], na.rm = TRUE),
    total = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(nativos = (usan_publico / total) * 100) %>%
  select(anio, nativos)

i10_mig <- base_i10 %>%
  filter(migrante == 1) %>%
  group_by(anio) %>%
  summarise(
    usan_publico = sum(ponderacion[usa_publico == 1], na.rm = TRUE),
    total = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(migrantes = (usan_publico / total) * 100) %>%
  select(anio, migrantes)

i10_rec <- base_i10 %>%
  filter(migrante == 1, migrante_reciente == "Si") %>%
  group_by(anio) %>%
  summarise(
    usan_publico = sum(ponderacion[usa_publico == 1], na.rm = TRUE),
    total = sum(ponderacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(migrantes_recientes = (usan_publico / total) * 100) %>%
  select(anio, migrantes_recientes)

sistema_salud_publico_eph <- i10_nat %>%
  full_join(i10_mig, by = "anio") %>%
  full_join(i10_rec, by = "anio") %>%
  mutate(indicador = "I10_sistema_salud_publico", pais = "Argentina", anio = as.numeric(anio)) %>%
  select(indicador, anio, pais, nativos, migrantes, migrantes_recientes)

indicadores_eph <- bind_rows(indicadores_eph, sistema_salud_publico_eph)
message("      ✓ Dependencia del sistema público de salud calculada\n")


message("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
message("INDICADORES EPH COMPLETADOS")
message("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

# =============================================================================
# GUARDADO DE INDICADORES
# =============================================================================

# Guardar indicadores
write_csv(indicadores_eph, "Data/indicadores_eph_2016_2024.csv")

