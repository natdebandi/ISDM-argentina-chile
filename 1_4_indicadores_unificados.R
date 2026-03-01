# =============================================================================
# UNIFICAR DATOS CASEN, ENE Y EPH 
# =============================================================================
# Autor: Natalia Debandi 
# Fecha: Enero 2026
# =============================================================================


rm(list = ls())
library(tidyverse)
library(readxl)
library(openxlsx)
library(dplyr)


# Cargar datos EPH (Argentina)
eph <- read_csv("Data/indicadores_eph_2016_2024.csv", show_col_types = FALSE)

# Cargar datos CASEN (Chile)
casen <- read_csv("Data/Indicadores_Casen_2015_2024.csv", show_col_types = FALSE)

# Cargar datos ENE (Chile) 
ene <- read_excel("Data/indicadores_ENE_CH.xlsx")


eph_long <- eph %>%
  pivot_longer(
    cols = c(nativos, migrantes, migrantes_recientes),
    names_to = "grupo",
    values_to = "valor"
  ) %>%
  mutate(
    fuente = "EPH",
    pais = "Argentina"
  )

casen_long <- casen %>%
  pivot_longer(
    cols = c(nativos, migrantes, migrantes_recientes),
    names_to = "grupo",
    values_to = "valor"
  ) %>%
  mutate(
    fuente = "CASEN",
    pais = "Chile"
  )


ene <- ene %>%
  #cambiar a numerico columna migrantes_recientes
  mutate(migrantes_recientes = as.numeric(migrantes_recientes))

ene_long <- ene %>%
  pivot_longer(
    cols = c(nativos, migrantes, migrantes_recientes),
    names_to = "grupo",
    values_to = "valor"
  ) %>%
  mutate(
    fuente = "ENE",
    pais = "Chile"
  )

datos_unificados <- bind_rows(
  eph_long,
  casen_long,
  ene_long
) %>%
  mutate(
    # Estandarizar nombres de grupos
    grupo = case_when(
      grupo == "nativos" ~ "Nativos",
      grupo == "migrantes" ~ "Migrantes",
      grupo == "migrantes_recientes" ~ "Migrantes recientes",
      TRUE ~ grupo
    ),
    # Limpiar nombres de indicadores
    indicador_clean = str_replace(indicador, "I[0-9]_", "") %>%
      str_replace_all("_", " ") %>%
      str_to_title()
  )


# =============================================================================
# RENUMERACIÓN: eliminar I4_informalidad_total y renombrar I5-I10 → I4-I9
# =============================================================================

datos_unificados <- datos_unificados %>%
  filter(indicador != "I4_informalidad_total") %>%
  mutate(
    indicador = case_when(
      indicador == "I5_cuentapropismo"           ~ "I4_cuentapropismo",
      indicador == "I6_pobreza"                  ~ "I5_pobreza",
      indicador == "I7_pobreza_extrema"          ~ "I6_pobreza_extrema",
      indicador == "I8_asistencia_escolar_6_17"  ~ "I7_asistencia_escolar_6_17",
      indicador == "I9_asistencia_superior_18_29"~ "I8_asistencia_superior_18_29",
      indicador == "I10_sistema_salud_publico"   ~ "I9_sistema_salud_publico",
      TRUE ~ indicador
    ),
    # Regenerar indicador_clean a partir del nuevo nombre
    indicador_clean = str_replace(indicador, "I[0-9]+_", "") %>%
      str_replace_all("_", " ") %>%
      str_to_title()
  )

#guardar los datos de indicadores
write_csv(datos_unificados, "Data/indicadores_unificados_arg_chile_2015_2024.csv")

