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


#guardar los datos de indicadores
write_csv(datos_unificados, "Data/indicadores_unificados_arg_chile_2015_2024.csv")

