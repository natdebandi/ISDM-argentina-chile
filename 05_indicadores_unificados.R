# =============================================================================
# 5. Unificación de Argentina y Chile
# =============================================================================
# Lee:      los indicadores de los pasos 3 y 4 y Data/indicadores_ENE_CH.xlsx
#           (indicadores publicados por el INE a partir de la ENE, cargados a mano).
# Hace:     formato largo, elimina la informalidad total (redundante) y renumera
#           I1 a I9. De la ENE se usan solo actividad y desempleo: su informalidad
#           es la del total de ocupados, no comparable con la informalidad
#           asalariada de la EPH (P12).
# Escribe:  Data/indicadores_unificados_arg_chile_2015_2024.csv
# Detalle de cada decisión: ISDM_metodologia_trazabilidad.docx (carpeta del artículo,
# VERSION_FINAL), códigos de problema P1 a P22.
# =============================================================================

rm(list = ls())
library(tidyverse)
library(readxl)
library(openxlsx)
library(dplyr)


# Cargar datos EPH (Argentina)
eph <- read_csv("Data/indicadores_eph_2016_2024.csv", show_col_types = FALSE)

# Cargar datos CASEN (Chile)
casen <- read_csv("Data/indicadores_casen_2015_2024.csv", show_col_types = FALSE)

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
  mutate(migrantes_recientes = as.numeric(migrantes_recientes)) %>%
  filter(indicador != "I3_informalidad_asalariada")   # definición no comparable (P12)

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
      indicador == "I11_cobertura_salud"         ~ "I9_cobertura_salud",
      TRUE ~ indicador
    ),
    # Regenerar indicador_clean a partir del nuevo nombre
    indicador_clean = str_replace(indicador, "I[0-9]+_", "") %>%
      str_replace_all("_", " ") %>%
      str_to_title()
  )

#guardar los datos de indicadores
write_csv(datos_unificados, "Data/indicadores_unificados_arg_chile_2015_2024.csv")

