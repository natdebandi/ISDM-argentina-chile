# =============================================================================
# 10. Calidad muestral de los indicadores (tamaño efectivo de Kish)
# =============================================================================
# Lee:      Data/EPH_indiv_2016_2024_proc.csv y Data/Casen_unificado_2015_2024_proc.csv
# Hace:     para cada indicador, país, año y grupo (no migrante, migrante,
#           migrante reciente) calcula el n muestral, el tamaño efectivo de Kish
#           (suma de pesos al cuadrado / suma de pesos al cuadrado de cada caso)
#           y un semáforo: OK (n >= 100 y n efectivo >= 50), CUIDADO (n >= 30 y
#           n efectivo >= 30) o NO USAR. Usa los mismos universos que los pasos 3
#           y 4. Es un diagnóstico: no filtra ningún indicador del índice (P19).
#           En la EPH los trimestres de un año se apilan y una misma persona
#           puede aparecer más de una vez, así que el n efectivo es optimista.
# Escribe:  output/calidad/calidad_muestral.csv
# =============================================================================

rm(list = ls())
suppressPackageStartupMessages({ library(tidyverse) })
dir.create("output/calidad", showWarnings = FALSE, recursive = TRUE)

kish <- function(w) { w <- w[!is.na(w) & w > 0]; if (!length(w)) NA_real_ else sum(w)^2 / sum(w^2) }

# Una fila por persona y grupo de comparación (el reciente es subconjunto del migrante)
tres_grupos <- function(d) {
  bind_rows(d %>% filter(grupo_m == "No migrante") %>% mutate(grupo = "No migrante"),
            d %>% filter(grupo_m == "Migrante") %>% mutate(grupo = "Migrante"),
            d %>% filter(grupo_m == "Migrante", reciente) %>% mutate(grupo = "Migrante reciente"))
}
resumir <- function(d, indicador, pais) {
  d %>% filter(anio != 2020) %>% tres_grupos() %>%
    group_by(anio, grupo) %>%
    summarise(n = n(), n_efectivo = kish(peso), .groups = "drop") %>%
    mutate(indicador = indicador, pais = pais, .before = 1)
}

# ---- Argentina (EPH)
eph <- read_csv("Data/EPH_indiv_2016_2024_proc.csv", show_col_types = FALSE) %>%
  filter(!is.na(migrante)) %>%
  mutate(grupo_m = if_else(migrante == 1, "Migrante", "No migrante"),
         reciente = migrante == 1 & migrante_reciente == "Si")
arg <- bind_rows(
  resumir(eph %>% filter(edad >= 15, !is.na(estado_actividad)) %>% mutate(peso = PONDERA), "I1_tasa_actividad", "Argentina"),
  resumir(eph %>% filter(edad >= 15, estado_actividad %in% c("Ocupado", "Desocupado")) %>% mutate(peso = PONDERA), "I2_tasa_desempleo", "Argentina"),
  resumir(eph %>% filter(edad >= 15, !is.na(formalidad_empleo_asalariada)) %>% mutate(peso = PONDERA), "I3_informalidad_asalariada", "Argentina"),
  resumir(eph %>% filter(edad >= 15, !is.na(categoria_informalidad)) %>% mutate(peso = PONDERA), "I4_cuentapropismo", "Argentina"),
  resumir(eph %>% filter(!is.na(situacion)) %>% mutate(peso = PONDIH), "I5_I6_pobreza", "Argentina"),
  resumir(eph %>% filter(edad >= 6, edad <= 17, !is.na(asistencia_educacion)) %>% mutate(peso = PONDERA), "I7_asistencia_escolar_6_17", "Argentina"),
  resumir(eph %>% filter(edad >= 18, edad <= 29) %>% mutate(peso = PONDERA), "I8_asistencia_superior_18_29", "Argentina")
)
rm(eph); gc()

# ---- Chile (CASEN). Los indicadores de la ENE (actividad y desempleo de los
# migrantes totales) vienen publicados por el INE y no tienen microdatos acá.
casen <- read_csv("Data/Casen_unificado_2015_2024_proc.csv", show_col_types = FALSE) %>%
  filter(!is.na(migrante)) %>%
  mutate(grupo_m = if_else(migrante == "Migrantes", "Migrante", "No migrante"),
         reciente = migrante == "Migrantes" & migrante_reciente %in% "Si", peso = expr)
chl <- bind_rows(
  resumir(casen %>% filter(edad >= 15), "I1_tasa_actividad", "Chile"),
  resumir(casen %>% filter(estado_actividad %in% c("Ocupado", "Desocupado")), "I2_tasa_desempleo", "Chile"),
  resumir(casen %>% filter(!is.na(formalidad_empleo_asalariada)), "I3_informalidad_asalariada", "Chile"),
  resumir(casen %>% filter(condicion_ocupacional %in% 1:9), "I4_cuentapropismo", "Chile"),
  resumir(casen %>% filter(!is.na(pobreza)), "I5_I6_pobreza", "Chile"),
  resumir(casen %>% filter(edad >= 6, edad <= 17, !is.na(asiste)), "I7_asistencia_escolar_6_17", "Chile"),
  resumir(casen %>% filter(edad >= 18, edad <= 29), "I8_asistencia_superior_18_29", "Chile"),
  resumir(casen %>% filter(!is.na(acceso_salud),
                           !(anio %in% c(2015, 2017) & acceso_salud == 99),
                           !(anio %in% c(2022, 2024) & acceso_salud == -88)), "I9_cobertura_salud", "Chile")
)

calidad <- bind_rows(arg, chl) %>%
  mutate(semaforo = case_when(n >= 100 & n_efectivo >= 50 ~ "OK",
                              n >= 30 & n_efectivo >= 30 ~ "CUIDADO",
                              TRUE ~ "NO USAR"))
write_csv(calidad, "output/calidad/calidad_muestral.csv")

cat("\nCasos que no están en OK:\n")
print(calidad %>% filter(semaforo != "OK") %>% mutate(n_efectivo = round(n_efectivo)), n = Inf)
