# =============================================================================
# VERSIÓN v3 (23/11/2026). Ver VERSION_FINAL/ISDM_metodologia_trazabilidad.docx
# (carpeta del artículo), problemas P12 y P18 a P22.
# =============================================================================
# Igual que 2_7_composicion_v2.R, con la CASEN procesada v3. Salidas en output_isdm_v3/.
# =============================================================================
# 2_7_composicion_v2.R — Composición de las poblaciones y control por edad
# (22/11/2026)
# =============================================================================
# Responde a Adrian Favell (selección, edad, nivel socioeconómico) y a A3/A4:
#  1. Tabla de composición por país y grupo: edad, sexo, nivel educativo
#     (25 años y más), proporción de recientes y, en Argentina, de limítrofes.
#  2. Tasa de actividad estandarizada por edad (método directo): la tasa
#     migrante se recalcula con la estructura etaria de la población no
#     migrante del mismo país y año, en grupos quinquenales de 15 años y más.
# Fuentes: EPH (procesada) y CASEN (procesada v2). En Chile se usa la CASEN
# también para actividad, porque la ENE no tiene microdatos en el proyecto.
# Promedios sin 2020. Salidas en output_isdm_v3/.
# =============================================================================

rm(list = ls())
suppressPackageStartupMessages({ library(tidyverse) })
dir.create("output_isdm_v3", showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Datos armonizados (una fila por persona)
# -----------------------------------------------------------------------------
eph <- read_csv("Data/EPH_indiv_2016_2024_proc.csv", show_col_types = FALSE,
                col_select = c(anio, PONDERA, edad, sexo, NIVEL_ED, estado_actividad,
                               migrante, migrante_reciente, pais_nacimiento)) %>%
  filter(!is.na(migrante), !is.na(edad)) %>%
  transmute(
    pais = "Argentina", anio, peso = PONDERA, edad,
    mujer = as.integer(sexo == "Mujer"),
    # NIVEL_ED: 7 = sin instrucción, 1-3 = hasta secundario incompleto,
    # 4-5 = secundario completo o superior incompleto, 6 = superior completo
    educ3 = case_when(NIVEL_ED %in% c(7, 1, 2, 3) ~ "Hasta secundaria incompleta",
                      NIVEL_ED %in% c(4, 5)       ~ "Secundaria completa o superior incompleta",
                      NIVEL_ED == 6               ~ "Superior completa"),
    activo = case_when(estado_actividad %in% c("Ocupado", "Desocupado") ~ 1L,
                       estado_actividad == "Inactivo" ~ 0L),
    grupo_m = if_else(migrante == 1, "Migrante", "No migrante"),
    reciente = migrante == 1 & migrante_reciente == "Si",
    limitrofe = migrante == 1 & pais_nacimiento == "En un pais limitrofe"
  )

casen <- read_csv("Data/Casen_unificado_2015_2024_proc_v3.csv", show_col_types = FALSE,
                  col_select = c(anio, expr, edad, sexo, educ, estado_actividad,
                                 migrante, migrante_reciente)) %>%
  filter(!is.na(migrante), !is.na(edad)) %>%
  transmute(
    pais = "Chile", anio, peso = expr, edad,
    mujer = as.integer(sexo == 2),
    # educ (2015, 2017, 2022): 0-4 hasta media incompleta; 5, 6, 7, 9 media
    # completa o superior incompleta; 8, 10, 11, 12 superior completa.
    # educc (2020, 2024): 0-3 hasta media incompleta; 4-5 media completa o
    # superior incompleta; 6 superior completa. [verificar contra libro de códigos]
    educ3 = case_when(
      anio %in% c(2015, 2017, 2022) & educ %in% 0:4            ~ "Hasta secundaria incompleta",
      anio %in% c(2015, 2017, 2022) & educ %in% c(5, 6, 7, 9)  ~ "Secundaria completa o superior incompleta",
      anio %in% c(2015, 2017, 2022) & educ %in% c(8, 10, 11, 12) ~ "Superior completa",
      anio %in% c(2020, 2024) & educ %in% 0:3 ~ "Hasta secundaria incompleta",
      anio %in% c(2020, 2024) & educ %in% 4:5 ~ "Secundaria completa o superior incompleta",
      anio %in% c(2020, 2024) & educ == 6     ~ "Superior completa"),
    activo = case_when(estado_actividad %in% c("Ocupado", "Desocupado") ~ 1L,
                       estado_actividad == "Inactivo" ~ 0L),
    grupo_m = if_else(migrante == "Migrantes", "Migrante", "No migrante"),
    reciente = migrante == "Migrantes" & migrante_reciente %in% "Si",
    limitrofe = NA
  )

personas <- bind_rows(eph, casen) %>% filter(anio != 2020)

# Tres grupos de comparación: no migrante, migrante (total) y migrante reciente
expandir_grupos <- function(d) {
  bind_rows(
    d %>% mutate(grupo = grupo_m),
    d %>% filter(reciente) %>% mutate(grupo = "Migrante reciente")
  )
}
pg <- expandir_grupos(personas)

# -----------------------------------------------------------------------------
# 1. Composición: promedio de los valores anuales (sin 2020)
# -----------------------------------------------------------------------------
wmean <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)])

comp_anual <- pg %>%
  group_by(pais, anio, grupo) %>%
  summarise(
    edad_media        = wmean(edad, peso),
    pct_menor_15      = 100 * wmean(edad < 15, peso),
    pct_15_64         = 100 * wmean(edad >= 15 & edad <= 64, peso),
    pct_65_mas        = 100 * wmean(edad >= 65, peso),
    pct_mujeres       = 100 * wmean(mujer, peso),
    pct_reciente      = 100 * wmean(reciente, peso),
    pct_limitrofe     = 100 * wmean(limitrofe, peso),
    .groups = "drop"
  )

educ_anual <- pg %>%
  filter(edad >= 25, !is.na(educ3)) %>%
  count(pais, anio, grupo, educ3, wt = peso) %>%
  group_by(pais, anio, grupo) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = educ3, values_from = pct, names_prefix = "educ25_")

comp <- comp_anual %>%
  left_join(educ_anual, by = c("pais", "anio", "grupo")) %>%
  group_by(pais, grupo) %>%
  summarise(across(where(is.numeric) & !anio, ~ mean(.x, na.rm = TRUE)),
            anios = paste(range(anio), collapse = "-"), .groups = "drop") %>%
  mutate(grupo = factor(grupo, levels = c("No migrante", "Migrante", "Migrante reciente"))) %>%
  arrange(pais, grupo)
# pct_reciente y pct_limitrofe solo tienen sentido para el grupo Migrante
comp <- comp %>%
  mutate(pct_reciente  = if_else(grupo == "Migrante", pct_reciente, NA_real_),
         pct_limitrofe = if_else(grupo == "Migrante" & pais == "Argentina", pct_limitrofe, NA_real_))
write_csv(comp, "output_isdm_v3/composicion_poblaciones.csv")
write_csv(comp_anual %>% left_join(educ_anual, by = c("pais", "anio", "grupo")),
          "output_isdm_v3/composicion_poblaciones_anual.csv")

# -----------------------------------------------------------------------------
# 2. Tasa de actividad estandarizada por edad (método directo)
# -----------------------------------------------------------------------------
act <- pg %>%
  filter(edad >= 15, !is.na(activo)) %>%
  mutate(edad_g = cut(edad, breaks = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, Inf),
                      right = FALSE))

tasas_edad <- act %>%
  group_by(pais, anio, grupo, edad_g) %>%
  summarise(tasa = wmean(activo, peso), pob = sum(peso), .groups = "drop")

estructura_ref <- tasas_edad %>%
  filter(grupo == "No migrante") %>%
  group_by(pais, anio) %>%
  mutate(w_ref = pob / sum(pob)) %>%
  ungroup() %>%
  select(pais, anio, edad_g, w_ref, tasa_ref = tasa)

act_std <- tasas_edad %>%
  left_join(estructura_ref, by = c("pais", "anio", "edad_g")) %>%
  group_by(pais, anio, grupo) %>%
  summarise(
    tasa_bruta        = 100 * sum(tasa * pob) / sum(pob),
    tasa_estandarizada = 100 * sum(tasa * w_ref, na.rm = TRUE) / sum(w_ref[!is.na(tasa)]),
    .groups = "drop"
  )

brechas_act <- act_std %>%
  filter(grupo != "No migrante") %>%
  left_join(act_std %>% filter(grupo == "No migrante") %>%
              select(pais, anio, tasa_no_migrante = tasa_bruta),
            by = c("pais", "anio")) %>%
  mutate(brecha_bruta        = tasa_no_migrante - tasa_bruta,
         brecha_estandarizada = tasa_no_migrante - tasa_estandarizada)

write_csv(brechas_act, "output_isdm_v3/actividad_estandarizada_edad_anual.csv")
resumen_act <- brechas_act %>%
  group_by(pais, grupo) %>%
  summarise(across(c(tasa_no_migrante, tasa_bruta, tasa_estandarizada,
                     brecha_bruta, brecha_estandarizada), mean), .groups = "drop")
write_csv(resumen_act, "output_isdm_v3/actividad_estandarizada_edad.csv")

cat("\nComposición (promedio sin 2020):\n")
print(comp %>% mutate(across(where(is.numeric), ~ round(.x, 1))), width = Inf)
cat("\nTasa de actividad 15+ bruta y estandarizada por edad (pp, promedio sin 2020):\n")
print(resumen_act %>% mutate(across(where(is.numeric), ~ round(.x, 1))), width = Inf)
