# =============================================================================
# 2_6_ISDM_v2.R — ISDM corregido, análisis de sensibilidad y salidas (22/11/2026)
# =============================================================================
# Reemplaza para el artículo a 2_1, 2_3 y 2_4, que se conservan sin cambios.
# Requiere haber corrido antes, en este orden:
#   1_2_indicadores_EPH_v2.R, 1_2_indicadores_casen_v2.R,
#   1_4_indicadores_unificados_v2.R, 1_6_construccion_brechas_v2.Rmd
# Salidas en output_isdm_v2/ (no pisa ninguna salida anterior).
# =============================================================================

rm(list = ls())
suppressPackageStartupMessages({ library(tidyverse) })
source("R/isdm_funciones.R")
dir.create("output_isdm_v2", showWarnings = FALSE)

datos <- cargar_brechas_v2()

# -----------------------------------------------------------------------------
# 1. Especificación principal: salud = cobertura (Chile), escala = dispersión individual (d)
# -----------------------------------------------------------------------------
prep  <- preparar_isdm(datos, salud = "cobertura", escala = "dispersion")
res   <- calcular_isdm(prep)

write_csv(prep,                     "output_isdm_v2/brechas_estandarizadas.csv")
write_csv(res$anual,                "output_isdm_v2/isdm_anual.csv")
write_csv(res$resumen,              "output_isdm_v2/isdm_resumen_pais.csv")         # Tabla 6 v2
write_csv(res$subindices_resumen,   "output_isdm_v2/subindices_resumen.csv")        # Gráfico 6 v2

# Tablas de brechas por indicador (Tablas 2, 4 y 5 del artículo), promedio sin 2020
brechas_indicador <- prep %>%
  group_by(dimension, indicador, pais, grupo) %>%
  summarise(brecha_pp = mean(brecha_normalizada, na.rm = TRUE), n_anios = n(), .groups = "drop") %>%
  arrange(dimension, indicador, grupo, pais)
write_csv(brechas_indicador, "output_isdm_v2/brechas_por_indicador.csv")

# -----------------------------------------------------------------------------
# 2. Documentación del cambio: misma especificación con las escalas anteriores
# -----------------------------------------------------------------------------
res_centrado  <- calcular_isdm(preparar_isdm(datos, salud = "cobertura", escala = "de_brechas", centrar = TRUE))
res_de_brecha <- calcular_isdm(preparar_isdm(datos, salud = "cobertura", escala = "de_brechas", centrar = FALSE))
comparacion <- res$resumen %>%
  select(pais, grupo, abs_d = isdm_abs_de, dir_d = isdm_dir_de, abs_pp = isdm_abs_pp, dir_pp = isdm_dir_pp) %>%
  left_join(res_de_brecha$resumen %>% select(pais, grupo, abs_de_brechas = isdm_abs_de, dir_de_brechas = isdm_dir_de),
            by = c("pais", "grupo")) %>%
  left_join(res_centrado$resumen %>% select(pais, grupo, abs_centrado_original = isdm_abs_de, dir_centrado_original = isdm_dir_de),
            by = c("pais", "grupo"))
write_csv(comparacion, "output_isdm_v2/comparacion_escalas.csv")

# -----------------------------------------------------------------------------
# 3. Análisis de sensibilidad del indicador de salud (Apéndice)
# -----------------------------------------------------------------------------
escenarios <- c(
  "S0 Original: uso del sistema público (valencia +)" = "publico",
  "S1 Sin indicador de salud"                          = "sin_salud",
  "S2 Uso del sistema público (valencia -)"            = "publico_invertido",
  "S3 Principal: cobertura (Chile), sin indicador ARG" = "cobertura"
)
sensibilidad <- imap_dfr(escenarios, function(spec, nombre) {
  r <- calcular_isdm(preparar_isdm(datos, salud = spec, escala = "dispersion"))
  social <- r$subindices_resumen %>%
    filter(dimension == "Social") %>%
    select(pais, grupo, social_dir_pp = sub_dir_pp)
  r$resumen %>%
    left_join(social, by = c("pais", "grupo")) %>%
    mutate(escenario = nombre, .before = 1)
})
write_csv(sensibilidad, "output_isdm_v2/sensibilidad_salud.csv")

# -----------------------------------------------------------------------------
# 4. Gráficos (etiquetas en inglés, como los del manuscrito)
# -----------------------------------------------------------------------------
colores_pais <- c("Argentina" = "#2E86AB", "Chile" = "#A23B72")

g5 <- ggplot(res$anual, aes(anio, isdm_abs_pp, colour = pais, linetype = grupo)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_colour_manual(values = colores_pais) +
  scale_x_continuous(breaks = sort(unique(res$anual$anio))) +
  labs(x = "Year", y = "Inequality magnitude (pp, distance to parity)",
       colour = "Country", linetype = "Group",
       caption = "2020 excluded. Chilean poverty and social indicators interpolated between CASEN waves.") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")
ggsave("output_isdm_v2/fig5_isdm_absoluto_pp.png", g5, width = 12, height = 6, dpi = 300)

g5b <- g5 + aes(y = isdm_abs_de) + labs(y = "Inequality magnitude (standardized gap, d)")
ggsave("output_isdm_v2/fig5b_isdm_absoluto_d.png", g5b, width = 12, height = 6, dpi = 300)

g6 <- res$subindices_resumen %>%
  mutate(dimension = factor(dimension, levels = c("Labor", "Poverty", "Social"))) %>%
  ggplot(aes(sub_dir_pp, dimension, fill = pais)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.9) +
  geom_vline(xintercept = 0, linewidth = 0.8) +
  geom_text(aes(label = sprintf("%+.2f", sub_dir_pp),
                hjust = if_else(sub_dir_pp >= 0, -0.15, 1.15)),
            position = position_dodge(width = 0.8), size = 3.5) +
  facet_wrap(~ grupo) +
  scale_fill_manual(values = colores_pais) +
  scale_x_continuous(expand = expansion(mult = 0.25)) +
  labs(x = "Directional sub-index (pp)\nNegative = Migrants better | Positive = Non-migrants better",
       y = NULL, fill = "Country") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")
ggsave("output_isdm_v2/fig6_subindices_direccionales.png", g6, width = 12, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# 5. Resumen en consola
# -----------------------------------------------------------------------------
cat("\nISDM v2 (principal: salud = cobertura, escala d), promedio sin 2020\n")
print(res$resumen %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
cat("\nSensibilidad (pp):\n")
print(sensibilidad %>% select(escenario, pais, grupo, isdm_abs_de, isdm_dir_de, isdm_abs_pp, isdm_dir_pp, social_dir_pp) %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))))
