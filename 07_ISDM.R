# =============================================================================
# 7. Índice Sintético de Desigualdad Migratoria (ISDM)
# =============================================================================
# Lee:      los dos CSV de brechas del paso 6 (funciones en R/isdm_funciones.R).
# Hace:     estandariza cada brecha como diferencia de proporciones (d =
#           brecha / sqrt(p(1-p))), agrega en subíndices por dimensión y en el
#           ISDM (absoluto y direccional, en d y en pp), compara con las escalas
#           anteriores y calcula la sensibilidad del indicador de salud (S0 a S3).
# Escribe:  output/isdm/ (Tabla 4, Tablas 1 a 3, Tabla A2 y series anuales)
# Detalle de cada decisión: ISDM_metodologia_trazabilidad.docx (carpeta del artículo,
# VERSION_FINAL), códigos de problema P1 a P22.
# =============================================================================

rm(list = ls())
suppressPackageStartupMessages({ library(tidyverse) })
source("R/isdm_funciones.R")
dir.create("output/isdm", showWarnings = FALSE, recursive = TRUE)

datos <- cargar_brechas()

# -----------------------------------------------------------------------------
# 1. Especificación principal: salud = cobertura (Chile), escala = dispersión individual (d)
# -----------------------------------------------------------------------------
prep  <- preparar_isdm(datos, salud = "cobertura", escala = "dispersion")
res   <- calcular_isdm(prep)

write_csv(prep,                     "output/isdm/brechas_estandarizadas.csv")
write_csv(res$anual,                "output/isdm/isdm_anual.csv")
write_csv(res$resumen,              "output/isdm/isdm_resumen_pais.csv")         # Tabla 4 del artículo
write_csv(res$subindices_resumen,   "output/isdm/subindices_resumen.csv")        # Gráfico 6 del artículo

# Tablas de brechas por indicador (Tablas 2, 4 y 5 del artículo), promedio sin 2020
brechas_indicador <- prep %>%
  group_by(dimension, indicador, pais, grupo) %>%
  summarise(brecha_pp = mean(brecha_normalizada, na.rm = TRUE), n_anios = n(), .groups = "drop") %>%
  arrange(dimension, indicador, grupo, pais)
write_csv(brechas_indicador, "output/isdm/brechas_por_indicador.csv")

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
write_csv(comparacion, "output/isdm/comparacion_escalas.csv")

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
write_csv(sensibilidad, "output/isdm/sensibilidad_salud.csv")

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
ggsave("output/isdm/fig5_isdm_absoluto_pp.png", g5, width = 12, height = 6, dpi = 300)

g5b <- g5 + aes(y = isdm_abs_de) + labs(y = "Inequality magnitude (standardized gap, d)")
ggsave("output/isdm/fig5b_isdm_absoluto_d.png", g5b, width = 12, height = 6, dpi = 300)

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
ggsave("output/isdm/fig6_subindices_direccionales.png", g6, width = 12, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# 5. Resumen en consola
# -----------------------------------------------------------------------------
cat("\nISDM (principal: salud = cobertura, escala d), promedio sin 2020\n")
print(res$resumen %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
cat("\nSensibilidad (pp):\n")
print(sensibilidad %>% select(escenario, pais, grupo, isdm_abs_de, isdm_dir_de, isdm_abs_pp, isdm_dir_pp, social_dir_pp) %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))))
