# =============================================================================
# VERSIÓN v3 (23/11/2026). Ver VERSION_FINAL/ISDM_metodologia_trazabilidad.docx
# (carpeta del artículo), problemas P12 y P18 a P22.
# =============================================================================
# Igual que 2_8_graficos_articulo_v2.R, con los datos v3. Salidas en output_isdm_v3/articulo/.
# =============================================================================
# 2_8_graficos_articulo_v2.R — Gráficos 2 a 6 del artículo con los datos v2
# (22/11/2026). Mismo diseño y proporciones que los de 2_4, para reemplazarlos
# en el manuscrito. Salidas en output_isdm_v3/articulo/.
# =============================================================================
rm(list = ls())
suppressPackageStartupMessages({ library(tidyverse) })
source("R/isdm_funciones.R")
dir.create("output_isdm_v3/articulo", showWarnings = FALSE, recursive = TRUE)

prep <- preparar_isdm(cargar_brechas_v2("Data/indicadores_con_brechas_arg_chile_2015_2024_v3.csv", "Data/indicadores_con_brechas_solocasen_arg_chile_2015_2024_v3.csv"), salud = "cobertura", escala = "dispersion")
res  <- calcular_isdm(prep)
colores_pais <- c("Argentina" = "#2E86AB", "Chile" = "#A23B72")
etiquetas_ind <- c(
  "I1_tasa_actividad" = "Activity rate (15+)", "I2_tasa_desempleo" = "Unemployment rate",
  "I3_informalidad_asalariada" = "Salaried informality", "I4_cuentapropismo" = "Self-employment",
  "I5_pobreza" = "Monetary poverty", "I6_pobreza_extrema" = "Extreme poverty",
  "I7_asistencia_escolar_6_17" = "School attendance (6-17)",
  "I8_asistencia_superior_18_29" = "Higher education attendance (18-29)",
  "I9_cobertura_salud" = "Health coverage (Chile)")

grafico_dimension <- function(dim, archivo, alto) {
  p <- prep %>%
    filter(dimension == dim) %>%
    mutate(ind_num = as.integer(str_extract(indicador, "(?<=I)\\d+")),
           etiqueta = paste0(str_extract(indicador, "I\\d+"), " - ", etiquetas_ind[indicador]),
           etiqueta = fct_reorder(etiqueta, ind_num)) %>%
    ggplot(aes(x = etiqueta, y = brecha_normalizada, color = pais)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
    geom_boxplot(alpha = 0.5, position = position_dodge(0.8), outlier.shape = NA) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8, seed = 1),
                alpha = 0.6, size = 2) +
    coord_flip() +
    scale_color_manual(values = colores_pais, name = "Country") +
    facet_wrap(~ grupo, ncol = 2) +
    labs(x = "", y = "Normalized gap (pp)\nLeft (negative) = Migrants better | Right (positive) = Non-migrants better") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom", strip.text = element_text(face = "bold", size = 13),
          axis.text = element_text(size = 11, color = "black"), panel.grid.minor = element_blank())
  ggsave(archivo, p, width = 14, height = alto, dpi = 300)
}
grafico_dimension("Labor",   "output_isdm_v3/articulo/grafico2_laboral.png", 6)
grafico_dimension("Poverty", "output_isdm_v3/articulo/grafico3_pobreza.png", 5)
grafico_dimension("Social",  "output_isdm_v3/articulo/grafico4_social.png", 6)

g5 <- ggplot(res$anual, aes(anio, isdm_abs_de, colour = pais, linetype = grupo)) +
  geom_line(linewidth = 1.2) + geom_point(size = 2.5) +
  scale_colour_manual(values = colores_pais) +
  scale_x_continuous(breaks = sort(unique(res$anual$anio))) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "Year", y = "Inequality magnitude (standardized gap, d)\n0 = parity",
       colour = "Country", linetype = "Group") +
  theme_minimal(base_size = 13) + theme(legend.position = "bottom", panel.grid.minor = element_blank())
ggsave("output_isdm_v3/articulo/grafico5_isdm_absoluto.png", g5, width = 12, height = 6, dpi = 300)

g6 <- res$subindices_resumen %>%
  mutate(dimension = factor(dimension, levels = c("Labor", "Poverty", "Social"))) %>%
  ggplot(aes(sub_dir_pp, dimension, fill = pais)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.9) +
  geom_vline(xintercept = 0, linewidth = 0.8) +
  geom_text(aes(label = sprintf("%+.2f", sub_dir_pp), hjust = if_else(sub_dir_pp >= 0, -0.15, 1.15)),
            position = position_dodge(width = 0.8), size = 4, fontface = "bold") +
  facet_wrap(~ grupo) +
  scale_fill_manual(values = colores_pais) +
  scale_x_continuous(expand = expansion(mult = 0.25)) +
  labs(x = "Directional sub-index (pp)\nNegative = Migrants better | Positive = Non-migrants better",
       y = NULL, fill = "Country") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold", size = 13))
ggsave("output_isdm_v3/articulo/grafico6_subindices.png", g6, width = 12, height = 5, dpi = 300)
cat("Gráficos generados\n")
