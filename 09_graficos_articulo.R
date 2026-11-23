# =============================================================================
# 9. Gráficos 2 a 6 del artículo
# =============================================================================
# Lee:      los dos CSV de brechas del paso 6.
# Escribe:  output/articulo/grafico2_laboral.png ... grafico6_subindices.png
#           (etiquetas en inglés).
# Detalle de cada decisión: ISDM_metodologia_trazabilidad.docx (carpeta del artículo,
# VERSION_FINAL), códigos de problema P1 a P22.
# =============================================================================

rm(list = ls())
suppressPackageStartupMessages({ library(tidyverse) })
source("R/isdm_funciones.R")
dir.create("output/articulo", showWarnings = FALSE, recursive = TRUE)

prep <- preparar_isdm(cargar_brechas(), salud = "cobertura", escala = "dispersion")
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
           etiqueta = fct_rev(fct_reorder(etiqueta, ind_num))) %>%   # I1 arriba (coord_flip invierte el eje)
    ggplot(aes(x = etiqueta, y = brecha_normalizada, color = pais)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
    geom_boxplot(alpha = 0.5, position = position_dodge(0.8), outlier.shape = NA) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8, seed = 1),
                alpha = 0.6, size = 1.3) +
    coord_flip() +
    scale_color_manual(values = colores_pais, name = "Country") +
    facet_wrap(~ grupo, ncol = 2) +
    scale_x_discrete(labels = scales::label_wrap(24)) +
    labs(x = "", y = "Normalized gap (pp)\nNegative = migrants better | Positive = non-migrants better") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom", strip.text = element_text(face = "bold", size = 10),
          axis.text = element_text(size = 9, color = "black"), panel.grid.minor = element_blank(),
          legend.margin = margin(0, 0, 0, 0), plot.margin = margin(4, 6, 4, 2))
  # Tamaño real del gráfico en el Word (16,5 cm de ancho): el texto queda en 9-10 pt
  ggsave(archivo, p, width = 6.5, height = alto, dpi = 300, bg = "white")
}
grafico_dimension("Labor",   "output/articulo/grafico2_laboral.png", 4.2)
grafico_dimension("Poverty", "output/articulo/grafico3_pobreza.png", 2.9)
grafico_dimension("Social",  "output/articulo/grafico4_social.png", 3.6)

g5 <- ggplot(res$anual, aes(anio, isdm_abs_de, colour = pais, linetype = grupo)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.6) +
  scale_colour_manual(values = colores_pais) +
  scale_x_continuous(breaks = sort(unique(res$anual$anio))) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = NULL, y = "Inequality magnitude (d, 0 = parity)",
       colour = "Country", linetype = "Group") +
  theme_minimal(base_size = 10) + theme(legend.position = "bottom", legend.box = "horizontal",
                                        legend.margin = margin(0, 0, 0, 0), panel.grid.minor = element_blank())
ggsave("output/articulo/grafico5_isdm_absoluto.png", g5, width = 6.5, height = 3.8, dpi = 300, bg = "white")

g6 <- res$subindices_resumen %>%
  mutate(dimension = factor(dimension, levels = c("Social", "Poverty", "Labor"))) %>%   # Labor arriba
  ggplot(aes(sub_dir_pp, dimension, fill = pais)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.9) +
  geom_vline(xintercept = 0, linewidth = 0.8) +
  geom_text(aes(label = sprintf("%+.2f", sub_dir_pp), hjust = if_else(sub_dir_pp >= 0, -0.15, 1.15)),
            position = position_dodge(width = 0.8), size = 3, fontface = "bold") +
  facet_wrap(~ grupo) +
  scale_fill_manual(values = colores_pais) +
  scale_x_continuous(expand = expansion(mult = 0.25)) +
  labs(x = "Directional sub-index (pp)\nNegative = Migrants better | Positive = Non-migrants better",
       y = NULL, fill = "Country") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold", size = 10),
        axis.text.y = element_text(size = 10, color = "black"), legend.margin = margin(0, 0, 0, 0))
ggsave("output/articulo/grafico6_subindices.png", g6, width = 6.5, height = 3.2, dpi = 300, bg = "white")
cat("Gráficos generados\n")
