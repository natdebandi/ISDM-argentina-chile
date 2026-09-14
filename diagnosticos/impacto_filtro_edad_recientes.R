# =============================================================================
# IMPACTO DEL FILTRO DE EDAD EN I1 — MIGRANTES RECIENTES
# =============================================================================
# Complemento de impacto_filtro_edad.R, que sólo mide la brecha contra el total
# de migrantes. Acá se mide la brecha contra migrantes recientes, que es la
# columna `brecha_nat_rec` y la que alimenta la variante 2_3 del ISDM.
#
# El error de fondo es el mismo: el denominador de I1 no filtraba edad, y los
# menores de 10 años (etiqueta no nula en estado_actividad) entraban a la PET.
# Como el peso de esa franja difiere entre nativos, migrantes y migrantes
# recientes, la magnitud del sesgo también difiere en cada brecha.
#
# Uso:  Rscript diagnosticos/impacto_filtro_edad_recientes.R
# Sale: diagnosticos/impacto_filtro_edad_recientes.csv

suppressMessages({
  library(readr)
  library(dplyr)
})

ruta <- "Data/EPH_indiv_2016_2024_proc.csv"
stopifnot(file.exists(ruta))

d <- read_csv(
  ruta,
  col_select = c(anio, edad, estado_actividad, ponderacion, migrante, migrante_reciente),
  show_col_types = FALSE
)

cat("Valores de migrante_reciente:", paste(sort(unique(d$migrante_reciente)), collapse = ", "), "\n")
cat("NA en migrante_reciente:", sum(is.na(d$migrante_reciente)), "\n\n")

# Ojo con los tipos: `migrante` es 0/1 numérico, pero `migrante_reciente` viene como
# texto ("Si"/"No"). Comparar la segunda contra 1 no matchea nunca y el resultado
# sale NaN en silencio, que fue lo que pasó en la primera corrida.
d <- d %>%
  filter(!is.na(ponderacion)) %>%
  mutate(
    activo = if_else(estado_actividad %in% c("Ocupado", "Desocupado"), 1, 0),
    es_reciente = migrante_reciente == "Si"
  )

stopifnot(sum(d$es_reciente, na.rm = TRUE) > 0)

# Tasa para un grupo, con umbral de edad aplicado a numerador y denominador.
tasa <- function(dd, minimo) {
  dd %>%
    filter(edad >= minimo) %>%
    group_by(anio) %>%
    summarise(
      nat = 100 * sum(ponderacion[migrante == 0 & activo == 1], na.rm = TRUE) /
                  sum(ponderacion[migrante == 0], na.rm = TRUE),
      rec = 100 * sum(ponderacion[es_reciente & activo == 1], na.rm = TRUE) /
                  sum(ponderacion[es_reciente], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(brecha = nat - rec)
}

actual <- tasa(d, 0)
pet10  <- tasa(d, 10)

resumen <- actual %>%
  transmute(anio, actual = brecha, pet_10 = pet10$brecha)

sin2020 <- resumen %>% filter(anio != 2020)

cat("===== BRECHA DE ACTIVIDAD nativo - migrante RECIENTE (pp) =====\n")
cat("positivo = los nativos tienen mayor tasa de actividad\n\n")
print(as.data.frame(sin2020), row.names = FALSE, digits = 4)

cat(sprintf("\npromedio sin 2020 — sin filtro : %+.2f pp\n", mean(sin2020$actual)))
cat(sprintf("promedio sin 2020 — PET 10+   : %+.2f pp\n", mean(sin2020$pet_10)))

write_csv(resumen, "diagnosticos/impacto_filtro_edad_recientes.csv")
cat("\nEscrito: diagnosticos/impacto_filtro_edad_recientes.csv\n")
