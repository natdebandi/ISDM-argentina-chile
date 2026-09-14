# =============================================================================
# IMPACTO DEL FILTRO DE EDAD EN LA TASA DE ACTIVIDAD (ARGENTINA)
# =============================================================================
# Mide cuánto cambia la brecha nativo-migrante de la tasa de actividad según el
# denominador que se use, y documenta el error detectado el 2026-09-13.
#
# El problema, en una línea
# -------------------------
# `1_2_indicadores_EPH.R` construía el denominador de I1 con
# `filter(!is.na(estado_actividad) & !is.na(migrante))`, con un comentario que
# decía "PET" pero sin filtrar edad. Como los menores de 10 años tienen un
# `estado_actividad` NO nulo ("Menor de 10 años"), entraban al denominador y
# diluían la tasa. El numerador no se altera, porque un menor de 10 años nunca
# está ocupado ni desocupado.
#
# Chile no tenía el problema: `1_2_indicadores_casen.R` filtra `edad >= 15`.
# Es decir que el indicador que el artículo presenta como el más consistente
# entre países se calculaba sobre poblaciones distintas en cada país.
#
# Este script NO rehace el pipeline. Replica el cálculo sobre el dataset ya
# procesado (`Data/EPH_indiv_2016_2024_proc.csv`) cambiando sólo el denominador,
# que es un contrafactual limpio: los ponderadores, la definición de migrante y
# el numerador son los del pipeline, y lo único que varía es el umbral de edad.
#
# Uso
# ---
#   Rscript diagnosticos/impacto_filtro_edad.R
#
# Salidas
# -------
#   diagnosticos/impacto_filtro_edad.csv   brecha por año y por denominador
#   (y este mismo archivo como documentación del procedimiento)

suppressMessages({
  library(readr)
  library(dplyr)
})

ruta_datos <- "Data/EPH_indiv_2016_2024_proc.csv"
if (!file.exists(ruta_datos)) {
  ruta_datos <- "Data/EPH_indiv_2016_2024_proc_PRE-FIX.csv.bak"
}
stopifnot(file.exists(ruta_datos))

datos <- read_csv(
  ruta_datos,
  col_select = c(anio, edad, estado_actividad, ponderacion, migrante),
  show_col_types = FALSE)

datos <- datos %>%
  filter(!is.na(migrante), !is.na(ponderacion)) %>%
  mutate(
    grupo = if_else(migrante == 1, "migrante", "nativo"),
    activo = if_else(estado_actividad %in% c("Ocupado", "Desocupado"), 1, 0)
  )

# Tasa de actividad para un subconjunto definido por su propio denominador.
# El umbral de edad se aplica a numerador y denominador por igual.
tasa <- function(d, minimo) {
  d %>%
    filter(edad >= minimo) %>%
    group_by(anio, grupo) %>%
    summarise(
      activos = sum(ponderacion[activo == 1], na.rm = TRUE),
      poblacion = sum(ponderacion, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(tasa = 100 * activos / poblacion)
}

brecha <- function(d, minimo) {
  tasa(d, minimo) %>%
    select(anio, grupo, tasa) %>%
    tidyr::pivot_wider(names_from = grupo, values_from = tasa) %>%
    mutate(brecha = nativo - migrante)
}

# "actual" = lo que reportaba el pipeline: sin umbral de edad.
# En la práctica, la rama sin filtro sólo difiere de PET 10+ en los años en que
# los menores de 10 años llegaron al dataset con un estado_actividad no nulo.
escenarios <- list(
  actual  = brecha(datos, 0),
  pet_10  = brecha(datos, 10),
  pet_15  = brecha(datos, 15)
)

resumen <- escenarios$actual %>%
  transmute(anio,
            actual = brecha,
            pet_10 = escenarios$pet_10$brecha,
            pet_15 = escenarios$pet_15$brecha)

# El artículo excluye 2020 del análisis principal por la pandemia.
sin2020 <- resumen %>% filter(anio != 2020)

cat("\n===== BRECHA DE TASA DE ACTIVIDAD (nativo - migrante), en puntos =====\n")
cat("positivo = los nativos tienen mayor tasa de actividad\n\n")
print(as.data.frame(sin2020), row.names = FALSE, digits = 3)

cat("\n===== PROMEDIOS (excluyendo 2020) =====\n")
cat(sprintf("  como lo reportaba el pipeline (sin filtro de edad) : %+.2f pp\n",
            mean(sin2020$actual)))
cat(sprintf("  PET 10+, definición oficial del INDEC              : %+.2f pp\n",
            mean(sin2020$pet_10)))
cat(sprintf("  PET 15+, para comparar con CASEN (Chile)           : %+.2f pp\n",
            mean(sin2020$pet_15)))
cat("\n")
cat(sprintf("  Chile, calculado por el pipeline                   : -18.9 pp\n"))
cat("  (el artículo lo compara contra Argentina, que sin filtro daba -9.2 pp)\n")

# Peso de los menores de 10 años, que es la causa de la diferencia.
peso <- datos %>%
  mutate(menor10 = edad < 10) %>%
  group_by(anio, grupo) %>%
  summarise(pct_menor10 = 100 * sum(ponderacion[menor10], na.rm = TRUE) /
                                sum(ponderacion, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = grupo, values_from = pct_menor10)

cat("\n===== PESO DE LOS MENORES DE 10 AÑOS EN CADA GRUPO =====\n")
print(as.data.frame(peso), row.names = FALSE, digits = 3)

write_csv(resumen, "diagnosticos/impacto_filtro_edad.csv")
cat("\nEscrito: diagnosticos/impacto_filtro_edad.csv\n")
