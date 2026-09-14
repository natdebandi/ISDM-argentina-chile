# =============================================================================
# I1 — LAS TRES BRECHAS BAJO CADA DENOMINADOR, CON EL FILTRO DEL PIPELINE
# =============================================================================
# Los dos diagnósticos anteriores medían una sola brecha cada uno y con un filtro
# propio, no el del pipeline. Como el denominador de I1 depende de cuáles filas
# entran, un filtro distinto da un número distinto y la comparación deja de ser
# válida. Acá se replica exactamente el bloque I1 de 1_2_indicadores_EPH.R
# (mismo filter, misma definición de reciente) y lo único que se mueve entre
# escenarios es PET_MIN_EDAD.
#
# Uso:  Rscript diagnosticos/i1_escenarios.R
# Sale: diagnosticos/i1_escenarios.csv

suppressMessages({
  library(readr)
  library(dplyr)
})

d <- read_csv("Data/EPH_indiv_2016_2024_proc.csv", show_col_types = FALSE)

# Réplica del bloque I1, parametrizando sólo el umbral de edad.
brechas <- function(min_edad) {
  base <- d %>%
    filter(edad >= min_edad & !is.na(estado_actividad) & !is.na(migrante)) %>%
    mutate(activo = ifelse(estado_actividad %in% c("Ocupado", "Desocupado"), 1, 0))

  tasa <- function(sub) {
    sub %>%
      group_by(anio) %>%
      summarise(
        activos = sum(ponderacion[activo == 1], na.rm = TRUE),
        poblacion = sum(ponderacion, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(tasa = 100 * activos / poblacion) %>%
      select(anio, tasa)
  }

  nat <- tasa(filter(base, migrante == 0))
  mig <- tasa(filter(base, migrante == 1))
  rec <- tasa(filter(base, migrante == 1, migrante_reciente == "Si"))

  nat %>%
    left_join(mig, by = "anio", suffix = c("_nat", "_mig")) %>%
    left_join(rec, by = "anio") %>%
    rename(tasa_rec = tasa) %>%
    mutate(
      brecha_nat_mig = tasa_nat - tasa_mig,
      brecha_nat_rec = tasa_nat - tasa_rec,
      pet = min_edad
    )
}

res <- bind_rows(brechas(0), brechas(10), brechas(15)) %>%
  select(pet, anio, tasa_nat, tasa_mig, tasa_rec, brecha_nat_mig, brecha_nat_rec)

sin2020 <- res %>% filter(anio != 2020)

cat("\n===== PROMEDIOS SIN 2020, POR UMBRAL DE PET =====\n\n")
cat(sprintf("%-6s %14s %14s\n", "PET", "nat-migrante", "nat-reciente"))
for (p in c(0, 10, 15)) {
  s <- sin2020 %>% filter(pet == p)
  cat(sprintf("%-6s %+14.2f %+14.2f\n", paste0(p, "+"),
              mean(s$brecha_nat_mig), mean(s$brecha_nat_rec)))
}

cat("\n===== DETALLE POR AÑO =====\n\n")
print(as.data.frame(sin2020 %>% arrange(pet, anio)), row.names = FALSE, digits = 4)

write_csv(res, "diagnosticos/i1_escenarios.csv")
cat("\nEscrito: diagnosticos/i1_escenarios.csv\n")
