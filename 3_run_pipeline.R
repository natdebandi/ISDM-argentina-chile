# =============================================================================
# RUNNER DEL PIPELINE — ISDM ARGENTINA Y CHILE
# =============================================================================
# Ejecuta los pasos del pipeline en orden y se detiene en el primer error.
#
# Por qué existe este archivo
# ---------------------------
# Los .Rmd del pipeline escriben sus tablas al vuelo, y `Rscript` no los ejecuta
# directamente. Acá se extrae el código R de cada .Rmd con knitr::purl() y se lo
# evalúa, lo que evita depender de pandoc para renderizar HTML cuando lo único
# que se necesita son los CSV de salida.
#
# Detalle importante: cada script del pipeline empieza con `rm(list = ls())`.
# Si se los evalúa en el entorno global, esa línea borra las funciones de este
# runner y el paso siguiente falla con "could not find function". Por eso cada
# script se evalúa en su propio entorno con `local =`, y las funciones del runner
# viven en `.runner`, fuera de su alcance.
#
# Uso
# ---
#   cd ~/investigacion/Workspace_R/ISDM
#   Rscript 3_run_pipeline.R               # cadena completa
#   Rscript 3_run_pipeline.R indicadores   # sin reprocesar microdatos de EPH
#   Rscript 3_run_pipeline.R eph           # sólo procesamiento + indicadores EPH
#   Rscript 3_run_pipeline.R casen         # sólo procesamiento CASEN
#
# Los logs quedan en logs/, uno por paso.

args <- commandArgs(trailingOnly = TRUE)
modo <- if (length(args)) args[[1]] else "completo"

dir.create("logs", showWarnings = FALSE)
t_global <- Sys.time()

# Las funciones viven acá para que el `rm(list = ls())` de los scripts no las toque.
.runner <- new.env(parent = globalenv())

.runner$trazar <- function(archivo, t0, ok, log = NULL) {
  cat(sprintf("--> %s en %.1f min%s\n",
              if (ok) "OK" else "FALLÓ",
              as.numeric(difftime(Sys.time(), t0, units = "mins")),
              if (!is.null(log)) paste0("  (log: ", log, ")") else ""))
  flush.console()
}

#' Evalúa un script .R en un entorno propio.
.runner$paso <- function(archivo) {
  cat(sprintf("\n%s\n== %s ==\n%s\n", strrep("=", 60), archivo, strrep("=", 60)))
  flush.console()
  t0 <- Sys.time()
  log <- file.path("logs", paste0(tools::file_path_sans_ext(basename(archivo)), ".log"))
  con <- file(log, open = "wt")
  sink(con, split = TRUE)
  ok <- TRUE
  tryCatch(
    source(archivo, local = new.env(parent = globalenv())),
    error = function(e) { cat("\n!!! ERROR:", conditionMessage(e), "\n"); ok <<- FALSE }
  )
  sink(); close(con)
  .runner$trazar(archivo, t0, ok, log)
  if (!ok) stop("Falló el paso: ", archivo, call. = FALSE)
  invisible(TRUE)
}

#' Extrae el código R de un .Rmd y lo evalúa en un entorno propio.
.runner$paso_rmd <- function(archivo) {
  cat(sprintf("\n%s\n== %s ==\n%s\n", strrep("=", 60), archivo, strrep("=", 60)))
  flush.console()
  t0 <- Sys.time()
  base <- tools::file_path_sans_ext(basename(archivo))
  purled <- file.path("logs", paste0(base, "_purl.R"))
  ok <- TRUE
  tryCatch({
    knitr::purl(archivo, output = purled, quiet = TRUE)
    source(purled, local = new.env(parent = globalenv()))
  }, error = function(e) { cat("\n!!! ERROR:", conditionMessage(e), "\n"); ok <<- FALSE })
  .runner$trazar(archivo, t0, ok)
  if (!ok) stop("Falló el paso: ", archivo, call. = FALSE)
  invisible(TRUE)
}

paso <- .runner$paso
paso_rmd <- .runner$paso_rmd

# --- Cadena -------------------------------------------------------------------
if (modo %in% c("completo", "eph")) {
  paso("1_1_procesamiento_EPH.R")
}

if (modo %in% c("completo", "casen")) {
  paso("1_1_procesamiento_casen.R")
}

if (modo %in% c("completo", "eph", "indicadores")) {
  paso("1_2_indicadores_EPH.R")
}

if (modo %in% c("completo", "indicadores")) {
  paso("1_2_indicadores_casen.R")
  paso("1_4_indicadores_unificados.R")
  # Genera indicadores_con_brechas_*.csv
  paso_rmd("1_6_construccion_brechas.Rmd")
  # ISDM principal (migrantes totales) y el de migrantes recientes
  paso_rmd("2_1_ISDM_jerarquico_migrantes.Rmd")
  paso_rmd("2_3_ISDM_jerarquico_migrantes_recientes.Rmd")
}

cat(sprintf("\n%s\nPIPELINE '%s' COMPLETADO en %.1f min\n%s\n", strrep("=", 60),
            modo, as.numeric(difftime(Sys.time(), t_global, units = "mins")),
            strrep("=", 60)))
