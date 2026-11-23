# =============================================================================
# Corre la cadena completa, paso por paso, cada uno en un proceso de R aparte
# (todos los scripts empiezan con rm(list = ls())). Para leer y entender cada
# paso conviene abrirlo y correrlo a mano en el orden 00 a 10.
# Los logs quedan en logs/. Requiere los paquetes callr y rmarkdown.
# Uso: source("correr_todo.R")   o   Rscript correr_todo.R
# =============================================================================

pasos <- c("01_procesamiento_EPH.R", "02_procesamiento_CASEN.R",
           "03_indicadores_EPH.R", "04_indicadores_CASEN.R",
           "05_indicadores_unificados.R", "06_construccion_brechas.Rmd",
           "07_ISDM.R", "08_composicion_y_edad.R", "09_graficos_articulo.R",
           "10_calidad_muestral.R")
# 00_contexto_ARG_CHL.Rmd (Gráficos 1, A1 y A2) descarga datos del Banco Mundial:
# se corre aparte, cuando haga falta.

dir.create("logs", showWarnings = FALSE)
for (s in pasos) {
  t0 <- Sys.time()
  log <- file.path("logs", sub("\\.(R|Rmd)$", ".log", s))
  callr::r(function(s, wd) {
    setwd(wd)
    if (grepl("\\.Rmd$", s)) rmarkdown::render(s, quiet = TRUE, envir = new.env())
    else source(s, encoding = "UTF-8")
    invisible(NULL)
  }, args = list(s, getwd()), stdout = log, stderr = log)
  cat(sprintf("OK  %-32s %5.1f min\n", s, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
