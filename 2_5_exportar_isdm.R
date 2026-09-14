# =============================================================================
# EXPORTACIÓN DE LOS VALORES DEL ISDM
# =============================================================================
# Por qué existe este archivo
# ---------------------------
# 2_1 y 2_3 construyen el ISDM y sus sub-índices, pero sólo los muestran como
# tablas dentro del documento y como figuras. No escriben los valores a disco.
# Sin eso no hay manera de contrastar las cifras que el artículo cita contra lo
# que el código calcula, ni de comparar antes y después de una corrección.
#
# No reimplementa nada: extrae el código de los .Rmd con knitr::purl y lo evalúa,
# igual que 3_run_pipeline.R. Si los .Rmd cambian, esto cambia con ellos.
#
# Uso
# ---
#   cd ~/investigacion/Workspace_R/ISDM
#   Rscript 2_5_exportar_isdm.R
#
# Salida: Data/isdm_export/*.csv (una serie por variante)

dir.create("logs", showWarnings = FALSE)
dir.create("Data/isdm_export", showWarnings = FALSE)

exportar <- function(rmd, etiqueta) {
  cat(sprintf("\n%s\n== %s  (%s) ==\n%s\n", strrep("=", 62), rmd, etiqueta, strrep("=", 62)))
  purled <- file.path("logs", paste0(tools::file_path_sans_ext(basename(rmd)), "_purl.R"))
  knitr::purl(rmd, output = purled, quiet = TRUE)

  # Cada .Rmd arranca con rm(list = ls()). Evaluado en su propio entorno, no
  # alcanza a este script ni al de la variante anterior.
  e <- new.env(parent = globalenv())
  ok <- tryCatch({
    source(purled, local = e)
    TRUE
  }, error = function(err) {
    cat("\n!!! ERROR:", conditionMessage(err), "\n")
    FALSE
  })
  if (!ok) return(invisible(NULL))

  escribir <- function(obj, nombre) {
    if (!exists(obj, envir = e, inherits = FALSE)) {
      cat(sprintf("  [falta] %s no quedó definido\n", obj))
      return(invisible(NULL))
    }
    x <- get(obj, envir = e)
    p <- file.path("Data/isdm_export", sprintf("%s_%s.csv", nombre, etiqueta))
    readr::write_csv(x, p)
    cat(sprintf("  %-22s -> %d filas\n", obj, nrow(x)))
  }

  escribir("isdm_jerarquico", "isdm")
  escribir("subindices_dimension", "subindices")
  escribir("resumen", "resumen_pais")
  escribir("resumen_dimensiones", "resumen_dimensiones")
  escribir("datos_analisis", "datos_analisis")
  invisible(e)
}

# Variante principal: migrantes totales (lo que citan los resultados del artículo)
exportar("2_1_ISDM_jerarquico_migrantes.Rmd", "totales")

# Variante de migrantes recientes
exportar("2_3_ISDM_jerarquico_migrantes_recientes.Rmd", "recientes")

# 2_4 sólo produce figuras (output_integrados/), que quedaron desactualizadas:
# se regeneran acá para que reflejen las brechas corregidas.
cat(sprintf("\n%s\n== 2_4_ISDM_analisis_integrado.Rmd  (figuras) ==\n%s\n", strrep("=", 62), strrep("=", 62)))
tryCatch({
  purled <- "logs/2_4_ISDM_analisis_integrado_purl.R"
  knitr::purl("2_4_ISDM_analisis_integrado.Rmd", output = purled, quiet = TRUE)
  source(purled, local = new.env(parent = globalenv()))
  cat("  figuras de output_integrados/ regeneradas\n")
}, error = function(err) cat("\n!!! ERROR en 2_4:", conditionMessage(err), "\n"))

cat(sprintf("\n%s\nEXPORTACIÓN TERMINADA\n%s\n", strrep("=", 62), strrep("=", 62)))
