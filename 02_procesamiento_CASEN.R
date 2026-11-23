# =============================================================================
# 2. Procesamiento de la CASEN, Chile 2015, 2017, 2020, 2022 y 2024
# =============================================================================
# Autora: Natalia Debandi (con el apoyo inicial de Joaquín Zajac)
# Lee:      Casen_AAAA.dta (carpeta c:/Data/Casen o variable ISDM_CASEN_DIR).
# Hace:
#  - Migrante: nacionalidad extranjera (r1a = 3). En 2020, lugar de nacimiento
#    (2020 no se usa en el análisis).
#  - Migrante reciente: vivía en otro país hace 5 años (r2; 2015 y 2022 código 3,
#    2017 y 2020 código 4). En 2024, llegó entre 2020 y 2024 (r1cp 1 a 3). (P16)
#  - Informalidad asalariada: asalariado (o15 3 a 8) que no cotiza (o29/o32).
#    Quien no está afiliado a un sistema previsional (o28/o31 = 2) no recibe la
#    pregunta de cotización y se clasifica como "no cotiza". (P22)
#  - Pobreza 2024 con la metodología anterior (pobreza_2013), comparable con
#    2015-2022; la oficial nueva queda en pobreza_nueva. (P18)
#  - Sistema de salud (s12/s13) para los indicadores de salud.
# Escribe:  Data/Casen_unificado_2015_2024_proc.csv
# Detalle de cada decisión: ISDM_metodologia_trazabilidad.docx (carpeta del artículo,
# VERSION_FINAL), códigos de problema P1 a P22.
# =============================================================================

rm(list = ls())

library(haven)
library(tidyverse)
library(labelled)
library(readr)
library(forcats)
library(openxlsx)
library(dplyr)

# Operador auxiliar "not in"
`%nin%` <- Negate(`%in%`)

# =============================================================================
# CONFIGURACIÓN DE RUTAS
# =============================================================================
# Ruta a los microdatos de CASEN. Se puede sobreescribir con la variable de
# entorno ISDM_CASEN_DIR, para correr el pipeline en otra máquina o desde otra
# ruta sin editar el script.
carpeta_casen <- Sys.getenv("ISDM_CASEN_DIR", unset = "c:/Data/Casen")
if (!dir.exists(carpeta_casen)) {
  alternativa <- path.expand("~/investigacion/datos/Casen")
  if (dir.exists(alternativa)) {
    message("carpeta_casen no existe; se usa ", alternativa)
    carpeta_casen <- alternativa
  }
}

# Los nombres de archivo no siguen un patrón uniforme aguas arriba: 2024 viene
# en minúscula (casen_2024.dta) mientras el resto usa mayúscula. Esta función
# resuelve el nombre real sin distinguir mayúsculas, y avisa si no lo encuentra.
ruta_casen <- function(archivo) {
  directo <- file.path(carpeta_casen, archivo)
  if (file.exists(directo)) return(directo)
  disponibles <- list.files(carpeta_casen, pattern = "\\.dta$", ignore.case = TRUE)
  coincide <- disponibles[tolower(disponibles) == tolower(archivo)]
  if (length(coincide) == 1) return(file.path(carpeta_casen, coincide))
  stop("No se encontró el microdato de CASEN: ", archivo,
       "\n  Buscado en: ", carpeta_casen,
       "\n  Archivos .dta presentes: ", paste(disponibles, collapse = ", "))
}

# =============================================================================
# PARTE 1: SELECCIÓN DE VARIABLES Y PREPARACIÓN INICIAL
# =============================================================================

# -----------------------------------------------------------------------------
# 1.1 Carga y selección de variables por año
# -----------------------------------------------------------------------------

Casen_2015 <- read_dta(ruta_casen("Casen_2015.dta")) %>%
  select(expr, nacionalidad=r1a, edad, pobreza, condicion_ocupacional=o15, temporalidad_contrato=o16, 
         formalidad_contrato=o17, descuento_jubilatorio=o29, acceso_salud=s12, 
         seguro_comple=s14, asiste, nivel_asiste=e6a, educ, activ, sexo,
         residencia_5anios_atras=r2,   # dónde vivía hace 5 años
         afiliado=o28)                 # afiliación previsional

Casen_2017 <- read_dta(ruta_casen("Casen_2017.dta")) %>%
  select(expr, nacionalidad=r1a, edad, pobreza, condicion_ocupacional=o15, temporalidad_contrato=o16, 
         formalidad_contrato=o17, descuento_jubilatorio=o29, acceso_salud=s12, 
         seguro_comple=s14, asiste, nivel_asiste=e6a, educ, activ, sexo,
         residencia_5anios_atras=r2,   # dónde vivía hace 5 años
         afiliado=o28)                 # afiliación previsional

# CASEN 2020 ("en pandemia"): nomenclatura distinta a otros años
# - nacionalidad: lugar_nac (0=Chile, 1=extranjero) en vez de r1a (1/2/3)
# - residencia hace 5 años: r2, con los códigos de 2017 (4=otro país)
# - descuento jubilatorio: o32, igual que 2022/2024
# - salud: s13 (1=FONASA, 2=FF.AA., 3=ISAPRE, 4=Ninguno, 5=Otro) — sin subgrupos FONASA
# - sin variables de tipo/temporalidad de contrato (o16/o17 en 2020 son para cuentapropistas)
Casen_2020 <- read_dta(ruta_casen("Casen_2020.dta")) %>%
  select(expr, nacionalidad=lugar_nac, edad, pobreza, condicion_ocupacional=o15,
         descuento_jubilatorio=o32, acceso_salud=s13,
         seguro_comple=s15, asiste, nivel_asiste=e6a, educ=educc, activ, sexo,
         residencia_5anios_atras=r2)

Casen_2022 <- read_dta(ruta_casen("Casen_2022.dta")) %>%
  select(expr, nacionalidad=r1a, edad, pobreza, condicion_ocupacional=o15, temporalidad_contrato=o18, 
         formalidad_contrato=o19, descuento_jubilatorio=o32, acceso_salud=s13, acceso_salud_fonasa=s13_fonasa,
         seguro_comple=s15, asiste, nivel_asiste=e6a_asiste, educ, activ, sexo,
         residencia_5anios_atras=r2,   # dónde vivía hace 5 años
         afiliado=o31)                 # afiliación previsional

Casen_2024 <- read_dta(ruta_casen("casen_2024.dta")) %>%
  select(expr, nacionalidad=r1a, edad, pobreza, condicion_ocupacional=o15, temporalidad_contrato=o18, 
         formalidad_contrato=o19, descuento_jubilatorio=o32, acceso_salud=s13, acceso_salud_fonasa=s13_fonasa, 
         seguro_comple=s15a, asiste, nivel_asiste=e6a_asiste, educ=educc, activ, sexo,
         periodo_llegada=r1cp, afiliado=o31, pobreza_2013) %>%
  # pobreza con la metodología 2013 (igual que 2015-2022); la oficial nueva queda en pobreza_nueva
  mutate(pobreza_nueva = pobreza, pobreza = pobreza_2013) %>%
  select(-pobreza_2013)

# -----------------------------------------------------------------------------
# 1.2 Agregar variable de año
# -----------------------------------------------------------------------------
Casen_2015 <- Casen_2015 %>% mutate(anio = 2015)
Casen_2017 <- Casen_2017 %>% mutate(anio = 2017)
Casen_2020 <- Casen_2020 %>% mutate(anio = 2020)
Casen_2022 <- Casen_2022 %>% mutate(anio = 2022)
Casen_2024 <- Casen_2024 %>% mutate(anio = 2024)

# -----------------------------------------------------------------------------
# 1.3 Eliminación de etiquetas antes de unificar
# -----------------------------------------------------------------------------
Casen_2015 <- Casen_2015 %>% zap_labels()
Casen_2017 <- Casen_2017 %>% zap_labels()
Casen_2020 <- Casen_2020 %>% zap_labels()
Casen_2022 <- Casen_2022 %>% zap_labels()
Casen_2024 <- Casen_2024 %>% zap_labels()

# -----------------------------------------------------------------------------
# 1.4 Unificación de bases
# -----------------------------------------------------------------------------
Casen_unificado <- bind_rows(
  Casen_2015,
  Casen_2017,
  Casen_2020,
  Casen_2022,
  Casen_2024
)

#borrar las bases casen individuales para liberar memoria
rm(Casen_2015, Casen_2017, Casen_2020, Casen_2022, Casen_2024)

# =============================================================================
# PARTE 2: RECODIFICACIÓN DE VARIABLES
# =============================================================================

Casen_unificado_proc <- Casen_unificado %>%
  mutate(
    # -------------------------------------------------------------------------
    # 2.1 MIGRANTE
    # -------------------------------------------------------------------------
    migrante = case_when(
      # 2015, 2017, 2022, 2024: r1a — 1=Chilena, 2=Doble con chilena, 3=Extranjera
      anio != 2020 & nacionalidad < 3 ~ "Nativos",
      anio != 2020 & nacionalidad == 3 ~ "Migrantes",
      # 2020: lugar_nac — 0=Nacido en Chile, 1=Nacido fuera de Chile
      anio == 2020 & nacionalidad == 0 ~ "Nativos",
      anio == 2020 & nacionalidad == 1 ~ "Migrantes",
      TRUE ~ NA_character_
    ),

    # -------------------------------------------------------------------------
    # 2.2 MIGRANTE RECIENTE
    # -------------------------------------------------------------------------
    # Definición: migrante que vivía en otro país hace 5 años (P16)
    #
    # METODOLOGÍA POR AÑO:
    # - 2015 y 2022: dónde vivía hace 5 años (r2)
    #   * Valores: 1=Esta comuna, 2=Otra comuna, 3=Otro país, 9=No sabe
    #   * Migrante reciente: valor 3
    #
    # - 2017 y 2020: dónde vivía hace 5 años (r2), con otra codificación
    #   * Valores: 1=Aún no nacía, 2=Esta comuna, 3=Otra comuna, 4=Otro país, 9=No sabe
    #   * Migrante reciente: valor 4
    #
    # - 2024: Pregunta período de llegada
    #   * Valores: 1=2024, 2=2022-2023, 3=2020-2021, 4=2018-2019, 5=2015-2017, etc.
    #   * Migrante reciente: llegó entre 2020-2024 (valores 1, 2, 3)
    #   * Nota: 2024 se aplicó en 2024, entonces <5 años = desde 2020

    migrante_reciente = case_when(
      # Solo aplica a migrantes (nativos no pueden ser migrantes recientes)
      migrante == "Nativos" ~ "No",

      # 2015 y 2022 usan r2 (1=esta comuna, 2=otra comuna, 3=otro país)
      anio %in% c(2015, 2022) & residencia_5anios_atras == 3 ~ "Si",
      anio %in% c(2015, 2022) & residencia_5anios_atras %in% c(1, 2) ~ "No",
      # 2017 usa r2 (1=aún no nacía, 2=esta comuna, 3=otra comuna, 4=otro país)
      anio == 2017 & residencia_5anios_atras == 4 ~ "Si",
      anio == 2017 & residencia_5anios_atras %in% c(1, 2, 3) ~ "No",

      # Para 2020: usar residencia_5anios_atras (valor 4 = otro país)
      anio == 2020 & residencia_5anios_atras == 4 ~ "Si",
      anio == 2020 & residencia_5anios_atras %in% c(1, 2, 3) ~ "No",

      # Para 2024: usar periodo_llegada
      # Valores 1-3 = llegó entre 2020-2024 (menos de 5 años)
      anio == 2024 & periodo_llegada %in% c(1, 2, 3) ~ "Si",
      anio == 2024 & periodo_llegada >= 4 ~ "No",

      # Casos sin información o no sabe
      TRUE ~ NA_character_
    ),
    estado_actividad = case_when(
      activ == 1 ~ "Ocupado",
      activ == 2 ~ "Desocupado",
      activ == 3 ~ "Inactivo",
      TRUE ~ NA_character_
    ),
    # -------------------------------------------------------------------------
    # 2.3 SITUACIÓN DE POBREZA (2024 con la metodología 2013, ver parte 1)
    # -------------------------------------------------------------------------
    pobreza = case_when(
      pobreza == 1 ~ "Pobres extremos",
      pobreza == 2 ~ "Pobres",
      pobreza == 3 ~ "No Pobres",
      TRUE ~ NA_character_
    ),
   
    # -------------------------------------------------------------------------
    # 2.4 COTIZACIÓN PREVISIONAL (base de la informalidad asalariada)
    # -------------------------------------------------------------------------
    # Equivale a la pregunta de descuento jubilatorio (PP07H) de la EPH.
    # 2015 y 2017: o29, código 7 = "No cotiza"
    # 2020, 2022 y 2024: o32, código 6 = "No cotiza"
    # Se estandariza a: 1-5 = Sí cotiza, 6 = No cotiza
    descuento_jubilatorio_std = case_when(
      # asalariado no afiliado a ningún sistema previsional -> no cotiza (6).
      # La CASEN no le hace la pregunta de cotización (o29/o32 = NA).
      is.na(descuento_jubilatorio) & afiliado == 2 ~ 6,
      # 2015 y 2017: el código 6 de o29 se trata como "sí cotiza" y se pasa a 5,
      # para que el 6 quede reservado a "no cotiza" en todos los años.
      # [verificar la etiqueta del código 6 en el libro de códigos de 2015 y 2017]
      anio %in% c(2015, 2017) & descuento_jubilatorio == 6 ~ 5,

      # Para 2015-2017: mantener códigos 1-5 (sí cotiza)
      anio %in% c(2015, 2017) & descuento_jubilatorio %in% c(1, 2, 3, 4, 5) ~ descuento_jubilatorio,
      
      # Para 2015-2017: convertir código 7 a código 6
      anio %in% c(2015, 2017) & descuento_jubilatorio == 7 ~ 6,
      
      # Para 2020, 2022 y 2024: ya está en el formato correcto (o32)
      anio %in% c(2020, 2022, 2024) ~ descuento_jubilatorio,
      
      # Otros casos (NS/NR, etc)
      TRUE ~ descuento_jubilatorio
    ),
    
    # -------------------------------------------------------------------------
    # 2.5 FORMALIDAD LABORAL - ASALARIADA (Compatible con EPH)
    # -------------------------------------------------------------------------
    # o15 condicion_ocupacional
    # 1	1. Patrón(a) o empleador(a)
    # 2	2. Trabajador(a) por cuenta propia
    # 3	3. Empleado(a) u obrero(a) del sector público (Gobierno Central o Municipal)
    # 4	4. Empleado(a) u obrero(a) de empresas públicas
    # 5	5. Empleado(a) u obrero(a) del sector privado
    # 6	6. Servicio doméstico puertas adentro
    # 7	7. Servicio doméstico puertas afuera
    # 8	8. FF.AA. y del Orden
    # 9	9. Familiar no remunerado
    # # 
    # o32 cotiza (2020-2024; en 2015 y 2017 es o29, recodificada arriba)
    # -88	No sabe
    # 1	1. Sí, AFP (Administradora de Fondos de Pensiones)
    # 2	2. Sí, IPS ex INP
    # 3	3. Sí, Caja de Previsión de la Defensa Nacional (CAPREDENA)
    # 4	4. Sí, Dirección de Previsión de Carabineros (DIPRECA)
    # 5	5. Sí, otro. Especifique
    # 6	6. No está cotizando
    # 
    
      
    formalidad_empleo_asalariada = case_when(
      # Formal: asalariado con descuento jubilatorio
      condicion_ocupacional %in% c(3, 4, 5, 6, 7, 8) &
        descuento_jubilatorio_std %in% c(1, 2, 3, 4, 5) ~ 
        "Asalariado formal",
      
      # Informal: asalariado sin descuento jubilatorio
      condicion_ocupacional %in% c(3, 4, 5, 6, 7, 8) &
        descuento_jubilatorio_std == 6 ~ 
        "Asalariado informal",
      
      # NA: no es asalariado o faltan datos
      TRUE ~ NA_character_
    ),
    
    # -------------------------------------------------------------------------
    # 2.6 FORMALIDAD LABORAL - TOTAL (Compatible con EPH; no entra en el índice)
    # -------------------------------------------------------------------------
    # Incluye asalariados, cuentapropistas y trabajadores familiares
    # Equivalente a: formalidad_empleo_total en EPH
    
    formalidad_empleo_total = case_when(
      # FORMALES
      # Asalariados con descuento jubilatorio
      condicion_ocupacional %in% c(3, 4, 5, 6, 7, 8) &
        descuento_jubilatorio_std %in% c(1, 2, 3, 4, 5) ~ "Formal",
      
      # Patrones/empleadores
      condicion_ocupacional == 1 ~ "Formal",
      
      # INFORMALES
      # Asalariados sin descuento jubilatorio
      condicion_ocupacional %in% c(3, 4, 5, 6, 7, 8) &
        descuento_jubilatorio_std == 6 ~ "Informal",
      
      # Cuentapropistas
      condicion_ocupacional == 2 ~ "Informal",
      
      # Trabajadores familiares sin remuneración
      condicion_ocupacional == 9 ~ "Informal",
      
      # NA: no ocupados o faltan datos
      TRUE ~ NA_character_
    ),
    
    # -------------------------------------------------------------------------
    # 2.7 CATEGORÍA DE INFORMALIDAD (para desagregación)
    # -------------------------------------------------------------------------
    categoria_informalidad = case_when(
      # Asalariados formales (incluye servicio doméstico)
      condicion_ocupacional %in% c(3, 4, 5, 6, 7, 8) &
        descuento_jubilatorio_std %in% c(1, 2, 3, 4, 5) ~ "Asalariado formal",   # _std
      
      # Asalariados informales
      condicion_ocupacional %in% c(3, 4, 5, 6, 7, 8) &
        descuento_jubilatorio_std == 6 ~ "Asalariado informal",   # _std
      
      # Patrones
      condicion_ocupacional == 1 ~ "Patrón",
      
      
      # Cuentapropistas
      condicion_ocupacional == 2 ~ "Cuentapropista",
      
      # Trabajadores familiares
      condicion_ocupacional == 9 ~ "Trabajador familiar",
      
      TRUE ~ NA_character_
    ),
    
    # -------------------------------------------------------------------------
    # 2.8 ASISTE A ESTABLECIMIENTO EDUCATIVO
    # -------------------------------------------------------------------------
    asiste = case_when(
      asiste == 1 ~ "Asiste",  # Sí asiste
      asiste == 2 ~ "No asiste",  # No asiste
      TRUE ~ NA_character_
    ),  
   
    
    # -------------------------------------------------------------------------
    # 2.9 SISTEMA DE SALUD
    # -------------------------------------------------------------------------
    # Esta clasificación (pública gratuita, pública paga, ISAPRE, particular)
    # solo se usa para el indicador I10 del análisis de sensibilidad. El
    # indicador del índice principal (cobertura: afiliación a algún sistema de
    # salud frente a ninguno) se calcula en el paso 4 a partir de acceso_salud.
    # CONTEXTO DEL SISTEMA DE SALUD CHILENO:
    # - FONASA: Sistema público con versión gratuita (grupos A y B para sectores
    #   vulnerables) y versión paga (grupos C y D)
    # - ISAPRE: Aseguradoras privadas, pago obligatorio descontado del salario
    # - FFAA: Sistema especial para fuerzas armadas y de seguridad
    # - Particular: Sin sistema, se atiende pagando directamente
    #
    # CAMBIO EN 2022: La variable de salud se dividió en dos:
    # - acceso_salud: indica el sistema (FONASA, ISAPRE, etc.)
    # - acceso_salud_fonasa: indica el grupo dentro de FONASA (solo 2022 y 2024)
    #
    # 2020: s13 tiene codificación propia (1=FONASA, 2=FF.AA., 3=ISAPRE, 4=Ninguno, 5=Otro)
    #       Sin variable de subgrupo FONASA → decisión metodológica: todo FONASA = gratuito
    #
    # DECISIÓN METODOLÓGICA: Se asigna NA a quienes:
    # - No saben o no responden
    # - Declaran FONASA pero no especifican si pagan o no (solo en 2022 y 2024)

    sistema_salud = case_when(
      # FONASA grupos A y B (gratuito)
      anio %in% c(2022, 2024) & acceso_salud == 1 &
        acceso_salud_fonasa %in% c(1, 2) ~ "Accede a salud pública gratuita",
      anio %in% c(2015, 2017) & acceso_salud %in% c(1, 2) ~
        "Accede a salud pública gratuita",
      # 2020: FONASA sin subgrupo, se asigna como gratuito
      anio == 2020 & acceso_salud == 1 ~ "Accede a salud pública gratuita",

      # FONASA grupos C y D (paga)
      anio %in% c(2022, 2024) & acceso_salud == 1 &
        acceso_salud_fonasa %in% c(3, 4) ~ "Accede a salud pública paga",
      anio %in% c(2015, 2017) & acceso_salud %in% c(3, 4) ~
        "Accede a salud pública paga",

      # ISAPRE (seguro privado obligatorio)
      anio %in% c(2022, 2024) & acceso_salud == 2 ~
        "ISAPRE (seguro laboral obligatorio)",
      anio %in% c(2015, 2017) & acceso_salud == 7 ~
        "ISAPRE (seguro laboral obligatorio)",
      anio == 2020 & acceso_salud == 3 ~
        "ISAPRE (seguro laboral obligatorio)",

      # Particulares y otros
      anio %in% c(2022, 2024) & acceso_salud %in% c(3, 4, 5) ~
        "Particulares y otros sistemas",
      anio %in% c(2015, 2017) & acceso_salud %in% c(6, 8, 9) ~
        "Particulares y otros sistemas",
      # 2020: FF.AA.(2), Ninguno(4), Otro(5)
      anio == 2020 & acceso_salud %in% c(2, 4, 5) ~
        "Particulares y otros sistemas",

      TRUE ~ NA_character_
    )
    
    
  )




# =============================================================================
# PARTE 3: GUARDADO DE RESULTADOS
# =============================================================================

write_csv(Casen_unificado_proc, "Data/Casen_unificado_2015_2024_proc.csv")





