# =============================================================================
# SCRIPT DE PROCESAMIENTO DE DATOS EPH - VERSIÓN SIMPLIFICADA
# =============================================================================
# Descripción: Procesamiento y homogeneización de datos de la Encuesta 
#              Permanente de Hogares (EPH) del INDEC
# Autor: Natalia Debandi
# Fecha: Enero 2026
# Versión: Simplificada - sin funciones, procesamiento lineal
# =============================================================================

rm(list = ls())

library(tidyverse)
library(haven)
library(readr)
library(eph)  # Librería para calcular pobreza


# Operador auxiliar "not in"
`%nin%` <- Negate(`%in%`)

# =============================================================================
# CONFIGURACIÓN INICIAL
# =============================================================================

# Carpeta donde están los archivos CSV de EPH
carpeta_eph <- "c:/Data/EPH"

# Años a procesar
anios_a_procesar <- 2016:2024

# =============================================================================
# PARTE 1: CARGA DE DATOS
# =============================================================================

message("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
message("CARGANDO ARCHIVOS EPH")
message("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

# Listar archivos disponibles
archivos <- list.files(
  path = carpeta_eph,
  pattern = "^EPH_(hogar|indiv)_\\d{4}\\.csv$",
  full.names = TRUE
)

if (length(archivos) == 0) {
  stop("⚠️ No se encontraron archivos .csv en: ", carpeta_eph)
}

# Cargar cada archivo
for (ruta in archivos) {
  nombre <- tools::file_path_sans_ext(basename(ruta))
  message("   Cargando: ", nombre)
  df <- read_csv2(ruta, show_col_types = FALSE)
  
  # Convertir etiquetas a códigos numéricos (soluciona problema 2024)
  df <- zap_labels(df)
  
  assign(nombre, df, envir = .GlobalEnv)
}


message("\n✔️ Archivos cargados\n")


# =============================================================================
# PARTE 2: PROCESAR CADA AÑO
# =============================================================================

# Lista para guardar bases procesadas
lista_indiv_procesadas <- list()
lista_hogar_procesadas <- list()

for (anio in anios_a_procesar) {
  
  message("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  message("PROCESANDO AÑO ", anio)
  message("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  
  # ---------------------------------------------------------------------------
  # 2.1 Verificar si existen las bases para este año
  # ---------------------------------------------------------------------------
  
  nombre_hogar <- paste0("EPH_hogar_", anio)
  nombre_indiv <- paste0("EPH_indiv_", anio)
  
  if (!exists(nombre_hogar) || !exists(nombre_indiv)) {
    message("⚠️  Saltando año ", anio, " (archivos no encontrados)\n")
    next
  }
  
  EPH_hogar <- get(nombre_hogar)
  EPH_indiv <- get(nombre_indiv)
  
  message("   ✓ Bases cargadas para ", anio)
  
  # ---------------------------------------------------------------------------
  # 2.2 NORMALIZAR VARIABLES (convertir texto a numérico)
  # ---------------------------------------------------------------------------
  
  
  
  
  EPH_indiv <- EPH_indiv %>%
    mutate(
      # Edad
      CH06 = as.numeric(as.character(CH06)),
      
      # Asistencia educativa (CH10) - manejar etiquetas de texto
      CH10 = case_when(
        CH10 == "Si, asiste" ~ 1,
        CH10 == "Sí, asiste" ~ 1,
        CH10 == "No asiste, pero asistio" ~ 2,
        CH10 == "No asiste, pero asistió" ~ 2,
        CH10 == "Nunca asistio" ~ 3,
        CH10 == "Nunca asistió" ~ 3,
        TRUE ~ as.numeric(as.character(CH10))
      ),
      
      # Nivel educativo al que asiste (CH12) - para quienes asisten actualmente
      CH12 = case_when(
        CH12 == "Jardin/Preescolar" ~ 1,
        CH12 == "Jardín/Preescolar" ~ 1,
        CH12 == "Primario" ~ 2,
        CH12 == "EGB" ~ 3,
        CH12 == "Secundario" ~ 4,
        CH12 == "Polimodal" ~ 5,
        CH12 == "Terciario" ~ 6,
        CH12 == "Universitario" ~ 7,
        CH12 == "Posgrado universitario" ~ 8,
        CH12 == "Educacion especial (discapacitado)" ~ 9,
        CH12 == "Educación especial (discapacitado)" ~ 9,
        TRUE ~ as.numeric(as.character(CH12))
      ),
      
      # Sexo
      CH04 = case_when(
        CH04 == "Varon" ~ 1,
        CH04 == "Varón" ~ 1,
        CH04 == "Mujer" ~ 2,
        TRUE ~ as.numeric(as.character(CH04))
      ),
      
      # Trimestre - asegurar que sea numérico
      TRIMESTRE = case_when(
        TRIMESTRE == "1er Trimestre" ~ 1,
        TRIMESTRE == "2do Trimestre" ~ 2,
        TRIMESTRE == "3er Trimestre" ~ 3,
        TRIMESTRE == "4to Trimestre" ~ 4,
        TRUE ~ as.numeric(as.character(TRIMESTRE))
      ),
      
      # Región
      REGION = case_when(
        REGION == "Gran Buenos Aires" ~ 1,
        REGION == "Noroeste" ~ 40,
        REGION == "Nordeste" ~ 41,
        REGION == "Cuyo" ~ 42,
        REGION == "Pampeana" ~ 43,
        REGION == "Patagonica" ~ 44,
        REGION == "Patagónica" ~ 44,
        TRUE ~ as.numeric(as.character(REGION))
      ),
      
      # Lugar de nacimiento
      CH15 = case_when(
        CH15 == "En esta localidad" ~ 1,
        CH15 == "En otra localidad de esta provincia" ~ 2,
        CH15 == "En otra provincia (especificar)" ~ 3,
        CH15 == "En un pais limitrofe (especificar Brasil, Bolivia, Chile, Paraguay, Uruguay)" ~ 4,
        CH15 == "En un país limítrofe (especificar Brasil, Bolivia, Chile, Paraguay, Uruguay)" ~ 4,
        CH15 == "En otro pais (especificar)" ~ 5,
        CH15 == "En otro país (especificar)" ~ 5,
        TRUE ~ as.numeric(as.character(CH15))
      ),
      
      # Residencia hace 5 años
      CH16 = case_when(
        CH16 == "En esta localidad" ~ 1,
        CH16 == "En otra localidad de esta provincia" ~ 2,
        CH16 == "En otra provincia (especificar)" ~ 3,
        CH16 == "En un pais limitrofe (especificar Brasil, Bolivia, Chile, Paraguay, Uruguay)" ~ 4,
        CH16 == "En un país limítrofe (especificar Brasil, Bolivia, Chile, Paraguay, Uruguay)" ~ 4,
        CH16 == "En otro pais (especificar)" ~ 5,
        CH16 == "En otro país (especificar)" ~ 5,
        CH16 == "No habia nacido" ~ 6,
        CH16 == "No había nacido" ~ 6,
        TRUE ~ as.numeric(as.character(CH16))
      ),
      
      # Estado ocupacional - manejar etiquetas de texto
      ESTADO = case_when(
        ESTADO == "Entrevista individual no realizada (no respuesta al cuestionario individual)" ~ 0,
        ESTADO == "Ocupado" ~ 1,
        ESTADO == "Desocupado" ~ 2,
        ESTADO == "Inactivo" ~ 3,
        ESTADO == "Menor de 10 años" ~ 4,
        ESTADO == "Menor de 10 anos" ~ 4,
        TRUE ~ as.numeric(as.character(ESTADO))
      ),
      
      # Descuento jubilatorio (PP07H)
      PP07H = case_when(
        PP07H == "Si" ~ 1,
        PP07H == "Sí" ~ 1,
        PP07H == "No" ~ 2,
        PP07H == "Ns./Nr." ~ 9,
        PP07H == "Ns./Nr.." ~ 9,
        PP07H == "NS./NR." ~ 9,
        TRUE ~ as.numeric(as.character(PP07H))
      ),
      
      # Nivel educativo - códigos oficiales
      NIVEL_ED = case_when(
        NIVEL_ED == "Primario incompleto (incluye educacion especial)" ~ 1,
        NIVEL_ED == "Primaria incompleta (incluye educacion especial)" ~ 1,
        NIVEL_ED == "Primario completo" ~ 2,
        NIVEL_ED == "Primaria completa" ~ 2,
        NIVEL_ED == "Secundario incompleto" ~ 3,
        NIVEL_ED == "Secundaria incompleta" ~ 3,
        NIVEL_ED == "Secundario completo" ~ 4,
        NIVEL_ED == "Secundaria completa" ~ 4,
        NIVEL_ED == "Superior y universitario incompleto" ~ 5,
        NIVEL_ED == "Superior universitaria incompleta" ~ 5,
        NIVEL_ED == "Superior y universitario completo" ~ 6,
        NIVEL_ED == "Superior universitaria completa" ~ 6,
        NIVEL_ED == "Sin instruccion" ~ 7,
        NIVEL_ED == "Sin instrucción" ~ 7,
        NIVEL_ED == "Ns/Nr" ~ 9,
        TRUE ~ as.numeric(as.character(NIVEL_ED))
      ),
      
      # Categoría ocupacional - códigos oficiales
      CAT_OCUP = case_when(
        CAT_OCUP == "Patron" ~ 1,
        CAT_OCUP == "Patrón" ~ 1,
        CAT_OCUP == "Cuenta propia" ~ 2,
        CAT_OCUP == "Obrero o empleado" ~ 3,
        CAT_OCUP == "Trabajador familiar sin remuneracion" ~ 4,
        CAT_OCUP == "Trabajador familiar sin remuneración" ~ 4,
        CAT_OCUP == "Ns/Nr" ~ 9,
        CAT_OCUP == "Ns./Nr." ~ 9,
        CAT_OCUP == "Ns./Nr.." ~ 9,
        CAT_OCUP == "0" ~ 0,
        TRUE ~ as.numeric(as.character(CAT_OCUP))
      ),
      # SISTEMA DE SALUD
      # CH08: ¿Tiene obra social y/o plan de salud privado o mutual?
      # 1 = Obra social (incluye PAMI)
      # 2 = Plan de salud privado o mutual
      # 3 = Tiene obra social y plan privado
      # 4 = No tiene obra social ni plan privado
      
      CH08 = case_when(
        # Para 2024 que viene como texto
        CH08 == "Obra social (incluye PAMI)" ~ 1,
        CH08 == "Mutual / Prepaga / Servicio de emergencia" ~ 2,
        CH08 == "Planes y seguros públicos" ~ 3,
        CH08 == "No paga ni le descuentan" ~ 4,
        CH08 == "Obra social y Mutual / Prepaga / Servicio de Emergencia" ~ 12,
        CH08 == "Obra social y Planes y Seguros Públicos" ~ 13,
        CH08 == "Mutual / Prepaga / Servicio de Emergencia / Planes y Seguros Publicos" ~ 23,
        CH08 == "Obra social, mutual / prepaga / servicio de emergencia y planes y seguros pubilcos" ~ 123,
        # Para años anteriores que ya vienen como números
        TRUE ~ as.numeric(as.character(CH08))
      )
    
    )
  
  message("   ✓ Normalización completada")
  
  
  # ---------------------------------------------------------------------------
  # 2.3 SELECCIONAR SOLO VARIABLES NECESARIAS
  # ---------------------------------------------------------------------------
  
  # Variables comunes a todos los años
  vars_comunes <- c("ANO4", "TRIMESTRE", "CODUSU", "COMPONENTE", "NRO_HOGAR", 
                    "REGION", "PONDERA", "CH04", "CH06","CH08", "CH10","CH12", "CH15", "CH16",
                    "ESTADO", "PP07H","NIVEL_ED", "CAT_OCUP")
  

  # Variables para calcular pobreza (requeridas por calculate_poverty)
  vars_pobreza <- c("PONDII", "PONDIH","ITF", "IPCF", "IV1", "IV2", "IV3", "IV4", 
                    "IV5", "IV6", "IV7", "IV8", "IV9", "IV10", "IV11", "IV12")
  
  # Seleccionar solo las que existen en la base
  todas_vars <- c(vars_comunes, vars_pobreza)
  vars_disponibles <- todas_vars[todas_vars %in% names(EPH_indiv)]
  
  EPH_indiv <- EPH_indiv %>% select(all_of(vars_disponibles))
  
  message("   ✓ Variables seleccionadas (", length(vars_disponibles), " variables)")
  
  # ---------------------------------------------------------------------------
  # 2.4 CALCULAR POBREZA (usando librería eph)
  # ---------------------------------------------------------------------------
  
  # Obtener líneas de pobreza regionales
  canastas_regionales <- get_poverty_lines(regional = TRUE)
  
  # Calcular pobreza en base individual
  EPH_indiv <- calculate_poverty(EPH_indiv, canastas_regionales, print_summary = FALSE)
  
  message("   ✓ Pobreza calculada")
  
  # ---------------------------------------------------------------------------
  # 2.5 RECODIFICAR VARIABLES (depende del año)
  # ---------------------------------------------------------------------------
  
  
    EPH_indiv <- EPH_indiv %>%
      mutate(
        anio = ANO4,
        trimestre = case_when(
          TRIMESTRE == "1er Trimestre" ~ 1,
          TRIMESTRE == "2do Trimestre" ~ 2,
          TRIMESTRE == "3er Trimestre" ~ 3,
          TRIMESTRE == "4to Trimestre" ~ 4,
          TRUE ~ as.numeric(TRIMESTRE)
        ),
        cod_usu = CODUSU,
        componente = COMPONENTE,
        nro_hogar = as.character(NRO_HOGAR),
        region = REGION,
        
        # Demográficas
        sexo = case_when(CH04 == 1 ~ "Hombre", CH04 == 2 ~ "Mujer"),
        edad = CH06,
        
        # Lugar de nacimiento
        pais_nacimiento = case_when(
          CH15 == 1 ~ "En esta localidad",
          CH15 == 2 ~ "En otra localidad de esta provincia",
          CH15 == 3 ~ "En otra provincia",
          CH15 == 4 ~ "En un pais limitrofe",
          CH15 == 5 ~ "En otro pais",
          CH15 == 9 ~ "NS/NR"
        ),
        
        # Residencia hace 5 años
        residencia_anterior = case_when(
          CH16 == 1 ~ "En esta localidad",
          CH16 == 2 ~ "En otra localidad de esta provincia",
          CH16 == 3 ~ "En otra provincia",
          CH16 == 4 ~ "En un pais limitrofe",
          CH16 == 5 ~ "En otro pais",
          CH16 == 6 ~ "No habia nacido",
          CH16 == 9 ~ "NS/NR"
        ),
        
        # Educación
        asistencia_educacion = case_when(
          CH10 == 1 ~ "Asiste actualmente",
          CH10 == 2 ~ "Asistió en el pasado",
          CH10 == 3 ~ "Nunca asistió"
        ),
        
        # Nivel educativo al que asiste (solo para quienes asisten actualmente)
        nivel_educativo_asiste = case_when(
          CH10 == 1 & CH12 %in% c(1, 2, 3) ~ "Primaria",
          CH10 == 1 & CH12 %in% c(4, 5) ~ "Secundaria",
          CH10 == 1 & CH12 == 6 ~ "Terciario",
          CH10 == 1 & CH12 == 7 ~ "Universitario",
          CH10 == 1 & CH12 == 8 ~ "Postgrado",
          TRUE ~ NA_character_
        ),
        
        max_nivel_educ_completado = case_when(
          NIVEL_ED %in% c(1, 2, 3) ~ "Hasta primaria completa",
          NIVEL_ED %in% c(4, 5) ~ "Hasta secundaria completa",
          NIVEL_ED %in% c(6, 7) ~ "Educación superior completa"
        ),
        
        # Mercado laboral
        estado_actividad = case_when(
          ESTADO == 1 ~ "Ocupado",
          ESTADO == 2 ~ "Desocupado",
          ESTADO == 3 ~ "Inactivo",
          ESTADO == 4 ~ "Menor de 10 años"
        ),
        
        tipo_actividad = case_when(
          CAT_OCUP == 1 ~ "Patrón",
          CAT_OCUP == 2 ~ "Cuenta propia",
          CAT_OCUP == 3 ~ "Obrero o empleado",
          CAT_OCUP == 4 ~ "Trabajador familiar sin remuneración"
        ),
        
        trabajo_incluye_jubilacion = case_when(
          PP07H == 1 ~ "Si",
          PP07H == 2 ~ "No"
        ),
        
      
        ponderacion = PONDERA,
        
        # Migración
        migrante = case_when(
          pais_nacimiento %in% c("En un pais limitrofe", "En otro pais") ~ 1,
          TRUE ~ 0
        ),
        
        migrante_reciente = case_when(
          residencia_anterior %in% c("En un pais limitrofe", "En otro pais") ~ "Si",
          TRUE ~ "No"
        ),
        
        # Edad agrupada
        edad_agrupada = case_when(
          is.na(edad) ~ "S/D",
          edad < 18 ~ "0-17",
          edad >= 18 & edad < 30 ~ "18-29",
          edad >= 30 & edad < 40 ~ "30-39",
          edad >= 40 & edad < 50 ~ "40-49",
          edad >= 50 & edad < 60 ~ "50-59",
          edad >= 60 & edad < 70 ~ "60-69",
          edad >= 70 ~ "70 y más",
          TRUE ~ "S/D"
        ),
        # SISTEMA DE SALUD
        # Criterio: se considera que usa principalmente sistema público 
        # si NO tiene obra social ni prepaga
        sistema_salud = case_when(
          # Obra social (sola o combinada con público)
          CH08 %in% c(1, 13) ~ "Obra social (incluye PAMI)",
          
          # Prepaga (sola o combinada)
          CH08 %in% c(2, 12, 23, 123) ~ "Prepaga",
          
          # Sistema Público (planes públicos o sin cobertura)
          CH08 %in% c(3, 4) ~ "Sistema Público",
          
          TRUE ~ NA_character_
        ),
        
        # Variable simplificada para el indicador (comparable con CASEN)
        usa_sistema_publico = case_when(
          sistema_salud == "Sistema Público" ~ 1,
          sistema_salud %in% c("Obra social (incluye PAMI)", "Prepaga") ~ 0,
          TRUE ~ NA_real_
        )
    )
  
  
  message("   ✓ Recodificación completada")
  
  # ---------------------------------------------------------------------------
  # 2.6 CREAR VARIABLES DERIVADAS
  # ---------------------------------------------------------------------------
  
  EPH_indiv <- EPH_indiv %>%
    mutate(
      # FORMALIDAD LABORAL - ASALARIADA
      formalidad_empleo_asalariada = case_when(
        tipo_actividad == "Obrero o empleado" & 
          trabajo_incluye_jubilacion == "Si" ~ "Asalariado formal",
        
        tipo_actividad == "Obrero o empleado" & 
          trabajo_incluye_jubilacion == "No" ~ "Asalariado informal",
        
        TRUE ~ NA_character_
      ),
      
      # FORMALIDAD LABORAL - TOTAL
      formalidad_empleo_total = case_when(
        # FORMALES
        tipo_actividad == "Obrero o empleado" & 
          trabajo_incluye_jubilacion == "Si" ~ "Formal",
        tipo_actividad == "Patrón" ~ "Formal",
        
        # INFORMALES
        tipo_actividad == "Obrero o empleado" & 
          trabajo_incluye_jubilacion == "No" ~ "Informal",
        tipo_actividad == "Cuenta propia" ~ "Informal",
        tipo_actividad == "Trabajador familiar sin remuneración" ~ "Informal",
        
        TRUE ~ NA_character_
      ),
      
      # CATEGORÍA DE INFORMALIDAD
      categoria_informalidad = case_when(
        tipo_actividad == "Obrero o empleado" & 
          trabajo_incluye_jubilacion == "Si" ~ "Asalariado formal",
        tipo_actividad == "Obrero o empleado" & 
          trabajo_incluye_jubilacion == "No" ~ "Asalariado informal",
        tipo_actividad == "Patrón" ~ "Patrón",
        tipo_actividad == "Cuenta propia" ~ "Cuentapropista",
        tipo_actividad == "Trabajador familiar sin remuneración" ~ "Trabajador familiar",
        TRUE ~ NA_character_
      )
    )
  
  message("   ✓ Variables derivadas creadas")
  
  # ---------------------------------------------------------------------------
  # 2.7 PROCESAR BASE DE HOGARES Y AGREGAR POBREZA
  # ---------------------------------------------------------------------------
  
  # Seleccionar variables necesarias de hogares
  vars_hogar <- c("ANO4", "TRIMESTRE", "CODUSU", "NRO_HOGAR", "REGION", "PONDERA")
  vars_hogar_disponibles <- vars_hogar[vars_hogar %in% names(EPH_hogar)]
  
  EPH_hogar <- EPH_hogar %>% 
    select(all_of(vars_hogar_disponibles)) %>%
    mutate(
      anio = as.numeric(as.character(ANO4)),
      trimestre = case_when(
        TRIMESTRE == "1er Trimestre" ~ 1,
        TRIMESTRE == "2do Trimestre" ~ 2,
        TRIMESTRE == "3er Trimestre" ~ 3,
        TRIMESTRE == "4to Trimestre" ~ 4,
        TRUE ~ as.numeric(as.character(TRIMESTRE))
      ),
      cod_usu = CODUSU,
      nro_hogar = as.character(NRO_HOGAR),
      region = as.numeric(as.character(REGION)),
      ponderador_hogar = PONDERA
    )
  
  # Agregar situación de pobreza a nivel hogar desde base individual
  EPH_hogar_pobreza <- EPH_indiv %>%
    group_by(anio, trimestre, cod_usu, nro_hogar) %>%  # Usar las variables ya recodificadas
    summarise(
      situacion = first(situacion),
      .groups = "drop"
    )
  
  # Unir datos de pobreza con base de hogares
  EPH_hogar <- EPH_hogar %>%
    left_join(EPH_hogar_pobreza, by = c("anio", "trimestre", "cod_usu", "nro_hogar")) %>%
    select(anio, trimestre, cod_usu, nro_hogar, region, ponderador_hogar)
  
  message("   ✓ Hogares procesados con pobreza")
  
  # ---------------------------------------------------------------------------
  # 2.8 CREAR BASE CONJUNTA
  # ---------------------------------------------------------------------------
  
  # EPH_indiv <- EPH_indiv %>% mutate(nro_hogar = as.character(nro_hogar))  # Ya convertido en línea 197
  
  eph_total <- left_join(
    EPH_hogar,
    EPH_indiv,
    by = c("anio", "trimestre", "cod_usu", "nro_hogar")
  )
  
  message("   ✓ Base conjunta creada")
  
  # ---------------------------------------------------------------------------
  # 2.9 CARACTERIZAR HOGARES CON MIGRANTES
  # ---------------------------------------------------------------------------
  

  hog_migrantes <- EPH_indiv %>%  # Usar EPH_indiv en vez de eph_total
    group_by(cod_usu, nro_hogar) %>%
    summarise(
      migrantes_en_hogar = sum(migrante, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(hogar_con_migrante = if_else(migrantes_en_hogar > 0, 1, 0))
  
  EPH_hogar <- left_join(
    EPH_hogar,
    hog_migrantes,
    by = c("cod_usu", "nro_hogar")
  )
  
  message("   ✓ Hogares con migrantes caracterizados")
  
  # ---------------------------------------------------------------------------
  # 2.10 GUARDAR EN LISTAS
  # ---------------------------------------------------------------------------
  
  lista_indiv_procesadas[[as.character(anio)]] <- EPH_indiv
  lista_hogar_procesadas[[as.character(anio)]] <- EPH_hogar
  
  # También guardar en el entorno global con nombre específico
  assign(paste0("EPH_indiv_", anio, "_proc"), EPH_indiv, envir = .GlobalEnv)
  assign(paste0("EPH_hogar_", anio, "_proc"), EPH_hogar, envir = .GlobalEnv)
  assign(paste0("EPH_total_", anio, "_proc"), eph_total, envir = .GlobalEnv)
  
  message("\n✔️ Año ", anio, " procesado exitosamente\n")
}

# =============================================================================
# PARTE 3: UNIFICAR AÑOS
# =============================================================================

message("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
message("UNIFICANDO AÑOS PROCESADOS")
message("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

if (length(lista_indiv_procesadas) > 0) {
  EPH_indiv_unificado <- bind_rows(lista_indiv_procesadas)
  message("   ✓ Base individual unificada creada")
  message("     Total de registros: ", nrow(EPH_indiv_unificado))
}

if (length(lista_hogar_procesadas) > 0) {
  EPH_hogar_unificado <- bind_rows(lista_hogar_procesadas)
  message("   ✓ Base hogares unificada creada")
  message("     Total de registros: ", nrow(EPH_hogar_unificado))
}

# =============================================================================
# PARTE 4: GUARDAR RESULTADOS (OPCIONAL)
# =============================================================================

# Descomentar si querés guardar las bases unificadas
write_csv(EPH_indiv_unificado, "Data/EPH_indiv_2016_2024_proc.csv")
write_csv(EPH_hogar_unificado, "Data/EPH_hogar_2016_2024_proc.csv")

message("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
message("PROCESAMIENTO COMPLETADO")
message("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")


