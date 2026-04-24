# ==============================================================
# Taller 2 Big Data
# Hernan Yepes, Juan Rueda y Andrés Suárez
# ==============================================================
# MASTER SCRIPT

# Running this file reproduces all results in the repository.
# ==============================================================

# ==============================================================
# SECTION 1: Preparing the environment
# ==============================================================

FULL_RESET <- FALSE

if (isTRUE(FULL_RESET)) {
  rm(list = setdiff(ls(envir = .GlobalEnv), "FULL_RESET"), envir = .GlobalEnv)
}

find_project_root <- function(start = getwd(), max_up = 10) {
  candidate <- normalizePath(start, winslash = "/", mustWork = FALSE)
  
  for (i in 0:max_up) {
    marker_file <- file.path(candidate, "01_code", "00_funciones.R")
    
    if (file.exists(marker_file)) {
      return(candidate)
    }
    
    parent <- dirname(candidate)
    
    if (identical(parent, candidate)) {
      break
    }
    
    candidate <- parent
  }
  
  stop(
    paste0(
      "No se encontró la raíz del proyecto.\n",
      "Se esperaba encontrar: 01_code/00_funciones.R\n",
      "Directorio inicial revisado: ", start
    ),
    call. = FALSE
  )
}

set_project_wd <- function(max_up = 10) {
  starts <- c(getwd())
  
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    
    ctx_path <- try(rstudioapi::getActiveDocumentContext()$path, silent = TRUE)
    
    if (!inherits(ctx_path, "try-error") &&
        length(ctx_path) == 1 &&
        !is.na(ctx_path) &&
        nzchar(ctx_path)) {
      starts <- c(dirname(ctx_path), starts)
    }
  }
  
  starts <- unique(starts)
  
  for (start in starts) {
    root <- try(find_project_root(start, max_up = max_up), silent = TRUE)
    
    if (!inherits(root, "try-error")) {
      setwd(root)
      return(invisible(TRUE))
    }
  }
  
  stop(
    "No se pudo fijar el working directory en la raíz del repositorio.",
    call. = FALSE
  )
}

set_project_wd()

cat("Working directory del repositorio:\n")
print(getwd())

# ==============================================================
# Ruta externa a la carpeta Bases
# ==============================================================

possible_bases_dirs <- c(
  file.path(getwd(), "Bases"),
  file.path(dirname(getwd()), "Bases"),
  file.path(Sys.getenv("USERPROFILE"), "OneDrive", "Documentos", "Bases"),
  file.path(Sys.getenv("USERPROFILE"), "Documentos", "Bases")
)

possible_bases_dirs <- normalizePath(
  possible_bases_dirs,
  winslash = "/",
  mustWork = FALSE
)

BASES_DIR <- possible_bases_dirs[dir.exists(possible_bases_dirs)][1]

if (is.na(BASES_DIR)) {
  stop(
    paste0(
      "No se encontró la carpeta Bases.\n\n",
      "Rutas revisadas:\n",
      paste("-", possible_bases_dirs, collapse = "\n")
    ),
    call. = FALSE
  )
}

options(bases_dir = BASES_DIR)

cat("Carpeta Bases encontrada en:\n")
print(BASES_DIR)

# ==============================================================
# SECTION 2: Loading packages
# ==============================================================


# Lista de paquetes requeridos para el análisis completo
required_packages <- c(
  "tidyverse",
  "magrittr",
  "dplyr",
  "stargazer",
  "tibble",
  "caret",
  "xtable",
  "scales",
  "glmnet",     # Para estimar Elastic NET
  "lightgbm",   # Para estimar LightGBM
  "ranger",     # Para estimar Random Forest (más rápido que randomForest)
  "reticulate",
  "readr",
  "readxl",
  "arrow",
  "jsonlite",
  "pROC",
  "Metrics",
  "rio",
  "MLmetrics",
  "gbm",          # Para 08_GBM.R
  "naivebayes",   # Para 09_Bayes.R (backend alternativo)
  "klaR",         # Requerido por caret::train(method = "nb")
  "gt",           # Para tablas en 06_Estadistica descriptiva.R
  "forcats"       # Utilidades factor para descriptivo
)


# Función auxiliar para instalar paquetes si no están disponibles ----
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) {
    cat("Instalando paquetes faltantes:", paste(new_packages, collapse = ", "), "\n")
    install.packages(new_packages)
  } else {
    cat("Todos los paquetes ya están instalados.\n")
  }
}
install_if_missing(required_packages)

# Cargar todas las librerías necesarias
lapply(required_packages, function(pkg) {
  cat("Cargando paquete:", pkg, "...\n")
  library(pkg, character.only = TRUE)
})

library(boot)
library(ggplot2)

# MASS (cargado como dependencia de klaR para caret::train method="nb")
# enmascara dplyr::select. No podemos desadjuntarlo porque klaR lo
# requiere. Solución: bindear los verbos de dplyr en el global env,
# que tiene precedencia sobre cualquier paquete del search path.
suppressPackageStartupMessages(library(dplyr))
select <- dplyr::select
filter <- dplyr::filter

# ==============================================================
# SECTION 3: Selección de algoritmos
# ==============================================================

opciones <- c(
  "Todos (descriptivo + 7 algoritmos + stacking)",
  "Estadísticas descriptivas",
  "Elastic Net",
  "LightGBM",
  "Random Forest",
  "GLM (MPL / Logit)",
  "GBM (gradient boosting machine)",
  "Naive Bayes",
  "LightGBM + Random Forest",
  "LightGBM + Random Forest + Stacking",
  "Los 5 algoritmos requeridos (EN + LGBM + RF + GLM + Bayes)",
  "Los 5 requeridos + GBM + Stacking",
  "Salir"
)

# ==============================================================
# SECTION 4: Running scripts
# ==============================================================

# Cambia a TRUE solo si quieres obligar a correr de nuevo:
# 00_funciones.R, 01_limpieza.R y 02_feature_engineering.R
FORZAR_PREPARACION <- FALSE

preparar_datos <- function() {
  
  preparacion_ya_cargada <- isTRUE(
    get0(".PREPARACION_BDML_OK", envir = .GlobalEnv, ifnotfound = FALSE)
  )
  
  if (!preparacion_ya_cargada || isTRUE(FORZAR_PREPARACION)) {
    
    cat("\n==============================================================\n")
    cat("Corriendo preparación base\n")
    cat("==============================================================\n")
    
    source("01_code/00_funciones.R", local = .GlobalEnv)
    source("01_code/01_limpieza.R", local = .GlobalEnv)
    source("01_code/02_feature_engineering.R", local = .GlobalEnv)
    
    assign(".PREPARACION_BDML_OK", TRUE, envir = .GlobalEnv)
    
    cat("\nPreparación base completada.\n")
    
  } else {
    
    cat("\n==============================================================\n")
    cat("Preparación base ya cargada\n")
    cat("==============================================================\n")
    cat("No se vuelven a correr funciones, limpieza ni feature engineering.\n")
  }
}

correr_modelos <- function(seleccion) {
  
  RUN_DESCRIPTIVO   <- seleccion %in% c(1, 2)
  RUN_ELASTIC_NET   <- seleccion %in% c(1, 3, 11, 12)
  RUN_LIGHTGBM      <- seleccion %in% c(1, 4, 9, 10, 11, 12)
  RUN_RANDOM_FOREST <- seleccion %in% c(1, 5, 9, 10, 11, 12)
  RUN_GLM           <- seleccion %in% c(1, 6, 11, 12)
  RUN_GBM           <- seleccion %in% c(1, 7, 12)
  RUN_BAYES         <- seleccion %in% c(1, 8, 11, 12)
  RUN_STACKING      <- seleccion %in% c(1, 10, 12)
  
  cat("\nModelos seleccionados:\n")
  cat("  Descriptivo   :", RUN_DESCRIPTIVO,   "\n")
  cat("  Elastic Net   :", RUN_ELASTIC_NET,   "\n")
  cat("  LightGBM      :", RUN_LIGHTGBM,      "\n")
  cat("  Random Forest :", RUN_RANDOM_FOREST, "\n")
  cat("  GLM           :", RUN_GLM,           "\n")
  cat("  GBM           :", RUN_GBM,           "\n")
  cat("  Naive Bayes   :", RUN_BAYES,         "\n")
  cat("  Stacking      :", RUN_STACKING,      "\n\n")
  
  if (RUN_DESCRIPTIVO) {
    cat("\nCorriendo estadísticas descriptivas...\n")
    source("01_code/10_Estadisticas_descriptivas.R", local = .GlobalEnv)
  }
  
  if (RUN_ELASTIC_NET) {
    cat("\nCorriendo Elastic Net...\n")
    source("01_code/03_ElasticNet.R", local = .GlobalEnv)
  }
  
  if (RUN_LIGHTGBM) {
    cat("\nCorriendo LightGBM...\n")
    source("01_code/04_LightGBM.R", local = .GlobalEnv)
  }
  
  if (RUN_RANDOM_FOREST) {
    cat("\nCorriendo Random Forest...\n")
    source("01_code/05_RandomForest.R", local = .GlobalEnv)
  }
  
  if (RUN_GLM) {
    cat("\nCorriendo GLM...\n")
    source("01_code/07_GLM.R", local = .GlobalEnv)
  }
  
  if (RUN_GBM) {
    cat("\nCorriendo GBM...\n")
    source("01_code/08_GBM.R", local = .GlobalEnv)
  }
  
  if (RUN_BAYES) {
    cat("\nCorriendo Naive Bayes...\n")
    source("01_code/09_Bayes.R", local = .GlobalEnv)
  }
  
  if (RUN_STACKING) {
    cat("\nCorriendo Stacking...\n")
    source("01_code/06_Stacking.R", local = .GlobalEnv)
  }
  
  cat("\nEjecución finalizada para la selección actual.\n")
}

# Primero prepara datos una sola vez.
preparar_datos()

# Luego deja al usuario volver al menú cuantas veces quiera.
repeat {
  
  cat("\n==============================================================\n")
  cat("MENÚ PRINCIPAL DE EJECUCIÓN\n")
  cat("==============================================================\n")
  cat("Selecciona qué deseas correr.\n")
  
  seleccion <- menu(opciones)
  
  if (seleccion == 0) {
    cat("\nNo se seleccionó ninguna opción. Saliendo del menú.\n")
    break
  }
  
  if (seleccion == length(opciones)) {
    cat("\nSaliendo del menú de ejecución.\n")
    break
  }
  
  correr_modelos(seleccion)
  
  cat("\n¿Deseas volver al menú para correr otra opción?\n")
  volver <- menu(c("Sí, volver al menú", "No, terminar"))
  
  if (volver != 1) {
    cat("\nProceso terminado por el usuario.\n")
    break
  }
}