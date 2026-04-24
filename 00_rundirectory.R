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

rm(list = ls())

# Fija el working directory a la raíz del proyecto.
# Estrategia en cascada:
#   1) Si estamos en RStudio y este archivo es el documento activo,
#      usamos su ruta.
#   2) Si no, buscamos hacia arriba desde el WD actual hasta encontrar
#      una carpeta que contenga "Bases/" (marca del proyecto).
# Así el script corre igual al darle Source desde cualquier archivo
# abierto, desde la terminal, o con `Rscript`.

set_project_wd <- function(max_up = 6) {
  # 1) Intento RStudio
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()) {
    ctx_path <- try(rstudioapi::getActiveDocumentContext()$path, silent = TRUE)
    if (!inherits(ctx_path, "try-error") && nzchar(ctx_path)) {
      candidate <- dirname(ctx_path)
      # Sube directorios hasta encontrar "Bases/"
      for (i in 0:max_up) {
        if (dir.exists(file.path(candidate, "Bases"))) {
          setwd(candidate); return(invisible(TRUE))
        }
        candidate <- dirname(candidate)
      }
    }
  }
  # 2) Fallback: sube desde el WD actual
  candidate <- getwd()
  for (i in 0:max_up) {
    if (dir.exists(file.path(candidate, "Bases"))) {
      setwd(candidate); return(invisible(TRUE))
    }
    candidate <- dirname(candidate)
  }
  warning("No se encontró la carpeta 'Bases/'. Fija el WD manualmente con setwd().")
  invisible(FALSE)
}

set_project_wd()

cat("Working directory:\n")
print(getwd())

# Crear carpetas de output si no existen
for (path in c("02_outputs/figures",
               "02_outputs/tables",
               "02_outputs/predictions")) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

# ==============================================================
# SECTION 2: Loading packages
# ==============================================================


# Lista de paquetes requeridos para el análisis completo
required_packages <- c(
  "rvest",      # Para web scraping (no se usa)
  "httr", #Hace solicitudes http a urls (no se usa)
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
# Los pasos de limpieza y feature engineering siempre se ejecutan.
# El usuario elige qué modelos correr.

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
  "Los 5 requeridos + GBM + Stacking"
)

cat("\n¿Qué modelos deseas correr?\n")
seleccion <- menu(opciones)

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


# ==============================================================
# SECTION 4: Running scripts
# ==============================================================
source("01_code/00_funciones.R")
source("01_code/01_limpieza.R")
source("01_code/02_feature_engineering.R")

if (RUN_DESCRIPTIVO)   source("01_code/10_Estadisticas_descriptivas.R")
if (RUN_ELASTIC_NET)   source("01_code/03_ElasticNet.R")
if (RUN_LIGHTGBM)      source("01_code/04_LightGBM.R")
if (RUN_RANDOM_FOREST) source("01_code/05_RandomForest.R")
if (RUN_GLM)           source("01_code/07_GLM.R")
if (RUN_GBM)           source("01_code/08_GBM.R")
if (RUN_BAYES)         source("01_code/09_Bayes.R")
if (RUN_STACKING)      source("01_code/06_Stacking.R")
