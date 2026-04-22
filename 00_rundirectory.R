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

# Fija el working directory a la carpeta donde vive este script.
# Esto es necesario cuando se corre con Source en RStudio sin un
# .Rproj abierto, porque en ese caso R no cambia el directorio
# automáticamente y las rutas relativas no se resuelven bien.
if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

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
  "reticulate",
  "readr",
  "readxl",
  "arrow",
  "jsonlite",
  "pROC",
  "Metrics",
  "rio",
  "MLmetrics"
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

# ==============================================================
# SECTION 3: Running scripts
# ==============================================================
source("01_code/00_funciones.R")
source("01_code/01_limpieza.R")
source("01_code/02_feature_engineering.R")
source("01_code/03_ElasticNet.R")
source("01_code/04_LightGBM.R")
