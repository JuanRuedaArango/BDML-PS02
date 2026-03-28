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
  "glmnet", # Para estimar Elastic NET
  "reticulate",
  "readr",
  "readxl",
  "arrow",
  "jsonlite",
  "pROC",
  "Metrics",
  "rio"
  
)


# Función auxiliar para instalar paquetes si no están disponibles ----
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    cat("Instalando paquetes faltantes:", paste(new_packages, collapse=", "), "\n")
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
# source("01_code/00_webscrapping.R")
# source("01_code/01_data_cleaning.R")
# source("01_code/02_seccion1.R")
# source("01_code/03_seccion2.R")
# source("01_code/04_seccion3.R")
