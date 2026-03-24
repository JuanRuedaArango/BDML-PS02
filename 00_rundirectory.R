##########################################################
# Master script
#
# Running this file reproduces all results in the repository.
#
# To reproduce all results, run:
# from an interactive R session: source("01_code/00_rundirectory.R")   
# or from the command line: R CMD BATCH 01_code/00_rundirectory.R
#
# Authors:
# - Hernán Yepes
# - Juan Rueda
# - Andrés Suárez
##########################################################

# ==============================================================
# Taller 1 Big Data
# (Sebastián Rodríguez, Hernan Yepez y Michael Salcedo)
# MAIN / RUN DIRECTORY
# ==============================================================

# ==============================================================
# SECCIÓN 1: Preparando el entorno
# ==============================================================

rm(list = ls())

cat("Working directory:\n")
print(getwd())

# Crear carpetas de output si no existen
for (path in c("02_output/figures",
               "02_output/tables")) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

# ==============================================================
# SECCIÓN 2: Cargue de paquetes
# ==============================================================


# Lista de paquetes requeridos para el análisis completo
required_packages <- c(
  "rvest",      # Para web scraping
  "httr", #Hace solicitudes http a urls
  "tidyverse",
  "magrittr",
  "dplyr",
  "stargazer",
  "tibble", 
  "caret",
  "xtable", 
  "scales"
  
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
# SECCIÓN 3: Ejecución de scripts
# ==============================================================
*/
source("01_code/00_webscrapping.R")
source("01_code/01_data_cleaning.R")
source("01_code/02_seccion1.R")
source("01_code/03_seccion2.R")
source("01_code/04_seccion3.R")
*/
