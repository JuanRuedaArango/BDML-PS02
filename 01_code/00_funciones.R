# ============================================================
# 1. Funciones auxiliares
# ============================================================

# ------------------------------------------------------------
# load_bases()
# ------------------------------------------------------------
# Objetivo:
#   Cargar las cuatro bases principales del ejercicio desde una
#   carpeta base del computador.
#
# ¿Qué espera?
#   Que dentro de la ruta indicada existan estos archivos:
#   - train_hogares.csv
#   - train_personas.csv
#   - test_hogares.csv
#   - test_personas.csv
#
# ¿Qué devuelve?
#   Una lista con 4 data frames, uno por cada archivo.
#
# ¿Por qué se hace así?
#   Porque después es más cómodo trabajar con una sola función
#   que centralice la lectura de los archivos y evite repetir
#   read.csv() cuatro veces en el flujo principal.
# ------------------------------------------------------------
load_bases <- function(path = "C:/Users/herna/Downloads") {
  list(
    train_hogares  = read.csv(file.path(path, "train_hogares.csv")),
    train_personas = read.csv(file.path(path, "train_personas.csv")),
    test_hogares   = read.csv(file.path(path, "test_hogares.csv")),
    test_personas  = read.csv(file.path(path, "test_personas.csv"))
  )
}

# ------------------------------------------------------------
# pre_process_personas()
# ------------------------------------------------------------
# Objetivo:
#   Transformar la base de personas para quedarnos solo con las
#   variables que realmente necesitamos para construir variables
#   a nivel hogar.
#
# Entrada:
#   Un data frame de personas (train_personas o test_personas).
#
# Variables nuevas que crea:
#   - woman:
#       1 si la persona es mujer, 0 en caso contrario.
#       Se construye desde P6020 (en el original, 2 es mujer
#       y 1 es hombre).
#
#   - head:
#       1 si la persona es cabeza del hogar, 0 en caso contrario.
#       Se construye desde P6050 (en el original, jefe de hogar es
#       1, los otros 8 valores son otro tipo de parentesco).
#
#   - minor:
#       1 si la persona tiene 17 años o menos, 0 en caso contrario.
#       Se construye de P6040 (años cumplidos)
#       Esto permite contar menores en el hogar.
#
#   - cat_educ:
#       Categoría educativa ajustada.
#       Aquí se recodifica P6210 == 9 como 0.
#       Esto porque 9 en la original es "No se sabe, no informa",
#       por lo que no añade valor) .
#
#   - occupied:
#       1 si la persona aparece ocupada, 0 si no.
#       Se usa la variable Oc; si está en NA asumimos no ocupado.
#
# Después:
#   Se conservan solo las columnas que servirán en el resto del
#   proceso: id, Orden y las variables derivadas.
#
# ¿Por qué?
#   Porque la predicción final es a nivel hogar, pero parte de la
#   información relevante está a nivel persona. Entonces primero
#   limpiamos y simplificamos esa base.
# ------------------------------------------------------------
pre_process_personas <- function(data) {
  data %>%
    mutate(
      woman    = ifelse(P6020 == 2, 1, 0),
      head     = ifelse(P6050 == 1, 1, 0),
      minor    = ifelse(P6040 <= 17, 1, 0),
      cat_educ = ifelse(P6210 == 9, 0, P6210),
      occupied = ifelse(is.na(Oc), 0, 1)
    ) %>%
    select(id, Orden, woman, head, minor, cat_educ, occupied)
}

# ------------------------------------------------------------
# build_personas_hogar()
# ------------------------------------------------------------
# Objetivo:
#   Pasar de una base a nivel persona a variables útiles a nivel
#   hogar, agrupando por id del hogar.
#
# Entrada:
#   Un data frame ya preprocesado con la función
#   pre_process_personas().
#
# Esta función construye dos objetos:
#
# 1) personas_nivel_hogar
#    Resume información del hogar usando todas las personas del
#    mismo id:
#    - num_women: número de mujeres en el hogar
#    - num_minors: número de menores en el hogar
#    - cat_maxEduc: máximo nivel educativo observado en el hogar
#    - num_occupied: número de personas ocupadas
#
#    ¿Por qué resumir?
#    Porque el modelo final trabaja con una fila por hogar, no
#    una fila por persona.
#
# 2) personas_hogar
#    Toma únicamente la fila de la cabeza del hogar (head == 1)
#    para obtener atributos específicos de esa persona:
#    - headWoman: si la cabeza del hogar es mujer
#    - cat_educHead: educación de la cabeza del hogar
#    - occupiedHead: si la cabeza del hogar está ocupada
#
#    Luego une esos datos con el resumen agregado del hogar.
#
# ¿Qué devuelve?
#   Una lista con:
#   - personas_nivel_hogar
#   - personas_hogar
#
# Nota importante:
#   personas_hogar es el objeto que normalmente se une después
#   con la base de hogares, porque ya contiene tanto variables
#   agregadas como variables de la persona cabeza del hogar.
# ------------------------------------------------------------
build_personas_hogar <- function(personas) {
  personas_nivel_hogar <- personas %>%
    group_by(id) %>%
    summarize(
      num_women    = sum(woman, na.rm = TRUE),
      num_minors   = sum(minor, na.rm = TRUE),
      cat_maxEduc  = max(cat_educ, na.rm = TRUE),
      num_occupied = sum(occupied, na.rm = TRUE),
      .groups = "drop"
    )
  
  personas_hogar <- personas %>%
    filter(head == 1) %>%
    select(id, woman, cat_educ, occupied) %>%
    rename(
      headWoman    = woman,
      cat_educHead = cat_educ,
      occupiedHead = occupied
    ) %>%
    left_join(personas_nivel_hogar, by = "id")
  
  list(
    personas_nivel_hogar = personas_nivel_hogar,
    personas_hogar = personas_hogar
  )
}

# ------------------------------------------------------------
# prepare_hogares()
# ------------------------------------------------------------
# Objetivo:
#   Limpiar y dejar lista la base de hogares con solo las
#   variables que se van a usar en el modelo.
#
# Entrada:
#   - hogares: data frame de hogares
#   - is_train: TRUE si es la base de entrenamiento,
#               FALSE si es la base de prueba
#
# Variable nueva:
#   - rent:
#       1 si P5090 == 3, 0 en otro caso.
#       Esta variable recodifica una característica del hogar
#       que será usada como predictor.
#
# Diferencia entre train y test:
#   - En train sí existe la variable respuesta Pobre, porque
#     esa es la etiqueta que queremos aprender.
#   - En test no debemos incluir Pobre, porque justamente eso
#     es lo que vamos a predecir.
#
# ¿Qué devuelve?
#   Un data frame reducido:
#   - train: id, Dominio, rent, Pobre
#   - test : id, Dominio, rent
# ------------------------------------------------------------
prepare_hogares <- function(hogares, is_train = TRUE) {
  hogares %>%
    mutate(rent = ifelse(P5090 == 3, 1, 0)) %>%
    {
      if (is_train) {
        select(., id, Dominio, rent, Pobre)
      } else {
        select(., id, Dominio, rent)
      }
    }
}

# ------------------------------------------------------------
# convert_factors()
# ------------------------------------------------------------
# Objetivo:
#   Convertir variables categóricas al tipo factor para que el
#   modelo las interprete correctamente.
#
# Entrada:
#   - data: base final ya construida
#   - is_train: indica si estamos trabajando con entrenamiento
#
# Transformaciones:
#   1) Dominio -> factor
#      Porque representa categorías y no una magnitud numérica.
#
#   2) cat_educHead -> factor con etiquetas legibles
#      Se reemplazan los códigos numéricos por nombres de nivel
#      educativo para que el modelo y la interpretación sean más
#      claros.
#
#   3) Pobre -> factor binario con niveles "No" y "Yes"
#      Esto solo se hace en train porque es la variable objetivo.
#      Además, caret trabaja mejor con clasificación binaria si
#      la respuesta está en formato factor.
#
# ¿Qué devuelve?
#   El mismo data frame, pero con variables correctamente
#   codificadas como factores.
# ------------------------------------------------------------
convert_factors <- function(data, is_train = TRUE) {
  data <- data %>%
    mutate(
      Dominio = factor(Dominio),
      cat_educHead = factor(
        cat_educHead,
        levels = 0:6,
        labels = c(
          "No sabe", "Ninguno", "Preescolar",
          "Primaria", "Secundaria", "Media", "Universitaria"
        )
      )
    )
  
  if (is_train) {
    data <- data %>%
      mutate(
        Pobre = factor(Pobre, levels = c(0, 1), labels = c("No", "Yes"))
      )
  }
  
  data
}

# ------------------------------------------------------------
# prepare_train_test_factors()
# ------------------------------------------------------------
# Objetivo:
#   Aplicar de forma consistente la conversión de factores en
#   train y test, asegurando que test use los mismos niveles
#   de Dominio y cat_educHead que train.
#
# Entrada:
#   - train: base de entrenamiento
#   - test : base de prueba
#
# Salida:
#   Una lista con:
#   - train
#   - test
# ------------------------------------------------------------
prepare_train_test_factors <- function(train, test) {
  
  cat_educ_labels <- c(
    "0" = "No sabe",
    "1" = "Ninguno",
    "2" = "Preescolar",
    "3" = "Primaria",
    "4" = "Secundaria",
    "5" = "Media",
    "6" = "Universitaria"
  )
  
  train <- train %>%
    mutate(
      Pobre = factor(Pobre, levels = c("Yes", "No")),
      Dominio = factor(Dominio),
      cat_educHead = case_when(
        cat_educHead %in% c("No sabe", "Preescolar") ~ "Ninguno",
        TRUE ~ cat_educHead
      ),
      cat_educHead = factor(
        cat_educHead,
        levels = c("Ninguno", "Primaria", "Secundaria", "Media", "Universitaria")
      )
    )
  
  test <- test %>%
    mutate(
      Dominio = factor(Dominio, levels = levels(train$Dominio)),
      cat_educHead = recode(as.character(cat_educHead), !!!cat_educ_labels),
      cat_educHead = case_when(
        cat_educHead %in% c("No sabe", "Preescolar") ~ "Ninguno",
        TRUE ~ cat_educHead
      ),
      cat_educHead = factor(cat_educHead, levels = levels(train$cat_educHead))
    )
  
  list(
    train = train,
    test = test
  )
}


# ------------------------------------------------------------
# multiStats()
# ------------------------------------------------------------
# Objetivo:
#   Calcular varias métricas de clasificación binaria al mismo
#   tiempo durante el proceso de validación cruzada en caret.
#
# ¿Por qué sirve?
#   Porque en lugar de ver solo una métrica, permite revisar
#   simultáneamente desempeño desde distintos enfoques:
#   - twoClassSummary: ROC, Sens, Spec
#   - defaultSummary : Accuracy, Kappa
#   - prSummary      : Precision, Recall, F
#
# Requisito importante:
#   Para que funcione correctamente dentro de trainControl(),
#   se debe usar:
#   - classProbs = TRUE
#   - summaryFunction = multiStats
#
# Además:
#   La variable objetivo debe ser un factor de dos clases.
# ------------------------------------------------------------
multiStats <- function(...) {
  c(
    caret:::twoClassSummary(...),
    caret:::defaultSummary(...),
    caret:::prSummary(...)
  )
}

# ------------------------------------------------------------
# make_submission_name()
# ------------------------------------------------------------
# Objetivo:
#   Construir automáticamente el nombre del archivo de envío
#   según el algoritmo o variante del modelo utilizado.
#
# ¿Por qué sirve?
#   Porque permite guardar cada predicción con un nombre
#   informativo y consistente, incorporando los principales
#   hiperparámetros del modelo (por ejemplo alpha, lambda y,
#   cuando aplica, el threshold óptimo). Esto ayuda a:
#   - diferenciar archivos de distintos modelos,
#   - evitar sobreescrituras accidentales,
#   - rastrear fácilmente con qué configuración se generó cada
#     submission.
#
# Entrada:
#   - best_algorithm:
#       string que identifica el tipo de modelo o estrategia.
#       Ejemplos:
#       "Elastic Net con Accuracy"
#       "Elastic Net con Sens"
#       "Elastic Net weighted + threshold óptimo PR"
#       "LDA"
#       "QDA"
#
#   - model:
#       objeto entrenado con caret, necesario para extraer
#       bestTune$alpha y bestTune$lambda en los modelos que usan
#       tuning.
#
#   - best_cutoff:
#       data frame o lista con el threshold óptimo, usado solo en
#       estrategias donde además del modelo se optimiza el punto
#       de corte de clasificación.
#
# ¿Qué hace?
#   1) Formatea los hiperparámetros numéricos para que puedan
#      usarse en nombres de archivo (reemplaza "." por "_").
#   2) Revisa qué tipo de algoritmo se está guardando.
#   3) Construye un nombre de archivo coherente para esa
#      estrategia.
#
# ¿Qué devuelve?
#   Un string con el nombre final del archivo .csv.
#
# Ejemplos de salida:
#   EN_Acc_lambda_0_001_alpha_0_5.csv
#   EN_Sen_lambda_0_1_alpha_1.csv
#   EN_weighted_PR_lambda_0_0316_alpha_0_7_threshold_0_242.csv
#   LDA.csv
#   QDA.csv
# ------------------------------------------------------------
make_submission_name <- function(best_algorithm, model = NULL, best_cutoff = NULL) {
  
  format_param <- function(x, digits = 4) {
    gsub("\\.", "_", as.character(round(x, digits)))
  }
  
  if (best_algorithm == "Elastic Net con Accuracy") {
    
    lambda_str <- format_param(model$bestTune$lambda, 4)
    alpha_str  <- gsub("\\.", "_", as.character(model$bestTune$alpha))
    
    submission_name <- paste0(
      "EN_Acc_lambda_", lambda_str,
      "_alpha_", alpha_str,
      ".csv"
    )
    
  } else if (best_algorithm == "Elastic Net con Sens") {
    
    lambda_str <- format_param(model$bestTune$lambda, 4)
    alpha_str  <- gsub("\\.", "_", as.character(model$bestTune$alpha))
    
    submission_name <- paste0(
      "EN_Sen_lambda_", lambda_str,
      "_alpha_", alpha_str,
      ".csv"
    )
    
  } else if (best_algorithm == "Elastic Net weighted + threshold óptimo PR") {
    
    lambda_str    <- format_param(model$bestTune$lambda, 4)
    alpha_str     <- gsub("\\.", "_", as.character(model$bestTune$alpha))
    threshold_str <- format_param(best_cutoff$threshold, 3)
    
    submission_name <- paste0(
      "EN_weighted_PR_lambda_", lambda_str,
      "_alpha_", alpha_str,
      "_threshold_", threshold_str,
      ".csv"
    )
    
  } else if (best_algorithm == "Elastic Net + threshold óptimo PR") {
    
    lambda_str    <- format_param(model$bestTune$lambda, 4)
    alpha_str     <- gsub("\\.", "_", as.character(model$bestTune$alpha))
    threshold_str <- format_param(best_cutoff$threshold, 3)
    
    submission_name <- paste0(
      "EN_PR_lambda_", lambda_str,
      "_alpha_", alpha_str,
      "_threshold_", threshold_str,
      ".csv"
    )
    
  } else if (best_algorithm == "Elastic Net con F1") {
    
    lambda_str <- format_param(model$bestTune$lambda, 4)
    alpha_str  <- gsub("\\.", "_", as.character(model$bestTune$alpha))
    
    submission_name <- paste0(
      "EN_F1_lambda_", lambda_str,
      "_alpha_", alpha_str,
      ".csv"
    )
    
  } else if (best_algorithm == "Elastic Net + sample weighting") {
    
    lambda_str <- format_param(model$bestTune$lambda, 4)
    alpha_str  <- gsub("\\.", "_", as.character(model$bestTune$alpha))
    
    submission_name <- paste0(
      "EN_weighted_lambda_", lambda_str,
      "_alpha_", alpha_str,
      ".csv"
    )
    
  } else if (best_algorithm == "Elastic Net + downsampling") {
    
    lambda_str <- format_param(model$bestTune$lambda, 4)
    alpha_str  <- gsub("\\.", "_", as.character(model$bestTune$alpha))
    
    submission_name <- paste0(
      "EN_down_lambda_", lambda_str,
      "_alpha_", alpha_str,
      ".csv"
    )
    
  } else if (best_algorithm == "Elastic Net + upsampling") {
    
    lambda_str <- format_param(model$bestTune$lambda, 4)
    alpha_str  <- gsub("\\.", "_", as.character(model$bestTune$alpha))
    
    submission_name <- paste0(
      "EN_up_lambda_", lambda_str,
      "_alpha_", alpha_str,
      ".csv"
    )
    
  } else if (best_algorithm == "LDA") {
    
    submission_name <- "LDA.csv"
    
  } else if (best_algorithm == "QDA") {
    
    submission_name <- "QDA.csv"
    
  } else {
    stop("Algoritmo no reconocido en make_submission_name().")
  }
  
  return(submission_name)
}