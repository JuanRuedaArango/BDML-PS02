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
# make_ctrl()
# ------------------------------------------------------------
# Objetivo:
#   Crear el objeto de control que usa caret::train() para
#   entrenar y validar el modelo.
#
# Entrada:
#   metric_type:
#   - "accuracy": entrena evaluando por exactitud
#   - "sens"    : entrena evaluando por sensibilidad
#
# ¿Qué configura?
#   - method = "cv":
#       validación cruzada
#   - number = 5:
#       5 folds
#   - classProbs = TRUE:
#       calcular probabilidades de clase
#   - savePredictions = TRUE:
#       guardar predicciones generadas en validación
#
# Caso "sens":
#   Se agrega summaryFunction = twoClassSummary para que caret
#   pueda calcular métricas como sensibilidad, especificidad y
#   AUC en problemas de clasificación binaria.
#
# ¿Por qué separar esto en una función?
#   Porque así no repetimos la configuración del entrenamiento y
#   podemos cambiar fácilmente el criterio de evaluación.
# ------------------------------------------------------------
make_ctrl <- function(metric_type = c("accuracy", "sens")) {
  metric_type <- match.arg(metric_type)
  
  if (metric_type == "accuracy") {
    trainControl(
      method = "cv",
      number = 5,
      classProbs = TRUE,
      savePredictions = TRUE
    )
  } else {
    trainControl(
      method = "cv",
      number = 5,
      classProbs = TRUE,
      summaryFunction = twoClassSummary,
      savePredictions = TRUE
    )
  }
}

# ------------------------------------------------------------
# fit_elastic_net()
# ------------------------------------------------------------
# Objetivo:
#   Entrenar un modelo Elastic Net para clasificar pobreza.
#
# Entrada:
#   - train_data: base de entrenamiento ya limpia y preparada
#   - ctrl: objeto creado con make_ctrl()
#   - metric: métrica que caret debe optimizar
#   - seed: semilla para reproducibilidad
#
# Modelo que se ajusta:
#   Pobre ~ .
#   Es decir, usamos Pobre como variable respuesta y todas las
#   demás columnas como predictores.
#
# method = "glmnet":
#   Usa regresión penalizada.
#
# family = "binomial":
#   Porque la variable respuesta es binaria.
#
# tuneGrid:
#   Busca combinaciones de:
#   - alpha:
#       controla el tipo de penalización
#       alpha = 0   -> Ridge
#       alpha = 1   -> Lasso
#       0 < alpha < 1 -> Elastic Net
#
#   - lambda:
#       controla la intensidad de la penalización
#
# ¿Por qué Elastic Net?
#   Porque ayuda cuando hay varios predictores y puede manejar
#   mejor problemas de selección/regularización que un logit
#   simple sin penalización.
#
# ¿Qué devuelve?
#   El modelo entrenado por caret, incluyendo la mejor
#   combinación de alpha y lambda.
# ------------------------------------------------------------
fit_elastic_net <- function(train_data, ctrl, metric = "Accuracy", seed = 2025) {
  set.seed(seed)
  
  train(
    Pobre ~ .,
    data = train_data,
    metric = metric,
    method = "glmnet",
    family = "binomial",
    trControl = ctrl,
    tuneGrid = expand.grid(
      alpha = seq(0, 1, by = 0.1),
      lambda = 10^seq(-3, 3, length = 10)
    )
  )
}

# ------------------------------------------------------------
# make_submission_name()
# ------------------------------------------------------------
# Objetivo:
#   Construir automáticamente el nombre del archivo de envío
#   usando los mejores hiperparámetros del modelo.
#
# Entrada:
#   Un modelo entrenado con caret.
#
# ¿Qué hace?
#   - Extrae bestTune$lambda
#   - Extrae bestTune$alpha
#   - Reemplaza puntos por guiones bajos para que el nombre del
#     archivo sea más cómodo y consistente
#
# Ejemplo de salida:
#   EN_lambda_0_0215_alpha_0_7.csv
#
# ¿Por qué sirve?
#   Porque así el archivo queda identificado con la combinación
#   de hiperparámetros usada y no se sobreescribe tan fácilmente.
# ------------------------------------------------------------
make_submission_name <- function(model) {
  lambda_str <- gsub("\\.", "_", as.character(round(model$bestTune$lambda, 4)))
  alpha_str  <- gsub("\\.", "_", as.character(model$bestTune$alpha))
  
  paste0("EN_lambda_", lambda_str, "_alpha_", alpha_str, ".csv")
}