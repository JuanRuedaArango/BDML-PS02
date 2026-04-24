# ============================================================
# 9A. Entrenamiento usando Accuracy
# ============================================================

# Configuración de validación cruzada para Accuracy
ctrl_acc <- trainControl(
  method = "cv",          # validación cruzada
  number = 5,             # 5 folds
  classProbs = TRUE,      # calcular probabilidades de clase
  savePredictions = TRUE  # guardar predicciones de cada fold
)

# Fijamos semilla para reproducibilidad
set.seed(2025)

# Entrenamos el modelo Elastic Net optimizando Accuracy
model1 <- train(
  Pobre ~ .,                      # variable objetivo y predictores
  data = train,                   # base de entrenamiento
  metric = "Accuracy",            # métrica a maximizar
  method = "glmnet",              # algoritmo Elastic Net
  family = "binomial",            # clasificación binaria
  trControl = ctrl_acc,           # control definido arriba
  tuneGrid = expand.grid(
    alpha  = seq(0, 1, by = 0.1), # mezcla Ridge-Lasso
    lambda = 10^seq(-3, 3, length = 10) # penalización
  )
)

head(model1$bestTune)

# ============================================================
# 9B. Entrenamiento usando Sens
# ============================================================

# Configuración especial para Sens (requiere summaryFunction)
ctrl_sen <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary, # necesario para Sens
  savePredictions = TRUE
)

# Fijamos semilla
set.seed(2025)

# Entrenamos el modelo optimizando Sens
model2 <- train(
  Pobre ~ .,                      # variable objetivo y predictores
  data = train,
  metric = "Sens",                # métrica a maximizar
  method = "glmnet",
  family = "binomial",
  trControl = ctrl_sen,
  tuneGrid = expand.grid(
    alpha  = seq(0, 1, by = 0.1),
    lambda = 10^seq(-3, 3, length = 10)
  )
)

head(model2$bestTune)

# ============================================================
# 9C. Elastic Net ponderado + umbral óptimo
# ============================================================

# Esta estrategia combina dos ideas:
# 1) Reponderar observaciones para darle más importancia a la
#    clase minoritaria ("Yes")
# 2) Escoger un umbral de clasificación distinto de 0.5,
#    buscando el que maximiza F1 sobre predicciones out-of-fold


# ------------------------------------------------------------
# 9C.1 Pesos por desbalance de clase
# ------------------------------------------------------------
# Peso relativo de la clase positiva:
# peso(Yes) = N_No / N_Yes
# peso(No)  = 1
pos_weight <- sum(train$Pobre == "No") / sum(train$Pobre == "Yes")

case_weights <- ifelse(train$Pobre == "Yes", pos_weight, 1)

# Normalización: deja el peso promedio en 1
case_weights <- case_weights / mean(case_weights)

# Revisión rápida
pos_weight
tapply(case_weights, train$Pobre, mean)
tapply(case_weights, train$Pobre, sum)


# ------------------------------------------------------------
# 9C.2 Control de entrenamiento para optimizar F1
# ------------------------------------------------------------
# Requiere:
# - classProbs = TRUE para obtener probabilidades
# - summaryFunction = multiStats para calcular F
# - savePredictions = "final" para guardar predicciones OOF del
#   mejor tuning y luego buscar el mejor umbral

set.seed(2025)

folds <- createFolds(train$Pobre, k = 5, returnTrain = TRUE)

en_grid <- expand.grid(
  alpha  = seq(0, 1, by = 0.1),
  lambda = 10^seq(-3, 3, length = 5)
)

ctrl_f1 <- trainControl(
  method = "cv",
  number = 5,
  index = folds,
  classProbs = TRUE,
  summaryFunction = multiStats,
  savePredictions = "final",
  verboseIter = TRUE
)

elastic_net_weighted <- train(
  Pobre ~ .,
  data = train,
  method = "glmnet",
  family = "binomial",
  metric = "F",              # optimizamos F1
  trControl = ctrl_f1,
  tuneGrid = en_grid,
  weights = case_weights
)

print(elastic_net_weighted)


# ------------------------------------------------------------
# 9C.3 Extraer predicciones out-of-fold del mejor modelo
# ------------------------------------------------------------
# Nos quedamos solo con las filas de la combinación óptima
# (alpha, lambda) para calcular el umbral óptimo
elastic_oof <- elastic_net_weighted$pred %>%
  dplyr::filter(
    alpha  == elastic_net_weighted$bestTune$alpha,
    lambda == elastic_net_weighted$bestTune$lambda
  )

# Variable real y probabilidad predicha para la clase positiva
yhat <- elastic_oof$obs
phat <- elastic_oof$Yes


# ------------------------------------------------------------
# 9C.4 Curva ROC y búsqueda del umbral óptimo
# ------------------------------------------------------------
roc_elastic_net <- pROC::roc(
  response = yhat,
  predictor = phat,
  levels = c("No", "Yes"),
  direction = "<"
)

# Evaluamos una grilla de umbrales entre 0 y 1
pr_cutoffs <- data.frame(
  pROC::coords(
    roc_elastic_net,
    x = seq(0, 1, length.out = 100),
    input = "threshold",
    ret = c("threshold", "precision", "recall")
  )
)

# Calculamos F1 para cada umbral
pr_cutoffs <- pr_cutoffs %>%
  mutate(
    F1 = ifelse(
      precision + recall == 0,
      0,
      (2 * precision * recall) / (precision + recall)
    )
  )

# Umbral que maximiza F1
best_cutoff <- pr_cutoffs %>%
  arrange(desc(F1)) %>%
  slice(1)

best_cutoff

# ============================================================
# 9D. Comparación de F1 y umbral óptimo para los 3 Elastic Net
# ============================================================

# Esta función toma un modelo caret, extrae las predicciones
# out-of-fold del mejor tuning y busca el umbral que maximiza F1.

get_best_f1_cutoff <- function(model, model_name, positive_class = "Yes") {
  
  if (is.null(model$pred)) {
    stop(
      paste0(
        "El modelo ", model_name, " no tiene predicciones guardadas. ",
        "Revisa que trainControl tenga savePredictions = TRUE o savePredictions = 'final'."
      ),
      call. = FALSE
    )
  }
  
  pred_oof <- model$pred
  
  # Filtrar solo la combinación óptima de hiperparámetros
  for (param in names(model$bestTune)) {
    pred_oof <- pred_oof[pred_oof[[param]] == model$bestTune[[param]], ]
  }
  
  if (!positive_class %in% names(pred_oof)) {
    stop(
      paste0(
        "No existe la columna de probabilidad para la clase positiva: ",
        positive_class,
        ". Columnas disponibles: ",
        paste(names(pred_oof), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  y_true <- pred_oof$obs
  prob_positive <- pred_oof[[positive_class]]
  
  thresholds <- seq(0, 1, length.out = 1000)
  
  resultados <- lapply(thresholds, function(th) {
    
    y_pred <- ifelse(prob_positive >= th, positive_class, "No")
    
    TP <- sum(y_true == positive_class & y_pred == positive_class, na.rm = TRUE)
    FP <- sum(y_true != positive_class & y_pred == positive_class, na.rm = TRUE)
    FN <- sum(y_true == positive_class & y_pred != positive_class, na.rm = TRUE)
    
    precision <- ifelse(TP + FP == 0, 0, TP / (TP + FP))
    recall    <- ifelse(TP + FN == 0, 0, TP / (TP + FN))
    
    F1 <- ifelse(
      precision + recall == 0,
      0,
      2 * precision * recall / (precision + recall)
    )
    
    data.frame(
      modelo = model_name,
      threshold = th,
      precision = precision,
      recall = recall,
      F1 = F1
    )
  })
  
  resultados <- dplyr::bind_rows(resultados)
  
  mejor_resultado <- resultados %>%
    dplyr::arrange(dplyr::desc(F1)) %>%
    dplyr::slice(1)
  
  return(mejor_resultado)
}

best_f1_model1 <- get_best_f1_cutoff(
  model = model1,
  model_name = "Elastic Net Accuracy"
)

best_f1_model2 <- get_best_f1_cutoff(
  model = model2,
  model_name = "Elastic Net Sens"
)

best_f1_model3 <- get_best_f1_cutoff(
  model = elastic_net_weighted,
  model_name = "Elastic Net ponderado + cutoff"
)

tabla_f1_elastic_net <- dplyr::bind_rows(
  best_f1_model1,
  best_f1_model2,
  best_f1_model3
)

cat("\n============================================================\n")
cat("Comparación de umbral óptimo y F1 - Elastic Net\n")
cat("============================================================\n")

print(tabla_f1_elastic_net)

# ============================================================
# 10A. Predicción sobre test con modelo 1 (Accuracy)
# ============================================================

predictSample1 <- test %>%
  mutate(
    pobre_lab = predict(model1, newdata = test, type = "raw")
  ) %>%
  mutate(
    pobre = ifelse(pobre_lab == "Yes", 1, 0)
  ) %>%
  select(id, pobre)

table(predictSample1$pobre)

# ============================================================
# 10B. Predicción sobre test con modelo 2 (Sens)
# ============================================================

predictSample2 <- test %>%
  mutate(pobre_lab = predict(model2, newdata = test, type = "raw")) %>%
  select(id,pobre_lab)

table(predictSample2$pobre_lab)

predictSample2 <- predictSample2 %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>%
  select(id, pobre)

table(predictSample2$pobre)

# ============================================================
# 10C. Predicción sobre test con modelo ponderado + cutoff óptimo
# ============================================================

# Probabilidad predicha de pertenecer a la clase "Yes"
predictSample3 <- test %>%
  mutate(
    pobre_prob = predict(elastic_net_weighted, newdata = test, type = "prob")[, "Yes"],
    pobre = ifelse(pobre_prob >= best_cutoff$threshold, 1, 0)
  ) %>%
  select(id, pobre)

head(predictSample3)

table(predictSample3$pobre)

# ============================================================
# 11A. Guardar archivo de envío del modelo 1
# ============================================================

submission_name1 <- make_submission_name(
  best_algorithm = "Elastic Net con Accuracy",
  model = model1
)

output_path1 <- file.path("02_outputs", "predictions", submission_name1)

write.csv(predictSample1, output_path1, row.names = FALSE)

cat("Archivo Accuracy guardado en:", output_path1, "\n")


# ============================================================
# 11B. Guardar archivo de envío del modelo 2
# ============================================================

submission_name2 <- make_submission_name(
  best_algorithm = "Elastic Net con Sens",
  model = model2
)

output_path2 <- file.path("02_outputs", "predictions", submission_name2)

write.csv(predictSample2, output_path2, row.names = FALSE)

cat("Archivo Sens guardado en:", output_path2, "\n")

# ============================================================
# 11C. Guardar archivo de envío del modelo ponderado + cutoff
# ============================================================

submission_name3 <- make_submission_name(
  best_algorithm = "Elastic Net + threshold óptimo PR",
  model = elastic_net_weighted,
  best_cutoff = best_cutoff
)

output_path3 <- file.path("02_outputs", "predictions", submission_name3)

write.csv(predictSample3, output_path3, row.names = FALSE)

cat("Archivo ponderado + cutoff óptimo guardado en:", output_path3, "\n")