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

table(predictSampleP$pobre_lab) # No hay pobres (24/03/2026)

predictSample2 <- predictSample2 %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>%
  select(id, pobre)

table(predictSample2$pobre)            

# ============================================================
# 11A. Guardar archivo de envío del modelo 1
# ============================================================
name1_base <- make_submission_name(model1)
name1 <- paste0(tools::file_path_sans_ext(name1_base), "_Acc.csv")
output_path1 <- file.path("02_outputs", "predictions", name1)

write.csv(predictSample1, output_path1, row.names = FALSE)

cat("Archivo Accuracy guardado en:", output_path1, "\n")

# ============================================================
# 11B. Guardar archivo de envío del modelo 2
# ============================================================
name2_base <- make_submission_name(model2)
name2 <- paste0(tools::file_path_sans_ext(name2_base), "_Sen.csv")
output_path2 <- file.path("02_outputs", "predictions", name2)

write.csv(predictSample2, output_path2, row.names = FALSE)

cat("Archivo Sens guardado en:", output_path2, "\n")