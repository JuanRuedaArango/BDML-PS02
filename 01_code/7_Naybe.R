# ============================================================
# NAIVE BAYES: múltiples variantes para predicción de pobreza
# ============================================================
#
# Base disponible:
# colnames(train)
#
# Se usan SOLO variables existentes en train/test.
# No se crean nuevas variables adicionales.
#
# Modelos:
#   A) Base estructural
#   B) Capital humano + laboral
#   C) Vivienda + entorno
#   D) Jefe del hogar
#   E) Full lineal
#   F) Full + pesos / sampling
#   G) Comparación final
#
# Luego:
#   - Comparación CV
#   - Selección mejor modelo
#   - Umbral óptimo F1 usando OOF
# ============================================================

# ============================================================
# 0. Librerías
# ============================================================
library(caret)
library(dplyr)
library(pROC)
library(klaR)

# ============================================================
# 1. Control de entrenamiento
# ============================================================
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = TRUE
)

set.seed(2025)

# ============================================================
# 2. Preparación clase
# ============================================================
train$Pobre <- factor(train$Pobre, levels = c("Yes", "No"))

# ============================================================
# 3. Grid NB
# ============================================================
grid_nb <- expand.grid(
  fL = c(0, 1, 2),
  usekernel = c(TRUE, FALSE),
  adjust = 1
)

# ============================================================
# MODELO BASE
# ============================================================
model_base_nb <- train(
  Pobre ~ num_minors + prop_dependiente + prop_informal + n_personas +
    headWoman + cat_educHead + cat_maxEduc + formalHead,
  data = train,
  method = "nb",
  metric = "Sens",
  trControl = ctrl,
  tuneGrid = grid_nb,
  preProcess = c("center", "scale")
)

# ============================================================
# M1
# ============================================================
model1_nb <- train(
  Pobre ~ num_minors + prop_dependiente + prop_informal + n_personas,
  data = train,
  method = "nb",
  metric = "Sens",
  trControl = ctrl,
  tuneGrid = grid_nb,
  preProcess = c("center", "scale")
)

# ============================================================
# M2
# ============================================================
model2_nb <- train(
  Pobre ~ cat_educHead + formalHead + prop_informal + persons_per_worker,
  data = train,
  method = "nb",
  metric = "Sens",
  trControl = ctrl,
  tuneGrid = grid_nb,
  preProcess = c("center", "scale")
)

# ============================================================
# M3
# ============================================================
model3_nb <- train(
  Pobre ~ n_personas + prop_dependiente + prop_informal + sin_ocupados,
  data = train,
  method = "nb",
  metric = "Sens",
  trControl = ctrl,
  tuneGrid = grid_nb,
  preProcess = c("center", "scale")
)

# ============================================================
# M4
# ============================================================
model4_nb <- train(
  Pobre ~ headWoman + cat_educHead + formalHead,
  data = train,
  method = "nb",
  metric = "Sens",
  trControl = ctrl,
  tuneGrid = grid_nb,
  preProcess = c("center", "scale")
)

# ============================================================
# FULL
# ============================================================
model5_nb <- train(
  Pobre ~ num_minors + prop_dependiente + prop_informal + n_personas +
    headWoman + cat_educHead + cat_maxEduc + formalHead +
    persons_per_worker + sin_ocupados,
  data = train,
  method = "nb",
  metric = "Sens",
  trControl = ctrl,
  tuneGrid = grid_nb,
  preProcess = c("center", "scale")
)

# ============================================================
# PESOS
# ============================================================
pos_weight <- sum(train$Pobre == "No") / sum(train$Pobre == "Yes")
train$w <- ifelse(train$Pobre == "Yes", pos_weight, 1)

model6_nb <- train(
  Pobre ~ num_minors + prop_dependiente + prop_informal + n_personas +
    cat_educHead + formalHead,
  data = train,
  method = "nb",
  metric = "Sens",
  weights = train$w,
  trControl = ctrl,
  tuneGrid = grid_nb,
  preProcess = c("center", "scale")
)

# ============================================================
# DOWN
# ============================================================
ctrl_down <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  sampling = "down",
  savePredictions = TRUE
)

model7_nb <- train(
  Pobre ~ num_minors + prop_dependiente + prop_informal + n_personas +
    cat_educHead + formalHead,
  data = train,
  method = "nb",
  metric = "Sens",
  trControl = ctrl_down,
  tuneGrid = grid_nb,
  preProcess = c("center", "scale")
)

# ============================================================
# UP
# ============================================================
ctrl_up <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  sampling = "up",
  savePredictions = TRUE
)

model8_nb <- train(
  Pobre ~ num_minors + prop_dependiente + prop_informal + n_personas +
    cat_educHead + formalHead,
  data = train,
  method = "nb",
  metric = "Sens",
  trControl = ctrl_up,
  tuneGrid = grid_nb,
  preProcess = c("center", "scale")
)

# ============================================================
# COMPARACIÓN
# ============================================================
results_nb <- resamples(list(
  Base = model_base_nb,
  M1 = model1_nb,
  M2 = model2_nb,
  M3 = model3_nb,
  M4 = model4_nb,
  Full = model5_nb,
  Weights = model6_nb,
  Down = model7_nb,
  Up = model8_nb
))

summary(results_nb)

# ============================================================
# THRESHOLD F1
# ============================================================
metrics <- function(y_true, y_pred) {
  tp <- sum(y_true == "Yes" & y_pred == "Yes")
  fp <- sum(y_true == "No" & y_pred == "Yes")
  fn <- sum(y_true == "Yes" & y_pred == "No")
  
  precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
  recall <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
  
  f1 <- ifelse(precision + recall == 0, 0,
               2 * precision * recall / (precision + recall))
  
  c(precision = precision, recall = recall, F1 = f1)
}

nb_oof <- model7_nb$pred
y_true <- nb_oof$obs
p_hat <- nb_oof$Yes

thresholds <- seq(0, 1, 0.01)

results_thr <- do.call(rbind, lapply(thresholds, function(t) {
  y_pred <- ifelse(p_hat > t, "Yes", "No")
  m <- metrics(y_true, y_pred)
  
  data.frame(
    threshold = t,
    precision = m["precision"],
    recall = m["recall"],
    F1 = m["F1"]
  )
}))

best_cutoff_nb <- results_thr %>%
  arrange(desc(F1)) %>%
  slice(1)

best_cutoff_nb