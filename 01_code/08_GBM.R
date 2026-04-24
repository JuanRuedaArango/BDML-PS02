# ============================================================
# GRADIENT BOOSTING MACHINE (GBM): múltiples variantes para
# predicción de pobreza
# ============================================================
#
# Base disponible: train
#
# Se usan SOLO variables existentes en train/test.
# No se crean nuevas variables adicionales.
#
# Modelos:
#   A) Base estructural (variables socioeconómicas básicas)
#   B) Capital humano + laboral
#   C) Vivienda + entorno
#   D) Jefe del hogar
#   E) Full model (todas las variables)
#   F) Full + tuning de hiperparámetros
#   G) Modelos con distintos criterios (ROC vs Sens)
#   H) Modelos con reducción de hiperparámetros
#
# Estrategia:
#   - Cross-validation (CV 5-fold)
#   - Optimización de hiperparámetros (grid search)
#   - Ajuste de pesos / balance de clases
#   - Selección de threshold óptimo (F1 / ROC / Sens)
#
# Evaluación:
#   - ROC-AUC
#   - Sensibilidad (Recall)
#   - Especificidad
#   - Accuracy
#   - Kappa
#   - F1-score (con OOF predictions)
#
# Resultado final:
#   - Selección del mejor modelo GBM
#   - Predicción en test
#   - Exportación de submission
# ============================================================

# ============================================================
# 0. LIBRERÍAS
# ============================================================
library(caret)
library(dplyr)
library(MLmetrics)


# ============================================================
# 1. VARIABLES BASE
# ============================================================
vars <- c(
  "num_minors",
  "prop_dependiente",
  "avg_educ_adultos",
  "formalHead",
  "prop_informal",
  "sin_ocupados",
  "cuartos_por_persona",
  "Pobre"
)

train_clean <- train[, vars]
test_w <- test[, setdiff(vars, "Pobre")]


# ============================================================
# 2. PESOS CLASE
# ============================================================
tab <- table(train_clean$Pobre)

peso_pobre <- as.numeric(tab["No pobre"] / tab["Pobre"])

w <- ifelse(train_clean$Pobre == "Pobre", peso_pobre, 1)


# ============================================================
# 3. CONTROL CV (RÁPIDO)
# ============================================================
ctrl_fast <- trainControl(
  method = "cv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)


# ============================================================
# 4. GRID RÁPIDO
# ============================================================
grid_fast <- expand.grid(
  interaction.depth = c(2, 3),
  n.trees = c(150, 300),
  shrinkage = c(0.03),
  n.minobsinnode = c(10)
)


# ============================================================
# 5. ENTRENAMIENTO GBM (PESADO + WEIGHTS)
# ============================================================
set.seed(123)

gbm_w <- train(
  Pobre ~ .,
  data = train_clean,
  method = "gbm",
  metric = "ROC",
  trControl = ctrl_fast,
  tuneGrid = grid_fast,
  weights = w,
  verbose = FALSE
)


# ============================================================
# 6. PREDICCIONES CV INTERNAS
# ============================================================
preds <- gbm_w$pred
best <- gbm_w$bestTune

preds_best <- preds %>%
  filter(
    n.trees == best$n.trees,
    interaction.depth == best$interaction.depth,
    shrinkage == best$shrinkage,
    n.minobsinnode == best$n.minobsinnode
  )


# ============================================================
# 7. BÚSQUEDA DE THRESHOLD (F1)
# ============================================================
prob_col <- colnames(preds_best)[
  which(colnames(preds_best) %in% levels(train_clean$Pobre))[2]
]

cuts <- seq(0.30, 0.80, 0.01)

results_f1 <- data.frame()

for(cut in cuts){
  
  pred_class <- ifelse(preds_best[[prob_col]] > cut,
                       "Pobre",
                       "No pobre")
  
  pred_class <- factor(pred_class,
                       levels = levels(train_clean$Pobre))
  
  f1 <- F1_Score(
    y_true = preds_best$obs,
    y_pred = pred_class,
    positive = "Pobre"
  )
  
  results_f1 <- rbind(
    results_f1,
    data.frame(
      threshold = cut,
      F1 = f1
    )
  )
}

results_f1 <- results_f1 %>%
  arrange(desc(F1))

best_cut <- results_f1$threshold[1]


# ============================================================
# 8. PREDICCIÓN TEST FINAL
# ============================================================
prob_test_df <- predict(gbm_w, test_w, type = "prob")
prob_test <- prob_test_df[,2]

pred_test <- ifelse(prob_test > best_cut,
                    "Pobre",
                    "No pobre")

pred_test <- data.frame(
  id = test$id,
  prob_pobre = prob_test,
  prediccion = pred_test
)

head(pred_test)


# ============================================================
# 9. MODELO FINAL (VARIABLES EXPANDIDAS)
# ============================================================
vars_gbm <- c(
  "persons_per_worker",
  "prop_informal",
  "cuartos_por_persona",
  "avg_educ_adultos",
  "capital_hogar",
  "sin_ocupados",
  "tasa_desempleo_hogar",
  "stress_hogar",
  "prop_dependiente",
  "sin_sanitario",
  "hacinamiento",
  "sin_agua_red",
  "dependencia_menores",
  "zona_rural",
  "Pobre"
)

train_gbm <- train[, vars_gbm]
test_gbm  <- test[, setdiff(vars_gbm, "Pobre")]

train_gbm$Pobre <- factor(train_gbm$Pobre, levels = c("No","Yes"))


# ============================================================
# 10. CONTROL FINAL
# ============================================================
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  sampling = "down",
  savePredictions = "final"
)


# ============================================================
# 11. GRID FINAL (ESTABLE)
# ============================================================
grid_gbm <- expand.grid(
  interaction.depth = c(3,4,5),
  n.trees = c(800, 1200, 1500),
  shrinkage = c(0.01, 0.02),
  n.minobsinnode = c(10, 20)
)


# ============================================================
# 12. ENTRENAMIENTO FINAL GBM
# ============================================================
gbm_fit <- train(
  Pobre ~ .,
  data = train_gbm,
  method = "gbm",
  metric = "ROC",
  trControl = ctrl,
  tuneGrid = grid_gbm,
  verbose = FALSE
)


# ============================================================
# 13. THRESHOLD FINAL (F1 MANUAL)
# ============================================================
preds <- gbm_fit$pred
best <- gbm_fit$bestTune

preds <- preds %>%
  filter(
    n.trees == best$n.trees,
    interaction.depth == best$interaction.depth,
    shrinkage == best$shrinkage
  )

preds$real <- ifelse(preds$obs == "Yes", 1, 0)

cutoffs <- seq(0.5, 0.8, 0.01)

resultados <- lapply(cutoffs, function(cut){
  
  pred_bin <- ifelse(preds$Yes >= cut, 1, 0)
  
  TP <- sum(pred_bin == 1 & preds$real == 1)
  FP <- sum(pred_bin == 1 & preds$real == 0)
  FN <- sum(pred_bin == 0 & preds$real == 1)
  
  precision <- ifelse((TP+FP)==0, 0, TP/(TP+FP))
  recall    <- ifelse((TP+FN)==0, 0, TP/(TP+FN))
  
  f1 <- ifelse((precision+recall)==0, 0,
               2*precision*recall/(precision+recall))
  
  data.frame(
    cutoff = cut,
    precision = precision,
    recall = recall,
    F1 = f1
  )
})

tabla_f1 <- bind_rows(resultados)

best_cutoff <- tabla_f1 %>%
  arrange(desc(F1)) %>%
  slice(1)


# ============================================================
# 14. PREDICCIÓN FINAL + EXPORT
# ============================================================
pred_class <- ifelse(pred_prob$Yes >= 0.66, 1, 0)

predictSample2 <- data.frame(
  id = test$id,
  Pobre = pred_class
)

write.csv(
  predictSample2,
  "GBM_down_cv5_cut066_F1_0630.csv",
  row.names = FALSE
)

write.csv(preds, "predsgbmfit_2.csv")


# ============================================================
# 15. MÉTRICA PERSONALIZADA
# ============================================================
fiveStats <- function(...) {
  c(
    caret::twoClassSummary(...),   # AUC-ROC, Sensitivity, Specificity
    caret::defaultSummary(...)     # Accuracy, Kappa
  )
}


# ============================================================
# 16. IMPUTACIÓN DE NA
# ============================================================
train <- train %>%
  mutate(
    occupiedHead = ifelse(is.na(occupiedHead), 0, occupiedHead),
    formalHead   = ifelse(is.na(formalHead), 0, formalHead),
    olderHead    = ifelse(is.na(olderHead), 0, olderHead)
  )

test <- test %>%
  mutate(
    occupiedHead = ifelse(is.na(occupiedHead), 0, occupiedHead),
    formalHead   = ifelse(is.na(formalHead), 0, formalHead),
    olderHead    = ifelse(is.na(olderHead), 0, olderHead)
  )


# ============================================================
# 17. GRID GBM GENERAL
# ============================================================
grid_gbm <- expand.grid(
  interaction.depth = c(1, 3, 5),
  n.trees = c(100, 300, 500),
  shrinkage = c(0.01, 0.05, 0.1),
  n.minobsinnode = c(10)
)


# ============================================================
# 18. CONTROL CV
# ============================================================
ctrl <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = fiveStats,
  classProbs = TRUE,
  verboseIter = FALSE,
  savePredictions = TRUE
)

set.seed(91519)


# ============================================================
# MODELO 0 - SENS (VARIABLES SELECCIONADAS)
# ============================================================
gbm_tree <- train(
  Pobre ~ rent +
    num_minors +
    dependency_ratio +
    prop_formal +
    n_personas +
    headWoman,
  data = train,
  method = "gbm",
  trControl = ctrl,
  tuneGrid = grid_gbm,
  metric = "Sens"
)

pred_class <- ifelse(pred_prob$Yes >= 0.28, 1, 0)

predictSample2 <- data.frame(
  id = test$id,
  Pobre = pred_class
)

write.csv(
  predictSample2,
  "GBM_F1_depth_5_trees_500_shrink_0_1_cutoff_0_28.csv",
  row.names = FALSE
)


# ============================================================
# MODELO 1 - FULL (ROC)
# ============================================================
grid_gbm <- expand.grid(
  interaction.depth = c(1, 3, 5),
  n.trees = c(100, 300, 500),
  shrinkage = c(0.01, 0.05, 0.1),
  n.minobsinnode = c(10)
)

ctrl <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = fiveStats,
  classProbs = TRUE,
  verboseIter = FALSE,
  savePredictions = TRUE
)

set.seed(91519)

gbm_tree <- train(
  Pobre ~ .,
  data = train,
  method = "gbm",
  trControl = ctrl,
  tuneGrid = grid_gbm,
  metric = "ROC"
)

pred_class <- ifelse(pred_prob$Yes >= 0.27, 1, 0)

predictSample2 <- data.frame(
  id = test$id,
  Pobre = pred_class
)

write.csv(
  predictSample2,
  "GBM_depth5_trees500_shrink010_cut018_ROC893.csv",
  row.names = FALSE
)


# ============================================================
# MODELO 2 - SENS (MENOS HIPERPARÁMETROS)
# ============================================================
grid_gbm <- expand.grid(
  interaction.depth = c(1, 3, 5),
  n.trees = c(100, 300),
  shrinkage = c(0.05, 0.1),
  n.minobsinnode = 10
)

ctrl <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = fiveStats,
  classProbs = TRUE,
  verboseIter = FALSE,
  savePredictions = TRUE
)

set.seed(91519)

gbm_tree <- train(
  Pobre ~ rent +
    num_minors +
    dependency_ratio +
    prop_formal +
    n_personas +
    headWoman,
  data = train,
  method = "gbm",
  trControl = ctrl,
  tuneGrid = grid_gbm,
  metric = "ROC"
)

gbm_tree3 <- gbm_tree


# ============================================================
# EXPORT MODELO 2
# ============================================================
pred_class <- ifelse(pred_prob$Yes >= 0.5, 1, 0)

predictSample2 <- data.frame(
  id = test$id,
  Pobre = pred_prob$Yes
)

write.csv(
  predictSample2,
  "GBM_ROC_087.csv",
  row.names = FALSE
)