# ============================================================
# Gradient Boosting Machine (GBM) vía caret — pobreza hogar
# ============================================================
#
# Depende de:
#   - train, test  (bases finales de 02_feature_engineering.R)
#   - caret, gbm, MLmetrics, pROC, dplyr
#
# Estrategia:
#   1. Grid search con CV 5-fold (métrica ROC) + downsampling para
#      manejar el desbalance de clases.
#   2. Extracción de predicciones OOF del mejor tune.
#   3. Búsqueda del umbral que maximiza F1 sobre OOF.
#   4. Reentrenamiento final sobre todo train y predicción en test.
#   5. Exportación de CSV con convención del pipeline: columnas
#      `id` y `pobre` (0/1) en 02_outputs/predictions/.
# ============================================================


# ============================================================
# 1. Preparación
# ============================================================
# Copias locales para no mutar el global (otros scripts dependen
# del estado original de train/test).
train_gbm <- train
test_gbm  <- test

# Aseguramos orden de niveles consistente con el resto del pipeline:
# "No" como primer nivel, "Yes" como positivo (segundo nivel).
train_gbm$Pobre <- factor(train_gbm$Pobre, levels = c("No", "Yes"))


# ============================================================
# 2. Control de entrenamiento
# ============================================================
# sampling = "down" equilibra el desbalance reduciendo la clase
# mayoritaria en cada fold (más rápido que pesos y similar en F1).
ctrl_gbm <- trainControl(
  method          = "cv",
  number          = 5,
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  sampling        = "down",
  savePredictions = "final",
  verboseIter     = FALSE
)


# ============================================================
# 3. Grid de hiperparámetros
# ============================================================
grid_gbm <- expand.grid(
  interaction.depth = c(3, 5),
  n.trees           = c(300, 600, 1000),
  shrinkage         = c(0.01, 0.05),
  n.minobsinnode    = c(10, 20)
)

cat("GBM — combinaciones en el grid:", nrow(grid_gbm), "\n\n")


# ============================================================
# 4. Entrenamiento
# ============================================================
set.seed(2025)

gbm_fit <- train(
  Pobre ~ .,
  data      = train_gbm,
  method    = "gbm",
  metric    = "ROC",
  trControl = ctrl_gbm,
  tuneGrid  = grid_gbm,
  verbose   = FALSE
)

cat("Mejor tune GBM:\n")
print(gbm_fit$bestTune)
cat("\n")


# ============================================================
# 5. Umbral óptimo F1 sobre OOF
# ============================================================
# `savePredictions = "final"` solo guarda las filas del mejor tune,
# así que podemos usarlas directamente.
oof_gbm <- gbm_fit$pred

oof_probs_gbm <- oof_gbm$Yes
y_oof         <- ifelse(oof_gbm$obs == "Yes", 1L, 0L)

best_f1_over_thresholds <- function(probs, y, grid = seq(0.1, 0.7, by = 0.01)) {
  best <- list(threshold = 0.5, f1 = 0, precision = 0, recall = 0)
  for (thr in grid) {
    pred <- as.integer(probs >= thr)
    tp <- sum(pred == 1 & y == 1)
    fp <- sum(pred == 1 & y == 0)
    fn <- sum(pred == 0 & y == 1)
    prec <- if (tp + fp > 0) tp / (tp + fp) else 0
    rec  <- if (tp + fn > 0) tp / (tp + fn) else 0
    f1   <- if (prec + rec > 0) 2 * prec * rec / (prec + rec) else 0
    if (f1 > best$f1) best <- list(threshold = thr, f1 = f1,
                                   precision = prec, recall = rec)
  }
  best
}

best_cut_gbm <- best_f1_over_thresholds(oof_probs_gbm, y_oof)

cat("Umbral óptimo GBM (OOF):\n")
cat("  threshold :", round(best_cut_gbm$threshold, 3), "\n")
cat("  precision :", round(best_cut_gbm$precision, 4), "\n")
cat("  recall    :", round(best_cut_gbm$recall,    4), "\n")
cat("  F1        :", round(best_cut_gbm$f1,        4), "\n\n")


# ============================================================
# 6. Predicción en test y submission
# ============================================================
probs_test_gbm <- predict(gbm_fit, newdata = test_gbm, type = "prob")[, "Yes"]

predictSample_gbm <- test_gbm %>%
  transmute(
    id    = id,
    pobre = as.integer(probs_test_gbm >= best_cut_gbm$threshold)
  )

cat("Distribución predicciones GBM:\n")
print(table(predictSample_gbm$pobre))


# ============================================================
# 7. Exportar CSV
# ============================================================
fmt_p <- function(x, d = 3) gsub("\\.", "_", as.character(round(x, d)))

name_gbm <- paste0(
  "GBM_depth_",  gbm_fit$bestTune$interaction.depth,
  "_trees_",     gbm_fit$bestTune$n.trees,
  "_shrink_",    fmt_p(gbm_fit$bestTune$shrinkage, 3),
  "_threshold_", fmt_p(best_cut_gbm$threshold,     3),
  ".csv"
)
path_gbm <- file.path("02_outputs", "predictions", name_gbm)
write.csv(predictSample_gbm, path_gbm, row.names = FALSE)
cat("\nGBM guardado en:", path_gbm, "\n")
