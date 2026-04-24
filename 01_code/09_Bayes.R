# ============================================================
# NAIVE BAYES: múltiples variantes para predicción de pobreza
# ============================================================
#
# Depende de:
#   - train, test  (bases finales de 02_feature_engineering.R)
#   - caret (con klaR instalado para method = "nb")
#   - dplyr, pROC
#
# Modelos:
#   A) Base estructural (educación + laboral + composición)
#   B) Educación del jefe + tamaño
#   C) Educación + empleo formal
#   D) Desempleo + dependencia
#   E) Solo jefe
#   Full) Todas las anteriores
#   Weights, Down, Up : Full con tres estrategias de desbalance
#
# Estructura:
#   1. Copias locales de train/test (no mutar globales).
#   2. Grid NB (fL, usekernel) con CV 5-fold.
#   3. Comparación de resamples.
#   4. Umbral óptimo F1 sobre OOF del mejor modelo.
#   5. Predicción en test y exportación CSV.
# ============================================================


# ============================================================
# 1. Preparación — copias locales
# ============================================================
# Nivel positivo = "Yes" como segundo factor (convención del pipeline).
# Usamos copias para no mutar train/test globales (stacking y demás
# dependen del estado original).
train_nb <- train
test_nb  <- test
train_nb$Pobre <- factor(train_nb$Pobre, levels = c("No", "Yes"))


# ============================================================
# 2. Control de entrenamiento
# ============================================================
ctrl <- trainControl(
  method          = "cv",
  number          = 5,
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

grid_nb <- expand.grid(
  fL        = c(0, 1, 2),
  usekernel = c(TRUE, FALSE),
  adjust    = 1
)

set.seed(2025)

# Conjuntos de predictores usados en los distintos modelos
vars_base <- c("num_minors", "prop_dependiente", "prop_informal", "n_personas",
               "headWoman", "cat_educHead", "cat_maxEduc", "formalHead")
vars_m1   <- c("num_minors", "prop_dependiente", "prop_informal", "n_personas")
vars_m2   <- c("cat_educHead", "formalHead", "prop_informal", "persons_per_worker")
vars_m3   <- c("n_personas", "prop_dependiente", "prop_informal", "sin_ocupados")
vars_m4   <- c("headWoman", "cat_educHead", "formalHead")
vars_full <- c(vars_base, "persons_per_worker", "sin_ocupados")


train_nb_formula <- function(vars) {
  as.formula(paste("Pobre ~", paste(vars, collapse = " + ")))
}


# ============================================================
# 3. Modelos
# ============================================================
fit_nb <- function(formula, trControl = ctrl, weights = NULL) {
  train(
    formula,
    data       = train_nb,
    method     = "nb",
    metric     = "Sens",
    trControl  = trControl,
    tuneGrid   = grid_nb,
    weights    = weights,
    preProcess = c("center", "scale")
  )
}

model_base_nb <- fit_nb(train_nb_formula(vars_base))
model1_nb     <- fit_nb(train_nb_formula(vars_m1))
model2_nb     <- fit_nb(train_nb_formula(vars_m2))
model3_nb     <- fit_nb(train_nb_formula(vars_m3))
model4_nb     <- fit_nb(train_nb_formula(vars_m4))
model5_nb     <- fit_nb(train_nb_formula(vars_full))

# Pesos — vector local (NO persiste en train)
pos_weight <- sum(train_nb$Pobre == "No") / sum(train_nb$Pobre == "Yes")
w_vec      <- ifelse(train_nb$Pobre == "Yes", pos_weight, 1)

model6_nb <- fit_nb(
  train_nb_formula(c(vars_m1, "cat_educHead", "formalHead")),
  weights = w_vec
)

ctrl_down <- ctrl; ctrl_down$sampling <- "down"
ctrl_up   <- ctrl; ctrl_up$sampling   <- "up"

model7_nb <- fit_nb(
  train_nb_formula(c(vars_m1, "cat_educHead", "formalHead")),
  trControl = ctrl_down
)
model8_nb <- fit_nb(
  train_nb_formula(c(vars_m1, "cat_educHead", "formalHead")),
  trControl = ctrl_up
)


# ============================================================
# 4. Comparación
# ============================================================
results_nb <- resamples(list(
  Base = model_base_nb,
  M1 = model1_nb, M2 = model2_nb, M3 = model3_nb, M4 = model4_nb,
  Full = model5_nb, Weights = model6_nb, Down = model7_nb, Up = model8_nb
))

cat("\n-- Resamples Naive Bayes --\n")
print(summary(results_nb))


# ============================================================
# 5. Umbral óptimo F1 sobre OOF del mejor modelo por ROC
# ============================================================
# Escogemos el modelo con mejor ROC promedio en CV (criterio data-driven).
roc_means  <- sapply(results_nb$values[, grep("~ROC", names(results_nb$values))],
                     mean, na.rm = TRUE)
names(roc_means) <- sub("~ROC", "", names(roc_means))
best_name <- names(which.max(roc_means))
cat("\nMejor modelo NB según ROC medio:", best_name,
    "(", round(max(roc_means), 4), ")\n")

modelos_nb <- list(
  Base = model_base_nb, M1 = model1_nb, M2 = model2_nb, M3 = model3_nb,
  M4 = model4_nb, Full = model5_nb, Weights = model6_nb,
  Down = model7_nb, Up = model8_nb
)
best_nb_model <- modelos_nb[[best_name]]

oof_best <- best_nb_model$pred
y_oof    <- ifelse(oof_best$obs == "Yes", 1L, 0L)
p_oof    <- oof_best$Yes

best_f1_over_thresholds <- function(probs, y, grid = seq(0.05, 0.95, by = 0.01)) {
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

best_cut_nb <- best_f1_over_thresholds(p_oof, y_oof)

cat("Umbral óptimo NB (OOF):\n")
cat("  threshold :", round(best_cut_nb$threshold, 3), "\n")
cat("  precision :", round(best_cut_nb$precision, 4), "\n")
cat("  recall    :", round(best_cut_nb$recall,    4), "\n")
cat("  F1        :", round(best_cut_nb$f1,        4), "\n\n")


# ============================================================
# 6. Predicción en test + submission
# ============================================================
probs_test_nb <- predict(best_nb_model, newdata = test_nb, type = "prob")[, "Yes"]

predictSample_nb <- test_nb %>%
  transmute(
    id    = id,
    pobre = as.integer(probs_test_nb >= best_cut_nb$threshold)
  )

cat("Distribución predicciones Naive Bayes:\n")
print(table(predictSample_nb$pobre))

fmt_p <- function(x, d = 3) gsub("\\.", "_", as.character(round(x, d)))
name_nb <- paste0(
  "NaiveBayes_", best_name,
  "_threshold_", fmt_p(best_cut_nb$threshold, 3),
  ".csv"
)
path_nb <- file.path("02_outputs", "predictions", name_nb)
write.csv(predictSample_nb, path_nb, row.names = FALSE)
cat("\nNaive Bayes guardado en:", path_nb, "\n")
