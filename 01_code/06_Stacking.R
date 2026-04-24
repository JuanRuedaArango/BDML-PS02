# ============================================================
# Stacking: meta-learner sobre OOF probs de RF + LightGBM
# ============================================================
#
# Depende de:
#   - 02_outputs/lgbm_probs_ensemble.rds  (04_LightGBM.R guarda
#                                          oof_probs_E, test_probs_E)
#   - 02_outputs/rf_probs_ensemble.rds    (05_RandomForest.R guarda
#                                          oob_probs, test_probs)
#   - pROC, glmnet, dplyr
#
# ¿Por qué stacking?
#   El promedio simple (0.5·RF + 0.5·LGBM) ignora que un modelo
#   puede ser más confiable que otro. Un meta-learner (regresión
#   logística regularizada) APRENDE los pesos óptimos desde los
#   OOF probs → suele ganar 1-2 pp sobre el promedio simple.
#
# Flujo:
#   1. Cargar OOF probs de ambos modelos + y_train
#   2. Entrenar glm logístico: Pobre ~ oof_RF + oof_LGBM
#   3. Predecir OOF del meta-learner (vía refit KFold) → umbral F1
#   4. Predecir sobre test y guardar
# ============================================================


# ============================================================
# 1. Cargar probabilidades OOF/OOB
# ============================================================

lgbm_path <- file.path("02_outputs", "lgbm_probs_ensemble.rds")
rf_path   <- file.path("02_outputs", "rf_probs_ensemble.rds")

if (!file.exists(lgbm_path) || !file.exists(rf_path)) {
  stop("Faltan probs guardadas. Corre primero 04_LightGBM.R y 05_RandomForest.R.")
}

lgbm <- readRDS(lgbm_path)
rf   <- readRDS(rf_path)

# Usar Modelo E de LGBM si está disponible (optimizado para F1),
# si no caer a Modelo C.
if (!is.null(lgbm$oof_probs_E)) {
  lgbm_oof  <- lgbm$oof_probs_E
  lgbm_test <- lgbm$test_probs_E
  cat("Usando LightGBM Modelo E (random search + F1 OOF).\n")
} else {
  lgbm_oof  <- lgbm$oof_probs
  lgbm_test <- lgbm$test_probs
  cat("Usando LightGBM Modelo C (fallback).\n")
}

rf_oof  <- rf$oob_probs
rf_test <- rf$test_probs
y_train <- lgbm$y_train

stopifnot(length(rf_oof) == length(lgbm_oof))
stopifnot(length(y_train) == length(rf_oof))

cat("Observaciones train:", length(y_train), "\n")
cat("Pobres en train    :", sum(y_train), "\n")
cat("Tasa de pobreza    :", round(mean(y_train), 4), "\n\n")


# ============================================================
# 2. Matrices de OOF para meta-learner
# ============================================================
# Features del meta-learner: las dos columnas de probabilidades.
# Agregamos transformaciones que la regresión logística no
# aprende por sí sola:
#   - logit de cada prob (lineariza la relación)
#   - producto cruzado (interacción entre modelos)

clip <- function(x, eps = 1e-6) pmin(pmax(x, eps), 1 - eps)

oof_mat <- cbind(
  rf_oof       = rf_oof,
  lgbm_oof     = lgbm_oof,
  rf_logit     = log(clip(rf_oof)   / (1 - clip(rf_oof))),
  lgbm_logit   = log(clip(lgbm_oof) / (1 - clip(lgbm_oof))),
  prod_probs   = rf_oof * lgbm_oof,
  diff_probs   = lgbm_oof - rf_oof
)

test_mat <- cbind(
  rf_oof       = rf_test,
  lgbm_oof     = lgbm_test,
  rf_logit     = log(clip(rf_test)   / (1 - clip(rf_test))),
  lgbm_logit   = log(clip(lgbm_test) / (1 - clip(lgbm_test))),
  prod_probs   = rf_test * lgbm_test,
  diff_probs   = lgbm_test - rf_test
)


# ============================================================
# 3. Meta-learner: regresión logística regularizada (glmnet)
# ============================================================
# Usamos cv.glmnet con lasso leve (alpha=0.5) para seleccionar
# automáticamente qué features del meta-learner sirven.

set.seed(2025)
cv_meta <- cv.glmnet(
  x      = oof_mat,
  y      = y_train,
  family = "binomial",
  alpha  = 0.5,       # Elastic Net 50/50
  nfolds = 5
)

cat("Mejor lambda:", cv_meta$lambda.min, "\n")
cat("\nCoeficientes del meta-learner:\n")
print(coef(cv_meta, s = "lambda.min"))


# ============================================================
# 4. OOF predictions del meta-learner para escoger umbral
# ============================================================
# Reentrenamos el glmnet en K folds para obtener probs OOF
# honestas del meta-learner (sin leakage del refit total).

set.seed(2025)
K_stack <- 5
folds_stack <- sample(rep(seq_len(K_stack), length.out = length(y_train)))
meta_oof <- numeric(length(y_train))

for (k in seq_len(K_stack)) {
  idx_val <- which(folds_stack == k)
  idx_tr  <- which(folds_stack != k)

  cv_k <- cv.glmnet(
    x      = oof_mat[idx_tr, , drop = FALSE],
    y      = y_train[idx_tr],
    family = "binomial",
    alpha  = 0.5,
    nfolds = 5
  )

  meta_oof[idx_val] <- as.numeric(predict(
    cv_k,
    newx = oof_mat[idx_val, , drop = FALSE],
    s    = "lambda.min",
    type = "response"
  ))

  cat(sprintf("  Fold %d/%d del stacking completado\n", k, K_stack))
}


# ============================================================
# 5. Umbral óptimo sobre OOF del meta-learner
# ============================================================

roc_meta <- pROC::roc(
  response  = factor(y_train, levels = c(0, 1), labels = c("No", "Yes")),
  predictor = meta_oof,
  levels    = c("No", "Yes"),
  direction = "<"
)

pr_cutoffs_meta <- data.frame(
  pROC::coords(
    roc_meta,
    x     = seq(0, 1, length.out = 300),
    input = "threshold",
    ret   = c("threshold", "precision", "recall")
  )
) %>%
  mutate(
    F1 = ifelse(precision + recall == 0, 0,
                2 * precision * recall / (precision + recall))
  )

best_cutoff_meta <- pr_cutoffs_meta %>% arrange(desc(F1)) %>% slice(1)

cat("\nUmbral óptimo stacking:\n")
print(best_cutoff_meta)
cat("\n")


# ============================================================
# 6. Predicción sobre test
# ============================================================

preds_stack_test <- as.numeric(predict(
  cv_meta,
  newx = test_mat,
  s    = "lambda.min",
  type = "response"
))

predictSample_stack <- test %>%
  transmute(
    id    = id,
    pobre = ifelse(preds_stack_test >= best_cutoff_meta$threshold, 1, 0)
  )

cat("Distribución predicciones Stacking:\n")
print(table(predictSample_stack$pobre))


# ============================================================
# 7. Guardar archivo de envío
# ============================================================

fmt_p <- function(x, d = 4) gsub("\\.", "_", as.character(round(x, d)))

name_stack <- paste0(
  "Stacking_RF_LGBM_threshold_",
  fmt_p(best_cutoff_meta$threshold, 3),
  ".csv"
)
path_stack <- file.path("02_outputs", "predictions", name_stack)
write.csv(predictSample_stack, path_stack, row.names = FALSE)
cat("\nStacking guardado en:", path_stack, "\n")
