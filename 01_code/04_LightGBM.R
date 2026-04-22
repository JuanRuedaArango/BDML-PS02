# ============================================================
# LightGBM: múltiples variantes de predicción de pobreza
# ============================================================
#
# Depende de:
#   - train, test      (bases finales de 02_feature_engineering.R)
#   - multiStats       (definida en 00_funciones.R)
#   - caret, pROC, lightgbm, dplyr
#
# Variantes que entrena:
#   A) Baseline    : grid search sobre AUC, umbral 0.5
#   B) Weighted    : scale_pos_weight por desbalance, umbral 0.5
#   C) Weighted+PR : igual que B pero umbral óptimo sobre OOF
#   D) Downsample+PR: downsampling + umbral óptimo sobre OOF
#
# Estructura de secciones:
#   1. Preparación de matrices
#   2. Grid search de hiperparámetros (Modelo A)
#   3. Entrenamiento de los cuatro modelos
#   4. Predicciones sobre test
#   5. Guardar archivos de envío
# ============================================================


# ============================================================
# 1. Preparación de matrices para LightGBM
# ============================================================
# LightGBM requiere matrices numéricas. Usamos model.matrix()
# para convertir los factores en dummies (expansión completa,
# sin intercepto) de forma consistente entre train y test.

# Predictores de train (excluimos la variable respuesta)
pred_vars <- setdiff(names(train), "Pobre")

X_train <- model.matrix(~ . - 1, data = train[, pred_vars])
y_train  <- as.integer(train$Pobre == "Yes")   # 1 = pobre, 0 = no pobre

# Predictores de test (excluimos id, que no es predictor)
X_test_raw <- model.matrix(~ . - 1, data = test %>% select(-id))

# Alineación defensiva de columnas: si alguna dummy aparece en
# train pero no en test (nivel sin representación), la añadimos
# con cero para evitar errores al predecir.
cols_faltantes <- setdiff(colnames(X_train), colnames(X_test_raw))
if (length(cols_faltantes) > 0) {
  mat_extra <- matrix(0,
                      nrow = nrow(X_test_raw),
                      ncol = length(cols_faltantes),
                      dimnames = list(NULL, cols_faltantes))
  X_test_raw <- cbind(X_test_raw, mat_extra)
}
X_test <- X_test_raw[, colnames(X_train), drop = FALSE]

# ---------------------Verificación---------------------------

cat("Columnas X_train :", ncol(X_train), "\n")
cat("Columnas X_test  :", ncol(X_test),  "\n")
cat("Columnas alineadas:", identical(colnames(X_train), colnames(X_test)), "\n\n")

cat("Filas X_train:", nrow(X_train), "\n")
cat("Filas X_test :", nrow(X_test),  "\n\n")

cat("Distribución de y_train:\n")
print(table(y_train))
cat("Proporción de pobres:", round(mean(y_train), 4), "\n\n")

# Dataset LightGBM completo (se reutiliza en todos los modelos)
dtrain_full <- lgb.Dataset(
  data  = X_train,
  label = y_train
)


# ============================================================
# 2. Grid search de hiperparámetros (base para Modelo A)
# ============================================================
# Exploramos combinaciones de:
#   num_leaves      : complejidad del árbol (profundidad implícita)
#   learning_rate   : tamaño del paso de gradient boosting
#   feature_fraction: fracción de columnas usadas por árbol
#   min_data_in_leaf: mínimo de observaciones por hoja
#
# La métrica de selección es AUC porque es robusta ante el
# desbalance de clases y permite ajustar el threshold después.
# Usamos early stopping (30 rondas sin mejora) para determinar
# el número óptimo de árboles en cada combinación.

lgbm_grid <- expand.grid(
  num_leaves         = c(31, 63, 127),
  learning_rate      = c(0.05, 0.1),
  feature_fraction   = c(0.7, 0.9),
  min_data_in_leaf   = c(20, 50),
  stringsAsFactors   = FALSE
)

cat("Combinaciones en la grilla:", nrow(lgbm_grid), "\n\n")

set.seed(2025)

resultados_cv <- vector("list", nrow(lgbm_grid))

for (i in seq_len(nrow(lgbm_grid))) {

  params_i <- list(
    objective        = "binary",
    metric           = "auc",
    num_leaves       = lgbm_grid$num_leaves[i],
    learning_rate    = lgbm_grid$learning_rate[i],
    feature_fraction = lgbm_grid$feature_fraction[i],
    min_data_in_leaf = lgbm_grid$min_data_in_leaf[i],
    bagging_fraction = 0.8,      # fracción de filas por árbol
    bagging_freq     = 5,        # bagging cada 5 árboles
    verbose          = -1        # silenciar output de LightGBM
  )

  cv_i <- lgb.cv(
    params                = params_i,
    data                  = dtrain_full,
    nrounds               = 500,
    nfold                 = 5,
    stratified            = TRUE,          # estratificado por clase
    early_stopping_rounds = 30,
    verbose               = -1
  )

  resultados_cv[[i]] <- data.frame(
    num_leaves       = lgbm_grid$num_leaves[i],
    learning_rate    = lgbm_grid$learning_rate[i],
    feature_fraction = lgbm_grid$feature_fraction[i],
    min_data_in_leaf = lgbm_grid$min_data_in_leaf[i],
    best_iter        = cv_i$best_iter,
    best_auc         = cv_i$best_score
  )

  cat(sprintf(
    "Combo %2d/%d | leaves=%3d lr=%.2f ff=%.1f min_leaf=%2d | iter=%3d AUC=%.4f\n",
    i, nrow(lgbm_grid),
    lgbm_grid$num_leaves[i], lgbm_grid$learning_rate[i],
    lgbm_grid$feature_fraction[i], lgbm_grid$min_data_in_leaf[i],
    cv_i$best_iter, cv_i$best_score
  ))
}

cv_summary <- bind_rows(resultados_cv)

# Mejor combinación según AUC promedio en CV
best_row <- cv_summary %>% arrange(desc(best_auc)) %>% slice(1)

cat("\n--- Mejor combinación (Modelo A) ---\n")
print(best_row)
cat("\n")

# Parámetros base compartidos por todos los modelos
params_base <- list(
  objective        = "binary",
  metric           = "auc",
  num_leaves       = best_row$num_leaves,
  learning_rate    = best_row$learning_rate,
  feature_fraction = best_row$feature_fraction,
  min_data_in_leaf = best_row$min_data_in_leaf,
  bagging_fraction = 0.8,
  bagging_freq     = 5,
  verbose          = -1
)


# ============================================================
# 3A. Modelo A — Baseline, umbral 0.5
# ============================================================
# Entrenamos con los mejores hiperparámetros del grid search.
# El número de árboles viene del early stopping en CV.
# Umbral de clasificación: 0.5 (decisión estándar).

set.seed(2025)

model_A <- lgb.train(
  params  = params_base,
  data    = dtrain_full,
  nrounds = best_row$best_iter
)

cat("Modelo A entrenado. Iteraciones:", best_row$best_iter, "\n\n")


# ============================================================
# 3B. Modelo B — scale_pos_weight, umbral 0.5
# ============================================================
# scale_pos_weight pondera la clase positiva (pobres) con el
# cociente N_negativos / N_positivos. Esto hace que cada error
# en la clase minoritaria pese más en el gradiente, corrigiendo
# el sesgo del modelo hacia la clase mayoritaria.
# Umbral de clasificación: 0.5.

pos_weight_lgbm <- sum(y_train == 0) / sum(y_train == 1)
cat("scale_pos_weight:", round(pos_weight_lgbm, 3), "\n")

params_B <- modifyList(params_base, list(scale_pos_weight = pos_weight_lgbm))

# Re-tuneamos el número de iteraciones con el nuevo peso
set.seed(2025)

cv_B <- lgb.cv(
  params                = params_B,
  data                  = dtrain_full,
  nrounds               = 500,
  nfold                 = 5,
  stratified            = TRUE,
  early_stopping_rounds = 30,
  verbose               = -1
)

best_iter_B <- cv_B$best_iter
cat("Mejor iteración Modelo B:", best_iter_B, "\n\n")

set.seed(2025)

model_B <- lgb.train(
  params  = params_B,
  data    = dtrain_full,
  nrounds = best_iter_B
)

cat("Modelo B entrenado.\n\n")


# ============================================================
# 3C. Modelo C — scale_pos_weight + umbral óptimo PR
# ============================================================
# Igual que B en parámetros, pero en lugar de usar 0.5 como
# umbral de clasificación, buscamos el umbral que maximiza F1
# sobre predicciones out-of-fold (OOF).
#
# ¿Por qué OOF?
#   Porque si buscáramos el umbral sobre los mismos datos de
#   entrenamiento cometemos data leakage. Las predicciones OOF
#   son independientes del modelo que las generó fold a fold,
#   lo que da una estimación honesta del F1 en datos nuevos.

set.seed(2025)
folds_C <- createFolds(y_train, k = 5, returnTrain = FALSE)

oof_probs_C <- numeric(length(y_train))

for (k in seq_along(folds_C)) {

  val_idx   <- folds_C[[k]]
  train_idx <- setdiff(seq_len(length(y_train)), val_idx)

  dtrain_k <- lgb.Dataset(
    data  = X_train[train_idx, , drop = FALSE],
    label = y_train[train_idx]
  )

  model_k <- lgb.train(
    params  = params_B,
    data    = dtrain_k,
    nrounds = best_iter_B,
    verbose = -1
  )

  oof_probs_C[val_idx] <- predict(model_k,
                                   X_train[val_idx, , drop = FALSE])

  cat(sprintf("  Modelo C — fold %d/%d completado\n", k, length(folds_C)))
}

# Curva PR y búsqueda del umbral que maximiza F1
roc_C <- pROC::roc(
  response  = factor(y_train, levels = c(0, 1), labels = c("No", "Yes")),
  predictor = oof_probs_C,
  levels    = c("No", "Yes"),
  direction = "<"
)

pr_cutoffs_C <- data.frame(
  pROC::coords(
    roc_C,
    x     = seq(0, 1, length.out = 200),
    input = "threshold",
    ret   = c("threshold", "precision", "recall")
  )
) %>%
  mutate(
    F1 = ifelse(
      precision + recall == 0, 0,
      (2 * precision * recall) / (precision + recall)
    )
  )

best_cutoff_C <- pr_cutoffs_C %>% arrange(desc(F1)) %>% slice(1)

cat("\nUmbral óptimo Modelo C:\n")
print(best_cutoff_C)
cat("\n")

# El modelo final para C es el mismo que B (mismos parámetros).
# Solo cambia el umbral de clasificación al predecir sobre test.
model_C <- model_B


# ============================================================
# 3D. Modelo D — Downsampling + umbral óptimo PR
# ============================================================
# Estrategia alternativa al ponderado: reducimos la clase
# mayoritaria (No pobres) para que train quede balanceado.
# El umbral lo buscamos sobre predicciones OOF donde el modelo
# fue entrenado con downsampling pero evaluado sobre los datos
# originales (honesto para el Kaggle real).

idx_pos    <- which(y_train == 1)
idx_neg    <- which(y_train == 0)

set.seed(2025)
idx_neg_ds <- sample(idx_neg, size = length(idx_pos), replace = FALSE)
idx_ds     <- sort(c(idx_pos, idx_neg_ds))

X_down <- X_train[idx_ds, , drop = FALSE]
y_down <- y_train[idx_ds]

cat("Dataset downsampled:", length(y_down),
    "obs | Pos:", sum(y_down), "| Neg:", sum(1 - y_down), "\n")

dtrain_down <- lgb.Dataset(data = X_down, label = y_down)

# Parámetros sin scale_pos_weight (los datos ya están balanceados)
params_D <- modifyList(params_base, list(scale_pos_weight = 1))

# Número óptimo de iteraciones con los datos balanceados
set.seed(2025)
cv_D <- lgb.cv(
  params                = params_D,
  data                  = dtrain_down,
  nrounds               = 500,
  nfold                 = 5,
  stratified            = TRUE,
  early_stopping_rounds = 30,
  verbose               = -1
)

best_iter_D <- cv_D$best_iter
cat("Mejor iteración Modelo D:", best_iter_D, "\n\n")

# OOF sobre datos originales (no balanceados): cada fold entrena
# con downsampling aplicado solo al subconjunto de train del fold.
set.seed(2025)
folds_D <- createFolds(y_train, k = 5, returnTrain = FALSE)

oof_probs_D <- numeric(length(y_train))

for (k in seq_along(folds_D)) {

  val_idx   <- folds_D[[k]]
  train_idx <- setdiff(seq_len(length(y_train)), val_idx)

  # Downsampling dentro de train_idx (no toca val_idx)
  pos_k     <- train_idx[y_train[train_idx] == 1]
  neg_k     <- train_idx[y_train[train_idx] == 0]
  neg_k_ds  <- sample(neg_k, size = length(pos_k), replace = FALSE)
  idx_k     <- sort(c(pos_k, neg_k_ds))

  dtrain_k <- lgb.Dataset(
    data  = X_train[idx_k, , drop = FALSE],
    label = y_train[idx_k]
  )

  model_k <- lgb.train(
    params  = params_D,
    data    = dtrain_k,
    nrounds = best_iter_D,
    verbose = -1
  )

  oof_probs_D[val_idx] <- predict(model_k,
                                   X_train[val_idx, , drop = FALSE])

  cat(sprintf("  Modelo D — fold %d/%d completado\n", k, length(folds_D)))
}

# Umbral óptimo
roc_D <- pROC::roc(
  response  = factor(y_train, levels = c(0, 1), labels = c("No", "Yes")),
  predictor = oof_probs_D,
  levels    = c("No", "Yes"),
  direction = "<"
)

pr_cutoffs_D <- data.frame(
  pROC::coords(
    roc_D,
    x     = seq(0, 1, length.out = 200),
    input = "threshold",
    ret   = c("threshold", "precision", "recall")
  )
) %>%
  mutate(
    F1 = ifelse(
      precision + recall == 0, 0,
      (2 * precision * recall) / (precision + recall)
    )
  )

best_cutoff_D <- pr_cutoffs_D %>% arrange(desc(F1)) %>% slice(1)

cat("\nUmbral óptimo Modelo D:\n")
print(best_cutoff_D)
cat("\n")

# Modelo final entrenado sobre todos los datos downsampled
set.seed(2025)

model_D <- lgb.train(
  params  = params_D,
  data    = dtrain_down,
  nrounds = best_iter_D
)

cat("Modelo D entrenado.\n\n")


# ============================================================
# 4. Predicciones sobre test
# ============================================================

# --- Modelo A: baseline, umbral 0.5 ---
preds_A <- predict(model_A, X_test)

predictSample_A <- test %>%
  transmute(
    id    = id,
    pobre = ifelse(preds_A >= 0.5, 1, 0)
  )

cat("Distribución predicciones Modelo A (baseline):\n")
print(table(predictSample_A$pobre))

# --- Modelo B: scale_pos_weight, umbral 0.5 ---
preds_B <- predict(model_B, X_test)

predictSample_B <- test %>%
  transmute(
    id    = id,
    pobre = ifelse(preds_B >= 0.5, 1, 0)
  )

cat("\nDistribución predicciones Modelo B (weighted):\n")
print(table(predictSample_B$pobre))

# --- Modelo C: scale_pos_weight + umbral óptimo ---
# Mismo modelo que B, umbral ajustado con OOF
preds_C <- predict(model_C, X_test)

predictSample_C <- test %>%
  transmute(
    id    = id,
    pobre = ifelse(preds_C >= best_cutoff_C$threshold, 1, 0)
  )

cat("\nDistribución predicciones Modelo C (weighted + PR threshold):\n")
print(table(predictSample_C$pobre))

# --- Modelo D: downsampling + umbral óptimo ---
preds_D <- predict(model_D, X_test)

predictSample_D <- test %>%
  transmute(
    id    = id,
    pobre = ifelse(preds_D >= best_cutoff_D$threshold, 1, 0)
  )

cat("\nDistribución predicciones Modelo D (downsample + PR threshold):\n")
print(table(predictSample_D$pobre))


# ============================================================
# 5. Guardar archivos de envío
# ============================================================
# Función local para formatear parámetros en nombres de archivo
fmt_p <- function(x, d = 4) gsub("\\.", "_", as.character(round(x, d)))

# Tag con los hiperparámetros del mejor modelo del grid search
base_tag <- paste0(
  "leaves_", best_row$num_leaves,
  "_lr_",   fmt_p(best_row$learning_rate, 2),
  "_iter_", best_row$best_iter
)

# --- A ---
name_A <- paste0("LGBM_base_", base_tag, ".csv")
path_A <- file.path("02_outputs", "predictions", name_A)
write.csv(predictSample_A, path_A, row.names = FALSE)
cat("\nModelo A guardado en:", path_A, "\n")

# --- B ---
name_B <- paste0("LGBM_weighted_", base_tag, ".csv")
path_B <- file.path("02_outputs", "predictions", name_B)
write.csv(predictSample_B, path_B, row.names = FALSE)
cat("Modelo B guardado en:", path_B, "\n")

# --- C ---
name_C <- paste0(
  "LGBM_weighted_PR_", base_tag,
  "_threshold_", fmt_p(best_cutoff_C$threshold, 3),
  ".csv"
)
path_C <- file.path("02_outputs", "predictions", name_C)
write.csv(predictSample_C, path_C, row.names = FALSE)
cat("Modelo C guardado en:", path_C, "\n")

# --- D ---
name_D <- paste0(
  "LGBM_down_PR_", base_tag,
  "_threshold_", fmt_p(best_cutoff_D$threshold, 3),
  ".csv"
)
path_D <- file.path("02_outputs", "predictions", name_D)
write.csv(predictSample_D, path_D, row.names = FALSE)
cat("Modelo D guardado en:", path_D, "\n")
