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
# LightGBM soporta features categóricas NATIVAMENTE: internamente
# busca splits óptimos sobre subconjuntos de categorías usando
# el algoritmo de Fisher (1958), lo cual es más potente que
# expandir a dummies (que solo permite splits "una categoría vs
# el resto"). Además reduce drásticamente la dimensionalidad
# (Dominio pasa de ~25 columnas one-hot a 1 columna entera).
#
# Requisitos:
#   - Las factor-columns deben estar codificadas como enteros
#     no negativos (usamos 0-indexed).
#   - train y test deben compartir EL MISMO mapeo nivel → código
#     (de lo contrario, "Bogotá" podría ser 3 en train y 7 en test
#     y el modelo haría predicciones sin sentido).

pred_vars <- setdiff(names(train), "Pobre")

cols_factor  <- pred_vars[sapply(train[pred_vars], is.factor)]
cols_numeric <- setdiff(pred_vars, cols_factor)

cat("Predictores numéricos   :", length(cols_numeric), "\n")
cat("Predictores categóricos :", length(cols_factor),  "\n")
if (length(cols_factor) > 0) {
  cat("Categóricos:", paste(cols_factor, collapse = ", "), "\n")
}

# Alineación de niveles: unión de los niveles vistos en train y test,
# para que el código entero sea consistente entre ambas bases.
for (col in cols_factor) {
  niveles <- union(levels(train[[col]]), levels(test[[col]]))
  train[[col]] <- factor(train[[col]], levels = niveles)
  test[[col]]  <- factor(test[[col]],  levels = niveles)
}

# Matriz numérica: columnas numéricas tal cual, factores → entero 0-indexed.
to_lgbm_matrix <- function(df) {
  m <- matrix(NA_real_, nrow = nrow(df), ncol = length(pred_vars),
              dimnames = list(NULL, pred_vars))
  for (col in cols_numeric) m[, col] <- as.numeric(df[[col]])
  for (col in cols_factor)  m[, col] <- as.integer(df[[col]]) - 1L
  m
}

X_train <- to_lgbm_matrix(train)
X_test  <- to_lgbm_matrix(test)
y_train <- as.integer(train$Pobre == "Yes")   # 1 = pobre, 0 = no pobre

# ---------------------Verificación---------------------------

cat("Columnas X_train :", ncol(X_train), "\n")
cat("Columnas X_test  :", ncol(X_test),  "\n")
cat("Columnas alineadas:", identical(colnames(X_train), colnames(X_test)), "\n\n")

cat("Filas X_train:", nrow(X_train), "\n")
cat("Filas X_test :", nrow(X_test),  "\n\n")

cat("Distribución de y_train:\n")
print(table(y_train))
cat("Proporción de pobres:", round(mean(y_train), 4), "\n\n")

# Dataset LightGBM completo (se reutiliza en todos los modelos).
# feature_pre_filter = FALSE permite cambiar min_data_in_leaf entre
# modelos sobre el mismo Dataset sin recrearlo.
dtrain_full <- lgb.Dataset(
  data                = X_train,
  label               = y_train,
  categorical_feature = cols_factor,
  params              = list(feature_pre_filter = FALSE)
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
    data                = X_train[train_idx, , drop = FALSE],
    label               = y_train[train_idx],
    categorical_feature = cols_factor
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

dtrain_down <- lgb.Dataset(
  data                = X_down,
  label               = y_down,
  categorical_feature = cols_factor,
  params              = list(feature_pre_filter = FALSE)
)

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
    data                = X_train[idx_k, , drop = FALSE],
    label               = y_train[idx_k],
    categorical_feature = cols_factor
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


# ============================================================
# 3E. Modelo E — Random search ampliado + OOF F1 directo
# ============================================================
# A diferencia de A–D (que seleccionan por AUC y luego ajustan
# umbral), Modelo E optimiza F1 directamente vía OOF:
#   - Explora un espacio MÁS grande (6 hiperparámetros).
#   - En cada trial hace CV de 5 folds y evalúa F1 con el mejor
#     umbral de cada configuración (búsqueda conjunta).
#   - Random search con 40 trials es ~equivalente a Bayesian opt
#     para 6 parámetros (Bergstra & Bengio 2012).

N_TRIALS_E <- 20
set.seed(2025)

# Espacio de búsqueda
search_space_E <- data.frame(
  num_leaves        = sample(c(31, 63, 95, 127, 191), N_TRIALS_E, replace = TRUE),
  learning_rate     = runif(N_TRIALS_E, 0.01, 0.08),
  feature_fraction  = runif(N_TRIALS_E, 0.6, 0.95),
  bagging_fraction  = runif(N_TRIALS_E, 0.6, 0.95),
  min_data_in_leaf  = sample(c(10, 20, 50, 100), N_TRIALS_E, replace = TRUE),
  lambda_l2         = 10 ^ runif(N_TRIALS_E, -2, 1)     # 0.01 – 10
)

cat("\n============================================================\n")
cat(" Modelo E — Random search de", N_TRIALS_E, "trials\n")
cat("============================================================\n")

# Helper: dado un vector de probs y etiquetas, busca el mejor F1
best_f1_over_thresholds <- function(probs, y, grid = seq(0.1, 0.7, by = 0.01)) {
  best <- list(threshold = 0.5, f1 = 0)
  for (thr in grid) {
    pred <- as.integer(probs >= thr)
    tp <- sum(pred == 1 & y == 1)
    fp <- sum(pred == 1 & y == 0)
    fn <- sum(pred == 0 & y == 1)
    prec <- if (tp + fp > 0) tp / (tp + fp) else 0
    rec  <- if (tp + fn > 0) tp / (tp + fn) else 0
    f1   <- if (prec + rec > 0) 2 * prec * rec / (prec + rec) else 0
    if (f1 > best$f1) best <- list(threshold = thr, f1 = f1)
  }
  best
}

# Folds compartidos entre trials (comparación justa)
set.seed(2025)
folds_E <- createFolds(y_train, k = 5, returnTrain = FALSE)

resultados_E <- vector("list", N_TRIALS_E)

for (t in seq_len(N_TRIALS_E)) {

  params_t <- list(
    objective        = "binary",
    metric           = "binary_logloss",
    num_leaves       = search_space_E$num_leaves[t],
    learning_rate    = search_space_E$learning_rate[t],
    feature_fraction = search_space_E$feature_fraction[t],
    bagging_fraction = search_space_E$bagging_fraction[t],
    bagging_freq     = 5,
    min_data_in_leaf = search_space_E$min_data_in_leaf[t],
    lambda_l2        = search_space_E$lambda_l2[t],
    scale_pos_weight = pos_weight_lgbm,
    verbose          = -1
  )

  # lgb.cv pasándole nuestros folds: así podemos reutilizar los
  # boosters entrenados internamente para calcular OOF, en vez de
  # reentrenar otros 5 modelos a mano (ahorra ~40% del tiempo del loop).
  cv_t <- lgb.cv(
    params                = params_t,
    data                  = dtrain_full,
    nrounds               = 2000,
    folds                 = folds_E,
    early_stopping_rounds = 50,
    verbose               = -1
  )
  best_iter_t <- cv_t$best_iter

  # OOF probs reusando los boosters de cv_t (uno por fold).
  oof_t <- numeric(length(y_train))
  for (k in seq_along(folds_E)) {
    val_idx <- folds_E[[k]]
    bst_k   <- cv_t$boosters[[k]]
    if (!inherits(bst_k, "lgb.Booster") && is.list(bst_k) && "booster" %in% names(bst_k)) {
      bst_k <- bst_k$booster
    }
    oof_t[val_idx] <- predict(
      bst_k,
      X_train[val_idx, , drop = FALSE],
      num_iteration = best_iter_t
    )
  }

  # Mejor F1 sobre umbrales
  bt <- best_f1_over_thresholds(oof_t, y_train)

  resultados_E[[t]] <- list(
    trial     = t,
    params    = params_t,
    best_iter = best_iter_t,
    threshold = bt$threshold,
    oof_f1    = bt$f1,
    oof_probs = oof_t
  )

  cat(sprintf(
    "Trial %2d/%d | leaves=%3d lr=%.3f ff=%.2f bf=%.2f min=%3d l2=%.3f | iter=%3d thr=%.2f F1=%.4f\n",
    t, N_TRIALS_E,
    params_t$num_leaves, params_t$learning_rate, params_t$feature_fraction,
    params_t$bagging_fraction, params_t$min_data_in_leaf, params_t$lambda_l2,
    best_iter_t, bt$threshold, bt$f1
  ))
}

# Mejor trial
f1_vec <- sapply(resultados_E, function(r) r$oof_f1)
best_t <- which.max(f1_vec)
best_E <- resultados_E[[best_t]]

cat("\n--- Mejor trial Modelo E ---\n")
cat("Trial        :", best_t, "\n")
cat("OOF F1       :", round(best_E$oof_f1, 4), "\n")
cat("Umbral óptimo:", best_E$threshold, "\n")
cat("Iteraciones  :", best_E$best_iter, "\n\n")

# Modelo final entrenado sobre todo train
set.seed(2025)
model_E <- lgb.train(
  params  = best_E$params,
  data    = dtrain_full,
  nrounds = best_E$best_iter
)

oof_probs_E <- best_E$oof_probs
preds_E     <- predict(model_E, X_test)

predictSample_E <- test %>%
  transmute(
    id    = id,
    pobre = ifelse(preds_E >= best_E$threshold, 1, 0)
  )

cat("Distribución predicciones Modelo E (random search + F1 OOF):\n")
print(table(predictSample_E$pobre))

name_E <- paste0(
  "LGBM_randsearch_F1_",
  "leaves_", best_E$params$num_leaves,
  "_iter_",  best_E$best_iter,
  "_threshold_", fmt_p(best_E$threshold, 3),
  ".csv"
)
path_E <- file.path("02_outputs", "predictions", name_E)
write.csv(predictSample_E, path_E, row.names = FALSE)
cat("Modelo E guardado en:", path_E, "\n\n")


# ============================================================
# 6. Guardar probabilidades para ensemble con Random Forest
# ============================================================
# Exportamos las probabilidades OOF (sobre train) y sobre test
# del Modelo E (random search optimizando F1 directo). También
# guardamos las del Modelo C para retro-compatibilidad con el RF.

saveRDS(
  list(
    # Modelo C (retro-compatibilidad con 05_RandomForest.R)
    oof_probs    = oof_probs_C,
    test_probs   = preds_C,
    # Modelo E (mejor según F1 OOF — úsese en stacking)
    oof_probs_E  = oof_probs_E,
    test_probs_E = preds_E,
    threshold_E  = best_E$threshold,
    y_train      = y_train
  ),
  file = file.path("02_outputs", "lgbm_probs_ensemble.rds")
)

cat("Probabilidades LightGBM (C y E) guardadas para ensemble.\n")
