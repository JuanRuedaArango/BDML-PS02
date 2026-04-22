# ============================================================
# Random Forest: múltiples variantes de predicción de pobreza
# ============================================================
#
# Depende de:
#   - train, test       (bases finales de 02_feature_engineering.R)
#   - 02_outputs/lgbm_probs_ensemble.rds  (generado por 04_LightGBM.R)
#   - pROC, ranger, dplyr, caret
#
# ¿Por qué Random Forest?
#   RF es bagging sobre árboles: construye árboles en paralelo
#   sobre subconjuntos aleatorios de datos y features. Sus errores
#   están poco correlacionados con los de LightGBM (boosting), lo
#   que hace que el ensemble de ambos sea mejor que cualquiera solo.
#   Además, ranger produce predicciones OOB (out-of-bag) gratuitas
#   sin necesidad de CV manual: cada árbol predice sobre las
#   observaciones que no usó, dando un estimador OOF honesto.
#
# Variantes:
#   A) Baseline   : grid search (mtry × min.node.size), umbral 0.5
#   B) Weighted+PR: mejores hiperparámetros + umbral óptimo PR (OOB)
#   C) Ensemble   : promedio RF_B + LightGBM_C, umbral óptimo PR
#
# Estructura:
#   1. Preparación
#   2. Grid search por OOB F1
#   3. Entrenamiento de los tres modelos
#   4. Importancia de variables
#   5. Predicciones sobre test
#   6. Guardar archivos de envío
# ============================================================


# ============================================================
# 1. Preparación
# ============================================================

# --- Peso por desbalance de clase ---
# Mismo criterio que en LightGBM: peso(Yes) = N_No / N_Yes
pos_weight_rf <- sum(train$Pobre == "No") / sum(train$Pobre == "Yes")

cat("Proporción de pobres en train :", round(mean(train$Pobre == "Yes"), 4), "\n")
cat("scale_pos_weight              :", round(pos_weight_rf, 3), "\n\n")

# Pesos por observación (ranger acepta case.weights)
obs_weights <- ifelse(train$Pobre == "Yes", pos_weight_rf, 1)
obs_weights  <- obs_weights / mean(obs_weights)  # normalizar a media 1

# --- Número de predictores ---
# train no tiene id; test sí, lo excluimos al predecir.
p <- ncol(train) - 1   # columnas menos Pobre
cat("Predictores disponibles:", p, "\n\n")

# --- Cargar probabilidades LightGBM para el ensemble ---
lgbm_path <- file.path("02_outputs", "lgbm_probs_ensemble.rds")

if (file.exists(lgbm_path)) {
  lgbm_saved      <- readRDS(lgbm_path)
  lgbm_oof_probs  <- lgbm_saved$oof_probs
  lgbm_test_probs <- lgbm_saved$test_probs
  cat("Probabilidades LightGBM cargadas correctamente.\n\n")
} else {
  lgbm_oof_probs  <- NULL
  lgbm_test_probs <- NULL
  warning("lgbm_probs_ensemble.rds no encontrado. ",
          "El Modelo C (ensemble) no podrá calcularse. ",
          "Corre primero 04_LightGBM.R.")
}


# ============================================================
# 2. Grid search por OOB F1
# ============================================================
# Exploramos combinaciones de:
#   mtry         : variables candidatas por split.
#                  La regla usual es sqrt(p); exploramos también
#                  p/3 (más aleatorio) y p/2 (más informado).
#   min.node.size: mínimo de observaciones en una hoja. Valores
#                  bajos → árboles más profundos y complejos.
#
# Usamos las predicciones OOB (out-of-bag) de ranger como
# estimador libre de data leakage: cada árbol predice las ~37%
# observaciones que no usó en su bootstrap. Esto nos da probs
# OOB para todo el dataset de entrenamiento sin CV manual.
#
# Métrica de selección: F1 con umbral 0.5 sobre probs OOB.

grid_rf <- expand.grid(
  mtry          = unique(c(floor(sqrt(p)),
                           floor(p / 3),
                           floor(p / 2))),
  min.node.size = c(5, 10, 20),
  stringsAsFactors = FALSE
)

cat("Combinaciones en la grilla:", nrow(grid_rf), "\n\n")

resultados_rf <- vector("list", nrow(grid_rf))

for (i in seq_len(nrow(grid_rf))) {

  set.seed(2025)

  m_i <- ranger(
    formula       = Pobre ~ .,
    data          = train,
    num.trees     = 500,
    mtry          = grid_rf$mtry[i],
    min.node.size = grid_rf$min.node.size[i],
    class.weights = c("No" = 1, "Yes" = pos_weight_rf),
    probability   = TRUE,       # predice probabilidades, no clases
    importance    = "none",     # desactivado en grid search (costo)
    seed          = 2025,
    num.threads   = 1           # fijamos hilo para reproducibilidad
  )

  # Probabilidades OOB para la clase positiva
  oob_probs_i <- m_i$predictions[, "Yes"]

  # F1 con umbral 0.5
  pred_i  <- ifelse(oob_probs_i >= 0.5, "Yes", "No")
  tp_i    <- sum(pred_i == "Yes" & train$Pobre == "Yes")
  fp_i    <- sum(pred_i == "Yes" & train$Pobre == "No")
  fn_i    <- sum(pred_i == "No"  & train$Pobre == "Yes")
  prec_i  <- ifelse(tp_i + fp_i > 0, tp_i / (tp_i + fp_i), 0)
  rec_i   <- ifelse(tp_i + fn_i > 0, tp_i / (tp_i + fn_i), 0)
  f1_i    <- ifelse(prec_i + rec_i > 0,
                    2 * prec_i * rec_i / (prec_i + rec_i), 0)

  # OOB error (tasa de error de la clase agregada)
  oob_err_i <- m_i$prediction.error

  resultados_rf[[i]] <- data.frame(
    mtry          = grid_rf$mtry[i],
    min.node.size = grid_rf$min.node.size[i],
    oob_f1        = f1_i,
    oob_error     = oob_err_i
  )

  cat(sprintf(
    "Combo %2d/%d | mtry=%2d min.node=%2d | OOB F1=%.4f OOB err=%.4f\n",
    i, nrow(grid_rf),
    grid_rf$mtry[i], grid_rf$min.node.size[i],
    f1_i, oob_err_i
  ))
}

cv_rf <- bind_rows(resultados_rf)

# Mejor combinación según OOB F1
best_rf <- cv_rf %>% arrange(desc(oob_f1)) %>% slice(1)

cat("\n--- Mejor combinación ---\n")
print(best_rf)
cat("\n")


# ============================================================
# 3A. Modelo A — Baseline, umbral 0.5
# ============================================================
# Entrenamos con los mejores hiperparámetros del grid search.
# Activamos importance = "impurity" para el análisis posterior.

set.seed(2025)

model_rf_A <- ranger(
  formula       = Pobre ~ .,
  data          = train,
  num.trees     = 500,
  mtry          = best_rf$mtry,
  min.node.size = best_rf$min.node.size,
  class.weights = c("No" = 1, "Yes" = pos_weight_rf),
  probability   = TRUE,
  importance    = "impurity",
  seed          = 2025,
  num.threads   = 1
)

cat("Modelo A entrenado. OOB error:", round(model_rf_A$prediction.error, 4), "\n\n")


# ============================================================
# 3B. Modelo B — Mejores hiperparámetros + umbral óptimo PR
# ============================================================
# Usamos las probabilidades OOB de model_rf_A (mismos parámetros)
# para encontrar el umbral que maximiza F1 sin data leakage.
# Ventaja: no necesitamos CV manual; las probs OOB ya son OOF.

oob_probs_B <- model_rf_A$predictions[, "Yes"]

roc_rf <- pROC::roc(
  response  = train$Pobre,
  predictor = oob_probs_B,
  levels    = c("No", "Yes"),
  direction = "<"
)

pr_cutoffs_rf <- data.frame(
  pROC::coords(
    roc_rf,
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

best_cutoff_rf <- pr_cutoffs_rf %>% arrange(desc(F1)) %>% slice(1)

cat("Umbral óptimo Modelo B:\n")
print(best_cutoff_rf)
cat("\n")

# El modelo para B es el mismo que A; solo cambia el umbral.
model_rf_B <- model_rf_A


# ============================================================
# 3C. Modelo C — Ensemble RF + LightGBM, umbral óptimo PR
# ============================================================
# Promediamos las probabilidades de RF y LightGBM para obtener
# una predicción más robusta. El ensemble se beneficia de que
# los dos algoritmos cometen errores distintos:
#   - RF   : bagging, alta varianza capturada por promedio de árboles
#   - LGBM : boosting, sesgo corregido iterativamente
#
# Para el umbral óptimo del ensemble usamos:
#   - OOB probs de RF (de model_rf_A)
#   - OOF probs de LightGBM modelo C (cargadas al inicio)
# Si LightGBM no está disponible, el Modelo C se omite.

if (!is.null(lgbm_oof_probs)) {

  # Promedio simple de las dos fuentes de probabilidad
  ensemble_oof_probs <- 0.5 * oob_probs_B + 0.5 * lgbm_oof_probs

  roc_ens <- pROC::roc(
    response  = train$Pobre,
    predictor = ensemble_oof_probs,
    levels    = c("No", "Yes"),
    direction = "<"
  )

  pr_cutoffs_ens <- data.frame(
    pROC::coords(
      roc_ens,
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

  best_cutoff_ens <- pr_cutoffs_ens %>% arrange(desc(F1)) %>% slice(1)

  cat("Umbral óptimo Modelo C (ensemble):\n")
  print(best_cutoff_ens)
  cat("\n")

} else {
  cat("Modelo C (ensemble) omitido: LightGBM no disponible.\n\n")
}


# ============================================================
# 4. Importancia de variables
# ============================================================
# La importancia por impureza (Gini) mide cuánto reduce cada
# variable la impureza promedio de los nodos donde se usa.
# Variables con mayor importancia son las que más separan
# pobres de no-pobres a lo largo de todos los árboles.

importancia <- data.frame(
  variable    = names(model_rf_A$variable.importance),
  importancia = model_rf_A$variable.importance
) %>%
  arrange(desc(importancia)) %>%
  mutate(importancia_rel = importancia / sum(importancia))

cat("============================================================\n")
cat(" Top 15 variables más importantes (impureza Gini)\n")
cat("============================================================\n")
print(head(importancia, 15))
cat("\n")


# ============================================================
# 5. Predicciones sobre test
# ============================================================

test_sin_id <- test %>% select(-id)

# --- Modelo A: umbral 0.5 ---
probs_test_rf <- predict(model_rf_A, data = test_sin_id)$predictions[, "Yes"]

predictSample_rfA <- test %>%
  transmute(
    id    = id,
    pobre = ifelse(probs_test_rf >= 0.5, 1, 0)
  )

cat("Distribución predicciones Modelo A (RF baseline):\n")
print(table(predictSample_rfA$pobre))

# --- Modelo B: umbral óptimo RF ---
predictSample_rfB <- test %>%
  transmute(
    id    = id,
    pobre = ifelse(probs_test_rf >= best_cutoff_rf$threshold, 1, 0)
  )

cat("\nDistribución predicciones Modelo B (RF + PR threshold):\n")
print(table(predictSample_rfB$pobre))

# --- Modelo C: ensemble, umbral óptimo ---
if (!is.null(lgbm_test_probs)) {

  ensemble_test_probs <- 0.5 * probs_test_rf + 0.5 * lgbm_test_probs

  predictSample_rfC <- test %>%
    transmute(
      id    = id,
      pobre = ifelse(ensemble_test_probs >= best_cutoff_ens$threshold, 1, 0)
    )

  cat("\nDistribución predicciones Modelo C (ensemble RF + LGBM):\n")
  print(table(predictSample_rfC$pobre))
}


# ============================================================
# 6. Guardar archivos de envío
# ============================================================

fmt_p <- function(x, d = 4) gsub("\\.", "_", as.character(round(x, d)))

base_tag_rf <- paste0(
  "mtry_", best_rf$mtry,
  "_node_", best_rf$min.node.size
)

# --- A ---
name_rfA <- paste0("RF_base_", base_tag_rf, ".csv")
path_rfA <- file.path("02_outputs", "predictions", name_rfA)
write.csv(predictSample_rfA, path_rfA, row.names = FALSE)
cat("\nModelo A guardado en:", path_rfA, "\n")

# --- B ---
name_rfB <- paste0(
  "RF_weighted_PR_", base_tag_rf,
  "_threshold_", fmt_p(best_cutoff_rf$threshold, 3),
  ".csv"
)
path_rfB <- file.path("02_outputs", "predictions", name_rfB)
write.csv(predictSample_rfB, path_rfB, row.names = FALSE)
cat("Modelo B guardado en:", path_rfB, "\n")

# --- C ---
if (!is.null(lgbm_test_probs)) {
  name_rfC <- paste0(
    "Ensemble_RF_LGBM_threshold_", fmt_p(best_cutoff_ens$threshold, 3),
    ".csv"
  )
  path_rfC <- file.path("02_outputs", "predictions", name_rfC)
  write.csv(predictSample_rfC, path_rfC, row.names = FALSE)
  cat("Modelo C guardado en:", path_rfC, "\n")
}
