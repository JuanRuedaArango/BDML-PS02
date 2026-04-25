# ============================================================
# LINEAR PROBABILITY MODEL (LPM) - POBREZA
# ============================================================
#
# Base disponible:
# train
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
#   F) Full + interacciones
#   G) Full + no linealidades
#
# Luego:
#   - Comparación CV
#   - Selección mejor modelo
#   - Umbral óptimo F1 usando OOF
#   - Predicción test
#   - Exportación CSV
# ============================================================

library(caret)
library(dplyr)
library(pROC)

set.seed(2025)

# ============================================================
# CONTROL DE VALIDACIÓN
# ============================================================

ctrl_lpm <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = TRUE
)

# ============================================================
# VARIABLE DEPENDIENTE (LPM requiere 0/1)
# ============================================================

train$Pobre_num <- ifelse(train$Pobre == "Yes", 1, 0)

# ============================================================
# FORMULA DEL MODELO
# ============================================================

formula_lpm <- Pobre_num ~
  num_minors +
  prop_dependiente +
  prop_informal +
  n_personas +
  headWoman +
  cat_educHead +
  cat_maxEduc +
  formalHead +
  hogar_grande +
  sin_ocupados +
  persons_per_worker +
  minors_per_worker +
  n_personas:num_minors +
  prop_dependiente:prop_informal +
  hogar_grande:minors_per_worker +
  headWoman:sin_ocupados +
  cat_maxEduc:prop_informal +
  I(n_personas^2) +
  I(prop_dependiente^2) +
  I(persons_per_worker^2)

# ============================================================
# MODELOS LPM
# ============================================================

lpm1 <- train(
  formula_lpm,
  data = train,
  method = "lm",
  metric = "RMSE",
  trControl = ctrl_lpm
)

lpm2 <- train(
  update(formula_lpm, . ~ . + cat_maxEduc),
  data = train,
  method = "lm",
  metric = "RMSE",
  trControl = ctrl_lpm
)

lpm3 <- train(
  update(formula_lpm, . ~ . + prop_dependiente:prop_informal),
  data = train,
  method = "lm",
  metric = "RMSE",
  trControl = ctrl_lpm
)

lpm4 <- train(
  update(formula_lpm, . ~ . + I(prop_dependiente^2)),
  data = train,
  method = "lm",
  metric = "RMSE",
  trControl = ctrl_lpm
)

lpm5 <- train(
  formula_lpm,
  data = train,
  method = "lm",
  metric = "RMSE",
  trControl = ctrl_lpm
)

# ============================================================
# COMPARACIÓN DE MODELOS
# ============================================================

results_LPM <- resamples(list(
  lpm1 = lpm1,
  lpm2 = lpm2,
  lpm3 = lpm3,
  lpm4 = lpm4,
  lpm5 = lpm5
))

summary(results_LPM)
results_LPM$values

# ============================================================
# MEJOR MODELO
# ============================================================

best_model_LPM <- lpm5

# ============================================================
# THRESHOLD ÓPTIMO (F1)
# ============================================================

oof <- best_model_LPM$pred

yhat <- oof$obs
phat <- oof$pred

roc_obj <- roc(
  response = yhat,
  predictor = phat,
  levels = c(0, 1),
  direction = "<"
)

cutoffs <- data.frame(
  coords(
    roc_obj,
    x = seq(0, 1, length.out = 200),
    input = "threshold",
    ret = c("threshold", "precision", "recall")
  )
)

cutoffs <- cutoffs %>%
  mutate(
    F1 = ifelse(
      precision + recall == 0,
      0,
      2 * precision * recall / (precision + recall)
    )
  )

best_cutoff_LPM <- cutoffs %>%
  arrange(desc(F1)) %>%
  slice(1)

best_cutoff_LPM

# ============================================================
# PREDICCIÓN FINAL EN TEST
# ============================================================

pred_prob_LPM <- predict(best_model_LPM, newdata = test)

pred_class_LPM <- ifelse(
  pred_prob_LPM >= best_cutoff_LPM$threshold,
  1, 0
)

output_LPM <- data.frame(
  id = test$id,
  Pobre = pred_class_LPM
)

write.csv(
  output_LPM,
  "02_outputs/predictions/LPM_model.csv",
  row.names = FALSE
)