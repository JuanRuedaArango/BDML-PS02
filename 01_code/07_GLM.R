# ============================================================
# GLM BINOMIAL: múltiples variantes para predicción de pobreza
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


# ============================================================
# 0. Librerías
# ============================================================
library(caret)
library(dplyr)
library(pROC)

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
# MODELO BASE GLM
# ============================================================

# ==========================================
# CREAR VARIABLES FALTANTES EN train
# ==========================================


train <- train %>%
  mutate(
    n_personas = round(num_occupied + (prop_dependiente * pmax(num_occupied,1))),
    formalHead = ifelse(occupiedHead == 1, 1, 0),
    hogar_grande = ifelse(n_personas >= 5, 1, 0),
    sin_ocupados = ifelse(num_occupied == 0, 1, 0),
    persons_per_worker = n_personas / pmax(num_occupied, 1),
    minors_per_worker = num_minors / pmax(num_occupied, 1)
    )

model_base <- train(
  Pobre ~
    num_minors +
    prop_dependiente +
    prop_informal +
    n_personas +
    headWoman +
    cat_educHead +
    cat_maxEduc +
    formalHead,
  
  data = train,
  method = "glm",
  family = binomial(),
  metric = "Sens",
  trControl = ctrl
)

# ============================================================
# BLOQUE A: MODELOS PROGRESIVOS
# ============================================================

#---------------------------------------------------
# M1: Base simple hogar + laboral
#---------------------------------------------------
model1 <- train(
  Pobre ~
    num_minors +
    prop_dependiente +
    prop_informal +
    n_personas,
  
  data = train,
  method = "glm",
  family = binomial(),
  metric = "Sens",
  trControl = ctrl
)

#---------------------------------------------------
# M2: Modelo completo original
#---------------------------------------------------
model2 <- train(
  Pobre ~
    num_minors +
    prop_dependiente +
    prop_informal +
    n_personas +
    headWoman +
    cat_educHead +
    cat_maxEduc +
    formalHead,
  
  data = train,
  method = "glm",
  family = binomial(),
  metric = "Sens",
  trControl = ctrl
)

#---------------------------------------------------
# M3: Interacción hogar
#---------------------------------------------------
model3 <- train(
  Pobre ~
    num_minors +
    prop_dependiente +
    prop_informal +
    n_personas +
    headWoman +
    cat_educHead +
    cat_maxEduc +
    formalHead +
    n_personas:num_minors,
  
  data = train,
  method = "glm",
  family = binomial(),
  metric = "Sens",
  trControl = ctrl
)

#---------------------------------------------------
# M4: Interacción económica
#---------------------------------------------------
model4 <- train(
  Pobre ~
    num_minors +
    prop_dependiente +
    prop_informal +
    n_personas +
    headWoman +
    cat_educHead +
    cat_maxEduc +
    formalHead +
    prop_dependiente:prop_informal,
  
  data = train,
  method = "glm",
  family = binomial(),
  metric = "Sens",
  trControl = ctrl
)

#---------------------------------------------------
# M5: No lineal + interacciones
#---------------------------------------------------
model5 <- train(
  Pobre ~
    num_minors +
    prop_dependiente +
    prop_informal +
    n_personas +
    headWoman +
    cat_educHead +
    cat_maxEduc +
    formalHead +
    n_personas:num_minors +
    prop_dependiente:prop_informal +
    I(n_personas^2) +
    I(prop_dependiente^2),
  
  data = train,
  method = "glm",
  family = binomial(),
  metric = "Sens",
  trControl = ctrl
)

# ============================================================
# Bloque A: Comparación de modelos
# ============================================================
results_BA <- resamples(list(
  model1 = model1,
  model2 = model2,
  model3 = model3,
  model4 = model4,
  model5 = model5
))

summary(results_BA)
results_BA$values

## Elección del mejor modelo ##
best_model_BA <- model5

# ============================================================
# Bloque A: Buscar threshold óptimo F1 con OOF para el mejor modelo
# ============================================================
glm_oof <- best_model_BA$pred

yhat <- glm_oof$obs
phat <- glm_oof$Yes

roc_obj <- roc(
  response = yhat,
  predictor = phat,
  levels = c("No", "Yes"),
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

best_cutoff_BA <- cutoffs %>%
  arrange(desc(F1)) %>%
  slice(1)

best_cutoff_BA


# ============================================================
# FORMULA BASE NUEVA: Se agregan nuevas variables a la estimación
# ============================================================

form_base <- Pobre ~
  cat_educHead +
  hogar_grande +
  formalHead +
  headWoman +
  sin_ocupados +
  prop_dependiente +
  persons_per_worker +
  cat_maxEduc +
  minors_per_worker +
  prop_informal

# ============================================================
# BLOQUE B: 10 MODELOS NUEVOS - Variables nuevas
# ============================================================

# M1 Base
m1 <- train(
  form_base,
  data = train,
  method = "glm",
  family = binomial(),
  metric = "Sens",
  trControl = ctrl
)

# M2
m2 <- train(
  update(form_base, . ~ . + I(prop_dependiente^2)),
  data = train,
  method = "glm",
  family = binomial(),
  metric = "Sens",
  trControl = ctrl
)

# M3
m3 <- train(
  update(form_base, . ~ . + I(persons_per_worker^2)),
  data = train,
  method = "glm",
  family = binomial(),
  metric = "Sens",
  trControl = ctrl
)

# M4
m4 <- train(
  update(form_base, . ~ . + hogar_grande:minors_per_worker),
  data = train,
  method = "glm",
  family = binomial(),
  metric = "Sens",
  trControl = ctrl
)

# M5
m5 <- train(
  update(form_base, . ~ . + cat_maxEduc:prop_informal),
  data = train,
  method = "glm",
  family = binomial(),
  metric = "Sens",
  trControl = ctrl
)

# M6
m6 <- train(
  update(form_base, . ~ . + headWoman:sin_ocupados),
  data = train,
  method = "glm",
  family = binomial(),
  metric = "Sens",
  trControl = ctrl
)

# M7
m7 <- train(
  update(form_base, . ~ . +
           I(prop_dependiente^2) +
           I(persons_per_worker^2)),
  data = train,
  method = "glm",
  family = binomial(),
  metric = "Sens",
  trControl = ctrl
)

# M8
m8 <- train(
  update(form_base, . ~ . +
           hogar_grande:minors_per_worker +
           headWoman:sin_ocupados),
  data = train,
  method = "glm",
  family = binomial(),
  metric = "Sens",
  trControl = ctrl
)

# M9
m9 <- train(
  update(form_base, . ~ . +
           prop_dependiente:prop_informal),
  data = train,
  method = "glm",
  family = binomial(),
  metric = "Sens",
  trControl = ctrl
)

# M10 Full
m10 <- train(
  update(form_base, . ~ . +
           I(prop_dependiente^2) +
           I(persons_per_worker^2) +
           hogar_grande:minors_per_worker +
           headWoman:sin_ocupados +
           prop_dependiente:prop_informal +
           cat_maxEduc:prop_informal),
  data = train,
  method = "glm",
  family = binomial(),
  metric = "Sens",
  trControl = ctrl
)


# ============================================================
# Bloque B: Comparación de modelos
# ============================================================
results_BB <- resamples(list(
  m1 = m1,
  m2 = m2,
  m3 = m3,
  m4 = m4,
  m5 = m5,
  m6 = m6,
  m7 = m7,
  m8 = m8,
  m9 = m9,
  m10 = m10
))

summary(results_BB)
results_BB$values

## Elección del mejor modelo ##
best_model_BB <- m10

# ============================================================
# Bloque A: Buscar threshold óptimo F1 con OOF para el mejor modelo
# ============================================================
glm_oof <- best_model_BB$pred

yhat <- glm_oof$obs
phat <- glm_oof$Yes

roc_obj <- roc(
  response = yhat,
  predictor = phat,
  levels = c("No", "Yes"),
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

best_cutoff_BB <- cutoffs %>%
  arrange(desc(F1)) %>%
  slice(1)

best_cutoff_BB


# ============================================================
# BLOQUE C: MODELOS FINALES
# ============================================================

#---------------------------------------------------------
# M11
#---------------------------------------------------------
m11 <- train(
  Pobre ~
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
    prop_informal,
  
  data = train,
  method = "glm",
  family = binomial(),
  metric = "Sens",
  trControl = ctrl
)

#---------------------------------------------------------
# M12
#---------------------------------------------------------
m12 <- train(
  Pobre ~
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
    prop_informal +
    n_personas:num_minors +
    prop_dependiente:prop_informal,
  
  data = train,
  method = "glm",
  family = binomial(),
  metric = "Sens",
  trControl = ctrl
)

#---------------------------------------------------------
# M13
#---------------------------------------------------------
m13 <- train(
  Pobre ~
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
    prop_informal +
    hogar_grande:minors_per_worker +
    headWoman:sin_ocupados +
    prop_dependiente:prop_informal +
    cat_maxEduc:prop_informal,
  
  data = train,
  method = "glm",
  family = binomial(),
  metric = "Sens",
  trControl = ctrl
)

#---------------------------------------------------------
# M14
#---------------------------------------------------------
m14 <- train(
  Pobre ~
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
    prop_informal +
    n_personas:num_minors +
    prop_dependiente:prop_informal +
    I(n_personas^2) +
    I(prop_dependiente^2) +
    I(persons_per_worker^2),
  
  data = train,
  method = "glm",
  family = binomial(),
  metric = "Sens",
  trControl = ctrl
)

#---------------------------------------------------------
# M15 (MEJOR)
#---------------------------------------------------------
m15 <- train(
  Pobre ~
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
    prop_informal +
    n_personas:num_minors +
    prop_dependiente:prop_informal +
    hogar_grande:minors_per_worker +
    headWoman:sin_ocupados +
    prop_dependiente:prop_informal +
    cat_maxEduc:prop_informal +
    I(n_personas^2) +
    I(prop_dependiente^2) +
    I(persons_per_worker^2),
  
  data = train,
  method = "glm",
  family = binomial(),
  metric = "Sens",
  trControl = ctrl
)

#---------------------------------------------------------
# M16
#---------------------------------------------------------
m16 <- train(
  Pobre ~
    num_minors +
    prop_dependiente +
    prop_informal +
    headWoman +
    cat_educHead +
    cat_maxEduc +
    formalHead +
    sin_ocupados +
    persons_per_worker +
    prop_dependiente:prop_informal +
    headWoman:sin_ocupados +
    cat_maxEduc:prop_informal +
    I(prop_dependiente^2),
  
  data = train,
  method = "glm",
  family = binomial(),
  metric = "Sens",
  trControl = ctrl
)


# ============================================================
# Bloque C: Comparación de modelos
# ============================================================
results_BC <- resamples(list(
  m11 = m11,
  m12 = m12,
  m13 = m13,
  m14 = m14,
  m15 = m15,
  m16 = m16
))

summary(results_BC)
results_BC$values

## Elección del mejor modelo ##
best_model_BC <- m15

# ============================================================
# Bloque A: Buscar threshold óptimo F1 con OOF para el mejor modelo
# ============================================================
glm_oof <- best_model_BC$pred

yhat <- glm_oof$obs
phat <- glm_oof$Yes

roc_obj <- roc(
  response = yhat,
  predictor = phat,
  levels = c("No", "Yes"),
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

best_cutoff_BC <- cutoffs %>%
  arrange(desc(F1)) %>%
  slice(1)

best_cutoff_BC

# ============================================================
# Comparación mejores modelos de cada bloque
# ============================================================

# Crear tabla resumen de thresholds óptimos

tabla_cutoffs <- data.frame(
  Modelo = c("BA", "BB", "BC"),
  Threshold = c(
    best_cutoff_BA$threshold,
    best_cutoff_BB$threshold,
    best_cutoff_BC$threshold
  ),
  Precision = c(
    best_cutoff_BA$precision,
    best_cutoff_BB$precision,
    best_cutoff_BC$precision
  ),
  Recall = c(
    best_cutoff_BA$recall,
    best_cutoff_BB$recall,
    best_cutoff_BC$recall
  ),
  F1 = c(
    best_cutoff_BA$F1,
    best_cutoff_BB$F1,
    best_cutoff_BC$F1
  )
)

# Ver tabla
tabla_cutoffs

# Ordenar por mejor F1
tabla_cutoffs[order(-tabla_cutoffs$F1), ]

# ============================================================
# Predicción sobre test de los mejores modelos
# ============================================================

fmt_p <- function(x, d = 4) gsub("\\.", "_", as.character(round(x, d)))

# Crear carpeta si no existe
dir.create(file.path("02_outputs", "predictions"),
           recursive = TRUE, showWarnings = FALSE)

# ============================================================
# MODELO A = model5
# ============================================================

# ==========================================
# CREAR VARIABLES FALTANTES EN test
# (mismas transformaciones que train)
# ==========================================

test <- test %>%
  mutate(
    
    n_personas = round(num_occupied + (prop_dependiente * pmax(num_occupied,1))),
    formalHead = ifelse(occupiedHead == 1, 1, 0),
    hogar_grande = ifelse(n_personas >= 5, 1, 0),
    sin_ocupados = ifelse(num_occupied == 0, 1, 0),
    persons_per_worker = n_personas / pmax(num_occupied,1),
    minors_per_worker = num_minors / pmax(num_occupied,1)
    )

pred_prob_A <- predict(model5, newdata = test, type = "prob")

pred_class_A <- ifelse(
  pred_prob_A$Yes >= best_cutoff_BA$threshold,
  1, 0
)

predictSample_A <- data.frame(
  id = test$id,
  pobre = pred_class_A
)

name_A <- paste0(
  "GLM_model5_threshold_",
  fmt_p(best_cutoff_BA$threshold, 3),
  ".csv"
)

path_A <- file.path("02_outputs", "predictions", name_A)

write.csv(predictSample_A, path_A, row.names = FALSE)

cat("Modelo A guardado en:", path_A, "\n")


# ============================================================
# MODELO B = m10
# ============================================================
pred_prob_B <- predict(m10, newdata = test, type = "prob")

pred_class_B <- ifelse(
  pred_prob_B$Yes >= best_cutoff_BB$threshold,
  1, 0
)

predictSample_B <- data.frame(
  id = test$id,
  pobre = pred_class_B
)

name_B <- paste0(
  "GLM_m10_threshold_",
  fmt_p(best_cutoff_BB$threshold, 3),
  ".csv"
)

path_B <- file.path("02_outputs", "predictions", name_B)

write.csv(predictSample_B, path_B, row.names = FALSE)

cat("Modelo B guardado en:", path_B, "\n")


# ============================================================
# MODELO C = m15
# ============================================================
best_model <- m15

pred_prob_C <- predict(best_model, newdata = test, type = "prob")

pred_class_C <- ifelse(
  pred_prob_C$Yes >= best_cutoff_BC$threshold,
  1, 0
)

predictSample_C <- data.frame(
  id = test$id,
  pobre = pred_class_C
)

name_C <- paste0(
  "GLM_m15_threshold_",
  fmt_p(best_cutoff_BC$threshold, 3),
  ".csv"
)

path_C <- file.path("02_outputs", "predictions", name_C)

write.csv(predictSample_C, path_C, row.names = FALSE)

cat("Modelo C guardado en:", path_C, "\n")
