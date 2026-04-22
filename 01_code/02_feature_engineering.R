# ============================================================
# Feature Engineering: Variables socioeconómicas del hogar
# ============================================================
# Construye ocho variables a nivel hogar a partir de los datos
# crudos de personas y hogares, y las incorpora a train y test.
#
# Depende de:
#   - bases$train_personas, bases$test_personas  (personas crudas)
#   - bases$train_hogares,  bases$test_hogares   (hogares crudos)
#   - train, test    (bases finales de 01_limpieza.R)
#   - build_features_hogar()  (definida en 00_funciones.R)
#
# Variables nuevas (desde personas_raw):
#   prop_dependiente     : (No PET + Inactivos PET) / Total hogar
#   prop_informal        : Informales / Ocupados del hogar
#   mujer_jefe_ocup      : 1 si jefe de hogar es mujer y está ocupada
#   prop_educ_alta       : Personas con educ. superior / Total hogar
#   avg_educ_adultos     : Promedio cat_educ de adultos (≥18 años)
#   tasa_desempleo_hogar : Desocupados / PEA del hogar
#
# Variables nuevas (desde hogares_raw):
#   hacinamiento         : Total personas / Cuartos para dormir
#   n_servicios          : Suma de servicios públicos disponibles (0-5)
# ============================================================


# ============================================================
# 1. Construir las ocho variables a nivel hogar
# ============================================================

# bases$train_personas / bases$test_personas  → datos crudos de personas
# bases$train_hogares  / bases$test_hogares   → datos crudos de hogares
# Ambos se conservan en la lista 'bases' tras el rm() de 01_limpieza.R

train_features <- build_features_hogar(
  personas_raw = bases$train_personas,
  hogares_raw  = bases$train_hogares
)

test_features <- build_features_hogar(
  personas_raw = bases$test_personas,
  hogares_raw  = bases$test_hogares
)


# ============================================================
# 2. Verificación de las nuevas variables
# ============================================================

cat("============================================================\n")
cat(" Verificación train_features\n")
cat("============================================================\n")
cat("Filas       :", nrow(train_features), "\n")
cat("IDs únicos  :", n_distinct(train_features$id), "\n")
cat("Duplicados  :", anyDuplicated(train_features$id), "\n\n")

# --- Rangos y distribuciones ---
cat("-- Variables desde personas --\n")
cat("prop_dependiente     — rango :", range(train_features$prop_dependiente,     na.rm = TRUE), "\n")
cat("prop_informal        — rango :", range(train_features$prop_informal,        na.rm = TRUE), "\n")
cat("prop_educ_alta       — rango :", range(train_features$prop_educ_alta,       na.rm = TRUE), "\n")
cat("avg_educ_adultos     — rango :", range(train_features$avg_educ_adultos,     na.rm = TRUE), "\n")
cat("tasa_desempleo_hogar — rango :", range(train_features$tasa_desempleo_hogar, na.rm = TRUE), "\n")
cat("mujer_jefe_ocup      — tabla :\n")
print(table(train_features$mujer_jefe_ocup, useNA = "ifany"))

cat("\n-- Variables desde hogares --\n")
cat("hacinamiento — rango :", range(train_features$hacinamiento, na.rm = TRUE), "\n")
cat("n_servicios  — tabla :\n")
print(table(train_features$n_servicios, useNA = "ifany"))

# --- NAs ---
cat("\nNAs en train_features:\n")
print(colSums(is.na(train_features)))

cat("\nNAs en test_features:\n")
print(colSums(is.na(test_features)))
cat("\n")


# ============================================================
# 3. Incorporar variables a train y test
# ============================================================

# train no tiene 'id' (fue removido al final de 01_limpieza.R).
# Lo recuperamos temporalmente desde train_hogares, que sí lo
# tiene y mantiene el mismo orden de filas que train.

# n_servicios se incluye solo si las columnas P4030Sx existen en los datos.
# build_features_hogar() devuelve NA en esa columna cuando no las encuentra.
todas_vars <- c(
  "prop_dependiente", "prop_informal", "mujer_jefe_ocup",
  "prop_educ_alta",   "avg_educ_adultos", "tasa_desempleo_hogar",
  "hacinamiento",     "n_servicios"
)

# Variables que realmente usaremos en los modelos: excluimos n_servicios
# si es todo NA (columnas de servicios ausentes en esta versión del GEIH).
vars_modelo <- todas_vars[
  sapply(todas_vars, function(v) !all(is.na(train_features[[v]])))
]

train <- train_hogares %>%
  select(id) %>%
  bind_cols(train) %>%                        # id + columnas existentes
  left_join(train_features, by = "id") %>%    # añadir las 8 nuevas variables
  select(-id) %>%                             # volver a quitar id
  select(-any_of(setdiff(todas_vars, vars_modelo)))  # quitar vars todo-NA

test <- test %>%
  left_join(test_features, by = "id") %>%
  select(-any_of(setdiff(todas_vars, vars_modelo)))  # consistente con train


# ============================================================
# 4. Verificación post-join
# ============================================================

cat("============================================================\n")
cat(" Verificación post-join\n")
cat("============================================================\n")

# 4.1 Número de filas
cat("Filas train antes:", nrow(train_hogares), "| después:", nrow(train), "\n")
cat("Filas test  antes:", nrow(test_hogares),  "| después:", nrow(test),  "\n\n")

# 4.2 Nuevas variables presentes (usamos vars_modelo: las que quedaron en train/test)
cat("Variables en modelos en train:", all(vars_modelo %in% names(train)), "\n")
cat("Variables en modelos en test :", all(vars_modelo %in% names(test)),  "\n")
cat("Variables usadas en modelos:", paste(vars_modelo, collapse = ", "), "\n\n")

# 4.3 NAs en nuevas variables (solo las que existen en train/test)
cat("NAs nuevas variables — train:\n")
print(colSums(is.na(train[vars_modelo])))

cat("\nNAs nuevas variables — test:\n")
print(colSums(is.na(test[vars_modelo])))

# 4.4 Distribución rápida en train
cat("\n-- Resúmenes en train --\n")
cat("prop_dependiente     :\n");  print(summary(train$prop_dependiente))
cat("prop_informal        :\n");  print(summary(train$prop_informal))
cat("avg_educ_adultos     :\n");  print(summary(train$avg_educ_adultos))
cat("tasa_desempleo_hogar :\n");  print(summary(train$tasa_desempleo_hogar))
cat("hacinamiento         :\n");  print(summary(train$hacinamiento))
if ("n_servicios" %in% vars_modelo) {
  cat("n_servicios          :\n");  print(table(train$n_servicios))
} else {
  cat("n_servicios          : no disponible en esta versión del GEIH\n")
}
cat("mujer_jefe_ocup      :\n");  print(table(train$mujer_jefe_ocup))
cat("prop_educ_alta       :\n");  print(summary(train$prop_educ_alta))


# ============================================================
# 5. Variables adicionales para mejorar la predicción
# ============================================================
# Las variables del bloque anterior capturan bien la estructura
# de empleo e informalidad del hogar. Este bloque añade dos
# dimensiones complementarias con fuerte respaldo en la literatura
# de pobreza para Colombia:
#
# (A) Condiciones de la vivienda y el entorno (desde hogares_raw):
#     zona_rural, vivienda_precaria, sin_agua_red, sin_sanitario y
#     cuartos_por_persona. Las tres primeras son dimensiones del
#     Índice de Pobreza Multidimensional (IPM) del DANE. La zona
#     rural por sí sola puede explicar 15-20 pp de diferencia en
#     la tasa de pobreza (DANE, 2023).
#
# (B) Perfil del jefe e ingresos no laborales (desde personas_raw):
#     jefe_edad, jefe_cuenta_propia, jefe_anos_educ,
#     jefe_sin_pension, prop_subsidiado, tiene_remesas,
#     tiene_pension_ing y prop_subempleado. La afiliación al
#     Régimen Subsidiado (prop_subsidiado) es un proxy directo de
#     pobreza porque el SGSSS solo subsidia a quienes no tienen
#     capacidad de pago.
#
# Todas las variables usan columnas disponibles en train Y test.
# ============================================================


# ============================================================
# 5.1 Variables desde hogares_raw
# ============================================================

nuevas_train_hog <- build_nuevas_hogares(bases$train_hogares)
nuevas_test_hog  <- build_nuevas_hogares(bases$test_hogares)


# ============================================================
# 5.2 Variables desde personas_raw
# ============================================================

nuevas_train_per <- build_nuevas_personas(bases$train_personas)
nuevas_test_per  <- build_nuevas_personas(bases$test_personas)


# ============================================================
# 5.3 Verificación de las nuevas variables
# ============================================================

cat("============================================================\n")
cat(" Verificación variables adicionales (train)\n")
cat("============================================================\n")

cat("--- Desde hogares ---\n")
cat("zona_rural         — tabla :\n"); print(table(nuevas_train_hog$zona_rural, useNA = "ifany"))
cat("vivienda_precaria  — tabla :\n"); print(table(nuevas_train_hog$vivienda_precaria, useNA = "ifany"))
cat("sin_agua_red       — tabla :\n"); print(table(nuevas_train_hog$sin_agua_red, useNA = "ifany"))
cat("sin_sanitario      — tabla :\n"); print(table(nuevas_train_hog$sin_sanitario, useNA = "ifany"))
cat("cuartos_por_persona— rango :", range(nuevas_train_hog$cuartos_por_persona, na.rm = TRUE),
    "| NAs:", sum(is.na(nuevas_train_hog$cuartos_por_persona)), "\n\n")

cat("--- Desde personas ---\n")
cat("jefe_edad          — rango :", range(nuevas_train_per$jefe_edad, na.rm = TRUE),
    "| NAs:", sum(is.na(nuevas_train_per$jefe_edad)), "\n")
cat("jefe_cuenta_propia — tabla :\n"); print(table(nuevas_train_per$jefe_cuenta_propia, useNA = "ifany"))
cat("jefe_anos_educ     — rango :", range(nuevas_train_per$jefe_anos_educ, na.rm = TRUE),
    "| NAs:", sum(is.na(nuevas_train_per$jefe_anos_educ)), "\n")
cat("jefe_sin_pension   — tabla :\n"); print(table(nuevas_train_per$jefe_sin_pension, useNA = "ifany"))
cat("prop_subsidiado    — rango :", range(nuevas_train_per$prop_subsidiado, na.rm = TRUE), "\n")
cat("tiene_remesas      — tabla :\n"); print(table(nuevas_train_per$tiene_remesas, useNA = "ifany"))
cat("tiene_pension_ing  — tabla :\n"); print(table(nuevas_train_per$tiene_pension_ing, useNA = "ifany"))
cat("prop_subempleado   — rango :", range(nuevas_train_per$prop_subempleado, na.rm = TRUE),
    "| NAs:", sum(is.na(nuevas_train_per$prop_subempleado)), "\n\n")


# ============================================================
# 5.4 Incorporar a train y test
# ============================================================
# train no tiene id → lo recuperamos temporalmente de train_hogares.
# test sí conserva id → basta con left_join directo.

nuevas_vars <- c(
  "zona_rural", "vivienda_precaria", "sin_agua_red", "sin_sanitario",
  "cuartos_por_persona", "jefe_edad", "jefe_cuenta_propia",
  "jefe_anos_educ", "jefe_sin_pension", "prop_subsidiado",
  "tiene_remesas", "tiene_pension_ing", "prop_subempleado"
)

train <- train_hogares %>%
  select(id) %>%
  bind_cols(train) %>%
  left_join(nuevas_train_hog, by = "id") %>%
  left_join(nuevas_train_per, by = "id") %>%
  select(-id)

test <- test %>%
  left_join(nuevas_test_hog, by = "id") %>%
  left_join(nuevas_test_per, by = "id")


# ============================================================
# 5.5 Verificación post-join
# ============================================================

cat("Dimensiones train:", dim(train), "\n")
cat("Dimensiones test :", dim(test),  "\n\n")

cat("Nuevas variables presentes en train:", all(nuevas_vars %in% names(train)), "\n")
cat("Nuevas variables presentes en test :", all(nuevas_vars %in% names(test)),  "\n\n")

cat("NAs nuevas variables — train:\n")
print(colSums(is.na(train[nuevas_vars])))

cat("\nNAs nuevas variables — test:\n")
print(colSums(is.na(test[nuevas_vars])))
