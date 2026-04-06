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

todas_vars <- c(
  "prop_dependiente", "prop_informal", "mujer_jefe_ocup",
  "prop_educ_alta",   "avg_educ_adultos", "tasa_desempleo_hogar",
  "hacinamiento",     "n_servicios"
)

train <- train_hogares %>%
  select(id) %>%
  bind_cols(train) %>%                        # id + columnas existentes
  left_join(train_features, by = "id") %>%    # añadir las 8 nuevas variables
  select(-id)                                 # volver a quitar id

test <- test %>%
  left_join(test_features, by = "id")


# ============================================================
# 4. Verificación post-join
# ============================================================

cat("============================================================\n")
cat(" Verificación post-join\n")
cat("============================================================\n")

# 4.1 Número de filas
cat("Filas train antes:", nrow(train_hogares), "| después:", nrow(train), "\n")
cat("Filas test  antes:", nrow(test_hogares),  "| después:", nrow(test),  "\n\n")

# 4.2 Nuevas variables presentes
cat("Nuevas variables en train:", all(todas_vars %in% names(train)), "\n")
cat("Nuevas variables en test :", all(todas_vars %in% names(test)),  "\n\n")

# 4.3 NAs en nuevas variables
cat("NAs nuevas variables — train:\n")
print(colSums(is.na(train[todas_vars])))

cat("\nNAs nuevas variables — test:\n")
print(colSums(is.na(test[todas_vars])))

# 4.4 Distribución rápida en train
cat("\n-- Resúmenes en train --\n")
cat("prop_dependiente     :\n");  print(summary(train$prop_dependiente))
cat("prop_informal        :\n");  print(summary(train$prop_informal))
cat("avg_educ_adultos     :\n");  print(summary(train$avg_educ_adultos))
cat("tasa_desempleo_hogar :\n");  print(summary(train$tasa_desempleo_hogar))
cat("hacinamiento         :\n");  print(summary(train$hacinamiento))
cat("n_servicios          :\n");  print(table(train$n_servicios))
cat("mujer_jefe_ocup      :\n");  print(table(train$mujer_jefe_ocup))
cat("prop_educ_alta       :\n");  print(summary(train$prop_educ_alta))
