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

# Resumen compacto: para cada columna nueva muestra NAs + estadística breve.
# Esto escala automáticamente cuando se agregan más features.
resumen_features <- function(df, nombre) {
  cat("\n---", nombre, "— columnas:", ncol(df), "---\n")
  for (col in setdiff(names(df), "id")) {
    x <- df[[col]]
    n_na <- sum(is.na(x))
    if (is.numeric(x)) {
      rng <- if (all(is.na(x))) c(NA, NA) else range(x, na.rm = TRUE)
      cat(sprintf("  %-25s NAs=%6d | rango=[%.3g, %.3g]\n", col, n_na, rng[1], rng[2]))
    } else {
      cat(sprintf("  %-25s NAs=%6d | niveles=%d\n", col, n_na, length(unique(x))))
    }
  }
}
resumen_features(nuevas_train_hog, "Desde hogares")
resumen_features(nuevas_train_per, "Desde personas")


# ============================================================
# 5.4 Incorporar a train y test
# ============================================================
# train no tiene id → lo recuperamos temporalmente de train_hogares.
# test sí conserva id → basta con left_join directo.

nuevas_vars <- c(
  # === Desde hogares_raw (build_nuevas_hogares) ===
  "zona_rural", "vivienda_precaria", "sin_agua_red", "sin_sanitario",
  "cuartos_por_persona", "lp_hogar", "li_hogar", "lp_pc", "li_pc",
  "npersug", "nper_vs_ug",

  # === Desde personas_raw: perfil jefe ===
  "jefe_edad", "jefe_cuenta_propia", "jefe_anos_educ", "jefe_sin_pension",
  "jefe_horas", "jefe_tam_empresa", "jefe_meses_trabajo",
  "jefe_seg_trabajo", "jefe_mujer",

  # === Cónyuge ===
  "conyuge_existe", "conyuge_ocupado", "conyuge_horas", "conyuge_anos_educ",

  # === Características generales del hogar ===
  "prop_subsidiado", "tiene_remesas", "tiene_pension_ing", "prop_subempleado",

  # === Diversificación de ingresos ===
  "n_fuentes_ingreso", "prop_trabajo_sec",

  # === Subsidios mensuales P6585 ===
  "prop_sub_transp", "prop_sub_alim", "prop_sub_fam", "prop_sub_educ",
  "any_sub_mensual",

  # === Primas anuales P6630 ===
  "total_primas_hogar", "max_primas_persona",

  # === Transferencias y ayudas ===
  "total_ayudas_hogar", "prop_p7500s2", "prop_p7500s3", "prop_p7505",

  # === Horas trabajadas ===
  "horas_mean_hog", "horas_max_hog", "horas_sd_hog", "total_horas_hog",

  # === Tamaño empresa ===
  "empresa_max_hog", "empresa_mean_hog",

  # === Estabilidad laboral ===
  "meses_trabajo_mean_hog", "meses_trabajo_max_hog",

  # === Segundo trabajo ===
  "prop_seg_trabajo", "horas_seg_trabajo_mean",

  # === Demográficos del hogar ===
  "prop_mujeres", "prop_minoria_etn"
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


# ============================================================
# 5.6 Imputación de NAs en train y test
# ============================================================
# Las variables nuevas pueden tener NAs en casos extremos (hogares
# sin jefe registrado, sin cuartos, sin ocupados, etc.). Los scripts
# de modelos (Elastic Net, LightGBM, Random Forest) no toleran NAs.
# Imputamos aquí una sola vez para que todos los modelos reciban
# datos limpios.
#
# Estrategia:
#   Numéricas : mediana de train (calculada antes de tocar test).
#   Factores  : nivel "Desconocido" para los NAs (más seguro que
#               asignar un string que puede no coincidir con un nivel).
#
# Los valores de imputación se calculan SOLO sobre train para evitar
# data leakage hacia test.

# --- Medianas de train para cada columna numérica ---
cols_num <- names(train)[sapply(train, is.numeric)]
medianas  <- sapply(cols_num, function(col) median(train[[col]], na.rm = TRUE))

# --- Aplicar imputación numérica ---
for (col in cols_num) {
  if (anyNA(train[[col]])) train[[col]][is.na(train[[col]])] <- medianas[[col]]
  if (col %in% names(test) && anyNA(test[[col]]))
    test[[col]][is.na(test[[col]])] <- medianas[[col]]
}

# --- Aplicar imputación de factores ---
cols_fac <- names(train)[sapply(train, is.factor) & names(train) != "Pobre"]
for (col in cols_fac) {
  if (anyNA(train[[col]])) {
    levels(train[[col]]) <- c(levels(train[[col]]), "Desconocido")
    train[[col]][is.na(train[[col]])] <- "Desconocido"
  }
  if (col %in% names(test) && anyNA(test[[col]])) {
    levels(test[[col]]) <- c(levels(test[[col]]), "Desconocido")
    test[[col]][is.na(test[[col]])] <- "Desconocido"
  }
}

# --- Verificación final ---
nas_train_final <- sum(sapply(train, anyNA))
nas_test_final  <- sum(sapply(select(test, -id), anyNA))
cat("\nNAs en train tras imputación:", nas_train_final, "\n")
cat("NAs en test  tras imputación:", nas_test_final,  "\n")


# ============================================================
# 6. Interacciones explícitas
# ============================================================
# LightGBM captura interacciones automáticamente via splits,
# pero agregarlas como features mejora el aprendizaje en pocos
# árboles y ayuda también a modelos lineales (Elastic Net).
#
# Selección basada en conocimiento del dominio colombiano:
#   - Rural + informal  → pobreza estructural agraria
#   - Educación × edad  → ciclo de vida laboral del jefe
#   - Hacinamiento × servicios → calidad multidimensional
#   - Lp_pc × dependencia → presión económica real
#   - Subsidiado × desempleo → vulnerabilidad combinada

make_interactions <- function(df) {
  df %>%
    mutate(
      # Rural × informalidad laboral
      int_rural_informal = as.numeric(zona_rural) * prop_informal,

      # Educación del jefe × edad (ciclo de vida)
      int_educ_edad_jefe = jefe_anos_educ * jefe_edad,

      # Hacinamiento × servicios faltantes (proxy de precariedad)
      int_hacin_servicios = if ("n_servicios" %in% names(df)) {
        hacinamiento * (5 - n_servicios)
      } else {
        hacinamiento
      },

      # Línea per cápita × proporción de dependientes
      int_lppc_dependiente = lp_pc * prop_dependiente,

      # Afiliación subsidiada × tasa de desempleo del hogar
      int_subsid_desempleo = prop_subsidiado * tasa_desempleo_hogar
    )
}

train <- make_interactions(train)
test  <- make_interactions(test)

interacciones <- c(
  "int_rural_informal", "int_educ_edad_jefe", "int_hacin_servicios",
  "int_lppc_dependiente", "int_subsid_desempleo"
)

cat("\n-- Interacciones creadas --\n")
for (v in interacciones) {
  cat(sprintf("%-22s | rango train: [%.3f, %.3f] | NAs: %d\n",
              v,
              min(train[[v]], na.rm = TRUE),
              max(train[[v]], na.rm = TRUE),
              sum(is.na(train[[v]]))))
}


# ============================================================
# 7. Target encoding de Dominio (KFold-safe)
# ============================================================
# Dominio tiene ~25 niveles (ciudades + resto urbano/rural). Un
# one-hot genera muchas columnas dispersas. El target encoding
# reemplaza cada nivel por la tasa de pobreza del grupo, lo que
# da una señal densa y ordinal que los árboles explotan mejor.
#
# Para evitar data leakage usamos K-fold: la codificación del
# fold k se calcula SOLO con los otros folds. Agregamos smoothing
# bayesiano para manejar niveles con pocas observaciones:
#     te = (n_i * mean_i + m * global_mean) / (n_i + m)
# donde m es el "peso" del prior (más alto → más regularización).

if ("Dominio" %in% names(train)) {

  set.seed(2025)
  K  <- 5
  m  <- 20   # fuerza del prior
  global_rate <- mean(train$Pobre == "Yes")

  folds_te <- sample(rep(seq_len(K), length.out = nrow(train)))

  train$dominio_te <- NA_real_

  for (k in seq_len(K)) {
    idx_val   <- which(folds_te == k)
    idx_train <- which(folds_te != k)

    # Estadísticas por Dominio solo con los folds de entrenamiento
    stats_k <- data.frame(
      Dominio = train$Dominio[idx_train],
      y       = as.integer(train$Pobre[idx_train] == "Yes")
    ) %>%
      group_by(Dominio) %>%
      summarize(n_k = n(), mean_k = mean(y), .groups = "drop") %>%
      mutate(te_k = (n_k * mean_k + m * global_rate) / (n_k + m))

    # Aplicar al fold de validación
    map_k <- setNames(stats_k$te_k, as.character(stats_k$Dominio))
    train$dominio_te[idx_val] <- map_k[as.character(train$Dominio[idx_val])]
    train$dominio_te[idx_val][is.na(train$dominio_te[idx_val])] <- global_rate
  }

  # Para test: encoding global usando TODO train (no hay leakage hacia test)
  stats_global <- data.frame(
    Dominio = train$Dominio,
    y       = as.integer(train$Pobre == "Yes")
  ) %>%
    group_by(Dominio) %>%
    summarize(n_g = n(), mean_g = mean(y), .groups = "drop") %>%
    mutate(te_g = (n_g * mean_g + m * global_rate) / (n_g + m))

  map_global <- setNames(stats_global$te_g, as.character(stats_global$Dominio))
  test$dominio_te <- map_global[as.character(test$Dominio)]
  test$dominio_te[is.na(test$dominio_te)] <- global_rate

  cat("\n-- Target encoding de Dominio --\n")
  cat("Tasa global de pobreza:", round(global_rate, 4), "\n")
  cat("Rango dominio_te train:", round(range(train$dominio_te), 4), "\n")
  cat("Rango dominio_te test :", round(range(test$dominio_te),  4), "\n")
  cat("NAs en dominio_te train:", sum(is.na(train$dominio_te)), "\n")
  cat("NAs en dominio_te test :", sum(is.na(test$dominio_te)),  "\n")

} else {
  cat("\nDominio no encontrado; target encoding omitido.\n")
}
