# ============================================================
# 3. Cargar bases
# ============================================================
bases <- load_bases()

# Guardamos copias crudas para poder verificar después
train_hogares_raw  <- bases$train_hogares
train_personas_raw <- bases$train_personas
test_hogares_raw   <- bases$test_hogares
test_personas_raw  <- bases$test_personas

# Asignación de trabajo
train_hogares  <- train_hogares_raw
train_personas <- train_personas_raw
test_hogares   <- test_hogares_raw
test_personas  <- test_personas_raw

# ============================================================
# 4. Preprocesar personas
# ============================================================
train_personas <- pre_process_personas(train_personas_raw)
test_personas  <- pre_process_personas(test_personas_raw)

# ---------------------Verificacion---------------------------

# 4.1 Verificar que no cambió el número de filas
cat("Filas train_personas_raw  :", nrow(train_personas_raw), "\n")
cat("Filas train_personas proc :", nrow(train_personas), "\n")

cat("Filas test_personas_raw   :", nrow(test_personas_raw), "\n")
cat("Filas test_personas proc  :", nrow(test_personas), "\n")

# 4.2 Verificar que id y Orden no se alteraron
cat("id train coincide:", identical(train_personas_raw$id, train_personas$id), "\n")
cat("Orden train coincide:", identical(train_personas_raw$Orden, train_personas$Orden), "\n\n")

cat("id test coincide:", identical(test_personas_raw$id, test_personas$id), "\n")
cat("Orden test coincide:", identical(test_personas_raw$Orden, test_personas$Orden), "\n\n")

# 4.3 Verificar recodificaciones con tablas cruzadas
cat("Tabla P6020 -> woman (train)\n")
print(table(ifelse(train_personas_raw$P6020 == 2, 1, 0), train_personas$woman, useNA = "ifany"))
cat("\n")

cat("Tabla P6050 -> head (train)\n")
print(table(ifelse(train_personas_raw$P6050 == 1, 1, 0), train_personas$head, useNA = "ifany"))
cat("\n")

cat("Tabla P6040 -> minor (train)\n")
print(table(ifelse(train_personas_raw$P6040 <= 6, 1, 0), train_personas$minor, useNA = "ifany"))
cat("\n")

cat("Tabla P6210 -> cat_educ (train)\n")
print(table(ifelse(train_personas_raw$P6210 == 9, 0, train_personas_raw$P6210), train_personas$cat_educ, useNA = "ifany"))
cat("\n")

cat("Tabla Oc -> occupied (train)\n")
print(table(ifelse(is.na(train_personas_raw$Oc), 0, 1), train_personas$occupied, useNA = "ifany"))
cat("\n")

# 4.4 Verificación lógica exacta
cat("woman correcto:", all(train_personas$woman == ifelse(train_personas_raw$P6020 == 2, 1, 0)), "\n")
cat("head correcto:", all(train_personas$head == ifelse(train_personas_raw$P6050 == 1, 1, 0)), "\n")
cat("minor correcto:", all(train_personas$minor == ifelse(train_personas_raw$P6040 <= 6, 1, 0)), "\n")
cat("cat_educ correcto:", all(train_personas$cat_educ == ifelse(train_personas_raw$P6210 == 9, 0, train_personas_raw$P6210)), "\n")
cat("occupied correcto:", all(train_personas$occupied == ifelse(is.na(train_personas_raw$Oc), 0, 1)), "\n\n")


# ============================================================
# 5. Construir variables a nivel hogar desde personas
# ============================================================
train_personas_data <- build_personas_hogar(train_personas)
test_personas_data  <- build_personas_hogar(test_personas)

train_personas_nivel_hogar <- train_personas_data$personas_nivel_hogar
train_personas_hogar       <- train_personas_data$personas_hogar

test_personas_nivel_hogar  <- test_personas_data$personas_nivel_hogar
test_personas_hogar        <- test_personas_data$personas_hogar

# ---------------------Verificacion---------------------------

# 5.1 Número de hogares únicos antes y después
cat("Hogares únicos en train_personas:", n_distinct(train_personas$id), "\n")
cat("Filas en train_personas_nivel_hogar:", nrow(train_personas_nivel_hogar), "\n")

# 5.2 Verificar que cada id aparece una sola vez en la tabla agregada
cat("IDs duplicados en train_personas_nivel_hogar:", anyDuplicated(train_personas_nivel_hogar$id), "\n")
cat("IDs duplicados en train_personas_hogar:", anyDuplicated(train_personas_hogar$id), "\n\n")

# 5.3 Verificar cuántas cabezas de hogar hay por hogar
heads_train <- train_personas %>%
  group_by(id) %>%
  summarise(n_head = sum(head, na.rm = TRUE), .groups = "drop")

cat("Distribución de número de cabezas por hogar (train)\n")
print(table(heads_train$n_head, useNA = "ifany"))
cat("\n")

# 5.4 Verificar agregados contra cálculo esperado
check_agregado_train <- train_personas %>%
  group_by(id) %>%
  summarise(
    num_women_check    = sum(woman, na.rm = TRUE),
    num_minors_check   = sum(minor, na.rm = TRUE),
    cat_maxEduc_check  = max(cat_educ, na.rm = TRUE),
    num_occupied_check = sum(occupied, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(train_personas_nivel_hogar, by = "id")

cat("num_women correcto:", all(check_agregado_train$num_women == check_agregado_train$num_women_check), "\n")
cat("num_minors correcto:", all(check_agregado_train$num_minors == check_agregado_train$num_minors_check), "\n")
cat("cat_maxEduc correcto:", all(check_agregado_train$cat_maxEduc == check_agregado_train$cat_maxEduc_check), "\n")
cat("num_occupied correcto:", all(check_agregado_train$num_occupied == check_agregado_train$num_occupied_check), "\n\n")


# ============================================================
# 6. Preparar base de hogares
# ============================================================
train_hogares <- prepare_hogares(train_hogares_raw, is_train = TRUE)
test_hogares  <- prepare_hogares(test_hogares_raw, is_train = FALSE)

# ---------------------Verificacion---------------------------

# 6.1 Verificar número de filas
cat("Filas train_hogares_raw :", nrow(train_hogares_raw), "\n")
cat("Filas train_hogares proc:", nrow(train_hogares), "\n")

cat("Filas test_hogares_raw  :", nrow(test_hogares_raw), "\n")
cat("Filas test_hogares proc :", nrow(test_hogares), "\n")

# 6.2 Verificar recodificación de rent
cat("Tabla P5090 -> rent (train)\n")
print(table(ifelse(train_hogares_raw$P5090 == 3, 1, 0), train_hogares$rent, useNA = "ifany"))
cat("\n")

cat("rent correcto train:", all(train_hogares$rent == ifelse(train_hogares_raw$P5090 == 3, 1, 0)), "\n\n")

cat("Tabla P5090 -> rent (test)\n")
print(table(ifelse(test_hogares_raw$P5090 == 3, 1, 0), test_hogares$rent, useNA = "ifany"))
cat("\n")

cat("rent correcto test:", all(test_hogares$rent == ifelse(test_hogares_raw$P5090 == 3, 1, 0)), "\n\n")

# 6.3 Verificar que Pobre no cambió en train
cat("Tabla Pobre antes vs después de prepare_hogares (train)\n")
print(table(train_hogares_raw$Pobre, train_hogares$Pobre, useNA = "ifany"))
cat("\n")

cat("Pobre coincide exactamente:", identical(train_hogares_raw$Pobre, train_hogares$Pobre), "\n\n")


# ============================================================
# 7. Construcción de la base final
# ============================================================
train_before_join <- train_hogares
test_before_join  <- test_hogares

train <- train_hogares %>%
  left_join(train_personas_hogar, by = "id") %>%
  select(-id)

test <- test_hogares %>%
  left_join(test_personas_hogar, by = "id")

# ---------------------Verificacion---------------------------

# 7.1 Verificar que no se pierdan filas
cat("Filas train antes join:", nrow(train_before_join), "\n")
cat("Filas train después join:", nrow(train), "\n")

cat("Filas test antes join:", nrow(test_before_join), "\n")
cat("Filas test después join:", nrow(test), "\n")

# 7.2 Verificar hogares sin match
train_join_check <- train_before_join %>%
  left_join(train_personas_hogar, by = "id")

test_join_check <- test_before_join %>%
  left_join(test_personas_hogar, by = "id")

cat("Hogares train sin match en personas_hogar:", sum(is.na(train_join_check$headWoman)), "\n")
cat("Hogares test sin match en personas_hogar:", sum(is.na(test_join_check$headWoman)), "\n\n")


# ============================================================
# 8. Conversión de variables categóricas
# ============================================================
train_before_factors <- train
test_before_factors  <- test

train <- convert_factors(train, is_train = TRUE)
test  <- convert_factors(test, is_train = FALSE)

# ---------------------Verificacion---------------------------

# 8.1 Verificar tipos
cat("Dominio en train es factor:", is.factor(train$Dominio), "\n")
cat("cat_educHead en train es factor:", is.factor(train$cat_educHead), "\n")
cat("Pobre en train es factor:", is.factor(train$Pobre), "\n\n")

cat("Dominio en test es factor:", is.factor(test$Dominio), "\n")
cat("cat_educHead en test es factor:", is.factor(test$cat_educHead), "\n\n")

# 8.2 Verificar Pobre antes vs después
# Antes: 0/1
# Después: No/Yes
pobre_recod_back <- ifelse(train$Pobre == "Yes", 1, 0)

cat("Tabla Pobre antes vs después de convert_factors\n")
print(table(train_before_factors$Pobre, pobre_recod_back, useNA = "ifany"))
cat("\n")

cat("Pobre coincide tras recodificar de regreso:",
    all(train_before_factors$Pobre == pobre_recod_back), "\n\n")

# 8.3 Verificar Dominio antes vs después
cat("Tabla Dominio antes vs después\n")
print(table(train_before_factors$Dominio, as.character(train$Dominio), useNA = "ifany"))
cat("\n")

cat("Dominio coincide:",
    all(as.character(train_before_factors$Dominio) == as.character(train$Dominio)), "\n\n")

# 8.4 Verificar cat_educHead antes vs después
niveles_educ <- c(
  "0" = "No sabe",
  "1" = "Ninguno",
  "2" = "Preescolar",
  "3" = "Primaria",
  "4" = "Secundaria",
  "5" = "Media",
  "6" = "Universitaria"
)

cat("Tabla cat_educHead antes vs después\n")
print(table(
  niveles_educ[as.character(train_before_factors$cat_educHead)],
  as.character(train$cat_educHead),
  useNA = "ifany"
))
cat("\n")

cat("cat_educHead coincide:",
    all(niveles_educ[as.character(train_before_factors$cat_educHead)] == as.character(train$cat_educHead)), "\n\n")

# ============================================================
# 8.5 Limpiar objetos temporales de chequeo
# ============================================================

objetos_a_conservar <- c(
  # Bases principales
  "bases",
  "train_hogares", "train_personas", "test_hogares", "test_personas",
  
  # Objetos construidos desde personas
  "train_personas_data", "test_personas_data",
  "train_personas_nivel_hogar", "train_personas_hogar",
  "test_personas_nivel_hogar", "test_personas_hogar",
  
  # Bases finales
  "train", "test",
  
  # Funciones (se conservan para poder seguir usando el script)
  "load_bases",
  "pre_process_personas",
  "build_personas_hogar",
  "prepare_hogares",
  "convert_factors",
  "make_ctrl",
  "fit_elastic_net",
  "make_submission_name"
)

rm(list = setdiff(ls(), objetos_a_conservar))