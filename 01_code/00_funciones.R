# ============================================================
# 1. Funciones auxiliares
# ============================================================

# ------------------------------------------------------------
# load_bases()
# ------------------------------------------------------------
# Objetivo:
#   Cargar las cuatro bases principales del ejercicio desde una
#   carpeta base del computador.
#
# ¿Qué espera?
#   Que dentro de la ruta indicada existan estos archivos:
#   - train_hogares.csv
#   - train_personas.csv
#   - test_hogares.csv
#   - test_personas.csv
#
# ¿Qué devuelve?
#   Una lista con 4 data frames, uno por cada archivo.
#
# ¿Por qué se hace así?
#   Porque después es más cómodo trabajar con una sola función
#   que centralice la lectura de los archivos y evite repetir
#   read.csv() cuatro veces en el flujo principal.
# ------------------------------------------------------------
load_bases <- function(path = getOption("bases_dir", "Bases")) {
  
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  
  required_files <- c(
    "train_hogares.csv",
    "train_personas.csv",
    "test_hogares.csv",
    "test_personas.csv"
  )
  
  required_paths <- file.path(path, required_files)
  
  missing_files <- required_files[!file.exists(required_paths)]
  
  if (length(missing_files) > 0) {
    stop(
      paste0(
        "No se encontraron todas las bases requeridas.\n\n",
        "Carpeta buscada:\n",
        path,
        "\n\nArchivos faltantes:\n",
        paste("-", missing_files, collapse = "\n")
      ),
      call. = FALSE
    )
  }
  
  list(
    train_hogares  = read.csv(file.path(path, "train_hogares.csv")),
    train_personas = read.csv(file.path(path, "train_personas.csv")),
    test_hogares   = read.csv(file.path(path, "test_hogares.csv")),
    test_personas  = read.csv(file.path(path, "test_personas.csv"))
  )
}

# ------------------------------------------------------------
# pre_process_personas()
# ------------------------------------------------------------
# Objetivo:
#   Transformar la base de personas para quedarnos solo con las
#   variables que realmente necesitamos para construir variables
#   a nivel hogar.
#
# Entrada:
#   Un data frame de personas (train_personas o test_personas).
#
# Variables nuevas que crea:
#   - woman:
#       1 si la persona es mujer, 0 en caso contrario.
#       Se construye desde P6020 (en el original, 2 es mujer
#       y 1 es hombre).
#
#   - head:
#       1 si la persona es cabeza del hogar, 0 en caso contrario.
#       Se construye desde P6050 (en el original, jefe de hogar es
#       1, los otros 8 valores son otro tipo de parentesco).
#
#   - minor:
#       1 si la persona tiene 17 años o menos, 0 en caso contrario.
#       Se construye de P6040 (años cumplidos)
#       Esto permite contar menores en el hogar.
#
#   - cat_educ:
#       Categoría educativa ajustada.
#       Aquí se recodifica P6210 == 9 como 0.
#       Esto porque 9 en la original es "No se sabe, no informa",
#       por lo que no añade valor) .
#
#   - occupied:
#       1 si la persona aparece ocupada, 0 si no.
#       Se usa la variable Oc; si está en NA asumimos no ocupado.
#
# Después:
#   Se conservan solo las columnas que servirán en el resto del
#   proceso: id, Orden y las variables derivadas.
#
# ¿Por qué?
#   Porque la predicción final es a nivel hogar, pero parte de la
#   información relevante está a nivel persona. Entonces primero
#   limpiamos y simplificamos esa base.
# ------------------------------------------------------------
pre_process_personas <- function(data) {
  data %>%
    mutate(
      woman    = ifelse(P6020 == 2, 1, 0),
      head     = ifelse(P6050 == 1, 1, 0),
      minor    = ifelse(P6040 <= 17, 1, 0),
      cat_educ = ifelse(P6210 == 9, 0, P6210),
      occupied = ifelse(is.na(Oc), 0, 1)
    ) %>%
    dplyr::select(id, Orden, woman, head, minor, cat_educ, occupied)
}

# ------------------------------------------------------------
# build_personas_hogar()
# ------------------------------------------------------------
# Objetivo:
#   Pasar de una base a nivel persona a variables útiles a nivel
#   hogar, agrupando por id del hogar.
#
# Entrada:
#   Un data frame ya preprocesado con la función
#   pre_process_personas().
#
# Esta función construye dos objetos:
#
# 1) personas_nivel_hogar
#    Resume información del hogar usando todas las personas del
#    mismo id:
#    - num_women: número de mujeres en el hogar
#    - num_minors: número de menores en el hogar
#    - cat_maxEduc: máximo nivel educativo observado en el hogar
#    - num_occupied: número de personas ocupadas
#
#    ¿Por qué resumir?
#    Porque el modelo final trabaja con una fila por hogar, no
#    una fila por persona.
#
# 2) personas_hogar
#    Toma únicamente la fila de la cabeza del hogar (head == 1)
#    para obtener atributos específicos de esa persona:
#    - headWoman: si la cabeza del hogar es mujer
#    - cat_educHead: educación de la cabeza del hogar
#    - occupiedHead: si la cabeza del hogar está ocupada
#
#    Luego une esos datos con el resumen agregado del hogar.
#
# ¿Qué devuelve?
#   Una lista con:
#   - personas_nivel_hogar
#   - personas_hogar
#
# Nota importante:
#   personas_hogar es el objeto que normalmente se une después
#   con la base de hogares, porque ya contiene tanto variables
#   agregadas como variables de la persona cabeza del hogar.
# ------------------------------------------------------------
build_personas_hogar <- function(personas) {
  personas_nivel_hogar <- personas %>%
    group_by(id) %>%
    summarize(
      num_women    = sum(woman, na.rm = TRUE),
      num_minors   = sum(minor, na.rm = TRUE),
      cat_maxEduc  = max(cat_educ, na.rm = TRUE),
      num_occupied = sum(occupied, na.rm = TRUE),
      .groups = "drop"
    )
  
  personas_hogar <- personas %>%
    filter(head == 1) %>%
    dplyr::select(id, woman, cat_educ, occupied) %>%
    rename(
      headWoman    = woman,
      cat_educHead = cat_educ,
      occupiedHead = occupied
    ) %>%
    left_join(personas_nivel_hogar, by = "id")
  
  list(
    personas_nivel_hogar = personas_nivel_hogar,
    personas_hogar = personas_hogar
  )
}

# ------------------------------------------------------------
# prepare_hogares()
# ------------------------------------------------------------
# Objetivo:
#   Limpiar y dejar lista la base de hogares con solo las
#   variables que se van a usar en el modelo.
#
# Entrada:
#   - hogares: data frame de hogares
#   - is_train: TRUE si es la base de entrenamiento,
#               FALSE si es la base de prueba
#
# Variable nueva:
#   - rent:
#       1 si P5090 == 3, 0 en otro caso.
#       Esta variable recodifica una característica del hogar
#       que será usada como predictor.
#
# Diferencia entre train y test:
#   - En train sí existe la variable respuesta Pobre, porque
#     esa es la etiqueta que queremos aprender.
#   - En test no debemos incluir Pobre, porque justamente eso
#     es lo que vamos a predecir.
#
# ¿Qué devuelve?
#   Un data frame reducido:
#   - train: id, Dominio, rent, Pobre
#   - test : id, Dominio, rent
# ------------------------------------------------------------
prepare_hogares <- function(hogares, is_train = TRUE) {
  hogares %>%
    mutate(rent = ifelse(P5090 == 3, 1, 0)) %>%
    {
      if (is_train) {
        dplyr::select(., id, Dominio, rent, Pobre)
      } else {
        dplyr::select(., id, Dominio, rent)
      }
    }
}

# ------------------------------------------------------------
# convert_factors()
# ------------------------------------------------------------
# Objetivo:
#   Convertir variables categóricas al tipo factor para que el
#   modelo las interprete correctamente.
#
# Entrada:
#   - data: base final ya construida
#   - is_train: indica si estamos trabajando con entrenamiento
#
# Transformaciones:
#   1) Dominio -> factor
#      Porque representa categorías y no una magnitud numérica.
#
#   2) cat_educHead -> factor con etiquetas legibles
#      Se reemplazan los códigos numéricos por nombres de nivel
#      educativo para que el modelo y la interpretación sean más
#      claros.
#
#   3) Pobre -> factor binario con niveles "No" y "Yes"
#      Esto solo se hace en train porque es la variable objetivo.
#      Además, caret trabaja mejor con clasificación binaria si
#      la respuesta está en formato factor.
#
# ¿Qué devuelve?
#   El mismo data frame, pero con variables correctamente
#   codificadas como factores.
# ------------------------------------------------------------
convert_factors <- function(data, is_train = TRUE) {
  data <- data %>%
    mutate(
      Dominio = factor(Dominio),
      cat_educHead = factor(
        cat_educHead,
        levels = 0:6,
        labels = c(
          "No sabe", "Ninguno", "Preescolar",
          "Primaria", "Secundaria", "Media", "Universitaria"
        )
      )
    )
  
  if (is_train) {
    data <- data %>%
      mutate(
        Pobre = factor(Pobre, levels = c(0, 1), labels = c("No", "Yes"))
      )
  }
  
  data
}

# ------------------------------------------------------------
# prepare_train_test_factors()
# ------------------------------------------------------------
# Objetivo:
#   Aplicar de forma consistente la conversión de factores en
#   train y test, asegurando que test use los mismos niveles
#   de Dominio y cat_educHead que train.
#
# Entrada:
#   - train: base de entrenamiento
#   - test : base de prueba
#
# Salida:
#   Una lista con:
#   - train
#   - test
# ------------------------------------------------------------
prepare_train_test_factors <- function(train, test) {
  
  cat_educ_labels <- c(
    "0" = "No sabe",
    "1" = "Ninguno",
    "2" = "Preescolar",
    "3" = "Primaria",
    "4" = "Secundaria",
    "5" = "Media",
    "6" = "Universitaria"
  )
  
  train <- train %>%
    mutate(
      Pobre = factor(Pobre, levels = c("Yes", "No")),
      Dominio = factor(Dominio),
      cat_educHead = case_when(
        cat_educHead %in% c("No sabe", "Preescolar") ~ "Ninguno",
        TRUE ~ cat_educHead
      ),
      cat_educHead = factor(
        cat_educHead,
        levels = c("Ninguno", "Primaria", "Secundaria", "Media", "Universitaria")
      )
    )
  
  test <- test %>%
    mutate(
      Dominio = factor(Dominio, levels = levels(train$Dominio)),
      cat_educHead = recode(as.character(cat_educHead), !!!cat_educ_labels),
      cat_educHead = case_when(
        cat_educHead %in% c("No sabe", "Preescolar") ~ "Ninguno",
        TRUE ~ cat_educHead
      ),
      cat_educHead = factor(cat_educHead, levels = levels(train$cat_educHead))
    )
  
  list(
    train = train,
    test = test
  )
}


# ------------------------------------------------------------
# build_features_hogar()
# ------------------------------------------------------------
# Objetivo:
#   Construir ocho variables socioeconómicas a nivel hogar
#   combinando la base cruda de personas y la de hogares.
#
# Entradas:
#   - personas_raw : data frame crudo de personas del GEIH.
#   - hogares_raw  : data frame crudo de hogares del GEIH.
#
# Variables que crea:
#
#   Desde personas_raw
#   ------------------
#   1) prop_dependiente
#       (No PET + Inactivos en PET) / Total del hogar.
#       PET = Población en Edad de Trabajar (≥12 años, DANE).
#       No PET: menores de 12 años.
#       Inactivos en PET: en edad de trabajar pero fuera de la
#       PEA (ni ocupados ni desocupados).
#       Si Des no existe en los datos los inactivos se aproximan
#       como PET − Ocupados.
#
#   2) prop_informal
#       Ocupados informales / Total ocupados del hogar.
#       Informal = ocupado que no cotiza a pensión (P6920 != 1).
#       Si no hay ocupados en el hogar se asigna 0.
#
#   3) mujer_jefe_ocup
#       Dummy: 1 si la cabeza del hogar es mujer y está ocupada,
#       0 en caso contrario.
#
#   4) prop_educ_alta
#       Personas con educación superior o universitaria (P6210 == 6)
#       divididas entre el total del hogar.
#
#   5) avg_educ_adultos
#       Promedio de cat_educ entre adultos (≥18 años) del hogar.
#       Captura el capital humano promedio, complementando el
#       máximo educativo y la proporción de alta educación.
#       Si el hogar no tiene adultos se asigna 0.
#
#   6) tasa_desempleo_hogar
#       Desocupados / (Ocupados + Desocupados) del hogar.
#       Mide la presión de desempleo activo dentro de la PEA
#       del hogar. Si Des no existe o la PEA es 0, se asigna 0.
#
#   Desde hogares_raw
#   -----------------
#   7) hacinamiento
#       Total de personas del hogar / Cuartos para dormir (P5010).
#       Proxy de condiciones de vivienda. Si P5010 no existe se
#       intenta con P5000 (total de cuartos).
#       Valores con 0 cuartos se tratan como NA.
#
#   8) n_servicios
#       Suma de servicios públicos disponibles (0–5):
#       acueducto (P4030S1), alcantarillado (P4030S2),
#       gas natural (P4030S3), energía eléctrica (P4030S4) y
#       recolección de basuras (P4030S5).
#       Cada servicio vale 1 si P4030Sx == 1, 0 en caso contrario.
#       Si ninguna de estas columnas existe en los datos (algunas
#       versiones del GEIH no las incluyen), n_servicios = NA.
#
# ¿Qué devuelve?
#   Un data frame con una fila por hogar (id) y las ocho variables.
# ------------------------------------------------------------
build_features_hogar <- function(personas_raw, hogares_raw) {

  # ----------------------------------------------------------
  # Parte 1: variables derivadas de personas
  # ----------------------------------------------------------

  # Verificar si existe variable de desocupados en los datos
  tiene_des <- "Des" %in% names(personas_raw)

  features_personas <- personas_raw %>%
    mutate(
      # PET: ≥12 años (definición DANE para módulos de trabajo)
      in_PET       = P6040 >= 12,

      # Ocupado: Oc tiene valor (no es NA)
      is_oc        = !is.na(Oc),

      # Desocupado: Des tiene valor, solo si la columna existe
      is_des       = if (tiene_des) !is.na(Des) else FALSE,

      # Dependiente: menor de 12 años O en PET pero fuera de la PEA
      is_dep       = !in_PET | (in_PET & !is_oc & !is_des),

      # Informal: ocupado sin cotización a pensión
      # P6920 == 1 -> cotiza; P6920 == 2 o NA -> no cotiza (informal)
      is_informal  = is_oc & (is.na(P6920) | P6920 != 1),

      # Cabeza del hogar
      is_head      = P6050 == 1,

      # Mujer
      is_woman     = P6020 == 2,

      # Nivel educativo ajustado (9 = no sabe -> 0)
      cat_educ_raw = ifelse(P6210 == 9, 0, P6210),

      # Educación alta: superior o universitaria (código 6 en P6210)
      is_educ_alta = cat_educ_raw == 6,

      # Adulto: ≥18 años (para promedio educativo)
      is_adult     = P6040 >= 18
    ) %>%
    group_by(id) %>%
    summarize(
      total_hogar      = n(),
      n_dependientes   = sum(is_dep,       na.rm = TRUE),
      n_ocupados       = sum(is_oc,        na.rm = TRUE),
      n_desocupados    = sum(is_des,       na.rm = TRUE),
      n_informales     = sum(is_informal,  na.rm = TRUE),
      n_educ_alta      = sum(is_educ_alta, na.rm = TRUE),
      mujer_jefe_ocup  = as.integer(any(is_head & is_woman & is_oc,
                                        na.rm = TRUE)),
      # Promedio educativo solo entre adultos del hogar
      avg_educ_adultos = mean(cat_educ_raw[is_adult == TRUE], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      prop_dependiente     = n_dependientes / total_hogar,
      prop_informal        = ifelse(n_ocupados > 0,
                                    n_informales / n_ocupados, 0),
      prop_educ_alta       = n_educ_alta / total_hogar,
      tasa_desempleo_hogar = ifelse(
        (n_ocupados + n_desocupados) > 0,
        n_desocupados / (n_ocupados + n_desocupados), 0
      ),
      # NaN aparece cuando no hay adultos en el hogar -> reemplazar por 0
      avg_educ_adultos     = ifelse(is.nan(avg_educ_adultos),
                                    0, avg_educ_adultos)
    )

  # ----------------------------------------------------------
  # Parte 2: variables derivadas de hogares
  # ----------------------------------------------------------

  # Variable de cuartos: usar P5010 (dormir) si existe, si no P5000
  tiene_p5010 <- "P5010" %in% names(hogares_raw)
  var_cuartos <- if (tiene_p5010) "P5010" else "P5000"

  # Detectar qué columnas de servicios públicos están disponibles.
  # En algunas versiones del GEIH se llaman P4030S1…P4030S5; en
  # otras no existen. Calculamos n_servicios solo con las que haya.
  cols_servicios <- paste0("P4030S", 1:5)
  cols_disponibles <- intersect(cols_servicios, names(hogares_raw))

  features_hogares <- hogares_raw %>%
    mutate(
      # Número de cuartos para dormir (0 se trata como NA)
      cuartos = ifelse(.data[[var_cuartos]] > 0,
                       .data[[var_cuartos]], NA_real_)
    ) %>%
    # Servicios públicos: suma de los servicios disponibles (0–5).
    # Usamos across() para operar solo sobre las columnas que existen.
    # Si ninguna columna P4030Sx está en los datos, n_servicios = NA.
    { if (length(cols_disponibles) == 0) {
        mutate(., n_servicios = NA_integer_)
      } else {
        mutate(.,
          n_servicios = rowSums(
            across(all_of(cols_disponibles), ~ as.integer(!is.na(.x) & .x == 1)),
            na.rm = TRUE
          )
        )
      }
    } %>%
    dplyr::select(id, cuartos, n_servicios)

  # ----------------------------------------------------------
  # Parte 3: combinar y calcular hacinamiento
  # ----------------------------------------------------------

  df <- features_personas %>%
    left_join(features_hogares, by = "id") %>%
    mutate(
      hacinamiento = total_hogar / cuartos
    ) %>%
    dplyr::select(id, prop_dependiente, prop_informal, mujer_jefe_ocup,
                  prop_educ_alta, avg_educ_adultos, tasa_desempleo_hogar,
                  hacinamiento, n_servicios)

  return(df)
}


# ------------------------------------------------------------
# build_nuevas_hogares()
# ------------------------------------------------------------
# Objetivo:
#   Construir variables adicionales a nivel hogar directamente
#   desde la base cruda de hogares, capturando dimensiones de
#   pobreza multidimensional que el primer bloque de features
#   no cubría.
#
# Entrada:
#   - hogares_raw: data frame crudo de hogares del GEIH.
#
# Variables que crea:
#   - zona_rural        : 1 si Clase != 1 (fuera de cabecera
#                         municipal). Las tasas de pobreza rurales
#                         son históricamente 3-4x las urbanas.
#   - vivienda_precaria : 1 si P5100 no es casa (1) ni apartamento
#                         (2): cuartos, viviendas indígenas, tiendas,
#                         carpas, etc. Dimensión del IPM.
#   - sin_agua_red      : 1 si P5130 != 1 (sin acueducto público o
#                         privado). Proxy de acceso a servicios.
#   - sin_sanitario     : 1 si P5140 != 1 (sin inodoro conectado a
#                         alcantarillado). Dimensión del IPM.
#   - cuartos_por_persona: P5000 / Nper. Más cuartos por persona
#                         indica mejores condiciones. Complementa
#                         hacinamiento desde la base de personas.
#
# ¿Qué devuelve?
#   Un data frame con una fila por hogar (id) y las cinco variables.
# ------------------------------------------------------------
build_nuevas_hogares <- function(hogares_raw) {
  hogares_raw %>%
    mutate(
      # Zona rural: cabecera = 1; centro poblado = 2; rural = 3
      zona_rural = as.integer(!is.na(Clase) & Clase != 1),

      # Vivienda precaria: no es casa (1) ni apartamento (2)
      vivienda_precaria = as.integer(!is.na(P5100) & !P5100 %in% c(1, 2)),

      # Sin acueducto: P5130 == 1 → acueducto público o comunitario
      sin_agua_red = as.integer(!is.na(P5130) & P5130 != 1),

      # Sin sanitario conectado: P5140 == 1 → inodoro con alcantarillado
      sin_sanitario = as.integer(!is.na(P5140) & P5140 != 1),

      # Cuartos totales por persona (0 cuartos → NA)
      cuartos_por_persona = ifelse(
        !is.na(P5000) & P5000 > 0 & !is.na(Nper) & Nper > 0,
        P5000 / Nper,
        NA_real_
      )
    ) %>%
    mutate(
      # Línea de pobreza (Lp) y extrema pobreza (Li) del hogar.
      # Reflejan composición del hogar y región → legítimas como features:
      # NO revelan ingreso, solo el umbral que define pobreza.
      lp_hogar = if ("Lp" %in% names(.)) as.numeric(Lp) else NA_real_,
      li_hogar = if ("Li" %in% names(.)) as.numeric(Li) else NA_real_,

      # Líneas per cápita: mejor comparables entre hogares de
      # distintos tamaños. Lp_pc baja implica hogar grande en zona barata.
      lp_pc = ifelse(
        !is.na(lp_hogar) & !is.na(Nper) & Nper > 0,
        lp_hogar / Nper, NA_real_
      ),
      li_pc = ifelse(
        !is.na(li_hogar) & !is.na(Nper) & Nper > 0,
        li_hogar / Nper, NA_real_
      ),

      # Tamaño del hogar "unidad de gasto": concepto distinto de Nper
      # (personas_unidad_gasto solo incluye miembros que comparten gastos).
      # Útil como denominador y como proxy de estructura familiar.
      npersug = if ("Npersug" %in% names(.)) as.numeric(Npersug) else NA_real_,

      # Diferencia Nper - Npersug: personas en el hogar que NO comparten
      # gastos (empleadas domésticas internas, pensionistas, etc.)
      nper_vs_ug = ifelse(
        !is.na(Nper) & !is.na(npersug),
        Nper - npersug, NA_real_
      )
    ) %>%
    dplyr::select(id, zona_rural, vivienda_precaria, sin_agua_red,
                  sin_sanitario, cuartos_por_persona,
                  lp_hogar, li_hogar, lp_pc, li_pc,
                  npersug, nper_vs_ug)
}


# ------------------------------------------------------------
# build_nuevas_personas()
# ------------------------------------------------------------
# Objetivo:
#   Construir variables adicionales a nivel hogar desde la base
#   de personas, centradas en el perfil del jefe del hogar y en
#   fuentes de ingreso no laborales.
#
# Entrada:
#   - personas_raw: data frame crudo de personas del GEIH.
#
# Variables que crea:
#   - jefe_edad         : edad del jefe del hogar (P6040).
#                         Hogares con jefes jóvenes o muy mayores
#                         tienen mayor riesgo de pobreza.
#   - jefe_cuenta_propia: 1 si el jefe es trabajador por cuenta
#                         propia (P6430 == 4). Ingreso más volátil
#                         e informal que el empleo dependiente.
#   - jefe_anos_educ    : años de educación del jefe (P6210s1).
#                         Complementa el nivel educativo categórico:
#                         captura heterogeneidad dentro de cada nivel.
#   - jefe_sin_pension  : 1 si el jefe está ocupado pero no cotiza
#                         a pensión (P6920 != 1). Indicador directo
#                         de informalidad del principal proveedor.
#   - prop_subsidiado   : proporción de miembros afiliados al Régimen
#                         Subsidiado de salud (P6090 == 3). El Estado
#                         subsidia a quienes no pueden costear el
#                         contributivo → proxy directo de pobreza.
#   - tiene_remesas     : 1 si algún miembro recibe remesas del
#                         exterior (P6620 == 1 o P7510s3 == 1).
#                         Las remesas pueden sacar hogares de la línea
#                         de pobreza.
#   - tiene_pension_ing : 1 si algún miembro recibe ingreso de pensión
#                         (P6610 == 1). Ingreso no laboral estable.
#   - prop_subempleado  : proporción de ocupados con subempleo por
#                         ingresos insuficientes (P7422 == 1) o por
#                         insuficiencia de horas (P7472 == 1).
#                         Captura precariedad laboral más allá del
#                         desempleo abierto.
#
# ¿Qué devuelve?
#   Un data frame con una fila por hogar (id) y las ocho variables.
# ------------------------------------------------------------
build_nuevas_personas <- function(personas_raw) {

  # ------------------------------------------------------------
  # Helpers defensivos (robustos ante columnas faltantes)
  # ------------------------------------------------------------
  # eq1_si_existe(): vector lógico TRUE donde la columna == 1.
  # Si la columna no existe, devuelve FALSE del tamaño correcto.
  eq1_si_existe <- function(col_name) {
    if (col_name %in% names(personas_raw)) {
      col <- personas_raw[[col_name]]
      !is.na(col) & col == 1
    } else {
      rep(FALSE, nrow(personas_raw))
    }
  }

  # num_si_existe(): columna numérica si existe, si no NA_real_.
  num_si_existe <- function(col_name) {
    if (col_name %in% names(personas_raw)) {
      suppressWarnings(as.numeric(personas_raw[[col_name]]))
    } else {
      rep(NA_real_, nrow(personas_raw))
    }
  }

  # n_cols_eq1(): para una familia de columnas "<prefix><sufijo>",
  # cuenta cuántas están == 1 por fila (0 si ninguna existe).
  n_cols_eq1 <- function(prefix, sufijos) {
    cols <- paste0(prefix, sufijos)
    cols <- intersect(cols, names(personas_raw))
    if (length(cols) == 0) return(rep(0L, nrow(personas_raw)))
    mat <- as.matrix(personas_raw[, cols, drop = FALSE])
    suppressWarnings(storage.mode(mat) <- "numeric")
    rowSums(mat == 1 & !is.na(mat))
  }

  # safe_max / safe_sd: versiones robustas que devuelven NA_real_
  # en lugar de -Inf o warning cuando todos los valores son NA.
  safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
  safe_sd  <- function(x) if (sum(!is.na(x)) > 1) sd(x, na.rm = TRUE) else NA_real_

  # ------------------------------------------------------------
  # Precomputar vectores de columnas variables
  # ------------------------------------------------------------
  remesas_p6620   <- eq1_si_existe("P6620")
  remesas_p7510s3 <- eq1_si_existe("P7510s3")

  # Primas anuales P6630s1..s4, s6 (s5 no existe en GEIH) — conteo por persona
  n_primas_persona <- n_cols_eq1("P6630s", c(1:4, 6))

  # Transferencias y ayudas P7510s1..s7 — conteo por persona
  n_ayudas_persona <- n_cols_eq1("P7510s", 1:7)

  # Otros transfers sueltos
  t_p7500s2 <- eq1_si_existe("P7500s2")
  t_p7500s3 <- eq1_si_existe("P7500s3")
  t_p7505   <- eq1_si_existe("P7505")

  # Subsidios mensuales P6585s1..s4
  sub_transp <- eq1_si_existe("P6585s1")
  sub_alim   <- eq1_si_existe("P6585s2")
  sub_fam    <- eq1_si_existe("P6585s3")
  sub_educ   <- eq1_si_existe("P6585s4")

  # Numéricas laborales
  hrs_trabaja     <- num_si_existe("P6800")  # horas semanales trabajo principal
  tam_empresa     <- num_si_existe("P6870")  # tamaño empresa (cat ordinal 1-9)
  meses_trabajo   <- num_si_existe("P6426")  # meses en trabajo actual
  hrs_seg_trabajo <- num_si_existe("P7090")  # horas segundo trabajo

  # Segundo trabajo
  seg_trabajo <- eq1_si_existe("P7040")

  # Demográficos
  es_mujer <- if ("P6020" %in% names(personas_raw)) {
    !is.na(personas_raw$P6020) & personas_raw$P6020 == 2
  } else rep(FALSE, nrow(personas_raw))

  # P6100: pertenencia étnica (1=indígena, 2=raizal, 3=palenquero, 4=negro,
  # 5=mestizo, 6=ninguna). Agrupamos como "minoría étnica" 1-4.
  es_minoria <- if ("P6100" %in% names(personas_raw)) {
    !is.na(personas_raw$P6100) & personas_raw$P6100 %in% 1:4
  } else rep(FALSE, nrow(personas_raw))

  # ------------------------------------------------------------
  # Construcción de features a nivel hogar
  # ------------------------------------------------------------
  personas_raw %>%
    mutate(
      is_head        = !is.na(P6050) & P6050 == 1,
      is_spouse      = !is.na(P6050) & P6050 == 2,
      is_ocupado     = !is.na(Oc),

      # Base original
      cuenta_propia  = !is.na(P6430) & P6430 == 4,
      anos_educ      = suppressWarnings(as.numeric(P6210s1)),
      sin_pension_oc = is_ocupado & (is.na(P6920) | P6920 != 1),
      subsidiado     = !is.na(P6090) & P6090 == 3,
      remesas        = remesas_p6620 | remesas_p7510s3,
      pension_ing    = eq1_si_existe("P6610"),
      subempleado    = is_ocupado & (
                         (!is.na(P7422) & P7422 == 1) |
                         (!is.na(P7472) & P7472 == 1)
                       ),

      # Fuentes de ingreso
      ing_trabajo_sec  = eq1_si_existe("P6510"),
      ing_agropecuario = eq1_si_existe("P6545"),
      ing_arriendo     = eq1_si_existe("P6580"),
      ing_jubilacion   = eq1_si_existe("P6600"),

      # Subsidios mensuales / anuales / ayudas (vectores precomputados)
      sub_transp_v = sub_transp,
      sub_alim_v   = sub_alim,
      sub_fam_v    = sub_fam,
      sub_educ_v   = sub_educ,
      n_primas_v   = n_primas_persona,
      n_ayudas_v   = n_ayudas_persona,
      t_p7500s2_v  = t_p7500s2,
      t_p7500s3_v  = t_p7500s3,
      t_p7505_v    = t_p7505,

      # Numéricas laborales
      hrs_v       = hrs_trabaja,
      emp_v       = tam_empresa,
      meses_v     = meses_trabajo,
      hrs2_v      = hrs_seg_trabajo,
      seg_v       = seg_trabajo,

      # Demográficos
      mujer_v     = es_mujer,
      minoria_v   = es_minoria
    ) %>%
    group_by(id) %>%
    summarize(
      # === Perfil del jefe ===
      jefe_edad          = dplyr::first(P6040[is_head],     default = NA_real_),
      jefe_cuenta_propia = as.integer(any(is_head & cuenta_propia,  na.rm = TRUE)),
      jefe_anos_educ     = dplyr::first(anos_educ[is_head], default = NA_real_),
      jefe_sin_pension   = as.integer(any(is_head & sin_pension_oc, na.rm = TRUE)),
      jefe_horas         = dplyr::first(hrs_v[is_head],     default = NA_real_),
      jefe_tam_empresa   = dplyr::first(emp_v[is_head],     default = NA_real_),
      jefe_meses_trabajo = dplyr::first(meses_v[is_head],   default = NA_real_),
      jefe_seg_trabajo   = as.integer(any(is_head & seg_v,  na.rm = TRUE)),
      jefe_mujer         = as.integer(any(is_head & mujer_v, na.rm = TRUE)),

      # === Cónyuge ===
      conyuge_existe     = as.integer(any(is_spouse, na.rm = TRUE)),
      conyuge_ocupado    = as.integer(any(is_spouse & is_ocupado, na.rm = TRUE)),
      conyuge_horas      = ifelse(any(is_spouse, na.rm = TRUE),
                                  dplyr::first(hrs_v[is_spouse], default = NA_real_),
                                  NA_real_),
      conyuge_anos_educ  = ifelse(any(is_spouse, na.rm = TRUE),
                                  dplyr::first(anos_educ[is_spouse], default = NA_real_),
                                  NA_real_),

      # === Características del hogar (existentes) ===
      prop_subsidiado    = mean(subsidiado, na.rm = TRUE),
      tiene_remesas      = as.integer(any(remesas,     na.rm = TRUE)),
      tiene_pension_ing  = as.integer(any(pension_ing, na.rm = TRUE)),
      prop_subempleado   = ifelse(
        sum(is_ocupado, na.rm = TRUE) > 0,
        sum(subempleado, na.rm = TRUE) / sum(is_ocupado, na.rm = TRUE),
        NA_real_
      ),

      # === Diversificación de ingresos (4 fuentes laborales básicas) ===
      n_fuentes_ingreso  = as.integer(
        any(ing_trabajo_sec,  na.rm = TRUE) +
        any(ing_agropecuario, na.rm = TRUE) +
        any(ing_arriendo,     na.rm = TRUE) +
        any(ing_jubilacion,   na.rm = TRUE)
      ),
      prop_trabajo_sec   = mean(ing_trabajo_sec, na.rm = TRUE),

      # === NUEVO: Subsidios mensuales (P6585s1-s4) ===
      prop_sub_transp    = mean(sub_transp_v, na.rm = TRUE),
      prop_sub_alim      = mean(sub_alim_v,   na.rm = TRUE),
      prop_sub_fam       = mean(sub_fam_v,    na.rm = TRUE),
      prop_sub_educ      = mean(sub_educ_v,   na.rm = TRUE),
      any_sub_mensual    = as.integer(
        any(sub_transp_v | sub_alim_v | sub_fam_v | sub_educ_v, na.rm = TRUE)
      ),

      # === NUEVO: Primas anuales (P6630s1-s6) ===
      total_primas_hogar = sum(n_primas_v,  na.rm = TRUE),
      max_primas_persona = safe_max(n_primas_v),

      # === NUEVO: Transferencias y ayudas detalladas ===
      total_ayudas_hogar = sum(n_ayudas_v,  na.rm = TRUE),
      prop_p7500s2       = mean(t_p7500s2_v, na.rm = TRUE),
      prop_p7500s3       = mean(t_p7500s3_v, na.rm = TRUE),
      prop_p7505         = mean(t_p7505_v,   na.rm = TRUE),

      # === NUEVO: Horas trabajadas en el hogar (principal) ===
      horas_mean_hog     = mean(hrs_v,    na.rm = TRUE),
      horas_max_hog      = safe_max(hrs_v),
      horas_sd_hog       = safe_sd(hrs_v),
      total_horas_hog    = sum(hrs_v,     na.rm = TRUE),

      # === NUEVO: Tamaño de empresa ===
      empresa_max_hog    = safe_max(emp_v),
      empresa_mean_hog   = mean(emp_v, na.rm = TRUE),

      # === NUEVO: Estabilidad laboral ===
      meses_trabajo_mean_hog = mean(meses_v, na.rm = TRUE),
      meses_trabajo_max_hog  = safe_max(meses_v),

      # === NUEVO: Segundo trabajo ===
      prop_seg_trabajo   = mean(seg_v, na.rm = TRUE),
      horas_seg_trabajo_mean = mean(hrs2_v, na.rm = TRUE),

      # === NUEVO: Demográficos del hogar ===
      prop_mujeres       = mean(mujer_v,   na.rm = TRUE),
      prop_minoria_etn   = mean(minoria_v, na.rm = TRUE),

      .groups = "drop"
    )
}


# ------------------------------------------------------------
# multiStats()
# ------------------------------------------------------------
# Objetivo:
#   Calcular varias métricas de clasificación binaria al mismo
#   tiempo durante el proceso de validación cruzada en caret.
#
# ¿Por qué sirve?
#   Porque en lugar de ver solo una métrica, permite revisar
#   simultáneamente desempeño desde distintos enfoques:
#   - twoClassSummary: ROC, Sens, Spec
#   - defaultSummary : Accuracy, Kappa
#   - prSummary      : Precision, Recall, F
#
# Requisito importante:
#   Para que funcione correctamente dentro de trainControl(),
#   se debe usar:
#   - classProbs = TRUE
#   - summaryFunction = multiStats
#
# Además:
#   La variable objetivo debe ser un factor de dos clases.
# ------------------------------------------------------------
multiStats <- function(...) {
  c(
    caret:::twoClassSummary(...),
    caret:::defaultSummary(...),
    caret:::prSummary(...)
  )
}

# ------------------------------------------------------------
# make_submission_name()
# ------------------------------------------------------------
# Objetivo:
#   Construir automáticamente el nombre del archivo de envío
#   según el algoritmo o variante del modelo utilizado.
#
# ¿Por qué sirve?
#   Porque permite guardar cada predicción con un nombre
#   informativo y consistente, incorporando los principales
#   hiperparámetros del modelo (por ejemplo alpha, lambda y,
#   cuando aplica, el threshold óptimo). Esto ayuda a:
#   - diferenciar archivos de distintos modelos,
#   - evitar sobreescrituras accidentales,
#   - rastrear fácilmente con qué configuración se generó cada
#     submission.
#
# Entrada:
#   - best_algorithm:
#       string que identifica el tipo de modelo o estrategia.
#       Ejemplos:
#       "Elastic Net con Accuracy"
#       "Elastic Net con Sens"
#       "Elastic Net weighted + threshold óptimo PR"
#       "LDA"
#       "QDA"
#
#   - model:
#       objeto entrenado con caret, necesario para extraer
#       bestTune$alpha y bestTune$lambda en los modelos que usan
#       tuning.
#
#   - best_cutoff:
#       data frame o lista con el threshold óptimo, usado solo en
#       estrategias donde además del modelo se optimiza el punto
#       de corte de clasificación.
#
# ¿Qué hace?
#   1) Formatea los hiperparámetros numéricos para que puedan
#      usarse en nombres de archivo (reemplaza "." por "_").
#   2) Revisa qué tipo de algoritmo se está guardando.
#   3) Construye un nombre de archivo coherente para esa
#      estrategia.
#
# ¿Qué devuelve?
#   Un string con el nombre final del archivo .csv.
#
# Ejemplos de salida:
#   EN_Acc_lambda_0_001_alpha_0_5.csv
#   EN_Sen_lambda_0_1_alpha_1.csv
#   EN_weighted_PR_lambda_0_0316_alpha_0_7_threshold_0_242.csv
#   LDA.csv
#   QDA.csv
# ------------------------------------------------------------
make_submission_name <- function(best_algorithm, model = NULL, best_cutoff = NULL) {
  
  format_param <- function(x, digits = 4) {
    gsub("\\.", "_", as.character(round(x, digits)))
  }
  
  if (best_algorithm == "Elastic Net con Accuracy") {
    
    lambda_str <- format_param(model$bestTune$lambda, 4)
    alpha_str  <- gsub("\\.", "_", as.character(model$bestTune$alpha))
    
    submission_name <- paste0(
      "EN_Acc_lambda_", lambda_str,
      "_alpha_", alpha_str,
      ".csv"
    )
    
  } else if (best_algorithm == "Elastic Net con Sens") {
    
    lambda_str <- format_param(model$bestTune$lambda, 4)
    alpha_str  <- gsub("\\.", "_", as.character(model$bestTune$alpha))
    
    submission_name <- paste0(
      "EN_Sen_lambda_", lambda_str,
      "_alpha_", alpha_str,
      ".csv"
    )
    
  } else if (best_algorithm == "Elastic Net weighted + threshold óptimo PR") {
    
    lambda_str    <- format_param(model$bestTune$lambda, 4)
    alpha_str     <- gsub("\\.", "_", as.character(model$bestTune$alpha))
    threshold_str <- format_param(best_cutoff$threshold, 3)
    
    submission_name <- paste0(
      "EN_weighted_PR_lambda_", lambda_str,
      "_alpha_", alpha_str,
      "_threshold_", threshold_str,
      ".csv"
    )
    
  } else if (best_algorithm == "Elastic Net + threshold óptimo PR") {
    
    lambda_str    <- format_param(model$bestTune$lambda, 4)
    alpha_str     <- gsub("\\.", "_", as.character(model$bestTune$alpha))
    threshold_str <- format_param(best_cutoff$threshold, 3)
    
    submission_name <- paste0(
      "EN_PR_lambda_", lambda_str,
      "_alpha_", alpha_str,
      "_threshold_", threshold_str,
      ".csv"
    )
    
  } else if (best_algorithm == "Elastic Net con F1") {
    
    lambda_str <- format_param(model$bestTune$lambda, 4)
    alpha_str  <- gsub("\\.", "_", as.character(model$bestTune$alpha))
    
    submission_name <- paste0(
      "EN_F1_lambda_", lambda_str,
      "_alpha_", alpha_str,
      ".csv"
    )
    
  } else if (best_algorithm == "Elastic Net + sample weighting") {
    
    lambda_str <- format_param(model$bestTune$lambda, 4)
    alpha_str  <- gsub("\\.", "_", as.character(model$bestTune$alpha))
    
    submission_name <- paste0(
      "EN_weighted_lambda_", lambda_str,
      "_alpha_", alpha_str,
      ".csv"
    )
    
  } else if (best_algorithm == "Elastic Net + downsampling") {
    
    lambda_str <- format_param(model$bestTune$lambda, 4)
    alpha_str  <- gsub("\\.", "_", as.character(model$bestTune$alpha))
    
    submission_name <- paste0(
      "EN_down_lambda_", lambda_str,
      "_alpha_", alpha_str,
      ".csv"
    )
    
  } else if (best_algorithm == "Elastic Net + upsampling") {
    
    lambda_str <- format_param(model$bestTune$lambda, 4)
    alpha_str  <- gsub("\\.", "_", as.character(model$bestTune$alpha))
    
    submission_name <- paste0(
      "EN_up_lambda_", lambda_str,
      "_alpha_", alpha_str,
      ".csv"
    )
    
  } else if (best_algorithm == "LDA") {
    
    submission_name <- "LDA.csv"
    
  } else if (best_algorithm == "QDA") {
    
    submission_name <- "QDA.csv"
    
  } else {
    stop("Algoritmo no reconocido en make_submission_name().")
  }
  
  return(submission_name)
}