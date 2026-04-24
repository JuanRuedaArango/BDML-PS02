# ============================================================
# ANÁLISIS EXPLORATORIO: gráficos descriptivos de pobreza
# ============================================================
#
# Objetivo:
# Visualizar cómo cambia la proporción de pobreza según:
#   - Variables categóricas
#   - Variables binarias
#   - Variables continuas
#
# Tipo de gráficos:
#   1) Barras apiladas proporcionales
#   2) Densidades comparadas
#
# Base usada:
#   train
# ============================================================


# ============================================================
# 0. LIBRERÍAS
# ============================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(gt)
library(scales)
library(forcats)

path_plots <- "02_outputs/figures"

dir.create(path_plots, recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 1. COLORES
# ============================================================
colores_pobreza <- c(
  "No"  = "#51a881",
  "Yes" = "#a75177"
)

# ============================================================
# 2. VARIABLES CONTINUAS (DENSIDADES)
# ============================================================

#------------------------------------------------------------
# Variables seleccionadas
#------------------------------------------------------------
vars_num <- c(
  "prop_dependiente",
  "prop_informal",
  "prop_educ_alta",
  "avg_educ_adultos",
  "hacinamiento",
  "jefe_edad",
  "persons_per_worker",
  "minors_per_worker"
)

# Filtra a las que sí existen en train (evita fallar si el FE cambió)
vars_num <- intersect(vars_num, names(train))

labels_vars <- c(
  prop_dependiente = "Tasa de dependencia",
  prop_informal = "Tasa de informalidad",
  prop_educ_alta = "Proporción personas con educación alta",
  avg_educ_adultos = "Promedio educación en adultos (años)",
  hacinamiento = "Porcentaje de hacinamiento",
  cuartos_por_persona = "Cuartos por persona",
  jefe_edad = "Edad jefe",
  n_personas = "Tamaño hogar",
  persons_per_worker = "Personas por ocupado",
  minors_per_worker = "Menores por ocupado"
)

#------------------------------------------------------------
# Transformación
#------------------------------------------------------------
dens_df <- train %>%
  select(Pobre, all_of(vars_num)) %>%
  pivot_longer(
    cols = -Pobre,
    names_to = "Variable",
    values_to = "Valor"
  )

#------------------------------------------------------------
# Gráfico
#------------------------------------------------------------
lims_vars <- list(
  hacinamiento = c(0, 8),
  persons_per_worker = c(0, 5),
  minors_per_worker = c(0, 4)
)

dens_df$lim_x <- dens_df$Variable

dens_df_plot <- dens_df %>%
  mutate(Valor_filtrado = case_when(
    Variable == "hacinamiento" ~ ifelse(Valor <= 8, Valor, NA),
    Variable == "persons_per_worker" ~ ifelse(Valor <= 5, Valor, NA),
    Variable == "minors_per_worker" ~ ifelse(Valor <= 4, Valor, NA),
    TRUE ~ Valor
  ))

plot_pobreza<-ggplot(dens_df_plot, aes(x = Valor_filtrado, fill = Pobre, color = Pobre)) +
  geom_density(alpha = 0.28, linewidth = 0.8, na.rm = TRUE) +
  facet_wrap(
    ~Variable,
    scales = "free",
    ncol = 2,
    labeller = as_labeller(labels_vars)
  ) +
  scale_fill_manual(values = colores_pobreza) +
  scale_color_manual(values = colores_pobreza) +
  labs(
    title = "Distribución de variables según condición de pobreza",
    subtitle = "Comparación entre hogares pobres y no pobres",
    x = NULL,
    y = "Densidad",
    fill = "",
    color = "",
    caption = "Fuente: DANE, 2018. Elaboración propia."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

# ============================================================
# 3. Variables discretas
# ============================================================

vars_faltantes_num <- c(
  "num_women",
  "num_minors",
  "cat_maxEduc",
  "num_occupied",
  "cuartos_por_persona",
  "jefe_anos_educ",
  "n_personas"
)
vars_faltantes_num <- intersect(vars_faltantes_num, names(train))

labels_vars <- c(
  num_women = "Número de mujeres",
  num_minors = "Número de menores",
  cat_maxEduc = "Máximo nivel educativo",
  num_occupied = "Número de ocupados",
  tasa_desempleo_hogar = "Tasa desempleo hogar",
  cuartos_por_persona = "Cuartos por persona",
  jefe_anos_educ = "Años educación jefe",
  prop_subsidiado = "Proporción subsidiado",
  prop_subempleado = "Proporción subempleado",
  n_personas = "Tamaño del hogar"
)

tabla_bonita <- train %>%
  select(Pobre, all_of(vars_faltantes_num)) %>%
  pivot_longer(
    cols = -Pobre,
    names_to = "Variable",
    values_to = "Valor"
  ) %>%
  group_by(Variable, Pobre) %>%
  summarise(
    Promedio = mean(Valor, na.rm = TRUE),
    Mediana  = median(Valor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Pobre,
    values_from = c(Promedio, Mediana)
  ) %>%
  mutate(
    Variable = labels_vars[Variable]
  ) %>%
  rename(
    `Promedio No pobre` = Promedio_No,
    `Promedio Pobre`    = Promedio_Yes,
    `Mediana No pobre`  = Mediana_No,
    `Mediana Pobre`     = Mediana_Yes
  ) %>%
  mutate(
    across(where(is.numeric), round, 2)
  )

tabla_2<-tabla_bonita %>%
  gt() %>%
  tab_header(
    title = md("**Perfil socioeconómico por condición de pobreza**"),
    subtitle = "Promedio y mediana de variables seleccionadas"
  ) %>%
  cols_label(
    Variable = "Variable",
    
    `Promedio No pobre` = "Promedio",
    `Mediana No pobre`  = "Mediana",
    
    `Promedio Pobre` = "Promedio",
    `Mediana Pobre`  = "Mediana"
  ) %>%
  cols_align(
    align = "center",
    columns = -Variable
  ) %>%
  tab_spanner(
    label = "No pobre",
    columns = c(`Promedio No pobre`, `Mediana No pobre`)
  ) %>%
  tab_spanner(
    label = "Pobre",
    columns = c(`Promedio Pobre`, `Mediana Pobre`)
  ) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  ) %>%
  tab_source_note(
    source_note = md("Fuente: DANE 2018. Elaboración propia.")
  ) %>%
  opt_row_striping() %>%
  tab_options(
    table.font.size = 13,
    heading.title.font.size = 16,
    data_row.padding = px(4)
  )

# ============================================================
# 4. Variables binarias
# ============================================================

vars_tabla <- c(
  "headWoman",
  "formalHead",
  "occupiedHead",
  "mujer_jefe_ocup",
  "sin_ocupados",
  "rent",
  "vivienda_precaria",
  "sin_agua_red",
  "sin_sanitario",
  "zona_rural",
  "tiene_remesas",
  "tiene_pension_ing",
  "jefe_sin_pension"
)
vars_tabla <- intersect(vars_tabla, names(train))

labels_vars <- c(
  headWoman = "Jefatura femenina",
  formalHead = "Jefe con empleo formal",
  occupiedHead = "Jefe ocupado",
  mujer_jefe_ocup = "Jefa ocupada",
  sin_ocupados = "Hogar sin ocupados",
  rent = "Vivienda en arriendo",
  vivienda_precaria = "Vivienda precaria",
  sin_agua_red = "Sin acceso a agua",
  sin_sanitario = "Sin sanitario",
  zona_rural = "Zona rural",
  tiene_remesas = "Recibe remesas",
  tiene_pension_ing = "Ingreso por pensión",
  jefe_sin_pension = "Jefe sin pensión"
)

tabla_final <- lapply(vars_tabla, function(v){
  
  p_yes <- mean(train[[v]][train$Pobre == "Yes"] == 1, na.rm = TRUE)
  p_no  <- mean(train[[v]][train$Pobre == "No"]  == 1, na.rm = TRUE)
  
  data.frame(
    Variable = labels_vars[v],
    Pobre = p_yes,
    No_pobre = p_no,
    Brecha = (p_yes - p_no)*100
  )
  
}) %>%
  bind_rows() %>%
  arrange(desc(Brecha))


tabla_desc<-tabla_final %>%
  gt() %>%
  tab_header(
    title = "Características del hogar según condición de pobreza",
    subtitle = "Porcentaje dentro de cada grupo (ordenado por mayor brecha)"
  ) %>%
  cols_label(
    Variable = "Variable",
    Pobre = "Pobre",
    No_pobre = "No pobre",
    Brecha = "Brecha (pp)"
  ) %>%
  fmt_percent(
    columns = c(Pobre, No_pobre),
    decimals = 1
  ) %>%
  fmt_number(
    columns = Brecha,
    decimals = 2
  ) %>%
  cols_align(
    align = "center",
    columns = c(Pobre, No_pobre, Brecha)
  ) %>%
  tab_source_note(
    source_note = "Fuente: DANE, 2018. Elaboración propia."
  )

# ============================================================
# 5. EXPORTAR
# ============================================================

dir.create("02_outputs/figures", recursive = TRUE, showWarnings = FALSE)

gtsave(
  tabla_2,
  filename = "02_outputs/figures/tabla_pobreza2.png"
)

gtsave(
  tabla_desc,
  filename = "02_outputs/figures/tabla_pobreza1.png"
)

ggsave(
  filename = "02_outputs/figures/grafico_pobreza.png",
  plot = plot_pobreza,
  width = 10,
  height = 7,
  dpi = 300
)


# ============================================================
# 6. Estadísticas descriptivas completas — predictores LGBM
# ============================================================
# Para la submission LGBM_weighted_PR_leaves_63_lr_0_05_iter_393_threshold_0_668.csv
# los predictores son todas las columnas de `train` excepto `Pobre`.
# Generamos tres tablas separadas por tipo (numérica, binaria, categórica)
# con estadísticas apropiadas + distribución por condición de pobreza.

predictores_lgbm <- setdiff(names(train), "Pobre")

# Clasificación por tipo
es_binaria <- function(x) {
  if (!is.numeric(x)) return(FALSE)
  vals <- unique(x[!is.na(x)])
  length(vals) <= 2 && all(vals %in% c(0, 1))
}

tipos <- sapply(predictores_lgbm, function(v) {
  x <- train[[v]]
  if (is.factor(x) || is.character(x)) "categorica"
  else if (es_binaria(x))               "binaria"
  else                                  "numerica"
})

vars_num_lgbm <- names(tipos)[tipos == "numerica"]
vars_bin_lgbm <- names(tipos)[tipos == "binaria"]
vars_cat_lgbm <- names(tipos)[tipos == "categorica"]

cat("\nPredictores LGBM — total:", length(predictores_lgbm), "\n")
cat("  Numéricas  :", length(vars_num_lgbm), "\n")
cat("  Binarias   :", length(vars_bin_lgbm), "\n")
cat("  Categóricas:", length(vars_cat_lgbm), "\n\n")


# ---- 6.1 Tabla numéricas: media/sd/min/p25/mediana/p75/max por grupo ----
stats_num <- lapply(vars_num_lgbm, function(v) {
  x_p <- train[[v]][train$Pobre == "Yes"]
  x_n <- train[[v]][train$Pobre == "No"]
  data.frame(
    Variable  = v,
    Media_P   = mean(x_p, na.rm = TRUE),
    Media_NP  = mean(x_n, na.rm = TRUE),
    Mediana_P  = median(x_p, na.rm = TRUE),
    Mediana_NP = median(x_n, na.rm = TRUE),
    SD_P      = sd(x_p, na.rm = TRUE),
    SD_NP     = sd(x_n, na.rm = TRUE),
    Min       = min(train[[v]], na.rm = TRUE),
    Max       = max(train[[v]], na.rm = TRUE),
    NAs       = sum(is.na(train[[v]]))
  )
}) %>% bind_rows() %>% arrange(Variable)

tabla_num_lgbm <- stats_num %>%
  gt() %>%
  tab_header(
    title    = md("**Estadísticas descriptivas — variables numéricas**"),
    subtitle = "Predictores usados en LGBM_weighted_PR (Modelo C)"
  ) %>%
  tab_spanner(label = "Media",    columns = c(Media_P, Media_NP)) %>%
  tab_spanner(label = "Mediana",  columns = c(Mediana_P, Mediana_NP)) %>%
  tab_spanner(label = "Desv. Est.", columns = c(SD_P, SD_NP)) %>%
  cols_label(
    Media_P = "Pobre",  Media_NP = "No pobre",
    Mediana_P = "Pobre", Mediana_NP = "No pobre",
    SD_P = "Pobre", SD_NP = "No pobre",
    Min = "Mín", Max = "Máx", NAs = "NAs"
  ) %>%
  fmt_number(columns = where(is.numeric), decimals = 2) %>%
  tab_source_note(source_note = md("Fuente: DANE (GEIH/MESE). Elaboración propia.")) %>%
  opt_row_striping() %>%
  tab_options(table.font.size = 10, data_row.padding = px(3))

gtsave(tabla_num_lgbm, "02_outputs/figures/tabla_descriptiva_num_lgbm.png")


# ---- 6.2 Tabla binarias: % en cada grupo + brecha ----
if (length(vars_bin_lgbm) > 0) {
  stats_bin <- lapply(vars_bin_lgbm, function(v) {
    p <- mean(train[[v]][train$Pobre == "Yes"] == 1, na.rm = TRUE)
    n <- mean(train[[v]][train$Pobre == "No"]  == 1, na.rm = TRUE)
    data.frame(
      Variable = v, Pobre = p, No_pobre = n, Brecha_pp = (p - n) * 100
    )
  }) %>% bind_rows() %>% arrange(desc(abs(Brecha_pp)))

  tabla_bin_lgbm <- stats_bin %>%
    gt() %>%
    tab_header(
      title    = md("**Estadísticas descriptivas — variables binarias**"),
      subtitle = "Proporción con valor = 1 por condición de pobreza (ordenado por brecha)"
    ) %>%
    fmt_percent(columns = c(Pobre, No_pobre), decimals = 1) %>%
    fmt_number(columns = Brecha_pp, decimals = 2) %>%
    cols_label(No_pobre = "No pobre", Brecha_pp = "Brecha (pp)") %>%
    tab_source_note(source_note = md("Fuente: DANE (GEIH/MESE). Elaboración propia.")) %>%
    opt_row_striping() %>%
    tab_options(table.font.size = 10, data_row.padding = px(3))

  gtsave(tabla_bin_lgbm, "02_outputs/figures/tabla_descriptiva_bin_lgbm.png")
}


# ---- 6.3 Tabla categóricas: niveles + distribución por grupo ----
if (length(vars_cat_lgbm) > 0) {
  stats_cat <- lapply(vars_cat_lgbm, function(v) {
    x <- train[[v]]
    data.frame(
      Variable    = v,
      Niveles     = length(unique(x[!is.na(x)])),
      Moda        = names(sort(table(x), decreasing = TRUE))[1],
      Pct_Moda    = round(100 * max(prop.table(table(x))), 1),
      NAs         = sum(is.na(x))
    )
  }) %>% bind_rows() %>% arrange(desc(Niveles))

  tabla_cat_lgbm <- stats_cat %>%
    gt() %>%
    tab_header(
      title    = md("**Estadísticas descriptivas — variables categóricas**"),
      subtitle = "Predictores categóricos de LGBM_weighted_PR"
    ) %>%
    cols_label(Pct_Moda = "% Moda") %>%
    tab_source_note(source_note = md("Fuente: DANE (GEIH/MESE). Elaboración propia.")) %>%
    opt_row_striping() %>%
    tab_options(table.font.size = 10, data_row.padding = px(3))

  gtsave(tabla_cat_lgbm, "02_outputs/figures/tabla_descriptiva_cat_lgbm.png")
}


# ---- 6.4 Export adicional a CSV (para editar libre en el informe) ----
write.csv(stats_num, "02_outputs/tables/descriptivo_numericas_lgbm.csv",
          row.names = FALSE)
if (length(vars_bin_lgbm) > 0) {
  write.csv(stats_bin, "02_outputs/tables/descriptivo_binarias_lgbm.csv",
            row.names = FALSE)
}
if (length(vars_cat_lgbm) > 0) {
  write.csv(stats_cat, "02_outputs/tables/descriptivo_categoricas_lgbm.csv",
            row.names = FALSE)
}

cat("\nTablas descriptivas LGBM exportadas:\n")
cat("  02_outputs/figures/tabla_descriptiva_num_lgbm.png\n")
if (length(vars_bin_lgbm) > 0) cat("  02_outputs/figures/tabla_descriptiva_bin_lgbm.png\n")
if (length(vars_cat_lgbm) > 0) cat("  02_outputs/figures/tabla_descriptiva_cat_lgbm.png\n")
cat("  + CSVs en 02_outputs/tables/\n")
