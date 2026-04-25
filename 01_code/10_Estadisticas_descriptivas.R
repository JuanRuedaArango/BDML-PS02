# ============================================================
# ANÁLISIS EXPLORATORIO: descriptivos sobre predictores LGBM
# ============================================================
#
# Objetivo:
#   Generar tablas y gráficos descriptivos para TODOS los
#   predictores usados en el modelo LGBM_weighted_PR (Modelo C).
#   Los predictores son todas las columnas de `train` excepto
#   `Pobre`. Se clasifican automáticamente en:
#     - Numéricas continuas
#     - Binarias (0/1)
#     - Categóricas (factor / character)
#
# Outputs:
#   - Gráfico de densidades por grupo (numéricas continuas).
#   - Tabla de medias/medianas por grupo (numéricas).
#   - Tabla de % por grupo + brecha (binarias).
#   - Tabla de niveles/moda (categóricas).
#   - CSVs en 02_outputs/tables/ con todas las estadísticas.
# ============================================================


# ============================================================
# 0. Librerías y paths
# ============================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(gt)
library(scales)
library(forcats)

path_plots  <- "02_outputs/figures"
path_tables <- "02_outputs/tables"
dir.create(path_plots,  recursive = TRUE, showWarnings = FALSE)
dir.create(path_tables, recursive = TRUE, showWarnings = FALSE)

colores_pobreza <- c("No" = "#0e2e5b", "Yes" = "#d97706")


# ============================================================
# 1. Clasificación automática de predictores LGBM
# ============================================================
predictores_lgbm <- setdiff(names(train), "Pobre")

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

vars_num <- names(tipos)[tipos == "numerica"]
vars_bin <- names(tipos)[tipos == "binaria"]
vars_cat <- names(tipos)[tipos == "categorica"]

cat("Predictores LGBM — total:", length(predictores_lgbm), "\n")
cat("  Numéricas  :", length(vars_num), "\n")
cat("  Binarias   :", length(vars_bin), "\n")
cat("  Categóricas:", length(vars_cat), "\n\n")


# ============================================================
# 2. Etiquetas legibles (opcional; se usa el nombre si falta)
# ============================================================
labels_vars <- c(
  # Numéricas continuas
  prop_dependiente     = "Tasa de dependencia",
  prop_informal        = "Tasa de informalidad",
  prop_educ_alta       = "Prop. con educación alta",
  avg_educ_adultos     = "Promedio educación adultos (años)",
  hacinamiento         = "Hacinamiento",
  cuartos_por_persona  = "Cuartos por persona",
  jefe_edad            = "Edad jefe",
  jefe_anos_educ       = "Años educación jefe",
  n_personas           = "Tamaño del hogar",
  num_women            = "Número de mujeres",
  num_minors           = "Número de menores",
  num_occupied         = "Número de ocupados",
  persons_per_worker   = "Personas por ocupado",
  minors_per_worker    = "Menores por ocupado",
  tasa_desempleo_hogar = "Tasa desempleo hogar",
  prop_subsidiado      = "Proporción subsidiado",
  prop_subempleado     = "Proporción subempleado",
  # Binarias
  headWoman            = "Jefatura femenina",
  formalHead           = "Jefe con empleo formal",
  occupiedHead         = "Jefe ocupado",
  mujer_jefe_ocup      = "Jefa ocupada",
  sin_ocupados         = "Hogar sin ocupados",
  rent                 = "Vivienda en arriendo",
  vivienda_precaria    = "Vivienda precaria",
  sin_agua_red         = "Sin acceso a agua",
  sin_sanitario        = "Sin sanitario",
  zona_rural           = "Zona rural",
  tiene_remesas        = "Recibe remesas",
  tiene_pension_ing    = "Ingreso por pensión",
  jefe_sin_pension     = "Jefe sin pensión",
  # Categóricas
  cat_maxEduc          = "Máximo nivel educativo",
  Depto                = "Departamento",
  Clase                = "Clase (urbano/rural)"
)

get_label <- function(v) ifelse(v %in% names(labels_vars), labels_vars[v], v)


# ============================================================
# 3. Densidades — numéricas continuas
# ============================================================
# Recortes de cola para que las densidades sean legibles.
lims_vars <- list(
  hacinamiento       = 8,
  persons_per_worker = 5,
  minors_per_worker  = 4
)

dens_df <- train %>%
  select(Pobre, all_of(vars_num)) %>%
  pivot_longer(cols = -Pobre, names_to = "Variable", values_to = "Valor") %>%
  mutate(
    Valor = mapply(function(val, var) {
      lim <- lims_vars[[var]]
      if (!is.null(lim) && !is.na(val) && val > lim) NA_real_ else val
    }, Valor, Variable),
    Variable_label = get_label(Variable)
  )

# Con muchas variables el facet_wrap se vuelve ilegible → paginamos.
vars_per_page <- 9
pags <- split(
  vars_num,
  ceiling(seq_along(vars_num) / vars_per_page)
)

for (i in seq_along(pags)) {

  dens_sub <- dens_df %>% filter(Variable %in% pags[[i]])

  p_i <- ggplot(dens_sub, aes(x = Valor, fill = Pobre, color = Pobre)) +
    geom_density(alpha = 0.28, linewidth = 0.8, na.rm = TRUE) +
    facet_wrap(~ Variable_label, scales = "free", ncol = 3) +
    scale_fill_manual(values = colores_pobreza) +
    scale_color_manual(values = colores_pobreza) +
    labs(
      title    = "Distribución de predictores numéricos por condición de pobreza",
      subtitle = sprintf("Predictores LGBM — página %d de %d",
                         i, length(pags)),
      x = NULL, y = "Densidad", fill = "", color = "",
      caption  = "Fuente: DANE (GEIH/MESE). Elaboración propia."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position  = "top",
      strip.text       = element_text(face = "bold", size = 9),
      plot.title       = element_text(face = "bold", size = 13),
      panel.grid.minor = element_blank()
    )

  ggsave(
    filename = file.path(path_plots,
                         sprintf("densidades_lgbm_p%02d.png", i)),
    plot = p_i, width = 11, height = 8, dpi = 300
  )
}


# ============================================================
# 4. Tabla descriptiva — numéricas
# ============================================================
stats_num <- lapply(vars_num, function(v) {
  x_p <- train[[v]][train$Pobre == "Yes"]
  x_n <- train[[v]][train$Pobre == "No"]
  data.frame(
    Variable   = get_label(v),
    Media_P    = mean(x_p,     na.rm = TRUE),
    Media_NP   = mean(x_n,     na.rm = TRUE),
    Mediana_P  = median(x_p,   na.rm = TRUE),
    Mediana_NP = median(x_n,   na.rm = TRUE),
    SD_P       = sd(x_p,       na.rm = TRUE),
    SD_NP      = sd(x_n,       na.rm = TRUE),
    Min        = min(train[[v]], na.rm = TRUE),
    Max        = max(train[[v]], na.rm = TRUE),
    NAs        = sum(is.na(train[[v]]))
  )
}) %>% bind_rows() %>% arrange(Variable)

tabla_num <- stats_num %>%
  gt() %>%
  tab_header(
    title    = md("**Estadísticas descriptivas — variables numéricas**"),
    subtitle = "Predictores LGBM (Modelo C)"
  ) %>%
  tab_spanner(label = "Media",      columns = c(Media_P, Media_NP)) %>%
  tab_spanner(label = "Mediana",    columns = c(Mediana_P, Mediana_NP)) %>%
  tab_spanner(label = "Desv. Est.", columns = c(SD_P, SD_NP)) %>%
  cols_label(
    Media_P    = "Pobre", Media_NP   = "No pobre",
    Mediana_P  = "Pobre", Mediana_NP = "No pobre",
    SD_P       = "Pobre", SD_NP      = "No pobre",
    Min = "Mín", Max = "Máx", NAs = "NAs"
  ) %>%
  fmt_number(columns = where(is.numeric), decimals = 2) %>%
  tab_source_note(source_note = md("Fuente: DANE (GEIH/MESE). Elaboración propia.")) %>%
  opt_row_striping() %>%
  tab_options(table.font.size = 10, data_row.padding = px(3))

gtsave(tabla_num, file.path(path_plots, "tabla_descriptiva_num_lgbm.png"))


# ============================================================
# 5. Tabla descriptiva — binarias
# ============================================================
if (length(vars_bin) > 0) {

  stats_bin <- lapply(vars_bin, function(v) {
    p <- mean(train[[v]][train$Pobre == "Yes"] == 1, na.rm = TRUE)
    n <- mean(train[[v]][train$Pobre == "No"]  == 1, na.rm = TRUE)
    data.frame(
      Variable  = get_label(v),
      Pobre     = p,
      No_pobre  = n,
      Brecha_pp = (p - n) * 100,
      NAs       = sum(is.na(train[[v]]))
    )
  }) %>% bind_rows() %>% arrange(desc(abs(Brecha_pp)))

  tabla_bin <- stats_bin %>%
    gt() %>%
    tab_header(
      title    = md("**Estadísticas descriptivas — variables binarias**"),
      subtitle = "Proporción con valor = 1 por condición de pobreza (orden por |brecha|)"
    ) %>%
    fmt_percent(columns = c(Pobre, No_pobre), decimals = 1) %>%
    fmt_number(columns = Brecha_pp, decimals = 2) %>%
    cols_label(No_pobre = "No pobre", Brecha_pp = "Brecha (pp)") %>%
    tab_source_note(source_note = md("Fuente: DANE (GEIH/MESE). Elaboración propia.")) %>%
    opt_row_striping() %>%
    tab_options(table.font.size = 10, data_row.padding = px(3))

  gtsave(tabla_bin, file.path(path_plots, "tabla_descriptiva_bin_lgbm.png"))
}


# ============================================================
# 6. Tabla descriptiva — categóricas
# ============================================================
if (length(vars_cat) > 0) {

  stats_cat <- lapply(vars_cat, function(v) {
    x <- train[[v]]
    tab_v <- table(x)
    data.frame(
      Variable = get_label(v),
      Niveles  = length(unique(x[!is.na(x)])),
      Moda     = names(sort(tab_v, decreasing = TRUE))[1],
      Pct_Moda = round(100 * max(prop.table(tab_v)), 1),
      NAs      = sum(is.na(x))
    )
  }) %>% bind_rows() %>% arrange(desc(Niveles))

  tabla_cat <- stats_cat %>%
    gt() %>%
    tab_header(
      title    = md("**Estadísticas descriptivas — variables categóricas**"),
      subtitle = "Predictores categóricos de LGBM (Modelo C)"
    ) %>%
    cols_label(Pct_Moda = "% Moda") %>%
    tab_source_note(source_note = md("Fuente: DANE (GEIH/MESE). Elaboración propia.")) %>%
    opt_row_striping() %>%
    tab_options(table.font.size = 10, data_row.padding = px(3))

  gtsave(tabla_cat, file.path(path_plots, "tabla_descriptiva_cat_lgbm.png"))
}


# ============================================================
# 7. Export a CSV
# ============================================================
write.csv(stats_num,
          file.path(path_tables, "descriptivo_numericas_lgbm.csv"),
          row.names = FALSE)

if (length(vars_bin) > 0) {
  write.csv(stats_bin,
            file.path(path_tables, "descriptivo_binarias_lgbm.csv"),
            row.names = FALSE)
}
if (length(vars_cat) > 0) {
  write.csv(stats_cat,
            file.path(path_tables, "descriptivo_categoricas_lgbm.csv"),
            row.names = FALSE)
}

cat("\nOutputs generados:\n")
cat("  ", length(pags), "página(s) de densidades en", path_plots, "\n")
cat("   tabla_descriptiva_num_lgbm.png\n")
if (length(vars_bin) > 0) cat("   tabla_descriptiva_bin_lgbm.png\n")
if (length(vars_cat) > 0) cat("   tabla_descriptiva_cat_lgbm.png\n")
cat("   CSVs en", path_tables, "\n")
