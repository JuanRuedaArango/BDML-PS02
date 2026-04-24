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

path_plots <- "02_outputs/plots/descriptive"

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

dir.create("02_outputs/plots/descriptive", recursive = TRUE, showWarnings = FALSE)

gtsave(
  tabla_2,
  filename = "02_outputs/plots/descriptive/tabla_pobreza2.png"
)

gtsave(
  tabla_desc,
  filename = "02_outputs/plots/descriptive/tabla_pobreza1.png"
)

ggsave(
  filename = "02_outputs/plots/descriptive/grafico_pobreza.png",
  plot = plot_pobreza,
  width = 10,
  height = 7,
  dpi = 300
)
