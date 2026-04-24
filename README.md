# BDML-PS02

## Descripción del proyecto

Este repositorio contiene el código y análisis completo para un modelo de clasificación de pobreza a nivel de hogar, construido con datos de la encuesta MESE del DANE. El objetivo es predecir si un hogar se encuentra por debajo de la línea de pobreza usando métodos de machine learning, equilibrando el desempeño predictivo con las restricciones prácticas del diseño de política pública.

El flujo de trabajo cubre la limpieza y construcción de variables a nivel de hogar e individuo, el entrenamiento y comparación de distintos algoritmos de clasificación, la selección de modelos y la evaluación del desempeño predictivo mediante métricas relevantes para clasificación, especialmente el F1 score. Los resultados se interpretan desde una perspectiva de política pública, analizando el balance entre subcobertura, asociada a falsos negativos, y filtración, asociada a falsos positivos, en instrumentos de focalización de pobreza.

Los modelos implementados en el proyecto son:

- Elastic Net
- LightGBM
- Random Forest
- GLM, incluyendo MPL / Logit
- GBM, Gradient Boosting Machine
- Naive Bayes
- Stacking

## Autores

Juan Rueda, Andrés Suárez, Hernán Yepes

Big Data y Machine Learning – Universidad de Los Andes – 2026

## Estructura general del repositorio

El repositorio está organizado alrededor de una carpeta principal de código, donde se encuentran los scripts de limpieza, construcción de variables, entrenamiento de modelos y generación de resultados.

Los principales scripts de modelos son:

```text
01_code/
├── 03_ElasticNet.R
├── 04_LightGBM.R
├── 05_RandomForest.R
├── 06_Stacking.R
├── 07_GLM.R
├── 08_GBM.R
└── 09_Bayes.R
````

Además, el proyecto cuenta con un script principal de ejecución, denominado `00_rundirectory.R`, que permite correr el flujo completo o seleccionar partes específicas del análisis.

## Uso del `00_rundirectory.R`

El archivo `rundirectory` funciona como el script maestro del proyecto. Su propósito es facilitar la reproducción del análisis completo sin que el usuario tenga que ejecutar manualmente cada archivo por separado.

Al ejecutarlo, el script realiza las siguientes acciones generales:

1. Identifica automáticamente la raíz del repositorio.
2. Busca la carpeta externa `Bases`, donde deben estar almacenadas las bases de datos necesarias para el proyecto.
3. Instala y carga los paquetes requeridos para el análisis.
4. Ejecuta una preparación inicial de datos, que incluye funciones, limpieza y construcción de variables.
5. Muestra un menú interactivo para que el usuario seleccione qué parte del proyecto desea correr.
6. Permite ejecutar modelos individuales o combinaciones de modelos.
7. Permite volver al menú y correr otro modelo sin repetir innecesariamente la limpieza y el feature engineering.

La preparación inicial incluye los scripts base:

```r
source("01_code/00_funciones.R")
source("01_code/01_limpieza.R")
source("01_code/02_feature_engineering.R")
```

Estos scripts se ejecutan una sola vez durante la sesión, salvo que el usuario decida forzar nuevamente la preparación de datos.

## Cómo ejecutar el proyecto

Para correr el proyecto, se debe abrir R o RStudio desde el repositorio y ejecutar el script maestro:

```r
source("00_rundirectory.R")
```

Una vez ejecutado, aparecerá un menú en consola con las opciones disponibles. Desde allí, el usuario puede seleccionar si desea correr todos los modelos, un modelo específico o una combinación de modelos.

Las opciones disponibles incluyen:

* Estadísticas descriptivas
* Elastic Net
* LightGBM
* Random Forest
* GLM
* GBM
* Naive Bayes
* LightGBM + Random Forest
* LightGBM + Random Forest + Stacking
* Los cinco algoritmos requeridos
* Los cinco algoritmos requeridos + GBM + Stacking
* Todos los modelos

Después de ejecutar una opción, el menú permite volver a seleccionar otra alternativa sin repetir la preparación base de datos.

## Recomendaciones de ejecución

Antes de correr el proyecto, se recomienda verificar que la carpeta `Bases` exista y contenga los archivos necesarios para el análisis. Esta carpeta puede estar ubicada dentro del repositorio, en el directorio padre del repositorio o en una ruta externa definida en el equipo del usuario.

También se recomienda ejecutar primero modelos individuales antes de correr todos los modelos al mismo tiempo. Algunos algoritmos pueden tardar más que otros dependiendo de la capacidad del computador y el número de hiperparámetros evaluados.

Dado que el proyecto realiza entrenamiento de modelos y generación de predicciones, el tiempo de ejecución puede variar considerablemente. Modelos como Random Forest, LightGBM, GBM o Stacking pueden tomar más tiempo que modelos más simples como GLM o Naive Bayes.

Si ya se ejecutó la limpieza y construcción de variables durante la sesión, no es necesario volver a correr esa parte. El `00_rundirectory.R` está diseñado para evitar repetir la preparación base y permitir que el usuario regrese al menú para ejecutar nuevos modelos de forma más eficiente.

## Resultados esperados

La ejecución de los modelos genera resultados asociados al desempeño predictivo y, cuando corresponde, archivos de predicción para evaluación externa. Estos resultados permiten comparar el rendimiento de los distintos algoritmos y seleccionar el modelo con mejor desempeño de acuerdo con la métrica principal del proyecto.

El criterio central de evaluación es el F1 score, ya que permite balancear precisión y sensibilidad en un problema donde los errores de clasificación tienen implicaciones relevantes para la focalización de hogares en condición de pobreza.
