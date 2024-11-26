# 1. Cargar Librerías
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

# 2. Cargar los Datos
ruta_archivo <- "wdbc.csv"  # Cambia esta ruta a donde se encuentre tu archivo CSV
datos <- read.csv(ruta_archivo)

# 3. Inspección Inicial
head(datos)  # Ver las primeras filas del dataset
str(datos)   # Estructura de los datos
summary(datos)  # Resumen estadístico de las columnas

# 4. Preprocesamiento de Datos

## Eliminar columna ID porque no aporta al análisis
datos$Id <- NULL

## Verificar valores NA
if (anyNA(datos)) {
  print("Existen valores NA en el dataset. Revisa los datos.")
} else {
  print("No se encontraron valores NA en el dataset.")
}

## Discretización de variables numéricas (por ejemplo, radius1)
datos$radius1 <- discretize(datos$radius1, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))

## Conversión de Diagnosis a factor
datos$Diagnosis <- as.factor(datos$Diagnosis)

# 5. Conversión a transacciones
transacciones <- as(datos, "transactions")

# Inspección del formato transaccional
summary(transacciones)
inspect(head(transacciones))

# 6. Generación de Reglas de Asociación

## Aplicar Apriori con soporte mínimo de 0.01 y confianza mínima de 0.8
reglas <- apriori(transacciones, parameter = list(supp = 0.05, conf = 0.8, maxlen = 4))

## Resumen de las reglas generadas
summary(reglas)

#7 Filtrar Reglas Relevantes
reglas_filtradas <- subset(reglas, lift > 2.5 & confidence > 0.9)
inspect(head(reglas_filtradas, 10))  # Inspeccionar las primeras 10 reglas relevantes
## Visualizar las 10 reglas principales ordenadas por lift
inspect(head(sort(reglas, by = "lift"), 10))

# Si aún hay muchas reglas, limitar a las 100 principales por lift
reglas_top <- head(sort(reglas_filtradas, by = "lift"), 100)
inspect(reglas_top)

# 8 Graficos
if (length(reglas_top) > 0) {
  # Gráfico de red
  plot(reglas_top, method = "graph", control = list(type = "items"))
  
  # Gráfico matriz
  plot(reglas_top, method = "matrix", measure = c("support", "confidence"))
} else {
  print("No se encontraron reglas relevantes para graficar.")
}




