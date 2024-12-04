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

## Eliminar columnas con sufijos '2' y '3'
datos <- datos[, !grepl("(2|3)$", names(datos))]

## Verificar valores NA
if (anyNA(datos)) {
  print("Existen valores NA en el dataset. Revisa los datos.")
} else {
  print("No se encontraron valores NA en el dataset.")
}

## Discretización de variables numéricas (por ejemplo, radius1)
datos$radius1 <- discretize(datos$radius1, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
datos$perimeter1 <- discretize(datos$perimeter1, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
datos$area1 <- discretize(datos$area1, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
datos$smoothness1 <- discretize(datos$smoothness1, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
datos$concave_points1 <- discretize(datos$concave_points1, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
datos$texture1 <- discretize(datos$texture1, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
datos$compactness1 <- discretize(datos$compactness1, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
datos$concavity1 <- discretize(datos$concavity1, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
datos$symmetry1 <- discretize(datos$symmetry1, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
datos$fractal_dimension1  <- discretize(datos$fractal_dimension1, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))

# 4.1 Transformación a una Matriz Esparza

## Conversión de Diagnosis a factor
datos$Diagnosis <- as.factor(datos$Diagnosis)

# Convierte todas las columnas a factores (si no lo están ya)
datos[] <- lapply(datos, function(x) if (is.character(x) || is.factor(x)) as.factor(x) else x)

# 5. Conversión a transacciones
transacciones <- as(datos, "transactions")

# Inspección de las transacciones
summary(transacciones)
inspect(head(transacciones, 5))

# 6. Aplicación de Reglas de Asociación (Algoritmo Apriori)

# Parámetros mínimos para generar reglas
soporte_min <- 0.05
confianza_min <- 0.95

# Generar las reglas usando el algoritmo Apriori
reglas <- apriori(transacciones, 
                  parameter = list(supp = soporte_min, conf = confianza_min, target = "rules"))

# Inspección inicial de las reglas generadas
summary(reglas)
inspect(head(sort(reglas, by = "lift"), 10))

# Filtrar reglas donde el consecuente es 'Diagnosis=M' o 'Diagnosis=B'
reglas_filtradas <- subset(reglas, rhs %pin% "Diagnosis=")

reglas_filtradas <- reglas_filtradas[!is.redundant(reglas_filtradas)]
# Inspección inicial de las reglas filtradas
summary(reglas_filtradas)
inspect(head(sort(reglas_filtradas, by = "lift"), 10))

# 7.- Visualización de las reglas filtradas
plot(reglas_filtradas, method = "grouped")
plot(reglas_filtradas, method = "graph", engine = "htmlwidget")


#8.- Filtrar Reglas más Relevantes
reglas_filtradas <- subset(reglas_filtradas, lift > 2.5 & confidence > 0.95 & support > 0.1)
inspect(head(reglas_filtradas, 10))  # Inspeccionar las primeras 10 reglas relevantes
## Visualizar las 10 reglas principales ordenadas por lift
inspect(head(sort(reglas_filtradas, by = "lift"), 10))

# Si aún hay muchas reglas, limitar a las 100 principales por lift
reglas_top <- head(sort(reglas_filtradas, by = "lift"), 10)
inspect(reglas_top)
reglas_top <- head(sort(reglas_top, by = "support"), 10)
inspect(reglas_top)
# Guardar las reglas filtradas en un archivo
write(reglas_top, file = "reglas_asociacion_filtradas.csv", sep = ",", quote = TRUE, row.names = FALSE)
# 9.- Graficos
#plot(reglas_top, method = "grouped")
plot(reglas_top, method = "graph", engine = "htmlwidget")

#----------------------------------------------------------------------------------------------
# Inspección del formato transaccional
summary(transacciones)
inspect(head(transacciones))

# 6. Generación de Reglas de Asociación

## Aplicar Apriori con soporte mínimo de 0.01 y confianza mínima de 0.8
reglas <- apriori(transacciones, parameter = list(supp = 0.05, conf = 0.8, maxlen = 4))

## Resumen de las reglas generadas
summary(reglas)

#7 Filtrar Reglas Relevantes
reglas_filtradas <- subset(reglas, lift > 2.5 & confidence > 0.9417 & support > 0.10401)
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




