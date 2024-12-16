install.packages("C50")
install.packages("rpart.plot")
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)
library(rpart.plot)
library(C50)
library(caret)
library(ggplot2)

dataset <- read.csv("wdbc.csv", stringsAsFactors = FALSE)
dataset$Diagnosis <- as.factor(dataset$Diagnosis)
#Eliminar columna ID porque no aporta al análisis
dataset$Id <- NULL
# Discretización de variables numéricas (por ejemplo, radius1)
dataset$radius1 <- discretize(dataset$radius1, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$perimeter1 <- discretize(dataset$perimeter1, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$area1 <- discretize(dataset$area1, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$smoothness1 <- discretize(dataset$smoothness1, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$concave_points1 <- discretize(dataset$concave_points1, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$texture1 <- discretize(dataset$texture1, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$compactness1 <- discretize(dataset$compactness1, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$concavity1 <- discretize(dataset$concavity1, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$symmetry1 <- discretize(dataset$symmetry1, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$fractal_dimension1  <- discretize(dataset$fractal_dimension1, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$radius2 <- discretize(dataset$radius2, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$perimeter2 <- discretize(dataset$perimeter2, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$area2 <- discretize(dataset$area2, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$smoothness2 <- discretize(dataset$smoothness2, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$concave_points2 <- discretize(dataset$concave_points2, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$texture2 <- discretize(dataset$texture2, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$compactness2 <- discretize(dataset$compactness2, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$concavity2 <- discretize(dataset$concavity2, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$symmetry2 <- discretize(dataset$symmetry2, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$fractal_dimension2  <- discretize(dataset$fractal_dimension2, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$radius3 <- discretize(dataset$radius3, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$perimeter3 <- discretize(dataset$perimeter3, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$area3 <- discretize(dataset$area3, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$smoothness3 <- discretize(dataset$smoothness3, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$concave_points3 <- discretize(dataset$concave_points3, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$texture3 <- discretize(dataset$texture3, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$compactness3 <- discretize(dataset$compactness3, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$concavity3 <- discretize(dataset$concavity3, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$symmetry3 <- discretize(dataset$symmetry3, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
dataset$fractal_dimension3  <- discretize(dataset$fractal_dimension3, method = "frequency", breaks = 3, labels = c("Bajo", "Medio", "Alto"))
# Dividir el dataset en datos de entrenamiento y prueba
set.seed(123)
indices_entrenamiento <- sample(1:nrow(dataset), 0.7 * nrow(dataset))
datos_entrenamiento <- dataset[indices_entrenamiento, ]
datos_prueba <- dataset[-indices_entrenamiento, ]

# Entrenar el modelo
modelo <- C5.0(datos_entrenamiento$Diagnosis ~ ., data = datos_entrenamiento)

# Ver Arbol
summary(modelo)

# Realizar predicciones
predicciones <- predict(modelo, datos_prueba)

# Evualuar el modelo

confusionMatrix(predicciones, datos_prueba$Diagnosis)
