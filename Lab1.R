# Cargar librerías necesarias
install.packages("modeest")  # Si no tienes instalada la librería para calcular la moda
library(modeest)

dataset <- read.csv("wdbc.csv", stringsAsFactors = FALSE)

# Seleccionar las columnas numéricas del dataset que deseas analizar
columnas_a_analizar <- dataset[, c("radius1", "texture1", "perimeter1", "area1", 
                                "smoothness1", "compactness1", "concavity1", 
                                "concave_points1", "symmetry1", "fractal_dimension1")]

#            # Calcular la media de cada columna
medias <- apply(columnas_a_analizar, 2, mean)

# # Calcular la mediana de cada columna
medianas <- apply(columnas_a_analizar, 2, median)

# # Calcular la moda de cada columna (usando la función mfv para calcular la moda)
modas <- apply(columnas_a_analizar, 2, function(x) mfv(x))             

# # Mostrar los resultados
medias
medianas
modas

# Separar los datos en benignos y malignos
benignos <- subset(dataset, Diagnosis == "B")
malignos <- subset(dataset, Diagnosis == "M")

# Prueba t para el área1 entre tumores benignos y malignos
t.test(benignos$area1, malignos$area1)

# Convertir la variable Diagnosis a factor
dataset$Diagnosis <- factor(dataset$Diagnosis, levels = c("B", "M"))

# Ajustar el modelo de regresión logística
modelo <- glm(Diagnosis ~ area1 + smoothness1 + concavity1, data = dataset, family = binomial)

# Resumen del modelo
summary(modelo)

# Predicciones
predicciones <- predict(modelo, type = "response")

library(caret)

# Dividir los datos en entrenamiento y prueba
set.seed(123)
trainIndex <- createDataPartition(dataset$Diagnosis, p = 0.8, list = FALSE)
trainData <- dataset[trainIndex, ]
testData <- dataset[-trainIndex, ]

# Entrenar el modelo con validación cruzada
modelo_cv <- train(Diagnosis ~ area1 + smoothness1 + concavity1, data = trainData, method = "glm", family = "binomial", trControl = trainControl(method = "cv", number = 10))

# Evaluación en el conjunto de prueba
predicciones_test <- predict(modelo_cv, newdata = testData)
confusionMatrix(predicciones_test, testData$Diagnosis)