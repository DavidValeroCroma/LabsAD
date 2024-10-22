
# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)

# Cargar el dataset
data <- read.csv("wdbc.csv")

# Convertir la columna 'Diagnosis' a factor (Benigno: B, Maligno: M)
data$Diagnosis <- factor(data$Diagnosis, levels = c("B", "M"))

# Estadísticas descriptivas
benign <- data %>% filter(Diagnosis == "B")
malign <- data %>% filter(Diagnosis == "M")

# Cálculo de medias para las características seleccionadas
mean_benign <- benign %>% summarise(across(c(radius1, perimeter1, area1, concavity1, concave_points1), mean))
mean_malign <- malign %>% summarise(across(c(radius1, perimeter1, area1, concavity1, concave_points1), mean))

cat("Medias para tumores benignos:\n")
print(mean_benign)

cat("Medias para tumores malignos:\n")
print(mean_malign)

# Pruebas t para comparar las medias entre tumores benignos y malignos
t_radius <- t.test(data$radius1 ~ data$Diagnosis)
t_perimeter <- t.test(data$perimeter1 ~ data$Diagnosis)
t_area <- t.test(data$area1 ~ data$Diagnosis)
t_concavity <- t.test(data$concavity1 ~ data$Diagnosis)
t_concave_points <- t.test(data$concave_points1 ~ data$Diagnosis)

# Mostrar resultados de las pruebas t
cat("\nPrueba t para radius1:\n")
print(t_radius)
cat("\nPrueba t para perimeter1:\n")
print(t_perimeter)
cat("\nPrueba t para area1:\n")
print(t_area)
cat("\nPrueba t para concavity1:\n")
print(t_concavity)
cat("\nPrueba t para concave_points1:\n")
print(t_concave_points)

# Regresión logística para predecir si un tumor es maligno o benigno
# Usaremos las características seleccionadas (radius1, perimeter1, area1, concavity1, concave_points1)
model <- glm(Diagnosis ~ radius1 + perimeter1 + area1 + concavity1 + concave_points1, data = data, family = binomial)

# Resumen del modelo
cat("\nResumen del modelo de regresión logística:\n")
summary(model)

# Predicción y evaluación del modelo
# Dividir el dataset en conjunto de entrenamiento y prueba
set.seed(123)
train_indices <- sample(1:nrow(data), size = 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Entrenar el modelo
model_train <- glm(Diagnosis ~ radius1 + perimeter1 + area1 + concavity1 + concave_points1, data = train_data, family = binomial)

# Predicciones
pred_prob <- predict(model_train, newdata = test_data, type = "response")
pred_class <- ifelse(pred_prob > 0.5, "M", "B")

# Matriz de confusión
confusion_matrix <- table(Predicted = pred_class, Actual = test_data$Diagnosis)
cat("\nMatriz de confusión:\n")
print(confusion_matrix)

# Cálculo de la precisión
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("\nPrecisión del modelo: ", accuracy, "\n")
