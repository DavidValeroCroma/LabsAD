
# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)

# Cargar el dataset
data <- read.csv("wdbc.csv")

# Realizar el Shapiro-Wilk test para cada característica en el grupo benigno y maligno

cat("Shapiro-Wilk test para tumores benignos:\n")
shapiro_radius_b <- shapiro.test(data$radius1[data$Diagnosis == "B"])
shapiro_perimeter_b <- shapiro.test(data$perimeter1[data$Diagnosis == "B"])
shapiro_area_b <- shapiro.test(data$area1[data$Diagnosis == "B"])
shapiro_concavity_b <- shapiro.test(data$concavity1[data$Diagnosis == "B"])
shapiro_concave_points_b <- shapiro.test(data$concave_points1[data$Diagnosis == "B"])

cat("\nRadio (Benignos):\n")
print(shapiro_radius_b)
cat("\nPerímetro (Benignos):\n")
print(shapiro_perimeter_b)
cat("\nÁrea (Benignos):\n")
print(shapiro_area_b)
cat("\nConcavidad (Benignos):\n")
print(shapiro_concavity_b)
cat("\nPuntos Cóncavos (Benignos):\n")
print(shapiro_concave_points_b)

cat("\nShapiro-Wilk test para tumores malignos:\n")
shapiro_radius_m <- shapiro.test(data$radius1[data$Diagnosis == "M"])
shapiro_perimeter_m <- shapiro.test(data$perimeter1[data$Diagnosis == "M"])
shapiro_area_m <- shapiro.test(data$area1[data$Diagnosis == "M"])
shapiro_concavity_m <- shapiro.test(data$concavity1[data$Diagnosis == "M"])
shapiro_concave_points_m <- shapiro.test(data$concave_points1[data$Diagnosis == "M"])

cat("\nRadio (Malignos):\n")
print(shapiro_radius_m)
cat("\nPerímetro (Malignos):\n")
print(shapiro_perimeter_m)
cat("\nÁrea (Malignos):\n")
print(shapiro_area_m)
cat("\nConcavidad (Malignos):\n")
print(shapiro_concavity_m)
cat("\nPuntos Cóncavos (Malignos):\n")
print(shapiro_concave_points_m)

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
#dado que la mayoria de los parametros no sigue una distribucion normal usaremos la prueba U de Mann-Whitney
# Prueba U para radius1
cat("Prueba U de Mann-Whitney para tumores benignos y malignos:\n")
u_test_radius <- wilcox.test(radius1 ~ Diagnosis, data = data)
cat("\nPrueba U para radius1:\n")
print(u_test_radius)

# Prueba U para perimeter1
u_test_perimeter <- wilcox.test(perimeter1 ~ Diagnosis, data = data)
cat("\nPrueba U para perimeter1:\n")
print(u_test_perimeter)

# Prueba U para area1
u_test_area <- wilcox.test(area1 ~ Diagnosis, data = data)
cat("\nPrueba U para area1:\n")
print(u_test_area)

# Prueba U para concavity1
u_test_concavity <- wilcox.test(concavity1 ~ Diagnosis, data = data)
cat("\nPrueba U para concavity1:\n")
print(u_test_concavity)

# Prueba U para concave_points1
u_test_concave_points <- wilcox.test(concave_points1 ~ Diagnosis, data = data)
cat("\nPrueba U para concave_points1:\n")
print(u_test_concave_points)


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



