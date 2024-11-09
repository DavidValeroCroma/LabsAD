install.packages("cluster") 
install.packages("dplyr") 
install.packages("factoextra") 

library(cluster)  # Para K-medoids
library(factoextra)  # Para visualización de PCA y clustering
library(dplyr)  # Para manipulación de datos
library(ggplot2)

dataset <- read.csv("wdbc.csv", stringsAsFactors = FALSE)

#Parte 1: Análisis de componentes principales
#Normalización de datos

datos_numericos <- dataset %>% select(-Id, -Diagnosis)
datos_normalizados <- scale(datos_numericos)

#Matriz de covarianza
matriz_covarianza <- cov(datos_normalizados)

#Eigenvectors
eigen_result <- eigen(matriz_covarianza)
eigen_valores <- eigen_result$values
eigen_vectores <- eigen_result$vectors

#Selección de componentes principales
varianza_acumulada <- cumsum(eigen_valores) / sum(eigen_valores)
num_componentes <- which(varianza_acumulada >= 0.9)[1]  # Primer índice que alcanza el 90%

#esta parte todavia no entiendo que hace, investigar 
datos_pca <- datos_normalizados %*% eigen_vectores[, 1:num_componentes]


#Visualizacion de los eigenvectors en el plano de los primeros 2 PC
# Realizar PCA
pca <- prcomp(datos_normalizados, center = TRUE, scale. = TRUE)

# Crear el biplot
fviz_pca_biplot(pca, 
                repel = TRUE,            # Evita el solapamiento de etiquetas
                col.var = "red",         # Color de los eigenvectores
                col.ind = "blue",        # Color de los puntos de datos
                geom.ind = "point",      # Representación de los datos como puntos
                labelsize = 4) +         # Tamaño de etiquetas
  ggtitle("Biplot de PCA: Datos y Eigenvectores") +
  theme_minimal()


#Parte 2: Clustering 

#K-medoids


