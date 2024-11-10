install.packages("cluster") 
install.packages("dplyr") 
install.packages("factoextra") 

library(cluster)  # Para K-medoids
library(factoextra)  # Para visualización de PCA y clustering
library(dplyr)  # Para manipulación de datos
library(ggplot2)

dataset <- read.csv("wdbc.csv", stringsAsFactors = FALSE)

#Parte 1: Análisis de componentes principales
#Normalización de datos y eliminacion de datos numericos 

datos_numericos <- dataset %>% select(-Id, -Diagnosis)
datos_normalizados <- scale(datos_numericos)

#Aplicacion de pc
respca <- prcomp(datos_normalizados, scale = TRUE)

#Sacamos info del PC
names(respca)

head(respca$rotation)[,1:5]

dim(respca$rotation)

summary(respca)

# Crear el biplot
fviz_pca_biplot(respca , 
                repel = TRUE,            # Evita el solapamiento de etiquetas
                col.var = "red",         # Color de los eigenvectores
                col.ind = "blue",        # Color de los puntos de datos
                geom.ind = "point",      # Representación de los datos como puntos
                labelsize = 4) +         # Tamaño de etiquetas
  ggtitle("Biplot de PCA: Datos y Eigenvectores") +
  theme_minimal()

#Analisis PREELIMINAR

#Podemos ver que el 91% de los datos se eplican con los primeros 7 PC y los dos primeros explican el 63,24%
#y al 4to PC mantenemos el 80% de la variacion original.
#PC1  es negativo para todas las var anteriores
#PC2 es positivo con los radios, perimetros, area, textura 1 y 3. (Osea medidas que definen el tamano del tumor)
#y negativo con textura2 concavidad, compactness, smothness, fractal dimension y 
#symetry; osea todos los datos que definen la forma del tumor
#PC3: es negativo con todo menos con textura y area 
#PC4: es positivO con todo menos textura

######################################################### CODIGO ANTERIOR (USAR SOLO DE REFERENCIA)
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
fviz_pca_biplot(respca , 
                repel = TRUE,            # Evita el solapamiento de etiquetas
                col.var = "red",         # Color de los eigenvectores
                col.ind = "blue",        # Color de los puntos de datos
                geom.ind = "point",      # Representación de los datos como puntos
                labelsize = 4) +         # Tamaño de etiquetas
  ggtitle("Biplot de PCA: Datos y Eigenvectores") +
  theme_minimal()


#Parte 2: Clustering 

#K-medoids


