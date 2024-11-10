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

# summary(respca)
print(summary(respca))
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

# Parte 2: Clustering con K-medoids

# Primero, cargamos la librería necesaria
library(cluster)

# Definir el número de clusters (por ejemplo, 2 o 3 según el análisis de los datos o el objetivo del análisis)
k <- 2

# Aplicamos el algoritmo K-medoids usando los datos de los componentes principales seleccionados (datos_pca)
# Utilizamos los primeros componentes principales que capturan el 90% de la varianza
kmedoids_result <- pam(datos_pca, k = k)

# Visualización de los clusters obtenidos en el espacio de los primeros dos componentes principales
fviz_cluster(kmedoids_result, data = datos_pca,
             geom = "point", ellipse.type = "norm", 
             main = paste("K-medoids Clustering con", k, "Clusters"),
             ggtheme = theme_minimal())

# Interpretación de resultados
# Se pueden ver los centros de cada cluster, los miembros de cada cluster y otras métricas.
# Imprimir información del clustering
print("Medoids:")
print(kmedoids_result$medoids)
print("Objective:")
print(kmedoids_result$objective)
# print("Silueta:")
# silhouette_info <- kmedoids_result$silinfo
# print(silhouette_info)

#graficar el coeficiente de silueta
# fviz_silhouette(silhouette_info) + ggtitle("Coeficiente de Silueta")

print("clusinfo: ")
print(kmedoids_result$clusinfo)
