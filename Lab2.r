install.packages("cluster") 
install.packages("dplyr") 
install.packages("factoextra") 

library(cluster)  # Para K-medoids
library(factoextra)  # Para visualización de PCA y clustering
library(dplyr)  # Para manipulación de datos
library(ggplot2)

dataset <- read.csv("wdbc.csv", stringsAsFactors = FALSE)

#Parte 1: Análisis de componentes principales

#Sacamos el prmedio de las mediciones por columnas
promedios <- data.frame(
  radio = rowMeans(dataset[, c(3, 13, 23)]),
  textura = rowMeans(dataset[, c(4, 14, 24)]),
  perimetro = rowMeans(dataset[, c(5, 15, 25)]),
  area = rowMeans(dataset[, c(6, 16, 26)]),
  suavidad = rowMeans(dataset[, c(7, 17, 27)]),
  compacidad = rowMeans(dataset[, c(8, 18, 28)]),
  concavidad = rowMeans(dataset[, c(9, 19, 29)]),
  puntos_concavos = rowMeans(dataset[, c(10, 20, 30)]),
  simetria = rowMeans(dataset[, c(11, 21, 31)]),
  dimension_fractal = rowMeans(dataset[, c(12, 22, 32)])
)
#Normalización de datos y eliminacion de datos numericos 

datos_normalizados <- scale(promedios)

#Aplicacion de pc
respca <- prcomp(datos_normalizados, scale = TRUE)

#Sacamos info del PC
names(respca)

head(respca$rotation)[,1:5]
print(head(respca$rotation)[,1:5])
dim(respca$rotation)

# summary(respca)
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
#PC1: Es negativo para todas las var previas
#PC2: 
#
#PC3: 
#PC4:

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


# Parte 2: Clustering con K-medoids

##chatgpt
# Seleccionar los componentes principales (por ejemplo, los primeros dos componentes)
pca_scores <- as.data.frame(respca$x[, 1:2])  # Usamos solo los dos primeros PCAs
pca_scores_numeric <- pca_scores[,1:2]
print(pca_scores_numeric)
#Aplicamos el metodo del codo 
fviz_nbclust(pca_scores_numeric, pam, method = "wss") +
  ggtitle("Método del Codo para K-medoids")
# Definir el número de clusters (por ejemplo, 3 clusters)
num_clusters <- 3

# Aplicar el algoritmo de k-medoids
set.seed(123)  # Para reproducibilidad
kmedoids_result <- pam(pca_scores_numeric, k = num_clusters) 
# # Añadir los clusters obtenidos al conjunto de datos de componentes principales
pca_scores$cluster <- as.factor(kmedoids_result$cluster)
# # Visualizar los clusters en el espacio PCA
fviz_cluster(list(data = pca_scores_numeric, cluster = kmedoids_result$cluster),
             geom = "point",
             ellipse.type = "norm",
             show.clust.cent = TRUE,
             palette = "jco",
             ggtheme = theme_minimal()) +
  ggtitle("Clustering de PCA con K-medoids") +
  theme_minimal()
