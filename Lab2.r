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
  radio = (dataset[, c(3)]),
  textura = (dataset[, c(4)]),
  perimetro = (dataset[, c(5)]),
  area = (dataset[, c(6)]),
  suavidad = (dataset[, c(7)]),
  compacidad = (dataset[, c(8)]),
  concavidad = (dataset[, c(9)]),
  puntos_concavos = (dataset[, c(10)]),
  simetria = (dataset[, c(11)]),
  dimension_fractal = (dataset[, c(12)])
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


######################################################### ANALISIS
#Matriz de covarianza
matriz_covarianza <- cov(datos_normalizados)

#Eigenvectors
eigen_result <- eigen(matriz_covarianza)
eigen_valores <- eigen_result$values
eigen_vectores <- eigen_result$vectors

print(eigen_vectores)

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


#Analisis PREELIMINAR

#Podemos ver que el 91,625% de los datos se eplican con los primeros 4 PC y los dos primeros explican el 77,55%
#y al 4to PC mantenemos el 80% de la variacion original.
#PC1: Es negativo para todas las variables previas
#PC2: Positivo con area, perimetro, radio. Indiferente con textura puntos concavos, 
#concavidad. Negativo con suavidad, compacidad, simetria y muy negativo con dimension fractal. 
#PC3: neutro con todo y  muy negativo con textura. (se define como el antonimo de textura)
#PC4: neutral con todo menos, positivo con dimension fractal y muy negativo con simetria (se define como el antonimo de simetria)


# Parte 2: Clustering con K-medoids

# Agregar la columna de diagnosis al conjunto de datos de promedios
promedios$Diagnosis <- dataset$Diagnosis

# Seleccionar los componentes principales (por ejemplo, los primeros dos componentes)
pca_scores <- as.data.frame(respca$x[, 1:2])  # Usamos solo los dos primeros PCAs
# pca_scores_numeric <- pca_scores[,1:2]
pca_scores$Diagnosis <- as.factor(promedios$Diagnosis)  # Agregar diagnosis como factor para el color en el gráfico
pca_scores_numeric <- pca_scores[,1:2]

#Aplicamos el metodo del codo 
fviz_nbclust(pca_scores_numeric, pam, method = "wss") +
  ggtitle("Método del Codo para K-medoids")
# Definir el número de clusters (por ejemplo, 3 clusters)
num_clusters <- 2

# Aplicar el algoritmo de k-medoids
set.seed(123)  # Para reproducibilidad
kmedoids_result <- pam(pca_scores, k = num_clusters) 
# # Añadir los clusters obtenidos al conjunto de datos de componentes principales
pca_scores$cluster <- as.factor(kmedoids_result$cluster)
# # Visualizar los clusters en el espacio PCA

# ggplot(pca_scores, aes(x = PC1, y = PC2, color = Diagnosis, shape = cluster)) +
#   geom_point(size = 3) +
#   scale_color_manual(values = c("blue", "green"), labels = c("Benigno", "Maligno")) +
#   scale_shape_manual(values = c(16, 17)) +  # Diferentes símbolos para cada cluster
#   labs(title = "Biplot de PCA: Clasificación Benigno/Maligno con Clusters",
#        x = "Componente Principal 1",
#        y = "Componente Principal 2",
#        color = "Diagnóstico",
#        shape = "Cluster") +
#   stat_ellipse(aes(group = cluster), type = "norm", linetype = 2) +  # Añade elipses para los clusters
#   theme_minimal()

ggplot(pca_scores, aes(x = PC1, y = PC2, shape = cluster)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("blue", "green"), labels = c("Benigno", "Maligno")) +
  scale_shape_manual(values = c(16, 17)) +  # Diferentes símbolos para cada cluster
  labs(title = "Biplot de PCA: Clasificación Benigno/Maligno con Clusters",
       x = "Componente Principal 1",
       y = "Componente Principal 2",
       color = "Diagnóstico",
       shape = "Cluster") +
  stat_ellipse(aes(group = cluster), type = "norm", linetype = 2) +  # Añade elipses para los clusters
  theme_minimal()