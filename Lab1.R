# Cargar librerías necesarias
install.packages("modeest")  # Si no tienes instalada la librería para calcular la moda
library(modeest)

datos3 <- read.csv("wdbc.csv", stringsAsFactors = FALSE)

# Seleccionar las columnas numéricas del dataset que deseas analizar
columnas_a_analizar <- datos3[, c("radius1", "texture1", "perimeter1", "area1", 
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