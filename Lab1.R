# Cargar librerías necesarias
install.packages("modeest")  # Si no tienes instalada la librería para calcular la moda
library(modeest)

dataset <- read.csv("wdbc.csv", stringsAsFactors = FALSE)

# Separar los datos en benignos y malignos
benignos <- subset(dataset, Diagnosis == "B")
malignos <- subset(dataset, Diagnosis == "M")

# Seleccionar las columnas numéricas del dataset que deseas analizar
columnas_a_analizar_benignas <- benignos[, c("radius1", "radius2", "radius3", "texture1", "texture2", 
                                "texture3", "perimeter1", "perimeter2", "perimeter3", 
                                "area1", "area2", "area3", "smoothness1", "smoothness2", 
                                "smoothness3", "compactness1", "compactness2", "compactness3",
                                 "concavity1", "concavity2", "concavity3", "concave_points1", 
                                 "concave_points2", "concave_points3", "symmetry1", "symmetry2", 
                                 "symmetry3", "fractal_dimension1", "fractal_dimension2", "fractal_dimension3")]

columnas_a_analizar_malignas <- malignos[, c("radius1", "radius2", "radius3", "texture1", "texture2", 
                                "texture3", "perimeter1", "perimeter2", "perimeter3", 
                                "area1", "area2", "area3", "smoothness1", "smoothness2", 
                                "smoothness3", "compactness1", "compactness2", "compactness3",
                                 "concavity1", "concavity2", "concavity3", "concave_points1", 
                                 "concave_points2", "concave_points3", "symmetry1", "symmetry2", 
                                 "symmetry3", "fractal_dimension1", "fractal_dimension2", "fractal_dimension3")]

                                

# Calcular la media de cada columna
medias_benignas <- apply(columnas_a_analizar_benignas, 2, mean)
medias_malignas <- apply(columnas_a_analizar_malignas, 2, mean)

# # Calcular la mediana de cada columna
medianas_benignas <- apply(columnas_a_analizar_benignas, 2, median)
medianas_malignas <- apply(columnas_a_analizar_malignas, 2, median)
# # Calcular la moda de cada columna (usando la función mfv para calcular la moda)
modas_benignas <- apply(columnas_a_analizar_benignas, 2, function(x) mfv(x))             
modas_malignas <- apply(columnas_a_analizar_malignas, 2, function(x) mfv(x))
# # Mostrar los resultados
medias_benignas
medias_malignas
medianas_benignas
medianas_malignas
modas_benignas
modas_malignas