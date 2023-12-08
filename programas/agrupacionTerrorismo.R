# Carga las librerías necesarias
library(dplyr)
library(tidyr)
library(cluster)
library(factoextra) # Para el análisis de clúster
library(fastDummies)

data <- read.csv("./globalterrorismdb_0718dist.csv",fileEncoding = "Latin1")

terrorismo<-data
#Filtrando datos
terrorismo <- data[data$nkill <=4 & data$nkill >= 1, ]
#reindexando
rownames(terrorismo) <- 1:nrow(terrorismo)
# Calculando la media de la columna "longitude", excluyendo los NA
media_longitude <- mean(terrorismo$longitude, na.rm = TRUE)
terrorismo$longitude[is.na(terrorismo$longitude)] <- media_longitude
media_latitude <- mean(terrorismo$latitude, na.rm = TRUE)
terrorismo$latitude[is.na(terrorismo$latitude)] <- media_latitude

#Normalizando
terrorismo <- terrorismo %>%
  mutate(across(c("nkill"), ~ ifelse(is.na(.), 0, log(.)))) %>%
  mutate(across(c("nkill","longitude","latitude"), ~ (. - min(.)) / (max(.) - min(.))))


features <- c('longitude', 'latitude','nkill')
#haciendo un data frame con nuestras caracteríasticas
X <- terrorismo %>%
  select(all_of(features)) %>%
  as.data.frame()
names(X)


print(paste('Shape:', dim(X)))

# 3. Clustering y Evaluación de Silhouette
set.seed(123)
kmeans_result <- kmeans(X, centers =16, nstart = 25)
fviz_cluster(kmeans_result, data=X)
terrorismo$Cluster <- kmeans_result$cluster
X$cluster<-kmeans_result$cluster


# Cálculo del puntaje de Silhouette
set.seed(123)  # semilla
sample_indices <- sample(nrow(X), 20000)
X_sample <- X[sample_indices, ]

cluster_assignments_sample <- kmeans_result$cluster[sample_indices]
library(cluster)  # Make sure you have this package

# Matriz de distancias para la muestra
dist_matrix_sample <- dist(X_sample)

# Puntaje de silueta
silhouette_score_sample <- silhouette(cluster_assignments_sample, dist_matrix_sample)
avg_silhouette_score_sample <- mean(silhouette_score_sample[, 3]) * 100
print(paste('Silhouette Score for Sample:', round(avg_silhouette_score_sample, 2), '%'))


#Perfilado
aggregate(X, by=list(kmeans_result$cluster), FUN=mean)

calcular_estadisticas <- function(x) {
  c(media = mean(x), varianza = var(x))
}
estadisticas_clusters <- aggregate(. ~ cluster, data = X, FUN = calcular_estadisticas)

#Reescalando las regiones
estadisticas_clusters$latitud[,1]*(max(data$latitude,na.rm=TRUE)-min(data$latitude,na.rm=TRUE)+min(data$latitude,na.rm=TRUE))
estadisticas_clusters$longitude[,1]*(max(data$longitude,na.rm=TRUE)-min(data$longitude,na.rm=TRUE)+min(data$longitude,na.rm=TRUE))
