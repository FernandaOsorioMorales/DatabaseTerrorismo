#vamos a obtner la matriz de clasificacion para clasi_arbol
#asi que para que funcione se tendria que haber ejecutado todo el escript antes mencionado

# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(123) # Para reproducibilidad
indices <- sample(1:nrow(arbol), size = 0.7 * nrow(arbol)) # 70% de los datos para entrenamiento
entrenamiento <- arbol[indices, ]
prueba <- arbol[-indices, ]

# Entrenar el modelo
modelo <- rpart(crit1 ~ ., data = entrenamiento, control = rpart.control(cp=0.01), method = "class")

# Obtener las predicciones
predicciones <- predict(modelo, newdata = prueba, type = "class")

# Obtener la matriz de confusión
matriz_confusion <- table(prueba$crit1, predicciones)
print(matriz_confusion)

#y para el de clasi_red ya esta integrada esta parte de la matriz, pero igualmente la adjunto
#igualmente en el supuesto que ya se corrio todo lo anterior a ese codigo

# Verificar los resultados
resultados_predichos <- as.factor(round(prediccion))

# Hacemos la matriz de confusion simple
matriz_confusion_simple <- table(Real = prueba$sobre, Prediccion = resultados_predichos)

# Mostrar la matriz de confusión simple
print(matriz_confusion_simple)

#ahora queremos hacer un ROC para poder comparar nuestros modelos
install.packages("pROC")
library(pROC)
library(dplyr)
# Obtener las probabilidades predichas para el modelo de árbol
probs_arbol <- predict(dt.rpart, newdata = prueba, type = "prob")

# Obtener las probabilidades predichas para el modelo de red
probs_red <- compute(sobreviv, prueba[,1:(ncol(prueba)-1)])$net.result

# Crear las curvas ROC
roc_arbol <- roc(prueba$crit1, probs_arbol[,2])
roc_red <- roc(prueba$sobre, probs_red)

# Graficar las curvas ROC
plot(roc_arbol, col = "blue", main = "Curvas ROC")
lines(roc_red, col = "red")

# al quitar año a la red vemos que ya no convergue
legend("bottomright", legend = c("Árbol", "Red"), col = c("blue", "red"), lty = 1)

form <- paste('sobre ~',caracteristicas[-which(caracteristicas %in% c("año"))])

sobreviv_nuevo <- neuralnet(form,entrena,hidden=c(6,4), linear.output = FALSE)

roc.red_nuevo <- roc(red1$sobre, predict(sobreviv_nuevo, red1, type="class"))

library(caTools)
library(neuralnet)

# Cargamos los datos
arbol <- read.csv("datoslimpios.csv")
red1 <- read.csv("datoslimpios.csv")

# Creamos la fórmula
form <- paste('sobre ~',caracteristicas[-which(caracteristicas %in% c("año"))])

# Entrenamos el modelo
sobreviv_nuevo <- neuralnet(form,entrena,hidden=c(6,4), linear.output = FALSE)

# Calculamos el ROC
roc.red_nuevo <- roc(red1$sobre, predict(sobreviv_nuevo, red1, type="class"))

# Imprimimos el AUC
print(roc.red_nuevo$auc)


#
