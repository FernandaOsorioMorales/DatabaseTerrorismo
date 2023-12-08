library(caTools)
library(neuralnet)
library(dplyr)
library(caret)
terrorismo <- read.csv(file.choose())

red1 <- subset(terrorismo, select = -c(X,eventid,city,
                                       target1, hostkidoutcome))

red1 <- subset(red1, red1$ishostkid==1)



# Y hagamos uno para ver si sobreviven todos
red1$sobre <- 0
red1$sobre <- ifelse(red1$nhostkid_log==red1$nreleased_log,1,red1$sobre)

table(red1$sobre)
red1 <- subset(red1,select =  c(iyear,country,targtype1,ndays,crit1,crit2,
                                crit3,nperps,
                                nhours,extended,nwound,weapsubtype1,
                                nhostkid_log,attacktype1,propextent,sobre))



#Creamos una muestra (porcentaje)
set.seed(306)

#Tomamos el 66% de las tuplas para entrenamiento, el resto para prueba
split = sample.split(red1$iyear, SplitRatio = 0.66)
summary(split)

#Partimos el dataset basado en el vector booleano split
entrena = subset(red1, split == TRUE)
prueba = subset(red1, split == FALSE)

summary(entrena)
summary(prueba)

#Antes de llamar a la función neuralnetwork(), necesitamos crear una 
#fórmula para insertar en el modelo de aprendizaje automático.
#La función neuralnetwork() no acepta el formato típico para una fórmula 
#que implique todas las características (Objetivo ~ .)

caracteristicas <- names(red1)
caracteristicas

caracteristicas <- caracteristicas[-which(names(red1) %in% c("sobre"))]
caracteristicas

# Concatenamos las cadenas
form <- paste(caracteristicas,collapse=' + ')
form

#Hacemos coincidir con el dataset de entrenamiento
form <- paste('sobre ~',form)
form

# Convertir a formula
form <- as.formula(form)
form

ncol(red1)
# Ahora si podemos hacer la red
#Entrenamos la red neuronal, hagamoslo con 2 capas usando la cantidad recomendada
# de neuronas

sobreviv<- neuralnet(form,entrena,hidden=c(6,4), linear.output = FALSE)


#Graficamos la red neuronal
plot(sobreviv)

#Veamos los resultados del entrenamiento
out <- cbind(sobreviv$covariate, + sobreviv$net.result[[1]])
head(out)

# Vamos a poner el nombre a la columna
out <- out %>% as.data.frame %>% 
  rename(sobrevivir = ncol(out))
head(out)


# Probamos el modelo entrenado
# Calcular las predicciones sobre el conjunto de prueba
prediccion <- predict(sobreviv,prueba)
#prediccion <- round(prediccion,1)
head(prediccion)

# Verificar los resultados
resultados_predichos <- as.factor(round(prediccion))

# Hacemos la matriz de confusion simple
matriz_confusion_simple <- table(Real = prueba$sobre, Prediccion = resultados_predichos)

# Mostrar la matriz de confusión simple
print(matriz_confusion_simple)


