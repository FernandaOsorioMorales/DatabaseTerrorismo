terrorismo <- read.csv("Documentos/SEMESTRE 5/Minería/PROYECTO/globalterrorismdb_0718dist.csv",na.strings = c("N/A"))
summary(terrorismo)

## ATRIBUTOS DEL 1 AL 27 FER ##
########################################################################
## eventid ##
sum(is.na(terrorismo$eventid)| terrorismo$eventid=="")/nrow(terrorismo)
summary(terrorismo$eventid)
#distribución
boxplot(terrorismo$eventid)


## iyear ##
sum(is.na(terrorismo$iyear)| terrorismo$iyear=="")/nrow(terrorismo)
summary(terrorismo$iyear)
#desviación estandar
sd(terrorismo$iyear,na.rm = TRUE)
#distribución
boxplot(terrorismo$iyear)


## imonth ##
sum(is.na(terrorismo$imonth)| terrorismo$imonth=="")/nrow(terrorismo)
summary(terrorismo$imonth)
#desviación estandar
sd(terrorismo$imonth,na.rm = TRUE)
#distribución
hist(terrorismo$imonth, breaks = 1000)


## iday ##
sum(is.na(terrorismo$iday)| terrorismo$iday=="")/nrow(terrorismo)
summary(terrorismo$iday)
#desviación estandar
sd(terrorismo$iday,na.rm = TRUE)
#distribución
hist(terrorismo$iday, breaks = 1000)

## approxdate##
sum(is.na(terrorismo$approxdate)| terrorismo$approxdate=="")/nrow(terrorismo)
summary(terrorismo$approxdate)
# Vemos las frecuencias
table(terrorismo$approxdate)/nrow(terrorismo)

## extended##
sum(is.na(terrorismo$extended)| terrorismo$extended=="")/nrow(terrorismo)
summary(terrorismo$approxdate)
#Vemos las frecuencias
table(terrorismo$extended)/nrow(terrorismo)

## resolution ##
sum(is.na(terrorismo$resolution)| terrorismo$resolution=="")/nrow(terrorismo)
summary(terrorismo$resolution)
#Vemos las frecuencias
table(terrorismo$resolution)/nrow(terrorismo)

## country ##
sum(is.na(terrorismo$country)| terrorismo$country=="")/nrow(terrorismo)
summary(terrorismo$country)
#distribución
hist(terrorismo$country, breaks = 1000)

## country_txt ##
sum(is.na(terrorismo$country_txt)| terrorismo$country_txt=="")/nrow(terrorismo)
summary(terrorismo$country_txt)
#frecuencias
table(terrorismo$country_txt)/nrow(terrorismo)

## region ##
sum(is.na(terrorismo$region)| terrorismo$region=="")/nrow(terrorismo)
summary(terrorismo$region)
#desviacion
sd(terrorismo$region,na.rm = TRUE)
#frecuencias
table(terrorismo$region)/nrow(terrorismo)

## region_txt ##
sum(is.na(terrorismo$region_txt)| terrorismo$region_txt=="")/nrow(terrorismo)
summary(terrorismo$region_txt)
#frecuencias
table(terrorismo$region_txt)/nrow(terrorismo)

## provstate ##
sum(is.na(terrorismo$provstate)| terrorismo$provstate=="")/nrow(terrorismo)
summary(terrorismo$provstate)
#frecuencias
table(terrorismo$provstate)/nrow(terrorismo)

## city ##
sum(is.na(terrorismo$city)| terrorismo$city=="")/nrow(terrorismo)
summary(terrorismo$city)
#frecuencias
table(terrorismo$city)/nrow(terrorismo)

## latitude ##
sum(is.na(terrorismo$latitude)| terrorismo$city=="")/nrow(terrorismo)
summary(terrorismo$latitude)
#desviacion
sd(terrorismo$latitude,na.rm = TRUE)
#distribución
hist(terrorismo$latitude, breaks = 1000)

## longitude ##
sum(is.na(terrorismo$longitude)| terrorismo$longitude=="")/nrow(terrorismo)
summary(terrorismo$longitude)
#desviacion
sd(terrorismo$longitude,na.rm = TRUE)
#distribución
hist(terrorismo$longitude, breaks = 1000)

## specifity ##
sum(is.na(terrorismo$specificity)| terrorismo$specificity=="")/nrow(terrorismo)
summary(terrorismo$specificity)
#frecuencias
table(terrorismo$specificity)/nrow(terrorismo)

## vicinity ##
sum(is.na(terrorismo$vicinity)| terrorismo$vicinity=="")/nrow(terrorismo)
summary(terrorismo$vicinity)
#frecuencias
table(terrorismo$vicinity)/nrow(terrorismo)

## location ##
sum(is.na(terrorismo$location)| terrorismo$location=="")/nrow(terrorismo)
summary(terrorismo$location)
#frecuencias
table(terrorismo$location)/nrow(terrorismo)

## summary ##
sum(is.na(terrorismo$summary)| terrorismo$summary=="")/nrow(terrorismo)
summary(terrorismo$summary)
#frecuencias
table(terrorismo$summary)/nrow(terrorismo)


##crit1 ##
sum(is.na(terrorismo$crit1)| terrorismo$crit1=="")/nrow(terrorismo)
summary(terrorismo$crit1)
#frecuencias
table(terrorismo$crit1)/nrow(terrorismo)

##crit2 ##
sum(is.na(terrorismo$crit2)| terrorismo$crit2=="")/nrow(terrorismo)
summary(terrorismo$crit2)
#frecuencias
table(terrorismo$crit2)/nrow(terrorismo)

##crit3 ##
sum(is.na(terrorismo$crit3)| terrorismo$crit3=="")/nrow(terrorismo)
summary(terrorismo$crit3)
#frecuencias
table(terrorismo$crit3)/nrow(terrorismo)

##doubtter##
sum(is.na(terrorismo$doubtterr)| terrorismo$doubtterr=="")/nrow(terrorismo)
summary(terrorismo$doubtterr)
#frecuencias
table(terrorismo$doubtterr)/nrow(terrorismo)

##alternative##
sum(is.na(terrorismo$alternative)| terrorismo$alternative=="")/nrow(terrorismo)
summary(terrorismo$alternative)
#frecuencias
table(terrorismo$alternative)/nrow(terrorismo)

##alternative_txt##
sum(is.na(terrorismo$alternative_txt)| terrorismo$alternative_txt=="")/nrow(terrorismo)
summary(terrorismo$alternative_txt)
#frecuencias
table(terrorismo$alternative_txt)/nrow(terrorismo)

##multiple ##
sum(is.na(terrorismo$multiple)| terrorismo$multiple=="")/nrow(terrorismo)
summary(terrorismo$multiple)
#frecuencias
table(terrorismo$multiple)/nrow(terrorismo)

##success ##
sum(is.na(terrorismo$success)| terrorismo$success=="")/nrow(terrorismo)
summary(terrorismo$success)
#frecuencias
table(terrorismo$succes)/nrow(terrorismo)

## Atributos Pepe 28-54 ##
############################################################################
subDatos <- terrorismo[,28:54]

#Suicide
summary(subDatos$suicide)
# Vemos los faltantes
sum(is.na(subDatos$suicide)| subDatos$suicide=="")/nrow(subDatos)

#attacktype1
summary(subDatos$attacktype1)
# Vemos los faltantes
sum(is.na(subDatos$attacktype1)| subDatos$attacktype1=="")/nrow(subDatos)

#attacktype1_txt
summary(subDatos$attacktype1_txt)
# Vemos los faltantes
sum(is.na(subDatos$attacktype1_txt)| subDatos$attacktype1_txt=="")/nrow(subDatos)

#attacktype2
summary(subDatos$attacktype2)
# Vemos los faltantes
sum(is.na(subDatos$attacktype2)| subDatos$attacktype2=="")/nrow(subDatos)

#attacktype2_txt
# Vemos los faltantes
sum(is.na(subDatos$attacktype2_txt)| subDatos$attacktype2_txt=="")/nrow(subDatos)


#attacktype3
summary(subDatos$attacktype3)
# Vemos los faltantes
sum(is.na(subDatos$attacktype3)| subDatos$attacktype3=="")/nrow(subDatos)

#attacktype3_txt
# Vemos los faltantes
sum(is.na(subDatos$attacktype3_txt)| subDatos$attacktype3_txt=="")/nrow(subDatos)

#targtype1
summary(subDatos$targtype1)
# Vemos los faltantes
sum(is.na(subDatos$targtype1)| subDatos$targtype1=="")/nrow(subDatos)

#targtype1_txt
# Vemos los faltantes
sum(is.na(subDatos$targtype1_txt)| subDatos$targtype1_txt=="")/nrow(subDatos)

#targsubtype1
summary(subDatos$targsubtype1)
# Vemos los faltantes
sum(is.na(subDatos$targsubtype1)| subDatos$targsubtype1=="")/nrow(subDatos)

#targsubtype1_txt
# Vemos los faltantes
sum(is.na(subDatos$targsubtype1_txt)| subDatos$targsubtype1_txt=="")/nrow(subDatos)

#corp1
# Vemos los faltantes
sum(is.na(subDatos$corp1)| subDatos$corp1=="")/nrow(subDatos)

#target1
# Vemos los faltantes
sum(is.na(subDatos$target1)| subDatos$target1=="")/nrow(subDatos)

#natlty1
summary(subDatos$natlty1)
# Vemos los faltantes
sum(is.na(subDatos$natlty1)| subDatos$natlty1=="")/nrow(subDatos)

#natlty1_txt
# Vemos los faltantes
sum(is.na(subDatos$natlty1_txt)| subDatos$natlty1_txt=="")/nrow(subDatos)

#targtype2
# Vemos los faltantes
sum(is.na(subDatos$targtype2)| subDatos$targtype2=="")/nrow(subDatos)

#targtype2_txt
# Vemos los faltantes
sum(is.na(subDatos$targtype2_txt)| subDatos$targtype2_txt=="")/nrow(subDatos)

#targsubtype2
# Vemos los faltantes
sum(is.na(subDatos$targsubtype2)| subDatos$targsubtype2=="")/nrow(subDatos)

#targsubtype2_txt
# Vemos los faltantes
sum(is.na(subDatos$targsubtype2_txt)| subDatos$targsubtype2_txt=="")/nrow(subDatos)

#corp2
# Vemos los faltantes
sum(is.na(subDatos$corp2)| subDatos$corp2=="")/nrow(subDatos)

#target2
# Vemos los faltantes
sum(is.na(subDatos$target2)| subDatos$target2=="")/nrow(subDatos)

#natlty2
# Vemos los faltantes
sum(is.na(subDatos$natlty2)| subDatos$natlty2=="")/nrow(subDatos)

#natlty2_txt
# Vemos los faltantes
sum(is.na(subDatos$natlty2_txt)| subDatos$natlty2_txt=="")/nrow(subDatos)

#targtype3
# Vemos los faltantes
sum(is.na(subDatos$targtype3)| subDatos$targtype3=="")/nrow(subDatos)

#targtype3_txt
# Vemos los faltantes
sum(is.na(subDatos$targtype3_txt)| subDatos$targtype3_txt=="")/nrow(subDatos)

#targsubtype3
# Vemos los faltantes
sum(is.na(subDatos$targsubtype3)| subDatos$targsubtype3=="")/nrow(subDatos)

#targsubtype3_txt
# Vemos los faltantes
sum(is.na(subDatos$targsubtype3_txt)| subDatos$targsubtype3_txt=="")/nrow(subDatos)

#########################################################################
#Análisis Adrian
# Función para calcular estadísticas descriptivas
calcular_estadisticas_descriptivas <- function(data, columna) {
  # Filtra los valores no perdidos en la columna especificada
  valores_no_perdidos <- data[!is.na(data[[columna]]), columna]
  
  # Calcula las estadísticas
  valor_minimo <- min(valores_no_perdidos)
  valor_maximo <- max(valores_no_perdidos)
  media <- mean(valores_no_perdidos)
  desviacion_estandar <- sd(valores_no_perdidos)
  
  # Devuelve los resultados
  return(list(Min = valor_minimo, Max = valor_maximo, Media = media, SD = desviacion_estandar))
}

# Llama a la función para la columna 'natltly3'
resultados_natltly3 <- calcular_estadisticas_descriptivas(data, 'natltly3')

# Imprime los resultados
cat("Valor mínimo: ", resultados_natltly3$Min, "\n")
cat("Valor máximo: ", resultados_natltly3$Max, "\n")
cat("Media: ", resultados_natltly3$Media, "\n")
cat("Desviación estándar: ", resultados_natltly3$SD, "\n")

# Cargar la librería tidyverse
library(tidyverse)

# Ruta del archivo CSV
ruta <- "C:/Users/gary1/OneDrive/Escritorio/ProyectoAyMD/archive/globalterrorismdb_0718dist.csv"

# Leer el archivo CSV
datos <- read_csv(ruta)

# Contar valores nulos en la columna corp3
nulos_corp3 <- sum(is.na(datos$corp3))

# Contar celdas que contienen "Unknow" en la columna corp3
unknown_corp3 <- sum(datos$corp3 == "Unknown")

# Mostrar los resultados
cat("Cantidad de valores nulos en corp3:", nulos_corp3, "\n")
cat("Cantidad de celdas con 'Unknown' en corp3:", unknown_corp3, "\n")

# Reemplazamos los valores -99 y "Unknown" por NA
data$nperpcap[data$nperpcap == -99 | data$nperpcap == "Unknown"] <- NA

# Calculamos el porcentaje de valores perdidos
missing_values <- sum(is.na(data$nperpcap))
total_values <- length(data$nperpcap)
percentage_missing <- (missing_values / total_values) * 100

# Imprimimos el resultado
print(paste("El porcentaje de valores perdidos en la columna 'nperpcap' es", percentage_missing, "%"))

# Calculamos el mínimo, el máximo, la media y la desviación estándar
min_value <- min(data$nperpcap, na.rm = TRUE)
max_value <- max(data$nperpcap, na.rm = TRUE)
mean_value <- mean(data$nperpcap, na.rm = TRUE)
sd_value <- sd(data$nperpcap, na.rm = TRUE)

# Imprimimos los resultados
print(paste("El valor mínimo es", min_value))
print(paste("El valor máximo es", max_value))
print(paste("La media es", mean_value))
print(paste("La desviación estándar es", sd_value))

##últimos atributos Delfin##
##########################################################################
## propcomment ##
# Veamos cuantos missing hay
sum(is.na(terrorismo$propcomment)| terrorismo$propcomment=="")/nrow(terrorismo)


## ishostkid ##
# Vemos los faltantes
sum(is.na(terrorismo$ishostkid)|terrorismo$ishostkid == -9)/nrow(terrorismo)

#Veamos sus frecuencias y niveles
table(terrorismo$ishostkid)/nrow(terrorismo)


## nhostkid ##
# vemos los faltantes
sum(is.na(terrorismo$nhostkid)|terrorismo$nhostkid < 0)/nrow(terrorismo)

#sacamos sus maximos, minimos, etc.
summary(terrorismo$nhostkid)
sd(terrorismo$nhostkid,na.rm = TRUE)

# Veamos su distribucion
barplot(table(subset(terrorismo$nhostkid,!is.na(terrorismo$nhostkid)& terrorismo$nhostkid!=-99)))


## nhostkidus ##
# vemos los faltantes
sum(is.na(terrorismo$nhostkidus)|terrorismo$nhostkidus < 0)/nrow(terrorismo)

# Veamos sus maximos, min, etc.
summary(terrorismo$nhostkidus)
sd(terrorismo$nhostkidus,na.rm = TRUE)

# Veamos su distribucion
barplot(table(subset(terrorismo$nhostkidus,!is.na(terrorismo$nhostkidus)& terrorismo$nhostkidus!=-99)))


## nhours ##
# vemos los faltantes
sum(is.na(terrorismo$nhours)|terrorismo$nhours < 0)/nrow(terrorismo)

# Veamos sus maximos, min, etc.
summary(terrorismo$nhours)
sd(terrorismo$nhours,na.rm = TRUE)

# Veamos su distribucion
barplot(table(subset(terrorismo$nhours,!is.na(terrorismo$nhours)& terrorismo$nhours>=0)))


## ndays ##
sum(is.na(terrorismo$ndays)|terrorismo$ndays < 0)/nrow(terrorismo)

# Veamos sus maximos, min, etc.
summary(terrorismo$ndays)
sd(terrorismo$ndays,na.rm = TRUE)

# Veamos su distribucion
barplot(table(subset(terrorismo$ndays,!is.na(terrorismo$ndays)& terrorismo$ndays>=0)))


## divert ##
# Veamos cuantos missing hay
sum(is.na(terrorismo$divert)| terrorismo$divert=="")/nrow(terrorismo)


## kidhijcountry ##
# Veamos cuantos missing hay
sum(is.na(terrorismo$kidhijcountry)| terrorismo$kidhijcountry=="")/nrow(terrorismo)


## ransom ##
sum(is.na(terrorismo$ransom)|terrorismo$ransom == -9)/nrow(terrorismo)

#Veamos sus frecuencias y niveles
table(terrorismo$ishostkid)/nrow(terrorismo)


## ransomamt ##
# vemos los faltantes
sum(is.na(terrorismo$ransomamt)|terrorismo$ransomamt == -99)/nrow(terrorismo)

#sacamos sus maximos, minimos, etc.
summary(terrorismo$ransomamt)
sd(terrorismo$ransomamt,na.rm = TRUE)

# Veamos su distribucion
barplot(table(subset(terrorismo$ransomamt,!is.na(terrorismo$ransomamt)& terrorismo$ransomamt!=-99)))


## ransomamtus ##
# vemos los faltantes
sum(is.na(terrorismo$ransomamtus)|terrorismo$ransomamtus < 0)/nrow(terrorismo)

#sacamos sus maximos, minimos, etc.
summary(terrorismo$ransomamtus)
sd(terrorismo$ransomamtus,na.rm = TRUE)

# Veamos su distribucion
barplot(table(subset(terrorismo$ransomamtus,!is.na(terrorismo$ransomamtus)& terrorismo$ransomamtus>=0)))


## ransompaid ##
# vemos los faltantes
sum(is.na(terrorismo$ransompaid)|terrorismo$ransompaid < 0)/nrow(terrorismo)

#sacamos sus maximos, minimos, etc.
summary(terrorismo$ransompaid)
sd(terrorismo$ransompaid,na.rm = TRUE)

# Veamos su distribucion
barplot(table(subset(terrorismo$ransompaid,!is.na(terrorismo$ransompaid)& terrorismo$ransompaid>=0)))


## ransompaidus ##
# vemos los faltantes
sum(is.na(terrorismo$ransompaidus)|terrorismo$ransompaidus < 0)/nrow(terrorismo)

#sacamos sus maximos, minimos, etc.
summary(terrorismo$ransompaidus)
sd(terrorismo$ransompaidus,na.rm = TRUE)

# Veamos su distribucion
barplot(table(subset(terrorismo$ransompaid,!is.na(terrorismo$ransompaid)& terrorismo$ransompaid>=0)))


## propcomment ##
# Veamos cuantos missing hay
sum(is.na(terrorismo$ransomnote)| terrorismo$ransomnote=="")/nrow(terrorismo)


## hostkidoutcome ##
sum(is.na(terrorismo$hostkidoutcome)|terrorismo$hostkidoutcome == 7)/nrow(terrorismo)

#Veamos sus frecuencias y niveles
table(terrorismo$hostkidoutcome)/nrow(terrorismo)


## hostkidoutcome ##
sum(is.na(terrorismo$hostkidoutcome_txt)|terrorismo$hostkidoutcome_txt == "" | 
      terrorismo$hostkidoutcome_txt=="Unknown")/nrow(terrorismo)

#Veamos sus frecuencias y niveles
table(terrorismo$hostkidoutcome_txt)/nrow(terrorismo)


## ransompaidus ##
# vemos los faltantes
sum(is.na(terrorismo$nreleased)|terrorismo$nreleased < 0)/nrow(terrorismo)

#sacamos sus maximos, minimos, etc.
summary(terrorismo$nreleased)
sd(terrorismo$nreleased,na.rm = TRUE)

# Veamos su distribucion
barplot(table(subset(terrorismo$nreleased,!is.na(terrorismo$nreleased)& terrorismo$nreleased>=0)))


## addnotes ##
# Veamos cuantos missing hay
sum(is.na(terrorismo$addnotes)| terrorismo$addnotes=="")/nrow(terrorismo)


## scite1 ##
# Veamos cuantos missing hay
sum(is.na(terrorismo$scite1)| terrorismo$scite1=="")/nrow(terrorismo)


## scite2 ##
# Veamos cuantos missing hay
sum(is.na(terrorismo$scite2)| terrorismo$scite2=="")/nrow(terrorismo)


## scite3 ##
# Veamos cuantos missing hay
sum(is.na(terrorismo$scite3)| terrorismo$scite3=="")/nrow(terrorismo)


## dbsource ##
# Veamos cuantos missing hay
sum(is.na(terrorismo$dbsource)| terrorismo$dbsource=="")/nrow(terrorismo)


## INT_LOG ##
# Vemos los faltantes
sum(is.na(terrorismo$INT_LOG)|terrorismo$INT_LOG == -9)/nrow(terrorismo)

#Veamos sus frecuencias y niveles
table(terrorismo$INT_LOG)/nrow(terrorismo)


## INT_IDEO ##
# Vemos los faltantes
sum(is.na(terrorismo$INT_IDEO)|terrorismo$INT_IDEO == -9)/nrow(terrorismo)

#Veamos sus frecuencias y niveles
table(terrorismo$INT_IDEO)/nrow(terrorismo)


## INT_MISC ##
# Vemos los faltantes
sum(is.na(terrorismo$INT_MISC)|terrorismo$INT_MISC == -9)/nrow(terrorismo)

#Veamos sus frecuencias y niveles
table(terrorismo$INT_MISC)/nrow(terrorismo)


## INT_ANY ##
# Vemos los faltantes
sum(is.na(terrorismo$INT_ANY)|terrorismo$INT_ANY == -9)/nrow(terrorismo)

#Veamos sus frecuencias y niveles
table(terrorismo$INT_ANY)/nrow(terrorismo)


## related ##
# Veamos cuantos missing hay
sum(is.na(terrorismo$related)| terrorismo$related=="")/nrow(terrorismo)