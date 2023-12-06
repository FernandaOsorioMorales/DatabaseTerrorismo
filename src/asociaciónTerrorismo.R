#==============================================================================
#INICIAMOS CON LA PARTE DE LAS REGLAS DE ASOCIACIÓN
#==============================================================================
library(dplyr)

#Cargamos los datos que ya han sido limpiados
datos <- read.csv (file = file.choose())
head(datos)


#Analicemos una transacci?n
datos %>% filter(eventid == 10000) %>% pull(extended)

library(arules)
datosAs<- subset(datos, select = c("extended","country", "crit1",
                                   "crit2","crit3","doubtterr","multiple", "success","suicide","guncertain1", "individual","claimed",
                                   "property","ishostkid"))
write.csv(datosAs, file="datosAs.csv",  row.names = TRUE)

transacciones <- read.transactions(file= file.choose(),
                                   sep = ",",
                                   header = TRUE,
                                   encoding = "UTF-8",
                                   rm.duplicates = TRUE)

transacciones
#Checamos el contenido de las primeras cinco transacciones
colnames(transacciones)[1:5]
rownames(transacciones)[1:5]
#Inspeccionamos para ver a fondo de que se transforma
inspect(transacciones[1:5])

#Para extraer el tama?o de cada transacción se emplea la función size().
tamano <- size(transacciones)
summary(tamano)
library(ggplot2)

#Vamos a graficar el tama?o de las transacciones:
data.frame(tamano) %>%
  ggplot(aes(x = tamano)) +
  geom_histogram() +
  labs(title = "Distribucion del tamaño de las transacciones",
       x = "Tamaño") +
  theme_bw()

quantile(tamano,prob = seq(0,1,0.1))

#Vemos cuáles son los ítems más frecuentes
frecuencia_items <- itemFrequency(x = transacciones, type = "relative")
frecuencia_items

# Podemos generar una gráfica de barras de frecuencia, para ver la distribución de
# objetos: cuantas veces aparece un producto en comparación con otros.
library(RColorBrewer)
itemFrequencyPlot(transacciones,topN = 20, type = "relative",
                  col = brewer.pal(8,'Pastel2'),
                  main="Gráfica de frecuencia relativa de items")

frecuencia_items %>% sort(decreasing = TRUE) %>% head(5)
# Si se indica el argumento type = "absolute", la función itemFrequency() devuelve el
# número de transacciones en las que aparece cada item.

frecuencia_items <- itemFrequency(x = transacciones, type = "absolute")
frecuencia_items

# Podemos generar una gráfica de barras de frecuencia, para ver la distribución de
# objetos
itemFrequencyPlot(transacciones,topN = 20, type = "absolute",
                  col = brewer.pal(8,'Pastel2'),
                  main="Gráfica de frecuencia absoluta de items")

frecuencia_items %>% sort(decreasing = TRUE) %>% head(5)

#===============================================================================
# Aplicación algoritmo Apriori
#===============================================================================
#Buscamos los itemsets que hayan ocurrido al menos 20 veces
soporte <- 20 / dim(transacciones)[1]
soporte
itemsets <- apriori(data = transacciones,
                    parameter = list(support = soporte,
                                     minlen = 1,
                                     maxlen = 20,
                                     target = "frequent itemset"))
#Visualizamos los datos principales de los cerca de los 800 itemsets que nos salieron
summary(itemsets)

# Se muestran los top 20 itemsets de mayor a menor soporte
top_20_itemsets <- sort(itemsets, by = "support", decreasing = TRUE)[1:20]
inspect(top_20_itemsets)

#Tomamos los que contienen con mayor frecuencia a estados Unidos
itemsets_filtrado <- arules::subset(itemsets,
                                    subset = items %in% "217")
itemsets_filtrado

# Se muestran
inspect(itemsets_filtrado)

# Visualizamos el número de subsets que se encuentran dentro de su mismos expresados
#de forma diferente.
subsets <- is.subset(x = itemsets, y = itemsets, sparse = FALSE)
sum(subsets)

# ==============================================================================
# REGLAS DE ASOCIACI?N
# ==============================================================================

# Creamos una lista de soporte de apoyo y confianza
soporteLev <- c(0.003, 0.004, 0.007, 0.01)
confianzaLev <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)

# Se crean listas vacías de números enteros para almacenar la cantidad de reglas creadas
rules_sup03 <- integer(length=9)
rules_sup04 <- integer(length=9)
rules_sup07 <- integer(length=9)
rules_sup1 <- integer(length=9)


# Algoritmo Apriori con soporte del 0.3%
for (i in 1:length(confianzaLev)) {
  
  rules_sup03[i] <- length(apriori(transacciones, 
                                   parameter=list(sup=soporteLev[1],
                                                  conf=confianzaLev[i], 
                                                  target="rules")))
  
}

# Algoritmo Apriori con soporte del 0.4%
for (i in 1:length(confianzaLev)){
  
  rules_sup04[i] <- length(apriori(transacciones, 
                                   parameter=list(sup=soporteLev[2],
                                                  conf=confianzaLev[i], 
                                                  target="rules")))
  
}

# Algoritmo Apriori con soporte del 0.7%
for (i in 1:length(confianzaLev)){
  
  rules_sup07[i] <- length(apriori(transacciones, 
                                   parameter=list(sup=soporteLev[3],
                                                  conf=confianzaLev[i], 
                                                  target="rules")))
  
}

# Algoritmo Apriori con soporte del 1%
for (i in 1:length(confianzaLev)){
  
  rules_sup1[i] <- length(apriori(transacciones, 
                                  parameter=list(sup=soporteLev[4], 
                                                 conf=confianzaLev[i], 
                                                 target="rules")))
  
}

library(gridExtra) # provides a number of user-level functions to work with "grid" graphics

# N?mero de reglas encontradas con un soporte del 0.3%
plot1 <- aes(confianzaLev, rules_sup03, geom=c("point", "line"), 
             xlab="Nivel de Confianza", ylab="N?mero de reglas encontradas", 
             main="Apriori con soporte del 0.3%") +
  theme_bw()

# N?mero de reglas encontradas con un soporte del 0.4%
plot2 <- aes(confianzaLev, rules_sup04, geom=c("point", "line"), 
             xlab="Nivel de Confianza", ylab="N?mero de reglas encontradas", 
             main="Apriori con soporte del 0.4%") + 
  theme_bw()

# N?mero de reglas encontradas con un soporte del 0.7%
plot3 <- aes(confianzaLev, rules_sup07, geom=c("point", "line"), 
             xlab="Nivel de Confianza", ylab="N?mero de reglas encontradas", 
             main="Apriori con soporte del 0.7%") + 
  theme_bw()

# N?mero de reglas encontradas con un soporte del 1%
plot4 <- aes(confianzaLev, rules_sup1, geom=c("point", "line"), 
             xlab="Nivel de Confianza", ylab="N?mero de reglas encontradas", 
             main="Apriori con soporte del 1%") + 
  theme_bw()


# Subplot
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=2)
# Estos puntos se trazaron en un gr?fico de l?neas m?ltiples. Cada l?nea 
# representaba un valor de soporte.

# Data frame
num_reglas <- data.frame(rules_sup03, rules_sup04, rules_sup07, rules_sup1, 
                         rules_sup10, rules_sup25, confianzaLev)

# N?mero de reglas encontradas con soportes del 0.3%, 0.4%, 0.7%, 1%, 10%, 25%
ggplot(data=num_reglas, aes(x=confianzaLev)) +
  
  # GRaficar l?neas y puntos (soporte del 0.3%)
  geom_line(aes(y=rules_sup03, colour="Soporte del 0.3%")) + 
  geom_point(aes(y=rules_sup03, colour="Soporte del 0.3%")) +
  
  # GRaficar l?neas y puntos (soporte del 0.4%)
  geom_line(aes(y=rules_sup04, colour="Soporte del 0.4%")) + 
  geom_point(aes(y=rules_sup04, colour="Soporte del 0.4%")) +
  
  # GRaficar l?neas y puntos (soporte del 0.7%)
  geom_line(aes(y=rules_sup07, colour="Soporte del 0.7%")) + 
  geom_point(aes(y=rules_sup07, colour="Soporte del 0.7%")) +
  
  # GRaficar l?neas y puntos (soporte del 1%)
  geom_line(aes(y=rules_sup1, colour="Soporte del 1%")) + 
  geom_point(aes(y=rules_sup1, colour="Soporte del 1%")) +
  
  
  # Labs and theme
  labs(x="Niveles de Confianza", y="Número de reglas encontradas", 
       title="Algoritmo Apriori con diferentes niveles de Soporte") +
  theme_bw() +
  theme(legend.title=element_blank())

reglas <- apriori(data = transacciones,
                  parameter = list(support = soporteLev[1],
                                   confidence = confianzaLev[5],
                                   # Se especifica que se creen reglas
                                   target = "rules"))

summary(reglas)

r <- as_tibble(as(reglas, Class = "data.frame"))
r

# Inspeccionamos las reglas, ordenando por confianza
inspect(sort(x = reglas, decreasing = TRUE, by = "confidence"))

# Podemos ordenar el tibble
r %>% arrange(desc(confidence))

metricas <- interestMeasure(reglas, measure = c("coverage", "fishersExactTest"),
                            transactions = transacciones)

metricas <- interestMeasure(reglas, measure = c("fishersExactTest"),
                            transactions = transacciones)
metricas

quality(reglas) <- cbind(quality(reglas), metricas)
# inspect(sort(x = reglas, decreasing = TRUE, by = "confidence"))
df_reglas <- as(reglas, Class = "data.frame") 
df_reglas %>% as_tibble() %>% arrange(desc(confidence)) %>% head()

# ==============================================================================
# FILTRADO DE REGLAS DE ASOCIACI?N
# ==============================================================================

