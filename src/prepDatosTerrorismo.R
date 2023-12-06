#----
#usamos las librerias:
library(mice)
library(missForest)
library(ggplot2)
library(ggcorrplot)
library(dplyr)
library(Hmisc)
library(funModeling)
library(stringr)
library(stringr)

#Cargamos los datos
terrorismo <- read.csv("globalterrorismdb_0718dist.csv")


# Empecemos con la primera parte de los datos.
#--------
# Todos los datos en esta seccion estan completos, excepto por doubtterr y 
# multiple
sum(is.na(terrorismo$multiple)|terrorismo$multiple<0)
sum(is.na(terrorismo$doubtterr)|terrorismo$doubtterr<0)
# Ya que no son tantos datos, nos conviene quitar todos aquellos que quedan
# desconocidos
terrorismo <- subset(terrorismo, !(is.na(terrorismo$doubtterr)|
                                         terrorismo$doubtterr<0)&
                       !(is.na(terrorismo$multiple)))
nrow(terrorismo)
rm(misAtributos)

# Vamos con la segunda--------

# Para la columna de sucidio no se realiza ningun cambio

# Para attacktype1
# no nos conviene borrar directamente, por lo que usaremos
# la libreria mice
sum(terrorismo$attacktype1==9|is.na(terrorismo$attacktype1))
# Hagamos todos los 9 (UNKOWN) en NA
terrorismo$attacktype1 <- ifelse(terrorismo$attacktype1==9,
                            NA, terrorismo$attacktype1)

terrorismo$attacktype1 <- as.factor(terrorismo$attacktype1)
ataque.mis <- subset(terrorismo,
                     select = c(iyear,imonth,extended,
                                  country_txt,crit1,crit2,crit3,
                                  doubtterr,multiple,success,
                                  suicide,attacktype1))

ataque_imp <- mice(ataque.mis, m=1, maxit = 5, 
        method = "polyreg", seed = 374)


terrorismo$attacktype1 <- c(complete(ataque_imp)$attacktype1)

# Ahora para targtype1, targsubtype1, creemos una categoria nueva para ellos
# así todos los unkown son other y en el subtype se hacen como otra cat
sum(terrorismo$targtype1==20)
# Hagamos primero una categoria para todos los subtipos desconocidos
terrorismo$targsubtype1 <- ifelse(terrorismo$targtype1==20,112,terrorismo$targsubtype1)

# Ahora sí transformemos a los que son unkown como otros
terrorismo$targtype1 <- ifelse(terrorismo$targtype1==20,13,terrorismo$targtype1)

# Podemos ver que nos siguen quedando valores perdidos para algunos subtipos.
sum(is.na(terrorismo$targsubtype1))

# Podemos usar que ya tenemos una categoria superior para predecir que subcategoria
# es. Para ello usemos un bosque aleatorio para llenar esos datos.
subtype_mis <- aregImpute(~ targtype1+targsubtype1, 
                        data = terrorismo[c("targtype1","targsubtype1")], 
                        n.impute = 5)

terrorismo$targsubtype1[is.na(terrorismo$targsubtype1)] <- 
  matrix(subtype_mis$imputed$targsubtype1[,3], ncol = 1)


# Ahora para target1 no tenemos tantos valores perdidos, así que podemos simple-
# mente borrarlos.
sum(is.na(terrorismo$target1))
sum(terrorismo$target1==""|terrorismo$target1=="Unknown",na.rm = TRUE)

terrorismo <- subset(terrorismo, !(is.na(terrorismo$target1)|
                                     terrorismo$target1==""|
                                     terrorismo$target1=="Unknown"))

# Ahora para natlty1 tenemos pocos perdidos, entonces podemos borrarlos
sum(is.na(terrorismo$natlty1)|terrorismo$natlty1<0)
terrorismo <- subset(terrorismo, !(is.na(terrorismo$natlty1)|
                                     terrorismo$natlty1<0))


nrow(terrorismo)

#Vamos con la tercera-----

# Para gname vemos que hay vacios ni perdidos
sum(terrorismo$gname==""|is.na(terrorismo$gname))
# Pero un gran pedazo es desconocido
sum(terrorismo$gname=="Unknown")

#Para guncertain1 hacemos solo tenemos vacios
sum(is.na(terrorismo$guncertain1))
# Podemos hacer que si es Unknown sean 0
terrorismo$guncertain1 <- ifelse(terrorismo$gname=="Unknown",0,terrorismo$guncertain1)

# Como son muy pocas podemos eliminarlas
terrorismo <- subset(terrorismo,!(is.na(terrorismo$guncertain1)))


# Para individual, no hacemos nada.
# Para nperps y nperpcap, podemos hacer para los vacios que la cantidad capturada
# sea la que habia
terrorismo$nperps[(is.na(terrorismo$nperps)|terrorismo$nperps<0)&
                    terrorismo$nperpcap>0&
                    !(is.na(terrorismo$nperpcap))] <- 
  terrorismo$nperpcap[(is.na(terrorismo$nperps)|terrorismo$nperps<0)&
                        terrorismo$nperpcap>0&
                        !(is.na(terrorismo$nperpcap))]

# Hagamos tambien que si son mayores los capturados a los que se registran
# que se registre entonces ese numero
terrorismo$nperps[!(is.na(terrorismo$nperps))&terrorismo$nperps>=0&
                    terrorismo$nperpcap>terrorismo$nperps&
                    !(is.na(terrorismo$nperpcap))] <- 
  terrorismo$nperpcap[!(is.na(terrorismo$nperps))&terrorismo$nperps>=0&
                        terrorismo$nperpcap>terrorismo$nperps&
                        !(is.na(terrorismo$nperpcap))]

# Podemos usar el numero de muertos de perpetradores y lastimados 
# para marcar el numero de
# perpetradores y hacer el numero de capturados 0 (pues fueron asesinados)
terrorismo$nperps[(is.na(terrorismo$nperps)|terrorismo$nperps<0)&
                    !(is.na(terrorismo$nkillter))&terrorismo$nkillter>0&
                    (is.na(terrorismo$nwoundte)|
                       terrorismo$nwoundte<=0)] <- 
  terrorismo$nkillter[(is.na(terrorismo$nperps)|terrorismo$nperps<0)&
                        !(is.na(terrorismo$nkillter))&terrorismo$nkillter>0&
                        (is.na(terrorismo$nwoundte)|
                           terrorismo$nwoundte<=0)]
terrorismo$nperpcap[(is.na(terrorismo$nperps)|terrorismo$nperps<0)&
                      !(is.na(terrorismo$nkillter))&terrorismo$nkillter>0&
                      (is.na(terrorismo$nwoundte)|
                         terrorismo$nwoundte<=0)]<-0

terrorismo$nperps[(is.na(terrorismo$nperps)|terrorismo$nperps<0)&
                    !(is.na(terrorismo$nkillter))&terrorismo$nkillter>0&
                    !(is.na(terrorismo$nwoundte)|
                       terrorismo$nwoundte<=0)] <- 
  c((terrorismo$nkillter[(is.na(terrorismo$nperps)|terrorismo$nperps<0)&
                        !(is.na(terrorismo$nkillter))&terrorismo$nkillter>0&
                        !(is.na(terrorismo$nwoundte)|
                           terrorismo$nwoundte<=0)]+
  terrorismo$nwoundte[(is.na(terrorismo$nperps)|terrorismo$nperps<0)&
                        !(is.na(terrorismo$nkillter))&terrorismo$nkillter>0&
                        !(is.na(terrorismo$nwoundte)|
                            terrorismo$nwoundte<=0)]))


# Ahora podemos llenar esos valores faltantes usando mice
# Primero hagamos todos los negativos en NA
terrorismo$nperps[terrorismo$nperps<0] <- NA
terrorismo$nperpcap[terrorismo$nperpcap<0] <- NA


perp.mis <- subset(terrorismo,
                     select = c(iyear,imonth,extended,
                                country,crit1,crit2,crit3,
                                doubtterr,multiple,success,
                                suicide,attacktype1,attacktype1,targtype1,
                                targsubtype1,
                                target1, natlty1,
                                gname, guncertain1, individual, 
                                nperps, nperpcap))


perp_imputados <- mice(perp.mis, m=2, maxit = 50, 
                         method = 'pmm', seed = 33)

terrorismo$nperps <- c(complete(perp_imputados)$nperps)
terrorismo$nperpcap <- c(complete(perp_imputados)$nperpcap)

# Como claimed se registra despues de 1997, podemos hacer una categoria de no 
# aplicable para todos aquellos que no fueron registrados (-1)
sum((is.na(terrorismo$claimed)|terrorismo$claimed< 0))
sum((is.na(terrorismo$claimed)|terrorismo$claimed<0)&
      terrorismo$iyear<1998)
terrorismo$claimed <- ifelse((is.na(terrorismo$claimed)|terrorismo$claimed<0)&
                               terrorismo$iyear<1998,
                             -1, terrorismo$claimed)

# Y nos quedamos con el conjunto ya sin desconocidos
terrorismo <- subset(terrorismo, !(is.na(terrorismo$claimed)|
                                     terrorismo$claimed< -1))
nrow(terrorismo)
#Vamos con la cuarta----
# Para el tipo de arma no hay perdidos, pero sí se clasifican como desconocidos
# 13. Quedemonos con sólo los conocidos
terrorismo <- subset(terrorismo, terrorismo$weaptype1!=13)
nrow(terrorismo)

# Ahora queremos ver sobre los subtipos
sum(is.na(terrorismo$weapsubtype1))

# Por la naturaleza de la variable es porque esos valores no aplican a un subtipo
# hagamos una categoria que signfica que no aplica
terrorismo$weapsubtype1 <- ifelse(is.na(terrorismo$weapsubtype1), -1, 
                                  terrorismo$weapsubtype1)

# Para nkill podemos quitar todos los desconocidos
sum(is.na(terrorismo$nkill))
terrorismo <- subset(terrorismo, !(is.na(terrorismo$nkill)))
terrorismo$nkill <- ceiling(terrorismo$nkill)


# Para nwound hacemos lo mismo
sum(is.na(terrorismo$nwound))
terrorismo <- subset(terrorismo, !(is.na(terrorismo$nwound)))
terrorismo$nwound <- ceiling(terrorismo$nwound)

# Para property, usemos propextent para ver los que no son desconocidos
sum(terrorismo$property<0)
sum(terrorismo$property<0&(terrorismo$propextent!=4&!is.na(terrorismo$propextent)))

terrorismo$property <- ifelse(terrorismo$propextent!=4&!is.na(terrorismo$propextent),1,
                              terrorismo$property)

sum(terrorismo$property<0)
# Eliminemos el resto
terrorismo <- subset(terrorismo,!terrorismo$property<0)
nrow(terrorismo)

# Para propextent usemos misc
# Primero hagamos los 4 en faltantes
terrorismo$propextent[terrorismo$propextent==4] <- NA
terrorismo$propextent <- as.factor(terrorismo$propextent)

property.mis <- subset(terrorismo, select = c(iyear,imonth,
                                          country,
                                          doubtterr,multiple,success,
                                          attacktype1,targtype1,
                                          targsubtype1,gname, 
                                          nperps, nperpcap, weaptype1, 
                                          weapsubtype1,
                                          property, propextent))

propied_imp <- mice(property.mis, m=2, maxit = 15, 
                         method = "polyreg", seed = 238)

terrorismo$propextent <- c(complete(propied_imp,action = 2)$propextent)

# Quitemos las malas predicciones, haciendo una clasificación de -1 para aquellos
# que no aplican
sum(terrorismo$property==0)
terrorismo$propextent <- ifelse(terrorismo$property==0,-1,terrorismo$propextent)

# La ultima parte----
# Para la variable INT_MISC simplemente borraremos las filas perdidas
terrorismo <- subset(terrorismo, !(is.na(terrorismo$INT_MISC)|
                                     terrorismo$INT_MISC < 0))

# Hacemos que si tenemos nhostkid mayor a 0, hacemos 1 ishostkid
terrorismo$ishostkid <- ifelse(terrorismo$nhostkid >0 &
                                 !(is.na(terrorismo$nhostkid))&
                                 is.na(terrorismo$ishostkid), 
                               1, terrorismo$ishostkid)

# Hacemos que si hay liberados, también sea 1
terrorismo$ishostkid <- ifelse(terrorismo$nreleased >0 &
                                 !(is.na(terrorismo$nreleased)), 
                               1, terrorismo$ishostkid)

# Limpiamos nhostkid para que sean 0 en lugar de NA
terrorismo$nhostkid <- ifelse(terrorismo$ishostkid == 0 & 
                                !(is.na(terrorismo$ishostkid)),
                              0, terrorismo$nhostkid)

# Si el hostkidoutcome esta en las categorias 2,3 o 5, entonces toma
# el valor del numero de rehenes liberados, ya que significa que sobrevieron
terrorismo$nhostkid[((terrorismo$nhostkid <= 0 | is.na(terrorismo$nhostkid)) 
                     & terrorismo$hostkidoutcome %in% c(2, 3, 5) & 
                       terrorismo$nreleased>0&!(is.na(terrorismo$nreleased)))] <-
  terrorismo$nreleased[((terrorismo$nhostkid <= 0 | is.na(terrorismo$nhostkid)) 
                        & terrorismo$hostkidoutcome %in% c(2, 3, 5) & 
                          terrorismo$nreleased>0&!(is.na(terrorismo$nreleased)))]

# Chequemos ahora su cantidad de valores perdidos
sum(is.na(terrorismo$ishostkid)|terrorismo$ishostkid < 0)/nrow(terrorismo)
sum(is.na(terrorismo$nhostkid)|terrorismo$nhostkid < 0)/nrow(terrorismo)


# Vamos ahora a quitar las filas con valores desconocidos, que están marcadas como
# -99
terrorismo <- subset(terrorismo, !(is.na(terrorismo$nhostkid)|terrorismo$nhostkid < 0))
terrorismo <- subset(terrorismo, !(is.na(terrorismo$ishostkid)|terrorismo$ishostkid < 0))


# Como nuestros datos están ahora en un intervalo entre 0 y 17000, el cual es
# bastante grande, nos conviene hacer un reescalamiento.
min(terrorismo$nhostkid)
max(terrorismo$nhostkid)

# Ahora apliquemos un reescalamiento logaritmico, esto nos ayuda a reducir 
# el intervalo de nuestros datos
terrorismo$nhostkid_log <- log(terrorismo$nhostkid + 1)

# Podemos observar que ahora quedan en un rango entre 0 y 10.
min(terrorismo$nhostkid_log)
max(terrorismo$nhostkid_log)

# Ahora hagamos una limpieza de los datos que registran un valor arriba de 0
# cuando la variable sucess es falsa, ya que no tendría sentido. Igual 
# se puede ver que son minimos.

nrow(subset(terrorismo,terrorismo$success==0&
              (terrorismo$ndays>0|terrorismo$nhours>0)))

terrorismo <- subset(terrorismo,!(terrorismo$success==0&
                                    (terrorismo$ndays>0|terrorismo$nhours>0)))



# Hagamos los valores de "nhours" y "ndays" -1 para cuando sucess es cero.
# Esto nos sirve para los valores perdidos (NA) y en su lugar hacer una categoria.

terrorismo$nhours <- ifelse(terrorismo$success == 0, -1, terrorismo$nhours)
terrorismo$ndays <- ifelse(terrorismo$success == 0, -1, terrorismo$ndays)


# Hagamos también otra modificacion para "nhours" y "ndays"
# ya que nhours es cero si ndays no lo es y vicersa


terrorismo$nhours <- ifelse((terrorismo$ndays > 0 & (is.na(terrorismo$nhours))|
                               terrorismo$nhours<0),
                            0, terrorismo$nhours)
terrorismo$ndays <- ifelse((terrorismo$nhours > 0 & (is.na(terrorismo$ndays))|
                              terrorismo$ndays<0),
                           0, terrorismo$ndays)


# Podemos observar también que no hay registro en donde la variable ishostkid
# sea 0 y la variable tome un valor positivo, así que podemos hacer lo mismo que
# con nhostkid



terrorismo$nhours[terrorismo$ishostkid==0] <- -1


terrorismo$ndays[terrorismo$ishostkid==0] <- -1

# Solo tenemos esta cantidad de datos:
sum((terrorismo$ndays>=0|
       terrorismo$nhours>=0),na.rm = TRUE)


# Veamos su mejoró sus valores perdidos
sum(is.na(terrorismo$nhours))
sum(is.na(terrorismo$nhours))/nrow(terrorismo)
sum(is.na(terrorismo$ndays))
sum(is.na(terrorismo$ndays))/nrow(terrorismo)

# Juntos tenemos
sum((is.na(terrorismo$ndays)|is.na(terrorismo$nhours)),na.rm = TRUE)

# En este caso no nos es tan conveniente simplemente borrar las filas,
# ya que es un porcentaje más significativo. Así que en este caso usaremos
# la paqueteria mice


# Hagamos todos los valores negativos en NA antes
terrorismo$nhours <- ifelse(terrorismo$nhours < (-1),
                            NA, terrorismo$nhours)
terrorismo$ndays <- ifelse(terrorismo$ndays < (-1),
                           NA, terrorismo$ndays)


tiempo.mis <- subset(terrorismo, (terrorismo$ndays != -1&
                                    terrorismo$nhours != -1)|
                       is.na(terrorismo$nhours)|
                       is.na(terrorismo$ndays),
                     select = c(nhostkid,ndays,nhours,ishostkid))


tiempo_imputados <- mice(tiempo.mis, m=3, maxit = 120, 
                         method = 'pmm', seed = 33)

terrorismo$ndays[which(is.na(terrorismo$ndays))] <- 
  tiempo_imputados$imp$ndays[,1]
terrorismo$nhours[which(is.na(terrorismo$nhours))] <- 
  tiempo_imputados$imp$nhours[,1]

# Quedamos con esta cantidad de datos limpios:
sum((terrorismo$ndays>=0|
       terrorismo$nhours>=0),na.rm = TRUE)

# Ahora, de ransomamt, es la cantidad que se pide, pero para ello el valor
# de ransom tiene que ser 1, así hacemos a todos los que sean 0, se vuelvan 0 en
# ransomamt y viceversa, también usamos ishostkid
nrow(subset(terrorismo,terrorismo$ransom==1&terrorismo$ishostkid == 0))
terrorismo$ransom <- ifelse(terrorismo$ishostkid == 0 & is.na(terrorismo$ransom)
                            & !(is.na(terrorismo$ishostkid)), 
                            0, terrorismo$ransom)


terrorismo$ransom[terrorismo$ransomamt==0&(is.na(terrorismo$ransom))&
                    !(is.na(terrorismo$ransomamt))] <- 0

# Para los valores que haya un registro de pagado sin la cantidad que se pidio
# hacemos que ese valor sea el mismo.
terrorismo$ransomamt[terrorismo$ransom== 1
                     &is.na(terrorismo$ransomamt)
                     &!(is.na(terrorismo$ransompaid))
                     &terrorismo$ransompaid>0] <- 
  terrorismo$ransompaid[terrorismo$ransom == 1&
                          is.na(terrorismo$ransomamt)&
                          !(is.na(terrorismo$ransompaid))&
                          terrorismo$ransompaid>0] 


terrorismo$ransom[terrorismo$ransomamt>0&
                    !(is.na(terrorismo$ransomamt))] <- 1

terrorismo$ransomamt <- ifelse(terrorismo$ransom == 0, 
                               0, terrorismo$ransomamt)



sum(is.na(terrorismo$ransomamt)|terrorismo$ransomamt < 0)/nrow(terrorismo)
sum(is.na(terrorismo$ransom)|terrorismo$ransom < 0)/nrow(terrorismo)
sum(terrorismo$ransom== 1&is.na(terrorismo$ransomamt),na.rm = TRUE)
subset(terrorismo,terrorismo$ransom== 1&is.na(terrorismo$ransomamt))
# Podemos hacer algo parecido para ransompaid
terrorismo$ransompaid <- ifelse(terrorismo$ransom == 0, 
                                0, terrorismo$ransompaid)

# Podemos hacer que si el hostkidoutcome es 2, entonces significa que se pago.
terrorismo$ransompaid[is.na(terrorismo$ransompaid) 
                      & terrorismo$hostkidoutcome == 2 &
                        !(is.na(terrorismo$hostkidoutcome))&
                        terrorismo$ransomamt>0 &
                        !(is.na(terrorismo$ransomamt))] <-
  terrorismo$ransomamt[is.na(terrorismo$ransompaid) 
                       & terrorismo$hostkidoutcome == 2 &
                         !(is.na(terrorismo$hostkidoutcome))&
                         terrorismo$ransomamt>0 &
                         !(is.na(terrorismo$ransomamt))]


sum(is.na(terrorismo$ransompaid)|terrorismo$ransompaid < 0)/nrow(terrorismo)

# Ya con eso quitemos todos los perdidos y desconocidos sobrantes
terrorismo <- subset(terrorismo, !(is.na(terrorismo$ransom)
                                   |terrorismo$ransom<0))
terrorismo <- subset(terrorismo, !(is.na(terrorismo$ransomamt)
                                   |terrorismo$ransomamt<0))
terrorismo <- subset(terrorismo, !(is.na(terrorismo$ransompaid)
                                   |terrorismo$ransompaid<0))

# Vamos ahora a discretizar los valores del precio de rescate y del precio pagado
# Primero nos conviene reescalar
ransomamt_scale <- log(terrorismo$ransomamt+1)


# Observemos que tenemos muchos valores que son 0, por lo que usaremos un bin completo
# para ellos de la sig. manera:
intervalos <- c(0, seq(0.6, max(ransomamt_scale)+0.3, length.out = 6))
terrorismo$ransomamt_disc <- cut(ransomamt_scale,breaks = intervalos,
                                 right = FALSE)


describe(terrorismo$ransomamt_disc)


# Ahora, para una manipulacion más sencilla, hagamoslo categoricos, donde cada
# categoria representa el inicio del intervalo. Esto nos ayudara ya que tenemos 
# un bin muy grande de 0.

limites_inferiores <- c(as.numeric(unlist(str_extract
                                          (as.character(terrorismo$ransomamt_disc),
                                            "\\d+\\.*\\d*"))))
terrorismo$ransomamt_disc <- limites_inferiores


# Hagamos lo mismo para la de la cantidad pagada.
ransompaid_scale <- log(terrorismo$ransompaid+1)

intervalos <- c(0, seq(4, max(ransompaid_scale)+0.3, length.out = 6))
terrorismo$ransompaid_disc <- cut(ransompaid_scale,breaks = intervalos,
                                  right = FALSE)

describe(terrorismo$ransompaid_disc)

limites_inferiores <- c(as.numeric(unlist(str_extract
                                          (as.character(terrorismo$ransompaid_disc),
                                            "\\d+\\.*\\d*"))))
terrorismo$ransompaid_disc <- limites_inferiores

# Ahora queremos predecir la categoria del resultado de la toma de rehenes
# Borremos todos aquellos que no tienen sentido.
terrorismo <- subset(terrorismo,!(terrorismo$ishostkid == 0 &
                                    !(is.na(terrorismo$hostkidoutcome))))

# Antes hagmos que si cae en las categorias 2,3 o 5, entonces toma el valor
# del numero de rehenes, ya que significa que sobrevieron
terrorismo$nreleased[terrorismo$hostkidoutcome %in% c(2, 3, 5)] <- 
  terrorismo$nhostkid[terrorismo$hostkidoutcome %in% c(2, 3, 5)]

# Tambien para la categoria 1 y 4, que significa que murieron, podemos hacer que sea
# 0

terrorismo$nreleased <- ifelse(terrorismo$hostkidoutcome %in% c(1,4)
                               & !(is.na(terrorismo$hostkidoutcome)), 
                               0, terrorismo$nreleased)

# Hagamos NA los valores 7 antes y NA los desconocidos de nreleased
terrorismo$hostkidoutcome[terrorismo$hostkidoutcome==7] <- NA
terrorismo$nreleased[terrorismo$nreleased<0]<-NA




perdi_out <- aregImpute(~ nhostkid+hostkidoutcome+nreleased, 
                        data = terrorismo[c("nhostkid","hostkidoutcome",
                                            "nreleased")], 
                        n.impute = 5)

terrorismo$hostkidoutcome[is.na(terrorismo$hostkidoutcome)] <- 
  matrix(perdi_out$imputed$hostkidoutcome[,3], ncol = 1)

terrorismo$nreleased[is.na(terrorismo$nreleased)] <- 
  matrix(perdi_out$imputed$nreleased[,3], ncol = 1)

# Para hostkidoutcome podemos crear una nueva categoria, una que describa que
# no aplica (no hubo toma de rehenes)

terrorismo$hostkidoutcome <- ifelse(terrorismo$ishostkid == 0
                                    & !(is.na(terrorismo$ishostkid)), 
                                    0, terrorismo$hostkidoutcome)

sum(is.na(terrorismo$hostkidoutcome))/nrow(terrorismo)


# Lo mismo para nreleased, donde -1 es una categoria qu nos dice que no aplica.

terrorismo$nreleased <- ifelse(terrorismo$ishostkid == 0
                               & !(is.na(terrorismo$ishostkid)), 
                               -1, terrorismo$nreleased)



# Ya solo quitemos las predicciones defectuosas
terrorismo <- subset(terrorismo,!(terrorismo$nhostkid<terrorismo$nreleased))

# Ahora vamos a reescalar igual el numero de liberados
terrorismo$nreleased_log <- ifelse(terrorismo$nreleased>=0,
                                   log(terrorismo$nreleased+1),
                                   terrorismo$nreleased)

# Nos quedamos con todos los que no sean NA
terrorismo <- terrorismo[complete.cases(terrorismo[, names(terrorismo) %in% 
                                                     c("ishostkid","nhostkid","nhours","ndays",
                                                       "ransom","ransomamt","ransompaid",
                                                       "hostkidoutcome","nreleased","INT_MISC")]), ]


# Ya podemos quitar las columnas que no usaremos
# Nos quedamos con nuestras columnas de interes
terrorismo <- subset(terrorismo, select = c(eventid,iyear,imonth,extended,
                                            country,city,crit1,crit2,crit3,
                                            doubtterr,multiple,success,
                                            suicide,attacktype1,targtype1,
                                            targsubtype1,
                                            target1, natlty1,
                                            gname, guncertain1, individual, 
                                            nperps, nperpcap, claimed,weaptype1, 
                                            weapsubtype1, 
                                            nkill, nwound, 
                                            property, propextent,
                                            ishostkid,nhostkid_log,nhours,ndays,
                                            ransom,ransomamt_disc,ransompaid_disc,
                                            hostkidoutcome,nreleased_log,INT_MISC))


# Al final nos quedamos con la siguiente cantidad de filas despues de toda
# la limpieza
nrow(terrorismo)

# Ya escribamos nuestro csv limpio
write.csv(terrorismo, file = "datoslimpios.csv")
