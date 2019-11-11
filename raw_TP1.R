rm(list=ls())

setwd("E:/UBA/2019-II/EEA/R Code")

library("dplyr")
library("tidyverse")



t0       <-  Sys.time()
ar_properties <- read.table("TP1/ar_properties.csv",
                            sep=",",
                            dec=".",
                            header = TRUE,
                            fill = TRUE)
t1       <-  Sys.time()
tcorridaCSV <-  as.numeric( t1 - t0, units = "secs")

#withencoding 12667
glimpse(ar_properties)
#rm(ar_properties_filtrado)


#ar_properties_filtrado <- ar_properties %>% filter( l2 == "Capital Federal")


#ar_properties_filtrado <- ar_properties %>% filter(l2 == "%Capital%")
#ar_column <- ar_properties %>% select(currency)


#Revisé la variable Currency para ver si todas estaban marcadas por USD para dolaers
#Revisé los NA en country l1
#Sólo por el fintro de capital tenemos 47577

ar_properties_filtrado <- ar_properties %>% filter(l1 == "Argentina", l2 == "Capital Federal",
                                          currency == "USD", property_type %in% c("Casa","PH", "Departamento"),
                                          operation_type == "Venta")

glimpse(ar_properties_filtrado)

ar_properties_filtrado <- ar_properties_filtrado %>% 
  select(id, l3, rooms, bedrooms, bathrooms, surface_total, surface_covered, price, property_type)
glimpse(ar_properties_filtrado)

#Deperatamento 42041
#PH 4564
#Casa 21535

#Para esto usaremos la función 'unique'
unique(ar_properties_filtrado)
#Pero con apply la podemos aplicar a todo el dataFrame sin duplicar código
apply(ar_properties_filtrado,2,unique)
#Tenemos como resultado que:
#id Efectivamente todos son diferentes
#l3 -58Barrios entontrados
#rooms 17 posibilidades(y NA) sin embargo se tienen valores que no tienen mucho sentido
#bedrooms 17 posibilidades (y NA) se tiene un outlier de 130 que debería ser un error de tipeo
#bathrooms12 posibilidades (y NA)
#surface cuenta con gran variabilidad 539valores diferentes, algunos outliers que deberían 
#coresponder a edificios, otros que deberían ser errores de tipeo
#Surface covered tiene 461 valores diferentes, con outliers a revisar como 126.062m2
#Price cuenta con 2417 valores diferentes
#property_type con los tres valores filtrados previamente PH, Casa y Departamento

#Ahora vamos a totalizar los valores faltantes por columnap para esto usamos la función apply para sumar
#sobre los resultados de la misma función para listar los is.na por columna #inceptionTime
apply(apply(ar_properties_filtrado,2,is.na),2,sum)


#Para las correlaciones usaremos la librería corrr
library(tidyverse)
library(corrr)
library(corrplot)

#Separaremos las variables numéricas
numeric_variables <-  ar_properties_filtrado %>% 
  select(rooms, bedrooms, bathrooms, surface_total, surface_covered, price)
glimpse(numeric_variables)

ar_properties_filtrado %>% 
  select(-id,-l3, -property_type) %>% 
  ggpairs(., 
          title = "Matriz de correlaciones",
          mapping = aes(colour= am))

number <- as.numeric(as.character(numeric_variables$rooms))

numeric_variables <- apply(apply(numeric_variables,2,as.character), 2, as.numeric)

crm <- cor(numeric_variables, use="complete.obs", method="pearson") 


library(RColorBrewer)
n <- ncol(crm)
p.mat<- matrix(NA, n, n)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(crm, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

#Todas las correlaciones entre las varialbes son positivas.
#Contra la hipótesis inicial no se observa una correlaciónmarcada entre la superficie cubierta y el precio
#Se observauna correlaciónentre el precio y el número de baños y cuartos
#Basados en la correlación entre bedrooms y rooms eliminaremos la variable bedrooms

#ar_properties_filtrado <- ar_properties_filtrado %>% select(-bedrooms)






