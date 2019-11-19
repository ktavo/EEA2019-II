rm(list=ls())

setwd("E:/UBA/2019-II/EEA/R Code")

library("dplyr")
library("tidyverse")

#1.Preparacion de los datos (I) 

#a.Leer el archivo ar_properties.csv y mostrar su estructura
t0       <-  Sys.time()
ar_properties <- read.table("TP1/ar_properties.csv",
                            sep=",",
                            dec=".",
                            header = TRUE,
                            fill = TRUE)
t1       <-  Sys.time()
tcorridaCSV <-  as.numeric( t1 - t0, units = "secs")
glimpse(ar_properties)


#b.Quedarse con aquellos registros que:
#  i.Pertenecen a Argentina y Capital Federal
#  ii.Cuyo precio esta en dolares (USD)
#  iii.El tipo de propiedad sea: Departamento, PH o Casa
#  iv.El tipo de operacion sea Venta
#Revisé la variable Currency para ver si todas estaban marcadas por USD para dolaers
#Revisé los NA en country l1
#Deperatamento 42041
#PH 4564
#Casa 21535
#Sólo por el filtro de "Capital Federal" tenemos 47577

ar_properties_filtrado <- ar_properties %>% filter(l1 == "Argentina", l2 == "Capital Federal",
                                                   currency == "USD" ,
                                                   property_type %in% c("Casa","PH", "Departamento"),
                                                   operation_type == "Venta")

glimpse(ar_properties_filtrado)

#c.Seleccionar las variables id, l3, rooms, bedrooms, bathrooms, surface_total, surface_covered, price y property_type
ar_properties_filtrado <- ar_properties_filtrado %>% 
  select(id, l3, rooms, bedrooms, bathrooms, surface_total, surface_covered, price, property_type)
glimpse(ar_properties_filtrado)



###2.Analisis exploratorios (I)
#a.Obtener la cantidad de valores unicos y de valores faltantes (NAs) para cada una de estas variables
#Para esto usaremos la función 'unique'
unique(ar_properties_filtrado)
#Pero con apply la podemos aplicar a todo el dataFrame sin duplicar código
uniqueValues <- apply(ar_properties_filtrado,2,unique)
#Tenemos como resultado:
uniqueValues$l3
#l3 -57 Barrios entontrados y NA
uniqueValues$rooms
#rooms 17 categorías (y NA) sin embargo se tienen valores que no tienen mucho sentido
#Algunos de ellos parecieran ser excesivos
uniqueValues$bedrooms
#bedrooms 17 categorías (y NA) se tiene un outlier de 130 que debería ser un error de tipeo
#dado que en rooms no tenemos ningún outlier que se le acerque
uniqueValues$bathrooms
#bathrooms 12 categorías (y NA) los valores más grandes son llamativos
uniqueValues$surface_total
uniqueValues$surface_covered
#surface_total y surface_covered alta variabilidad, dado que es una variable numérica, 
#se podría transformar en categórica revisando más a fondo los valores
summary(as.integer(uniqueValues$surface_total))
summary(as.integer(uniqueValues$surface_covered))
#Price
uniqueValues$price
#price alta variabilidad, dado que es una variable numérica, 
#se podría transformar en categórica revisando más a fondo los valores
summary(as.integer(uniqueValues$price))
#Tipo de propiedad
uniqueValues$property_type
#Las tres gategorías filtradas del dataset: PH, Casa, Departamento

#a.Obtener la cantidad de valores unicos y de valores faltantes (NAs) para cada una de estas variables
#Ahora vamos a totalizar los valores faltantes por columnap para esto usamos la función apply para sumar
#sobre los resultados de la misma función para listar los is.na por columna #inceptionTime
apply(apply(ar_properties_filtrado,2,is.na),2,sum)

#b.Obtener la matriz de correlacion para las variables numericas. 
#Para las correlaciones usaremos la librería corrr
library(tidyverse)
library(corrr)
library(corrplot)

#Separaremos las variables numéricas
numeric_variables <-  ar_properties_filtrado %>% 
  select(rooms, bedrooms, bathrooms, surface_total, surface_covered, price)
glimpse(numeric_variables)

#Transformamos las variables categóricas en numéricas
number <- as.numeric(as.character(numeric_variables$rooms))
numeric_variables <- apply(apply(numeric_variables,2,as.character), 2, as.numeric)
crm <- cor(numeric_variables, use="complete.obs", method="pearson") 

#Generámos la matriz de corelaciones
library(RColorBrewer)
library(corrplot)

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
#Encontramos que:
#Todas las correlaciones entre las varialbes son positivas.
#Contra la hipótesis inicial no se observa una correlaciónmarcada entre la superficie cubierta y el precio
#Se observauna correlaciónentre el precio y el número de baños y cuartos


#Basados en la correlación entre bedrooms y rooms eliminaremos la variable bedrooms

#ar_properties_filtrado <- ar_properties_filtrado %>% select(-bedrooms)






