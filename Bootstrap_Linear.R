#Bootstrap

##Ejercicios regresión lineal simple##

#install.packages('tidymodels', dependencies=TRUE, repos='http://cran.rstudio.com/')

rm(list=ls())
gc()

setwd("E:/UBA/2019-II/EEA/R Code")

library(tidyverse)
library(rsample)
library(GGally)
library(robust)
library(ggridges)
library(ggthemes)
library(magrittr)
library(tidymodels)


#Compararemos el ajuste lineal clásico (mínimos cuadrados) con el método
#robusto, se evaluará la performance con Bootstraping

#Fijamos una semilla
set.seed(141414)

n <- 100
x1 <- runif(n,0,10)
x2 <- runif(n,0,10)
x3 <- runif(n,0,10)
err <- rnorm(n,mean = 0, sd = 1)
# Con los valores de las X y el error calculados, construimos los dos modelos para y
y1 <- 4+1.5*x1[1:90]+8*x2[1:90]-2*x3[1:90]+err[1:90]
y2 <- 4+1.5*x1[91:100]+8*x2[91:100]-60*x3[91:100]+err[91:100]
# Jutnamos ambas y-es
y <- c(y1,y2)
# Va a resultar conveniente tener todo junto en un DF (el error no lo vamos a precisar)
df <- data.frame(x1,x2,x3,y)

#Podemos randomizar el orden de las filas, para que no queden todos los casos patológicos al final
df <- df[sample(nrow(df)),]
df

#Para visualizar los datos usaremos diagramas de dispersión

df %>% 
  gather(var,val,1:3) %>% 
  ggplot(.,aes(val,y,color=var))+
  geom_point()+
  facet_wrap(~var, scales = "free")+
  theme(legend.position = "none")














