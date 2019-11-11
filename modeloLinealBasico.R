####Ejercicios Clase 5-10 ####

##Ejercicios regresión lineal simple##

#install.packages('plotly', dependencies=TRUE, repos='http://cran.rstudio.com/')

rm(list=ls())
gc()

setwd("E:/UBA/2019-II/EEA/R Code")


library(tidyverse)
library(modelr)
library(plotly)
options(na.action = na.warn)

#Datos sim1 vienen con modelr
ggplot(sim1, aes(x, y)) + 
  geom_point()


#Generamos modelos al azar
models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) + 
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point() 
















