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

#Se buscaría disminuir las distancias de los puntos a la racta que generemos

dist1 <- sim1 %>% 
  mutate(
    dodge = rep(c(-1, 0, 1) / 20, 10),
    x1 = x + dodge,
    pred = 7 + x1 * 1.5
  )

ggplot(dist1, aes(x1, y)) + 
  geom_abline(intercept = 7, slope = 1.5, colour = "grey40") +
  geom_point(colour = "grey40") +
  geom_linerange(aes(ymin = y, ymax = pred), colour = "#3366FF") 


#Generamos una función para esto

linealmodel <- function(coefficients, data) 
{
  dist1 <- data %>% 
    mutate(
      dodge = rep(c(-1, 0, 1) / 20, 10),
      x1 = x + dodge,
      pred = coefficients[1] + x1 * coefficients[2]
    )
  
  ggplot(dist1, aes(x1, y)) + 
    geom_abline(intercept = coefficients[1], slope = coefficients[2], colour = "grey40") +
    geom_point(colour = "grey40") +
    geom_linerange(aes(ymin = y, ymax = pred), colour = "#FF6600") 
  
 
   
}
linealmodel(c(7, 1.5), sim1)


#Ahora haremos una función para calcular las distancias
measure_distance <- function(mod, data) 
{
  mod
  data$x
  data <- data %>% mutate(lineValue = mod[1] + (mod[2]*data$x))
  data <- data %>% mutate(diff = (lineValue - data$y)^2)
  sumaDiffs <- data %>% 
    summarise(sumaDiffs = sum(diff))
  sumaDiffs <- sumaDiffs/length(data$x)
  sumaDiffs <- sqrt(sumaDiffs)
}
distance <- measure_distance(c(7, 1.5), sim1)
distance
#extendedData <- measure_distance(c(7, 1.5), sim1)






