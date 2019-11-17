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
  sumaDiffs <-  as.numeric(sqrt(sumaDiffs))
}
distance <- measure_distance(c(7, 1.5), sim1)
distance
#extendedData <- measure_distance(c(7, 1.5), sim1)



#Ahora vamos a evaluar los modelos aleatorios
sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models

#Superponemos los 10 mejores modelos

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(models, rank(dist) <= 10)
  )

#Vemos los modelos como gráficos de dispersión

ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))



#Grid Search
# Crear la grilla
grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  # Calcular la distancia
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

#Generamos la grilla para evaluar los modelos

grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist)) 

#Vemos los mejores 10 modelos
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(grid, rank(dist) <= 10))


#Superficie del ECM
#Podemos ver los puntos de la grilla en tres dimensiones

# Matriz para el grafico
rss_matrix <- matrix(models[["dist"]],nrow = length(models$a1),ncol = length(models$a1), byrow = TRUE)

# Grafico usando plotly
rss_graph = plot_ly(x=models$a1, y=models$a2, z=rss_matrix) %>% add_surface(contours = list(
  z = list(
    show=TRUE,
    usecolormap=TRUE,
    highlightcolor="#ff0000",
    project=list(z=TRUE)
  )
), reversescale=TRUE)  %>%
  layout(
    title = "Superficie del ECM",
    scene = list(
      xaxis = list(title = "a0"),
      yaxis = list(title = "a1"),
      zaxis = list(title = "RSS")
    ))

rss_graph


#Clase 2
#Óptimo por métodos numéricos
#Por métodos numéricos se puede optimizar, La intuición de estas dos herramientas
#es bastante simple: Se elige un punto de partida y se busca la pendiente más inclinada
#Búsqueda Newton-Raphson.
#Gradient Descent

#Usamos optim
optim(c(4,2), measure_distance, data = sim1)
best <- optim(c(0, 0), measure_distance, data = sim1)
best$par

#Se grafica basado en el mejor punto encontrado
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(intercept = best$par[1], slope = best$par[2])

#Óptimo para el modelo lineal
#Al óptimo para el modelo lineal lo podemos resolver matemáticamente
sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

#Predicciones
#Iniciamos generando una cuadrilla
grid <- sim1 %>% 
  data_grid(x) 
grid

grid <- grid %>% 
  add_predictions(sim1_mod) 
grid

#Agregamos las predicciones al dataset original
ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, colour = "red", size = 1)

#Residuos del modelo
#Para los residuos necesitamos los valores reales

sim1 <- sim1 %>% 
  add_residuals(sim1_mod)
sim1

#Con un polígono de frecuencia podemos graficar la dispersión de los residuos

ggplot(sim1, aes(resid)) + 
  geom_freqpoly(binwidth = 0.5)

#El promedio de los residuos debería ser muy cercano a 0
mean(sim1$resid)

#Los residuos también se pueden graficar del siguiente modo

ggplot(sim1, aes(x, resid)) + 
  geom_ref_line(h = 0, size = 2,colour = "firebrick") +
  geom_point() 

#Esperamos que los residuos NO tengan estructura!












