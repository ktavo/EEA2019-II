#Bootstrap

##Ejercicios regresión lineal simple##

#install.packages('robustbase', dependencies=TRUE, repos='http://cran.rstudio.com/')

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
library(robust)



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

#Esta segunda grfica nos muestra 10 datos con un comportamiento patológico
df %>% 
  ggplot(.,aes(y=y))+
  geom_point(aes(x3))+
  geom_abline(slope = -2,intercept = 4, color="green")+
  geom_abline(slope = -60,intercept = 4, color = "firebrick")


#Ajuste de modelo lineal
#Modelo clásico
modelo <- lm(y~x1+x2+x3,data = df)
summary(modelo)

tidy(modelo,conf.int = TRUE,conf.level = 0.95)
glance(modelo)

au <- augment(modelo,df)

#Modelo robusto
modelo_robusto <- lmRob(y~x1+x2+x3,data = df)
summary(modelo_robusto)

tidy(modelo_robusto)
glance(modelo_robusto)
au_rob <- augment(modelo_robusto,df)

#Gráficos de los residuos
#Modelo clásico
ggplot(au, aes(.fitted, .resid)) +
  geom_point()+
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE)+
  labs(title= "OLS")

#Modelo robusto
ggplot(au_rob, aes(.fitted, .resid)) +
  geom_point()+
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE)+
  labs(title= "Modelo Robusto")

#QQplot modelo clásico

ggplot(au, aes(sample= .resid))+
  stat_qq()+
  geom_abline()+
  labs(title="QQ-plot OLS",
       x = "Distribución teórica",
       y= "Residuos")

ggplot(au_rob, aes(sample= .resid))+
  stat_qq()+
  geom_abline()+
  labs(title="QQ-plot Modelo Robusto",
       x = "Distribución teórica",
       y= "Residuos")


#Bootstrap CAserto
set.seed(1234)
muestras_bootstrapeadas <- list()
#quiero construir 100 muestras bootstrpeadas. Podrían ser más
for (i in 1:100) { 
  #quiero que los dataset que muestree tengan el mismo tamaño que el dataset original
  muestra_i <- data.frame()
  index_i <- c()
  for (j in 1:nrow(df)) { 
    #eligo el número indice de la muestra de forma aleatoria entre los indices de mi set original
    index <- sample(rownames(df),1)
    #selecciono de mi df el elemento con el indice correspondiente
    muestra_unitaria <- df[index,]
    #agrego la observación a la muestra i
    muestra_i <- bind_rows(muestra_i,muestra_unitaria)
    #guardo el indice
    index_i <- c(index_i,index)
  }
  bootstrap_i <- list("df"=muestra_i, "indices" = index_i)
  muestras_bootstrapeadas[[i]] <- bootstrap_i
}

#Revisamos nuestra primera muestra generada
data.frame(table(muestras_bootstrapeadas[[1]]$indices)) %>% 
  arrange(-Freq)

#Bootstrap con tidyverse
library(rsample)

muestras_bootstrapeadas <- bootstraps(df,times = 100)
muestras_bootstrapeadas

#Tomamos el primero de nuestros nuevos resultados conformado por un split

muestras_bootstrapeadas %>% 
  filter(id=="Bootstrap001") %$%
  splits[[1]][[1]]

#Ahora organizándolo por frequencia de lso resultados
#Cada split contiene un dataframe con la muestra bootstrapeada, 
#y un vector con los números de fila originales.
data.frame(table(muestras_bootstrapeadas %>% 
                   filter(id=="Bootstrap001") %$%
                   splits[[1]][[2]])) %>% 
  arrange(-Freq)


#Wrapeamos las funcionas para que soólo necesiten un parámetro de entrada
ajuste_lineal_simple <- function(df){
  lm(y~x1+x2+x3,data = df)
}

ajuste_lineal_robusto <- function(df){
  lmRob(y~x1+x2+x3,data = df)
}

#Con esto podemos calcular el ajuste lineal y el ajuste robusto para cada una de las muestras.
muestras_bootstrapeadas <- muestras_bootstrapeadas %>% 
  mutate(lm_simple = map(splits, ajuste_lineal_simple),
         lm_rob = map(splits, ajuste_lineal_robusto))


muestras_bootstrapeadas

#Para recuperar los parámetros de cada una de las estimaciones, vamos a realizar un map()
#sobre la función tidy()


parametros <- muestras_bootstrapeadas %>%
  gather(3:4) %>% 
  mutate(tdy = map(statistic,tidy)) %>% 
  unnest(tdy, .drop=TRUE)

parametros

#Con estas estiamciones podemos graficar los resultados
parametros_verdaderos <- data_frame(term=c("(Intercept)","x1","x2","x3"),valor=c(4,1.5,8,-2))

ggplot(parametros, aes(estimate, fill = model))+
  geom_histogram(position= "dodge")+
  geom_vline(data=parametros_verdaderos,aes(xintercept = valor,color = "Verdadero"), size=1)+
  scale_color_manual(values = "darkolivegreen","")+
  theme_minimal()+
  scale_fill_gdocs()+
  theme(legend.position = "bottom")+
  facet_wrap(~term,scales = "free")


ggplot(parametros, aes(estimate,y=model, fill = model))+
  geom_density_ridges(alpha=.6)+
  theme_minimal()+
  geom_vline(data=parametros_verdaderos,aes(xintercept = valor,color = "Verdadero"), size=1)+
  scale_color_manual(values = "darkolivegreen","")+
  scale_fill_gdocs()+
  theme(legend.position = "bottom")+
  facet_wrap(~term,scales = "free")

#Conclusiones:

#La distribución de los estimadores en el modelo robusto esta centrada en los parámetros verdaderos
#La distribución de los estimadores en el modelo robusto esta Tiene mucha menos variabilidad
#Particularmente para la variable patológica (x3), la distribución del modelo lineal simple 
#es especialmente mala, con el valor verdadero muy alejado del centro de masa.






