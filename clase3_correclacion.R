#Práctica 3 parte 2 Correlación


rm(list=ls())
setwd("E:/UBA/2019-II/EEA/R Code")

#install.packages('shiny', dependencies=TRUE, repos='http://cran.rstudio.com/')

library(tidyverse)
library(openintro)
library(GGally)
library(corrr)
library(knitr)
library(kableExtra)
options(knitr.table.format = "html") 

#Vemos la base 
mtcars %>% 
  head() %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

#Graficamos las correlaciones entre las variables
mtcars %>% 
  select(-carb,-vs) %>% 
  mutate(cyl = factor(cyl),
         am = factor(am)) %>% 
  ggpairs(., 
          title = "Matriz de correlaciones",
          mapping = aes(colour= am))


#Correlación con corrr

#Matriz Correlación entre variables 
mtcars %>% 
  correlate() %>% 
  shave() %>% 
  fashion()

#Gráfica decorrelaciones en el espacio
mtcars %>% 
  correlate() %>% 
  network_plot(min_cor = 0.7)


#Una correlación mas sencilla
mtcars %>% 
  correlate() %>% 
  rplot()

#Significancia estimador
cor.test(mtcars$mpg,mtcars$hp)

#Comparar la relación entre dos variables específicas
#Drat y Gear
mtcars %>% 
  group_by(am) %>% 
  summarise(cor = cor(drat, gear))


mtcars2 <- mtcars %>% filter(am==0)
ggplot(mtcars2, aes(gear,drat, group=gear, fill = factor(gear)))+
  geom_boxplot(alpha= 0.75)

#Pearson NO sería lo mejor para relacionar una variable que toma dos valores
cor.test(mtcars2$gear,mtcars2$drat, method = "pearson")
#Es más apropiado spaearman
cor.test(mtcars2$gear,mtcars2$drat, method = "spearman")









