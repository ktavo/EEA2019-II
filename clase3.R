#Práctica 3 Guiada

rm(list=ls())
setwd("E:/UBA/2019-II/EEA/R Code")

#install.packages('gapminder', dependencies=TRUE, repos='http://cran.rstudio.com/')
library("fs")
library("tidyverse")
library("openxlsx")
library("glue")

bases_individuales_path <- dir_ls(path = '../CodigoProf/Fuentes/', regexp= 'individual')
bases_individuales_path


leer_base_eph <- function(path) {
  read.table(path,sep=";", dec=",", header = TRUE, fill = TRUE) %>% 
    select(ANO4,TRIMESTRE,REGION,P21,CH04, CH06)
}
#Data set nesteado
bases_df <- tibble(bases_individuales_path) %>%
  mutate(base = map(bases_individuales_path, leer_base_eph))

bases_df
#Para separar las bases del nest
bases_df <- bases_df %>% unnest()
bases_df

#Para agrupar por ejemplo por region
bases_df %>% 
  group_by(REGION) %>% 
  nest()

#Ejemplo 2
#Veamos un tercer ejemplo con otra base de datos que ya conocemos: 
#Gapminder, que muestra algunos datos sobre la población de los países por año.


library(gapminder)

gapminder_unfiltered %>% 
  sample_n(10)

#Información de Argentina
data_argentina <- gapminder_unfiltered %>% 
  filter(country=='Argentina')

ggplot(data_argentina, aes(year, lifeExp, size= pop, color=gdpPercap))+
  geom_point()+
  geom_line(alpha=0.6)+
  labs(title = unique(data_argentina$country))




