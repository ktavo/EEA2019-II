#Clase 2 EEA

rm(list=ls())
setwd("E:/UBA/2019-II/EEA/R Code")

#install.packages('tidyverse', dependencies=TRUE, repos='http://cran.rstudio.com/')

library("tidyverse")
library("dplyr")
library("tidyr")
library("ggplot2")
library("lubridate")

INDICE  <- c(100,   100,   100,
             101.8, 101.2, 100.73,
             102.9, 102.4, 103.2)

FECHA  <-  c("Oct-16", "Oct-16", "Oct-16",
             "Nov-16", "Nov-16", "Nov-16",
             "Dec-16", "Dec-16", "Dec-16")

GRUPO  <-  c("Privado_Registrado","Público","Privado_No_Registrado",
             "Privado_Registrado","Público","Privado_No_Registrado",
             "Privado_Registrado","Público","Privado_No_Registrado")

Datos <- data.frame(INDICE, FECHA, GRUPO)

#Para ver un vistazo de datos
glimpse(Datos)

#Para ver un subset filtrado por condición
Datos %>% filter(INDICE>101 , GRUPO == "Privado_Registrado")


#Para ver filtro de OR se usa | para AND se usa ,
Datos %>% filter(INDICE>101 | GRUPO == "Privado_Registrado")

#Renombrar columna Data %>% rename( nuevo_nombre = viejo_nombre )
Datos %>% rename(Periodo = FECHA)


#mutate permite modiicar la tabla añadiendo una colmna
Datos <- Datos %>% mutate(Doble=INDICE*2)
Datos


#case_when La sintaxis de la función es case_when( condicion lógica1 ~ valor asignado1)
Datos <- Datos %>% 
  mutate(Caso_cuando = case_when(GRUPO == "Privado_Registrado"   ~ INDICE*2,
                                 GRUPO == "Público"              ~ INDICE*3))
Datos

#Añadiendo un caso "else"
Datos %>% 
  mutate(Caso_cuando = case_when(GRUPO == "Privado_Registrado"   ~ INDICE*2,
                                 GRUPO == "Público"              ~ INDICE*3,
                                 TRUE ~ 1000))

#Select permite seleccionar columnas
Datos2 <- Datos %>% 
  select(INDICE, FECHA, GRUPO)
Datos2

#select para eliminar columnas
Datos <- Datos %>% 
  select(-c(Doble,Caso_cuando))
Datos

#arange ordenar la tabla por las variables
Datos <- Datos %>% 
  arrange(GRUPO, INDICE)
Datos

#sumarise
Indprom <- Datos %>% 
  summarise(Indprom = mean(INDICE))

#group_by permite operar sobre subsets de la tabla
monthlyMeanIndex <- Datos %>% 
  group_by(FECHA) %>%
  summarise(Indprom = mean(INDICE))













