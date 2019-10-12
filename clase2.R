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
