#Ejercicios clase 2
#IPC vs. Dolar

rm(list=ls())
setwd("E:/UBA/2019-II/EEA/R Code")

#install.packages('ggridges', dependencies=TRUE, repos='http://cran.rstudio.com/')


library("lubridate")
library("dplyr")
library("tidyr")
library("ggplot2")

dolarDiario <- read.csv("../CodigoProf/fuentes/dolar-diario.csv", encoding="UTF-8")
ipcMensual <- read.csv("../CodigoProf/fuentes/ipc-mensual.csv", encoding="UTF-8")

glimpse(dolarDiario)
glimpse(ipcMensual)

dolarDiario <- dolarDiario %>% rename(tipoCambio = Tipo.de.Cambio.de.Referencia...en.Pesos...por.Dólar)

dolarDiario <- dolarDiario %>% mutate(date=dmy(Fecha), year=year(date), month=month(date), day=day(date))


