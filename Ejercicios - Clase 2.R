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

fecha_cotizacion= dolarDiario %>% group_by(year,month) %>% summarise(day=min(day))

#Innerjoin  para primer día del mes
dolarDiario = dolarDiario %>%
  inner_join(fecha_cotizacion, by=c('year', 'month', 'day')) %>%
  select(-c(Fecha, day))


dolarGraph <- ggplot(data = dolarDiario, aes(x = date, y = tipoCambio))+
                    geom_line() + 
                    labs(x = "Fecha", y = "Tipo de Cambio" ,title = "Tipo de cambio vs tiempo")
dolarGraph

#IPC
ipcMensual = ipcMensual %>% gather(., key = date,value = ipc, 2:29)
glimpse(ipcMensual)

ipcMensual <- ipcMensual %>% mutate(date=ymd(parse_date_time(date, orders = "my")), 
                                    year=year(date), month=month(date))

ipcMensualGeneral <- ipcMensual %>% filter(Apertura == "Nivel general")

ipcGraph <- ggplot(data = ipcMensualGeneral, aes(x = date, y = ipc)) +
                    geom_point(color="firebrick") +
                    geom_line(size= 1,alpha=0.75, color = "firebrick") + 
                    labs(x = "Fecha", y = "IPC Mensual", title = "Evolución IPC")
ipcGraph
















