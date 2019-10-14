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
                    geom_point(color="seagreen") +
                    geom_line(size= 1,alpha=0.75, color = "seagreen") +  
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

#IPC por grupo
ipcGroupGraph <- ggplo(data = ipcMensualGeneral, aes(x = date, y = ipc)) +



ipcGroupGraph <- ggplot(ipcMensual, aes(x= date, y = ipc, group = Apertura, color = Apertura )) +
                        geom_point()+
                        geom_line(size=1,alpha=0.75)+
                        labs(x="Fecha", y="IPC", title="Evolución del IPC por grupo")+
                        theme_bw()+
                        scale_color_brewer(palette = "Set1")
ipcGroupGraph

#Join dolar e ipc
ipc_dolar <- ipcMensual %>% inner_join(dolarDiario, by=c("year", "month"))
glimpse(ipc_dolar)

ipc_dolar = ipc_dolar %>% rename(date=date.x) %>% select(-date.y)


resumen = ipc_dolar %>% filter(Apertura=="Nivel general") %>%
  group_by(year) %>% 
  summarise(dolar_promedio=mean(tipoCambio),
            desvio_dolar=sd(tipoCambio),
            mediana_dolar=median(tipoCambio),
            rango_dolar=max(tipoCambio)-min(tipoCambio),
            ipc_promedio=mean(ipc),
            desvio_ipc=sd(ipc),
            mediana_ipc=median(ipc),
            rango_ipc=max(ipc)-min(ipc))
resumen


#IPC Versus Dolar

ggplot()+
  geom_line(data = ipc_dolar %>% filter(Apertura=="Nivel general"),aes(x=date,y=ipc, color ="Nivel general"))+
  geom_line(data = ipc_dolar,aes(date,tipoCambio*10, color ="Dolar"))+
  labs(x="Fecha", y="IPC", title="Evolución del IPC y Dolar")+
  theme_bw()+
  scale_y_continuous(sec.axis = sec_axis(~.*.1, name = "Dolar"))

#IPC versus dolar por aperturas

ggplot()+
  geom_line(data = ipc_dolar,aes(date,ipc))+
  geom_line(data = ipc_dolar,aes(date,tipoCambio*7, color ="Dolar"))+
  scale_y_continuous(sec.axis = sec_axis(~./7, name = "Dolar"))+
  scale_colour_manual("Color",values=c("darkgreen"))+
  theme_bw()+
  labs(x="Fecha", y="IPC", title="Evolución del IPC y Dolar por grupo")+
  facet_wrap(.~Apertura)



