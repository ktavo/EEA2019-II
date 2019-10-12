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



#*********Dplyr joins************#
#Left Join
Ponderadores <- data.frame(GRUPO = c("Privado_Registrado","Público","Privado_No_Registrado"),
                           PONDERADOR = c(50.16,29.91,19.93))

Datos_join <- Datos %>% 
  left_join(.,Ponderadores, by = "GRUPO")
Datos_join

#Calculando el índice ponderado con weighted.mean 
Datos_Indice_Gral <- Datos_join %>% 
  group_by(FECHA) %>% 
  summarise(Indice_Gral = weighted.mean(INDICE,w = PONDERADOR))
Datos_Indice_Gral


#Tidyr facilitará el emprolijamiento de los datos
#aather pasar los datos de forma horizontal a una forma vertical.
#spread pasar los datos de forma vertical a una forma horizontal.


#Utilzamos un conjunto de datos que viene con la librería datasets
library(datasets)

data(iris)
iris <- iris %>% 
  mutate(id = 1:nrow(.)) %>%  #le agrego un ID
  select(id, everything()) # lo acomodo para que el id este primero. 

iris

#Usando gather
iris_vertical <- iris %>% gather(., # el . llama a lo que esta atras del %>% 
                                 key   = Variables,
                                 value = Valores,
                                 2:5) #le indico que columnas juntar
iris_vertical

#usando spread
iris_horizontal <- iris_vertical %>%
  spread(. ,
         key   = Variables, #la llave es la variable que va a dar los nombres de columna
         value = Valores) #los valores con que se llenan las celdas
iris_horizontal

#Lubridate para trabajar con fechas
#Para cambio de formato
fecha  <- "04/12/92 17:35:16"
fecha

fecha  <- dmy_hms(fecha)
fecha

#parse_date_time permite operaciones un poco más complejas
fecha2  <- "Dec-92"
fecha2 <- parse_date_time(fecha2, orders = 'my')
fecha2

#extraer información de la fecha
year(fecha) # Obtener el año
month(fecha) #Obtener el mes
day(fecha) # Obtener el día
wday(fecha, label = TRUE) #Obtener el nombre del día
hour(fecha) #Obtener la hora

#operaciones
# Sumo dos días 
fecha + days(2)
# Resto 1 semana y dos horas
fecha - (weeks(1) + hours(2))










