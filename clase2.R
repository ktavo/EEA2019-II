#Clase 2 EEA

rm(list=ls())
setwd("E:/UBA/2019-II/EEA/R Code")

#install.packages('ggridges', dependencies=TRUE, repos='http://cran.rstudio.com/')

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



#ggplot2
#ggplot tiene su sintaxis propia. La idea central es pensar los gráficos como una sucesión de capas, que se construyen una a la vez.
#El operador + nos permite incorporar nuevas capas al gráfico.

#El comando ggplot() nos permite definir los datos y las variables (x,y,color,forma,etc).
#Las sucesivas capas nos permiten definir:
  #Uno o más tipos de gráficos (de columnas, geom_col(), de línea, geom_line(), de puntos,geom_point(), boxplot, geom_boxplot())
  #Títulos labs()
  #Estilo del gráfico theme()
  #Escalas de los ejes scale_y_continuous,scale_x_discrete
  #División en subconjuntos facet_wrap(),facet_grid()

library(ggplot2)
library(ggthemes)  # estilos de gráficos
library(ggrepel)   # etiquetas de texto más prolijas que las de ggplot
library(scales)    # tiene la función 'percent()'


ggplot(data = iris, aes(x = Petal.Length, Petal.Width, color = Species))+
  geom_point(alpha=0.75)+
  labs(title = "Medidas de los pétalos por especie")+
  theme(legend.position = 'none')+
  facet_wrap(~Species)

#Paso a paso
#Ejes
g <- ggplot(data = iris, aes(x = Petal.Length, Petal.Width, color = Species))
g
#Gráfico ocn densidad de puntos
g <- g +  geom_point(alpha=0.25)
g

#Paso siguiente
#Definir el título del gráfico
#Quitar la leyenda
#Abrir el gráfico en tres fragmentos, uno para cada especie
g <- g +
  labs(title = "Medidas de los pétalos por especie")+
  theme(legend.position = 'none')+
  facet_wrap(~Species)
g

#Extensiones de ggplot GGally
library(GGally)
ggpairs(iris,  mapping = aes(color = Species))


#Extensiones de ggplot ggridges
library(ggridges)
ggplot(iris, aes(x = Sepal.Length, y = Species, fill=Species)) + 
  geom_density_ridges()

#Ejemplo con individuo t117
Individual_t117 <- read.table(paste0("../CodigoProf/Fuentes/usu_individual_t117.txt"),
                              sep=";", dec=",", header = TRUE, fill = TRUE)

ggdata <- Individual_t117 %>% 
  filter(P21>0, !is.na(NIVEL_ED)) %>% 
  mutate(NIVEL_ED = as.factor(NIVEL_ED),
         CH04     = as.factor(CH04))


ggplot(ggdata, aes(x = NIVEL_ED, y = P21, group = NIVEL_ED, fill = NIVEL_ED )) +
  geom_boxplot()+
  scale_y_continuous(limits = c(0, 40000))


ggplot(ggdata, aes(x= NIVEL_ED, y = P21, group = NIVEL_ED, fill = NIVEL_ED )) +
  geom_boxplot()+
  scale_y_continuous(limits = c(0, 40000))+
  facet_wrap(~ CH04, labeller = "label_both")


ggplot(ggdata, aes(x= CH04, y = P21, group = CH04, fill = CH04 )) +
  geom_boxplot()+
  scale_y_continuous(limits = c(0, 40000))+
  facet_wrap(~ NIVEL_ED, labeller = "label_both")


#Kernels

datagraf <-Individual_t117 %>% 
  select(REGION,P47T,T_VI, TOT_P12, P21 , PONDII, CH04) %>% 
  filter(!is.na(P47T), P47T > 0 ) %>% 
  mutate(REGION             = case_when(REGION == 1    ~ 'GBA',
                                        REGION == 40   ~ 'NOA',
                                        REGION == 41   ~ 'NEA',
                                        REGION == 42   ~ 'Cuyo',
                                        REGION == 43   ~ 'Pampeana',
                                        REGION == 44   ~ 'Patagonia',
                                        FALSE          ~ 'otro'),
         ingreso_laboral    = as.numeric(TOT_P12 + P21),
         ingreso_no_laboral = as.numeric(T_VI),
         CH04               = case_when(CH04 == 1 ~ "Varon",
                                        CH04 == 2 ~ "Mujer",
                                        FALSE     ~ "Otro") ) %>% 
  gather(., key = Tipo_ingreso, Ingreso, c((ncol(.)-1):ncol(.)))
datagraf  


datagraf2 <- datagraf %>% filter( Ingreso !=0)

ggplot(datagraf2, aes(
  x = Ingreso,
  weights = PONDII,
  group = Tipo_ingreso,
  fill = Tipo_ingreso)) +
  geom_density(alpha=0.7,adjust =2)+
  labs(x="Distribución del ingreso", y="",
       title=" Total según tipo de ingreso y género", 
       caption = "Fuente: Encuesta Permanente de Hogares")+
  scale_x_continuous(limits = c(0,50000))+
  theme_tufte()+
  scale_fill_gdocs()+
  theme(legend.position = "bottom",
        plot.title      = element_text(size=12))+
  facet_wrap(~ CH04, scales = "free")

ggsave(filename = paste0("../CodigoProf/Resultados/", "Kernel_1.png"),scale = 2)


ggplot(datagraf2, aes(
  x = Ingreso,
  weights = PONDII,
  group = CH04,
  fill = CH04)) +
  geom_density(alpha=0.7,adjust =2)+
  labs(x="Distribución del ingreso", y="",
       title=" Total según tipo de ingreso y género", 
       caption = "Fuente: Encuesta Permanente de Hogares")+
  scale_x_continuous(limits = c(0,50000))+
  theme_tufte()+
  scale_fill_gdocs()+
  theme(legend.position = "bottom",
        plot.title      = element_text(size=12))+
  facet_wrap(~Tipo_ingreso, scales = "free")

ggsave(filename = paste0("../CodigoProf/Resultados/", "Kernel_2.png"),scale = 2)


#Tendencia

ggdata <- Individual_t117 %>% 
  filter(P21>0,
         !is.na(NIVEL_ED),
         NIVEL_ED!=7, 
         PP04A !=3) %>% 
  mutate(NIVEL_ED = factor(case_when(NIVEL_ED == 1  ~ 'Primaria \n Incompleta', # '\n' significa carriage return, o enter
                                     NIVEL_ED == 2  ~ 'Primaria \n Completa',
                                     NIVEL_ED == 3  ~ 'Secundaria \nIncompleta',
                                     NIVEL_ED == 4  ~ 'Secundaria \nCompleta',
                                     NIVEL_ED == 5  ~ 'Superior \nUniversitaria \nIncompleta',
                                     NIVEL_ED == 6  ~ 'Superior \nUniversitaria \nCompleta',
                                     FALSE          ~ 'Otro'),
                           levels= c('Primaria \n Incompleta',
                                     'Primaria \n Completa',
                                     'Secundaria \nIncompleta',
                                     'Secundaria \nCompleta',
                                     'Superior \nUniversitaria \nIncompleta',
                                     'Superior \nUniversitaria \nCompleta')),
         Sexo     = case_when(CH04 == 1 ~ 'Varón',
                              CH04 == 2 ~ 'Mujer'),
         Establecimiento    = case_when(PP04A == 1 ~ 'Estatal',
                                        PP04A == 2 ~ 'Privado',
                                        FALSE      ~ 'Otro'))

ggdata

ggplot(ggdata, aes(CH06, P21, colour = Sexo, shape = Sexo, alpha = P21))+
  geom_smooth() + 
  labs(
    x = 'Edad',
    y = 'ingreso',
    title = 'Ingreso por ocupación principal',
    subtitle = 'Según edad, nivel educativo y sexo') +
  theme_minimal()+
  scale_y_continuous(labels = comma)+
  scale_alpha(guide = FALSE)+
  facet_grid(.~NIVEL_ED)



ggplot(ggdata, aes(CH06, P21, colour = Sexo, weight = PONDIIO)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  labs(x = 'Edad',
       y = 'ingreso',
       title = 'Regresion cuadrática del Ingreso por ocupación principal respecto de la Edad',
       subtitle = 'Según Nivel educativo y sexo') +
  theme_minimal()+
  facet_grid(. ~ NIVEL_ED)



ggplot(ggdata, aes(CH06, P21, colour = Establecimiento, weight = PONDIIO)) +
  geom_smooth(method = "lm") +
  labs(
    x = 'Edad',
    y = 'ingreso',
    title = 'Tendencia del ingreso por ocupación principal',
    subtitle = 'Según edad, nivel educativo, sexo y tipo de establecimiento') +
  theme_minimal()+
  facet_grid(Sexo ~ NIVEL_ED)

ggsave(filename = paste0("../Resultados/", "regresion lineal.png"),scale = 2)
















