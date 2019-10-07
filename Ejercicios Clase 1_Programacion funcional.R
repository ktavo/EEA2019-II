#Reiniciar R


setwd("E:/UBA/2019-II/EEA/R Code")

#install.packages('tidyverse', dependencies=TRUE, repos='http://cran.rstudio.com/')

library(tidyverse)
library(purrr)

library(openxlsx)
library(ggthemes)
library(fs)

ABC_123 <- data.frame(Letras = LETTERS[1:20],Num = 1:20)
funcion_prueba <- function(parametro1,parametro2) {
  paste(parametro1, parametro2, sep = " <--> ")
}
funcion_prueba(parametro1 = "A ver", parametro2 = "Que pasa")
resultado <- map2(.x = ABC_123$Letras, .y = ABC_123$Num,.f = funcion_prueba)
resultado[1:3]
resultado[1:3] %>% unlist()

#Combinacion de todas las letras con todos los numeros
map(.x = ABC_123$Letras,.f = funcion_prueba,ABC_123$Num)[1:2]
ABC_123 %>% mutate(resultado= map(Letras,funcion_prueba,Num))

#Funciones anónimas
map_dbl(.x = c(1:10), .f = function(x) x^2) 

# Buscamos en el path aquellos aquellos archivos que matchean a la expresion regular
bases_individuales_path <- dir_ls(path = '../CodigoProf/Fuentes/', regexp= 'individual')
bases_individuales_path


leer_base_eph <- function(path) {
  # Lectura de archivo
  read.table(path,sep=";", dec=",", header = TRUE, fill = TRUE) %>%
    # Seleccion de variables relevantes
    select(ANO4,TRIMESTRE,REGION,P21,CH04, CH06)
}
# Leer a un dataframe las tablas especificadas en el vector de bases individuales
bases_df <- tibble(bases_individuales_path) %>%
    mutate(base = map(.x = bases_individuales_path, .f = leer_base_eph))






# Crear una **función** llamada _HolaMundo_ que imprima el texto "Hola mundo"

HolaM <- function(){
  print("Hola mundo")
}

HolaM()

# - Crear una **función** que devuelva la sumatoria de los números enteros comprendidos entre 1 y un parámetro _x_ a definir.

Sumatoria_enteros <- function(x){
  Vector <- 1:x
  return(sum(Vector))
}

Sumatoria_enteros(x = 10)

# - Levantar la base Individual del 4to trimestre de 2016

individual_t416 <- read.table("Fuentes/usu_individual_t416.txt",
                              sep=";", dec=",", header = TRUE, fill = TRUE)

# - Guardar la base Individual del 4to trimestre de 2016 como un archivo de extensión .RDS
saveRDS(individual_t416,"Resultados/Base_formato_r.RDS")

# - Volver a levantar la base, pero como .RDS y asignarla con el nombre _BaseRDS_ ¿tarda más o menos?
Base_RDS <- readRDS("Resultados/Base_formato_r.RDS")


# - Crear una **función** que calcule la frecuencia expandida por un ponderador a designar


expansion <- function(data){
  
  sum(data['PONDERA'])
}

expansion(individual_t416)



# - Utilizar dicha función para calcular la frecuencia poblaciónal por Sexo y Región



## usando dplyr
individual_t416 %>% 
  group_by(CH04, REGION) %>% 
  summarise(frecuencia_poblacional = sum(PONDERA))

# usando purrr
individual_t416 %>% 
  group_by(CH04, REGION) %>% 
  nest() %>% 
  mutate(frecuencia_poblacional=unlist(map(data,expansion)))



# - Modificar la función anterior para que devuelva un vector con la frecuencia muestra **y** la frecuencia poblacional

expansion <- function(data){
  
  tibble('frecuencia_poblacional'=sum(data['PONDERA']),'frecuencia_muestral'=nrow(data))
}

expansion(individual_t416)

# - Utilizar la función modificada para calcular la frecuencias muestrales y poblacionales por Sexo y Región
# usando purrr
individual_t416 %>% 
  group_by(CH04, REGION) %>% 
  nest() %>% 
  mutate(frecuencias=map(data,expansion)) %>% 
  unnest(frecuencias) #abrimos el dataframe resultante


