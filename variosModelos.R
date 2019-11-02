
rm(list=ls())

setwd("E:/UBA/2019-II/EEA/R Code")

#install.packages('ggridges', dependencies=TRUE, repos='http://cran.rstudio.com/')

#Clase 26-Oct
#Muchos modelos clase 1
library(modelr)
library(broom)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

library(gapminder)
gapminder

#En este estudio de caso, vamos a centrarnos solo en tres variables para responder a la pregunta
#“¿Cómo cambia la esperanza de vida (lifeExp) en el tiempo (year) para cada país (country)?”


#Gráfico de la expectativa de vida par alos paises
gapminder %>% 
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)


#Para calcular el modelo lineal para algunos países
#Partimos de:
expectativaColombia <- filter(gapminder, country == "Colombia")
expectativaColombia %>% 
  ggplot(aes(year, lifeExp)) + 
  geom_line() + 
  ggtitle("Y = ")

#Generamos el modelo lineal
modeloColombia <- lm(lifeExp ~ year, data = expectativaColombia)
expectativaColombia %>% 
  add_predictions(modeloColombia) %>%
  ggplot(aes(year, pred)) + 
  geom_line() + 
  ggtitle(expression(beta[0] + beta[1]*x))


expectativaColombia %>% 
  add_residuals(modeloColombia) %>% 
  ggplot(aes(year, resid)) + 
  geom_hline(yintercept = 0, colour = "white", size = 3) + 
  geom_line() + 
  ggtitle(expression(+ epsilon))

#Nested Data
#Usaremos nested data para ver las gráicas para mútiples países
by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()
#Data es una lista de dataframes acorde a la agrupación por países
by_country
#Para Afganistán:
by_country$data[[1]]

#Podemos usar la función para ajustar los modelos
country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}
#Para aplicar la función a todo el DataSet
#Queremos aplicarlo a cada uno de los dataframes anidados.
#Podemos usar purrr::map() para aplicar la función que definimos a cada elemento
models <- map(by_country$data, country_model)
#En lugar de crear un nuevo objeto en el entorno global, vamos a crear una nueva 
#variable en el DF by_country. Ese es un trabajo para dplyr :: mutate ():

by_country <- by_country %>% 
  mutate(model = map(data, country_model))
by_country

#Filtramos por continente ahora
by_country %>% 
  filter(continent == "Americas")

by_country %>% 
  filter(continent == "Europa")

#Organizamos por continente con arrange
by_country %>% 
  arrange(continent, country)

#Para añadir los residuos a todos los 142 modelos
by_country <- by_country %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  )
by_country
#Paa analizar los resultados necesitamos utilizar unnets
resids <- unnest(by_country, resids)
resids

#Con esto podemos graficar sobre los residuos
resids %>% 
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 / 3) + 
  geom_smooth(se = FALSE)

#Faceteando por continente
resids %>% 
  ggplot(aes(year, resid, group = country)) +
  geom_line(alpha = 1 / 3) + 
  facet_wrap(~continent)


#Clase 2-Nov
#Muchos modelos clase 2

#El que tiene el coeficiente mpás grande es Omán, que tiene un cambio mayor en 
#la expectativa de vida








