
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
#Nueva Zelanda
#Partimos de:
nz <- filter(gapminder, country == "New Zealand")
nz %>% 
  ggplot(aes(year, lifeExp)) + 
  geom_line() + 
  ggtitle("Y = ")

#Generamos el modelo lineal
nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>% 
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) + 
  geom_line() + 
  ggtitle(expression(beta[0] + beta[1]*x))


nz %>% 
  add_residuals(nz_mod) %>% 
  ggplot(aes(year, resid)) + 
  geom_hline(yintercept = 0, colour = "white", size = 3) + 
  geom_line() + 
  ggtitle(expression(+ epsilon))



#Colombia
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

glance(modeloColombia)
glance(nz_mod)

#Ahira con mutate y unnest creamos un modelo por país
by_country %>% 
  mutate(glnc = map(model, glance)) %>% 
  unnest(glnc)

#Si queremos sacarnos de encima las columnas de listas (data,model,resids),
#necesitamos agregar .drop = TRUE: DEPRECATED –> ahora es con select
glnc <- by_country %>% 
  mutate(glnc = map(model, glance)) %>% 
  unnest(glnc) %>% 
  select(-c(data,model,resids))
glnc

#Organizamos por las variables que tenemos

#r.suqared me muestra para quienes está funcionando mejor el modelo
#Para ver para que paises funciona peor (para áfrica pareciera funcionar peor)
glnc %>% 
  arrange(r.squared)
#Para ver para que paises funciona mejor (para las américas y europa pareciera funcionar mejor)
glnc %>% 
  arrange(desc(r.squared))
#O por statistic que parece interesante
glnc %>% 
  arrange(desc(statistic))

#Graficamente comparando por continente, vemos que en África tenemos mucha variabilidad del modelo
#La variabilidad para el de oceanía pareciera mostrar que anda bien (o que tiene pocos datos)
glnc %>% 
  ggplot(aes(continent, r.squared, fill = continent)) + 
  geom_boxplot()+
  theme(legend.position = "none")

#En una gráfica diferente
glnc %>% 
  ggplot(aes(continent, r.squared)) + 
  geom_jitter(width = 0.5)

#Si queremos ver los 6 peores modelos
#Aquí vemos dos efectos principales: las tragedias de la epidemia de VIH / SIDA 
#y el genocidio de Rwanda.

bad_fit <- filter(glnc, r.squared < 0.25)

gapminder %>% 
  semi_join(bad_fit, by = "country") %>% 
  ggplot(aes(year, lifeExp, colour = country)) +
  geom_line()


#El comando tidy(model) nos permite obtener la salida del modelo lineal de forma prolija.
#En el caso particular de un modelo. El output tradicional se vería de esta forma

nz_mod <- lm(lifeExp ~ year, data = nz)
summary(nz_mod)

modeloColombia <- lm(lifeExp ~ year, data = expectativaColombia)
summary(modeloColombia)




#El que tiene el coeficiente mpás grande es Omán, que tiene un cambio mayor en 
#la expectativa de vida








