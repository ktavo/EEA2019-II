#Ejemplo regresión lineal

install.packages('utf8', dependencies=TRUE, repos='http://cran.rstudio.com/')

rm(list=ls())
setwd("E:/UBA/2019-II/EEA/R Code")

bases_individuales_path <- dir_ls(path = '../CodigoProf/Fuentes', regexp= 'individual')
bases_individuales_path

bases_individuales_path1 <- '../CodigoProf/Fuentes/usu_individual_t316.txt'


leer_base_eph <- function(path) {
  # Lectura de archivo
  read.table(path,sep=";", dec=",", header = TRUE, fill = TRUE) %>%
    # Seleccion de variables relevantes
    select(ANO4,TRIMESTRE,REGION,P21,CH04, CH06)
}
# Leer a un dataframe las tablas especificadas en el vector de bases individuales
bases_df <- tibble(bases_individuales_path1) %>%
  mutate(base = map(.x = bases_individuales_path1, .f = leer_base_eph))

bases_df <- bases_df %>% unnest()
bases_df

# Calculamos una regresion lineal sobre el dataset que tiene todas las observaciones de EPH
lmfit <- lm(P21~factor(CH04)+CH06,data = bases_df)
# Resumen del modelo
summary(lmfit)


#Con loop
resultados <- tibble()
for (region in unique(bases_df$REGION)) {
  # Creamos un dataset por region
  data <- bases_df %>% 
    filter(REGION==region)
  # Calcular el modelo lineal
  lmfit <- lm(P21~factor(CH04)+CH06,data = data)
  # Guardar los resultados de los coeficientes 
  lmtidy <- broom::tidy(lmfit)
  # Asignamos la variable region
  lmtidy$region <- region
  # Unimos los dataframes
  resultados <- bind_rows(resultados,lmtidy)
}
resultados

#Usando MAP
fun<-function(porcion,grupo) {  broom::tidy(lm(P21~factor(CH04)+CH06,data = porcion))}

bases_df_lm <- bases_df %>% 
  # Agrupamos por region
  group_by(REGION) %>%
  # Anidamos
  nest() %>% 
  # Creamos una columna que tenga el dataframe como resultado de la funcion
  mutate(lm = map(data,fun))
bases_df_lm

bases_df_lm %>% 
  unnest(lm)

bases_df %>% 
  group_by(REGION) %>% 
  group_modify(fun)


# La funcion calcula el modelo lineal sobre cada porcion de datos
fun<-function(porcion,grupo) {  lm(P21~factor(CH04)+CH06,data = porcion)}
bases_df %>%
  # Agrupar por region
  group_by(REGION) %>%
  # Anidar
  nest() %>%
  # Calcular el modelo lineal
  mutate(lm = map(data,fun))
