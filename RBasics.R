#RBasics


test <- paste("Pega", "estas", 4, "palabras", sep = "-")
test <- paste0(1,2, 1,2)
sum(1:5)
#Crear Un vector

elVector <- c(1,2,3,7)
elVector <- elVector +2

#Sumar una lista a un vector
elVector <- elVector + 1:4

#Vector de nombres
vectorNombres <- c("Nathaly", "Canela", "Simba")
vectorNombres[3]

#Borrar elementos
deleteMe <- vectorNombres[2]
rm(deleteMe)

#DataFrames
INDICE  <- c(100,   100,   100,
             101.8, 101.2, 100.73,
             102.9, 102.4, 103.2)

FECHA  <-  c("Oct-16", "Oct-16", "Oct-16",
             "Nov-16", "Nov-16", "Nov-16",
             "Dic-16", "Dic-16", "Dic-16")


GRUPO  <-  c("Privado_Registrado","Público","Privado_No_Registrado",
             "Privado_Registrado","Público","Privado_No_Registrado",
             "Privado_Registrado","Público","Privado_No_Registrado")


Datos <- data.frame(INDICE, FECHA, GRUPO)
Datos
rm(INDICE)
rm(FECHA)
rm(GRUPO)

#Obtener Columna Fecha
Datos$FECHA
Datos$FECHA[3]
Datos[3,2]

#Media de los datos de diciembre
Indices_Dic <- Datos$INDICE[Datos$FECHA=="Dic-16"]
Indices_Dic
mean(Indices_Dic)

#Listas
A <- factor("Soy un factor, con niveles fijos")
B <- A
C <- c(1,2,3,7)
E <- C +3

superlista <- list(A,B,C,D,E,FECHA, DF = Datos, INDICE, GRUPO)
superlista
superlista$DF$FECHA[2]
getwd()

