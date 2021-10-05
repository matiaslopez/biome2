# *** Bienvenidos a Biometria II 2021 *** #
# Este es el script asociado al TP # 0
# Deben resolverlo antes de comenzar la cursada

# Nota util antes de comenzar:
# Este archivo .R es lo que se conoce como un "script de R"
# El simbolo # sirve para hacer comentarios, no interpretados por el R.
# Para ver los elementos listados usar el siguiente comando
ls()
# Si no tenemos nada aparecera "character(0)"
# Si tenemos algo aparecera la lista de elementos (objetos) 

# Para borrar todos los elementos que tenemos listados
rm(list=ls())

# Usar R como calculadora
2+7
4^3
10/2
sqrt(9)
log(1)
exp(1)
cos(pi)
0/0
# Para generar secuencias de numeros
1:30  # Secuencia regular de numeros enteros, por ejemplo de 1 hasta 30.
# La funcion seq puede generar secuencias de numeros reales:
seq(1, 5, 0.5)
# Tambien se puede usar:
seq(length=9, from=1, to=5)
#para mas informacion sobre esta funcion
?seq()

# Crear objetos
X <-1:7 #indicamos que llame "X" a la secuencia, es decir que cree un objeto conteniendo a esa secuencia
X #ahora podemos invocar al objeto cuando querramos y ver que valores lo componen 
Y<-c(10,15,20,25,30,35,40) #"c" concatenamos los valores indicados y creamos un objeto que los contenga a todos
Y

#Podemos pedir la estadistica basica del objeto creado:
sum(Y)
mean(Y)
var(Y)
min(Y)
range(Y)
median (Y)
summary(Y)
#Crear matrices:
M<-matrix(c(2,3,5,7,11,13,15,17,19),ncol=3)
M
#Extraer primera fila de la matriz M
M[1,]
#Extraer tercera columna
M[,3]

# Importar bases de datos

# Creen una carpeta en el disco C llamada "Biome2" y dentro de ella 
# otra carpeta llamada "TP_0" 
# y guarden alli los archivos para el ejercicio.
# Vamos a importar datos desde una tabla, "Datos_intro.txt"
# Guarden el archivo como texto delimitado por tabulaciones (.txt) 

# Corroboren que el archivo este guardado con la siguiente 
# ruta "C:/Biome2/TP_0/Datos_intro.txt"

# Importamos los datos al R

# Primero seteamos el directorio de trabajo
# Ete paso es conveniente hacerlo al inicio del script
setwd("C:\\Biome2\\TP_0\\") 
#en MAC ir a Session/Set working directory/Choose directory/seleccionar la carpeta 

# Para importar la base a la que llamaremos "Datos"
Datos <- read.table("TP_0_datos.txt", header = T)
#el parametro header sirve para considerar a la primer fila del archivo como encabezado de columnas por ejemplo. Debe ser true (T) si queremos que las tome como tal. 

# Alternativamente se puede importar la base de datos desde el comando "Import Dataset"
# (ventana superior derecha)

# Investigue como puede abrir data frame en R con las extensiones
# .txt
# .csv
# .xls

# practique abrir tablas en estos formatos 

# El data.frame Datos_intro contiene datos provenientes de un estudio realizado en hospitales de referencia cuyo objetivo fue estudiar si el peso al nacer y la talla ("Largo" en cm, como medida del tamanio de los bebes) de los mismos estaba relacionada con la edad de la madre (en anios), la edad gestacional (medida en semanas) y si la madre tuvo o no eclampsia durante el embarazo (la eclampsia, entre otras complicaciones, aumenta la presion sanguinea de la madre durante el embarazo).
# El diagnostico de eclampsia se codifico con un 1 si la madre tuvo eclampsia y con 0 si la madre no tuvo.
# Para visualizar la tabla

Datos
View(Datos)
head(Datos)
# Revise si la tabla se abrio correctamente
# Chequee si la base consta de 100 filas y 6 columnas
nrow(Datos)
ncol(Datos)
dim(Datos)

# que clase de objeto es?
class(Datos) 
#que clase de variables tenemos?
str(Datos)

# descriptiva general
summary(Datos)

# cuantos casos corresponden a mujeres con y cuantos a mujeres sin eclampsia?
table(Datos$Eclampsia)

# Seleccionar un subconjunto de variables
# Creamos un vector con los nombres de las variables que queremos seleccionar
misvars <- c("EdadGest", "pesoalnacer", "Eclampsia")

# Generamos una nueva tabla extrayendo las variables de la tabla original
nuevaTabla <- Datos[misvars]
names(nuevaTabla)#aclara que variables hay en la tabla
nuevaTabla

# Podemos obtener el mismo resultado, usando la funcion "subset"
nuevaTabla2 <- subset(Datos, select=c("EdadGest", "pesoalnacer", "Eclampsia"))
names(nuevaTabla2) 
nuevaTabla2

##NOTA IMPORTANTE: CUANDO SE CREA UNA VARIABLE EL NOMBRE DEBERIA SER INFORMATIVO
##DE LA FUNCION. SINO SE CORRE EL RIESGO DE PERDERSE EN SCRIPTS COMPLEJOS.

#otra manera con subset
subset(Datos[,misvars])

# Seleccionar casos, por ejemplo los primeros 20
tabla20 <- Datos[1:20,]
View(tabla20)

# Seleccinamos los primeros 20 y los ultimos 10
tabla_sel <- Datos[c(1:20,91:100),]

# borramos el primer caso
tabla_menos_1 <- Datos[-1,]
head(tabla_menos_1)

# borramos los casos 1, 3, 5 y 8
tabla_menos_algunos <- Datos[c(-1,-3,-5,-8),]

# Visualice las tablas anteriores y compruebe que selecciono o elimino 
# lo que realmente queria

# Y si queremos seleccionar los casos con eclampsia?
DatosEclamp <- Datos[Datos$Eclampsia=="1",]


# Investigue otras formas alternativas de armar la base datosEclamp
Datos[Datos$Eclampsia==1,]
Datos[which(Datos$Eclampsia==1),]

# La variable Eclampsia fue codificada como 1 y 0 y el R la tomo como numero entero
# (como puede comprobar esto?)
# Pero nos podria interesar que sea categorica (1, tuvo, 0 no tuvo)

class(Datos$Eclampsia) #"integer"
Datos$Eclampsia_factor <- as.factor(Datos$Eclampsia)
class(Datos$Eclampsia_factor) # "factor"

#A veces queremos agregar columnas que surgen operaciones de otras columnas
Datos <- cbind(Datos, Datos$pesoalnacer/Datos$Largo)
head(Datos)

#Otras veces queremos ordenar un data.frame de acuerdo a alguna columna
Datos_orden_peso <- Datos[order(Datos$pesoalnacer),]
head(Datos_orden_peso)


# Un poco mas de practica... 

#Calculamos la media de datos. 
mean(Datos)
# Se imagina por que nos devolvio "NA" ? 
# Seguramente le aparecio junto con el resultado un "Warning message"
# Lealo y seguramnete encontrara la respuesta

#Consideremos ahora el caso de los NA's (not available). Hay veces en los que
#algun objeto posee celdas con NA, o con variables que no son numericas.
#En esos casos hacer calculos que involucren al objeto puede arrojar resultados
#no deseados. Considere la siguiente matriz
matrizEjemplo=matrix(data=NA,5,5)
matrizEjemplo
#Calculamos la media. Que resultado arroja?
mean(matrizEjemplo)

#Reemplazamos algunos valores por 4.
matrizEjemplo[,2:5]=4
matrizEjemplo

#Como hacemos para eliminar los NA's?
matrizEjemplo[, colSums(is.na(matrizEjemplo)) != nrow(matrizEjemplo)]

#Trate de entender la diferencia con esta sintaxis
matrizEjemplo[ , colSums(is.na(matrizEjemplo)) == 0]

#Y ahora si le calculamos la media
mean(matrizEjemplo[, colSums(is.na(matrizEjemplo)) != nrow(matrizEjemplo)])