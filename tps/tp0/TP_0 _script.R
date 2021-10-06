# *** Bienvenidos a Biometria II 2021 *** #
# Este es el script asociado al TP # 0
# Deben resolverlo antes de comenzar la cursada

# Creen una carpeta en el disco C llamada "Biome2" y dentro de ella
# otra carpeta llamada "TP_0"
# y guarden alli los archivos para el ejercicio.
# Vamos a importar datos desde una tabla, "datos_intro.txt"
# Guarden el archivo como texto delimitado por tabulaciones (.txt)

# Corroboren que el archivo este guardado con la siguiente
# ruta "C:/Biome2/TP_0/datos_intro.txt"

# Importamos los datos al R

# Primero seteamos el directorio de trabajo
# Ete paso es conveniente hacerlo al inicio del script
setwd("~/git/biome2/tps/tp0/")

#en MAC ir a Session/Set working directory/Choose directory/seleccionar la carpeta

# Para importar la base a la que llamaremos "datos"
datos <- read.table("TP_0_datos.txt", header = T)
#el parametro header sirve para considerar a la primer fila del archivo como encabezado de columnas por ejemplo. Debe ser true (T) si queremos que las tome como tal.

# Alternativamente se puede importar la base de datos desde el comando "Import Dataset"
# (ventana superior derecha)

# Investigue como puede abrir data frame en R con las extensiones
# .txt
# .csv
# .xls

# practique abrir tablas en estos formatos

# El data.frame datos_intro contiene datos provenientes de un estudio realizado en hospitales de referencia cuyo objetivo fue estudiar si el peso al nacer y la talla ("Largo" en cm, como medida del tamanio de los bebes) de los mismos estaba relacionada con la edad de la madre (en anios), la edad gestacional (medida en semanas) y si la madre tuvo o no eclampsia durante el embarazo (la eclampsia, entre otras complicaciones, aumenta la presion sanguinea de la madre durante el embarazo).
# El diagnostico de eclampsia se codifico con un 1 si la madre tuvo eclampsia y con 0 si la madre no tuvo.
# Para visualizar la tabla

datos
View(datos)
head(datos)
# Revise si la tabla se abrio correctamente
# Chequee si la base consta de 100 filas y 6 columnas
nrow(datos)
ncol(datos)
dim(datos)

# que clase de objeto es?
class(datos)
#que clase de variables tenemos?
str(datos)

# descriptiva general
summary(datos)

# cuantos casos corresponden a mujeres con y cuantos a mujeres sin eclampsia?
table(datos$Eclampsia)

# Seleccionar un subconjunto de variables
# Creamos un vector con los nombres de las variables que queremos seleccionar
misvars <- c("EdadGest", "pesoalnacer", "Eclampsia")

# Generamos una nueva tabla extrayendo las variables de la tabla original
nuevaTabla <- datos[misvars]
names(nuevaTabla)#aclara que variables hay en la tabla
nuevaTabla

# Podemos obtener el mismo resultado, usando la funcion "subset"
nuevaTabla2 <- subset(datos, select=c("EdadGest", "pesoalnacer", "Eclampsia"))
names(nuevaTabla2)
nuevaTabla2

##NOTA IMPORTANTE: CUANDO SE CREA UNA VARIABLE EL NOMBRE DEBERIA SER INFORMATIVO
##DE LA FUNCION. SINO SE CORRE EL RIESGO DE PERDERSE EN SCRIPTS COMPLEJOS.

#otra manera con subset
subset(datos[,misvars])

# Seleccionar casos, por ejemplo los primeros 20
tabla20 <- datos[1:20,]
View(tabla20)

# Seleccinamos los primeros 20 y los ultimos 10
tabla_sel <- datos[c(1:20,91:100),]

# borramos el primer caso
tabla_menos_1 <- datos[-1,]
head(tabla_menos_1)

# borramos los casos 1, 3, 5 y 8
tabla_menos_algunos <- datos[c(-1,-3,-5,-8),]

# Visualice las tablas anteriores y compruebe que selecciono o elimino
# lo que realmente queria

# Y si queremos seleccionar los casos con eclampsia?
datosEclamp <- datos[datos$Eclampsia=="1",]


# Investigue otras formas alternativas de armar la base datosEclamp
datos[datos$Eclampsia==1,]
datos[which(datos$Eclampsia==1),]

# La variable Eclampsia fue codificada como 1 y 0 y el R la tomo como numero entero
# (como puede comprobar esto?)
# Pero nos podria interesar que sea categorica (1, tuvo, 0 no tuvo)

class(datos$Eclampsia) #"integer"
datos$Eclampsia_factor <- as.factor(datos$Eclampsia)
class(datos$Eclampsia_factor) # "factor"

#A veces queremos agregar columnas que surgen operaciones de otras columnas
datos <- cbind(datos, datos$pesoalnacer/datos$Largo)
head(datos)

#Otras veces queremos ordenar un data.frame de acuerdo a alguna columna
datos_orden_peso <- datos[order(datos$pesoalnacer),]
head(datos_orden_peso)


# Un poco mas de practica...

#Calculamos la media de datos.
mean(datos)
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