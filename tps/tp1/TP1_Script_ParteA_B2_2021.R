# *** Bienvenidos a Biometria II 2021 *** #
# Este es el script asociado al TP # 1 Parte A


# Antes que nada veamos si hay algun comando u objeto almacenado en la memoria
ls()
# Si te aparece character(0) es que la memoria esta limpia, si no ejecuta
# la siguiente sentencia para borrar la memoria.
rm(list=ls())
# revisamos...
ls()
# No deberia haber nada en el entorno de trabajo ahora!


# Librerias que vamos a utilizar en este TP
library(pastecs) # Para la funcion stat.desc
library(tableone)  # Tablas de estadistica descriptiva

# el comando "library" llama (activa) una libreria si esta ya fue previamente instalada
# (se puede chequear si esta instalada mirando en la solapa "packages" en la ventana gr?fica)
# si no esta el paquete instalado, antes de llamar a la libreria instalada hay que instalarlo usando la funcion install.packages()
# Ejemplo:
# install.packages("pastecs")



# Setee el directorio de trabajo (Fijate en el TP 0 como lo hicimos)
setwd("~/git/biome2/tps/tp1/")


#############################################################################
## ## Ejercicio  1 "Caracteristicas morfologicas de plantas del genero Iris"
#############################################################################

# La base de datos con la que vamos a estar trabajando se denomina "Iris" (Fisher, 1936).
# Fue recolectada por Edgar Anderson con el objetivo de identificar las caracteristicas morfologicas de plantas del genero Iris que permitiesen diferenciar distintas especies.
# La base consta de datos de cuatro rasgos de flores (longitud y ancho del sepalo, longitud y ancho del petalo; todas en centimetros), correspondientes a 150 ejemplares pertenecientes a tres especies: Iris setosa, Iris versicolor e Iris virginica.

# Abrimos y exploramos el data.frame con el que vamos a trabajar
# Lo vamos a llamar "datos".

datos  <- read.delim("Iris.txt")

# Exploramos caracter?sticas / estructura del data.frame

View(datos)
head(datos)
nrow(datos)
ncol(datos)
dim(datos)

names(datos)
str(datos)
head(datos)
class(datos$especie)
class(datos$LongSepalo)

# Estadistica descriptiva / medidas resumen

summary(datos)

# y si queremos efectuar estadistica descriptiva por especie?
tapply(datos$LongSepalo, datos$especie, summary)

# y si queremos obtener el desvio estandar?
tapply(datos$LongSepalo, datos$especie, sd)

# y si queremos mas medidas resumen?
tapply(datos$LongSepalo, datos$especie, stat.desc)


names(datos)

# Descriptiva con table1
descriptiva <- CreateTableOne(
                  vars= c("LongSepalo","AnchoSepalo", "LongPetalo","AnchoPetalo"),
                  strata= "especie",
                  datos,
                  test= F)


descriptiva

# Para conocer mas de CreateTableOne
?CreateTableOne


# tabla de frecuencias

tabla_frecuencias <- table(datos$especie, useNA = "always")
tabla_frecuencias

# frecuencias relativas
prop.table(tabla_frecuencias)

# Mas adelante exploraremos "prop.table" para tablas con dos variables


# graficos exploratorios sencillos

plot(datos)
# graficamos separado por especie
plot(datos[,2:5], col=datos$especie, main="Asoc entre caracteristicas morfologicas de Sp del gen Iris")

# multiples subplots
par(mfrow=c(2,2)) # este comando sirve para incluir mas de un grafico en la ventana,
                  # indicando par(mfrow=c(N filas, N columnas))
# en este caso divide el panel en 2 filas y 2 columnas, permitiendo incorporar 4 graficos al mismo panel.

hist(datos$LongSepalo)
hist(datos$LongPetalo)
hist(datos$AnchoSepalo)
hist(datos$AnchoPetalo)

# subset para una especie

Setosa <- subset(datos, especie=="Setosa") #creamos el subset

# Setosa
par(mfrow=c(2,2))
hist(Setosa$LongSepalo)
hist(Setosa$LongPetalo)
hist(Setosa$AnchoSepalo)
hist(Setosa$AnchoPetalo)

# boxplot - comparacion variables entre especies
par(mfrow=c(2,2))
plot(datos$LongSepalo~datos$especie)
plot(datos$LongPetalo~datos$especie)
plot(datos$AnchoPetalo~datos$especie)
plot(datos$AnchoSepalo~datos$especie)

# observe las diferencias de codigo entre los plot
# plot para subsets
plot(datos[1:50,2:5], main="Iris setosa") # subset para setosa
plot(datos[51:100,2:5], main="Versicolor ")

# observo las diferencias entre los dos graficos en
# la sintaxis y la salida obtenida

# ?Como se relacionan la longitud con el ancho del sepalo para la especie Setosa?
# esta pregunta la podemos responder con el coef de correlacion

cor(Setosa$LongSepalo, Setosa$AnchoSepalo) # 0.7425467

#graficamente
par(mfrow=c(1,1)) # vuelvo a configurar un unico grafico en la ventana
plot(Setosa$LongSepalo, Setosa$AnchoSepalo)

# Si queremos estimar la media poblacional de la long de sepalo a partir de nuestra muestra
# construimos un intervalo de confianza para la media de long de sepalo en Setosa
t.test(Setosa$LongSepalo,
       conf.level=0.95)

## Repasamos modelo lineal para comparacion de medias (O SEA "ANOVA DE 1 FACTOR")
#  ?Difieren las especies en relacion a la longitud de sepalo?
# Escribimos la sintaxis para un modelo lineal

m1 <- lm(LongSepalo~especie, data=datos)

## Con la funci?n lm (linear model) se indica la implementacion de un modelo
#  lineal para explicar, en este caso, la longitud del s?palo -que es una
#  variable cuantitativa continua con potencial distribuci?n normal- seg?n la
#  variable explicatoria especie -que es una variable categorica con 3 niveles-)


# para repasar:
# cuales son los supuestos de este modelo?

# Le pedimos la "tabla de ANOVA" del modelo
anova(m1)

# Interprete  salida

# Supuestos?
par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(1,1))

# QQ-plot con car
rduo<-residuals(m1)
library(car)
qqPlot(rduo)
# + info
?qqPlot

# comparaciones a posteriori
# como analizarian la magnitud del efecto?
library(emmeans)

# seteo de opciones de salida
options(emmeans= list(emmeans = list(infer = c(TRUE, TRUE)),contrast = list(infer = c(TRUE, TRUE))))

# comparaciones
comp <- emmeans(m1, pairwise ~ especie)
comp

# comparaciones y plot
comp$contrasts
plot(comp$emmeans, comparisons = TRUE)


# medias y plot
confint(comp$emmeans)
plot(comp$emmeans)


# Grafico (una de muchas opciones gr?ficas!)
# extraemos las medidas resumen

resumen_modelo <-as.data.frame(comp$emmeans)

# exploramos el objeto resumen_modelo
resumen_modelo  # emmeans es la media estimada

# Plot
library(ggplot2)
ggplot(resumen_modelo, aes(x=especie, y=emmean)) +
  labs(x="especie") + labs(y="longitud sepalo") +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), color="blue", width=0.2)+
  geom_point(shape=1, size=2, color="blue") +
  ylim(0,8)+
  ggtitle("Comparaci?n de long de sepalo entre especies", "Media ? Error est?ndar") +
  annotate("text", x = c("Setosa","Versicolor", "Virginica"), y=c(6,7,8), label = c("A", "B", "C"))




################################################################################
## Ejercicio  2 "Rendimiento de Girasol en el Oeste de la Prov de Buenos Aires"
################################################################################

## Ejercicio inspirado en ejemplo de RLS de Puhl y Perelman (2019)

## Se desea predecir el rendimiento del girasol (kg/ha) en el oeste de la Provincia de Buenos Aires en funcion de la cobertura del suelo generada por el cultivo al momento de la floracion.

## Para ello se eligieron 9 valores de cobertura del suelo que variaron entre 50 y 90 %, dentro de las cuales se seleccionaron al azar 8 lotes sembrados en el oeste de la Provincia de Buenos Aires a los que posteriormente se les registr? el rendimiento.

## En este problema, tendr? la particularidad de que en lugar de contar con una unica muestra, cada grupo contara con su propia muestra, es decir existe un set de datos distinto por equipo.

## Trabajo grupal:
##
## 1. Trabaje con la muestra obtenida por su grupo ('su muestra') de valores de rendimiento para cada uno de los valores de cobertura estudiados
##    Para ello descargue una base con registros desde: https://msfernandez.shinyapps.io/appmuestras/
##    Introduza su numero de LU (5 numeros) y obtendra la muestra de su equipo
## 2. Cargue la base en R Studio y confirme que los datos se hayan cargado correctamente
##    (recuerde que las variables numericas son de clase "integer" si son discretas y "numeric" si son continuas
## 3. Confirme cuantas observaciones por nivel de cobertura existen
## 4. Realice un gr?fico de dispersi?n de Y ~ X
## 5. Plantee el modelo en parametros
## 6. Enuncie los supuestos del modelo
## 7. Estime beta 0 y beta 1 del modelo. Calcule el intervalo de confianza para la pendiente       e interpretelo
##    ***** Ayuda para implementar el modelo: nombre_modelo <- lm(Rend~Cob, nombre_data.frame)
##    ***** Calculo de IC para los coeficientes: confint(nombre_modelo)

## 8. Vuelque en la planilla compartida estos 4 valores: valor estimado para beta 0, beta 1, el p-valor para la prueba de hipotesis de beta 1, y la varianza del modelo.
## 7. Cual es el R2 del modelo? Interprete el valor. Vuelque el valor en la planilla
## 8. Compare con los resultados de sus compa?eros. ?Como son las estimaciones entre los grupos? ?Por que son distitnas? ?Alguna es mejor que otra?





