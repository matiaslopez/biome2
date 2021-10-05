#############################
#** Biometría II - 2021   **#
#**       TP Nº 4         **#
#**   Modelos múltiples   **#
#############################


#####################################################################################
## Problema 2.                                                                      #
## Regulación de la producción de aceites esenciales en cedrón (Aloysia citriodora) #
#####################################################################################

# Antes que nada veamos si hay algun comando u objeto almacenado en la memoria
ls()
# Si te aparece character(0) es que la memoria esta limpia, si no ejecuta
# la siguiente sentencia para borrar la memoria:
rm(list=ls())

# revisamos...
ls()


# Cargamos las librerias que vamos a utilizar

library(ggplot2) # Graficos

library(psych)    #estadística descriptiva
library(tableone) #estadística descriptiva

library(car)      # Para prueba de Levene, QQplot
library(emmeans)  # comparaciones


# setiemos nuestro directorio de trabajo ....
setwd ()
# setwd("~/Biome2_2021/TP_4_2021")

# Abramos el data.frame
Datos  <- read.delim("Cedron.txt")


#############################
# Inspeccion del data.frmae #
#---------------------------#
#############################

class(Datos)
str(Datos)
dim(Datos)
head(Datos)
summary(Datos)

# View(Datos) # abre el data.frame en una solapa

# Miremos como se llaman nuestras variables
names(Datos)

# Tipos de variables? # lo pueden ver con #str" (ver linea 44) o con "class"
class(Datos$provincia)
class(Datos$densidad)
class(Datos$aceite) 


#############################
# Analisis exploratorio     #
#---------------------------#
#############################

# Caracterice las muestras estadísticamente, con estadísticos de posición, 
#dispersión y gráfico.

# opcion con describeBy (libreria psych)
describeBy(Datos$aceite,list(Datos$densidad,Datos$provincia))

# (adentro de "list" se ponen las variables clasificatorias)

# con tapply
# medias
tapply(Datos$aceite,list(Datos$densidad,Datos$provincia),mean)

#desvío estándar
round(tapply(Datos$aceite,list(Datos$densidad,Datos$provincia),sd),2) #para qué sirve la función "round"?


# con CreateTableOne (libreria tableone)

tabla <- CreateTableOne(vars= "aceite",
                        strata = c("densidad", "provincia"),
                        data=Datos,
                        test = F)
tabla



# Graficos de perfiles (fundamental mirarlos para evaluar cualitativamente 
# si puede existir interacción entre las var predictoras)
# Como deberían verse los perfiles si no hubiera interacción?

# tabla de medias de densidad*provincia
(medias.Datos<-aggregate(aceite~densidad+provincia, Datos,mean))

# Opcion grafica 1
gp <- ggplot(medias.Datos, aes(x=densidad, y=aceite, colour=provincia, group=provincia))
gp + geom_line(aes(linetype=provincia), size=.6) +geom_point(aes(shape=provincia), size=3) 

# Opcion grafica 2
gp1 <- ggplot(medias.Datos, aes(x=provincia, y=aceite, colour=densidad, group=densidad))
gp1 + geom_line(aes(linetype=densidad), size=.6) +geom_point(aes(shape=densidad), size=3) 

# Otra opción gráfica 
interaction.plot(Datos$provincia, Datos$densidad, Datos$aceite, col=c(4,2))


################################
# Modelo e implementacion en R #
#------------------------------#
################################

# Escribimos el modelo con sus parámetros (¿cuál es el modelo?)

# Variable respuesta:
# Variables/s explicatorias y tipo:

# ¿Las hipotesis que se quieren poner a prueba con que parametros están vinculadas?)



# Sintaxis del modelo con interaccion en R
# Dos opciones:

modelo1 <- lm(aceite~densidad+provincia+densidad:provincia, data=Datos)

modelo2 <- lm(aceite~densidad*provincia, data=Datos)


# Notar: con el signo "*" se incluyen los fatores individuales y la interacción

# Compruebe que ambas formas incluyeron el término de interaccion: 

anova(modelo1)
anova(modelo2)

# (vamos a elegir escribirlo como "modelo2")

# Antes de interpretar la salida.... supuestos! 


#############
# Supuestos #
#-----------#
#############


#Calculamos los residuos y los valores predichos por el modelo

e<-resid(modelo2) # residuos
re<-rstandard(modelo2) #residuos estandarizados
pre<-predict(modelo2) #predichos
res<-cbind(Datos$provincia, Datos$densidad, Datos$aceite,pre,e,round(re,3)) # cbind: "Combine R Objects by Columns"
colnames(res)<-c("provincia", "densidad", "aceite", "Predichos", "Residuos", "residuos std") # agregamos los "Column Names"
res

# View(res)

#Supuestos

# Graficos diagnosticos
par(mfrow = c(1, 2))
# Residuos est vs valores predichos
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="Gráfico de dispersión de RE vs PRED" )
abline(0,0)

# qqplot 
qqPlot(e, main = "QQ Plot residuos")

# Pruebas estadisticas

# prueba de shapiro
shapiro.test(e)
# Levene
leveneTest(aceite~provincia*densidad, Datos, center=mean)


########################################
# Modelo: Resultados e interpretacion  #
#------------------------------------  #
########################################

# Volvamos al modelo
modelo2 <- lm(aceite~densidad*provincia, data=Datos)  

# test "global" para 1) interaccion
anova(modelo2)

# que puede decir de la interaccion?

# pueden mirar el summary (pero no sacamos info de p-valores)
summary(modelo2)

# analice los coeficientes estimados ('estimate') del summary
# porque obtiene una intercept + 5 coeficientes? 
# que combinacion de provincia y densidad representa el valor medio estimado de aceite 
#   en la intercept?


##################
# Comparaciones  #
# -------------- #
##################

# Cuando tenemos interacciones involucradas podemos estar en alguno de los siguientes casos:
# 1) variables explicatorias están involucradas en una interaccion SIG
# 2) variables explicatorias no están involucradas en una interaccion SIG   

# Si ocurre 1) ->
#  Comparaciones de interaccion (todas las medias contra todas) 
#  o Efectos Simples (Fijar un nivel de uno de los factores y comparar entre los niveles del otro)

# Si ocurre 2) ->
#  Comparaciones de efectos principales


# En función del resultado del modelo [ver anova(modelo2)] , 
# ¿Que tipo de comparaciones realizaria?



### Estamos en el caso 1) 
##    Dos posibilidad de enfoques analiticos

#       1.1   Comparaciones de interaccion (todas las medias contra todas) 

options(emmeans= list(emmeans = list(infer = c(TRUE, TRUE)),
                      contrast = list(infer = c(TRUE, TRUE)))) # opciones de seteo de las opciones de salida


comp <- emmeans(modelo2, pairwise ~ densidad*provincia) # Tukey por default
comp


plot(comp$emmeans, comparisons = TRUE)


#      1.2    Efectos simples
#             Dentro de cada nivel de uno de los factores comparo los niveles del otro

#       (*) Efecto simple A. Comparar los niveles de densidad dentro de cada nivel provincia

efsimple_densidad <- emmeans(modelo2, pairwise ~ densidad | provincia)
efsimple_densidad

plot(efsimple_densidad$emmeans, comparisons = TRUE)

#       (*) Efecto simple B. Comparar entre provincias dentro de cada nivel de densidad

efsimple_provincia <- emmeans(modelo2, pairwise ~ provincia | densidad )
efsimple_provincia

plot(efsimple_provincia$emmeans, comparisons = TRUE)


#       NOTA: solo realizar efectos simples en un sentido (opcion A o B, no ambas)
#              sino no se controla el error global


###################
# Grafico final y #
# conclusiones    #
# --------------  #
###################

# Para concluir elegimos quedarnos con los resultados de Efectos simples, opcion A
#  comparacion de los niveles de densidad dentro de cada nivel provincia


# Grafico (una de muchas opciones gráficas!)
# extraemos las medidas resumen 

resumen_modelo <-as.data.frame(comp$emmeans)

# exploramos el objeto resumen_modelo
resumen_modelo  # emmeans es la media estimada

# Plot
ggplot(resumen_modelo, aes(x=provincia, y=emmean, fill=densidad, coulor=densidad)) + 
  xlab("Procedencia del genotipo") +  ylab("concentración de aceite (ml de aceite/100gr de MS)") +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=.2, position=position_dodge(.9))+
  annotate("text", x = c("ba","mza","sl"), y=2, label = c("A               B", "A                B", "A               A"))

# Escriba un epigrafe para ese grafico, señalando que indican las letras distintas


### FIN ###
###########
