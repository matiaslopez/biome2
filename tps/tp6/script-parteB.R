#Problema 2. Atropellamiento de anfibios en una carretera en las cercanias de un parque natural (Modificado a partir de base de datos de Zuur 2009)
datos <- read.delim("roadkills.txt")
names(datos)

#Realice un analisis exploratorio de las variables involucradas. ¿Detecta datos atipicos?
#¿Como son las relaciones entre las variables explicatorias? ¿Y entre las explicatorias y la respuesta?

datos <- subset(datos[,c(5,7,9,11,12,14,15,17,19,20)])
names(datos)
datos$ln_TOT.N <- log(datos$TOT.N)
library(GGally)
ggpairs(datos,mapping = ggplot2::aes(),lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1),
                    discrete = "blank", combo="blank"),
       diag = list(discrete="barDiag",
                   continuous = wrap("densityDiag", alpha=0.5 )),
       upper = list(combo = wrap("box_no_facet", alpha=0.5),
                    continuous = wrap("cor", size=4, alignPercent=0.8))) +
       theme(panel.grid.major = element_blank())


#Se decide aplicar raiz cuadrada sobre las variables POLIC, SHRUB, WAT.RES, L.P.ROAD,
# D.WAT.COUR. ¿Por que cree que se realizo este procedimiento?
datos$SQ.POLIC <- sqrt(datos$POLIC)
datos$SQ.SHRUB <- sqrt(datos$SHRUB)
datos$SQ.WAT.RES <- sqrt(datos$WAT.RES)
datos$SQ.L.P.ROAD <- sqrt(datos$L.P.ROAD)
datos$SQ.D.WAT.COUR <- sqrt(datos$D.WAT.COUR)


#Se aplica la raiz cuadrada debido a los altos valores que toman las variables.

# Este es el modelo que se propone
m1bis <- glm(TOT.N ~ OPEN.L + MONT.S + POLIC +D.PARK + SHRUB + WAT.RES +
            L.WAT.C + L.P.ROAD + D.WAT.COUR, family = poisson,data = datos)

m1 <- glm(TOT.N ~ OPEN.L + MONT.S + SQ.POLIC +D.PARK + SQ.SHRUB + SQ.WAT.RES +
 L.WAT.C +SQ.L.P.ROAD + SQ.D.WAT.COUR, family = poisson,data = datos)

library(car)
vif(m1)

#Calcule la sobredispersion del modelo aditivo completo.
(dispersion<-sum(resid(m1, type="pearson")^2/m1$df.residual))

#¿Es correcto modelar suponiendo distribucion de Poisson? ¿Por que?
#¿Como deberia modelar la variable respuesta segun la sobredispersion encontrada?

######################## Time out para un ejemplo #############################

#Se sabe que la cantidad de hojas de una plantula a los 30 dias post-germinacion tiene una media de 8.56 hojas. Se extrae una muestra y se obtienen los siguientes datos:

datos1=c(rep(1,1),rep(3,3),rep(4,5),rep(5,3),rep(6,7),rep(7,4),rep(8,3),rep(9,5),rep(10,3),rep(11,4),rep(12,4),rep(13,2),rep(15,1),rep(16,4),rep(17,1))

#?Que media y que varianza esperaria obtener? ?Es razonable suponer una distribucion *Poisson*?

datos1 = data.frame(1:25, c(1, 0, 2, 5, 3, 7, 4, 3, 5, 3, 4, 4, 2,
0, 1, 4, 1, 0, 0, 0, 0, 0, 0, 0, 0))
names(datos1) = c("num", "frel")
datos1$frel=datos1$frel/sum(datos1$frel)
p <- ggplot(data = datos1,fill="skyblue", aes(x = num, y = frel)) + geom_bar(stat = "identity",width=0.1)
p + stat_function(fun = dpois, args = list(lambda = 8.5), geom = "col",
fill = "blue", n = 25, width = 0.1,alpha=0.2, position = "dodge") + xlab("Valores") + ylab("Frecuencia relativa") +
theme_bw()

#Ahora vamos a caracterizar a la distribucion *Binomial Negativa* para entender como influyen los diferentes parametros sobre ella.

#vamos a simular 1000 datos para cada mu para cada size para 3 size diferentes
n=1000
mu=1:10
size=c(50,10,2)
vars=c()
var=c()
for (i in 1:length(mu)){
  for (j in 1:length(size)){
  ranBinNeg = rnbinom(n = n, size=size[j], mu = mu[i])
  vars=append(vars,var(ranBinNeg))
  }
  var=rbind(var,vars)
  vars=c()
}

#armamos un data frame con los resultados
nbinom = data.frame(rbind(cbind(mu,var[,1]),cbind(mu,var[,2]), cbind(mu,var[,3])))
#cambiamos nombre a columnas
colnames(nbinom)=c("esp","var")
#hacemos una columna nueva como identificador de grupo para el ggplot
nbinom$size=as.factor(c(rep(size[1],length(mu)),rep(size[2],length(mu)),rep(size[3],length(mu))))

#Si graficamos una en funcion de la otra:

nBinomEspVar <- ggplot(nbinom, aes(x=esp,y=var,colour=size,group=size)) + geom_point(size = 2) + geom_line() + labs(x = "Esperanza",
y = "Varianza", colour = "Size") + ggtitle("Relacion Esperanza-Varianza para Binomial Negativa") + theme(plot.title = element_text(hjust = 0.5))
nBinomEspVar

###################################################################


###########Seguimos con el ejercicio ###############################

#Realice el modelo utilizando la distribucion Binomial negativa.
#Aplique un metodo de seleccion de modelos de manera de incorporar variables de a una.
#Utilice AIC para decidir la incusion o no de una variable en el modelo.

library(MASS)
mnulo <- glm.nb(TOT.N ~ 1, data = datos, link = log)

#add1 hace lo contrario que drop1, evalua la incorporacion
#de variables de a una.
add1(mnulo, TOT.N ~ OPEN.L + MONT.S + SQ.POLIC + D.PARK + SQ.SHRUB + SQ.WAT.RES + L.WAT.C +
       SQ.L.P.ROAD + SQ.D.WAT.COUR, test="Chisq")

m2<-update(mnulo,.~.+D.PARK)

#Repito el procedimiento hasta que no pueda incorporar mas variables.
add1(m2, TOT.N ~ OPEN.L + MONT.S + SQ.POLIC + D.PARK + SQ.SHRUB + SQ.WAT.RES + L.WAT.C +
       SQ.L.P.ROAD + SQ.D.WAT.COUR, test="Chisq")

m3<-update(m2,.~.+OPEN.L)


add1(m3, TOT.N ~ OPEN.L + MONT.S + SQ.POLIC + D.PARK + SQ.SHRUB + SQ.WAT.RES + L.WAT.C +
       SQ.L.P.ROAD + SQ.D.WAT.COUR, test="Chisq")

m4<-update(m3,.~.+L.WAT.C)

add1(m4, TOT.N ~ OPEN.L + MONT.S + SQ.POLIC +
       D.PARK + SQ.SHRUB + SQ.WAT.RES + L.WAT.C +
       SQ.L.P.ROAD + SQ.D.WAT.COUR, test="Chisq")

#Evalue los supuestos del modelo seleccionado.
#Primero vemos si hay sobre o subdispersion
#Para m4
sum(resid(m4, type="pearson")^2)/(m4$df.residual-1)

#Verificamos los supuestos segun Dharma

#Vemos los supuestos
#Simulaciones Dharma
library(DHARMa)
sim <- simulateResiduals(m4, n = 1000)
plot(sim)

#Comparemos. Corremos un modelo como el 4 pero en family Poisson
m5<- glm(TOT.N ~ OPEN.L +D.PARK + L.WAT.C, family = poisson,data = datos)

#Vemos los supuestos
#Simulaciones Dharma
sim <- simulateResiduals(m5, n = 1000)
plot(sim)

#Por lo tanto, haber utilizado una distribucion binomial negativa fue muy util. Seguimos analizando.

#Concluya en relacion a las condiciones que favorecen el atropellamiento de anfibios.

summary(m4)

#intervalos de confianza escala PL
round(confint(m4),5)

# escala VR
round(exp(coefficients(m4)),5)
#Intervalos de confianza
round(exp(confint(m4)),5)


library(ggeffects)
plot(ggpredict(m4), add.data = TRUE)
# cuantos graficos realiza? como se interpretan?


#Para saber mas: Zuur (2009), plantea este caso y realiza diversos analisis con distinto nivel de complejidad, debido a la naturaleza de los datos y las relaciones entre las variables; ademas compara distintas formas de modelar. Es un libro de cabecera, aproveche este ejercicio para conseguirlo, enriquecer el contexto y aprender sobre otras estrategias de análisis.




