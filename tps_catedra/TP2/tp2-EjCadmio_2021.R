# Antes que nada veamos si hay algun comando u objeto almacenado en la memoria
ls()
# Si te aparece character(0) es que la memoria esta limpia, si no ejecut? 
# la siguiente sentencia para borrar la memoria.
rm(list=ls())
# revisamos...
ls()


## Problema 1. Fitorremediacion en plantas

# 1) Indique la cantidad de replicas y el tipo de variables involucradas.  

# cargue el data.frame
cadmio <- read.table("su ruta /Cadmio.txt",header=T)

# alternativamente, previo seteo del directorio de tabajo
setwd("~/Biome2_2021/TP_2_2021/TP 2")
Cadmio <- read.table("Cadmio.txt",header=T)


# inspeccion del data.frame
class(Cadmio)
str(Cadmio)
dim(Cadmio)
head(Cadmio)
tail(Cadmio)
summary(Cadmio)

#cantidad de replicas (hay varias formas)
table(Cadmio$Cd)

#2) Describa grafica y estadisticamente los datos. (*Ayuda: tapply, summary y stat.esc)*

# graficos
#R base
plot(Cadmio, ylab = "cadmio acumulado", xlab = "cadmio agregado")

# ggplot2
library(ggplot2)
disp <- ggplot(Cadmio, aes(x=Cd, y=Cdenplanta)) + 
  geom_point(size=3, color="red", shape=19) +  
  geom_smooth(method=lm, se=F, fullrange=F, size=0.5)+ 
  xlab("Conc. Cd suministrada") +  ylab("Cd tallo (mg/g MS)") 
disp

# resumen descriptiva
# general
summary(Cadmio)

# por nivel de dosis de Cadmio
# ¿Que hace la funcion *tapply*?  
# *tapply* toma como argumentos (X,INDEX,FUN)  
# -La funcion *tapply* aplica (de ahi parte de su nombre) una funcion (FUN) a un vector (X) agrupado para cada factor definido en INDEX. En este caso, queremos que para cada valor de la categoria "Cd", aplique *summary* sobre el vector "Cdenplanta".  

library(pastecs)
tapply(Cadmio$Cdenplanta, Cadmio$Cd, summary) 
tapply(Cadmio$Cdenplanta, Cadmio$Cd, sd)

# mean y sd (con tableone)
library(tableone)

tabla <- CreateTableOne(vars= "Cdenplanta",
                        strata = "Cd",
                        data=Cadmio,
                        test = F)
tabla


#3) Analice como se modifica la concentracion de cadmio absorbida por las plantas en relacion a la concentracion de cadmio ambiental. Plantee el modelo, compruebe los supuestos.

# Considerando Cd como variable cuantitativa (modelo regresión)
# Plantee el modelo en parámetros

# Sintaxis del modelo en R:
Modelo_Cdcuant <- lm(Cdenplanta ~ Cd, data=Cadmio)

# Supuestos

#Calculamos los residuos y los predichos

e<-residuals(Modelo_Cdcuant) # residuos
re<-rstandard(Modelo_Cdcuant) #residuos estandarizados
pre<-predict(Modelo_Cdcuant) #predichos

# generamos un data frame que contenga las variables x, y, los valores predichos por el modelo, los residuos y los res estandarizados
res <- cbind.data.frame(Cadmio$Cd,Cadmio$Cdenplanta,pre,e,round(re,2))
colnames(res) <- c("dosis Cd", "Cd tallo", "Predichos", "Residuos", "residuos std") 
head(res)


#Graficamos residuos en el scatter plot
ggplot(Cadmio, aes(x = Cd, y = Cdenplanta)) +
geom_smooth(method = "lm", se = FALSE, color = "blue") +
geom_segment(aes(xend = Cd, yend = pre)) +
geom_point(aes(), colour ="deepskyblue", size=2) +
geom_point(aes(y = pre), shape = 1) +
xlab("Concentracion Cd (mg/kg suelo)") +  ylab("Cd en tallo (mg/g MS)") +  ggtitle("")

#Supuestos modelo
# Graficos diagnosticos 
par(mfrow = c(1, 3))
plot(Modelo_Cdcuant, which=c(1,2,4))

sort(cooks.distance(Modelo_Cdcuant))

# nos concentramos en dos de ellos por ahora
library(car)
par(mfrow = c(1, 2))

# Grafico de residuos estandarizados vs valores predichos por el modelo
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="Grafico de dispersion de RE vs PRED" )
abline(0,0)

# QQ plot 
qqPlot(e, main = "QQ Plot residuos")

par(mfrow = c(1, 1))

# Prueba de normalidad (Shapiro-Wilk test of normality)
shapiro.test(e)


# Se puede testear la homogeneidad de varianzas?
leveneTest(Modelo_Cdcuant)

leveneTest(Cadmio$Cdenplanta, Cadmio$Cd)
# devuelve un warning: 'Cadmio$Cd coerced to factor' (lo transforma solo a factor)
# por que? 


#4) ¿Considera que se puede proponer a *Thlaspi caerulescens* como un agente fitorremediador? Justifique.
summary(Modelo_Cdcuant)
confint(Modelo_Cdcuant)

# Veamos que el summary es equivalente a la "tabla de Anova"
anova(Modelo_Cdcuant)
  
# 5) Analice como se modifica la concentracion de cadmio absorbida por las plantas 
#    en relacion a la concentracion de cadmio ambiental, pero ahora considerando 
#    al cadmio ambiental como una **variable categorica**. 
#    Plantee el nuevo modelo, compruebe los supuestos e interprete los resultados obtenidos.


# Pasamos la variable explicatoria (continua) a una variable categorica
Cadmio$Cd.f <- as.factor(Cadmio$Cd) #crean el objeto aparte... tambien se puede aplicar la funcion as.factor 
#en el modelo
levels(Cadmio$Cd.f) # devuelve los niveles del factor

# graficos exploratorios

boxplot(Cadmio$Cdenplanta~Cadmio$Cd.f, ylab = "cadmio acumulado", xlab = "cadmio agregado")

# con ggplot2
# haciendo mas informativos los boxplot
library(ggplot2)
box <- ggplot(Cadmio, aes(x=Cd.f, y=Cdenplanta)) +
  geom_boxplot(aes(color=Cd.f), color="black")+
  theme_bw()+
  geom_jitter(alpha=0.3, size=2,aes(color=Cd.f), position = position_jitter(width = .2))+theme(legend.position="top", legend.text=element_text(size = 14),legend.title = element_text(size=16, face="bold")) +
  ylab("Cd en planta")+xlab("Cd")
box

# Modelo en R
Modelo_CdFactor <- lm(Cdenplanta ~ Cd.f, Cadmio)

# Supuestos (calculamos y construimos el data frame)
# residuos 
eFac <- residuals.lm(Modelo_CdFactor)
reFac<-rstandard(Modelo_CdFactor) #residuos estandarizados
preFac<-predict(Modelo_CdFactor) #predichos

resFac<-cbind.data.frame(Cadmio$Cd,Cadmio$Cdenplanta,eFac,reFac,preFac)
colnames(resFac)<-c("Dosis Cd", "Cd tallo", "Residuos", "Residuos Std","Predichos") 
head(resFac)

# Prueba de shapiro
shapiro.test(eFac)

# Supuestos modelo

#Supuestos modelo
par(mfrow = c(1, 2))
plot(preFac, reFac, xlab="Predichos", ylab="Residuos estandarizados",main="Graf de dispersion de RE vs PRED" )
abline(0,0, col="red")
qqPlot(eFac, main = "QQ Plot residuos")
par(mfrow = c(1, 1))

shapiro.test(eFac)


# Homogeneidad de varianza. Prueba de Levene
library(car)
leveneTest(Modelo_CdFactor)
leveneTest(Cadmio$Cdenplanta, Cadmio$Cd.f) 

# Veamos la tabla de Anova 
anova(Modelo_CdFactor)

# Vemos el resumen del modelo
summary(Modelo_CdFactor)

# como analizarian la magnitud del efecto?
library(emmeans)

options(emmeans= list(emmeans = list(infer = c(TRUE, TRUE)),contrast = list(infer = c(TRUE, TRUE))))

# comparaciones
comp <- emmeans(Modelo_CdFactor, pairwise ~ Cd.f)
comp


# comparaciones y plot
comp$contrasts
plot(comp$emmeans, comparisons = TRUE)

# medias 
confint(comp$emmeans)


# Grafico (una de muchas opciones gráficas!)
# extraemos las medidas resumen 

resumen_modelo <-as.data.frame(comp$emmeans)

# exploramos el objeto resumen_modelo
resumen_modelo  # emmeans es la media estimada

# Plot
library(ggplot2)
ggplot(resumen_modelo, aes(x=Cd.f, y=emmean)) +
  labs(x="[Cd]") + labs(y="Cd acumulado en tallos [mg.kg-1]") +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), color="blue", width=0.2)+
  geom_point(shape=1, size=2, color="blue") +
  ggtitle("Comparaciones", "Media ± Error estándar") +
  annotate("text", 
            x = c("5","10","25","50","100","200","300","500"), 
            y = c(450,460,470, 480, 550,600,650,800), 
            label = c("A", "A", "A","A", "B", "C","D", "E"))



#6) Discuta ventajas, desventajas y alcances de cada aproxiacion (predictora cuantitativa o cualitativa).  

## Predicciones

#7) Pronostique por ambos modelos la concentracion de cadmio absorbida por las plantas de *Thlaspi caerulescens* sometidas a 500 $\mu$M de cadmio. ¿Podria predecir la respuesta esperada a 450 $\mu$M? ¿Y si la concentracion de cadmio ambiental supera los 600 $\mu$M?   

#Cd=500 con Cadmio como cuantitativa y Cadmio como factor.


#Modelo Cd cuantitativa
new <-data.frame(Cd=c(500)) # "new" es el objeto que guarda el valor de cd suministrado 
# para el cual quiero predecir el valor de cd en tallo
predict.lm(Modelo_Cdcuant,new) # valor predicho para "new" (500)

# Modelo Cd factor
new <-data.frame(Cd.f=c("500"))
predict.lm(Modelo_CdFactor,new) 


# Cd=450 

#Modelo Cd cuantitativa
new <-data.frame(Cd=c(450))
predict.lm(Modelo_Cdcuant,new)

# Modelo Cd factor
new <-data.frame(Cd.f=c("450")) # Analizar el error
predict.lm(Modelo_CdFactor,new)


# revisen:
#Haciendo la cuenta rapidamente para 450
summary(Modelo_Cdcuant)
(y=420.81+0.66*450)

#Haciendo la cuenta exacta para 450
coefficients(Modelo_Cdcuant)[2]*450+coefficients(Modelo_Cdcuant)[1]


# ¿Se puede hacer para ambos modelos? ¿Por que?


# Y con Cd=600 ?


