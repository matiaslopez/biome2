########################
#Anidado

# de rutina ....
ls()
rm(list=ls())
ls()

# Setear directorio de trabajo
setwd("")

# Abramos el data.frame

Datos <- read.delim("HF.txt")
names(Datos)

#Explorando...
library(ggplot2)
ggplot(Datos, aes(sexo, horas.suenio, color=Fumador)) +  geom_point() + labs(y="Horas de suenio") 

#Para calcular las medias de horas suenio para cada combinacion de niveles de los dos factores
(medias.int<-aggregate(horas.suenio~sexo*Fumador, Datos,mean))

#Planteando el modelo...
#Incorporemos individuo como factor
#?De que tipo es? ?Como se relaciona con los otros factores?

library(lme4)
modelo<- lmer(horas.suenio ~ sexo*Fumador + (1|Individuo), Datos)
summary(modelo)
anova(modelo)

#Supuestos
e<-resid(modelo) # residuos de pearson
pre<-predict(modelo) #predichos
par(mfrow = c(1, 2))
plot(pre, e, xlab="Predichos", ylab="Residuos de pearson",main="Grafico de dispersi?n de RE vs PRED",cex.main=.8 )
abline(0,0)
library(car)
qqPlot(e)
shapiro.test(e)
par(mfrow = c(1, 1))

#?Que supuesto quedaria probar? 
(alfai<-ranef(modelo)$Individuo$'(Intercept)')
shapiro.test(alfai)
qqPlot(alfai)

#Parte fija
fitted(modelo)  #predicciones parte fija + aleatoria
fixef(modelo) #estimacion parametros efectos fijos
X<-model.matrix(modelo)
pred_fija<-X %*% fixef(modelo)#predicciones parte fija 
pred_fija

#Parte aleatoria
alfai<-ranef(modelo)$Individuo$'(Intercept)' #alfa i de cada individuo
alfai

#significacion del modelo (comparacion con modelo nulo)
modelo0<- lmer(horas.suenio ~  (1|Individuo), Datos)
anova(modelo, modelo0)
AIC(modelo0,modelo)

#otra posibilidad (incluye Test Wald para coeficientes)
library(nlme)
modelo_lme <- lme(horas.suenio ~ sexo*Fumador, random= ~1 | Individuo, data = Datos)
summary(modelo_lme)

# significancia del aleatorio
library(lmerTest)
ranova(modelo)

#Resumen
library(sjPlot)
tab_model(modelo)
#Conclusiones?
