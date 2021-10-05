####################################################################
# Problema ** Biomasa de comunidades de Datos granívoros **  # 
####################################################################
rm(list=ls())


# Librerias necesarias
library(pastecs)
library(ggplot2)
library(ggcorrplot)
library(corrplot) 
library(car) # 
library(caret)
library(sjPlot)
library(faraway)
library(ggeffects)
library(lm.beta)
library(MuMIn) 

# luego se indican nuevamente antes de llamar a las funciones

# directorio d etrabajo
setwd("")


#############################
# Inspeccion del data.frmae #
#---------------------------#
#############################

Datos <- read.delim("BiomasaRatones.txt")
names(Datos)
head(Datos)
str(Datos)
View(Datos)
summary(Datos)


#############################
# Analisis exploratorio     #
#---------------------------#
#############################

summary(Datos)

# library(pastecs)
round(stat.desc(Datos[2:5]), 2)

# para explorar asociaciones entre variables
plot(Datos)

# Correlation matrix
names(Datos)
corr_datos <- round(cor(Datos), 2)
print(corr_datos)

# library(ggplot2)
# library(ggcorrplot)
# library(corrplot) 


# Asociaciones entre variables
# Diferentes opciones graficas

ggcorrplot(corr_datos,  
           type = "lower", 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           outline.color="black",
           ggtheme=theme_bw)

ggcorrplot(corr_datos,  
           type = "lower", 
           method="square", 
           outline.color="black",
           ggtheme=theme_bw)

# con otra libreria 
corrplot(corr_datos, 
         type = "upper",
         method = "number")

# mixto
corrplot.mixed(corr_datos, 
               lower.col = "black", 
               number.cex = .7)

# ¿Qué variables se encuentran asociadas?

###############
# Modelo(s)   #
#------------ #
###############

# m1
# modelo aditivo completo 
m1 <- lm(ratones ~ lluvia+predadores+cobertura+semillas, Datos)

# factor de inflacion de la varianza (VIF)
# library(car)
vif(m1)

# ¿Existe colinealidad "importante" entre las variables incluidas en 
# el m1?

# Evaluo los supuestos del modelo, así posibles datos atípicos en el m1 que podrían afectar la selección
e<-resid(m1) # residuos
re<-rstandard(m1) #residuos estandarizados
pre<-predict(m1) #predichos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="Gráfico de dispersión de RE vs PRED m1" )
abline(0,0)
qqPlot(e, main = "QQplot -Modelo 1")
shapiro.test(e)

# continuo ...
summary(m1)

# comienza la "Backward selection"

# Existen varios criterios distintos (entre otros):

#  Identificar los terminos NS en el summary. Que variable eliminarian con este criterio?

#  Comparar a traves de cambios en el AIC . 
#   Una forma de ver esto es con la funcion drop1, que compara el modelo con y sin
#    "X" termino. Además esta funcion drop1 devuelve la Suma de cuadrados residual del modelo. Ejemplo:

drop1(m1)

# "none" indica la RSS y el AIC del modelo SIN REMOVER ningun termino
# "lluvia" indica la RSS y el AIC del modelo Si decidimos eliminar lluvia del modelo
# "predadores" indica ....
# "cobertura" indica ....
# " semillas" indica .... 

# En el proximo paso se decide eliminar del modelo la variable predictora predadores
# ¿esta de acuerdo con esta decision? ¿Como es su resultado en el 'summary'? 
#  ¿ y en el drop1? ¿Como se modifica la RSS y el AIC al eliminar la variable predadores del modelo?



#...continuamos

# m2
# modelo aditivo sin predadores como variable predictora

m2 <- lm(ratones ~ lluvia+cobertura+semillas, Datos)

e<-resid(m2) # residuos
re<-rstandard(m2) #residuos estandarizados
pre<-predict(m2) #predichos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="Gráfico de dispersión de RE vs PRED m2")
abline(0,0)
qqPlot(e, main = "QQplot -Modelo 2")
shapiro.test(e)


# comparacion m1 vs m2
# prueba de hipotesis para modelos anidados
# determina la significacion de la reducción en la SC residual

anova(m1,m2) # comparando modelo con y sin la variable predadores

drop1(m1, test="F") # y tambien se puede ver con drop1, agregando el test 


# Analizamos resultados de m2

summary(m2)

drop1(m2, test = "F")


# corresponde elinar alguna nueva variable del m2? Si? NO? Por que?


# m3
# modelo aditivo sin predadores y sin cobertura como variables predictoras

m3 <- lm(ratones ~ lluvia+semillas, Datos)

e<-resid(m3) # residuos
re<-rstandard(m3) #residuos estandarizados
pre<-predict(m3) #predichos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="Gráfico de dispersión de RE vs PRED m3" )
abline(0,0)
qqPlot(e, main = "QQplot -Modelo 3")
shapiro.test(e)


# comparacion # m1 vs m2 (aunque ya la teniamos) y m2 vs m3

anova(m1,m2,m3)


# Analizamos resultados de m3

summary(m3)

drop1(m3, test = "F")


# ¿semillas queda en el modelo o sale?

# m4
# Dejamos semillas y exploramos la posible interaccion entre lluvia y semillas

m4 <- lm(ratones ~ lluvia*semillas, Datos)

e<-resid(m4) # residuos
re<-rstandard(m4) #residuos estandarizados
pre<-predict(m4) #predichos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="Gráfico de dispersión de RE vs PRED m4" )
abline(0,0)
qqPlot(e, main = "QQplot -Modelo 4")
shapiro.test(e)

# comparacion # m4 vs m3

anova(m4,m3)

# considera que el modelo debe incluir la interaccion entre lluvia y semillas?

# (((
# Parentesis
# Analicemos que ocurre con el vif en el m4, que incorpora un termino de interaccion
vif(m4)
# La inclusion de una interacción suele inducir colinealidad, 
#   solo afecta los EE de los términos de menor orden
# Fin de parentesis
# )))


# m5
# ¿Y si excluimos semillas del m3?

m5 <- lm(ratones ~ lluvia, Datos)
e<-resid(m5) # residuos
re<-rstandard(m5) #residuos estandarizados
pre<-predict(m5) #predichos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="Gráfico de dispersión de RE vs PRED - m5" )
abline(0,0)
qqPlot(e, main = "QQplot -Modelo 5")
shapiro.test(e)

# Analizamos resultados de m5

summary(m5)

drop1(m5, test = "F")


# modelos alternativos
# Y si no quisiera incluir en el mismo modelo lluvia y semillas? 
# comparar modelos alternativos para comenzar la seleccion ...

# modelo comenzando con lluvia (+predadores+cobertura)
m_lluvia <- lm(ratones ~ lluvia+predadores+cobertura, Datos)

# modelo comenzando con semillas (+predadores+cobertura)
m_semillas <- lm(ratones ~ semillas+predadores+cobertura, Datos)

# Puedo comparar esos modelos, y continuar la seleccion
AIC(m_lluvia, m_semillas)



###############################
# Comparacion de modelos
###############################

# CMe 
CMe <- round(c(summary(m1)$sigma^2,summary(m2)$sigma^2,summary(m3)$sigma^2,summary(m4)$sigma^2,summary(m5)$sigma^2), 2)

# R2 (no para comparar entre modelos)
R2 <- c(summary(m1)$r.squared, summary(m2)$r.squared, summary(m3)$r.squared, summary(m4)$r.squared, summary(m5)$r.squared)

# R2 ajustado
R2aj <- c(summary(m1)$adj.r.squared, summary(m2)$adj.r.squared, summary(m3)$adj.r.squared, summary(m4)$adj.r.squared, summary(m5)$adj.r.squared)

#AIC
AIC <- c(AIC(m1), AIC(m2), AIC(m3), AIC(m4), AIC(m5))

# Nombre modelo (para generar un data frame bonito)
modelo <- c(1,2,3,4,5)

comp <- cbind(modelo, CMe, round(R2,2), round(R2aj,2), AIC)

colnames(comp)<-c("modelo", "CMe", "R2", "R2 ajust", "AIC")

comp


####################
# Validacion cruzada
####################

# LOO CV
# library(caret)

# comparo mis cuatro modelos candidatos
# agrego un m5, muy simple

# Indicamos la función para el entrenamiento 
train.control<-trainControl(method = "LOOCV") 

# Entrenamos (estimamos) el modelo  (n modelos con n-1 observaciones) 

m1loo <- train(ratones ~ lluvia+predadores+cobertura+semillas, 
               data=Datos, method ="lm",trControl= train.control)
m2loo <- train(ratones ~ lluvia+cobertura+semillas, 
               data=Datos, method ="lm",trControl= train.control)
m3loo <- train(ratones ~ lluvia+semillas, 
               data=Datos, method ="lm",trControl= train.control)
m4loo <- train(ratones ~ lluvia*semillas, 
               data=Datos, method ="lm",trControl= train.control)
m5loo <- train(ratones ~ lluvia, 
               data=Datos, method ="lm",trControl= train.control)

# resultados

print(m1loo)
print(m2loo)
print(m3loo)
print(m4loo)
print(m5loo)

#dataframe con los resultados
b<-m1loo$results
c<-m2loo$results
d<-m3loo$results
e<-m4loo$results
f<-m5loo$results

comparacion_modelos <-rbind(b,c,d,e,f)
comparacion_modelos <- comparacion_modelos[,2:4] 

#agrego error relativo
comparacion_modelos$ER=comparacion_modelos$RMSE/mean(Datos$ratones)*100  
comparacion_modelos



###########################################################
# Modelo(s) seleccionado(s): Resultados e interpretacion  #
#-------------------------------------------------------  #
###########################################################

# Analicemos m3
summary(m3)

# chequeamos VIF
vif(m3)

# Ecuacion estimada del modelo

# 



# concluya, control bottom-up o top-down?



# Interpretacion de b0

#

# Interpretacion de b0 si centramos lluvia y semillas

#

########################
# Grafico/s final/es   #
# -------------------  #
########################


# Efecto parcial de cada variable. Predicciones marginales 
# a qué valor ajusta a la otra variable? a una media ponderada
#library(ggeffects)


## opcion con ggpredict
#  si se explora el objeto "a" se puede ver a que valor de la otra variable se 
#   esta graficando

(a<-ggpredict(m3))

plot(a, add.data = TRUE, grid = TRUE)


## opcion plot model
# efectos parciales a valores especificos
# seleccionando el valor al que mantiene constante la otra variable
# ratones en fc de semilla para 3 valores de semillas
plot_model(m3, type = "pred", terms = c("lluvia", "semillas [3, 6, 9]"))

# ratones en fc de semillas para 3 valores de lluvia
plot_model(m3, type = "pred", terms = c("semillas", "lluvia [1, 5, 9]"))



# estimacion y comparación de la magnitud del efecto

tab_model(m3) # sjPlot

# puede chequear que son los IC
confint(m3)


# install.packages("lm.beta")
#   library(lm.beta)
lm.beta(m3) #coeficientes estandarizados



###########################
# Seleccion automatica
###########################

# library(MuMIn)
# modelo aditivo
dredge(lm(ratones ~ lluvia+predadores+cobertura+semillas, data = Datos, na.action = "na.fail"))

# modelo con interaccion
dredge(lm(ratones ~ lluvia*predadores*semillas, data = Datos, na.action = "na.fail"))

# Modelos candidatos? dónde se hubica nuestro "m3"?
# que podemos concluir en base a todos estos criterios acerca de la capacidad explicativa
# de cada una de las variables?


### FIN ###