# Este es el script asociado al  "Caracterización de aguas residuales" 
# del TP # 4  ##


# Antes que nada veamos si hay algun comando u objeto almacenado en la memoria
ls()
# Si te aparece character(0) es que la memoria esta limpia, si no ejecuta
# la siguiente sentencia para borrar la memoria.
rm(list=ls())
# revisamos...
ls()
# No deberia haber nada en el entorno de trabajo ahora!


# setee el directorio de trabajo
# setwd("~/Biome_2_2021/TP_4")

# cargue el data.frame
Aguas <- read.delim("aguas.txt")

# librerias necesarias:
library(ggplot2)
library(emmeans)


###########
# Parte A #
# Items 1, 2 y 3 #

###########

# Aguas
head(Aguas)
str(Aguas)
names(Aguas)

View(Aguas)
summary(Aguas)

# opcional:
# attach(Aguas)

#Gráfico de dispersión
DBO_vs_pH <-ggplot(Aguas, aes(x =pH , y = Y)) +  geom_point(aes(), colour ="deepskyblue", size=4)
DBO_vs_pH <-DBO_vs_pH+ xlab("pH") +  ylab("DBO") +  ggtitle("DBO vs pH") + DBO_vs_pH

DBO_vs_pHyFue <-ggplot(Aguas, aes(x =pH , y = Y, colour =fuente)) + geom_point(size=2)
DBO_vs_pHyFue <- DBO_vs_pHyFue + xlab("pH") +  ylab("DBO") +  ggtitle("DBO vs pH y fuente")
DBO_vs_pHyFue 


# boxplot
box <- ggplot(Aguas, aes(x=fuente, y=pH))+        
  geom_boxplot()+ 
  stat_summary(fun=mean, geom="point", shape=19, size=4,color="black")+
theme_bw()
box # sencillo

box <- box + geom_jitter(alpha=0.3, size=2,aes(color=fuente), position = position_jitter(width = .2))+theme(legend.position="top", legend.text=element_text(size = 14),legend.title = element_text(size=16, face="bold"))
box # con algunos agregados


#Modelo completo

modelo1<-lm(Y ~ pH*fuente, Aguas)
anova(modelo1)
summary(modelo1)

# ojo, primero:

#Supuestos
e<-resid(modelo1) # residuos
re<-rstandard(modelo1) #residuos estandarizados
pre<-predict(modelo1) #predichos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="RE vs PRED - Modelo 1" )
abline(0,0)
library(car)
qqPlot(e, main = "QQplot -Modelo 1")
shapiro.test(e)
# que puede decir acerca de la validez del modelo?


anova(modelo1)
summary(modelo1)
# existe relacion lineal entre DBO y pH? 
# difiere entre las fuentes?

#Ecuaciones estimadas para el modelo completo
p1 <- ggplot(Aguas, aes(x =pH , y = Y, colour =fuente)) + geom_point(size=2)
q1 <- p1 + xlab("pH") +  ylab("DBO") +  ggtitle("DBO en función del pH y la fuente")
r1 <- q1 + geom_smooth(method = "lm", se = FALSE)
r1


# Es un mejor sin el término de interaccion?
#Modelo 3: sin interaccion
modelo2<-lm(Y ~ pH+fuente, Aguas)

#Supuestos
e <- resid(modelo2) # residuos
re<-rstandard(modelo2) #residuos estandarizados
pre<-predict(modelo2) #predichos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="RE vs PRED - Modelo 2" )
abline(0,0)
qqPlot(e, main = "QQplot -Modelo 2")
shapiro.test(e)

anova(modelo2)
summary(modelo2)


# plot modelo 2
p<-ggplot(Aguas, aes(x =pH , y = Y, colour =fuente)) + geom_point(size=4)
q<-p+ xlab("pH") +  ylab("DBO") +  ggtitle("DBO en función del pH, para cada fuente")
r <- q + geom_abline(intercept = -71.899, slope = 54.294) + geom_abline(intercept =-134.731 , slope = 54.294) +
  geom_abline(intercept =-161.897 , slope = 54.294)
r



# Modelo que no considera las fuentes
modelo3<-lm(Y ~ pH, Aguas)

#Supuestos
e<-resid(modelo3) # residuos
re<-rstandard(modelo3) #residuos estandarizados
pre<-predict(modelo3) #predichos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="RE vs PRED - Modelo 3" )
abline(0,0)
qqPlot(e, main = "QQplot -Modelo 3")
shapiro.test(e)

anova(modelo3)
summary(modelo3)

# que puede decir acerca de la validez del modelo?
# y de su poder explicativo?

# AIC todos los modelos
AIC(modelo1, modelo2, modelo3)

# Modelo seleccionado
summary(modelo1)
# escriba a partir del summary las ecuaciones estimadas para las 3 fuentes

# interpretacion
confint(modelo1)


##############
# Parte A - 2 #
# Comparaciones

# Comparacion de pendientes
comp_pendientes <- emtrends(modelo1, ~ fuente, var="pH") #, contr="cld"
pairs(comp_pendientes)
plot(comp_pendientes, comparisons = TRUE)


options(emmeans= list(emmeans = list(infer = c(TRUE, TRUE)),contrast = list(infer = c(TRUE, TRUE))))

Fuentes_en_PH_promedio <- emmeans(modelo1, pairwise ~ fuente|pH)
Fuentes_en_PH_promedio #dif entre fuentes para pH promedio
plot(Fuentes_en_PH_promedio$emmeans, comparisons = TRUE)

## Si quiero comparar en el min y max de PH: 
Fuentes_en_PH_PH_min_max<-emmeans(modelo1, pairwise~ fuente:pH, cov.reduce = range)
Fuentes_en_PH_PH_min_max

# Para chequear que este haciendo el min y el max del PH: 
summary(Aguas$pH)


###########
# Parte B #
# Items 4 y 5 #
###########

#Centrando en el valor de pH promedio
mean(Aguas$pH) # 
Aguas$pH_c <- Aguas$pH - mean(Aguas$pH)

modelo4<-lm(Y ~ pH_c*fuente, Aguas)

#Supuestos
e<-resid(modelo4) # residuos
re<-rstandard(modelo4) #residuos estandarizados
pre<-predict(modelo4) #predichos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="RE vs PRED - Modelo 4" )
abline(0,0)
qqPlot(e, main="QQplot - Modelo 4")
shapiro.test(e)

summary(modelo4)
AIC(modelo4)
round(confint(modelo4),2)

p<-ggplot(Aguas, aes(x =pH_c , y = Y, colour =fuente)) + geom_point(size=4)
q<-p+ xlab("pH") +  ylab("DBO") +  ggtitle("DBO en fc del pH y la fuente")
q<-q + geom_smooth(method = "lm", se = FALSE)
q

# comparaciones
Fuentes_en_PH_PH_c <- emmeans(modelo4, pairwise ~ fuente|pH_c)
Fuentes_en_PH_PH_c
confint(Fuentes_en_PH_PH_c)
plot(Fuentes_en_PH_PH_c$emmeans, comparisons = TRUE)

# Comparacion de pendientes
comp_pendientes <- emtrends(modelo4, ~ fuente, var="pH_c")
pairs(comp_pendientes)
plot(comp_pendientes, comparisons = TRUE)

# ¿Que se modificó (y que no) al centrar PH?

######  Validación modelo 1 ###################

# Predichos vs observados
p<-ggplot(Aguas, aes(x =Y , y = predict(modelo1), colour =fuente)) + geom_point(size=4)
p + geom_abline(intercept = 0, slope =1) +  ggtitle("Predichos vs observados")
cor <- cor(predict(modelo1), Aguas$Y)
cor
cor^2


#predicciones
#Banda de confianza
p<-ggplot(Aguas, aes(x =pH , y = Y, colour =fuente)) + geom_point(size=2)
q<-p+ xlab("pH") +  ylab("DBO") +  ggtitle("DBO vs pH y fuente")
q + geom_smooth(method = "lm", se = TRUE)


####  prediccion de Y para nuevos valores de x  ######
nuevo = data.frame(fuente= "A", pH=7)
nuevo # exploren que es el objeto "nuevo"
predict(modelo1, nuevo, interval="predict") 
#observar que el intervalo de prediccion para una observacion individual es mas amplio que la banda de confianza del modelo para la media poblacional



###########################################################
#### BONUS EXTRA
# MAs sobre comparacion de modelos


# CMe
CMe <- round(c(summary(modelo1)$sigma^2,summary(modelo2)$sigma^2,summary(modelo3)$sigma^2,summary(modelo4)$sigma^2),2)

# R2 (no para comparar entre modelos)
R2 <- c(summary(modelo1)$r.squared, summary(modelo2)$r.squared, summary(modelo3)$r.squared, summary(modelo4)$r.squared)

# R2 ajustado
R2aj <- c(summary(modelo1)$adj.r.squared, summary(modelo2)$adj.r.squared, summary(modelo3)$adj.r.squared, summary(modelo4)$adj.r.squared)

#AIC
AIC <- c(AIC(modelo1), AIC(modelo2), AIC(modelo3), AIC(modelo4))

# Nombre modelo (para generar un data frame bonito)
modelo <- c(1,2,3,4)

comp <- cbind(modelo, CMe, round(R2,2), round(R2aj,2), AIC)

colnames(comp)<-c("modelo", "CMe", "R2", "R2 ajust", "AIC")

comp

#### BONUS EXTRA
### MAs BONUS 

#validación cruzada
library(caret)
set.seed(123) #para hacer reproducibles los resultados
# Indicamos la función para el entrenamiento 
train.control<-trainControl(method = "LOOCV") 
# Entrenamos (estimamos) el modelo  (n modelos con n-1 observaciones) 

m1loo <- train(Y ~ pH * fuente, data = Aguas, method ="lm",trControl= train.control)
m2loo <- train(Y ~ pH, data = Aguas, method ="lm",trControl= train.control)
m3loo <- train(Y ~ pH + fuente, data = Aguas, method ="lm",trControl= train.control)

# resultados
print(m1loo)
print(m2loo)
print(m3loo)

#dataframe con los resultados
b<-m1loo$results
c<-m2loo$results
d<-m3loo$results
e<-rbind(b,c,d)
e<-e[,2:4] 
#agrego error relativo
e$ER=e$RMSE/mean(Aguas$Y)*100  
e

### FIN ###

