#Clase 6 Biome II
# Regresion con variables explicatorias cuantitativas y categóricas

bd <- read.delim2("pulmon.txt")
View(bd)
summary(bd)
library(ggplot2)

#Gráfico de dispersión
q<-ggplot(bd, aes(x =edad , y = vvm)) +  
  geom_point(aes(), colour ="deepskyblue", size=2) + 
  xlab("Edad (años)") +  
  ylab("VVM (litros)") +  
  ggtitle("Variación del VVM en función de la edad")
q

######################################################## #
#modelo 0: nulo ##### 
modelo0<-lm(vvm ~ 1, bd)
summary(modelo0)
round(confint(modelo0),2)
summary(modelo0)$r.squared
q + geom_hline(yintercept = mean(bd$vvm))
AIC(modelo0)

######################################################## #
#Modelo 1: con una predictora. VVM en funcion de la edad ##### 
modelo1<-lm(vvm ~ edad, bd)
summary(modelo1)
anova(modelo1)
round(confint(modelo1),2) 
summary(modelo1)$sigma^2
summary(modelo1)$r.squared
summary(modelo1)$adj.r.squared
AIC(modelo1)

#Agregamos la recta al gráfico
q + geom_smooth(method = "lm", se = FALSE)

#Supuestos
e<-resid(modelo1) # residuos
re<-rstandard(modelo1) #residuos estandarizados
pre<-predict(modelo1) #predichos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="Gráfico de dispersión de RE vs PRED" )
abline(0,0)
qqnorm(e)
qqline(e)
shapiro.test(e)

###################################################### #
#Modelo 2: con 2 VE sin interaccion ##### 
ggplot(bd, aes(x =edad , y = vvm, colour =sexo)) +
  geom_point(size=2) + 
  xlab("Edad (años)") +  
  ylab("VVM (litros)") +  
  ggtitle("Variación del VVM en función de la edad y el sexo") + 
  geom_abline(intercept = 155.85, slope = -0.91, colour="red") +
  geom_abline(intercept = 188.06, slope = -0.91, colour="skyblue")


modelo2<-lm(vvm ~ edad+sexo, bd)
summary(modelo2)
summary(modelo2)$r.squared
summary(modelo2)$adj.r.square
AIC(modelo2)
round(confint(modelo2),2)

#Supuestos
e<-resid(modelo2) # residuos
re<-rstandard(modelo2) #residuos estandarizados
pre<-predict(modelo2) #predichos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="Gráfico de dispersión de RE vs PRED" )
abline(0,0)
qqnorm(e)
qqline(e)
shapiro.test(e)

###################################################### #
#Modelo 3: con 2 VE con interaccion ####
ggplot(bd, aes(x =edad , y = vvm, colour =sexo)) +
  geom_point(size=2) + 
  xlab("Edad (años)") +  
  ylab("VVM (litros)") +  
  ggtitle("Variación del VVM en función de la edad y el sexo") + 
  geom_smooth(method = "lm", se = FALSE)

 
modelo3<-lm(vvm ~ edad*sexo, bd)
summary(modelo3)
summary(modelo3)$r.squared
AIC(modelo3)
summary(modelo3)$adj.r.squared
round(confint(modelo3),2)

#Supuestos
e<-resid(modelo3) # residuos
re<-rstandard(modelo3) #residuos estandarizados
pre<-predict(modelo3) #predichos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="Gráfico de dispersión de RE vs PRED" )
abline(0,0)
qqnorm(e)
qqline(e)
shapiro.test(e)

###################################################### #
#Modelo 4: idem 3 pero con centrado de X ####
bd$edad_c<-bd$edad-20
ggplot(bd, aes(x =edad_c , y = vvm, colour =sexo)) +
  geom_point(size=2) + 
  xlab("edad - 20 (años)") +  
  ylab("VVM (litros)") +  
  ggtitle("Variación del VVM en función de la edad y el sexo") + 
  geom_smooth(method = "lm", se = FALSE)


modelo4<-lm(vvm ~ edad_c*sexo, bd)
summary(modelo4)
summary(modelo4)$r.squared
AIC(modelo4)
summary(modelo4)$adj.r.squared
round(confint(modelo4),2)

#Supuestos
e<-resid(modelo4) # residuos
re<-rstandard(modelo4) #residuos estandarizados
pre<-predict(modelo4) #predichos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="Gráfico de dispersión de RE vs PRED" )
abline(0,0)
qqnorm(e)
qqline(e)
shapiro.test(e)

##############################
#Comparacion de modelos
CMe<-round(c(summary(modelo0)$sigma^2,summary(modelo1)$sigma^2,summary(modelo2)$sigma^2,summary(modelo3)$sigma^2, summary(modelo4)$sigma^2),3)
comp<-cbind(modelo, CMe, round(R2,3), round(R2aj,3))
colnames(comp)<-c("modelo", "CMe", "R2", "R2 ajust")
comp

R2<-c(summary(modelo0)$r.squared, summary(modelo1)$r.squared, summary(modelo2)$r.squared, summary(modelo3)$r.squared, summary(modelo4)$r.squared)
R2aj<-c(summary(modelo0)$adj.r.squared, summary(modelo1)$adj.r.squared, summary(modelo2)$adj.r.squared, summary(modelo3)$adj.r.squared, summary(modelo4)$adj.r.squared)
modelo<-c(0, 1,2,3,4)
compR<-cbind(modelo, round(R2,3), round(R2aj,3))
colnames(compR)<-c("modelo", "R2", "R2 ajust")
compR

AIC(modelo0, modelo1, modelo2, modelo3, modelo4)
anova(modelo2,modelo3)
drop1(modelo3, test="F")

#validación cruzada
library(caret)
set.seed(123) #para hacer reproducibles los resultados
# Indicamos la función para el entrenamiento 
train.control<-trainControl(method = "LOOCV") 
# Entrenamos (estimamos) el modelo  (n modelos con n-1 observaciones) 

m1loo <- train(vvm~ edad, data=bd, method ="lm",trControl= train.control)
m2loo <- train(vvm~ edad+sexo, data=bd, method ="lm",trControl= train.control)
m3loo <- train(vvm~ edad*sexo, data=bd, method ="lm",trControl= train.control)
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
e$ER=e$RMSE/mean(bd$vvm)*100  
e

##############
#Validación
p<-ggplot(bd, aes(x =vvm , y = predict(modelo3), colour =sexo)) + geom_point(size=2)
p + geom_abline(intercept = 0, slope =1) +  ggtitle("Predichos vs observados Modelo 3")
cor(predict(modelo3),bd$vvm)

#predicciones
#Banda de confianza
p<-ggplot(bd, aes(x =edad , y = vvm, colour =sexo)) + geom_point(size=2)
q<-p+ xlab("Edad (años)") +  ylab("VVM (litros)") +  ggtitle("Variación del VVM en función de la edad y el sexo")
q + geom_smooth(method = "lm", se = TRUE)
q

#prediccion de Y para nuevos valores de x
nuevo = data.frame(sexo= "varón", edad=50)
predict(modelo3, nuevo, interval="predict") 



