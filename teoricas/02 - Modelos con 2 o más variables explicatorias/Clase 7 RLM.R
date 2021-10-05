#Regresion con VE cuantitativas

bd <- read.csv("resp.csv")

## Descriptiva ####
summary(bd)
names(bd)

#Matriz de correlación
corr<-round(cor(bd),2)

plot (bd)
library(ggplot2)
library(GGally)
ggpairs(bd)

library(ggcorrplot)
ggcorrplot(corr, lab = TRUE)



## Modelos ####

m1<-lm(VVM~ edad+altura+peso, data = bd)
summary(m1) #SC marginal
anova(m1)  #SC secuencial (importa el orden)

m1b<-lm(VVM~ edad+peso+altura, data = bd)
summary(m1b)  #no cambia
anova(m1b)  #cambia

#si quiero SC marginal en formato anova:
library(car)
Anova(m1, type="III")
Anova(m1b, type="III")

library(car)
vif(m1)

#Seleccion de modelos
m2<-lm(VVM~ edad+peso, data = bd)
m3<-lm(VVM~ edad+altura, data = bd)
anova(m3)m4<-lm(VVM~ edad, data = bd)
m5<-lm(VVM~ altura, data = bd)
m6<-lm(VVM~ edad*altura, data = bd)

sigma<-c(summary(m1)$sigma, summary(m2)$sigma, summary(m3)$sigma, summary(m4)$sigma, summary(m5)$sigma, summary(m6)$sigma)
  
R2<-c(summary(m1)$r.squared, summary(m2)$r.squared, summary(m3)$r.squared, summary(m4)$r.squared, summary(m5)$r.squared, summary(m6)$r.squared)

R2aj<-c(summary(m1)$adj.r.squared, summary(m2)$adj.r.squared, summary(m3)$adj.r.squared, summary(m4)$adj.r.squared, summary(m5)$adj.r.squared, summary(m6)$r.squared)

AIC<-AIC(m1, m2, m3, m4, m5, m6)

modelo<-c(1,2, 3, 4, 5, 6)
compR<-cbind(sigma, R2,R2aj,AIC)
colnames(compR)<-c( "sigma", "R2", "R2 ajust", "df", "AIC")
round(compR,3)


# LOO CV
library(caret)
train.control<-trainControl(method = "LOOCV") 
m1loo <- train(VVM~ edad+peso+altura, data=bd, 
               method ="lm",trControl= train.control)
m2loo <- train(VVM~ edad+peso, data=bd,  
               method ="lm",trControl= train.control)
m3loo <- train(VVM~ edad+altura, data=bd, 
               method ="lm",trControl= train.control)
m4loo <- train(VVM~ edad, data=bd, 
               method ="lm",trControl= train.control)
m5loo <- train(VVM~ altura, data=bd, 
               method ="lm",trControl= train.control)
m6loo <- train(VVM~ edad*altura, data=bd, 
               method ="lm",trControl= train.control)

#dataframe con los resultados
b<-m1loo$results
c<-m2loo$results
d<-m3loo$results
e<-m4loo$results
f<-m5loo$results
g<-m6loo$results

comparacion_modelos <-rbind(b,c,d,e,f,g)
comparacion_modelos <- comparacion_modelos[,2:4] 
#agrego error relativo
comparacion_modelos$ER=comparacion_modelos$RMSE/mean(bd$VVM)*100  
round(comparacion_modelos,3)

#manual

#comparando modelos por anova (extra SC)
anova(m1,m2)
anova(m1,m5)
anova(m4,m6)


#entre todos
drop1(m1, test="F")

plot(m1)


#Ranking de modelos por AIC, modelos aditivos
library(MuMIn)
dredge(lm(VVM~ edad+peso+altura, data = bd, na.action = "na.fail"))
#Ranking de modelos por AIC, modelos con interaccion
dredge(lm(VVM~ edad*peso*altura, data = bd, na.action = "na.fail"))

# Supuestos m3 ####
e<-resid(m3) # residuos
re<-rstandard(m3) #residuos estandarizados
pre<-predict(m3) #predichos
par(mfrow = c(2, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="Gráfico de dispersión de RE vs PRED" )
abline(0,0)
qqnorm(e)
qqline(e)
plot(m3, which=c(4))
par(mfrow = c(1, 1))
shapiro.test(e)
vif(m3)

#Visualizando resultados
#grafico 3d modelo final
library(car)
library(rgl)
scatter3d(VVM~ edad+altura, fill=FALSE, data=bd)
confint(m3)

library(sjPlot)
# efectos principales:
plot_model(m3, type = "pred", terms = c("altura"))
plot_model(m3, type = "pred", terms = c("edad"))
#ef simples, seleccionado algunos valores de la otra VE 
plot_model(m3, type = "pred", terms = c("altura", "edad[20, 50, 80]"))
plot_model(m3, type = "pred", terms = c("edad", "altura[150, 170, 190]"))

tab_model(m3)

#otra opción
library(ggeffects) 
pred<-ggpredict(m3) 
plot(pred) 

#coef estandarizados
library(boot)
library(MASS)
library(QuantPsyc)
lm.beta(m3)

# Validación ####
plot(predict(m3),bd$VVM, ylab="Observados", xlab="Predichos")
abline(0,1, col="red")

# Predicciones para nuevas observaciones ####
nuevo = data.frame(edad=30, altura =170)
predict(m3, nuevo) 
#con IC
predict(m3, nuevo, interval="prediction", level=0.95)

################################################################## #
##Ej con interaccion
set.seed(234) #para que los resultados sean reproducibles
x1 = rnorm(1000,20,2)
x2 = rnorm(1000,10,2)
beta0 <-5
beta1 <-2
beta2<-3
beta3<-0.5
e = rnorm(1000,mean=0,sd=2)
y= beta0+beta1*x1+beta2*x2+beta3*x1*x2+e
bd1<-cbind.data.frame(y,x1,x2)
plot(bd1)
ggpairs(bd1)

#ajustamos modelo aditivo
m1= lm(y ~ x1+x2, data=bd1)
summary(m1)
par(mfrow = c(1, 2))
plot(m1, which=c(1,2))
par(mfrow = c(1, 1))
library(car)
vif(m1)

#ajustamos modelo multiplicativo
m2= lm(y ~ x1*x2, data=bd1)
summary(m2)
par(mfrow = c(1, 2))
plot(m2, which=c(1,2))
par(mfrow = c(1, 1))
vif(m2)

#centrando?
bd1$x1c=x1-mean(x1)
bd1$x2c=x2-mean(x2)
m3= lm(y ~ x1c*x2c, data=bd1)
vif(m3)

#y si la colinealidad no es por una interacción?
bd1$x3=x1*1.2+rnorm(1000,mean=0,sd=0.5)
plot(bd1$x1, bd1$x3)
cor(bd1$x1, bd1$x3)

m4= lm(y ~ x1+x3, data=bd1)
summary(m4)
vif(m4)

#centramos
bd1$x1c=x1-mean(x1)
bd1$x3c=x3-mean(x3)
m5= lm(y ~ x1c+x3c, data=bd1)
summary(m5)
vif(m5)
ggpairs(bd1, columns=c(y,x1,x3,x1c,x3c))

#####################################################
#Como estudiamos la interaccion?
summary(bd1)
sd(x1)
sd(x2)

#grafico
plot(x1,y)
abline(28.87,6, col="red")
abline(34.4,7.025, col="blue")
abline(40,8.045, col="green")

