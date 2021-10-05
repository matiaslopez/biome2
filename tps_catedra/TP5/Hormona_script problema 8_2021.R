# de rutina ....
ls()
rm(list=ls())
ls()

# Setear directorio de trabajo
setwd("")

# Abramos el data.frame problema "Hormonas" 
Datos  <- read.delim("Hormona.txt")

# Miremos la tabla
View(Datos)

#¿Cómo está tomando al factor camada?
#Pasamos el bloque ("camada") a variable cualitativa
Datos$Camada <- factor(Datos$Camada)

# ordeno los niveles (Testigo-Baja-Alta)¿para que lo hacemos?
levels(Datos$Hormona)
Datos$Hormona<-factor(Datos$Hormona,levels=c("Testigo","Baja", "Alta"))
levels(Datos$Hormona)

library(ggplot2)
p <- ggplot(Datos, aes(Hormona, GanPeso, color=Camada, group=Camada))
p + geom_point() +labs(x="Hormona", y="Ganancia en peso (dgr)")+ geom_line()


# Modelo considerando Camadas como de efectos aleatorios
library(lme4)

m2 <- lmer(GanPeso ~ Hormona + (1 | Camada), data = Datos)
summary(m2)

#Supuestos
e<-resid(m2) # residuos de pearson
pre<-predict(m2) #predichos
be_k<-ranef(m2)$Camada$'(Intercept)'
par(mfrow = c(1, 3))
plot(pre, e, xlab="Predichos", ylab="Residuos de pearson",main="Gráfico de dispersión de RE vs PRED",cex.main=.8 )
abline(0,0)
library(car)
qqPlot(e)
qqPlot(be_k)
par(mfrow = c(1, 1))
plot(m2) #RP vs PRED
shapiro.test(e)
shapiro.test(be_k)

library(sjPlot)
plot_model(m2, type="diag")[[2]]


#### como analizar la significancia de la parte fija?

#1) significacion, parte fija #####
m0<- lmer(GanPeso ~  (1|Camada), data=Datos)
anova(m0,m2)
AIC(m0,m2)

#2) significacion coeficientes, 
library(lmerTest) # sobreescribe la funcion lmer de lme4 y estima df a traves de un metodo de aproximacion 
m3 <- lmer(GanPeso ~ Hormona + (1 | Camada), data = Datos)
summary(m3)

#También se puede usar la library nlme
library("nlme")
m2b<-lme(GanPeso~Hormona, random=~1|Camada,data=Datos)
summary(m2b)
intervals(m2b) # para intervalos de confianza de los estimadores


###### ¿Como sabemos si los tratamientos fueron diferentes entre si? 
### Comparaciones a posteriori #####
library (emmeans)
comp <- emmeans(m2, pairwise ~ Hormona)
summary(comp)
confint(comp)
plot(comp$emmeans, comparisons = TRUE)

### Parte Aleatoria #####

# predicciones de Bk
ranef(m2) #B k (BLUP) de cada camada, graficado
library(sjPlot)
plot_model(m2, type = "re")

library(lmerTest)
ranova(m2)

## Grafico de componentes de varianza
# Simple Pie Chart
slices <- c(1.53, 0.15)
lbls <- c("Camadas", "Residual")
pie(slices, labels = lbls, main="Variance components")


#### Predicciones sujeto-especificas: parte fija + aleatoria ######

coef(m2) #muestra los coef para cada nivel aleatorio (camada)

fitted(m2)  #predicciones parte fija + aleatoria
fixef(m2) #estimacion parametros efectos fijos
X<-model.matrix(m2)
pred_fija<-X %*% fixef(m2)#predicciones parte fija 
be_k<-rep(round(ranef(m2)$Camada$'(Intercept)', 4),each=3)
pred<-cbind(Datos$Hormona,Datos$Camada, Datos$GanPeso,pred_fija, be_k,round(fitted(m2),4))
colnames(pred)<-c("Hormona", "camada","GanPes", "pred fija", "efecto aleat", "pred fija + aleat")
pred

#Resumen
tab_model(m2)

#Porqué no se consideró a camada como de efectos fijos?

