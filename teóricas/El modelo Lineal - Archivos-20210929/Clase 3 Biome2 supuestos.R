#Clase 3 Biome II - Supuestos de modelos lineales

library(ggplot2) 
library(MASS)

#Ensayo de cadmio
bd <- read.csv2("cadmio.csv")
View(bd)

## Descriptiva ####
summary(bd)

#Gráfico de dispersión
p<-ggplot(bd, aes(x =dosis_cd , y = cd_tallo)) +  geom_point(aes(), colour ="deepskyblue", size=2) + xlab("Dosis Cd (mg/kg suelo") +  ylab("Cd tallo (mg/g MS)") +  ggtitle("Absorción de Cd por tallo y hojas de E.ophiuroides") + theme(text=element_text(size=14))
p

## Modelo de regresión ####
modelo1<-lm(cd_tallo ~ dosis_cd, data=bd)
summary(modelo1)
confint(modelo1)

#Agregamos la recta al gráfico
p + geom_smooth(method = "lm", se = FALSE)

## Supuestos ####
#Calculamos los residuos y los predichos
names(modelo1)
e<-residuals(modelo1) # residuos
re<-rstandard(modelo1) #residuos estandarizados
pre<-predict(modelo1) #predichos

#creamos un dataframe (con fines didácticos)
res<-cbind(bd$dosis_cd,bd$cd_tallo,pre,e,round(re,2))
colnames(res)<-c("dosis Cd", "Cd tallo", "Predichos", "Residuos", "residuos std") 
res

#Grafico de dispersión con los residuos
ggplot(bd, aes(x = dosis_cd, y = cd_tallo)) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # recta de regresion slope
  geom_segment(aes(xend = dosis_cd, yend = pre)) + #residuos
  geom_point(aes(), colour ="deepskyblue", size=2) +
  geom_point(aes(y = pre), shape = 1) + # Agrega los predichos
  xlab("Dosis Cd (mg/kg suelo") +  ylab("Cd tallo (mg/g MS)") +  ggtitle("Absorción de Cd por tallo y hojas de E.ophiuroides")

#gráficos de residuos a mano
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="Gráfico de dispersión de RE vs PRED" )
abline(0,0)
qqnorm(e)
qqline(e)

#gráficos de residuos usando la función plot(modelo)
windows()
par(mfrow = c(1, 3))
plot(modelo1, which=c(1,2,4))
par(mfrow = c(1, 1))

#Prueba de Shapiro para normalidad
shapiro.test(e)

#Prueba de Levene para homocedasticidad
library(car)
leveneTest(bd$cd_tallo,bd$dosis_cd)  

#IC para coeficientes del modelo
round(confint(modelo1), 2)

#Banda de confianza
p<-ggplot(bd, aes(x =dosis_cd , y = cd_tallo)) +  geom_point(aes(), colour ="deepskyblue", size=2)
p+ xlab("Dosis Cd (mg/kg suelo") +  ylab("Cd tallo (mg/g MS)") +  ggtitle("Absorción de Cd por tallo y hojas de E.ophiuroides")+ geom_smooth(method = "lm") #nivel de confianza 0.95 
p+ xlab("Dosis Cd (mg/kg suelo") +  ylab("Cd tallo (mg/g MS)") +  ggtitle("Absorción de Cd por tallo y hojas de E.ophiuroides")+ geom_smooth(method = "lm", level = 0.99)


## Validación ####

#Grafico de observados vs predichos
p<-ggplot(bd, aes(x =predict(modelo1) , y = cd_tallo)) +  geom_point(aes(), colour ="deepskyblue", size=2)
p+ xlab("Predichos") +  ylab("Cd tallo (mg/g MS)") +  ggtitle("Predichos vs observados") + geom_smooth(method = "lm", se = FALSE)

cor(pre,bd$cd_tallo)

#coeficiente de determinación (en summary)
summary(modelo1)$r.squared

############################################ #
# Cd aereo y raiz #####

#Para visualizar ambas VR en un mismo gráfico generamos un dataframe con formato long
library(reshape2)
id<-factor(1:20) #identificador de cada maceta, debe ser factor
bd<-cbind(id, cadmio)
# Specify id.vars: the variables to keep but not split apart on
bd2<-melt(bd, id.vars=c("id", "dosis_cd"))
bd2

p<-ggplot(bd2, aes(x =dosis_cd , y = value, colour = variable))
p+  geom_point(aes(colour = factor(variable)), size=2) +
  xlab("Dosis Cd (mg/kg suelo") +  ylab("Cd planta (mg/g MS)") +    geom_smooth(se = FALSE) 

# Modelo para Cd raiz ####

modelo2<-lm(cd_raiz ~ dosis_cd, data = bd)
summary(modelo2) 

#grafico de dispersion
g1<-ggplot(bd, aes(x=dosis_cd, y=cd_raiz))+geom_point(size=2) + geom_smooth(method="lm", se=FALSE) +
  xlab("Dosis Cd (mg/kg suelo") +  ylab("Cd raíz (mg/g MS)") + ggtitle("Variación del Cd acumulado en raíz según dosis de Cd")
g1

# Supuestos
e<-residuals(modelo2) # residuos
re<-rstandard(modelo2) #residuos estandarizados
pre<-predict(modelo2) #predichos

#grafico de RE vs PRED
g2=ggplot(bd, aes(x=fitted(modelo2), y=re))+geom_point(size=2)+ geom_smooth()+ ylim(-2.5,2.5)+geom_abline(slope=0, intercept=0) + geom_abline(slope=0, intercept=-2, color="red", linetype="dashed") + geom_abline(slope=0, intercept=2, color="red", linetype="dashed")  + ggtitle("Gráfico de RE vs predichos") +ylab("RE")
g2

library(gridExtra)
grid.arrange(g1, g2, ncol=1)

# Modelo cuadratico generando una VE^2
bd$dosis_cd_cuad <- bd$dosis_cd ** 2
modelo3 <- lm(cd_raiz ~ dosis_cd + dosis_cd_cuad, data = bd)
summary(modelo3)

#Otra opción por sintaxis:
modelo3<-lm(cd_raiz ~ dosis_cd + I(dosis_cd^2), bd)

#graficamos los residuos vs predichos
e<-residuals(modelo3) # residuos
re<-rstandard(modelo3) #residuos estandarizados
pre<-predict(modelo3) #predichos

#grafico de dispersion
g1=ggplot(bd, aes(x=dosis_cd, y=cd_raiz))+geom_point(size=2) + stat_smooth(method="lm", formula=y~ poly(x,2) )  +
  xlab("Dosis Cd (mg/kg suelo") +  ylab("Cd raíz (mg/g MS)") + ggtitle("Variación del Cd acumulado en raíz según dosis de Cd")

#grafico de RE vs PRED
g2=ggplot(bd, aes(x=fitted(modelo3), y=re))+geom_point(size=2)+ geom_smooth()+ ylim(-2.5,2.5)+geom_abline(slope=0, intercept=0) + geom_abline(slope=0, intercept=-2, color="red", linetype="dashed") + geom_abline(slope=0, intercept=2, color="red", linetype="dashed")  + ggtitle("Gráfico de RE vs predichos") +ylab("RE")

grid.arrange(g1, g2, ncol=1)

