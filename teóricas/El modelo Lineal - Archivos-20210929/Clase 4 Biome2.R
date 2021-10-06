#Modelado de Varianzas. Teorica Biome II

bd <- read.csv("Plomo.txt")

# Estadistica descriptiva####

#Graficos
#Reordeno ambientes
bd$Ambiente<-factor(bd$Ambiente,levels=c("Esp verdes","Residencial", "B carenciados","Riachuelo"))

#descriptiva por grupo
library(ggplot2)
#grafico de medias y DE con observaciones
ggplot(bd, aes(x=Ambiente, y=Pb)) + ylab("Plomo en fémur (ppm)")+
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), size=1,
               geom="pointrange", color="darkblue", alpha=0.6) +
  geom_jitter(alpha=0.3, size=2, position = position_jitter(width = .2))

#Boxplot
ggplot(data = bd, aes(x = Ambiente, y =Pb)) +
  geom_boxplot()+
  ylab("Plomo en hueso (PPM)")

#Boxplot con observaciones
ggplot(data = bd, aes(x = Ambiente, y =Pb)) +
  geom_boxplot() +
  geom_dotplot(binaxis='y',stackdir='center',dotsize = .5, fill="blue")+   ylab("Plomo en hueso (PPM)")

#Grafico de densidad
ggplot(data = bd, aes(Pb)) +
  geom_density(aes(fill=factor(Ambiente), alpha=0.8)) +
  xlab("Plomo en hueso (PPM)")

#Estadisticos
library(dplyr)
estad<-bd %>% 
  group_by(Ambiente) %>%
  summarise_all(.funs = c(
    n = length, 
    media = mean, 
    DE = sd,
    var= var,
    min = min, 
    max = max
  )
  )

estad

#Relacion media varianza
ggplot(estad, aes(x=media, y=var))+geom_point(size=4) +   xlab("Media") +  ylab("Varianza") +  ggtitle("Relación media - varianza") +   
  geom_smooth(method="lm", se = FALSE, color="red", linetype="dashed")

# Modelo de comparacion de medias ####

#cambio orden para q la referencia sean los espacios verdes
bd$Ambiente<-factor(bd$Ambiente,levels=c("Esp verdes","Residencial", "B carenciados","Riachuelo"))
modelo1<-lm(Pb ~ Ambiente, data=bd)
anova(modelo1)

# supuestos ####
#Graficamente

#qqplot
ggplot(bd, aes(sample=residuals(modelo1)))+stat_qq() + stat_qq_line()+ggtitle("QQ plot")

#grafico de RE vs PRED
ggplot(bd, aes(x=fitted(modelo1), y=rstandard(modelo1)))+geom_point(size=2)+geom_abline(slope=0, intercept=0) + geom_abline(slope=0, intercept=-2, color="red", linetype="dashed") + geom_abline(slope=0, intercept=2, color="red", linetype="dashed")  + ggtitle("Gráfico de RE vs predichos") +ylab("RE")

##Prueba de Shapiro para normalidad
shapiro.test(residuals(modelo1))

### Prueba de Levene para homogeneidad de varianzas
library(car)
leveneTest(modelo1) 

#Modelado de varianza #### 
#Cuadrados minimos generalizados. Estimacion por maxima verosimilitud restringida

library("nlme")
#modelo 2 sin modelar varianzas. Idem modelo1
modelo2<-gls(Pb~Ambiente, data=bd)
anova(modelo2)
modelo2
AIC(modelo2)

#Estudiamos los residuos
r2<-residuals(modelo2,type="p") #guardamos los residuos de Pearson = estandarizados
pred2<-fitted(modelo2)

plot(pred2, r2, xlab="Predichos", ylab="Residuos estandarizados",main="Grafico de dispersion de RE vs PRED" )
abline(0,0)
boxplot(r2~bd$Ambiente,xlab="Ambiente", ylab="Residuos estandarizados")
qqnorm(modelo2, abline = c(0,1)) #QQ plot

#modelo 3 modelado varianzas. VarIdent(ambiente)
modelo3<-gls(Pb~Ambiente, weights=varIdent(form=~1|Ambiente), data=bd)
anova(modelo3)
summary(modelo3)
AIC(modelo3)

#por default, estima por REML. Si quiero por LM:
modelo3<-gls(Pb~Ambiente, weights=varIdent(form=~1|Ambiente), method="ML", data=bd)
summary(modelo3)
modelo3

#Estudiamos los residuos
r3<-residuals(modelo3,type="p") #guardamos los residuos de Pearson = estandarizados
pred3<-fitted(modelo3)
plot(pred3, r3, xlab="Predichos", ylab="Residuos estandarizados",main="Grafico de dispersi?n de RE vs PRED" )
abline(0,0)
boxplot(r3~bd$Ambiente,xlab="Ambiente", ylab="Residuos estandarizados")
qqnorm(modelo3, abline = c(0,1)) #QQ plot

#modelo 4 modelado varianzas. varPower
modelo4<-gls(Pb~Ambiente, weights=varPower(), data=bd)
anova(modelo4)
summary(modelo4)
modelo4

#Calculo residuos del individuo Nro 2
residuals(modelo4)[2] #Sin normalizar
residuals(modelo4,type="n")[2] #Normalizado
#formula de normalizacion
(residuals(modelo4)[2])/(sqrt((0.1362282^2)*(fitted(modelo4)[2])^(2*1.529801)))


#Estudiamos los residuos
r4<-residuals(modelo4,type="p") #guardamos los residuos de Pearson = estandarizados
pred4<-fitted(modelo4)
plot(pred4, r4, xlab="Predichos", ylab="Residuos estandarizados",main="Gr?fico de dispersi?n de RE vs PRED" )
abline(0,0)
boxplot(r4~bd$Ambiente,xlab="Ambiente", ylab="Residuos estandarizados")
qqnorm(modelo4, abline = c(0,1)) #QQ plot

#modelo 5 modelado varianzas. varExp
modelo5<-gls(Pb~Ambiente, weights=varComb(varExp()), data=bd)
anova(modelo5)
modelo5

#Estudiamos los residuos
r5<-residuals(modelo5,type="p") #guardamos los residuos de Pearson = estandarizados
pred5<-fitted(modelo5)

plot(pred5, r5, xlab="Predichos", ylab="Residuos estandarizados",main="Grafico de dispersi?n de RE vs PRED" )
abline(0,0)
boxplot(r5~bd$Ambiente,xlab="Ambiente", ylab="Residuos estandarizados")
qqnorm(modelo5, abline = c(0,1)) #QQ plot

AIC(modelo2,modelo3,modelo4,modelo5)

# Ganador: Modelo4

anova(modelo4)


########################################################### #
# Comparaciones entre Ambientes ####
library(emmeans)
#https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
Comp2<-emmeans(modelo4, pairwise ~ Ambiente) #Tukey por default  
Comp2
plot(Comp2, comparisons = TRUE)
confint(Comp2) #IC para la dif de medias

#Bonferroni
Comp3<-emmeans(modelo4, pairwise ~ Ambiente, adjust = "bonf")
Comp3
confint(Comp3)

#Dunnet (todos vs control; en este caso espacios verdes)
Comp4 <- emmeans(modelo4, ~ Ambiente)   
dunnet <- contrast(Comp4 , method = "trt.vs.ctrl", ref = 1) 
dunnet
confint(dunnet)

#Contrastes ortogonales
#especificamos los contrastes
c1<-c(1/2,1/2,-1/2,-1/2)
c2<-c(1,-1,0,0)
#creamos la matriz de coeficientes:
matriz_contrastes<-cbind(c1,c2) 
matriz_contrastes
modelo4b <- lm(Pb ~ Ambiente, data=bd, contrasts=list(Ambiente=matriz_contrastes))
summary(modelo4b)

#Gráfico con las estimaciones del modelo
estimaciones<-as.data.frame(Comp2$emmeans)

ggplot(estimaciones, aes(x=Ambiente, y=emmean)) + 
  labs(x="Ambiente") + labs(y="Plomo en fémur (ppm)") +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), color="darkblue", width=0.1)+ geom_point(shape=15, size=4, color="darkblue") + 
  ggtitle("Acumulación de plomo en ratas de distintos ambientes en CABA", "Media (EE)")+ annotate("text", x=c(1,2,3,4), y=c(3.5, 4.3, 4.3, 5.4), label = c("A", "AB", "B","C")) + theme(text=element_text(size=14))


#grafico de medias e IC95% por bootstrap
ggplot(bd, aes(x=Ambiente, y=Pb)) + ylab("Plomo en fémur (ppm)")+
  stat_summary(fun.data=mean_cl_boot,size=1, color="darkblue") 

#otra opción:
library(ggeffects)
ggpredict(modelo4)
plot(ggpredict(modelo4), add.data = TRUE) 
