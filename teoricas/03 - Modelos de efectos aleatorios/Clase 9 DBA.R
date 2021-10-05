#DBA

bd <- read.delim("ratones_camada.txt")
summary(bd)
head(bd)

#bloque como cuali
bd$camada <- factor(bd$camada)

# Descriptiva ####
library(dplyr)
bd %>% 
  group_by(etanol) %>%
  summarise(n = length (vol), 
    media = mean (vol), 
    DE = sd(vol),
    min = min(vol), 
    max = max(vol))
    
bd %>% 
  group_by(camada) %>%
  summarise(n = length (vol), 
                media = mean (vol), 
                DE = sd(vol),
                min = min(vol), 
                max = max(vol))

# Grafico de dispersion sin camadas
library(ggplot2)
ggplot(bd, aes(etanol, vol)) + 
  geom_point(aes(colour=camada)) + 
  labs(x="dosis etanol (g/kg)", y="volumen cerebro (cm3)") +
  geom_smooth( method = "lm", se=F)

# Grafico de dispersion con camadas
ggplot(bd, aes(etanol, vol)) + 
  geom_point(aes(colour=camada)) + 
  labs(x="dosis etanol (g/kg)", y="volumen cerebro (cm3)") + 
  geom_line(aes(colour=camada))

# Grafico de dispersion con camadas, efectos fijos
ggplot(bd, aes(etanol, vol, colour=camada))+
  geom_point() + 
  geom_abline(intercept = 1.954, slope = -0.4375, colour="red") + 
  labs(x="dosis etanol (g/kg)", y="volumen cerebro (cm3)") + 
  geom_abline(intercept = (1.954-0.13333), slope = -0.4375, colour="olivedrab3") +
  geom_abline(intercept = (1.954+0.13333), slope = -0.4375, colour="green") +
  geom_abline(intercept = (1.954+0.03333), slope = -0.4375, colour="#56B4E9")+
  geom_abline(intercept = (1.954-0.21667), slope = -0.4375, colour=4)+
  geom_abline(intercept = (1.954+0.23333), slope = -0.4375, colour=6)


#Modelos ####
# sin considerar la camada (muy mal!) 
m0<-lm(vol~etanol, bd)
summary(m0) 
anova(m0)

#considerando camadas como de efectos fijos
m1<-lm(vol~etanol+camada, bd)
summary(m1) 
anova(m1)

#considerando camadas como de efectos aleatorios (modelo condicional) usando lme4
library(lme4)
m2 <- lmer(vol ~ etanol + (1 | camada), data = bd)
summary(m2)

#para ver la significación:
library(lmerTest)
m2 <- lmer(vol ~ etanol + (1 | camada), data = bd)
summary(m2)

#considerando camadas como de efectos aleatorios pero usando nlme (modelo condicional)
library(nlme)
m2b <- lme(vol ~ etanol, random= ~1|camada, data = bd)
summary(m2b)

#Supuestos ####
e<-resid(m2) # residuos de pearson
pre<-predict(m2) #predichos

#QQplot
ggplot(bd, aes(sample=residuals(m2)))+
  stat_qq() + 
  stat_qq_line()+
  ggtitle("QQ plot")

#grafico de RE vs PRED
ggplot(bd, aes(x=fitted(m1), y=rstandard(m1)))+geom_point(size=2)+geom_abline(slope=0, intercept=0) + geom_abline(slope=0, intercept=-2, color="red", linetype="dashed") + geom_abline(slope=0, intercept=2, color="red", linetype="dashed")  + ggtitle("Grafico de RE vs predichos") + ylab("RE") +xlab("Predichos") + ylim(-2.5, 2.5)

shapiro.test(e)

#Parte fija ####
summary(m2)
confint(m2)

# Grafico de dispersion del modelo estimado
library(ggeffects)
(a<-ggpredict(m2)) #predicciones del modelo
p<-pred$etanol #extraemos las predicciones
#usando ggeffects
plot(a, add.data = TRUE) 
#+ labs(x=, y=, title= "")
#usando ggplot
ggplot(p, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) + 
  labs(x="dosis etanol (g/kg)", y="volumen cerebro (cm3)") 

#Parte fija + aleatoria ####
# Grafico de dispersion con camadas y modelo estimado
ggplot(bd, aes(etanol, vol, colour=camada))+
  geom_point() + 
  geom_abline(intercept = 1.955, slope = -0.4375, colour="red") + labs(x="dosis etanol (g/kg)", y="volumen cerebro (cm3)") + 
  geom_abline(intercept = (1.844453), slope = -0.4375, colour="olivedrab3") +
  geom_abline(intercept = (2.066659), slope = -0.4375, colour="green") +
  geom_abline(intercept = (1.983332), slope = -0.4375, colour="#56B4E9")+
  geom_abline(intercept = (1.775014), slope = -0.4375, colour=4)+
  geom_abline(intercept = (2.149986), slope = -0.4375, colour=6) +
  geom_abline(intercept = (1.9625), slope = -0.4375, colour="black", linetype="dashed", size=1)


coef(m2) #muestra los coef para cada nivel aleatorio (camada)

fitted(m2)  #predicciones parte fija + aleatoria
fixef(m2) #estimacion parametros efectos fijos
pred_fija<-fixef(m2)#predicciones parte fija 
alfai<-round(ranef(m2)$camada$'(Intercept)', 4)
pred<-cbind(bd$etanol,bd$camada, bd$vol,pred_fija, alfai,round(fitted(m2),4))
colnames(pred)<-c("etanol", "camada","vol", "pred fija", "efecto aleat", "pred fija + aleat")
pred

#Atenti, los interceptos fijos no son iguales a los aleatorios
m1b<-lm(vol~-1+etanol+camada, bd) #-1 saca el grupo de ref
summary(m1b) # pendiente y ordenadas (fijas) para cada camada
interc_fijo<-as.numeric(m1b$coefficients[-1])
interc_aleat<-(coef(m2)$camada[,"(Intercept)"]) 
camada<-c("camada1", "camada2", "camada3", "camada4", "camada5", "camada6")
interc<-data.frame(camada, interc_fijo,interc_aleat)
interc

#BLUE vs BLUP
BLUE_BLUP<-data.frame(camada, interc_fijo-1.9625, interc_aleat-1.9625)
colnames(BLUE_BLUP)<-c("camada", "BLUE", "BLUP")
BLUE_BLUP #los BLUPS tb se puede pedir como 
ranef(m2)

#para graficar
camada<-c("camada1", "camada2", "camada3", "camada4", "camada5", "camada6", "camada1", "camada2", "camada3", "camada4", "camada5", "camada6")
x<-c("fijo", "aleatorio")
intercepto<-rep(x, each=6)                                                                                            
valor<-as.numeric(c(interc_fijo, interc_aleat))
interc<-data.frame(camada, intercepto, valor)
interc
interc$intercepto<- factor(interc$intercepto,levels = c("fijo", "aleatorio")) #reordeno para q primero vaya fijo

ggplot(interc, aes(intercepto, valor, colour=camada, group=camada))+
  geom_line(aes(linetype=camada)) + geom_point(aes(shape=camada))  +
    labs(x="intercepto", y="estimación/predicción") +
  geom_hline(yintercept=1.9625, linetype="dashed")

#seleccion de modelos
ranova(m2) #factor aleatorio es relevante?

m0<- lmer(vol ~  (1|camada), bd)
anova(m0,m2)
AIC(m0,m2)

#usando pendientes aleatorias
m3 <- lmer(vol ~ etanol + (etanol| camada), data = bd)
summary(m3)
coef(m3) #muestra los coef para cada nivel aleatorio (camada)
ranova(m3)

#usando gls (modelo marginal)
m2c <- gls(vol ~ etanol, correlation=corCompSymm(form = ~ 1 | camada), data = bd)
summary(m2c)




