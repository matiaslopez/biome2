## Distribucion de Poisson
## Que significa la relacion esperanza-varianza?

library(ggplot2)
n = 1000
lambda = 1:20
var=c()
for (i in 1:length(lambda)){
  ranPois = rpois(n = n, lambda = lambda[i])
  var=c(var,var(ranPois))
}
pois=data.frame(cbind(lambda,var))
poisEspVar<-ggplot(pois, aes(lambda, var))+ geom_point(size = 2, aes(color = factor(lambda))) + labs(x = "Esperanza",y = "Varianza",colour = "Lambda") + ggtitle("Relacion Esperanza-Varianza para Poisson") + theme(plot.title = element_text(hjust = 0.5))+theme(legend.text=element_text(size=6))+ guides(col = guide_legend(ncol = 2))
poisEspVar

##
ls()
rm(list=ls())
ls()

# setwd("XXXXXX")
setwd("~/Biome2_2021/Modulo 2/TP_Poisson/Poisson")


############################################
## Problema 1.                             #
## Reservas Urbanas y conservacion de aves #
############################################


#Indique cual es la variable dependiente o respuesta. 
# ¿Cual es su potencial distribucion de probabilidades? ¿Cual es la variable explicatoria? 
# ¿De que tipo es?  


#############################
# Inspeccion del data.frmae #
#---------------------------#
#############################

aves <- read.table("aves.txt", header = T)
summary(aves)
str(aves)
head(aves)
table(aves$Ambiente)

# Pasamos la variable offset a logaritmo
aves$Log_DurObserv <- log(aves$DurObserv)

# calculamos la tasa de avisatjes por minuto a partir del N de aves (para descriptiva)
aves$tasa_Aves <- aves$Aves / aves$DurObserv

# chequeamos data.frame
summary(aves)
str(aves)
head(aves)


#############################
# Analisis exploratorio     #
#---------------------------#
#############################

library(ggplot2)
ggplot(aves, aes(Ambiente, tasa_Aves)) + 
            geom_boxplot() + 
            geom_jitter(width = 0.1, alpha=0.3) 
            

ggplot(data = aves, aes(tasa_Aves)) +
  geom_density(aes(fill=factor(Ambiente), alpha=0.1)) +
  xlab("Número de aves/mim")


library(GGally)
ggpairs(aves)

#Veamos que ggpairs puede ser mucho mas configurable
ggpairs(aves,mapping = ggplot2::aes(colour=Ambiente),lower = list(continuous = wrap("smooth", alpha = 0.3, size=2), 
                    discrete = "blank", combo="blank"), 
       diag = list(discrete="barDiag", 
                   continuous = wrap("densityDiag", alpha=0.5 )), 
       upper = list(combo = wrap("box_no_facet", alpha=0.5),
                    continuous = wrap("cor", size=4, align_percent=0.6, family="sans"))) + 
       theme(panel.grid.major = element_blank())


################################
# Modelo e implementacion en R #
#------------------------------#
################################

#Plantee y escriba el modelo en formato regresion


m1 <- glm(Aves~ Ambiente + offset(Log_DurObserv), data=aves, family=poisson)
summary(m1)


#############
# Supuestos #
#-----------#
#############

# Primero vemos si hay sobre o subdispersion
# Para modelo1
sum(resid(m1, type="pearson")^2)/m1$df.residual


# Supuestos ####
# Residuos 
names(m1)
r <- resid(m1, type="response") # residuos crudos, por default
rd<-resid(m1, type="deviance") # residuos devianza
rp<-resid(m1, type="pearson") #residuos de Pearson

#predichos
pred.PL<-predict(m1, type="link") #predichos en escala del PL
pred.VR<-predict(m1, type = "response") #predichos en escala de Y
#Otra forma:
pred.VR<-fitted(m1) 

# Armamos un data.frame con VR, offset, predichos en escala de PL y de la VR y residuos
res<-data.frame(aves$Ambiente, aves$Aves, aves$Log_DurObserv, round(pred.PL,2),round(pred.VR,2), round(r,2),round(rp,2), round(rd,2))
colnames(res)<-c("Amb", "Aves", "LogDurac", "pred.PL", "pred.VR", "resid crudos", "resid Pearson","resid devianza")
res
head(res)

# Grafico de residuos vs predichos
# Estandarizados
ggplot(aves, aes(x=fitted(m1), y=resid(m1, type="pearson")))+geom_point(size=2)+geom_abline(slope=0, intercept=0) + ggtitle("Grafico de Residuos de Pearson vs predichos") +ylab("Residuos de pearson")

plot(cooks.distance(m1), type="h", xlab="Id", ylab= "Distancia de Cook", main="Gráfico de Distancia de Cook")


# Analizamos supuestos con Dharma

library(DHARMa)

# Simulaciones Dharma
sim <- simulateResiduals(m1, n = 1000)
plot(sim)



# Validacion Modelo

# Grafico de valores observados y predichos
graf <- data.frame(m1$fitted.values, aves$Aves)
ggplot(data = graf, aes(x = m1.fitted.values, y = aves.Aves)) + geom_point(colour = "blue") +
  ylab("Obs") + xlab("predichos") + geom_abline(intercept = 0, slope = 1) +
  theme_classic()

#Devianza explicada
((m1$null.deviance-m1$deviance)/m1$null.deviance)*100


########################################
# Modelo: Resultados e interpretacion  #
#------------------------------------  #
########################################

# En que medida las reservas favorecen la conservacion de aves nativas?. 
# Informe la magnitud del efecto en escala de la variable respuesta.  

# Interpretacion del resultado

# significancia 'global'
drop1(m1, test = "Chisq") # en este caso no es necesario, 1 variable cat con 2 niveles

# coeficientes del modelo
summary(m1)               # en que escala estan los coeficientes?



##################
# Comparaciones  #
# Mag del efecto #
# -------------- #
##################

# Reconstruccion de las medias y Magnitud del efecto a partir del summary()

# calcule las medias estimadas en la escala de la variable respuesta
exp(coef(m1))

# Valor esperado Urbano:

# Valor esperado Reserva:


# y la magnitud del efecto? 
exp(confint(m1))


# con tab_model
library(sjPlot)
tab_model(m1)

# usando emmeans (solo a fines didacticos, no es necesario para analizar magnitud del efecto en este caso)
library(emmeans)

options(emmeans= list(emmeans = list(infer = c(TRUE, TRUE)),
                      contrast = list(infer = c(TRUE, TRUE)))) # opciones de seteo de las opciones de salida


# Se compara entre ambientes en escala log
compPL <- emmeans(m1, pairwise ~ Ambiente, offset = 0) # Tukey por default
compPL

# Si quisieramos en escala de la variable respuesta
compVR <- emmeans(m1, pairwise ~ Ambiente, type="response", offset = 0) # Tukey por default
compVR

# ¿Por que incluimos offset=0?
# A common usage would be to specify offset = 0 for a Poisson regression model, so that predictions from the reference grid become rates relative to the offset that had been specified in the model.

# pero por default emmeans no pone offset = 0
# Si no ponemos offset=0
compVRof <- emmeans(m1, pairwise ~ Ambiente, type="response") # 
compVRof

#Observar que la diferencia entre incluir o no incluir offset=0 es:
ref_grid(m1)


###############################
# Grafico del modelo predicho #
# --------------  #
###############################

library(ggeffects)
estim<-ggpredict(m1, terms = c("Ambiente"))
plot(estim, add.data = TRUE)

### FIN ###
