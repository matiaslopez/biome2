#Medidas repetidas
bd <- read.csv("asma.csv")
names(bd)
summary(bd)
str(bd)
head(bd)
#paciente como cuali
bd$paciente <- factor(bd$paciente)

#Descriptiva ####
library(ggplot2)
#spaguetti plots
ggplot(data = bd, aes(x = tiempo, y = vef, group = paciente)) + 
  geom_line(aes(colour=paciente)) + 
  facet_grid (. ~ droga)+ labs(y="VEF (litros)")

#cambio el orden de los niveles y vuelvo a graficar
bd$tiempo<-factor(bd$tiempo,levels=c("basal","1h" ,   "2h",    "3h",    "4h",    "5h",    "6h",    "7h",    "8h"))

#cada paciente por separado
ggplot(data = bd, aes(x = tiempo, y = vef, color=droga)) +
  facet_wrap(~ paciente, ncol=6) + 
  geom_point() + 
  geom_line(aes(x=as.integer(tiempo))) +  # conecta los puntos
  labs(y="VEF (litros)")

# grafico de perfiles
bd %>% 
  group_by(droga, tiempo) %>% 
  summarize(vef = mean(vef)) %>% 
  ggplot(aes(x = tiempo, y = vef, colour=droga))+
    geom_point(size=3) +
  geom_line(aes(x=as.integer(tiempo))) + 
  labs(y="VEF (litros)")

#estadisticos droga x tiempo
library(dplyr)
bd %>% 
  select(-paciente) %>% 
  group_by(droga, tiempo) %>%
  summarise_all(.funs = c(
    n = length, 
    media = mean, 
    DE = sd,
    var= var,
    min = min, 
    max = max
  )
  )

#Correlaciones entre tiempos ####
#para convertir long to wide (cada tiempo en una columna distinta)
library(tidyr)
bd_wide <- spread(bd, tiempo, vef)
tiempos <-bd_wide[3:11]
library(ggplot2)
library(GGally)
ggpairs(tiempos) 
round(cov(tiempos),2) #matriz de covarianza 
round(cor(tiempos),2) #matriz de correlacion

#Heatmap
ggcorr(tiempos, label = TRUE, label_size = 3,limits = FALSE, midpoint = NULL, label_round = 2) #heatmap

#r vs tiempo
Tiempo<-c(1:7)
r<-c(0.927142857,	0.898333333,	0.866,	0.8275,	0.8,	0.76,	0.77)
r_t<-as.data.frame(cbind(Tiempo,r))
plot(r_t)

library(doBy)
summaryBy(vef~c(droga, tiempo), data=bd, FUN=c(mean, var, length))

#Modelos marginales para Medidas repetidas ####
library(nlme)
#Incluyo al basal como variable explicatoria
#uso funcion gather de tidyr
library(tidyr)
keycol <- "tiempo"
valuecol <- "vef"
gathercols <- c("1h", "2h", "3h", "4h", "5h", "6h", "7h", "8h")
bd2 <- gather(bd_wide, keycol, valuecol, gathercols)
colnames(bd2)[4] <- "tiempo"
colnames(bd2)[5] <- "vef"


#Modelo 1: Simetría compuesta. cada individuo con id unico
m1<-gls(vef ~droga*tiempo+basal,  correlation = corCompSymm(form = ~ 1 | paciente), bd2)
getVarCov(m1) #extrae la matriz de cov estimada

#Modelo 2: Simetría compuesta. varianzas distintas
m2<-gls(vef ~droga*tiempo+basal,  correlation = corCompSymm(form = ~ 1 | paciente), bd2, weights=varIdent(form= ~ 1|tiempo ))
getVarCov(m2)

#Modelo 3: AR1, varianzas iguales
m3<-gls(vef ~droga*tiempo+basal,  correlation = corAR1(form = ~ 1 | paciente), bd2)
getVarCov(m3)

#Modelo 4: AR1, varianzas distintas
m4<-gls(vef ~droga*tiempo+basal,  correlation = corAR1(form = ~ 1 | paciente), bd2, weights=varIdent(form= ~ 1|tiempo ))

#Modelo 5: matriz desestructurada
m5<-gls(vef ~droga*tiempo+basal,  correlation = corSymm(form = ~ 1 | paciente), bd2)

library(nlme)
#0tra opcion equivalente a m1 que incorpora al individuo como factor aleatorio para inducir la estructura
m6<-lme(vef ~droga*tiempo+basal,  random = ~1|paciente, bd2)

#Seleccion de modelos #####
summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
AIC(m1,m2,m3,m4, m5)

anova(m1,m2,m3,m4, m5)
anova(m5) #winner
getVarCov(m5)

#Supuestos ####
e<-resid(m5) # residuos de pearson
pre<-predict(m5) #predichos
par(mfrow = c(2, 2))
plot(pre, e, xlab="Predichos", ylab="Residuos de pearson",main="Gráfico de dispersión de RE vs PRED",cex.main=.8 )
plot(bd2$droga, e, xlab="Predichos", ylab="Residuos de pearson",main="Gráfico de dispersión de RE vs Trat",cex.main=.8 )
abline(0,0)
qqnorm(e, cex.main=.8)
qqline(e)
par(mfrow = c(1, 1))
shapiro.test(e)
hist(e)

#Significación Parte fija ####
anova(m5)  #interaccion tiempo trat signif
#Comparaciones de ef simples (entre drogas a cada tiempo)
library(emmeans)
emmeans(m5, pairwise ~ droga | tiempo)

#Comparaciones de interaccion
comp1<-emmeans(m5, pairwise ~ droga*tiempo) 


#grafico de predicciones del modelo ####
resumen_modelo<-as.data.frame(comp1$emmeans)
ggplot(resumen_modelo, aes(x=tiempo, y=emmean, fill=droga)) + 
  geom_point(aes(colour=droga))  +
  labs(x="Tiempo") + labs(y="VEF (litros)") +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE, colour=droga),  width=0.2)+
  geom_line(aes(x=as.integer(tiempo), colour=droga)) +
   ggtitle("Variación de VEF en función del tiempo según tratamiento", "Media ± error estándar a partir del modelo")


#otra opción:
library(ggeffects)
(a<-ggpredict(m5))
a <- ggpredict(m5, terms = c("tiempo", "droga"))

plot(a, add.data = TRUE, grid = TRUE) 

##################### #
#Biblio ####

install.packages(c("shiny", "nlme", "lattice", "lme4", "MCMCglmm", "MASS",
                   "geepack", "corrplot"), dependencies = TRUE)

library(shiny)
library(nlme)
library(lattice)
library(lme4)
library(MCMCglmm)
library(MASS)
library(geepack)
library(corrplot)

  
shiny::runGitHub("Repeated_Measurements", "drizopoulos")
