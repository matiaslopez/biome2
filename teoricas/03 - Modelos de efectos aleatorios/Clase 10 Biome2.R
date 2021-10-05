#Anidado
bd <- read.csv("semillas.csv")
names(bd)
summary(bd)
#pasamos potrero como factor
bd$potrero<-as.factor(bd$potrero)

#opcion 1. Ignorando potreros ####
library(dplyr)
bd %>%
  group_by(tratamiento) %>%
  summarise(n=n(),
            media = mean(biomasa),
            DE=sd(biomasa),
            EE=DE/sqrt(n)) 

library(ggplot2)
ggplot(bd, aes(tratamiento, biomasa, fill=tratamiento))+ 
  geom_boxplot() + 
  geom_jitter(position=position_jitter(0.2)) + 
  labs(y="Biomasa de semillas (g/m2)") 

#grafico de medias y DE con observaciones segun potrero
ggplot(bd, aes(tratamiento, biomasa, color=potrero)) + 
  ylab("Biomasa de semillas (g/m2)")+
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), size=1, 
               geom="pointrange", color="darkblue") +
   geom_jitter(size=2, position = position_jitter(width = .2))

m1<-lm(biomasa~tratamiento, bd)
anova(m1)
summary(m1)  

#opcion 2. promediando VR por potrero ####

#promediando biomasa por potrero y calculando estadísticos
bd %>%
  group_by(tratamiento, potrero) %>%
  summarise(n=n(),
            media1 = mean(biomasa)) %>% 
  summarize(n=n(),
            media = mean(media1),
            DE=sd(media1),
            EE=DE/sqrt(n)) 

ggplot(bd, aes(tratamiento, biomasa, fill=interaction(tratamiento, potrero), dodge=potrero)) +  
  geom_boxplot()  + geom_jitter() +labs(y="Biomasa de semillas (g/m2)") 

#grafico medias de potreros para cada trat
bd %>%
  group_by(tratamiento, potrero) %>%
  summarise(biomasa = mean(biomasa)) %>% 
  ggplot(aes(tratamiento, biomasa, color=potrero)) + 
  ylab("Biomasa de semillas (g/m2)")+
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), size=1, 
               geom="pointrange", color="darkblue") +
  geom_jitter(size=2, position = position_jitter(width = .2))+
  ylim(0,17.5)

m2<-lm(biomasa~tratamiento, medias.potrero)
anova(m2)
summary(m2)

#opcion 3. Considerando potreros en el modelo. Modelo condicional ####
a<-bd %>% 
  group_by(tratamiento, potrero) %>% 
  summarize(biomasa = mean(biomasa)) 

b<-bd %>%
  group_by(tratamiento, potrero) %>%
  summarise(n=n(),
            media1 = mean(biomasa)) %>% 
  summarize(n=n(),
            biomasa = mean(media1),
            DE=sd(media1)) 

ggplot(bd, aes(tratamiento, biomasa, color=potrero)) + 
  ylab("Biomasa de semillas (g/m2)")+
  geom_jitter(size=2, alpha=0.3, position = position_jitter(width = .2))+  
  geom_jitter(data=a,  size=4, position = position_jitter(width = .2))+
  geom_point(data=b, aes(x=tratamiento, y=biomasa), color="darkblue", size=4)+
  geom_errorbar(data=b, aes(ymin = biomasa-DE, ymax = biomasa+DE), color="darkblue", width=0, size=1)
  
library(lme4)
m3<- lmer(biomasa ~ tratamiento + (1|potrero), bd)
anova(m3) #no informa p-valores
summary(m3)

library(lmerTest) #Test de Wald, agrega p-valores a la salida de lmer
m3<- lmer(biomasa ~ tratamiento + (1|potrero), bd)
anova(m3)
summary(m3)

#opcion 3b. idem anterior, pero usando nlme
library(nlme)
m3b <- lme(biomasa ~ tratamiento, random= ~1 | potrero, data = bd)
anova(m3)
summary(m3b)

#opcion 4. usando gls (modelo marginal)
m4 <- gls(biomasa ~ tratamiento, correlation=corCompSymm(form = ~ 1 | potrero), data = bd)
summary(m4)

#Nos quedamos con m3. Supuestos
e<-resid(m3) # residuos de pearson
pre<-predict(m3) #predichos
Bi<-ranef(m3)$potrero$'(Intercept)'
par(mfrow = c(1, 3))
plot(pre, e, xlab="Predichos", ylab="Residuos de pearson",main="Gráfico de dispersión de RE vs PRED",cex.main=.8 )
abline(0,0)
qqnorm(e, cex.main=.8)
qqline(e)
qqnorm(Bi, cex.main=.8)
qqline(Bi)
par(mfrow = c(1, 1))
plot(m3) #RP vs PRED
shapiro.test(e)
shapiro.test(Bi)

#Parte fija
fitted(m3)  #predicciones parte fija + aleatoria
fixef(m3) #estimacion parametros efectos fijos
X<-model.matrix(m3)
pred_fija<-X %*% fixef(m3)#predicciones parte fija 

#para ver la significación de la parte fija:
#Test de Wald
library(lmerTest)
m3<- lmer(biomasa ~ tratamiento + (1|potrero), bd)
summary(m3)
#LRT: usando la funcion drop o anova
drop1(m3, test="Chisq") 
#significacion parte aleatoria
ranova(m3)

#Parte aleatoria
Bi<-ranef(m3)$potrero$'(Intercept)' #alfa i de cada potrero
sd(Bi)^2
library(sjPlot)
library(sjlabelled)
library(sjmisc)
plot_model(m3, type="re", show.values = TRUE, value.offset = .3)

#signif parte aleatoria
confint(m3, level = 0.95, method = c("profile"))
#como siempre
AIC(m1,m3)

#Fijos y presentación
tab_model(m3, p.val = "kr")

#Grafico final con estimaciones segun modelo
library(emmeans)
comp1<-emmeans(m3, pairwise ~ tratamiento)
confint(comp1) #magnitud del efecto

resumen_anidado<-as.data.frame(comp1$emmeans)
ggplot(resumen_anidado, aes(x=tratamiento, y=emmean)) + 
  labs(x="Tratamiento") + labs(y="Biomasa de semillas (g/m2)") +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), color="blue", width=0.2)+
  geom_point(shape=15, size=4, color="blue") + 
  ggtitle("Comparación de la biomasa de semillas según pastoreo", "Media ± error estándar")+
  annotate("text", x=1.5, y=9, label = "p=0,028")

#otra opción:
library(ggeffects)
(a<-ggpredict(m3))
plot(a, add.data = TRUE, grid = TRUE) 

#Comparando estimaciones
m3b<- lmer(biomasa ~ tratamiento + (1|potrero), bd, REML=FALSE)
summary(m3b)
m0<-lm(biomasa ~ tratamiento, bd)
summary(m0)
anova(m0)

##############################
#Otras estructuras de anidamiento

#Anidado con dos efectos aleatorios
Compvar <- read.csv2("Compvar.csv")
BD<-Compvar

names (BD)
BD<-BD[,1:3]
head(BD)
str(BD)
summary(BD)

#modelo anidado
m4<- lmer(largo ~ 1 + (1|poblacion/familia), BD)
m4b<-lme(largo ~ 1, random = ~ 1|poblacion/familia, BD)

summary(m4)
dotplot(ranef(m4, condVar = TRUE))
sjp.lmer(m4,sort.est = "sort.all", facet.grid = FALSE, y.offset = .4)

#Significacion
m5<- lmer(largo ~ 1 + (1|poblacion), BD)
anova(m5,m4)
AIC(m5, m4)

#Predichos
F0 <- fitted(m5, level = 0) #poblacional
F1 <- fitted(m5, level = 1) #condicional a cada familia
a<-cbind(F0,F1)
a

###############################

#Intercepto y pendiente aleatoria?
m4<- lmer(biomasa ~ tratamiento + (1+tratamiento|potrero), bd)
summary(m4)
sjp.lmer(m4, type = "rs.ri")
