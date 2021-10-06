library(ggplot2)
library(car)
library(agricolae)
bd<- read.csv2("FITOR.csv")
names(bd)# "veg"  "fert" "HC"  


############################################################ #
# Descriptiva #####
#Estadisticos
library(dplyr)
bd %>% 
  group_by(veg, fert) %>%
  summarise_all(.funs = c(
    n = length, 
    media = mean, 
    DE = sd,
    var= var,
    min = min, 
    max = max
  )
  )

#Graficos de perfiles
#ordenamos los niveles
bd$veg<-factor(bd$veg,levels=c("Testigo","Agropiro", "Charcao"))

ggplot(bd) +
  aes(x = veg, y = HC, color = fert, group = fert) +
  geom_jitter(width = 0.2) +
  stat_summary(fun.data = "mean_se", 
               geom = "pointrange", size=1) +
  geom_line(stat = "summary", fun = mean) + 
  labs(x="Planta") + labs(y="Hidrocarburos (g/100 g de suelo)") +
  theme(text=element_text(size=14))

ggplot(bd) +
  aes(x = fert, y = HC, color = veg, group = veg) +
  geom_jitter(width = 0.2) +
  stat_summary(fun.data = "mean_se", 
               geom = "pointrange", size=1) +
  geom_line(stat = "summary", fun = mean)  + 
  labs(x="Planta") + labs(y="Hidrocarburos (g/100 g de suelo)") + 
  theme(text=element_text(size=14))


################################################################### #
# Estimamos el modelo #####
m1<-lm(HC~veg*fert, bd)

#Reordeno niveles de los factores para que quede la referencia que elijo
bd$veg<-factor(bd$veg,levels=c("Testigo","Agropiro", "Charcao"))
bd$fert<-factor(bd$fert,levels=c("NF","F"))

anova(m1)

# Supuestos ####

#Calculamos los residuos y los predichos
e<-resid(m1) # residuos
re<-rstandard(m1) #residuos estandarizados
pre<-predict(m1) #predichos

#Supuestos

#qqplot
ggplot(bd, aes(sample=residuals(m1)))+stat_qq() + stat_qq_line()+ggtitle("QQ plot")

#grafico de RE vs PRED
ggplot(bd, aes(x=fitted(m1), y=rstandard(m1)))+geom_point(size=2)+geom_abline(slope=0, intercept=0) + geom_abline(slope=0, intercept=-2, color="red", linetype="dashed") + geom_abline(slope=0, intercept=2, color="red", linetype="dashed")  + ggtitle("Grafico de RE vs predichos") +ylab("RE")

shapiro.test(e)
leveneTest(HC~veg*fert, bd, center=mean)

########################################################## #
# Comparaciones de Interaccion ####
##todos contra todos 
library(emmeans)
#Tukey por default
comp1<-emmeans(m1, pairwise ~ fert*veg)
comp1
plot(comp1, comparisons = TRUE)

medias<-aggregate(HC~fert+veg, bd,mean)

#Efectos simples
#Como la interaccion fue signif, se comparan fert para cada sp
emmeans(m1, pairwise ~ fert | veg) #Tukey por default
efsimple_veg<-emmeans(m1, pairwise ~ fert | veg)
plot(efsimple_veg, comparisons = TRUE)

#O al reves
emmeans(m1, pairwise ~ veg | fert) #entre sp para cada nivel de fert
plot(emmeans(m1, pairwise ~ veg | fert), comparisons = TRUE)

#Gráfico con las estimaciones del modelo ####
estimaciones<-as.data.frame(comp1$emmeans)
ggplot(estimaciones) +
  aes(x = veg, y = emmean , color = fert, group = fert) +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=0.2)+
  geom_point(size=4) +
  geom_line(stat = "summary", fun = mean) + 
  labs(x="Planta") + labs(y="Hidrocarburos (g/100 g de suelo)") +
  theme(text=element_text(size=14))

#otra opción:
library(ggeffects)
estim<-ggpredict(m1, terms = c("veg", "fert"))
plot(estim, add.data = TRUE) 

################################################################ #
#Comparaciones de efectos principales (no corresponden en este caso!)
emmeans(m1, pairwise ~ fert) #entre fert 
emmeans(m1, pairwise ~ veg) #entre veg

#Diseños desbalanceados
#genero una muestra desbalanceada a partir del ensayo anterior
bd_desb <- bd[sample(1:nrow(bd), 20, replace=FALSE),]
library(Rmisc)
medias<-aggregate(HC~fert+veg, bd_desb,mean)
(n<-aggregate(HC~fert+veg, bd_desb,length))

# Graficos de perfiles
(medias.bd_desb<-aggregate(HC~fert+veg, bd_desb,mean))
gp <- ggplot(medias.bd_desb, aes(x=fert, y=HC, colour=veg, group=veg))
gp + geom_line(aes(linetype=veg), size=.6) +geom_point(aes(shape=veg), size=3) 

# Analisis de varianza
modelo3<-lm(HC~veg*fert, bd_desb)
anova(modelo3)  #solo tipo I
library(car)
Anova(modelo3, type="III") #paquete car


