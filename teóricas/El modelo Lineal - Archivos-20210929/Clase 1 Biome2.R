#Clase 1 Biome II - modelos lineales

# Modelo de regresión ####

bd <- read.csv("ratones_etanol.csv", sep=";")

#exploramos la base (variables, tipo, n, missing)
summary(bd)

#descriptiva por grupo
library(doBy)
summaryBy(vol~etanol, data=bd, FUN=c(mean, sd, length))

library(dplyr)
bd %>% 
  group_by(etanol) %>%
  select(-individuo) %>% 
  summarise_all(.funs = c(
    n = length, 
    ybar = mean, 
    sd = sd, 
    min = min, 
    max = max
  )
  )

library(ggplot2)
ggplot(bd, aes(x=etanol, y=vol))+
  geom_point()

#ajustamos un modelo lineal
m1<-lm(vol~etanol, bd)

#agregamos recta estimada al grafico de dispersion
ggplot(bd, aes(x=etanol, y=vol))+
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  xlab("Dosis de etanol (g/kg)") + ylab("Volumen cerebral (cm3)")

summary(m1)
anova(m1)
confint(m1)

######################## #
#Modelo de comp de medias ####
bd<-read.csv("ratones_vino.csv")

summary(bd)

#descriptiva
library(doBy)
summaryBy(vol~tratamiento, data=bd, FUN=c(mean, sd, length))

bd %>% 
  group_by(tratamiento) %>%
  select(-raton) %>% 
  summarise_all(.funs = c(
    n = length, 
    ybar = mean, 
    sd = sd, 
    min = min, 
    max = max
  )
  )

library(ggplot2)
ggplot(bd, aes(x=tratamiento, y=vol))+
  geom_boxplot()+ geom_jitter(width = 0.2)

ggplot(bd, aes(x=tratamiento, y=vol)) + ylab("Volumen cerebral (mm3)")+
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), size=1, 
               geom="pointrange", color="darkblue") +
  geom_jitter(alpha=0.3, size=2, position = position_jitter(width = .2))

#estimamos modelo lineal
m2<-lm(vol~tratamiento, bd)
anova(m2)

#Comparaciones de Tukey
library(emmeans)
emmeans(m2, pairwise ~ tratamiento)
confint(emmeans(m2, pairwise ~ tratamiento)) #IC para la magnitud de efecto
plot(emmeans(m2, pairwise ~ tratamiento))

#Gráfico con las estimaciones del modelo
comp1<-emmeans(m2, pairwise ~ tratamiento) #extraemos estadísticos
estimaciones<-as.data.frame(comp1$emmeans)

ggplot(estimaciones, aes(x=tratamiento, y=emmean)) + 
  labs(x="Tratamiento") + labs(y="Volumen cerebral (mm3)") +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), color="darkblue", width=0.1)+ geom_point(shape=15, size=4, color="darkblue") + 
  ggtitle("Comparación de volumen cerebral según tratamiento", "Media (EE)")+ annotate("text", x=c(1,2,3), y=c(2.2, 1.65, 1.85), label = c("A", "B", "C"))

#otra opción:
library(ggeffects)
ggpredict(m2)
plot(ggpredict(m2), add.data = TRUE, grid = TRUE) 

summary(m2)

######################################################## #
# Volviendo al primer experimento y modelando con x como categórica ####
bd <- read.csv("ratones_etanol.csv", sep=";")
m3<-lm(vol~factor(etanol), bd)
anova(m3)

#Comparaciones de Tukey
emmeans(m3, pairwise ~ factor(etanol))


#Grafico final con estimaciones segun modelo
emmeans(m3, pairwise ~ etanol)
confint(comp1) #magnitud del efecto

comp1<-emmeans(m3, pairwise ~ etanol)
estimaciones<-as.data.frame(comp1$emmeans)
ggplot(estimaciones, aes(x=factor(etanol), y=emmean)) + 
  labs(x="Dosis de etanol (g/kg)") + labs(y="Volumen cerebral (mm3)") +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), color="darkblue", width=0.1)+ geom_point(shape=15, size=4, color="darkblue") + 
  ggtitle("Comparación de volumen cerebral según tratamiento", "Media (EE)")+
  annotate("text", x=c(1,2,3), y=c(2.25, 2.25, 2.25), label = c("A", "B", "C"))

ggpredict(m3)
plot(ggpredict(m3), add.data = TRUE, grid = TRUE) 

