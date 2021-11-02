rm(list = ls())
# Me paro en mi directorio de trabajo
if(dir.exists(file.path("/home", "mlopez"))){
  setwd("/home/mlopez/git/biome2/tp_final/")
}

df <- read.csv("bd_entornos.csv", fileEncoding = "ISO-8859-1") # levanto los datos
str(df) #veo que los números sean números y los str sean factores

# sub base con les que tienen NA en la fruta
df[is.na(df$fruta),]

# sub base con les que *no* tienen NA en la fruta
df[!is.na(df$fruta),]

#sobrescribo la base
df <- df[!is.na(df$fruta),]

df %>%
  mutate(publica=as.factor(strtrim(Escuela,4)=="PUBL")) %>%
  mutate(parent_ed4=as.factor(parent_ed4)) %>%
  mutate(grado=as.factor(grado))%>%
  mutate(salud_auto=as.factor(salud_auto)) -> df

library(ggplot2)
library(dplyr)


df %>%
  ggplot(aes(x=grado, fill=Escuela)) + geom_bar(position = "dodge")  + ggtitle("Cantidad por grado")
  


df %>%
  ggplot(aes(x=grado, y=age, color=publica)) + geom_jitter() + 
  ggtitle("Edad por grado por tipo de gestión")



df %>%
  ggplot(aes(x=grado, y=age, color=publica)) + geom_jitter() + 
  ggtitle("Edad por grado por tipo de gestión y educación de los padres") + 
  facet_wrap(~parent_ed4)  


df %>%
  ggplot(aes(x=grado, y=age, color=publica)) + geom_jitter() + 
  ggtitle("Edad por grado por SES auto y gestión de la escuela") + 
  facet_wrap(~ses_auto)  



df %>%
  ggplot(aes(x=grado, y=age, color=publica)) + geom_jitter() + 
  ggtitle("Edad por grado por SES auto y gestión de la escuela") + 
  facet_wrap(~ses_auto)  

df %>%
  ggplot(aes(x=grado, y=age, color=ses_auto)) + geom_jitter() + 
  ggtitle("Edad por grado por SES auto y gestión de la escuela") + 
  facet_wrap(~ses_auto)  

df %>%
  ggplot(aes(x=salud_auto)) + geom_bar() + 
  ggtitle("Salud auto percibida")


ggplot(aes(x=salud_auto, fill=fruta), data = df) + geom_bar() + 
  ggtitle("Salud auto percibida")  + facet_wrap(~fruta, scale="free_y")


df %>%
  ggplot(aes(x=salud_auto, fill=fruta)) + geom_bar(position = "dodge") +
  ggtitle("Salud auto percibida") + facet_wrap(~fruta, scale="free_y")


df %>%
  ggplot(aes(x=ses_auto, fill=fruta)) + geom_bar(position = "dodge") +
  ggtitle("Salud auto percibida") + facet_wrap(~fruta)

library(GGally)

df_temp <- df %>% select(fruta, grado, age, sex,
                         parent_ed4, fas3, ses_auto,
                         conectiv_i, salud_auto, 
                         publica
                    )

ggpairs(df_temp)


df %>%
  ggplot(aes(x=Escuela, fill=fruta)) + geom_bar(position = "dodge") +
  ggtitle("")

df %>%
  ggplot(aes(x=Escuela, fill=fruta)) + geom_bar(position="fill") +
  ggtitle("") + ylab("Porcentaje")

df %>%
  ggplot(aes(x=age, fill=fruta)) + geom_bar(position="fill") +
  ggtitle("") + ylab("Porcentaje")


df %>%
  ggplot(aes(x=publica, fill=fruta)) + geom_bar(position = "dodge") +
  ggtitle()


summary(df)

library(lme4)

df %>%
  mutate(ses_auto_agrupado = as.factor(ifelse(ses_auto<=3, "Baja", ifelse(ses_auto<=7, "Media", "Alta")))) -> df


data <-  na.omit(df)

m1 <- glm(fruta_logical ~ fas3 + (1|Escuela), family=binomial, data=data)

summary(m1)
anova(m1)


