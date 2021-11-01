rm(list = ls())
# Me paro en mi directorio de trabajo
if(dir.exists(file.path("/home", "mlopez"))){
  setwd("/home/mlopez/git/biome2/tp_final/")
}

df <- read.csv("bd_entornos.csv", fileEncoding = "ISO-8859-1") # levanto los datos
str(df) #veo que los números sean números y los str sean factores

df %>%
  mutate(publica=as.factor(strtrim(Escuela,4)=="PUBL")) %>%
  mutate(parent_ed4=as.factor(parent_ed4)) %>%
  mutate(grado=as.factor(grado))%>%
  mutate(salud_auto=as.factor(salud_auto))%>%
  mutate(ses_auto=as.factor(ses_auto)) -> df

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

df %>%
  ggplot(aes(x=salud_auto, fill=fruta)) + geom_bar(position = "dodge") +
  ggtitle("Salud auto percibida") + facet_wrap(~fruta, scale="free_y")


df %>%
  ggplot(aes(x=ses_auto, fill=fruta)) + geom_bar(position = "dodge") +
  ggtitle("Salud auto percibida") + facet_wrap(~fruta, scale="free_y")

