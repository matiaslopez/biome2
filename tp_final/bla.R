rm(list = ls())
# Me paro en mi directorio de trabajo
if(dir.exists(file.path("/home", "mlopez"))){
  setwd("/home/mlopez/git/biome2/tp_final/")
}

df <- read.csv("bd_entornos.csv", fileEncoding = "ISO-8859-1") # levanto los datos
str(df) #veo que los nÃºmeros sean nÃºmeros y los str sean factores

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



library(lme4)

df %>%
  mutate(ses_auto_agrupado = as.factor(ifelse(ses_auto<=3, "Baja", ifelse(ses_auto<=7, "Media", "Alta")))) -> df


data <-  na.omit(df)
data$fruta<-as.factor(data$fruta)
levels(data$fruta)[levels(data$fruta) == "sí"]<- "1"
levels(data$fruta)[levels(data$fruta) == "no"]<- "0"

m1<-glm(fruta ~  age, data =data, family = binomial)
summary(m1) 

m2<-glm(fruta ~  sex, data =data, family = binomial)
summary(m2)

m3<-glm(fruta ~  grado, data =data, family = binomial)
summary(m3) 

m4<-glm(fruta ~  parent_ed4, data =data, family = binomial)
summary(m4)

m5<-glm(fruta~ fas3, data=data, family = binomial)
summary(m5)#Solo fas3alta da sig

m6<-glm(fruta~conectiv_i, data=data, family = binomial)
summary(m6)

m7<-glm(fruta~ ses_auto, data=data, family = binomial)
summary(m7)

m8<-glm(fruta~ salud_auto, data=data, family = binomial)
summary(m8) #da sig

m9<-glm(fruta~ publica, data=data, family = binomial)
summary(m9)#da sig publicaFALSE?


d <- data %>% mutate(fruta = as.numeric(fruta)-1)

d %>%
  rowwise() %>%
  mutate(esc_gestion=as.factor(strsplit(as.character(Escuela), "-")[[1]][1]), 
         esc_estrato=as.factor(strsplit(as.character(Escuela), "-")[[1]][2])) %>%
  ungroup() -> d


library(lme4)
m10n <- glmer(fruta ~ fas3 + (1|esc_gestion), family=binomial, data=d)
m10nn <- glmer(fruta ~ fas3 + (1|esc_estrato), family=binomial, data=d)
m10 <- glmer(fruta ~ fas3 + (1|grado), family=binomial, data=d)
summary(m10)
m11 <- glmer(fruta ~ parent_ed4 + (1|grado), family=binomial, data=d)
summary(m11)


mmm1 <-  glm(fruta ~ 1, data=d_temp, family=binomial)
summary(mmm1)

add1(mmm1, fruta ~ Escuela + grado + age + parent_ed4 + fas3 + ses_auto + af_hasta2h + conectiv_i + salud_auto + publica + ses_auto_agrupado, test="Chisq")

m2 <- update(mmm1, .~.+age)

add1(m2, fruta ~ Escuela + grado + parent_ed4 + fas3 + conectiv_i + salud_auto  + ses_auto_agrupado, test="Chisq")

