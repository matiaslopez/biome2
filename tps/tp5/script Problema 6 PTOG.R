#######################################
# Modelos marginales versus condicionales para VR continuas

# install.packages("readxl")
library(readxl)

PTOG <- read_excel("PTOG.xlsx")
View(PTOG)
str(PTOG)

# Equino y Tratamiento como factor
PTOG$Equino <- factor(PTOG$Equino)
PTOG$Tratamiento <- factor(PTOG$Tratamiento)
PTOG$Tiempo <- factor(PTOG$Tiempo)


# install.packages("dplyr")
library(dplyr)
# lo ordeno para ver todas las observaciones de los equinos
ordenado <- arrange(PTOG, Equino, Tiempo)
head(ordenado, 8)
 
# Medidas repetidas

# Spaguetti plots
library(ggplot2)
p <- ggplot(PTOG, aes(Tiempo, Glucemia, group = Equino))
p + geom_point(aes(color = Equino)) +
    geom_line(aes(color = Equino)) + facet_grid (. ~ Tratamiento)+ labs(x = "Tiempo (minutos)", 
                                                                        y = "Glucemia (mg/dl)")


###############
# VR continua #
###############

#### Modelo condicional usando un factor aleatorio para inducir la estructura de correlación
library(nlme)
m1a <- lme(Glucemia ~ Tratamiento * Tiempo,  random = ~ 1 | Equino, PTOG)
m1a
summary(m1a)

#alternativamente
library(lme4)
m1b <- lmer(Glucemia ~ Tratamiento * Tiempo + (1 | Equino), PTOG)
m1b
summary(m1b)

# calculemos el coeficiente de correlacion intraclase
VarCorr(m1b)
(icc<-22.83^2/(22.83^2+13.5^2))

#### Modelo marginal usando simetría compuesta
m2 <- gls(Glucemia ~ Tratamiento * Tiempo,  correlation = corCompSymm(form = ~ 1 | Equino), PTOG)
m2
summary(m2)

# compare el modelo marginal con simetria compuesta y el condicional
#¿Que puede decir acerca de los coeficientes de los modelos?
# Y acerca de las varianzas? Calcule la varianza total no explicada por cada modelo
#¿Cuál es el equivalente al ICC en el modelo marginal?

(var_tot_condicional<- (22.83^2+13.50^2))
(var_tot_marginal<- 26.52^2) 



#### BONUS ROMPE-CABEZAS
# con lme() podemos incluso combinar estructuras marginales y condicionales. Por ej. podriamos combinar un efecto aleatorio de Equino con una estructura de modelado de varianza segun el Tiempo

m3 <- lme(Glucemia ~ Tratamiento * Tiempo,  random = ~ 1 | Equino, weights= varIdent(form = ~1 | Tiempo) ,PTOG)
m3
summary(m3)
