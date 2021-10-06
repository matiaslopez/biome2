##### TRABAJO PRACTICO 3 EJERCICIO 1: HORMIGAS ##################################

rm(list = ls())

### Carga de datos y estadistica descriptiva. ----------------------------------

## Carga de datos.

setwd("") # Configurar directorio de trabajo.

Datos <- read.delim("Hormigas.txt") # Cargar datos.

# Mirar la tabla.

View(Datos)

# Mirar los nombres de la variables.

names(Datos)

str(Datos) # Observar estructura de la base de datos.

# Para ver el tipo de la variables individualmente.

class(Datos$Tratamiento)
class(Datos$T_ing) 
class(Datos$MTox_ing)

## Estadistica descriptiva.

# Estudiamos la masa de cebo toxico consumido por las hormigas en cada uno
# de los tratamientos.

# Estadistica descriptiva d etoda la base de datos.

summary(Datos)

# Tambien puede usarse la funcion stat.desc() del paquete pastecs.

#pueden cargar la libreria
library("pastecs")

#o, si solo van a usar una funcion, la pueden llamar asi
pastecs::stat.desc(Datos)

# Podemos armar subsets de datos para cada nivel de la variable toxico
# (cc, sac68 y sac30) para pedir medidas descriptivas por separado.

setCC <- subset(Datos, Tratamiento == "CC")
setSAC68 <- subset(Datos, Tratamiento == "SAC68")
setSAC30 <- subset(Datos, Tratamiento == "SAC30")

#una libreria muy utilizada para estas cosas es dplyr
library(dplyr)

setCC1 <- Datos%>%filter(Tratamiento == "CC")

#probemos a ver si hace lo mismo
setCC == setCC1


stat.desc(setCC)
stat.desc(setSAC68)
stat.desc(setSAC30)

# Realizar algunos graficos exploratorios.

plot(Datos$T_ing ~ Datos$Tratamiento,
     main = "Tiempo de ingesta de toxico en funcion del tratamiento",
     ylab = "Tiempo de ingesta",
     xlab = "Tipos de toxicos")

plot(Datos$MTox_ing ~ Datos$Tratamiento,
     main = "Masa de toxico ingerida en funcion del tratamiento",
     ylab = "Masa de toxico ingerida",
     xlab = "Tipos de toxicos")

# Hacer boxplots.

par(mfrow = c(2, 3)) # Permite colocar varios graficos en una misma pantalla.

boxplot(Datos$MTox_ing, main="Mtox_ing")
boxplot(Datos$VSol_ing, main="Vsol_ing")
boxplot(Datos$T_ing, main="T_ing")
boxplot(Datos$T_pau, main="T_pau")
boxplot(Datos$T_tot, main="MT_tot")

par(mfrow = c(1, 1))

### Evaluacion de supuestos. ---------------------------------------------------

# Para evaluar los supuestos primero tenemos que generar el modelo. Esto permite
# calcular los residuos del modelo.

modelo_0 <- lm(MTox_ing ~ Tratamiento, data = Datos) # Generar modelo.

# Calcular los residuos y los predichos del modelo.

e <- residuals(modelo_0) # Residuos.
re <- rstandard(modelo_0) # Residuos estandarizados.
pre <- predict(modelo_0) # Predichos.

# Unir todo en un solo objeto.

res <- data.frame(Datos$Tratamiento, Datos$MTox_ing, pre, e, round(re, 2))

# Darle nombre a las columnas.

colnames(res) <- c("Tratamiento", "MTox_ing", "Predichos", "Residuos",
                   "Residuos std") 

# Observar el contenido del data frame.

head(res)

class(res) # Es un data frame.

## Metodos graficos.

# Grafico de dispersion residuos v. predichos y QQplot.

par(mfrow = c(1, 2))

# Grafico de dispersion.
library(car)
plot(x = pre,
     y = re,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion de RE vs PRED" )

abline(h = 0, lty = 2) # Agregar linea horizontal y = 0.

# QQplot.

qqPlot(e, main = "QQ Plot residuos")

par(mfrow = c(1, 1)) # Restaura a un grafico por pantalla.

## Metodos analiticos.

# Prueba analitica para la homogeneidad de varianzas

car::leveneTest(Datos$MTox_ing, Datos$Tratamiento)

# Piense que hipotesis nula pone a prueba el analisis anterior.

# Prueba analitica para normalidad (sobre los residuos).

shapiro.test(e)

# Que es lo mismo que hacer

shapiro.test(modelo_0$residuals)

# Piense que hipotesis nula pone a prueba el analisis anterior.

### Resultados del modelo. -----------------------------------------------------

## Resultados generales.

# Ver resultados en formato regresion y en formato anova.

summary(modelo_0)

anova(modelo_0)

## Comparaciones multiples.

library('emmeans')

emmeans(modelo_0,pairwise ~ Tratamiento)

confint(emmeans(modelo_0,pairwise ~ Tratamiento))

### Modelado del tiempo de ingesta. --------------------------------------------

## Analisis exploratorio.

library("psych")

describeBy(Datos$T_ing, Datos$Tratamiento)

# Graficar amigablemente.

boxplot(Datos$T_ing ~ Datos$Tratamiento,
        xlab = "Tratamiento",
        ylab = "t de ingesta (s)",
        col = "orange")

# A partir del grafico anterior discuta si el supuesto de homogeneidad de
# varianzas se sostiene para estos datos. 

## Relacion entre varianzas y valores esperados.

# Calcular medias y varianzas, y graficar.

media <- matrix(tapply(Datos$T_ing, Datos$Tratamiento, mean))
varianza <- matrix(tapply(Datos$T_ing, Datos$Tratamiento, var))

# Con ggplot (armo previamente un data frame con medias-varianzas)
DF <- as.data.frame(media)       # DF es el nombre del data.frame. Agrego la media
DF$varianza <- round(varianza,1) # Agrego varianza a DF, yredondeo a 1 decimal

# Genero y agrago la columna Tratamiento (en el orden que corresponde,ver descriptiva)
Tratamiento <- c("CC", "SAC30", "SAC68")  
DF$Tratamiento <- Tratamiento
DF                               # Exploro el data.frame

library(ggplot2)
ggplot(DF, aes(x=media, y=varianza)) +
        theme_bw() +
        geom_point(size=6, shape=1) +
        annotate("text", x=70,  y=2000,   label= "SAC30", size=6) +
        annotate("text", x=350, y=21000, label= "SAC68", size=6) +
        annotate("text", x=400, y=18000, label= "CC", size=6)

## Evaluacion de los supuestos. 

# Obtener modelo, predichos y residuos.

modelo_1 <- lm(T_ing ~ Tratamiento, data = Datos)

# Graficamente.

e <- residuals(modelo_1)
re <- rstandard(modelo_1)
pre <- predict(modelo_1)

par(mfrow = c(1, 2))

# Grafico de dispersion.

plot(x = pre,
     y = re,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion de RE vs PRED")

abline(h = 0)

# QQplot.
qqPlot(e, main = "QQ Plot residuos")

par(mfrow = c(1, 1))

# Analiticamente.

shapiro.test(modelo_1$residuals)

library(car)
leveneTest(Datos$T_ing, Datos$Tratamiento, center = "median")

### Modelado con minimos cuadrados generalizados. ------------------------------

library("nlme") # incluye funcion gls().

modelo_2 <- gls(T_ing ~ Tratamiento, data = Datos)

# Estudiar los supuestos.

# Se puede utilizar la funcion plot() en los objetos que genera la funcion gls()
# para graficar. Indique el tipo de grafico que se obtiene.

plot(modelo_2)

# Calcular de los residuos de pearson.

r2 <- residuals(modelo_2, type = "pearson") # = estandarizados.

# Calcular los valores predichos por el modelo.

pred2 <- fitted(modelo_2)

# Graficar los residuos en funcion de los valores predichos.

plot(x = pred2,
     y = r2,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion de RE vs PRED")

abline(h = 0, lty = 2)

# Graficar un boxplot de los residuos del modelo.

boxplot(r2 ~ Datos$Tratamiento,
        xlab = "Tratamiento",
        ylab = "Residuos estandarizados")

# Graficar un QQplot.
qqPlot(r2, main = "QQ Plot residuos estandarizados")


# Prueba de Levene.

leveneTest(r2, Datos$Tratamiento, center="median")

# Analice los resultados de los graficos diagnosticos
# Analice los resultados de la prueba de Shapiro y Levene
# Explique los resultados en terminos del problema.

### Modelado de varianzas. -----------------------------------------------------

# Para modelar las varianzas utilizaremos la funcion gls().

## Modelo 3: VarIdent(Tratamiento).

modelo_varIdent <- gls(T_ing ~ Tratamiento,
                       weights = varIdent(form = ~1 | Tratamiento),
                       data = Datos)

# Evaluar supuestos.

r3 <- residuals(modelo_varIdent, type="pearson")
pred3 <- fitted(modelo_varIdent)

plot(x = pred3,
     y = r3,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion de RE vs PRED")

abline(h = 0, lty = 2)

# Graficar un boxplot de los residuos del modelo

boxplot(r3 ~ Datos$Tratamiento,
        xlab = "Tratamiento",
        ylab = "Residuos estandarizados")

# Graficar un qqplot.

qqPlot(r3, main = "QQ Plot residuos estandarizados")

# Prueba de Levene.

leveneTest(r3, Datos$Tratamiento, center = "median")

# Discuta que modelo elegiria teniendo en cuenta los supuestos.

# Se pueden comparar modelos, por ejemplo, calculando sus AIC:

AIC(modelo_2, modelo_varIdent)

## Otro modelados de varianza.

# Ahora probaran otros modelados de varianza, comprobaran el cumplimiento de los
# supuestos y decidiran cual es el mejor. Luego, interpreran su salida.

# Modelo 4: "varPower".

modelo_varPower <- gls(T_ing ~ Tratamiento,
                       weights = varPower(),
                       data = Datos)

# Supuestos.

r4 <- residuals(modelo_varPower, type = "pearson")
pred4 <- fitted(modelo_varPower)
plot(x = pred4,
     y = r4,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion de RE vs PRED")

abline(h = 0)

boxplot(r4 ~ Datos$Tratamiento,
        xlab = "Tratamiento",
        ylab = "Residuos estandarizados")

qqPlot(r4, main = "QQ Plot residuos estandarizados")

leveneTest(r4, Datos$Tratamiento, center = "median")

# Comparacion.

AIC(modelo_2, modelo_varIdent, modelo_varPower)

# Modelo 5: "varExp".

modelo_varExp <- gls(T_ing ~ Tratamiento,
                     weights = varExp(),
                     data = Datos)

# Supuestos.

r5 <- residuals(modelo_varExp, type = "pearson")
pred5 <- fitted(modelo_varExp)

plot(x = pred5,
     y = r5,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion de RE vs PRED")

abline(h = 0)

boxplot(r5 ~ Datos$Tratamiento,
        xlab = "Tratamiento",
        ylab = "Residuos estandarizados")

qqPlot(r5, main = "QQ Plot residuos estandarizados")

leveneTest(r5, Datos$Tratamiento, center = "median")

# Seleccion de modelos.

AIC(modelo_2, modelo_varIdent, modelo_varPower, modelo_varExp)

# Decidir cual es el mejor.

# Interpreta las salidas del modelo elegido.

summary(modelo_varPower)
anova(modelo_varPower)

# Indique si se rechaza la H0 del AnOVa.
# Interprete los parametros del modelado de varianza.

# Comparaciones a posteriori.
library(multcompView)
library(emmeans)

multCompTukey <- emmeans(modelo_varPower, pairwise ~ Tratamiento)

summary(multCompTukey)

confint(multCompTukey)

plot(multCompTukey$emmeans, comparisons = TRUE)

CLD(multCompTukey)   # Asigna letras a los tratamientos


# extraemos las medidas resumen 

resumen_modelo <-as.data.frame(multCompTukey$emmeans)

# exploramos el objeto resumen_modelo
resumen_modelo  # emmeans es la media estimada

# Plot
library(ggplot2)
ggplot(resumen_modelo, aes(x=Tratamiento, y=emmean)) +
        labs(x="Tratamiento") + labs(y="Tiempo ingesta [seg]") +
        geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), color="blue", width=0.2)+
        geom_point(shape=1, size=2, color="blue") +
        ggtitle("Comparaciones", "Media ± Error estándar") +
        annotate("text", 
                 x = c("CC","SAC30","SAC68"), 
                 y = c(650,650,650), 
                 label = c("B", "A", "B"))

### FIN ###