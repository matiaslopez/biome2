
#############################
#** Biometría II - 2021   **#
#**       TP Nº 3         **#
#* Modelado de la Varianza *#
#############################

###################################################
## Problema 4.                                    #
## Exposición prenatal a mercurio y actividad     #
##                 de la bomba de calcio          #
###################################################

# Antes que nada veamos si hay algun comando u objeto almacenado en la memoria
ls()
# Si te aparece character(0) es que la memoria esta limpia, si no ejecuta

# la siguiente sentencia para borrar la memoria:
rm(list=ls())

# revisamos...
ls()


## Carga de datos.

setwd("") # Configurar directorio de trabajo.
setwd("~/Biome2_2021/TP_3_2021/TP3")


Datos  <- read.table("pelo.txt")  

names(Datos)
# Si su data.frame no tiene los nombres de las variables, puede utilizar 
# la funcion colnames()

str(Datos) 

# Represente los datos en un diagrama de dispersión. 
# ¿Parece que existe una tendencia lineal en los datos? 
# ¿Directa o inversa? ¿Débil o fuerte?

library(ggplot2)
ggplot(Datos, aes(x = Hg_pelo, y = act_bomba)) + 
      geom_point(aes(), colour = "deepskyblue", size = 2) +
      theme_bw()

# correlación "pearson" (default) 
cor(Datos$Hg_pelo, Datos$act_bomba) # -0.859967


# Estime la relación funcional. Analice los supuestos del modelo.

modelo1 <- lm(act_bomba ~ Hg_pelo, Datos)  

# Analisis supuestos

# Calcular los residuos y los predichos del modelo.

e <- residuals(modelo1) # Residuos.
re <- rstandard(modelo1) # Residuos estandarizados.
pre <- predict(modelo1) # Predichos.

# Unir todo en un solo objeto.

res <- data.frame(Datos$Hg_pelo, Datos$act_bomba, pre, e, round(re, 2))

# Darle nombre a las columnas.

colnames(res) <- c("Hg_pelo", "act_bomba", "Predichos", "Residuos",
                   "Residuos std") 

# Observar el contenido del data frame.

head(res)

## Metodos graficos.

# Grafico de dispersion residuos v. predichos y QQplot.

par(mfrow = c(1, 2))

# Grafico de dispersion.

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

# En este caso no podemos hacer Levene, no hay replicas para los distintos 
#   niveles de x (lo usual en una regresion)

# Prueba analitica para normalidad (sobre los residuos).

shapiro.test(re)



#############################
# Modelamos varianza
#############################


########################
# Modelo2 - varPower  
########################

Modelo2 <- gls(act_bomba ~ Hg_pelo, weights=varPower(), data=Datos) #BASTANTE MAS BONITO
Modelo2

# Analizamos los supuestos.

# Se puede utilizar la funcion plot() en los objetos que genera la funcion gls()
# para graficar. 

plot(Modelo2) # grafico diagnostico residuos estandarizados vs valores predichos

# Calcular de los residuos de pearson.

r2 <- residuals(Modelo2, type = "pearson") # = estandarizados.

# Calcular los valores predichos por el modelo.

pred2 <- fitted(Modelo2)

# Graficar los residuos en funcion de los valores predichos. (mismo grafico que antes)

par(mfrow = c(1, 2))

plot(x = pred2,
     y = r2,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion de RE vs PRED")

abline(h = 0, lty = 2)

# Graficar un QQplot.

qqPlot(r2, main = "QQ Plot residuos estandarizados")

# Shapiro
shapiro.test(r2) # res estandarizados 


########################
# Modelo3 - varExp  
########################

Modelo3<- gls(act_bomba ~ Hg_pelo, weights=varExp(), data=Datos)

# Analizamos los supuestos.
plot(Modelo3)

# Calcular de los residuos de pearson.

r3 <- residuals(Modelo3, type = "pearson") # = estandarizados.

# Calcular los valores predichos por el modelo.

pred3 <- fitted(Modelo3)

# Graficar los residuos en funcion de los valores predichos. (mismo grafico que antes)

par(mfrow = c(1, 2))

plot(x = pred3,
     y = r3,
     xlab = "Predichos",
     ylab = "Residuos estandarizados",
     main = "Grafico de dispersion de RE vs PRED")

abline(h = 0, lty = 2)

# Graficar un QQplot.

qqPlot(r3, main = "QQ Plot residuos estandarizados")

# Shapiro
shapiro.test(r3) # res estandarizados 




########################
# Seleccion de modelos
########################

# Modelos candidatos

AIC(Modelo2, Modelo3)


#########################################
# Interpretacion del modelo seleccionado
#########################################

### Estime la relación funcional. 
summary(Modelo2)


### ¿Existe evidencia significativa de que la actividad basal de la bomba de Ca en el recién 
#    nacido disminuye linealmente con el nivel de mercurio en pelo materno?

# Si, el coef estimado b1 difiere significativamente de 0


### Interprete la pendiente en contexto. ¿Cuáles son sus unidades?

confint(Modelo2) # Hg_pelo:     -467.1662 -346.6415


#    Por cada unidad (??g/g) de Hg de cabello materno en los 3-4 meses previos al parto,
#     se observa un descenso de entre 346.6 y 467.2 glóbulos rojos de sangre en el
#     cordón umbilical del recien nacido (nmol/mg /hr) con una confianza del 95%



### Sobre la base de sus respuestas anteriores  y el diseño de este estudio, 
#   ¿Puede decir que la exposición materna al mercurio medida por depósitos de mercurio en cabello 
#   afecta la actividad de la bomba de Ca en recién nacidos?


# Es un estudio observacional, no se puede establecer causa-efecto. Si se observa una relación (asociación) 


# Presentacion de resultados - Grafico Final

library(ggeffects) # explore en el help la libreria ggeffects
ggpredict(Modelo2)
(a <- ggpredict(Modelo2))
plot(a, add.data = TRUE, grid = TRUE)
