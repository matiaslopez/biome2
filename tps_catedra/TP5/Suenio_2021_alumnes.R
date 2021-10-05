##### HORAS DE SUENO. ###############################################################

##### PREPARACION DEL ENTORNO E IMPORTACION Y EDICION DE DATOS. ---------------------

### Preparacion del entorno.

# De ser necesario, eliminamos los objetos que esten en el ambiente 

rm(list = ls()) 

# Configuramos el directorio de trabajo.

setwd("")


# Cargamos los paquetes que vamos a utilizar.

library("ggplot2")  # Lo utilizaremos para realizar algunos graficos.
library("reshape2") # Utilizaremos la funcion dcast() para pasar de formato long a wide.
library("nlme")     # Utilizaremos la funcion gls(), que permite el modelado de varianza y tambien estructuras de correlacion.

### Carga de datos.

# Importamos los datos del archivo delimitado.

datos <- read.delim("Suenio.txt")

### Manejo de datos.

# Veamos la tabla de datos.

View(datos)

# Ahora veamos lo que nos devuelva la funcion summary().

summary(datos)

# Observen que el R asumio que la variable individuo se trata de una variable cuantitativa, no de un factor. Para resolver esto utilizamos la funcion factor() sobre la variable individuo.

datos$individuo <- factor(datos$individuo)

# Veamos el summary() nuevamente.

summary(datos)

# NOTA: Es importante revisar los datos antes de proceder con su analisis, para saber como esta leyendo el R a cada variable

# Lo siguiente que haremos sera reordenar los niveles de dos factores para que los niveles que vayan al intercept sean los que deseamos. Para esto tambien utilizaremos la funcion factor(). 

# Primer reordenaremos la variable 'Tratamiento' para que el placebo se ubique primero.
levels(datos$tratamiento)

# Utilizamos el argumento 'levels' de la funcion factor() para reordenar los niveles.

datos$tratamiento <- factor(datos$tratamiento,
                            levels = c("placebo", "cafeina"))

# Veamos si dio resultado.

levels(datos$tratamiento)

# Ahora realizamos el mismo procedimiento para la variable 'tiempo'.

levels(datos$tiempo)

# En este caso queremos que el nivel 'inicial' vaya al intercept.

datos$tiempo <- factor(datos$tiempo,
                       levels = c("inicial","dia 1", "dia 2", "dia 3"))

# Confirmamos.

levels(datos$tiempo)

##### EXPLORACION DE DATOS. -------------------------------------------------------

### Graficos descriptivos.

## Box-plot.

boxplot <- ggplot(datos, aes(x = tratamiento, y = t_reac)) +
  geom_boxplot(aes(color=tratamiento), color="black")+
  theme_bw()+
  geom_jitter(alpha=0.3, size=2,aes(color=tratamiento), position = position_jitter(width = .2))+theme(legend.position="right", legend.text=element_text(size = 14),legend.title = element_text(size=12, face="bold")) +
  ylab("T de reaccion")+xlab("Tratamiento")

boxplot

## Spaghetti-plots.

# Los spaghetti-plots permiten observar el comportamiento de una de las variables a
# lo largo de las observaciones, para cada individuo.
ggplot(data = datos, aes(x = tiempo,
                         y = t_reac,
                         colour = factor(individuo),
                         group = individuo)) +
  labs(title = "Spaghetti-plot",
       x = "Tiempo",
       y = "Tiempo de reaccion") +
  geom_point() +
  geom_line() +
  facet_grid(. ~tratamiento ) +
  theme_bw()

## Evolucion de los distintos tratamientos en el tiempo.

# Generamos un grafico que une con lineas las medias de cada 'tiempo', con el color dependiendo del tratamiento.

ggplot(data = datos, aes(x = tiempo,
                         y = t_reac, 
                         fill = tratamiento)) +
  
  geom_smooth(aes(group = tratamiento, colour = tratamiento), se = FALSE) +
  
  labs(title = "Box-plot considerando tratamiento y tiempo",
       x = "Tiempo",
       y = "Tiempo de reaccion") +
  
  theme_bw()

# NOTA: Todos los warnings que surgen luego de crear este grafico se deben a que a ggplot2 no le gusta que se puedan unir facilmente con lineas las medias de distintos niveles de factores. El codigo presentado es una solucion rapida a esta limitacion.       

### Correlaciones entre tiempos.

# Vamos a estudiar la correlacion entre los datos tomados a distintos tiempos. Para hacer esto deberemos utilizar la funcion dcast() del paquete 'reshape2'.

# El argumento 'formula' especifica que una individuo y tratamiento en funcion de la variable tiempo. 'value.var' especifica el nombre de la columna de la cual se toman los valores de la variable respuesta.

?dcast # Para saber mas

datos_wide <- dcast(datos,
                    formula = individuo + tratamiento ~ tiempo,
                    value.var = "t_reac")
datos_wide

# Nos vamos a quedar con las columnas que tienen informacion sobre el tiempo de
# respuesta para facilitar el calculo de las correlaciones.

datos_correlaciones <- datos_wide[,3:6]

# Grafiquemos para buscar relaciones entre las nuevas variables.

plot(datos_correlaciones)

# coeficientes de correlacion
coef_corr <- round(cor(datos_correlaciones), 2)
coef_corr

# otro grafico
library(corrplot)
corrplot(coef_corr, 
         type = "upper",
         method = "number")

# Y si queremos ver la matriz de covarianza (redondeando los valores a dos digitos decimales).
round(cov(datos_correlaciones), digits = 2) 


##### MODELADO. ---------------------------------------------------------------------

# En primer lugar, crearemos una nueva base de datos utilizando la funcion melt() del paquete 'reshape2' en el dataframe en formato wide para tener al tiempo inicial como una covariable.   #### ¿Por qué hacemos este paso? 

## Creacion de la base de datos.
## Argumentos de la funcion melt().
# 'id.vars' = las variables que quiero utilizar para definir cada fila de la matriz.
# 'variable.name' = el nombre del factor que se creara. Este contendra en sus niveles los nombres de las variables que se funden (= melt). En este caso nos devolvera el 'tiempo', porque estamos haciendo el camino inverso al dcast() que hicimos al comienzo.

# value.name = el nombre de la variable que almacena los valores. En este caso sera el tiempo de reaccion.

?melt # Para saber mas

## Creacion de la nueva base de datos.

datos_con_inicial <- melt(datos_wide,
                          id.vars = c("individuo", "tratamiento", "inicial"),
                          variable.name = "tiempo",
                          value.name = "t_reac")

# exploremos las caracteristicas del nuevo data frame
dim(datos_con_inicial)
head(datos_con_inicial)
tail(datos_con_inicial)
summary(datos_con_inicial)

# como sacamos el t_inicial del factor tiempo, revisemos como quedaron las correlaciones 
datos_wide <- dcast(datos_con_inicial,
                    formula = individuo + tratamiento ~ tiempo,
                    value.var = "t_reac")
datos_correlaciones <- datos_wide[,3:5]
coef_corr <- round(cor(datos_correlaciones), 2)
corrplot(coef_corr, 
         type = "upper",
         method = "number")

#### Generemos modelos con distintas estructuras de correlacion y chequeemos los supuestos

# Simetria compuesta. Varianzas iguales (recordar que la funcion lme() se puede utilizar aqui tambien).

modelo_corCompSymm <- gls(t_reac ~ tratamiento * tiempo + inicial,
                 correlation = corCompSymm(form = ~ 1 | individuo),
                 data = datos_con_inicial)

# Autocorrelacion de orden 1. Varianzas iguales.
modelo_AR1 <- gls(t_reac ~ tratamiento * tiempo + inicial,
                 correlation = corAR1(form = ~ 1 | individuo),
                 data = datos_con_inicial)

# Modelo 3: Matriz de correlacion general.
modelo_corSymm <- gls(t_reac ~ tratamiento * tiempo + inicial,
                correlation = corSymm(form = ~ 1 | individuo),
                data = datos_con_inicial)

##### COMPROBACION DE SUPUESTOS. ----------------------------------------------------

# En esta seccion evaluaremos si se cumplen los supuestos de normalidad  y de homogeneidad de varianzas (errores con distribucion normal con media cero y variaza homogenea). Para hacer esto en una forma mas simple crearemos una funcion que realice los graficos y las pruebas que hacemos habitualmente, en un solo paso.

### Creacion de la funcion.

# nuestra.funcion <- function (argumento1) {
#
#   Lo que quiero que haga la funcion.
# 
# }

# Vamos a llamarla supuestos() y va a tener un unico argumento, que es el modelo que queremos evaluar. Este argumento se llamara 'modelo'. 
library(car)
supuestos <- function (modelo) {
  
  residuos <- resid(modelo, type = "pearson")
  predichos <- predict(modelo)
  
  par(mfrow = c(1, 2))
  
  plot(x = predichos,
       y = residuos,
       ylim = c(-4, 4),
       xlab = "Predichos",
       ylab = "Residuos de Pearson",
       main = "Grafico de dispersion de residuos v. predichos", 
       cex.main = 0.8 )
  
  abline(h = c(-2, 2, 0),
         col = c("red", "red", "black"),
         lty = c(2, 2, 1))
  
  qqPlot(residuos)
  
  shapiro.test(residuos)
  
}

# Vamos a probarla con los modelos generados!

supuestos(modelo_corCompSymm)
supuestos(modelo_AR1)
supuestos(modelo_corSymm)

# Que podemos concluir respecto a los supuestos en los distintos modelos generados?


##### ANALISIS DE MODELOS. ----------------------------------------------------------

# solo para fines didacticos, veamos el summary de los 3 modelos para comparar las estructuras de correlacion obtenidas
summary(modelo_corCompSymm)
summary(modelo_AR1)
summary(modelo_corSymm)

### En base al analisis de los supuestos decida los modelos candidatos

AIC(modelo_corCompSymm,modelo_AR1,modelo_corSymm)

# Seleccione el modelo mas apropiado
modelo_seleccionado <-
# por que lo eligio?
  

### Interpretacion del modelo seleccionado.

# Una vez seleccionado el modelo, utilizar las funciones anova() y summary() para
# interpretar los resultados del modelo y realizar las conclusiones.



### Comparaciones.

# Para realizar comparaciones multiples utilice el paquete emmean
# realice las comparaciones necesarias en funcion de los resultados obtenidos y la pregunta de interes
library(emmeans)



################ Predicciones
library("margins")
model_p<-prediction(modelo_AR1, at = list(inicial=rep(mean(datos_con_inicial$inicial),78)))
ggplot(data = model_p, aes(x = tiempo,
                                     y = fitted,
                                     colour = tratamiento,
                                     group = tratamiento)) +
  labs(title = "Modelo marginal predicho con t_inicial fijo (valor medio: 6.58seg)",
       x = "",
       y = "Tiempo de reaccion (seg)") +
  geom_point() +
  geom_line() +
  theme_bw()

### FIN ###
