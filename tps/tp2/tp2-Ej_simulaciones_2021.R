# Antes que nada veamos si hay algun comando u objeto almacenado en la memoria
ls()
# Si te aparece character(0) es que la memoria esta limpia, si no ejecut?
# la siguiente sentencia para borrar la memoria.
rm(list=ls())
# revisamos...
ls()


######   PARTE B: CARACTERIZACION DE LA DISTRIBUCION NORMAL  ##########

# B.1 Distribucion normal y funciones de R

#Comenzaremos con la distribucion normal y la utilizaremos como ejemplo para aprender las funciones de R asociadas al manejo de distribuciones de probabilidad.

# Funciones

#Las funciones que veremos son comunes a todas las distribuciones de probabilidad. Hay cuatro tipos de funciones, que estan determinados por la letra con la que comienzan. Seguido a esta letra se encuentra la abreviatura de la distribucion.

# Distribucion normal: `norm'.
# Distribucion binomial: `binom'.
# Distribucion binomial negativa: `nbinom'.
# Distribucion de Poisson: `pois'.

## dXXX

#La primera funcion que veremos es *dnorm()*. Esta funcion calcula la densidad de probabilidad de la distribucion para un valor particular de la variable, o para un vector de valores. Esto nos permite por ejemplo graficar la distribucion.

#X es el punto en el que se desea calcular la densidad, mean es la media (parametro mu) y sd es el Desvio estandar (parametro sigma).
dnorm(x = 0,
      mean = 0,
      sd = 1)

# El valor obtenido NO es la probabilidad de que la variable tome valor 0. Si queremos graficar la funcion, podrias hacer lo siguiente:

# Creamos un objeto para los valores de la funcion. En caso de dudas, consultar *?seq()*.

(valores_x <- seq(-4, 4, by = 0.5))
(valores_y <- dnorm(x = seq(-4, 4, by = 0.5), mean = 0, sd = 1))

# Graficamos.

plot(x = valores_x,
     y = valores_y,
     main = "Distribucion normal; media = 0, sd = 1")

#Prueben achicar el valor del argumento *by* de la funcion *seq()*.
#Si especificamos que el plot sea *type = "l"*, se creara una linea uniendo los puntos que le proveamos.

plot(x = valores_x,
     y = valores_y,
     type = "l",
     main = "Distribucion normal; media = 0, sd = 1",
     xlab = "X",
     ylab = "Densidad de probabilidad")

#Existen formas mas elegantes de hacer este tipo de graficos...

library("ggplot2")

grafico <- ggplot(data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm,
                args = list("mean" = 0, "sd" = 1),
                geom = "area",
                fill = "skyblue",
                colour = "black",
                lwd = 1.1) +
  ggtitle("Distribucion normal") +
  xlab("X") +
  ylab("Densidad de probabilidad") +
  theme_bw()
grafico

# ...pero se escapan de lo que queremos ver hoy.

## pXXX

#La siguente funcion que veremos es *pnorm()*. Esta funcion devuelve la probabilidad acumulada hasta el valor del argumento *q*. Al tratarse de una distribucion de probabilidad continua, esta probabilidad es la integral de la curva que se genera con *dnorm()*, entre -inf y el valor de 'q'. Si fuese una distribucion discreta, seria la sumatoria.

# Hagamos algunos calculos,

#Calculamos la probabilidad acumulada hasta x = 0.
pnorm(q = 0, mean = 0, sd = 1)
# Probabilidad acumulada dentro de dos desvios estandard centrales.
pnorm(q = 2, mean = 0, sd = 1) - pnorm(q = -2, mean = 0, sd = 1)

## qXXX

#La funcion *qnorm()* permite calcular el valor de X que acumula una dada probabilidad *p*.
# Hagamos algunos calculos,

# Calculamos el valor de X que acumula el 0.5 de probabilidad.
qnorm(p = 0.5, mean = 0, sd = 1)


qnorm(p = pnorm(0), mean = 0, sd = 1)
## rXXX

#La ultima funcion que vamos a ver es *rnorm()*, que genera *n* valores aleatorios, siguiendo las probabilidades establecidas por una dada distribucion normal.

# Generemos 20 valores.

(valoresNormales <- rnorm(n = 20, mean = 0, sd = 1))

#*NOTA: los numeros aleatorios generados por el R son PSEUDOALEATORIOS. La semilla de aleatoriedad puede establecerse mediante la funcion set.seed(). Esto permite la reproduccion de los resultados.*

### B.2 Evaluacion de supuestos y graficos diagnosticos ###

#En esta seccion vamos a practicar como se evaluan los supuestos de las muestras y aprenderemos a observar graficos diagnosticos.

#Veamos que dice la prueba de Shapiro-Wilks sobre la muestra recien generada.

shapiro.test(valoresNormales)

#Ahora vamos a simular cuatro muestras de una misma distribucion normal con n de 25, 50, 100 y 200. A cada muesta le haremos un QQ-plot y realizaremos la prueba de Shapiro-Wilks.

#La funcion *lapply()* toma un vector (en este caso los numeros 25, 50, 100, y 200) y les aplica una funcion, en este caso *rnorm()*. Esto significa que creara cuatro vectores, de largos 25, 50, 100, y 200, y los guardara en una lista. Los otros argumentos de la funcion *rnorm()* tambien se incluyen.

# Creamos los datos.

sampleList <- lapply(X = c(25, 50, 100, 200),
                     FUN = rnorm,
                     mean = 0,
                     sd = 1)

#Utilizando la misma logica, realizo la prueba de Shapiro-Wilks para cada uno de los vectores que obtuve en el paso anterior.

pShapiroWilks <- lapply(sampleList, shapiro.test)

#A continuacion crearemos un QQ-plot para cada vector y mostraremos en el titulo de cada grafico el n utilizado y el p-valor de la prueba.

# Establecemos un arreglo grafico de dos filas y dos columnas.
par(mfrow = c(2, 2))
# Para los valores del 1 al 4 (recordar que tenemos 4 elementos en la lista).
for (i in 1:4) {
  qqnorm(sampleList[[i]],
         main = paste0("n = ",
                       length(sampleList[[i]]),
                       " | Shapiro-Wilks p-valor = ",
                       round(pShapiroWilks[[i]]$p.value, digits = 2)))
  qqline(sampleList[[i]])
}

#Si volvemos a correr la linea que crea el objeto pShapiroWilks, podremos graficar cuatro muestras diferentes.

#Como mencionamos en la guia, la prueba de Shapiro-Wilks es una prueba de hipotesis y utiliza por default un alfa de 0,05. Esto significa que de 100 veces, en 5 casos se rechazaria Ho.

# Vamos a simular 1000 muestras de 20 observaciones con mu = 30 y sd = 10. Realizamos un histograma de los p-valores obtenidos y observamos en cuantos casos se rechazaria la Ho.

#Creamos un dataframe con 2 columnas y 1000 filas.
datos_SW <- data.frame("Simulacion" = vector(mode = "numeric", length = 1000),
                       "p_valor" = vector(mode = "numeric", length = 1000))
# Hacemos un loop que extrae 1000 muestras de n = 20, ejecuta la prueba de Shapiro-wilks y  almacena el p-valor en el dataframe.

for (i in 1:1000) {
  simulacion <- rnorm(20, 30, 10)
  p_valor_SW <- shapiro.test(simulacion)$p.value
  datos_SW[i, ] <- list(i, p_valor_SW)
}

# Inspeccionemos el dataframe.
head(datos_SW)
tail(datos_SW)

# Construyamos el histograma.
par(mfrow = c(1, 1))
hist(datos_SW$p_valor, breaks=4)

# Veamos en cuantos casos se rechaza H0.
datos_SW$conclusion <- ifelse(test = datos_SW$p_valor < 0.05,
                              yes = "Rechazo normalidad",
                              no = "No rechazo normalidad")
table(datos_SW$conclusion)

#En la siguiente seccion graficaremos los QQ-plots de muestras provenientes de una poblacion normal que no superan la prueba de Shapiro-Wilks.

# Generamos muestras de una distribucion normal con p-valores de Shapiro-Wilks < 0,05.

# Simulemos.
muestras <- vector(mode = "list", length = 4)
# Para cada elemento de 'muestras, creo un vector con la muestra y obtengo el p-valor.
# Mientras el p-valor sea >= 0.05 creo un nuevo vector, obtengo el p-valor y vuelvo a evaluar la condicion.
# Cuando la condicion deja de cumplirse guardo la muestra x en el objeto 'muestra'
cant = 0
for (i in seq_along(muestras)) {
  x <- rnorm(n = 100, mean = 0, sd = 1)
  cant = cant + 1
  p_valor <- shapiro.test(x)$p.value
  while (p_valor >= 0.05) {
    cant = cant + 1
    x <- rnorm(n = 100, mean = 0, sd = 1)
    p_valor <- shapiro.test(x)$p.value
  }
  muestras[[i]] <- x
}
cant
# Ahora grafiquemos los QQ-plots de las cuatros muestras.

par(mfrow = c(2, 2))
for (i in seq_along(muestras)) {
  qqnorm(muestras[[i]])
  qqline(muestras[[i]])
}

#Si repiten ambos loops pueden observar diferentes muestras de a cuatro para ajustar un poco el analisis de graficos.

par(mfrow = c(1, 1)) # Restauramos la cantidad de graficos por dispositivo.

### B.3 Simulaciones en regresion ####

# Ahora queremos ver que es lo que pasa con las estimaciones cuando simulamos regresiones normales. Para esto primero vamos a ver como generar artificialmente una regresion lineal simple.

# Simule una regresion lineal simple con tamano de la muestra de 50, $\beta_0$=40, $\beta_1$=-4 y $\sigma$=5. Grafique.

#*Sugerencia: para generar los $\epsilon$ utilice la funcion rnorm. Recuerde que la funcion lm ajusta un modelo lineal*

beta0 = 40
beta1 = -4
sigma = 5
n=50
X<-seq(0, 10, length=n)
e = rnorm(n,0,sigma)
Y = beta0 + beta1*X + e
m1<-lm(Y~X)

# Grafique para insepccionar los datos.

plot(X,Y)
abline(lm(Y~X), col="red")

# Inspeccione el resultado del modelo utilizando la funcion *summary()*

summary(m1)

# Verifique los supuestos de normalidad y homogeneidad de varianzas. Ayuda: utilice la funcion *rstandard* y *predict*.

#normalidad
em1<-rstandard(m1)
qqnorm(em1)
qqline(em1)
#homogeneidad de varianzas
plot(predict(m1),rstandard(m1))
abline(0,0, col="red")

# Repita la simulacion del modelado 1000 veces, extraiga el coeficiente $\beta_1$ en cada simulacion y realice un histograma. Ayuda: para obtener el valor del coeficiente haga *model$coefficients[2]*.

#*Mas ayuda: aca debajo les dejamos una forma de simular varias veces algun proceso que uno desea y lo *guarda* en un data frame.*
sigma = 5
sigma = 50
n=10
n=50
n=1000
data<-data.frame()
for(i in 1:1000) {
  X<-runif(n,0,10)
  e = rnorm(n,0,sigma*X)
  Y = beta0 + beta1*X + e
  model<-lm(Y~X)
  b1<-c(i, model$coefficients[2],summary(model)[[4]][4])
  data<-cbind(rbind(data, b1))
}
colnames(data)<-c("simulacion", "b1","se")

par(mfrow=c(1,2))
hist(data$b1,freq=FALSE, main="bla")
abline(v=beta1,col="red")
hist(data$se,freq=FALSE)
abline(v=mean(data$se),col="blue")

n = 100000
X<-runif(n,0,10)
e = rnorm(n,0,sigma*X)
Y = beta0 + beta1*X + e

par(mfrow=c(1,2))
plot(X,Y, main ="Y=Beta0 + Beta1*X + rnorm(n, 0, sigma*X)")
# ?Que ocurre si aumentamos el error del modelo (varianza residual, no explicada)?.
# Realice el mismo procedimiento que antes pero con $\sigma$=50 y justifique.

# ?Que ocurre si aumentamos el n?.
# Realice el mismo procedimiento que antes pero con n=1000 y justifique.

# Ahora bien, ?Que pasa si el supuesto de homogeneidad de varianzas no se cumple? ?Como se visualiza?

# Simule la misma regresion pero ahora calcule a $\epsilon$ como: $\epsilon$=rnorm(n,0,sigma * X).
# Verifique los supuestos y grafique.

# Simule la misma regresion pero ahora calcule a $\epsilon$ como: $\epsilon$=runif(n,0,10).
# Verifique los supuestos y grafique.
