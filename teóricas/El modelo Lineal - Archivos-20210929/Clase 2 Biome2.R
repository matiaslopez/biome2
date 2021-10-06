#Clase 2
library(ggplot2)
library(boot)
library(pastecs)

#simulando distribuciones https://en.wikibooks.org/wiki/R_Programming/Probability_Distributions
rpois(n, lambda)
rbinom(n, size, prob)
rnorm(n, mean,sd) 
rgamma(n, shape, scale)
runif(n, min, max)
rbeta(n, shape1, shape2)

#simulamos una normal
x<-rnorm(n = 100, mean = 30, sd = 5)
#Describimos con gráficos.
#Para verlos varios gráficos en una matriz de 2x2:
par(mfrow = c(2, 2))   #esto permite mostrar los gráficos en una matriz de 2x2
par(mfrow = c(1, 1)) #para volver a la normalidad (1 gráfico a la vez)
hist(x)
boxplot(x)
#para ver si ajusta a distribución normal:
qqnorm(x)
qqline(x)
mean(x)
sd(x)

#si queremos que la simulación sea la misma cada vez que corremos el script
# se debe fijar la semilla de aleatorización:
set.seed(198911)

#simulamos una Poisson con distintos parámetros
x<-rpois(n = 1000, lambda = 1) 
g<- rpois(n = 1000, lambda = 4) 
u<-rpois(n = 1000, lambda = 10) 
# los ordenamos en una matriz
v3<-data.frame(x,g,u)  

library(psych)
round(stat.desc(v3),2)
summary(v3)

psych 
sapply(v3, c(mean, median), na.rm=TRUE)

#describimos las distribuciones gráficamente
#si te aparece el siguiente error: figure margins too large, 
# podes agrandar los margenes o usar una ventana:
#windows()

windows()
par(mfrow = c(3, 2))
hist(x)
boxplot(x)
hist(g)
boxplot(g)
hist(u)
boxplot(u)

#simulamos una variable con distr normal, gamma y uniforme
x<-rnorm(n = 1000, mean = 30, sd = 5) 
g<- rgamma(1000, shape=2, scale=3)
u<-runif(1000, min = 0, max = 60)
# los ordenamos en una matriz
v3<-cbind(x,g,u)  

#describimos las distribuciones gráficamente
#si te aparece el siguiente error: figure margins too large, 
# podes agrandar los margenes o usar una ventana:
#windows()

par(mfrow = c(3, 3))
hist(x)
boxplot(x)
qqnorm(x)
qqline(x)

hist(g)
boxplot(g)
qqnorm(g)
qqline(g)

hist(u)
boxplot(u)
qqnorm(u)
qqline(u)

#Bootstrap
#1. remuestrear un conjunto de datos - con reposicion - un número determinado de veces
#2.Calcular un estadístico específico en cada muestra
#3.Hallar el desvío estándar de la distribución de ese estadístico (EE), percentiles, etc

#IC para la mediana
#Se midió la tasa de degradación de hojarasca en 10 puntos de un bosque tropical.
#4.2  0.9  3.9 12.4 20.0  1.5  8.3  8.2  4.9  3.1  
#Se Interesa estimar la mediana

#creamos el vector de datos
deg<-c(4.2,  0.9,  3.9, 12.4, 20.0,  1.5,  8.3,  8.2,  4.9,  3.1,  4.5, 17.3,  8.6, 11.7,  5.6,  3.4,  6.6,  2.7,  7.2, 10.1)
deg
length(deg)
hist(deg)
boxplot(deg)
mediana(deg)
#extraemos 1000 muestras bootstrap con reposición de tamaño n=20 
resamples <- lapply(1:1000, function(i) sample(deg, replace = T))
#miramos la primera de las muestras bootstrap
resamples[1]
#calculamos la mediana para cada muestra bootstrap 
r.median <- sapply(resamples, median)
r.median
#histograma de la distribucion de las medianas 
hist(r.median)
#calculamos el desvío estandar de la distribucion de medianas (EE)
sqrt(var(r.median))

#construimos el IC usando el método del percentil
quantile(r.median,c(0.025,0.975))

#construimos el IC usando el método de aproximacion a la normal
LI<-mean(r.median)-1.96*sqrt(var(r.median))
LS<-mean(r.median)+1.96*sqrt(var(r.median))
LI
LS

#repetir el procedimiento a partir de la linea 77. ¿Se obtienen los mismos resultados?

#Otra opcion: usando paquete boot
library(boot)
#Funcion boot
bootobjeto <- boot(data= , statistic= , R=, ...) where
#statistics es una funcion que genera el estadistico k que será remuestreado
#R es el número de remuestreos 
#La función boot extrae R muestras aleatorias con reposicion 
#y en cada una aplica la función definida en statistic
#y almacena los resultados en bootobjeto

#generamos la funcion que genera el estadistico de interes
#(mediana en nuestro caso, aunque puede ser cq otro o inclusive más de uno)
mediana <- function(x, i) {
  return(median(x[i]))
}
#En cada remuestreo se llama a esta función; x son los datos (siempre los mismos)
# e i es un indice que identifica a cada remuestreo

res<-boot(deg, mediana, R=1000)
res #muestra el valor del estadistico en la muestra original, 
#el sesgo (dif entre la media de las estimaciones y el valor original del estadístico y su DE)
res$t #muestra las R estadísticos obtenidos por remuestreo
plot(res)

#Solicitamos el IC
boot.ci(res)


boot(x,function(x,i) median(x[i])
boot.ci(boot(x,function(x,i) median(x[i]), R=10000))
library (boot)
