#####################################################
#### Biometr?a 2 - 1er Parcial - 6.10.21 ####
#####################################################

### Nombre y apellido: Matíás Lopez y Rosenfeld

### Turno de Trabajos Pr?cticos: C
#  [ Martin = A / Soledad = B / Jose = C ] 
 
### Correo electr?nico: matiaslopez@gmail.com

### Versi?n de R: R version 3.6.3 (2020-02-29)
#   [ Si no lo sabe, escriba la palabra version en la consola y copie y pegue aqu? la l?nea 'version.string' . Ejemplo: R version 3.6.3 (2020-02-29) ]

### Sistema operativo: Linux - Ubuntu 18.04 LTS
#  [ Linux/Windows/Mac ]




#################################
### Antes de comenzar:
#################################

## Lea atentamente el enunciado y responda lo que se pregunta en el script con comentarios '#'.

## Recuerde que su script debe correr. Esto significa que solo con el script y la base de datos en el mismo directorio el docente que corrija su examen deberá poder ejecutar y visualizar todos los resultados sin necesidad de agregar o eliminar líneas de código. 

## Todo código que escriba debe ser comentado (con #). Esto significa que debe explicitar para qué incluye cada línea o grupo de líneas (según corresponda) de comando (ej. si lo hizo para evaluar un supuesto, para implementar un modelo, para recodificar una variable, para generar un gráfico, etc.).  

## Si toma decisiones en base a algún resultado debe incluir los comentarios pertinentes en el script (ej. eliminación de un dato atípico, cambio de la distribución para modelar, etc.).

## Para aprobar el examen, además de la correcta resolución del ejercicio, tenga en cuenta que:
#	El script debe correr correctamente
#	No deben faltar comentarios en relación a las líneas de código que decida utilizar
#	Debe existir coherencia interna entre comandos y comentarios. 
#	Las conclusiones deben estar fundamentadas


### *****************************
#    El examen es INDIVIDUAL    #  
### *****************************

### *****************************
#   Vaya guardando su Script !!!!
#   Apellido_Turno
#   Ej: "Perez_Turno_A"
### *****************************

.rs.restartR()
rm(list = ls())
# Me paro en mi directorio de trabajo
if(dir.exists(file.path("/home", "mlopez"))){
  setwd("/home/mlopez/git/biome2/primer_parcial/")
}

df <- read.table("datos.txt", header = T) # levanto los datos
names(df) <- c("id", names(df)[2:5]) # elimino la primer columna
str(df) #veo que los números sean números y los str sean factores

#################################
### Enunciado 
#################################

# La contaminación de suelos con metales pesados representa una grave preocupación por sus posibles consecuencias para el medio ambiente y la salud humana. El cromo en particular es un contaminante industrial común y usualmente ocurre en el ambiente en la forma de Cr(VI), que es tóxico y soluble con alta movilidad. Los hongos micorrízicos son hongos que establecen relaciones simbióticas con las plantas, particularmente en sus raíces. Investigaciones recientes sugieren que los hongos micorrízicos aumentan la tolerancia al estrés de las plantas, por lo que podrían protegerlas contra las toxinas ambientales. 

# Se llevó a cabo un estudio a fin de investigar si el hongo micorrízico Beauveria bassiana presenta un efecto protector frente al cromo en el desarrollo de plantas de Arabidopsis thaliana. Para ello, se prepararon 40 cajas conteniendo suelo esterilizado y en cada una se colocaron dos plantas. A cada caja se le asignó en forma aleatoria y balanceada una combinación de dosis de cromo (0, 10, 20 y 40 ppm) y de inóculo de hongo (sí/no). Las plantas crecieron hasta alcanzar la madurez. En cada planta se registró la biomasa total (en gramos) a los 45 días como indicador de desarrollo vegetativo.

# VALOR DE CADA PREGUNTA: 1 punto. 

# Antes de empezar grafico los datos para poder entender lo que estoy buscando modelar

library(dplyr)
library(ggplot2)

df %>% 
  ggplot(aes(x=dosis, y=biomasa, color=hongo)) +
  geom_point() -> p # preparo el gráfico

p  # grafico sin divisiones
p + facet_wrap(~hongo) # grafico separado por hongo
p + facet_grid(hongo~dosis) # grafico separado por dosis y hongo
p + facet_wrap(~caja) # separado por cada (no se ve mucho acá)
p + facet_wrap(~as.factor(caja), scales="free") # separado por caja, escalas libres, caja 2 única que dio lo mismo.


# 1) Identifique la/s variable/s explicatoria/s. Señale si son cualitativas (y niveles) o cuantitativas, de efectos fijos o aleatorios y si los niveles de cada una están cruzados o anidados y con quién.

    # Las variasbles explicatorias son:
    #     - dosis de cromo: cuantitativa, si bien es continua, en la base contamos con valores entre 0 y 40ppm  
    #                       (no podremos predecir fuera de este rango), de efecto fijo 
    #     - inoculación de hongo: cualitativa, 2 valores: sí/no, de efecto fijo 
    #     - caja: efecto aleatorio, si bien es numérica, es cualitativa ya que es un identificador
    # Es un diseño anidado, porque por cada UE tenemos 2 observaciones, la caja está anidada en la inoculación y el cromo.
    # (cada caja tiene un único tratamiento de inoculación y de dosis de cromo)

# 2) Se desea incluir en el modelo a Cromo como variable explicativa cuantitativa. PEro antes ¿cuántos (y cuáles) parámetros estimaría el modelo en caso de haberla incluirla como CUALITATIVA?

    # En caso de querer modelar la dosis de cromo como variable cualitativa, se debería transformar en un factor dicha 
    # columna y esto no traería que tenemos una variable con 4 valores. Para poder trabajar con ella tenemos que 
    # re-codificar la información con variables _dummy_ en las que cada una indica si se tiene o no cierta dosis de cromo.
    # En este caso tenemos 4 niveles de dosis, por lo que podemos tomar uno como referncia (ej. el cero)  y generar 3
    # variables nuevas que sean: esCromo10, esCromo20 y esCromo40. Si bien lm lo hace automaticamente, esto se puede ver con:

df_cromo_cuali <- df # hago una copia para no romper lo que tengo.
df_cromo_cuali["esCromo10"] <- df["dosis"]==10 # dummy de esCromo10
df_cromo_cuali["esCromo20"] <- df["dosis"]==20 # esCromo20
df_cromo_cuali["esCromo40"] <- df["dosis"]==40 # ...
str(df_cromo_cuali)

# 3) Considerando las preguntas (1) y (2), Implemente el modelo lineal en R. De existir datos dependientes, incluir la correlación de manera implícita. 

library(lme4) # cargo lme4
library(lmerTest)
m1 <- lmer(biomasa ~ dosis * hongo + (1|caja), data=df) # modelo 
# m2 <- lmer(biomasa ~ dosis + hongo + (1|caja), data=df) # modelo sin la interacción

# 4) ¿Cuántas prueba de Shapiro realiza? Plantee las Ho que corresponda.

    # en el caso de mi modelo realizaría 2 pruebas de Shapiro, par realizar la prueba de normaliadd sobre los 
    # residuos y sobre el efecto aleatorio.
    # - residuos: H0: los epsilon del modelo siguen ditribución normal con media 0 y sigma^2
    # - parte aletaoria: H0: los B_j del modelo siguen ditribución normal con media 0 y sigma^2_caja

# 5) Realice el diagnóstico de los supuestos. a) Responda específicamente si modelaría la varianza. Justifique por qué SI o por qué NO. b) ¿Se rechaza el supuesto de linealidad? Justifique.

    # Supuestos:
    #   - muestra aleatoria: es un experimento en que los tratamientos fueron administrados al azar [OK]
    #   - linealidad: los paramaros del modelo son lineales [OK]
    #   - normalidad de e y de B_j

e<-resid(m1) # residuos
pre<-predict(m1) # predichos
Bj<-ranef(m1)$caja$'(Intercept)' #efecto aleatrio
par(mfrow = c(1, 3))
plot(pre, e, xlab="Predichos", ylab="Residuos de pearson",
    main="Gráfico de dispersión de RE vs PRED",cex.main=.8 ) # chequeando el supuesto de homocedasticidad
abline(0,0) # agregando un línea en el cero para 
qqnorm(e, cex.main=.8) # qqplot de los residuos
qqline(e)
qqnorm(Bj, cex.main=.8) #qq plot del efecto alatorio
qqline(Bj)
par(mfrow = c(1, 1))

#Pureba analítica de ambos:
shapiro.test(e) # p-value = 0.199
shapiro.test(Bj) # p-value = 0.1825

    #     no tenemos evidencia para rechazar H0, por lo que mantenemos que cada uno sigue una distribución normal 
    #     con su respectivo sigma^2 [OK]
    #   - ausencia de patrones en los residuos: no se observa [OK]


# 6) En base a su modelo final responda ¿Considera que la presencia de hongos micorrizos tiene un efecto protector sobre la tolerancia al Cromo de Arabidopsis? Justifique. Interprete la magnitud de efecto (IC95%) para una dosis de cromo promedio. 

anova(m1)
summary(m1)

    # Hay evidencia para afirmar que la presencia de hongos micorrizos tiene un efecto protector sobre la 
    # tolerancia al Cromo de Arabidopsis. 
    # Esta info se puede ver en:

tab_model(m1, p.val= "kr")
    # Donde tenemos el intervalo de confianza para la relación entre dosis y hongo:
    # la presencia de hongo del hongo ante la dosis tiene una diferencia en la biomasa observada de -0.22, -0.08 (IC95%) gramos


# 7) a) Interprete en contexto la estimación del parámetro beta 2. b) Escriba la ecuación estimada para predecir la biomasa de Arabidopsis en presencia del hongo en función de la dosis de Cromo.   

    # a) Siendo beta_2 el paramatro que respresenta la presencia o ausencia del hongo, lo que nos dice es la diferencia 
    # a la ordenada al origen con respecto a la otra condición. En nuestro caso, beta 2 representa la ausencia. 

    # b) biomasa_gr = beta_0 + beta_1 * dosis + beta_2 * hongo + beta_3 * dosis * hongo
    # como tomamos la presencia del hongo, "hongo" vale 1
m1@beta
#[1] 18.4380 -0.2326 -0.7740 -0.1502
    # biomasa_gr = 18.4380 gr + (-0.2326 gr/ppm) * dosis + (-0.7740 gr) + (-0.1502 gr/ppm) * dosis
    # es decir:
    # biomasa_gr = 17.664 gr -0.3828 gr/ppm dosis

# 8) Calcule el coeficiente de correlación intraclase e interprételo en contexto.

VarCorr(m1)
(icc<-1.64765^2/(1.64765^2+ 0.35373^2))

    # El coeficiente de correlación intraclase CCI mide la correlación entre puntos de la misma caja; 
    # a valore más altos se indica que las mediciones dentro de la misma caja son muy similares, por 
    # lo que la variación viene dada por esto (es nuestro caso 0.95594)


# 9) ¿Si tuviese que repetir el ensayo, mantendría la elección de utilizar dos plantas por caja? Justifique su respuesta utilizando una prueba de hipótesis que permita responder esta pregunta. 

    # Dependiendo de los costos, en este caso estoy realizando submuestras, pero no agregando observaciones.
    # Esto nos explicaca parte de las variaciones observadas, por lo que indirectamente está aumentando la potencia
    # de la prueba. Pero esto también podría lograrse aumentando la cantidad de observaciones realizadas.

# 10) Concluya en función de los objetivos del estudio. Acompañe con un gráfico con las predicciones del modelo y un epígrafe explicativo. 

# Grafico 1
library(effects)

ee <- Effect(c("dosis", "hongo"),m1) 
ggplot(as.data.frame(ee),
       aes(dosis,fit,colour=hongo,fill=hongo))+
  geom_line()+
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper)) +
  ggtitle("Predicciones del modelo con banda de confianza para\nlas dos condiciones de presencia de hongo")  + 
  ylab("Biomasa (gr)") +
  xlab("Dosis de plomo (ppm)") +
  theme_ggeffects()
  
##########################
##########################
##########################

# Cosas que quedaron sin incluir:

# Versión con ggpredict
(a<-ggpredict(m1))
plot(a, add.data = TRUE, grid = TRUE)

# predicciones:
library(sjPlot)
library(sjmisc)

ranef(m1)
plot_model(m1) # 

fitted(m1)  #predicciones parte fija + aleatoria
fixef(m1) #estimacion parametros efectos fijos
X<-model.matrix(m1)
pred_fija<-X %*% fixef(m1)#predicciones parte fija 
pred_fija
pred_df <- as.data.frame(X)
pred_df$predichos <- pred_fija




##########################
##########################
### FIN
##########################
##########################
