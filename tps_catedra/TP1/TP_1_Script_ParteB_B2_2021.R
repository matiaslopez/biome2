
# *** Bienvenidos a Biometria II 2021 *** #
# Este es el script asociado al TP # 1 Parte B


# Los gráficos serán un insumo a lo largo de toda la cursada.
# Necesarios para, por ejemplo, la descripción de una muestra, la interpretación de relaciones entre variables o la presentación de un resultado estadístico. 
# En este script se presentan una serie de gráficos a realizarse con el R base y con el paquete ggplot. 
# Les proponemos explorarlos y familiarizarse con los comandos básicos para lograr distintos tipos de gráficos. 



# Antes que nada veamos si hay algun comando u objeto almacenado en la memoria
ls()
# Si te aparece character(0) es que la memoria esta limpia, si no ejecuta
# la siguiente sentencia para borrar la memoria.
rm(list=ls())
# revisamos...
ls()
# No deberia haber nada en el entorno de trabajo ahora!


# Librerias que vamos a utilizar en este TP
library(ggplot2)      # Graficos con ggplot
library(ggcorrplot)   # Correlaciones con libreria ggcorrplot
library(corrplot)     # Correlaciones con libreria corrplot 
library(gridExtra)    # Visualizacion en paneles de graficos con ggplot

# si no las tiene instaladas debe hacerlo previamente 
# install.packages("ggplot2") 
# install.packages("ggcorrplot")
# etc etc

# Setee el directorio de trabajo (Fijate en el TP 0 como lo hicimos)



########### Importacion de Bases de Datos ############

Datos <- read.delim("Iris.txt") # Solo se abre si la BD esta en el directorio seteado

# opcional: 
# attach(Datos)

# attachear la base permite no indicar el data.frame donde se alojan las variables, pero como vimos puede traer sus problemas
# y hay que estar atentos si lo hacemos


########################################

### R BASE ####

########################################

plot(Datos$LongSepalo, Datos$AnchoSepalo, col=Datos$especie)

pairs(Datos[,2:5], pch=as.numeric(Datos$especie))	

hist(Datos$LongSepalo, ylab="Frecuencia", xlab="Longitud del Sépalo")

plot(Datos$LongSepalo~Datos$especie, main="Longitud del sepalo por especie")

barplot(tapply(Datos$LongSepalo,Datos$especie,mean), main="Longitud del sepalo por especie")


# modificar breaks
hist(Datos$LongSepalo, main="Histograma",ylab="Frecuencia", xlab="Longitud del Sépalo", breaks=7)

plot(density(Datos$LongSepalo), main="Densidad de LongSepalo")

# multiples subplots
# windows() # comando opcional si quiere el grafico en una ventana externa
par(mfrow=c(2,2))
hist(Datos$LongSepalo)
hist(Datos$LongPetalo)
hist(Datos$AnchoSepalo)
hist(Datos$AnchoPetalo)

# boxplot - datos atipicos
par(mfrow=c(1,1)) # seteo nuevamente la ventana
boxplot(Datos[2:5])

# boxplot - comparacion variables entre especies
# windows()
par(mfrow=c(2,2))
plot(Datos$LongSepalo~Datos$especie)
plot(Datos$LongPetalo~Datos$especie)
plot(Datos$AnchoPetalo~Datos$especie)
plot(Datos$AnchoSepalo~Datos$especie)


## almacenamiento de archivos

# png
png("myplot2.png", width= 15, height= 10, units= "cm", res = 90)   # myplot2 nombre del grafico
plot(Datos$LongPetalo,Datos$LongSepalo,
     col=Datos$especie,
     ylab="Longitud del Sépalo (cm)",
     xlab="Longitud del Pétalo (cm)")
dev.off()

# tiff
tiff("myplot2.tiff", width= 15, height= 10, units= "cm", res = 90)
plot(Datos$LongPetalo,Datos$LongSepalo,
     col=Datos$especie,
     ylab="Longitud del Sépalo (cm)",
     xlab="Longitud del Pétalo (cm)")
dev.off()

# pdf
pdf("myplot2.pdf", width= 7, height= 8, paper="special") 
# ancho y alto en pulgadas de la region para graficar
# paper "special", setea el tamaño de la figura al del ancho y largo del grafico
# alternativas paper "a4", "letter" etc
plot(Datos$LongPetalo,Datos$LongSepalo,
     col=Datos$especie,
     ylab="Longitud del Sépalo (cm)",
     xlab="Longitud del Pétalo (cm)")
dev.off()

 
## GRAFICO DESAFIO (Pag 2 guia de TP 1 Parte B)
# ¿Cómo hacer un grafico "presentable"?
windows(10,6)
par(mfrow=c(1,3), mar=c(6,4.1,4.1,2),oma=c(1.5,2,1,1))
boxplot(Datos[2:5], ylab="(cm)",col="gray", notch = TRUE, cex.lab=1.2, cex.axis=1.1, las=3)

title(main="Características florísticas de Iris", outer=TRUE, cex.main=2)

plot(Datos$LongPetalo~Datos$especie, col=c("yellow","orange","green"),xaxt="n", xlab="", ylab="Longitud del Pétalo (cm)", cex.lab=1.2)
legend ("topleft", bty="n",legend = levels(Datos$especie), col = c("yellow","orange","green"), lty=1, lwd=3, cex = 1.3)

hist(Datos$LongPetalo[1:50], main="", col=rgb(1,1,0,1), xlim = c(1,7), ylim=c(0,20), xlab="Longitud del Pétalo (cm)", ylab="Frecuencia", cex.lab=1.2)
box("plot",lty="solid")
par(new=TRUE)
hist(Datos$LongPetalo[51:100], main="", col=rgb(1,0.75,0,1), xlim = c(1,7), ylim=c(0,20), xlab="", ylab="")
par(new=TRUE)
hist(Datos$LongPetalo[101:150], main="", col=rgb(0,1,0,0.8), xlim = c(1,7), ylim=c(0,20), xlab="", ylab="")
legend ("topleft", bty="n",legend = levels(especie), col = c("yellow","orange","green"), lty=1, lwd=3, cex = 1.3)


# Al finalizar:
par(mfrow=c(1,1)) # reinicializa el tipo de ventana 




########################################

### ggplot2 ####

########################################


# Exploremos que ocurre cuando pedimos los siguientes graficos (o partes de...) 
# Siga la ejecusion de estos comendos leyendo la guia de TP

AyL_Sepalo <- ggplot(Datos,aes(LongSepalo,AnchoSepalo))
summary(AyL_Sepalo)
AyL_Sepalo

AyL_Sepalo<- AyL_Sepalo + theme_classic() + labs(x="Longitud del Sepalo (cm)",y="Ancho del Sepalo (cm)") 
AyL_Sepalo

# grafico de dispersion con puntos iguales
AyL_Sepalo_n<- AyL_Sepalo + geom_point(size=2) 
AyL_Sepalo_n

# poniendo color a los puntos
AyL_Sepalo_col<- AyL_Sepalo + geom_point(size=2, aes(color=especie)) + theme(legend.title = element_blank())
AyL_Sepalo_col

# cambiando el color ...
AyL_Sepalo_col + scale_color_manual(values=c("red","green","orange"))

# cambiando los simbolos
AyL_Sepalo_sym<- AyL_Sepalo + geom_point(size=2, aes(shape=especie)) + theme(legend.title = element_blank())
AyL_Sepalo_sym

# separando en grillas
AyL_Sepalo_grid<- AyL_Sepalo + 
  facet_grid(~ especie, scales="free_x") +
  geom_point()
AyL_Sepalo_grid

# mas estetica para las grillas
AyL_Sepalo_grid <- AyL_Sepalo_grid + theme(strip.background = element_rect(fill="lightgreen"))
AyL_Sepalo_grid

# incluyo lineas de tendencia
AyL_Sepalo_grid <- AyL_Sepalo_grid + geom_smooth(method = "lm",se=F, color="red")
AyL_Sepalo_grid


# Guardemos este ultimo grafico
ggsave("sepalo.png", AyL_Sepalo_grid, width=10, height=5)



# boxplot
box <- ggplot(Datos, aes(x=especie, y=AnchoSepalo))+        
  geom_boxplot()+ 
  stat_summary(fun.y=mean, geom="point", shape=19, size=4,color="black")
box

# haciendo mas informativos los boxplot
box2 <- ggplot(Datos, aes(x=especie, y=LongSepalo)) +
  geom_boxplot(aes(color=especie), color="black")+
  theme_bw()+
  geom_jitter(alpha=0.3, size=2,aes(color=especie), position = position_jitter(width = .2))+theme(legend.position="top", legend.text=element_text(size = 14),legend.title = element_text(size=16, face="bold")) +
  ylab("Long Sepalo")+xlab("Especie")
box2


# barplot
# para frecuencias
barplot <- qplot(Datos$especie, geom="bar")
barplot

# histograma
hist <- ggplot(Datos, aes(x=LongSepalo))+
  geom_histogram(binwidth=.5, fill="blue", colour="black")+
  theme_bw()
hist

# agragando informacion
# overlap density and histogram
dens <- ggplot(Datos, aes(x = LongSepalo)) +
  geom_histogram(aes(y = ..density..), 
                 binwidth = 0.2, color = "grey30", fill = "white") +
  geom_density()
dens


# modificar el argumento binwidth =  y extraer conclusiones
ggplot(Datos, aes(x = LongSepalo)) +
  geom_histogram(aes(y = ..density..), 
                 binwidth = 0.9, color = "grey30", fill = "white") +
  geom_density()

ggplot(Datos, aes(x = LongSepalo)) +
  geom_histogram(aes(y = ..density..), 
                 binwidth = 0.09, color = "grey30", fill = "white") +
  geom_density()



# Graficos de dispersion
# Grafico exploratorio con suavizado por defecto (geom_smooth)
grafico1 <- ggplot(Datos, aes(x=LongSepalo, y=AnchoPetalo)) + 
  geom_point(size=3, color="blue", shape=19) +  
  geom_smooth() 
grafico1


# setando geom_smooth
grafico2 <- ggplot(Datos, aes(x=LongSepalo, y=AnchoPetalo)) + 
  geom_point(size=3, color="red", shape=19) +  
  geom_smooth(method=lm, se=T, fullrange=F, size=0.5) 
grafico2


# guardar un grafico
ggsave("AnchoPetaloVsLongSepalo.png", grafico2, width=10, height=5) 
# Para mas opciones o extensiones 
# https://ggplot2.tidyverse.org/reference/ggsave.html




# Multiples histogramas

ggplot(Datos, aes(x=LongPetalo, y=..density.., fill=especie)) +
  labs(x="Longitud del Petalo (cm)",y="densidad (u.a.)")+
  geom_histogram(colour="grey60", size=0.2, binwidth = 0.15, alpha=.65) +
  geom_density(alpha=.6,colour=NA)+
  theme_classic() + theme(legend.title = element_blank()) +
  scale_fill_manual(values=c("red","green","orange")) 

# cambiando estetica
ggplot(Datos, aes(x=LongPetalo, y=..density.., fill=especie)) +
  labs(x="Longitud del Petalo (cm)",y="densidad (u.a.)")+
  geom_histogram(colour="grey60", size=0.2, binwidth = 0.15, alpha=.65) +
  geom_density(alpha=.6,colour=NA)+
  theme_classic() + theme(legend.title = element_blank()) +
  scale_fill_manual(values=c("red","green","orange")) +
  theme(legend.position = c(0.875,0.8))



### Asociacion entre variables 

# Primero hay que armar la matriz de correlacion 
# Correlation matrix
corr_datos <- round(cor(Datos[,2:5]), 1)
print(corr_datos)

# Plot con ggcprrplot
ggcorrplot(corr_datos,  
           type = "lower", 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           outline.color="black",
           ggtheme=theme_bw)

# mas opciones
ggcorrplot(corr_datos,  
           type = "lower", 
           method="square", 
           outline.color="black",
           ggtheme=theme_bw)

# con la libreria corrplot
 
corrplot(corr_datos, 
         type = "upper",
         method = "number")

# otro grafico, con mas opciones
corrplot.mixed(corr_datos, 
               lower.col = "black", 
               number.cex = .7)




# Para guardar/visualizar multiples paneles en ggplot 2
# Ejemplo con 4 graficos

grafico_a <- ggplot(Datos,aes(LongSepalo,AnchoSepalo))+ 
  theme_classic() + 
  labs(x="Longitud del Sepalo (cm)",y="Ancho del Sepalo (cm)") + 
  geom_point(size=2) 
grafico_a

# cambiando el color de los puntos y agragando leyenda
grafico_b <- ggplot(Datos,aes(LongSepalo,AnchoSepalo))+ 
  theme_classic() + 
  labs(x="Longitud del Sepalo (cm)",y="Ancho del Sepalo (cm)") + 
  geom_point(size=2, aes(color=especie)) + theme(legend.title = element_blank())
grafico_b

# cambiando la forma de los puntos  
grafico_c <- ggplot(Datos,aes(LongSepalo,AnchoSepalo))+ 
  theme_classic() + 
  labs(x="Longitud del Sepalo (cm)",y="Ancho del Sepalo (cm)") + 
  scale_color_manual(values=c("red","green","orange"))+ 
  geom_point(size=2, aes(shape=especie)) + theme(legend.title = element_blank())
grafico_c

# idem mas suavizado
grafico_d <- ggplot(Datos,aes(LongSepalo,AnchoSepalo))+ 
  theme_classic() + 
  labs(x="Longitud del Sepalo (cm)",y="Ancho del Sepalo (cm)") + 
  geom_point(size=2, aes(color=especie)) + theme(legend.title = element_blank()) +
  facet_grid(~ especie, scales="free_x") +
  geom_smooth(method = "lm",se=F, color="red")
grafico_d


# funcion grid.arrange

grid.arrange(grafico_a, grafico_b,
             grafico_c, grafico_d, 
             ncol=2, nrow=2)



