# Cargar datos
# En caso de qué no tenga instaladas las librerías, es necesario instalarlas primero. Para esto. 
# 1) Elimine el numeral desde la linea 4 - 7. 2) Corra las lineas 4-7 para que se instalen las librerías
#install.packages("readr")
#install.packages("lmtest")
#install.packages("nortest")
#install.packages("car")
 

# Validación de un modelo de regresión lineal -----------------------------
# Juan Llivisaca - Febrero 2025
# Iniciamos #
data(anscombe) # Datos de U. S. State Public-School Expenditures, librería (datasets)
head(anscombe) # Cuatro variables X-Y que tienen la misma propiedades estadísticas (mean, variance, correlation, regression line, etc.).
attach(anscombe)
m1<-lm(y1~x1)
m2<-lm(y2~x2)
m3<-lm(y3~x3)
m4<-lm(y4~x4)
plot(m1)
plot (y1~x1)
abline(m1)
summary(m1)
summary(m2)
summary(m3)
summary(m4)
#vemos que todos los modelos tienen aprox. el mismo R cuadrado

#Nubes de puntos
par(mfrow=c(2,2))
plot(y1~x1);abline(m1,col=2,lwd=2)
plot(y2~x2);abline(m2,col=2,lwd=2)
plot(y3~x3);abline(m3,col=2,lwd=2)
plot(y4~x4);abline(m4,col=2,lwd=2)
par(mfrow=c(1,1))

# ¿Son lineales todas las relaciones? 

# Propiedades estadísticas de los datos
mean(y1);mean(y2);mean(y3);mean(y4) #todos tienen la misma media
sd(y1);sd(y2);sd(y3);sd(y4) #las desviaciones estándar son similares
cov(x1,y1);cov(x2,y2);cov(x3,y3);cov(x4,y4) #las covarianzas son similares

# Muchas veces el Rcuadrado es muy parecido para variables con 
# medias, varianzas y covarianzas similares. 

#### Modelo no lineal y heterocedastico ####
m<-function(x) {0.2*sin(2*pi*x)}
n<-50
x<-seq(0,1,length=n)
eps<-numeric(n) #este sera el vector de errores que no sera constante, sino creciente
for(i in 1:n) {eps[i]<-rnorm(1,sd=0.1*x[i]^2)}
y<-m(x)+eps
plot(y~x) #vemos que el modelo ¿ Es lineal?
curve(m,from=0,to=1,add=TRUE) #vemos que los datos son heterocedasticos, porque no tienen la misma varianza por la curva de estos datos

# Modelo de regresion lineal
m3<-lm(y~x)
summary(m3)
abline(m3,col=2,lwd=2)

#vemos que el R cuadrado no es muy bajo, y los parámetros son significativos, 
# pero el modelo no cumple linealidad 

# Por lo tanto es válido contrastar los supuestos de la regresión, recuerde 
# vamos a trabajar solo con modelos validados para obtener buenas conclusiones.

#### Problema Producción ####

library(readr)
RunTime = c(195,215,243,162,185,231,234,166,253,196,220,168,207,225,169,215,147,230,208,172) 
RunSize = c(175,189,344,88,114,338,271,173,284,277,337,58,146,277,123,227,63,337,146,68)

production <- data.frame(RunTime,RunSize)
attach(production)

#Figura del diagrama de dispersión, importante para saber si hay una relación lineal
plot(RunTime~RunSize,xlab="Run Size", ylab="Run Time",
     pch=16,main="Diagrama de dispersión, correlación = 0.8545", col="blue",)
abline(lsfit(production$RunSize,production$RunTime), col="red")
cor(RunTime,RunSize)
# Modelo de regresión lineal
m1 <- lm(RunTime~RunSize)
# Informe del modelo m1
summary(m1)
anova(m1)

## Intervalos de confianza para Bo y B1 ##
confint(m1, level= 0.95)

# Esto indica que 2.5 %      97.5 %
#(Intercept) 132.2509062 167.2444999
#RunSize       0.1812107   0.3372755
# Estos intervalos nos indican como pueden variar los datos y nos van a servir para 
# extrapolar 
# Los límites de confianza calculados mediante las expresiones anteriores son válidos 
# únicamente si el modelo cumple con los supuestos. Un riesgo evidente de extrapolar 
# el modelo fuera del rango de datos mediante el cual se ha construido, es que la relación entre las variables deje de ser lineal. 


# Supuestos del modelo ----------------------------------------------------

#Siempre se deben realizar los supuestos con los residuos (preferiblemente los estándarizados)

residuos<- m1$residuals

#### CONTRASTAR LINEALIDAD ####
res<-m1$residuals #guardamos los residuos
plot(res~RunSize);abline(h=0)
#según la grafica no hay patrones, o se puede suponer eso (ruido blanco)
#para hacer test de linealidad se requiere instalar el paquete lmtest
library(lmtest) # Si no tiene la librería, se debe instalar.
?reset
reset(m1) #Ramsey's test de linealidad 
# H0: El modelo es lineal (existe ruido blanco en los residuos)
# H1: lo que significa que existe alguna variable independiente de orden superior al de la lineal que
# influye en la variable dependiente
# #como el p-valor es mayor que 5%, p valor es de 0.5249
# no se rechaza la hipotesis de linealidad

#### Normalidad ####
# Como los datos son menores a 50, se puede utilizar la prueba de Shapiro. Caso contrario se ususará la prueba de Kolmogorov-Smirnov
shapiro.test(residuos)
# H0: Los residuos del modelo siguen una distribución normal
# H1: Los residuos del modelo no siguen una distribución normal 
# (las hipótesis hacen relación a la distribución de los datos, que en este caso nos interesa la Normalidad )
#como el p-valor es mayor que 5%, p valor es de 0.8917
# se puede decir que los residuos siguen una distribución normal

## Otros tests normalidad##
library(nortest)
lillie.test(residuos)
ad.test(residuos)


#### homocedasticidad ####

library(car)
# Prueba de Harrison-McCabe 
?hmctest
hmctest(m1) #Test de homocedasticidad Harrison McCabe: 
# Ho: Hay homocedasticidad, Ha: No hay homocedasticidad

# Una prueba más robusta a las desviaciones de normalidad en los datos (usa la mediana)
# Esta prueba es adecuada para muestras pequeñas y no requiere que los datos sean normalmente distribuidos
# Este test está implementado en la función hmctest del paquete lmtest.

#leveneTest(m1$residuals, production$RunTime) 
## el test de levene mide la homogeneidad de varianzas de una serie de observaciones a lo largo de un factor (que agrupa)
# la prueba de Levene se utiliza para comprobar la hipótesis nula de que las muestras que se van a comparar proceden de una población con la misma varianza
# Se usa más en ANOVA, y toma la media como comparación entre grupos
# H0: Los grupos tienen varianzas iguales
# H1: Los grupos tienen varianzas diferentes

#### CONTRASTES DE INDEPENDENCIA ####
# Test de Durbin Watson
# Es una prueba estadística que se utiliza para detectar la presencia de autocorrelación en los residuos de un análisis de la regresión.
# la autocorrelación es para conocer si los datos cercanos se parecen más que los datos lejanos.
# necesitamos cargar el paquete lmtest
dwtest(m1) # Ho: no existe autocorrelacion de orden 1, es decir son independientes
           # H1: existe autocorrelación de primer orden en los residuos, los residuos no son independientes y están correlacionados 

# test de Ljung-Box (requiere asumir normalidad)
# En resumen, el test de Ljung-Box se utiliza para verificar si los residuos de un modelo de regresión lineal (o de series temporales) exhiben autocorrelación y,
# por lo tanto, si el modelo puede necesitar ajustes adicionales para capturar la estructura temporal de los datos de manera más efectiva. 
# Esto es importante para asegurar que las predicciones del modelo sean válidas y fiables.
# Hipótesis nula (H0): No hay autocorrelación en los residuos del modelo.
# Hipótesis alternativa (H1): Existe autocorrelación en los residuos del modelo.

Box.test(m1$residuals,type="Ljung-Box") #para autocorrelacion de orden 1
# Se tiene un p valor de 0.06557, por lo tanto No se rechaza Ho
Box.test(m1$residuals,lag=2,type="Ljung-Box") #autocorrelacion de orden 2
# Se tiene un p valor de 0.1834, por lo tanto No se rechaza Ho

