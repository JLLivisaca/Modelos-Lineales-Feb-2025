#### Regresión múltiple ####
# Juan Llivisaca - Marzo 2025

# Iniciamos ---------------------------------------------------------------

#DIAGNOSIS DE DATOS ATIPICOS O INFLUYENTES
set.seed(123456)
x<-1:20
y<-1+0.5*x+rnorm(20,mean=0,sd=1)
plot(y~x,xlim=c(0,30),ylim=c(0,20))
m1<-lm(y~x)
abline(m1,lwd=2, col="red")

#CASO 1: anadir un punto en el centro de gravedad de los datos
x0<-mean(x)#media de x
y0<-mean(y)#media de y
xa<-c(x,x0)#vector ampliado de x
ya<-c(y,y0)#vector ampliado de y
ma1<-lm(ya~xa)
summary(ma1)

# En la grafica
plot(y~x,xlim=c(0,30),ylim=c(0,20))#puntos originales
abline(m1,lwd=2) #ajuste inicial
points(x0,y0,pch=16,col=2) #anadimos el nuevo punto
abline(ma1,lwd=2, col=2)#anadimos nuevo ajuste
summary(m1)
summary(ma1)
#las estimaciones se mantienen pero,
#los valores de la varianza y los errores tipicos se reducen
# el segundo modelo es mejor

#CASO 2: anadir un punto con x=mean, y= valor bajo
x0<-mean(x)#media de x
y0<-0.25#valor muy bajo
xa<-c(x,x0)#vector ampliado de x
ya<-c(y,y0)#vector ampliado de y
ma2<-lm(ya~xa)

# En la grafica
plot(y~x,xlim=c(0,30),ylim=c(0,20))#puntos originales
abline(m1,lwd=5) #ajuste inicial
points(x0,y0,pch=16,col=2) #anadimos el nuevo punto
abline(ma2,lwd=2, col=2)#anadimos nuevo ajuste
#vemos que la pendiente no se ve afectada, porque el punto 
#esta en la media de X
#este valor es ATIPICO pero no es influyente

#CASO 3: anadir un punto "en el modelo" pero lejos de los datos
x0<-30 # es un valor muy alto en x
y0<-predict(m1,data.frame(x=x0)) #Y es atipico, pero que responde al modelo
xa<-c(x,x0)#vector ampliado de x
ya<-c(y,y0)#vector ampliado de y
ma3<-lm(ya~xa)
# En la grafica
plot(y~x,xlim=c(0,30),ylim=c(0,20))#puntos originales
abline(m1,lwd=2) #ajuste inicial
points(x0,y0,pch=16,col=2) #anadimos el nuevo punto
abline(ma3,lwd=2, col=2)#anadimos nuevo ajuste
# la recta no cambia, porque aunque es dato atipico, 
# mantiene la tendencia

#CASO 4: anadir un punto TEMIBLE
x0<-30 # es un valor muy alto en x
y0<-5 #Y es un valor MUY bajo en Y
xa<-c(x,x0)#vector ampliado de x
ya<-c(y,y0)#vector ampliado de y
ma4<-lm(ya~xa)
# En la grafica
plot(y~x,xlim=c(0,30),ylim=c(0,20))#puntos originales
abline(m1,lwd=2) #ajuste inicial
points(x0,y0,pch=16,col=2) #anadimos el nuevo punto
abline(ma4,lwd=2, col=2)#anadimos nuevo ajuste
#vemos que la pendiente SI se ve afectada, 
# porque el punto esta lejos de la media de X
#este valor es ATIPICO y ES INFLUYENTE

#CALCULO DE LEVERAGE
n<-21
4/n # este es el nivel maximo de leverage
#calculamos los leverage de los datos
hat(model.matrix(ma2))#todos son menores a 4/n, no hay datos preocupantes
hat(model.matrix(ma3))#el ultimo es alto
hat(model.matrix(ma4))#el ultimo es alto e igual al anterior porque el x es el mismo
#OJO el leverge solo depende de X y de la media de X

#CALCULO DE RESIDUOS ESTANDARIZADOS
residuals(ma2) #residuos normales
rstandard(ma2) #residuos estandarizados (con estos se debn hacer las pruebas de validez) 
#Un dato es atipico si los resid estandarizados salen de [-2,2]
rstudent(ma2) #residuos estudentizados.. son mejores para detectar atipicos. 
hist(rstudent(ma2)) #son atipicos si estan fuera de[-2,2]

#DISTANCIAS DE COOK
# Las distancias de Cook son una medida utilizada en el análisis de regresión 
# para evaluar la influencia de cada observación en los resultados del modelo.
# Estas distancias miden cómo cambian los parámetros del modelo de regresión 
# cuando se excluye una observación particular del conjunto de datos.
# En otras palabras, las distancias de Cook indican cuánto cambian las estimaciones 
# de los coeficientes de regresión y otras métricas del modelo (como el coeficiente de determinación) 
# cuando una observación individual se excluye del análisis. 
# Una distancia de Cook grande para una observación sugiere que esa observación tiene una influencia significativa en el modelo.
plot(ma2) #ahi salen los graficos dando click paso a paso
plot(ma4)
# Las distancias de Cook se utilizan para identificar observaciones atípicas o influentes 
# en un modelo de regresión. Si una distancia de Cook es mucho mayor que 1, se considera que 
# la observación correspondiente tiene una influencia sustancial en el modelo
cooks.distance(ma4)# el ultimo valor es de 4.734 (muy alto)


