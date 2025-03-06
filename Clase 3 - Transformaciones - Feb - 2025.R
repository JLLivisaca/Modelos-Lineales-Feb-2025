#### Validación de un modelo de regresión lineal ####
# Juan Llivisaca - Marzo 2025

# Iniciamos ---------------------------------------------------------------

# Se está realizando un estudio sobre la producción de madera que se obtiene de cierta especie
# arbórea. Esta especie alcanza la mayor producción en zonas costeras o de baja altitud, mientras
# que la producción es menor en zonas más elevadas. Para constatar este hecho y evaluarlo de
# manera empírica, se registran las producciones de treinta parcelas semejantes situadas a diferentes
# altitudes. Los datos se encuentran disponibles en el ﬁchero "madera.txt".
# En caso de instalar paquetes desde la línea 12 -16 quitar el # y correr el codigo
#install.packages("readr")
#install.packages("lmtest")
#install.packages("nortest")
#install.packages("MASS") 
#install.packages("gridExtra")

# Ingreso de datos

altitud <- c(0, 0, 5, 5, 5, 10, 10, 10, 10, 15, 15, 25, 50, 50, 80, 80, 80, 
             100, 110, 115, 125, 135, 150, 205, 230, 230, 250, 260, 280, 285)
produccion <- c(29.3, 30.3, 28.6, 36.2, 24.5, 28.3, 31.2, 28.5, 31.1, 26.5, 
                26.7, 22.6, 18.5, 23.7, 16.4, 15.3, 14.4, 15.6, 13.4, 13.2, 
                13.8, 9.9, 8.9, 6.0, 4.8, 4.8, 5.1, 4.3, 4.0, 4.0)
madera <- data.frame(altitud, produccion)
attach(madera)
#Modelo de regresión
modelo<-lm(produccion~altitud) # Y = Producción, X=Altitud
summary(modelo)
# Gráfica de dispersión
plot(altitud,produccion, pch=16,
     main="Diagrama de dispersión, Altitud vs Producción", col="blue" )
abline(modelo, col="red")
# se observa que la tendencia es no lineal y que 
# cuando x es bajo, hay mas dispersion de Y en la gráfica

# Ahora ¿cómo están los residuos?
res<-modelo$residuals
plot(res~produccion, pch=16, col="black");abline(h=0, col="red")

# vemos que el grafico de residuos presenta tendencia, como una U
# por tanto podemos intuir que no hay normalidad, 
# y la dispersion de errores no es constante

# Histograma
ggplot(madera, aes(x = produccion)) +
  geom_histogram(fill = "skyblue", color = "black")+
  labs(title = "Histograma de Producción",
       x = "Producción",
       y = "Frecuencia") +
  theme_minimal()
# no existe normalidad
# ¿qué hacemos?

# Vamos a transformar los datos

# DATOS TRANSFORMADOS DEL MODELO: Transformacion Log ----------------------
x = altitud 
y = produccion
yt<-log(y) # Acá transformamos los datos de altitud

#### Modelo de regresión con Variable Altitud transformada ####
modelot<-lm(yt~x) 
summary(modelot) #mejoro el R cuadrado y el RSS
# R2 (modelo sin trandformar)=0.8816
# R2 (modelo transformado)= 0.981
par(mfrow=c(1,2))
plot(yt~x, main="Variable transformada");abline(modelot)#el modelo es ahora LINEAL
plot(y~x, main="Variable sin transformar")
abline(modelo)
par(mfrow=c(1,1))
# veamos los residuos
rest<-modelot$residuals
plot(rest~x, main="Variable transformada");abline(h=0) #vemos que ahora ya no hay tendencia en los residuos
plot(res~x, main="Variable sin transformar");abline(h=0)


# Ejemplo 2 ---------------------------------------------------------------


# DATOS TRANSFORMADOS DEL MODELO: Transformacion Raíz  --------------------

#Ejemplo: Se está analizando la contratación de un servicio de limpieza para ciertas oﬁcinas. 
# El coste del servicio de limpieza depende del número de personas que se contraten. Para poder valo-
# rar el número de personas, se han tomado unos datos sobre servicios realizados en el pasado, en
# los cuales consta el número de habitaciones que se han podido limpiar junto al número de personas
# que realizaron el servicio. Los datos se encuentran en el ﬁchero " cleaning.txt".

# ingreso de datos
Case <- 1:53
Crews <- c(16,10,12,16,16,4,2,4,6,2,12,8,16,2,2,2,6,10,16,16,
           10,6,2,6,10,12,4,4,16,8,10,16,6,10,12,8,10,8,8,2,
           16,8,8,12,10,16,2,2,8,12,4,4,12)

Rooms <- c(51,37,37,46,45,11,6,19,29,14,47,37,60,6,11,10,19,33,46,69,
           41,19,6,27,35,55,15,18,72,22,55,65,26,52,55,33,38,23,38,10,
           65,31,33,47,42,78,6,6,40,39,9,22,41)

datos <- data.frame(case, crews, rooms)

attach(datos)

y<-Rooms # habitaciones
x<-Crews # personas
#Modelo que relaciona el número de cuartos limpiados por las personas que limpian
modelo<-lm(y~x)
summary(modelo)
#Grafica modelo
library(ggplot2)
library(gridExtra) # si no tiene la librería, por favor instalarla.
ggplot(datos, aes(x = x, y = y)) +
  geom_point() +  # Puntos de datos
  geom_smooth(method = "lm", se = FALSE) +  # Línea de regresión
  labs(title = "Modelo Regresión",
       x = "Número de personas",
       y = "Número de habitaciones") + 
  annotate("text", x = Inf, y = 20, label = "Línea de regresión", hjust = 1, vjust = 1, color = "blue")+
  theme_classic()

#X es una variable de conteo y 
# Y tiene dispersion no constante
#cuando se tienen variables de conteo, se debe transformar Y y X
# esto de conteo se ve en la grafica
yt<-sqrt(y)
xt<-sqrt(x)
modelot<-lm(yt~xt)
summary(modelot)
plot(yt~xt, main="Modelo de regresión");abline(modelot, col="blue") 
# hay mejoramiento de R cuadrado y del RSS

#### Graficas para comprobar homocedasticidad
residuos <- residuals(modelo)
# Crea un dataframe con los residuos estandarizados y los valores ajustados
datos_residuos <- data.frame(residuos, valores_ajustados = predict(modelo))
# Crea el gráfico de residuos estandarizados versus valores ajustados
p1=ggplot(datos_residuos, aes(x = valores_ajustados, y = residuos)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Línea horizontal en y = 0
  labs(title = "Gráfico de Residuos vs. Valores Ajustados",
       x = "Valores Ajustados",
       y = "Residuos")
#Modelo Transformado
residuos_t <- residuals(modelot)
# Crea un dataframe con los residuos estandarizados y los valores ajustados
datos_residuos <- data.frame(residuos_t, valores_ajustados = predict(modelot))
# Crea el gráfico de residuos estandarizados versus valores ajustados
p2= ggplot(datos_residuos, aes(x = valores_ajustados, y = residuos_t)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Línea horizontal en y = 0
  labs(title = "Gráfico de Residuos vs. Valores Ajustados",
       x = "Valores Ajustados transformados",
       y = "Residuos transformados")
grid.arrange(p1, p2, ncol = 2)


# DATOS TRANSFORMADOS DEL MODELO: Transformacion Box-Cox ------------------

# Instala y carga el paquete MASS si aún no está instalado
library(MASS)
y<-datos$rooms # habitaciones
x<-datos$crews # personas
# Aplica la transformación Box-Cox, se encuentra el valor óptimo de lambda
transformacion_boxcox <- boxcox(y ~ x, data = datos)
# Se puede ver en la gráfica cuál sería el lambda óptimo
# Muestra el valor óptimo de lambda
lambda_optimo <- transformacion_boxcox$x[which.max(transformacion_boxcox$y)]
print(lambda_optimo)
# lambda óptimo = 0.6666667
# Aplica la transformación Box-Cox con el lambda óptimo
#datos_transformados <- predict(transformacion_boxcox, lambda = lambda_optimo)
datos_transformados <- (datos$rooms^lambda_optimo - 1) / lambda_optimo
yt<-datos_transformados
xt<-datos$crews
modelo_boxcox<-lm(yt~xt)
summary(modelo_boxcox)


