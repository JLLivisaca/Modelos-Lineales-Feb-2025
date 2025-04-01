#### REGRESION DE POISSON Y MODELOS LINEALES GENERALIZADOS ####
# Juan Llivisaca - Marzo 2025

#Ejemplo. Datos de atropellos 
#--- Lectura de los datos
library(readr)
datos11=read.csv("GitHub/estadistica-inferencia/Modelos_Lineales/Modelos-Lineales-Feb-2025/RoadKills.csv")
attach(datos11)

#--- Diagrama de dispersi?n
plot(Num_Atropellos~Distancia_Parque,xlab="Distancia al parque",ylab="Atropellos")
#la variable dependiente es el total de atropellos (DEBE SER UN NUMERO ENTERO POSITIVO)
#la variable x es la distancia al parque, vemos que en este modelo no se cumple
#la homocedasticidad

#--- Modelo lineal
mod_lineal=lm(Num_Atropellos~Distancia_Parque)
summary(mod_lineal)
plot(Num_Atropellos~Distancia_Parque,main="Ajuste lineal")
abline(mod_lineal, col ="red")
#vemos que el modelo lineal no se ajusta bien

#--- Modelo de Poisson
mod_poisson=glm(Num_Atropellos~Distancia_Parque,family=poisson(link="log"))
#observe que ahora la familia ahora es poisson, funcion link =log (parecido al modelo logístico binomial)
summary(mod_poisson)
exp(coef(mod_poisson))
#igualmente los coeficientes se deben calcular con exponenciales

exp(1000*coef(mod_poisson))
#windows() use este comando si desea una ventana emergente para su gráfico
plot(Num_Atropellos~Distancia_Parque,main="Ajuste del modelo de Poisson")
beta=coef(mod_poisson)
curve(exp(beta[1]+beta[2]*x),add=TRUE,lwd=2,from=0,to=25000, col="blue")
abline(v=0,lty=2, col = "red")
abline(h=0,lty=2, col = "red")
#vemos que el modelo de poisson se ajusta mejor a los datos.

#--- Intervalos de confianza para los coeficientes basados en Profile Likelihood
int_beta=confint(mod_poisson)
int_expbeta=exp(1000*int_beta)
int_beta
int_expbeta

#--- Diagnosis del modelo de Poisson
#windows() use este comando si desea una ventana emergente para su gráfico
par(mfrow=c(2,2))
plot(mod_poisson)
par(mfrow=c(1,1))

#probamos otros modelos para corregir la sobredesviacion
#--- Modelo Binomial Negativa
library(MASS)
mod_NB=glm.nb(Num_Atropellos~Distancia_Parque,link=log) # glm.nb lo toma para una binomial negativo
#aqui se parametriza el modelo binomal negativo
summary(mod_NB)
par(mfrow=c(2,2))
plot(mod_NB)
par(mfrow=c(1,1))

#se puede comprobar la sobredispersion entre dos modelos con el siguiente paquete
# Comparación de los dos modelos
AIC(mod_poisson, mod_NB)
library(lmtest)
lrtest(mod_poisson,mod_NB)
#se observa que entre ambos modelos, preferimos el modelo binomial negativo
# Un valor menos negativo (alto) de log-verosimilitud indica un mejor ajuste del modelo a los datos.
# Gráfica final

library(ggplot2)

# Predicciones
datos11$pred_poisson <- predict(mod_poisson, type = "response")
datos11$pred_nb <- predict(mod_NB, type = "response")

# Gráfico comparativo
ggplot(datos11, aes(x = Distancia_Parque, y = Num_Atropellos)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = pred_poisson), color = "red", linetype = "dashed", size = 1.2) +
  geom_line(aes(y = pred_nb), color = "blue", size = 1.2) +
  labs(title = "Comparación Poisson vs Binomial Negativa",
       y = "Número de Atropellos", x = "Distancia al Parque")+
  theme_minimal()

