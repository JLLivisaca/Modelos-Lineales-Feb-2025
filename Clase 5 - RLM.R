#### Validación de un modelo de regresión lineal ####
# Juan Llivisaca - Marzo 2025
# Paqutes, en caso de tener los paquetes instalados, NO CORRA LAS LÍNEAS 4-6
install.packages("MASS")
install.packages("readr")
install.packages("ggplot")
# Iniciamos ---------------------------------------------------------------

# El modelo estadístico en regresión lineal múltiple es una generalización del regresión lineal simple para k covariables.
# El modelo en este caso se puede escribir de dos formas como se muestra a continuación.
# 1) Yi=β0+β1X1i+β2X2i+⋯+βkXki+ϵi,
# 2) μi=β0+β1X1i+β2X2i+⋯+βkXki,

# Ingreso de datos
# Cargar librerías necesarias
library(readr)
nyc <- read_csv("GitHub/estadistica-inferencia/Modelos_Lineales/Modelos_Lineales/nyc.csv")
attach(nyc)

#Modelo de regresión múltiple
m1 <- lm(Price~Food+Decor+Service+East)
summary(m1)

# ¿qué nota del primer modelo?

##Segundo modelo de regresión múltiple
m2 <- lm(Price~Food+Decor+East)
summary(m2)

#¿qué puede comparar del primer y segundo modelo?

# Podríamos decir que: 
# El modelo inicial de regresión es: 

# Price = – 24.02 + 1.54 Food + 1.91 Decor – 0.003 Service + 2.07 East  

# dejaremos la variable Servicio en el modelo, su coeficiente de regresión no sea estadísticamente significativo. 

# La variable Decoración(Decor) es la que más influye en el Precio, ya que su error es el más bajo (0.217005). 
# El coeficiente de la variable "Food" es 1.54. Esto significa que, manteniendo constantes las demás variables, si la variable "Food" aumenta en una unidad, 
#   el precio aumentará en 1.54 unidades.
# Si analizamos Food, solo para mencionar, El coeficiente de la variable "Service" es -0.003. Esto significa que, manteniendo constantes las demás variables, si la variable "Service" aumenta en una unidad, el precio disminuirá en 0.003 unidades.
# Tenga en cuenta que la comida (Food), la decoración y el servicio (Service) se miden en la misma escala de 0 a 30, por lo que resulta útil comparar los coeficientes de regresión. 
# La variable Decoración (Decor) es también la más significativa desde el punto de vista estadístico (Valor de t = 8.802), ya que su valor p es el menor de las tres. 
# Para que el precio alcanzado por la cena sea máximo, el nuevo restaurante 
# debe estar al este de la ubicación EAST (Variable ficticia creada para saber si es útil o no la ubicación), ya que el coeficiente de la variable ficticia es estadísticamente significativo. 
# No parece posible conseguir un cambio en el precio por el servicio, ya que la variable no es significativa.
# Intercept:
# La interpretación de este valor en el contexto del modelo de regresión múltiple es que, si todas las variables explicativas (Food, Decor, Service y East) son iguales a cero, el precio esperado (variable dependiente) sería de -24.02 unidades.
# En un contexto práctico, esto puede no tener una interpretación directa o útil, especialmente si las variables independientes no pueden tomar valores de cero o si no tiene sentido que lo hagan. 
# Por ejemplo, puede que no tenga sentido que todas las variables explicativas sean cero simultáneamente en el contexto del problema modelado.

## ¿qué podríamos decir del segundo modelo) 


# Regresión Múltiple con Variables Cualitativas  --------------------------


# ¿Es posible incluir variables cualitativas?
# La respuesta es si, pero se debe contar con una variable dummy, ficticia o binaria es la denominación genérica 
# para una variable que toma valores de 0 o de 1 y que se utiliza para re-expresar variables cualitativas.

# Ejemplo

#En este ejemplo vamos a utilizar la base de datos de la librería Cars93 del paquete MASS. 
# El objetivo es ajustar el siguiente modelo para explicar el precio del auto en función del tamaño del motor y del tipo de auto, es decir, 
# el objetivo es ajustar un modelo lineal para explicar el precio de un auto.
library (MASS)
data= Cars93
str(Cars93[, c('Price', 'EngineSize', 'Type')])
levels(Cars93$Type)
Cars93$Type <- relevel(Cars93$Type, ref = 'Small')
levels(Cars93$Type)  # Para verificar el cambio

# Grafica de variables

library(ggplot2)
ggplot(Cars93, aes(x=EngineSize, y=Price, color=Type)) + 
  geom_point() + theme_light()

# Modelo de regresión lienal múltiple variables cualitativas
# H0:la variable de la FILA no aporta información para el modelo,
# HA:la variable de la FILA si aporta información para el modelo

mod <- lm(Price ~ EngineSize + Type, data=Cars93)
summary(mod)
anova(mod)
# Se tiene un p-valor de 0.000542, por lo tanto, se concluye que hay evidencias 
# para rechazar H0, es decir, la variable Tipo(Type) si aporta información al modelo
# Muchas veces solo el ver el p-valor con la función summary(), conlleva a que se vean 
# variables cualitativas que no son significativas. Pero esto no siempre es correcto, para 
# corrobar esta afirmación, es usual utilizar la función ANOVA, para comprabar si la variable
# cualitativa tiene o no efecto en el modelo. Las hipótesis planteadas son: 
# H0:la variable CUALITATIVA no influye en la media de Y(variable dependiente), 
# HA:la variable CUALITATICA si influye en la media de Y(variable dependiente) 




