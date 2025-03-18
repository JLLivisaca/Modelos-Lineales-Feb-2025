#### Validación de un modelo de regresión lineal ####
# Juan Llivisaca - Marzo 2025

# Iniciamos ---------------------------------------------------------------
# Paquetes, en caso de qué no tenga instalados, elimine los "#" y corra las líneas 6-9
#install.packages("readr")
#install.packages("car")
#install.packages("MPV")
#install.packages("mixlm")

#### Ejmemplo regresión múltiple: vinos ####
library(readr)
vinos <- read_csv("GitHub/estadistica-inferencia/Modelos_Lineales/Modelos_Lineales/Vinos.csv")
attach(vinos)

# Modelo de Regresión Múltiple Sin interacción
mfull <- lm(Calidad ~ Cosecha+Fin_cosecha + Lluvia)
summary(mfull)
# Modelo regresión considerando interacción
mfull.interaccion <- lm(Calidad ~ Fin_cosecha + Lluvia+ Lluvia:Fin_cosecha)
summary(mfull.interaccion)

# Si deseo graficar los datos relizando segregación por si llovió o no.
# Grafico
vinos$Lluvia <- factor(vinos$Lluvia)
# Crear el gráfico de dispersión con ggplot y añadir líneas de regresión
library(ggplot2)
ggplot(vinos, aes(x = Fin_cosecha, y = Calidad, color = Lluvia, shape = Lluvia)) +
  geom_point(size = 3) +  # Puntos de dispersión
  scale_shape_manual(values = c(16, 17)) +  # Define los símbolos
  labs(title = "Gráfico de dispersión de Calidad vs. Fin_cosecha",
       x = "Fin de cosecha",
       y = "Calidad") +
  theme_minimal() +
  geom_smooth(data = vinos[vinos$Lluvia == 1,], aes(group = 1), method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed", color = "black", size = 1) +  # Línea de regresión para modelo1
  geom_smooth(data = vinos[vinos$Lluvia == 0,], aes(group = 1), method = "lm", formula = y ~ x, se = FALSE, linetype = "solid", color = "blue", size = 1) +  # Línea de regresión para modelo2
  geom_text(aes(label = Lluvia), hjust = 1, vjust = -1) +  # Etiqueta de datos para identificar el color del punto
  geom_text(aes(x = 15, y = 5, label = "Línea de Regresión"), color = "black", size = 3)  # Etiqueta de datos para identificar el color de la línea de regresión


# ¿existirán outliers en el modelo?

# Para conocer esto, se realiza la prueba de Bonferroni
# Prueba de Bonferroni para detectar outliers
library(car)
outlierTest(mfull, cutoff=Inf, n.max=4) #n.max son las interaciones máximas que se visualizarán
influenceIndexPlot(mfull, vars="Bonf", las=1) # Grafica el pvalor de los puntos de la prueba
# Hipótesis nula (H0): No hay outliers en los datos. En otras palabras, todos los valores son consistentes con el modelo de regresión lineal 
# Hipótesis alternativa (H1): Hay al menos un outlier presente en los datos. Esto significa que al menos uno de los puntos de datos está demasiado alejado de la tendencia general del modelo de regresión lineal y puede afectar negativamente la precisión del modelo.
#  La observación ubicada en la línea 15 es la única con un valor-P muy pequeño 
#  y por lo tanto hay evidencias para considerar esa observación como un posible outlier

# Modelo con interacción

# Prueba de bonferroni
outlierTest(mfull.interaccion, cutoff=Inf, n.max=4)
influenceIndexPlot(mfull.interaccion, vars="Bonf", las=1)
#  La observación ubicada en la línea 17, 15 y 32 tienen un valor-P muy pequeño 
#  y por lo tanto hay evidencias para considerar esas observaciones como posibles outliers 
# Considerar el valor 17 como dato inicial para cambiar.

# Distancias de cook 

# Son puntos influyentes las observaciones que presenten Di=4/ n−p−2. 
# n = total de datos, p = variables
# Si una distancia de Cook es mucho mayor que 1, se considera que 
# la observación correspondiente tiene una influencia sustancial en el modelo
cooks.distance(mfull)
cooks.distance(mfull.interaccion)

#Forma grafica
cutoff <- 4/(44-3-2)  # Cota
plot(mfull, which=4, cook.levels=cutoff, las=1)
abline(h=cutoff, lty="dashed", col="blue")

# Distancia de cook para modelo con interacción 
cutoff <- 4/(44-4-2)  # Cota
plot(mfull.interaccion, which=4, cook.levels=cutoff, las=1)
abline(h=cutoff, lty="dashed", col="blue")

#### Elección de variables ####
library(MPV) # Aqui estan los datos, contiene 30 observaciones de diferentes automóviles en cuanto al rendimiento 
datos <- table.b3[-c(23, 25), ] # Eliminando 2 observaciones con NA
modelo <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11,
             data=datos)
library(mixlm)
backward(modelo, alpha=0.05) # se puede usar: backward, forward,stepWise, wideForward 
# El modelo final es 
# y ~ x8 + x10
# por que esye es es el que permitió tener el menor AIC y RSS
stepWiseBack(modelo, alpha.remove = 0.15, alpha.enter = 0.15, full = F)
# El modelo final es 
# y ~ x5 + x8 + x10
# por que esye es es el que permitió tener el menor AIC y RSS


#### Regresión Polinómica ####

# Vamos a considerar la relación entre la concentración de madera dura (%) y la resistencia del papel medida en psi
conc <- c(1, 1.5, 2, 3, 4, 4.5, 5, 5.5, 6, 6.5, 7, 8, 9, 10, 11, 12, 13, 14, 15)
resis <- c(6.3, 11.1, 20, 24, 26.1, 30, 33.8, 34, 38.1, 39.9, 42, 46.1, 53.1, 
           52, 52.5, 48, 42.8, 27.8, 21.9)
datos <- data.frame(Concentración=conc,Resistencia=resis)
#Grafica
library(ggplot2)
ggplot(datos, aes(x=Concentración, y=Resistencia)) + 
  geom_point() + theme_light()

# De este diagrama se ve claramente que hay una relación de tipo no lineal entre las variables. 
# ¿Será mejor un modelo de grado 2 que un modelo de grado 1?

mod1 <- lm(Resistencia ~ Concentración, data=datos) # Modelo grado 1
mod2 <- lm(Resistencia ~ Concentración + I(Concentración^2), data=datos) # Modelo grado 2, la función I(x^2) indica el grado del polinomio
# Graficos 
ggplot(datos, aes(x=Concentración, y=Resistencia)) + 
  geom_point() +
  geom_smooth(method='lm', formula=y~x, se=FALSE, col='dodgerblue1') +
  geom_smooth(method='lm', formula=y~x+I(x^2), se=FALSE, col='tomato') +
  theme_light()

# Resumen modelo 
summary(mod1)
summary(mod2)
anova(mod1)
anova(mod2)
anova(mod1, mod2) # Comparando los residuos de los dos modelos, nos indica que el segundo es el mejor
# Grafica residuos 
par(mfrow=c(1, 2))
plot(mod1, which=1, caption='Modelo lineal')
plot(mod2, which=1, caption='Modelo cuadratico')
par(mfrow=c(1, 1))
# Si aumento el polinomio, ¿qué ocurre?
mod3 <- lm(Resistencia ~ Concentración + poly(Concentración,degree=3), data=datos)
summary(mod3)
# Grafica de los modelos con grado 2 a 5
ggplot(datos, aes(x=Concentración, y=Resistencia)) + 
  geom_point() +
  geom_smooth(method='lm', formula=y~x, se=FALSE, col='dodgerblue1') +
  geom_smooth(method='lm', formula=y~x+I(x^2), se=FALSE, col='tomato') +
  geom_smooth(method='lm', formula=y~x+I(x^2)+I(x^3), se=FALSE, col='gold') +
  geom_smooth(method='lm', formula=y~x+I(x^2)+I(x^4), se=FALSE, col='green') +
  geom_smooth(method='lm', formula=y~x+I(x^2)+I(x^4)+I(x^5), se=FALSE, col='purple')+
  theme_light()

# Cuidado con el ajuste del modelo. 

# Colinealidad en las variables
library(car)
vif(mod2) # Considerar este detalle.

# Resistencia se predice en función de Concentración y Concentración^2. Es decir:

#Resistencia= 𝛽0+𝛽1Concentracion+𝛽2Concentracion^2+𝜀

#VIF = 1/ 1−R^2 
#Valores típicos de VIF:

#VIF < 5 → No hay problemas graves de colinealidad.
#VIF entre 5 y 10 → Hay colinealidad moderada.
#VIF > 10 → Existe una colinealidad alta y puede afectar la interpretación del modelo.

#¿entonces, qué se puede decir de la colinealidad obtenida?

