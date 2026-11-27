library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(mctest)
library(car)
library(GGally)
library(nortest)
library(lmtest)
install.packages("zoo")

imdb_datos_completos  <- read_csv("imdb_datos_completos.csv")
View(imdb_datos_completos)

colnames(imdb_datos_completos)

datos_bien <- imdb_datos_completos |>
  mutate(PrimerGenero = word(genres, 1, sep = ","),
         #Añade una nueva columna para usar
         #solo el primer género de la pelicula, pues con tres se forman muchas cátegorias
         Edad = 2026 - startYear) |> filter(if_all(everything(), ~ !is.na(.x)))
View(datos_bien)

ggpairs(datos_bien[, c("popularity", "runtimeMinutes", "averageRating", "numVotes",
                       "presupuesto", "startYear", "taquilla")])

ggpairs(datos_bien[, c("popularity", "runtimeMinutes", "averageRating", "numVotes",
                       "presupuesto", "Edad", "taquilla")])

ggplot(data = datos_bien, aes(x = presupuesto, y = taquilla)) + geom_point() +
  geom_smooth(method = "lm")#parece haber relación

ggplot(data = datos_bien, aes(x = averageRating, y = taquilla)) + geom_point() +
  geom_smooth(method = "lm")#no parece haber relación

ggplot(data = datos_bien, aes(x = runtimeMinutes, y = taquilla)) + geom_point() +
  geom_smooth(method = "lm")#no parece haber relación

ggplot(data = datos_bien, aes(x = popularity, y = taquilla)) + geom_point() +
  geom_smooth(method = "lm")#parece haber relación

ggplot(data = datos_bien, aes(x = Edad, y = taquilla)) + geom_point() +
  geom_smooth(method = "lm")

ggplot(data = datos_bien, aes(x = exp(Edad), y = taquilla)) + geom_point() +
  geom_smooth(method = "lm") # A causa de la existencia de peliculas con recaudo
# en taquilla muy cercano a 0, se pierde la información relevante de la variable

ggplot(data = datos_bien, aes(x = PrimerGenero, y = taquilla)) + geom_point()
ggplot(data = datos_bien, aes(x = PrimerGenero, y = taquilla)) + geom_boxplot()

attach(datos_bien)

modelo1 <- lm(taquilla ~ presupuesto + runtimeMinutes + popularity)
summary(modelo1) #Modelo inicial

modelo1_alterno <- lm(taquilla ~ presupuesto + popularity + PrimerGenero)
summary(modelo1_alterno) #Viendo el modelo parece que los géneros de las películas no
#ayudan a explicar el dinero conseguido en taquilla

ggplot(data = datos_bien, aes(y = modelo1$residuals, x = modelo1$fitted.values)) +
  geom_point() # Parece ser que los datos no son homocedasticos

ggplot(data = datos_m1, aes(y = residuales1, x = numVotes)) + geom_point() +
  geom_smooth(method = "lm") #Parece haber relación

modelo2 <- update(modelo1, . ~ . - runtimeMinutes + numVotes)
summary(modelo2) #runtimeMinutes aparece como no significativo en el modelo1,
# entonces lo quité. Pero ahora con numVotes y popularity parecen ser colineales

##Revisión multicolinealidad
omcdiag(modelo2)
imcdiag(modelo2)
car::vif(modelo2)

modelo2_alternativo <- update(modelo1, .~. -runtimeMinutes)
summary(modelo2_alternativo)

datos_m2 <- datos_m1 |> mutate(residuales2Alternos = modelo2_alternativo$residuals)

ggplot(data = datos_bien, aes(y = modelo2_alternativo$residuals, x = startYear*presupuesto)) + geom_point() +
  geom_smooth(method = "lm") #la línea parece horizontal, no parece haber relación
ggplot(data = datos_bien, aes(y = modelo2_alternativo$residuals, x = startYear)) + geom_point() +
  geom_smooth(method = "lm") #la línea parece horizontal, no parece haber relación

modelo3 <- update(modelo2_alternativo, . ~ . + startYear:presupuesto)
summary(modelo3)

##Revisión multicolinealidad
omcdiag(modelo3)
imcdiag(modelo3)
car::vif(modelo3)

AIC(modelo2_alternativo)
AIC(modelo3) #Mejor pero no estoy del todo seguro por el análisis visual

ggplot(data = datos_bien, aes(y = modelo3$residuals, x = startYear)) + geom_point() +
  geom_smooth(method = "lm")

modelo3_alterno <- update(modelo3, .~. +startYear)
summary(modelo3_alterno)

##Revisión multicolinealidad
omcdiag(modelo3_alterno)
imcdiag(modelo3_alterno)
car::vif(modelo3_alterno)
#presupuesto y presupuesto:startYear son colineales, entonces usaré el modelo3

modelo4 <- update(modelo3, . ~ . + averageRating)
summary(modelo4) #Va mejorando (estoy mirando el R cuadrado ajustado, nada más)

ggplot(data = datos_bien, aes(y = modelo4$residuals, x = averageRating*numVotes)) +
  geom_point() + geom_smooth(method = "lm")

modelo5 <- update(modelo3, . ~ . - presupuesto:startYear + startYear + averageRating)
summary(modelo5) #Quería ver qué pasaba si usaba solo el starYear,
#pues las gráficas con los residuales parecían iguales y
#resultó que es mejor la interacción, pero por lo que dijo la profe mejor quitarlo


ggplot(data = datos_bien, aes(x = modelo5$fitted.values, y = modelo5$residuals)) +
  geom_point() # Otra vez se ve la heterocedasticidad

modelo6 <- update(modelo5, .~. - startYear)
summary(modelo6)

##Revisión multicolinealidad
omcdiag(modelo6)
imcdiag(modelo6)
car::vif(modelo6)

##Modelo optimizado por AIC
modelo_full <- lm(taquilla ~ (runtimeMinutes + averageRating + numVotes + presupuesto +
                                startYear + popularity + PrimerGenero)^2)
summary(modelo_full)

modelo_full2 <- lm(taquilla ~ (runtimeMinutes + averageRating + numVotes + presupuesto +
                                 startYear + popularity)^2)
summary(modelo_full2)

modelo_optimizado <- step(modelo_full)
summary(modelo_optimizado)

modelo_optimizado2 <- step(modelo_full2)
summary(modelo_optimizado2)

# Verificación AIC

AIC(modelo6)
AIC(modelo_optimizado)
AIC(modelo_optimizado2)

### Análisis residual
## Normalidad residuales
# Gráfico

qqnorm(modelo_optimizado2$residuals)
qqline(modelo_optimizado2$residuals)

# Tests de normalidad

shapiro.test(modelo_optimizado2$residuals)
lillie.test(modelo_optimizado2$residuals)
ad.test(modelo_optimizado2$residuals)

## Homocedasticidad

plot(modelo_optimizado2, which=3, col=c("blue")) # Claramente no se cumple la homocedasticidad

bptest(modelo_optimizado2)
ncvTest(modelo_optimizado2)

## Datos atípicos del modelo

plot(modelo_optimizado2, which=5, col=c("blue"))

datos_bien[456,]
datos_bien[425,]
datos_bien[769,] # Este grupo de datos ayuda a ver más claramente que la capacidad
# explicativa de la transformación "exp(Edad)" no se vé afectada
# por valores atípicos

## Media cero
ggplot(data = datos_bien, aes(x = modelo_optimizado2$fitted.values, y = modelo_optimizado2$residuals)) +
  geom_point() # La gráfica muestra a los datos al rededor de cero

## Autocorrelación
# Residuales vs ajustados

plot(modelo_optimizado2, which=1, col=c("blue")) # No parece haber un patron claro, a parte de la clara heterocedasticidad

dwtest(modelo_optimizado2) # No hay autocorrelación para significancias mayores o iguales a 5%

## Predicciones de taquilla e intervalos de confianza

nuevas_peliculas <- data.frame(
  runtimeMinutes = c(100, 120, 140, 160),
  averageRating = c(5.5, 6.5, 7.5, 8.5),
  numVotes = c(50000, 150000, 500000, 1000000),
  presupuesto = c(10000000, 30000000, 80000000, 150000000),
  startYear = c(2010, 2015, 2020, 2025),
  popularity = c(10, 30, 70, 120)
)

intervalos_confianza <- predict(modelo_optimizado2,
                                newdata = nuevas_peliculas,
                                interval = "confidence",
                                level = 0.95)

intervalos_prediccion <- predict(modelo_optimizado2,
                                 newdata = nuevas_peliculas,
                                 interval = "prediction",
                                 level = 0.95)

nuevas_peliculas$taquilla_predicha <- intervalos_confianza[, "fit"]
nuevas_peliculas$limite_inferior_confianza <- intervalos_confianza[, "lwr"]
nuevas_peliculas$limite_superior_confianza <- intervalos_confianza[, "upr"]
nuevas_peliculas$limite_inferior_prediccion <- intervalos_prediccion[, "lwr"]
nuevas_peliculas$limite_superior_prediccion <- intervalos_prediccion[, "upr"]

nuevas_peliculas

