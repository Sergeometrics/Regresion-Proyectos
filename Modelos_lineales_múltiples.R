library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(mctest)
library(car)

imdb_datos_completos  <- read_csv("imdb_datos_completos.csv")
View(imdb_datos_completos)

colnames(imdb_datos_completos)

datos_bien1 <- imdb_datos_completos |> 
  mutate(PrimerGenero = word(genres, 1, sep = ",")) #Añade una nueva columna para usar 
      #solo el primer género de la pelicula, pues con tres se forman muchas cátegorias
View(datos_bien)

datos_bien <- datos_bien1 |> filter(if_all(everything(), ~ !is.na(.x)))

pairs(datos_bien[, c("taquilla", "runtimeMinutes", "averageRating", "numVotes",
  "presupuesto", "startYear", "popularity")], cex.labels = 2)

ggplot(data = datos_bien, aes(x = presupuesto, y = taquilla)) + geom_point() + 
  geom_smooth(method = "lm")#parece haber relación

ggplot(data = datos_bien, aes(x = averageRating, y = taquilla)) + geom_point() + 
  geom_smooth(method = "lm")#no parece haber relación

ggplot(data = datos_bien, aes(x = runtimeMinutes, y = taquilla)) + geom_point() + 
  geom_smooth(method = "lm")#no parece haber relación

ggplot(data = datos_bien, aes(x = popularity, y = taquilla)) + geom_point() + 
  geom_smooth(method = "lm")#parece haber relación

ggplot(data = datos_bien, aes(x = PrimerGenero, y = taquilla)) + geom_point()
ggplot(data = datos_bien, aes(x = PrimerGenero, y = taquilla)) + geom_boxplot()

attach(datos_bien)

modelo1 <- lm(taquilla ~ presupuesto + runtimeMinutes + popularity)
summary(modelo1) #Modelo inicial

modelo1_alterno <- lm(taquilla ~ presupuesto + popularity + PrimerGenero)
summary(modelo1_alterno) #Viendo el modelo parece que los géneros de las películas no
#ayudan a explicar el dinero conseguido en taquilla

datos_m1 <- datos_bien |> mutate(residuales1 = modelo1$residuals)

ggplot(data = datos_m1, aes(y = residuales1, x = numVotes)) + geom_point() +
  geom_smooth(method = "lm") #Parece haber relación

modelo2 <- update(modelo1, . ~ . - runtimeMinutes + numVotes)
summary(modelo2) #runtimeMinutes aparece como no significativo, entonces lo quité. 
#Pero ahora con numVotes y popularity parecen ser colineales

##Revisión multicolinealidad
omcdiag(modelo2)
imcdiag(modelo2)
car::vif(modelo2)

modelo2_alternativo <- update(modelo1, .~. -runtimeMinutes)
summary(modelo2_alternativo)

datos_m2 <- datos_m1 |> mutate(residuales2 = modelo2$residuals,
  residuales2Alternos = modelo2_alternativo$residuals)
colnames(datos_m2)

ggplot(data = datos_m2, aes(y = residuales2, x = startYear*presupuesto)) + geom_point() +
  geom_smooth(method = "lm") #la línea parece horizontal, no parece haber relación
ggplot(data = datos_m2, aes(y = residuales2, x = startYear)) + geom_point() +
  geom_smooth(method = "lm") #la línea parece horizontal, no parece haber relación

ggplot(data = datos_m2, aes(y = residuales2Alternos, x = startYear*presupuesto)) + geom_point() +
  geom_smooth(method = "lm") #la línea parece horizontal, no parece haber relación
ggplot(data = datos_m2, aes(y = residuales2Alternos, x = startYear)) + geom_point() +
  geom_smooth(method = "lm") #la línea parece horizontal, no parece haber relación

modelo3 <- update(modelo2_alternativo, . ~ . + startYear:presupuesto)
summary(modelo3) 

AIC(modelo2_alternativo)
AIC(modelo3) #Mejor pero no estoy del todo seguro por el análisis visual

datos_m3 <- datos_m2 |> mutate(residuales3 = modelo3$residuals)

ggplot(data = datos_m3, aes(y = residuales3, x = startYear)) + geom_point() +
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

datos_m4 <- datos_m2 |> mutate(residuales = modelo4$residuals)

ggplot(data = datos_m4, aes(y = residuales, x = averageRating*numVotes)) + geom_point()

modelo4_alterno <- update(modelo3, . ~ . - presupuesto:startYear + startYear)
summary(modelo4_alterno) #Quería ver qué pasaba si usaba solo el starYear, 
#pues las gráficas con los residuales parecían iguales y 
# #resultó que es mejor la interacción

modelo5 <- update(modelo4, .~. +startYear)
summary(modelo5)

##Revisión multicolinealidad
omcdiag(modelo5)
imcdiag(modelo5)
car::vif(modelo5) 
#Refuerza que hay colinealidad entre presupuesto y presupueto:startYear, 
#pero no entiendo por qué ambos betas aparecen altamente significativos

##Modelo optimizado por AIC
modelo_full <- lm(taquilla ~ (runtimeMinutes + averageRating + numVotes + presupuesto + 
  startYear + popularity + PrimerGenero)^2)
summary(modelo_full)

modelo_optimizado <- step(modelo_full)
summary(modelo_optimizado)

AIC(modelo5)
AIC(modelo_optimizado)