library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(mctest)
library(car)
library(nortest)
library(tseries)
library(lmtest)

imdb_datos_completos  <- read_csv("imdb_datos_completos.csv")
View(imdb_datos_completos)

colnames(imdb_datos_completos)

datos_bien <- imdb_datos_completos |> 
  mutate(
    #Añade una nueva columna para usar 
      #solo el primer género de la pelicula, pues con tres se forman muchas cátegorias 
    PrimerGenero = word(genres, 1, sep = ",")) |> 
  filter(if_all(everything(), ~ !is.na(.x))) 
View(datos_bien)

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
ggplot(data = datos_m2, aes(y = residuales2Alternos, x = startYear)) + geom_point()



modelo3 <- update(modelo2_alternativo, . ~ . + startYear:presupuesto)
summary(modelo3) 

AIC(modelo2_alternativo)
AIC(modelo3) #Mejor pero no estoy del todo seguro de añadir startYear:presupuesto 
#por la colinealidad con presupuesto


datos_m3 <- datos_m2 |> mutate(residuales3 = modelo3$residuals)

ggplot(data = datos_m3, aes(y = residuales3, x = startYear)) + geom_point() +
  geom_smooth(method = "lm")

modelo4 <- update(modelo3, .~. +startYear)
summary(modelo4)

##Revisión multicolinealidad
omcdiag(modelo4)
imcdiag(modelo4)
car::vif(modelo4) 
#presupuesto y presupuesto:startYear son colineales, entonces eliminaré la variable interactiva

modelo4_alterno <- update(modelo3, ~. +averageRating)
summary(modelo4_alterno)

modelo5 <- update(modelo4, . ~ . -startYear:presupuesto + averageRating)
summary(modelo5) #Peor que modelo4_alterno

datos_m4 <- datos_m4 |> mutate(residuales = modelo4$residuals)
datos_m4_a <- datos_m3 |> mutate(residuales4_a = modelo4_alterno$residuals)

ggplot(data = datos_m4, aes(y = residuales, x = averageRating*numVotes)) + geom_point()

ggplot(data = datos_m4_a, aes(y = residuales4_a, x = averageRating*numVotes)) + geom_point() +
  geom_smooth(method = "lm")
ggplot(data = datos_m4_a, aes(y = residuales4_a, x = numVotes)) + geom_point() +
  geom_smooth(method = "lm")


modelo5_alterno <- update(modelo4_alterno, .~. + numVotes)
summary(modelo5_alterno)

##Revisión multicolinealidad
omcdiag(modelo5_alterno)
imcdiag(modelo5_alterno)
car::vif(modelo5_alterno) 
#Refuerza que hay colinealidad entre presupuesto y presupueto:startYear, 
#pero no entiendo por qué ambos betas aparecen altamente significativos

modelo6_alterno <- update(modelo5_alterno, .~. - presupuesto:startYear)
summary(modelo6_alterno)

##Revisión multicolinealidad
omcdiag(modelo6_alterno)
imcdiag(modelo6_alterno)
car::vif(modelo6_alterno) 

datos_m6_a <- datos_m4_a |> mutate(residuales6_a = modelo6_alterno$residuals)

ggplot(data = datos_m6_a, aes(y = residuales6_a, x = numVotes^2)) + geom_point() +
  geom_smooth(method = "lm") # Tal vez
ggplot(data = datos_m6_a, aes(y = residuales6_a, x = log(numVotes))) + geom_point() +
  geom_smooth(method = "lm") # No parece
ggplot(data = datos_m6_a, aes(y = residuales6_a, x = exp(popularity))) + geom_point() +
  geom_smooth(method = "lm") # No parece haber relación alguna con transformaciones de popularidad

##Modelo optimizado por AIC
modelo_full <- lm(taquilla ~ (runtimeMinutes + averageRating + numVotes + presupuesto + 
  startYear + popularity + PrimerGenero)^2)
summary(modelo_full)

modelo_optimizado <- step(modelo_full)
summary(modelo_optimizado)

AIC(modelo6_alterno)
AIC(modelo_optimizado)
AIC(modelo6_alterno) - AIC(modelo_optimizado)

BIC(modelo6_alterno)
BIC(modelo_optimizado)
BIC(modelo6_alterno) - BIC(modelo_optimizado)

summary(modelo6_alterno)
summary(modelo_optimizado)

#Hay diferencias a tener en cuenta con el AIC y el BIC

### Análisis residual
## Modelo 6 alterno
# qqplot

qqnorm(modelo6_alterno$residuals)
qqline(modelo6_alterno$residuals) # Parecen no ser normales

# Tests de normalidad

shapiro.test(modelo6_alterno$residuals) # Los datos no son normales
lillie.test(modelo6_alterno$residuals) # Los datos no son normales
ad.test(modelo6_alterno$residuals) # Los datos no son normales
jarque.bera.test(modelo6_alterno$residuals) # Los datos no son normales

# Homocedasticidad

ggplot(data = datos_bien, aes(x = modelo6_alterno$fitted.values, y = modelo6_alterno$residuals)) +
geom_point() +
geom_smooth(color = "firebrick", se = FALSE) +
geom_hline(yintercept = 0) +
theme_bw() # Parece que los datos no son homocedásticos

bptest(modelo6_alterno) # Los datos no son homocedásticos

# Multicolinealidad

omcdiag(modelo6_alterno)
imcdiag(modelo6_alterno)
car::vif(modelo6_alterno) # No hay multicolinealidad
dwt(modelo6_alterno, alternative = "two.sided") # No hay autocorrelación
