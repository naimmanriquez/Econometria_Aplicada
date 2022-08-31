## Analisis de datos de la endutih

# 1. Removemos notacion cient?fica
options(scipen = 999)

# Instalamos

install.packages("sjmisc")
install.packages("sjlabelled")
install.packages("survey")
install.packages("dplyr")
install.packages("pacman")

## Cargamos librerias 

library(pacman)
p_load(sjmisc, sjlabelled, tidyverse, haven, survey, dplyr, ggplot2)

## Carga de base de datos
base_endutih <- readRDS(url("https://github.com/naimmanriquez/Econometria_Aplicada/blob/main/Sesion_01/ENDUTIH/01_datos/base_endutih.rds?raw=true"))

# Nos quedamos solo con los mayores de 18 anios
endutih_mayores <- base_endutih %>%
  filter(EDAD %in% c(18:80))

# Convertir a numerico por si no esta
endutih_mayores <- endutih_mayores %>%
  mutate_at("EDAD", ~as.numeric(.)) 

endutih_mayores <- endutih_mayores %>%
  mutate_at("P7_4", ~as.numeric(.)) 

## CONOCIENDO LA ESTRUCTURA DE LOS DATOS ##

# Estructura de la base de datos y variables
str(endutih_mayores)
glimpse(endutih_mayores)

# Para una sola variable: Generalmente cuantas horas al dia utiliza Internet?
str(endutih_mayores$P7_4)

## Estadisticos descriptivos del uso de internet
summary(endutih_mayores$P7_4)

# Tabla de frecuencias
endutih_mayores %>% frq(P7_4)


## PREGUNTA ¿qué promedio de tiempo se dedica a Internet una persona en promedio.
summary(endutih_mayores$P7_4)

## Mas estadisticos descriptivos.
# varianza
var(endutih_mayores$P7_4, na.rm = TRUE)
# desviacion estandar
sd(endutih_mayores$P7_4, na.rm = TRUE)

# Grafica
# Boxplot
ggplot(endutih_mayores, aes(y = P7_4)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.15,
               color = 1) +  # Color barras 
  geom_boxplot(fill = 3,           # Color caja
               alpha = 0.5,        # Transparencia
               color = 1,          # Color del borde
               outlier.colour = 2) + # Color atípicos
  labs(title = 'Horas de uso del internet al día',
       y = 'Horas de uso',
       subtitle = 'Diagrama de cajas',
       caption = 'Fuente: ENDUTIH 2021. INEGI')

# Distribucion
ggplot(endutih_mayores, aes(x = P7_4)) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white") +
  stat_function(aes(P7_4),fun=dexp) +
  labs(title = 'Horas de uso del internet al día',
       y = 'Horas de uso',
       subtitle = 'Curvas de distribución',
       caption = 'Fuente: ENDUTIH, 2021. INEGI')

# Promedio quien dedica mas tiempo a Internet: 
## hombres o mujeres.
endutih_mayores$genero <- endutih_mayores$SEXO
endutih_mayores$genero <-factor(endutih_mayores$genero, labels = c("Masculino","Femenino"))
tapply(endutih_mayores$P7_4, endutih_mayores$genero, summary)

# Grafica
# Boxplot
ggplot(endutih_mayores, aes(y = P7_4)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.15,
               color = 1) +  # Color barras 
  geom_boxplot(fill = 3,           # Color caja
               alpha = 0.5,        # Transparencia
               color = 1,          # Color del borde
               outlier.colour = 2) + # Color atípicos
  facet_wrap(~ genero) +
  labs(title = 'Horas de uso del internet al día por género',
       y = 'Horas de uso',
       subtitle = 'Diagrama de cajas',
       caption = 'Fuente: ENDUTIH 2021. INEGI')

# Distribucion
ggplot(endutih_mayores, aes(x = P7_4)) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white") +
  facet_wrap(~ genero) +
  stat_function(aes(P7_4),fun=dexp) +
  labs(title = 'Horas de uso del internet al día por género',
       y = 'Horas de uso',
       subtitle = 'Curvas de distribución',
       caption = 'Fuente: ENDUTIH, 2021. INEGI')

## Modelo econometrico: regresion lineal simple
# Convertir a numerico
endutih_mayores <- endutih_mayores %>%
  mutate_at("P7_13", ~as.numeric(.)) 

# Librerias: fastdummies para crear dicotomicas
library(fastDummies)

# Dicotomica para uso de redes sociales
# Variable dicotomica para genero
endutih_mayores <-dummy_cols(endutih_mayores,select_columns=c("P7_13"))
# Tabla de datos
table(endutih_mayores$P7_13_1)

# Variable dependiente: horas que usa el internet: P7_4
# variable independiente: tiene redes sociales: P7_13

# Datos para el modelo
attach(endutih_mayores)

# Define variables
Y <- cbind(P7_4)
X1 <- cbind(P7_13_1)

# Estadisticos descriptivos
summary(Y)
summary(X1)

# Regresion lineal simple
olsreg1 <- lm(Y ~ X1)
summary(olsreg1)
plot(endutih_mayores$P7_13_1, endutih_mayores$P7_4)
abline(olsreg1, col = "red")

# Residuos analisis 
e1hat <- resid(olsreg1)
summary(e1hat)

## Homocedasticidad ##
library(lmtest)
bptest(olsreg1)

## Tabla de resultados
library(sjPlot)
library(Rcpp)

# Tabla
tab_model(olsreg1,show.est=TRUE,show.se=TRUE,show.std=TRUE,show.df=TRUE,show.p=TRUE,show.fstat=TRUE,digits=2,title="Resumen del modelo",p.style="numeric_stars")
