# 1. Removemos notacion cient?fica
options(scipen = 999)

## Cargamos librerias 

library(pacman)
p_load(sjmisc, sjlabelled, tidyverse, haven, survey, dplyr, ggplot2)

## Carga de base de datos
bd_enoe <- readRDS(url("https://github.com/naimmanriquez/Econometria_Aplicada/blob/main/Sesion_01/ENOE/01_datos/bd_enoe.rds?raw=true"))

# Filtro

bd_enoe$eda <- as.numeric(bd_enoe$eda)

bd_rotacion <- bd_enoe %>%
  filter(eda <= 90)

bd_rotacion <- bd_rotacion %>%
  filter(eda >= 16)

summary(bd_rotacion$eda)

# Filtrado de informacion

bd_rotacion <- bd_rotacion %>%    
  filter (p1 == "1")

# Busqueda de otro empleo
table(bd_rotacion$p8_2)

# Dicotomica de rotacion
bd_rotacion$rotacion <- 0
bd_rotacion$rotacion[bd_rotacion$p8_2 == 2] <- "1"
table(bd_rotacion$rotacion)
bd_rotacion$rotacion <- as.numeric(bd_rotacion$rotacion)

# Edad al cuadrado

bd_rotacion$eda2 = bd_rotacion$eda * bd_rotacion$eda

# Dicotomica de ahorros personales
bd_rotacion$prestamos <- 0
bd_rotacion$prestamos[bd_rotacion$p3m7 == 7] <- "1"
table(bd_rotacion$prestamos)
bd_rotacion$prestamos <- as.numeric(bd_rotacion$prestamos)

# Dicotomica de ahorros personales
bd_rotacion$prestamos <- 0
bd_rotacion$prestamos[bd_rotacion$p3m7 == 7] <- "1"
table(bd_rotacion$prestamos)
bd_rotacion$prestamos <- as.numeric(bd_rotacion$prestamos)

# Dicotomica de medio de obtencion del empleo, internet
bd_rotacion$internet <- 0
bd_rotacion$internet[bd_rotacion$p3n == 6] <- "1"
table(bd_rotacion$internet)
bd_rotacion$internet <- as.numeric(bd_rotacion$internet)

# Dicotomica de contrato de base, planta o tiempo indefinido
bd_rotacion$indefinido <- 0
bd_rotacion$indefinido[bd_rotacion$p3k1 == 2] <- "1"
table(bd_rotacion$indefinido)
bd_rotacion$indefinido <- as.numeric(bd_rotacion$indefinido)



## Modelo logit
# Cargamos libreria y removemos notacion cientifica
library(rms)
options(scipen = 999)

class(bd_rotacion$rotacion)
class(bd_rotacion$eda)
class(bd_rotacion$prestamos)
class(bd_rotacion$internet)
class(bd_rotacion$indefinido)


attach(bd_rotacion)
# Variable dependiente
Y <- cbind(rotacion)
# Variable independiente
X <- cbind(eda, eda2, prestamos, internet, indefinido)

# Regression coefficients
olsreg <- lm(Y ~ X)
summary(olsreg)

# Logit model coefficients
logit<- glm(Y ~ X, family=binomial (link = "logit"))
summary(logit)

# Logit model odds ratios
exp(logit$coefficients)

# Probit model coefficients
probit<- glm(Y ~ X, family=binomial (link="probit"))
summary(probit)

plot(bd_rotacion$eda, bd_rotacion$rotacion)