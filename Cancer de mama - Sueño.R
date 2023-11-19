rm(list = ls())
library(corrplot)
library(olsrr)
library(wooldridge)
library(lmtest)
library(RcmdrMisc)
library(car)
library(FactoMineR)
cmama <- read.csv ("data.csv")
View (cmama)

######################

# Eliminación a priori de variables.
##Nos quedamos con las variables que contienen las medias por sencillez para comunicar los resultados, y porque expresan mejor la situación de cada caso. Mientras que las variables que expresan los errores estándar o los valores límite, resultan más complejas de comunicar.

##Además, definimos la problemática: la variable diagnóstico ¿es explicada por las variables del radio, textura, perimetro, area, uniformidad, commpactabilidad, concavidad, puntos cóncavos, por la simetría media y por la dimensión fractal media? ¿Por qué variables, si no? ¿En qué medida?

##Variables
colnames (cmama)

cmama2 <- cmama [, c(2:12)] #Recorte de datos
colnames (cmama2) #Lista de variables
colnames (cmama2) <-
  c(
    "diagnostico",
    "radio_medio",
    "textura_media",
    "perimetro_medio",
    "area_media",
    "uniformidad_media",
    "compactabilidad_media",
    "concavidad_media",
    "puntos_concavos_medios",
    "simetria_media", "dimension_fractal_media")

colnames (cmama2)

cmama2$diagnostico <- ifelse (cmama2$diagnostico == "B", 0, 1)

table (cmama2$diagnostico)

##Yo creo que podemos omitir otras tantas variables o mediciones porque desconocemos de qué tratan y porque son muy específicas del campo de estudio, quizás. POr ejemplo, compactness.... Pero hay variables, como perimeter que SON CLAVE, porque hablan específicamente del perímetro del tumor.

#Análisis exploratorio de datos

##Valores faltantes o duplicados
anyNA (cmama2) #No hay NAs
anyDuplicated (cmama2) #No hay elementos duplicados

#Diagnóstico de los datos

modelo <-
  glm (
    diagnostico ~ radio_medio + textura_media + perimetro_medio + area_media + uniformidad_media + compactabilidad_media + concavidad_media + puntos_concavos_medios + simetria_media + dimension_fractal_media,
    family = binomial(link = "logit"),
    data = cmama2
  )

summary (modelo)

## Casos outliers

scatterplotMatrix (cmama2, pch = 19) # Vemos colinealidad en las gráficas. Medidas remedio para la colinealidad.

#Diagnostico: vif()
vif (modelo)

##Medidas remedio: Componentes Principales? Regresión Ridge?
# Si PCS, entonces agrupamos las medidas de tamaño: radio, perimetro y área en un índice que se denomine "tamaño"

PCA(cmama2[,-1])
PCA(cmama2[,-1])$eig
PCA(cmama2[,-1])$var

tamaño <- 

## Pruebas Ómnibus

print(stepwise(modelo))

# ols_regress (diagnostico)
# 
# lrtest (diagnostico)

## Analisis de factores

factanal(cmama2[,-1], 3)

#########################
#########################

sueno <- data ("sleep75")