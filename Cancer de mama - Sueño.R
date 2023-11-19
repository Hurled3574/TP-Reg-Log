rm(list = ls())
library(corrplot)
library(olsrr)
library(wooldridge)
library(lmtest)
library(RcmdrMisc)
library(car)
library(FactoMineR)
library(MASS)
cmama <- read.csv ("data.csv")
# View (cmama)

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

# Descartamos la agrupación de variables por componentes principales por problemas de explicación de las variables indexadas. En otras palabras, no era conveniente la agrupación por PCA


## Pruebas Ómnibus

stepwise(modelo)

# La agrupación mediante método por pasos hacia adelante y hacia atrás nos llevó a obtener un conjunto de variables que podían explicar el diagnostico. ENtonces, el criterio de Akkaike nos fue útik para hallar tres variables que explicaban los tres tópicos principales que explicaban el diagnóstico y que reducían significativamente la colinealidad entre variables.

## Analisis de factores

factanal(cmama2[,-1], 3)

# Descartamos la agrupación de variables en Factores porque los agrupamientos no servían para explicar el diagnóstico ni comunicarlo

#Regresión Ridge

# ridge1 <- lm.ridge(diagnostico ~ radio_medio + textura_media + perimetro_medio + area_media + uniformidad_media + compactabilidad_media + concavidad_media + puntos_concavos_medios + simetria_media + dimension_fractal_media, cmama2, lambda = 0)

# No realizamos regresión Ridge para evitar problemas de interpretación

##Por criterio de Akkaikke nos quedamos con el siguiente modelo que explica el diagnostico

modelo_aic <- glm(diagnostico ~ textura_media + area_media + puntos_concavos_medios, family = binomial(link = "logit"), cmama2)

vif(modelo_aic)

summary(modelo_aic)

# Nueva exploración de los datos

cmama3 <- cmama2[, c(1, 3, 5, 9)]

scatterplotMatrix (cmama3, pch = 19)

#Prueba omnibus

modelo_aic

modelo_aic_r1 <-
  glm(diagnostico ~ textura_media, family = binomial(link = "logit"), cmama3)

modelo_aic_r2 <- glm(diagnostico ~ area_media, family = binomial(link = "logit"), cmama3)

modelo_aic_r3 <- glm(diagnostico ~ puntos_concavos_medios, family = binomial(link = "logit"), cmama3)

modelo_aic_b1 <- glm(diagnostico ~ textura_media + area_media, family = binomial(link = "logit"), cmama3)

modelo_aic_b2 <- glm(diagnostico ~ textura_media + puntos_concavos_medios, family = binomial(link = "logit"), cmama3)

modelo_aic_b3 <- glm(diagnostico ~ area_media + puntos_concavos_medios, family = binomial(link = "logit"), cmama3)

modelo_aic_c <- glm(diagnostico ~ 1, family = binomial(link="logit"), cmama3)

lrtest(modelo_aic, modelo_aic_r1)
lrtest(modelo_aic, modelo_aic_r2)
lrtest(modelo_aic, modelo_aic_r3)
lrtest(modelo_aic, modelo_aic_b1)
lrtest(modelo_aic, modelo_aic_b2)
lrtest(modelo_aic, modelo_aic_b3)
lrtest(modelo_aic, modelo_aic_c)



#########################
#########################

sueno <- data ("sleep75")