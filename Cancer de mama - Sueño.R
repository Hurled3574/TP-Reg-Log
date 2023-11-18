rm(list = ls())
library(corrplot)
library(olsrr)
library(wooldridge)
library(lmtest)
library(RcmdrMisc)
cmama <- read.csv("data.csv")
View(cmama)

######################

# Eliminación a priori de variables.
##Nos quedamos con las variables que contienen las medias por sencillez para comunicar los resultados, y porque expresan mejor la situación de cada caso.

cmama2 <- cmama[,c(2:3, 5:6, 11)] #Recorte de datos
colnames(cmama2) #Lista de variables
colnames(cmama2) <- c("diagnostico", "radio_medio", "perimetro_medio", "area_media", "simetria_media")
cmama2

##Yo creo que podemos omitir otras tantas variables o mediciones porque desconocemos de qué tratan y porque son muy específicas del campo de estudio, quizás. POr ejemplo, compactness.... Pero hay variables, como perimeter que SON CLAVE, porque hablan específicamente del perímetro del tumor.

#Análisis exploratorio de datos

##Valores duplicados
cmama[duplicated(cmama$id), "id"] #NO hay

##Boxplots
# boxplot(datos$radius_mean, horizontal = T)
# boxplot(datos$texture_mean, horizontal = T)
# boxplot(datos$perimeter_mean, horizontal = T)
# boxplot(datos$area_mean, horizontal = T)
# boxplot(datos$smoothness_mean, horizontal = T)
# boxplot(datos$compactness_mean, horizontal = T)

##Variables
colnames(cmama)

##Eliminar variable X?

##Cambio valores variable rta
table(cmama$diagnosis)

cmama2$diagnostico <- factor(cmama2$diagnostico,
                          levels = c("B", "M"),
                          labels = c(0, 1))
cmama2


## Pruebas Ómnibus
ols_regress(diagnostico)

lrtest(diagnostico)

#########################
#########################

sueno <- data("sleep75")