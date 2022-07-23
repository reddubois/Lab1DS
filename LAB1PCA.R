
library(dplyr)
library(ggplot2)
library(corrplot)
library(DataExplorer)


setwd("C:/Users/oscar/OneDrive/Escritorio/Oscar/Semestre 8/Data Science/Lab 1/Lab1DS-main/Lab1DS-main")

dataOriginal <- read.csv("train.csv")

# ================ Analisis Exploratorio =================

# Summary y glimpse del data set
summary(dataOriginal)
glimpse(dataOriginal)

# Exploracion inicial 

introduce(dataOriginal)
plot_intro(dataOriginal)

# El data set contiene información sobre 1460 casas en la ciudad de Ames, Iowa. 
# Para cada casa, las cuales se identifican con un ID único,
# contamos con 79 variables exploratorias. Estas describen aspectos de la casa como 
# zona, tamaño, forma, amenidades, entre otras. La variable objetivo, que se busca 
# predecir, es el precio de venta de la casa. 

plot_missing(dataOriginal)

# Procedemos a analizar la calidad de los datos. Vemos que hay variables con altos
# porcentajes de NA's, por lo que las descartamos. Esats variables 

# PoolQC
# MiscFeature
# Alley 
# Fence
# FirePlaceQu
table(dataOriginal$FireplaceQu)


plot_bar(dataOriginal)
plot_histogram(dataOriginal)
plot_correlation(na.omit(dataNum))

summary(dataOriginal)

dataNum <- dataOriginal %>%
  select(where(is.numeric))

corrplot(cor(dataNum))
cor(dataNum)

summary(dataNum[3])




