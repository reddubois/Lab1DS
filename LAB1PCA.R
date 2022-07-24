
library(dplyr)
library(ggplot2)
library(corrplot)
library(DataExplorer)

#library(rela)
library(psych)
library(FactoMineR)
library(fpc)
library(factoextra)


setwd("C:/Users/oscar/OneDrive/Escritorio/Oscar/Semestre 8/Data Science/Lab 1/Lab1DS-main/Lab1DS-main")

dataOriginal <- read.csv("train.csv")

# ================ Analisis Exploratorio =================

# Summary y glimpse del data set
summary(dataOriginal)
glimpse(dataOriginal)

# Exploracion inicial 

# El data set contiene información sobre 1460 casas en la ciudad de Ames, Iowa. 
# Para cada casa, las cuales se identifican con un ID único,
# contamos con 79 variables exploratorias. Estas describen aspectos de la casa como 
# zona, tamaño, forma, amenidades, entre otras. La variable objetivo, que se busca 
# predecir, es el precio de venta de la casa. 

introduce(dataOriginal)
plot_intro(dataOriginal)

# Procedemos a analizar la calidad de los datos. Vemos que hay variables con altos
# porcentajes de NA's, por lo que las descartamos. Estas variables tienen el factor en común 
# de referirse a utilidades poco comunes que una casa pueda tener, como piscinas o cercas. 
# La mayoría de las casas en el data set no posee estas caracteristicas. En total, removemos 
# las siguientes 5 variables:
# PoolQC
# MiscFeature
# Alley 
# Fence
# FireplaceQu

plot_missing(dataOriginal)

dataOriginal <- dataOriginal %>%
  select(-c(PoolQC, MiscFeature, Alley, Fence, FireplaceQu))

# Ahora analizamos la distribucion de las variables 
# cuantitativas y cualitativas. Descartamos las variables cualitativas
# que sean muy homogeneas en la poblacion. Se elige descartar aquellas 
# variables donde una categoria concentra mas del 80% de las casas. 
# Entonces, descartamos las siguientes 20 variables:

# Street
# LandContour
# Utilities
# LandSlope
# Condition1
# Condition2
# BldgType
# RoofMatl
# ExterCond
# BsmtCond
# BsmtFinType2
# Heating
# CentralAir
# Electrical
# Functional
# GarageQual
# GarageCond
# PavedDrive
# SaleType
# SaleCondition

plot_bar(dataOriginal)

dataOriginal <- dataOriginal %>%
  select(-c(Street, LandContour, Utilities, LandSlope, Condition1, 
            Condition2, BldgType, RoofMatl, ExterCond, BsmtCond, 
            BsmtFinType2, Heating, CentralAir, Electrical, Functional, 
            GarageQual, GarageCond, PavedDrive, SaleType, SaleCondition))

# De igual manera, descartamos las variables cuantitativas 
# que esten muy sesgadas. Descartamos aquellas variables donde 
# al menos el 80% de los datos asuman un mismo valor. 
# Encontramos 9 de estas variables:

# BsmtFinSF2
# LotArea
# LowQualFinSF
# BsmtHalfBath
# EnclosedPorch
# KitchenAbvGr
# X3SsnPorch
# MiscVal
# PoolArea
# ScreenPorch

plot_histogram(dataOriginal)

dataOriginal <- dataOriginal %>%
  select(-c(BsmtFinSF2, LotArea, LowQualFinSF, BsmtHalfBath, 
            EnclosedPorch, KitchenAbvGr, X3SsnPorch, MiscVal, 
            PoolArea, ScreenPorch))

introduce(dataOriginal)

## ====================== PCA ======================

# Filtramos las variables numéricas para realizarles 
# un PCA. Luego, de las variables restantes descartamos aquellas que 
# no sean continuas. Variables discretas y ordinales se evitan. 
# Las variables descartadas son las siguientes:

# Id
# MSSubClass
# OverallQual
# OverallCond
# BsmtFullBath
# FullBath
# HalfBath
# BedroomAbvGr
# TotRmsAbvGrd
# Fireplaces
# GarageCars
# MoSold

glimpse(dataOriginal)

dataPCA <- dataOriginal %>%
  select(where(is.numeric)) %>%
  select(-c(Id, MSSubClass, OverallQual, OverallCond, 
            BsmtFullBath, FullBath, HalfBath, BedroomAbvGr, 
            TotRmsAbvGrd, Fireplaces, GarageCars, MoSold))



# Despues de la limpieza, quedamos con 16 variables continuas. 
# Deseamos reducir la dimensionalidad de este data set empleando PCA. 
# Primero, creamos la matriz de correlación de este set de datos, y 
# notamos que su determinante es muy cercano a 0. 

rcor <- cor(dataPCA, use = "pairwise.complete.obs")
det(rcor)

# 

pafData <- paf(as.matrix(dataPCA))
pafData$KMO #0.42 La adecuaciÃ³n a la muestra es mala
pafData$Bartlett #198.58 Mientras mas alto sea mejor
summary(pafDatos)



## ====================== Reglas de Asociación ======================

#Llamamos al dataset original para quitarle todo lo que quitamos para PCA
#excepto algunas variables categóricas cuyas entradas aparecen numéricas:
#MSSubClass
#OverallQual
#OverallCond
#MoSold

dataOriginal <- read.csv("train.csv")

dataOriginal <- dataOriginal %>%
  select(-c(PoolQC, MiscFeature, Alley, Fence, FireplaceQu, Street, 
            LandContour, Utilities, LandSlope, Condition1, 
            Condition2, BldgType, RoofMatl, ExterCond, BsmtCond, 
            BsmtFinType2, Heating, CentralAir, Electrical, Functional, 
            GarageQual, GarageCond, PavedDrive, SaleType, SaleCondition,
            BsmtFinSF2, LotArea, LowQualFinSF, BsmtHalfBath, 
            EnclosedPorch, KitchenAbvGr, X3SsnPorch, MiscVal, 
            PoolArea, ScreenPorch,RoofStyle))

# También se removió RoofStyle porque provocaba muchas reglas sobre techos triangulares.
# Se concluyó que probablemente se debe a que muchas casas poseen techos de este tipo.

dataARules <- dataOriginal %>%
  select(where(is.character))

dataARules$MSSubClass <- dataOriginal$MSSubClass
dataARules$OverallQual <- dataOriginal$OverallQual
dataARules$OverallCond <- dataOriginal$OverallCond
dataARules$MoSold <- dataOriginal$MoSold

introduce(dataARules)

glimpse(dataARules)

reglas<-apriori(dataARules[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)],
                                              parameter = list(support = 0.48,
                                              confidence = 0.60,
                                              target = "rules"))
inspect(reglas)

# Usando un support de 48% y un confidence de 60%, se obtuvieron las siguientes reglas:

#      lhs                    rhs                 support   confidence  coverage   lift      count
#[1]  {}                  => {ExterQual=TA}      0.6205479 0.6205479  1.0000000 1.0000000  906 
#[2]  {}                  => {LotShape=Reg}      0.6335616 0.6335616  1.0000000 1.0000000  925 
#[3]  {}                  => {BsmtExposure=No}   0.6527397 0.6527397  1.0000000 1.0000000  953 
#[4]  {}                  => {LotConfig=Inside}  0.7205479 0.7205479  1.0000000 1.0000000 1052 
#[5]  {}                  => {MSZoning=RL}       0.7883562 0.7883562  1.0000000 1.0000000 1151 

#[6]  {GarageType=Attchd} => {MSZoning=RL}       0.5308219 0.8908046  0.5958904 1.1299520  775 
#[7]  {MSZoning=RL}       => {GarageType=Attchd} 0.5308219 0.6733275  0.7883562 1.1299520  775 

#[8]  {LotShape=Reg}      => {LotConfig=Inside}  0.5123288 0.8086486  0.6335616 1.1222690  748 
#[9]  {LotConfig=Inside}  => {LotShape=Reg}      0.5123288 0.7110266  0.7205479 1.1222690  748 

#[10] {BsmtExposure=No}   => {LotConfig=Inside}  0.4801370 0.7355719  0.6527397 1.0208507  701 
#[11] {LotConfig=Inside}  => {BsmtExposure=No}   0.4801370 0.6663498  0.7205479 1.0208507  701 

#[12] {BsmtExposure=No}   => {MSZoning=RL}       0.4883562 0.7481637  0.6527397 0.9490174  713 
#[13] {MSZoning=RL}       => {BsmtExposure=No}   0.4883562 0.6194613  0.7883562 0.9490174  713 

#[14] {LotConfig=Inside}  => {MSZoning=RL}       0.5506849 0.7642586  0.7205479 0.9694331  804 
#[15] {MSZoning=RL}       => {LotConfig=Inside}  0.5506849 0.6985230  0.7883562 0.9694331  804 

# Ignoramos las reglas triviales (con premisa nula).

# Algunas reglas interesantes... (todas hablan de probabilidades, no equivalencias exactas)
# 1. Las primeras dos reglas nos dan una probable equivalencia entre las casas que poseen un garage pegado
# a ellas y las casas ubicadas en residenciales con baja densidad poblacional.
# 2. Las siguientes dos reglas nos dicen que las casas cuyo terreno  tiene una forma regular también mantienen
# todo su terreno en el interior de la propiedad.
# 3. Las casas sin exposición exterior a su sótano son casas que mantienen su terreno en el interior.
# 4. Las casas sin exposición exterior a su sótano son casas en residenciales con baja densidad poblacional.
# 5. La quinta regla junta la 3 y 4. Las casas con su terreno completamente en el interior son casas en
# residenciales con baja densidad poblacional.
