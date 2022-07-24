
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










