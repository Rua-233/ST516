library(boot)
library(ISLR)
library(MASS)
library(leaps)
library(glmnet)
library(pls)
library(caTools)
library(caret)
library(tidyverse)
housingData <- read.csv("housingData.csv", header = T)

housingData$LotShapes <- factor(housingData$LotShape)
is.factor(housingData$LotShapes)
housingData$LotShapes[1:10]
summary(lm(housingData$SalePrice ~ housingData$LotShapes, data = housingData))
housingData$LotConfigs <- factor(housingData$LotConfig)
housingData$Neighborhoods <- factor(housingData$Neighborhood)
housingData$BldgTypes <- factor(housingData$BldgType)
housingData$HouseStyles <- factor(housingData$HouseStyle)
housingData$Exterior_1st <- factor(housingData$Exterior1st)
housingData$Foundations <- factor(housingData$Foundation)
housingData$BsmtFin_Type1 <- factor(housingData$BsmtFinType1)
housingData$Kitchen_Qual <- factor(housingData$KitchenQual)
housingData$GarageTypes <- factor(housingData$GarageType)
housing <- housingData[-c(3:7, 12:14, 20, 23)] %>% drop_na(.)

summary(lm(housing$SalePrice ~ ., data  = housing))
##Lasso
x_hs <- model.matrix(SalePrice ~ .-1, data = housing)
y_hs <- housing$SalePrice
set.seed(9)
train_hs = sample(1:nrow(x_hs), nrow(x_hs)/1.3)
test_hs = (-train_hs)
y.test = y_hs[test_hs]
cv.out <- glmnet(x_hs[train_hs,], y_hs[train_hs], alpha = 1)
lam = cv.out$lambda.min
Lsm_hs = glmnet(x_hs[train_hs,], y_hs[train_hs], alpha = 1,lambda = 2.5)
Lspred_hs = predict(Lsm_hs, s = 2.5, newx = x_hs[test_hs,])
mean((Lspred_hs - y_hs[test_hs])^2) #602244948 #lambda = 2.5

##Ridge
cv.out2 = cv.glmnet(x_hs[train_hs,], y_hs[train_hs], alpha = 0)
lam4 = cv.out2$lambda.min
lam4
Rrm2 = glmnet(x_hs[train_hs,], y_hs[train_hs], alpha = 0,lambda = lam4, thresh=1e-12)
Rrpred2 = predict(Rrm2, s = lam4, newx=x_hs[test_hs,])
mean((Rrpred2 - y_hs[test_hs])^2) #662614219
     