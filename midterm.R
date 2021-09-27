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

housingData$LotConfigs <- factor(housingData$LotConfig)
housingData$Neighborhoods <- factor(housingData$Neighborhood)
housingData$BldgTypes <- factor(housingData$BldgType)
housingData$HouseStyles <- factor(housingData$HouseStyle)
housingData$Exterior_1st <- factor(housingData$Exterior1st)
housingData$Foundations <- factor(housingData$Foundation)
housingData$BsmtFin_Type1 <- factor(housingData$BsmtFinType1)
housingData$Kitchen_Qual <- factor(housingData$KitchenQual)
housingData$GarageTypes <- factor(housingData$GarageType)
housingData <- housingData %>% drop_na(.)
housing <- housingData[-c(3:7, 12:14, 20, 23)] %>% drop_na(.)
housing <- housing[c(16, 2:4, 7, 11, 14, 20, 25, 26)]
test_dt = sample(1:nrow(housing), 0.25*nrow(housing), replace=F, set.seed(12))
train = housing[-test_dt,]
test = housing[test_dt,]
summary(lm(housing$SalePrice ~ ., data  = housing))
slr <- lm(SalePrice ~ LotArea + OverallQual + OverallCond + TotalBsmtSF + BedroomAbvGr
          + GarageCars + Kitchen_Qual + GarageTypes, data = train)
slr_pred <- predict(slr, test)
mean((slr_pred - test$SalePrice)^2)

##Lasso
x_hs <- model.matrix(SalePrice ~ .-1, data = housingData)
y_hs <- housing$SalePrice
set.seed(12)
train_hs = sample(1:nrow(x_hs), nrow(x_hs)*0.75)
test_hs <- (-train_hs)
y.test <- y_hs[test_hs]
cv.out <- cv.glmnet(x_hs[train_hs,], y_hs[train_hs], alpha = 1)
lam <- min(cv.out$lambda)
Lsm_hs = glmnet(x_hs[train_hs,], y_hs[train_hs], alpha = 1,lambda = lam)
Lspred_hs = predict(Lsm_hs, s = lam, newx = x_hs[test_hs,])
mean((Lspred_hs - y_hs[test_hs])^2) #595343729 

##Ridge
cv.out2 = cv.glmnet(x_hs[train_hs,], y_hs[train_hs], alpha = 0)
lam4 <- min(cv.out2$lambda)
lam4
Rrm2 = glmnet(x_hs[train_hs,], y_hs[train_hs], alpha = 0,lambda = lam4, thresh=1e-12)
Rrpred2 = predict(Rrm2, s = lam4, newx=x_hs[test_hs,])
mean((Rrpred2 - y_hs[test_hs])^2) #971974919

