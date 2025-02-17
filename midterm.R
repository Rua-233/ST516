library(boot)
library(ISLR)
library(MASS)
library(leaps)
library(glmnet)
library(pls)
library(caTools)
library(caret)
library(tidyverse)
library(car)
housingData <- read.csv("housingData.csv", header = T)

housingData$LotShapes <- factor(housingData$LotShape)
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
housing <- housing %>% mutate_if(is.factor, as.numeric)
## Try simple linear regression
OLS <- lm(housing$SalePrice ~ ., data  = housing)
summary(OLS) # r square: 88.69%
layout(matrix(c(1,2,3,4),2,2)) 
plot(OLS)
vif(OLS)
## box cox transformation
layout(matrix(c(1,2,3,4),1,1)) 
bc <- boxcox(housing$SalePrice ~ ., data = housing)
lambda <- bc$x[which.max(bc$y)]
lambda # 0.1414
bc_data <- housing %>% mutate(bc_SalePrice = (SalePrice^lambda-1)/lambda) %>% .[-c(16)]
test_dt2 <- sample(1:nrow(bc_data), 0.25*nrow(bc_data), replace=F, set.seed(12))
train2 <- bc_data[-test_dt2,]
test2 <- bc_data[test_dt2,]
new_model <- lm(train2$bc_SalePrice ~ ., data = train2)
layout(matrix(c(1,2,3,4),2,2)) 
plot(new_model)
summary(new_model) #r square 91.27%
boxcox_pred <- predict(new_model, test2)
mean((boxcox_pred - test2$bc_SalePrice)^2) #0.3345681
sst <- sum((test2$bc_SalePrice - mean(test2$bc_SalePrice))^2)
sse <- sum((boxcox_pred - test2$bc_SalePrice)^2)
rsq <- 1 - sse/sst
rsq #0.9202345

## Only select variables with significance
housing_lm <- housing[c(16, 2:4, 7, 11, 14, 20, 25, 26)]
test_dt <- sample(1:nrow(housing_lm), 0.25*nrow(housing_lm), replace=F, set.seed(12))
train <- housing[-test_dt,]
test <- housing[test_dt,]
## multiple linear regression
mlr <- lm(SalePrice ~ LotArea + OverallQual + OverallCond + TotalBsmtSF + BedroomAbvGr
          + GarageCars + Kitchen_Qual + GarageTypes, data = train)
summary(mlr)
mlr_pred <- predict(mlr, test)
mean((mlr_pred - test$SalePrice)^2) #815196421
plot(mlr)
## multiple linear regression with interactions
mlr_inter <- lm(SalePrice ~ LotArea + OverallQual + OverallCond + TotalBsmtSF + BedroomAbvGr 
          + GarageCars + Kitchen_Qual + GarageTypes + LotArea*OverallQual + LotArea*OverallCond
          + LotArea*TotalBsmtSF + LotArea*BedroomAbvGr + LotArea*GarageCars + LotArea*Kitchen_Qual
          + LotArea*GarageTypes + OverallQual*OverallCond + OverallQual*TotalBsmtSF 
          + OverallQual*BedroomAbvGr + OverallQual*GarageCars + OverallQual*Kitchen_Qual 
          + OverallQual*GarageTypes + OverallCond*TotalBsmtSF + OverallCond*BedroomAbvGr
          + OverallCond*GarageCars + OverallCond*Kitchen_Qual + OverallCond*GarageTypes
          + TotalBsmtSF*BedroomAbvGr + TotalBsmtSF*GarageCars + TotalBsmtSF*Kitchen_Qual
          + TotalBsmtSF*GarageTypes + BedroomAbvGr*GarageCars + BedroomAbvGr*Kitchen_Qual
          + BedroomAbvGr*GarageTypes + GarageCars*Kitchen_Qual + GarageCars*GarageTypes
          + Kitchen_Qual*GarageTypes, data = train)
summary(mlr_inter)
mlr_inter_pred <- predict(mlr_inter, test)
mean((mlr_inter_pred - test$SalePrice)^2) #770517867
plot(mlr_inter)
## multiple linear regression with interaction and 2nd order
mlr_2_inter <- lm(SalePrice ~ LotArea + OverallQual + OverallCond + TotalBsmtSF + BedroomAbvGr 
          + GarageCars + Kitchen_Qual + GarageTypes + LotArea*OverallQual + LotArea*OverallCond
          + LotArea*TotalBsmtSF + LotArea*BedroomAbvGr + LotArea*GarageCars + LotArea*Kitchen_Qual
          + LotArea*GarageTypes + OverallQual*OverallCond + OverallQual*TotalBsmtSF 
          + OverallQual*BedroomAbvGr + OverallQual*GarageCars + OverallQual*Kitchen_Qual 
          + OverallQual*GarageTypes + OverallCond*TotalBsmtSF + OverallCond*BedroomAbvGr
          + OverallCond*GarageCars + OverallCond*Kitchen_Qual + OverallCond*GarageTypes
          + TotalBsmtSF*BedroomAbvGr + TotalBsmtSF*GarageCars + TotalBsmtSF*Kitchen_Qual
          + TotalBsmtSF*GarageTypes + BedroomAbvGr*GarageCars + BedroomAbvGr*Kitchen_Qual
          + BedroomAbvGr*GarageTypes + GarageCars*Kitchen_Qual + GarageCars*GarageTypes
          + Kitchen_Qual*GarageTypes + LotArea^2 + OverallQual^2 + OverallCond^2 + TotalBsmtSF^2 + BedroomAbvGr^2
          + GarageCars^2 + Kitchen_Qual^2 + GarageTypes^2, data = train)
summary(mlr_2_inter)
mlr_2_inter_pred <- predict(mlr_2_inter, test)
mean((mlr_2_inter_pred - test$SalePrice)^2) #770517867
## multiple linear regression with interaction and 2nd and 3rd order
mlr_23_inter <- lm(SalePrice ~ LotArea + OverallQual + OverallCond + TotalBsmtSF + BedroomAbvGr 
                  + GarageCars + Kitchen_Qual + GarageTypes + LotArea*OverallQual + LotArea*OverallCond
                  + LotArea*TotalBsmtSF + LotArea*BedroomAbvGr + LotArea*GarageCars + LotArea*Kitchen_Qual
                  + LotArea*GarageTypes + OverallQual*OverallCond + OverallQual*TotalBsmtSF 
                  + OverallQual*BedroomAbvGr + OverallQual*GarageCars + OverallQual*Kitchen_Qual 
                  + OverallQual*GarageTypes + OverallCond*TotalBsmtSF + OverallCond*BedroomAbvGr
                  + OverallCond*GarageCars + OverallCond*Kitchen_Qual + OverallCond*GarageTypes
                  + TotalBsmtSF*BedroomAbvGr + TotalBsmtSF*GarageCars + TotalBsmtSF*Kitchen_Qual
                  + TotalBsmtSF*GarageTypes + BedroomAbvGr*GarageCars + BedroomAbvGr*Kitchen_Qual
                  + BedroomAbvGr*GarageTypes + GarageCars*Kitchen_Qual + GarageCars*GarageTypes
                  + Kitchen_Qual*GarageTypes + LotArea^2 + OverallQual^2 + OverallCond^2 + TotalBsmtSF^2 + BedroomAbvGr^2
                  + GarageCars^2 + Kitchen_Qual^2 + GarageTypes^2 + LotArea^3 + OverallQual^3 + OverallCond^3 + TotalBsmtSF^3 + BedroomAbvGr^3
                  + GarageCars^3 + Kitchen_Qual^3 + GarageTypes^3 ,data = train)
mlr_23_inter_pred <- predict(mlr_23_inter, test)
mean((mlr_23_inter_pred - test$SalePrice)^2) #770517867

##Lasso
x_hs <- model.matrix(SalePrice ~ .-1, data = housing)
y_hs <- housing$SalePrice
set.seed(12)
train_hs = sample(1:nrow(x_hs), nrow(x_hs)*0.75)
test_hs <- (-train_hs)
y.test <- y_hs[test_hs]
cv.out <- cv.glmnet(x_hs[train_hs,], y_hs[train_hs], alpha = 1)
plot(cv.out)
lam <- min(cv.out$lambda)
lam
Lsm_hs <- glmnet(x_hs[train_hs,], y_hs[train_hs], alpha = 1,lambda = lam)
Lspred_hs <- predict(Lsm_hs, s = lam, newx = x_hs[test_hs,])
mean((Lspred_hs - y_hs[test_hs])^2) #703625127
coef(Lsm_hs)
plot(cv.out$glmnet.fit, "lambda", label=FALSE)
###Lasso R square
sst1 <- sum((y.test - mean(y.test))^2)
sse1 <- sum((Lspred_hs - y.test)^2)
rsq1 <- 1 - sse1/sst1
rsq1 #0.8712954

##Ridge
cv.out2 <- cv.glmnet(x_hs[train_hs,], y_hs[train_hs], alpha = 0)
plot(cv.out2)
lam4 <- min(cv.out2$lambda)
lam4
Rrm2 <- glmnet(x_hs[train_hs,], y_hs[train_hs], alpha = 0,lambda = lam4, thresh=1e-12)
Rrpred2 <- predict(Rrm2, s = lam4, newx=x_hs[test_hs,])
mean((Rrpred2 - y_hs[test_hs])^2) #724540745
coef(Rrm2)
plot(cv.out2$glmnet.fit, "lambda", label=FALSE)
###Ridge regression r square
sse2 <- sum((Rrpred2 - y.test)^2)
rsq2 <- 1 - sse2/sst1
rsq2 #0.8674696

