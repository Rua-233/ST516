library(tree)
library(ISLR)
library(tidyverse)
library(caTools)
attach(Carseats)
library(randomForest)
library(MASS)
library(rpart)
library(gbm)
library(glmnet)
# 8.4.8
## a. split the data into train and test
CS <- Carseats
set.seed(1)
train <- sample.split(CS$Sales, SplitRatio = 0.75)
CS_train <- subset(CS, train == T)
CS_test <- subset(CS, train == F)
## b. regression tree of train dataset
CS_train_tree <- tree(Sales ~ ., data = CS_train)
summary(CS_train_tree)
plot(CS_train_tree)
text(CS_train_tree, pretty = 0)
CS_pred <- predict(CS_train_tree, CS_test)
MSE1 <- mean((CS_pred - CS_test$Sales)^2)
MSE1 #4.584482

## c. cross validation
set.seed(1)
CS_cv = cv.tree(CS_train_tree)
plot(CS_cv$size, CS_cv$dev, xlab = "Terminal Nodes", ylab = "CV Error", type="b")
### Prune the tree
CS_pru <- prune.tree(CS_train_tree, best = 13)
CS_pru_pred <- predict(CS_pru, CS_test)
MSE_pru <- mean((CS_pru_pred - CS_test$Sales)^2)
MSE_pru #4.210724

## d. bagging
set.seed(1)
CS_bag <- randomForest(Sales ~ ., data = CS_train, mtry = 10, importance = T)
importance(CS_bag)
bag_y <- predict(CS_bag, CS_test)
MSE_bag <- mean((bag_y - CS_test$Sales)^2)
MSE_bag #2.514795

## e. random forest
set.seed(1)
### m = 10/2
CS_rf1 = randomForest(Sales~., data = CS_train, mtry = 5, importance=T)
CS_rf1_pred <- predict(CS_rf1, newdata = CS_test)
MSE_rf1 <- mean((CS_rf1_pred - CS_test$Sales)^2)
MSE_rf1 #2.41984
importance(CS_rf1)
### m = square root of 10
CS_rf2 = randomForest(Sales~., data = CS_train, mtry = sqrt(10), importance=T)
CS_rf2_pred <- predict(CS_rf2, newdata = CS_test)
MSE_rf2 <- mean((CS_rf2_pred - CS_test$Sales)^2)
MSE_rf2 #2.665754
importance(CS_rf2)
### m = 10/5
CS_rf3 = randomForest(Sales~., data = CS_train, mtry = 2, importance=T)
CS_rf3_pred <- predict(CS_rf3, newdata = CS_test)
MSE_rf3 <- mean((CS_rf3_pred - CS_test$Sales)^2)
MSE_rf3 #3.024262
importance(CS_rf3)

# 8.4.9
## a. split the data into train and test
OJ <- OJ
set.seed(2)
train2 <- sample.split(OJ$Purchase, SplitRatio = 800/1070)
OJ_train <- subset(OJ, train2 == T)
OJ_test <- subset(OJ, train2 == F)

## b. fit a tree model
OJ_train_tree <- tree(Purchase ~ ., data = OJ_train)
summary(OJ_train_tree)

## c. a closer look at the model detail
OJ_train_tree

## d. plot the tree
plot(OJ_train_tree)
text(OJ_train_tree, pretty = 0)

## e. prediction
OJ_pred <- predict(OJ_train_tree, OJ_test, type = "class")
table(OJ_pred, OJ_test$Purchase)

## f. g. h. cross validation and optimal tree size.
OJ_cv <- cv.tree(OJ_train_tree, FUN = prune.misclass)
plot(OJ_cv$size, OJ_cv$dev, xlab = "Tree Size", ylab = "CV Classification Error", type = "b")

## i. j. k
OJ_pru <- prune.misclass(OJ_train_tree, best = 7)
OJ_pru_pred <- predict(OJ_pru, newdata = OJ_train, type = "class")
table(OJ_pru_pred, OJ_train$Purchase)
OJ_pru_pred_t <- predict(OJ_pru, newdata = OJ_test, type = "class")
table(OJ_pru_pred_t, OJ_test$Purchase)

# 8.4.10
## a. drop NA and log transform the data
Hitters <- Hitters %>% drop_na(Salary)
Hitters$Salary <- log(Hitters$Salary)

## b. split data to train and test
set.seed(3)
train3 <- sample.split(Hitters, SplitRatio = 200/263)
Ht_train <- subset(Hitters, train3 == T)
Ht_test <- subset(Hitters, train3 == F)

## c. boosting on train data with 1,000 trees for a range of values of λ
set.seed(3)
lambda <- seq(0.0001,0.5,0.01)
mse_train = rep(NA,length(lambda))
mse_test = rep(NA,length(lambda))
set.seed(3)
for (i in lambda){
  boost.Hitters = gbm(Salary ~ ., data = Ht_train, distribution = "gaussian", n.trees = 1000,
  interaction.depth = 4, shrinkage = i)
  train_pred = predict(boost.Hitters, newdata = Ht_train, n.trees = 1000)
  mse_train[which(i == lambda)] = mean((train_pred - Ht_train$Salary)^2)
  test_pred = predict(boost.Hitters, newdata = Ht_test, n.trees = 1000)
  mse_test[which(i == lambda)] = mean((test_pred - Ht_test$Salary)^2)
}
plot(lambda, mse_train, xlab = "Shrinkage Parameter", ylab = "MSE", type = "b", col = "red", 
     pch = 20)

## d. plot of test data
plot(lambda, mse_test, xlab = "Shrinkage Parameter", ylab = "MSE", type = "b", col = "blue", 
     pch = 20)
min(mse_test) #0.1617
lambda[which.min(mse_test)] #0.0101
min(mse_train) #8.723252e-12
lambda[which.min(mse_train)] #0.4901

## e. multi-linear regression and lasso regression
### multiple linear regression
mlr <- lm(Salary ~ ., data = Ht_train)
mlr_pred <- predict(mlr, Ht_test)
mse_mlr <- mean((mlr_pred - Ht_test$Salary)^2)
mse_mlr #0.2881416
### Lasso regression
x <- model.matrix(Salary ~ ., data = Ht_train)
y <- Ht_train$Salary
x_test <- model.matrix(Salary ~ ., data = Ht_test)
lasso <- glmnet(x, y, alpha = 1)
set.seed(3)
cv.out <- cv.glmnet(x, y, alpha = 1)
lam <- cv.out$lambda.min
lam #0.0160553
lasso_pred = predict(lasso, s = lam, newx = x_test)
mse_ls <- mean((lasso_pred - Ht_test$Salary)^2)
mse_ls #0.2988359

## f. the most importrant predictor of boost model
imp_boost = gbm(Salary ~ ., data = Ht_train, distribution = "gaussian", n.trees = 1000,
                 interaction.depth = 4, shrinkage = lambda[which.min(mse_test)])
summary(imp_boost)

## g. test set MSE of bagging
Ht_bag <- randomForest(Salary ~ ., data = Ht_train, mtry = 19, importance =T)
Ht_bag_pred <- predict(Ht_bag, Ht_test)
mse_Ht_bag <- mean((Ht_bag_pred - Ht_test$Salary)^2)
mse_Ht_bag #0.1508025

#8.4.11
## a. split data to train and test
### we need to convert Purchase column to numeric values
Caravan <- Caravan
Caravan$Purchase = ifelse(Caravan$Purchase == "Yes", 1, 0)
cara_train <- Caravan[1:1000, ]
cara_test <- Caravan[1001:5822, ]

## b. fit a boosting model
set.seed(4)
cara_boost = gbm(Purchase ~ ., data = cara_train, n.trees = 1000, shrinkage = 0.01, 
                    distribution = "bernoulli")
summary(cara_boost)
