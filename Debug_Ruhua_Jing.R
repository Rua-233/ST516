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

## c. a closer look at the model detail (here is the problem)
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