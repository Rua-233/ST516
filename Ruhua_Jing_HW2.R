install.packages("boot")
install.packages("ISLR")
install.packages("leaps")
install.packages("glmnet")
install.packages("pls")
library(boot)
library(ISLR)
library(MASS)
library(leaps)
library(glmnet)
library(pls)
library(caTools)
library(caret)
# 5.4.8
## 5.4.8.a
set.seed(1)
x = c(rnorm(100))
y = x - 2*x^2 + rnorm(100)
## 5.4.8.b
plot(x, y)
## 5.4.8.c
set.seed(1)
m <- mean(c(rnorm(100)))
x_c = c(rnorm(100) - m)
dataXY = data.frame(x, x_c, y)
cv.err = rep(0,4)
for (i in 1:4) {
  if (i == 1) {
  lr.fit = glm(y~poly(x,degree=i,raw=TRUE))
  cv.err[i] = cv.glm(dataXY, lr.fit)$delta[1]
  } else {
  lr.fit = glm(y~poly(x_c,degree=i,raw=TRUE))
  cv.err[i] = cv.glm(dataXY, lr.fit)$delta[1]}
}
cv.err
## 5.4.8.d
set.seed(666)
x = c(rnorm(100))
m <- mean(c(rnorm(100)))
x_c = c(rnorm(100) - m)
dataXY = data.frame(x, x_c, y)
cv.err = rep(0,4)
for (i in 1:4) {
  if (i == 1) {
    lr.fit = glm(y~poly(x,degree=i,raw=TRUE))
    cv.err[i] = cv.glm(dataXY, lr.fit)$delta[1]
  } else {
    lr.fit = glm(y~poly(x_c,degree=i,raw=TRUE))
    cv.err[i] = cv.glm(dataXY, lr.fit)$delta[1]}
}
cv.err
## 5.4.8.f
summary(glm(y~poly(x,degree=1,raw=TRUE)))
summary(glm(y~poly(x_c,degree=2,raw=TRUE)))
summary(glm(y~poly(x_c,degree=3,raw=TRUE)))
summary(glm(y~poly(x_c,degree=4,raw=TRUE)))

# 6.8.9
## 6.8.9 a
x = model.matrix(Apps~.,College)[,-1]
y = College$Apps
College
### split data into train and test
train <- sample(1:nrow(x), nrow(x)*0.75)
test = (-train)
## 6.8.9.b
### least square model with lm()
train_df = data.frame(College[train,])
test_df = data.frame(College[test,])
lm1 = lm(Apps~., data = train_df)
lm_pred = predict(lm1, test_df, type=c("response"))
err.lm = mean((lm_pred-test_df$Apps)^2)
err.lm
### use K-fold cross validation with the entire data, K = 5
set.seed(1)
train_control <- trainControl(method = "cv", number = 5)
model <- train(Apps ~., data = College,
               method = "lm",
               trControl = train_control)
model
lm2 <- glmnet(x, y, alpha = 0, lambda = 0, thresh = 1e-12)
cv.error <- cv.glm(College, lm2, K = 5)$delta[1]
cv.error
##6.8.9.c
### build ridge regression model
cv.out <- cv.glmnet(x[train,], y[train],alpha = 0)
lam <- cv.out$lambda.min
Rrm = glmnet(x[train,],y[train],alpha = 0,lambda = lam, thresh = 1e-12)
Rrpred = predict(Rrm, s = lam, newx=x[test,])
err.ridge = mean((Rrpred-y[test])^2)
err.ridge #1029644
lam #370.544
## 6.8.9.d
### fit a lasso model
cv.out2 <- cv.glmnet(x[train,], y[train],alpha = 1)
lam2 <- cv.out2$lambda.min
Lsm = glmnet(x[train,],y[train],alpha = 1,lambda = lam2, thresh = 1e-12)
Lspred = predict(Lsm, s = lam2, newx=x[test,])
err.lasso = mean((Lspred-y[test])^2)
err.lasso #1073491
lam2 #1.977

# 6.8.11
## 6.8.11.a
data("Boston")
### lasso model
x_bos <- model.matrix(crim ~ .-1, data = Boston)
y_bos <- Boston$crim
set.seed(123)
train_bos = sample(1:nrow(x_bos), nrow(x_bos)/1.3)
test_bos = (-train_bos)
y.test = y[test_bos]
cv.out = cv.glmnet(x_bos[train_bos,], y_bos[train_bos], alpha = 1)
lam3 = cv.out$lambda.min
Lsm_bos = glmnet(x_bos[train_bos,], y_bos[train_bos], alpha = 1,lambda = lam3)
Lspred_bos = predict(Lsm_bos, s = lam3, newx = x_bos[test_bos,])
mean((Lspred_bos - y_bos[test_bos])^2) #19.5585
lam3 #0.0547
### ridge regression
cv.out = cv.glmnet(x_bos[train_bos,], y_bos[train_bos], alpha = 0)
lam4 = cv.out$lambda.min
Rrm2 = glmnet(x_bos[train_bos,], y_bos[train_bos], alpha = 0,lambda = lam4, thresh=1e-12)
Rrpred2 = predict(Rrm2, s = lam4, newx=x_bos[test_bos,])
mean((Rrpred2 - y_bos[test_bos])^2) #18.8724
lam4 #0.0573
### subset

