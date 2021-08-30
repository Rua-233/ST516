asphalt <- read.csv(file = "asphalt.csv", header = T)
diele <- asphalt$dielectric_constant
airv <- asphalt$air_void_.
slm <- lm(airv ~ diele, data = asphalt)
summary(slm)
layout(matrix(c(1,2,3,4),2,2)) 
plot(slm)

confint(slm)

grid <- 
AirvPred = predict(slm, new = grid, interval = "prediction")

#airplane problem
install.packages("caTools")
install.packages("caret")
install.packages("Metrics")
library(tidyverse)
library(Metrics)
library(caret)
library(caTools)
airplane <- read.csv(file = "airplane.csv", header = T)

smp_size <- floor(0.75 * nrow(airplane))
set.seed(10)
train_ind <- sample(seq_len(nrow(airplane)), size = smp_size)
train <- airplane[train_ind, ]
test <- airplane[-train_ind, ]

fit1 <- lm(max_speed_mph ~ poly(horsepower, degree = 2, raw = T), data = train)
summary(fit1)
coef(summary(fit1))
layout(matrix(c(1,2,3,4),2,2)) 
plot(fit1)

fit2 <- lm(max_speed_mph ~ poly(horsepower, degree = 3, raw = T), data = train)
summary(fit2)
coef(summary(fit2))
layout(matrix(c(1,2,3,4),2,2)) 
plot(fit2)

fit3 <- lm(max_speed_mph ~ poly(horsepower, degree = 4, raw = T), data = train)
summary(fit3)
coef(summary(fit3))
layout(matrix(c(1,2,3,4),2,2)) 
plot(fit3)

fit4 <- lm(max_speed_mph ~ poly(horsepower,  degree = 5, raw = T), data = train)
summary(fit4)
coef(summary(fit4))
layout(matrix(c(1,2,3,4),2,2)) 
plot(fit4)
print(anova(fit1, fit2, fit3, fit4))

prediction <- predict(fit3, test)
summary(prediction)
mse(actual = test$max_speed_mph, predicted = prediction)
