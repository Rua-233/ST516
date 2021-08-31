asphalt <- read.csv(file = "asphalt.csv", header = T)
slm <- lm(air_void_. ~ dielectric_constant, data = asphalt) 
summary(slm)
layout(matrix(c(1,2,3,4),2,2)) 
plot(slm)

confint(slm)

grid <- data.frame(dielectric_constant = seq(4.2, 4.6, 0.01))
muYx = predict(slm, new = grid, interval = "confidence")
muYx 
AirvPred = predict(slm, new = grid, interval = "prediction")
AirvPred
dev.new(width=10, height=8, unit="cm")
plot(grid$dielectric_constant, muYx[,1], type = "l", lwd = 2, 
     xlab = "dielectric constant", ylab = "air void (%)", ylim = c(0,10))
lines(grid$dielectric_constant, muYx[,2], lty = 2)
lines(grid$dielectric_constant, muYx[,3], lty = 2)
lines(grid$dielectric_constant, AirvPred[,2], lty = 3, col = "red")
lines(grid$dielectric_constant, AirvPred[,3], lty = 3, col = "red")
points(asphalt$dielectric_constant, asphalt$air_void_., pch = 19, col = "blue")
legend("topright", legend = c("fit", "95% CI", "95% PI", "actual"), 
       lwd = c(2,1,1,NA), pch = c(NA,NA,NA,19), lty = c(1,2,3,NA), col = c("black", "black", "red", "blue"), 
       bty = "n")
#airplane problem
install.packages("caTools")
install.packages("caret")
install.packages("Metrics")
library(tidyverse)
library(ggplot2)
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


prediction <- predict(fit2, test)
summary(prediction)
data <- data.frame(pred = prediction, actual = test$max_speed_mph)
mse <- mean((data$actual - data$pred)^2)
mse

#coconut

## Q.a
fibers <- read.csv("fiber.csv", header = T)
pairs(fibers)
plot(fibers)
cor(fibers) #strong positive correlation found between "grad" and "vel", positive correlation found between "cont" and "lngth" but not that strong. It would be a good try to start with additive model, and then use interaction model

## model 1
mr_fit1 <- lm(vel ~ cont + lngth + grad, data = fibers)
plot(mr_fit1)
summary(mr_fit1)

## model 2
mr_fit2 <- lm(vel ~ grad + lngth, data = fibers)
plot(mr_fit2)
summary(mr_fit2)

## model 3
mr_fit3 <- lm( vel ~ grad + lngth + cont 
               + I(cont^2) + I(lngth^2) + I(grad^2) 
               + cont*lngth + cont*grad + lngth*grad, 
               data = fibers)
plot(mr_fit3)
summary(mr_fit3)

## model 4
mr_fit4 <- lm( vel ~ I(cont^2) + I(lngth^2) + I(grad^2) 
               + cont*lngth + cont*grad + lngth*grad, 
               data = fibers)
plot(mr_fit4)
summary(mr_fit4)

## model 5
mr_fit5 <- lm( vel ~ + I(lngth^2) + I(grad^2) 
               + cont*lngth + lngth*grad, 
               data = fibers)
plot(mr_fit5)
summary(mr_fit5)
