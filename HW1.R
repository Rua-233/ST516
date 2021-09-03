# Asphalt
asphalt <- read.csv(file = "asphalt.csv", header = T)
plot(asphalt$dielectric_constant ~ asphalt$air_void_.)
## Question a and b
slm <- lm(air_void_. ~ dielectric_constant, data = asphalt) 
summary(slm)
layout(matrix(c(1,2,3,4),2,2)) 
plot(slm)
## Question c
confint(slm)
## Question d
grid <- data.frame(dielectric_constant = seq(4.2, 4.6, 0.01))
muYx = predict(slm, new = grid, interval = "confidence")
muYx 
AirvPred = predict(slm, new = grid, interval = "prediction")
AirvPred
layout(matrix(c(1,2,3,4),1,1)) 
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
layout(matrix(c(1,2,3,4),1,1)) 
plot(airplane$horsepower, airplane$max_speed_mph, pch = 19)
## split the data
test_dt = sample(1:79,20,replace=F, set.seed(10))
train = airplane[-test_dt,]
test = airplane[test_dt,]
plot(test$horsepower, test$max_speed_mph)
## build and fit models
fit1 <- lm(max_speed_mph ~ horsepower, data = train)
summary(fit1)

fit2 <- lm(max_speed_mph ~ poly(horsepower, degree = 2, raw = T), data = train)
summary(fit2)
coef(summary(fit2))
layout(matrix(c(1,2,3,4),2,2)) 
plot(fit2)

fit3 <- lm(max_speed_mph ~ poly(horsepower, degree = 3, raw = T), data = train)
summary(fit3)
coef(summary(fit3))
layout(matrix(c(1,2,3,4),2,2)) 
plot(fit3)

fit4 <- lm(max_speed_mph ~ poly(horsepower, degree = 4, raw = T), data = train)
summary(fit4)
coef(summary(fit4))
layout(matrix(c(1,2,3,4),2,2)) 
plot(fit4)

fit5 <- lm(max_speed_mph ~ poly(horsepower,  degree = 5, raw = T), data = train)
summary(fit5)
coef(summary(fit5))
layout(matrix(c(1,2,3,4),2,2)) 
plot(fit5)

fit6 <- lm(max_speed_mph ~ poly(horsepower,  degree = 6, raw = T), data = train)
summary(fit6)
coef(summary(fit6))
layout(matrix(c(1,2,3,4),2,2)) 
plot(fit6)

fit7 <- lm(max_speed_mph ~ poly(horsepower,  degree = 7, raw = T), data = train)
summary(fit7)
coef(summary(fit7))
layout(matrix(c(1,2,3,4),2,2)) 
plot(fit7)
print(anova(fit1, fit2, fit3, fit4, fit5, fit6))

## choose the model with degree = 2 to improve
fit2.1 <- lm(max_speed_mph ~ horsepower + I(horsepower^2), data = train)
summary(fit2.1)
coef(summary(fit2.1))
layout(matrix(c(1,2,3,4),2,2)) 
plot(fit2.1)
prediction <- predict(fit2.1, test)
muYx_ap <- predict(fit2.1, test, interval = "confidence")
muYx_ap
pred_ap <- predict(fit2.1, test, interval = "prediction")
pred_ap
layout(matrix(c(1,2,3,4),1,1)) # smooth.spline(test$max_speed_mph, muYx_ap[,1]
## plot with 95% CI and 95% PI
plot(test$horsepower, test$max_speed_mph, xlim = c(100, 500), ylim = c(100, 300), xlab = "Horsepower", ylab = "Max Speed (mph)")
lines(smooth.spline(test$horsepower, muYx_ap[,1]), lty = 1, lwd = 2)
lines(smooth.spline(test$horsepower, pred_ap[,2]), lty = 2, lwd = 2, col = "red")
lines(smooth.spline(test$horsepower, pred_ap[,3]), lty = 2, lwd = 2, col = "red")
lines(smooth.spline(test$horsepower, muYx_ap[,2]), lty = 3)
lines(smooth.spline(test$horsepower, muYx_ap[,3]), lty = 3)
points(test$horsepower, test$max_speed_mph, pch = 19, col = "blue")
legend("topleft", legend = c("fit", "95% CI", "95% PI", "actual"), 
       lwd = c(2,1,1,NA), pch = c(NA,NA,NA,19), lty = c(1,2,3,NA), col = c("black", "black", "red", "blue"), 
       bty = "n")
summary(prediction)
## calculate the mse
data <- data.frame(pred = prediction, actual = test$max_speed_mph)
mse <- mean((data$actual - data$pred)^2)
mse
#coconut

## Q.a
install.packages("faraway")
library(faraway)
fibers <- read.csv("fiber.csv", header = T)
pairs(fibers)
plot(fibers)
cor(fibers) #strong positive correlation found between "grad" and "vel", positive correlation found between "cont" and "lngth" but not that strong. It would be a good try to start with additive model, and then use interaction model

## model 1
mr_fit1 <- lm(vel ~ cont + lngth + grad, data = fibers)
plot(mr_fit1)
summary(mr_fit1)
vif(mr_fit1)
## model 2
mr_fit2 <- lm(vel ~ grad + lngth, data = fibers)
plot(mr_fit2)
summary(mr_fit2)
vif(mr_fit2)
## model 3
mr_fit3 <- lm( vel ~ grad + lngth + cont 
               + I(cont^2) + I(lngth^2) + I(grad^2) 
               + cont*lngth + cont*grad + lngth*grad, 
               data = fibers)
plot(mr_fit3)
summary(mr_fit3)
vif(mr_fit3)
## model 4
fibers$cont_c <- fibers$cont - mean(fibers$cont)
fibers$lngth_c <- fibers$lngth - mean(fibers$lngth)
fibers$grad_c <- fibers$grad - mean(fibers$grad)
mr_fit4 <- lm( vel ~ grad + lngth + cont + I(cont_c^2) + I(lngth_c^2) + I(grad_c^2) 
               + cont_c*lngth_c + cont_c*grad_c + lngth_c*grad_c, 
               data = fibers)
plot(mr_fit4)
summary(mr_fit4)
vif(mr_fit4)
## model 5
mr_fit5 <- lm( vel ~ grad + lngth + I(lngth_c^2) + I(grad_c^2) 
               + cont_c*lngth_c + lngth_c*grad_c, 
               data = fibers)
plot(mr_fit5)
summary(mr_fit5)
vif(mr_fit5)
