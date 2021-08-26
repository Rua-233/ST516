asphalt <- read.table(file = "clipboard", sep = "\t", header = T)
diele <- asphalt$dielectric_constant
airv <- asphalt$air_void_.
slm <- lm(airv ~ diele, data = asphalt)
summary(slm)
layout(matrix(c(1,2,3,4),2,2)) 
plot(slm)

confint(slm)

grid <- 
AirvPred = predict(slm, new = grid, interval = "prediction")
