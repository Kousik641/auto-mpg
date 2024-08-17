data = read.csv("https://raw.githubusercontent.com/Kousik641/auto-mpg/main/auto-mpg.csv", sep = ",")

par(mfrow = c(1,2))
mdl = lm(mpg~weight,data=data)
plot(data$weight, residuals(mdl), xlab="Weight of car", ylab="residuals")
plot(fitted(mdl), residuals(mdl), xlab="Fitted value", ylab="residuals")

mdl2 = lm(mpg~log(weight),data=data)
par(mfrow = c(1,2))
plot(log(data$weight), residuals(mdl2), xlab = "logarithm of Weight of car", ylab = "residuals of new model")
abline(h = 0, col = "blue", lty = 2)
qqnorm(residuals(mdl2))
qqline(residuals(mdl2), col = "red")

library(MASS)
bc.mpg.weight.lm <- boxcox(mpg~log(weight), data = data, plotit = F)
lambda.hat <- bc.mpg.weight.lm$x[which.max(bc.mpg.weight.lm$y)]
data$mpg.new <- (data$mpg^lambda.hat-1) / lambda.hat
bc.mdl <- lm(mpg.new~log(weight), data=data)
par(mfrow = c(1,2))
plot(log(data$weight), residuals(bc.mdl), xlab="log of weight of cars", ylab="new residuals")
abline(h = 0, col = "blue", lty = 2)
qqnorm(residuals(bc.mdl))
qqline(residuals(bc.mdl), col = "red")
