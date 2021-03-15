setwd("~/code/stats")
acc <- read.csv("../../data/temp_collective/roi/stats_before_loom_percentile_speed_acc_low_mask.csv",header=TRUE,na.strings=c("[nan]"))

model1 <- lm(X50_acc ~ Temperature + Groupsize, acc)
summary(model1)
plot(fitted(model1), residuals(model1))

model2 <- lm(X50_acc ~ I(Temperature^2) +Temperature + Groupsize, acc)
summary(model2)
plot(fitted(model2), residuals(model2))

model3 <- lm (log(X50_acc + 1 ) ~ Temperature + Groupsize, acc)
summary(model3)
plot(fitted(model3), residuals(model3))

model4 <- lm (log(X50_acc + 1 ) ~ Temperature + I(Temperature^2) + Groupsize, acc)
summary(model4)
plot(fitted(model4), residuals(model4))

require(faraway)
cook <- cooks.distance(model4)
halfnorm(cook)
#127, 193

model5 <- lm (log(X50_acc.1 + 1 ) ~ Temperature + Groupsize, acc)
summary(model5)
plot(fitted(model5), residuals(model5))


model6 <- lm (log(X50_acc.1 + 1 ) ~ Temperature + I(Temperature^2) + Groupsize, acc)
summary(model6)
plot(fitted(model6), residuals(model6))
#best

## speed
model6 <- lm (log(X50_speed + 1 ) ~ Temperature + Groupsize, acc)
summary(model6)
plot(fitted(model6), residuals(model6))

model7 <- lm (log(X50_speed + 1 ) ~ Temperature + I(Temperature^2) + Groupsize, acc)
summary(model7)
plot(fitted(model7), residuals(model7))

model8 <- lm (log(X99_speed + 1 ) ~ Temperature + Groupsize, acc)
summary(model8)
plot(fitted(model8), residuals(model8))

model9 <- lm (log(X99_speed + 1 ) ~ Temperature + I(Temperature^2) + Groupsize, acc)
summary(model9)
plot(fitted(model9), residuals(model9))

model10 <- lm (I(X99_speed^0.5) ~ Temperature + Groupsize, acc)
summary(model10)
plot(fitted(model10), residuals(model10))

qqnorm(residuals(model10))
qqline(residuals(model10))
