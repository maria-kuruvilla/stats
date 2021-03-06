setwd("~/Documents/code/stats")
data <- read.csv("../../data/temp_collective/roi/stats_loom_latency_nan.csv",header=TRUE,na.strings=c("[nan]"))

lat <- data$latency
#lat <- as.numeric(lat1)
temp <- data$Temperature
gs <- data$Groupsize
loom <- data$loom

n <- length(lat)

model1 <- lm(lat ~ temp + gs,data)
summary(model1)

model2 <- lm(lat ~ temp*gs,data)
summary(model2)

model3 <- lm(lat ~ temp + gs + loom,data)
summary(model3)

model4 <- lm(lat ~ temp*gs + loom,data)
summary(model4)

model5 <- lm(lat ~ temp+gs + loom+I(temp^2),data)
summary(model5)

model6 <- lm(lat ~ temp*gs + loom+I(temp^2),data)
summary(model6)

model7 <- lm(lat ~ temp*loom + gs +I(temp^2),data)
summary(model7)

model8 <- lm(lat ~ loom*gs + temp + I(temp^2),data)
summary(model8)
plot(fitted(model8), residuals(model8))
qqnorm(residuals(model8))
qqline(residuals(model8))

model9 <- lm(lat ~ temp*loom*gs +I(temp^2),data)
summary(model9)
plot(fitted(model9), residuals(model9))
qqnorm(residuals(model9))
qqline(residuals(model9))

model10 <- lm(lat ~ temp*loom*gs*I(temp^2),data)
summary(model10)
plot(fitted(model10), residuals(model10))
qqnorm(residuals(model10))
qqline(residuals(model10))

model_pois <- glm(lat ~ temp + gs + I(temp^2), family = quasipoisson, data)
summary(model_pois)

model_pois2 <- glm(lat ~ temp*gs, family = quasipoisson, data)
summary(model_pois2)

model_pois3 <- glm.nb(lat ~ temp*gs, data)
summary(model_pois3)

setwd("~/Documents/code/stats")
data <- read.csv("../../data/temp_collective/roi/stats_loom_latency_nan.csv",header=TRUE,na.strings=c("[nan]"))

lat <- data$latency
#lat <- as.numeric(lat1)
temp <- data$Temperature
gs <- data$Groupsize
loom <- data$loom

n <- length(lat)

model1 <- lm(lat ~ temp + gs,data)
summary(model1)

model2 <- lm(lat ~ temp*gs,data)
summary(model2)

model3 <- lm(lat ~ temp + gs + loom,data)
summary(model3)

model4 <- lm(lat ~ temp*gs + loom,data)
summary(model4)

model5 <- lm(lat ~ temp+gs + loom+I(temp^2),data)
summary(model5)

model6 <- lm(lat ~ temp*gs + loom+I(temp^2),data)
summary(model6)

model7 <- lm(lat ~ temp*loom + gs +I(temp^2),data)
summary(model7)

model8 <- lm(lat ~ loom*gs + temp + I(temp^2),data)
summary(model8)
plot(fitted(model8), residuals(model8))
qqnorm(residuals(model8))
qqline(residuals(model8))

model9 <- lm(lat ~ temp*loom*gs +I(temp^2),data)
summary(model9)
plot(fitted(model9), residuals(model9))
qqnorm(residuals(model9))
qqline(residuals(model9))

model10 <- lm(lat ~ temp*loom*gs*I(temp^2),data)
summary(model10)
plot(fitted(model10), residuals(model10))
qqnorm(residuals(model10))
qqline(residuals(model10))

model_pois <- glm(lat ~ temp + gs, family = quasipoisson, data)
summary(model_pois)

model_pois2 <- glm(lat ~ temp*gs, family = quasipoisson, data)
summary(model_pois2)

model_pois3 <- glm.nb(lat ~ temp*gs, data)
summary(model_pois3)

model_pois4 <- glm(lat ~ temp + gs + I(temp^2), family = quasipoisson, data)
summary(model_pois4)

model_pois5 <- glm(lat ~ temp + gs*I(temp^2), family = quasipoisson, data)
summary(model_pois5)


### this is the one 
model_pois6 <- glm(lat ~ temp + gs*loom + I(temp^2), family = quasipoisson, data)
summary(model_pois6)

require(faraway)
cook <- cooks.distance(model_pois6)
halfnorm(cook)
