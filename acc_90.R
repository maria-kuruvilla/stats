stats_speed_acc_latency <- read.csv("../../data/temp_collective/roi/stats_speed_acc_latency.csv",header=TRUE,na.strings=c("[nan]"))


acc <- stats_speed_acc_latency$X90_acc
lat <- stats_speed_acc_latency$latency
temp <- stats_speed_acc_latency$Temperature
gs <- stats_speed_acc_latency$Groupsize

n <- length(stats_speed_acc_latency$X90_acc)
data <- as.data.frame(cbind(sample = (1:n),temp,gs,acc,lat))
model_acc <- lm(log(acc) ~ temp + gs,data)
summary(model_acc)

plot(fitted(model_acc), residuals(model_acc))
qqnorm(residuals(model_acc))
qqline(residuals(model_acc))

shapiro.test(residuals(model_acc))


model_acc_int <- lm(log(acc) ~ temp*gs,data)
summary(model_acc_int)
plot(fitted(model_acc_int), residuals(model_acc_int))
qqnorm(residuals(model_acc_int))
qqline(residuals(model_acc_int))

shapiro.test(residuals(model_acc_int))

model_acc_int_quad <- lm(log(acc) ~ temp*gs + I(temp^2),data)
summary(model_acc_int_quad)
plot(fitted(model_acc_int_quad), residuals(model_acc_int_quad))
qqnorm(residuals(model_acc_int_quad))
qqline(residuals(model_acc_int_quad))

shapiro.test(residuals(model_acc_int))

require(MASS)

boxcox(model_acc,plotit=TRUE)


boxcox(model_acc_int,plotit=TRUE)

model_inv <- lm(log(acc) ~ temp + gs,speed_data)
summary(model_inv)
plot(fitted(model_inv), residuals(model_inv))

qqnorm(residuals(model_inv))
qqline(residuals(model_inv))

shapiro.test(residuals(model_exp))

model_inv_int <- lm(acc^(-1) ~ temp*gs,speed_data)
summary(model_inv_int)
plot(fitted(model_inv_int), residuals(model_inv_int))

qqnorm(residuals(model_inv_int))
qqline(residuals(model_inv_int))

model_x <- lm(acc ~ temp + I(temp^2) + gs, data)
summary(model_x)
