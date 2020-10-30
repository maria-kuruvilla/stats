acc <- stats_speed_acc_latency$`90_acc`
lat <- stats_speed_acc_latency$latency
temp <- stats_speed_acc_latency$Temperature
gs <- stats_speed_acc_latency$Groupsize

n <- length(stats_speed_acc_latency$`90_acc`)
data <- as.data.frame(cbind(sample = (1:n),temp,gs,acc,lat))
model_acc <- lm(acc ~ temp + gs,data)
summary(model_acc)
plot(fitted(model_acc), residuals(model_acc))
qqnorm(residuals(model_acc))
qqline(residuals(model_acc))

shapiro.test(residuals(model_acc))


model_acc_int <- lm(acc ~ temp*gs,data)
summary(model_acc_int)
plot(fitted(model_acc_int), residuals(model_acc_int))
qqnorm(residuals(model_acc_int))
qqline(residuals(model_acc_int))

shapiro.test(residuals(model_acc_int))

require(MASS)

boxcox(model_acc,plotit=TRUE)


boxcox(model_acc_int,plotit=TRUE)

model_inv <- lm(acc^(-1) ~ temp + gs,speed_data)
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
