setwd("~/code/stats")
percentiles <- read.csv("../../data/temp_collective/roi/stats_loom_low_pass_data.csv",header=TRUE,na.strings=c("[nan]"))

speed99 <- percentiles$X99_speed
speed90 <- percentiles$X90_speed
speed_max <- percentiles$max_speed

hist(log(speed_max))

temperature <- percentiles$Temperature
group_size <- percentiles$Groupsize
replicate <- percentiles$Replicate
loom <- percentiles$loom

n <- length(percentiles$Temperature)
data <- as.data.frame(cbind(sample = (1:n),speed99, speed90, speed_max,temperature,group_size, replicate, loom))


model_speed99 <- lm(speed99 ~ temperature*log(group_size,2) + loom + replicate,data)
summary(model_speed99)
plot(fitted(model_speed99), residuals(model_speed99))

qqnorm(residuals(model_speed99))
qqline(residuals(model_speed99))

shapiro.test(residuals(model_speed99))

model_speed90 <- lm(speed90 ~ temperature + I(temperature^0.5) + log(group_size,2) + loom + replicate,data)
summary(model_speed90)
plot(fitted(model_speed90), residuals(model_speed90))

qqnorm(residuals(model_speed90))
qqline(residuals(model_speed90))

shapiro.test(residuals(model_speed90))

require(MASS)
boxcox(model_speed99,lambda = seq(0.2, 0.8, 1/10),plotit=TRUE)
## 0.4 for model 90
## 0.6 for model 99

model_speed90_boxcox <- lm(I(speed90^0.5) ~ temperature + I(temperature^0.5) + log(group_size,2) + loom + replicate,data)
summary(model_speed90_boxcox)
plot(fitted(model_speed90_boxcox), residuals(model_speed90_boxcox))

qqnorm(residuals(model_speed90))
qqline(residuals(model_speed90))

shapiro.test(residuals(model_speed90))

#####################
model_speed99 <- lm(speed99 ~ temperature + log(group_size,2) + loom + replicate,data)
summary(model_speed99)
plot(fitted(model_speed99), residuals(model_speed99))

qqnorm(residuals(model_speed99))
qqline(residuals(model_speed99))
boxcox(model_speed99,plotit=TRUE)


model_speed99 <- lm((speed99)^0.5 ~  temperature  +I(temperature^2)+ log(group_size,2) + I(log(group_size,2)^2) + loom + replicate,data)
summary(model_speed99)
plot(fitted(model_speed99), residuals(model_speed99))
plot(temperature, residuals(model_speed99))
plot(log(group_size,2), residuals(model_speed99))

qqnorm(residuals(model_speed99))
qqline(residuals(model_speed99))

shapiro.test(residuals(model_speed99))
