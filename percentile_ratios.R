setwd("~/code/stats")
percentiles_ratio <- read.csv("../../data/temp_collective/roi/stats_loom_data.csv",header=TRUE,na.strings=c("[nan]"))

speed99 <- percentiles_ratio$ratio_99_speed
speed90 <- percentiles_ratio$ratio_90_speed
speed_max <- percentiles_ratio$max_speed_ratio
acc90 <- percentiles_ratio$ratio_90_acc
hist(log(speed99))

temperature <- percentiles_ratio$Temperature
group_size <- percentiles_ratio$Groupsize
replicate <- percentiles_ratio$Replicate
loom <- percentiles_ratio$loom

n <- length(percentiles_ratio$Temperature)
data_ratio <- as.data.frame(cbind(sample = (1:n),speed99, speed90, acc90, speed_max,temperature,group_size, replicate, loom))


model_speed99 <- lm(log(speed99) ~ temperature + log(group_size,2) + loom,data_ratio)
summary(model_speed99)
plot(fitted(model_speed99), residuals(model_speed99))

qqnorm(residuals(model_speed99))
qqline(residuals(model_speed99))

shapiro.test(residuals(model_speed99))


###############

model_speed90 <- lm(log(speed99) ~ temperature + log(group_size,2) + replicate,data_ratio)
summary(model_speed90)
plot(fitted(model_speed90), residuals(model_speed90))

qqnorm(residuals(model_speed90))
qqline(residuals(model_speed90))

shapiro.test(residuals(model_speed90))

##################
model_speed99 <- lm(log(speed99) ~ I(temperature^2) + temperature+ log(group_size,2) + replicate,data_ratio)
summary(model_speed99)
plot(fitted(model_speed99), residuals(model_speed99))

qqnorm(residuals(model_speed99))
qqline(residuals(model_speed99))

shapiro.test(residuals(model_speed99))

#################### box cox ##############

require(MASS)

model1 <- lm(speed99 ~ temperature + log(group_size,2) + loom,data_ratio)
summary(model1)
plot(fitted(model1), residuals(model1))

boxcox(model1,plotit=TRUE) #####result is close to 0.5


model2 <- lm(speed99^0.5 ~ temperature+ log(group_size,2) + loom,data_ratio)
summary(model2)
plot(fitted(model2), residuals(model2))

qqnorm(residuals(model2))
qqline(residuals(model2))

shapiro.test(residuals(model2))

model3 <- lm(speed90 ~ temperature + log(group_size,2) + loom,data_ratio)
summary(model3)
plot(fitted(model3), residuals(model3))

boxcox(model3,plotit=TRUE) #####result is close to 0

model4 <- lm(log(speed90) ~ temperature + log(group_size,2) + loom,data_ratio)
summary(model4)
plot(fitted(model4), residuals(model4))

qqnorm(residuals(model4))
qqline(residuals(model4))

shapiro.test(residuals(model4))


model5 <- lm(acc90 ~ temperature + log(group_size,2) + loom,data_ratio)
summary(model5)
plot(fitted(model5), residuals(model5))

qqnorm(residuals(model5))
qqline(residuals(model5))

boxcox(model5,plotit=TRUE) #####result is close to -0.5

model6 <- lm(acc90^(-0.5) ~ temperature + log(group_size,2) + loom,data_ratio)
summary(model6)
plot(fitted(model6), residuals(model6))

qqnorm(residuals(model6))
qqline(residuals(model6))
