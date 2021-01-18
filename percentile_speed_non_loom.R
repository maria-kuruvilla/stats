setwd("~/code/stats")
percentiles_non_loom <- read.csv("../../data/temp_collective/roi/stats_non_loom_data.csv",header=TRUE,na.strings=c("[nan]"))

speed99 <- percentiles_non_loom$X99_speed
speed90 <- percentiles_non_loom$X90_speed
speed_max <- percentiles_non_loom$max_speed

hist(log(speed_max))

temperature <- percentiles_non_loom$Temperature
group_size <- percentiles_non_loom$Groupsize
replicate <- percentiles_non_loom$Replicate
loom <- percentiles_non_loom$loom

n <- length(percentiles_non_loom$Temperature)
data <- as.data.frame(cbind(sample = (1:n),speed99, speed90, speed_max,temperature,group_size, replicate, loom))


model_speed99 <- lm(log(speed99) ~ temperature + log(group_size,2) + replicate,data)
summary(model_speed99)
plot(fitted(model_speed99), residuals(model_speed99))

qqnorm(residuals(model_speed99))
qqline(residuals(model_speed99))

shapiro.test(residuals(model_speed99))

model_speed99 <- lm(speed99^0.5 ~ temperature + log(group_size,2) + replicate,data)
summary(model_speed99)
plot(fitted(model_speed99), residuals(model_speed99))

qqnorm(residuals(model_speed99))
qqline(residuals(model_speed99))

shapiro.test(residuals(model_speed99))