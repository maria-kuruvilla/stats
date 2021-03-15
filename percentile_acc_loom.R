setwd("~/code/stats")
percentiles <- read.csv("../../data/temp_collective/roi/stats_loom_low_pass_data.csv",header=TRUE,na.strings=c("[nan]"))

acc99 <- percentiles$X99_acc
acc90 <- percentiles$X90_acc


hist(log(acc90))

temperature <- percentiles$Temperature
group_size <- percentiles$Groupsize
replicate <- percentiles$Replicate
loom <- percentiles$loom

n <- length(percentiles$Temperature)
data <- as.data.frame(cbind(sample = (1:n),acc99, acc90,temperature,group_size, replicate, loom))


model_acc90 <- lm(acc90 ~ temperature + I(temperature^2) + log(group_size,2) + loom + replicate,data)
summary(model_acc90)
plot(fitted(model_acc90), residuals(model_acc90))

qqnorm(residuals(model_acc90))
qqline(residuals(model_acc90))

shapiro.test(residuals(model_acc90))

model_acc90 <- lm(log(acc90+1) ~ temperature + I(temperature^2) + log(group_size,2)*loom + replicate,data)
summary(model_acc90)
plot(fitted(model_acc90), residuals(model_acc90))

qqnorm(residuals(model_acc90))
qqline(residuals(model_acc90))

shapiro.test(residuals(model_acc90))



###

model_acc99 <- lm(log(acc99+1) ~ temperature + I(temperature^2) + log(group_size,2)*loom + replicate,data)
summary(model_acc99)
plot(fitted(model_acc99), residuals(model_acc99))

qqnorm(residuals(model_acc99))
qqline(residuals(model_acc99))