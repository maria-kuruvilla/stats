annd <- read.csv("../../data/temp_collective/roi/stats_annd_data.csv",header=TRUE,na.strings=c("[nan]"))

temperature <- annd$Temperature
group_size <- annd$Groupsize
date <- annd$Date
trial <- annd$Trial
acclimation <-  as.numeric(as.POSIXct(annd$Time_start_record, format = "%H:%M") - as.POSIXct(annd$Time_fish_in, format = "%H:%M"))
annd_values <- annd$annd
log_annd_values <- log(annd_values)
time_in <- as.POSIXct(annd$Time_fish_in, format = "%H:%M")

hist(annd_values,30)


n <- length(annd$annd)
data_annd <- as.data.frame(cbind(sample = (1:n),as.numeric(temperature), as.numeric(group_size), as.numeric(trial), date, as.numeric(acclimation),as.numeric(time_in), as.numeric(log_annd_values), as.numeric((annd_values))))

model1 <- lm(annd_values ~ temperature + group_size + as.factor(date) + acclimation, data_annd)
summary(model1)
plot(fitted(model1), residuals(model1))

qqnorm(residuals(model1))
qqline(residuals(model1))
#bad residual plot and bad qq plot

model2 <- lm(log_annd_values ~ temperature + group_size + as.factor(date) + acclimation, data_annd)
summary(model2)
plot(fitted(model2), residuals(model2))

qqnorm(residuals(model2))
qqline(residuals(model2))


model6 <- lm(annd_values^(-0.5) ~ temperature + I(temperature^2) +  group_size + trial + acclimation, data_annd)
summary(model6)

plot(fitted(model6), residuals(model6))

library(lme4)

model_random <- lmer(annd_values ~ temperature + group_size + (1|trial) + (1|date) + acclimation, data_annd)
summary(model_random)
plot(fitted(model_random), residuals(model_random))



model_random <- lmer(log(annd_values) ~ temperature + group_size + (1|trial) + (1|date) + acclimation, data_annd)
summary(model_random)
plot(fitted(model_random), residuals(model_random))


model_random <- lmer(log(annd_values) ~ temperature + I(temperature^2) + group_size + (1|trial) + (1|date) + acclimation, data_annd)
summary(model_random)
plot(fitted(model_random), residuals(model_random))

model_random <- lmer(log(annd_values) ~ temperature + I(temperature^2) + I(group_size^2) + group_size + (1|trial) + (1|date) + acclimation, data_annd)
summary(model_random)
plot(fitted(model_random), residuals(model_random))

model_fixed <- lm(log(annd_values) ~ temperature + I(temperature^2) + I(group_size^2) + group_size + acclimation, data_annd)
summary(model_fixed)
plot(fitted(model_fixed), residuals(model_fixed))
qqnorm(residuals(model_fixed))
qqline(residuals(model_fixed))


#### with time fish in as variable

model_fixed2 <- lm(log(annd_values) ~ temperature + I(temperature^2) + I(group_size^2) + group_size + acclimation + time_in, data_annd)
summary(model_fixed2)
plot(fitted(model_fixed2), residuals(model_fixed2))

## needs to be logged


#interactions?
model_fixed2 <- lm(log(annd_values) ~ temperature*group_size+ I(temperature^2) + I(group_size^2)  + acclimation + time_in, data_annd)
summary(model_fixed2)
plot(fitted(model_fixed2), residuals(model_fixed2))

model_fixed2 <- lm(log(annd_values) ~ group_size+ I(temperature^2) + temperature*I(group_size^2)  + acclimation + time_in, data_annd)
summary(model_fixed2)
plot(fitted(model_fixed2), residuals(model_fixed2))


model_fixed2 <- lm(log(annd_values) ~  I(temperature^2)*group_size+ + temperature + I(group_size^2)  + acclimation + time_in, data_annd)
summary(model_fixed2)
plot(fitted(model_fixed2), residuals(model_fixed2))

model_fixed2 <- lm(log(annd_values) ~  I(temperature^2)*group_size+ + temperature + I(group_size^2)  + acclimation + time_in, data_annd)
summary(model_fixed2)
plot(fitted(model_fixed2), residuals(model_fixed2))


#random

model_fixed2 <- lmer(log(annd_values) ~ temperature + I(temperature^2) + I(group_size^2) + group_size + acclimation + time_in + trial + (1|date), data_annd)
summary(model_fixed2)
plot(fitted(model_fixed2), residuals(model_fixed2))


model_random <- lmer(log(annd_values) ~ temperature + I(temperature^2) + I(group_size^2) + group_size + acclimation + time_in +  (1|date:trial), data_annd)
summary(model_random)
plot(fitted(model_random), residuals(model_random))
##Good model

model_fixed2 <- lm(log(annd_values) ~ temperature + I(temperature^2) + I(group_size^2) + group_size + acclimation + time_in + trial, data_annd)
summary(model_fixed2)
plot(fitted(model_fixed2), residuals(model_fixed2))
#trial is not significant

model_fixed2 <- lm(log(annd_values) ~ temperature + I(1/temperature) + I(1/group_size) + group_size + acclimation, data_annd)
summary(model_fixed2)
plot(fitted(model_fixed2), residuals(model_fixed2))



#glm?
model_glm_random <- glmer(annd_values ~ temperature + I(temperature^2) + I(group_size^2) + group_size + acclimation + time_in +  (1|date:trial), data_annd, family = Gamma)
summary(model_glm_random)
plot(fitted(model_glm_random), residuals(model_glm_random))
#reconsider rescaling

model_glm <- glm(annd_values ~ temperature + I(temperature^2) + I(group_size^2) + group_size + acclimation + time_in, data_annd, family = Gamma)
summary(model_glm)
plot(fitted(model_glm), residuals(model_glm))

X2 <- sum((annd_values - fitted(model_glm))^2 / fitted(model_glm))
## likelihood ratio test
pchisq(X2, df = n - length(coef(model_glm)),
       lower.tail = FALSE)
