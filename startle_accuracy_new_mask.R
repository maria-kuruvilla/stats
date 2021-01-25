setwd("~/Documents/code/stats")
data_startles <- read.csv("../../data/temp_collective/roi/stats_startles_ratio_new_mask.csv")

accuracy <- data_startles$startles_ratio
accuracy_norm <- data_startles$startles_ratio_normalized

hist(accuracy)
hist(accuracy_norm)

temp <- data_startles$Temperature
gs <- data_startles$Groupsize
date <- as.numeric(as.Date(data_startles$Date, format = "%d/%m/%Y"))
time_in <- as.numeric(as.POSIXct(data_startles$Time_fish_in, format = "%H:%M"))
time_record <- as.numeric(as.POSIXct(data_startles$Time_start_record, format = "%H:%M"))
difference <- time_record - time_in
trial <- data_startles$Trial
subtrial <- data_startles$Subtrial

model_linear <- lm(accuracy~ temp + gs)
summary(model_linear)
plot(fitted(model_linear),residuals(model_linear))
qqnorm(residuals(model_linear))
qqline(residuals(model_linear))

var.test(accuracy[temp==9],accuracy[temp==29])
# F test shows that variance is not equal

model0 <- glm(accuracy ~ 1, family = binomial,data_startles)
summary(model0)
#aic = 318

model1 <- glm(accuracy ~ temp + gs, family = binomial,data_startles)
summary(model1)
#aic = 306

model2 <- glm(accuracy ~ temp*gs, family = binomial,data_startles)
summary(model2)
#aic = 308

model3 <- glm(accuracy ~ temp + gs + date, family = binomial,data_startles)
summary(model3)
#aic = 308

model4 <- glm(accuracy ~ temp + gs + difference, family = binomial,data_startles)
summary(model4)
#aic = 306


model5 <- glm(accuracy ~ temp + gs + time_in, family = binomial,data_startles)
summary(model5)
#aic = 307

model6 <- glm(accuracy ~ temp + gs + trial, family = binomial,data_startles)
summary(model6)
#aic = 308


library(arm)
binnedplot(fitted(model1),residuals(model1)) 

model_glmm_1 <- glmer(accuracy ~ temp + gs +(1|trial), family = binomial,data_startles)
summary(model_glmm_1)
#aic = 301, variance = 0 

model_glmm_2 <- glmer(accuracy ~ temp + gs +(1|date), family = binomial,data_startles)
summary(model_glmm_2)
#aic = 301 #bic = 314

model_glmm_3 <- glmer(accuracy ~ temp + gs +(1|date:trial), family = binomial,data_startles)
summary(model_glmm_3)
#aic = 301 #bic = 315, variance = 0

model_glmm_4 <- glmer(accuracy ~ temp + gs +(1|date)+(1|trial), family = binomial,data_startles)
summary(model_glmm_4)
#aic = 303 #bic = 315


model_glmm_5 <- glmer(accuracy ~ temp + gs +(1|date)+(1|trial:subtrial), family = binomial,data_startles)
summary(model_glmm_5)
#aic = 303 #bic = 320

### normalized startles_during_loom

data_startles_loom <- read.csv("../../data/temp_collective/roi/stats_startles_during_loom_normalized.csv")
temp <- data_startles_loom$Temperature
gs <- data_startles_loom$Groupsize
trial <- data_startles_loom$Trial

date <- as.numeric(as.Date(data_startles_loom$Date, format = "%d/%m/%Y"))
time_in <- as.numeric(as.POSIXct(data_startles_loom$Time_fish_in, format = "%H:%M"))
time_record <- as.numeric(as.POSIXct(data_startles_loom$Time_start_record, format = "%H:%M"))
difference <- time_record - time_in
subtrial <- data_startles_loom$Subtrial
loom <- data_startles_loom$Loom
startles <- data_startles_loom$startles_during_loom

model1 <- lm((startles+1)~temp+gs+loom, data_startles_loom)
summary(model1)
plot(fitted(model1),residuals(model1))
qqnorm(residuals(model1))
qqline(residuals(model1))

require(MASS)
boxcox(model1,plotit=TRUE)



model2 <- lm(log(startles+1)~ I(temp^2)+ temp +gs+loom, data_startles_loom)
summary(model2)
plot(fitted(model2),residuals(model2))
qqnorm(residuals(model2))
qqline(residuals(model2))

model_glm <- glm(startles ~ temp + gs + loom, family = poisson, data_startles_loom)
summary(model_glm)



data_startles_loom <- read.csv("../../data/temp_collective/roi/stats_startles_during_loom.csv")
temp <- data_startles_loom$Temperature
gs <- data_startles_loom$Groupsize
trial <- data_startles_loom$Trial

date <- as.numeric(as.Date(data_startles_loom$Date, format = "%d/%m/%Y"))
time_in <- as.numeric(as.POSIXct(data_startles_loom$Time_fish_in, format = "%H:%M"))
time_record <- as.numeric(as.POSIXct(data_startles_loom$Time_start_record, format = "%H:%M"))
difference <- time_record - time_in
subtrial <- data_startles_loom$Subtrial
loom <- data_startles_loom$Loom
startles <- data_startles_loom$startles_during_loom

model_glm <- glm(startles ~ temp + gs + loom, family = poisson, data_startles_loom)
summary(model_glm)


