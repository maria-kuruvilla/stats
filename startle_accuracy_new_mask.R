setwd("~/Documents/code/stats")
data_startles <- read_csv("../../data/temp_collective/roi/stats_startles_ratio_new_mask.csv")

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

### normalized accuracy

