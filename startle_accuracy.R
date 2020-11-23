data <- read.csv("../../data/temp_collective/roi/startles_ratio_csv.csv",header=TRUE,na.strings=c("[nan]"))
accuracy <- data$accuracy
hist(accuracy)
temp <- data$Temperature
gs <- data$Groupsize
date <- as.numeric(as.Date(data$Date, format = "%d/%m/%Y"))
time_in <- as.numeric(as.POSIXct(data$Time_fish_in, format = "%H:%M"))
time_record <- as.numeric(as.POSIXct(data$Time_start_record, format = "%H:%M"))
difference <- time_record - time_in

model1 <- lm(accuracy ~ temp + gs,data)
summary(model1)
plot(fitted(model1), residuals(model1))
qqnorm(residuals(model1))
qqline(residuals(model1))

require(MASS)

#boxcox(model1,plotit=TRUE)

model2 <- lm(accuracy ~ temp + gs + I(temp^2),data)
summary(model2)


model3 <- lm(accuracy ~ temp + gs + date + difference,data)
summary(model3)
plot(fitted(model3), residuals(model3)^2)
qqnorm(residuals(model3))
qqline(residuals(model3))


model3 <- lm(accuracy ~ temp + gs + date + difference,data)
summary(model3)
plot(fitted(model3), residuals(model3)^2)
qqnorm(residuals(model3))
qqline(residuals(model3))


data1 <- read.csv("../../data/temp_collective/roi/startles_ratio_zero.csv",header=TRUE,na.strings=c("[nan]"))
accuracy1 <- data1$accuracy
temp1 <- data1$Temperature
gs1 <- data1$Groupsize
date1 <- as.numeric(as.Date(data1$Date, format = "%d/%m/%Y"))
time_in1 <- as.numeric(as.POSIXct(data1$Time_fish_in, format = "%H:%M"))
time_record1 <- as.numeric(as.POSIXct(data1$Time_start_record, format = "%H:%M"))
difference1 <- time_record - time_in

model4 <- lm(accuracy1 ~ temp1 + gs1,data1)
summary(model4)
plot(fitted(model4), residuals(model4))
qqnorm(residuals(model4))
qqline(residuals(model4))

require(MASS)

boxcox(model4,plotit=TRUE)

model5 <- lm(accuracy1^(3/2) ~ temp1 + gs1,data1)
summary(model5)
plot(fitted(model5), residuals(model5))
qqnorm(residuals(model5))
qqline(residuals(model5))

model6 <- lm(accuracy^(3/2) ~ temp + gs,data)
summary(model6)
plot(fitted(model6), residuals(model6))
qqnorm(residuals(model6))
qqline(residuals(model6))

model7 <- lm(accuracy ~ temp*gs,data)
summary(model7)
plot(fitted(model7), residuals(model7))
qqnorm(residuals(model7))
qqline(residuals(model7))

model8 <- lm(accuracy1 ~ temp1*gs1,data1)
summary(model8)
plot(fitted(model8), residuals(model8))
qqnorm(residuals(model8))
qqline(residuals(model8))

require(MASS)

boxcox(model8,plotit=TRUE)

model9 <- lm(accuracy1^(3/2) ~ temp1*gs1,data1)
summary(model9)
plot(fitted(model9), residuals(model9))
qqnorm(residuals(model9))
qqline(residuals(model9))

model10 <- glm(accuracy ~ temp + gs, family = binomial,data)
summary(model10)

model11 <- glm(accuracy ~ temp*gs, family = binomial,data)
summary(model11)