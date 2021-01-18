setwd("~/code/stats")
annd <- read.csv("../../data/temp_collective/roi/stats_annd_data.csv",header=TRUE,na.strings=c("[nan]"))

temperature <- annd$Temperature
group_size <- annd$Groupsize
date <- annd$Date
trial <- annd$Trial
acclimation <-  as.numeric(as.POSIXct(annd$Time_start_record, format = "%H:%M") - as.POSIXct(annd$Time_fish_in, format = "%H:%M"))
annd_values <- annd$annd
log_annd_values <- log(annd_values)
      
hist(annd_values,30)


n <- length(annd$annd)
data_annd <- as.data.frame(cbind(sample = (1:n),as.numeric(temperature), as.numeric(group_size), as.numeric(trial), date, as.numeric(acclimation), as.numeric(log_annd_values), as.numeric((annd_values))))



model1 <- lm(log_annd_values ~ temperature + group_size + as.factor(date) + acclimation, data_annd)
summary(model1)

model2 <- lm(log_annd_values ~ temperature + group_size + acclimation, data_annd)
summary(model2)

plot(fitted(model2), residuals(model2))

qqnorm(residuals(model2))
qqline(residuals(model2))


model3 <- lm(log_annd_values ~ I(temperature^2) + group_size + acclimation, data_annd)
summary(model3)

plot(fitted(model3), residuals(model3))

model4 <- lm(log_annd_values ~ temperature*group_size + acclimation, data_annd)
summary(model4)

plot(fitted(model4), residuals(model4))

qqnorm(residuals(model3))
qqline(residuals(model3))

############### without log ##############
model5<- lm(annd_values ~ temperature + group_size + trial + acclimation, data_annd)
summary(model5)

plot(fitted(model5), residuals(model5))

require(MASS)
boxcox(model5,plotit=TRUE) 

model6 <- lm(annd_values^(-0.5) ~ temperature + group_size + trial + acclimation, data_annd)
summary(model6)

plot(fitted(model6), residuals(model6))

qqnorm(residuals(model6))
qqline(residuals(model6))

shapiro.test(residuals(model6))