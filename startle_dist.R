
startles  <- read.csv("../../data/temp_collective/roi/all_params_w_loom.csv",header=TRUE,na.strings=c("[nan]"))

hist(startles$startle_data_percentile50)
hist(startles$startle_data_percentile90)
hist(startles$startle_data_percentile99)
hist(startles$startle_data_percentile999)
hist(startles$avg_startle_speed)

model1 <- lm(avg_startle_speed ~ Temperature + Groupsize, startles)
summary(model1)
plot(fitted(model1),residuals(model1))
#R squared= 0.01

model2 <- lm(avg_startle_speed ~ Temperature + Groupsize + I(Temperature^2), startles)
summary(model2)
plot(fitted(model2),residuals(model2))
# R squared = 0.058

model3 <- lm(avg_startle_speed ~ Temperature + Groupsize + I(Temperature^2) + I(Groupsize^2), startles)
summary(model3)
plot(fitted(model3),residuals(model3))
# R squared = 0.057

model4 <- lm(avg_startle_speed ~ Temperature + Groupsize + I(Temperature^2) + I(Groupsize^2) + Loom, startles)
summary(model4)
plot(fitted(model4),residuals(model4))
# R squared = 0.057 same as above

hist(log(startles$avg_startle_speed))

model5 <- lm(log(avg_startle_speed) ~ Temperature + Groupsize + I(Temperature^2), startles)
summary(model5)
plot(fitted(model5),residuals(model5))
# R squared = 0.0599

model6 <- lm(log(avg_startle_speed) ~ Temperature + Groupsize + I(Temperature^2) + I(Groupsize^2), startles)
summary(model6)
plot(fitted(model6),residuals(model6))
# R squared = 0.059

model7 <- lm(log(avg_startle_speed) ~ Temperature + Groupsize + I(Temperature^2) + Loom, startles)
summary(model7)
plot(fitted(model7),residuals(model7))
# R squared = 0.06

model8 <- lm(log(avg_startle_speed) ~ Temperature*Groupsize + I(Temperature^2) + Loom, startles)
summary(model8)
plot(fitted(model8),residuals(model8))
# R squared = 0.06


model9 <- lm(log(avg_startle_speed) ~ Temperature*Groupsize*Loom + I(Temperature^2) , startles)
summary(model9)
plot(fitted(model9),residuals(model9))
# R squared = 0.0597


## other variables
# date <- annd$Date
# trial <- annd$Trial
# acclimation <-  as.numeric(as.POSIXct(annd$Time_start_record, format = "%H:%M") - as.POSIXct(annd$Time_fish_in, format = "%H:%M"))
# annd_values <- annd$annd
# log_annd_values <- log(annd_values)
# time_in <- as.POSIXct(annd$Time_fish_in, format = "%H:%M")

model10 <- lm(log(avg_startle_speed) ~ Temperature*Groupsize + I(Temperature^2) + Loom + Trial, startles)
summary(model10)
plot(fitted(model10),residuals(model10))
# R squared = 0.0609

a <- as.numeric(as.POSIXct(startles$Time_start_record, format = "%H:%M"))
b <- as.numeric(as.POSIXct(startles$Time_fish_in, format = "%H:%M"))

model11 <- lm(log(avg_startle_speed) ~ Temperature*Groupsize + I(Temperature^2) + Loom + Trial + a + b, startles)
summary(model11)
plot(fitted(model11),residuals(model11))
# R squared = 0.06075

model12 <- lm(log(avg_startle_speed) ~ Temperature*Groupsize + I(Temperature^2) + Loom + Trial + Subtrial, startles)
summary(model12)
plot(fitted(model12),residuals(model12))
# R squared = 0.0618


qqnorm(residuals(model12))
qqline(residuals(model12))

model13 <- lm(log(startle_data_percentile50) ~ Temperature*Groupsize + I(Temperature^2) + Loom + Trial + Subtrial, startles)
summary(model13)
plot(fitted(model13),residuals(model13))
# R squared = 0.045

model14 <- lm(log(startle_data_percentile90) ~ Temperature*Groupsize + I(Temperature^2) + Loom + Trial + Subtrial, startles)
summary(model14)
plot(fitted(model14),residuals(model14))
# R squared = 0.09

qqnorm(residuals(model14))
qqline(residuals(model14))


model15 <- lm(log(startle_data_percentile99) ~ Temperature*Groupsize + I(Temperature^2) + Loom + Trial + Subtrial, startles)
summary(model15)
plot(fitted(model15),residuals(model15))
# R squared = 0.1579

qqnorm(residuals(model15))
qqline(residuals(model15))

model16 <- lm(log(startle_data_percentile99) ~ Temperature*Groupsize + I(Temperature^2) + I(Groupsize^2) + Loom + Trial + Subtrial, startles)
summary(model16)
plot(fitted(model16),residuals(model16))
# R squared = 0.1577

qqnorm(residuals(model16))
qqline(residuals(model16))


model17 <- lm(log(startle_data_percentile999) ~ Temperature*Groupsize + I(Temperature^2) + Loom + Trial + Subtrial, startles)
summary(model17)
plot(fitted(model17),residuals(model17))
# R squared = 0.1677

qqnorm(residuals(model17))
qqline(residuals(model17))


#without transformation
model18 <- lm(startle_data_percentile999 ~ Temperature*Groupsize + I(Temperature^2) + Loom + Trial + Subtrial, startles)
summary(model18)
plot(fitted(model18),residuals(model18))
# R squared = 0.1733 but residual plot not good

require(MASS)
boxcox(model18, plotit = TRUE)

qqnorm(residuals(model18))
qqline(residuals(model18))

model19 <- lm(I(startle_data_percentile999)^0.5 ~ Temperature*Groupsize + I(Temperature^2) + Loom + Trial + Subtrial, startles)
summary(model19)
plot(fitted(model19),residuals(model19))
# R squared = 0.1716 but residual plot not good

qqnorm(residuals(model19))
qqline(residuals(model19))

model20 <- lm(I(startle_data_percentile999)^2 ~ Temperature*Groupsize + I(Temperature^2) + Loom + Trial + Subtrial, startles)
summary(model20)
plot(fitted(model20),residuals(model20))
# R squared = 0.1733 but residual plot worse!

qqnorm(residuals(model20))
qqline(residuals(model20))


model21 <- lm(log(startle_data_percentile999) ~ Temperature*Groupsize + Loom + I(1/Temperature) + Trial + Subtrial, startles)
summary(model21)
plot(fitted(model21),residuals(model21))
# R squared = 0.1701 but residual bad

qqnorm(residuals(model21))
qqline(residuals(model21))

##Try glm?

model22 <- glm(startle_data_percentile999 ~ Temperature*Groupsize + Loom + I(1/Temperature) + Trial + Subtrial, startles, family = "Gamma")
summary(model22)
n <- length(startles$startle_data_percentile999[!is.na(startles$startle_data_percentile999)])
X2 <- sum((startles$startle_data_percentile999[!is.na(startles$startle_data_percentile999)] - fitted(model22))^2 / fitted(model22))
## likelihood ratio test
pchisq(X2, df = n - length(coef(model22)),
       lower.tail = FALSE)


model23 <- glm(startle_data_percentile999 ~ Temperature*Groupsize + Loom + I(1/Temperature) + Trial + Subtrial, startles, family = "inverse.gaussian")
summary(model23)

n <- length(startles$startle_data_percentile999[!is.na(startles$startle_data_percentile999)])
X2 <- sum((startles$startle_data_percentile999[!is.na(startles$startle_data_percentile999)] - fitted(model23))^2 / fitted(model23))
## likelihood ratio test
pchisq(X2, df = n - length(coef(model23)),
       lower.tail = FALSE)


model24 <- glm(startle_data_percentile999 ~ Temperature*Groupsize + Loom + I(1/Temperature) + Trial + Subtrial, startles, family = "gaussian")
summary(model24)

n <- length(startles$startle_data_percentile999[!is.na(startles$startle_data_percentile999)])
X2 <- sum((startles$startle_data_percentile999[!is.na(startles$startle_data_percentile999)] - fitted(model24))^2 / fitted(model24))
## likelihood ratio test
pchisq(X2, df = n - length(coef(model24)),
       lower.tail = FALSE)


#none of the glms working!



#predictions with model 18
model18 <- lm(startle_data_percentile999 ~ Temperature*Groupsize +I(Temperature^2) + Loom , startles)
summary(model18)
plot(fitted(model18),residuals(model18))


df <- startles[complete.cases(startles),]

model18 <- lm(startle_data_percentile999 ~ Temperature*Groupsize +I(Temperature^2) + Loom , df)
summary(model18)
plot(fitted(model18),residuals(model18))

newData1 <- data.frame(expand.grid(Temperature = seq(from = 9, to = 29, by = 1), Groupsize = c(1,2,4,8,16),Loom = c(1,2,3,4,5)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(model18))
  ymod <- update(model18,ynew ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=3)
results[,1] <- predict(model18,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest[j,],probs = c(0.025))
  results[j,3] <- quantile(yest[j,],probs = c(0.975))
}  
newData1$startle_speed <- results[,1]
newData1$startle_speed025 <- results[,2]
newData1$startle_speed975 <- results[,3]

chknyes <- intersect(which(newData1$Groupsize == 16),which(newData1$Loom == 1))  
chknno <- which(newData1$Groupsize == 1)
temp <- newData1$Temperature[chknyes]

plot(startles$Temperature, startles$startle_data_percentile999, xlab = "Temperature",ylab = "99.9th percentile of startle speed")
lines(temp,results[chknyes,1], lty = "solid", col = "green")
#plot(temp,results[chknyes,1], type = 'l', col = "green", ylim = c(0,40), xlab = "Temperature",ylab = "Startles")
lines(temp,results[chknyes,2], lty = "dashed", col = "green")
lines(temp,results[chknyes,3], lty = "dashed", col = "green")

############# 99

model16 <- lm(startle_data_percentile99 ~ Temperature*Groupsize + I(Temperature^2)  + Loom, df)
summary(model16)
plot(fitted(model16),residuals(model16))


newData1 <- data.frame(expand.grid(Temperature = seq(from = 9, to = 29, by = 1), Groupsize = c(1,2,4,8,16),Loom = c(1,2,3,4,5)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(model16))
  ymod <- update(model16,ynew ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=3)
results[,1] <- predict(model16,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest[j,],probs = c(0.025))
  results[j,3] <- quantile(yest[j,],probs = c(0.975))
}  
newData1$startle_speed <- results[,1]
newData1$startle_speed025 <- results[,2]
newData1$startle_speed975 <- results[,3]

chknyes <- intersect(which(newData1$Groupsize == 16),which(newData1$Loom == 1))  
chknno <- which(newData1$Groupsize == 1)
temp <- newData1$Temperature[chknyes]

plot(startles$Temperature, startles$startle_data_percentile99, xlab = "Temperature",ylab = "99th percentile of startle speed")
lines(temp,results[chknyes,1], lty = "solid", col = "green")
#plot(temp,results[chknyes,1], type = 'l', col = "green", ylim = c(0,40), xlab = "Temperature",ylab = "Startles")
lines(temp,results[chknyes,2], lty = "dashed", col = "green")
lines(temp,results[chknyes,3], lty = "dashed", col = "green")


###speed$one_kT <- 1/(0.00008617 * (speed$temp +273.1))
