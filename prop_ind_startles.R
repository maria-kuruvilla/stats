prop_data <- read.csv("../../data/temp_collective/roi/stats_prop_ind_startles_new_mask.csv",header=TRUE,na.strings=c("[nan]"))

model_1 <- lm(prop_startles10 ~ Temperature + Groupsize + Loom, prop_data)
summary(model_1)
plot(fitted(model_1), residuals(model_1))

model_2 <- lm(prop_startles10 ~ Temperature + I(Temperature^2) +Groupsize + Loom, prop_data)
summary(model_2)
plot(fitted(model_2), residuals(model_2))

qqnorm(residuals(model_2))
qqline(residuals(model_2))

date <- as.numeric(as.Date(prop_data$Date, format = "%d/%m/%Y"))
time_in <- as.numeric(as.POSIXct(prop_data$Time_fish_in, format = "%H:%M"))
time_record <- as.numeric(as.POSIXct(prop_data$Time_start_record, format = "%H:%M"))
difference <- time_record - time_in

model_glm1 <-  glm(prop_startles10 ~ 1, family = binomial,prop_data)
summary(model_glm1)
#aic = 992

model_glm2 <-  glm(prop_startles10 ~ Temperature, family = binomial,prop_data)
summary(model_glm2)
#aic = 985

model_glm22 <-  glm(prop_startles10 ~ I(1/Temperature), family = binomial,prop_data)
summary(model_glm22)
#aic = 992

model_glm3 <-  glm(prop_startles10 ~ I(Temperature^2), family = binomial,prop_data)
summary(model_glm3)
#aic = 982

model_glm4 <-  glm(prop_startles10 ~ Temperature + I(Temperature^2), family = binomial,prop_data)
summary(model_glm4)
#aic = 973

model_glm5 <-  glm(prop_startles10 ~ Temperature + I(Temperature^2) +Groupsize, family = binomial,prop_data)
summary(model_glm5)
#aic = 976

model_glm6 <-  glm(prop_startles10 ~ Temperature + I(Temperature^2) +Groupsize + Loom, family = binomial,prop_data)
summary(model_glm6)
#aic = 970


model_glm7 <-  glm(prop_startles10 ~ Temperature*Loom + I(Temperature^2) +Groupsize, family = binomial,prop_data)
summary(model_glm7)
#aic = 972

model_glm8 <-  glm(prop_startles10 ~ Temperature + I(Temperature^2)*Loom +Groupsize, family = binomial,prop_data)
summary(model_glm8)
#aic = 973

model_glm9 <-  glm(prop_startles10 ~ Temperature + I(Temperature^2) +Groupsize*Loom, family = binomial,prop_data)
summary(model_glm9)
#aic = 972


model_glm10 <-  glm(prop_startles10 ~ Temperature + I(Temperature^2)*Groupsize + Loom, family = binomial,prop_data)
summary(model_glm10)
#aic = 972.4

model_glm11 <-  glm(prop_startles10 ~ Temperature*Groupsize + I(Temperature^2) + Loom, family = binomial,prop_data)
summary(model_glm11)
#aic = 973

require(arm)
binnedplot(predict(model_glm6),resid(model_glm6))


#predictions

mydata <- data.frame(Temperature=seq(9,29,1))
pred <- predict.glm(model_glm,newdata = mydata,type = "response", se=T)
f <- exp(pred$fit)
plot(prop_data$Temperature, prop_data$prop_startles10)

newData1 <- data.frame(expand.grid(Temperature = seq(from = 9, to = 29, by = 1), Groupsize = c(1,2,4,8,16),Loom = c(1,2,3,4,5)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(model_glm6))
  ymod <- update(model_glm6,ynew ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=3)
results[,1] <- predict(model_glm_7,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest[j,],probs = c(0.025))
  results[j,3] <- quantile(yest[j,],probs = c(0.975))
}  
chknyes <- interesect(which(newData1$Groupsize == 16),which(newData1$Loom == 1))  
chknno <- which(newData1$Groupsize == 1)
temp <- newData1$Temperature[chknyes]
plot(data1$Temperature, data1$startles_during_loom, xlab = "Temperature",ylab = "Startles during loom")
lines(temp,results[chknyes,1], lty = "solid", col = "green")
#plot(temp,results[chknyes,1], type = 'l', col = "green", ylim = c(0,40), xlab = "Temperature",ylab = "Startles")
lines(temp,results[chknyes,2], lty = "dashed", col = "green")
lines(temp,results[chknyes,3], lty = "dashed", col = "green")