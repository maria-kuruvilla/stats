setwd("~/Documents/code/stats")
data1 <- read.csv("../../data/temp_collective/roi/stats_startles_during_loom_unnormalized16_new.csv")

startles <- data1$startles_during_loom
hist(startles,40)

temp <- data1$Temperature
gs <- data1$Groupsize
rep <- data1$Replicate
trial <- data1$Trial
date <- as.numeric(as.Date(data1$Date, format = "%d/%m/%Y"))
time_in <- as.numeric(as.POSIXct(data1$Time_fish_in, format = "%H:%M"))
time_record <- as.numeric(as.POSIXct(data1$Time_start_record, format = "%H:%M"))
difference <- time_record - time_in
subtrial <- data1$Subtrial
loom <- data1$Loom

model_glm_1 <- glm(startles ~ temp + gs + loom , family = poisson,data1)
summary(model_glm_1)
#aic = 5678

library(arm)
binnedplot(fitted(model_glm_1),residuals(model_glm_1)) 

binnedplot(loom,residuals(model_glm_1)) 

model_glm_2 <- glm(startles ~ I(temp^2) + gs + loom , family = poisson,data1)
summary(model_glm_2)
#aic = 5631

binnedplot(fitted(model_glm_2),residuals(model_glm_2)) 

model_glm_4 <- glm(startles ~ temp + I(temp^2) + gs + loom , family = poisson,data1)
summary(model_glm_4)
#aic = 5556

binnedplot(fitted(model_glm_3),residuals(model_glm_3)) 

model_glm_5 <- glm(startles ~ temp + I(temp^2) + I(1/gs) + loom , family = poisson,data1)
summary(model_glm_5)
#aic = 5720

model_glm_6 <- glm(startles ~ temp + I(temp^2) +  I(gs^2) + loom , family = poisson,data1)
summary(model_glm_6)
#aic = 5839

model_glm_7 <- glm(startles ~ temp + I(temp^2) + gs+  I(gs^2) + loom , family = poisson,data1)
summary(model_glm_7)
#aic = 5394

binnedplot(fitted(model_glm_7),residuals(model_glm_7)) 

model_glm <- glm(startles ~ temp + I(temp^2) + gs+  I(gs^2) + loom , family = poisson)
summary(model_glm)

plot(temp,startles)

model_glm_7 <- glm(startles_during_loom ~ I(Temperature^2) + Temperature + Groupsize + I(Groupsize^2) + Loom, family = poisson, data1)
mydata <- data.frame(Temperature=seq(9,29,1))
pred <- predict.glm(model_glm,newdata = mydata,type = "link", se=T)
f <- exp(pred$fit)
lines(mydata$Temperature,f,lty = 1)

newData1 <- data.frame(expand.grid(Temperature = seq(from = 9, to = 29, by = 1), Groupsize = c(1,2,4,8,16),Loom = c(1,2,3,4,5)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(model_glm_7))
  ymod <- update(model_glm_7,ynew ~ .)
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
#lines(temp,results[chknno,1], lty = "solid", col = "red")
#lines(temp,results[chknno,2], lty = "dashed", col = "red")
#lines(temp,results[chknno,3], lty = "dashed", col = "red")


