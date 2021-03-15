prop_data_new <- read.csv("../../data/temp_collective/roi/stats_prop_ind_startles_no_nan_new_mask.csv",header=TRUE,na.strings=c("[nan]"))

model_1 <- lm(prop_startles10 ~ Temperature + Groupsize + Loom, prop_data_new)
summary(model_1)
plot(fitted(model_1), residuals(model_1))

hist(prop_data_new$prop_startles10_40)

date <- as.numeric(as.Date(prop_data_new$Date, format = "%d/%m/%Y"))
time_in <- as.numeric(as.POSIXct(prop_data_new$Time_fish_in, format = "%H:%M"))
time_record <- as.numeric(as.POSIXct(prop_data_new$Time_start_record, format = "%H:%M"))
difference <- time_record - time_in

model_glm1 <-  glm(prop_startles10 ~ 1, family = binomial,prop_data_new)
summary(model_glm1)
#aic = 1124

model_glm2 <-  glm(prop_startles10 ~ Temperature, family = binomial,prop_data_new)
summary(model_glm2)
#aic = 1106

model_glm3 <-  glm(prop_startles10 ~ I(Temperature^2), family = binomial,prop_data_new)
summary(model_glm3)
#aic = 1099


model_glm4 <-  glm(prop_startles10 ~ Temperature + I(Temperature^2), family = binomial,prop_data_new)
summary(model_glm4)
#aic = 1087

model_glm5 <-  glm(prop_startles10 ~ Temperature + I(Temperature^2) + Groupsize, family = binomial,prop_data_new)
summary(model_glm5)
#aic = 1088

model_glm6 <-  glm(prop_startles10 ~ Temperature + I(Temperature^2) + Loom, family = binomial,prop_data_new)
summary(model_glm6)
#aic = 1079

model_glm61 <-  glm(prop_startles10 ~ Temperature*Groupsize + I(Temperature^2) + Loom, family = binomial,prop_data_new)
summary(model_glm61)
#aic = 1083

model_glm62 <-  glm(prop_startles10 ~ Temperature + I(Temperature^2)*Groupsize + Loom, family = binomial,prop_data_new)
summary(model_glm62)
#aic = 1083

model_glm7 <-  glm(prop_startles10 ~ Temperature*Loom + I(Temperature^2), family = binomial,prop_data_new)
summary(model_glm7)
#aic = 1079


model_glm8 <-  glm(prop_startles10 ~ Temperature + I(Temperature^2)*Loom, family = binomial,prop_data_new)
summary(model_glm8)
#aic = 1080

require(arm)
binnedplot(predict(model_glm6),resid(model_glm6))

##senstitivity analysis

model_glm9 <-  glm(prop_startles7 ~ Temperature + I(Temperature^2) + Loom, family = binomial,prop_data_new)
summary(model_glm6)
#aic = 1078

require(arm)
binnedplot(predict(model_glm9),resid(model_glm9))

model_glm10 <-  glm(prop_startles10_40 ~ Temperature + I(Temperature^2) + Loom, family = binomial,prop_data_new)
summary(model_glm10)
#aic = 1080

require(arm)
binnedplot(predict(model_glm10),resid(model_glm10))

model_glm11 <-  glm(prop_startles7_40 ~ Temperature + I(Temperature^2) + Loom, family = binomial,prop_data_new)
summary(model_glm11)
#aic = 1202

require(arm)
binnedplot(predict(model_glm11),resid(model_glm11))


#predictions

## not sure
mydata <- data.frame(expand.grid(Temperature=seq(9,29,1), Loom = c(1,2,3,4,5)))
pred <- predict.glm(model_glm6,newdata = mydata,type = "response", se=T)
f <- exp(pred$fit)
plot(prop_data_new$Temperature, prop_data_new$prop_startles10)
lines(mydata$Temperature,pred$fit)
##

newData1 <- data.frame(expand.grid(Temperature = seq(from = 9, to = 29, by = 1), Loom = c(1,2,3,4,5)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(model_glm6))
  ymod <- update(model_glm6,ynew ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=3)
results[,1] <- predict(model_glm6,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest[j,],probs = c(0.025))
  results[j,3] <- quantile(yest[j,],probs = c(0.975))
}  

chknyes <- which(newData1$Loom == 1) 
#chknno <- which(newData1$Groupsize == 1)
temp <- newData1$Temperature[chknyes]
plot(prop_data_new$Temperature, prop_data_new$prop_startles10, xlab = "Temperature",ylab = "Proportion of individuals that startle")
lines(temp,results[chknyes,1], lty = "solid", col = "green")
#plot(temp,results[chknyes,1], type = 'l', col = "green", ylim = c(0,40), xlab = "Temperature",ylab = "Startles")
lines(temp,results[chknyes,2], lty = "dashed", col = "green")
lines(temp,results[chknyes,3], lty = "dashed", col = "green")

chknyes1 <- which(newData1$Loom == 5) 
#chknno <- which(newData1$Groupsize == 1)
temp <- newData1$Temperature[chknyes1]
#plot(prop_data_new$Temperature, prop_data_new$prop_startles10, xlab = "Temperature",ylab = "Proportion of individuals that startle")


library("viridis")     

c = viridis(5)


library(scales)

plot(prop_data_new$Temperature[prop_data_new$Loom==1], prop_data_new$prop_startles10[prop_data_new$Loom==1], xlab = "Temperature",ylab = "Proportion of individuals that startle", col = alpha(c[1],0.5), pch = 16,main = "Startles threshold = 10 BL/s, Speed threshold = 30 BL/s")
points(prop_data_new$Temperature[prop_data_new$Loom==2], prop_data_new$prop_startles10[prop_data_new$Loom==2], xlab = "Temperature",ylab = "Proportion of individuals that startle", col = alpha(c[2],0.5), pch = 16)
points(prop_data_new$Temperature[prop_data_new$Loom==3], prop_data_new$prop_startles10[prop_data_new$Loom==3], xlab = "Temperature",ylab = "Proportion of individuals that startle", col = alpha(c[3],0.5), pch = 16)
points(prop_data_new$Temperature[prop_data_new$Loom==4], prop_data_new$prop_startles10[prop_data_new$Loom==4], xlab = "Temperature",ylab = "Proportion of individuals that startle", col = alpha(c[4],0.5), pch = 16)
points(prop_data_new$Temperature[prop_data_new$Loom==5], prop_data_new$prop_startles10[prop_data_new$Loom==5], xlab = "Temperature",ylab = "Proportion of individuals that startle", col = alpha(c[5],0.5), pch = 16)

lines(temp,results[chknyes,1], lty = "solid", col = c[1])
#plot(temp,results[chknyes,1], type = 'l', col = "green", ylim = c(0,40), xlab = "Temperature",ylab = "Startles")
lines(temp,results[chknyes,2], lty = "dashed", col = c[1])
lines(temp,results[chknyes,3], lty = "dashed", col = c[1])

lines(temp,results[chknyes1,1], lty = "solid", col = c[5])
#plot(temp,results[chknyes,1], type = 'l', col = "green", ylim = c(0,40), xlab = "Temperature",ylab = "Startles")
lines(temp,results[chknyes1,2], lty = "dashed", col = c[5])
lines(temp,results[chknyes1,3], lty = "dashed", col = c[5])
legend("topright", legend = c("Loom 1", "Loom 5"), col = c(c[1],c[5]), lty =1)



#Predictions for 7_30

newData1 <- data.frame(expand.grid(Temperature = seq(from = 9, to = 29, by = 1), Loom = c(1,2,3,4,5)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(model_glm9))
  ymod <- update(model_glm9,ynew ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=3)
results[,1] <- predict(model_glm9,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest[j,],probs = c(0.025))
  results[j,3] <- quantile(yest[j,],probs = c(0.975))
}  

chknyes <- which(newData1$Loom == 1) 
#chknno <- which(newData1$Groupsize == 1)
temp <- newData1$Temperature[chknyes]



plot(prop_data_new$Temperature[prop_data_new$Loom==1], prop_data_new$prop_startles7[prop_data_new$Loom==1], xlab = "Temperature",ylab = "Proportion of individuals that startle", col = alpha(c[1],0.5), pch = 16,main = "Startles threshold = 7 BL/s, Speed threshold = 30 BL/s")
points(prop_data_new$Temperature[prop_data_new$Loom==2], prop_data_new$prop_startles7[prop_data_new$Loom==2], xlab = "Temperature",ylab = "Proportion of individuals that startle", col = alpha(c[2],0.5), pch = 16)
points(prop_data_new$Temperature[prop_data_new$Loom==3], prop_data_new$prop_startles7[prop_data_new$Loom==3], xlab = "Temperature",ylab = "Proportion of individuals that startle", col = alpha(c[3],0.5), pch = 16)
points(prop_data_new$Temperature[prop_data_new$Loom==4], prop_data_new$prop_startles7[prop_data_new$Loom==4], xlab = "Temperature",ylab = "Proportion of individuals that startle", col = alpha(c[4],0.5), pch = 16)
points(prop_data_new$Temperature[prop_data_new$Loom==5], prop_data_new$prop_startles7[prop_data_new$Loom==5], xlab = "Temperature",ylab = "Proportion of individuals that startle", col = alpha(c[5],0.5), pch = 16)

lines(temp,results[chknyes,1], lty = "solid", col = c[1])
#plot(temp,results[chknyes,1], type = 'l', col = "green", ylim = c(0,40), xlab = "Temperature",ylab = "Startles")
lines(temp,results[chknyes,2], lty = "dashed", col = c[1])
lines(temp,results[chknyes,3], lty = "dashed", col = c[1])

chknyes1 <- which(newData1$Loom == 5) 
#chknno <- which(newData1$Groupsize == 1)
temp <- newData1$Temperature[chknyes1]
#plot(prop_data_new$Temperature, prop_data_new$prop_startles10, xlab = "Temperature",ylab = "Proportion of individuals that startle")

lines(temp,results[chknyes1,1], lty = "solid", col = c[5])
#plot(temp,results[chknyes,1], type = 'l', col = "green", ylim = c(0,40), xlab = "Temperature",ylab = "Startles")
lines(temp,results[chknyes1,2], lty = "dashed", col = c[5])
lines(temp,results[chknyes1,3], lty = "dashed", col = c[5])
legend("topright", legend = c("Loom 1", "Loom 5"), col = c(c[1],c[5]), lty =1)


#Predictions for 10_40

newData1 <- data.frame(expand.grid(Temperature = seq(from = 9, to = 29, by = 1), Loom = c(1,2,3,4,5)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(model_glm10))
  ymod <- update(model_glm10,ynew ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=3)
results[,1] <- predict(model_glm10,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest[j,],probs = c(0.025))
  results[j,3] <- quantile(yest[j,],probs = c(0.975))
}  

chknyes <- which(newData1$Loom == 1) 
#chknno <- which(newData1$Groupsize == 1)
temp <- newData1$Temperature[chknyes]



plot(prop_data_new$Temperature[prop_data_new$Loom==1], prop_data_new$prop_startles10_40[prop_data_new$Loom==1], xlab = "Temperature",ylab = "Proportion of individuals that startle", col = alpha(c[1],0.5), pch = 16,main = "Startles threshold = 10 BL/s, Speed threshold = 40 BL/s")
points(prop_data_new$Temperature[prop_data_new$Loom==2], prop_data_new$prop_startles10_40[prop_data_new$Loom==2], xlab = "Temperature",ylab = "Proportion of individuals that startle", col = alpha(c[2],0.5), pch = 16)
points(prop_data_new$Temperature[prop_data_new$Loom==3], prop_data_new$prop_startles10_40[prop_data_new$Loom==3], xlab = "Temperature",ylab = "Proportion of individuals that startle", col = alpha(c[3],0.5), pch = 16)
points(prop_data_new$Temperature[prop_data_new$Loom==4], prop_data_new$prop_startles10_40[prop_data_new$Loom==4], xlab = "Temperature",ylab = "Proportion of individuals that startle", col = alpha(c[4],0.5), pch = 16)
points(prop_data_new$Temperature[prop_data_new$Loom==5], prop_data_new$prop_startles10_40[prop_data_new$Loom==5], xlab = "Temperature",ylab = "Proportion of individuals that startle", col = alpha(c[5],0.5), pch = 16)

lines(temp,results[chknyes,1], lty = "solid", col = c[1])
#plot(temp,results[chknyes,1], type = 'l', col = "green", ylim = c(0,40), xlab = "Temperature",ylab = "Startles")
lines(temp,results[chknyes,2], lty = "dashed", col = c[1])
lines(temp,results[chknyes,3], lty = "dashed", col = c[1])

chknyes1 <- which(newData1$Loom == 5) 
#chknno <- which(newData1$Groupsize == 1)
temp <- newData1$Temperature[chknyes1]
#plot(prop_data_new$Temperature, prop_data_new$prop_startles10, xlab = "Temperature",ylab = "Proportion of individuals that startle")

lines(temp,results[chknyes1,1], lty = "solid", col = c[5])
#plot(temp,results[chknyes,1], type = 'l', col = "green", ylim = c(0,40), xlab = "Temperature",ylab = "Startles")
lines(temp,results[chknyes1,2], lty = "dashed", col = c[5])
lines(temp,results[chknyes1,3], lty = "dashed", col = c[5])
legend("topright", legend = c("Loom 1", "Loom 5"), col = c(c[1],c[5]), lty =1)


#Predictions for 7_40

newData1 <- data.frame(expand.grid(Temperature = seq(from = 9, to = 29, by = 1), Loom = c(1,2,3,4,5)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(model_glm11))
  ymod <- update(model_glm11,ynew ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=3)
results[,1] <- predict(model_glm11,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest[j,],probs = c(0.025))
  results[j,3] <- quantile(yest[j,],probs = c(0.975))
}  

chknyes <- which(newData1$Loom == 1) 
#chknno <- which(newData1$Groupsize == 1)
temp <- newData1$Temperature[chknyes]



plot(prop_data_new$Temperature[prop_data_new$Loom==1], prop_data_new$prop_startles7_40[prop_data_new$Loom==1], xlab = "Temperature",ylab = "Proportion of individuals that startle", col = alpha(c[1],0.5), pch = 16,main = "Startles threshold = 7 BL/s, Speed threshold = 40 BL/s")
points(prop_data_new$Temperature[prop_data_new$Loom==2], prop_data_new$prop_startles7_40[prop_data_new$Loom==2], xlab = "Temperature",ylab = "Proportion of individuals that startle", col = alpha(c[2],0.5), pch = 16)
points(prop_data_new$Temperature[prop_data_new$Loom==3], prop_data_new$prop_startles7_40[prop_data_new$Loom==3], xlab = "Temperature",ylab = "Proportion of individuals that startle", col = alpha(c[3],0.5), pch = 16)
points(prop_data_new$Temperature[prop_data_new$Loom==4], prop_data_new$prop_startles7_40[prop_data_new$Loom==4], xlab = "Temperature",ylab = "Proportion of individuals that startle", col = alpha(c[4],0.5), pch = 16)
points(prop_data_new$Temperature[prop_data_new$Loom==5], prop_data_new$prop_startles7_40[prop_data_new$Loom==5], xlab = "Temperature",ylab = "Proportion of individuals that startle", col = alpha(c[5],0.5), pch = 16)

lines(temp,results[chknyes,1], lty = "solid", col = c[1])
#plot(temp,results[chknyes,1], type = 'l', col = "green", ylim = c(0,40), xlab = "Temperature",ylab = "Startles")
lines(temp,results[chknyes,2], lty = "dashed", col = c[1])
lines(temp,results[chknyes,3], lty = "dashed", col = c[1])

chknyes1 <- which(newData1$Loom == 5) 
#chknno <- which(newData1$Groupsize == 1)
temp <- newData1$Temperature[chknyes1]
#plot(prop_data_new$Temperature, prop_data_new$prop_startles10, xlab = "Temperature",ylab = "Proportion of individuals that startle")

lines(temp,results[chknyes1,1], lty = "solid", col = c[5])
#plot(temp,results[chknyes,1], type = 'l', col = "green", ylim = c(0,40), xlab = "Temperature",ylab = "Startles")
lines(temp,results[chknyes1,2], lty = "dashed", col = c[5])
lines(temp,results[chknyes1,3], lty = "dashed", col = c[5])
legend("topright", legend = c("Loom 1", "Loom 5"), col = c(c[1],c[5]), lty =1)

ggplot(aes(Temperature, prop_startles10), data = prop_data_new) +
  geom_point() +
  geom_line(aes(temp,chknyes))

