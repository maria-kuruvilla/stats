#startles_during_loom


data1 <- read.csv("../../data/temp_collective/roi/stats_startles_during_loom_unnormalized16_new.csv")

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


#latency

data <- read.csv("../../data/temp_collective/roi/stats_loom_latency_nan.csv",header=TRUE,na.strings=c("[nan]"))


model_pois6 <- glm(latency ~ Temperature + Groupsize*loom + I(Temperature^2), family = quasipoisson, data)
summary(model_pois6)

newdata3 <- data.frame(Temperature = seq(9,29,1), Groupsize = 16, loom = 1)
preds3 <- predict(model_pois6,newdata3, type = "response")
plot(data$Temperature,data$latency)
newdata2 <- data.frame(Temperature = seq(9,29,1), Groupsize = 1, Loom = 1)
preds2 <- predict(model_pois6,newdata2, type = "response")
lines(newdata2$Temperature,preds2)
lines(newdata3$Temperature,preds3, col = "green")

newData1 <- data.frame(expand.grid(Temperature = seq(from = 9, to = 29, by = 1), Groupsize = c(1,2,4,8,16),loom = c(1,2,3,4,5)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(model_pois6))
  ymod <- update(model_pois6,ynew ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=3)
results[,1] <- predict(model_pois6,newData1, type = "response")
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


#loom_speed_percentiles
percentiles <- read.csv("../../data/temp_collective/roi/stats_loom_low_pass_data.csv",header=TRUE,na.strings=c("[nan]"))
model_speed99 <- lm((speed99)^0.5 ~  temperature  +I(temperature^2)+ log(group_size,2) + I(log(group_size,2)^2) + loom + replicate,data)
summary(model_speed99)

model_99 <- lm((X99_speed)^0.5 ~ Temperature + I(Temperature^2) + log(Groupsize,2) + loom + Replicate +I(log(Groupsize,2)^2),percentiles)
summary(model_99)
model_99$coefficients

x <- seq(9,29,1)
y <- model_99$coefficients[1] + model_99$coefficients[2]*x + model_99$coefficients[3]*x^2
lines(x,y)

model1 <- lm((X99_speed)^0.5 ~ Temperature + I(Temperature^2),percentiles)
summary(mode1)
plot(fitted(model1), residuals(model1))

x <- seq(9,29,1)
y <- model1$coefficients[1] + model1$coefficients[2]*x + model1$coefficients[3]*x^2
plot(percentiles$Temperature, percentiles$X99_speed^0.5, xlab = "Temperature", ylab= "99th percentile of speed during loom")
lines(x,y)
mydata <- seq(9,29,1)
pred <- predict(model1, data.frame(Temperature = mydata), interval="confidence",level = 0.95)

lines(mydata,pred[,2],lty=2)
lines(mydata,pred[,3],lty=2)

#bootstrapping predictions
model_99 <- lm((X99_speed)^0.5 ~ Temperature + I(Temperature^2) + log(Groupsize,2) + loom +I(log(Groupsize,2)^2),percentiles)
summary(model_99)
newData1 <- data.frame(expand.grid(Temperature = seq(from = 9, to = 29, by = 1), Groupsize = c(1,2,4,8,16),loom = c(1,2,3,4,5)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(model_99))
  ymod <- update(model_99,ynew ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=3)
results[,1] <- predict(model_99,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest[j,],probs = c(0.025))
  results[j,3] <- quantile(yest[j,],probs = c(0.975))
}  
newData1$loom_speed <- results[,1]
newData1$loom_speed025 <- results[,2]
newData1$loom_speed975 <- results[,3]
write.csv(newData1,"C:\\Users\\maria\\OneDrive\\Documents\\data\\temp_collective\\roi\\loom_speed_predictions.csv")

chknyes <- interesect(which(newData1$Groupsize == 16),which(newData1$Loom == 1))  
chknno <- which(newData1$Groupsize == 1)
temp <- newData1$Temperature[chknyes]
plot(data1$Temperature, data1$startles_during_loom, xlab = "Temperature",ylab = "Startles during loom")
lines(temp,results[chknyes,1], lty = "solid", col = "green")
#plot(temp,results[chknyes,1], type = 'l', col = "green", ylim = c(0,40), xlab = "Temperature",ylab = "Startles")
lines(temp,results[chknyes,2], lty = "dashed", col = "green")
lines(temp,results[chknyes,3], lty = "dashed", col = "green")



#prediction for speed

model_99 <- lm((X99_speed)^0.5 ~ Temperature + I(Temperature^2) + log(Groupsize,2) + loom + I(log(Groupsize,2)^2),percentiles)
summary(model_99)
newData1 <- data.frame(expand.grid(Temperature = seq(from = 9, to = 29, by = 1), Groupsize = c(1,2,4,8,16),loom = c(1,2,3,4,5)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(model_99))
  ymod <- update(model_99,ynew ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=3)
results[,1] <- predict(model_99,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest[j,],probs = c(0.025))
  results[j,3] <- quantile(yest[j,],probs = c(0.975))
}  
chknyes <- interesect(which(newData1$Groupsize == 16),which(newData1$Loom == 1))  
chknno <- which(newData1$Groupsize == 1)
temp <- newData1$Temperature[chknyes]
plot(percentiles$Temperature, percentiles$X99_speed^0.5, xlab = "Temperature",ylab = "(99th percentile of speed during loom)^0.5")
lines(temp,results[chknyes,1], lty = "solid", col = "green")
#plot(temp,results[chknyes,1], type = 'l', col = "green", ylim = c(0,40), xlab = "Temperature",ylab = "Startles")
lines(temp,results[chknyes,2], lty = "dashed", col = "green")
lines(temp,results[chknyes,3], lty = "dashed", col = "green")

### prop of ind startling

prop_data_new <- read.csv("../../data/temp_collective/roi/stats_prop_ind_startles_no_nan_new_mask.csv",header=TRUE,na.strings=c("[nan]"))



model_glm6 <-  glm(prop_startles10 ~ Temperature + I(Temperature^2) + Loom, family = binomial,prop_data_new)
summary(model_glm6)
#aic = 1079
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



### acceleration during loom
percentiles <- read.csv("../../data/temp_collective/roi/stats_loom_low_pass_data.csv",header=TRUE,na.strings=c("[nan]"))

acc99 <- percentiles$X99_acc
acc90 <- percentiles$X90_acc

temperature <- percentiles$Temperature
group_size <- percentiles$Groupsize
replicate <- percentiles$Replicate
loom <- percentiles$loom

n <- length(percentiles$Temperature)
data <- as.data.frame(cbind(sample = (1:n),acc99, acc90,temperature,group_size, replicate, loom))

model_acc99 <- lm(log(X99_acc+1) ~ Temperature + I(Temperature^2) + log(Groupsize,2),percentiles)
summary(model_acc99)
plot(fitted(model_acc99), residuals(model_acc99))

newData1 <- data.frame(expand.grid(Temperature = seq(from = 9, to = 29, by = 1), Groupsize = c(1,2,4,8,16)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(model_acc99))
  ymod <- update(model_acc99,ynew ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=3)
results[,1] <- predict(model_acc99,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest[j,],probs = c(0.025))
  results[j,3] <- quantile(yest[j,],probs = c(0.975))
}  
chknyes <- which(newData1$Groupsize == 16)  
chknno <- which(newData1$Groupsize == 1)
temp <- newData1$Temperature[chknyes]
plot(percentiles$Temperature, log((percentiles$X99_acc)+1), xlab = "Temperature",ylab = "log(99th percentile of acceleration during loom)")
lines(temp,results[chknyes,1], lty = "solid", col = "green")
#plot(temp,results[chknyes,1], type = 'l', col = "green", ylim = c(0,40), xlab = "Temperature",ylab = "Startles")
lines(temp,results[chknyes,2], lty = "dashed", col = "green")
lines(temp,results[chknyes,3], lty = "dashed", col = "green")

##accelerations before loom

acc <- read.csv("../../data/temp_collective/roi/stats_before_loom_percentile_speed_acc_low_mask.csv",header=TRUE,na.strings=c("[nan]"))


model6 <- lm (log(X50_acc.1 + 1 ) ~ Temperature + I(Temperature^2) + Groupsize, acc)
summary(model6)
plot(fitted(model6), residuals(model6))
#best


newData1 <- data.frame(expand.grid(Temperature = seq(from = 9, to = 29, by = 1), Groupsize = c(1,2,4,8,16)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(model6))
  ymod <- update(model6,ynew ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=3)
results[,1] <- predict(model6,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest[j,],probs = c(0.025))
  results[j,3] <- quantile(yest[j,],probs = c(0.975))
}  
chknyes <- which(newData1$Groupsize == 16)  
chknno <- which(newData1$Groupsize == 1)
temp <- newData1$Temperature[chknyes]
plot(acc$Temperature, log((acc$X50_acc.1)+1), xlab = "Temperature",ylab = "log(median acceleration before loom)")
lines(temp,results[chknyes,1], lty = "solid", col = "green")
#plot(temp,results[chknyes,1], type = 'l', col = "green", ylim = c(0,40), xlab = "Temperature",ylab = "Startles")
lines(temp,results[chknyes,2], lty = "dashed", col = "green")
lines(temp,results[chknyes,3], lty = "dashed", col = "green")