percentile <- read.csv("../../data/temp_collective/roi/stats_before_loom_percentile.csv",header=TRUE,na.strings=c("[nan]"))

model1 <- lm(X99_speed ~ Temperature + Groupsize, percentile)
summary(model1)
plot(fitted(model1), residuals(model1))
#best. Maybe different variances?
qqnorm(residuals(model1))
qqline(residuals(model1))


model2 <- lm(X99_speed ~ Temperature + I(Temperature^2) + Groupsize, percentile)
summary(model2)
plot(fitted(model2), residuals(model2))

model1 <- lm(X99_speed ~ Temperature*Groupsize, percentile)
summary(model1)
plot(fitted(model1), residuals(model1))

model1 <- lm(I(X99_speed)^0.5 ~ Temperature + Groupsize, percentile)
summary(model1)
plot(fitted(model1), residuals(model1))
#best. Maybe different variances?
qqnorm(residuals(model1))
qqline(residuals(model1))


model1 <- lm(I(X99_speed)^0.5 ~ I(Temperature^2) + Temperature + Groupsize, percentile)
summary(model1)
plot(fitted(model1), residuals(model1))

qqnorm(residuals(model1))
qqline(residuals(model1))


#PREDICTIONS
model1 <- lm(I(X99_speed)^0.5 ~ Temperature + Groupsize, percentile)
summary(model1)
newData1 <- data.frame(expand.grid(Temperature = seq(from = 9, to = 29, by = 1), Groupsize = c(1,2,4,8,16)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(model1))
  ymod <- update(model1,ynew ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=3)
results[,1] <- predict(model1,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest[j,],probs = c(0.025))
  results[j,3] <- quantile(yest[j,],probs = c(0.975))
}  
chknyes <- which(newData1$Groupsize == 16)  
chknno <- which(newData1$Groupsize == 1)
temp <- newData1$Temperature[chknyes]
plot(percentile$Temperature, percentile$X99_speed^0.5, xlab = "Temperature",ylab = "(99th percentile of speed before loom)^0.5")
lines(temp,results[chknyes,1], lty = "solid", col = "green")
#plot(temp,results[chknyes,1], type = 'l', col = "green", ylim = c(0,40), xlab = "Temperature",ylab = "Startles")
lines(temp,results[chknyes,2], lty = "dashed", col = "green")
lines(temp,results[chknyes,3], lty = "dashed", col = "green")



#ggplot predictions?

model1 <- lm(I(X99_speed)^0.5 ~ Temperature + Groupsize, percentile)
plottemp <- seq(9,29,1)

newData1 <- data.frame(expand.grid(Temperature = seq(from = 9, to = 29, by = 0.1), Groupsize = c(1,2,4,8,16)))
newData1$pred <- predict(model1, newdata = newData1)

newData1$back <- newData1$pred^2
newData1$q1 <- results[,2]^2
newData1$q2 <- results[,3]^2


require(tidyverse)
ggplot(percentile, aes(x = Temperature, y = X99_speed, color = Groupsize)) +
  geom_jitter(width = 1) + geom_line(data = newData1, aes(y = back) ) +
  geom_line(data = newData1, aes(y = q1) ) +
  geom_line(data = newData1, aes(y = q2) )

ggplot(percentile,aes(x=Temperature, y = X99_speed,color = Groupsize)) + 
  geom_point(aes(y = X99_speed), shape = 16) +
  stat_smooth(aes(y = X99_speed),method = "lm", formula = y ~ x, size = 1) +
  ylab("99th percentile of Speed")
  