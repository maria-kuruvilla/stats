data_dtc <- read.csv("../../data/temp_collective/roi/stats_dtc_before_loom.csv")

model_1 <- lm(dtc_before_loom_norm ~ Temperature + Groupsize, data_dtc)
summary(model_1)

plot(data_dtc$Temperature,data_dtc$dtc_before_loom_norm, xlab = "Temperature", ylab = "Distance to center")

model_temp <- lm(dtc_before_loom_norm ~ Temperature, data_dtc)

abline(model_temp, col = "green")
confint(model_temp)

mydata <- seq(9,29,1)
pred <- predict(model_temp, data.frame(Temperature = mydata), interval="confidence",level = 0.95)

lines(mydata,pred[,2],lty=2, col = "green")
lines(mydata,pred[,3],lty=2, col = "green")
