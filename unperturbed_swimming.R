# one file for all unperturbed swimming analysis


data <- read.csv(here("Documents","data","temp_collective","roi","all_params_wo_loom.csv"),header=TRUE,na.strings=c("[nan]"))




# 99th percentile speed



my_data<-data.frame("temp" = data$Temperature[complete.cases(data$speed_percentile99)],
                    "gs" = data$Groupsize[complete.cases(data$speed_percentile99)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$speed_percentile99)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$speed_percentile99)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$speed_percentile99)],
                    "subtrial" = data$Subtrial[complete.cases(data$speed_percentile99)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$speed_percentile99)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$speed_percentile99)], format = "%H:%M")),
                    "speed" = data$speed_percentile99[complete.cases(data$speed_percentile99)]
)

model_lm <- lm(log(speed+1) ~ temp,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.1127
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
extractAIC(model_lm) #-683


model_lm <- lm(log(speed+1) ~ temp + I(temp^2),my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.1197
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
extractAIC(model_lm) #-684
#best!

#predictions

newData1<- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(model_lm))
  ymod <- update(model_lm,ynew ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=3)
results[,1] <- predict(model_lm,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest[j,],probs = c(0.025))
  results[j,3] <- quantile(yest[j,],probs = c(0.975))
}  
newData1$speed99 <- results[,1]
newData1$speed99_025 <- results[,2]
newData1$speed99_975 <- results[,3]

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/speed99_before_loom_predictions_new.csv")


#median 

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$speed_percentile50)],
                    "gs" = data$Groupsize[complete.cases(data$speed_percentile50)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$speed_percentile50)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$speed_percentile50)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$speed_percentile50)],
                    "subtrial" = data$Subtrial[complete.cases(data$speed_percentile50)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$speed_percentile50)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$speed_percentile50)], format = "%H:%M")),
                    "speed" = data$speed_percentile50[complete.cases(data$speed_percentile50)]
)

model_lm <- lm(log(speed+1) ~ temp,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.02
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
extractAIC(model_lm) #-812
#best?

model_lm <- lm(log(speed+1) ~ temp + I(temp^2),my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.018
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
extractAIC(model_lm) #-810

#predictions

newData1<- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(model_lm))
  ymod <- update(model_lm,ynew ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=3)
results[,1] <- predict(model_lm,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest[j,],probs = c(0.025))
  results[j,3] <- quantile(yest[j,],probs = c(0.975))
}  
newData1$speed99 <- results[,1]
newData1$speed99_025 <- results[,2]
newData1$speed99_975 <- results[,3]

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/speed50_before_loom_predictions_new.csv")

#avg speed

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$avg_speed)],
                    "gs" = data$Groupsize[complete.cases(data$avg_speed)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$avg_speed)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$avg_speed)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$avg_speed)],
                    "subtrial" = data$Subtrial[complete.cases(data$avg_speed)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$avg_speed)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$avg_speed)], format = "%H:%M")),
                    "speed" = data$avg_speed[complete.cases(data$avg_speed)]
)

model_lm <- lm(log(speed+1) ~ temp,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.04
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
extractAIC(model_lm) #-835
#best?

model_lm <- lm(log(speed+1) ~ temp + I(temp^2),my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.04
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
extractAIC(model_lm) #-834

newData1<- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(model_lm))
  ymod <- update(model_lm,ynew ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=3)
results[,1] <- predict(model_lm,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest[j,],probs = c(0.025))
  results[j,3] <- quantile(yest[j,],probs = c(0.975))
}  
newData1$speed99 <- results[,1]
newData1$speed99_025 <- results[,2]
newData1$speed99_975 <- results[,3]

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/speed_avg_before_loom_predictions_new.csv")




#99th percentile of acceleration

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$acc_percentile99)],
                    "gs" = data$Groupsize[complete.cases(data$acc_percentile99)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$acc_percentile99)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$acc_percentile99)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$acc_percentile99)],
                    "subtrial" = data$Subtrial[complete.cases(data$acc_percentile99)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$acc_percentile99)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$acc_percentile99)], format = "%H:%M")),
                    "acc" = data$acc_percentile99[complete.cases(data$acc_percentile99)],
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$acc_percentile99)], format = "%H:%M")) - as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$acc_percentile99)], format = "%H:%M"))
)


model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1946
extractAIC(model_lm) #-580

require(faraway)

studentized <- rstudent(model_lm)
halfnorm(studentized)
#128

halfnorm(cooks <- cooks.distance(model_lm),5)
nn <- length(my_data$acc)
t_crit <- qt(0.025/nn,nn-4-1,lower.tail = FALSE) 
my_data[which(abs(studentized)>t_crit),]
#128

my_new_data <- my_data[-which(abs(studentized)>t_crit),]


model_lm <- lm(log(acc+1) ~ temp + log(gs,2) + I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1982
extractAIC(model_lm) #-579



model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2),my_new_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1962
extractAIC(model_lm) #-657 #using this for prediction


model_lm <- lm(log(acc+1) ~ temp + log(gs,2) + I(log(gs,2)^2),my_new_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1936
extractAIC(model_lm) #-657


newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(1,2,4,8,16)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(model_lm))
  ymod <- update(model_lm,ynew ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=3)
results[,1] <- predict(model_lm,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest[j,],probs = c(0.025))
  results[j,3] <- quantile(yest[j,],probs = c(0.975))
}  
newData1$acc99 <- results[,1]
newData1$acc99_025 <- results[,2]
newData1$acc99_975 <- results[,3]

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/acc_before_loom_99_predictions_new_squared.csv")

#median acc

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$acc_percentile50)],
                    "gs" = data$Groupsize[complete.cases(data$acc_percentile50)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$acc_percentile50)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$acc_percentile50)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$acc_percentile50)],
                    "subtrial" = data$Subtrial[complete.cases(data$acc_percentile50)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$acc_percentile50)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$acc_percentile50)], format = "%H:%M")),
                    "acc" = data$acc_percentile50[complete.cases(data$acc_percentile50)],
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$acc_percentile50)], format = "%H:%M")) - as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$acc_percentile50)], format = "%H:%M"))
)

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.3091
extractAIC(model_lm) #-741 #best

model_lm <- lm(log(acc+1) ~ temp  + log(gs,2) + I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.2592
extractAIC(model_lm) #-726



newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(1,2,4,8,16)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(model_lm))
  ymod <- update(model_lm,ynew ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=3)
results[,1] <- predict(model_lm,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest[j,],probs = c(0.025))
  results[j,3] <- quantile(yest[j,],probs = c(0.975))
}  
newData1$acc99 <- results[,1]
newData1$acc99_025 <- results[,2]
newData1$acc99_975 <- results[,3]

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/acc_before_loom_50_predictions_new_squared.csv")


# average acc

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$avg_acc)],
                    "gs" = data$Groupsize[complete.cases(data$avg_acc)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$avg_acc)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$avg_acc)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$avg_acc)],
                    "subtrial" = data$Subtrial[complete.cases(data$avg_acc)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$avg_acc)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$avg_acc)], format = "%H:%M")),
                    "acc" = data$avg_acc[complete.cases(data$avg_acc)],
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$avg_acc)], format = "%H:%M")) - as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$avg_acc)], format = "%H:%M"))
)

model_lm <- lm(log(acc+1) ~ temp  + log(gs,2) + I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.244
extractAIC(model_lm) #-709


model_lm <- lm(log(acc+1) ~ temp  + I(temp^2) +  log(gs,2) + I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.27
extractAIC(model_lm) #-719
