#goal - stats on speed before loom only for small group sizes.

data  <- read.csv("../../data/temp_collective/roi/all_params_wo_loom.csv",
                  header=TRUE,na.strings=c("[nan]"))


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

hist(my_data$speed,20)
hist(log(my_data$speed),20)

my_data1 <-data.frame("temp" = data$Temperature[data$Groupsize == 1],
                    "gs" = data$Groupsize[data$Groupsize == 1],
                    "kt"=1/(0.00008617*(data$Temperature[data$Groupsize == 1]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[data$Groupsize == 1], format = "%d/%m/%Y")),
                    "trial" = data$Trial[data$Groupsize == 1],
                    "subtrial" = data$Subtrial[data$Groupsize == 1],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[data$Groupsize == 1], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[data$Groupsize == 1], format = "%H:%M")),
                    "speed" = data$avg_speed[data$Groupsize == 1]
)

#linear model with all gs


model_lm <- lm((speed)^0.5 ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.12
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

#linear model with all gs -log transformation 

model_lm <- lm(log(speed+1) ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.12
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

#linear model with all gs -log transformation + interaction 

model_lm <- lm(log(speed+1) ~ temp*log(gs,2) + I(temp^2) + I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.05
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

#linear model with all gs -log transformation + interaction 

model_lm <- lm(log(speed+1) ~ temp + I(temp^2)*log(gs,2) + I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.05
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

#linear model with all gs -log transformation + interaction 

model_lm <- lm(log(speed+1) ~ temp + log(gs,2) + I(temp^2)*I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.05
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))


#linear model with all gs -log transformation + date

model_lm <- lm(log(speed+1) ~ temp*log(gs,2) + I(temp^2) + I(log(gs,2)^2) + date ,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.05
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))


#linear model with all gs -log transformation + date

model_lm <- lm(log(speed+1) ~ temp*log(gs,2) + date ,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.0426
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

#linear model with all gs -log transformation + all variables

model_lm <- lm(log(speed+1) ~ temp*log(gs,2) + date + trial + t1,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.035
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

#linear model with all gs -log transformation + all variables

model_lm <- lm(log(speed+1) ~ temp*log(gs,2) + date + trial + t1 + t2 + subtrial,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.04185
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))



#### for gs = 1 ######

######################

#linear model with gs 1 -log transformation + all variables

model_lm <- lm(log(speed+1) ~ temp + date + trial + t1 + t2 + subtrial ,my_data1)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.095
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))


#linear model with gs 1 -log transformation + date

model_lm <- lm(log(speed+1) ~ temp + date + t1 ,my_data1)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.13
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

#### gs = 2 #########

#####################

my_data2 <-data.frame("temp" = data$Temperature[data$Groupsize == 2],
                      "gs" = data$Groupsize[data$Groupsize == 2],
                      "kt"=1/(0.00008617*(data$Temperature[data$Groupsize == 2]+273.1)),
                      "date"=as.numeric(as.Date(data$Date[data$Groupsize == 2], format = "%d/%m/%Y")),
                      "trial" = data$Trial[data$Groupsize == 2],
                      "subtrial" = data$Subtrial[data$Groupsize == 2],
                      "t1" = as.numeric(as.POSIXct(data$Time_fish_in[data$Groupsize == 2], format = "%H:%M")),
                      "t2" = as.numeric(as.POSIXct(data$Time_start_record[data$Groupsize == 2], format = "%H:%M")),
                      "speed" = data$avg_speed[data$Groupsize == 2]
)

model_lm <- lm(log(speed+1) ~ temp + date + t1 ,my_data2)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.2451
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))


model_lm <- lm(log(speed+1) ~ temp,my_data2)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.1252
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))


# with temp sq

model_lm <- lm(log(speed+1) ~ temp + I(temp^2),my_data2)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.1085
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
# neither significant

# with temp sq and other variables

model_lm <- lm(log(speed+1) ~ temp + I(temp^2) + date + trial + t1,my_data2)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.2387
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
# neither significant

model_lm <- lm(log(speed+1) ~ temp + date + trial + t1,my_data2)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.2486
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
# temp significant

model_lm <- lm(log(speed+1) ~ temp + date + t1,my_data2)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.2451
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
# temp and date significant

model_lm <- lm(log(speed+1) ~ I(temp^2) + date + t1,my_data2)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.2535
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
# temp sq and date significant
# best maybe?

model_lm <- lm(log(speed+1) ~  date + t1,my_data2)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.1581
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
#date significant

#predictions

newData1<- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), 
                                   date = unique(my_data2$date), t1 = unique(my_data2$t1)))
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
newData1$avg_speed <- results[,1]
newData1$avg_speed025 <- results[,2]
newData1$avg_speed975 <- results[,3]

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/avg_speed_before_loom_predictions.csv")

#t1 - 1616979600, date 18110
intersection <- intersect(which(newData1$t1 == 1616979600),which(newData1$date == 18110))

temp <- newData1$temp[intersection]

plot(my_data2$temp, my_data2$speed)
lines(temp, exp(results[intersection,1])-1,lty = "solid")
lines(temp, exp(results[intersection,2])-1,lty = "dashed")
lines(temp, exp(results[intersection,3])-1,lty = "dashed")


##################### try model with all the gs for all the speed variable ################3

###### avg speed ############

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

hist(my_data$speed,20)
hist(log(my_data$speed),20)



model_lm <- lm(log(speed+1) ~ date + t1,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =-0.0002 ?? 
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

# add temp
model_lm <- lm(log(speed+1) ~ temp +  date + t1,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.039 
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

# replace with  temp^2
model_lm <- lm(log(speed+1) ~ I(temp^2) +  date + t1,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.033
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm)) 
extractAIC(model_lm) #-831

# add gs to temp
model_lm <- lm(log(speed+1) ~ gs + temp +  date + t1,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.038
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

# add log(gs) to temp
model_lm <- lm(log(speed+1) ~ log(gs,2) + temp +  date + t1,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.036
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))


# add interaction between log(gs) and temp
model_lm <- lm(log(speed+1) ~ log(gs,2)*temp +  date + t1,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.038
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
#interaction not sigificant

model_lm <- lm(log(speed+1) ~ temp,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.04
extractAIC(model_lm) #-834

model_lm <- lm(log(speed+1) ~ I(temp^2),my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.04
extractAIC(model_lm) #-833

# only 99th percentile of speed


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

hist(my_data$speed,20)
hist(log(my_data$speed),20)


# add interaction between log(gs) and temp
model_lm <- lm(log(speed+1) ~ log(gs,2)*temp +  date + t1,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.099
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
#interaction not sigificant
#this is best

# add interaction between log(gs) and temp^2
model_lm <- lm(log(speed+1) ~ log(gs,2)*I(temp^2) +  date + t1,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.08574
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
#interaction not sigificant
extractAIC(model_lm)

#use kt instead

model_lm <- lm(log(speed+1) ~ log(gs,2)*kt +  date + t1,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.1013
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
#interaction not sigificant
#this is best

model_lm <- lm(log(speed+1) ~ kt,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.1141
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
extractAIC(model_lm)
#interaction not sigificant
#this is best

## predictions for 99th percentile of speed with temp model
# add interaction between log(gs) and temp
model_lm <- lm(log(speed+1) ~ temp,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.1127
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
extractAIC(model_lm)
#this is best


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

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/speed99_before_loom_predictions.csv")

#t1 - 1616979600, date 18110
#intersection <- intersect(which(newData1$t1 == 1616979600),which(newData1$date == 18110))

temp <- newData1$temp

plot(my_data$temp, my_data$speed)
lines(temp, exp(results[,1])-1,lty = "solid")
lines(temp, exp(results[,2])-1,lty = "dashed")
lines(temp, exp(results[,3])-1,lty = "dashed")


## predictions for 99th percentile of speed with kt model
# add interaction between log(gs) and temp
model_lm <- lm(log(speed+1) ~ kt,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.1141
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

#this is best


newData1<- data.frame(expand.grid(kt = seq(from = 38, to = 42, by = 0.1)))
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

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/speed99_kt_before_loom_predictions.csv")

#t1 - 1616979600, date 18110
#intersection <- intersect(which(newData1$t1 == 1616979600),which(newData1$date == 18110))

temp <- 1/(newData1$kt*0.00008617) - 273.1

plot(my_data$temp, my_data$speed)
lines(temp, exp(results[,1])-1,lty = "solid")
lines(temp, exp(results[,2])-1,lty = "dashed")
lines(temp, exp(results[,3])-1,lty = "dashed")