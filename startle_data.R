#############

##distance##

#############

data <- read.csv("../../data/temp_collective/roi/all_params_w_loom_startle.csv",header=TRUE,na.strings=c("[nan]"))

# make new dataframe

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$distance)],
                    "gs" = data$Groupsize[complete.cases(data$distance)],
                    "loom" = data$Loom[complete.cases(data$distance)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$distance)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$distance)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$distance)],
                    "subtrial" = data$Subtrial[complete.cases(data$distance)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$distance)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$distance)], format = "%H:%M")),
                    "t" = (as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$distance)], format = "%H:%M")) - 
                      as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$distance)], format = "%H:%M"))),
                    "distance" = data$distance[complete.cases(data$distance)]
)

model_lm <- lm(distance ~ temp + gs + loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# bad residuals

# log transformed

model_lm <- lm(log(distance) ~ temp + gs + loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.20

model_lm <- lm(log(distance) ~ temp + gs + temp*gs + loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.21 #interaction significant

model_lm <- lm(log(distance) ~ temp + gs + temp*gs + loom + I(temp^2)*gs, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.2185 #interaction significant

model_lm <- lm(log(distance) ~ temp + gs + temp*gs + loom*temp + I(temp^2)*gs, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.2187 #interaction significant #loom interaction not significant

model_lm <- lm(log(distance) ~ temp + gs + temp*gs + loom*temp + I(temp^2)*gs + loom*I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.22 #interaction significant #loom temp^2 interaction almost significant


# predictions

model_lm <- lm(log(distance) ~ temp + gs + temp*gs + loom*temp + I(temp^2)*gs + loom*I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))


newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(1,2,4,8,16),
                                   loom = c(1,2,3,4,5)))
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
newData1$distance <- results[,1]
newData1$distance_025 <- results[,2]
newData1$distance_975 <- results[,3]

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/startle_distance_predictions.csv")




###############

## avg speed ##

###############

data <- read.csv("../../data/temp_collective/roi/all_params_w_loom_startle.csv",header=TRUE,na.strings=c("[nan]"))

# make new dataframe

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$avg_startle_speed)],
                    "gs" = data$Groupsize[complete.cases(data$avg_startle_speed)],
                    "loom" = data$Loom[complete.cases(data$avg_startle_speed)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$avg_startle_speed)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$avg_startle_speed)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$avg_startle_speed)],
                    "subtrial" = data$Subtrial[complete.cases(data$avg_startle_speed)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$avg_startle_speed)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$avg_startle_speed)], format = "%H:%M")),
                    "t" = (as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$avg_startle_speed)], format = "%H:%M")) - 
                             as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$avg_startle_speed)], format = "%H:%M"))),
                    "speed" = data$avg_startle_speed[complete.cases(data$avg_startle_speed)]
)

model_lm <- lm(speed ~ temp + gs + loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# bad residuals

model_lm <- lm(log(speed + 1) ~ temp + gs + loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# okay residuals #rsq 0.01499

model_lm <- lm(log(speed + 1) ~ temp*gs + temp + gs + loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# okay residuals #rsq 0.015


###############

## 99th percentile of speed ##

###############

data <- read.csv("../../data/temp_collective/roi/all_params_w_loom_startle.csv",header=TRUE,na.strings=c("[nan]"))

# make new dataframe

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$startle_data_percentile99)],
                    "gs" = data$Groupsize[complete.cases(data$startle_data_percentile99)],
                    "loom" = data$Loom[complete.cases(data$startle_data_percentile99)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$startle_data_percentile99)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$startle_data_percentile99)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$startle_data_percentile99)],
                    "subtrial" = data$Subtrial[complete.cases(data$startle_data_percentile99)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$startle_data_percentile99)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$startle_data_percentile99)], format = "%H:%M")),
                    "t" = (as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$startle_data_percentile99)], format = "%H:%M")) - 
                             as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$startle_data_percentile99)], format = "%H:%M"))),
                    "speed" = data$startle_data_percentile99[complete.cases(data$startle_data_percentile99)]
)

model_lm <- lm(speed ~ temp + gs + loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# bad residuals

model_lm <- lm(log(speed + 1) ~ temp + gs + loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# okay residuals #rsq 0.04

model_lm <- lm(log(speed + 1) ~ temp*gs + temp + gs + loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# okay residuals #rsq 0.042 # interaction not significant # temp and temp sq is significant




#############

##distance with new csv file ##

#############

data <- read.csv("../../data/temp_collective/roi/all_params_w_loom_startle2.csv",header=TRUE,na.strings=c("[nan]"))

# make new dataframe

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$distance)],
                    "gs" = data$Groupsize[complete.cases(data$distance)],
                    "loom" = data$Loom[complete.cases(data$distance)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$distance)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$distance)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$distance)],
                    "subtrial" = data$Subtrial[complete.cases(data$distance)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$distance)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$distance)], format = "%H:%M")),
                    "t" = (as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$distance)], format = "%H:%M")) - 
                             as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$distance)], format = "%H:%M"))),
                    "distance" = data$distance[complete.cases(data$distance)]
)

model_lm <- lm(distance ~ temp + gs + loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# bad residuals

# log transformed

model_lm <- lm(log(distance) ~ temp + gs + loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.20


model_lm <- lm(log(distance) ~ temp + gs + temp*gs + loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.21 #interaction significant

model_lm <- lm(log(distance) ~ temp + gs + temp*gs + loom + I(temp^2)*gs, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.2185 #interaction significant

model_lm <- lm(log(distance) ~ temp + gs + temp*gs + loom*temp + I(temp^2)*gs, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.2187 #interaction significant #loom interaction not significant

model_lm <- lm(log(distance) ~ temp + gs + temp*gs + loom*temp + I(temp^2)*gs + loom*I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.22 #interaction significant #loom temp^2 interaction almost significant

# adding date

model_lm <- lm(log(distance) ~ temp + gs + temp*gs + loom*temp + I(temp^2)*gs + loom*I(temp^2) + date, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.23 #interaction significant #loom temp^2 interaction almost significant

#removing second interaction

model_lm <- lm(log(distance) ~ temp + gs + temp*gs + loom + I(temp^2)*gs + loom*I(temp^2) + date, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.23 #interaction significant 

#adding loom*gs interaction

model_lm <- lm(log(distance) ~ temp + gs + temp*gs + loom + I(temp^2)*gs + loom*I(temp^2) + loom*gs + date, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.23 #interaction significant #loom gs interaction almost significant 
qqnorm(residuals(model_lm))
qqline(residuals(model_lm))


# adding date and t

model_lm <- lm(log(distance) ~ temp + gs + temp*gs + loom*temp + I(temp^2)*gs + loom*I(temp^2) + date + t, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.23 #interaction significant #loom temp^2 interaction almost significant


# adding date and t1

model_lm <- lm(log(distance) ~ temp + gs + temp*gs + loom*temp + I(temp^2)*gs + loom*I(temp^2) + date + t1, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.23 #interaction significant #loom temp^2 interaction almost significant
# predictions
# predictions

model_lm <- lm(log(distance) ~ temp + gs + temp*gs + loom + I(temp^2)*gs + loom*I(temp^2) + loom*gs + date, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))

newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(1,2,4,8,16),
                                   loom = c(1,2,3,4,5), date = unique(my_data$date)))
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
newData1$distance <- results[,1]
newData1$distance_025 <- results[,2]
newData1$distance_975 <- results[,3]

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/startle_distance_predictions2.csv")

#############

##speed with 3rd csv file ##

#############

data <- read.csv("../../data/temp_collective/roi/all_params_w_loom_startle3.csv",header=TRUE,na.strings=c("[nan]"))

# make new dataframe

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$startle_data_percentile99)],
                    "gs" = data$Groupsize[complete.cases(data$startle_data_percentile99)],
                    "loom" = data$Loom[complete.cases(data$startle_data_percentile99)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$startle_data_percentile99)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$startle_data_percentile99)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$startle_data_percentile99)],
                    "subtrial" = data$Subtrial[complete.cases(data$startle_data_percentile99)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$startle_data_percentile99)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$startle_data_percentile99)], format = "%H:%M")),
                    "t" = (as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$startle_data_percentile99)], format = "%H:%M")) - 
                             as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$startle_data_percentile99)], format = "%H:%M"))),
                    "speed" = data$startle_data_percentile99[complete.cases(data$startle_data_percentile99)]
)

model_lm <- lm(speed ~ temp + gs + loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# bad residuals

# log transformed

model_lm <- lm(log(speed) ~ temp + gs + loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.079

model_lm <- lm(log(speed) ~ temp + log(gs,2) + loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.075


model_lm <- lm(log(speed) ~ temp + log(gs,2) + loom + I(temp^2) + temp*log(gs,2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.081 #interaction significant

model_lm <- lm(log(speed) ~ temp + log(gs,2) + loom + I(temp^2) + temp*log(gs,2) + I(temp^2)*log(gs,2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.0801 #interaction not significant

model_lm <- lm(log(speed) ~ temp + log(gs,2) + loom + I(temp^2) + I(temp^2)*log(gs,2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.0813 #interaction significant

model_lm <- lm(log(speed) ~ temp + log(gs,2) + loom + I(temp^2) + I(temp^2)*log(gs,2) + temp*loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.0818 #second interaction not significant


model_lm <- lm(log(speed) ~ temp + log(gs,2) + loom + I(temp^2) + I(temp^2)*log(gs,2) + temp*loom + I(temp^2)*loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.0817 #second and third interaction not significant


model_lm <- lm(log(speed) ~ temp + log(gs,2) + loom + I(temp^2) + I(temp^2)*log(gs,2)  + I(temp^2)*loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.0817 #second interaction not significant

model_lm <- lm(log(speed) ~ temp + log(gs,2) + loom + I(temp^2) + I(temp^2)*log(gs,2)  + I(temp^2)*loom + loom*log(gs,2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.0831 #second interaction not significant

model_lm <- lm(log(speed) ~ temp + log(gs,2) + loom + I(temp^2) + I(temp^2)*log(gs,2)  + I(temp^2)*loom + 
                 loom*log(gs,2) + date, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.085 #second interaction not significant # date not significant

model_lm <- lm(log(speed) ~ temp + log(gs,2) + loom + I(temp^2) + I(temp^2)*log(gs,2)  + I(temp^2)*loom + 
                 loom*log(gs,2) + date + t, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.083 #second interaction not significant # date not significant # t not significant

model_lm <- lm(log(speed) ~ temp + log(gs,2) + loom + I(temp^2) + I(temp^2)*log(gs,2)  + I(temp^2)*loom + 
                 loom*log(gs,2) + date + t1, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.083 #second interaction not significant # date not significant # t1 not significant


model_lm <- lm(log(speed) ~ temp + log(gs,2) + loom + I(temp^2) + I(temp^2)*log(gs,2) + date, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.084 #second interaction not significant # date not significant


#################################################################

######## distance and speed with corrected csv file #############

#################################################################

data <- read.csv("../../data/temp_collective/roi/all_params_w_loom_startle_corrected.csv",header=TRUE,na.strings=c("[nan]"))

# make new dataframe

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$distance)],
                    "gs" = data$Groupsize[complete.cases(data$distance)],
                    "loom" = data$Loom[complete.cases(data$distance)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$distance)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$distance)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$distance)],
                    "subtrial" = data$Subtrial[complete.cases(data$distance)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$distance)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$distance)], format = "%H:%M")),
                    "t" = (as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$distance)], format = "%H:%M")) - 
                             as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$distance)], format = "%H:%M"))),
                    "distance" = data$distance[complete.cases(data$distance)]
)

model_lm <- lm(log(distance+1) ~ temp + gs + temp*gs + loom*temp + I(temp^2)*gs + loom*I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))

model_lm <- lm(log(distance+1) ~ temp + gs + temp*gs + loom*temp + I(temp^2)*gs + loom*I(temp^2) + date, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))

model_lm <- lm(log(distance+1) ~ temp + gs + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#bad residuals

model_lm <- lm(log(distance+1) ~ temp + log(gs,2) + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))






################# speed #######################




data <- read.csv("../../data/temp_collective/roi/all_params_w_loom_startle_corrected.csv",header=TRUE,na.strings=c("[nan]"))

# make new dataframe

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$startle_data_percentile99)],
                    "gs" = data$Groupsize[complete.cases(data$startle_data_percentile99)],
                    "loom" = data$Loom[complete.cases(data$startle_data_percentile99)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$startle_data_percentile99)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$startle_data_percentile99)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$startle_data_percentile99)],
                    "subtrial" = data$Subtrial[complete.cases(data$startle_data_percentile99)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$startle_data_percentile99)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$startle_data_percentile99)], format = "%H:%M")),
                    "t" = (as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$startle_data_percentile99)], format = "%H:%M")) - 
                             as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$startle_data_percentile99)], format = "%H:%M"))),
                    "speed" = data$startle_data_percentile99[complete.cases(data$startle_data_percentile99)]
)



model_lm <- lm(log(speed) ~ temp + log(gs,2) + loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.05 # temp and temp^2 significant

model_lm <- lm(log(speed) ~ temp + log(gs,2) + loom + I(temp^2) + loom*temp, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.049 # temp and temp^2 significant # interaction not significant

model_lm <- lm(log(speed) ~ temp + log(gs,2) + loom + I(temp^2) + loom*I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.049 # temp and temp^2 significant # interaction not significant

model_lm <- lm(log(speed) ~ temp + log(gs,2) + loom + I(temp^2) + loom*I(temp^2) + loom*temp, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.051 # temp and temp^2 significant # interaction not significant

model_lm <- lm(log(speed) ~ temp + log(gs,2) + loom + I(temp^2) + date, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.052 # temp and temp^2 significant 

model_lm <- lm(log(speed) ~ temp + log(gs,2) + loom + I(temp^2) + date + t, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.052 # temp and temp^2 significant 

model_lm <- lm(log(speed) ~ temp + log(gs,2) + loom + I(temp^2) + date + t + t1, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.052 # temp and temp^2 significant 

model_lm <- lm(log(speed) ~ temp + log(gs,2) + loom + I(temp^2) + loom*log(gs,2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.049 # temp and temp^2 significant 

model_lm <- lm(log(speed) ~ temp +loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.049 # temp and temp^2 significant 


# predictions

model_lm <- lm(log(speed) ~ temp +loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.049 # temp and temp^2 significant 

newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1),
                                   loom = c(1,2,3,4,5)))
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

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/startle_speed99_predictions.csv")






################# avg speed #######################




data <- read.csv("../../data/temp_collective/roi/all_params_w_loom_startle_corrected.csv",header=TRUE,na.strings=c("[nan]"))

# make new dataframe

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$avg_startle_speed)],
                    "gs" = data$Groupsize[complete.cases(data$avg_startle_speed)],
                    "loom" = data$Loom[complete.cases(data$avg_startle_speed)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$avg_startle_speed)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$avg_startle_speed)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$avg_startle_speed)],
                    "subtrial" = data$Subtrial[complete.cases(data$avg_startle_speed)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$avg_startle_speed)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$avg_startle_speed)], format = "%H:%M")),
                    "t" = (as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$avg_startle_speed)], format = "%H:%M")) - 
                             as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$avg_startle_speed)], format = "%H:%M"))),
                    "speed" = data$avg_startle_speed[complete.cases(data$avg_startle_speed)]
)


model_lm <- lm(log(speed) ~ temp +loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.017 # temp and temp^2 significant 

model_lm <- lm(log(speed) ~ temp +loom + I(temp^2) + log(gs,2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#good residuals #r sq = 0.017 # temp and temp^2 significant 
# too low!


################ distance with corrected code2 ###############
data <- read.csv(here("Documents","data","temp_collective","roi","all_params_w_loom_startle_corrected.csv"),header=TRUE,na.strings=c("[nan]"))

data <- read.csv("../../data/temp_collective/roi/all_params_w_loom_startle_corrected.csv",header=TRUE,na.strings=c("[nan]"))

# make new dataframe

my_data<-data.frame("temp" = data$Temperature[data$distance != 0],
                    "gs" = data$Groupsize[data$distance != 0],
                    "loom" = data$Loom[data$distance != 0],
                    "kt"=1/(0.00008617*(data$Temperature[data$distance != 0]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[data$distance != 0], format = "%d/%m/%Y")),
                    "trial" = data$Trial[data$distance != 0],
                    "subtrial" = data$Subtrial[data$distance != 0],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[data$distance != 0], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[data$distance != 0], format = "%H:%M")),
                    "t" = (as.numeric(as.POSIXct(data$Time_start_record[data$distance != 0], format = "%H:%M")) - 
                             as.numeric(as.POSIXct(data$Time_fish_in[data$distance != 0], format = "%H:%M"))),
                    "distance" = data$distance[data$distance != 0]
)

hist(my_data$distance,20)

hist(log(my_data$distance),20)



model_lm <- lm(log(distance) ~ temp + gs + temp*gs + loom*temp + I(temp^2)*gs + loom*I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# r sq = 0.11 #interactions are all significant

model_lm <- lm(log(distance) ~ temp + gs + temp*gs + loom*temp + I(temp^2)*gs + loom*I(temp^2) + date, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# r sq = 0.1265 #interactions are all significant

model_lm <- lm(log(distance) ~ temp + gs + temp*gs + loom*temp + I(temp^2)*gs + loom*I(temp^2) + t, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# r sq = 0.1161 #interactions are all significant

model_lm <- lm(log(distance) ~ temp + gs + temp*gs + loom*temp + I(temp^2)*gs + loom*I(temp^2) + t1, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# r sq = 0.1182 #interactions are all significant


model_lm <- lm(log(distance) ~ temp + gs + temp*gs + loom*temp + I(temp^2)*gs + loom*I(temp^2) + date + loom*gs, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# r sq = 0.1275 #interactions are all significant


model_lm <- lm(log(distance) ~ temp + log(gs,2) + temp*log(gs,2) + loom*temp + I(temp^2)*log(gs,2) + loom*I(temp^2) + 
                 date + loom*log(gs,2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# r sq = 0.154 #interactions are all significant

# predictions


model_lm <- lm(log(distance) ~ temp + log(gs,2) + temp*log(gs,2) + loom*temp + I(temp^2)*log(gs,2) + loom*I(temp^2) + 
                 date + loom*log(gs,2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# r sq = 0.154 #interactions are all significant
qqnorm(residuals(model_lm))
qqline(residuals(model_lm))



newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(1,2,4,8,16),
                                   loom = c(1,2,3,4,5), date = unique(my_data$date)))
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
newData1$distance <- results[,1]
newData1$distance_025 <- results[,2]
newData1$distance_975 <- results[,3]

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/startle_distance_predictions3.csv")



################ distance with corrected code3 ###############

data <- read.csv(here("Documents","data","temp_collective","roi","all_params_w_loom_startle_corrected3.csv"),header=TRUE,na.strings=c("[nan]"))

# make new dataframe

my_data2<-data.frame("temp" = data$Temperature[complete.cases(data$distance)],
                    "gs" = data$Groupsize[complete.cases(data$distance)],
                    "loom" = data$Loom[complete.cases(data$distance)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$distance)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$distance)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$distance)],
                    "subtrial" = data$Subtrial[complete.cases(data$distance)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$distance)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$distance)], format = "%H:%M")),
                    "t" = (as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$distance)], format = "%H:%M")) - 
                             as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$distance)], format = "%H:%M"))),
                    "distance" = data$distance[complete.cases(data$distance)]
)

model_lm <- lm(log(distance) ~ temp + log(gs,2) + temp*log(gs,2) + loom*temp + I(temp^2)*log(gs,2) + loom*I(temp^2) + 
                 date + loom*log(gs,2), my_data2)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# r sq = 0.02 #interactions are not significant
qqnorm(residuals(model_lm))
qqline(residuals(model_lm))

model_lm <- lm(log(distance) ~ temp + log(gs,2) +loom, my_data2)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# r sq = 0.01 #interactions are not significant
qqnorm(residuals(model_lm))
qqline(residuals(model_lm))

model_lm <- lm(log(distance) ~ temp + I(temp^2) + log(gs,2) +loom, my_data2)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# r sq = 0.01 #interactions are not significant
qqnorm(residuals(model_lm))
qqline(residuals(model_lm))

model_lm <- lm(log(distance) ~ temp +loom + temp*loom, my_data2)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# r sq = 0.012 #interactions are not significant
qqnorm(residuals(model_lm))
qqline(residuals(model_lm))

model_lm <- lm(log(distance) ~ temp +loom + temp*loom*log(gs,2), my_data2)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# r sq = 0.012 #interactions are not significant
qqnorm(residuals(model_lm))
qqline(residuals(model_lm))


# let's look at gs 1

my_data2<-data.frame("temp" = data$Temperature[data$Groupsize==1],
                     "gs" = data$Groupsize[data$Groupsize==1],
                     "loom" = data$Loom[data$Groupsize==1],
                     "kt"=1/(0.00008617*(data$Temperature[data$Groupsize==1]+273.1)),
                     "date"=as.numeric(as.Date(data$Date[data$Groupsize==1], format = "%d/%m/%Y")),
                     "trial" = data$Trial[data$Groupsize==1],
                     "subtrial" = data$Subtrial[data$Groupsize==1],
                     "t1" = as.numeric(as.POSIXct(data$Time_fish_in[data$Groupsize==1], format = "%H:%M")),
                     "t2" = as.numeric(as.POSIXct(data$Time_start_record[data$Groupsize==1], format = "%H:%M")),
                     "t" = (as.numeric(as.POSIXct(data$Time_start_record[data$Groupsize==1], format = "%H:%M")) - 
                              as.numeric(as.POSIXct(data$Time_fish_in[data$Groupsize==1], format = "%H:%M"))),
                     "distance" = data$distance[data$Groupsize==1]
)

model_lm <- lm(log(distance) ~ temp + I(temp^2) +loom, my_data2)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# r sq = 0.01 #interactions are not significant
qqnorm(residuals(model_lm))
qqline(residuals(model_lm))
