# goal - try one model for all params.
# one model should have temp + temp^2 + groupsize + loom .

#speed during loom

require(here)

data <- read.csv(here("Documents","data","temp_collective","roi","all_params_w_loom_corrected.csv"),header=TRUE,na.strings=c("[nan]"))


#speed during loom
my_data<-data.frame("temp" = data$Temperature[complete.cases(data$speed_percentile99)],
                    "gs" = data$Groupsize[complete.cases(data$speed_percentile99)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$speed_percentile99)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$speed_percentile99)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$speed_percentile99)],
                    "subtrial" = data$Subtrial[complete.cases(data$speed_percentile99)],
                    "loom" = data$Loom[complete.cases(data$speed_percentile99)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$speed_percentile99)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$speed_percentile99)], format = "%H:%M")),
                    "speed" = data$speed_percentile99[complete.cases(data$speed_percentile99)]
)

model_lm <- lm(sqrt(speed) ~ temp + I(temp^2) + log(gs,2) + loom ,my_data)
summary(model_lm)
rsq(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.17 #this works
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

require(rsq)


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
newData1$speed99 <- results[,1]
newData1$speed99_025 <- results[,2]
newData1$speed99_975 <- results[,3]

write.csv(newData1,here("Documents","data","temp_collective","roi","loom_speed_99_predictions_one_model_corrected.csv"))


#acc during loom

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$acc_percentile99)],
                    "gs" = data$Groupsize[complete.cases(data$acc_percentile99)],
                    "loom" = data$Loom[complete.cases(data$acc_percentile99)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$acc_percentile99)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$acc_percentile99)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$acc_percentile99)],
                    "subtrial" = data$Subtrial[complete.cases(data$acc_percentile99)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$acc_percentile99)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$acc_percentile99)], format = "%H:%M")),
                    "acc" = data$acc_percentile99[complete.cases(data$acc_percentile99)],
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$acc_percentile99)], format = "%H:%M")) - as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$acc_percentile99)], format = "%H:%M"))
)

model_lm_int <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + loom + temp*log(gs,2),my_data)
summary(model_lm_int)
rsq(model_lm_int)
plot(fitted(model_lm_int), residuals(model_lm_int))

newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(1,2,4,8,16),
                                   loom = c(1,2,3,4,5)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(model_lm_int))
  ymod <- update(model_lm_int,ynew ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=3)
results[,1] <- predict(model_lm_int,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest[j,],probs = c(0.025))
  results[j,3] <- quantile(yest[j,],probs = c(0.975))
}  
newData1$acc99 <- results[,1]
newData1$acc99_025 <- results[,2]
newData1$acc99_975 <- results[,3]

write.csv(newData1,here("Documents","data","temp_collective","roi","loom_acc_99_int_predictions_one_model_corrected.csv"))


#latency

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$latency)],
                    "gs" = data$Groupsize[complete.cases(data$latency)],
                    "loom" = data$Loom[complete.cases(data$latency)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$latency)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$latency)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$latency)],
                    "subtrial" = data$Subtrial[complete.cases(data$latency)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$latency)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$latency)], format = "%H:%M")),
                    "latency" = data$latency[complete.cases(data$latency)]/60 -10
)

#my_new_data2 <-my_data
my_new_data2 <- my_data[-c(743,741,323,686),]

model_lm <- lm(latency ~ temp + I(temp^2) + log(gs,2) + loom,my_new_data2)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
rsq(model_lm)
#0.0628


model_lm_int <- lm(latency ~ temp + I(temp^2) + log(gs,2) + loom + temp*log(gs,2),my_new_data2)
summary(model_lm_int)
plot(fitted(model_lm_int), residuals(model_lm_int))
rsq(model_lm_int)
#0.0628


lrtest(model_lm,model_lm_int) # not significant
library(faraway)

h <- hatvalues(model_lm)

halfnorm(h)

two.p.n <- 2*sum(h)/length(h)

abline(a=two.p.n,b=0)

studentized <- rstudent(model_lm)
halfnorm(studentized)

nn <- length(my_data$latency)
halfnorm(cooks <- cooks.distance(model_lm),5)
t_crit <- qt(0.025/nn,nn-5-1,lower.tail = FALSE) 
my_data[which(abs(studentized)>t_crit),]

# #looking at non corrected data
# data <- read.csv(here("Documents","data","temp_collective","roi","all_params_w_loom.csv"),header=TRUE,na.strings=c("[nan]"))
# 
# my_data<-data.frame("temp" = data$Temperature[complete.cases(data$latency)],
#                     "gs" = data$Groupsize[complete.cases(data$latency)],
#                     "loom" = data$Loom[complete.cases(data$latency)],
#                     "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$latency)]+273.1)),
#                     "date"=as.numeric(as.Date(data$Date[complete.cases(data$latency)], format = "%d/%m/%Y")),
#                     "trial" = data$Trial[complete.cases(data$latency)],
#                     "subtrial" = data$Subtrial[complete.cases(data$latency)],
#                     "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$latency)], format = "%H:%M")),
#                     "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$latency)], format = "%H:%M")),
#                     "latency" = data$latency[complete.cases(data$latency)]/60 -10
# )
# my_new_data2 <-my_data
# 
# model_lm <- lm(latency ~ temp + I(temp^2) + log(gs,2) + loom,my_new_data2)
# summary(model_lm)
# plot(fitted(model_lm), residuals(model_lm))
# rsq(model_lm)
# #0.0628
# 
# library(faraway)
# 
# h <- hatvalues(model_lm)
# 
# halfnorm(h)
# 
# two.p.n <- 2*sum(h)/length(h)
# 
# abline(a=two.p.n,b=0)
# 
# studentized <- rstudent(model_lm)
# halfnorm(studentized)
# 
# nn <- length(my_data$latency)
# halfnorm(cooks <- cooks.distance(model_lm),5)
# t_crit <- qt(0.025/nn,nn-5-1,lower.tail = FALSE) 
# my_data[which(abs(studentized)>t_crit),]

newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(2,4,8,16),
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
newData1$latency <- results[,1]
newData1$latency_025 <- results[,2]
newData1$latency_975 <- results[,3]

write.csv(newData1,here("Documents","data","temp_collective","roi","latency_seconds_predictions_one_model_corrected.csv"))

#prop ind startling

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$prop_startles)],
                    "gs" = data$Groupsize[complete.cases(data$prop_startles)],
                    "loom" = data$Loom[complete.cases(data$prop_startles)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$prop_startles)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$prop_startles)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$prop_startles)],
                    "subtrial" = data$Subtrial[complete.cases(data$prop_startles)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$prop_startles)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$prop_startles)], format = "%H:%M")),
                    "prop_startles" = data$prop_startles[complete.cases(data$prop_startles)],
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$prop_startles)], format = "%H:%M")) - as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$prop_startles)], format = "%H:%M"))
)

model_glm<-  glm(prop_startles ~ temp + I(temp^2) + loom + log(gs,2), family = binomial,my_data)
summary(model_glm)
rsq(model_glm)


library(arm)
binnedplot(fitted(model_glm),residuals(model_glm)) 


newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(1,2,4,8,16),
                                   loom = c(1,2,3,4,5)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(model_glm))
  ymod <- update(model_glm,ynew ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=3)
results[,1] <- predict(model_glm,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest[j,],probs = c(0.025))
  results[j,3] <- quantile(yest[j,],probs = c(0.975))
}  
newData1$prop_startles <- results[,1]
newData1$prop_startles025 <- results[,2]
newData1$prop_startles975 <- results[,3]

write.csv(newData1,here("Documents","data","temp_collective","roi","prop_startles_predictions_one_model_corrected.csv"))


#annd after loom

data <- read.csv(here("..","..","..","Documents","data","temp_collective","roi","all_params_w_loom.csv"),header=TRUE,na.strings=c("[nan]"))

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$annd)],
                    "gs" = data$Groupsize[complete.cases(data$annd)],
                    "loom" = data$Loom[complete.cases(data$annd)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$annd)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$annd)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$annd)],
                    "subtrial" = data$Subtrial[complete.cases(data$annd)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$annd)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$annd)], format = "%H:%M")),
                    "annd" = data$annd[complete.cases(data$annd)]
)


model_lm_trans_int <- lm(log(annd) ~ temp +log(gs,2) + loom + I(temp^2) + log(gs,2)*temp, my_data)
summary(model_lm_trans_int)
plot(fitted(model_lm_trans_int),residuals(model_lm_trans_int))
rsq(model_lm_trans_int)

newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(2,4,8,16),
                                   loom = c(1,2,3,4,5)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(model_lm_trans_int))
  ymod <- update(model_lm_trans_int,ynew ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=3)
results[,1] <- predict(model_lm_trans_int,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest[j,],probs = c(0.025))
  results[j,3] <- quantile(yest[j,],probs = c(0.975))
}  
newData1$annd <- results[,1]
newData1$annd_025 <- results[,2]
newData1$annd_975 <- results[,3]

write.csv(newData1,here("Documents","data","temp_collective","roi","annd_predictions_one_model_corrected.csv"))

#convex hull area after loom
my_data<-data.frame("temp" = data$Temperature[complete.cases(data$convex_hull_area)],
                    "gs" = data$Groupsize[complete.cases(data$convex_hull_area)],
                    "loom" = data$Loom[complete.cases(data$convex_hull_area)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$convex_hull_area)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$convex_hull_area)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$convex_hull_area)],
                    "subtrial" = data$Subtrial[complete.cases(data$convex_hull_area)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$convex_hull_area)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$convex_hull_area)], format = "%H:%M")),
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$convex_hull_area)], format = "%H:%M")) -
                      as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$convex_hull_area)], format = "%H:%M")),
                    "hull" = data$convex_hull_area[complete.cases(data$convex_hull_area)]
)

model_lm <- lm(hull^0.5 ~  temp + I(temp^2) +loom + log(gs,2) , my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
rsq(model_lm)

newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(2,4,8,16),
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
newData1$hull <- results[,1]
newData1$hull_025 <- results[,2]
newData1$hull_975 <- results[,3]

write.csv(newData1,here("Documents","data","temp_collective","roi","hull_predictions_one_model_corrected.csv"))

#polarization after loom


data <- read.csv(here("Documents","data","temp_collective","roi","all_params_w_loom_pol_corrected.csv"),header=TRUE,na.strings=c("[nan]"))


# make new dataframe

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$polarization_1_postloom)],
                    "gs" = data$Groupsize[complete.cases(data$polarization_1_postloom)],
                    "loom" = data$Loom[complete.cases(data$polarization_1_postloom)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$polarization_1_postloom)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$polarization_1_postloom)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$polarization_1_postloom)],
                    "subtrial" = data$Subtrial[complete.cases(data$polarization_1_postloom)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$polarization_1_postloom)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$polarization_1_postloom)], format = "%H:%M")),
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$polarization_1_postloom)], format = "%H:%M")) -
                      as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$polarization_1_postloom)], format = "%H:%M")),
                    "pol" = data$polarization_1_postloom[complete.cases(data$polarization_1_postloom)]
)


model_lm <- lm(sqrt(abs(pol)) ~ temp + I(temp^2) + log(gs,2) + loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#rsq 0.038 #temp and temp^2 is significant
rsq(model_lm)

newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(2,4,8,16),
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
newData1$pol <- results[,1]
newData1$pol_025 <- results[,2]
newData1$pol_975 <- results[,3]

write.csv(newData1,here("Documents","data","temp_collective","roi","pol_postloom_predictions_one_model_corrected.csv"))

#speed before loom

data <- read.csv(here("Documents","data","temp_collective","roi","all_params_wo_loom_corrected.csv"),header=TRUE,na.strings=c("[nan]"))


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

model_lm <- lm(log(speed+1) ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#0.1157
rsq(model_lm)

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
newData1$speed99 <- results[,1]
newData1$speed99_025 <- results[,2]
newData1$speed99_975 <- results[,3]

write.csv(newData1,here("Documents","data","temp_collective","roi","preloom_speed_99_predictions_one_model_corrected.csv"))


#preloom acc

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
model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.09

studentized <- rstudent(model_lm)
halfnorm(studentized)
#126

halfnorm(cooks <- cooks.distance(model_lm),5)
nn <- length(my_data$acc)
t_crit <- qt(0.025/nn,nn-4-1,lower.tail = FALSE) 
my_data[which(abs(studentized)>t_crit),]
#126

my_new_data <- my_data[-126,]

model_lm<- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2),my_new_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.09
rsq(model_lm)

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

write.csv(newData1,here("Documents","data","temp_collective","roi","preloom_acc_99_predictions_one_model_corrected.csv"))


#annd during unperturbed swimming

data <- read.csv(here("Documents","data","temp_collective","roi","all_params_wo_loom_corrected.csv"),header=TRUE,na.strings=c("[nan]"))

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$annd)],
                    "gs" = data$Groupsize[complete.cases(data$annd)],
                    "annd" = data$annd[complete.cases(data$annd)]
)

model_lm <- lm(log(annd) ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#temp and temp^2 not significant
rsq(model_lm)

#preloom hull

data <- read.csv(here("Documents","data","temp_collective","roi","all_params_wo_loom_corrected.csv"),header=TRUE,na.strings=c("[nan]"))

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$convex_hull_area)],
                    "gs" = data$Groupsize[complete.cases(data$convex_hull_area)],
                    "hull" = data$convex_hull_area[complete.cases(data$convex_hull_area)]
)

model_lm <- lm(sqrt(hull) ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#temp and temp^2 not significant
rsq(model_lm)

#pol before loom
data <- read.csv(here("Documents","data","temp_collective","roi","all_params_wo_loom_pol_corrected.csv"),header=TRUE,na.strings=c("[nan]"))

# make new dataframe

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$polarization_1)],
                    "gs" = data$Groupsize[complete.cases(data$polarization_1)],
                    "pol" = data$polarization_1[complete.cases(data$polarization_1)]
)



model_lm <- lm(sqrt(abs(pol)) ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#temp and temp^2 not significant

