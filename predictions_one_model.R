#goal - make predictions for all params with the same model

require(here)
require(lmtest)
data <- read.csv(here("..","..","..","Documents","data","temp_collective","roi","all_params_w_loom.csv"),header=TRUE,na.strings=c("[nan]"))


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
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.17 #this works
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
rsq(model_lm) #0.1731
rsq.partial(model_lm)



# testing for intereaction between temp and gs with lrt

model_lm_int <- lm(sqrt(speed) ~ temp + I(temp^2) + log(gs,2) + loom + temp*log(gs,2),my_data)
summary(model_lm_int)
plot(fitted(model_lm_int), residuals(model_lm_int))

lrtest(model_lm,model_lm_int)

# (Intercept)  2.3527820  0.2279874  10.320  < 2e-16 ***
#   temp         0.0518650  0.0252381   2.055   0.0401 *  
#   I(temp^2)   -0.0021388  0.0006589  -3.246   0.0012 ** 
#   log(gs, 2)   0.2284672  0.0192687  11.857  < 2e-16 ***
#   loom        -0.0783119  0.0189434  -4.134 3.83e-05 ***
  
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

write.csv(newData1,here("..","..","..","Documents","data","temp_collective","roi","loom_speed_99_predictions_one_model.csv"))

#acceleration during loom

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

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1763 #this works!

# (Intercept)  4.7577198  0.1671428  28.465  < 2e-16 ***
#   temp         0.0731255  0.0185027   3.952 8.23e-05 ***
#   I(temp^2)   -0.0025823  0.0004831  -5.346 1.09e-07 ***
#   log(gs, 2)   0.1498198  0.0141263  10.606  < 2e-16 ***
#   loom        -0.0568259  0.0138878  -4.092 4.58e-05 ***
#   

qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
rsq(model_lm)
rsq.partial(model_lm)
# 
# variable
# [1] "temp"       "I(temp^2)"  "log(gs, 2)" "loom"      
# 
# $partial.rsq
# [1] 0.01363420 0.02466482 0.09052954 0.01460022
# 

# testing for intereaction between temp and gs with lrt

model_lm_int <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + loom + temp*log(gs,2),my_data)
summary(model_lm_int)
plot(fitted(model_lm_int), residuals(model_lm_int))

lrtest(model_lm,model_lm_int) # significant

qqnorm(residuals(model_lm_int), main= "")
qqline(residuals(model_lm_int))
rsq(model_lm_int) #0.1823
rsq.partial(model_lm_int)

# $variable
# [1] "temp"            "I(temp^2)"       "log(gs, 2)"      "loom"            "temp:log(gs, 2)"
# 
# $partial.rsq
# [1] 0.016541258 0.025582519 0.027135200 0.014654451 0.003755625
# 
# > rsq(model_lm_int)
# [1] 0.1823035
#predictions for model without interaction

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
newData1$acc99 <- results[,1]
newData1$acc99_025 <- results[,2]
newData1$acc99_975 <- results[,3]

write.csv(newData1,here("..","..","..","Documents","data","temp_collective","roi","loom_acc_99_predictions_one_model.csv"))

#predictions for model with interaction

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

write.csv(newData1,here("..","..","..","Documents","data","temp_collective","roi","loom_acc_99_int_predictions_one_model.csv"))

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
                    "latency" = data$latency[complete.cases(data$latency)]
)



my_new_data2 <- my_data[-c(750,752,327,695),]

model_pois6 <- glm(latency ~ temp + loom + I(temp^2) + log(gs,2), family = quasipoisson, my_new_data2)
summary(model_pois6)
plot(fitted(model_pois6),residuals(model_pois6))

rsq(model_pois6)
rsq.partial(model_pois6)

# > rsq(model_pois6)
# [1] 0.06258895

# $variable
# [1] "temp"       "loom"       "I(temp^2)"  "log(gs, 2)"
# 
# $partial.rsq
# [1] 0.017143434 0.004740903 0.016090973 0.041742987

model_pois_int <- glm(latency ~ temp + loom + I(temp^2) + log(gs,2) + temp*log(gs,2), family = quasipoisson, my_new_data2)
summary(model_pois_int)
plot(fitted(model_pois_int),residuals(model_pois_int))

rsq(model_pois_int)
rsq.partial(model_pois_int)

lrtest(model_pois6,model_pois_int)


teststat <- -2*(deviance(model_pois6)-deviance(model_pois_int))
(p.val <- pchisq(teststat, df = 1, lower.tail = FALSE)) # not significant

#predictions


newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), 
                                   gs = c(1,2,4,8,16),
                                   loom = c(1,2,3,4,5)))

require(ciTools)
df_ints <- add_ci(newData1, model_pois6, names = c("lcb", "ucb"), alpha = 0.05)


write.csv(df_ints,here("..","..","..","Documents","data","temp_collective","roi","latency_predictions_one_model.csv"))


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

write.csv(newData1,here("..","..","..","Documents","data","temp_collective","roi","prop_startles_predictions_one_model.csv"))



#trying with interaction

rsq(model_glm) #0.0391
rsq.partial(model_glm)


model_glm_int<-  glm(prop_startles ~ temp + I(temp^2) + loom + log(gs,2) + temp*log(gs,2), family = binomial,my_data)
summary(model_glm_int)

library(arm)
binnedplot(fitted(model_glm_int),residuals(model_glm_int)) 

lrtest(model_glm,model_glm_int) # not significant

#speed before loom

data <- read.csv(here("..","..","..","Documents","data","temp_collective","roi","all_params_wo_loom.csv"),header=TRUE,na.strings=c("[nan]"))


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
rsq.partial(model_lm)

# [1] 0.1274204
# > rsq.partial(model_lm)
# $adjustment
# [1] FALSE
# 
# $variable
# [1] "temp"       "I(temp^2)"  "log(gs, 2)"
# 
# $partial.rsq
# [1] 2.659542e-02 1.219798e-02 1.209435e-05
model_lm_int <- lm(log(speed+1) ~ temp + I(temp^2) + log(gs,2) + temp*log(gs,2),my_data)
summary(model_lm_int)
plot(fitted(model_lm_int), residuals(model_lm_int))

lrtest(model_lm,model_lm_int) # not significant


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

write.csv(newData1,here("..","..","..","Documents","data","temp_collective","roi","preloom_speed_99_predictions_one_model.csv"))


#pre loom acceleration

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
#128

halfnorm(cooks <- cooks.distance(model_lm),5)
nn <- length(my_data$acc)
t_crit <- qt(0.025/nn,nn-4-1,lower.tail = FALSE) 
my_data[which(abs(studentized)>t_crit),]
#128

my_new_data <- my_data[-which(abs(studentized)>t_crit),]

model_lm<- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2),my_new_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.09
rsq(model_lm)
rsq.partial(model_lm)

# > rsq(model_lm)
# [1] 0.0984856
# > rsq.partial(model_lm)
# $adjustment
# [1] FALSE
# 
# $variable
# [1] "temp"       "I(temp^2)"  "log(gs, 2)"
# 
# $partial.rsq
# [1] 0.01912525 0.01266149 0.05780389

model_lm_int <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + temp*log(gs,2),my_new_data)
summary(model_lm_int)
plot(fitted(model_lm_int), residuals(model_lm_int))
#r sq = 0.08

lrtest(model_lm,model_lm_int) # not significant

#predictions

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

write.csv(newData1,here("..","..","..","Documents","data","temp_collective","roi","preloom_acc_99_predictions_one_model.csv"))


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

model_lm_trans <- lm(log(annd) ~ temp +log(gs,2) + loom + I(temp^2), my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#this works



model_lm_trans_int <- lm(log(annd) ~ temp +log(gs,2) + loom + I(temp^2) + log(gs,2)*temp, my_data)
summary(model_lm_trans_int)
plot(fitted(model_lm_trans_int),residuals(model_lm_trans_int))
#this works

lrtest(model_lm_trans,model_lm_trans_int) # significant

rsq(model_lm_trans_int)
rsq.partial(model_lm_trans_int)

# > rsq(model_lm_trans_int)
# [1] 0.4167516
# > rsq.partial(model_lm_trans_int)
# $adjustment
# [1] FALSE
# 
# $variable
# [1] "temp"            "log(gs, 2)"      "loom"            "I(temp^2)"       "temp:log(gs, 2)"
# 
# $partial.rsq
# [1] 1.104543e-02 1.134305e-01 2.800796e-06 3.398251e-03 6.344730e-03

#predictions

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

write.csv(newData1,here("..","..","..","Documents","data","temp_collective","roi","annd_predictions_one_model.csv"))


# convex hull area

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


qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

rsq(model_lm)
rsq.partial(model_lm)


model_lm_int <- lm(hull^0.5 ~  temp + I(temp^2) +loom + log(gs,2) +log(gs,2)*temp, my_data)
summary(model_lm_int)
plot(fitted(model_lm_int),residuals(model_lm_int))


lrtest(model_lm,model_lm_int) # not significant


#predictions

# > rsq(model_lm)
# [1] 0.4178037
# > rsq.partial(model_lm)
# $adjustment
# [1] FALSE
# 
# $variable
# [1] "temp"       "I(temp^2)"  "loom"       "log(gs, 2)"
# 
# $partial.rsq


# [1] 0.0060876909 0.0011404016 0.0004368472 0.3924370723
#predictions

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

write.csv(newData1,here("..","..","..","Documents","data","temp_collective","roi","hull_predictions_one_model.csv"))

#polarization after loom


data <- read.csv(here("..","..","..","Documents","data","temp_collective","roi","all_params_w_loom_pol.csv"),header=TRUE,na.strings=c("[nan]"))


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

write.csv(newData1,here("..","..","..","Documents","data","temp_collective","roi","pol_postloom_predictions_one_model.csv"))

#latency in seconds


data <- read.csv(here("..","..","..","Documents","data","temp_collective","roi","all_params_w_loom.csv"),header=TRUE,na.strings=c("[nan]"))


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
my_new_data2 <- my_data[-c(750,752,327,695),]

model_lm <- lm(latency ~ temp + I(temp^2) + log(gs,2) + loom,my_new_data2)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
rsq(model_lm)
#0.0628



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

write.csv(newData1,here("..","..","..","Documents","data","temp_collective","roi","latency_seconds_predictions_one_model.csv"))



#avg acc before loom

data <- read.csv(here("..","..","..","Documents","data","temp_collective","roi","all_params_wo_loom.csv"),header=TRUE,na.strings=c("[nan]"))


my_data<-data.frame("temp" = data$Temperature[complete.cases(data$avg_acc)],
                    "gs" = data$Groupsize[complete.cases(data$avg_acc)],
                    "acc" = data$avg_acc[complete.cases(data$avg_acc)]
)

model_lm_int <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + temp*log(gs,2),my_data)
summary(model_lm_int)
plot(fitted(model_lm_int), residuals(model_lm_int))

newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(1,2,4,8,16)))
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
newData1$avg_acc <- results[,1]
newData1$avg_acc_025 <- results[,2]
newData1$avg_acc_975 <- results[,3]

write.csv(newData1,here("..","..","..","Documents","data","temp_collective","roi","avg_acc_preloom_predictions_one_model.csv"))





#median acc before loom

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$acc_percentile50)],
                    "gs" = data$Groupsize[complete.cases(data$acc_percentile50)],
                    "acc" = data$acc_percentile50[complete.cases(data$acc_percentile50)]
)

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))

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
newData1$acc50<- results[,1]
newData1$acc50_025 <- results[,2]
newData1$acc50_975 <- results[,3]

write.csv(newData1,here("..","..","..","Documents","data","temp_collective","roi","median_acc_preloom_predictions_one_model.csv"))


# mean speed before loom


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


model_lm <- lm(log(speed+1) ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#temp terms are not significant 

model_lm_int <- lm(log(speed+1) ~ temp + I(temp^2) + log(gs,2) +temp*log(gs,2),my_data)
summary(model_lm_int)
plot(fitted(model_lm_int), residuals(model_lm_int))
#temp terms are not significant 

lrtest(model_lm,model_lm_int) #not significant 

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
newData1$avg_speed<- results[,1]
newData1$avg_speed_025 <- results[,2]
newData1$avg_speed_975 <- results[,3]

write.csv(newData1,here("..","..","..","Documents","data","temp_collective","roi","avg_speed_preloom_predictions_one_model.csv"))


#median speed before loom
my_data<-data.frame("temp" = data$Temperature[complete.cases(data$speed_percentile50)],
                    "gs" = data$Groupsize[complete.cases(data$speed_percentile50)],
                    "speed" = data$speed_percentile50[complete.cases(data$speed_percentile50)]
)

model_lm <- lm(log(speed+1) ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#temp terms are not significant 

model_lm_int <- lm(log(speed+1) ~ temp + I(temp^2) + log(gs,2) + temp*log(gs,2),my_data)
summary(model_lm_int)
plot(fitted(model_lm_int), residuals(model_lm_int))
#temp terms are not significant 


lrtest(model_lm,model_lm_int) #not significant 



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
newData1$speed50<- results[,1]
newData1$speed50_025 <- results[,2]
newData1$speed50_975 <- results[,3]

write.csv(newData1,here("..","..","..","Documents","data","temp_collective","roi","median_speed_preloom_predictions_one_model.csv"))


#mean speed during predation threat
require(rsq)
require(lmtest)
data <- read.csv(here("Documents","data","temp_collective","roi","all_params_w_loom.csv"),header=TRUE,na.strings=c("[nan]"))


my_data<-data.frame("temp" = data$Temperature[complete.cases(data$avg_speed)],
                    "gs" = data$Groupsize[complete.cases(data$avg_speed)],
                    "loom" = data$Loom[complete.cases(data$avg_speed)],
                    "speed" = data$avg_speed[complete.cases(data$avg_speed)]
)


model_lm <- lm(log(speed+1) ~ temp + I(temp^2) + log(gs,2) + loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#temp terms are not significant 
rsq(model_lm) #0.0535

qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm_int <- lm(log(speed+1) ~ temp + I(temp^2) + log(gs,2) + loom+ temp*log(gs,2),my_data)
summary(model_lm_int)
plot(fitted(model_lm_int), residuals(model_lm_int))
#temp terms are not significant 

lrtest(model_lm,model_lm_int) #not significant

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
newData1$avg_speed<- results[,1]
newData1$avg_speed_025 <- results[,2]
newData1$avg_speed_975 <- results[,3]

write.csv(newData1,here("Documents","data","temp_collective","roi","avg_speed_loom_predictions_one_model.csv"))


#median speed during predation threat

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$speed_percentile50)],
                    "gs" = data$Groupsize[complete.cases(data$speed_percentile50)],
                    "loom" = data$Loom[complete.cases(data$speed_percentile50)],
                    "speed" = data$speed_percentile50[complete.cases(data$speed_percentile50)]
)

model_lm <- lm(log(speed+1) ~ temp + I(temp^2) + log(gs,2) + loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#temp terms are not significant 
rsq(model_lm) #0.0344
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm_int <- lm(log(speed+1) ~ temp + I(temp^2) + log(gs,2) + loom + temp*log(gs,2),my_data)
summary(model_lm_int)
plot(fitted(model_lm_int), residuals(model_lm_int))
#temp terms are not significant 

lrtest(model_lm,model_lm_int) #not significant

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
newData1$speed50<- results[,1]
newData1$speed50_025 <- results[,2]
newData1$speed50_975 <- results[,3]

write.csv(newData1,here("Documents","data","temp_collective","roi","median_speed_loom_predictions_one_model.csv"))

#mean acceleration during loom

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$avg_acc)],
                    "gs" = data$Groupsize[complete.cases(data$avg_acc)],
                    "loom" = data$Loom[complete.cases(data$avg_acc)],
                    "acc" = data$avg_acc[complete.cases(data$avg_acc)]
)

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))

rsq(model_lm) #0.0797
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm_int <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + loom + temp*log(gs,2),my_data)
summary(model_lm_int)
plot(fitted(model_lm_int), residuals(model_lm_int))
rsq(model_lm_int) #0.0871

lrtest(model_lm,model_lm_int) # significant

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
newData1$avg_acc<- results[,1]
newData1$avg_acc_025 <- results[,2]
newData1$avg_acc_975 <- results[,3]

write.csv(newData1,here("Documents","data","temp_collective","roi","average_acc_loom_predictions_one_model.csv"))


#median acceleration during loom

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$acc_percentile50)],
                    "gs" = data$Groupsize[complete.cases(data$acc_percentile50)],
                    "loom" = data$Loom[complete.cases(data$acc_percentile50)],
                    "acc" = data$acc_percentile50[complete.cases(data$acc_percentile50)]
)

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))

rsq(model_lm) #0.0826
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm_int <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + loom + temp*log(gs,2),my_data)
summary(model_lm_int)
plot(fitted(model_lm_int), residuals(model_lm_int))

lrtest(model_lm,model_lm_int) # not significant

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
newData1$acc50<- results[,1]
newData1$acc50_025 <- results[,2]
newData1$acc50_975 <- results[,3]

write.csv(newData1,here("Documents","data","temp_collective","roi","median_acc_loom_predictions_one_model.csv"))

#number of startles

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$number_startles)],
                    "gs" = data$Groupsize[complete.cases(data$number_startles)],
                    "loom" = data$Loom[complete.cases(data$number_startles)],
                    "startles" = data$number_startles[complete.cases(data$number_startles)]
)

model_lm <- lm(startles ~ temp + I(temp^2) + log(gs,2) + loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))

my_data2 <-data.frame("temp" = data$Temperature[complete.cases(data$number_startles)],
                    "gs" = data$Groupsize[complete.cases(data$number_startles)],
                    "loom" = data$Loom[complete.cases(data$number_startles)],
                    "startles" = data$number_startles[complete.cases(data$number_startles)]/data$Groupsize[complete.cases((data$number_startles))]
)

model_lm <- lm(startles ~ temp + I(temp^2) + log(gs,2) + loom,my_data2)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))

require(MASS)
boxcox(model_lm, lambda = seq(0, 1, 1/10), plotit = TRUE) #0.5


model_glm <- glm.nb(startles ~ temp + I(temp^2) + log(gs,2) + loom,my_data)
summary(model_glm)
plot(fitted(model_glm), residuals(model_glm))

model_glm <- glm(startles ~ temp + I(temp^2) + log(gs,2) + loom, family = poisson, my_data)
summary(model_glm)
plot(fitted(model_glm), residuals(model_glm))

#not working

#spontaneous startles
data <- read.csv(here("Documents","data","temp_collective","roi","spontaneous_startles_preloom.csv"),header=TRUE,na.strings=c("[nan]"))


my_data<-data.frame("temp" = data$Temperature[complete.cases(data$number_startles)],
                    "gs" = data$Groupsize[complete.cases(data$number_startles)],
                    "startles" = data$number_startles[complete.cases(data$number_startles)]
)

model_lm <- lm(startles ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#residuals are really bad

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$number_startles)],
                    "gs" = data$Groupsize[complete.cases(data$number_startles)],
                    "startles" = data$number_startles[complete.cases(data$number_startles)]/data$Groupsize[complete.cases(data$number_startles)]
)

model_lm <- lm(startles ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#residuals are really bad
