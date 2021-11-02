# latency

require(here)
data <- read.csv(here("Documents","data","temp_collective","roi","sensitivity_analysis_new.csv"),header=TRUE,na.strings=c("[nan]"))


my_data<-data.frame("temp" = data$Temperature[complete.cases(data$latency8_25_3000)],
                    "gs" = data$Groupsize[complete.cases(data$latency8_25_3000)],
                    "loom" = data$Loom[complete.cases(data$latency8_25_3000)],
                    "latency" = data$latency8_25_3000[complete.cases(data$latency8_25_3000)]/60 -10
)

my_data2<-data.frame("temp" = data$Temperature[complete.cases(data$latency10_25_3000)],
                    "gs" = data$Groupsize[complete.cases(data$latency10_25_3000)],
                    "loom" = data$Loom[complete.cases(data$latency10_25_3000)],
                    "latency" = data$latency10_25_3000[complete.cases(data$latency10_25_3000)]/60 -10
)


my_data3<-data.frame("temp" = data$Temperature[complete.cases(data$latency12_25_3000)],
                    "gs" = data$Groupsize[complete.cases(data$latency12_25_3000)],
                    "loom" = data$Loom[complete.cases(data$latency12_25_3000)],
                    "latency" = data$latency12_25_3000[complete.cases(data$latency12_25_3000)]/60 -10
)


my_new_data2 <- my_data[-c(750,752,327,695),]

model1 <-  lm(latency ~ temp + I(temp^2) + log(gs,2) + loom,my_data)

summary(model1)

model2 <-  lm(latency ~ temp + I(temp^2) + log(gs,2) + loom,my_data2)

summary(model2)

model3 <-  lm(latency ~ temp + I(temp^2) + log(gs,2) + loom,my_data3)

summary(model3)


newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(1,2,4,8,16),
                                   loom = c(1,2,3,4,5)))
boots <- 10000
yest1 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
yest2 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
yest3 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew1 <- unlist(simulate(model1))
  ymod1 <- update(model1,ynew1 ~ .)
  yest1[,i] <- predict(ymod1,newdata = newData1, type="response")
  ynew2 <- unlist(simulate(model2))
  ymod2 <- update(model2,ynew2 ~ .)
  yest2[,i] <- predict(ymod2,newdata = newData1, type="response")
  ynew3 <- unlist(simulate(model3))
  ymod3 <- update(model3,ynew3 ~ .)
  yest3[,i] <- predict(ymod3,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=9)
results[,1] <- predict(model1,newData1, type = "response")
results[,4] <- predict(model2,newData1, type = "response")
results[,7] <- predict(model3,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest1[j,],probs = c(0.025))
  results[j,3] <- quantile(yest1[j,],probs = c(0.975))
  results[j,5] <- quantile(yest2[j,],probs = c(0.025))
  results[j,6] <- quantile(yest2[j,],probs = c(0.975))
  results[j,8] <- quantile(yest3[j,],probs = c(0.025))
  results[j,9] <- quantile(yest3[j,],probs = c(0.975))}  
newData1$latency8 <- results[,1]
newData1$latency8_025 <- results[,2]
newData1$latency8_975 <- results[,3]

newData1$latency10 <- results[,4]
newData1$latency10_025 <- results[,5]
newData1$latency10_975 <- results[,6]

newData1$latency12 <- results[,7]
newData1$latency12_025 <- results[,8]
newData1$latency12_975 <- results[,9]

write.csv(newData1,here("Documents","data","temp_collective","roi","latency_seconds_predictions_one_model_threshold_sensitivity.csv"))



#prop of ind




my_data<-data.frame("temp" = data$Temperature[complete.cases(data$prop_startles8_25_3000)],
                    "gs" = data$Groupsize[complete.cases(data$prop_startles8_25_3000)],
                    "loom" = data$Loom[complete.cases(data$prop_startles8_25_3000)],
                    "prop_startles" = data$prop_startles8_25_3000[complete.cases(data$prop_startles8_25_3000)]
)

my_data2<-data.frame("temp" = data$Temperature[complete.cases(data$prop_startles10_25_3000)],
                     "gs" = data$Groupsize[complete.cases(data$prop_startles10_25_3000)],
                     "loom" = data$Loom[complete.cases(data$prop_startles10_25_3000)],
                     "prop_startles" = data$prop_startles10_25_3000[complete.cases(data$prop_startles10_25_3000)]
)


my_data3<-data.frame("temp" = data$Temperature[complete.cases(data$prop_startles12_25_3000)],
                     "gs" = data$Groupsize[complete.cases(data$prop_startles12_25_3000)],
                     "loom" = data$Loom[complete.cases(data$prop_startles12_25_3000)],
                     "prop_startles" = data$prop_startles12_25_3000[complete.cases(data$prop_startles12_25_3000)]
)

model1<-  glm(prop_startles ~ temp + I(temp^2) + loom + log(gs,2), family = binomial,my_data)
summary(model1)

model2<-  glm(prop_startles ~ temp + I(temp^2) + loom + log(gs,2), family = binomial,my_data2)
summary(model2)

model3<-  glm(prop_startles ~ temp + I(temp^2) + loom + log(gs,2), family = binomial,my_data3)
summary(model3)


newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(1,2,4,8,16),
                                   loom = c(1,2,3,4,5)))
boots <- 10000
yest1 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
yest2 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
yest3 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew1 <- unlist(simulate(model1))
  ymod1 <- update(model1,ynew1 ~ .)
  yest1[,i] <- predict(ymod1,newdata = newData1, type="response")
  ynew2 <- unlist(simulate(model2))
  ymod2 <- update(model2,ynew2 ~ .)
  yest2[,i] <- predict(ymod2,newdata = newData1, type="response")
  ynew3 <- unlist(simulate(model3))
  ymod3 <- update(model3,ynew3 ~ .)
  yest3[,i] <- predict(ymod3,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=9)
results[,1] <- predict(model1,newData1, type = "response")
results[,4] <- predict(model2,newData1, type = "response")
results[,7] <- predict(model3,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest1[j,],probs = c(0.025))
  results[j,3] <- quantile(yest1[j,],probs = c(0.975))
  results[j,5] <- quantile(yest2[j,],probs = c(0.025))
  results[j,6] <- quantile(yest2[j,],probs = c(0.975))
  results[j,8] <- quantile(yest3[j,],probs = c(0.025))
  results[j,9] <- quantile(yest3[j,],probs = c(0.975))}  
newData1$prop8 <- results[,1]
newData1$prop8_025 <- results[,2]
newData1$prop8_975 <- results[,3]

newData1$prop10 <- results[,4]
newData1$prop10_025 <- results[,5]
newData1$prop10_975 <- results[,6]

newData1$prop12 <- results[,7]
newData1$prop12_025 <- results[,8]
newData1$prop12_975 <- results[,9]

write.csv(newData1,here("Documents","data","temp_collective","roi","prop_startles_predictions_one_model_threshold_sensitivity.csv"))


#speed during loom



my_data<-data.frame("temp" = data$Temperature[complete.cases(data$speed25_3000)],
                    "gs" = data$Groupsize[complete.cases(data$speed25_3000)],
                    "loom" = data$Loom[complete.cases(data$speed25_3000)],
                    "speed" = data$speed25_3000[complete.cases(data$speed25_3000)]
)

my_data2<-data.frame("temp" = data$Temperature[complete.cases(data$speed30_3000)],
                     "gs" = data$Groupsize[complete.cases(data$speed30_3000)],
                     "loom" = data$Loom[complete.cases(data$speed30_3000)],
                     "speed" = data$speed30_3000[complete.cases(data$speed30_3000)]
)


my_data3<-data.frame("temp" = data$Temperature[complete.cases(data$speed35_3000)],
                     "gs" = data$Groupsize[complete.cases(data$speed35_3000)],
                     "loom" = data$Loom[complete.cases(data$speed35_3000)],
                     "speed" = data$speed35_3000[complete.cases(data$speed35_3000)]
)




model1 <- lm(sqrt(speed) ~ temp + I(temp^2) + log(gs,2) + loom ,my_data)
summary(model1)

model2 <- lm(sqrt(speed) ~ temp + I(temp^2) + log(gs,2) + loom ,my_data2)
summary(model2)

model3 <- lm(sqrt(speed) ~ temp + I(temp^2) + log(gs,2) + loom ,my_data3)
summary(model3)

newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(1,2,4,8,16),
                                   loom = c(1,2,3,4,5)))
boots <- 10000
yest1 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
yest2 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
yest3 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew1 <- unlist(simulate(model1))
  ymod1 <- update(model1,ynew1 ~ .)
  yest1[,i] <- predict(ymod1,newdata = newData1, type="response")
  ynew2 <- unlist(simulate(model2))
  ymod2 <- update(model2,ynew2 ~ .)
  yest2[,i] <- predict(ymod2,newdata = newData1, type="response")
  ynew3 <- unlist(simulate(model3))
  ymod3 <- update(model3,ynew3 ~ .)
  yest3[,i] <- predict(ymod3,newdata = newData1, type="response")
}

results <- matrix(NA,nrow=nrow(newData1),ncol=9)
results[,1] <- predict(model1,newData1, type = "response")
results[,4] <- predict(model2,newData1, type = "response")
results[,7] <- predict(model3,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest1[j,],probs = c(0.025))
  results[j,3] <- quantile(yest1[j,],probs = c(0.975))
  results[j,5] <- quantile(yest2[j,],probs = c(0.025))
  results[j,6] <- quantile(yest2[j,],probs = c(0.975))
  results[j,8] <- quantile(yest3[j,],probs = c(0.025))
  results[j,9] <- quantile(yest3[j,],probs = c(0.975))}  
newData1$speed25 <- results[,1]
newData1$speed25_025 <- results[,2]
newData1$speed25_975 <- results[,3]

newData1$speed30 <- results[,4]
newData1$speed30_025 <- results[,5]
newData1$speed30_975 <- results[,6]

newData1$speed35 <- results[,7]
newData1$speed35_025 <- results[,8]
newData1$speed35_975 <- results[,9]

write.csv(newData1,here("Documents","data","temp_collective","roi","speed_predictions_one_model_speed_threshold_sensitivity.csv"))



# latency and speed threshold

require(here)
data <- read.csv(here("Documents","data","temp_collective","roi","sensitivity_analysis_new.csv"),header=TRUE,na.strings=c("[nan]"))


my_data<-data.frame("temp" = data$Temperature[complete.cases(data$latency10_25_3000)],
                    "gs" = data$Groupsize[complete.cases(data$latency10_25_3000)],
                    "loom" = data$Loom[complete.cases(data$latency10_25_3000)],
                    "latency" = data$latency10_25_3000[complete.cases(data$latency10_25_3000)]/60 -10
)

my_data2<-data.frame("temp" = data$Temperature[complete.cases(data$latency10_30_3000)],
                     "gs" = data$Groupsize[complete.cases(data$latency10_30_3000)],
                     "loom" = data$Loom[complete.cases(data$latency10_30_3000)],
                     "latency" = data$latency10_30_3000[complete.cases(data$latency10_30_3000)]/60 -10
)


my_data3<-data.frame("temp" = data$Temperature[complete.cases(data$latency10_35_3000)],
                     "gs" = data$Groupsize[complete.cases(data$latency10_35_3000)],
                     "loom" = data$Loom[complete.cases(data$latency10_35_3000)],
                     "latency" = data$latency10_35_3000[complete.cases(data$latency10_35_3000)]/60 -10
)


my_new_data2 <- my_data[-c(750,752,327,695),]

model1 <-  lm(latency ~ temp + I(temp^2) + log(gs,2) + loom,my_data)

summary(model1)

model2 <-  lm(latency ~ temp + I(temp^2) + log(gs,2) + loom,my_data2)

summary(model2)

model3 <-  lm(latency ~ temp + I(temp^2) + log(gs,2) + loom,my_data3)

summary(model3)


newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(1,2,4,8,16),
                                   loom = c(1,2,3,4,5)))
boots <- 10000
yest1 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
yest2 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
yest3 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew1 <- unlist(simulate(model1))
  ymod1 <- update(model1,ynew1 ~ .)
  yest1[,i] <- predict(ymod1,newdata = newData1, type="response")
  ynew2 <- unlist(simulate(model2))
  ymod2 <- update(model2,ynew2 ~ .)
  yest2[,i] <- predict(ymod2,newdata = newData1, type="response")
  ynew3 <- unlist(simulate(model3))
  ymod3 <- update(model3,ynew3 ~ .)
  yest3[,i] <- predict(ymod3,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=9)
results[,1] <- predict(model1,newData1, type = "response")
results[,4] <- predict(model2,newData1, type = "response")
results[,7] <- predict(model3,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest1[j,],probs = c(0.025))
  results[j,3] <- quantile(yest1[j,],probs = c(0.975))
  results[j,5] <- quantile(yest2[j,],probs = c(0.025))
  results[j,6] <- quantile(yest2[j,],probs = c(0.975))
  results[j,8] <- quantile(yest3[j,],probs = c(0.025))
  results[j,9] <- quantile(yest3[j,],probs = c(0.975))}  
newData1$latency25 <- results[,1]
newData1$latency25_025 <- results[,2]
newData1$latency25_975 <- results[,3]

newData1$latency30 <- results[,4]
newData1$latency30_025 <- results[,5]
newData1$latency30_975 <- results[,6]

newData1$latency35 <- results[,7]
newData1$latency35_025 <- results[,8]
newData1$latency35_975 <- results[,9]

write.csv(newData1,here("Documents","data","temp_collective","roi","latency_seconds_predictions_one_model_speed_threshold_sensitivity.csv"))





# latency and acc threshold

require(here)
data <- read.csv(here("Documents","data","temp_collective","roi","sensitivity_analysis_new.csv"),header=TRUE,na.strings=c("[nan]"))


my_data<-data.frame("temp" = data$Temperature[complete.cases(data$latency10_30_2000)],
                    "gs" = data$Groupsize[complete.cases(data$latency10_30_2000)],
                    "loom" = data$Loom[complete.cases(data$latency10_30_2000)],
                    "latency" = data$latency10_30_2000[complete.cases(data$latency10_30_2000)]/60 -10
)

my_data2<-data.frame("temp" = data$Temperature[complete.cases(data$latency10_30_3000)],
                     "gs" = data$Groupsize[complete.cases(data$latency10_30_3000)],
                     "loom" = data$Loom[complete.cases(data$latency10_30_3000)],
                     "latency" = data$latency10_30_3000[complete.cases(data$latency10_30_3000)]/60 -10
)


my_data3<-data.frame("temp" = data$Temperature[complete.cases(data$latency10_30_4000)],
                     "gs" = data$Groupsize[complete.cases(data$latency10_30_4000)],
                     "loom" = data$Loom[complete.cases(data$latency10_30_4000)],
                     "latency" = data$latency10_30_4000[complete.cases(data$latency10_30_4000)]/60 -10
)


my_new_data2 <- my_data[-c(750,752,327,695),]

model1 <-  lm(latency ~ temp + I(temp^2) + log(gs,2) + loom,my_data)

summary(model1)

model2 <-  lm(latency ~ temp + I(temp^2) + log(gs,2) + loom,my_data2)

summary(model2)

model3 <-  lm(latency ~ temp + I(temp^2) + log(gs,2) + loom,my_data3)

summary(model3)


newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(1,2,4,8,16),
                                   loom = c(1,2,3,4,5)))
boots <- 10000
yest1 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
yest2 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
yest3 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew1 <- unlist(simulate(model1))
  ymod1 <- update(model1,ynew1 ~ .)
  yest1[,i] <- predict(ymod1,newdata = newData1, type="response")
  ynew2 <- unlist(simulate(model2))
  ymod2 <- update(model2,ynew2 ~ .)
  yest2[,i] <- predict(ymod2,newdata = newData1, type="response")
  ynew3 <- unlist(simulate(model3))
  ymod3 <- update(model3,ynew3 ~ .)
  yest3[,i] <- predict(ymod3,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=9)
results[,1] <- predict(model1,newData1, type = "response")
results[,4] <- predict(model2,newData1, type = "response")
results[,7] <- predict(model3,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest1[j,],probs = c(0.025))
  results[j,3] <- quantile(yest1[j,],probs = c(0.975))
  results[j,5] <- quantile(yest2[j,],probs = c(0.025))
  results[j,6] <- quantile(yest2[j,],probs = c(0.975))
  results[j,8] <- quantile(yest3[j,],probs = c(0.025))
  results[j,9] <- quantile(yest3[j,],probs = c(0.975))}  
newData1$latency2000 <- results[,1]
newData1$latency2000_025 <- results[,2]
newData1$latency2000_975 <- results[,3]

newData1$latency3000 <- results[,4]
newData1$latency3000_025 <- results[,5]
newData1$latency3000_975 <- results[,6]

newData1$latency4000 <- results[,7]
newData1$latency4000_025 <- results[,8]
newData1$latency4000_975 <- results[,9]

write.csv(newData1,here("Documents","data","temp_collective","roi","latency_seconds_predictions_one_model_acc_threshold_sensitivity.csv"))


# prop - speed threshold


my_data<-data.frame("temp" = data$Temperature[complete.cases(data$prop_startles10_25_3000)],
                    "gs" = data$Groupsize[complete.cases(data$prop_startles10_25_3000)],
                    "loom" = data$Loom[complete.cases(data$prop_startles10_25_3000)],
                    "prop_startles" = data$prop_startles10_25_3000[complete.cases(data$prop_startles10_25_3000)]
)

my_data2<-data.frame("temp" = data$Temperature[complete.cases(data$prop_startles10_30_3000)],
                     "gs" = data$Groupsize[complete.cases(data$prop_startles10_30_3000)],
                     "loom" = data$Loom[complete.cases(data$prop_startles10_30_3000)],
                     "prop_startles" = data$prop_startles10_30_3000[complete.cases(data$prop_startles10_30_3000)]
)


my_data3<-data.frame("temp" = data$Temperature[complete.cases(data$prop_startles10_35_3000)],
                     "gs" = data$Groupsize[complete.cases(data$prop_startles10_35_3000)],
                     "loom" = data$Loom[complete.cases(data$prop_startles10_35_3000)],
                     "prop_startles" = data$prop_startles10_35_3000[complete.cases(data$prop_startles10_35_3000)]
)

model1<-  glm(prop_startles ~ temp + I(temp^2) + loom + log(gs,2), family = binomial,my_data)
summary(model1)

model2<-  glm(prop_startles ~ temp + I(temp^2) + loom + log(gs,2), family = binomial,my_data2)
summary(model2)

model3<-  glm(prop_startles ~ temp + I(temp^2) + loom + log(gs,2), family = binomial,my_data3)
summary(model3)


newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(1,2,4,8,16),
                                   loom = c(1,2,3,4,5)))
boots <- 10000
yest1 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
yest2 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
yest3 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew1 <- unlist(simulate(model1))
  ymod1 <- update(model1,ynew1 ~ .)
  yest1[,i] <- predict(ymod1,newdata = newData1, type="response")
  ynew2 <- unlist(simulate(model2))
  ymod2 <- update(model2,ynew2 ~ .)
  yest2[,i] <- predict(ymod2,newdata = newData1, type="response")
  ynew3 <- unlist(simulate(model3))
  ymod3 <- update(model3,ynew3 ~ .)
  yest3[,i] <- predict(ymod3,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=9)
results[,1] <- predict(model1,newData1, type = "response")
results[,4] <- predict(model2,newData1, type = "response")
results[,7] <- predict(model3,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest1[j,],probs = c(0.025))
  results[j,3] <- quantile(yest1[j,],probs = c(0.975))
  results[j,5] <- quantile(yest2[j,],probs = c(0.025))
  results[j,6] <- quantile(yest2[j,],probs = c(0.975))
  results[j,8] <- quantile(yest3[j,],probs = c(0.025))
  results[j,9] <- quantile(yest3[j,],probs = c(0.975))}  
newData1$prop25 <- results[,1]
newData1$prop25_025 <- results[,2]
newData1$prop25_975 <- results[,3]

newData1$prop30 <- results[,4]
newData1$prop30_025 <- results[,5]
newData1$prop30_975 <- results[,6]

newData1$prop35 <- results[,7]
newData1$prop35_025 <- results[,8]
newData1$prop35_975 <- results[,9]

write.csv(newData1,here("Documents","data","temp_collective","roi","prop_startles_predictions_one_model_speed_threshold_sensitivity.csv"))



# prop - acc threshold


my_data<-data.frame("temp" = data$Temperature[complete.cases(data$prop_startles10_30_2000)],
                    "gs" = data$Groupsize[complete.cases(data$prop_startles10_30_2000)],
                    "loom" = data$Loom[complete.cases(data$prop_startles10_30_2000)],
                    "prop_startles" = data$prop_startles10_30_2000[complete.cases(data$prop_startles10_30_2000)]
)

my_data2<-data.frame("temp" = data$Temperature[complete.cases(data$prop_startles10_30_3000)],
                     "gs" = data$Groupsize[complete.cases(data$prop_startles10_30_3000)],
                     "loom" = data$Loom[complete.cases(data$prop_startles10_30_3000)],
                     "prop_startles" = data$prop_startles10_30_3000[complete.cases(data$prop_startles10_30_3000)]
)


my_data3<-data.frame("temp" = data$Temperature[complete.cases(data$prop_startles10_30_4000)],
                     "gs" = data$Groupsize[complete.cases(data$prop_startles10_30_4000)],
                     "loom" = data$Loom[complete.cases(data$prop_startles10_30_4000)],
                     "prop_startles" = data$prop_startles10_30_4000[complete.cases(data$prop_startles10_30_4000)]
)

model1<-  glm(prop_startles ~ temp + I(temp^2) + loom + log(gs,2), family = binomial,my_data)
summary(model1)

model2<-  glm(prop_startles ~ temp + I(temp^2) + loom + log(gs,2), family = binomial,my_data2)
summary(model2)

model3<-  glm(prop_startles ~ temp + I(temp^2) + loom + log(gs,2), family = binomial,my_data3)
summary(model3)


newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(1,2,4,8,16),
                                   loom = c(1,2,3,4,5)))
boots <- 10000
yest1 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
yest2 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
yest3 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew1 <- unlist(simulate(model1))
  ymod1 <- update(model1,ynew1 ~ .)
  yest1[,i] <- predict(ymod1,newdata = newData1, type="response")
  ynew2 <- unlist(simulate(model2))
  ymod2 <- update(model2,ynew2 ~ .)
  yest2[,i] <- predict(ymod2,newdata = newData1, type="response")
  ynew3 <- unlist(simulate(model3))
  ymod3 <- update(model3,ynew3 ~ .)
  yest3[,i] <- predict(ymod3,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=9)
results[,1] <- predict(model1,newData1, type = "response")
results[,4] <- predict(model2,newData1, type = "response")
results[,7] <- predict(model3,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest1[j,],probs = c(0.025))
  results[j,3] <- quantile(yest1[j,],probs = c(0.975))
  results[j,5] <- quantile(yest2[j,],probs = c(0.025))
  results[j,6] <- quantile(yest2[j,],probs = c(0.975))
  results[j,8] <- quantile(yest3[j,],probs = c(0.025))
  results[j,9] <- quantile(yest3[j,],probs = c(0.975))}  
newData1$prop2000 <- results[,1]
newData1$prop2000_025 <- results[,2]
newData1$prop2000_975 <- results[,3]

newData1$prop3000 <- results[,4]
newData1$prop3000_025 <- results[,5]
newData1$prop3000_975 <- results[,6]

newData1$prop4000 <- results[,7]
newData1$prop4000_025 <- results[,8]
newData1$prop4000_975 <- results[,9]

write.csv(newData1,here("Documents","data","temp_collective","roi","prop_startles_predictions_one_model_acc_threshold_sensitivity.csv"))



#speed during loom - acc threshold 



my_data<-data.frame("temp" = data$Temperature[complete.cases(data$speed30_2000)],
                    "gs" = data$Groupsize[complete.cases(data$speed30_2000)],
                    "loom" = data$Loom[complete.cases(data$speed30_2000)],
                    "speed" = data$speed30_2000[complete.cases(data$speed30_2000)]
)

my_data2<-data.frame("temp" = data$Temperature[complete.cases(data$speed30_3000)],
                     "gs" = data$Groupsize[complete.cases(data$speed30_3000)],
                     "loom" = data$Loom[complete.cases(data$speed30_3000)],
                     "speed" = data$speed30_3000[complete.cases(data$speed30_3000)]
)


my_data3<-data.frame("temp" = data$Temperature[complete.cases(data$speed30_4000)],
                     "gs" = data$Groupsize[complete.cases(data$speed30_4000)],
                     "loom" = data$Loom[complete.cases(data$speed30_4000)],
                     "speed" = data$speed30_4000[complete.cases(data$speed30_4000)]
)




model1 <- lm(sqrt(speed) ~ temp + I(temp^2) + log(gs,2) + loom ,my_data)
summary(model1)

model2 <- lm(sqrt(speed) ~ temp + I(temp^2) + log(gs,2) + loom ,my_data2)
summary(model2)

model3 <- lm(sqrt(speed) ~ temp + I(temp^2) + log(gs,2) + loom ,my_data3)
summary(model3)

newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(1,2,4,8,16),
                                   loom = c(1,2,3,4,5)))
boots <- 10000
yest1 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
yest2 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
yest3 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew1 <- unlist(simulate(model1))
  ymod1 <- update(model1,ynew1 ~ .)
  yest1[,i] <- predict(ymod1,newdata = newData1, type="response")
  ynew2 <- unlist(simulate(model2))
  ymod2 <- update(model2,ynew2 ~ .)
  yest2[,i] <- predict(ymod2,newdata = newData1, type="response")
  ynew3 <- unlist(simulate(model3))
  ymod3 <- update(model3,ynew3 ~ .)
  yest3[,i] <- predict(ymod3,newdata = newData1, type="response")
}

results <- matrix(NA,nrow=nrow(newData1),ncol=9)
results[,1] <- predict(model1,newData1, type = "response")
results[,4] <- predict(model2,newData1, type = "response")
results[,7] <- predict(model3,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest1[j,],probs = c(0.025))
  results[j,3] <- quantile(yest1[j,],probs = c(0.975))
  results[j,5] <- quantile(yest2[j,],probs = c(0.025))
  results[j,6] <- quantile(yest2[j,],probs = c(0.975))
  results[j,8] <- quantile(yest3[j,],probs = c(0.025))
  results[j,9] <- quantile(yest3[j,],probs = c(0.975))}  
newData1$speed2000 <- results[,1]
newData1$speed2000_025 <- results[,2]
newData1$speed2500_975 <- results[,3]

newData1$speed3000 <- results[,4]
newData1$speed3000_025 <- results[,5]
newData1$speed3000_975 <- results[,6]

newData1$speed4000 <- results[,7]
newData1$speed4000_025 <- results[,8]
newData1$speed4000_975 <- results[,9]

write.csv(newData1,here("Documents","data","temp_collective","roi","speed_predictions_one_model_acc_threshold_sensitivity.csv"))




# acc - speed threshold sensitivity


my_data<-data.frame("temp" = data$Temperature[complete.cases(data$acc25_3000)],
                    "gs" = data$Groupsize[complete.cases(data$acc25_3000)],
                    "loom" = data$Loom[complete.cases(data$acc25_3000)],
                    "acc" = data$acc25_3000[complete.cases(data$acc25_3000)]
)

my_data2<-data.frame("temp" = data$Temperature[complete.cases(data$acc30_3000)],
                     "gs" = data$Groupsize[complete.cases(data$acc30_3000)],
                     "loom" = data$Loom[complete.cases(data$acc30_3000)],
                     "acc" = data$acc30_3000[complete.cases(data$acc30_3000)]
)


my_data3<-data.frame("temp" = data$Temperature[complete.cases(data$acc35_3000)],
                     "gs" = data$Groupsize[complete.cases(data$acc35_3000)],
                     "loom" = data$Loom[complete.cases(data$acc35_3000)],
                     "acc" = data$acc35_3000[complete.cases(data$acc35_3000)]
)

model1 <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + loom + temp*log(gs,2),my_data)
summary(model1)

model2 <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + loom + temp*log(gs,2),my_data2)
summary(model2)


model3 <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + loom + temp*log(gs,2),my_data3)
summary(model3)

newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(1,2,4,8,16),
                                   loom = c(1,2,3,4,5)))
boots <- 10000
yest1 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
yest2 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
yest3 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew1 <- unlist(simulate(model1))
  ymod1 <- update(model1,ynew1 ~ .)
  yest1[,i] <- predict(ymod1,newdata = newData1, type="response")
  ynew2 <- unlist(simulate(model2))
  ymod2 <- update(model2,ynew2 ~ .)
  yest2[,i] <- predict(ymod2,newdata = newData1, type="response")
  ynew3 <- unlist(simulate(model3))
  ymod3 <- update(model3,ynew3 ~ .)
  yest3[,i] <- predict(ymod3,newdata = newData1, type="response")
}

results <- matrix(NA,nrow=nrow(newData1),ncol=9)
results[,1] <- predict(model1,newData1, type = "response")
results[,4] <- predict(model2,newData1, type = "response")
results[,7] <- predict(model3,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest1[j,],probs = c(0.025))
  results[j,3] <- quantile(yest1[j,],probs = c(0.975))
  results[j,5] <- quantile(yest2[j,],probs = c(0.025))
  results[j,6] <- quantile(yest2[j,],probs = c(0.975))
  results[j,8] <- quantile(yest3[j,],probs = c(0.025))
  results[j,9] <- quantile(yest3[j,],probs = c(0.975))}  
newData1$acc25 <- results[,1]
newData1$acc25_025 <- results[,2]
newData1$acc25_975 <- results[,3]

newData1$acc30 <- results[,4]
newData1$acc30_025 <- results[,5]
newData1$acc30_975 <- results[,6]

newData1$acc35 <- results[,7]
newData1$acc35_025 <- results[,8]
newData1$acc35_975 <- results[,9]

write.csv(newData1,here("Documents","data","temp_collective","roi","acc_predictions_one_model_speed_threshold_sensitivity.csv"))



# acc - acc threshold sensitivity


my_data<-data.frame("temp" = data$Temperature[complete.cases(data$acc30_2000)],
                    "gs" = data$Groupsize[complete.cases(data$acc30_2000)],
                    "loom" = data$Loom[complete.cases(data$acc30_2000)],
                    "acc" = data$acc30_2000[complete.cases(data$acc30_2000)]
)

my_data2<-data.frame("temp" = data$Temperature[complete.cases(data$acc30_3000)],
                     "gs" = data$Groupsize[complete.cases(data$acc30_3000)],
                     "loom" = data$Loom[complete.cases(data$acc30_3000)],
                     "acc" = data$acc30_3000[complete.cases(data$acc30_3000)]
)


my_data3<-data.frame("temp" = data$Temperature[complete.cases(data$acc30_4000)],
                     "gs" = data$Groupsize[complete.cases(data$acc30_4000)],
                     "loom" = data$Loom[complete.cases(data$acc30_4000)],
                     "acc" = data$acc30_4000[complete.cases(data$acc30_4000)]
)

model1 <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + loom + temp*log(gs,2),my_data)
summary(model1)

model2 <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + loom + temp*log(gs,2),my_data2)
summary(model2)


model3 <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + loom + temp*log(gs,2),my_data3)
summary(model3)

newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(1,2,4,8,16),
                                   loom = c(1,2,3,4,5)))
boots <- 10000
yest1 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
yest2 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
yest3 <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew1 <- unlist(simulate(model1))
  ymod1 <- update(model1,ynew1 ~ .)
  yest1[,i] <- predict(ymod1,newdata = newData1, type="response")
  ynew2 <- unlist(simulate(model2))
  ymod2 <- update(model2,ynew2 ~ .)
  yest2[,i] <- predict(ymod2,newdata = newData1, type="response")
  ynew3 <- unlist(simulate(model3))
  ymod3 <- update(model3,ynew3 ~ .)
  yest3[,i] <- predict(ymod3,newdata = newData1, type="response")
}

results <- matrix(NA,nrow=nrow(newData1),ncol=9)
results[,1] <- predict(model1,newData1, type = "response")
results[,4] <- predict(model2,newData1, type = "response")
results[,7] <- predict(model3,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest1[j,],probs = c(0.025))
  results[j,3] <- quantile(yest1[j,],probs = c(0.975))
  results[j,5] <- quantile(yest2[j,],probs = c(0.025))
  results[j,6] <- quantile(yest2[j,],probs = c(0.975))
  results[j,8] <- quantile(yest3[j,],probs = c(0.025))
  results[j,9] <- quantile(yest3[j,],probs = c(0.975))}  
newData1$acc2000 <- results[,1]
newData1$acc2000_025 <- results[,2]
newData1$acc2000_975 <- results[,3]

newData1$acc3000 <- results[,4]
newData1$acc3000_025 <- results[,5]
newData1$acc3000_975 <- results[,6]

newData1$acc4000 <- results[,7]
newData1$acc4000_025 <- results[,8]
newData1$acc4000_975 <- results[,9]

write.csv(newData1,here("Documents","data","temp_collective","roi","acc_predictions_one_model_acc_threshold_sensitivity.csv"))




