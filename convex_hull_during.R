data <- read.csv(here("Documents","data","temp_collective","roi","convex_hull_during_loom.csv"),header=TRUE,na.strings=c("[nan]"))

# make new dataframe

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$convex_hull_area_600_650)],
                    "gs" = data$Groupsize[complete.cases(data$convex_hull_area_600_650)],
                    "loom" = data$Loom[complete.cases(data$convex_hull_area_600_650)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$convex_hull_area_600_650)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$convex_hull_area_600_650)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$convex_hull_area_600_650)],
                    "subtrial" = data$Subtrial[complete.cases(data$convex_hull_area_600_650)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$convex_hull_area_600_650)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$convex_hull_area_600_650)], format = "%H:%M")),
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$convex_hull_area_600_650)], format = "%H:%M"))
                    - as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$convex_hull_area_600_650)], format = "%H:%M")),
                    "hull" = data$convex_hull_area_600_650[complete.cases(data$convex_hull_area_600_650)]
)

model_lm <- lm(hull ~ gs + temp + loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.4258
extractAIC(model_lm)
#2373

model_lm <- lm(hull ~ gs + temp, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.4266
extractAIC(model_lm)
#2371 #best

model_lm <- lm(hull ~ log(gs,2) + temp, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.4261
extractAIC(model_lm)
#2372

model_lm <- lm(hull ~ I(log(gs,2)^2) + temp, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.43
extractAIC(model_lm)
#2376

model_lm <- lm(hull ~ I(gs^2) + temp, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.407
extractAIC(model_lm)
#2394

model_lm <- lm(hull ~ gs + temp + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.4263
extractAIC(model_lm)
#2372

model_lm <- lm(hull ~ gs + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.427
extractAIC(model_lm)
#2370

model_lm <- lm(hull ~ gs + I(temp^2) + date, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.4266
extractAIC(model_lm)
#2372

model_lm <- lm(hull ~ gs + I(temp^2) + t, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.4287
extractAIC(model_lm)
#2369 

model_lm <- lm(hull ~ gs + I(temp^2) + t + t1, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.428
extractAIC(model_lm)
#2371

model_lm <- lm(hull ~ gs + t, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.4193
extractAIC(model_lm)
#2380


model_lm <- lm(hull ~ gs + temp + t, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.4282
extractAIC(model_lm)
#2370 

model_lm <- lm(hull ~ gs + temp, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.4266
extractAIC(model_lm)
#2371 #best

#conclusion - convex hull area during the loom (600-650) is decreasing with increasing temperature. 
# About 42% of the variation is explained but most of the variation is explained with group size. Least AIC is one with temp.

## during loom 500 - 700 

# make new dataframe

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$convex_hull_area_500_700)],
                    "gs" = data$Groupsize[complete.cases(data$convex_hull_area_500_700)],
                    "loom" = data$Loom[complete.cases(data$convex_hull_area_500_700)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$convex_hull_area_500_700)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$convex_hull_area_500_700)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$convex_hull_area_500_700)],
                    "subtrial" = data$Subtrial[complete.cases(data$convex_hull_area_500_700)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$convex_hull_area_500_700)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$convex_hull_area_500_700)], format = "%H:%M")),
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$convex_hull_area_500_700)], format = "%H:%M"))
                    - as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$convex_hull_area_500_700)], format = "%H:%M")),
                    "hull" = data$convex_hull_area_500_700[complete.cases(data$convex_hull_area_500_700)]
)


model_lm <- lm(hull ~ gs + I(temp^2) + t, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.4329
extractAIC(model_lm)
#2362

#try glm?
model_glm <- glm(hull ~ gs + I(temp^2) + t, family = Gamma, my_data)
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm)) #residuals are really bad
#r sq = 0.4329


model_lm <- lm(hull ~ gs + temp + t, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.4321
extractAIC(model_lm)
#2363 

model_lm <- lm(hull ~ gs + temp , my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.431
extractAIC(model_lm)
#2363 #best


#prediction for 600 - 650 
my_data<-data.frame("temp" = data$Temperature[complete.cases(data$convex_hull_area_600_650)],
                    "gs" = data$Groupsize[complete.cases(data$convex_hull_area_600_650)],
                    "loom" = data$Loom[complete.cases(data$convex_hull_area_600_650)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$convex_hull_area_600_650)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$convex_hull_area_600_650)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$convex_hull_area_600_650)],
                    "subtrial" = data$Subtrial[complete.cases(data$convex_hull_area_600_650)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$convex_hull_area_600_650)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$convex_hull_area_600_650)], format = "%H:%M")),
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$convex_hull_area_600_650)], format = "%H:%M"))
                    - as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$convex_hull_area_600_650)], format = "%H:%M")),
                    "hull" = data$convex_hull_area_600_650[complete.cases(data$convex_hull_area_600_650)]
)
#predictions

model_lm <- lm(hull ~ gs + temp , my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.4266
extractAIC(model_lm)
#2371 #best

newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), 
                                   gs = c(4,8,16)))
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
newData1$hull025 <- results[,2]
newData1$hull975 <- results[,3]

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/hull_during_loom_predictions_600_650.csv")

#prediction for 500-700

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$convex_hull_area_500_700)],
                    "gs" = data$Groupsize[complete.cases(data$convex_hull_area_500_700)],
                    "loom" = data$Loom[complete.cases(data$convex_hull_area_500_700)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$convex_hull_area_500_700)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$convex_hull_area_500_700)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$convex_hull_area_500_700)],
                    "subtrial" = data$Subtrial[complete.cases(data$convex_hull_area_500_700)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$convex_hull_area_500_700)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$convex_hull_area_500_700)], format = "%H:%M")),
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$convex_hull_area_500_700)], format = "%H:%M"))
                    - as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$convex_hull_area_500_700)], format = "%H:%M")),
                    "hull" = data$convex_hull_area_500_700[complete.cases(data$convex_hull_area_500_700)]
)

model_lm <- lm(hull ~ gs + temp , my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.431
extractAIC(model_lm)
#2363 #best

newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), 
                                   gs = c(4,8,16)))
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
newData1$hull025 <- results[,2]
newData1$hull975 <- results[,3]

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/hull_during_loom_predictions_500_700.csv")