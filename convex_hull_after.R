#convex hull area after the loom


data <- read.csv(here("Documents","data","temp_collective","roi","all_params_w_loom.csv"),header=TRUE,na.strings=c("[nan]"))


# make new dataframe

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

model_lm <- lm(hull ~ gs , my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.34
extractAIC(model_lm)
#2464


model_lm <- lm(hull ~ gs + temp , my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.3872
extractAIC(model_lm)
#2414

model_lm <- lm(hull ~ gs + temp + t, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.388
extractAIC(model_lm)
#2414

model_lm <- lm(hull ~ gs + temp + date, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.386
extractAIC(model_lm)
#2416

model_lm <- lm(hull ~ gs + temp + loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.386
extractAIC(model_lm)
#2416

model_lm <- lm(hull ~ gs + temp + t1, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.386
extractAIC(model_lm)
#2416


model_lm <- lm(hull ~ gs + temp + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.3874
extractAIC(model_lm)
#2415

model_lm <- lm(hull ~ gs + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.384
extractAIC(model_lm)
#2418

model_lm <- lm(hull ~ gs + temp + I(gs^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.388
extractAIC(model_lm)
#2414

model_lm <- lm(hull ~  temp + I(gs^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.37
extractAIC(model_lm)
#2431

model_lm <- lm(hull ~ gs*temp + gs + temp, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.386
extractAIC(model_lm)
#2416

model_lm <- lm(hull ~ loom*temp + gs + temp, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.3885
extractAIC(model_lm)
#2415

model_lm <- lm(hull ~ gs+  temp + I(gs^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.3887
extractAIC(model_lm)
#2414

#prediction
model_lm <- lm(hull ~ gs + temp , my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.3872
extractAIC(model_lm)
#2414 #Best

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

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/hull_after_loom_predictions_700_900.csv")