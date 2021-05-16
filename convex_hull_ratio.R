#goal - stats on convex hull ratio
data <- read.csv("../../data/temp_collective/roi/convex_hull_ratio_600_650w_loom.csv")

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$convex_hull_area_600_650)],
                    "gs" = data$Groupsize[complete.cases(data$convex_hull_area_600_650)],
                    "loom" = data$Loom[complete.cases(data$convex_hull_area_600_650)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$convex_hull_area_600_650)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$convex_hull_area_600_650)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$convex_hull_area_600_650)],
                    "subtrial" = data$Subtrial[complete.cases(data$convex_hull_area_600_650)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$convex_hull_area_600_650)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$convex_hull_area_600_650)], format = "%H:%M")),
                    "hull" = data$convex_hull_area_600_650[complete.cases(data$convex_hull_area_600_650)]
)

hist(my_data$hull)

model_lm <- lm(hull~ gs + temp + loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))

model_lm <- lm(hull~ gs + temp + loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))

model_lm <- lm(log(hull) ~ gs + temp + loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))


model_lm <- lm(log(hull) ~ gs + temp + loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.062

model_lm <- lm(log(hull) ~ gs*temp + loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.063

model_lm <- lm(log(hull) ~ gs*temp*loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.077

model_lm <- lm(log(hull) ~ gs*temp*loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.0812

model_lm <- lm(log(hull) ~ gs + temp*loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.0828

model_lm <- lm(log(hull) ~ gs + temp*loom + I(temp^2) + I(gs^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.0808

model_lm <- lm(log(hull) ~ gs + temp + I(temp^2)*loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.077

model_lm <- lm(log(hull) ~ log(gs,2) + temp*loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.08216
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm <- lm(log(hull+1) ~ gs + temp*loom + I(temp^2) + date + trial + t1 + t2, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.0854
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

#try glm
model_glm <- glm(hull ~ gs + temp*loom + I(temp^2) + date + trial + t1 + t2, my_data, family = "Gamma")
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
#aic = 190

#goodness of fit?
X2 <- sum((my_data$hull - fitted(model_glm))^2/fitted(model_glm))
nn <- length(my_data$hull)
pchisq(X2, df = nn-length(coef(model_glm)), lower.tail = FALSE)
#very high! = 1

#try glm without extra variables
model_glm <- glm(hull ~ gs + temp*loom + I(temp^2), my_data, family = "Gamma")
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
#aic = 194

#try glm without extra variables
model_glm <- glm(hull ~ gs*temp*loom + I(temp^2), my_data, family = "Gamma")
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
#aic = 199


#try glm without extra variables
model_glm <- glm(hull ~ gs*temp + temp*loom + I(temp^2), my_data, family = "Gamma")
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
#aic = 196

#try glm with date
model_glm <- glm(hull ~ gs + temp*loom + I(temp^2) + date, my_data, family = "Gamma")
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
#aic = 186

#goodness of fit?
X2 <- sum((my_data$hull - fitted(model_glm))^2/fitted(model_glm))
nn <- length(my_data$hull)
pchisq(X2, df = nn-length(coef(model_glm)), lower.tail = FALSE)
#very high! = 1 same!


#predictions
newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), 
                                   gs = c(4,8,16),
                                   loom = c(1,2,3,4,5), date = unique(my_data$date)))
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
newData1$hull <- results[,1]
newData1$hull025 <- results[,2]
newData1$hull975 <- results[,3]

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/hull_ratio_600_650_predictions.csv")

#gs 4 loom 1 date 18101
intersection <- intersect(which(newData1$gs == 4),
                          intersect(which(newData1$loom == 1),which(newData1$date == 18101)))

temp <- newData1$temp[intersection]

plot(my_data$temp, my_data$hull)
lines(temp, (results[intersection,1]),lty = "solid")
lines(temp, (results[intersection,2]),lty = "dashed")
lines(temp, (results[intersection,3]),lty = "dashed")

#gs 4 loom 1 date 18113
intersection <- intersect(which(newData1$gs == 4),
                          intersect(which(newData1$loom == 1),which(newData1$date == 18113)))

temp <- newData1$temp[intersection]

lines(temp, (results[intersection,1]),lty = "solid", col = "green")
lines(temp, (results[intersection,2]),lty = "dashed", col = "green")
lines(temp, (results[intersection,3]),lty = "dashed", col = "green")

#gs 4 loom 5
intersection <- intersect(which(newData1$gs == 4),
                          intersect(which(newData1$loom == 5),which(newData1$date == 18106)))

temp <- newData1$temp[intersection]

lines(temp, results[intersection,1],lty = "solid", col = "green")
lines(temp, results[intersection,2],lty = "dashed", col = "green")
lines(temp, results[intersection,3],lty = "dashed", col = "green")

#gs 16 loom 1
intersection <- intersect(which(newData1$gs == 16),
                          intersect(which(newData1$loom == 1),which(newData1$date == 18106)))

temp <- newData1$temp[intersection]

lines(temp, results[intersection,1],lty = "solid", col = "red")
lines(temp, results[intersection,2],lty = "dashed", col = "red")
lines(temp, results[intersection,3],lty = "dashed", col = "red")



############# ratios with only one frame
data <- read.csv("../../data/temp_collective/roi/convex_hull_ratio_w_loom.csv")

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$convex_hull_area_649_650)],
                    "gs" = data$Groupsize[complete.cases(data$convex_hull_area_649_650)],
                    "loom" = data$Loom[complete.cases(data$convex_hull_area_649_650)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$convex_hull_area_649_650)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$convex_hull_area_649_650)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$convex_hull_area_649_650)],
                    "subtrial" = data$Subtrial[complete.cases(data$convex_hull_area_649_650)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$convex_hull_area_649_650)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$convex_hull_area_649_650)], format = "%H:%M")),
                    "hull" = data$convex_hull_area_649_650[complete.cases(data$convex_hull_area_649_650)]
)

hist(my_data$hull)

model_lm <- lm(log(hull+1) ~ gs + temp*loom + I(temp^2) + date + trial + t1 + t2, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.07
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

#try glm with date
model_glm <- glm(hull ~ gs + temp*loom + I(temp^2) + date, my_data, family = "Gamma")
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))


#goodness of fit?
X2 <- sum((my_data$hull - fitted(model_glm))^2/fitted(model_glm))
nn <- length(my_data$hull)
pchisq(X2, df = nn-length(coef(model_glm)), lower.tail = FALSE)
#very high! = 1 same!


#predictions
newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), 
                                   gs = c(4,8,16),
                                   loom = c(1,2,3,4,5), date = unique(my_data$date)))
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
newData1$hull <- results[,1]
newData1$hull025 <- results[,2]
newData1$hull975 <- results[,3]

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/hull_ratio_649_650_predictions.csv")

#gs 4 loom 1 date 18101
intersection <- intersect(which(newData1$gs == 4),
                          intersect(which(newData1$loom == 1),which(newData1$date == 18101)))

temp <- newData1$temp[intersection]

plot(my_data$temp, my_data$hull)
lines(temp, (results[intersection,1]),lty = "solid")
lines(temp, (results[intersection,2]),lty = "dashed")
lines(temp, (results[intersection,3]),lty = "dashed")

#gs 4 loom 1 date 18113
intersection <- intersect(which(newData1$gs == 4),
                          intersect(which(newData1$loom == 1),which(newData1$date == 18113)))

temp <- newData1$temp[intersection]

lines(temp, (results[intersection,1]),lty = "solid", col = "green")
lines(temp, (results[intersection,2]),lty = "dashed", col = "green")
lines(temp, (results[intersection,3]),lty = "dashed", col = "green")

#gs 4 loom 5
intersection <- intersect(which(newData1$gs == 4),
                          intersect(which(newData1$loom == 5),which(newData1$date == 18106)))

temp <- newData1$temp[intersection]

lines(temp, results[intersection,1],lty = "solid", col = "green")
lines(temp, results[intersection,2],lty = "dashed", col = "green")
lines(temp, results[intersection,3],lty = "dashed", col = "green")

#gs 16 loom 1
intersection <- intersect(which(newData1$gs == 16),
                          intersect(which(newData1$loom == 1),which(newData1$date == 18106)))

temp <- newData1$temp[intersection]

lines(temp, results[intersection,1],lty = "solid", col = "red")
lines(temp, results[intersection,2],lty = "dashed", col = "red")
lines(temp, results[intersection,3],lty = "dashed", col = "red")

################################################

############ using second csv file #############

################################################

data <- read.csv("../../data/temp_collective/roi/convex_hull_ratio_600_650w_loom.csv")

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$convex_hull_area_ratio)],
                    "gs" = data$Groupsize[complete.cases(data$convex_hull_area_ratio)],
                    "loom" = data$Loom[complete.cases(data$convex_hull_area_ratio)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$convex_hull_area_ratio)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$convex_hull_area_ratio)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$convex_hull_area_ratio)],
                    "subtrial" = data$Subtrial[complete.cases(data$convex_hull_area_ratio)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$convex_hull_area_ratio)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$convex_hull_area_ratio)], format = "%H:%M")),
                    "t" = (as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$convex_hull_area_ratio)], format = "%H:%M")) -
                      as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$convex_hull_area_ratio)], format = "%H:%M"))),
                    "hull" = data$convex_hull_area_ratio[complete.cases(data$convex_hull_area_ratio)]
)


hist(my_data$hull,20)

model_lm <- lm(hull~ gs + temp + loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))

model_lm <- lm(log(hull) ~ gs + temp + loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm <- lm(log(hull) ~ gs + temp + loom + temp*gs, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
#rsq = 0.014 #interaction not significant

model_lm <- lm(log(hull) ~ gs + temp + loom + temp*loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
#rsq = 0.012 #interaction not significant # residuals bettter

model_lm <- lm(log(hull) ~ gs + temp + loom + temp*loom + date, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
#rsq = 0.04 #interaction not significant # residuals bettter

model_lm <- lm(log(hull) ~ gs + temp + loom + temp*loom + date + t, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
#rsq = 0.039 #interaction not significant # residuals better

model_lm <- lm(log(hull) ~ gs + temp + loom + temp*loom + date + t1, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
#rsq = 0.039 #interaction not significant # residuals better


######### trying the hull ratio with loom ###########

data <- read.csv("../../data/temp_collective/roi/convex_hull_ratio_600_650w_loom.csv")

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$convex_hull_area_ratio_loom)],
                    "gs" = data$Groupsize[complete.cases(data$convex_hull_area_ratio_loom)],
                    "loom" = data$Loom[complete.cases(data$convex_hull_area_ratio_loom)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$convex_hull_area_ratio_loom)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$convex_hull_area_ratio_loom)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$convex_hull_area_ratio_loom)],
                    "subtrial" = data$Subtrial[complete.cases(data$convex_hull_area_ratio_loom)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$convex_hull_area_ratio_loom)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$convex_hull_area_ratio_loom)], format = "%H:%M")),
                    "t" = (as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$convex_hull_area_ratio_loom)], format = "%H:%M")) -
                             as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$convex_hull_area_ratio_loom)], format = "%H:%M"))),
                    "hull" = data$convex_hull_area_ratio_loom[complete.cases(data$convex_hull_area_ratio_loom)]
)


hist(my_data$hull,20)
hist(log(my_data$hull),20)

model_lm <- lm(hull~ gs + temp + loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.03


model_lm <- lm(log(hull)~ gs + temp + loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.01

model_lm <- lm(log(hull)~ gs + temp + loom + date, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.044
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm <- lm(log(hull)~ gs + temp + loom + date + t, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.044
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm <- lm(log(hull)~ gs + temp + loom + date + t1, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.043
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))


model_lm <- lm(log(hull)~ gs + temp + loom + date + gs*temp, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.043 # interaction not significant
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))


model_lm <- lm(log(hull)~ gs + temp + loom + date + gs*temp + temp*loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.046 # interaction not significant
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm <- lm(log(hull)~ gs + temp + I(temp^2) + loom + date + gs*temp + temp*loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.045 # interaction not significant
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm <- lm(log(hull)~ gs + temp + I(temp^2) + loom + date, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.043
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm <- lm(log(hull)~ gs + temp + I(temp^2) + loom + date + I(temp^2)*gs, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.043 # interaction not significant
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm <- lm(log(hull)~ log(gs,2) + temp + I(temp^2) + loom + date + I(temp^2)*log(gs,2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.043 # interaction not significant
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm <- lm(log(hull)~ log(gs,2) + I(temp^2) + loom + date + I(temp^2)*log(gs,2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.04471 # interaction not significant
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm <- lm(log(hull)~ log(gs,2) + I(temp^2) + loom + date + I(temp^2)*loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.045 # interaction not significant #temp and gs significant
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm <- lm(log(hull)~ log(gs,2) + I(temp^2) + loom + date, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.045 #temp and gs significant
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

#predictions
model_lm <- lm(log(hull)~ log(gs,2) + I(temp^2) + loom + date, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.045 #temp and gs significant

newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), 
                                   gs = c(4,8,16),
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
newData1$hull <- results[,1]
newData1$hull025 <- results[,2]
newData1$hull975 <- results[,3]

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/hull_ratio_600_650_predictions2.csv")

################################################

############ using third csv file with one frame 

################################################

data <- read.csv("../../data/temp_collective/roi/convex_hull_ratio_649_650_w_loom2.csv")

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$convex_hull_area_ratio)],
                    "gs" = data$Groupsize[complete.cases(data$convex_hull_area_ratio)],
                    "loom" = data$Loom[complete.cases(data$convex_hull_area_ratio)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$convex_hull_area_ratio)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$convex_hull_area_ratio)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$convex_hull_area_ratio)],
                    "subtrial" = data$Subtrial[complete.cases(data$convex_hull_area_ratio)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$convex_hull_area_ratio)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$convex_hull_area_ratio)], format = "%H:%M")),
                    "t" = (as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$convex_hull_area_ratio)], format = "%H:%M")) -
                             as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$convex_hull_area_ratio)], format = "%H:%M"))),
                    "hull" = data$convex_hull_area_ratio[complete.cases(data$convex_hull_area_ratio)]
)


hist(log(my_data$hull + 1),20)

model_lm <- lm(log(hull)~ log(gs,2) + I(temp^2) + loom + date, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.03 #temp and gs significant

######### trying the hull ratio with loom ###########



my_data<-data.frame("temp" = data$Temperature[complete.cases(data$convex_hull_area_ratio_loom)],
                    "gs" = data$Groupsize[complete.cases(data$convex_hull_area_ratio_loom)],
                    "loom" = data$Loom[complete.cases(data$convex_hull_area_ratio_loom)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$convex_hull_area_ratio_loom)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$convex_hull_area_ratio_loom)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$convex_hull_area_ratio_loom)],
                    "subtrial" = data$Subtrial[complete.cases(data$convex_hull_area_ratio_loom)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$convex_hull_area_ratio_loom)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$convex_hull_area_ratio_loom)], format = "%H:%M")),
                    "t" = (as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$convex_hull_area_ratio_loom)], format = "%H:%M")) -
                             as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$convex_hull_area_ratio_loom)], format = "%H:%M"))),
                    "hull" = data$convex_hull_area_ratio_loom[complete.cases(data$convex_hull_area_ratio_loom)]
)

model_lm <- lm(log(hull) ~ temp +  log(gs,2) + I(temp^2) + loom + date, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.038 #temp and gs significant


###### convex hull only during loom ########3

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

model_lm <- lm(log(hull) ~ gs + temp*loom + I(temp^2) + date + trial + t1, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))

model_glm <- glm(hull ~ gs + temp*loom + I(temp^2) + date, my_data, family = "Gamma")
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))

model_lm <- lm(log(hull) ~ gs + temp, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))

model_lm <- lm(hull ~ gs + temp, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))

model_lm <- lm(hull ~ gs + temp + loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
extractAIC(model_lm)
#aic = 2373, r sq =  0.4258

model_lm <- lm(hull ~ gs, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
extractAIC(model_lm)
#aic = 2381, r sq =  0.4177

model_lm <- lm(hull ~ log(gs,2) + temp, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
extractAIC(model_lm)
#aic = 2372, r sq =  0.4261

model_lm <- lm(hull ~ log(gs,2) + temp + I(log(gs,2)^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
extractAIC(model_lm)
#aic = 2369, r sq =  0.4292
#temp is negative

data <- read.csv(here("Documents","data","temp_collective","roi","convex_hull_during_loom.csv"),header=TRUE,na.strings=c("[nan]"))

# make new dataframe

# max convex hull
my_data<-data.frame("temp" = data$Temperature[complete.cases(data$max_convex_hull)],
                    "gs" = data$Groupsize[complete.cases(data$max_convex_hull)],
                    "loom" = data$Loom[complete.cases(data$max_convex_hull)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$max_convex_hull)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$max_convex_hull)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$max_convex_hull)],
                    "subtrial" = data$Subtrial[complete.cases(data$max_convex_hull)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$max_convex_hull)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$max_convex_hull)], format = "%H:%M")),
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$max_convex_hull)], format = "%H:%M"))
                    - as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$max_convex_hull)], format = "%H:%M")),
                    "hull" = data$max_convex_hull[complete.cases(data$max_convex_hull)]
)

model_lm <- lm(hull ~ log(gs,2) + temp + I(log(gs,2)^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
extractAIC(model_lm)
#aic = 2428, r sq =  0.4068
#temp is negative




#####################################

#####################################

#####################################

### looking at both convex hull during loom and after loom 

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

model_lm_trans <- lm(log(hull) ~ loom*temp + I(temp^2) + gs + I(gs^2), my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
# r sq 0.4285 #bad residuals
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))
extractAIC(model_lm_trans) #-1358

model_lm_trans <- lm(hull ~ loom*temp + I(temp^2) + gs + I(gs^2), my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
# r sq 0.3903 #bad residuals
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))
extractAIC(model_lm_trans) #2415



model_lm_trans <- lm(log(hull) ~ loom*temp + log(gs,2) + I(log(gs,2)^2) + temp + loom + t + temp*log(gs,2), my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
# r sq 0.4313
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))

## split residuals (ee) into 2 groups
ee <- residuals(model_lm_trans)
g1 <- ee[ee < median(ee)]
g2 <- ee[ee > median(ee)]
## Levene's Test
var.test(g1, g2)
#p very low


model_lm_trans <- lm((hull)^0.5 ~ loom*temp + log(gs,2) + I(log(gs,2)^2) + temp + loom + t + temp*log(gs,2), my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
# r sq 0.418
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))
## split residuals (ee) into 2 groups
ee <- residuals(model_lm_trans)
g1 <- ee[ee < median(ee)]
g2 <- ee[ee > median(ee)]
## Levene's Test
var.test(g1, g2) #p very low

model_glm <- glm(hull~temp + gs, family = inverse.gaussian, my_data)
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))

model_glm <- glm(log(hull)~temp + gs, family = inverse.gaussian, my_data)
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))

model_glm <- glm((hull)^0.5~temp + gs, family = inverse.gaussian, my_data)
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))