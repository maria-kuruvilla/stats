setwd("~/code/stats") #change if running in different computer

data  <- read.csv("../../data/temp_collective/roi/distance_w_loom.csv",
                  header=TRUE,na.strings=c("[nan]"))

# make new dataframe

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$Distance)],
                    "gs" = data$Groupsize[complete.cases(data$Distance)],
                    "loom" = data$Loom[complete.cases(data$Distance)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$Distance)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$Distance)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$Distance)],
                    "subtrial" = data$Subtrial[complete.cases(data$Distance)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$Distance)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$Distance)], format = "%H:%M")),
                    "d" = data$Distance[complete.cases(data$Distance)])

# basic model with temp and gs
model_lm <- lm(d ~ temp + gs + loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))

qqnorm(residuals(model_lm))
qqline(residuals(model_lm))


#adding more variables
model_lm <- lm(d ~ temp + gs + loom + kt + date + trial+ subtrial +t1 + t2, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))

qqnorm(residuals(model_lm))
qqline(residuals(model_lm))

#quadratic temperature term?

model_lm <- lm(d ~ temp + I(temp^2) + gs + loom + kt + date + trial+ subtrial +t1 + t2, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))

qqnorm(residuals(model_lm))
qqline(residuals(model_lm))


#transforming the variable?
model_lm_trans<- lm(log(d) ~ temp + gs + loom, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))

qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))


#transforming variable with all terms
model_lm_trans <- lm(log(d) ~ temp + I(temp^2) + gs + loom + kt + date + trial+ subtrial +t1 + t2, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))

qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))

#boxcox?
library(MASS)
boxcox(model_lm,plotit=TRUE) #0.5

model_lm_trans <- lm(d^0.5 ~ temp + I(temp^2) + gs + loom + kt + date + trial+ subtrial +t1 + t2, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))

qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))
# good residuals but only 0.067 R sq


#interactions
model_lm_trans <- lm(d^0.5 ~ temp*gs + I(temp^2)  + loom + kt + date + trial+ subtrial +t1 + t2, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))

qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))
# interaction not significant


model_lm_trans <- lm(d^0.5 ~ temp + I(temp^2)*gs  + loom + kt + date + trial+ subtrial +t1 + t2, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#interaction not significant
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))

model_lm_trans <- lm(d^0.5 ~ temp + I(temp^2) + loom*gs + kt + date + trial+ subtrial +t1 + t2, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#interaction not significant
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))


model_lm_trans <- lm(d^0.5 ~ temp*loom + I(temp^2)  + gs + kt + date + trial+ subtrial +t1 + t2, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#interaction not significant
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))

model_lm_trans <- lm(d^0.5 ~ temp + I(temp^2)*loom  + gs + kt + date + trial+ subtrial +t1 + t2, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#interaction not significant
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))

model_lm_trans <- lm(d^0.5 ~ temp*gs*loom  + I(temp^2)+ kt + date + trial+ subtrial +t1 + t2, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#interaction between temp and loom is slightly significant
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))

model_lm_trans <- lm(d^0.5 ~ temp  + I(temp^2)*gs*loom+ kt + date + trial+ subtrial +t1 + t2, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#interaction between temp^2 and loom is significant
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))

model_lm_trans <- lm(d^0.5 ~ temp  + I(temp^2)+ kt*gs*loom + date + trial+ subtrial +t1 + t2, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#interaction between kt and loom is close to being significant
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))

model_lm_trans <- lm(d^0.5 ~ temp  + I(temp^2)*gs*loom + date + trial+ subtrial +t1 + t2, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#interaction between temp^2 and loom is significant but R sq is lesser
#than when kt was there
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))

#gs squared
model_lm_trans <- lm(d^0.5 ~ temp  + I(temp^2)*gs*loom+ I(gs^2)+kt + date + trial+ subtrial +t1 + t2, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#interaction between temp^2 and loom is significant
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))

#taking subtrial out
model_lm_trans <- lm(d^0.5 ~ temp  + I(temp^2)*gs*loom+ I(gs^2)+kt + date + trial +t1 + t2, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#interaction between temp^2 and loom is significant
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))

#interaction with gs^2
model_lm_trans <- lm(d^0.5 ~ temp  + gs + I(temp^2)*loom*I(gs^2)+kt + date + trial +t1 + t2, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#interaction between temp^2 and loom is not as significant any more
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))



#best model
#taking subtrial and date and t2 out #taking everything out except temp,gs,loom,kt

model_lm_trans <- lm(d^0.5 ~ temp  + I(temp^2)*gs*loom+ I(gs^2)+kt, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#interaction between temp^2 and loom is significant
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))

##################################################

#predictions
newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), 
                                   gs = c(1,2,4,8,16),loom = c(1,2,3,4,5),
                                   kt = unique(my_data$kt)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(model_lm_trans))
  ymod <- update(model_lm_trans,ynew ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=3)
results[,1] <- predict(model_lm_trans,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest[j,],probs = c(0.025))
  results[j,3] <- quantile(yest[j,],probs = c(0.975))
}  
chknyes <- intersect(which(newData1$gs == 16),which(newData1$loom == 1))  
chknno <- which(newData1$Groupsize == 1)
temp <- newData1$temp[chknyes]
plot(my_data$temp, my_data$d, xlab = "Temperature",ylab = "Distance travelled")
lines(temp,results[chknyes,1], lty = "solid", col = "green")
#plot(temp,results[chknyes,1], type = 'l', col = "green", ylim = c(0,40), xlab = "Temperature",ylab = "Startles")
lines(temp,results[chknyes,2], lty = "dashed", col = "green")
lines(temp,results[chknyes,3], lty = "dashed", col = "green")

#####################################

model_lm_trans <- lm(d^0.5 ~ temp  + I(temp^2)*gs*loom+ I(gs^2), my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))

#predictions
newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), 
                                   gs = c(1,2,4,8,16),loom = c(1,2,3,4,5)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(model_lm_trans))
  ymod <- update(model_lm_trans,ynew ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=3)
results[,1] <- predict(model_lm_trans,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest[j,],probs = c(0.025))
  results[j,3] <- quantile(yest[j,],probs = c(0.975))
}  
chknyes <- intersect(which(newData1$gs == 16),which(newData1$loom == 1))  
chknno <- which(newData1$Groupsize == 1)
temp <- newData1$temp[chknyes]
plot(my_data$temp, (my_data$d), xlab = "Temperature",ylab = "Distance travelled")
lines(temp,(results[chknyes,1])^2, lty = "solid", col = "green")
#plot(temp,results[chknyes,1], type = 'l', col = "green", ylim = c(0,40), xlab = "Temperature",ylab = "Startles")
lines(temp,(results[chknyes,2])^2, lty = "dashed", col = "green")
lines(temp,(results[chknyes,3])^2, lty = "dashed", col = "green")