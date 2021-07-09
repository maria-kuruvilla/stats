################################

########## LATENCY #############

################################

data <- read.csv("../../data/temp_collective/roi/all_params_w_loom.csv",header=TRUE,na.strings=c("[nan]"))

# make new dataframe

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

model_pois6 <- glm(latency ~ temp + gs*loom + I(temp^2), family = quasipoisson, my_data)
summary(model_pois6)
plot(fitted(model_pois6),residuals(model_pois6))

model_pois6 <- glm(latency ~ temp*gs + loom + I(temp^2), family = quasipoisson, my_data)
summary(model_pois6)
plot(fitted(model_pois6),residuals(model_pois6))

model_pois6 <- glm(latency ~ temp*gs + loom + I(temp^2) + date, family = quasipoisson, my_data)
summary(model_pois6)
plot(fitted(model_pois6),residuals(model_pois6))
#residual dev = 240

model_pois6 <- glm(latency ~ temp*gs + loom + I(temp^2) + date + trial, family = quasipoisson, my_data)
summary(model_pois6)
plot(fitted(model_pois6),residuals(model_pois6))
#residual dev = 240

model_pois6 <- glm(latency ~ temp*gs + loom + I(temp^2)+ t1, family = quasipoisson, my_data)
summary(model_pois6)
plot(fitted(model_pois6),residuals(model_pois6))
#residual dev = 240

model_pois6 <- glm(latency ~ temp*gs*loom + I(temp^2), family = quasipoisson, my_data)
summary(model_pois6)
plot(fitted(model_pois6),residuals(model_pois6))
#residual dev = 239

model_pois6 <- glm(latency ~ temp*gs + gs*loom + I(temp^2), family = quasipoisson, my_data)
summary(model_pois6)
plot(fitted(model_pois6),residuals(model_pois6))
#residual dev = 240

model_pois6 <- glm(latency ~ temp*gs + gs*loom + I(temp^2)*gs, family = quasipoisson, my_data)
summary(model_pois6)
plot(fitted(model_pois6),residuals(model_pois6))
#residual dev = 239

model_pois6 <- glm(latency ~ temp*gs + loom + I(temp^2)*loom, family = quasipoisson, my_data)
summary(model_pois6)
plot(fitted(model_pois6),residuals(model_pois6))
#residual dev = 240


model_pois6 <- glm(latency ~ temp*gs + I(temp^2) + loom, family = quasipoisson, my_data)
summary(model_pois6)
plot(fitted(model_pois6),residuals(model_pois6))
#residual dev = 240

X2 <- sum((my_data$latency - fitted(model_pois6))^2/fitted(model_pois6))
nn <- length(my_data$latency)
pchisq(X2, df = nn-length(coef(model_pois6)), lower.tail = FALSE)

#diagnostics


ee <- residuals(model_pois6)
g1 <- ee[ee>median(ee)]
g2 <- ee[ee<median(ee)]
var.test(g1,g2) #0.51

library(faraway)

h <- hatvalues(model_pois6)

halfnorm(h)

two.p.n <- 2*sum(h)/length(h)

abline(a=two.p.n,b=0)

studentized <- rstudent(model_pois6)
halfnorm(studentized)


halfnorm(cooks <- cooks.distance(model_pois6),5)
my_data[which(abs(studentized)>t_crit),]
t_crit <- qt(0.025/nn,nn-5-1,lower.tail = FALSE) 

my_new_data <- my_data[-which(abs(studentized)>t_crit),]

model_pois6 <- glm(latency ~ temp*gs + I(temp^2) + loom, family = quasipoisson, my_new_data)
summary(model_pois6)
plot(fitted(model_pois6),residuals(model_pois6))
#residual dev = 240

my_new_data2 <- my_data[-c(750,752,327,695),]

model_pois6 <- glm(latency ~ temp*gs + I(temp^2) + loom, family = quasipoisson, my_new_data2)
summary(model_pois6)
plot(fitted(model_pois6),residuals(model_pois6))
#residual dev = 199 out of 215

# goodness of fit?

X2 <- sum((my_new_data2$latency - fitted(model_pois6))^2 / fitted(model_pois6))
## likelihood ratio test
pchisq(X2, df = nn - length(coef(model_pois6)),
       lower.tail = FALSE)

# p value

## deviance of full model
D_full <- summary(model_pois6)$deviance
## deviance of null model
D_null <- summary(model_pois6)$null.deviance
## test statistic
lambda <- D_null - D_full
## LRT with df = 2
(p_value <- pchisq(lambda, 3, lower.tail = FALSE))

#predictions

require(ciTools)
df_ints <- add_ci(my_new_data2, model_pois6, names = c("lcb", "ucb"), alpha = 0.05)


#intersection
intersection <- intersect(which(df_ints$gs==1),which(df_ints$loom==1))
plot(my_new_data2$temp,my_new_data2$latency)
lines(df_ints$temp[intersection],df_ints$lcb[intersection])
lines(df_ints$temp[intersection],df_ints$ucb[intersection])

intersection <- intersect(which(df_ints$gs==16),which(df_ints$loom==1))
plot(my_new_data2$temp,my_new_data2$latency)
lines(df_ints$temp[intersection],df_ints$lcb[intersection])
lines(df_ints$temp[intersection],df_ints$ucb[intersection])

intersection <- intersect(which(df_ints$gs==16),which(df_ints$loom==5))
plot(my_new_data2$temp,my_new_data2$latency)
lines(df_ints$temp[intersection],df_ints$lcb[intersection])
lines(df_ints$temp[intersection],df_ints$ucb[intersection])

#write scv
write.csv(df_ints,"/home/maria/Documents/data/temp_collective/roi/latency_predictions.csv")


#predictions
newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), 
                                   gs = c(1,2,4,8,16),
                                   loom = c(1,2,3,4,5)))

require(ciTools)
df_ints <- add_ci(newData1, model_pois6, names = c("lcb", "ucb"), alpha = 0.05)

write.csv(df_ints,"/home/maria/Documents/data/temp_collective/roi/latency_predictions.csv")

################################

########## Prop ind startling #############

################################

data <- read.csv("../../data/temp_collective/roi/all_params_w_loom.csv",header=TRUE,na.strings=c("[nan]"))

# make new dataframe

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

model_glm6 <-  glm(prop_startles ~ temp + I(temp^2) + loom + date + t, family = binomial,my_data)
summary(model_glm6)
plot(fitted(model_glm6),residuals(model_glm6))
#resid = 693/739, residual plot bad

model_glm <-  glm(prop_startles ~ temp + I(temp^2) + loom + t+date, family = binomial,my_data)
summary(model_glm)
#plot(fitted(model_glm),residuals(model_glm))
#resid = 689/739, residual plot bad. binned residual plot?

#goodness of fit
X2 <- sum((my_data$prop_startles - fitted(model_glm))^2/fitted(model_glm))
nn <- length(my_data$prop_startles)
pchisq(X2, df = nn-length(coef(model_glm)), lower.tail = FALSE)
#p  = 1

library(arm)
binnedplot(fitted(model_glm),residuals(model_glm)) 


#halfnorm(cooks <- cooks.distance(model_glm),5)
#my_data[which(abs(studentized)>t_crit),]
#t_crit <- qt(0.025/nn,nn-5-1,lower.tail = FALSE) 

model_glm <-  glm(prop_startles ~ temp + I(temp^2) + loom + t+date, family = binomial,my_data)
summary(model_glm)
newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1),
                                   loom = c(1,2,3,4,5), date = unique(my_data$date), t = c(600,1200,1800)))
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

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/prop_startles_predictions.csv")

#p value

## deviance of full model
D_full <- summary(model_glm)$deviance
## deviance of null model
D_null <- summary(model_glm)$null.deviance
## test statistic
lambda <- D_null - D_full
## LRT with df = 1
(p_value <- pchisq(lambda, 5, lower.tail = FALSE))


#########################################

###### acceleration during loom #########

#########################################

data <- read.csv("../../data/temp_collective/roi/all_params_w_loom.csv",header=TRUE,na.strings=c("[nan]"))

# make new dataframe

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

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2)*loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.17

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2)*loom + date,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.18

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2)*loom + date + t,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.18

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2)*loom + date + t + I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.18

model_lm <- lm(log(acc+1) ~ temp + I(temp^2)*log(gs,2)*loom + date + t,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1871

model_lm <- lm(log(acc+1) ~ temp + I(temp^2)*loom + log(gs,2)+ date + t,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.18

ee <- residuals(model_lm)
g1 <- ee[ee>median(ee)]
g2 <- ee[ee<median(ee)]
var.test(g1,g2) #0.98

# predictions
model_lm <- lm(log(acc+1) ~ temp + I(temp^2)*log(gs,2)*loom + date + t,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))

newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(1,2,4,8,16),
                                   loom = c(1,2,3,4,5), date = c(18101,18106,18113), t = c(600,1200,1800)))
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

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/loom_acc_99_predictions.csv")

#########################################

###### acceleration before loom #########

#########################################

data <- read.csv("../../data/temp_collective/roi/all_params_wo_loom.csv",header=TRUE,na.strings=c("[nan]"))

# make new dataframe

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
#r sq = 0.099

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.19

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2) + date,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.19 #date is not significant

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2) + t,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.19 #t is not significant

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2) + t1,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1934 #t1 is not significant

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2) + trial,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.19 #trial is not significant

model_lm <- lm(log(acc+1) ~ log(gs,2) + I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.16

#interaction
model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2) + temp*gs,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.19 #interaction is not significant

#interaction
model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2) + temp*I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.19 #interaction is not significant

################
model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.19

#diagnostics


ee <- residuals(model_lm)
g1 <- ee[ee>median(ee)]
g2 <- ee[ee<median(ee)]
var.test(g1,g2) #0.51

library(faraway)

h <- hatvalues(model_lm)

halfnorm(h)

two.p.n <- 2*sum(h)/length(h)

abline(a=two.p.n,b=0)

studentized <- rstudent(model_lm)
halfnorm(studentized)
#128

halfnorm(cooks <- cooks.distance(model_lm),5)
nn <- length(my_data$acc)
t_crit <- qt(0.025/nn,nn-4-1,lower.tail = FALSE) 
my_data[which(abs(studentized)>t_crit),]
#128

my_new_data <- my_data[-which(abs(studentized)>t_crit),]
plot(my_data$temp, my_data$acc)
plot(my_new_data$temp, my_new_data$acc)

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2),my_new_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1962 #neither temp terms significant

model_lm <- lm(log(acc+1) ~ temp + log(gs,2) + I(log(gs,2)^2),my_new_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1936 # temp is significant


model_lm <- lm(log(acc+1) ~ temp + log(gs,2) + I(log(gs,2)^2) + date,my_new_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1936 # date is not significant

model_lm <- lm(log(acc+1) ~ temp + log(gs,2) + I(log(gs,2)^2) + t,my_new_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1936 # t is not significant

model_lm <- lm(log(acc+1) ~ temp + log(gs,2) + I(log(gs,2)^2) + t1,my_new_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1936 # t1 is not significant

model_lm <- lm(log(acc+1) ~ temp + log(gs,2) + I(log(gs,2)^2) + temp*gs,my_new_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1936 # interaction is not significant

model_lm <- lm(log(acc+1) ~ temp + log(gs,2) + I(log(gs,2)^2) + temp*I(log(gs,2)^2),my_new_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1936 # interaction is not significant


##predictions


model_lm <- lm(log(acc+1) ~ temp + log(gs,2) + I(log(gs,2)^2),my_new_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1936 # temp is significant

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

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/acc_99_predictions.csv")




## new predictions


model_lm <- lm(log(acc+1) ~ temp + log(gs,2) + I(log(gs,2)^2) + t1 + date,my_new_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.187 # date is not significant
extractAIC(model_lm) #-654


newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(1,2,4,8,16), t1 = unique(my_new_data$t1), date = unique(my_new_data$date)))
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

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/acc_99_predictions_new.csv")

# new predictions with tempr squared


model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2),my_new_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1962 #neither temp terms significant
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

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/acc_99_predictions_new_squared.csv")



## new predictions for avg acceleration

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

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.2786 # date is not significant
extractAIC(model_lm) #-719

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
newData1$acc <- results[,1]
newData1$acc_025 <- results[,2]
newData1$acc_975 <- results[,3]

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/acc_avg_predictions_new.csv")




#median

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
#r sq = 0.3091 # date is not significant
extractAIC(model_lm) #-741


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
newData1$acc50 <- results[,1]
newData1$acc50_025 <- results[,2]
newData1$acc50_975 <- results[,3]

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/acc_50_predictions_new.csv")


#########################################

###### annd before loom #################

#########################################


data <- read.csv(here("Documents","data","temp_collective","roi","all_params_wo_loom.csv"),header=TRUE,na.strings=c("[nan]"))

# make new dataframe

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$annd)],
                    "gs" = data$Groupsize[complete.cases(data$annd)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$annd)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$annd)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$annd)],
                    "subtrial" = data$Subtrial[complete.cases(data$annd)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$annd)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$annd)], format = "%H:%M")),
                    "annd" = data$annd[complete.cases(data$annd)],
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$annd)], format = "%H:%M")) - as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$annd)], format = "%H:%M"))
)

hist(my_data$annd,40)
hist(log(my_data$annd),40)

model_lm <- lm(log(annd) ~ temp + log(gs,2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.76

qqnorm(residuals(model_lm))
qqline(residuals(model_lm))

model_lm <- lm(log(annd) ~ log(gs,2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.75

model_lm <- lm(log(annd) ~ log(gs,2) + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.76

model_lm <- lm(log(annd) ~ log(gs,2) + I(temp^2) + temp, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.76


model_lm <- lm(log(annd) ~ log(gs,2)*temp, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.76

model_lm <- lm(log(annd) ~ log(gs,2)*temp + date, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.76

model_lm <- lm(log(annd) ~ log(gs,2) + temp + date, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.76


model_lm <- lm(log(annd) ~ log(gs,2) + temp + t, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.76

model_lm <- lm(log(annd) ~ log(gs,2) + temp + t1, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.76

model_lm <- lm(log(annd) ~ log(gs,2) + temp + I(log(gs,2)^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.76

X2 <- sum((my_data$annd - fitted(model_lm))^2/fitted(model_lm))
nn <- length(my_data$annd)
pchisq(X2, df = nn-length(coef(model_lm)), lower.tail = FALSE)



model_lm <- lm(log(annd) ~ temp, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.001

model_lm <- lm(log(annd) ~ kt + log(gs,2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.76

model_glm <- glm(annd ~ temp + log(gs,2),family = Gamma,my_data)
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))

model_glm <- glm(annd ~ temp + log(gs,2) + I(log(gs,2)^2),family = Gamma,my_data)
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))


model_glm <- glm(annd ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2),family = Gamma,my_data)
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
#aic = -34
X2 <- sum((my_data$annd - fitted(model_glm))^2/fitted(model_glm))
nn <- length(my_data$annd)
pchisq(X2, df = nn-length(coef(model_glm)), lower.tail = FALSE) #1


model_glm <- glm(annd ~ temp + I(temp^2),family = Gamma,my_data)
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
#aic = 246
X2 <- sum((my_data$annd - fitted(model_glm))^2/fitted(model_glm))
nn <- length(my_data$annd)
pchisq(X2, df = nn-length(coef(model_glm)), lower.tail = FALSE) #0.99

model_glm <- glm(annd ~  log(gs,2) + I(log(gs,2)^2),family = Gamma,my_data)
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
#aic = -33
X2 <- sum((my_data$annd - fitted(model_glm))^2/fitted(model_glm))
nn <- length(my_data$annd)
pchisq(X2, df = nn-length(coef(model_glm)), lower.tail = FALSE) #1

# best

model_lm <- lm(log(annd) ~ temp + log(gs,2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.76 #residuals not good


qqnorm(residuals(model_lm))
qqline(residuals(model_lm))


ee <- residuals(model_lm)
g1 <- ee[ee>median(ee)]
g2 <- ee[ee<median(ee)]
var.test(g1,g2) #0.0003


library(faraway)

h <- hatvalues(model_lm)

halfnorm(h)

two.p.n <- 2*sum(h)/length(h)

abline(a=two.p.n,b=0)

studentized <- rstudent(model_lm)
halfnorm(studentized)


halfnorm(cooks <- cooks.distance(model_lm))
nn <- length(my_data$annd)
t_crit <- qt(0.025/nn,nn-length(coef(model_lm)),lower.tail = FALSE) 
which(abs(studentized)>t_crit)

##predictions

model_lm <- lm(log(annd) ~ temp + log(gs,2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.76 #residuals not good

newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(2,4,8,16)))
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
newData1$annd <- results[,1]
newData1$annd025 <- results[,2]
newData1$annd975 <- results[,3]

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/annd_before_loom_predictions.csv")


#####################################

########## NO. STARTLES #############

#####################################

data <- read.csv(here("Documents","data","temp_collective","roi","all_params_w_loom.csv"),header=TRUE,na.strings=c("[nan]"))


# make new dataframe

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$number_startles)],
                    "gs" = data$Groupsize[complete.cases(data$number_startles)],
                    "loom" = data$Loom[complete.cases(data$number_startles)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$number_startles)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$number_startles)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$number_startles)],
                    "subtrial" = data$Subtrial[complete.cases(data$number_startles)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$number_startles)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$number_startles)], format = "%H:%M")),
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$number_startles)], format = "%H:%M")) -
                    as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$number_startles)], format = "%H:%M")),
                    "startles" = data$number_startles[complete.cases(data$number_startles)]
)
hist(my_data$startles,20)
hist(log(my_data$startles+1),20)

model_lm <- lm(log(startles+1) ~ temp + log(gs,2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.76 #residuals not good

model_glm <- glm(startles ~ I(temp^2) + temp + gs + I(gs^2) + loom, family = poisson, my_data)
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
#aic = 5313

model_glm <- glm(startles ~ I(temp^2) + temp + gs + I(gs^2) + loom + date, family = poisson, my_data)
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
#aic = 5286

model_glm <- glm(startles ~ I(temp^2) + temp + gs + I(gs^2) + loom + date + t1, family = poisson, my_data)
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
#aic = 5268

model_glm <- glm(startles ~ I(temp^2) + temp + gs + I(gs^2) + loom + date + t1 + t, family = poisson, my_data)
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
#aic = 5261


model_glm <- glm(startles ~ I(temp^2) + temp + gs + I(gs^2) + loom + date + t1 + t2, family = poisson, my_data)
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
#aic = 5261

model_glm <- glm(startles ~ I(temp^2) + temp + log(gs,2) + I(log(gs,2)^2) + loom + date + t1 + t2, family = poisson, my_data)
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
#aic = 5223

model_glm <- glm(startles ~ I(temp^2) + temp + log(gs,2)  + loom + date + t1 + t2, family = poisson, my_data)
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
#aic = 5222

model_glm <- glm(startles ~ I(temp^2) + temp + log(gs,2)  + loom + date + t1 + t2 + temp*log(gs,2), family = poisson, my_data)
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
#aic = 5224 #interaction not significant

model_glm <- glm(startles ~ I(temp^2) + temp + log(gs,2)  + loom + date + t1 + t2 + temp*loom, family = poisson, my_data)
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
#aic = 5222 #interaction not significant

model_glm <- glm(startles ~ I(temp^2) + temp + log(gs,2)  + loom + date + t1 + t2 + log(gs,2)*loom, family = poisson, my_data)
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
#aic = 5223 #interaction not significant

model_glm <- glm(startles ~ log(gs,2)  + loom + date + t1 + t2, family = poisson, my_data)
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
#aic = 5562

model_glm <- glm(startles ~ I(temp^2) + temp + log(gs,2)  + loom + date + t1 + t2, family = poisson, my_data)
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
#aic = 5222 # best


ee <- residuals(model_glm)
g1 <- ee[ee>median(ee)]
g2 <- ee[ee<median(ee)]
var.test(g1,g2) #0.0000000000003


X2 <- sum((my_data$startles - fitted(model_glm))^2/fitted(model_glm))
nn <- length(my_data$startles)
pchisq(X2, df = nn-length(coef(model_glm)), lower.tail = FALSE) #very low


#try quasi poisson?
(c_hat <- X2 / (nn - length(coef(model_glm))))

model_glm <- glm(startles ~ I(temp^2) + temp + log(gs,2)  + loom + date + t1 + t2, family = quasipoisson(link = "log"), my_data)
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
#aic = NA

#negative binomial

library(MASS)

model_glm <- glm.nb(startles ~ I(temp^2) + temp + log(gs,2)  + loom + date + t1 + t2, data = my_data,
                     link = "log")
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
# residuals and gof not good


# make new dataframe without zeros

my_data<-data.frame("temp" = data$Temperature[which(data$number_startles>0)],
                    "gs" = data$Groupsize[which(data$number_startles>0)],
                    "loom" = data$Loom[which(data$number_startles>0)],
                    "kt"=1/(0.00008617*(data$Temperature[which(data$number_startles>0)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[which(data$number_startles>0)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[which(data$number_startles>0)],
                    "subtrial" = data$Subtrial[which(data$number_startles>0)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[which(data$number_startles>0)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[which(data$number_startles>0)], format = "%H:%M")),
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[which(data$number_startles>0)], format = "%H:%M")) -
                      as.numeric(as.POSIXct(data$Time_fish_in[which(data$number_startles>0)], format = "%H:%M")),
                    "startles" = data$number_startles[which(data$number_startles>0)]
)


library(MASS)

model_glm <- glm.nb(startles ~ I(temp^2) + temp + log(gs,2)  + loom + date + t1 + t2, data = my_data,
                    link = "log")
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
# residuals and gof not good

X2 <- sum((my_data$startles - fitted(model_glm))^2/fitted(model_glm))
nn <- length(my_data$startles)
pchisq(X2, df = nn-length(coef(model_glm)), lower.tail = FALSE) #very low

model_glm <- glm.nb(startles/gs ~ I(temp^2) + temp + log(gs,2)  + loom + date + t1 + t2, data = my_data,
                    link = "log")
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
# residuals and gof not good





#####################################

########## Polarization #############

#####################################

data <- read.csv(here("Documents","data","temp_collective","roi","all_params_w_loom_pol.csv"),header=TRUE,na.strings=c("[nan]"))


# make new dataframe

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$polarization_1)],
                    "gs" = data$Groupsize[complete.cases(data$polarization_1)],
                    "loom" = data$Loom[complete.cases(data$polarization_1)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$polarization_1)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$polarization_1)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$polarization_1)],
                    "subtrial" = data$Subtrial[complete.cases(data$polarization_1)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$polarization_1)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$polarization_1)], format = "%H:%M")),
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$polarization_1)], format = "%H:%M")) -
                      as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$polarization_1)], format = "%H:%M")),
                    "pol" = data$polarization_1[complete.cases(data$polarization_1)]
)
hist(my_data$pol,20)

hist(log(abs(my_data$pol)+1),40)

model_lm <- lm(pol ~ temp + log(gs,2)  + loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.023 #residuals are okay

model_lm <- lm(pol ~ temp + log(gs,2)  + loom + I(temp^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.022 #neither temp term is significant

model_lm <- lm(pol ~ temp + log(gs,2)  + loom + temp*log(gs,2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.022 #residuals are okay #interaction and temp not sugnificant

model_lm <- lm(pol ~ temp + log(gs,2)  + loom + I(log(gs,2)^2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.023 #residuals are okay #neither gs term is sugnificant

model_lm <- lm(pol ~ temp + log(gs,2)  + loom + loom*temp, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.022  #residuals are okay. interaction not sugnificant

model_lm <- lm(pol ~ temp + log(gs,2)  + loom + date, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.02368  #residuals are okay. interaction not sugnificant

model_lm <- lm(pol ~ temp + log(gs,2)  + loom + date + t, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.02357  #residuals are okay. interaction not sugnificant


model_lm <- lm(pol ~ temp + log(gs,2)  + loom + date + t1, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.02684  #residuals are okay. interaction not sugnificant

model_lm <- lm(pol ~ temp  + loom + t1, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.028  #residuals are okay. 

##predictions

model_lm <- lm(pol ~ temp  + loom + t1, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.028  #residuals are okay. 

newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), loom = c(1,2,3,4,5), t1 = unique(my_data$t1)))
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
newData1$pol_1 <- results[,1]
newData1$pol1_025 <- results[,2]
newData1$pol1_975 <- results[,3]

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/pol1_during_loom_predictions.csv")


### absolute values for pol

model_lm <- lm(abs(pol) ~ temp  + loom + t1, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.028  #residuals not great


#pol  2


data <- read.csv(here("Documents","data","temp_collective","roi","all_params_w_loom_pol.csv"),header=TRUE,na.strings=c("[nan]"))


# make new dataframe

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$polarization_2)],
                    "gs" = data$Groupsize[complete.cases(data$polarization_2)],
                    "loom" = data$Loom[complete.cases(data$polarization_2)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$polarization_2)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$polarization_2)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$polarization_2)],
                    "subtrial" = data$Subtrial[complete.cases(data$polarization_2)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$polarization_2)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$polarization_2)], format = "%H:%M")),
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$polarization_2)], format = "%H:%M")) -
                      as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$polarization_2)], format = "%H:%M")),
                    "pol" = data$polarization_2[complete.cases(data$polarization_2)]
)


model_lm <- lm(pol ~ temp  + loom + t1, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.03  #residuals good




#####################################

####### Polarization wo loom ########

#####################################

data <- read.csv(here("Documents","data","temp_collective","roi","all_params_wo_loom_pol.csv"),header=TRUE,na.strings=c("[nan]"))


# make new dataframe

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$polarization_1)],
                    "gs" = data$Groupsize[complete.cases(data$polarization_1)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$polarization_1)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$polarization_1)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$polarization_1)],
                    "subtrial" = data$Subtrial[complete.cases(data$polarization_1)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$polarization_1)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$polarization_1)], format = "%H:%M")),
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$polarization_1)], format = "%H:%M")) -
                      as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$polarization_1)], format = "%H:%M")),
                    "pol" = data$polarization_1[complete.cases(data$polarization_1)]
)
hist(my_data$pol,20)

hist(log(abs(my_data$pol)+1),40)


model_lm <- lm(pol ~ temp  + log(gs,2), my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = -0.001  #residuals are okay. 

model_lm <- lm(pol ~ temp  + log(gs,2) + t1, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = -0.006  #residuals are okay. 

model_lm <- lm(pol ~ temp  + log(gs,2) + t, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.006  #residuals are okay. 

model_lm <- lm(pol ~ temp  + log(gs,2) + t + date, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.002  #residuals are okay. 

# local polariozation 2nd nearest neighbor
# make new dataframe

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$polarization_2)],
                    "gs" = data$Groupsize[complete.cases(data$polarization_2)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$polarization_2)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$polarization_2)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$polarization_2)],
                    "subtrial" = data$Subtrial[complete.cases(data$polarization_2)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$polarization_2)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$polarization_2)], format = "%H:%M")),
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$polarization_2)], format = "%H:%M")) -
                      as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$polarization_2)], format = "%H:%M")),
                    "pol" = data$polarization_2[complete.cases(data$polarization_2)]
)

model_lm <- lm(pol ~ temp  + log(gs,2) + t + date, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq = 0.02  #residuals are okay. 

## acc just before each loom



data <- read.csv(here("Documents","data","temp_collective","roi","speed_acc_before_loom_params_w_loom.csv"),header=TRUE,na.strings=c("[nan]"))

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$acc_percentile99)],
                    "gs" = data$Groupsize[complete.cases(data$acc_percentile99)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$acc_percentile99)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$acc_percentile99)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$acc_percentile99)],
                    "subtrial" = data$Subtrial[complete.cases(data$acc_percentile99)],
                    "loom" = data$Loom[complete.cases(data$acc_percentile99)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$acc_percentile99)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$acc_percentile99)], format = "%H:%M")),
                    "acc" = data$acc_percentile99[complete.cases(data$acc_percentile99)]
)


model_lm <- lm(log(acc+1) ~ temp + log(gs,2) + I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.078 # temp is significant
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
extractAIC(model_lm) #-2562

model_lm <- lm(log(acc+1) ~ I(temp^2) + temp + log(gs,2) + I(log(gs,2)^2) + loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.088 # temp is significant
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
extractAIC(model_lm) #-2572 #best

model_lm <- lm(log(acc+1) ~ I(temp^2) + temp + log(gs,2) + I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.088 # temp is significant
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
extractAIC(model_lm) #-2573 #best

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

