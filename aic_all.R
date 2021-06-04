###############################################################################

##########################    speed before loom ###############################

###############################################################################


data <- read.csv(here("Documents","data","temp_collective","roi","all_params_wo_loom.csv"),header=TRUE,na.strings=c("[nan]"))

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

model_lm <- lm(log(speed+1) ~ temp,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.1127
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
extractAIC(model_lm) #-683
#this is best

model_lm <- lm(log(speed+1) ~ temp + log(gs,2) ,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
extractAIC(model_lm) #-681

model_lm <- lm(log(speed+1) ~ temp + log(gs,2) + temp*log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
extractAIC(model_lm) #-680

model_lm <- lm(log(speed+1) ~ temp + log(gs,2) + temp*log(gs,2) + date,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
extractAIC(model_lm) #-678

model_lm <- lm(log(speed+1) ~ temp + log(gs,2) + temp*log(gs,2) + t,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
extractAIC(model_lm) #-678


model_lm <- lm(log(speed+1) ~ temp + log(gs,2) + temp*log(gs,2) + t1,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
extractAIC(model_lm) #-678


model_lm <- lm(log(speed+1) ~ I(temp^2) ,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
extractAIC(model_lm) #-680

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$speed_percentile50)],
                    "gs" = data$Groupsize[complete.cases(data$speed_percentile50)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$speed_percentile50)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$speed_percentile50)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$speed_percentile50)],
                    "subtrial" = data$Subtrial[complete.cases(data$speed_percentile50)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$speed_percentile50)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$speed_percentile50)], format = "%H:%M")),
                    "speed" = data$speed_percentile50[complete.cases(data$speed_percentile50)]
)


model_lm <- lm(log(speed+1) ~ temp,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.02
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
extractAIC(model_lm) #-812
#this is best

###############################################################################

#############################   acc before loom ###############################

###############################################################################


data <- read.csv(here("Documents","data","temp_collective","roi","all_params_wo_loom.csv"),header=TRUE,na.strings=c("[nan]"))


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


model_lm <- lm(log(acc+1) ~ temp + log(gs,2) + I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1936 # temp is significant
extractAIC(model_lm) #-579

studentized <- rstudent(model_lm)
halfnorm(studentized)
#128

halfnorm(cooks <- cooks.distance(model_lm),5)
nn <- length(my_data$acc)
t_crit <- qt(0.025/nn,nn-4-1,lower.tail = FALSE) 
my_data[which(abs(studentized)>t_crit),]
#128

my_new_data <- my_data[-which(abs(studentized)>t_crit),]


model_lm <- lm(log(acc+1) ~ temp + log(gs,2) + I(log(gs,2)^2),my_new_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1936 # temp is significant
extractAIC(model_lm) #-657

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2),my_new_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1962 #neither temp terms significant
extractAIC(model_lm) #-657

model_lm <- lm(log(acc+1) ~ I(temp^2) + log(gs,2) + I(log(gs,2)^2),my_new_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1962 #neither temp terms significant
extractAIC(model_lm) #-656

model_lm <- lm(log(acc+1) ~ temp + log(gs,2) + I(log(gs,2)^2) + temp*log(gs,2),my_new_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1936 # interaction is not significant
extractAIC(model_lm) #-657

model_lm <- lm(log(acc+1) ~ temp + log(gs,2) + I(log(gs,2)^2) + date,my_new_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1904 # date is not significant
extractAIC(model_lm) #-655

model_lm <- lm(log(acc+1) ~ temp + log(gs,2) + I(log(gs,2)^2) + t,my_new_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1922 # date is not significant
extractAIC(model_lm) #-656

model_lm <- lm(log(acc+1) ~ temp + log(gs,2) + I(log(gs,2)^2) + t1,my_new_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1901 # date is not significant
extractAIC(model_lm) #-655

model_lm <- lm(log(acc+1) ~ temp + log(gs,2) + I(log(gs,2)^2) + t1 + date,my_new_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.187 # date is not significant
extractAIC(model_lm) #-654

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

model_lm <- lm(log(acc+1) ~ temp + log(gs,2) + I(log(gs,2)^2) + t1 + date,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.2382 # date is not significant
extractAIC(model_lm) #-705

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2) + t1 + date,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.273 # date is not significant
extractAIC(model_lm) #-715

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.2786 # date is not significant
extractAIC(model_lm) #-719

qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

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

model_lm <- lm(log(acc+1) ~ temp + log(gs,2) + I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.2592 # date is not significant
extractAIC(model_lm) #-726

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2) + date + t1,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.3091 # date is not significant
extractAIC(model_lm) #-737


model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.302 # date is not significant
extractAIC(model_lm) #-740


###############################################################################

####################### convex hull before loom ###############################

###############################################################################

data <- read.csv(here("Documents","data","temp_collective","roi","all_params_wo_loom.csv"),header=TRUE,na.strings=c("[nan]"))

# make new dataframe

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$convex_hull_area)],
                    "gs" = data$Groupsize[complete.cases(data$convex_hull_area)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$convex_hull_area)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$convex_hull_area)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$convex_hull_area)],
                    "subtrial" = data$Subtrial[complete.cases(data$convex_hull_area)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$convex_hull_area)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$convex_hull_area)], format = "%H:%M")),
                    "hull" = data$convex_hull_area[complete.cases(data$convex_hull_area)],
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$convex_hull_area)], format = "%H:%M")) - as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$convex_hull_area)], format = "%H:%M"))
)

hist(my_data$hull,40)
hist(log(my_data$hull),40)


model_lm <- lm(hull ~ temp+ log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.55
extractAIC(model_lm) #395

model_lm <- lm(hull ~ temp+ I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.5584
extractAIC(model_lm) #394

model_lm <- lm(hull ~ temp+ I(temp^2) + log(gs,2) + I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.5584
extractAIC(model_lm) #396

#all model residuals have trend

model_lm <- lm(hull ~ temp+ I(temp^2) + log(gs,2) + date,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.5584
extractAIC(model_lm) #396

model_lm <- lm(hull ~ temp+ I(temp^2) + log(gs,2) + date + t,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.5584
extractAIC(model_lm) #398

model_lm <- lm(hull ~ temp+ I(temp^2) + log(gs,2) + date + t1,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.5584
extractAIC(model_lm) #398

model_lm <- lm(hull ~ temp + log(gs,2) + date + t1,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.5584
extractAIC(model_lm) #399

model_lm <- lm(log(hull) ~ temp + log(gs,2) + date + t1,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.5584
extractAIC(model_lm) #-340

model_lm <- lm(log(hull) ~ temp + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.5584
extractAIC(model_lm) #-343
#best?

model_lm <- lm(log(hull) ~ temp + log(gs,2) + temp*log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.56
extractAIC(model_lm) #-341

model_lm <- lm(log(hull) ~ log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.52
extractAIC(model_lm) #-332


###############################################################################

##########################    speed during loom ###############################

###############################################################################


data <- read.csv(here("Documents","data","temp_collective","roi","all_params_w_loom.csv"),header=TRUE,na.strings=c("[nan]"))

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$speed_percentile99)],
                    "gs" = data$Groupsize[complete.cases(data$speed_percentile99)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$speed_percentile99)]+273.1)),
                    "loom" = data$Loom[complete.cases(data$speed_percentile99)],
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$speed_percentile99)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$speed_percentile99)],
                    "subtrial" = data$Subtrial[complete.cases(data$speed_percentile99)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$speed_percentile99)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$speed_percentile99)], format = "%H:%M")),
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$speed_percentile99)], format = "%H:%M")) - 
                      as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$speed_percentile99)], format = "%H:%M")),
                    "speed" = data$speed_percentile99[complete.cases(data$speed_percentile99)]
)

model_lm <- lm((speed)^0.5 ~ temp + I(temp^2) + log(gs,2) + loom + I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1772
extractAIC(model_lm) #-236
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))


#glm?

model_lm <- glm(speed ~ temp + I(temp^2) + log(gs,2) + loom + I(log(gs,2)^2),family = Gamma, my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#residuals are really bad
extractAIC(model_lm) #6796
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))


model_lm <- lm((speed)^0.5 ~ temp + I(temp^2) + log(gs,2) + loom + I(log(gs,2)^2) + trial,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1799
extractAIC(model_lm) #-239

model_lm <- lm((speed)^0.5 ~ temp + I(temp^2) + log(gs,2) + loom + I(log(gs,2)^2) + trial + date,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1823
extractAIC(model_lm) #-241

model_lm <- lm((speed)^0.5 ~ temp + I(temp^2) + log(gs,2) + loom + I(log(gs,2)^2) + t ,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1777
extractAIC(model_lm) #-235

model_lm <- lm((speed)^0.5 ~ temp + I(temp^2) + log(gs,2) + loom + I(log(gs,2)^2) + t1 ,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1829
extractAIC(model_lm) #-243

model_lm <- lm((speed)^0.5 ~ temp + I(temp^2) + log(gs,2) + loom + I(log(gs,2)^2) + t1 + date ,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1844
extractAIC(model_lm) #-244

model_lm <- lm(log(speed) ~ temp + I(temp^2) + log(gs,2) + loom + I(log(gs,2)^2) + t1 + date ,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.2197
extractAIC(model_lm) #-397
#heteroscedatic

qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))


model_lm <- lm((speed)^0.5 ~ temp + I(temp^2) + log(gs,2) + loom + I(log(gs,2)^2) + date,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1852
extractAIC(model_lm) #-240
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))



#mixed model?
require(lme4)
model_lmm <- lmer((speed)^0.5 ~ temp + I(temp^2) + log(gs,2) + loom + I(log(gs,2)^2) + (1|date), my_data)
summary(model_lmm)
plot(fitted(model_lmm), residuals(model_lmm))
extractAIC(model_lmm) #2973





model_lm <- lm((speed)^0.5 ~ temp + log(gs,2) + loom + I(log(gs,2)^2) + t1,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1872
extractAIC(model_lm) #-232
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm <- lm(log(speed) ~ temp + I(temp^2) + log(gs,2) + loom + I(log(gs,2)^2) + t1,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1872
extractAIC(model_lm) #-398 but residuals are terrible
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))


model_lm <- lm((speed)^0.5 ~ temp + I(temp^2) + log(gs,2) + loom + I(log(gs,2)^2) + t1,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1872
extractAIC(model_lm) #-243
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
#This is the best model

#prediction

newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), loom = c(1,2,3,4,5), t1 = unique(my_data$t1),
                                   gs = c(1,2,4,8,16)))
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

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/99th_percentile_speed_during_loom_with_t1_predictions2.csv")

# average speed during loom

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$avg_speed)],
                    "gs" = data$Groupsize[complete.cases(data$avg_speed)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$avg_speed)]+273.1)),
                    "loom" = data$Loom[complete.cases(data$avg_speed)],
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$avg_speed)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$avg_speed)],
                    "subtrial" = data$Subtrial[complete.cases(data$avg_speed)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$avg_speed)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$avg_speed)], format = "%H:%M")),
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$avg_speed)], format = "%H:%M")) - 
                      as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$avg_speed)], format = "%H:%M")),
                    "speed" = data$avg_speed[complete.cases(data$avg_speed)]
)

model_lm <- lm((speed)^0.5 ~ temp + I(temp^2) + log(gs,2) + loom + I(log(gs,2)^2) + t1,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.06
extractAIC(model_lm) #-2609
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm <- lm((speed)^0.5 ~ temp + log(gs,2) + loom + I(log(gs,2)^2) + t1,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.058
extractAIC(model_lm) #-2607
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm <- lm(log(speed) ~ temp + log(gs,2) + loom + I(log(gs,2)^2) + t1,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.08
extractAIC(model_lm) #-859
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm <- lm(log(speed) ~ temp  + I(temp^2) + log(gs,2) + loom + I(log(gs,2)^2) + t1,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.0928 #bad residuals with log transformation
extractAIC(model_lm) #-862
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
# avg speed follows same trend

#median

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$speed_percentile50)],
                    "gs" = data$Groupsize[complete.cases(data$speed_percentile50)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$speed_percentile50)]+273.1)),
                    "loom" = data$Loom[complete.cases(data$speed_percentile50)],
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$speed_percentile50)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$speed_percentile50)],
                    "subtrial" = data$Subtrial[complete.cases(data$speed_percentile50)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$speed_percentile50)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$speed_percentile50)], format = "%H:%M")),
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$speed_percentile50)], format = "%H:%M")) - 
                      as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$speed_percentile50)], format = "%H:%M")),
                    "speed" = data$speed_percentile50[complete.cases(data$speed_percentile50)]
)

model_lm <- lm((speed)^0.5 ~ temp + I(temp^2) + log(gs,2) + loom + I(log(gs,2)^2) + t1,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.0362
extractAIC(model_lm) #-3094
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm <- lm((speed)^0.5 ~ temp + log(gs,2) + loom + I(log(gs,2)^2) + t1,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.03
extractAIC(model_lm) #-3094
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

#########################################

###### acceleration during loom #########

#########################################

data <- read.csv(here("Documents","data","temp_collective","roi","all_params_w_loom.csv"),header=TRUE,na.strings=c("[nan]"))

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

model_lm <- lm(log(acc+1) ~ temp + I(temp^2)*log(gs,2)*loom + date + t,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1871
extractAIC(model_lm) #-941
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
# this is the best model

model_lm <- lm(log(acc+1) ~ temp*log(gs,2)*loom + date + t,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.166
extractAIC(model_lm) #-913
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm <- lm(log(acc+1) ~ temp + I(temp^2)*log(gs,2)*loom + date ,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1852
extractAIC(model_lm) #-939
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm <- lm(log(acc+1) ~ temp + I(temp^2)*log(gs,2)*loom + t,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.182
extractAIC(model_lm) #-935
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + loom + date + t,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1821
extractAIC(model_lm) #-938
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm <- lm(log(acc+1) ~ temp + log(gs,2) + I(temp^2)*loom + date + t,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1815
extractAIC(model_lm) #-936
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

#average acc
my_data<-data.frame("temp" = data$Temperature[complete.cases(data$avg_acc)],
                    "gs" = data$Groupsize[complete.cases(data$avg_acc)],
                    "loom" = data$Loom[complete.cases(data$avg_acc)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$avg_acc)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$avg_acc)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$avg_acc)],
                    "subtrial" = data$Subtrial[complete.cases(data$avg_acc)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$avg_acc)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$avg_acc)], format = "%H:%M")),
                    "acc" = data$avg_acc[complete.cases(data$avg_acc)],
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$avg_acc)], format = "%H:%M")) - as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$avg_acc)], format = "%H:%M"))
)

model_lm <- lm(log(acc+1) ~ temp + I(temp^2)*log(gs,2)*loom + date + t,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.099
extractAIC(model_lm) #-2567
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
# this is the best model

model_lm <- lm(log(acc+1) ~ temp*log(gs,2)*loom + date + t,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.064
extractAIC(model_lm) #-2523
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
# this is the best model

#median acc
my_data<-data.frame("temp" = data$Temperature[complete.cases(data$acc_percentile50)],
                    "gs" = data$Groupsize[complete.cases(data$acc_percentile50)],
                    "loom" = data$Loom[complete.cases(data$acc_percentile50)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$acc_percentile50)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$acc_percentile50)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$acc_percentile50)],
                    "subtrial" = data$Subtrial[complete.cases(data$acc_percentile50)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$acc_percentile50)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$acc_percentile50)], format = "%H:%M")),
                    "acc" = data$acc_percentile50[complete.cases(data$acc_percentile50)],
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$acc_percentile50)], format = "%H:%M")) - as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$acc_percentile50)], format = "%H:%M"))
)

model_lm <- lm(log(acc+1) ~ temp + I(temp^2)*log(gs,2)*loom + date + t,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.091
extractAIC(model_lm) #-2804
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
# this is the best model

model_lm <- lm(log(acc+1) ~ temp*log(gs,2)*loom + date + t,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.061
extractAIC(model_lm) #-2768
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
# this is the best model

################################

########## LATENCY #############

################################


#data <- read.csv(here("Documents","data","temp_collective","roi","all_params_w_loom.csv"),header=TRUE,na.strings=c("[nan]"))


data <- read.csv(here("data","temp_collective","roi","all_params_w_loom.csv"),header=TRUE,na.strings=c("[nan]"))


# make new dataframe

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$latency)],
                    "gs" = data$Groupsize[complete.cases(data$latency)],
                    "loom" = data$Loom[complete.cases(data$latency)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$latency)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$latency)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$latency)],
                    "subtrial" = data$Subtrial[complete.cases(data$latency)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$latency)], format = "%H:%M")),
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$latency)], format = "%H:%M")) -
                      as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$latency)], format = "%H:%M")),
                    "latency" = data$latency[complete.cases(data$latency)]
)

my_new_data2 <- my_data[-c(750,752,327,695),]


model_pois6 <- glm(latency ~ temp*gs + I(temp^2) + loom, family = quasipoisson, my_new_data2)
summary(model_pois6)
plot(fitted(model_pois6),residuals(model_pois6))
#residual dev = 199 out of 215
(chat <- deviance(model_pois6) / df.residual(model_pois6))
require(MuMIn)
QAIC(model_pois6, chat = chat)


##trying again


dfun <- function(object){with(object,sum((weights * residuals^2)[weights > 0])/df.residual)}


model_pois <- glm(latency ~ temp*gs + I(temp^2) + loom, family = poisson, my_new_data2)

model_qpois <- glm(latency ~ temp*gs + I(temp^2) + loom, family = quasipoisson, my_new_data2)


#extract log likelihoods

(sum(dpois(my_new_data2$latency,lambda=exp(predict(model_pois)),log=TRUE)))

(logLik(model_pois))

library(MuMIn); packageVersion("MuMIn")

x.quasipoisson <- function(...){
  res <- quasipoisson(...)
  res$aic <- poisson(...)$aic
  res
}

model_qpois2 <- update(model_pois,family="x.quasipoisson",na.action=na.fail)

(gg <-  dredge(model_qpois2,rank="QAIC", chat=dfun(model_pois)))


###########################################

########## Prop ind startling #############

###########################################

data <- read.csv(here("Documents","data","temp_collective","roi","all_params_w_loom.csv"),header=TRUE,na.strings=c("[nan]"))

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
library(arm)
model_glm <-  glm(prop_startles ~ temp + I(temp^2) + loom + t+date, family = binomial,my_data)
summary(model_glm)
binnedplot(fitted(model_glm),residuals(model_glm)) 
#aic = 1312

model_glm <-  glm(prop_startles ~ temp + I(temp^2) + loom + t, family = binomial,my_data)
summary(model_glm)
binnedplot(fitted(model_glm),residuals(model_glm)) 
#aic = 1317

model_glm <-  glm(prop_startles ~ temp + I(temp^2) + loom + date, family = binomial,my_data)
summary(model_glm)
binnedplot(fitted(model_glm),residuals(model_glm)) 
#aic = 1311 #residuals not bad #best model?

model_glm <-  glm(prop_startles ~ temp + I(temp^2) + loom + date + temp*log(gs,2), family = binomial,my_data)
summary(model_glm)
binnedplot(fitted(model_glm),residuals(model_glm)) 
#aic = 1318

model_glm <-  glm(prop_startles ~ temp + I(temp^2) + loom + date + I(temp^2)*log(gs,2), family = binomial,my_data)
summary(model_glm)
binnedplot(fitted(model_glm),residuals(model_glm)) 
#aic = 1318

model_glm <-  glm(prop_startles ~ temp + I(temp^2) + loom + date + I(temp^2)*loom, family = binomial,my_data)
summary(model_glm)
binnedplot(fitted(model_glm),residuals(model_glm)) 
#aic = 1313

model_glm <-  glm(prop_startles ~ temp + I(temp^2) + loom + date +temp*loom, family = binomial,my_data)
summary(model_glm)
binnedplot(fitted(model_glm),residuals(model_glm)) 
#aic = 1312

model_glm <-  glm(prop_startles ~ temp + I(temp^2) + loom + date, family = binomial,my_data)
summary(model_glm)
binnedplot(fitted(model_glm),residuals(model_glm)) 
#aic = 1311 #residuals not bad #best model?

newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1),
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
newData1$prop_startles <- results[,1]
newData1$prop_startles025 <- results[,2]
newData1$prop_startles975 <- results[,3]

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/prop_startles_predictions2.csv")


###########################################

########## annd after loom ################

###########################################


data <- read.csv(here("Documents","data","temp_collective","roi","all_params_w_loom.csv"),header=TRUE,na.strings=c("[nan]"))

# make new dataframe

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

model_lm_trans <- lm(log(annd) ~ temp*log(gs,2) + trial + date, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#r sq = 0.414
extractAIC(model_lm_trans) #-689
qqnorm(residuals(model_lm_trans), main= "")
qqline(residuals(model_lm_trans))

model_lm_trans <- lm(log(annd) ~ temp*log(gs,2), my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#r sq = 0.414
extractAIC(model_lm_trans) #-697
qqnorm(residuals(model_lm_trans), main= "")
qqline(residuals(model_lm_trans))

model_lm_trans <- lm(log(annd) ~ I(temp^2) + temp*log(gs,2), my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#r sq = 0.414
extractAIC(model_lm_trans) #-699
qqnorm(residuals(model_lm_trans), main= "")
qqline(residuals(model_lm_trans))