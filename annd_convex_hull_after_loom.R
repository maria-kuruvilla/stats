# Goal - To use the all params csv file to look at annd and convex hull area
#     after the loom ends
# Date - March 9th 2021


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

model_lm <- lm(annd ~ temp + gs + loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))

model_lm <- lm(annd ~ temp + log(gs,2) + loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))

model_lm <- lm(annd ~ kt + log(gs,2) + loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))

model_lm <- lm(annd ~ I(temp^2) + log(gs,2) + loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))

model_lm <- lm(annd ~ temp + I(log(gs,2)^2) + loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))

##Transformaing the response variable


model_lm_trans <- lm(log(annd) ~ temp + gs + loom, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#better residual plot

model_lm_trans <- lm(log(annd) ~ temp + log(gs,2) + loom, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))

model_lm_trans <- lm(log(annd) ~ I(temp^2) + temp + log(gs,2) + loom, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))

model_lm_trans <- lm(log(annd) ~ I(temp^2) + temp + log(gs,2) + I(log(gs,2)^2) + loom, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))

model_lm_trans <- lm(log(annd) ~ I(temp^2) + temp*log(gs,2) + I(log(gs,2)^2) + loom, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#r sq = 0.41


model_lm_trans <- lm(log(annd) ~ kt*log(gs,2), my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#r sq = 0.413

model_lm_trans <- lm(log(annd) ~ temp*log(gs,2), my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#r sq 0.4129

model_lm_trans <- lm(log(annd) ~ temp*log(gs,2) + date, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#r sq 0.414


model_lm_trans <- lm(log(annd) ~ temp*log(gs,2) + trial + date, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#r sq 0.4144 #interaction is significant

model_lm_trans <- lm(log(annd) ~ temp*log(gs,2) + trial + date + t1 + t2, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#r sq 0.4135

model_lm_trans <- lm(log(annd) ~ temp*log(gs,2) + trial + date + t1 + t2 + subtrial, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#r sq 0.4135

qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))

model_lm_trans <- lm(log(annd) ~ temp*log(gs,2) + I(loom^2)+trial + date + t1 + t2 + subtrial, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#r sq 0.4127

qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))

## GLM?

model_glm <- glm(annd ~ temp*log(gs,2),family = Gamma,my_data)
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))

#how much variation does temp explain - 2%
model_lm_trans <- lm(log(annd) ~ log(gs,2) + trial + date, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))

### let us go with this model
model_lm_trans <- lm(log(annd) ~ temp*log(gs,2) + trial + date + temp + log(gs,2), my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))

ee <- residuals(model_lm_trans)
g1 <- ee[ee>median(ee)]
g2 <- ee[ee<median(ee)]
var.test(g1,g2) #>0.05 but not by much

library(faraway)

h <- hatvalues(model_lm_trans)

halfnorm(h)

two.p.n <- 2*sum(h)/length(h)

abline(a=two.p.n,b=0)

studentized <- rstudent(model_lm_trans)
halfnorm(studentized)


halfnorm(cooks <- cooks.distance(model_lm_trans))

nn <- length(my_data$annd)
t_crit <- qt(0.025/nn,nn-length(coef(model_lm_trans)),lower.tail = FALSE) 
which(abs(studentized)>t_crit)


halfnorm(cooks <- cooks.distance(model_lm))
nn <- length(my_data$annd)
t_crit <- qt(0.025/nn,nn-length(coef(model_lm_trans)),lower.tail = FALSE) 
which(abs(studentized)>t_crit)
#6,200,556,658


#random effect
library(lme4)
model_rand <- lmer(annd~temp*gs + (1|date) + (1|trial),my_data)
summary(model_rand)
plot(fitted(model_rand),residuals(model_rand))

model_rand <- lmer(annd~temp*gs + (1|date:trial:subtrial),my_data)
summary(model_rand)
plot(fitted(model_rand),residuals(model_rand))

model_rand <- lmer(annd~temp*gs + (1|date:trial),my_data)
summary(model_rand)
plot(fitted(model_rand),residuals(model_rand))

model_lm_trans <- lm(log(annd) ~ log(gs,2), my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
# the residual plots are not good

## predictions 
model_lm_trans <- lm(log(annd) ~ temp*log(gs,2) + trial + date, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))

ee <- residuals(model_lm_trans)
g1 <- ee[ee>median(ee)]
g2 <- ee[ee<median(ee)]
var.test(g1,g2) #0.08
#0.4

newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), 
                                   gs = c(2,4,8,16),
                                   trial = unique(my_data$trial),
                                   date = unique(my_data$date)))
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

intersection <- intersect(which(newData1$gs == 8),
                          intersect(which(newData1$trial == 10),
                          which(newData1$date == 18106)))

temp <- newData1$temp[intersection]

plot(my_data$temp, my_data$annd)
lines(temp, exp(results[intersection,1]),lty = "solid")
lines(temp, exp(results[intersection,2]),lty = "dashed")
lines(temp, exp(results[intersection,3]),lty = "dashed")

#gs 16
intersection <- intersect(which(newData1$gs == 16),
                          intersect(which(newData1$trial == 10),
                                    which(newData1$date == 18106)))

temp <- newData1$temp[intersection]

plot(my_data$temp, my_data$annd)
lines(temp, exp(results[intersection,1]),lty = "solid")
lines(temp, exp(results[intersection,2]),lty = "dashed")
lines(temp, exp(results[intersection,3]),lty = "dashed")

#gs 1
intersection <- intersect(which(newData1$gs == 1),
                          intersect(which(newData1$trial == 10),
                                    which(newData1$date == 18106)))

temp <- newData1$temp[intersection]

plot(my_data$temp, my_data$annd)
lines(temp, exp(results[intersection,1]),lty = "solid")
lines(temp, exp(results[intersection,2]),lty = "dashed")
lines(temp, exp(results[intersection,3]),lty = "dashed")

newData1$annd <- results[,1]
newData1$annd025 <- results[,2]
newData1$annd975 <- results[,3]

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/annd_after_loom_predictions.csv")



#########################################################################

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

model_lm <- lm(hull ~ temp*gs*loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))

model_lm <- lm(hull ~ kt*gs*loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))

model_lm <- lm(hull ~ temp + gs + loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# r sq 0.38

model_lm <- lm(hull ~ temp + I(temp^2) + gs + loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# r sq 0.38

model_lm <- lm(hull ~ temp + I(temp^2)*gs + loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# r sq 0.38

model_lm <- lm(hull ~ temp*gs + I(temp^2) + loom, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# r sq 0.38

model_lm <- lm(hull ~ temp + I(temp^2) + loom*gs, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# r sq 0.38

model_lm <- lm(hull ~ temp + loom*I(temp^2) + gs, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# r sq 0.388

model_lm <- lm(hull ~ loom*temp + I(temp^2) + gs, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# r sq 0.389

model_lm <- lm(hull ~ loom*temp + I(temp^2) + gs + date, my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
# r sq 0.388

model_lm_trans <- lm(log(hull) ~ loom*temp + I(temp^2) + gs, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
# r sq 0.412

model_lm_trans <- lm((hull)^0.5 ~ loom*temp + I(temp^2) + gs, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
# r sq 0.409

#compare with just gs
model_lm_trans <- lm(log(hull) ~ gs, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
# r sq 0.35

model_lm_trans <- lm(log(hull) ~ loom*temp + I(temp^2) + gs + I(gs^2), my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
# r sq 0.4285


model_lm_trans <- lm(log(hull) ~ loom*temp + I(temp^2) + gs + I(gs^2), my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
# r sq 0.4285

model_lm_trans <- lm(log(hull) ~ loom*temp + I(temp^2) + gs + I(gs^2), my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
# r sq 0.4285
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))

#predictions
newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), 
                                   gs = c(4,8,16),
                                   loom = c(1,2,3,4,5)))
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
newData1$hull <- results[,1]
newData1$hull025 <- results[,2]
newData1$hull975 <- results[,3]

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/hull_after_loom_predictions.csv")

#gs 4 loom 1
intersection <- intersect(which(newData1$gs == 4),
                          which(newData1$loom == 1))

temp <- newData1$temp[intersection]

plot(my_data$temp, my_data$hull)
lines(temp, exp(results[intersection,1]),lty = "solid")
lines(temp, exp(results[intersection,2]),lty = "dashed")
lines(temp, exp(results[intersection,3]),lty = "dashed")

#gs 4 loom 5
intersection <- intersect(which(newData1$gs == 4),
                          which(newData1$loom == 5))

temp <- newData1$temp[intersection]

lines(temp, exp(results[intersection,1]),lty = "solid", col = "green")
lines(temp, exp(results[intersection,2]),lty = "dashed", col = "green")
lines(temp, exp(results[intersection,3]),lty = "dashed", col = "green")

#gs 16 loom 1
intersection <- intersect(which(newData1$gs == 16),
                          which(newData1$loom == 1))

temp <- newData1$temp[intersection]

lines(temp, exp(results[intersection,1]),lty = "solid", col = "red")
lines(temp, exp(results[intersection,2]),lty = "dashed", col = "red")
lines(temp, exp(results[intersection,3]),lty = "dashed", col = "red")

#gs 16 loom 5
intersection <- intersect(which(newData1$gs == 16),
                          which(newData1$loom == 5))

temp <- newData1$temp[intersection]

lines(temp, exp(results[intersection,1]),lty = "solid", col = "blue")
lines(temp, exp(results[intersection,2]),lty = "dashed", col = "blue")
lines(temp, exp(results[intersection,3]),lty = "dashed", col = "blue")



## looking at convex hull area after loom 



model_lm_trans <- lm(log(hull) ~ loom*temp + I(temp^2) + gs + I(gs^2) + t, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
# r sq 0.4298
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))


model_lm_trans <- lm(log(hull) ~ loom*temp + I(temp^2) + gs + I(gs^2) + date, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
# r sq 0.428
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))

model_lm_trans <- lm(log(hull) ~ loom*temp + I(temp^2) + gs + I(gs^2) + t1, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
# r sq 0.4278
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))


model_lm_trans <- lm(log(hull) ~ loom*temp + gs + I(gs^2) + temp + loom, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
# r sq 0.4291
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))


model_lm_trans <- lm(log(hull) ~ loom*temp + gs + I(gs^2) + temp + loom, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
# r sq 0.4291
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))

model_lm_trans <- lm(log(hull) ~ loom*temp + gs + I(gs^2) + temp + loom + t, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
# r sq 0.4303
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))

model_lm_trans <- lm(log(hull) ~ loom*temp + log(gs,2) + I(log(gs,2)^2) + temp + loom + t + temp*log(gs,2), my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
# r sq 0.4313
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))

model_lm_trans <- lm(log(hull) ~ loom*temp + log(gs,2)  + temp + loom + t, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
# r sq 0.4308
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))

model_lm_trans <- lm(hull ~ loom*temp + log(gs,2)  + temp + loom + t, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
# r sq 0.3907
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))

model_lm_trans <- lm(hull ~ loom*temp + log(gs,2)  + temp + loom + t, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
# r sq 0.3907
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))

require(MASS)
boxcox(model_lm,lambda = seq(0, 1, 1/10),plotit = TRUE)
#0.4 but 0.5 is acceptable

model_lm_trans <- lm(hull^0.5 ~ loom*temp + log(gs,2)  + temp + loom + t, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
# r sq 0.4192
qqnorm(residuals(model_lm_trans))
qqline(residuals(model_lm_trans))

model_glm <- glm(hull ~ loom*temp + log(gs,2)  + temp + loom + t, family=Gamma, my_data)
summary(model_glm)
plot(fitted(model_glm),residuals(model_glm))
