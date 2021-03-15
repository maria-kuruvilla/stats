# Goal - To use the all params csv file to look at annd and convex hull area
#     after the loom ends
# Date - March 9th 2021

setwd("~/code/stats") #change if running in different computer

data  <- read.csv("../../data/temp_collective/roi/all_params_w_loom.csv",
                      header=TRUE,na.strings=c("[nan]"))

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

### let us go with this model
model_lm_trans <- lm(log(annd) ~ temp*log(gs,2) + trial + date, my_data)
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


# the residual plots are not good

## predictions 
model_lm_trans <- lm(log(annd) ~ temp*log(gs,2) + trial + date, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))

newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), 
                                   gs = c(1,2,4,8,16),
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