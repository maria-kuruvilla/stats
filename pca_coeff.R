###############################################################################

##########################   pca oeff           ###############################

###############################################################################


data <- read.csv(here("Documents","data","temp_collective","roi","pca_coeff.csv"),header=TRUE,na.strings=c("[nan]"))

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$pca_coeff)],
                    "gs" = data$Groupsize[complete.cases(data$pca_coeff)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$pca_coeff)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$pca_coeff)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$pca_coeff)],
                    "subtrial" = data$Subtrial[complete.cases(data$pca_coeff)],
                    "loom" = data$Loom[complete.cases(data$pca_coeff)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$pca_coeff)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$pca_coeff)], format = "%H:%M")),
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$pca_coeff)], format = "%H:%M")) - 
                      as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$pca_coeff)], format = "%H:%M")),
                    "pca" = data$pca_coeff[complete.cases(data$pca_coeff)]
)

hist(abs(my_data$pca),30)

model_lm <- lm((pca) ~ temp,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.04
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
extractAIC(model_lm) #-3953
#this is best

require(MASS)
boxcox(model_lm,plotit = TRUE,seq(6,12,0.1))
#9?
hist(((my_data$pca)+1)^9,20)


hist(((my_data$pca))^2,20)

model_lm <- lm(pca+1 ~ temp + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.04
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
extractAIC(model_lm) #-3960
#this is best

require(MASS)
boxcox(model_lm,plotit = TRUE,seq(6,12,0.1))
#9?

model_lm <- lm((pca+1)^9 ~ temp + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.04
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
extractAIC(model_lm) #-724
#all of them have very bad residuals


model_lm <- lm((pca) ~ temp + log(gs,2) + loom + date + t1,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.04
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
extractAIC(model_lm) #-3953


# bernoulli


data <- read.csv(here("Documents","data","temp_collective","roi","pca_coeff_bernoulli.csv"),header=TRUE,na.strings=c("[nan]"))

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$pca_coeff)],
                    "gs" = data$Groupsize[complete.cases(data$pca_coeff)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$pca_coeff)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$pca_coeff)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$pca_coeff)],
                    "subtrial" = data$Subtrial[complete.cases(data$pca_coeff)],
                    "loom" = data$Loom[complete.cases(data$pca_coeff)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$pca_coeff)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$pca_coeff)], format = "%H:%M")),
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$pca_coeff)], format = "%H:%M")) - 
                      as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$pca_coeff)], format = "%H:%M")),
                    "pca" = data$pca_coeff[complete.cases(data$pca_coeff)]
)

hist(my_data$pca)

model_lm <- lm((pca) ~ temp + log(gs,2) + loom + date + t1,my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))
#r sq =0.04
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))
extractAIC(model_lm) #-1100

model_glm <- glm(pca ~ temp + log(gs,2) + loom, family = binomial, data = my_data)
summary(model_glm)
#aic = 841
library(arm)
binnedplot(fitted(model_glm),residuals(model_glm))

model_glm <- glm(pca ~ log(gs,2) + loom, family = binomial, data = my_data)
summary(model_glm)
#aic = 877
library(arm)
binnedplot(fitted(model_glm),residuals(model_glm))


model_glm <- glm(pca ~ temp + I(temp^2) + log(gs,2) + loom, family = binomial, data = my_data)
summary(model_glm)
#aic = 834
library(arm)
binnedplot(fitted(model_glm),residuals(model_glm))


model_glm <- glm(pca ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2), family = binomial, data = my_data)
summary(model_glm)
#aic = 832
library(arm)
binnedplot(fitted(model_glm),residuals(model_glm))



model_glm <- glm(pca ~ temp + I(temp^2) + date, family = binomial, data = my_data)
summary(model_glm)
#aic = 837
library(arm)
binnedplot(fitted(model_glm),residuals(model_glm))

model_glm <- glm(pca ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2) + date + t, family = binomial, data = my_data)
summary(model_glm)
#aic = 836
library(arm)
binnedplot(fitted(model_glm),residuals(model_glm))

model_glm <- glm(pca ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2) + date + t1, family = binomial, data = my_data)
summary(model_glm)
#aic = 836
library(arm)
binnedplot(fitted(model_glm),residuals(model_glm))


#predictions 

model_glm <- glm(pca ~ temp + I(temp^2) + log(gs,2) + I(log(gs,2)^2) + date, family = binomial, data = my_data)
summary(model_glm)
#aic = 834
library(arm)
binnedplot(fitted(model_glm),residuals(model_glm))
# this is what I am going with 


newData1 <- data.frame(expand.grid(temp = seq(from = 9, to = 29, by = 1), gs = c(1,2,4,8,16), date = unique(my_data$date)))
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
newData1$pca <- results[,1]
newData1$pca_025 <- results[,2]
newData1$pca_975 <- results[,3]

write.csv(newData1,"/home/maria/Documents/data/temp_collective/roi/pca_predictions.csv")
