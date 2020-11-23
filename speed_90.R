stats_speed <- read.csv("../../data/temp_collective/roi/stats_speed.csv",header=TRUE,na.strings=c("[nan]"))

speed <- stats_speed$X90_speed
temp <- stats_speed$Temperature
gs <- stats_speed$Groupsize

n <- length(stats_speed$X90_speed)
speed_data <- as.data.frame(cbind(sample = (1:n),speed,temp,gs))

######### Linear model with temperature and group size ####

model <- lm(speed ~ temp + gs,speed_data)
summary(model)


############## Errors ####################

plot(fitted(model), residuals(model))

plot(temp, residuals(model))
plot(gs, residuals(model))

qqnorm(residuals(model))
qqline(residuals(model))

shapiro.test(residuals(model))


model_int <- lm(speed ~ temp*gs,speed_data)
summary(model_int)
plot(fitted(model_int), residuals(model_int))

qqnorm(residuals(model_int))
qqline(residuals(model_int))

################
require(MASS)
boxcox(model_int,plotit=TRUE)

model_transform <- lm(speed^0.1 ~ temp + gs,speed_data)
summary(model_transform)
plot(fitted(model_transform), residuals(model_transform))

qqnorm(residuals(model_transform))
qqline(residuals(model_transform))
########################################### log tranformation ####
require(MASS)
boxcox(model,plotit=TRUE)

model_exp <- lm(log(speed) ~ temp + log(gs,2),speed_data)
summary(model_exp)
plot(fitted(model_exp), residuals(model_exp))

qqnorm(residuals(model_exp))
qqline(residuals(model_exp))

shapiro.test(residuals(model_exp))

model_exp_inv <- lm(log(speed) ~ temp*gs,speed_data)
summary(model_exp_inv)
plot(fitted(model_exp_inv), residuals(model_exp_inv))

qqnorm(residuals(model_exp_inv))
qqline(residuals(model_exp_inv))

shapiro.test(residuals(model_exp_inv))

############### 


transform.bc <- function(x)(exp(x))
out <- predict(model_exp, newdata = data.frame(x = temp), interval="prediction")

plot(speed ~ temp, ylab = "90th percentile of speed", xlab = "Temperature")
lines(sort(temp),sort(transform.bc(out)[,1] ) ,col="red")
lines(sort(temp),sort(transform.bc(out)[,2] ),lty = 2,col="red")
lines(sort(temp),sort(transform.bc(out)[,3] ),lty = 2,col="red")

out2 <- predict(model_exp, newdata = data.frame(x = gs), interval="prediction")

plot(speed ~ gs, ylab = "90th percentile of speed", xlab = "Group Size")
lines(sort(gs),sort(transform.bc(out2)[,1] ) ,col="red")
lines(sort(gs),sort(transform.bc(out2)[,2] ),lty = 2,col="red")
lines(sort(gs),sort(transform.bc(out2)[,3] ),lty = 2,col="red")

h <- hatvalues(model_exp)
two_p_over_n <- 2*mean(h)
speed_data$sample[which(h>two_p_over_n)]

require(faraway)
num <- speed_data$sample
halfnorm(h,nlab = 5, labs= num, ylab="Leverages")

studentized <- rstudent(model_exp)
(max.student <- studentized[which.max(abs(studentized))]) 
speed_data[which(abs(studentized)>t_crit),]
t_crit <- qt(0.025/n,n-3-1,lower.tail = FALSE) 


############ GLM ########################

model_glm_int <- glm(speed ~ temp*gs, family = Gamma)
summary(model_glm_int,dispersion=1) 

model_glm <- glm(speed ~ temp + gs, family = Gamma)
summary(model_glm,dispersion=1) 