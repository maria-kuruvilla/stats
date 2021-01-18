#Loom speed percentiles

setwd("~/code/stats")
percentiles <- read.csv("../../data/temp_collective/roi/stats_loom_low_pass_data.csv",header=TRUE,na.strings=c("[nan]"))
speed99 <- percentiles$X99_speed
speed90 <- percentiles$X90_speed
speed_max <- percentiles$max_speed
temperature <- percentiles$Temperature
group_size <- percentiles$Groupsize
replicate <- percentiles$Replicate
loom <- percentiles$loom
n <- length(percentiles$Temperature)
data <- as.data.frame(cbind(sample = (1:n),speed99, speed90, speed_max,temperature,group_size, replicate, loom))

model_speed90_boxcox <- lm(speed90^0.5 ~ temperature + I(temperature^0.5) + log(group_size,2) + loom + replicate,data)
summary(model_speed90_boxcox)
plot(fitted(model_speed90_boxcox), residuals(model_speed90_boxcox))

model_speed90_boxcox <- lm(speed90^0.4 ~ temperature + I(temperature^0.5) + log(group_size,2) + loom + replicate,data)
summary(model_speed90_boxcox)
plot(fitted(model_speed90_boxcox), residuals(model_speed90_boxcox))

model_speed99 <- lm((speed99)^0.5 ~  temperature  +I(temperature^2)+ log(group_size,2) + I(log(group_size,2)^2) + loom + replicate,data)
summary(model_speed99)
plot(fitted(model_speed99), residuals(model_speed99))

#Loom acc percentiles

percentiles <- read.csv("../../data/temp_collective/roi/stats_loom_low_pass_data.csv",header=TRUE,na.strings=c("[nan]"))
acc99 <- percentiles$X99_acc
acc90 <- percentiles$X90_acc
temperature <- percentiles$Temperature
group_size <- percentiles$Groupsize
replicate <- percentiles$Replicate
loom <- percentiles$loom
n <- length(percentiles$Temperature)
data <- as.data.frame(cbind(sample = (1:n),acc99, acc90, speed_max,temperature,group_size, replicate, loom))

model_acc90 <- lm(log(acc90) ~ temperature + I(temperature^2)*loom + log(group_size,2)*loom + replicate,data)
summary(model_acc90)
plot(fitted(model_acc90), residuals(model_acc90))

#Non Loom speeds

setwd("~/code/stats")
percentiles_non_loom <- read.csv("../../data/temp_collective/roi/stats_non_loom_data.csv",header=TRUE,na.strings=c("[nan]"))
speed99 <- percentiles_non_loom$X99_speed
acc99 <- percentiles_non_loom$X99_acc
acc90 <- percentiles_non_loom$X90_speed
speed90 <- percentiles_non_loom$X90_speed
speed_max <- percentiles_non_loom$max_speed
temperature <- percentiles_non_loom$Temperature
group_size <- percentiles_non_loom$Groupsize
replicate <- percentiles_non_loom$Replicate
loom <- percentiles_non_loom$loom
n <- length(percentiles_non_loom$Temperature)
data <- as.data.frame(cbind(sample = (1:n),speed99, speed90, acc99, acc90,speed_max,temperature,group_size, replicate, loom))


model_speed99 <- lm(speed99^0.5 ~ temperature + I(temperature^2) + log(group_size,2) + replicate,data)
summary(model_speed99)
plot(fitted(model_speed99), residuals(model_speed99))

model_speed90 <- lm(speed90^0.5 ~ temperature + I(temperature^2) + log(group_size,2) + replicate,data)
summary(model_speed90)
plot(fitted(model_speed90), residuals(model_speed90))

#non loom accelerations

model_acc90 <- lm(acc90^0.5 ~ temperature + I(temperature^2) + log(group_size,2) + replicate,data)
summary(model_acc90)
plot(fitted(model_acc90), residuals(model_acc90))

####RATIOS

percentiles_ratio <- read.csv("../../data/temp_collective/roi/stats_loom_data.csv",header=TRUE,na.strings=c("[nan]"))
speed99 <- percentiles_ratio$ratio_99_speed
speed90 <- percentiles_ratio$ratio_90_speed
speed_max <- percentiles_ratio$max_speed_ratio
acc90 <- percentiles_ratio$ratio_90_acc
temperature <- percentiles_ratio$Temperature
group_size <- percentiles_ratio$Groupsize
replicate <- percentiles_ratio$Replicate
loom <- percentiles_ratio$loom
n <- length(percentiles_ratio$Temperature)
data_ratio <- as.data.frame(cbind(sample = (1:n),speed99, speed90, acc90, speed_max,temperature,group_size, replicate, loom))



model2 <- lm(speed99^0.5 ~ temperature+ log(group_size,2) + loom,data_ratio)
summary(model2)
plot(fitted(model2), residuals(model2))

model6 <- lm(acc90^(-0.5) ~ temperature + log(group_size,2) + loom,data_ratio)
summary(model6)
plot(fitted(model6), residuals(model6))

##startle accuracy


data1 <- read.csv("../../data/temp_collective/roi/startles_ratio_zero.csv",header=TRUE,na.strings=c("[nan]"))
accuracy1 <- data1$accuracy
temp1 <- data1$Temperature
gs1 <- data1$Groupsize
date1 <- as.numeric(as.Date(data1$Date, format = "%d/%m/%Y"))
time_in1 <- as.numeric(as.POSIXct(data1$Time_fish_in, format = "%H:%M"))
time_record1 <- as.numeric(as.POSIXct(data1$Time_start_record, format = "%H:%M"))
difference1 <- time_record - time_in


model10 <- glm(accuracy1 ~ temp1 + gs1, family = binomial,data)
summary(model10)

####### latency

setwd("~/Documents/code/stats")
data <- read.csv("../../data/temp_collective/roi/stats_loom_latency_nan.csv",header=TRUE,na.strings=c("[nan]"))

lat <- data$latency
#lat <- as.numeric(lat1)
temp <- data$Temperature
gs <- data$Groupsize
loom <- data$loom

model_pois6 <- glm(lat ~ temp + gs*loom + I(temp^2), family = quasipoisson, data)
summary(model_pois6)
