#goal - linear model with temperature and group size to see if tracking failure, fish swimming in background,
#fish jumping out, etc are correlated with either

#tracking failure
require(here)

data <- read.csv(here("Documents","data","temp_collective","failed_tracking.csv"),header=TRUE,na.strings=c("[nan]"))

my_data <- data.frame("temp" = data$Temperature, "gs" = data$Group.size, "n" = data$n)


model_lm <- lm(n ~ temp + log(gs,2), my_data)
summary(model_lm) # gs is positive and significant
plot(fitted(model_lm), residuals(model_lm))

require(MASS)
boxcox(model_lm, lambda = seq(0, 1, 1/10), plotit = TRUE) #0.5
#response variable must be positive

model_lm <- lm(n ~ temp + I(temp^2) + log(gs,2), my_data)
summary(model_lm) # gs is positive and significant
plot(fitted(model_lm), residuals(model_lm)) #residuals look bad               

#background/fish swimming
data <- read.csv(here("Documents","data","temp_collective","background_analysis.csv"),header=TRUE,na.strings=c("[nan]"))

my_data <- data.frame("temp" = data$Temperature, "gs" = data$Group.size, "n" = data$n)


model_lm <- lm(n ~ temp + log(gs,2), my_data)
summary(model_lm) # temp is positive and significant
plot(fitted(model_lm), residuals(model_lm)) # model is not significant

model_lm <- lm(n ~ temp + I(temp^2) + log(gs,2), my_data)
summary(model_lm) 
plot(fitted(model_lm), residuals(model_lm)) # model is not significant



#fish jumping out
require(here)

data <- read.csv(here("Documents","data","temp_collective","jumping_analysis.csv"),header=TRUE,na.strings=c("[nan]"))

my_data <- data.frame("temp" = data$Temperature, "gs" = data$Group.size, "n" = data$n)


model_lm <- lm(n ~ temp + log(gs,2), my_data)
summary(model_lm) 
plot(fitted(model_lm), residuals(model_lm)) # model is not significant

model_lm <- lm(n ~ temp + log(gs,2) + I(temp^2), my_data)
summary(model_lm) 
plot(fitted(model_lm), residuals(model_lm)) # model is not significant

#at least one fish not moving
require(here)

data <- read.csv(here("Documents","data","temp_collective","stationary_analysis.csv"),header=TRUE,na.strings=c("[nan]"))

my_data <- data.frame("temp" = data$Temperature, "gs" = data$Group.size, "n" = data$n)

model_lm <- lm(n ~ temp + log(gs,2), my_data)
summary(model_lm)  #gs is negative and significant
plot(fitted(model_lm), residuals(model_lm)) # model is significant

model_lm <- lm(n ~ temp + log(gs,2) + I(temp^2), my_data)
summary(model_lm)  #gs is negative and significant
plot(fitted(model_lm), residuals(model_lm)) # model is significant

#wall following  behaviour
require(here)

data <- read.csv(here("Documents","data","temp_collective","wall_following_analysis.csv"),header=TRUE,na.strings=c("[nan]"))

my_data <- data.frame("temp" = data$Temperature, "gs" = data$Group.size, "n" = data$n)

model_lm <- lm(n ~ temp + log(gs,2), my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm)) # model is not significant

model_lm <- lm(n ~ temp + log(gs,2) + I(temp^2), my_data)
summary(model_lm)  
plot(fitted(model_lm), residuals(model_lm)) # model is not significant
