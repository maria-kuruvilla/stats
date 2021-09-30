require(here)

data <- read.csv(here("Documents","data","temp_collective","roi","spontaneous_startles_preloom_edited.csv"),header=TRUE,na.strings=c("[nan]"))

my_data <- data.frame("temp" = data$Temperature[complete.cases(data$round_norm_startles)],
                    "gs" = data$Groupsize[complete.cases(data$round_norm_startles)],
                    "rep" = data$Replicate[complete.cases(data$round_norm_startles)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$round_norm_startles)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$round_norm_startles)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$round_norm_startles)],
                    
                    "subtrial" = data$Subtrial[complete.cases(data$round_norm_startles)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$round_norm_startles)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$round_norm_startles)], format = "%H:%M")),
                    "startles" = data$round_norm_startles[complete.cases(data$round_norm_startles)]
)


model_lm <- lm(startles ~ temp + gs, my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))

model_lm <- lm(startles ~ temp + log(gs,2), my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))

model_glm <- glm(startles ~ temp + log(gs,2), family = poisson, my_data)
summary(model_glm)
plot(fitted(model_glm), residuals(model_glm))

require(MASS)


model_glm <- glm.nb(startles ~ temp + I(temp^2) + log(gs,2), my_data)
summary(model_glm)
plot(fitted(model_glm), residuals(model_glm))

model_glm <- glm.nb(startles ~ temp + log(gs,2), my_data)
summary(model_glm)
plot(fitted(model_glm), residuals(model_glm))


nn <- length(my_data$startles)

require(faraway)
cook <- cooks.distance(model_glm)
halfnorm(cook,5)
h <- 4/nn
abline(h = 4/nn, col="red")


studentized <- rstudent(model_glm)
halfnorm(studentized)

nn <- length(my_data$startles)
t_crit <- qt(0.025/nn,nn-5-1,lower.tail = FALSE) 
my_data[which(abs(studentized)>t_crit),]

influential <- my_data[which(cook>h),]
influential <- data.frame(my_data[which(cook>h),],"cook" = cook[which(cook>h)])

my_new_data <- my_data[which(cook<h),]

#trying the same model without influential points

bad <- c(39,40,78,93,115,122,196,199,201,226,227,221)

my_new_data <- my_data[-bad,]


model_glm <- glm.nb(startles ~ temp + log(gs,2), my_new_data)
summary(model_glm)
plot(fitted(model_glm), residuals(model_glm))

model_glm <- glm.nb(startles ~ temp + I(temp^2) + log(gs,2), my_new_data)
summary(model_glm)
plot(fitted(model_glm), residuals(model_glm))


