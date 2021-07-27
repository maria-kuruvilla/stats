require(here)

data <- read.csv(here("Documents","data","temp_collective","roi","spontaneous_startles_before_loom.csv"),header=TRUE,na.strings=c("[nan]"))

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$prop_startles)],
                    "gs" = data$Groupsize[complete.cases(data$prop_startles)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$prop_startles)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$prop_startles)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$prop_startles)],
                    "subtrial" = data$Subtrial[complete.cases(data$prop_startles)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$prop_startles)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$prop_startles)], format = "%H:%M")),
                    "prop" = data$prop_startles[complete.cases(data$prop_startles)]
)

library(arm)
model_glm <-  glm(prop ~ temp + gs, family = binomial,my_data)
summary(model_glm)
binnedplot(fitted(model_glm),residuals(model_glm)) 
#aic = 13

model_glm <-  glm(prop ~ temp + I(temp^2) +  gs, family = binomial,my_data)
summary(model_glm)
binnedplot(fitted(model_glm),residuals(model_glm)) 
#aic = 15


model_glm <-  glm.nb(prop ~ temp + I(temp^2) +  gs,my_data)
summary(model_glm)
binnedplot(fitted(model_glm),residuals(model_glm)) 
#aic = 13

#trying linear models

model_lm <- lm(prop ~ temp + gs, my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))


model_lm <- lm(prop ~ temp + I(temp^2) + gs, my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))

model_lm <- lm(prop ~ temp + I(temp^2) + gs + I(gs^2), my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))


#look at data that is not zero

my_data<-data.frame("temp" = data$Temperature[data$prop_startles!=0],
                    "gs" = data$Groupsize[data$prop_startles!=0],
                    "kt"=1/(0.00008617*(data$Temperature[data$prop_startles!=0]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[data$prop_startles!=0], format = "%d/%m/%Y")),
                    "trial" = data$Trial[data$prop_startles!=0],
                    "subtrial" = data$Subtrial[data$prop_startles!=0],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[data$prop_startles!=0], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[data$prop_startles!=0], format = "%H:%M")),
                    "prop" = data$prop_startles[data$prop_startles!=0]
)


library(arm)
model_glm <-  glm(prop ~ temp + gs, family = binomial,my_data)
summary(model_glm)
binnedplot(fitted(model_glm),residuals(model_glm)) 
#aic = 14.5


model_glm <-  glm(prop ~ temp + I(temp^2) + gs, family = binomial,my_data)
summary(model_glm)
binnedplot(fitted(model_glm),residuals(model_glm)) 
#aic = 16.5



model_glm <-  glm(prop ~ temp + I(temp^2) + gs + I(gs^2), family = binomial,my_data)
summary(model_glm)
binnedplot(fitted(model_glm),residuals(model_glm)) 
#aic = 18

#look at data that is not zero - numer of startles

my_data<-data.frame("temp" = data$Temperature[data$number_startles!=0],
                    "gs" = data$Groupsize[data$number_startles!=0],
                    "kt"=1/(0.00008617*(data$Temperature[data$number_startles!=0]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[data$number_startles!=0], format = "%d/%m/%Y")),
                    "trial" = data$Trial[data$number_startles!=0],
                    "subtrial" = data$Subtrial[data$number_startles!=0],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[data$number_startles!=0], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[data$number_startles!=0], format = "%H:%M")),
                    "startles" = data$number_startles[data$number_startles!=0]
)



model_glm <-  glm(startles ~ temp + gs, family = poisson,my_data)
summary(model_glm)
binnedplot(fitted(model_glm),residuals(model_glm)) 
#aic = 87



model_glm <-  glm(startles ~ temp + I(temp^2) + gs, family = poisson,my_data)
summary(model_glm)
binnedplot(fitted(model_glm),residuals(model_glm)) 
#aic = 87








