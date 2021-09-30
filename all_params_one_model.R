# goal - try one model for all params.
# one model should have temp + temp^2 + groupsize + loom .

#speed during loom

require(here)

data <- read.csv(here("..","..","..","Documents","data","temp_collective","roi","all_params_w_loom.csv"),header=TRUE,na.strings=c("[nan]"))


#speed during loom
my_data<-data.frame("temp" = data$Temperature[complete.cases(data$speed_percentile99)],
                    "gs" = data$Groupsize[complete.cases(data$speed_percentile99)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$speed_percentile99)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$speed_percentile99)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$speed_percentile99)],
                    "subtrial" = data$Subtrial[complete.cases(data$speed_percentile99)],
                    "loom" = data$Loom[complete.cases(data$speed_percentile99)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$speed_percentile99)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$speed_percentile99)], format = "%H:%M")),
                    "speed" = data$speed_percentile99[complete.cases(data$speed_percentile99)]
)

model_lm <- lm(log(speed+1) ~ temp + I(temp^2) + log(gs,2) + loom + t1 + I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.20

model_lm <- lm(sqrt(speed) ~ temp + I(temp^2) + log(gs,2) + loom + t1 + I(log(gs,2)^2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.18

model_lm <- lm(log(speed) ~ temp + I(temp^2) + log(gs,2) + loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.20

model_lm <- lm(sqrt(speed) ~ temp + I(temp^2) + log(gs,2) + loom ,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.17 #this works
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

#acceleration during loom

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

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1763 #this works!


model_lm <- lm(log(acc) ~ temp + I(temp^2) + log(gs,2) + loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.1763 #this works!


#latency

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

model_pois6 <- glm(latency ~ temp + loom + I(temp^2) + log(gs,2), family = quasipoisson, my_data)
summary(model_pois6)
plot(fitted(model_pois6),residuals(model_pois6))

my_new_data2 <- my_data[-c(750,752,327,695),]

model_pois6 <- glm(latency ~ temp + loom + I(temp^2) + log(gs,2), family = quasipoisson, my_new_data2)
summary(model_pois6)
plot(fitted(model_pois6),residuals(model_pois6))
#residual dev = 202 out of 215

#prop ind startling

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

model_glm <-  glm(prop_startles ~ temp + I(temp^2) + loom + log(gs,2), family = binomial,my_data)
summary(model_glm)

#resid = 693/739, residual plot bad

library(arm)
binnedplot(fitted(model_glm),residuals(model_glm)) 


#goodness of fit
X2 <- sum((my_data$prop_startles - fitted(model_glm))^2/fitted(model_glm))
nn <- length(my_data$prop_startles)
pchisq(X2, df = nn-length(coef(model_glm)), lower.tail = FALSE)
#p  = 1

#annd after loom


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

model_lm_trans <- lm(log(annd+1) ~ temp*log(gs,2) + trial + date, my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))

model_lm_trans <- lm(log(annd+1) ~ temp +log(gs,2) + loom + I(temp^2), my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))


model_lm_trans <- lm(log(annd) ~ temp +log(gs,2) + loom + I(temp^2), my_data)
summary(model_lm_trans)
plot(fitted(model_lm_trans),residuals(model_lm_trans))
#this works

qqnorm(residuals(model_lm_trans), main= "")
qqline(residuals(model_lm_trans))

#convex hull area


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

model_lm<- lm(hull ~  temp + I(temp^2) +loom + log(gs,2) , my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))

require(MASS)
boxcox(model_lm, lambda = seq(0, 1, 1/10), plotit = TRUE)

qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

model_lm <- lm(hull^0.5 ~  temp + I(temp^2) +loom + log(gs,2) , my_data)
summary(model_lm)
plot(fitted(model_lm),residuals(model_lm))


qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

#unperturbed


data <- read.csv(here("..","..","..","Documents","data","temp_collective","roi","all_params_wo_loom.csv"),header=TRUE,na.strings=c("[nan]"))


#speed beforeloom
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

model_lm <- lm(log(speed+1) ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.20 

#acceleration before loom

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
#r sq = 0.09


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

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2),my_new_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#r sq = 0.09

#polarization during loom


data <- read.csv(here("..","..","..","Documents","data","temp_collective","roi","all_params_w_loom_pol.csv"),header=TRUE,na.strings=c("[nan]"))


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
hist(abs(my_data$pol),20)
hist(sqrt(abs(my_data$pol)),20)


model_lm <- lm(pol ~ temp + I(temp^2) + log(gs,2) + loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#rsq 0.023 #temp not significant


model_lm <- lm(sqrt(abs(pol)) ~ temp + I(temp^2) + log(gs,2) + loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#rsq 0.027 #temp not significant



model_lm_int <- lm(pol ~ temp + I(temp^2) + log(gs,2) + loom + temp*log(gs,2),my_data)
summary(model_lm_int)
plot(fitted(model_lm_int), residuals(model_lm_int))
#rsq 0.022 #temp not significant


lrtest(model_lm,model_lm_int) # not significant


#polarization during loom with 2nd closest neighbor
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


#polarization after loom


data <- read.csv(here("..","..","..","Documents","data","temp_collective","roi","all_params_w_loom_pol.csv"),header=TRUE,na.strings=c("[nan]"))


# make new dataframe

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$polarization_1_postloom)],
                    "gs" = data$Groupsize[complete.cases(data$polarization_1_postloom)],
                    "loom" = data$Loom[complete.cases(data$polarization_1_postloom)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$polarization_1_postloom)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$polarization_1_postloom)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$polarization_1_postloom)],
                    "subtrial" = data$Subtrial[complete.cases(data$polarization_1_postloom)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$polarization_1_postloom)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$polarization_1_postloom)], format = "%H:%M")),
                    "t" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$polarization_1_postloom)], format = "%H:%M")) -
                      as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$polarization_1_postloom)], format = "%H:%M")),
                    "pol" = data$polarization_1_postloom[complete.cases(data$polarization_1_postloom)]
)

model_lm <- lm(pol ~ temp + I(temp^2) + log(gs,2) + loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#rsq 0.014 #temp and temp^2 is significant

model_lm <- lm(abs(pol) ~ temp + I(temp^2) + log(gs,2) + loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#rsq 0.053 #temp and temp^2 is significant

require(MASS)
boxcox(model_lm, lambda = seq(0, 1, 1/10), plotit = TRUE) #0.5

model_lm <- lm(sqrt(abs(pol)) ~ temp + I(temp^2) + log(gs,2) + loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#rsq 0.038 #temp and temp^2 is significant
rsq(model_lm) #0.04256
qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))




model_lm_int <- lm(sqrt(abs(pol)) ~ temp + I(temp^2) + log(gs,2) + loom + log(gs,2)*temp,my_data)
summary(model_lm_int)
plot(fitted(model_lm_int), residuals(model_lm_int))
#rsq 0.037 #temp and temp^2 is significant


lrtest(model_lm,model_lm_int) #not significant


#polarization of 2nd closest neighbor

data <- read.csv(here("..","..","..","Documents","data","temp_collective","roi","all_params_w_loom_pol.csv"),header=TRUE,na.strings=c("[nan]"))


# make new dataframe

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$polarization_2_postloom)],
                    "gs" = data$Groupsize[complete.cases(data$polarization_2_postloom)],
                    "loom" = data$Loom[complete.cases(data$polarization_2_postloom)],
                    "pol" = data$polarization_2_postloom[complete.cases(data$polarization_2_postloom)]
)

model_lm <- lm(sqrt(abs(pol)) ~ temp + I(temp^2) + log(gs,2) + loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#rsq 0.019 #temp and temp^2 is not significant


qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))


model_lm <- lm(sqrt(abs(pol)) ~ temp + I(temp^2) + log(gs,2) + loom + log(gs,2)*temp,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#rsq 0.017 #temp and temp^2 is not significant


#trying latency with negative values

data <- read.csv(here("..","..","..","Documents","data","temp_collective","roi","all_params_w_loom.csv"),header=TRUE,na.strings=c("[nan]"))


my_data<-data.frame("temp" = data$Temperature[complete.cases(data$latency)],
                    "gs" = data$Groupsize[complete.cases(data$latency)],
                    "loom" = data$Loom[complete.cases(data$latency)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$latency)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$latency)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$latency)],
                    "subtrial" = data$Subtrial[complete.cases(data$latency)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$latency)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$latency)], format = "%H:%M")),
                    "latency" = data$latency[complete.cases(data$latency)]/60 -10
)


model_lm <- lm(latency ~ temp + I(temp^2) + log(gs,2) + loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
rsq(model_lm)
#0.0558


qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))


library(faraway)

h <- hatvalues(model_lm)

halfnorm(h)

two.p.n <- 2*sum(h)/length(h)

abline(a=two.p.n,b=0)

studentized <- rstudent(model_lm)
halfnorm(studentized)


halfnorm(cooks <- cooks.distance(model_lm),5)
my_data[which(abs(studentized)>t_crit),]
t_crit <- qt(0.025/nn,nn-5-1,lower.tail = FALSE) 

my_new_data2 <- my_data[-c(750,752,327,695),]

model_lm <- lm(latency ~ temp + I(temp^2) + log(gs,2) + loom,my_new_data2)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
rsq(model_lm)
#0.0628


qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))


model_lm_int <- lm(latency ~ temp + I(temp^2) + log(gs,2) + loom +log(gs,2)*temp,my_new_data2)
summary(model_lm_int)
plot(fitted(model_lm_int), residuals(model_lm_int))
rsq(model_lm_int)
#0.0659
lrtest(model_lm,model_lm_int) #not significant


#polarization before loom


data <- read.csv(here("..","..","..","Documents","data","temp_collective","roi","all_params_wo_loom_pol.csv"),header=TRUE,na.strings=c("[nan]"))

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

model_lm <- lm(pol ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#rsq negative #temp and temp^2 is not significant

qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))



# trying proportion with linear model 
data <- read.csv(here("..","..","..","Documents","data","temp_collective","roi","all_params_w_loom.csv"),header=TRUE,na.strings=c("[nan]"))

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


model_lm <- lm(prop_startles ~ temp + I(temp^2) + log(gs,2) + loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#residuals suck

qqnorm(residuals(model_lm), main= "")
qqline(residuals(model_lm))

#mean speed before loom


data <- read.csv(here("..","..","..","Documents","data","temp_collective","roi","all_params_wo_loom.csv"),header=TRUE,na.strings=c("[nan]"))

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$avg_speed)],
                    "gs" = data$Groupsize[complete.cases(data$avg_speed)],
                    "kt"=1/(0.00008617*(data$Temperature[complete.cases(data$avg_speed)]+273.1)),
                    "date"=as.numeric(as.Date(data$Date[complete.cases(data$avg_speed)], format = "%d/%m/%Y")),
                    "trial" = data$Trial[complete.cases(data$avg_speed)],
                    "subtrial" = data$Subtrial[complete.cases(data$avg_speed)],
                    "t1" = as.numeric(as.POSIXct(data$Time_fish_in[complete.cases(data$avg_speed)], format = "%H:%M")),
                    "t2" = as.numeric(as.POSIXct(data$Time_start_record[complete.cases(data$avg_speed)], format = "%H:%M")),
                    "speed" = data$avg_speed[complete.cases(data$avg_speed)]
)

model_lm <- lm(log(speed+1) ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#temp terms are not significant 

model_lm <- lm(speed ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))

require(MASS)
boxcox(model_lm, lambda = seq(0, 1, 1/10), plotit = TRUE) #0.5

model_lm <- lm(sqrt(speed) ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#temp terms not significant

model_lm <- lm(log(speed+1) ~ temp ,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#temp term is significant

model_lm_int <- lm(log(speed+1) ~ temp + I(temp^2) + log(gs,2)+log(gs,2)*temp,my_data)
summary(model_lm_int)
plot(fitted(model_lm_int), residuals(model_lm_int))
#temp terms are not significant 


model_lm <- lm(log(speed+1) ~ temp + I(temp^2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#temp terms are not significant

#median speed before loom

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$speed_percentile50)],
                    "gs" = data$Groupsize[complete.cases(data$speed_percentile50)],
                    "speed" = data$speed_percentile50[complete.cases(data$speed_percentile50)]
)

model_lm <- lm(log(speed+1) ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#temp terms are not significant 

#avg acc before loom

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$avg_acc)],
                    "gs" = data$Groupsize[complete.cases(data$avg_acc)],
                    "acc" = data$avg_acc[complete.cases(data$avg_acc)]
)

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))

model_lm_int <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + temp*log(gs,2),my_data)
summary(model_lm_int)
plot(fitted(model_lm_int), residuals(model_lm_int))
lrtest(model_lm,model_lm_int) # significant



#median acc before loom

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$acc_percentile50)],
                    "gs" = data$Groupsize[complete.cases(data$acc_percentile50)],
                    "acc" = data$acc_percentile50[complete.cases(data$acc_percentile50)]
)

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))

model_lm_int <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) + temp*log(gs,2),my_data)
summary(model_lm_int)
plot(fitted(model_lm_int), residuals(model_lm_int))
lrtest(model_lm,model_lm_int) # not significant

#avg speed during loom

data <- read.csv(here("..","..","..","Documents","data","temp_collective","roi","all_params_w_loom.csv"),header=TRUE,na.strings=c("[nan]"))

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$avg_speed)],
                    "gs" = data$Groupsize[complete.cases(data$avg_speed)],
                    "loom" = data$Loom[complete.cases(data$avg_speed)],
                    "speed" = data$avg_speed[complete.cases(data$avg_speed)]
)

model_lm <- lm(log(speed+1) ~ temp + I(temp^2) + log(gs,2) +loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#temp terms are not significant 

model_lm <- lm(sqrt(speed) ~ temp + I(temp^2) + log(gs,2)+loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#temp terms are not significant 

#median speed during loom

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$speed_percentile50)],
                    "gs" = data$Groupsize[complete.cases(data$speed_percentile50)],
                    "loom" = data$Loom[complete.cases(data$speed_percentile50)],
                    "speed" = data$speed_percentile50[complete.cases(data$speed_percentile50)]
)



model_lm <- lm(log(speed+1) ~ temp + I(temp^2) + log(gs,2) +loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#temp terms are not significant 

#avg acceleration during loom



my_data<-data.frame("temp" = data$Temperature[complete.cases(data$avg_acc)],
                    "gs" = data$Groupsize[complete.cases(data$avg_acc)],
                    "loom" = data$Loom[complete.cases(data$avg_acc)],
                    "acc" = data$avg_acc[complete.cases(data$avg_acc)]
)

model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) +loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#temp terms are  significant 


#median acc during loom

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$acc_percentile50)],
                    "gs" = data$Groupsize[complete.cases(data$acc_percentile50)],
                    "loom" = data$Loom[complete.cases(data$acc_percentile50)],
                    "acc" = data$acc_percentile50[complete.cases(data$acc_percentile50)]
)



model_lm <- lm(log(acc+1) ~ temp + I(temp^2) + log(gs,2) +loom,my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))



#collective behavior during unperturbed swimming
#annd
data <- read.csv(here("..","..","..","Documents","data","temp_collective","roi","all_params_wo_loom.csv"),header=TRUE,na.strings=c("[nan]"))

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$annd)],
                    "gs" = data$Groupsize[complete.cases(data$annd)],
                    "annd" = data$annd[complete.cases(data$annd)]
)


hist(my_data$annd)
hist(log(my_data$annd+1))
hist(log(my_data$annd))
hist(sqrt(my_data$annd))

model_lm <- lm(annd ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))

require(MASS)
boxcox(model_lm, lambda = seq(0, 1, 1/10), plotit = TRUE) #0


model_lm <- lm(log(annd) ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#temp and temp^2 not significant

model_lm_int <- lm(log(annd) ~ temp + I(temp^2) + log(gs,2)+log(gs,2)*temp,my_data)
summary(model_lm_int)
plot(fitted(model_lm_int), residuals(model_lm_int))
#temp and temp^2 not significant
rsq(model_lm_int)
lrtest(model_lm_int,model_lm)

#convex hull

data <- read.csv(here("..","..","..","Documents","data","temp_collective","roi","all_params_wo_loom.csv"),header=TRUE,na.strings=c("[nan]"))

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$convex_hull_area)],
                    "gs" = data$Groupsize[complete.cases(data$convex_hull_area)],
                    "hull" = data$convex_hull_area[complete.cases(data$convex_hull_area)]
)

hist(my_data$hull)
hist(sqrt(my_data$hull))

model_lm <- lm(hull~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#temp and temp^2 not significant

require(MASS)
boxcox(model_lm, lambda = seq(0, 1, 1/10), plotit = TRUE) #0.5

model_lm <- lm(sqrt(hull) ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#temp and temp^2 not significant
rsq(model_lm)

model_lm_int <- lm(sqrt(hull) ~ temp + I(temp^2) + log(gs,2) +log(gs,2)*temp,my_data)
summary(model_lm_int)
plot(fitted(model_lm_int), residuals(model_lm_int))
#temp and temp^2 not significant


lrtest(model_lm,model_lm_int) #not significant

#pol before loom
data <- read.csv(here("..","..","..","Documents","data","temp_collective","roi","all_params_wo_loom_pol.csv"),header=TRUE,na.strings=c("[nan]"))

# make new dataframe

my_data<-data.frame("temp" = data$Temperature[complete.cases(data$polarization_1)],
                    "gs" = data$Groupsize[complete.cases(data$polarization_1)],
                    "pol" = data$polarization_1[complete.cases(data$polarization_1)]
)

model_lm <- lm(sqrt(abs(pol)) ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#temp and temp^2 not significant


model_lm <- lm(pol ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#temp and temp^2 not significant


model_lm <- lm(abs(pol) ~ temp + I(temp^2) + log(gs,2),my_data)
summary(model_lm)
plot(fitted(model_lm), residuals(model_lm))
#temp and temp^2 not significant
rsq(model_lm)


model_lm_int <- lm(abs(pol) ~ temp + I(temp^2) + log(gs,2) +log(gs,2)*temp,my_data)
summary(model_lm_int)
plot(fitted(model_lm_int), residuals(model_lm_int))
#temp and temp^2 not significant

lrtest(model_lm,model_lm_int) #not significant

