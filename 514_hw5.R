#Predation avoidance is a key behavior for animals. We have a dataset on the running speed of
#golden marmots (Marmota caudata aurea), collected in Pakistan's Khunjerab National Park.
#Running short to moderate distances to reach burrows is the major predation avoidance behavior
#employed by marmots. We want to model velocity (Vel; m/s) as a function of Sex (M or F),
#substrate (Sub; S = stones, D = dirt, V = vegetation), slope (Slpe; negative values indicate downslope run), 
#and marmot weight (Wt; g). Variable names given in bold identify the variables in
#the attached dataset, "HW5marmot_speed.csv."



#1. Begin by building a global model and assessing the model diagnostics.
#a. Are the model residuals NIID? Explain your reasoning and demonstrate with
#plots, null hypothesis tests, or whatever evidence is appropriate.



full_model <- lm(Vel ~ as.factor(Sex)+as.factor(Sub)+Slpe+Wt ,HW5marmot_speed, x= TRUE, y=TRUE)

plot(fitted(full_model),full_model$residuals)

plot(fitted(full_model),sqrt(abs(full_model$residuals)))

plot.default(HW5marmot_speed$Slpe,full_model$residuals)
plot.default(HW5marmot_speed$Wt,full_model$residuals)
plot.default(as.factor(HW5marmot_speed$Sex),full_model$residuals)
plot.default(as.factor(HW5marmot_speed$Sub),full_model$residuals)

qqnorm(residuals(full_model))
qqline(residuals(full_model))

shapiro.test(residuals(full_model))


#b. Are there any high leverage points or outliers that influence the model fit to a
#surprising degree? Explain your reasoning and demonstrate with plots, null
#hypothesis tests, or whatever evidence is appropriate.

h <- hatvalues(full_model)
two_p_over_n <- 2*mean(h)
HW5marmot_speed$ID[which(h>two_p_over_n)]

require(faraway)
num <- HW5marmot_speed$ID
halfnorm(h,nlab = 3, labs= num, ylab="Leverages")


studentized <- rstudent(full_model)
(max.student <- studentized[which.max(abs(studentized))]) 
n <- length(HW5marmot_speed$ID)
t_crit <- qt(0.025/n,n-6-1,lower.tail = FALSE) 
HW5marmot_speed[which(abs(studentized)>t_crit),]




#c. Are you comfortable proceeding with the analysis?





#2. Based on multiple past analyses, we know that weight has a substantial effect on running
#velocity. Therefore, weight will appear in all our candidate models. Build all possible
#models that include weight and any combinations of the other potential predictor
#variables, Sex, Sub, and Slpe (Hint: there will be 2^3 models).

model7 <- lm(Vel ~ as.factor(Sex)+as.factor(Sub)+Wt ,HW5marmot_speed, x= TRUE, y= TRUE)
model6 <- lm(Vel ~ as.factor(Sub)+Slpe+Wt ,HW5marmot_speed, x= TRUE, y= TRUE)
model5 <- lm(Vel ~ as.factor(Sex)+Slpe+Wt ,HW5marmot_speed, x= TRUE, y= TRUE)
model4 <- lm(Vel ~ as.factor(Sub)+Wt ,HW5marmot_speed, x= TRUE, y= TRUE)
model3 <- lm(Vel ~ as.factor(Sex)+Wt ,HW5marmot_speed, x= TRUE, y= TRUE)
model2 <- lm(Vel ~ Slpe+Wt ,HW5marmot_speed, x= TRUE, y= TRUE)
model1 <- lm(Vel ~ Wt ,HW5marmot_speed, x= TRUE, y= TRUE)

#2a
#Show, for your most complex model, the R code to calculate AIC, AICc, and BIC
#based on the residual sum of squares, n, and K for the model.

p_full_model <- nrow(summary(full_model)$coefficients)
RSS_full <- anova(full_model)$'Sum Sq'[ nrow(anova(full_model))]

#AIC
aic_full <- n*log(RSS_full/n) + p_full_model*2 

#AICC
aicc_full <- extractAIC(full_model)[2] + ((2*p_full_model*(p_full_model+1)) /(n - p_full_model - 1)) 

#BIC
bic_full <- n*log(RSS_full/n) + p_full_model*log(n) 



#b. Produce and report a table including the sigma hat squared, the number of parameters, the AIC,
#the AICc, and the BIC for each of your candidate models.
p <- RSS <- aic <- aicc <- bic <- loo <- rep(NA,8)

p[1] <- nrow(summary(model1)$coefficients)
p[2] <- nrow(summary(model2)$coefficients)
p[3] <- nrow(summary(model3)$coefficients)
p[4] <- nrow(summary(model4)$coefficients)
p[5] <- nrow(summary(model5)$coefficients)
p[6] <- nrow(summary(model6)$coefficients)
p[7] <- nrow(summary(model7)$coefficients)
p[8] <- p_full_model

RSS[1] <- anova(model1)$'Sum Sq'[ nrow(anova(model1))]
RSS[2] <- anova(model2)$'Sum Sq'[ nrow(anova(model2))]
RSS[3] <- anova(model3)$'Sum Sq'[ nrow(anova(model3))]
RSS[4] <- anova(model4)$'Sum Sq'[ nrow(anova(model4))]
RSS[5] <- anova(model5)$'Sum Sq'[ nrow(anova(model5))]
RSS[6] <- anova(model6)$'Sum Sq'[ nrow(anova(model6))]
RSS[7] <- anova(model7)$'Sum Sq'[ nrow(anova(model7))]
RSS[8] <- RSS_full

aic[1] <- n*log(RSS[1]/n) + p[1]*2 
aic[2] <- n*log(RSS[2]/n) + p[2]*2
aic[3] <- n*log(RSS[3]/n) + p[3]*2
aic[4] <- n*log(RSS[4]/n) + p[4]*2
aic[5] <- n*log(RSS[5]/n) + p[5]*2
aic[6] <- n*log(RSS[6]/n) + p[6]*2
aic[7] <- n*log(RSS[7]/n) + p[7]*2
aic[8] <- aic_full

#AICC

aicc[1] <- extractAIC(model1)[2] + ((2*p[1]*(p[1]+1)) /(n - p[1] - 1)) 
aicc[2] <- extractAIC(model2)[2] + ((2*p[2]*(p[2]+1)) /(n - p[2] - 1)) 
aicc[3] <- extractAIC(model3)[2] + ((2*p[3]*(p[3]+1)) /(n - p[3] - 1)) 
aicc[4] <- extractAIC(model4)[2] + ((2*p[4]*(p[4]+1)) /(n - p[4] - 1)) 
aicc[5] <- extractAIC(model5)[2] + ((2*p[5]*(p[5]+1)) /(n - p[5] - 1)) 
aicc[6] <- extractAIC(model6)[2] + ((2*p[6]*(p[6]+1)) /(n - p[6] - 1)) 
aicc[7] <- extractAIC(model7)[2] + ((2*p[7]*(p[7]+1)) /(n - p[7] - 1)) 
aicc[8] <- aicc_full


#BIC

bic[1] <- n*log(RSS[1]/n) + p[1]*log(n) 
bic[2] <- n*log(RSS[2]/n) + p[2]*log(n) 
bic[3] <- n*log(RSS[3]/n) + p[3]*log(n) 
bic[4] <- n*log(RSS[4]/n) + p[4]*log(n) 
bic[5] <- n*log(RSS[5]/n) + p[5]*log(n) 
bic[6] <- n*log(RSS[6]/n) + p[6]*log(n) 
bic[7] <- n*log(RSS[7]/n) + p[7]*log(n) 
bic[8] <- bic_full

table <- as.data.frame(cbind(RSS,p,aic,aicc,bic))

#c. Conduct leave-one-out cross validation for your model set. Add the key result
#(mean squared prediction error, MSPE) to the table from (b) and report your table
#with this added.

require(boot)

loo[1] <- cv.glm(data = HW5marmot_speed, model1, K=n)$delta[1]
loo[2] <- cv.glm(data = HW5marmot_speed, model2, K=n)$delta[1]
loo[3] <- cv.glm(data = HW5marmot_speed, model3, K=n)$delta[1]
loo[4] <- cv.glm(data = HW5marmot_speed, model4, K=n)$delta[1]
loo[5] <- cv.glm(data = HW5marmot_speed, model5, K=n)$delta[1]
loo[6] <- cv.glm(data = HW5marmot_speed, model6, K=n)$delta[1]
loo[7] <- cv.glm(data = HW5marmot_speed, model7, K=n)$delta[1]
loo[8] <- cv.glm(data = HW5marmot_speed, full_model, K=n)$delta[1]


#d. Summarize your model selection results based on AIC, AICC, BIC, and MSPE.
#What did you learn? Is there anything that is surprising? Anything inconsistent
#across methods that isn't particularly surprising? Why?





#e. Choose one model in your set that seems most appropriate for prediction. Justify
#your choice. Produce one statistic that tells us something about the overall
#predictive power of this model, i.e., how good a model is it?




#3 Use what you have concluded is your best predictive model to produce predicted
#velocities for your dataset. Plot these predictions against each of the predictors, one at a
#time (e.g., if you have 2 predictors in your top model, you'll produce two plots). Make
#sure to include prediction intervals in your plots.