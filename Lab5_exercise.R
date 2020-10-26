num <- Lab5_cater_data$X1
exposure <- Lab5_cater_data$exp
temp <- Lab5_cater_data$temp
growth <- Lab5_cater_data$growth
cater_data <- as.data.frame(cbind(num,exposure,temp,growth))

n <- length(Lab5_cater_data$X1)


#Leave one out cross validation

model1 <- lm(growth ~ 1,cater_data,x=TRUE,y=TRUE)
model2 <- lm(growth ~ temp, cater_data,x=TRUE,y=TRUE)
model3 <- lm(growth ~ exposure, cater_data,x=TRUE,y=TRUE)
model4 <- lm(growth ~ temp*exposure, cater_data,x=TRUE,y=TRUE)

require(boot)

loo1 <- cv.glm(data = cater_data, model1, K=n)$delta[1]
loo2 <- cv.glm(data = cater_data, model2, K=n)$delta[1]
loo3 <- cv.glm(data = cater_data, model3, K=n)$delta[1]
loo4 <- cv.glm(data = cater_data, model4, K=n)$delta[1]


#K-fold validation 

k=10

kfold1 <- cv.glm(data = cater_data, model1, K=k)$delta[1]
kfold2 <- cv.glm(data = cater_data, model2, K=k)$delta[1]
kfold3 <- cv.glm(data = cater_data, model3, K=k)$delta[1]
kfold4 <- cv.glm(data = cater_data, model4, K=k)$delta[1]

#AIC

n <- nrow(cater_data)
p1 <- nrow(summary(model1)$coefficients)
p2 <- nrow(summary(model2)$coefficients)
p3 <- nrow(summary(model3)$coefficients)
p4 <- nrow(summary(model4)$coefficients)
RSS1 <- anova(model1)$'Sum Sq'[ nrow(anova(model1))]
RSS2 <- anova(model2)$'Sum Sq'[ nrow(anova(model2))]
RSS3 <- anova(model3)$'Sum Sq'[ nrow(anova(model3))]
RSS4 <- anova(model4)$'Sum Sq'[ nrow(anova(model4))]

aic1 <- n*log(RSS1/n) + p1*2 
aic2 <- n*log(RSS2/n) + p2*2
aic3 <- n*log(RSS3/n) + p3*2
aic4 <- n*log(RSS4/n) + p4*2

#AICC

aicc1 <- extractAIC(model1)[2] + ((2*p1*(p1+1)) /(n - p1 - 1)) 
aicc2 <- extractAIC(model2)[2] + ((2*p2*(p2+1)) /(n - p2 - 1)) 
aicc3 <- extractAIC(model3)[2] + ((2*p3*(p3+1)) /(n - p3 - 1)) 
aicc4 <- extractAIC(model4)[2] + ((2*p4*(p4+1)) /(n - p4 - 1)) 



#BIC

bic1 <- n*log(RSS1/n) + p1*log(n) 
bic2 <- n*log(RSS2/n) + p2*log(n) 
bic3 <- n*log(RSS3/n) + p3*log(n) 
bic4 <- n*log(RSS4/n) + p4*log(n) 

plot(x= temp,y = growth)
plot(x=exposure, y= growth)



