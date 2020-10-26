#1a

turtle_data <- data.frame(HW4turtle_data)
n <- length(turtle_data$turtle.number)
model1 <- lm(Wt.g ~ CL.mm + PL.mm + IAB.mm + as.factor(Sex),turtle_data)
summary(model1)


require(lessR)
CL <- turtle_data$CL.mm
PL <- turtle_data$PL.mm
IAB <- turtle_data$IAB.mm
Correlation(CL, PL)
Correlation(CL, IAB)
Correlation(IAB, PL)

#1b

model2 <- lm(Wt.g ~ scale(CL)  + scale(IAB) + as.factor(Sex),turtle_data)
summary(model2)
xtable(summary(model2))


#1c Is there evidence that the residuals from this model are not normally distributed? 
# Provide two lines of evidence to support your conclusion


#produce a qqplot 
qqnorm(residuals(model2))
qqline(residuals(model2))

#Shapiro-Wilk test 
shapiro.test(residuals(model2))





#1d Is there evidence that the residuals from this model are not identically distributed?
#Provide two lines of evidence to support your conclusion.

plot(fitted(model2),residuals(model2))
abline(h = 0)
plot(fitted(model2), sqrt(abs(residuals(model2))))
summary(model.test <- lm(sqrt(abs(residuals(model2))) ~ fitted(model2)) )
plot.default(scale(turtle_data$CL.mm),residuals(model2),type="p")
plot.default(scale(turtle_data$IAB.mm),residuals(model2),type="p")
plot.default(as.factor(turtle_data$Sex),residuals(model2),type="p")


#1e Are there any data points with particularly large leverages? Provide two lines of
#evidence to support your conclusion.

h <- hatvalues(model2)
two_p_over_n <- 2*mean(h)
turtle_data$turtle.number[which(h>two_p_over_n)]

require(faraway)
num <- turtle_data$turtle.number
halfnorm(h,nlab = 5, labs= num, ylab="Leverages")


#1f Which turtle (identified by turtle number) has the largest leverage? Which predictor
#makes this turtle notably extreme in predictor space? 
# (Hint: look at the scaled #predictors and recall that we expect >99% of a normal distribution to occur within ~3
# SD of the mean).

turtle_data_scaled <- data.frame(cbind(turtle_data$turtle.number,scale(CL),scale(PL),scale(IAB),turtle_data$Wt.g,turtle_data$Sex))
turtle_data_scaled[which.max(h),]


#1g Are there any data points with particularly large influence? Provide two lines of
#evidence to support your conclusion. Which turtle (identified by turtle number) has
#the largest influence?

studentized <- rstudent(model2)
(max.student <- studentized[which.max(abs(studentized))]) 
turtle_data[which(abs(studentized)>t_crit),]
t_crit <- qt(0.025/n,n-4-1,lower.tail = FALSE) 

#1h
beta1 <- rep(0,n)
beta2 <- rep(0,n)
for(i in 1:n){
  if(turtle_data$Sex[i] == "M"){
    beta1[i] <- 1
  } else{
    beta2[i] <- 1
  }
}
scaleCL <- scale(CL)
beta4 <- scaleCL*beta1
beta5 <- scaleCL*beta2
wt <- turtle_data$Wt.g
designmat_data <- data.frame(cbind(num,beta1,beta2,beta4,beta5,wt))
model3 <- lm(wt ~ -1 + beta1 + beta2 + V4 + V5, designmat_data)
summary(model3)
#1i

plot(scaleCL,wt)
abline(model3$coefficients[1],model3$coefficients[3],col="blue")
abline(model3$coefficients[2],model3$coefficients[4],col="green")
text(1.5,340,"females")
text(-2.5,280,"males")


#1j

nb <- 1000
coefmat <- matrix(NA,nb,4)
resids <- residuals (model3)
preds <- fitted (model3)  
for(j in 1:nb){
  boot <- preds + sample(resids, rep = T)
  bmod <- update(model3, boot ~ .)
  coefmat [j, ] <- coef(bmod)
}
colnames(coefmat) <- c("Intercept Males","Intercept Females","Effect of CL on Males","Effect of CL on Females")
coefmat <- data.frame(coefmat)
apply(coefmat,2,function(x) quantile(x,probs = c(0.025,0.975)))


#1k

scaleIAB <- scale(IAB)
designmat_hyp2 <- data.frame(cbind(beta1,beta2,scaleIAB,wt))
model4 <- lm(wt ~ -1 + beta1 + beta2 + V3, designmat_hyp2)
summary(model4)

plot(scaleIAB,wt)
abline(model4$coefficients[1],model4$coefficients[3],col="blue")
abline(model4$coefficients[2],model4$coefficients[3],col="green")
