#QERM 514 - University of Washington
#Module 12 - Modeling Binary Data 
#See the calculation of the log likelihood of the saturated model for several GLMs 
#Code updated 20 May 2019 
#Code written by Sarah Converse, sconver@uw.edu

#scarification effects on blowout penstemon germination
set.seed(2)
n <- 80
b0 <- -3.5
b1 <- 1.5
acid.levs <- c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
acid <- sample(acid.levs,80,replace = TRUE)
eta <- 1/(1+exp(-(b0 + b1*acid)))

#generate response data and put in data frame
germ <- numeric(n)
for(i in 1:n){
germ[i] <- rbinom(1,1,eta[i])
}
data.pen <- data.frame(cbind(germ,acid))

#run the model 
lmod <- glm(germ ~ acid, data.pen, family = binomial)
summary(lmod)

#run the null model
lmod.null <- glm(germ ~ 1, data.pen, family = binomial)
summary(lmod.null)

#deviance 
p <- predict(lmod,type = "response")
dev <- -2*sum(p*logit(p) + (log(1-p)))
deviance(lmod)

#can get this also from -2*log likelihood 
eta <- predict(lmod)
y <- data.pen$germ
-2*sum(y*eta -log(1+exp(eta)))

#predict 
newdata <- data.frame(acid <- seq(from = 0, to = 5, by = 0.2))
preds <- 1/(1 + exp(-predict(lmod,newdata)))

#plot 
library(scales)
plot(newdata$acid,preds, type = 'l', xlab = "Acid", ylab = "Germination", ylim = c(0,1))
points(x = data.pen$acid, y = data.pen$germ, pch = 20, cex = 1.5, col = alpha("black", 0.2))

mean <- 1/(1+exp(-(coef(lmod.null))))
library(scales)
plot(newdata$acid, rep(mean, nrow(newdata)), type = 'l', xlab = "Acid", ylab = "Germination",ylim = c(0,1))
points(x = data.pen$acid, y = data.pen$germ, pch = 20, cex = 1.5, col = alpha("black", 0.2))

#confidence intervals
library(MASS)
confint(lmod)

#DIAGNOSTICS 

#raw residuals
rawres <- data.pen$germ - predict(lmod,type = "response")
#same as 
rawres <- residuals(lmod,type = "response")

#plot the raw residuals against the linear predictor on logit or real scale
plot(predict(lmod),rawres)
plot(predict(lmod, type = "response"), rawres)

#deviance residuals
residuals(lmod)
plot(predict(lmod),residuals(lmod))

#calculation of deviance residuals by hand 
p <- predict(lmod, type = "response")
y <- data.pen$germ
sign.y <- ifelse(y==1,1,-1)
dev.resid <- sign.y*sqrt(-2*(y*log(p) + ((1-y)*(log(1-p)))))

#binned residuals 
library(arm)
binnedplot(fitted(lmod),residuals(lmod)) 

#qqplot 
qqnorm(residuals(lmod))

#hatvalues
library(faraway)
halfnorm(hatvalues(lmod))

(twop.n <- 2*2/(dim(data.pen))[1] )
max(hatvalues(lmod))

#cooks 
cooks <- cooks.distance(lmod)
halfnorm(cooks)
mean(cooks) + 3*sd(cooks)
max(cooks)

#plot predictions against outcomes 
plot(y, fitted(lmod, type = "response"),xlim = c(-1,2), xaxt = "n", ylab = "Probability of Germination", xlab = "Germination")
axis(1, at = c(0,1), labels = c("Fail","Success"))

#hosmer lemeshow - sample size is too small here 
library(generalhoslem)
logitgof(data.pen$germ,fitted(lmod))

#H-L by hand - try different breaks 
library(dplyr)
data.pen <- mutate(data.pen, predprob = predict(lmod, type = "response"))
gdf <- group_by(data.pen, cut(eta, breaks = unique(quantile(eta,(1:15/16)))))
hldf <- summarise(gdf,y = sum(germ), ppred=mean(predprob), count = n())
hlstat <- with(hldf, sum((y-count*ppred)^2/(count*ppred*(1-ppred))))
c(hlstat,nrow(hldf))
1-pchisq(hlstat,nrow(hldf)-1)
#classification
p <- fitted(lmod,type = "response")
y <- data.pen$germ

cor.yes <- length(intersect(which(y == 1),which(p > 0.5)))
incor.yes <- length(intersect(which(y == 1),which(p < 0.5)))
cor.no <- length(intersect(which(y == 0),which(p < 0.5)))
incor.no <- length(intersect(which(y == 0),which(p > 0.5)))

#classification threshold
thresh <- seq(0.1,0.9,0.01)
sens <- spec <- numeric(length(thresh))
for(j in seq(along=thresh)){
  PP <- ifelse(p < thresh[j],"no","yes")
  xx <- xtabs(~y + PP, data.pen)
  spec[j] <- xx[1,1]/(xx[1,1] + xx[1,2])
  sens[j] <- xx[2,2]/(xx[2,1] + xx[2,2])
}

matplot(thresh,cbind(sens,spec),type = "l", xlab = "Threshold", ylab = "Proportion",lty = 1:2)

#prediction
nD <- data.frame(sort(data.pen$acid)); colnames(nD) <- c("acid")
preds <- predict(lmod,newdata = nD, type="link",se.fit=TRUE)

crit <- qt(0.025,80-2,lower.tail = FALSE)
lower <- preds$fit - crit*preds$se.fit
upper <- preds$fit + crit*preds$se.fit

conv.preds <- 1/(1+exp(-(preds$fit)))
conv.lower <- 1/(1+exp(-(lower)))
conv.upper <- 1/(1+exp(-(upper)))

plot(nD$acid,conv.preds,type='l')
lines(nD$acid,conv.lower,lty=2)
lines(nD$acid,conv.upper,lty=2)
