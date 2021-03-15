#QERM 514 - University of Washington
#Module 13 - Modeling Count Data 
#See the calculation of the log likelihood of the saturated model for several GLMs 
#Code updated 20 May 2019 
#Code written by Sarah Converse, sconver@uw.edu

#poisson data 
l5 <- rpois(1000000,lambda = 5)
l10 <- rpois(1000000, lambda = 10)
l100 <- rpois(1000000, lambda = 100)
hist(l5, xlab = "Count", main = "lambda = 5")
hist(l10, xlab = "Count", main = "lambda = 10")
hist(l100, xlab = "Count", main = "lambda = 100")

#generate a dataset
#effect of bait type (chicken) and temperature on blue crab captures
set.seed(4)
n <- 382
b0 = 3
b.chkn <- -0.1
b.temp <- 0.013

chkn <- sample(c(0,1),n,replace = TRUE)
temp <- runif(n,12,20)
eta <- exp(b0 + b.chkn*chkn + b.temp*temp)
crabs <- rep(NA,length(eta)) 
for(i in 1:length(crabs)){crabs[i] <- rpois(1,eta[i])}
Ches <- data.frame(cbind(crabs,chkn,temp))

#run a model 
pmod <- glm(crabs ~ chkn + temp, family = "poisson", Ches)
summary(pmod)

pmod <- glm(crabs ~ chkn + temp, family = "gaussian", Ches)

confint(pmod)

#get the deviance 
#get the logLikelihood of the saturated model 
Ches$fact <- seq(from = 1, to = n, by = 1)
saturated.pois <- glm(crabs ~ as.factor(fact), family = "poisson", Ches)
logLik(saturated.pois)
#get the logLikelihood of the saturated model analytically (via the log likelihood function)
I <- which(Ches$crabs>0)
eta <- predict(saturated.pois,type = "link")
(logLik.sat <- sum(Ches$crabs[I] * eta[I]  -  exp(eta[I])  - log(factorial(Ches$crabs[I]))) )

#deviance GoF 
2*(logLik.sat - logLik(pmod))
deviance(pmod)
pchisq(deviance(pmod),n-3, lower.tail = F)

#Pearson's GoF
(pearsons <- sum(((Ches$crabs - fitted(pmod) )^2)/(fitted(pmod))) )
pchisq(pearsons,n-3, lower.tail = F)

#Diagnostics - Influential Points
sort(hatvalues(pmod) )
max(hatvalues(pmod))
library(faraway)
halfnorm(hatvalues(pmod))
(2*3)/n
Ches$hat <- hatvalues(pmod)
Ches[which(Ches$hat > (2*3)/n),]

halfnorm(cooks <- cooks.distance(pmod))
high.cooks <- which(cooks > (mean(cooks) + 4*sd(cooks)))
Ches[high.cooks,]

#residual plots 
plot(fitted(pmod),residuals(pmod))
plot(pmod,1)

#confidence intervals on predictions 
#get bootstrapped and t-distributed confidence intervals 
#plot together
newData1 <- data.frame(expand.grid(temp = seq(from = 12, to = 20, by = 0.25), chkn = c(0,1)))
boots <- 10000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  ynew <- unlist(simulate(pmod))
  ymod <- update(pmod,ynew ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response")
}
results <- matrix(NA,nrow=nrow(newData1),ncol=3)
results[,1] <- predict(pmod,newData1, type = "response")
for(j in 1:nrow(newData1)){
  results[j,2] <- quantile(yest[j,],probs = c(0.025))
  results[j,3] <- quantile(yest[j,],probs = c(0.975))
}  
chknyes <- which(newData1$chkn == 1)  
chknno <- which(newData1$chkn == 0)
temp <- newData1$temp[chknyes]
plot(temp,results[chknyes,1], type = 'l', col = "green", ylim = c(20,30), xlab = "Temperature",ylab = "Expected Catch/Trap Line")
lines(temp,results[chknyes,2], lty = "dashed", col = "green")
lines(temp,results[chknyes,3], lty = "dashed", col = "green")
lines(temp,results[chknno,1], lty = "solid", col = "red")
lines(temp,results[chknno,2], lty = "dashed", col = "red")
lines(temp,results[chknno,3], lty = "dashed", col = "red")

preds <- predict(pmod,newData1,se.fit = TRUE, type = "response")
crit.t <- qt(0.025,n-3,lower.tail = FALSE)
lower <- preds$fit - crit.t*preds$se.fit
upper <- preds$fit + crit.t*preds$se.fit 
lines(temp,results[chknyes,1], type = 'l', col = "black")
lines(temp,lower[chknyes], lty = "dashed", col = "black")
lines(temp,upper[chknyes], lty = "dashed", col = "black")
lines(temp,results[chknno,1], lty = "solid", col = "black")
lines(temp,lower[chknno], lty = "dashed", col = "black")
lines(temp,upper[chknno], lty = "dashed", col = "black")

#plot over-dispersed Poisson 
pois <- rpois(n = 10000000,lambda = 20)
plot(density(pois, bw = 1), xlab = "",main = "")
negbin <- rnbinom(n=10000000, mu=20,size = 5)
lines(density(negbin, bw = 1))

