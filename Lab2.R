#Generate Kery's wallcreeper data 
set.seed(123)
n <- 16 #number of years
a <- 40 #intercept
b <- -1.5 #slope
sigma2 <- 25 #residual variance 
years <- 1:16  #years
eps <- rnorm(n,mean=0,sd=sqrt(sigma2))
prcnt <- a + b*years + eps
junk <- rnorm(16,3,2) #also generate a junk predictor variable 
KW <- as.data.frame(cbind(prcnt,years,junk)) 

#plot it 
plot(KW$years,KW$prcnt,xlab = "Year",las=1,ylab="% occupied",xaxt='n',pch=19)
years <- c(1990:2005)
axis(side = 1, lab=years,at=c(1:16))

###################################################
#                                                 #  
#         SOLVING FOR BETA-HAT AND Y-HAT          #
#                                                 #
################################################### 

#this is our design matrix 
d.mat <- cbind(rep(1,1),KW$years)

#solve for b-hat the hard way 
#note that this is not ideal way to solve this
#can turn out poorly with messier data
(inv.xtx <- solve(t(d.mat)%*%d.mat))
(beta.hat <- inv.xtx%*%t(d.mat)%*%KW$prcnt)
(y.hat <- d.mat%*%beta.hat)

#and let's just get the hat matrix 
#hat matrix is n*n dimensional 
hat.matrix <- d.mat%*%inv.xtx%*%t(d.mat)
#we can also use our hat matrix to get y.hat (and some other stuff later)
(y.hat <- hat.matrix%*%KW$prcnt)

#plot the regression line and y.hat
abline(a = beta.hat[1], b = beta.hat[2],col="red")
points(y.hat,col="red",pch=19)

#we can also do this the easy way
(summary(mod <- lm(prcnt ~ years,data = KW)))

#and plot it again - it will be the same 
plot(KW$years,KW$prcnt,xlab = "Year",las=1,ylab="% occupied",xaxt='n',pch=19)
years <- c(1990:2005)
axis(side = 1, lab=years,at=c(1:16))
abline(mod,col="red")
points(y.hat,col="red",pch=19)

#and a third way to solve this   
#this is useful to know later when you want to customize a DM
(summary(mod.v2 <- lm(KW$prcnt ~ -1 + d.mat)))

#What are the residuals? 
(resid <- KW$prcnt-y.hat)  
#or 
(resid <- KW$prcnt-predict(mod,type="response"))
#or 
resid <- residuals(mod)

#plot the residuals - are there any patterns?  
plot(x = KW$years, y = resid,xlab ="Year", ylab = "Residuals")

###################################################
#                                                 #  
#                 TESTS                           #
#                                                 #
################################################### 

#get Total, Residual, and Model SS
(TotSS <- t(KW$prcnt - mean(KW$prcnt))%*%(KW$prcnt-mean(KW$prcnt)) )
(ResSS <- t(KW$prcnt-y.hat) %*% (KW$prcnt-y.hat))
#note that we can also get ResSS with our hat matrix 
#and note that diag(16) is a 16*16 identity matrix
(ResSS <- t(KW$prcnt)%*%(diag(16) - hat.matrix) %*% KW$prcnt)
(ModSS <- TotSS - ResSS)

#Mean SS
(M.ModSS <- ModSS/1) #df = ncol(X)-1 
(M.ResSS <- ResSS/14) #df = df(total)-df(model)
(M.TotSS <- TotSS/15) #df = n-1 

#and finally our F statistic and associated p-value
(f.stat <- (TotSS-ResSS)/(15-14)/(ResSS/14) )
pf(f.stat,15-14,14,lower.tail = F)

#Here's an example to demonstrate that RSS will always decline with increasing model complexity
#even if not by much 
mod <- lm(prcnt ~ years, data = KW)
(RSS.mod <- anova(mod)$'Sum Sq' [2])

#this is a "junk" model  because I added a random variable to it
mod.junk <- lm(prcnt ~ years + junk, data = KW)  
(RSS.junk.mod <- anova(mod.junk)$'Sum Sq' [3]) 

#the RSS decreases by about 7.5% in this realization 
(RSS.mod - RSS.junk.mod)/RSS.mod

#how do we get the sampling (residual) standard error from the RSS? 
# we an also see this by looking at summary(mod)
(sigma <- sqrt( anova(mod)$'Sum Sq'[2]/(n-2) ) )

#F-test 
#here we are testing against TotSS (i.e., testing all predictors)
#the F is always positively valued
#this is for model (mod)
( F.stat <- ( (TotSS - ResSS)/(2-1) )/ ( ResSS / (16-2)) )
pf(F.stat,2-1,16-2,lower.tail = F)
#this is the same as we get from summary(mod)
summary(mod)

#here we are testing just one predictor 
anova(mod,mod.junk) #null hypothesis is that beta(junk) = 0 | b0, b1 in model 
#note that this is equivalent to p we would get from t-test 
summary(mod.junk)

#Here is a model with just the mean (intercept)
mod.mean <- lm(prcnt ~ 1, data = KW) 

#so we can test a pair of predictors  
#the p-value tells us to retain the pair of predictors
#though one of them is "junk" 
#because the other one is useful 
anova(mod.junk,mod.mean)

#and we can test a model subspace, such as H0: b(x1) = b(x2)
mod.equal <- lm(prcnt ~ I(years + junk), data = KW)  
anova(mod.equal,mod.junk)


###################################################
#                                                 #  
#                 PERMUTATION TESTS               #
#                                                 #
################################################### 

#we can test the entire model, i.e., H0: b1 = b2 = 0
nreps <- 100000
fstats <- numeric(nreps)
for(i in 1:nreps){
  lmods <- lm(sample(prcnt) ~ years + junk, data = KW)
  # lmods <- update(mod.junk, sample(prcnt) ~ .) #this is equivalent
  fstats[i] <- summary(lmods)$fstat[1]
}
mean(fstats > summary(mod.junk)$fstat[1])

#we can test one predictor, e.g., H0: b2 = 0 | b1 in model
nreps <- 100000
tstats <- numeric(nreps)
for(i in 1:nreps){
  lmods <- lm(prcnt ~ years + sample(junk), data = KW)
  tstats[i] <- summary(lmods)$coefficients[3,3]
}
mean(abs(tstats) > abs(summary(mod.junk)$coefficients[3,3]) ) 


###################################################
#                                                 #  
#         CONFIDENCE INTERVALS on Beta            #
#                                                 #
################################################### 

summary(mod.junk)

( crit.t <- qt(0.975,16-3) )
summary(mod.junk)$coefficients [,1] - summary(mod.junk)$coefficients[,2] * crit.t
summary(mod.junk)$coefficients [,1] + summary(mod.junk)$coefficients[,2] * crit.t

#or, for a shortcut 
confint(mod.junk)

#relationship to t-value 
( p.value <- summary( mod.junk)$coefficients[3,4] )
confint(mod.junk, level = 1-p.value)[3,]

#Bootstrap CIs
nb <- 1000
coefmat <- matrix(NA,nb,3)
resids <- residuals (mod.junk)
#Note the following is equivalent to below: preds <- model.matrix(mod.junk)%*%coef(mod.junk)
preds <- fitted (mod.junk)  
for(i in 1:nb){
  boot <- preds + sample(resids, rep = T)
  bmod <- update(mod.junk, boot ~ .)
  coefmat [i, ] <- coef(bmod)
}
colnames(coefmat) <- c("Intercept","Years","Junk")
coefmat <- data.frame(coefmat)
apply(coefmat,2,function(x) quantile(x,probs = c(0.025,0.975)))
#above should look very similar to confint(mod.junk)

###################################################
#                                                 #  
#         CONFIDENCE INTERVALS on y-hat           #
#                                                 #
################################################### 

#CI
summary( mod.junk <- lm(prcnt ~ years, data = KW) )

x <- model.matrix(mod)
# get the median year
(x0 <- apply(x,2,median)) 
#what is the expected number of species in the median year? 
(y0 <- sum(x0*coef(mod)))

#we can also use this command
predict(mod,new = data.frame (t(x0)))

#this command to get a prediction interval
predict(mod, new = data.frame (t(x0)), interval = "prediction") 
#to get this from the equation
tcrit <- qt(0.975, df = (16-2))
sigma <- summary(mod)$sigma
y0 - tcrit*sigma*sqrt(1 + t(x0) %*% inv.xtx %*% x0)
y0 + tcrit*sigma*sqrt(1 + t(x0) %*% inv.xtx %*% x0)

#this command to get a confidence interval on the prediction 
predict(mod, new = data.frame (t(x0)), interval = "confidence")
#and from equation
y0 - tcrit*sigma*sqrt(t(x0) %*% inv.xtx %*% x0)
y0 + tcrit*sigma*sqrt(t(x0) %*% inv.xtx %*% x0)

###################################################
#                                                 #  
#         What is the SE on beta-hat?             #
#                                                 #
################################################### 

#generate 1000 realizations of a data set
reps <- 1000
store <- matrix(NA,nrow=2,ncol=reps)
rownames(store) <- c("beta","se.beta")
for(i in 1:reps){
n <- 16 #number of years
a <- 40 #intercept
b <- -1.5 #slope
sigma2 <- 25 #residual variance 
years <- 1:16  #years
eps <- rnorm(n,mean=0,sd=sqrt(sigma2))
prcnt <- a + b*years + eps
KW <- as.data.frame(cbind(prcnt,years))
#fit a model and store the estimate and the se of the coefficient 
mod <- lm(prcnt ~ years, KW)
store[1,i] <- summary(mod)$coefficients[2,1]
store[2,i] <- summary(mod)$coefficients[2,2]
} 
#the standard deviation of the estimate is the mean estimate of the standard error
#the standard error is a measure of how variable the beta-hat would be from multiple realizations of the dataset
sd(store[1,])
mean(store[2,])



