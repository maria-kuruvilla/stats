#Generate Kery's wallcreeper data 
set.seed(101)
n <- 30 #number of years
a <- 40 #intercept
b <- -0.9 #slope
sigma2 <- 20 #residual variance 
years <- 1:n  #years
eps <- rnorm(n,mean=0,sd=sqrt(sigma2))
prcnt <- a + b*years + eps
KW <- as.data.frame(cbind(prcnt,years)) 

#Model 
lmod <- lm(prcnt ~ years, KW)

#Evaluate Error Assumptions########################################################

#plot error against fitted
plot(fitted(lmod),residuals(lmod))
abline(h = 0)

#plot sqrt(abs(error)) against fitted
plot(fitted(lmod), sqrt(abs(residuals(lmod))))

#regress sqrt(abs(error)) on fitted
summary(lmod.test <- lm(sqrt(abs(residuals(lmod))) ~ fitted(lmod)) )

#plot fitted against x 
plot(KW$years,residuals(lmod))

#produce a qqplot 
qqnorm(residuals(lmod))
qqline(residuals(lmod))

qqnorm(rstandard(lmod))
qqline(rstandard(lmod))

#Shapiro-Wilk test 
shapiro.test(residuals(lmod))

#DW test for correlated residuals 
require(lmtest)
dwtest(lmod)

#Plot error(t+1) against error(t)
plot(head(residuals(lmod),n-1),tail(residuals(lmod),n-1),xlab = expression(epsilon(t)),ylab = expression(epsilon(t+1)))

#Unusual Observations########################################################

#look for any large leverage points 
hatv <- hatvalues(lmod)
KW[order(hatv)[28:30],]
sort(hatv)
(two_p_over_n <- 2*2/n )

#half normal plot of leverages
require(faraway)
yrs <- KW$years
halfnorm(hatv,nlab = 3, labs= yrs, ylab="Leverages")

#get studentized residuals 
studentized <- rstudent(lmod)
(max.student <- studentized[which.max(abs(studentized))] )
#this is the quantile that your studentized residual must exceed to be significant at 0.05 (note 2-sided test) 
qt(0.025/n,n-2-1,lower.tail = FALSE)
#you can also get this here  
library(car)
outlierTest(lmod)

#calculate the cooks.distance for any potentially influential points 
cook <- cooks.distance(lmod)
halfnorm(cook,3,labs=row.names(KW),ylab="Cook's distances")
#exclude point with largest Cook's distance - does it change model fit 
lmod2 <- lm(prcnt ~ years, KW, subset = (years != 20))
#plot all - the point with largest Cook's distance didn't make much difference to fit
plot(KW$years, KW$prcnt)
abline(lmod)
abline(lmod2, lty = 2)

#Box-Cox Transformations########################################################

require(MASS)
bc <- boxcox(lmod,plotit=TRUE,lambda = seq(-2,3,by = 0.1))
bc$x[which.max(bc$y)]

#this looks good but we will fit a square root transformation to see how it works 
summary( lmod.trans <- lm(sqrt(prcnt) ~ years, KW) )

#plot error against fitted
plot(fitted(lmod.trans),residuals(lmod.trans))
abline(h = 0)

#predict and plot 
plot(sqrt(prcnt) ~ years, cex.lab = 1.5, cex.axis = 1.5, ylab = expression(sqrt(Prcnt)), xlab = "Years")
abline(lmod.trans)

out <- predict(lmod.trans, newdata = data.frame(x = years), interval="prediction")

plot(prcnt ~ years, cex.lab = 1.5, cex.axis = 1.5, ylab = "Prcnt", xlab = "Years")
lines(years,out[,1]^2,col="red")
lines(years,out[,2]^2,lty = 2,col="red")
lines(years,out[,3]^2,lty = 2,col="red")