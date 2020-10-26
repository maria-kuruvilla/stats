# 1. You have a dataset on 1-year survival of white-tailed deer fawns in a long-term study with
# multi-generational radio-collar data. Thus, the reproductive histories of the does who
# produced the fawns are known. The animals were located in 3 different study areas. You can
# find the data in HW8_fawn.csv. The data include area (1, 2, or 3), the number of fawns the
# doe previously produced (0-5), and whether the fawn survived the year (1 = yes, 0 = no).



# a. Build a model to estimate fawn survival as a function of area and doe reproductive
# history. Report the summary table.
model1 <- glm(outcome ~ as.factor(area) + female.prev,HW8_fawn,family = "binomial")
(summary(model1))
require(xtable)
xtable(summary(model1))

# b. Evaluate model fit through a binned residual plot and a Hosmer-Lemeshow test.
library(arm)
binnedplot(fitted(model1),residuals(model1)) 

#hosmer lemeshow - sample size is too small here

library(generalhoslem)
logitgof(HW8_fawn$outcome,fitted(model1))


# c. Make and present halfnormal plots of hat values, studentized residuals, and Cook's
# statistics. Identify the points that have the highest influence and examine the
# predictors and response for these points. Why does it appear that these points have
# high influence? Is there any justification for removing these points?

library(faraway)

h <- hatvalues(model1)

halfnorm(h)

two.p.n <- 2*sum(h)/length(h)

abline(a=two.p.n,b=0)

studentized <- rstudent(model1)
halfnorm(studentized)

max(rstudent(model1))
n <- nrow(HW8_fawn)
p <- sum(h)
qt(0.025/n,n-p-1,lower.tail = FALSE)
HW8_fawn[which.max(studentized),]
newData1 <- data.frame(area=3, female.prev=1)
predicted.outcome <- predict(model1,newdata = newData1, type="response")

halfnorm(cooks <- cooks.distance(model1))



#   d. Make a plot of predictions and confidence intervals for female.prev = c(0,1,2,3,4,5) at
# each of the 3 sites. Use bootstrapping to get the confidence intervals. Show your
# code.

unique <- length(unique(HW8_fawn$female.prev))
newData2 <- data.frame(area = c(rep("1",unique),rep("2",unique),rep("3",unique)),female.prev = rep(sort(unique(HW8_fawn$female.prev)),3) )

#simulate data with best data and update model with new data set as response and predict a new response under this model
nrows <- nrow(newData2)
summ.best <- matrix(NA,nrow = nrows, ncol=ncol(newData2) +1)
boots <- 10000
yest <- matrix(NA,nrow=nrows,ncol=boots)
for(i in 1:boots){
  y <- unlist(simulate(model1))
  ymod <- update(model1,y ~ .) #keep everything the same except the y in update
  yest[,i] <- predict(ymod,newdata = newData2, type="response")
  
}
for(i in 1:nrows){
  summ.best[i,1] <- mean(yest[i,]) 
  summ.best[i,2] <- quantile(yest[i,],probs=0.025) 
  summ.best[i,3] <- quantile(yest[i,],probs=0.975) 
}

female.prev <- sort(unique(HW8_fawn$female.prev))

#par(mai = c(1,1,0.5,0.5))
plot(female.prev, summ.best[1:unique,1],type = "l",ylab = "Survival",xlab = "Number of previous fawns",ylim=c(0,1))
lines(female.prev, summ.best[1:unique,2],lty= 2)
lines(female.prev, summ.best[1:unique,3],lty= 2)

lines(female.prev, summ.best[unique+1 :unique,1],lty= 1,col="red")
lines(female.prev, summ.best[unique+1 :unique,2],lty= 2,col="red")
lines(female.prev, summ.best[unique+1 :unique,3],lty= 2,col="red")

lines(female.prev, summ.best[(2*unique)+1 :unique,1],lty= 1,col="green")
lines(female.prev, summ.best[(2*unique)+1 :unique,2],lty= 2,col="green")
lines(female.prev, summ.best[(2*unique)+1 :unique,3],lty= 2,col="green")

text(x=1,y=0.39,"area 2")
text(x=2,y=0.22,"area 1")
text(x=4,y=0.08,"area 3")




# e. Summarize the data by the predictors and create a dataset you can use for a binomial
# analysis with n > 1. Run the model. Report the summary table from the analysis.

uniquearea <- length(unique(HW8_fawn$area))
count <- 0
for(i in 1:uniquearea){
  for(j in 1:unique){
    y[count] <- y[count] + 1
  }
}

model2 <- glm(outcome(y,n-y) ~ as.factor(area) + female.prev,HW8_fawn,family = "binomial")






# f. Evaluate model fit based on the deviance. Show your code. Provide a p-value to
# support your result. 




