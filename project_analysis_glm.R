require(faraway)
library(arm)


#model with proportion of prev fish of same species, species, temp ratio and flow ratio
model1 <- glm(IHA ~ prevproportion + as.factor(Species) + ratio + outflowratio, datanew, family = "binomial")
(summary(model1))
confint(model1)

#model with proportion of prev fishof same species , interaction between count and prevfish of same species, species, temp ratio and flow ratio
model2 <- glm(IHA ~ prevproportion*prevcount + as.factor(Species) + ratio + outflowratio, datanew, family = "binomial")
(summary(model2))
model.matrix(model2)

require(stargazer)
stargazer(model2)

#model with  proportion of prev fishof same species , interaction between count and prevfish of same species, species, temp ratio
model3 <- glm(IHA ~ prevproportion*prevcount + as.factor(Species) + ratio, datanew, family = "binomial")
(summary(model3))

#model  with  proportion of prev fishof same species , interaction between count and prevfish of same species, species
model4 <- glm(IHA ~ prevproportion*prevcount + as.factor(Species), datanew, family = "binomial")
(summary(model4))

#model  with  proportion of prev fishof same species , interaction between count and prevfish of same species
model5 <- glm(IHA ~ prevproportion*prevcount, datanew, family = "binomial")
(summary(model5))

#model  with  temp ratio and flow ratio
model6 <- glm(IHA ~ ratio + outflowratio, datanew, family = "binomial")
(summary(model6))

#model with proportion of prev fish of same species, flow ratio
model7 <- glm(IHA ~ prevproportion + outflowratio, datanew, family = "binomial")
(summary(model7))

#model with proportion of prev fish of same species
model8 <- glm(IHA ~ prevproportion, datanew, family = "binomial")
(summary(model8))

#model with only flow ratio as predictor
model9 <- glm(IHA ~ outflowratio,datanew,family = "binomial")
(summary(model9))

#DIAGNOSTICS

#binned residual plot
binnedplot(fitted(model2),residuals(model2))

#hosmer lemeshow test
library(generalhoslem)
logitgof(datanew$IHA,fitted(model2))
##############################################################################
#binned residual plot
binnedplot(fitted(model1),residuals(model1))

#hosmer lemeshow test
library(generalhoslem)
logitgof(datanew$IHA,fitted(model1))
###############################################################################
#qqplot
qqnorm(residuals(model1))


h <- hatvalues(model1)

halfnorm(h)

two.p.n <- 2*sum(h)/length(h)

abline(a=two.p.n,b=0)
###############################################################################
#studenized

studentized <- rstudent(model1)
halfnorm(studentized)

max(rstudent(model1))
n <- nrow(datanew)
p <- sum(h)
qt(0.025/n,n-p-1,lower.tail = FALSE)
################################################################################

#MODEL SELECTION

n <- nrow(datanew)
AIC <- matrix(0,5,5)

AIC[1,2] <- nrow(summary(model1)$coefficients)
AIC[2,2] <- nrow(summary(model6)$coefficients)
AIC[3,2] <- nrow(summary(model7)$coefficients)
AIC[4,2] <- nrow(summary(model8)$coefficients)
AIC[5,2] <- nrow(summary(model9)$coefficients)
AIC[1,3] <- extractAIC(model1)[2] + ((2*AIC[1,2]*(AIC[1,2]+1)) /(n - AIC[1,2] - 1)) 
AIC[2,3] <- extractAIC(model6)[2] + ((2*AIC[2,2]*(AIC[2,2]+1)) /(n - AIC[2,2] - 1)) 
AIC[3,3] <- extractAIC(model7)[2] + ((2*AIC[3,2]*(AIC[3,2]+1)) /(n - AIC[3,2] - 1)) 
AIC[4,3] <- extractAIC(model8)[2] + ((2*AIC[4,2]*(AIC[4,2]+1)) /(n - AIC[4,2] - 1))
AIC[5,3] <- extractAIC(model9)[2] + ((2*AIC[5,2]*(AIC[5,2]+1)) /(n - AIC[5,2] - 1)) 
AIC[1,4] <- AIC[1,3] - min(AIC[,3])
AIC[2,4] <- AIC[2,3] - min(AIC[,3])
AIC[3,4] <- AIC[3,3] - min(AIC[,3])
AIC[4,4] <- AIC[4,3] - min(AIC[,3])
AIC[5,4] <- AIC[5,3] - min(AIC[,3])
AIC[1,5] <- exp(-0.5*AIC[1,4])/(exp(-0.5*AIC[1,4]) + exp(-0.5*AIC[2,4]) + exp(-0.5*AIC[3,4]) + exp(-0.5*AIC[4,4]) + exp(-0.5*AIC[5,4]))
AIC[2,5] <- exp(-0.5*AIC[2,4])/(exp(-0.5*AIC[1,4]) + exp(-0.5*AIC[2,4]) + exp(-0.5*AIC[3,4]) + exp(-0.5*AIC[4,4]) + exp(-0.5*AIC[5,4]))
AIC[3,5] <- exp(-0.5*AIC[3,4])/(exp(-0.5*AIC[1,4]) + exp(-0.5*AIC[2,4]) + exp(-0.5*AIC[3,4]) + exp(-0.5*AIC[4,4]) + exp(-0.5*AIC[5,4]))
AIC[4,5] <- exp(-0.5*AIC[4,4])/(exp(-0.5*AIC[1,4]) + exp(-0.5*AIC[2,4]) + exp(-0.5*AIC[3,4]) + exp(-0.5*AIC[4,4]) + exp(-0.5*AIC[5,4]))
AIC[5,5] <- exp(-0.5*AIC[5,4])/(exp(-0.5*AIC[1,4]) + exp(-0.5*AIC[2,4]) + exp(-0.5*AIC[3,4]) + exp(-0.5*AIC[4,4]) + exp(-0.5*AIC[5,4]))
AIC[1,1] <- "Model 1"
AIC[2,1] <- "Model 6"
AIC[3,1] <- "Model 7"
AIC[4,1] <- "Model 8"
AIC[5,1] <- "Model 9"

#prediction
flowratio <- matrix(1,n,1)   # flowratio = 1
colnames(flowratio) <- c("outflowratio")
nD <- data.frame(sort(datanew$prevproportion)); 
colnames(nD) <- c("previous proportion")
nD <- cbind(nD,flowratio)
prevproportion <- sort(datanew$prevproportion)
preds <- predict(model7,newdata = nD, type="response",se.fit=TRUE)

crit <- qt(0.025,n-3,lower.tail = FALSE)
lower <- preds$fit - crit*preds$se.fit
upper <- preds$fit + crit*preds$se.fit


plot(nD$`previous proportion`,preds$fit,type='l',main = "Prediction", xlab = "Proportion of individuals that chose IHA on previous day",ylab = "Probability of choosing IHA")
lines(nD$`previous proportion`,lower,lty=2)
lines(nD$`previous proportion`,upper,lty=2)

conf.int <- confint(model1)
avg <- summary(model1)$coefficients[-c(1),1]
x <- 1:4
plot(x, avg, ylim = c(-5,8), ylab = "Estimate CI")
# hack: we draw arrows but with very special "arrowheads"
arrows(x, conf.int[-c(1),1], x, conf.int[-c(1),2], length=0.05, angle=90, code=3)

abline(a=0,b=0)
text(1,-3,"P")
text(2,-3,"Species")
text(3,-3,"Temp")
text(3.9,-3,"Flow")