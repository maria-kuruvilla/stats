require(faraway)
library(arm)


#model with only proportion of prev fish of same species
model1 <- glm(IHA ~ prevproportion,datanew,family = "binomial")
(summary(model1))


library(MASS)
confint(model1)

#binned residual plot
binnedplot(fitted(model1),residuals(model1))

#hosmer lemeshow test
library(generalhoslem)
logitgof(datanew$IHA,fitted(model1))

#hatvales

h <- hatvalues(model1)

halfnorm(h)

two.p.n <- 2*sum(h)/length(h)

abline(a=two.p.n,b=0)

#studenized

studentized <- rstudent(model1)
halfnorm(studentized)

max(rstudent(model1))
n <- nrow(datanew)
p <- sum(h)
qt(0.025/n,n-p-1,lower.tail = FALSE)

#prediction
nD <- data.frame(sort(datanew$prevproportion)); colnames(nD) <- c("previous proportion")
prevproportion <- sort(datanew$prevproportion)
preds <- predict(model1,newdata = nD, type="response",se.fit=TRUE)

crit <- qt(0.025,n-2,lower.tail = FALSE)
lower <- preds$fit - crit*preds$se.fit
upper <- preds$fit + crit*preds$se.fit

conv.preds <- 1/(1+exp(-(preds$fit)))
conv.lower <- 1/(1+exp(-(lower)))
conv.upper <- 1/(1+exp(-(upper)))

plot(nD$`previous proportion`,preds$fit,type='l',main = "Prediction", xlab = "Proportion of individuals that chose IHA on previous day",ylab = "Probability of choosing IHA")
lines(nD$`previous proportion`,lower,lty=2)
lines(nD$`previous proportion`,upper,lty=2)

#model with proportion of prev fish of same species and species
model2 <- glm(IHA ~ prevproportion + as.factor(Species),datanew,family = "binomial")
(summary(model2))

#model with proportion of prev fish, species and rear
model3 <- glm(IHA ~ prevproportion + as.factor(Species) + as.factor(Rear),datanew,family = "binomial")
(summary(model3))

#model with prop of prev fish, species, rear, scale diff temp
model4 <- glm(IHA ~ prevproportion + as.factor(Species) + as.factor(Rear) + scaledifference, datanew,family="binomial")
(summary(model4))

#model with prop of prev fish, Species, rear, scale ratio
model5 <- glm(IHA ~ prevproportion + as.factor(Species) + as.factor(Rear) + scaleratio, datanew, family = "binomial")
(summary(model5))
model.matrix(model5)

#binned residual plot
binnedplot(fitted(model5),residuals(model5))

library(generalhoslem)
logitgof(datanew$IHA,fitted(model5))


#RANDOM EFFECTS

#model with prop of prev fish, Species, rear, scale ratio, random effect of release site
require(lme4)
model6 <- glmer(IHA ~ prevproportion + as.factor(Species) + as.factor(Rear) + scaleratio + (1|RS),datanew, family="binomial")
(summary(model6))

#binned residual plot
binnedplot(fitted(model6),residuals(model6))

#goodness of fit
library(generalhoslem)
logitgof(datanew$IHA,fitted(model5))

#DIAGNOSTICS

#hatvales

h <- hatvalues(model6)

halfnorm(h)

two.p.n <- 2*sum(h)/length(h)

abline(a=two.p.n,b=0)

#studenized

studentized <- rstudent(model6)
halfnorm(studentized)

max(rstudent(model1))
n <- nrow(datanew)
p <- sum(h)
qt(0.025/n,n-p-1,lower.tail = FALSE)

#cooks

halfnorm(cooks <- cooks.distance(model6))


#produce a qqplot 
qqnorm(residuals(model6))
qqline(residuals(model6))


#MODEL SELECTION
AIC <- rep(NA,4)

#model with prop of prev fish, Species, rear, scale ratio, random effect of release site
require(lme4)
model7 <- glmer(IHA ~ prevproportion + as.factor(Species) + as.factor(Rear) + scaleratio + (1|RS),datanew, family="binomial",REML = FALSE)
(summary(model7))
AIC[1] <- extractAIC(model7)[2]

#model with prop of prev fish, Species, rear,  random effect of release site
#no ratio of temp
model8 <- glmer(IHA ~ prevproportion + as.factor(Species) + as.factor(Rear)  + (1|RS),datanew, family="binomial",REML = FALSE)
(summary(model8))
AIC[2] <- extractAIC(model8)[2]

#model with prop of prev fish, Species, scale ratio, random effect of release site
require(lme4)
model9 <- glmer(IHA ~ prevproportion + as.factor(Species) + scaleratio + (1|RS),datanew, family="binomial",REML = FALSE)
(summary(model9))
AIC[3] <- extractAIC(model9)[2]

#model with prop of prev fish, rear, scale ratio, random effect of release site
require(lme4)
model10 <- glmer(IHA ~ prevproportion + as.factor(Rear) + scaleratio + (1|RS),datanew, family="binomial",REML = FALSE)
(summary(model10))
AIC[4] <- extractAIC(model10)[2]

#RANDOM EFFECTS AND FLOW

#model with scaled prop of prev fish, Species, rear, scale ratio, random effect of release site
require(lme4)
model11 <- glmer(IHA ~ scale(prevproportion) + as.factor(Species) + as.factor(Rear) + scaleratio + scalediffoutflow +(1|RS),datanew, family="binomial", nAGQ = 50)
(summary(model11))

iv <- summary(model11)$coefficients[,1]

require(glmmML)
RS <- datanew$RS
IHA <- datanew$IHA
prevproportion <- datanew$prevproportion
Species <- datanew$Species
Rear <- datanew$Rear
scaleratio <- datanew$scaleratio
scalediffoutflow <- datanew$scalediffoutflow
model11 <- glmmML(IHA ~ scale(prevproportion) + as.factor(Species) + as.factor(Rear) + scaleratio + scalediffoutflow,cluster =RS, family="binomial", method = "ghq")
(summary(model11))



