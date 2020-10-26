# a. Build a linear mixed model including all of the appropriate fixed and random effects.
# Provide the command you gave R to run your model. Provide an estimate for parameters
# associated with your random and fixed effects, based on your summary() output.
require(lme4)
model1 <- lmer(scale(growth) ~ as.factor(treatment) + (1|bed) + (1|female), HW7_caterpillars_new)
plot(as.factor(HW7_caterpillars_new$treatment),HW7_caterpillars_new$growth)
(summary(model1))


# b. Evaluate the degree to which you meet assumptions of normal and identically distributed
# errors. Provide two plots to support your findings.
require(faraway)

qqnorm(residuals(model1))
qqline(residuals(model1))
shapiro.test(residuals(model1))


plot(fitted(model1),residuals(model1))

plot(fitted(model1),sqrt(abs(residuals(model1))))




# c. Evaluate your random effects, one at a time, to decide which to keep in the model. Use a
# bootstrap-based likelihood ratio test.
model2 <- lmer(scale(growth) ~ as.factor(treatment) + (1|female), HW7_caterpillars_new)
model3 <- lmer(scale(growth) ~ as.factor(treatment) + (1|bed), HW7_caterpillars_new)
(summary(model2))
(summary(model3))
require(RLRsim)
#testing effect of female
exactRLRT(model2,model1,model3)

#testing effect of bed
exactRLRT(model3,model1,model2)

#testing for female

lrstat <- 2*(logLik(model1)-logLik(model3)) 

b.lrstat <- numeric(1000)
set.seed(123)
for(i in 1:1000){
  y <- unlist(simulate(model3))
  bnull <- refit(model3, y)
  balt <- refit(model1, y)
  b.lrstat[i] <- as.numeric(2*(logLik(balt)-logLik(bnull)))
}
(p <- mean(b.lrstat > lrstat)  ) 

#testing for bed

lrstat <- 2*(logLik(model1)-logLik(model2)) 

b.lrstat <- numeric(1000)
set.seed(123)
for(i in 1:1000){
  y <- unlist(simulate(model2))
  bnull <- refit(model2, y)
  balt <- refit(model1, y)
  b.lrstat[i] <- as.numeric(2*(logLik(balt)-logLik(bnull)))
}
(p_bed <- mean(b.lrstat > lrstat)  ) 


# d. Once you've decided what random effects to retain, interpret the outcome of the
# experiment in terms of the treatment effect. Remember to use REML or ML as
# appropriate. Provide AIC for models with and without your treatment effect but including
# the appropriate random effects as determined in (c). Calculate the delta AIC and the
# model weights. Put these pieces of information in a table.

modelfull <- lmer(scale(growth) ~ as.factor(treatment) + (1|bed) + (1|female), HW7_caterpillars_new,REML = FALSE)
modelnull <- lmer(scale(growth) ~ 1 + (1|bed) + (1|female), HW7_caterpillars_new, REML = FALSE)

AIC <- matrix(NA,2,3)
AIC[1,1] <- extractAIC(modelfull)[2]
AIC[2,1] <- extractAIC(modelnull)[2]
AIC[1,2] <- extractAIC(modelfull)[2] - min(AIC[,1])
AIC[2,2] <- extractAIC(modelnull)[2] - min(AIC[,1])
AIC[1,3] <- exp(-0.5*AIC[1,2])/(exp(-0.5*AIC[1,2]) + exp(-0.5*AIC[2,2]))
AIC[2,3] <- exp(-0.5*AIC[2,2])/(exp(-0.5*AIC[1,2]) + exp(-0.5*AIC[2,2]))


# e. Provide a valid confidence interval around any fixed effects and any random effects
# parameters in your model. Use a method of your choice to calculate these. Be sure to be
# clear about the parameter value that you're putting a confidence interval on. Show the
# parameters and their confidence intervals in a table.
confint(model1, method = "boot")

bsd <- matrix(NA,nrow = 1000, ncol = 5)
for(i in 1:1000){
  y <- unlist(simulate(model1))
  bmod <- refit(model1,y)
  bsd[i,1:3] <- as.data.frame(VarCorr(bmod))$sdcor[1:3]
  bsd[i,4] <- summary(bmod)$coefficients[1,1]
  bsd[i,5] <- summary(bmod)$coefficients[2,1]
}
quantile(bsd[,1],probs=c(0.025,0.975))
quantile(bsd[,2],probs=c(0.025,0.975))
quantile(bsd[,3],probs=c(0.025,0.975))
quantile(bsd[,4],probs=c(0.025,0.975))
quantile(bsd[,5],probs=c(0.025,0.975))


# f. Make a prediction, including 95% prediction intervals, for what we would expect growth
# to be if we got eggs from a new female and placed them in bed 2; make the predictions
# for both "T" and "C". Show your complete code.
require(depmixS4)
newdata1 <- with(HW7_caterpillars_new, expand.grid(female = NA, bed=2, treatment=c("T","C")))
levs <- nrow(newdata1)

female.sd <- as.data.frame(VarCorr(model1))$sdcor[1]
bed.sd <- as.data.frame(VarCorr(model1))$sdcor[2]
resid.sd <- as.data.frame(VarCorr(model1))$sdcor[3]

pv1 <- matrix(NA,nrow=1000,ncol = levs)
for(i in 1:1000){
  y <- unlist(simulate(model1,use.u=TRUE))
  bmod <- refit(model1,y)
  pv1[i,] <- predict(bmod, re.form = ~ (1|bed), newdata1)  + rnorm(1,sd=female.sd) + rnorm(n=1,sd=resid.sd)
}

mean.t <- mean(pv1[,1])*sd(HW7_caterpillars_new$growth) + mean(HW7_caterpillars_new$growth)
mean.c <- mean(pv1[,2])*sd(HW7_caterpillars_new$growth) + mean(HW7_caterpillars_new$growth)



# g. Make a prediction, including 95% prediction intervals, for what we would expect growth
# to be if we got eggs from a new female and placed them in any random bed in the
# greenhouse; again, make the predictions for both "T" and "C". Show your complete
# code.



# h. Plot information from (f) and (g) on the same plot.



# i. Build a model with fixed effects analogous to your random effects. Use the output to
# make a plot that demonstrates the concept of shrinkage in predictions. Make predictions
# for "C" only, and then thin the predictions after modeling to show only the (unique)
# predictions for the 12 females in bed 1. Make sure to show the mean on the plot.