require(lme4)
#1
bat <- Lab7_bats
model1 <- lmer(bat.temp ~ ambient + (1|colony) + (1|colony:individual) + (1|day),bat)
summary(model1)
#2
bat2 <- cbind(bat,batind = (bat$colony - 1)*7 + bat$individual)
model2 <- lmer(bat.temp ~ ambient + (1|batind) + (1|day),bat2)
summary(model2)
#4
model3 <- lmer(bat.temp ~ ambient + (1|colony) + (1|colony:individual),bat)

model4 <- lmer(bat.temp ~ ambient + (1|colony) + (1|colony:individual) + (1|day),bat)

model5 <- lmer(bat.temp ~ ambient + (1|day),bat)

require(RLRsim)
exactRLRT(model5,model4,model3)


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

require(faraway)
#produce a qqplot 
qqnorm(residuals(model1))
qqline(residuals(model1))

#5
modela <- lmer(bat.temp ~ ambient + (1|colony) + (1|colony:individual) + (1|day),bat,REML=FALSE)


modelb <- lmer(bat.temp ~ 1 + (1|colony) + (1|colony:individual) + (1|day),bat,REML=FALSE)

extractAIC(modela)
extractAIC(modelb)


#6. bootstrap CIs using the manual boostrap 
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

#bootstrap CIs using the confint() method 
confint(model1,method = "boot")


#7 predict for winter day
newdata <- with(bat, expand.grid(ambient=c(-0.5,0,0.5)))
levs <- nrow(newdata)


indcol.sd <- as.data.frame(VarCorr(model1))$sdcor[1]
day.sd <- as.data.frame(VarCorr(model1))$sdcor[2]
colony.sd <- as.data.frame(VarCorr(model1))$sdcor[3]
resid.sd <- as.data.frame(VarCorr(model1))$sdcor[4]
pv <- matrix(NA,nrow=1000,ncol = levs)
for(i in 1:1000){
  y <- unlist(simulate(model1))
  bmod <- refit(model1,y)
  pv[i,] <- predict(bmod, re.form = ~0, newdata) + rnorm(1,sd=indcol.sd) + rnorm(1,sd=day.sd) + rnorm(1,sd=colony.sd) + rnorm(n=1,sd=resid.sd)
}
plot(x = unlist(newdata), y = apply(pv, 2, mean), xlab = "Ambient", ylab = "Temp", ylim = c(0,10))
lines(x = unlist(newdata), y = apply(pv, 2, quantile, probs=c(0.025)) )
lines(x = unlist(newdata), y = apply(pv, 2, quantile, probs=c(0.975)) )


#8. predict on a future day for a new bat from colony 2 
newdata1 <- with(bat, expand.grid(colony=2, ambient=c(-0.5,0,0.5), day = NA, indcol = NA))

pv1 <- matrix(NA,nrow=1000,ncol = levs)
for(i in 1:1000){
  y <- unlist(simulate(model1,use.u=TRUE))
  bmod <- refit(model1,y)
  pv1[i,] <- predict(bmod, re.form = ~ (1|colony), newdata1) + rnorm(1,sd=indcol.sd) + rnorm(1,sd=day.sd) + rnorm(n=1,sd=resid.sd)
}
points(x = unlist(newdata1[,2]), y = apply(pv1, 2, mean), pch = 3)
lines(x = unlist(newdata1[,2]), lty = 3, y = apply(pv1, 2, quantile, probs=c(0.025)) )
lines(x = unlist(newdata1[,2]), lty = 3, y = apply(pv1, 2, quantile, probs=c(0.975)) )