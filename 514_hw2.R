require(xtable)
cones <- HW2_sequoia$cones
rain <- HW2_sequoia$rain
canopy1 <- HW2_sequoia$canopy1
canopy2 <- HW2_sequoia$canopy2
elevation <- HW2_sequoia$elev

n <- length(HW2_sequoia$cones)
conesdata <- as.data.frame(cbind(sample = (1:n),cones,rain,canopy1,canopy2,elevation))
model <- lm(cones ~ rain + canopy1 + canopy2 + elevation,conesdata)
xtable(summary(model), type = "latex")
#get the X design matrix
X <- cbind(rep(1,n),rain,canopy1,canopy2,elevation)
#get the y response variable
y <- cones
inv.xtx <- solve(t(X)%*%X) 
#estimates of beta (parameters)
beta.hat <- inv.xtx%*%t(X)%*%y
#the hat matrix 
hat.matrix <- X%*%inv.xtx%*%t(X)
#estimates of y.hat using the hat matrix (model predictions)
y.hat <- hat.matrix%*%y

#sum of squared errors
ymean <- mean(y)
squares <- (y.hat-ymean)^2
mss <- sum(squares)
tss <- sum((y-ymean)^2)
rss <- sum((y-y.hat)^2)

#f statistic
f <- ((tss-rss)/4)/(rss/42)

#p value
pf(f,4,42,lower.tail = F)

nreps <- 10000
fstats <- numeric(nreps)
for(i in 1:nreps){
  lmods <- lm(sample(cones) ~ rain+canopy1+canopy2+elevation, data = conesdata)
  fstats[i] <- summary(lmods)$fstat[1]
}
length(fstats[fstats>summary(lmods)$fstat[1]])/nreps

model1 <- lm(cones ~ rain + I(canopy1 + canopy2) + elevation,conesdata)
anova(model1,model)

res1 <- sum(model1$residuals^2)
res <- sum(model$residuals^2)

f2 <- (res1-res)*42/res
pf(f2,1,42,lower.tail = F)

#only elevation
model3 <- lm(cones ~ -1 + elevation,conesdata)
confint(model3,level = 0.9)