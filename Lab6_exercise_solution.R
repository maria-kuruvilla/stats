#MIXED MODELS LAB 
#5/9/2019 

set.seed(3)
#Generate Kery's wallcreeper data
#trend in density in 12 different regions 
yr <- 16
rgn <- 12 #number of regions
b0 <- 30 #overall intercept (mean)
b <- -0.8 #overall slope 
sigma.regions <- 3 #SD of region-specific intercept effects 
sigma.year <- 1.5 #SD of region-specific slope effects 
sigma <- 5 #SD residual  

#1. simulating data
rgn.int <- rnorm(rgn,0,sigma.regions) #generate deviates
yr.int <- rnorm(yr,0,sigma.year) #generate deviates 

KW.slope1 <- matrix(NA,nrow = (rgn*yr),ncol = 3) #region #year #prcnt_occupied
count <- 1
for(i in 1:rgn){
  for(j in 1:yr){
    KW.slope1[count,1] <- i
    KW.slope1[count,2] <- j 
    KW.slope1[count,3] <- b0 + rgn.int[i] + b*j + yr.int[j] + rnorm(1,0,sigma)
    count <- count + 1
  }
}
KW.slope1 <- as.data.frame(KW.slope1) 
colnames(KW.slope1) <- c("region","year","prcnt")

##############################################################
#WRT REML vs ML
#We can use REML when testing two different (nested) random effects models (and it is preferred)
#We can't use REML when testing a random effects model against a model without the random effect 
#We can use ML to test (and calculate AIC etc) for fixed effects tests 
##############################################################

#2. fit the global model - run the model with the random effects of region and year 
library(lme4)
mmod1 <- lmer(prcnt ~ year + (1|region) + (1|year), KW.slope1, REML = TRUE)
summary(mmod1)

#3. run the model with only the effect of region
mmod2 <- lmer(prcnt ~ year + (1|region), KW.slope1, REML = TRUE)
summary(mmod2)

#4. run the model with only the random effect of year 
mmod3 <- lmer(prcnt ~ year + (1|year), KW.slope1, REML = TRUE)
summary(mmod3)

#5. use exactRLRT to evaluate the random effects 
#exactRLRT(model.randA,model.randAB,model.randB) would test AB against A
library(RLRsim)
exactRLRT(mmod3,mmod1,mmod2) #testing mmod3 against mmod1 (does region effect matter?)
exactRLRT(mmod2,mmod1,mmod3) #testing mmod2 against mmod1 (does year effect matter?)

#6. fit a model with no random effect for comparison 
#also need to fit our global model using REML = FALSE
mmod1.f <- lmer(prcnt ~ year + (1|region) + (1|year), KW.slope1, REML = FALSE)
mmod4 <- lm(prcnt ~ year, KW.slope1)

#7. now test both random effects against a model with no random effect using bootstrapping 
lrstat <- numeric(1000)
set.seed(123)
LR.stat <- 2*(logLik(mmod1.f) - logLik(mmod4))
for(i in 1:1000){
  y <- unlist(simulate(mmod4))
  balt <- lmer(y ~ year + (1|region) + (1|year), KW.slope1, REML = FALSE)
  bnull <- lm(y ~ year, KW.slope1)
  lrstat[i] <- as.numeric(2*(logLik(balt) - logLik(bnull)))
}
mean(lrstat > LR.stat)

#also can look at AIC without REML 
summary(mmod1.f <- lmer(prcnt ~ year + (1|region) + (1|year), KW.slope1, REML = FALSE) )
summary(mmod5.f <- lmer(prcnt ~ 1 + (1|region) + (1|year), KW.slope1, REML = FALSE) )
extractAIC(mmod1.f)
extractAIC(mmod5.f)

#8. get confidence intervals 
confint(mmod1, method = "boot") #note, this provides CIs on SDs 


