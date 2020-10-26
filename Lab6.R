#Mixed Effects Models 
#effect of forest block and year (random) and rainfall (fixed) on herbaceous growth  
#kg/10m2 
n.block <- 12 #number of blocks 
n.year <- 8 #number of years
n.samp <- 6 #number of samples per block per year
b0 <- 12 #mean growth 
b.rain <- 3 #mean rain effect 
sigma.block <- 2 #block int SD 
sigma.slope <- 2 #block slope SD
sigma.year <- 2 #year SD
sigma.resid <- 3 #residual SD
covar <- -0.5 #covariance between block int and block slope

#generate deviates from a bivariate normal distribution (block int and block slope are correlated)   
mu.vector <- c(b0,b.rain)
sigma <- matrix(c(sigma.block^2,covar, covar, sigma.slope^2),2,2)
library(MASS)
block <- mvrnorm(n = n.block, mu = mu.vector, Sigma = sigma)

#note that I could also simulate uncorrelated slope and intercepts
#I would substitute these in for block[,1] and block[,2] in the data simulation below nrow
block.nocov <- matrix(0,nrow=n.block,ncol=2)
block.nocov[,1] <- rnorm(n.block,0,sigma.block) + b0
block.nocov[,2] <- rnorm(n.block,0,sigma.slope) + b.rain

#set up to simulate
t.samps <- n.block*n.year*n.samp #number of data points 
rain <- round( runif(t.samps,0,0.6), dig = 3) #simulate your predictor
year.eff <- rnorm(n.year,0,sigma.year) #get your year effects 

#simulate data 
set.seed(2)
count <- 1
data.herb <- matrix(NA,nrow=(t.samps),ncol=5)
for(i in 1:n.year){
  for(j in 1:n.block){
    for(k in 1:n.samp){
      data.herb[count,1] <- i
      data.herb[count,2] <- j
      data.herb[count,3] <- k
      data.herb[count,4] <- rain[count]
      #this version for correlated effects 
      data.herb[count,5] <- block[j,1] + block[j,2]*rain[count] + year.eff[i] + rnorm(1,0,sigma.resid) 
      #this version has no covariance
      #data.herb[count,5] <- block.nocov[j,1] + block.nocov[j,2]*rain[count] + year.eff[i] + rnorm(1,0,sigma.resid) 
      count <- count + 1
    }  
  }
}
data.herb <- as.data.frame(data.herb)
colnames(data.herb) <- c("year","block","samp","rain","biomass")
data.herb$s.rain <- scale(data.herb$rain)

#run some RE models 
library(lme4)

#correlated random slope and intercept 
mod1 <- lmer(biomass ~ s.rain + (s.rain|block), data.herb) #slope and intercept random

#uncorrelated random slope and intercept 
mod1v <- lmer(biomass ~ s.rain + (1|block) + (0 + s.rain|block), data.herb) #slope and intercept random

mod2 <- lmer(biomass ~ s.rain + (0 + s.rain|block), data.herb) #random slope (maybe not sensible?)

mod3 <- lmer(biomass ~ s.rain + (1|block) + (1|year), data.herb) #block intercept and year 

mod4 <- lmer(biomass ~ s.rain + (1|block), data.herb) #random intercept

mod5 <- lmer(biomass ~ s.rain + (1|year), data.herb) #year only 

#testing the random intercepts of block with exact restricted likelihood ratio tests 
#must be uncorrelated random effects 
#given RE of year in model 
require(RLRsim)
exactRLRT(mod4,mod3,mod5)

#do a bootstrap LRT to test random slopes  
lrstat <- numeric(1000)
set.seed(123)
LR.stat <- 2*(logLik(mod3) - logLik(mod5))
for(i in 1:1000){
  y <- unlist(simulate(mod5))
  bnull <- lmer(y ~ s.rain + (1|year), data.herb, REML = TRUE)
  balt <- lmer(y ~ s.rain + (1|block) + (1|year), data.herb, REML = TRUE)
  lrstat[i] <- as.numeric(2*(logLik(balt) - logLik(bnull)))
}
mean(lrstat > LR.stat)

#LR Tests 
library(RLRsim)
exactRLRT(mod5,mod3,mod4) #testing the year RE given block in model 

mod2f <- lmer(biomass ~ s.rain + (1|block),data.herb, REML = FALSE) 
mod5f <- lmer(biomass ~ s.rain + (1|year), data.herb, REML = FALSE)
mod6 <- lm(biomass ~ s.rain, data.herb)
exactLRT(mod2f,mod6) #testing the block RE without year in model
exactLRT(mod5f,mod6) #testing the block RE without year in model

#now look at the fixed effects with AIC 
mod1f <- lmer(biomass ~ s.rain + (1|block), data.herb, REML = FALSE) #slope and intercept random, fixed rain
mod7f <- lmer(biomass ~ 1 + (1|block), data.herb, REML = FALSE) #slope and intercept random, no rain

extractAIC(mod1f)
extractAIC(mod7f)

#confidence intervals 
confint(mod1,method = "boot") #bootstrap confidence intervals 
confint(mod1) #profile intervals (we would prefer bootstrap here)
