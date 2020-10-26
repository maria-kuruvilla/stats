##SIMULATING DATA 

#Impacts of channel dredging on crabs 
#Data are crabs/1000 cubic yds dredged (response)
#and crab density (predictor) crabs/ha  
set.seed(7) 
n <- 30 
crab.dens <- runif(n,600,1500) #crabs/ha 
b0 <- 0 
b1 <- 0.2
entrain <- b0 + b1*crab.dens + rnorm(n,0,100)
crab <- as.data.frame(cbind(sample = (1:n),entrain,crab.dens))
summary(lmod <- lm(entrain ~ crab.dens, data = crab))
min(entrain)

summary(lmod <- lm(entrain ~ -1 + crab.dens, data = crab))


##PLOTTING DATA 
plot(entrain ~ crab.dens)

#get the X design matrix
X <- cbind(rep(1,n),crab.dens)
#get the y response variable
y <- entrain
inv.xtx <- solve(t(X)%*%X) 
#estimates of beta 
beta.hat <- inv.xtx%*%t(X)%*%y
#the hat matrix 
hat.matrix <- X%*%inv.xtx%*%t(X)
#estimates of y.hat using the hat matrix 
y.hat <- hat.matrix%*%y

#using lm()
summary(mod.crab <- lm(entrain ~ crab.dens, data = crab))
#checking predictions from lm() vs y.hat
y.hat.lm <- predict(mod.crab,type="response")
#get the design matrix 
model.matrix(mod.crab)

#is this a more sensible model? 
summary(mod.crab.no.int <- lm(entrain ~ -1 + crab.dens, data = crab))
