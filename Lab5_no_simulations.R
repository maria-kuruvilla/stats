#Frequentist model selection procedures on a simulated dataset 

#this code demonstrates leave-one-out and k-fold cross validation for a simulated data set
#the cross-validation procedures are done by hand in the first half of the code
#and are done using cross-validation functions in the second half of the code 
#finally, AIC, AICc, and BIC are calculated 

#May 1, 2019 

n <- 50 #samples
x1 <- runif(n,0,1) #predictor 1 
x2 <- runif(n,0,1) #predictor 2
b0 <- 5 #intercept
b1 <- 3 #effect of predictor 1
b2 <- 2.5 #effect of predictor 2
sigma <- 25 #residual SD

K <- 5 #folds (only applies to k-fold)

store.loo <- rep(NA,4) #store leave-one-out results
store.kfold <- rep(NA,4) #store k-fold results

store.loo.auto <- rep(NA,4) #for function-based LOO
store.kfold.auto <- rep(NA,4) #for function-based k-fold

store.IC <- matrix(NA, nrow = 4, ncol = 3) #information criteria results

y <- b0 + b1*x2 + b2*x2 + runif(n,0,sigma) #simulate dataset 
data.set <- as.data.frame( cbind(y,x1,x2) )

#####################################################################################################################
#                                                                                                                   #
#                                       Perform Cross-Validation by Hand                                            #
#                                                                                                                   #
#####################################################################################################################

#LEAVE ONE OUT CROSS VALIDATION 
p1 <- p2 <- p3 <- p4 <- rep(NA,n)
for(j in 1:n){
  training <- setdiff(1:nrow(data.set),j) #identify training data 
  
  #run model on training data 
  lmod1t <- lm(y ~ x1 + x2, data = data.set, subset = c(training)) 
  lmod2t <- lm(y ~ x1, data = data.set, subset = c(training))
  lmod3t <- lm(y ~ x2, data = data.set, subset = c(training))
  lmod4t <- lm(y ~ 1, data = data.set, subset = c(training))
  
  #make predictions on testing data point 
  p1[j] <- predict(lmod1t,newdata = data.frame(data.set[j,]))
  p2[j] <- predict(lmod2t,newdata = data.frame(data.set[j,]))
  p3[j] <- predict(lmod3t,newdata = data.frame(data.set[j,]))
  p4[j] <- predict(lmod4t,newdata = data.frame(data.set[j,]))

}
#calculate MSPE for each of the models and store 
store.loo[1] <- sum( (data.set$y - p1)^2/n)
store.loo[2] <- sum( (data.set$y - p2)^2/n)
store.loo[3] <- sum( (data.set$y - p3)^2/n)
store.loo[4] <- sum( (data.set$y - p4)^2/n)

  
#5-fold CROSS VALIDATION 
p1 <- p2 <- p3 <- p4 <- rep(NA,K)
for(j in 1:K){
  #identify testing and training data     
  testing <- c( (1+(j-1)*(n/K)) : ((n/K)+(j-1)* (n/K) ) )
  training <- setdiff(1:nrow(data.set),testing) 
    
  #run models 
  lmod1t <- lm(y ~ x1 + x2, data = data.set, subset = training)
  lmod2t <- lm(y ~ x1, data = data.set, subset = training)
  lmod3t <- lm(y ~ x2, data = data.set, subset = training)
  lmod4t <- lm(y ~ 1, data = data.set, subset = training)
    
  #calculate MSPE for a given fold  
  p1[j] <- sum((data.set$y[testing] - predict(lmod1t,newdata = data.frame(data.set[testing,])) )^2/ (n/K) )
  p2[j] <- sum((data.set$y[testing] - predict(lmod2t,newdata = data.frame(data.set[testing,])) )^2/ (n/K) )
  p3[j] <- sum((data.set$y[testing] - predict(lmod3t,newdata = data.frame(data.set[testing,])) )^2/ (n/K) )
  p4[j] <- sum((data.set$y[testing] - predict(lmod4t,newdata = data.frame(data.set[testing,])) )^2/ (n/K) )
    
}
#take the mean MSPE over folds and store 
store.kfold[1] <- mean(p1)
store.kfold[2] <- mean(p2)
store.kfold[3] <- mean(p3)
store.kfold[4] <- mean(p4) 
  
#####################################################################################################################
#                                                                                                                   #
#                                       Perform Cross-Validation using Functions                                    #
#                                                                                                                   #
#####################################################################################################################

#Run Models 
lmod1 <- lm(y ~ x1 + x2, data = data.set, x = TRUE, y = TRUE)
lmod2 <- lm(y ~ x1, data = data.set, x = TRUE, y = TRUE)
lmod3 <- lm(y ~ x2, data = data.set, x = TRUE, y = TRUE)
lmod4 <- lm(y ~ 1, data = data.set, x = TRUE, y = TRUE)
  
require(boot)
#k-fold cross validation
store.kfold.auto[1] <- cv.glm(data = data.set, lmod1, K=K)$delta[1]
store.kfold.auto[2] <- cv.glm(data = data.set, lmod2, K=K)$delta[1]
store.kfold.auto[3] <- cv.glm(data = data.set, lmod3, K=K)$delta[1]
store.kfold.auto[4] <- cv.glm(data = data.set, lmod4, K=K)$delta[1]

#leave-one-out cross validation
store.loo.auto[1] <- cv.glm(data = data.set, lmod1, K=n)$delta[1]
store.loo.auto[2] <- cv.glm(data = data.set, lmod2, K=n)$delta[1]
store.loo.auto[3] <- cv.glm(data = data.set, lmod3, K=n)$delta[1]
store.loo.auto[4] <- cv.glm(data = data.set, lmod4, K=n)$delta[1]

#####################################################################################################################
#                                                                                                                   #
#                                       AIC, AICc, and BIC                                                          #
#                                                                                                                   #
#####################################################################################################################
#calculate statistics for lmod1
n <- nrow(data.set)
p1 <- nrow(summary(lmod1)$coefficients)
p2 <- nrow(summary(lmod2)$coefficients)
p3 <- nrow(summary(lmod3)$coefficients)
p4 <- nrow(summary(lmod4)$coefficients)
RSS1 <- anova(lmod1)$'Sum Sq'[ nrow(anova(lmod1))]
RSS2 <- anova(lmod2)$'Sum Sq'[ nrow(anova(lmod2))]
RSS3 <- anova(lmod3)$'Sum Sq'[ nrow(anova(lmod3))]
RSS4 <- anova(lmod4)$'Sum Sq'[ nrow(anova(lmod4))]

#note also extractAIC(lmod1) or AIC(lmod1) but the latter will give you a different value
store.IC[1,1] <- n*log(RSS1/n) + p1*2 
store.IC[2,1] <- n*log(RSS2/n) + p2*2 
store.IC[3,1] <- n*log(RSS3/n) + p3*2 
store.IC[4,1] <- n*log(RSS4/n) + p4*2 

#note also BIC(lmod1) but it will give you a different value
store.IC[1,2] <- extractAIC(lmod1)[2] + ((2*p1*(p1+1)) /(n - p1 - 1))  
store.IC[2,2] <- extractAIC(lmod2)[2] + ((2*p2*(p2+1)) /(n - p2 - 1))  
store.IC[3,2] <- extractAIC(lmod3)[2] + ((2*p3*(p3+1)) /(n - p3 - 1))  
store.IC[4,2] <- extractAIC(lmod4)[2] + ((2*p4*(p4+1)) /(n - p4 - 1))  

store.IC[1,3] <- n*log(RSS1/n) + p1*log(n)  
store.IC[2,3] <- n*log(RSS2/n) + p2*log(n) 
store.IC[3,3] <- n*log(RSS3/n) + p3*log(n) 
store.IC[4,3] <- n*log(RSS4/n) + p4*log(n) 
  