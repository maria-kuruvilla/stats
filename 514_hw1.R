time <- HW1gecko_snakes$time
length <- HW1gecko_snakes$length
plot(length,time)
#abline(lm(time~length))

#get the X design matrix
X <- cbind(rep(1,n),length)
#get the y response variable
y <- time
inv.xtx <- solve(t(X)%*%X) 
#estimates of beta 
beta.hat <- inv.xtx%*%t(X)%*%y
#the hat matrix 
hat.matrix <- X%*%inv.xtx%*%t(X)
#estimates of y.hat using the hat matrix 
y.hat <- hat.matrix%*%y

ymean <- mean(y)
squares <- (y.hat-ymean)^2
mss <- sum(squares)

tss <- sum((y-ymean)^2)

rss <- sum((y-y.hat)^2)