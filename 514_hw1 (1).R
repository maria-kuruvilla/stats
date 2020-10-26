time <- HW1gecko_snakes$time
length <- HW1gecko_snakes$length
plot(length,time)
n <- length(HW1gecko_snakes$sample)
#get the X design matrix
X <- cbind(rep(1,n),length)
#get the y response variable
y <- time
inv.xtx <- solve(t(X)%*%X) 
#estimates of beta (parameters)
beta.hat <- inv.xtx%*%t(X)%*%y
#the hat matrix 
hat.matrix <- X%*%inv.xtx%*%t(X)
#estimates of y.hat using the hat matrix (model predictions)
y.hat <- hat.matrix%*%y

ymean <- mean(y)
squares <- (y.hat-ymean)^2
mss <- sum(squares)
tss <- sum((y-ymean)^2)
rss <- sum((y-y.hat)^2)

s <- which.min(HW1gecko_snakes$length)
shortest <- y.hat[s]
l <- which.max(HW1gecko_snakes$length)
longest <- y.hat[l]
m <- which(HW1gecko_snakes$length==median(HW1gecko_snakes$length))
medianvalue <- y.hat[m]

##### 2 


set.seed(1) 
n <- 20
prop <- runif(n,0,1) #proportion of informed individuals 
b0 <- 15  #time (in minutes) taken by naive group to find food 
b1 <- -2.5
timetaken <- b0 + b1*prop + rnorm(n,0,6)
foraging <- as.data.frame(cbind(sample = (1:n),timetaken,prop))
summary(lmod <- lm(timetaken ~ prop, data = foraging))

  


