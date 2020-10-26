#three different plots, 50 shrubs per plot 
set.seed(107)
n <- 150
b.group1 <- 1.8
b.group2  <- 1.1
b.group3 <- 1.6
sigma <- 0.3
plot1 <- c(rep(1,50),rep(0,100))
plot2 <- c(rep(0,50),rep(1,50),rep(0,50))
plot3 <- c(rep(0,100),rep(1,50))
plot <- c(rep(1,50),rep(2,50),rep(3,50)) 
height <- b.group1*plot1 + b.group2*plot2 + b.group3*plot3+rnorm(n,0,sigma)
shrbHt <- data.frame(cbind(height,plot))

mean(height[1:50]);mean(height[51:100]);mean(height[101:150])

#model 1 - default R coding 
lmod1 <- lm(height ~ as.factor(plot), shrbHt)
summary(lmod1)
modelmat <- model.matrix(lmod1); fix(modelmat)

#model2 
mat <- cbind(rep(1,150),c(rep(1,50),rep(0,100)),c(rep(0,50),rep(1,50),rep(0,50)))
lmod2 <- lm(height ~ -1 + mat)
summary(lmod2)
modelmat <- model.matrix(lmod2); fix(modelmat)

#model3 
mat <- cbind(c(rep(1,50),rep(0,100)),c(rep(0,50),rep(1,50),rep(0,50)),c(rep(0,100),rep(1,50)))
lmod3 <- lm(height ~ -1 + mat)
summary(lmod3)
modelmat <- model.matrix(lmod3); fix(modelmat)

#model4 
mat <- cbind(c(rep(1,150)),c(rep(1,50),rep(0,50),rep(-1,50)),c(rep(0,50),rep(1,50),rep(-1,50)))
lmod4 <- lm(height ~ -1 + mat)
summary(lmod4)
modelmat <- model.matrix(lmod4); fix(modelmat)

#what are SSE for each model? 
anova(lmod1)$'Sum Sq'[2]
anova(lmod2)$'Sum Sq'[2]
anova(lmod3)$'Sum Sq'[2]
anova(lmod4)$'Sum Sq'[2]

#what are predictions for each model? 
p.1 <- predict(lmod1, x = years, interval = "prediction")
p.2 <- predict(lmod2, x = years, interval = "prediction")
p.3 <- predict(lmod3, x = years, interval = "prediction")
p.4 <- predict(lmod4, x = years, interval = "prediction")
plot(x = plot, y = height, col= "grey", ylim= c(0,3))

plot.data1 <- as.data.frame (cbind(c(1,2,3),c(p.1[1,1],p.1[51,1],p.1[101,1]),c(p.1[1,2],p.1[51,2],p.1[101,2]),c(p.1[1,3],p.1[51,3],p.1[101,3])) ) 
colnames(plot.data1) <- c("plot","predict","lwr","upr")
points(x = plot.data1$plot, y = plot.data1$predict, col = "red", cex = 3)
points(x = plot.data1$plot, y = plot.data1$lwr, col = "red", cex = 3, pch = 3)
points(x = plot.data1$plot, y = plot.data1$upr, col = "red", cex = 3, pch = 3)

plot.data2 <- as.data.frame (cbind(c(1,2,3),c(p.2[1,1],p.2[51,1],p.2[101,1]),c(p.2[1,2],p.2[51,2],p.2[101,2]),c(p.2[1,3],p.2[51,3],p.2[101,3])) ) 
colnames(plot.data2) <- c("plot","predict","lwr","upr")
points(x = plot.data2$plot, y = plot.data2$predict, col = "green", cex = 2.5)
points(x = plot.data2$plot, y = plot.data2$lwr, col = "green", cex = 2.5, pch = 3)
points(x = plot.data2$plot, y = plot.data2$upr, col = "green", cex = 2.5, pch = 3)

plot.data3 <- as.data.frame (cbind(c(1,2,3),c(p.3[1,1],p.3[51,1],p.3[101,1]),c(p.3[1,2],p.3[51,2],p.3[101,2]),c(p.3[1,3],p.3[51,3],p.3[101,3])) ) 
colnames(plot.data3) <- c("plot","predict","lwr","upr")
points(x = plot.data3$plot, y = plot.data3$predict, col = "blue", cex = 2)
points(x = plot.data3$plot, y = plot.data3$lwr, col = "blue", cex = 2, pch = 3)
points(x = plot.data3$plot, y = plot.data3$upr, col = "blue", cex = 2, pch = 3)



#################################################################
#Factor Models - still need to do model diagnostics 
par(mai = c(1,1,0.5,0.5))
plot(fitted(lmod1),residuals(lmod1), ylab = "Residuals",xlab = "Fitted",cex.lab=1.5,cex.axis=1.5)
abline(h=0)

qqnorm(residuals(lmod1),main = "",cex.lab=1.5,cex.axis=1.5)
qqline(residuals(lmod1))

#and more... 

######################################################Calculating SEs#######################################
