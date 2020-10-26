dat <- data.frame(HW3PV_data)

model <- lm(score ~ -1 + as.factor(stage) + as.factor(form) + as.factor(cond) + as.factor(risk) + as.factor(unit) + as.factor(prog) + as.factor(BMP) + as.factor(degflex),dat)
summary(model)
modelmat <- model.matrix(model); fix(modelmat)

#plot error against fitted
plot(fitted(model),residuals(model))
abline(h = 0)


#plot sqrt(abs(error)) against fitted
plot(fitted(model), sqrt(abs(residuals(model))))


#regress sqrt(abs(error)) on fitted
summary(model.test <- lm(sqrt(abs(residuals(model))) ~ fitted(model)) )

plot.default(as.factor(dat$stage),residuals(model),type="p")

#1f
#produce a qqplot 
qqnorm(residuals(model))
qqline(residuals(model))

#1g
#standardized 
qqnorm(rstandard(model))
qqline(rstandard(model))


#1h
#Shapiro-Wilk test 
shapiro.test(residuals(model))
