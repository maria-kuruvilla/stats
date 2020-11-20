acc <- stats_speed_acc_latency$`90_acc`
lat <- stats_speed_acc_latency$latency
temp <- stats_speed_acc_latency$Temperature
gs <- stats_speed_acc_latency$Groupsize

n <- length(stats_speed_acc_latency$latency)
data <- as.data.frame(cbind(sample = (1:n),temp,gs,acc,lat))
model_lat <- lm(lat ~ temp + gs,data)
summary(model_lat)
plot(fitted(model_lat), residuals(model_lat))
qqnorm(residuals(model_lat))
qqline(residuals(model_lat))

model_quad <- lm(lat ~ temp + gs + I(temp^2),data)
summary(model_quad)
plot(fitted(model_quad), residuals(model_quad))
qqnorm(residuals(model_quad))
qqline(residuals(model_quad))

shapiro.test(residuals(model_quad))

model_quad_int <- lm(lat ~ temp*gs + I(temp^2),data)
summary(model_quad_int)
plot(fitted(model_quad_int), residuals(model_quad_int))
qqnorm(residuals(model_quad_int))
qqline(residuals(model_quad_int))

shapiro.test(residuals(model_quad_int))


model_lat_int <- lm(lat ~ temp*gs,data)
summary(model_lat_int)
plot(fitted(model_lat_int), residuals(model_lat_int))
qqnorm(residuals(model_lat_int))
qqline(residuals(model_lat_int))

shapiro.test(residuals(model_lat_int))

require(MASS)

boxcox(model_lat, lambda = seq(-10, 0, 1/10),plotit=TRUE)


model_inv <- lm(acc^(-1) ~ temp + gs,speed_data)
summary(model_inv)
plot(fitted(model_inv), residuals(model_inv))

qqnorm(residuals(model_exp))
qqline(residuals(model_exp))

shapiro.test(residuals(model_exp))

model_glm <- glm.nb(lat ~ gs*temp, data)
summary(model_glm)

model_pois <- glm(lat ~ temp + gs, family = poisson,stats_speed_acc_latency)
summary(model_pois)

model_pois_quad <- glm(lat ~ temp + gs + I(temp^2), family = poisson,stats_speed_acc_latency)
summary(model_pois_quad)
  
