data_startle_speed <- read.csv("../../data/temp_collective/roi/stats_ind_startle_speed_loom.csv")

lm <- lm(max_startle_speed ~ Temperature + Groupsize + Loom, data_startle_speed)
summary(lm)
plot(fitted(lm),residuals(lm))

lm_quad <- lm(max_startle_speed ~ I(Temperature^2) + Groupsize + Loom, data_startle_speed)
summary(lm_quad)
plot(fitted(lm_quad),residuals(lm_quad))

lm_quad_lin <- lm(max_startle_speed ~ I(Temperature^2) + Temperature + Groupsize + Loom, data_startle_speed)
summary(lm_quad_lin)
plot(fitted(lm_quad_lin),residuals(lm_quad_lin))

require(MASS)
boxcox(lm,plotit=TRUE)

lm_quad_lin_trans <- lm(max_startle_speed^(-0.5) ~ I(Temperature^2) + Temperature + Groupsize + Loom, data_startle_speed)
summary(lm_quad_lin_trans)
plot(fitted(lm_quad_lin_trans),residuals(lm_quad_lin_trans))

lm_quad_lin_trans <- lm(log(max_startle_speed) ~ I(Temperature^2) + Temperature + Groupsize + Loom, data_startle_speed)
summary(lm_quad_lin_trans)
plot(fitted(lm_quad_lin_trans),residuals(lm_quad_lin_trans))

qqnorm(residuals(lm_quad_lin_trans))
qqline(residuals(lm_quad_lin_trans))

model_gamma <- glm(max_startle_speed ~ I(Temperature^2) + Temperature + Groupsize + Loom, family = Gamma, data_startle_speed )
summary(model_gamma)

lm <- lm(ratio ~ Temperature + Groupsize + Loom, data_startle_speed)
summary(lm)
plot(fitted(lm),residuals(lm))

