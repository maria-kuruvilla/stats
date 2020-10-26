#We have a dataset on morel mushroom population growth as a function of rainfall.
# Morels were measured in 13 years at 8 different forested sites. At each site, 
#5 permanent sampling quadrats
# were established, and each year mushrooms were counted, and the change in population
# size was calculated. In our dataset, HW6_morels.csv, we have year, site, sample, rainfall
# (between count at t and count at t+1), and ????????????????. Note that throughout the next steps, you
# should be aware of whether it is appropriate to use restricted maximum likelihood or
# maximum likelihood for the task at hand.
morel <- HW6_morels
plot(morel$rain,morel$lambda)
plot(morel$site,morel$lambda)
plot(morel$sample,morel$lambda)
plot(morel$year,morel$lambda)

# a. Begin by building a global model including appropriate fixed and random effects.
# Report your fixed parameter estimates and your random parameter estimates in a
# table.
library(lme4)

model1 <- lmer(lambda ~ scale(rain) + (1|year), morel)
model2 <- lmer(lambda ~ scale(rain) + (1|sample), morel)
model3 <- lmer(lambda ~ scale(rain) + (1|site), morel)
model4 <- lmer(lambda ~ scale(rain) + (scale(rain)|site), morel)
model5 <- lmer(lambda ~ scale(rain) + (1|site:sample) + (1|year), morel)
model6 <- lmer(lambda ~ scale(rain) + (1|site) + (1|year), morel)
model7 <- lmer(lambda ~ scale(rain) + (scale(rain)|site) + (1|year), morel)

# b. Lay out step-by-step how you would evaluate the importance of including the
# random effects in this model. What models would you compare in what order, and
# how. Show your code and your result. You can use either exactRLRT() or
# exactLRT() as appropriate - specify which at each step.
require(RLRsim)
#testing effect of site
exactRLRT(model3,model6,model1)

#testing effect of year
exactRLRT(model1,model6,model3)



# c. Once you have settled on a random effects structure, include it in the model and
# evaluate your fixed effects using AIC. Describe how you've done this and report
# your results.

#now look at the fixed effects with AIC 
#slope and intercept random, fixed rain
model6f <- lmer(lambda ~ scale(rain) + (1|site) + (1|year), morel, REML = FALSE) 
#slope and intercept random, no rain
modelf <- lmer(lambda ~ 1 + (1|site) + (1|year), morel, REML = FALSE)

extractAIC(model6f)
extractAIC(modelf)



# d. Provide bootstrapped confidence intervals for fixed and random parameter
# estimates in your top model.
confint(model6,method="boot")
xtable(confint(model6,method="boot"))