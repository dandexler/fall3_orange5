library(haven)
library(dplyr)
library(ggplot2)
library(survival)
library(survminer)
library(flexsurv)

setwd('C:/Users/dande/Desktop/MAIN/Documents/NCState Advanced Analytics/Fall 2019/Fall 3/Survival Analysis/Homework/Homework 1')

hurricane <- read_sas("hurricane.sas7bdat")

# Removes H1:H48 variables, survive, and reason2
#1-8, 58, 59 remain
# backup, age, bridgecrane, servo, gear, trashrack, slope, elevation
new_hurricane <- hurricane %>% select(1, 2, 3, 4, 5, 7, 8, 58, 59)

# Accelerated failure time model - LogNormal Distribution
flood.aft.ln <- survreg(Surv(hour, reason == 1) ~ backup + age + bridgecrane + servo + gear + slope + elevation, data = new_hurricane, dist = 'lognormal')

# AFT - Weibull distribution
flood.aft.w <- survreg(Surv(hour, reason == 1) ~ backup + age + bridgecrane + servo + gear + slope + elevation, data = new_hurricane, dist = 'weibull')
summary(flood.aft.w)
# Results: Use Weibull instead of Exponential


# Checking Distributions

# Weibull distribution
flood.aft.w <- flexsurvreg(Surv(hour, reason == 1) ~ backup + age + bridgecrane + servo + gear + slope + elevation, data = new_hurricane, dist = "weibull")

# Weibull cumulative hazard plot
plot(
  flood.aft.w,
  type = "cumhaz",
  ci = TRUE,
  conf.int = FALSE,
  las = 1,
  bty = "n",
  xlab = "hour",
  ylab = "Cumulative Hazard",
  main = "Weibull Distribution"
)
# Result: Good.

# Exponential distribution
flood.aft.e <- flexsurvreg(Surv(hour, reason == 1) ~ backup + age + bridgecrane + servo + gear + slope + elevation, data = new_hurricane, dist = "exp")

# Exponential distribution cumulative hazard plot
plot(
  flood.aft.e,
  type = "cumhaz",
  ci = TRUE,
  conf.int = FALSE,
  las = 1,
  bty = "n",
  xlab = "hour",
  ylab = "Cumulative Hazard",
  main = "Exponential Distribution"
)
# Result: Not good.

# Gamma distribution
flood.aft.g <- flexsurvreg(Surv(hour, reason == 1) ~ backup + age + bridgecrane + servo + gear + slope + elevation, data = new_hurricane, dist = "gamma")

# Gamma cumulative hazard plot
plot(
  flood.aft.g,
  type = "cumhaz",
  ci = TRUE,
  conf.int = FALSE,
  las = 1,
  bty = "n",
  xlab = "hour",
  ylab = "Cumulative Hazard",
  main = "Gamma Distribution"
)
# Result: Decent.

# Log Logistic Distribution
flood.aft.ll <- flexsurvreg(Surv(hour, reason == 1) ~ backup + age + bridgecrane + servo + gear + slope + elevation, data = new_hurricane, dist = "llogis")

# Log Logistic cumulative hazard plot
plot(
  flood.aft.ll,
  type = "cumhaz",
  ci = TRUE,
  conf.int = FALSE,
  las = 1,
  bty = "n",
  xlab = "hour",
  ylab = "Cumulative Hazard",
  main = "Log-Logistic Distribution"
)
# Result: Decent.

# General F Distribution
flood.aft.f <- flexsurvreg(Surv(hour, reason == 1) ~ backup + age + bridgecrane + servo + gear + slope + elevation, data = new_hurricane, dist = "genf")

# General F cumulative hazard plot
plot(
  flood.aft.f,
  type = "cumhaz",
  ci = TRUE,
  conf.int = FALSE,
  las = 1,
  bty = "n",
  xlab = "hour",
  ylab = "Cumulative Hazard",
  main = "General F Distribution"
)
# Result: Not good.

# Log Normal Distribution
flood.aft.ln <- flexsurvreg(Surv(hour, reason == 1) ~ backup + age + bridgecrane + servo + gear + slope + elevation, data = new_hurricane, dist = "lognormal")

# Log Normal cumulative hazard plot
plot(
  flood.aft.ln,
  type = "cumhaz",
  ci = TRUE,
  conf.int = FALSE,
  las = 1,
  bty = "n",
  xlab = "hour",
  ylab = "Cumulative Hazard",
  main = "Log-Normal Distribution"
)
# Result: Decent.

# Goodness of Fit tests
like.e <- flood.aft.e$loglik
like.w <- flood.aft.w$loglik
like.g <- flood.aft.g$loglik
like.ll <- flood.aft.ll$loglik
like.f <- flood.aft.f$loglik
like.ln <- flood.aft.ln$loglik

# Compared nested models to full models with LRT = -2(LogL_nested-LogL_full)
pval.e.g <- 1 - pchisq((-2 * (like.e - like.g)), 2)
pval.w.g <- 1 - pchisq((-2 * (like.w - like.g)), 1)
pval.ln.g <- 1 - pchisq((-2 * (like.ln - like.g)), 1)
pval.g.f <- 1 - pchisq((-2 * (like.g - like.f)), 1)
pval.ll.f <- 1 - pchisq((-2 * (like.ll - like.f)), 1)

# Check if degrees of freedom=3 correct
pval.w.f <- 1 - pchisq((-2 * (like.w - like.f)), 3)

# Reject null = full model has more information
# Fail to reject = nested model has more information
Tests <-
  c('Exp vs. Gam', 'Wei vs. Gam', 'LogN vs. Gam', 'Gam vs. F', 'LogL vs. F', 'Weibull vs. F')
P_values <- c(pval.e.g, pval.w.g, pval.ln.g, pval.g.f, pval.ll.f, pval.w.f)
cbind(Tests, P_values)
# Result: Gamma better than exponential
# Weibull better than Gamma
# Gamma better than LogN
# F better than Gamma
# F better than LogLogistic
# F better than Weibull???
# Result: F is clearly broken. Weibull is selected distribution.


# Backward selection on variables by AIC. Recommendation: Use AIC vs p-values. Still rank by p-values if requested.

flood.aft.w <- survreg(Surv(hour, reason == 1) ~ backup + age + bridgecrane + servo + gear + slope + elevation, data = new_hurricane, dist = "weibull")

back.model <- step(flood.aft.w, direction = "backward")
# To get variable p-values
summary(back.model)
# Result: Variables remaining: intercept, gear, backup, servo, slope

# Final model
flood.aft.w <- survreg(Surv(hour, reason == 1) ~ backup + servo + gear + slope, data = new_hurricane, dist = "weibull")

# Interpretation of coefficients
(exp(coef(flood.aft.w))-1)*100
# Result: if backup system is present, flooding occurs 31.60% later. If servo mechanism present, flooding occurs 37.36% later. If gear box is present, flooding occurs 48.355% later. If slope of surroundingg area increases by 1 unit, flooding occurs 6.08% sooner.




# Predicted Survival Quantiles #
#predicted survival quantiles
hurricane.survprob.75.50.25 <-
  predict(
    flood.aft.w,
    type = "quantile",
    se.fit = TRUE,
    p = c(0.25, 0.5, 0.75)
  )
head(hurricane.survprob.75.50.25$fit)

# Predicted Mean Event Time #
hurricane.p.time.mean <-
  predict(flood.aft.w, type = "response", se.fit = TRUE)
head(hurricane.p.time.mean$fit, n = 10)

# Predicted Survival Probabilities #
hurricane.survprob.actual <- 1 - psurvreg(
  hurricane$hour,
  mean = predict(flood.aft.w, type = "lp"),
  scale = flood.aft.w$scale,
  distribution = flood.aft.w$dist
)
head(hurricane.survprob.actual, n = 10)

hurricane.survprob.10wk <- 1 - psurvreg(
  10,
  mean = predict(flood.aft.w, type = "lp"),
  scale = flood.aft.w$scale,
  distribution = flood.aft.w$dist
)
head(hurricane.survprob.10wk)

# Predicted Change in Event Time #
hurricane.new_time <-  qsurvreg(
  1 - hurricane.survprob.actual,
  mean = predict(flood.aft.w, type = "lp") + coef(flood.aft.w)['servo'],
  scale = flood.aft.w$scale,
  distribution = flood.aft.w$dist
)

hurricane$new_time <- hurricane.new_time
hurricane$diff <- hurricane$new_time - hurricane$hour

head(data.frame(hurricane$hour, hurricane$new_time, hurricane$diff),
     n = 10)


#subset hurricane dataframe so only shows the ones that didnt survive
hurricane_sub <- hurricane[which(hurricane$hour != 48), ]
View(hurricane_sub)

#order by diff descending
hurricane_sub <- arrange(hurricane_sub,-diff)