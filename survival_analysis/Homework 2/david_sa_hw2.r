library(haven)
library(dplyr)
library(ggplot2)
library(survival)
library(survminer)
library(flexsurv)

setwd('C:/Users/dande/Desktop/MAIN/Documents/NCState Advanced Analytics/Fall 2019/Fall 3/Survival Analysis/Homework/Homework 1')

hurricane <- read_sas("hurricane.sas7bdat")
hurricane <- cbind("index"=1:nrow(hurricane), hurricane)

# Removes H1:H48 variables, survive, and reason2
#1-8, 58, 59 remain
# backup, age, bridgecrane, servo, gear, trashrack, slope, elevation
new_hurricane <- hurricane %>% select(1, 2, 3, 4, 5, 6, 7, 8, 9, 59, 60)



#############################################
# Distributions
#############################################


# Accelerated failure time model - LogNormal Distribution
flood.aft.ln <- survreg(Surv(hour, reason == 1) ~ backup + age + bridgecrane + trashrack + servo + gear + slope + elevation, data = new_hurricane, dist = 'lognormal')

# AFT - Weibull distribution
flood.aft.w <- survreg(Surv(hour, reason == 1) ~ backup + age + bridgecrane + trashrack + servo + gear + slope + elevation, data = new_hurricane, dist = 'weibull')
summary(flood.aft.w)
# Results: Use Weibull instead of Exponential


# Checking Distributions

# Weibull distribution
flood.aft.w <- flexsurvreg(Surv(hour, reason == 1) ~ backup + age + bridgecrane+ trashrack + servo + gear + slope + elevation, data = new_hurricane, dist = "weibull")

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
  main = "Cumulative Hazard Plot for Flood Failure Type",
  col= "steelblue3"
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

flood.aft.w <- survreg(Surv(hour, reason == 1) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = new_hurricane, dist = "weibull")

back.model <- step(flood.aft.w, direction = "backward")
# To get variable p-values
summary(back.model)
# Result: Variables remaining: intercept, gear, backup, servo, slope, trashrack


###########################################
# Final Model
###########################################


# Final model
flood.aft.w.final <- survreg(Surv(hour, reason == 1) ~ backup + servo + gear+ trashrack + slope, data = new_hurricane, dist = "weibull")

# Interpretation of coefficients
(exp(coef(flood.aft.w.final))-1)*100
# Result: if backup system is present, flooding occurs 29.0087% later. If servo mechanism present, flooding occurs 36.95992% later. If gear box is present, flooding occurs 52.2502% later. If slope of surrounding area increases by 1 unit, flooding occurs 6.09% sooner. If trashrack is present, flooding occurs 21.74% sooner.


############################################
# Event Time Prediction
############################################


# Predicted Mean Event Time 
hurricane.p.time.mean <-
  predict(flood.aft.w.final, type = "response", se.fit = TRUE)

head(hurricane.p.time.mean$fit, n = 10)

# Predicted Survival Probabilities #
hurricane.survprob.actual <- 1 - psurvreg(
  new_hurricane$hour,
  mean = predict(flood.aft.w.final, type = "lp"),
  scale = flood.aft.w.final$scale,
  distribution = flood.aft.w.final$dist
)
head(hurricane.survprob.actual, n = 10)

# Predicted Change in Event Time

# Predicts change in event time if flooded pump had gear
hurricane.new_time.gear <-  qsurvreg(
  1 - hurricane.survprob.actual,
  mean = predict(flood.aft.w.final, type = "lp") + coef(flood.aft.w.final)['gear'],
  scale = flood.aft.w.final$scale,
  distribution = flood.aft.w.final$dist
)

# Predicts change in event time if flooded pump had servo
hurricane.new_time.servo <-  qsurvreg(
  1 - hurricane.survprob.actual,
  mean = predict(flood.aft.w.final, type = "lp") + coef(flood.aft.w.final)['servo'],
  scale = flood.aft.w.final$scale,
  distribution = flood.aft.w.final$dist
)

# Predicts change in event time if flooded pump had backup
hurricane.new_time.backup <-  qsurvreg(
  1 - hurricane.survprob.actual,
  mean = predict(flood.aft.w.final, type = "lp") + coef(flood.aft.w.final)['backup'],
  scale = flood.aft.w.final$scale,
  distribution = flood.aft.w.final$dist
)

# Predicted change in event time if flooded pump had trashrack
hurricane.new_time.trashrack <-  qsurvreg(
  1 - hurricane.survprob.actual,
  mean = predict(flood.aft.w.final, type = "lp") + coef(flood.aft.w.final)['trashrack'],
  scale = flood.aft.w.final$scale,
  distribution = flood.aft.w.final$dist
)


#############################################################
# Change in predicted times for gear
#############################################################

gear_upgrade_cost <- 75000

# Copy dataframe
hurricane_gear_final <- data.frame(new_hurricane)

# Create vector with new predicted failure time
hurricane_gear_final$new_time <- hurricane.new_time.gear

# Difference between actual failure time and predicted failure time
hurricane_gear_final$diff <- hurricane_gear_final$new_time - hurricane_gear_final$hour

# New df with only reason, gear, actual_fail (hour), pred_fail (new_time), diff
gear_diff <- tibble('reason'=hurricane_gear_final$reason, 'gear'=hurricane_gear_final$gear, 'actual_fail'= hurricane_gear_final$hour, 'pred_fail'=hurricane_gear_final$new_time, 'diff'=hurricane_gear_final$diff, 'cost_per_hour'=gear_upgrade_cost/hurricane_gear_final$diff, "index"=hurricane_gear_final$index)

# Removes all fail types that are not flooding. Removes pumps that did not fail. Arranges by descending diff
gear_diff <- gear_diff %>% filter(reason == 1,  gear == 0) %>% arrange(cost_per_hour)
gear_diff['cumulative_cost']<- seq(from=75000, to=8025000, by = 75000)
gear_diff <- gear_diff %>% filter(cumulative_cost <= 2500000)
gear_diff

setwd('C:/Users/dande/Desktop/MAIN/Documents/NCState Advanced Analytics/Fall 2019/Fall 3/Survival Analysis/Homework/Homework 2/')
write.csv(x = gear_diff, file = "gear_diff_final.csv")


#############################################################
# Change in predicted times for servo
#############################################################

servo_upgrade_cost <- 150000

# Copy dataframe
hurricane_servo_final <- data.frame(new_hurricane)

# Create vector with new predicted failure time
hurricane_servo_final$new_time <- hurricane.new_time.servo

# Difference between actual failure time and predicted failure time
hurricane_servo_final$diff <- hurricane_servo_final$new_time - hurricane_servo_final$hour

# New df with only reason, servo, actual_fail (hour), pred_fail (new_time), diff
servo_diff <- tibble('reason'=hurricane_servo_final$reason, 'servo'=hurricane_servo_final$servo, 'actual_fail'= hurricane_servo_final$hour, 'pred_fail'=hurricane_servo_final$new_time, 'diff'=hurricane_servo_final$diff, 'cost_per_hour'=servo_upgrade_cost/hurricane_servo_final$diff, "index"=hurricane_servo_final$index)

# Removes all fail types that are not flooding. Removes pumps that did not fail. Arranges by descending diff
servo_diff <- servo_diff %>% filter(reason == 1,  servo == 0) %>% arrange(cost_per_hour)
servo_diff['cumulative_cost']<- seq(from=150000, to=16050000, by = 150000)
servo_diff <- servo_diff %>% filter(cumulative_cost <= 2500000)
servo_diff

setwd('C:/Users/dande/Desktop/MAIN/Documents/NCState Advanced Analytics/Fall 2019/Fall 3/Survival Analysis/Homework/Homework 2/')
write.csv(x = servo_diff, file = "servo_diff_final.csv")


#############################################################
# Change in predicted times for backup
#############################################################

backup_upgrade_cost <- 100000

# Copy dataframe
hurricane_backup_final <- data.frame(new_hurricane)

# Create vector with new predicted failure time
hurricane_backup_final$new_time <- hurricane.new_time.backup

# Difference between actual failure time and predicted failure time
hurricane_backup_final$diff <- hurricane_backup_final$new_time - hurricane_backup_final$hour

# New df with only reason, backup, actual_fail (hour), pred_fail (new_time), diff
backup_diff <- tibble('reason'=hurricane_backup_final$reason, 'backup'=hurricane_backup_final$backup, 'actual_fail'= hurricane_backup_final$hour, 'pred_fail'=hurricane_backup_final$new_time, 'diff'=hurricane_backup_final$diff, 'cost_per_hour'=backup_upgrade_cost/hurricane_backup_final$diff, "index"=hurricane_backup_final$index)

# Removes all fail types that are not flooding. Removes pumps that did not fail. Arranges by descending diff
backup_diff <- backup_diff %>% filter(reason == 1,  backup == 0) %>% arrange(cost_per_hour)
backup_diff['cumulative_cost']<- seq(from=100000, to=10700000, by = 100000)
backup_diff <- backup_diff %>% filter(cumulative_cost <= 2500000)
backup_diff

setwd('C:/Users/dande/Desktop/MAIN/Documents/NCState Advanced Analytics/Fall 2019/Fall 3/Survival Analysis/Homework/Homework 2/')
write.csv(x = backup_diff, file = "backup_diff_final.csv")
