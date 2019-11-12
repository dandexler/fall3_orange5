#hm2-Sunny Qin 11/10/2019

# Needed Libraries for Analysis #
#install.packages("survival")
#install.packages("survminer")
#install.packages("flexsurv")

library(survival)
library(survminer)
library(flexsurv)
library (dplyr)
#load data in
setwd("C:\\Users\\SunnyAhh\\OneDrive\\Documents\\fall3\\survival analysis\\hm1\\Homework1_SA\\")
hurricane<- read.sas7bdat("hurricane.sas7bdat")

View(hurricane)
########Build a model with your target variable being the time to flood events

#create new column "flood" to seperate data to two categories- flood=1 and non-flood=0 
hurricane$flood = ifelse(hurricane$reason== 1, 1, 0)
#View(hurricane)

# Accelerated Failure Time Model #
hurricane.aft.ln <- survreg(Surv(hour, flood == 1) ~ backup + age + bridgecrane + servo + trashrack + slope + elevation, data = hurricane, dist = 'lognormal')
summary(hurricane.aft.ln)

# Interpretation of Parameter Estimates #
(exp(coef(hurricane.aft.ln))-1)*100

# Exponential vs. Weibull #
hurricane.aft.w <- survreg(Surv(hour, flood == 1) ~ backup + age + bridgecrane + servo + trashrack + slope + elevation, data = hurricane, dist = 'weibull')
summary(hurricane.aft.w)

#p-value is very small, so reject, go with fuller model, weibull
#but the plot looks really bad

# Checking Distributions #
#weibull looks good #2
hurricane.aft.w <- flexsurvreg(Surv(hour, flood == 1) ~ backup + age + bridgecrane + servo + trashrack + slope + elevation, data = hurricane, dist = "weibull")

plot(hurricane.aft.w, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "Cumulative Hazard", main = "Weibull Distribution")

#exponential looks bad #4
hurricane.aft.e <- flexsurvreg(Surv(hour, flood == 1) ~ backup + age + bridgecrane + servo + trashrack + slope + elevation, data = hurricane, dist = "exp")

plot(hurricane.aft.e, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "Cumulative Hazard", main = "Exponential Distribution")

#gamma looks the best #1
hurricane.aft.g <- flexsurvreg(Surv(hour, flood == 1) ~ backup + age + bridgecrane + servo + trashrack + slope + elevation, data = hurricane, dist = "gamma")

plot(hurricane.aft.g, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "Cumulative Hazard", main = "Gamma Distribution")

#Log-Logistic Distribution looks ok #3
hurricane.aft.ll <- flexsurvreg(Surv(hour, flood == 1) ~ backup + age + bridgecrane + servo + trashrack + slope + elevation, data = hurricane, dist = "llogis")

plot(hurricane.aft.ll, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "Cumulative Hazard", main = "Log-Logistic Distribution")

#so we choose gamma or weibull?
#goodness of fit tests
hurricane.like.e <- flexsurvreg(Surv(hour, flood == 1) ~backup + age + bridgecrane + servo + trashrack + slope + elevation, data = hurricane, dist = "exp")$loglik
hurricane.like.w <- flexsurvreg(Surv(hour, flood == 1) ~backup + age + bridgecrane + servo + trashrack + slope + elevation, data = hurricane, dist = "weibull")$loglik
hurricane.like.ln <- flexsurvreg(Surv(hour, flood == 1) ~backup + age + bridgecrane + servo + trashrack + slope + elevation, data = hurricane, dist = "lnorm")$loglik
hurricane.like.g <- flexsurvreg(Surv(hour, flood == 1) ~backup + age + bridgecrane + servo + trashrack + slope + elevation, data = hurricane, dist = "gamma")$loglik
hurricane.like.ll <- flexsurvreg(Surv(hour, flood == 1) ~backup + age + bridgecrane + servo + trashrack + slope + elevation, data = hurricane, dist = "llogis")$loglik
hurricane.like.f <- flexsurvreg(Surv(hour, flood == 1) ~backup + age + bridgecrane + servo + trashrack + slope + elevation, data = hurricane, dist = "genf")$loglik

hurricane.pval.e.g <- 1 - pchisq((-2*(hurricane.like.e-hurricane.like.g)), 2)
hurricane.pval.w.g <- 1 - pchisq((-2*(hurricane.like.w-hurricane.like.g)), 1)
hurricane.pval.ln.g <- 1 - pchisq((-2*(hurricane.like.ln-hurricane.like.g)), 1)
hurricane.pval.g.f <- 1 - pchisq((-2*(hurricane.like.g-hurricane.like.f)), 1)
hurricane.pval.ll.f <- 1 - pchisq((-2*(hurricane.like.ll-hurricane.like.f)), 1)

Tests <- c('Exp vs. Gam', 'Wei vs. Gam', 'LogN vs. Gam', 'Gam vs. F', 'LogL vs. F')
hurricane.P_values <- c(hurricane.pval.e.g, hurricane.pval.w.g, hurricane.pval.ln.g, hurricane.pval.g.f, hurricane.pval.ll.f)
cbind(Tests, hurricane.P_values)

#1, exp vs gam, not reject,"1.35183619616175e-05" use exponential model
#[2,] "Wei vs. Gam"  "1"  weibull                 
#[3,] "LogN vs. Gam" "0.00642966868224448"  reject, use full, gamma
#[4,] "Gam vs. F"    "0.156021269377724"   not rejectm use nested, gamma
#[5,] "LogL vs. F"   "0.179312914731771" , not reject, use nested, logL

#use gamma??

#Perform variable selection once you have your optimal distribution
#use backwards selection
# Logistic Regression Model - Backward Selection #

#full model
#used weibull bcz there sint an distribution option gamma
full.model <- survreg(Surv(hour , flood==1) ~ backup + age + bridgecrane + servo + trashrack + slope + elevation, 
                  data = hurricane, dist = "weibull")

#backwards selection
back.model <- step(full.model, direction = "backward")
summary(back.model)

#since "trashrack" has a p value = 0.06 larger than 0.03, i will remove it from the model manually
full.model <- survreg(Surv(hour , flood==1) ~ backup + age + bridgecrane + servo + slope + elevation, 
                      data = hurricane, dist = "weibull")

back.model <- step(full.model, direction = "backward")
summary(back.model)

#and now every variable is significant
# backup + servo + slope

#Include a table of significant variables ranked by p-value. 
#1 slope p=0.00051 
#2 servo p=0.003
#3 backup p=0.028

#now we can trust the p value and its parameters to make interpretation
#aft model
hurricane.aft.ln <- survreg(Surv(hour, flood == 1) ~ backup + servo + slope , data = hurricane, dist = 'weibull')
summary(hurricane.aft.ln)
#now back model and aft model looks the same

#interpretation
# Interpretation of Parameter Estimates #
(exp(coef(hurricane.aft.ln))-1)*100

#for every unit increase in slope, we expect the flood to to happen 5.89% sooner 
#the pumps with servo machanism are expected to experience flood 47% later than if it weren't to have a servo machanism

View(hurricane)

#predicted survival quantiles
hurricane.survprob.75.50.25 <- predict(hurricane.aft.ln, type = "quantile", se.fit = TRUE,
                             p = c(0.25, 0.5, 0.75))
head(hurricane.survprob.75.50.25$fit)

# Predicted Mean Event Time #
hurricane.p.time.mean <- predict(hurricane.aft.ln, type = "response", se.fit = TRUE)
head(hurricane.p.time.mean$fit, n = 10)

# Predicted Survival Probabilities #
hurricane.survprob.actual <- 1 - psurvreg(hurricane$hour,
                                mean = predict(hurricane.aft.ln, type = "lp"),
                                scale = hurricane.aft.ln$scale,
                                distribution = hurricane.aft.ln$dist)
head(hurricane.survprob.actual, n = 10)

hurricane.survprob.10wk <- 1 - psurvreg(10,
                              mean = predict(hurricane.aft.ln, type = "lp"),
                              scale = hurricane.aft.ln$scale,
                              distribution = hurricane.aft.ln$dist)
head(hurricane.survprob.10wk)

# Predicted Change in Event Time #
hurricane.new_time <-  qsurvreg(1 - hurricane.survprob.actual,
                      mean = predict(hurricane.aft.ln, type = "lp") + coef(hurricane.aft.ln)['servo'],
                      scale = hurricane.aft.ln$scale,
                      distribution = hurricane.aft.ln$dist)

hurricane$new_time <- hurricane.new_time
hurricane$diff <- hurricane$new_time - hurricane$hour

head(data.frame(hurricane$hour, hurricane$new_time, hurricane$diff), n = 10)


#subset hurricane dataframe so only shows the ones that didnt survive
hurricane_sub <- hurricane[which(hurricane$hour!=48),]
View(hurricane_sub)

#order by diff descending
hurricane_sub<- arrange(hurricane_sub, -diff)
