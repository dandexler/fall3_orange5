############################################################################################################
#                                  Survival Analysis - Homework 1
#                                              Fall 3
#                                             11/04/19
#                                            Bob Bayer
############################################################################################################

library(haven)
library(survival)
library(survminer)
library(flexsurv)
library(RColorBrewer)
library(dplyr)

hurricane <- read_sas("~/AA502 Analytics - Fall 2019/Survival Analysis/Homework 1/hurricane.sas7bdat", NULL)
View(hurricane)

max(hurricane$hour)
# percentage of survives
percent_survived = table(hurricane$survive)/nrow(hurricane)*100
percent_survived
# percentage of reasons
percent_reasons = table(hurricane$reason)/nrow(hurricane)*100
percent_reasons
barplot(percent_survived, ylim = c(0,100), xlab = 'Failure', ylab="Percent")
title(main = list("Percent Failures", font=2))
barplot(percent_reasons, ylim = c(0,100), xlab = 'Failure Code', ylab="Percent")
title(main = list("Percent Total Failures by Reason", font=2))


# Survival Function - Hurricane Data
# ALL PUMPS
hurricane_surv <- Surv(time = hurricane$hour, event = hurricane$survive == 0)

# Survival equation with everyone rolled up "~1"
hurricane_km <- survfit(hurricane_surv ~ 1, data = hurricane)
summary(hurricane_km)

plot(hurricane_km, main = "Survival Function", xlab = "hour", ylab = "Pump Survival Probability")

ggsurvplot(hurricane_km, data = hurricane, conf.int = TRUE, palette = "blue",
           xlab = "Time (hours)", ylab = "Pump Survival Probability", legend = "none",
           break.y.by = 0.1,  risk.table = TRUE, risk.table.col = "strata")

# Stratified Analysis - Hurricane Data
# Create a subset by remove the reason code of "0" 
# R will throw an error becasue it won't have any data to put confidence bands on
hurricane_sub = hurricane[which(hurricane$reason!='0'),]

# Check For Differences
hurricane_sub_surv <- Surv(time = hurricane_sub$hour, event = hurricane_sub$survive == 0)
survdiff(hurricane_sub_surv ~ reason, rho = 0, data = hurricane_sub)

# Create the stratified object from the subset
hurricane_strat = survfit(Surv(time = hour, event = survive == 0) ~ reason, data = hurricane_sub)
summary(hurricane_strat)

#  Plot the stratafied survival analysis
ggsurvplot(hurricane_strat, data = hurricane_sub,
           legend.title = "Failure Type",
           legend.labs = c("flood", "motor", "surge", "jammed"),
           xlab = "Time (hours)", 
           ylab = "Pump Survival Probability", 
           break.y.by = 0.1,
           conf.int = TRUE,
           risk.table = TRUE,
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           palette = "Dark",
           ggtheme = theme_bw()
)



# Hazard Function - All reasons in one graph
hurricane_km$hp <- hurricane_km$n.event/hurricane_km$n.risk
hurricane_haz <- merge(data.frame(time = seq(1,48,1)), data.frame(time = hurricane_km$time, hp = hurricane_km$hp), by = "time", all = TRUE)
hurricane_haz[is.na(hurricane_haz) == TRUE] <- 0

plot(y = hurricane_haz$hp, x = hurricane_haz$time, main = "Hazard Probability Function", xlab = "Tenure", ylab = "Hazard Probability",
     type = 'l')

# Cumulative hazard
ggsurvplot(hurricane_km, data = hurricane, fun = "cumhaz", conf.int = TRUE, palette = "blue",
           xlab = "hour", ylab = "Cumulative Hazard", legend = "none")

# Hazard function for all pumps overlaid
hurricane_strat$hp <- hurricane_strat$n.event/hurricane_strat$n.risk

ggsurvplot(hurricane_strat, data = hurricane_sub,
           legend.title = "Failure Type",
           legend.labs = c("lood", "motor", "surge", "jammed"),
           legend = "bottom",
           fun = "cumhaz", 
           conf.int = TRUE, 
           xlab = "Time (hours)", 
           ylab = "Cumulative Hazard",
           risk.table = TRUE,
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           palette = "Dark",
           ggtheme = theme_bw()
)

# check pairwise differences
# throws an error
pairwise_survdiff(hurricane_sub_surv ~ reason, rho = 0, data = hurricane_sub)

# Also thorws a fit
pairwise_survdiff(hurricane_surv ~ reason, rho = 0, data = hurricane)
