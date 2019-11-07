######################################
# Survival Analysis
# Homework 1
# Orange Team 5
# David Andexler
######################################


library(haven)
library(dplyr)
library(ggplot2)

setwd('C:/Users/dande/Desktop/MAIN/Documents/NCState Advanced Analytics/Fall 2019/Fall 3/Survival Analysis/Homework/Homework 1')

hurricane <- read_sas("hurricane.sas7bdat")

# Summary statistics for each type of pump station failure
# Percentage of pumps that survived hurricane
total_survive <- sum(hurricane$survive)/nrow(hurricane)

# Average failure time of pumps for each failure type
avg_failure_time_bygroup <- aggregate(hurricane[, which(colnames(hurricane)=='hour')], list(hurricane$reason), mean)
colnames(avg_failure_time_bygroup) <- c('reason', 'avg_hour')

# Appends percentage of total pumps per failure category
percent_by_reason <- hurricane %>% count(reason)/nrow(hurricane)
avg_failure_time_bygroup <- cbind(avg_failure_time_bygroup, percent_by_reason$n)
colnames(avg_failure_time_bygroup) <- c('reason', 'avg_hour', 'percentage_pumps')

# Testing if the average failure hour is different between the failure types
# One-Way ANOVA

# remove reason = 0, will affect ANOVA
new_hurricane <- hurricane[hurricane$reason != 0,]
# Convert reason to factor
new_hurricane$reason <- as.factor(new_hurricane$reason)

# Create ANOVA model
anova_model <- aov(new_hurricane$hour~new_hurricane$reason)
summary(anova_model)
# F-value: 88.86, p-value: <2e-16
# Reject null hypothesis, conclude that at least one mean is different between groups

# Conduct pair-wise comparison to detect which groups are different
tukey <- TukeyHSD(anova_model)
plot(tukey, las=1)
# Result: Differences in
# 2-1, 3-1, 4-1, 4-2, 4-3

# ANOVA diagnostics
boxplot(new_hurricane$hour~new_hurricane$reason)
plot(anova_model)
# Equal variance and Normality assumptions not met
# Must use non-parametric test

# Kruskal-Wallis non-parametric test for equal means by rank
kruskal_wallis_model <- kruskal.test(new_hurricane$hour~new_hurricane$reason)

# Recommendation: Recommended to use median, recommendation to utilize survival curve differences. 


########################################
# Survival Analysis
########################################

# Create survival function. Tenure is hour and event is survive = 0. Event indicates objects NOT censored, i.e. pumps that failed.
hurricane_survival <- Surv(time = hurricane$hour, event = hurricane$survive == 0)

# Kaplan-Meier Method to estimate survival function
hurricane_km <- survfit(hurricane_surv ~ 1, data = hurricane)
summary(hurricane_km)

# Plot survival function
p <- ggsurvplot(hurricane_km,
           color = "steelblue3", 
           risk.table = TRUE,
           xlab = "Failure Time (Hours)",
           ylab = "Pump Survival Probability",
           legend="right",
           theme = theme_bw(),
           title = "Survival Function for all Pump Stations During Hurricane Katrina"
           )

#p$plot <- p$plot +
 # scale_x_continuous(breaks = sort(c(seq(0, 48, 8))))

#p$table <- p$table + scale_x_continuous(breaks = sort(c(seq(0, 48, 6))))

# Subset hurricane by just failure types
hurricane_only_failures = hurricane[which(hurricane$reason!=0),]

# New survival object for only failures
surv_hurricane_fails <- Surv(time = hurricane_only_failures$hour, event = hurricane_only_failures$survive == 0)

# Stratified analysis to see if there is a difference in the survival functions using Wilcoxon
survdiff(surv_hurricane_fails ~ reason, rho = 1, data = hurricane_only_failures)
# Result: Chisq = 205, df = 3, p<2e-16

# Fits all curves and plots
hurricane_strat = survfit(Surv(time = hour, event = survive == 0) ~ reason, data = hurricane_only_failures)

#  Plot the stratafied survival analysis
ggsurvplot(hurricane_strat, data = hurricane_sub,
           legend.title = "Failure Type",
           legend.labs = c("flood", "motor", "surge", "jammed"),
           xlab = "Failure Time (hours)", 
           ylab = "Pump Survival Probability",
           title="Stratified Survival Analysis of Pumps by Failure Type",
           break.y.by = 0.1,
           legend = "right",
           conf.int = TRUE,
           risk.table = TRUE,
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           palette = "Dark",
           ggtheme = theme_bw()
)




