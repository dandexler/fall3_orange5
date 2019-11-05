######################################
# Survival Analysis
# Homework 1
# Orange Team 5
# David Andexler
######################################


library(haven)
library(dplyr)
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



