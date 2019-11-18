#########################################
# Survival Analysis Homework 1
# Team Orange 5
# Bob Bayer, David Andexler, Sunny Qin,
# Samantha Everett, Andrea Kopaskie
#########################################

library(haven)
library(dplyr)
library(ggplot2)
library(survival)
library(survminer)
library(flexsurv)
library(RColorBrewer)
library(dplyr)

# Set path to hurricane data
setwd('SETYOURDATAPATH')

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

###############################
# Survival Function
###############################

# Survival Function
hurricane_surv <- Surv(time = hurricane$hour, event = hurricane$survive == 0)

# Survival equation with everyone rolled up "~1" 
# Kaplan-Meier method
hurricane_km <- survfit(hurricane_surv ~ 1, data = hurricane)
summary(hurricane_km)

# Basic Plot of the survival function
plot(hurricane_km, main = "Survival Function", xlab = "hour", ylab = "Pump Survival Probability")

# Good plot of the survival Function
survplot_all = ggsurvplot(hurricane_km, data = hurricane, 
                          conf.int = TRUE,
                          conf.int.style = "ribbon",
                          conf.int.fill = "#00BFFF",
                          title = "Survival Probability of All Pumps",
                          palette = "blue",
                          xlab = "Time (hours)", 
                          ylab = "Pump Survival Probability", 
                          legend = "none",
                          ggtheme = theme_bw() + theme(plot.title = element_text(hjust = 0.5), text = element_text(size=18)),
                          break.y.by = 0.1,  
                          #           risk.table = TRUE,
                          #           risk.table.fontsize = 5,
                          #           risk.table.y.text = FALSE,
                          tables.theme = theme_cleantable()
) 
#survplot_all$plot = survplot_all$plot + scale_x_continuous(breaks = c(0,10,20,30,40,48))
survplot_all$plot = survplot_all$plot + 
  geom_vline(xintercept = 48, linetype = "dashed") +
  annotate("text", x = 48, y = .41, label = "max", hjust = -0.5)
survplot_all

####################################
# Survival Curves by Failure Type
####################################

# Stratified Analysis

# Remove failure type 0 (for no failure)
hurricane_sub = hurricane[which(hurricane$reason!='No Failure'),]

# Check for survival curve differences
# P value < .001 means at least one is statistically different
hurricane_sub_surv <- Surv(time = hurricane_sub$hour, event = hurricane_sub$survive == 0)
survdiff(hurricane_sub_surv ~ reason, rho = 0, data = hurricane_sub)
# Result: Chisq=1120, p-value = <2e-16
# Result: At least one curve is different


# Create the stratified object from the subset
hurricane_strat = survfit(Surv(time = hour, event = survive == 0) ~ reason, data = hurricane_sub)
summary(hurricane_strat)

#  Plot the stratafied survival analysis
survplot = ggsurvplot(
  hurricane_strat,
  data = hurricane_sub,
  legend.title = "Failure Type",
  legend.labs = c("Flood", "Motor", "Surge", "Jammed"),
  legend = "bottom",
  title = "Survival Probability of Pumps by Failure Reason",
  xlab = "Time (hours)",
  ylab = "Pump Survival Probability",
  break.y.by = 0.1,
  conf.int = TRUE,
  conf.int.style = "ribbon",
  tables.height = 0.2,
  tables.theme = theme_cleantable(),
  palette = "Dark2",
  #                      risk.table = TRUE,
  #                      risk.table.title = "Number at Risk",
  #                      risk.table.y.text = FALSE,
  ggtheme = theme_bw() + theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 18))
)

survplot

#########################################
# Hazard Function
#########################################

# Hazard Function - All reasons in one graph
hurricane_km$hp <- hurricane_km$n.event/hurricane_km$n.risk
hurricane_haz <- merge(data.frame(time = seq(1,48,1)), data.frame(time = hurricane_km$time, hp = hurricane_km$hp), by = "time", all = TRUE)
hurricane_haz[is.na(hurricane_haz) == TRUE] <- 0

# Basic plot for hazard probability function
plot(
  y = hurricane_haz$hp,
  x = hurricane_haz$time,
  main = "Hazard Probability Function",
  xlab = "Tenure",
  ylab = "Hazard Probability",
  type = 'l',
)

# Good Plot to publish
hazard_plot =  ggplot() +
  geom_line(data = hurricane_haz,
            aes(x = time, y = hp),
            color = "blue",
            size = 1.2) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size =
                                                                      18)) +
  ggtitle("Hazard Probability for All Pumps") +
  xlab('Time (hours)') +
  ylab('Hazard Probability')
hazard_plot

#########################################
#  Hazard Function by Failure Type
#########################################

# Hazard function by failure type overlaid
flood_km = survfit(Surv(time = hour, event = survive == 0) ~ 1, data = hurricane_sub, subset=(reason=='Flood'))
motor_km = survfit(Surv(time = hour, event = survive == 0) ~ 1, data = hurricane_sub, subset=(reason=='Motor'))
surge_km = survfit(Surv(time = hour, event = survive == 0) ~ 1, data = hurricane_sub, subset=(reason=='Surge'))
jammed_km = survfit(Surv(time = hour, event = survive == 0) ~ 1, data = hurricane_sub, subset=(reason=='Jammed'))


flood_km$hp = flood_km$n.event/flood_km$n.risk
motor_km$hp = motor_km$n.event/motor_km$n.risk
surge_km$hp = surge_km$n.event/surge_km$n.risk
jammed_km$hp = jammed_km$n.event/jammed_km$n.risk
flood_haz = merge(data.frame(time = seq(1,48,1)), data.frame(time = flood_km$time, flood_hp = flood_km$hp), by = "time", all = TRUE)
flood_haz[is.na(flood_haz) == TRUE] = 0
motor_haz = merge(data.frame(time = seq(1,48,1)), data.frame(time = motor_km$time, motor_hp = motor_km$hp), by = "time", all = TRUE)
motor_haz[is.na(motor_haz) == TRUE] = 0
surge_haz = merge(data.frame(time = seq(1,48,1)), data.frame(time = surge_km$time, surge_hp = surge_km$hp), by = "time", all = TRUE)
surge_haz[is.na(surge_haz) == TRUE] = 0
jammed_haz = merge(data.frame(time = seq(1,48,1)), data.frame(time = jammed_km$time, jammed_hp = jammed_km$hp), by = "time", all = TRUE)
jammed_haz[is.na(jammed_haz) == TRUE] = 0


hazard_plot_all =  ggplot() +
  geom_line(data = flood_haz, aes(x = time, y = flood_hp, colour = "Flood"),size=1.2) +
  geom_line(data = motor_haz, aes(x = time, y =motor_hp, colour = "Motor"), size=1.2) +
  geom_line(data = surge_haz, aes(x = time, y = surge_hp, colour = "Surge"), size=1.2) +
  geom_line(data = jammed_haz, aes(x = time, y = jammed_hp, colour = "Jammed"), size=1.2) +
  scale_colour_manual("", 
                      values = c("Flood"="#1B9E77", "Motor"="#D95F02", 
                                 "Surge"="#7570B3", "Jammed"="#E7298A")) +
  
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=18), legend.position="bottom") +
  ggtitle("Hazard Probability of Pumps by Failure Reason") +
  xlab('Time (hours)') +
  ylab('Hazard Probability')
hazard_plot_all

#########################################
#  Pairwise comparisons of survival 
#  curves within existing groups "water
# -based failure" and "mechanical 
# failure".
# These existing groups are specified by 
# the Committee.
#########################################

# Committee groupings
# Jammed - Motor
jammed_motor_group = hurricane[which(hurricane$reason==c('Jammed', 'Motor')),]
jammed_motor_surv = Surv(time = jammed_motor_group$hour, event = jammed_motor_group$survive == 0)
survdiff(jammed_motor_surv ~ reason, rho = 0, data = jammed_motor_group)

# Flood - Surge
flood_surge_group = hurricane[which(hurricane$reason==c('Flood', 'Surge')),]
flood_surge_surv = Surv(time = flood_surge_group$hour, event = flood_surge_group$survive == 0)
survdiff(flood_surge_surv ~ reason, rho = 0, data = flood_surge_group)

# other groupings
#Motor - Surge
motor_surge_group = hurricane[which(hurricane$reason==c('Motor', 'Surge')),]
motor_surge_surv = Surv(time = motor_surge_group$hour, event = motor_surge_group$survive == 0)
survdiff(motor_surge_surv ~ reason, rho = 0, data = motor_surge_group)

# Jammed - Surge
jammed_surge_group = hurricane[which(hurricane$reason==c('Jammed', 'Surge')),]
jammed_surge_surv = Surv(time = jammed_surge_group$hour, event = jammed_surge_group$survive == 0)
survdiff(jammed_surge_surv ~ reason, rho = 0, data = jammed_surge_group)

# Jammed - Flood
jammed_flood_group = hurricane[which(hurricane$reason==c('Jammed', 'Flood')),]
jammed_flood_surv = Surv(time = jammed_flood_group$hour, event = jammed_flood_group$survive == 0)
survdiff(jammed_flood_surv ~ reason, rho = 0, data = jammed_flood_group)


#########################################
#  Cumulative hazard probability
#########################################

# Cumulative hazard
ggsurvplot(hurricane_km, 
           data = hurricane, 
           fun = "cumhaz",
           title = "Cumulative Hazard Probability for All Pumps",
           conf.int = TRUE,
           conf.int.style = "ribbon",
           conf.int.fill = "#00BFFF",
           palette = "blue",
           xlab = "Time (hours)", 
           ylab = "Cumulative Hazard Probability", 
           legend = "none",
           ggtheme = theme_bw() + theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 18))
)
#########################################
#  CUMULATIVE HAZARD PROBABILITY FOR EACH #  REASON
#########################################

#########################################
# Cumulative hazard function by each
# failure type overlaid
#########################################

ggsurvplot(hurricane_strat, data = hurricane_sub,
           legend.title = "Failure Type",
           legend.labs = c("Flood", "Motor", "Surge", "Jammed"),
           legend = "bottom",
           title = "Cumulative Hazard Probability of Pumps by Failure Reason",
           fun = "cumhaz", 
           conf.int = TRUE, 
           xlab = "Time (hours)", 
           ylab = "Cumulative Hazard Probability",
           #           risk.table = TRUE,
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           palette = "Dark2",
           ggtheme = theme_bw() + theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 18))
)           

# Issue - length of legend labels in some plots