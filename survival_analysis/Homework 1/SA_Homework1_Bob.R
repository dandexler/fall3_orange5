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

# Recode the failure modes from numbers
hurricane$reason = as.factor(hurricane$reason)
hurricane$reason = recode(hurricane$reason, "0" = "No Failure", "1" = "Flood", "2" = "Motor", "3" = "Surge", "4"= "Jammed")

############################################################################################################
# SUMMARY STATISTICS
############################################################################################################

# Make sure the greatest hour for the pumps is 48
max(hurricane$hour)

# 1 = Pump Survived
# 0 = Pump Failed

# Percentage of Pumps that survived and failed
# Failed = 58.96%
# Survived = 41.04%
percent_survived = table(hurricane$survive)/nrow(hurricane)*100
round(percent_survived, 2)

# Reason Table
# No Failure = 41.04%
# Flood = 14.94%
# Motor = 14.55%
# Surge = 14.42%
# Jammed = 15.06%
# percentage of reasons
percent_reasons = table(hurricane$reason)/nrow(hurricane)*100
round(percent_reasons, 2)

# Average Failure Time by Group

# No failure 48.00 hrs
# Flood = 26.44 hrs
# Motor = 41.04 hrs
# Surge = 38.83 hrs
# Jammed = 21.94 hrs
avg_failure_time_bygroup <- aggregate(hurricane[, which(colnames(hurricane)=='hour')], list(hurricane$reason), mean)
colnames(avg_failure_time_bygroup) <- c('reason', 'avg_hour')
avg_failure_time_bygroup$avg_hour = round(avg_failure_time_bygroup$avg_hour, 2)
avg_failure_time_bygroup

# Calculate percent failure by reason and concatenate the dataframes
percent_by_reason <- hurricane %>% count(reason)/nrow(hurricane)
avg_failure_time_bygroup <- cbind(avg_failure_time_bygroup, percent_by_reason$n)
colnames(avg_failure_time_bygroup) <- c('reason', 'avg_hour', 'percentage_pumps')
avg_failure_time_bygroup$percentage_pumps = round(avg_failure_time_bygroup$percentage_pumps*100, 2)
avg_failure_time_bygroup

# Not so great bar charts
barplot(percent_survived, ylim = c(0,100), xlab = 'Failure', ylab="Percent")
title(main = list("Percent Failures", font=2))
barplot(percent_reasons, ylim = c(0,100), xlab = 'Failure Code', ylab="Percent")
title(main = list("Percent Total Failures by Reason", font=2))

############################################################################################################
# SURVIVAL OF ALL PUMPS
############################################################################################################

# Survival Function
hurricane_surv <- Surv(time = hurricane$hour, event = hurricane$survive == 0)
hurricane_surv

# Survival equation with everyone rolled up "~1" 
#Kaplan Meier function
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
############################################################################################################
# SURVIVAL OF PUMPS BY REASON
############################################################################################################

# Stratified Analysis - Hurricane Data
# Create a subset by remove the reason code of "0" or "No Failure"
# R will throw an error becasue it won't have any data to put confidence bands on
hurricane_sub = hurricane[which(hurricane$reason!='No Failure'),]

# Check For Differences based on the subset of data with 'No failure' reason removed
# P value < .001 means at least one is statistically different
hurricane_sub_surv <- Surv(time = hurricane_sub$hour, event = hurricane_sub$survive == 0)
survdiff(hurricane_sub_surv ~ reason, rho = 0, data = hurricane_sub)


# Create the stratified object from the subset
hurricane_strat = survfit(Surv(time = hour, event = survive == 0) ~ reason, data = hurricane_sub)
summary(hurricane_strat)

#  Plot the stratafied survival analysis
survplot = ggsurvplot(hurricane_strat, 
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

############################################################################################################
# HAZARD PROBABILITY OF ALL PUMPS
############################################################################################################
# Hazard Function - All reasons in one graph
hurricane_km$hp <- hurricane_km$n.event/hurricane_km$n.risk
hurricane_haz <- merge(data.frame(time = seq(1,48,1)), data.frame(time = hurricane_km$time, hp = hurricane_km$hp), by = "time", all = TRUE)
hurricane_haz[is.na(hurricane_haz) == TRUE] <- 0

# Basic plot for hazard probability function
plot(y = hurricane_haz$hp, 
     x = hurricane_haz$time, 
     main = "Hazard Probability Function", 
     xlab = "Tenure", 
     ylab = "Hazard Probability",
     type = 'l',
     )

# Good Plot to publish
hazard_plot =  ggplot() +
                geom_line(data = hurricane_haz, aes(x = time, y = hp), color = "blue", size = 1.2) +
                theme_bw() + 
                theme(plot.title = element_text(hjust = 0.5), text = element_text(size=18)) +
                ggtitle("Hazard Probability for All Pumps") +
                xlab('Time (hours)') +
                ylab('Hazard Probability')
hazard_plot

############################################################################################################
#  HAZARD FUNCTION FOR EACH REASONS
############################################################################################################

# Hazard Function for individual reasons overlaid in one graph
# I got sick of trying to make it in an elegant way and just made a ton of objects
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

############################################################################################################
#  PAIRWISE DIFFERENCES FOR THE GROUPS
############################################################################################################

# Also brute forcing this one because it is annoying

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


############################################################################################################
#  CUMULATIVE HAZARD FPROBABILITY FOR ALL PUMPS
############################################################################################################
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
############################################################################################################
#  CUMULATIVE HAZARD PROBABILITY FOR EACH REASON
############################################################################################################

# Cumulative Probability for all pumps overlaid
# FOR SOME REASON THIS SCALES THE Y AXIS TO 0-4??
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



