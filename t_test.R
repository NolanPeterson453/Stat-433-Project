#########################################################
# T-Test for Difference of Means (Short Vs No Short)    #
#                                                       #
# Data from American Community Survey 2016/2019 and     # 
# Occupational Employment and Wage Survey 2016/2019     #
#                                                       #
# Authors: Taran Katta, Nolan Peterson, Mark Wu         #
#                                                       #
# Date: December 11th, 2021                             #
#########################################################

# H_0: Mean_age_shortage_group = Mean_age_no_shortage_group

# H_a: Mean_age_shortage_group > Mean_age_no_shortage_group

data <- read_csv(file = "https://raw.githubusercontent.com/NolanPeterson453/Stat-433-Project/main/cleaned_data_433.csv")

n_1 <- length(data$OCCP[data$shortage == "Shortage"])

n_2 <- length(data$OCCP[data$shortage == "No Shortage"])

x_bar_1 <- mean(data$mean_age_2016[data$shortage == "Shortage"])

x_bar_2 <- mean(data$mean_age_2016[data$shortage == "No Shortage"])

sd_1 <- sd(data$mean_age_2016[data$shortage == "Shortage"])

sd_2 <- sd(data$mean_age_2016[data$shortage == "No Shortage"])

t_obs <- (x_bar_1 - x_bar_2) / sqrt(sd_1^2 / n_1 + sd_2^2 / n_2) #1.948891

t_crit <- qt(p = 0.05, df = n_1 + n_2 - 2, lower.tail=FALSE) #1.663884

p_value <- pt(q = t_obs, df = n_1 + n_2 - 2, lower.tail=FALSE) #0.02738522

# 95% Confidence intervals for difference in pop means using the t test statistic 

S_p <- sqrt(((n_1 - 1) * sd_1^2 + (n_2 - 1) * sd_2^2  ) / (n_1 + n_2 - 2)) #6.016206

lower <- (x_bar_1 - x_bar_2) - t_obs * S_p * sqrt(1/n_1 + 1/n_2) #-0.6078939

upper <- (x_bar_1 - x_bar_2) + t_obs * S_p * sqrt(1/n_1 + 1/n_2) #6.711236

