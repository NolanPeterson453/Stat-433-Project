#########################################################
# Data Cleaning and Joining For Stat 433 Final Project  #
#                                                       #
# Data from American Community Survey 2016/2019 and     # 
# Occupational Employment and Wage Survey 2016/2019     #
#                                                       #
# Authors: Taran Katta, Nolan Peterson, Mark Wu         #
#                                                       #
# Date: November 20th, 2021                             #
#########################################################


## Library Statements 
library(tidyverse)
library(readxl)
library(BBmisc)

## Read in the ACS data for 2016 and 2019 
ACS_16_raw <- read_csv(file = 'https://raw.githubusercontent.com/NolanPeterson453/Stat-433-Project/main/ACS_WI_2016.csv')
ACS_19_raw <- read_csv(file = 'https://raw.githubusercontent.com/NolanPeterson453/Stat-433-Project/main/ACS_WI_2019.csv')

## Select the variables OCCP (occupation), AGEP (age), PWGTP (sample weight)
ACS_16 <- select(ACS_16_raw, OCCP, AGEP, PWGTP, ESR)
ACS_19 <- select(ACS_19_raw, OCCP, AGEP, PWGTP, ESR)

## Add variable YEAR that tells what year observation comes from 
ACS_16 <- mutate(ACS_16, YEAR = rep(2016, length(PWGTP)))
ACS_19 <- mutate(ACS_19, YEAR = rep(2019, length(PWGTP)))

## Fixing column types for easy binding 
ACS_16 <- lapply(ACS_16, as.numeric)
ACS_19 <- lapply(ACS_19, as.numeric)

## Combine the data 
comb_ACS <- bind_rows(ACS_16, ACS_19)

## Convert OCCP back to character
comb_ACS$OCCP <- as.character(comb_ACS$OCCP)

## Create a data frame that gives the weighted average age for each occupation in each year 
## and unemployment rate for each occupation by year 
mean_age_occp <- comb_ACS %>% 
  group_by(OCCP, YEAR) %>% 
  mutate(weight = PWGTP / sum(PWGTP)) %>% 
  summarise(mean_age = sum(AGEP * weight, na.rm = TRUE),
            unemployed_rate = (sum(PWGTP[which(ESR == 3)], na.rm = TRUE) / 
                                 sum(PWGTP, na.rm = TRUE)) * 100 ) 

## Load in OES data and get the Wisconsin data only ##
OES_16_raw <- read_excel('state_M2016_dl.xlsx',1)%>%
  filter(STATE=="Wisconsin")%>%
  mutate(YEAR=2016)%>%
  select(-ST)

OES_19_raw <- read_excel('state_M2019_dl.xlsx',1)%>%
  rename(STATE=area_title,OCC_GROUP=o_group,LOC_Q=loc_quotient)
names(OES_19_raw)<-toupper(names(OES_19_raw))
OES_19_raw <-OES_19_raw%>%
  filter(STATE=="Wisconsin")%>%
  mutate(YEAR=2019)%>%
  select(-AREA_TYPE,-NAICS,-NAICS_TITLE,-I_GROUP,-OWN_CODE,-PCT_TOTAL)


## Stack both years into one data frame
comb_OES <- rbind(OES_16_raw, OES_19_raw)

## Function to create header
header.true <- function(df){
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

## Read in crosswalk code key data and format it
key <- read_excel('nem-occcode-acs-crosswalk.xlsx',1)[-c(1,2,3), ] %>% 
  header.true() %>% 
  rename(OCC_CODE = 'Hybrid SOC Code',
         OCCP = 'ACS Code')


## Join Key and OES data
comb_OES_KEY <- comb_OES %>%
  left_join(key, by = "OCC_CODE") %>%
  mutate(OCCP = OCCP)

## Join OES and ACC data frames 
comb_ACS_OES_KEY <- mean_age_occp %>%
  left_join(comb_OES_KEY, by = c("OCCP", "YEAR"))

## Select the useful variables 
comb_ACS_OES_KEY <- comb_ACS_OES_KEY %>% 
  select(YEAR, OCCP, mean_age, unemployed_rate,
         OCC_CODE, OCC_TITLE, TOT_EMP, EMP_PRSE, 
         JOBS_1000, H_MEAN, A_MEAN, MEAN_PRSE)

## Filter out rows where sample size is too small to collect unemployment data
## And filter out rows where OES and ACS have no comparison 
comb_ACS_OES_KEY <- comb_ACS_OES_KEY %>% 
  filter(unemployed_rate != 0,
         unemployed_rate != 100,
         !is.na(OCC_TITLE))

## Assign NA to missing values in the data
## Assign data types to numeric and factor variables 
## Remove NA's from data
comb_ACS_OES_KEY[comb_ACS_OES_KEY == "*" | comb_ACS_OES_KEY == "**"] <- NA
comb_ACS_OES_KEY[,7:12] <- as.data.frame(sapply(comb_ACS_OES_KEY[,7:12], as.numeric))
comb_ACS_OES_KEY <- comb_ACS_OES_KEY %>% 
  filter(!is.na(OCCP))
comb_ACS_OES_KEY$YEAR <- as.factor(comb_ACS_OES_KEY$YEAR)

## Reconcile difference in acs coding system oes coding system
cleaned_data <- comb_ACS_OES_KEY %>% 
  group_by(YEAR, OCCP) %>% 
  summarise(mean_age = mean(mean_age, na.rm = TRUE),
            unemployed_rate = mean(unemployed_rate, na.rm = TRUE),
            total_employ = sum(TOT_EMP, na.rm = TRUE),
            employ_prse = mean(EMP_PRSE, na.rm = TRUE),
            jobs_per_1000 = sum(JOBS_1000, na.rm = TRUE),
            mean_hourly = mean(H_MEAN, na.rm = TRUE),
            mean_annual = mean(A_MEAN, na.rm = TRUE),
            mean_prse = mean(MEAN_PRSE, na.rm = TRUE))

## Create a clean 2016 data
cleaned_data_2016 <- cleaned_data %>% 
  filter(YEAR == 2016 )

## Create a clean 2019 data
cleaned_data_2019 <- cleaned_data %>% 
  filter(YEAR == 2019 )

## Join these two data set to get Year specific columns (x = 2016, y = 2019)
## could possibly be done with 'pivot_wider' but I had no luck
## Filter out where no jobs in 2016
cleaned_data <- inner_join(cleaned_data_2016, cleaned_data_2019, by = "OCCP") %>% 
  filter(jobs_per_1000.x != 0)

  
## Create comparison variables 
cleaned_data <- cleaned_data %>% 
  mutate(emp_growth = (jobs_per_1000.y / jobs_per_1000.x) * (1/(3 - 1)), 
         wage_growth = (mean_annual.y / mean_annual.x) * (1/(3 - 1)))

## Row 79 is a huge outlier in employment growth 
cleaned_data <- cleaned_data[-79,]

## Create indicator for shortage (0 = no shortage, 1 shortage)
cleaned_data <- cleaned_data %>% 
  mutate(shortage_ind = ifelse(test = (  (emp_growth > mean(emp_growth)) &
                                      (wage_growth > mean(wage_growth)) &
                                     (unemployed_rate.y <  mean(unemployed_rate.y))),
                           yes = 1,
                           no = 0),
         shortage = ifelse(test = (shortage_ind == 1), 
                           yes = "Shortage",
                           no = "No Shortage"),
         severity = ifelse(test = (shortage_ind == 1),
                           yes = normalize((mean(unemployed_rate.y) - unemployed_rate.y) +
                                (wage_growth - mean(wage_growth))),
                           no = 0))

## Model of shortage predicted by age 
model <- glm(shortage_ind ~ mean_age.x, family = binomial, data = cleaned_data)
summary(model)

## Visualize the relationship
cleaned_data %>% 
  ggplot(aes(y = mean_age.x, color = shortage)) +
  geom_boxplot() +
  facet_wrap(~ shortage) + 
  labs(title = "Occupational Age Averages in 2016 vs Shortages in 2019",
      y = "Occupational Average Ages")

cleaned_data %>% 
  ggplot(aes(x = mean_age.x, fill = shortage)) +
  geom_density(alpha=0.4) +
  labs(title = "Occupational Age Averages Distributions in 2016 by Shortages in 2019",
       x = "Occupational Average Ages")


## No real significant relationship between the severity and average age in 2016
cleaned_data %>% 
  filter(shortage_ind == 1) %>% 
  ggplot(aes(x = mean_age.x, y = severity)) +
  geom_point()
summary(lm(severity ~ mean_age.x, data = cleaned_data[cleaned_data$shortage_ind == 1,]))

## plots looking at each metric individually 
cleaned_data %>% 
  ggplot(aes(x = mean_age.x, y = unemployed_rate.y)) +
  geom_point() +
  geom_hline(yintercept = mean(cleaned_data$unemployed_rate.y))

cleaned_data %>% 
  ggplot(aes(x = mean_age.x, y = emp_growth)) +
  geom_point() +
  geom_hline(yintercept = mean(cleaned_data$emp_growth))

cleaned_data %>% 
  ggplot(aes(x = mean_age.x, y = wage_growth)) +
  geom_point() +
  geom_hline(yintercept = mean(cleaned_data$wage_growth))


################################################################################################


# jobs_plot <- ggplot(data = comb_ACS_OES_KEY, aes(x = mean_age, 
#                                     y = unemployed_rate, 
#                                     color = A_MEAN,
#                                     size = JOBS_1000)) +
#   geom_point() + 
#   labs(title = "Average Age vs Unemployment Rate by Occupation",
#        y = "Umeployment Rate",
#        x = "Mean Age of Workers in Occupation", 
#        color = "Mean Annual Salary of Occupation",
#        size = "Number of Total Jobs per Thousand in Occupation") +
#   scale_color_continuous(breaks=c(0,50000,100000,150000),
#                          labels=c("$0","$50,000","$100,000", "$150,000")) +
#   facet_wrap(~YEAR)
#   
# ggsave('jobs_plot.png', plot = jobs_plot, units = "in", width = 20, height = 10)

