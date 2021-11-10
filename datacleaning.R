## Library Statements 
library(tidyverse)

## Read in the ACS data for all 5 years 
ACS_15_raw <- read_csv(file = 'https://raw.githubusercontent.com/NolanPeterson453/Stat-433-Project/main/ACS_WI_2015.csv')
ACS_16_raw <- read_csv(file = 'https://raw.githubusercontent.com/NolanPeterson453/Stat-433-Project/main/ACS_WI_2016.csv')
ACS_17_raw <- read_csv(file = 'https://raw.githubusercontent.com/NolanPeterson453/Stat-433-Project/main/ACS_WI_2017.csv')
ACS_18_raw <- read_csv(file = 'https://raw.githubusercontent.com/NolanPeterson453/Stat-433-Project/main/ACS_WI_2018.csv')
ACS_19_raw <- read_csv(file = 'https://raw.githubusercontent.com/NolanPeterson453/Stat-433-Project/main/ACS_WI_2019.csv')

## Select the variables OCCP (occupation), AGEP (age), PWGTP (sample weight)
ACS_15 <- select(ACS_15_raw, OCCP, AGEP, PWGTP)
ACS_16 <- select(ACS_16_raw, OCCP, AGEP, PWGTP)
ACS_17 <- select(ACS_17_raw, OCCP, AGEP, PWGTP)
ACS_18 <- select(ACS_18_raw, OCCP, AGEP, PWGTP)
ACS_19 <- select(ACS_19_raw, OCCP, AGEP, PWGTP)

## Add variable YEAR that tells what year observation comes from 
## and sample weight as proportion total of year
ACS_15 <- mutate(ACS_15, YEAR = rep(2015, length(PWGTP)))
ACS_16 <- mutate(ACS_16, YEAR = rep(2016, length(PWGTP)))
ACS_17 <- mutate(ACS_17, YEAR = rep(2017, length(PWGTP)))
ACS_18 <- mutate(ACS_18, YEAR = rep(2018, length(PWGTP)))
ACS_19 <- mutate(ACS_19, YEAR = rep(2019, length(PWGTP)))

## Fixing column types for easy binding 
ACS_15 <- lapply(ACS_15, as.numeric)
ACS_16 <- lapply(ACS_16, as.numeric)
ACS_17 <- lapply(ACS_17, as.numeric)
ACS_18 <- lapply(ACS_18, as.numeric)
ACS_19 <- lapply(ACS_19, as.numeric)

## Combine the data 
comb_ACS <- bind_rows(ACS_15, ACS_16, ACS_17, ACS_18, ACS_19)

## Convert OCCP back to character
comb_ACS$OCCP <- as.character(comb_ACS$OCCP)

## Create a data frame that give the weighted average age for each occupation in each year
mean_age_occp <- comb_ACS %>% 
  group_by(OCCP, YEAR) %>% 
  mutate(weight = PWGTP / sum(PWGTP)) %>% 
  summarise(mean_age = sum(AGEP * weight, na.rm = TRUE)) 


## Need to load in OES data and clean ##
