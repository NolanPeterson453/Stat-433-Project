## Library Statements 
library(tidyverse)

## Read in the ACS data for all 5 years 
ACS_15_raw <- read_csv(file = 'https://raw.githubusercontent.com/NolanPeterson453/Stat-433-Project/main/ACS_WI_2015.csv')
ACS_16_raw <- read_csv(file = 'https://raw.githubusercontent.com/NolanPeterson453/Stat-433-Project/main/ACS_WI_2016.csv')
ACS_17_raw <- read_csv(file = 'https://raw.githubusercontent.com/NolanPeterson453/Stat-433-Project/main/ACS_WI_2017.csv')
ACS_18_raw <- read_csv(file = 'https://raw.githubusercontent.com/NolanPeterson453/Stat-433-Project/main/ACS_WI_2018.csv')
ACS_19_raw <- read_csv(file = 'https://raw.githubusercontent.com/NolanPeterson453/Stat-433-Project/main/ACS_WI_2019.csv')

## Select the variables OCCP (occupation), AGEP (age), PWGTP (sample weight)
ACS_15 <- select(ACS_15_raw, OCCP, AGEP, PWGTP, ESR)
ACS_16 <- select(ACS_16_raw, OCCP, AGEP, PWGTP, ESR)
ACS_17 <- select(ACS_17_raw, OCCP, AGEP, PWGTP, ESR)
ACS_18 <- select(ACS_18_raw, OCCP, AGEP, PWGTP, ESR)
ACS_19 <- select(ACS_19_raw, OCCP, AGEP, PWGTP, ESR)

## Add variable YEAR that tells what year observation comes from 
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

## Create a data frame that gives the weighted average age for each occupation in each year 
## and unemployment rate for each occupation by year 
mean_age_occp <- comb_ACS %>% 
  group_by(OCCP, YEAR) %>% 
  mutate(weight = PWGTP / sum(PWGTP)) %>% 
  summarise(mean_age = sum(AGEP * weight, na.rm = TRUE),
            unemployed_rate = (sum(PWGTP[which(ESR == 3)], na.rm = TRUE) / sum(PWGTP, na.rm = TRUE)) * 100 ) 

## Average Unemployment rates by year for all sectors 
mean_age_occp %>% 
  group_by(OCCP) %>% 
  summarise(overall_unemp_rate = mean(unemployed_rate)) %>% 
  View()


## Load in OES data and get the Wisconsin data only ##
OES_16_raw <- read_excel('state_M2016_dl.xlsx',1)%>%
  filter(STATE=="Wisconsin")%>%
  mutate(YEAR=2016)%>%
  select(-ST)
OES_17_raw <- read_excel('state_M2017_dl.xlsx',1)%>%
  filter(STATE=="Wisconsin")%>%
  mutate(YEAR=2017)%>%
  select(-ST)
OES_18_raw <- read_excel('state_M2018_dl.xlsx',1)%>%
  filter(STATE=="Wisconsin")%>%
  mutate(YEAR=2018)%>%
  select(-ST)
OES_19_raw <- read_excel('state_M2019_dl.xlsx',1)%>%
  rename(STATE=area_title,OCC_GROUP=o_group,LOC_Q=loc_quotient)
names(OES_19_raw)<-toupper(names(OES_19_raw))
OES_19_raw <-OES_19_raw%>%
  filter(STATE=="Wisconsin")%>%
  mutate(YEAR=2019)%>%
  select(-AREA_TYPE,-NAICS,-NAICS_TITLE,-I_GROUP,-OWN_CODE,-PCT_TOTAL)
OES_20_raw <- read_excel('state_M2020_dl.xlsx',1)%>%
  rename(STATE=AREA_TITLE,OCC_GROUP=O_GROUP,LOC_Q=LOC_QUOTIENT)%>%
  filter(STATE=="Wisconsin")%>%
  mutate(YEAR=2020)%>%
  select(-AREA_TYPE,-NAICS,-NAICS_TITLE,-I_GROUP,-OWN_CODE,-PCT_TOTAL,-PRIM_STATE)


## Stack all 5 years into one dataframe
total <- rbind(OES_16_raw,OES_17_raw,OES_18_raw,OES_19_raw,OES_20_raw)
