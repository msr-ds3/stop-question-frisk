library(tidyverse)
library(asciiSetupReader)
library(foreign)
library(dplyr)

ppcs_1999 <- read_ascii_setup('03151-0001-Data.txt','03151-0001-Setup.sas')
ppcs_1999 <- ppcs_1999[, !duplicated(colnames(ppcs_1999))]

view(head(ppcs_1999))
#add row number
ppcs_1999 <- ppcs_1999 %>%
  mutate(row = row_number())

#Civilian Race
ppcs_1999 <- ppcs_1999 %>%
  mutate(civilian_race = case_when(
    (RESPONDENT_S_RACE == "White" & RESPONDENT_S_HISPANIC_ORIGIN == "No") ~ "white",
    (RESPONDENT_S_RACE == "Black" ) ~ "black",
    (RESPONDENT_S_HISPANIC_ORIGIN == "Hispanic" & RESPONDENT_S_RACE != "Black") ~ "hispanic",
    TRUE ~ "other"
  )) %>% 
  mutate(civilian_race = as.factor(civilian_race))

#Civilian Age
ppcs_1999 <- ppcs_1999 %>% 
  mutate(civilian_age = RESPONDENT_S_AGE)

#Civilian Gender
ppcs_1999 <- ppcs_1999 %>%
  mutate(civilian_gender = case_when(
    (RESPONDENT_S_SEX == "Male") ~ 1,
    (RESPONDENT_S_SEX == "Female") ~ 2
  ))

#Civilian Income
ppcs_1999 <- ppcs_1999 %>%
  mutate(civilian_income = case_when(
    (INCOME == 'Less than $20,000 or NA')~1,
    (INCOME == '$20,000-49,999')~2,
    (INCOME == '$50,000 or more')~3
  ))

#Civilian Employed
ppcs_1999 <- ppcs_1999 %>%
  mutate(civilian_employed = case_when(
    (WORKED_LAST_WEEK == 'Yes') ~1,
    (WORKED_LAST_WEEK == 'No')~0,
    TRUE ~ NA_real_
     ))
  
    
#Population-size
ppcs_1999 <- ppcs_1999 %>%
  mutate(population_size = case_when(
    (PLACE_SIZE == 'Under 100,000/not a place') ~ 1,
    (PLACE_SIZE == '100,000-499,999') ~ 2,
    (PLACE_SIZE == '500,000-999,999') ~ 3,
    (PLACE_SIZE == '1 million or more')~4,
    TRUE ~ NA_real_
  )) %>%
  mutate(population_size = as.factor(population_size))

#Time of encounter
ppcs_1999 <- ppcs_1999 %>%
  mutate(time_of_encounter = NA)

#officer_race
black <- c('All black', 'Mostly black', 'Black')
white <- c('All white', 'Mostly white', 'White')
other <- c('Some other race', 'All of another race', 'Equally Mixed', 'Mostly another race', 'Other', 'Mix of races')
ppcs_tmp <- ppcs_1999 %>% 
  select(row, OFFICERS_RACES_VEHICLE_STOP, OFFICER_RACE_VEHICLE_STOP, RACE_OF_OFFICERS_USE_OR_THREATEN_FORCE, RACE_OF_OFFICER_USE_OR_THREATEN_FORCE, RACE_OF_OFFICERS_VEHICLE_STOP, RACE_OF_OFFICERS_OTHER_CONTACT) %>%
  gather(key = "officer_race_incidents", value = "officer_race" , OFFICERS_RACES_VEHICLE_STOP, OFFICER_RACE_VEHICLE_STOP, RACE_OF_OFFICERS_USE_OR_THREATEN_FORCE, RACE_OF_OFFICER_USE_OR_THREATEN_FORCE, RACE_OF_OFFICERS_VEHICLE_STOP, RACE_OF_OFFICERS_OTHER_CONTACT ) %>%
  mutate(officer_races = case_when(
    (officer_race %in% black) ~ 'black',
    (officer_race %in% white) ~ 'white',
    (officer_race %in% other) ~ 'other'
  )) %>%
  select(row, officer_races)

ppcs_1999 <- left_join(ppcs_1999, ppcs_tmp, by='row')

ppcs_1999 <- ppcs_1999 %>%
  mutate(off_black = ifelse(officer_races == 'black', 1, 0)) %>%
  mutate(off_white = ifelse(officer_races == 'white', 1, 0)) %>%
  mutate(off_other = ifelse(officer_races == 'other', 1, 0)) 

view(head(ppcs_1999))
#type_of_incident

is_not_missing <- function(x) {
  ifelse(x == "Out of Universe/Missing",0, 1)
}

is_not_missing <- Vectorize(is_not_missing)

#civilian_behavior
colnames(ppcs_1999)
columns <- c(colnames(ppcs_1999)[127:136], colnames(ppcs_1999)[235:243])
columns
ppcs_1999_be <- ppcs_1999 %>%
  mutate_at(vars(columns), is_not_missing) %>%
  select(columns)

View(ppcs_1999_be)

ppcs_1999 <- ppcs_1999 %>%
  mutate(row_sum = rowSums(ppcs_1999_be)) %>%
  mutate(civilian_behavior = case_when(
    (row_sum > 0) ~ 1,
    TRUE ~ 0
  ))
view(ppcs_1999$civilian_behavior)

#alternative outcomes
ppcs_1999 <- ppcs_1999 %>%
  mutate(civilian_search = ifelse(VEHICLE_PERSONAL_SEARCH_CONDUCTED == 'Yes', 1, 0))

ppcs_1999 <- ppcs_1999 %>%
  mutate(civilian_arrested = ifelse(FORCE_USED_AND_RESPONDENT_ARRESTED == 'Yes', 1, 0))

ppcs_1999 <- ppcs_1999 %>% 
  mutate(civilian_guilty_of_illegal = ifelse(ITEMS_FOUND_DURING_VEHICLE_PERSONAL_SEAR == 'Evidence found', 1, 0))

  
tmp <-data.frame(ppcs_1999$ARRESTED_VEHICLE_STOP, ppcs_1999$ARRESTED_USE_OR_THREATEN_FORCE, ppcs_1999$FORCE_USED_AND_RESPONDENT_ARRESTED)
view(tmp)
