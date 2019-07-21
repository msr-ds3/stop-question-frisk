library(tidyverse)
library(asciiSetupReader)
library(foreign)
library(dplyr)

ppcs_1999 <- read_ascii_setup('03151-0001-Data.txt','03151-0001-Setup.sas')
ppcs_1999 <- ppcs_1999[, !duplicated(colnames(ppcs_1999))]

ppcs_1999 <- ppcs_1999 %>%
  filter(ANY_POLICE_CONTACT_IN_LAST_12_MONTHS == 'Yes' & FACE_TO_FACE_CONTACT == 'Yes')
#add row number
ppcs_1999 <- ppcs_1999 %>%
  mutate(row = row_number())

#Civilian Race
ppcs_1999 <- ppcs_1999 %>%
  mutate(civilian_race = case_when(
    (RESPONDENT_S_RACE == "White" & RESPONDENT_S_HISPANIC_ORIGIN == "No") ~ "white",
    (RESPONDENT_S_RACE == "Black" ) ~ "black",
    (RESPONDENT_S_HISPANIC_ORIGIN == "Yes" & RESPONDENT_S_RACE != "Black") ~ "hispanic",
    TRUE ~ "other"
  ))

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
    TRUE ~ 0
     ))
  
    
#Population-size
ppcs_1999 <- ppcs_1999 %>%
  mutate(population_size = case_when(
    (PLACE_SIZE == 'Under 100,000/not a place') ~ 1,
    (PLACE_SIZE == '100,000-499,999') ~ 2,
    (PLACE_SIZE == '500,000-999,999') ~ 3,
    (PLACE_SIZE == '1 million or more')~4,
    TRUE ~ NA_real_
  ))

#Time of encounter
ppcs_1999 <- ppcs_1999 %>%
  mutate(time_of_encounter = NA)

#officer_race
black <- c('All black', 'Mostly black', 'Black')
white <- c('All white', 'Mostly white', 'White')
other <- c('Some other race', 'All of another race','Mostly another race', 'Other')
split <- c('Equally Mixed','Mix of races')

columns <- cbind(ppcs_1999$OFFICERS_RACES_VEHICLE_STOP, ppcs_1999$OFFICER_RACE_VEHICLE_STOP, ppcs_1999$RACE_OF_OFFICERS_USE_OR_THREATEN_FORCE, ppcs_1999$RACE_OF_OFFICER_USE_OR_THREATEN_FORCE, ppcs_1999$RACE_OF_OFFICERS_VEHICLE_STOP, ppcs_1999$RACE_OF_OFFICERS_OTHER_CONTACT)
ppcs_1999 <- ppcs_1999 %>%
  mutate(off_black = apply(columns, 1, function(x){max(x %in% black)}))

ppcs_1999 <- ppcs_1999 %>%
  mutate(off_white = apply(columns, 1, function(x){max(x %in% white)}))

ppcs_1999 <- ppcs_1999 %>%
  mutate(off_other = apply(columns, 1, function(x){max(x %in% other)}))

ppcs_1999 <- ppcs_1999 %>%
  mutate(off_split = apply(columns, 1, function(x){max(x %in% split)}))

ppcs_1999 <- ppcs_1999 %>%
  mutate(off_hispanic = 0)

#type_of_incident
traffic_stops <- c('Roadside check drunk driver', 'Seat belt', 'Some other traffic offense', 'Vehicle defect', 'Suspected/charged with drinking & driving', 'Speeding')
#ppcs_1999 %>% group_by(REASON_FOR_STOP) %>% summarise(count =n()) %>% view

ppcs_1999 <- ppcs_1999 %>%
  mutate(type_of_incident = case_when(
    (REASON_FOR_STOP %in% traffic_stops) ~ 2,
    (REASON_FOR_STOP == 'Out of Universe/Missing') ~ NA_real_,
    TRUE ~ 3
  ))


#civilian_behavior
is_not_missing <- function(x) {
  ifelse(x == "Out of Universe/Missing",0, 1)
}

is_not_missing <- Vectorize(is_not_missing)

colnames(ppcs_1999)
columns <- c(colnames(ppcs_1999)[127:136], colnames(ppcs_1999)[235:243])
ppcs_1999_be <- ppcs_1999 %>%
  mutate_at(vars(columns), is_not_missing) %>%
  select(columns)


ppcs_1999 <- ppcs_1999 %>%
  mutate(row_sum = rowSums(ppcs_1999_be)) %>%
  mutate(civilian_behavior = case_when(
    (row_sum > 0) ~ 1,
    TRUE ~ 0
  ))


#alternative outcomes
ppcs_1999 <- ppcs_1999 %>%
  mutate(civilian_searched = ifelse(VEHICLE_PERSONAL_SEARCH_CONDUCTED == 'Yes', 1, 0))

ppcs_1999 <- ppcs_1999 %>%
  mutate(civilian_arrested = ifelse(FORCE_USED_AND_RESPONDENT_ARRESTED == 'Yes', 1, 0))

ppcs_1999 <- ppcs_1999 %>% 
  mutate(civilian_guilty_of_illegal = ifelse(ITEMS_FOUND_DURING_VEHICLE_PERSONAL_SEAR == 'Evidence found', 1, 0))

ppcs_1999 <- ppcs_1999 %>%
  mutate(civilian_injured = ifelse(FORCE_USED_AND_RESPONDENT_INJURED == 'Yes', 1, 0))

#perceived excessive force

ppcs_1999 <- ppcs_1999 %>%
  mutate(excess_force = case_when(
  (EXCESSIVE_FORCE_USED_OR_THREATENED_VEH == 'Yes') ~ 1,
  (EXCESSIVE_FORCE_USED_OR_THREATENED_OTHER == 'Yes') ~1,
  TRUE ~ 0
  ))

ppcs_1999 <- ppcs_1999 %>%
  mutate(force = case_when(
    (FORCE_USED_IN_TRAFFIC_STOP_OR_OTHER_CONT == 'Yes') ~ 1,
    (FORCE_USED_AND_RESPONDENT_HANDCUFFED == 'Yes') ~1,
    (PUSHED_OR_GRABBED_WITHOUT_PAIN_VEHICLE == 'Pushed or grabbed without pain') ~1,
    (PUSHED_OR_GRABBED_WITH_PAIN_VEHICLE_ST == 'Pushed or grabbed with pain')~1,
    (PUSHED_OR_GRABBED_WITHOUT_PAIN_OTHER_CON == 'Pushed or grabbed without pain')~1,
    (PUSHED_OR_GRABBED_WITH_PAIN_OTHER_CONTAC == 'Pushed or grabbed with pain')~1,
    (SPRAYED_WITH_CHEMICAL_PEPPER_SPRAY_VEH ==  'Sprayed with chemical/pepper spray' )~1,
    (SPRAYED_WITH_CHEMICAL_PEPPER_SPRAY_OTHER == ' Sprayed with chemical/pepper spray')~1,
    (KICKED_OR_HIT_VEHICLE_STOP == 'Kicked or hit')~1,
    (KICKED_OR_HIT_OTHER_CONTACT == 'Kicked or hit')~1,
    (POINTED_GUN_VEHICLE_STOP == 'Pointed gun')~1,
    (POINTED_GUN_OTHER_CONTACT == 'Pointed gun')~1,
    TRUE ~ 0
  ))

ppcs_1999_cleaned <- ppcs_1999 %>%
  select(civilian_race, civilian_age, civilian_gender, civilian_income, civilian_employed, population_size, time_of_encounter, off_black, off_white, off_other, off_split, off_hispanic, type_of_incident, civilian_behavior, civilian_searched, civilian_arrested, civilian_guilty_of_illegal, civilian_injured, excess_force, force)

ppcs_1999_cleaned <- ppcs_1999_cleaned %>%
  mutate(year = 1999) %>% view

save(ppcs_1999_cleaned, file = 'ppcs_1999.RData')
