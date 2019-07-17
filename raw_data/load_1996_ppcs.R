library(tidyverse)
library(asciiSetupReader)
library(foreign)

ppcs_1996 <- read_ascii_setup('06999-0001-Data.txt','06999-0001-Setup.sps')

colnames(ppcs_1996)

#Race and ethnicity
ppcs_1996 <- ppcs_1996 %>%
  mutate(civilian_race = case_when(
    (RESPONDENT_RACE == "White" & RESPONDENT_ETHNICITY == "Non-Hispanic") ~ "white",
    (RESPONDENT_RACE == "Black" ) ~ "black",
    (RESPONDENT_ETHNICITY == "Hispanic" & RESPONDENT_RACE != "Black") ~ "hispanic",
    TRUE ~ "other"
  )) %>% 
  mutate(civilian_race = as.factor(civilian_race))

#Age - same as RESPONDENT_AGE column
ppcs_1996 <- ppcs_1996 %>% 
  mutate(civilian_age = NCVS_RESPONDENT_AGE)


#Gender
ppcs_1996 <- ppcs_1996 %>%
  mutate(civilian_gender = case_when(
    (RESPONDENT_SEX == "Male") ~ 1,
    (RESPONDENT_SEX == "Female") ~ 2
))

#civilian income - can't find
ppcs_1996 <- ppcs_1996 %>%
  mutate(civilian_income = 1) %>%
  mutate(civilian_income = as.factor(civilian_income))

#civilian employed

#population_size

#time of encounter
ppcs_1996 <- ppcs_1996 %>%
  mutate(time_of_encounter = case_when(
    (DAYTIME_OR_NIGHTTIME_WHEN_INCIDENT == "Daytime") ~ 3,
    (DAYTIME_OR_NIGHTTIME_WHEN_INCIDENT == "Nighttime") ~ 6
))
  
#officer_race
ppcs_1996 <- ppcs_1996 %>%
  mutate(officer_race_black = case_when(
    (OFFICERS_WHITE_BLACK_OR_OTHER_RACE == 'All black' | OFFICERS_WHITE_BLACK_OR_OTHER_RACE == 'Mostly black' | OFFICER_WHITE_BLACK_OR_OTHER_RACE == 'Black' ) ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(officer_race_black = as.factor(officer_race_black))

#officer race white:
#includes:
#multiple officers white, mostly white

ppcs_1996 <- ppcs_1996%>%
  mutate(officer_race_white = case_when(
    (OFFICERS_WHITE_BLACK_OR_OTHER_RACE == "All white" | OFFICERS_WHITE_BLACK_OR_OTHER_RACE == "Mostly white" | OFFICER_WHITE_BLACK_OR_OTHER_RACE == "White" ) ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(officer_race_white = as.factor(officer_race_white))

#officer race other
#includes:
#multiple officers (V24B) 
ppcs_1996 <- ppcs_1996 %>%
  mutate(officer_race_other = case_when(
    (OFFICERS_WHITE_BLACK_OR_OTHER_RACE == 'All of some other race' | OFFICERS_WHITE_BLACK_OR_OTHER_RACE == 'Mostly some other race' | OFFICERS_WHITE_BLACK_OR_OTHER_RACE == 'Equally mixed' | OFFICER_WHITE_BLACK_OR_OTHER_RACE == 'Other' ) ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(officer_race_other = as.factor(officer_race_other))

#-------------------------------------------------------------------------------------

#View(data.frame(ppcs_1996$NCVS_RESPONDENT_AGE, ppcs_1996$RESPONDENT_AGE))


#type of incident
view(ppcs_1996$REASON_INVOLVED_IN_TRAFFIC_ACCIDENT)
#there are many columns with reasons for contact with police
#After going through it they are all other stops, not traffic stop, not street stop
#I think this is a crazy way to do this, and there must be a better way. Open to suggestions!!
 

#do gather
ppcs_1996 <- ppcs_1996 %>% 
  gather( key = "incident", value = "occurence", REASON_REPORT_A_CRIME, REASON_ASK_FOR_ASSISTANCE, REASON_PROBLEM_IN_THE_NEIGHBORHOOD,
         REASON_TRAFFIC_PARKING_VIOLATION, REASON_INVOLVED_IN_TRAFFIC_ACCIDENT, REASON_WITNESS_TO_TRAFFIC_ACCIDENT,
         REASON_VICTIM_OF_A_CRIME, REASON_WITNESS_TO_A_CRIME, REASON_CRIME_THOUGHT_INVOLVED_IN, REASON_WHAT_YOU_WERE_DOING_IN_THE_AREA,
         REASON_HAD_WARRANT_FOR_YOUR_ARREST, REASON_CASUAL_ENCOUNTERS, REASON_COMMUNITY_MEETINGS, REASON_SOME_OTHER_REASON,
         REASON_NO_SPECIFIC_REASON) %>%
  mutate(type_of_incident = ifelse(occurence == 'Once' | occurence == "More than once", 3, NA))


#----------------------------------------------------------------------------------------------------
#Civilian behavior
#disobeyed = YOU_DO_INTERFERE_WITH_THE_OFFICER
#threat = YOU_DO_THREATEN_THE_OFFICER
#force = YOU_DO_THREATEN_THE_OFFICER
#hit
#
ppcs_1996 <- ppcs_1996 %>%
  gather(key = "behavior", value = "behavior_occurence", YOU_DO_ASSAULT_OR_ATTACK_THE_OFFICER, YOU_DO_THREATEN_THE_OFFICER, YOU_DO_ARGUE_WITH_THE_OFFICER, YOU_DO_INTERFERE_WITH_THE_OFFICER,
         YOU_DO_BLOCK_OFFICER_S_EXIT_ENTRANCE, YOU_DO_ATTEMPT_TO_ESCAPE_HIDE_EVADE, YOU_DO_RESIST_BEING_HANDCUFFED, YOU_DO_RESIST_PLACED_IN_POL_VEHICLE ) %>% 
  mutate(civilian_behavior = ifelse(behavior_occurence == 'Once' | behavior_occurence == 'More than once', 1, 0)) 

ppcs_1996 <- ppcs_1996 %>%
  spread(incident, occurence) %>%
  mutate(civilian_arrested = case_when(
    (REASON_HAD_WARRANT_FOR_YOUR_ARREST == 'Yes' | HANDCUFF_HAD_WARRANT_FOR_YOUR_ARREST == 'Yes') ~ 1,
    TRUE ~ 0
  ))
view(ppcs_1996$civilian_arrested)
