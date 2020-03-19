if (!require("pacman")) install.packages("pacman")
pacman::p_load(asciiSetupReader, tidyverse, here)

# loading in the dataset
ppcs_2002 <- read_ascii_setup(here("raw_data", "04273-0001-Data.txt"), here("raw_data", "04273-0001-Setup.sas"))

# filtering setup
ppcs_2002 <- ppcs_2002 %>%
  mutate(face_to_face = FACE_TO_FACE_CONTACT) %>%
  mutate(contact = NA_real_) %>%
  mutate(num_face_to_face = NA_real_)

# civilian_race column
ppcs_2002 <- ppcs_2002 %>% 
  mutate(civilian_race = case_when(
    (RACE_OF_RESPONDENT == "White" & HISPANIC_ORIGIN_OF_RESPONDENT == "No") ~ "white", # white and not hispanic
    (RACE_OF_RESPONDENT == "Black") ~ "black", # black and black-hispanic
    (HISPANIC_ORIGIN_OF_RESPONDENT == "Yes" & RACE_OF_RESPONDENT != "Black") ~ "hispanic", # hispanic
    TRUE ~ "other" # other
  ))

# civilian_age
ppcs_2002 <- ppcs_2002 %>% 
  mutate(civilian_age = AGE_OF_RESPONDENT)

# civilian_gender
ppcs_2002 <- ppcs_2002 %>%
  mutate(civilian_gender = case_when((SEX_OF_RESPONDENT == "Male") ~ 1,
                                     (SEX_OF_RESPONDENT == "Female") ~ 2
  ))

# civilian_income
  ppcs_2002 <- ppcs_2002 %>%
    mutate(civilian_income = case_when(
      (INCOME == "Less than $20,000 or na") ~ 1,  
      (INCOME == "$20,000-$49,999") ~ 2, 
      (INCOME == "$50,000 or more") ~ 3
      ))

# civilian_employed
ppcs_2002 <- ppcs_2002 %>%
  mutate(civilian_employed = case_when(
    (HAD_JOB_OR_WORKED_AT_A_BUSINESS_LAST_WEEK == "Yes") ~ 1,
    TRUE ~ 0))

# population_size
ppcs_2002 <- ppcs_2002 %>%
  mutate(population_size = case_when((SIZE_OF_JURISDICTION_WHERE_RESPONDENT_RESIDED == "Under 100,000/not in a place") ~ 1,
                                     (SIZE_OF_JURISDICTION_WHERE_RESPONDENT_RESIDED == "100,000-499,999") ~ 2,
                                     (SIZE_OF_JURISDICTION_WHERE_RESPONDENT_RESIDED == "500,000-999,999") ~ 3,
                                     (SIZE_OF_JURISDICTION_WHERE_RESPONDENT_RESIDED == "1 million or more") ~ 4
  ))

# time_of_encounter
ppcs_2002 <- ppcs_2002 %>%
  mutate(time_of_encounter = case_when(
    TRAFFIC_STOP_AT_NIGHT == "Yes" ~ 6,
    TRAFFIC_STOP_AT_NIGHT == "No" ~ 3,
    TRUE ~ NA_real_
  ))

# officer_race
ppcs_2002 <- ppcs_2002 %>%
  mutate(off_black = case_when((RACE_OF_OFFICER_BLACK == "Yes" | RACE_OF_OFFICERS == "Mostly black") ~ 1,
                               TRUE ~ 0
  )) %>%
  mutate(off_white = case_when((RACE_OF_OFFICER_WHITE == "Yes" | RACE_OF_OFFICERS == "Mostly white") ~ 1,
                               TRUE ~ 0
  )) %>% 
  mutate(off_other = case_when((RACE_OF_OFFICER_OTHER_RACE == "Yes" | RACE_OF_OFFICERS %in% c("Mostly some other race")) ~ 1,
                               TRUE ~ 0
  )) %>%
  mutate(off_split = case_when((RACE_OF_OFFICERS %in% c("Equal number of each race")) ~ 1,
                               TRUE ~ 0
  )) %>%
  mutate(off_hispanic = 0)

# type_of_incident
ppcs_2002 <- ppcs_2002 %>%
  mutate(type_of_incident = case_when((TRAFFIC_STOP == "Yes") ~ 2,
                                      (TRAFFIC_ACCIDENT == "Yes" | REPORTED_CRIME_OR_PROBLEM_TO_POLICE == "Yes" | POLICE_PROVIDED_ASSISTANCE_OR_SERVICE == "Yes" | SUSPECTED_OF_SOMETHING_BY_POLICE == "Yes" | POLICE_INVESTIGATING_CRIME == "Yes") ~ 3
  ))

# civilian_behavior
ppcs_2002 <- ppcs_2002 %>%
  mutate(civilian_behavior = case_when((DISOBEYED_OR_INTERFERED_WITH_POLICE == "Yes") ~ 1,
                                       (TRIED_TO_GET_AWAY_FROM_POLICE == "Yes") ~ 1,
                                       (PUSHED_GRABBED_OR_HIT_OFFICER == "Yes") ~ 1,
                                       (CHARGED_WITH_RESISTING_ARREST == "Yes") ~ 1,
                                       (ARGUED_CURSED_INSULTED_OR_THREATENED_POLICE == "Yes") ~ 1,
                                       (CHARGED_WITH_ASSAULTING_OFFICER == "Yes") ~ 1,
                                       (RESISTED_BEING_HANDCUFFED_ARRESTED_OR_SEARCHED == "Yes") ~ 1, # possible fix
                                       (OTHER_PHYSICAL_BEHAVIOR_AGAINST_POLICE == "Yes") ~ 1,
                                       TRUE ~ 0
  ))

# alt_outcomes
ppcs_2002 <- ppcs_2002 %>%
  mutate(civilian_searched = case_when((SEARCH_OF_DRIVER_OR_VEHICLE %in% c("Yes")) ~ 1,
                                       TRUE ~ 0)) %>%
  mutate(civilian_searched = case_when((SEARCH_OCCURRED_BEFORE_ARREST %in% c("Yes")) ~ 1,
                                       TRUE ~ 0)) %>%
  mutate(civilian_searched = case_when((OTHER_CONTACT_PERSONAL_SEARCH %in% c("Yes")) ~ 1,
                                       TRUE ~ 0)) %>%
  mutate(civilian_arrested = case_when((ARRESTED_DURING_CONTACT == "Yes") ~ 1,
                                       TRUE ~ 0)) %>%
  mutate(civilian_guilty_of_illegal = case_when((ILLEGAL_DRUGS_FOUND %in% c("Yes")) ~ 1,
                                       TRUE ~ 0)) %>%
  mutate(civilian_guilty_of_illegal = case_when((ILLEGAL_WEAPON_S_FOUND %in% c("Yes")) ~ 1,
                                                TRUE ~ 0)) %>%
  mutate(civilian_guilty_of_illegal = case_when((OPEN_CONTAINER_OF_ALCOHOL_FOUND %in% c("Yes")) ~ 1,
                                                TRUE ~ 0)) %>%
  mutate(civilian_injured = case_when((INJURED_FROM_FORCE %in% c("Yes")) ~ 1,
                                                TRUE ~ 0)) %>%
  mutate(excess_force = case_when((POLICE_USED_OR_THREATENED_EXCESSIVE_FORCE %in% c("Yes")) ~ 1,
                                      TRUE ~ 0))

# force
ppcs_2002 <- ppcs_2002 %>%
  mutate(any_force = case_when(
    (POLICE_USED_OR_THREATENED_FORCE == "Yes") ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(grab_push = case_when(
    (PUSHED_OR_GRABBED_BY_POLICE == 'Yes') ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(hit_kick = case_when(
    (KICKED_OR_HIT_BY_POLICE == 'Yes') ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(point_gun = case_when(
    (POLICE_POINTED_GUN == 'Yes') ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(handcuffed = case_when(
    (HANDCUFFED_DURING_CONTACT == 'Yes') ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(pepper_stun = case_when(
    TRUE ~ 0
  ))

# year 
ppcs_2002 <- ppcs_2002 %>%
  mutate(year = 2002)


# final dataframe
ppcs_2002 <- ppcs_2002 %>%
  select(civilian_race,
         civilian_age,
         civilian_gender,
         civilian_income,
         civilian_employed,
         population_size,
         time_of_encounter,
         off_black,
         off_white,
         off_hispanic,
         off_other,
         off_split,
         type_of_incident,
         civilian_behavior,
         civilian_searched,
         civilian_arrested,
         civilian_guilty_of_illegal,
         civilian_injured,
         excess_force,
         year,
         contact,
         face_to_face,
         num_face_to_face,
         any_force,
         grab_push, 
         hit_kick,
         point_gun,
         handcuffed,
         pepper_stun
  )

save(ppcs_2002, file = 'clean_data/ppcs_2002.RData')

sessionInfo()
