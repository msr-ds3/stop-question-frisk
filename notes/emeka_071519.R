library(tidyverse)

# loading in the dataset
ppcs2005 <- read_tsv("20020-0001-Data.tsv")

# civilian_race column
# v5 -- race of respondent
# ------------------------------------------------------------
# 1 - white only
# 2 - black only
# 3 - ai/an only
# 4 - asian only
# 5 ha/pi only
# 6 - white-black
# 7 - white-ai/an
# 8 - white-asian
# 9 - white-hawaiian
# 10 - black-ai/an
# 11 - black-asian
# 12 - black-ha/pi
# 13 - ai-asian
# 14 - asian-ha/pi
# 15 - white-black-ai
# 16 - white-black-asian
# 17 - white-ai-asian
# 18 - white-asian-ha
# 19 - 2 or 3 races
# 20 - 4 or 5 races
# v6 - hispanic origin of response
# ------------------------------------------------------------
# 1 - yes 
# 2 - no 
# 8 (M) - missing
ppcs2005 <- ppcs2005 %>% 
  mutate(civilian_race = case_when(
    (v5 == 1 & v6 == 2) ~ "white", # white and not hispanic
    (v5 == 2) ~ "black", # black and black-hispanic
    (v6 == 1 & v5 != 2) ~ "hispanic", # hispanic
    TRUE ~ "other" # other
  )) %>%
  mutate(civilian_race = factor(civilian_race, 
                                levels = c("black",
                                "hispanic",
                                "other",
                                "white")))
summary(ppcs2005$civilian_race)
# black hispanic    other    white 
# 7102     6197     3025    37168 

# civilian_age
ppcs2005 <- ppcs2005 %>% 
  mutate(civilian_age = v4)
summary(ppcs2005$civilian_age)

# civilian_gender
ppcs2005 <- ppcs2005 %>%
  mutate(civilian_gender = v3) %>%
  mutate(civilian_gender = factor(civilian_gender,
                                  levels = c(1,2)))

summary(ppcs2005$civilian_gender)

# civilian_income
ppcs2005 <- ppcs2005 %>%
  mutate(civilian_income = income) %>%
  mutate(civilian_income = factor(civilian_income,
                                  levels = c(1,2,3)))

summary(ppcs2005$civilian_income)

# civilian_employed
ppcs2005 <- ppcs2005 %>%
  mutate(civilian_employed = case_when(
                              work == 1 ~ 1,
                              work == 2 ~ 2, 
                              TRUE ~ NA_real_)) %>%
  mutate(civilian_employed = factor(civilian_employed,
                                    levels = c(1,2)))
summary(ppcs2005$civilian_employed)

# population_size
ppcs2005 <- ppcs2005 %>%
  mutate(population_size = place) %>%
  mutate(population_size = factor(population_size,
                                  levels = c(1,2,3)))
summary(ppcs2005$population_size)

# time_of_encounter
ppcs2005 <- ppcs2005 %>%
  mutate(time_of_encounter = case_when(
                              v48 == 1 ~ 6,
                              v48 == 2 ~ 3,
                              TRUE ~ NA_real_
  )) %>%
  mutate(time_of_encounter = factor(time_of_encounter,
                                    levels = c(1,2,3,4,5,6)))
summary(ppcs2005$time_of_encounter)

# officer_race
ppcs2005 <- ppcs2005 %>%
  mutate(off_black = case_when((v46 == 2 | v47 == 2 | v47 == 5) ~ 1,
                               TRUE ~ 0
    )) %>%
  mutate(off_white = case_when((v46 == 1 | v47 == 1 | v47 == 4) ~ 1,
                               TRUE ~ 0
                               )) %>% 
  mutate(off_other = case_when((v46 == 3 | v47 == 3 | v47 %in% c(6,7)) ~ 1,
                               TRUE ~ 0
  )) %>%
  mutate(off_black = factor(off_black,
                            levels = c(0,1))) %>%
  mutate(off_white = factor(off_white,
                            levels = c(0,1))) %>%
  mutate(off_other = factor(off_other,
                            levels = c(0,1)))
summary(ppcs2005$off_black)
summary(ppcs2005$off_white)
summary(ppcs2005$off_other)

# type_of_incident
ppcs2005 <- ppcs2005 %>%
  mutate(type_of_incident = case_when((reason %in% c(2,3)) ~ 2,
                                      (reason %in% c(1,4:8)) ~ 3
                                        )) %>%
  mutate(type_of_incident = factor(type_of_incident,
                                   levels = c(1,2,3)))
summary(ppcs2005$type_of_incident)

# civilian_behavior
ppcs2005 <- ppcs2005 %>%
  mutate(civilian_behavior = 0) %>%
  mutate(civilian_behavior = factor(civilian_behavior,
                                    levels = c(0)))
summary(ppcs2005$civilian_behavior)

# alt_outcomes
ppcs2005 <- ppcs2005 %>%
  mutate(civilian_searched = case_when((v75 == 1 | v83 == 1) ~ 1,
                                       TRUE ~ 0)) %>%
  mutate(civilian_arrested = case_when((v61 == 1 | v82 == 1) ~ 2,
                                       TRUE ~ 0)) %>%
  mutate(civilian_guilty_of_illegal = case_when((v56 == 1 | v78 == 1) ~ 3,
                                       TRUE ~ 0)) %>%
  mutate(civilian_searched = factor(civilian_searched,
                                    levels = c(0,1))) %>%
  mutate(civilian_arrested = factor(civilian_arrested,
                                    levels = c(0,1))) %>%
  mutate(civilian_guilty_of_illegal = factor(civilian_guilty_of_illegal,
                                    levels = c(0,1)))

summary(ppcs2005$civilian_searched)
summary(ppcs2005$civilian_arrested)
summary(ppcs2005$civilian_guilty_of_illegal)

# year 
ppcs2005 <- ppcs2005 %>%
  mutate(year = 2005) %>%
  mutate(year = factor(year, levels = c(2005)))
summary(ppcs2005$year)

# final dataframe
ppcs2005_polished <- ppcs2005 %>%
  select(civilian_race,
         civilian_age,
         civilian_gender,
         civilian_income,
         civilian_employed,
         population_size,
         time_of_encounter,
         off_black,
         off_white,
         off_other,
         type_of_incident,
         civilian_behavior,
         civilian_searched,
         civilian_arrested,
         civilian_guilty_of_illegal,
         year
         )

save(ppcs2005, file = 'ppcs2005.Rdata')
