library(tidyverse)
library(foreign)

# loading in the dataset

ppcs2005 <- read.spss("20020-0001-Data.sav", to.data.frame=TRUE)
# ppcs2005 <- read_tsv("ppcs_2005.tsv")

# ppcs2005 <- ppcs2005 %>%
#  filter(v10 == 1)

# filter 
ppcs2005 <- ppcs2005 %>%
  mutate(face_to_face = V10) %>%
  mutate(contact = NA_real_) %>%
  mutate(num_face_to_face = NA_real_)

# civilian_race
ppcs2005 <- ppcs2005 %>% 
  mutate(civilian_race = case_when(
    (V5 == "White only" & V6 == "No") ~ "white", # white and not hispanic
    (V5 == "Black only") ~ "black", # black and black-hispanic
    (V6 == "Yes" & V5 != "Black only") ~ "hispanic", # hispanic
    TRUE ~ "other" # other
  ))
  # )) %>%
  # mutate(civilian_race = factor(civilian_race, 
  #                               levels = c("black",
  #                               "hispanic",
  #                               "other",
  #                               "white")))
summary(ppcs2005$civilian_race)
# black hispanic    other    white 
# 7102     6197     3025    37168 

# civilian_age
ppcs2005 <- ppcs2005 %>% 
  mutate(civilian_age = V4)
summary(ppcs2005$civilian_age)

# civilian_gender
ppcs2005 <- ppcs2005 %>%
  mutate(civilian_gender = case_when((V3 == "Male") ~ 1,
                                     (V3 == "Female") ~ 2
  ))
  # ) %>%
  # mutate(civilian_gender = factor(civilian_gender,
  #                                 levels = c(1,2)))

summary(ppcs2005$civilian_gender)

# civilian_income
ppcs2005 <- ppcs2005 %>%
  mutate(civilian_income = case_when((INCOME == "Less than $20,000 or na") ~ 1,
                                     (INCOME == "$20,000-$49,000") ~ 2,
                                     (INCOME == "$50,000 or more") ~ 3
    
  ))
  #        ) %>%
  # mutate(civilian_income = factor(civilian_income,
  #                                 levels = c(1,2,3)))

summary(ppcs2005$civilian_income)

# civilian_employed
ppcs2005 <- ppcs2005 %>%
  mutate(civilian_employed = case_when(
                              (WORK == "Yes") ~ 1,
                              TRUE ~ 0))
  # )) %>%
  # mutate(civilian_employed = factor(civilian_employed,
  #                                   levels = c(1,0)))
summary(ppcs2005$civilian_employed)

# population_size
ppcs2005 <- ppcs2005 %>%
  mutate(population_size = case_when((PLACE == "Under 100,000/not in a place") ~ 1,
                                     (PLACE == "100,000-499,999") ~ 2,
                                     (PLACE == "500,000-999,999") ~ 3,
                                     (PLACE == "1 million or more") ~ 4,
    
  ))
  # ) %>%
  # mutate(population_size = factor(population_size,
  #                                 levels = c(1,2,3,4)))
summary(ppcs2005$population_size)

# time_of_encounter
ppcs2005 <- ppcs2005 %>%
  mutate(time_of_encounter = case_when(
                              V48 == "Yes" ~ 6,
                              V48 == "No" ~ 3,
                              TRUE ~ NA_real_
  ))
  # )) %>%
  # mutate(time_of_encounter = factor(time_of_encounter,
  #                                   levels = c(1,2,3,4,5,6)))
summary(ppcs2005$time_of_encounter)

# officer_race
ppcs2005 <- ppcs2005 %>%
  mutate(off_black = case_when((V46 == "Black" | V47 == "All Black" | V47 == "Mostly Black") ~ 1,
                               TRUE ~ 0
    )) %>%
  mutate(off_white = case_when((V46 == "White" | V47 == "All White" | V47 == "Mostly White") ~ 1,
                               TRUE ~ 0
                               )) %>% 
  mutate(off_other = case_when((V46 == "Other" | V47 == "All some other race" | V47 == "Mostly some other race") ~ 1,
                               TRUE ~ 0
  )) %>%
  mutate(off_split = case_when(V47 == "Equally mixed" ~ 1,
                               TRUE ~ 0
  )) %>%
  mutate(off_hispanic = 0)
  # )) %>%
  # mutate(off_black = factor(off_black,
  #                           levels = c(0,1))) %>%
  # mutate(off_white = factor(off_white,
  #                           levels = c(0,1))) %>%
  # mutate(off_other = factor(off_other,
  #                           levels = c(0,1)))
summary(ppcs2005$off_black)
summary(ppcs2005$off_white)
summary(ppcs2005$off_other)
summary(ppcs2005$off_split)

# type_of_incident
ppcs2005 <- ppcs2005 %>%
  mutate(type_of_incident = case_when((V27 == "Yes") ~ 2, 
                                      (V29 == "Yes") ~ 3,
                                      (V30 == "Yes") ~ 3,
                                      (V31 == "Yes") ~ 3,
                                      (V32 == "Yes") ~ 3,
                                      (V25 == "Yes") ~ 3,
                                      TRUE ~ NA_real_
    
    
  ))
  # )) %>%
  # mutate(type_of_incident = factor(type_of_incident,
  #                                  levels = c(1,2,3)))
summary(ppcs2005$type_of_incident)

# civilian_behavior
ppcs2005 <- ppcs2005 %>%
  mutate(civilian_behavior = 0)
  # ) %>%
  # mutate(civilian_behavior = factor(civilian_behavior,
  #                                   levels = c(0,1)))
summary(ppcs2005$civilian_behavior)

# alt_foutcomes
ppcs2005 <- ppcs2005 %>%
  mutate(civilian_searched = case_when((V75 == "Yes" | V83 == "Yes") ~ 1,
                                       TRUE ~ 0)) %>%
  mutate(civilian_arrested = case_when((V61 == "Yes" | V82 == "Yes") ~ 1,
                                       TRUE ~ 0)) %>%
  mutate(civilian_guilty_of_illegal = case_when((V56 == "Yes" | V78 == "Yes") ~ 1,
                                       TRUE ~ 0)) %>%
  mutate(civilian_injured = case_when((INJURED == "Yes") ~ 1,
                                                TRUE ~ 0)) %>%
  mutate(excess_force = case_when((EXFORCE == "Yes") ~ 1,
                                      TRUE ~ 0))
  # )) %>%
  # mutate(civilian_searched = factor(civilian_searched,
  #                                   levels = c(0,1))) %>%
  # mutate(civilian_arrested = factor(civilian_arrested,
  #                                   levels = c(0,1))) %>%
  # mutate(civilian_guilty_of_illegal = factor(civilian_guilty_of_illegal,
  #                                   levels = c(0,1)))

summary(ppcs2005$civilian_searched)
summary(ppcs2005$civilian_arrested)
summary(ppcs2005$civilian_guilty_of_illegal)

# force
# ppcs2005 <- ppcs2005 %>%
#   mutate(force = case_when(
#     (V12 == "Yes") ~ 1,
#     TRUE ~ 0
#   )) 

ppcs2005 <- ppcs2005 %>%
  mutate(any_force = case_when(
    (V12 == "Yes") ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(grab_push = case_when(
    (V17 == 'Push or grab') ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(hit_kick = case_when(
    (V18 == 'Kick or hit') ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(point_gun = case_when(
    (V20 == 'Point gun') ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(handcuffed = case_when(
    (FRCHCFF == 'Yes') ~ 1,
    (V81 == 'Yes') ~ 1,
    (V60 == 'Yes') ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(pepper_stun = case_when(
    (V19 == "Use pepper spray") ~ 1,
    TRUE ~ 0
  ))

# year 
ppcs2005 <- ppcs2005 %>%
  mutate(year = 2005)
  # ) %>%
  # mutate(year = factor(year, levels = c(2005)))
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

save(ppcs2005_polished, file = 'ppcs_2005.Rdata')
