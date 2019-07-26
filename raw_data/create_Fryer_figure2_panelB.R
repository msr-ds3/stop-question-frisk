library(tidyverse)
library(naniar)
library(scales)

# Load stop and frisk data for 2003-2013
load("sqf_03_13.RData")

hours <- sf_data1 %>%
  # filter out unknown races, unknown genders, ages outside of the range 10-90,
  # and types of id equal to "other"
  filter(race != " " & race != "U" & race != "X" & sex != "Z" & typeofid != "O" &
           age > 10 & age <= 90) %>%
  # recode Black Hispanic as Black, American Indian as Other
  mutate(race = recode_factor(race,"P" = "B", "I" = "Z"),
         # create an any_force_used column - ADDED pf_other TO THIS CALCULATION
         any_force_used = paste(pf_hands, pf_grnd, pf_wall, pf_drwep, pf_ptwep,
                                pf_hcuff,pf_baton, pf_pepsp, sep = ""),
         # create factors from columns
         any_force_used = if_else(grepl("Y",any_force_used), 1, 0),
         timestop = as.numeric(timestop),
         # recode race names for clarity
         race = recode_factor(race, "W" = "White", "B" = "Black",  "Q" ="Hispanic",
                              "A" = "Asian", "Z" = "Other")) %>%
  mutate(hour = str_sub(timestop, 0, -3))



panel_B_white <- hours %>% filter(race == "White") %>%
  group_by(hour) %>% select(hour, any_force_used) %>%
  ungroup() %>% group_by(hour) %>% 
  summarize(avg_force = mean(any_force_used), CI = 2 * (sd(any_force_used)/sqrt(n()))) %>%
  filter(!is.na(hour) & hour != "") %>% 
  mutate(hour = as.numeric(hour)) %>%
  arrange(hour)

panel_B_black <- hours %>% filter(race == "Black") %>%
  group_by(hour) %>% select(hour, any_force_used) %>%
  ungroup() %>% group_by(hour) %>% 
  summarize(avg_force = mean(any_force_used), CI = 2 * (sd(any_force_used)/sqrt(n()))) %>%
  filter(!is.na(hour) & hour != "") %>% 
  mutate(hour = as.numeric(hour)) %>%
  arrange(hour)

ggplot() + 
  geom_line(data = panel_B_white, mapping = aes(x = hour, y = avg_force)) +
  geom_pointrange(data = panel_B_white, aes(x = hour, y = avg_force, 
                                            ymin = avg_force - CI, ymax = avg_force + CI)) +
  geom_line(data = panel_B_black, mapping = aes(x = hour, y = avg_force)) +
  geom_pointrange(data = panel_B_black, aes(x = hour, y = avg_force, 
                                            ymin = avg_force-CI, ymax = avg_force + CI)) +
  ylim(.05, .3) + scale_x_continuous(breaks = c(0:23))