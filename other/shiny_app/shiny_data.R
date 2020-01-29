library(tidyverse)
library(httr)
library(rgdal)


# load the stop and frisk data
load("sqf_03_13.RData")

# get the list of police precincts
load("precinct_shape_file.RData")


log_panel_data <- sf_data1 %>%
  # filter out unknown races, unknown genders, ages outside of the range 10-90,
  # and types of id equal to "other"
  filter(race != " " & race != "U" & race != "X" & sex != "Z") %>%
  # recode Black Hispanic as Black, American Indian as Other
  mutate(race = recode_factor(race,"P" = "B", "I" = "Z"),
         sex = as.factor(if_else(grepl("M",sex), 1, 0)),
         race = recode_factor(race, "W" = "White", "B" = "Black",  "Q" ="Hispanic",
                              "A" = "Asian", "Z" = "Other"))

#Splitting intensity Low and High
low_intensity <- log_panel_data %>%
  mutate(pf_low = paste(pf_hands, pf_wall,pf_hcuff, sep = ""),
         pf_low = if_else(grepl("Y", pf_low), 1, 0))


high_intensity <- log_panel_data %>% 
  mutate(pf_high = paste(pf_grnd, pf_drwep, pf_ptwep,
                         pf_baton, pf_pepsp, sep = ""),
         pf_high = if_else(grepl("Y",pf_high), 1, 0))


model_low <- glm(pf_low ~ race + sex + addrpct,
                      data = low_intensity,
                      family = "binomial", y = FALSE,
                      model = FALSE)

model_high <- glm(pf_high ~ race + sex + addrpct,
                 data = high_intensity,
                 family = "binomial", y = FALSE,
                 model = FALSE)


# create a table of test data with one row for each precinct race combination
# and using the most common outcome for every other field (column) - 
# see the appendix for the summary statistics used for determining majorities
race <- rep(c("White", "Black", "Hispanic", "Asian", "Other"), 154)
sex <- as.factor(rep(c("M", "F"), 385))
addrpct <- rep(police_precincts$Precinct, 10)

toy_data <- data.frame(race, sex, addrpct) %>% 
            mutate(sex = as.factor(if_else(grepl("M",sex), 1, 0)))


# add predictions to this test data using the logistic model
prediction_low <- predict(model_low, toy_data, type = "response")
prediction_high <- predict(model_high, toy_data, type = "response")

low_predictions <- data.frame(toy_data, prediction_low)
high_predictions <- data.frame(toy_data, prediction_high)

#Splitting Intensities into different dataframes given race
prob_low_white <- low_predictions  %>% filter(race == "White")
prob_low_black <- low_predictions  %>% filter(race == "Black")

prob_high_white <- high_predictions  %>% filter(race == "White")
prob_high_black <- high_predictions  %>% filter(race == "Black")


# Join the precinct shape data with the data about the precincts
joint_prop_low_white <- geo_join(police_precincts, low_white, "Precinct", "addrpct")

joint_prop_low_black <- geo_join(police_precincts, prob_low_black, "Precinct", "addrpct")

joint_prop_high_white <- geo_join(police_precincts, prob_high_white, "Precinct", "addrpct")

joint_prop_high_black <- geo_join(police_precincts, prob_high_black, "Precinct", "addrpct")











