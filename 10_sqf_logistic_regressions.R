library(tidyverse)
library(gridExtra)
library(naniar)
library(scales)
library(miceadds)


#load the 2003-2018 dataset
#make sure you set the directory using "setwd" to the "stop-question-frisk" directory before loading the "here" library

load(here("clean_data","sqf_03_13.Rdata"))


#create a new data frame for the logistic regression model by converting the necessary columns to factors and creating new columns

log_data <- sf_data1 %>%
  filter(race != "X" & race != " " & typeofid != " ") %>%
  mutate(race = recode_factor(race,"P" = "B","Q" = "H"),
         any_force_used = paste(pf_hands, pf_grnd, pf_wall, pf_drwep, pf_ptwep,
                                pf_hcuff,pf_baton, pf_pepsp, sep = ""),
         any_force_used = as.factor(if_else(grepl("Y",any_force_used), 1, 0)),
         sex = as.factor(sex),
         inout = as.factor(if_else(grepl("I",inout), 1, 0)),
         timestop = as.numeric(gsub(":","",timestop)),
         daytime = as.factor(ifelse(600<=timestop & timestop<=1700,1,0)),
         ac_incid = as.factor(if_else(grepl("Y",ac_incid), 1, 0)),
         ac_time = as.factor(if_else(grepl("Y",ac_time), 1, 0)),
         offunif = as.factor(if_else(grepl("Y",offunif), 1, 0)),
         othpers = as.factor(if_else(grepl("Y",othpers), 1, 0)),
         cs_objcs = as.factor(if_else(grepl("Y",cs_objcs), 1, 0)),
         cs_descr = as.factor(if_else(grepl("Y",cs_descr), 1, 0)),
         cs_casng = as.factor(if_else(grepl("Y",cs_casng), 1, 0)),
         cs_lkout = as.factor(if_else(grepl("Y",cs_lkout), 1, 0)),
         cs_cloth = as.factor(if_else(grepl("Y",cs_cloth), 1, 0)),
         cs_drgtr = as.factor(if_else(grepl("Y",cs_drgtr), 1, 0)),
         cs_furtv = as.factor(if_else(grepl("Y",cs_furtv), 1, 0)),
         cs_vcrim = as.factor(if_else(grepl("Y",cs_vcrim), 1, 0)),
         cs_bulge = as.factor(if_else(grepl("Y",cs_bulge), 1, 0)),
         cs_other = as.factor(if_else(grepl("Y",cs_other), 1, 0)),
         wepnfnd = paste(contrabn,asltweap,pistol,riflshot,knifcuti,machgun,othrweap, sep = ""),
         wepnfnd = as.factor(if_else(grepl("Y",wepnfnd), 1, 0)),
         pf_pp_spray_baton = paste(pf_pepsp, pf_baton,sep = ""),
         pf_pp_spray_baton = as.factor(ifelse(grepl("Y",pf_pp_spray_baton), "Y", "N")),
         race = recode_factor(race, "W" = "White", "B" = "Black",  "H" ="Hispanic",
                              "A" = "Asian", "I" ="Other", "Z" = "Other"),
         year = as.factor(year),
         pct = as.factor(pct),
         at_least_hands = case_when((pf_hands == "Y" |
                                       pf_wall == "Y" |
                                       pf_hcuff == "Y" |
                                       pf_drwep == "Y" |
                                       pf_grnd == "Y" |
                                       pf_ptwep == "Y" |
                                       pf_pp_spray_baton == "Y") ~ 1,
                                    TRUE ~ 0),
         at_least_wall = case_when(
           (pf_wall == "Y" |
              pf_hcuff == "Y" |
              pf_drwep == "Y" |
              pf_grnd == "Y" |
              pf_ptwep == "Y" |
              pf_pp_spray_baton == "Y") ~ 1,
           (pf_hands == "Y") ~ NA_real_,
           TRUE ~ 0),
         at_least_hcuff = case_when(
           (pf_hcuff == "Y" |
              pf_drwep == "Y" |
              pf_grnd == "Y" |
              pf_ptwep == "Y" |
              pf_pp_spray_baton == "Y") ~ 1,
           (pf_hands == "Y" |
              pf_wall == "Y") ~ NA_real_,
           TRUE ~ 0),
         at_least_drwep = case_when(
           (pf_drwep == "Y" |
              pf_grnd == "Y" |
              pf_ptwep == "Y" |
              pf_pp_spray_baton == "Y") ~ 1,
           (pf_hands == "Y" |
              pf_wall == "Y" |
              pf_hcuff == "Y") ~ NA_real_,
           TRUE ~ 0),
         at_least_grnd = case_when(
           (pf_grnd == "Y" |
              pf_ptwep == "Y" |
              pf_pp_spray_baton == "Y") ~ 1,
           (pf_hands == "Y" |
              pf_wall == "Y" |
              pf_hcuff == "Y" |
              pf_drwep == "Y" ) ~ NA_real_,
           TRUE ~ 0),
         at_least_ptwep = case_when(
           (pf_ptwep == "Y" |
              pf_pp_spray_baton == "Y") ~ 1,
           (pf_hands == "Y" |
              pf_wall == "Y" |
              pf_hcuff == "Y" |
              pf_drwep == "Y" |
              pf_grnd == "Y") ~ NA_real_,
           TRUE ~ 0),
         at_least_pp_spray_baton = case_when(
           (pf_pp_spray_baton == "Y") ~ 1,
           (pf_hands == "Y" |
              pf_wall == "Y" |
              pf_hcuff == "Y" |
              pf_drwep == "Y" |
              pf_grnd == "Y" |
              pf_ptwep == "Y" ) ~ NA_real_,
           TRUE ~ 0))


#model with no control: race as the only predictor and Use of force as the outcome
model_no_cntrl <- glm(any_force_used ~ race,
                      data = log_data,
                      family = "binomial")

#create a new data frame where the observations are only for the white population
log_w <- log_data %>%
  filter(race == "White")

#calculate the white mean to 3 decimal points
white_mean <- round(mean(log_w$any_force_used == 1),3)

#create a data frame with the odds ratios of each race relative to the white mean from the no control model
no_control <- data.frame(Model= "No Control", WhiteMean = as.character(white_mean),
                         Black = exp(coef(model_no_cntrl))[2], Hispanic = exp(coef(model_no_cntrl))[3],
                         Asian = exp(coef(model_no_cntrl))[4], Others = exp(coef(model_no_cntrl))[5])




#model with civilian demographics added as control
model_civilian_demo <- glm(any_force_used ~ race + sex + age + I(age^2),
                           data = log_data,
                           family = "binomial")

#create a data frame with the odds ratios of each race relative to the white mean from the civilian demographics model
civilian_demographics <- data.frame(Model = "+ Civilian Demographics", WhiteMean = "",
                                    Black = exp(coef(model_civilian_demo))[2], Hispanic = exp(coef(model_civilian_demo))[3],
                                    Asian = exp(coef(model_civilian_demo))[4], Others = exp(coef(model_civilian_demo))[5])







