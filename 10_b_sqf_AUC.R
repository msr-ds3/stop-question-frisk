library(tidyverse)
library(gridExtra)
library(naniar)
library(scales)
library(miceadds)
library(pROC)
library(ROCR)
library(here)

#make sure you have run "01_download_sqf_data.sh" and "02_stop_and_frisk_clean_data.R" before running this script (only for the first time. After the first time, you can just load the RData file)

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

#model with precinct and year levels added as controls
model_full_control <- glm(any_force_used ~ race + sex + age + I(age^2) + inout + daytime + ac_incid + ac_time +
                            offunif + typeofid + othpers + cs_bulge + cs_cloth + cs_casng + cs_lkout +
                            cs_descr + cs_drgtr + cs_furtv + cs_vcrim +
                            cs_objcs + cs_other + wepnfnd + pct + year,
                          data = log_data,
                          family = "binomial")

#model without race as a control
model_no_race <- glm(any_force_used ~ sex + age + I(age^2) + inout + daytime + ac_incid + ac_time +
                            offunif + typeofid + othpers + cs_bulge + cs_cloth + cs_casng + cs_lkout +
                            cs_descr + cs_drgtr + cs_furtv + cs_vcrim +
                            cs_objcs + cs_other + wepnfnd + pct + year,
                          data = log_data,
                          family = "binomial")


 

#########################################
#calculating AUC for model_ful_cntrl
#########################################

#drop the missing values and create a new dataframe
new_log_data <- log_data %>%
  drop_na(daytime,race , sex , age, inout, ac_incid, ac_time,
          offunif, typeofid , othpers , cs_bulge , cs_cloth , cs_casng , cs_lkout ,
          cs_descr , cs_drgtr , cs_furtv , cs_vcrim ,
          cs_objcs , cs_other ,pct , year,
          any_force_used,
          wepnfnd)

#create a new data frame with two columns of actual force used and predicted probability of use of force
df <- data.frame(actual = new_log_data$any_force_used,
                 probs = predict(model_full_control, new_log_data, type = "response"))

#create numeric objects for predicted probabilities and actual values
pred_probs <- as.numeric(round(df$probs))
actual_values <- as.numeric(as.character(df$actual))

#calculate the area under the curve
roc_obj <- roc(pred_probs,actual_values)
auc(roc_obj)


#########################################
#calculating AUC for model_no_race
#########################################

#drop the missing values and create a new dataframe
new_data_no_race <- log_data %>%
  drop_na(daytime, sex , age, inout, ac_incid, ac_time,
          offunif, typeofid , othpers , cs_bulge , cs_cloth , cs_casng , cs_lkout ,
          cs_descr , cs_drgtr , cs_furtv , cs_vcrim ,
          cs_objcs , cs_other ,pct , year,
          any_force_used,
          wepnfnd)

#create a new data frame with two columns of actual force used and predicted probability of use of force
df_no_race <- data.frame(actual = new_data_no_race$any_force_used,
                      probs = predict(model_no_race, new_data_no_race, type = "response"))

#create numeric objects for predicted probabilities and actual values
pred_probs_no_race <- as.numeric(round(df_no_race$probs))
actual_values__no_race <- as.numeric(as.character(df_no_race$actual))

#calculate the area under the curve
roc_obj_no_race <- roc(pred_probs_no_race,actual_values_no_race)
auc(roc_obj_no_race)

