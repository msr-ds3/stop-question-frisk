library(pacman)
library(tidyverse)
library(gridExtra)
library(naniar)
library(scales)
library(miceadds)
library(here)


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







#model with encounter characteristics added as control
model_encnt_char <- glm(any_force_used ~ race + sex + I(age^2)
                        + inout + daytime + ac_incid + ac_time +
                          offunif + typeofid + othpers,
                        data = log_data,
                        family = "binomial")

#create a data frame with the odds ratios of each race relative to the white mean from the encounter characteristics model
encounter_characteristics <- data.frame(Model = "+ Encounter Characteristics", WhiteMean = "",
                                        Black = exp(coef(model_encnt_char))[2],
                                        Hispanic = exp(coef(model_encnt_char))[3],
                                        Asian = exp(coef(model_encnt_char))[4],
                                        Others = exp(coef(model_encnt_char))[5])







#model with civilian behavior added as control
model_civ_behv <- glm(any_force_used ~ race + sex + I(age^2) + inout + daytime + ac_incid + ac_time +
                        offunif + typeofid + othpers + cs_bulge + cs_cloth + cs_casng + cs_lkout +
                        cs_descr + cs_drgtr + cs_furtv + cs_vcrim +
                        cs_objcs + cs_other + wepnfnd,
                      data = log_data,
                      family = "binomial")

#create a data frame with the odds ratios of each race relative to the white mean from the civilian behavior model
civilian_behavior <- data.frame(Model = "+ Civilian Behavior", WhiteMean = "", Black = exp(coef(model_civ_behv))[2],
                                Hispanic = exp(coef(model_civ_behv))[3],
                                Asian = exp(coef(model_civ_behv))[4],
                                Others = exp(coef(model_civ_behv))[5])





#model with precinct and year levels added as controls
model_full_control <- glm(any_force_used ~ race + sex + age + I(age^2) + inout + daytime + ac_incid + ac_time +
                            offunif + typeofid + othpers + cs_bulge + cs_cloth + cs_casng + cs_lkout +
                            cs_descr + cs_drgtr + cs_furtv + cs_vcrim +
                            cs_objcs + cs_other + wepnfnd + pct + year,
                          data = log_data,
                          family = "binomial")

#create a data frame with the odds ratios of each race relative to the white mean from the full control model
full_control <- data.frame(Model = "(+) Precinct, Year", WhiteMean = "", Black = exp(coef(model_full_control))[2],
                           Hispanic = exp(coef(model_full_control))[3],
                           Asian = exp(coef(model_full_control))[4],
                           Others = exp(coef(model_full_control))[5])




#creating a dataframe with our results
our_results <- bind_rows(no_control, civilian_demographics, encounter_characteristics,
                         civilian_behavior, full_control) %>%
  mutate(Black = round(Black, 3),
         Hispanic = round(Hispanic,3), Asian = round(Asian, 3), Others = round(Others, 3))


#assigning new row names to the "our_results" data frame
rownames(our_results) <- c("(a)", "(b)",
                           "(c)",
                           "(d)", "(e)")


#outputting the "our_results" data frame as a table in a pdf file
pdf("our_result.pdf", height=11, width=10)
grid.table(our_results)
dev.off()



#Fryer's no control model
fryer_no_control <- data.frame(Model = "No Control", WhiteMean = as.character(0.153),Black = 1.534, Hispanic = 1.582, Asian = 1.044,
                               Others = 1.392)



#Fryer's model with civilian demographics added as control
fryer_civilian_demographics <- data.frame(Model = "+ Civilian Demographics", WhiteMean = "", Black = 1.480, Hispanic = 1.517,
                                          Asian = 1.010,
                                          Others = 1.346)


#Fryer's model with encounter characteristics added as control
fryer_encounter_characteristics <- data.frame(Model = "+ Encounter Characteristics", WhiteMean = "", Black = 1.655, Hispanic = 1.641,
                                              Asian = 1.059,
                                              Others = 1.452)

#Fryer's model with civilian behavior added as control
fryer_civilian_behavior <- data.frame(Model = "+ Civilian Behavior", WhiteMean = "", Black = 1.462, Hispanic = 1.516,
                                      Asian = 1.051,
                                      Others = 1.372)


#Fryer's model with full control added as control
fryer_full_control <- data.frame(Model = "+ Precinct, Year", WhiteMean = "", Black = 1.178, Hispanic = 1.122, Asian = 0.953,
                                 Others = 1.060)


#creating a dataframe with Fryer's results
fryer_results <- bind_rows(fryer_no_control, fryer_civilian_demographics,
                           fryer_encounter_characteristics,fryer_civilian_behavior, fryer_full_control)


#assigning new row names to the "fryer_results " data frame
rownames(fryer_results ) <- c("(a)", "(b)",
                           "(c)",
                           "(d)", "(e)")

#outputting the "fryer_results" data frame as a table in a pdf file
pdf("fryer_results.pdf", height=11, width=10)
grid.table(fryer_results )
dev.off()


# kable(our_results, format = "latex", booktabs = T, caption = "Our Results") %>%
#   kable_styling(latex_options = c("striped", "hold_position"),
#                 full_width = F)
# 
# write.csv(fryer_results, "fryer_results.csv", row.names = FALSE)
# write.csv(our_results, "our_results.csv", row.names = FALSE)
# write.csv(log_data, "our_results.csv", row.names = FALSE)


log_data <- log_data %>% select(any_force_used,race,sex,age,inout,daytime,ac_incid,ac_time,
                                  offunif,typeofid,othpers,cs_bulge,cs_cloth,cs_casng,cs_lkout,
                                  cs_descr,cs_drgtr,cs_furtv,cs_vcrim,
                                  cs_objcs,cs_other,wepnfnd,pct,year)

save(model_full_control,file = 'model.rda')
saveRDS(log_data, file = here("log_data2.rds"))


