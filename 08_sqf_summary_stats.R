library(pacman)
library(tidyr)
library(tidyselect)
library(gridExtra)
library(readr)

#set working directory
setwd(here("raw_data"))
load("sqf_03_13.RData")
fryers <- read.csv("Fryers_results.csv")

sf_data1 <- sf_data1 %>%
  mutate(race = recode_factor(race,"P" = "B","Q" = "H"))

#race proportion
race_prop <- sf_data1 %>%
  select(race) %>%
  group_by(race) %>% 
  summarize(count = n()) %>%
  spread(race, count) %>%
  summarize(total = A+B+I+W+Z+H, white = W/total, black = B/total, 
            hispanic = H/total, asian = A/total, other = (I+Z)/total) %>% 
  mutate(total=NULL) %>% round(2) %>% 
  gather() %>% data.frame() %>% 
  rename("Characteristics"="key", "Full_sample"="value")
  
#Age proportion
age_prop <- sf_data1 %>% select(age) %>% 
  filter(age > 10 & age < 100) %>% 
  summarize(age = mean(age,na.rm = TRUE))

#Male proportion
male_prop <- sf_data1 %>%
  select(sex) %>%
  group_by(sex) %>%
  summarize(count = n()) %>%
  spread(sex, count) %>%
  summarize(total = M+F, male = M/total) %>% 
  mutate(total=NULL)

#Inside_Outside prop
inside_outside_prop <- sf_data1 %>%
  select(inout) %>%
  group_by(inout) %>%
  summarise(count = n()) %>%
  spread(inout, count) %>%
  summarize(total = I+O, indoors = I/total) %>% 
  mutate(total=NULL)

#Daytime_prop
daytime_prop <- sf_data1 %>%
  select(timestop) %>%
  mutate(timestop = as.numeric(gsub(":","",timestop)),
         daytime = as.character(if_else(timestop >= 600 & timestop <= 1800, "Y","N"))) %>%
  group_by(daytime) %>%
  summarise(count = n()) %>%
  spread(daytime, count) %>%
  summarize(total = Y+N,
            daytime = Y/total) %>% 
  mutate(total=NULL)

#High-crime Area
crime_area <- sf_data1 %>%
  select(ac_incid) %>%
  group_by(ac_incid) %>% 
  summarise(count = n()) %>%
  spread(ac_incid, count) %>%
  summarize(total = Y+N, high_crime_area = Y/total) %>% 
  mutate(total=NULL)

#High-crime time
crime_time <- sf_data1 %>%
  select(ac_time) %>%
  group_by(ac_time) %>% 
  summarise(count = n()) %>%
  spread(ac_time, count) %>%
  summarize(total = Y+N, high_crime_time = Y/total) %>% 
  mutate(total=NULL)

#Police in Uniform
uniform <- sf_data1 %>%
  select(offunif) %>%
  group_by(offunif) %>% 
  summarise(count = n()) %>%
  spread(offunif, count) %>%
  summarize(total = Y+N, police_in_uniform = Y/total) %>% 
  mutate(total=NULL)

#Type of ID
id_type <- sf_data1 %>%
  select(typeofid) %>% 
  group_by(typeofid) %>% 
  summarise(count = n()) %>%
  spread(typeofid, count) %>%
  summarize(total = V+R+P+O, 
            photo_id = P/total,
            verbal_id = V/total,
            refuse_id = R/total,
            other_id = O/total) %>% 
  mutate(total=NULL)

#Stop with others
stop_w_other <- sf_data1 %>%
  select(othpers) %>%
  group_by(othpers) %>% 
  summarise(count = n()) %>%
  spread(othpers, count) %>%
  summarize(total = Y+N, stop_with_others = Y/total) %>% 
  mutate(total=NULL)

#Carrying Suspicious Objects
carry_susp_obj <- sf_data1 %>%
  select(cs_objcs) %>%
  group_by(cs_objcs) %>% 
  summarise(count = n()) %>%
  spread(cs_objcs, count) %>%
  summarize(total = Y+N, 
            carrying_suspicious_objects = Y/total) %>% 
  mutate(total=NULL)

#Fit Relevant Description	
fit_description <- sf_data1 %>%
  select(cs_descr) %>%
  group_by(cs_descr) %>% 
  summarise(count = n()) %>%
  spread(cs_descr, count) %>%
  summarize(total = Y+N, fit_relevant_description = Y/total) %>% 
  mutate(total=NULL)

#Preparing for Crime
preparing_crime <- sf_data1 %>%
  select(cs_casng) %>%
  group_by(cs_casng) %>% 
  summarise(count = n()) %>%
  spread(cs_casng, count) %>%
  summarize(total = Y+N, preparing_for_crime = Y/total) %>% 
  mutate(total=NULL)

#Lookout for Crime
lookout_crime <- sf_data1 %>%
  select(cs_lkout) %>%
  group_by(cs_lkout) %>% 
  summarise(count = n()) %>%
  spread(cs_lkout, count) %>%
  summarize(total = Y+N, lookout_for_crime = Y/total) %>% 
  mutate(total=NULL)

#Dressed in Criminal Attire
criminal_attire <- sf_data1 %>%
  select(cs_cloth) %>%
  group_by(cs_cloth) %>% 
  summarise(count = n()) %>%
  spread(cs_cloth, count) %>%
  summarize(total = Y+N, criminal_attire = Y/total) %>% 
  mutate(total=NULL)

#Appearance of Drug Transaction
drug <- sf_data1 %>%
  select(cs_drgtr) %>%
  group_by(cs_drgtr) %>% 
  summarise(count = n()) %>%
  spread(cs_drgtr, count) %>%
  summarize(total = Y+N, appearance__drug_transaction = Y/total) %>% 
  mutate(total=NULL)

#Suspicious Movements
suspi_move <- sf_data1 %>%
  select(cs_furtv) %>%
  group_by(cs_furtv) %>% 
  summarise(count = n()) %>%
  spread(cs_furtv, count) %>%
  summarize(total = Y+N, suspicious_movements = Y/total) %>% 
  mutate(total=NULL)

#Engaging in Violent Crime
violent_crime <- sf_data1 %>%
  select(cs_vcrim) %>%
  group_by(cs_vcrim) %>% 
  summarise(count = n()) %>%
  spread(cs_vcrim, count) %>%
  summarize(total = Y+N, engaging_in_violent_crime = Y/total) %>% 
  mutate(total=NULL)

#Concealing Suspicious Object	
conceal <- sf_data1 %>%
  select(cs_bulge) %>%
  group_by(cs_bulge) %>% 
  summarise(count = n()) %>%
  spread(cs_bulge, count) %>%
  summarize(total = Y+N, concealing_suspicious_object = Y/total) %>% 
  mutate(total=NULL)

#Other Suspicious Behavior	
other_susp_behavior <- sf_data1 %>%
  select(cs_other) %>%
  group_by(cs_other) %>% 
  summarise(count = n()) %>%
  spread(cs_other, count) %>%
  summarize(total = Y+N, other_suspicious_behavior = Y/total) %>% 
  mutate(total=NULL)

#Contraband or Weapon Found	
contraband_weapon <- sf_data1 %>%
  mutate(wepnfnd = paste(contrabn,asltweap,pistol,riflshot,knifcuti,machgun,othrweap, sep = "")) %>%
  mutate(wepnfnd = if_else(grepl("Y",wepnfnd), "Y", "N"))  %>%
  select(wepnfnd) %>%
  group_by(wepnfnd) %>%
  summarise(count = n()) %>%
  spread(wepnfnd, count) %>%
  summarize(total = Y+N, contraband_or_weapon = Y/total) %>% 
  mutate(total=NULL)

#Frisked
frisk <- sf_data1 %>%
  select(frisked) %>%
  group_by(frisked) %>% 
  summarise(count = n()) %>%
  spread(frisked, count) %>%
  summarize(total = Y+N, frisked = Y/total) %>% 
  mutate(total=NULL)

#Searched
search <- sf_data1 %>%
  select(searched) %>%
  group_by(searched) %>% 
  summarise(count = n()) %>%
  spread(searched, count) %>%
  summarize(total = Y+N, searched = Y/total) %>% 
  mutate(total=NULL)

#Arrested
arrest <- sf_data1 %>%
  select(arstmade) %>%
  group_by(arstmade) %>% 
  summarise(count = n()) %>%
  spread(arstmade, count) %>%
  summarize(total = Y+N, arrested = Y/total) %>% 
  mutate(total=NULL)

#Summonsed
summon <- sf_data1 %>%
  select(sumissue) %>%
  group_by(sumissue) %>% 
  summarise(count = n()) %>%
  spread(sumissue, count) %>%
  summarize(total = Y+N, summonsed = Y/total) %>% 
  mutate(total=NULL)

#Hands
hand <- sf_data1 %>%
  select(pf_hands) %>%
  group_by(pf_hands) %>% 
  summarise(count = n()) %>%
  spread(pf_hands, count) %>%
  summarize(total = Y+N, hands = Y/total) %>% 
  mutate(total=NULL)

#Wall
wall <- sf_data1 %>%
  select(pf_wall) %>%
  group_by(pf_wall) %>% 
  summarise(count = n()) %>%
  spread(pf_wall, count) %>%
  summarize(total = Y+N, push_to_wall = Y/total) %>% 
  mutate(total=NULL)

#Handcuffs
handcuff <- sf_data1 %>%
  select(pf_hcuff) %>%
  group_by(pf_hcuff) %>% 
  summarise(count = n()) %>%
  spread(pf_hcuff, count) %>%
  summarize(total = Y+N, handcuffs = Y/total) %>% 
  mutate(total=NULL)

#draw weapon
draw_weapons <- sf_data1 %>%
  select(pf_drwep) %>%
  group_by(pf_drwep) %>% 
  summarise(count = n()) %>%
  spread(pf_drwep, count) %>%
  summarize(total = Y+N, draw_weapon = Y/total) %>% 
  mutate(total=NULL)

#ground
ground <- sf_data1 %>%
  select(pf_grnd) %>%
  group_by(pf_grnd) %>% 
  summarise(count = n()) %>%
  spread(pf_grnd, count) %>%
  summarize(total = Y+N, push_to_ground = Y/total) %>% 
  mutate(total=NULL)

#point weapon
point_weapons <- sf_data1 %>%
  select(pf_ptwep) %>%
  group_by(pf_ptwep) %>% 
  summarise(count = n()) %>%
  spread(pf_ptwep, count) %>%
  summarize(total = Y+N, point_weapon = Y/total) %>% 
  mutate(total=NULL)

#pepper spray baton
pepper_baton <- sf_data1 %>%
  mutate(pf_pp_spray_baton = paste(pf_pepsp, pf_baton,sep = "")) %>%
  mutate(pf_pp_spray_baton = ifelse(grepl("Y",pf_pp_spray_baton), "Y", "N")) %>%
  select(pf_pp_spray_baton) %>%
  group_by(pf_pp_spray_baton) %>% 
  summarise(count = n()) %>%
  spread(pf_pp_spray_baton, count) %>%
  summarize(total = Y+N, pepperspray_baton = Y/total) %>% 
  mutate(total=NULL)

fullsample_prop <- bind_cols(age_prop,male_prop,inside_outside_prop,daytime_prop,
                             crime_area,crime_time,uniform,id_type,stop_w_other,
                             carry_susp_obj,fit_description,preparing_crime,lookout_crime,
                             criminal_attire,drug,suspi_move,violent_crime,conceal,
                             other_susp_behavior, contraband_weapon, frisk,search,
                             arrest,summon,hand,wall,handcuff,draw_weapons,ground,
                             point_weapons,pepper_baton) %>% 
  round(2) %>% gather() %>% data.frame() %>% 
  rename("Characteristics"="key", "Full_sample"="value")

fullsample_prop <- rbind(race_prop,fullsample_prop)

#Proportion by race
sf_data1 <- sf_data1 %>%
  mutate(race = recode_factor(race,"P" = "B","Q" = "H", "I" = "Z")) %>% 
  mutate(race = recode_factor(race, "W"= "White", "B"="Black","H"="Hispanic"))

race_prop <- data.frame(White = c(1,0,0,0,0),
                        Black = c(0,1,0,0,0),
                        Hispanic = c(0,0,1,0,0))

#Age proportion
avg_age <- sf_data1 %>%
  select(race, age) %>%
  filter(10<= age & age<= 90) %>% 
  group_by(race) %>%
  summarise(ages = mean(age, na.rm = T)) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>%
  spread(race, ages)

#Male proportion
male_prop <- sf_data1 %>%
  select(race, sex) %>%
  group_by(race, sex) %>%
  summarize(count = n()) %>%
  spread(sex, count) %>%
  summarize(total = M+F, male = M/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, male)

#Inside_Outside prop
inside_outside_prop <- sf_data1 %>%
  select(race,inout) %>%
  mutate(race = recode_factor(race, "W"= "White", "B"="Black","H"="Hispanic"))%>% 
  group_by(race,inout) %>%
  summarise(count = n()) %>%
  spread(inout, count) %>%
  summarize(total = I+O, indoors = I/total) %>%
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, indoors)

#Daytime_prop
daytime_prop <- sf_data1 %>%
  select(race,timestop) %>%
  mutate(timestop = as.numeric(gsub(":","",timestop)),
         daytime = as.character(if_else(timestop >= 600 & timestop <= 1800, "Y","N"))) %>%
  group_by(race,daytime) %>%
  summarise(count = n()) %>%
  spread(daytime, count) %>%
  summarize(total = Y+N,
            daytime = Y/total)%>% 
  filter(race!="U", race!="X",race!="Z", race!="A",race!=" ", !is.na(race)) %>% 
  mutate(total=NULL) %>% spread(race, daytime)

#High-crime Area
crime_area <- sf_data1 %>%
  select(race, ac_incid) %>%
  group_by(race, ac_incid) %>% 
  summarise(count = n()) %>%
  spread(ac_incid, count) %>%
  summarize(total = Y+N, high_crime_area = Y/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, high_crime_area)

#High-crime time
crime_time <- sf_data1 %>%
  select(race, ac_time) %>%
  group_by(race, ac_time) %>% 
  summarise(count = n()) %>%
  spread(ac_time, count) %>%
  summarize(total = Y+N, high_crime_time = Y/total) %>% 
  filter(race != "I",race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, high_crime_time)

#Police in Uniform
uniform <- sf_data1 %>%
  select(race, offunif) %>%
  #mutate(race = recode_factor(race, "W"= "White", "B"="Black","H"="Hispanic")) %>% 
  group_by(race, offunif) %>% 
  summarise(count = n()) %>%
  spread(offunif, count) %>%
  summarize(total = Y+N, police_in_uniform = Y/total) %>% 
  filter(race != "I",race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, police_in_uniform)

#Type of ID
id_type <- sf_data1 %>%
  select(race,typeofid) %>%
  group_by(race, typeofid) %>% 
  summarise(count = n()) %>%
  spread(typeofid, count) %>% 
  summarize(total = V+R+P+O,
            photo_id = P/total,
            verbal_id = V/total,
            refuse_id = R/total,
            other_id = O/total) %>% 
  filter(race != "I",race != "U",race!="X", race != "Z", 
         race != "A", race != " ", !is.na(race)) %>%
  mutate(race=NULL, total=NULL) %>% t() %>% as_data_frame() %>% 
  rename("White"="V1","Black"="V2","Hispanic"="V3")

#Stop with others
stop_w_other <- sf_data1 %>%
  select(race, othpers) %>%
  group_by(race, othpers) %>% 
  summarise(count = n()) %>%
  spread(othpers, count) %>%
  summarize(total = Y+N, stop_with_others = Y/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, stop_with_others)

#Carrying Suspicious Objects
carry_susp_obj <- sf_data1 %>%
  select(race, cs_objcs) %>%
  group_by(race, cs_objcs) %>% 
  summarise(count = n()) %>%
  spread(cs_objcs, count) %>%
  summarize(total = Y+N, carrying_suspicious_objects = Y/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, carrying_suspicious_objects)

#Fit Relevant Description	
fit_description <- sf_data1 %>%
  select(race, cs_descr) %>%
  group_by(race, cs_descr) %>% 
  summarise(count = n()) %>%
  spread(cs_descr, count) %>%
  summarize(total = Y+N, fit_relevant_description = Y/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, fit_relevant_description)

#Preparing for Crime
preparing_crime <- sf_data1 %>%
  select(race,cs_casng) %>%
  group_by(race,cs_casng) %>% 
  summarise(count = n()) %>%
  spread(cs_casng, count) %>%
  summarize(total = Y+N, preparing_for_crime = Y/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, preparing_for_crime)

#Lookout for Crime
lookout_crime <- sf_data1 %>%
  select(race,cs_lkout) %>%
  group_by(race,cs_lkout) %>% 
  summarise(count = n()) %>%
  spread(cs_lkout, count) %>%
  summarize(total = Y+N, lookout_for_crime = Y/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, lookout_for_crime)

#Dressed in Criminal Attire
criminal_attire <- sf_data1 %>%
  select(race,cs_cloth) %>%
  group_by(race,cs_cloth) %>% 
  summarise(count = n()) %>%
  spread(cs_cloth, count) %>%
  summarize(total = Y+N, criminal_attire = Y/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, criminal_attire)

#Appearance of Drug Transaction
drug <- sf_data1 %>%
  select(race,cs_drgtr) %>%
  group_by(race,cs_drgtr) %>% 
  summarise(count = n()) %>%
  spread(cs_drgtr, count) %>%
  summarize(total = Y+N, appearance__drug_transaction = Y/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, appearance__drug_transaction)

#Suspicious Movements
suspi_move <- sf_data1 %>%
  select(race,cs_furtv) %>%
  group_by(race,cs_furtv) %>% 
  summarise(count = n()) %>%
  spread(cs_furtv, count) %>%
  summarize(total = Y+N, suspicious_movements = Y/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, suspicious_movements)

#Engaging in Violent Crime
violent_crime <- sf_data1 %>%
  select(race,cs_vcrim) %>%
  group_by(race,cs_vcrim) %>% 
  summarise(count = n()) %>%
  spread(cs_vcrim, count) %>%
  summarize(total = Y+N, engaging_in_violent_crime = Y/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, engaging_in_violent_crime)

#Concealing Suspicious Object	
conceal <- sf_data1 %>%
  select(race,cs_bulge) %>%
  group_by(race,cs_bulge) %>% 
  summarise(count = n()) %>%
  spread(cs_bulge, count) %>%
  summarize(total = Y+N, concealing_suspicious_object = Y/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, concealing_suspicious_object)

#Other Suspicious Behavior	
other_susp_behavior <- sf_data1 %>%
  select(race,cs_other) %>%
  group_by(race,cs_other) %>% 
  summarise(count = n()) %>%
  spread(cs_other, count) %>%
  summarize(total = Y+N, other_suspicious_behavior = Y/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, other_suspicious_behavior)

#Contraband or Weapon Found	
contraband_weapon <- sf_data1 %>%
  mutate(wepnfnd = paste(contrabn,asltweap,pistol,riflshot,knifcuti,machgun,othrweap, sep = "")) %>%
  mutate(wepnfnd = if_else(grepl("Y",wepnfnd), "Y", "N"))  %>%
  select(race,wepnfnd) %>%
  group_by(race,wepnfnd) %>%
  summarise(count = n()) %>%
  spread(wepnfnd, count) %>%
  summarize(total = Y+N, contraband_or_weapon = Y/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, contraband_or_weapon)

#Frisked
frisk <- sf_data1 %>%
  select(race,frisked) %>%
  group_by(race,frisked) %>% 
  summarise(count = n()) %>%
  spread(frisked, count) %>%
  summarize(total = Y+N, frisked = Y/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, frisked)

#Searched
search <- sf_data1 %>%
  select(race,searched) %>%
  group_by(race,searched) %>% 
  summarise(count = n()) %>%
  spread(searched, count) %>%
  summarize(total = Y+N, searched = Y/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, searched)

#Arrested
arrest <- sf_data1 %>%
  select(race,arstmade) %>%
  group_by(race,arstmade) %>% 
  summarise(count = n()) %>%
  spread(arstmade, count) %>%
  summarize(total = Y+N, arrested = Y/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, arrested)

#Summonsed
summon <- sf_data1 %>%
  select(race,sumissue) %>%
  group_by(race,sumissue) %>% 
  summarise(count = n()) %>%
  spread(sumissue, count) %>%
  summarize(total = Y+N, summonsed = Y/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, summonsed)

#Hands
hand <- sf_data1 %>%
  select(race,pf_hands) %>%
  group_by(race,pf_hands) %>% 
  summarise(count = n()) %>%
  spread(pf_hands, count) %>%
  summarize(total = Y+N, hands = Y/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, hands)

#Wall
wall <- sf_data1 %>%
  select(race,pf_wall) %>%
  group_by(race,pf_wall) %>% 
  summarise(count = n()) %>%
  spread(pf_wall, count) %>%
  summarize(total = Y+N, push_to_wall = Y/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, push_to_wall)

#Handcuffs
handcuff <- sf_data1 %>%
  select(race,pf_hcuff) %>%
  group_by(race,pf_hcuff) %>% 
  summarise(count = n()) %>%
  spread(pf_hcuff, count) %>%
  summarize(total = Y+N, handcuffs = Y/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, handcuffs)

#draw weapon
draw_weapons <- sf_data1 %>%
  select(race,pf_drwep) %>%
  group_by(race,pf_drwep) %>% 
  summarise(count = n()) %>%
  spread(pf_drwep, count) %>%
  summarize(total = Y+N, draw_weapon = Y/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, draw_weapon)

#ground
ground <- sf_data1 %>%
  select(race,pf_grnd) %>%
  group_by(race,pf_grnd) %>% 
  summarise(count = n()) %>%
  spread(pf_grnd, count) %>%
  summarize(total = Y+N, push_to_ground = Y/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, push_to_ground)

#point weapon
point_weapons <- sf_data1 %>%
  select(race,pf_ptwep) %>%
  group_by(race,pf_ptwep) %>% 
  summarise(count = n()) %>%
  spread(pf_ptwep, count) %>%
  summarize(total = Y+N, point_weapon = Y/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race))%>% 
  mutate(total=NULL) %>% spread(race, point_weapon)

#pepper spray baton
pepper_baton <- sf_data1 %>%
  mutate(pf_pp_spray_baton = paste(pf_pepsp, pf_baton,sep = "")) %>%
  mutate(pf_pp_spray_baton = ifelse(grepl("Y",pf_pp_spray_baton), "Y", "N")) %>%
  select(race,pf_pp_spray_baton) %>%
  group_by(race,pf_pp_spray_baton) %>% 
  summarise(count=n()) %>%
  spread(pf_pp_spray_baton, count) %>%
  summarize(total = Y+N, pepperspray_baton = Y/total) %>% 
  filter(race != "U",race!="X", race != "Z", race != "A",race != " ", !is.na(race)) %>% 
  mutate(total=NULL) %>% spread(race, pepperspray_baton)

prop_by_race <- bind_rows(race_prop,avg_age, male_prop,inside_outside_prop,daytime_prop,
                          crime_area,crime_time,uniform,id_type,stop_w_other,
                          carry_susp_obj,fit_description,preparing_crime,lookout_crime,
                          criminal_attire,drug,suspi_move,violent_crime,conceal,
                          other_susp_behavior, contraband_weapon, frisk,search,
                          arrest,summon,hand,wall,handcuff,draw_weapons,ground,
                          point_weapons,pepper_baton) %>% round(2)

table <- bind_cols(fullsample_prop, prop_by_race)
pdf("summary_stats1.pdf", height=11, width=10)
grid.table(table)
dev.off()
