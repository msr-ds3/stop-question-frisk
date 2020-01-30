# library(pacman)
library(tidyverse)
library(naniar)
library(scales)
library(here)

#set directory to source file location

#read in the stop, question, and frisk dataset for the years 2003-2018
df3 <- read.csv(here("raw_data", "sqf_2003.csv")) 
df4 <- read.csv(here("raw_data", "sqf_2004.csv")) 
df5 <- read.csv(here("raw_data", "sqf_2005.csv")) 
df6 <- read.csv(here("raw_data", "sqf_2006.csv")) 
df7 <- read.csv(here("raw_data", "sqf_2007.csv")) 
df8 <- read.csv(here("raw_data", "sqf_2008.csv"))  
df9 <- read.csv(here("raw_data", "sqf_2009.csv")) 
df10 <- read.csv(here("raw_data", "sqf_2010.csv")) 
df11 <- read.csv(here("raw_data", "sqf_2011.csv")) 
df12 <- read.csv(here("raw_data", "sqf_2012.csv")) 
df13 <- read.csv(here("raw_data", "sqf_2013.csv")) 
df14 <- read.csv(here("raw_data", "sqf_2014.csv")) 
df15 <- read.csv(here("raw_data", "sqf_2015.csv")) 
df16 <- read.csv(here("raw_data", "sqf_2016.csv")) 
df17 <- readxl::read_xlsx (here("raw_data", "sqf_2017.xlsx"))
df18<- readxl::read_xlsx (here("raw_data", "sqf_2018.xlsx"))



#set the column names for 2013-18 to lowercase
colnames(df13) <- tolower(colnames(df13))
colnames(df14) <- tolower(colnames(df14))
colnames(df15) <- tolower(colnames(df15))
colnames(df16) <- tolower(colnames(df16))
colnames(df17) <- tolower(colnames(df17))
colnames(df18) <- tolower(colnames(df18))

#change column names from the 2006 dataset to match the naming conventions in the rest of the data
df6 <- df6 %>%
  rename("stname" = "strname", "stinter" = "strintr", "rescode" = "rescod",
         "premtype" = "premtyp", "premname" = "prenam", "dettypcm" = "dettyp_c",
         "addrnum" = "adrnum", "addrpct" = "adrpct", "detailcm" = "details_") %>%
  mutate(forceuse = NA, linecm = NA)

#change the "ï..year" column name to "year"
df16 <- df16 %>%
  rename("year" = "ï..year")

#drop the detail1_ column in the 2006 data
df6$detail1_ <- NULL

#change the "stop frisk time" column name to "stop_frisk_time" in order to match the 2017 data
df18 <- df18 %>%
  rename("stop_frisk_time" = "stop frisk time")



#bind the rows of all the data from 2003 - 2010
data_03_10 <- rbind(df3, df4, df5, df7, df8, df9, df10) %>%
  mutate(forceuse = NA, wepfound = NA)

#bind the rows of all the data from 2011 - 2013
data_11_13 <- rbind(df11, df12, df13) %>%
  mutate(wepfound = NA)

#bind the rows of all the data from 2003 - 2013 except 2006
data_03_13 <- rbind(data_03_10, data_11_13)

#bind the rows of all the data from 2003 - 2013 including 2006
sf_data1 <- rbind(df6, data_03_13)

#bind the rows of all the data from 2014 - 2016 
data_14_16 <- rbind(df14, df15, df16) %>%
  mutate(wepfound = NA)

#bind the rows of all the data from 2003 - 2016
sf_data2 <- rbind(sf_data1, data_14_16)

#bind the rows of all the data from 2017 - 2018
sf_2017_2018 <- rbind(df17, df18)



#replacing "(null)" values with '' in 17/18 data
sf_2017_2018[] <- lapply(sf_2017_2018, gsub, pattern="[(null)]", replacement='')


#paste the id_card_identifies values together to create "officrid" column 
# (the same column name as the rest of the data) 
sf_2017_2018 <- sf_2017_2018 %>%
  mutate(officrid = paste(sf_2017_2018$id_card_identifies_officer_flag,
                          sf_2017_2018$shield_identifies_officer_flag,
                          sf_2017_2018$verbal_identifies_officer_flag,sep = ""))


#create a new dataset with the 17/18 data
sf_1718 <- sf_2017_2018

#create a new column called "suspect_height" by collapsing "ht_feet" and "ht_inch" columns
sf_data2 <- sf_data2 %>%
  mutate(suspect_height = paste(ht_feet,".",ht_inch,sep = ""))

#create a new dataset with the 03-16 data
sf_data3 <- sf_data2 %>% mutate(year = as.character(year), perobs = as.character(perobs),
                                weight = as.character(weight), xcoord = as.character(xcoord),
                                ycoord = as.character(ycoord))


#renaming all the common columns in 17/18 and 03-16 datasets
sf_data3 <- sf_data3 %>%
  rename(stop_frisk_date = datestop,
       stop_frisk_time = timestop,
       location_in_out_code = inout,
       observed_duration_minutes = perobs,
       stop_duration_minutes = perstop,
       year2 = year,
       suspected_crime_description = crimsusp,
       officer_explained_stop_flag = explnstp,
       other_person_stopped_flag = othpers,
       suspect_arrested_flag = arstmade,
       suspect_arrest_offense = arstoffn,
       summons_issued_flag = sumissue,
       summons_offense_description = sumoffen,
       officer_in_uniform_flag = offunif,
       frisked_flag = frisked,
       searched_flag = searched,
       other_contraband_flag = contrabn,
       knife_cutter_flag = knifcuti,
       other_weapon_flag = othrweap,
       suspect_sex = sex,
       suspect_race_description = race,
       suspect_reported_age = age,
       suspect_weight = weight,
       suspect_hair_color = haircolr,
       suspect_eye_color = eyecolor,
       suspect_other_description = othfeatr,
       physical_force_handcuff_suspect_flag = pf_hcuff,
       physical_force_oc_spray_used_flag = pf_pepsp,
       physical_force_other_flag = pf_other,
       search_basis_hard_object_flag = sb_hdobj,
       search_basis_outline_flag = sb_outln,
       search_basis_admission_flag = sb_admis,
       search_basis_other_flag = sb_other,
       suspect_body_build_type = build,
       stop_location_street_name = stname,
       stop_location_zip_code = zip,
       stop_location_sector_code = sector,
       stop_location_apartment = aptnum,
       stop_location_x = xcoord,
       stop_location_y = ycoord,
       record_status_code = recstat,
       jurisdiction_code = trhsloc,
       backround_circumstances_violent_crime_flag = rf_vcrim,
       backround_circumstances_suspect_known_to_carry_weapon_flag = rf_othsw,
       suspects_actions_proximity_to_scene_flag = ac_proxm,
       suspects_actions_lookout_flag = cs_lkout,
       suspects_actions_decription_flag = cs_descr,
       suspects_actions_casing_flag = cs_casng,
       suspects_actions_drug_transactions_flag = cs_drgtr,
       suspects_actions_other_flag = cs_other,
       suspects_actions_identify_crime_pattern_flag = rf_knowl,
       stop_location_premises_name = premname,
       stop_location_precinct = addrpct)


#recode the factor values of "suspect_sex" and "suspect_race_description" for the 17/18 data

sf_1718 <- sf_1718 %>%
  replace_with_na(replace = list(suspect_sex = c("","19","23","24","39"),
                                 suspect_race_description = c("","MALE"))) %>%
  mutate(suspect_sex = recode_factor(suspect_sex,
                                     "MALE" = "M", "FEMALE" = "F")) %>%
  mutate(suspect_race_description = recode_factor(suspect_race_description,
                                                  "ASIAN/PAC.ISL" = "A",
                                                  "ASIAN / PACIFIC ISLANDER" = "A",
                                                  "AMER IND" = "I",
                                                  "AMERICAN INDIAN/ALASKAN NATIVE" = "I",
                                                  "BLACK HISPANIC" = "P",
                                                  "WHITE HISPANIC" = "Q",
                                                  "BLACK" = "B",
                                                  "WHITE" = "W"))


#bind all the data from 2003-2018 together
stop_and_frisk <- bind_rows(sf_data3, sf_1718)



#save the 03-13 data_set
#this is the prime dataset
save(sf_data1, file = "sqf_03_13.RData")

#save the 03-18 dataset (the whole clean dataset)
save(stop_and_frisk, file = "sqf_03_18.RData")


