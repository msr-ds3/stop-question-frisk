if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyverse, naniar, scales, here)

#make sure you have run "01_download_sqf_data.sh" before running this script

#read in the stop, question, and frisk dataset for the years 2003-2018
df3 <- fread(here("raw_data","sqf_2003.csv")) 
df4 <- fread(here("raw_data","sqf_2004.csv")) 
df5 <- fread(here("raw_data","sqf_2005.csv")) 
df6 <- fread(here("raw_data","sqf_2006.csv"))
df7 <- fread(here("raw_data","sqf_2007.csv")) 
df8 <- fread(here("raw_data","sqf_2008.csv"))
df9 <- fread(here("raw_data","sqf_2009.csv")) 
df10 <- fread(here("raw_data","sqf_2010.csv")) 
df11 <- fread(here("raw_data","sqf_2011.csv"))
df12 <- fread(here("raw_data","sqf_2012.csv")) 
df13 <- fread(here("raw_data","sqf_2013.csv")) 


#set the column names for 2013-18 to lowercase
colnames(df13) <- tolower(colnames(df13))


#change column names from the 2006 dataset to match the naming conventions in the rest of the data
df6 <- df6 %>%
  rename("stname" = "strname", "stinter" = "strintr", "rescode" = "rescod",
         "premtype" = "premtyp", "premname" = "prenam", "dettypcm" = "dettyp_c",
         "addrnum" = "adrnum", "addrpct" = "adrpct", "detailcm" = "details_") %>%
  mutate(forceuse = NA, linecm = NA)


#drop the detail1_ column in the 2006 data
df6$detail1_ <- NULL


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

# remove columns that we don't use to speed up saving and loading this dataset
sf_data1 <- sf_data1 %>% select(-ser_num, -datestop, -recstat, -trhsloc, -perobs, -crimsusp, -perstop, -explnstp, -arstoffn, -sumoffen, -compyear, -comppct, -officrid, -adtlrept, -pf_other, -radio,-ac_rept, -ac_inves, -rf_vcrim, -rf_othsw, -ac_proxm, -rf_attir, -rf_vcact, -ac_evasv, -ac_assoc, -rf_rfcmp, -ac_cgdir, -rf_verbl, -rf_knowl, -ac_stsnd, -ac_other, -sb_hdobj, -sb_outln, -sb_admis, -sb_other, -repcmd, -revcmd, -rf_furt, -rf_bulg, -offverb, -offshld, -wepfound, -dettypcm, -detailcm, -dob, -ht_feet, -ht_inch, -weight, -haircolr, -eyecolor, -build, -othfeatr, -addrtyp, -rescode, -premtype, -premname, -addrnum, -stname, -stinter, -aptnum, -city, -state, -zip, -sector, -beat, -post, -xcoord, -ycoord, -crossst, -forceuse, -linecm )

#save the 03-13 data_set
#this is the dataset we use for the logistic regression
save(sf_data1, file = here("clean_data","sqf_03_13.RData"))

