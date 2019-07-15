
library(tidyverse)
library(naniar)


#set directory to source file location

#read in the stop, question, and frisk dataset for the years 2003-2018

df3 <- read.csv("sqf_2003.csv") 

df4 <- read.csv("sqf_2004.csv") 

df5 <- read.csv("sqf_2005.csv") 

df6 <- read.csv("sqf_2006.csv")

df7 <- read.csv("sqf_2007.csv") 

df8 <- read.csv("sqf_2008.csv") 

df9 <- read.csv("sqf_2009.csv") 

df10 <- read.csv("sqf_2010.csv") 


df11 <- read.csv("sqf_2011.csv") 

df12 <- read.csv("sqf_2012.csv") 

df13 <- read.csv("sqf_2013.csv") 

df14 <- read.csv("sqf_2014.csv") 

df15 <- read.csv("sqf_2015.csv") 

df16 <- read.csv("sqf_2016.csv") 

df17 <- readxl::read_xlsx ("sqf_2017.xlsx")

df18<- readxl::read_xlsx ("sqf_2018.xlsx")


#set the column names for 2013-18 to lowercase

colnames(df13) <- tolower(colnames(df13))
colnames(df14) <- tolower(colnames(df14))
colnames(df15) <- tolower(colnames(df15))
colnames(df16) <- tolower(colnames(df16))
colnames(df17) <- tolower(colnames(df17))
colnames(df18) <- tolower(colnames(df18))

#change column names from the 2006 dataset to match the naming conventions in the rest of the data

df6 <- df6 %>%
  rename("stname" = "strname", "stinter" = "strintr", "rescode" = "rescod", "premtype" = "premtyp",
         "premname" = "prenam", "dettypcm" = "dettyp_c", "addrnum" = "adrnum", "addrpct" = "adrpct",
         "detailcm" = "details_") %>% mutate(forceuse = NA, linecm = NA)

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

#bind the ro



