
library(tidyverse)
library(naniar)


#set directory to source file location

#read in the stop, question, and frisk dataset for the year 2003

df3 <- read.csv("sqf_2003.csv")

#read in the stop, question, and frisk dataset for the year 2003

df4 <- read.csv("sqf_2004.csv")

#read in the stop, question, and frisk dataset for the year 2003

df5 <- read.csv("sqf_2005.csv")

#read in the stop, question, and frisk dataset for the year 2003

df6 <- read.csv("sqf_2006.csv")

#read in the stop, question, and frisk dataset for the year 2003

df7 <- read.csv("sqf_2007.csv")

#read in the stop, question, and frisk dataset for the year 2003

df8 <- read.csv("sqf_2008.csv")

#read in the stop, question, and frisk dataset for the year 2003

df9 <- read.csv("sqf_2009.csv")

#read in the stop, question, and frisk dataset for the year 2003

df10 <- read.csv("sqf_2010.csv")

#read in the stop, question, and frisk dataset for the year 2003

df11 <- read.csv("sqf_2011.csv")

#read in the stop, question, and frisk dataset for the year 2003

df12 <- read.csv("sqf_2012.csv") 

#read in the stop, question, and frisk dataset for the year 2003

df13 <- read.csv("sqf_2013.csv") 

#read in the stop, question, and frisk dataset for the year 2003

df14 <- read.csv("sqf_2014.csv")

#read in the stop, question, and frisk dataset for the year 2003

df15 <- read.csv("sqf-2015.csv")

#read in the stop, question, and frisk dataset for the year 2003

df16 <- read.csv("sqf-2016.csv")

#read in the stop, question, and frisk dataset for the year 2003

df17 <- readxl::read_xlsx ("sqf-2017.xlsx")

#read in the stop, question, and frisk dataset for the year 2003

df18<- readxl::read_xlsx ("sqf-2018.xlsx")


#set the column names for 2013-18 to lowercase

colnames(df13) <- tolower(colnames(df13))
colnames(df14) <- tolower(colnames(df14))
colnames(df15) <- tolower(colnames(df15))
colnames(df16) <- tolower(colnames(df16))
colnames(df17) <- tolower(colnames(df17))
colnames(df18) <- tolower(colnames(df18))

#change column names from the 2006 dataset to match the naming conventions in the rest of the data


colnames(df6)[colnames(df6)=="strname"] <- "stname"
colnames(df6)[colnames(df6)=="strintr"] <- "stinter"
colnames(df6)[colnames(df6)=="rescod"] <- "rescode"
colnames(df6)[colnames(df6)=="premtyp"] <- "premtype"
colnames(df6)[colnames(df6)=="prenam"] <- "premname"
colnames(df6)[colnames(df6)=="dettyp_c"] <- "dettypcm"
colnames(df6)[colnames(df6)=="adrnum"] <- "addrnum"
colnames(df6)[colnames(df6)=="adrpct"] <- "addrpct"
colnames(df6)[colnames(df6)=="details_"] <- "detailcm"
colnames(df16)[colnames(df16)=="ï..year"] <- "year"


#drop the detail1_ column in the 2006 data
df6$detail1_ <- NULL


#change the "stop frisk time" column name to "stop_frisk_time" in order to match the 2017 data

colnames(df18)[colnames(df18)=="stop frisk time"] <- "stop_frisk_time"




#bind the rows of all the data from 2003 - 2013
sf_data1 <- bind_rows(df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13)

