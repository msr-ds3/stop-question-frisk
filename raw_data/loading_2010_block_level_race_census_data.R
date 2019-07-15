library(tidyverse)
library(tidycensus)


census_api_key('5365371ad843ba3249f2e88162f10edcfe529d87', install = TRUE)
readRenviron("~/.Renviron")

#variables to load from census data
vars = c("P003001", "P003002", "P003003", "P003004", "P003005", "P003006", "P003007", "P003008")

#counties to load from census data
counties = c("Richmond", "Kings", "New York", "Queens", "Bronx")

#variable names
label = c("Total", "White alone", "Black or African American alone",
          "American Indian and Alaska Native alone", "Asian alone",
          "Native Hawaiian and Other Pacific Islander alone",
          "Some Other Race alone", "Two or More Races")

#load census data
census <- get_decennial(geography = "block", variables = vars, 
                        state = "NY", county = counties, year = 2010)

#summarize census data
totals <- census %>% group_by(variable) %>% summarize(total = sum(value)) %>% select(total)
totals_by_race <- data.frame(vars, label, totals)

#spread census data by race
data <- spread(census, variable, value)

#rename columns for clarity
colnames(data) = c("GEOID", "Block Name", "Total", "White", "Black_African_American", 
                   "American_Indian_Alaska_Native", 
                   "Asian", "Native_Hawaiian_Pacific_Islander", "Other", "Two_Or_More_Races")

#convert counts to fractions of total
data <- data %>% mutate(frac_white = White/Total, frac_black = Black_African_American/Total,
                        frac_alaskan = American_Indian_Alaska_Native/Total, frac_asian = Asian/Total, 
                        frac_islander = Native_Hawaiian_Pacific_Islander/Total,
                        frac_other = Other/Total, frac_mixed = Two_Or_More_Races/Total)
