library(tidyverse)
library(tidycensus)


census_api_key('5365371ad843ba3249f2e88162f10edcfe529d87', install = TRUE)
readRenviron("~/.Renviron")

vars = c("P003001", "P003002", "P003003", "P003004", "P003005", "P003006", "P003007", "P003008")
counties = c("Richmond", "Kings", "New York", "Queens", "Bronx")
label = c("Total", "White alone", "Black or African American alone",
          "American Indian and Alaska Native alone", "Asian alone",
          "Native Hawaiian and Other Pacific Islander alone",
          "Some Other Race alone", "Two or More Races")

census <- get_decennial(geography = "block", variables = vars, 
                        state = "NY", county = counties, year = 2010)

totals <- census %>% group_by(variable) %>% summarize(total = sum(value)) %>% select(total)

totals_by_race <- data.frame(vars, label, totals)
