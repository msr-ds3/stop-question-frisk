install.packages("tidycensus")
install.packages("totalcensus")
library(tidyverse)
library(dplyr)
library(tidycensus)
library(totalcensus)

#Cencus API key retrieved from ACS website
census_api_key("5365371ad843ba3249f2e88162f10edcfe529d87", install=TRUE)
readRenviron("~/.Renviron")

#Vectors of arguments to be passed to get decennial census for NYC at block level
var_race <- c("P003001","P003002","P003003","P003004","P003005","P003006","P003007",
              "P003008")
counties <- c("Richmond", "Kings", "New York", "Queens", "Bronx")


#Loading the census
census <- get_decennial(geography = "block", variables = var_race,
                        state = "NY", county = counties,
                        year = 2010)
View(census)







