library(tidyverse)
library(tidycensus)
library(totalcensus)
library(sf)
library(tmap)
library(tmaptools)
library(tigris)
library(leaflet)
library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)


# Load stop and frisk data for 2003-2013
load("sqf_03_13.RData")

# Set up census data
census_api_key('5365371ad843ba3249f2e88162f10edcfe529d87', install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

#variables to load from census data
vars = c("P003004", "P003005", "P003006", "P003007", "P003008",
         "P005003", "P005004", "P005011", "P005012")

#counties to load from census data
counties = c("Richmond", "Kings", "New York", "Queens", "Bronx")

#load census data
census <- get_decennial(geography = "block", variables = vars, state = "NY", 
                        county = counties, year = 2010, tigris_use_cache = TRUE)

# set digits so converting geoid's to numbers will retain all sigfigs
options(digits = 15)

# convert GEOIDs to numbers, variables to a factor
census <- mutate(census, variable = as.factor(variable)) %>%
  mutate(geoid10 = as.numeric(GEOID)) %>% select(-GEOID)

# reset digits to the default
options(digits = 7)

# rename variables for clarity
census$variable <- recode(census$variable, P003004 = "American_Indian_and_Alaska_Native",
                          P003005 = "Asian", P003006 = "Native_Hawaiian_and_Pacific_Islander",
                          P003007 = "Other", P003008 = "Two_Or_More_Races",
                          P005003 = "White_other", P005004 = "Black_or_African_American_other",
                          P005011 = "White_Hispanic_Latino", P005012 = "Black_or_African_American_Hispanic_Latino")

# load file to map block-level data to precinct level
precinct_block_key <- read_csv("precinct_blocks_key.csv")

# add precinct numbers to census data
precinct_populations <- left_join(census, precinct_block_key)

# find the population of each race in each precinct
precinct_race <- precinct_populations %>% ungroup() %>%
  group_by(precinct, variable) %>%
  summarize(total = sum(value))

# find the proportion of each precinct that is Black/African American (Hispanic or not)
# (filter out N/A's - blocks with no corresponding precint - 
# this is justified because no people live in these blocks (population 0))
black_proportions <- precinct_race %>%
  group_by(precinct) %>%
  filter(!(is.na(precinct))) %>%
  mutate(props = total/sum(total)) %>%
  filter(variable == "Black_or_African_American_other" |
           variable == "Black_or_African_American_Hispanic_Latino") %>%
  summarize(pop_prop_black = sum(props)) %>%
  select(precinct, pop_prop_black) %>%
  ungroup()


sqf_race_dist <- sf_data1 %>% 
  select(addrpct, race)

sqf_black_prop <- sqf_race_dist %>%
  group_by(addrpct, race) %>%
  summarize(count = n()) %>%
  filter(!(is.na(addrpct))) %>%
  mutate(props = count/sum(count)) %>%
  filter(race == "B" | race == "P") %>%
  mutate(sqf_prop_black = sum(props)) %>%
  mutate(precinct = addrpct) %>%
  ungroup() %>%
  select(precinct, sqf_prop_black) %>%
  group_by(precinct) %>%
  summarize(sqf_prop_black = mean(sqf_prop_black))

proportions <- left_join(sqf_black_prop, black_proportions) %>%
  mutate(discrimination = sqf_prop_black > pop_prop_black)

no_discrimination <- proportions %>% filter(discrimination == FALSE)
