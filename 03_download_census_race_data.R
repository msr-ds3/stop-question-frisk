# library(pacman)
library(here)
library(tidyverse)
library(tidycensus)


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

# rename variables for clarity
census$variable <- recode(census$variable, P003004 = "American_Indian_and_Alaska_Native",
                          P003005 = "Asian", P003006 = "Native_Hawaiian_and_Pacific_Islander",
                          P003007 = "Other", P003008 = "Two_Or_More_Races",
                          P005003 = "White_other", P005004 = "Black_or_African_American_other",
                          P005011 = "White_Hispanic_Latino", P005012 = "Black_or_African_American_Hispanic_Latino")


# load file to map block-level data to precinct level
precinct_block_key <- read_csv(here("clean_data", "precinct_blocks_key.csv"))

# add precinct numbers to census data
precinct_populations <- left_join(census, precinct_block_key)

# find the population of each race in each precinct
race_by_precinct <- precinct_populations %>% ungroup() %>%
  group_by(precinct, variable) %>%
  summarize(total = sum(value))

# Rename race variables for clarity and so that they are grouped as
# Fryer grouped them. Summarize the population of each race in each precinct.
precinct_race <- race_by_precinct %>% filter(!is.na(precinct)) %>%
  mutate(variable = recode_factor(variable,"Two_Or_More_Races" = "Other", 
                                  "American_Indian_and_Alaska_Native" = "Other",
                                  "Native_Hawaiian_and_Pacific_Islander" = "Other",
                                  "Black_or_African_American_Hispanic_Latino" = "Black",
                                  "Black_or_African_American_other" = "Black",
                                  "White_other" = "White",
                                  "White_Hispanic_Latino" = "Hispanic")) %>%
  rename("race" = "variable", "census_count" = "total") %>%
  group_by(precinct, race) %>%
  summarize(census_count = sum(census_count)) %>%
  ungroup()

save(precinct_race, file = here("clean_data", "census_race_data.RData"))


sessionInfo()
