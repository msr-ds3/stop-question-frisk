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

load("census_race_data.RData")

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

nyc_blocks <- block_groups(state = "NY", county = "New York", year = 2010)
mypopup <- paste0(nyc_blocks$GEOID10)
leaflet(nyc_blocks) %>%
  addTiles() %>% 
  addPolygons(popup = mypopup) %>%
  addProviderTiles("CartoDB.Positron")
nyc_tracts <- tracts(state = "NY", county = "New York", year = 2010)
plot(nyc_tracts)
plot(nyc_blocks)

# Conclusion: The proportion of the people stopped in each precinct that are black
# is greater than the proportion of the people who live in that precinct that are black
# for every precinct except precinct 22 - Central Park - which has a population of 25
# (16 of whom are Black/African American)
