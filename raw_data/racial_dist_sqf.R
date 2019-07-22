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
vars = c("P003003", "P003004", "P003005", "P003006", "P003007", "P003008",
         "P005003", "P005011")

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
census$variable <- recode(census$variable, P003003 = "Black",
                          P003004 = "American_Indian_Alaska_Native",
                          P003005 = "Asian_Pacific_Islander", 
                          P003006 = "Asian_Pacific_Islander",
                          P003007 = "Other", 
                          P003008 = "Two_Or_More_Races",
                          P005003 = "White",
                          P005011 = "White_Hispanic")

# load file to map block-level data to precinct level
precinct_block_key <- read_csv("precinct_blocks_key.csv")

# add precinct numbers to census data
precinct_populations <- left_join(census, precinct_block_key)

# find the population of each race in each precinct
precinct_race <- precinct_populations %>% ungroup() %>%
  filter(!is.na(precinct)) %>%
  group_by(precinct, variable) %>%
  summarize(pop_total = sum(value)) %>% ungroup()

sqf_race_dist <- sf_data1 %>% 
  ungroup() %>%
  mutate(race = recode_factor(race,
                              "A" = "Asian_Pacific_Islander",
                              "I" = "American_Indian_Alaska_Native",
                              "P" = "Black",
                              "Q" = "White_Hispanic",
                              "B" = "Black",
                              "W" = "White",
                              "Z" = "Other",
                              "U" = "Unknown")) %>%
  select(addrpct, race) %>%
  group_by(addrpct, race) %>%
  summarize(num_stops = n())


colnames(precinct_race) = c("precinct", "race", "pop_total")
colnames(sqf_race_dist) = c("precinct", "race", "num_stops")

stop_race_dist <- full_join(precinct_race, sqf_race_dist) %>%
  filter(race == "Black" | race == "White" | race == "White_Hispanic") %>%
  filter(!is.na(precinct)) %>% filter(precinct != 121)

# reset digits to the default
options(digits = 7)

# read in police precinct shape data
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nypp/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

# Join the precinct shape data with the data about the precincts
joint_sqf_prop <- geo_join(police_precincts, sqf_black_prop, "Precinct", "addrpct")

# Map the proportion of each precinct that is black
mypopup <- paste0("Precinct: ", joint_prop$Precinct, "<br>", 
                  "Population Proportion Black: ", joint_prop$prop)

mypal <- colorNumeric(
  palette = "YlOrRd",
  domain = 0:1
)

leaflet(joint_prop) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal(joint_prop$prop),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal, 
            values = joint_prop$prop, 
            position = "topleft", 
            title = "Population Proportion Black")

