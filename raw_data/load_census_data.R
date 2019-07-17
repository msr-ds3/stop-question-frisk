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
census_api_key('5365371ad843ba3249f2e88162f10edcfe529d87', install = TRUE)
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
census2 <- mutate(census, variable = as.factor(variable)) %>%
  mutate(geoid10 = as.numeric(GEOID)) %>% select(-GEOID)

# rename variables for clarity
census2$variable <- recode(census2$variable, P003004 = "American_Indian_and_Alaska_Native",
        P003005 = "Asian", P003006 = "Native_Hawaiian_and_Pacific_Islander",
        P003007 = "Other", P003008 = "Two_Or_More_Races",
        P005003 = "White_other", P005004 = "Black_or_African_American_other",
        P005011 = "White_Hispanic_Latino", P005012 = "Black_or_African_American_Hispanic_Latino")

# load file to map block-level data to precinct level
precinct_block_key <- read_csv("precinct_blocks_key.csv")

# add precinct numbers to census data
precinct_populations <- left_join(census2, precinct_block_key)

# find the population of each race in each precinct
precinct_race <- precinct_populations %>% ungroup() %>%
  group_by(precinct, variable) %>%
  summarize(total = sum(value))

# find the race with the most people in each precinct
# (filter out N/A's - blocks with no corresponding precint - 
# this is justified because no people live in these blocks)
precinct_majority_races <- precinct_race %>%
  group_by(precinct) %>%
  filter(!(is.na(precinct))) %>%
  filter(total == max(total)) %>%
  mutate(majority_race = variable) %>%
  select(precinct, majority_race) %>% ungroup()

# read file with police precinct shape data
# precinct_shapes <- read_csv("NYC_Police_Precinct_Shapes_4326.csv")

# read a different file with police precinct shape data - maybe this format is easier to work with?
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nypp/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

joint <- geo_join(police_precincts, precinct_majority_races, "Precinct", "precinct")

df <- joint

mypopup <- paste0("Precinct: ", df$Precinct, "<br>", 
                  "Majority Race: ", df$majority_race)

# Using leaflet to plot the precinct area polygons - working for non-tidy version
# of first version of precint shape data only
leaflet(df) %>%
  addTiles() %>% 
  addPolygons(popup = mypopup) %>%
  addProviderTiles("CartoDB.Positron")


# IGNORE BELOW THIS LINE - not using this anymore
mypal <- colorFactor(
  palette = "Spectral",
  domain = df$majority_race
)

mymap <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = df, 
              fillColor = ~mypal(df$majority_race), 
              color = "#b2aeae",
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = mypopup) %>%
  addLegend(pal = mypal, 
            values = df$majority_race, 
            position = "bottomright", 
            title = "Majority Race",
            labFormat = labelFormat(prefix = ""))

mymap

## NOTES:
# precint 121 used to be part of 122 (before 2013) - this is different in diff parts of the data...
