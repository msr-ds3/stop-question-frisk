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

load("sqf_03_13.RData")

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

options(digits = 15)

census2 <- mutate(census, variable = as.factor(variable)) %>%
  mutate(geoid10 = as.numeric(GEOID)) %>% select(-GEOID)

census2$variable <- recode(census2$variable, P003004 = "American_Indian_and_Alaska_Native",
        P003005 = "Asian", P003006 = "Native_Hawaiian_and_Pacific_Islander",
        P003007 = "Other", P003008 = "Two_Or_More_Races",
        P005003 = "White_other", P005004 = "Black_or_African_American_other",
        P005011 = "White_Hispanic_Latino", P005012 = "Black_or_African_American_Hispanic_Latino")

precinct_block_key <- read_csv("precinct_blocks_key.csv")

precinct_populations <- left_join(census2, precinct_block_key)

precinct_race <- precint_populations %>% ungroup() %>%
  group_by(precinct, variable) %>%
  summarize(total = sum(value))

precinct_majority_races <- precinct_race %>%
  group_by(precinct) %>%
  filter(!(is.na(precinct))) %>%
  filter(total == max(total)) %>%
  mutate(majority_race = variable) %>%
  select(precinct, majority_race) %>% ungroup()

precinct_shapes <- read_csv("NYC_Police_Precinct_Shapes_4326.csv")

#Try getting precinct shapes this way instead?
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nypp/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

precinct_shapes_races <- left_join(precinct_majority_races, precinct_shapes, by = c("precinct" = "Precinct"))

#Using leaflet to plot the precinct area polygons - not working
leaflet(precinct_shapes_races) %>%
  addTiles() %>% 
  addPolygons(popup = ~majority_race) %>%
  addProviderTiles("CartoDB.Positron")



nyc <- tracts(state = "NY", county = counties, year = 2010)

joint <- geo_join(nyc, data, "GEOID10", "GEOID")

df <- joint

mypal <- colorFactor(
  palette = "Spectral",
  domain = df$majority_race
)

mypopup <- paste0("GEOID: ", df$GEOID, "<br>", "Majority race: ", df$majority_race)

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

