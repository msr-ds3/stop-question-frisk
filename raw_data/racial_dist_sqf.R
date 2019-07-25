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

# Load census data with race distributions on the precinct level
load("census_race_data.RData")

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

