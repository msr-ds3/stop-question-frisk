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
  select(addrpct, race)

census_race_dist <- precinct_race %>% filter(!is.na(precinct)) %>%
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

sqf_race_dist <- sqf_race_dist %>%
  ungroup() %>%
  filter(race != " " & race != "U" & race != "X") %>%
  mutate(race = recode_factor(race,"P" = "B", "I" = "Z"),
         race = recode_factor(race, "W" = "White", "B" = "Black",  "Q" ="Hispanic",
                              "A" = "Asian", "Z" = "Other")) %>%
  rename("precinct" = "addrpct") %>%
  group_by(precinct, race) %>%
  summarize(sqf_count = n()) %>%
  ungroup()

joint <- left_join(census_race_dist, sqf_race_dist)

stop_rates <- joint %>%
  mutate(per_capita_stop_rate = sqf_count/census_count) %>%
  select(precinct, race, per_capita_stop_rate) %>%
  spread(race, per_capita_stop_rate)

proportions <- stop_rates %>%
  mutate(proportion = Black/White) %>%
  select(precinct, proportion)

# get the list of police precincts
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nypp/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

spatial_proportions <- geo_join(police_precincts, proportions, "Precinct", "precinct")

mypopup <- paste0("Precinct: ", spatial_proportions$Precinct, "<br>", 
                  "Ratio Black to White Stop Rates: ", spatial_proportions$proportion)

mypal <- colorNumeric(
  palette = "YlOrRd",
  domain = log(spatial_proportions$proportion)
)

leaflet(spatial_proportions) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal(log(spatial_proportions$proportion)),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal, 
            values = log(spatial_proportions$proportion), 
            position = "topleft", 
            title = "Stop Rate Ratio")
