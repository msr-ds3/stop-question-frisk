library(pacman)
library(here)
library(tidyverse)
library(leaflet)
library(tigris)
library(tmap)
library(maptools)
library(tmaptools)
library(sp)


# Load stop and frisk data for 2003-2013
load(here("clean_data", "sqf_03_13.RData"))

# Load census data with race distributions on the precinct level
load(here("clean_data", "census_race_data.RData"))

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

sqf_race_dist <- sf_data1 %>% 
  select(addrpct, race) %>%
  filter(race != " " & race != "U" & race != "X") %>%
  mutate(race = recode_factor(race,"P" = "B", "I" = "Z"),
         race = recode_factor(race, "W" = "White", "B" = "Black",  "Q" ="Hispanic",
                              "A" = "Asian", "Z" = "Other")) %>%
  rename("precinct" = "addrpct") %>%
  group_by(precinct, race) %>%
  summarize(sqf_count = n()) %>%
  ungroup()

joint <- left_join(census_race_dist, sqf_race_dist) %>%
  mutate(stop_rate = sqf_count/census_count)

white_rates <- joint %>% filter(race == "White") %>% filter(precinct != 22)
black_rates <- joint %>% filter(race == "Black") %>% filter(precinct != 22)


# get police precinct shape data
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nypp/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

# Join stop rate ratios with precinct shape data
white_precinct_rates <- geo_join(police_precincts, white_rates, "Precinct", "precinct")
black_precinct_rates <- geo_join(police_precincts, black_rates, "Precinct", "precinct")

#Map the results:

mypopupW <- paste0("Precinct: ", white_precinct_rates$Precinct, "<br>", 
                  "Stop Rate: ", white_precinct_rates$stop_rate)

mypopupB <- paste0("Precinct: ", black_precinct_rates$Precinct, "<br>", 
                   "Stop Rate: ", black_precinct_rates$stop_rate)

mypal <- colorNumeric(
  palette = "YlOrRd",
  domain = c(-log10(35), log10(35))
)

white_stop_rates <- leaflet(white_precinct_rates) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal(log10(white_precinct_rates$stop_rate)),
              fillOpacity = .9,
              weight = 1,
              popup = mypopupW) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal, 
            values = c(-1.5,1.5),
            labFormat = labelFormat(transform = function(x) signif(10^x, 1)),
            position = "topleft",
            title = "White<br>Stop Rate")

white_stop_rates

black_stop_rates <- leaflet(black_precinct_rates) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal(log10(black_precinct_rates$stop_rate)),
              fillOpacity = .9,
              weight = 1,
              popup = mypopupB) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal, 
            values = c(-1.5,1.5),
            labFormat = labelFormat(transform = function(x) signif(10^x, 1)),
            position = "topleft",
            title = "Black<br>Stop Rate")

black_stop_rates

sessionInfo()