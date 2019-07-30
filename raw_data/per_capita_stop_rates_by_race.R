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
library(htmlwidgets)
library(webshot)

# Load stop and frisk data for 2003-2013
load("sqf_03_13.RData")

# Load census data with race distributions on the precinct level
load("census_race_data.RData")

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

white_rates <- joint %>% filter(race == "White")
black_rates <- joint %>% filter(race == "Black")


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
  palette = "Spectral",
  domain = c(log(1/600), log(600)),
  reverse = TRUE
)

white_stop_rates <- leaflet(white_precinct_rates) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal(log(white_precinct_rates$stop_rate)),
              fillOpacity = .9,
              weight = 1,
              popup = mypopupW) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal, 
            values = log(white_precinct_rates$stop_rate),
            position = "topleft",
            labels = white_precinct_rates$stop_rate,
            title = "White<br>Stop Rate")

white_stop_rates

black_stop_rates <- leaflet(black_precinct_rates) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal(log(black_precinct_rates$stop_rate)),
              fillOpacity = .9,
              weight = 1,
              popup = mypopupB) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal, 
            values = log(black_precinct_rates$stop_rate),
            position = "topleft",
            labels = black_precinct_rates$stop_rate,
            title = "Black<br>Stop Rate")

black_stop_rates

saveWidget(per_capita_stop_rates, 
           "../figures/per_capita_stop_rates.html", 
           selfcontained = FALSE)
webshot("../figures/per_capita_stop_rates.html",
        file = "../figures/per_capita_stop_rates.png",
        cliprect = "viewport")
  