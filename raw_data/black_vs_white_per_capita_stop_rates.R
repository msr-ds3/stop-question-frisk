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

joint <- left_join(census_race_dist, sqf_race_dist)

# Calculate per capita stop rates (# stopped / # in population)
# for every race in every precinct
stop_rates <- joint %>%
  mutate(per_capita_stop_rate = sqf_count/census_count) %>%
  select(precinct, race, per_capita_stop_rate) %>%
  spread(race, per_capita_stop_rate)

# Calculate the ratio of Black to White per capita stop rates
proportions <- stop_rates %>%
  mutate(proportion = Black/White) %>%
  select(precinct, proportion)

# get police precinct shape data
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nypp/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

# Join stop rate ratios with precinct shape data
spatial_proportions <- geo_join(police_precincts, proportions, "Precinct", "precinct")

#Map the results:

mypopup <- paste0("Precinct: ", spatial_proportions$Precinct, "<br>", 
                  "Ratio Black to White Stop Rates: ", spatial_proportions$proportion)

mypal <- colorNumeric(
  palette = "Spectral",
  domain = c(-log(150), log(150)),
  reverse = TRUE
)

mypal2 <- colorNumeric(
  palette = "Spectral",
  domain = c(-150, 150),
  reverse = TRUE
)

per_capita_stop_rates <- leaflet(spatial_proportions) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal(log(spatial_proportions$proportion)),
              fillOpacity = .9,
              weight = 1,
              popup = mypopup) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal2, 
            values = exp(c(-log(150), log(150), length.out = 5)),
            position = "topleft",
            title = "Log Stop<br>Rate Ratio")

per_capita_stop_rates

saveWidget(per_capita_stop_rates, 
           "../figures/per_capita_stop_rates.html", 
           selfcontained = FALSE)
webshot("../figures/per_capita_stop_rates.html",
        file = "../figures/per_capita_stop_rates.png",
        cliprect = "viewport")

# Map explanation: Dark red areas have high levels of discrimination against blacks
# white areas show no  discrimination, green/blue areas are biased against whites.
# Precinct 121 is gray because it was only created in 2013, after the 2010 census.
