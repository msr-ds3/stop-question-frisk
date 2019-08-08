library(pacman)
library(here)
library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(tigris)
library(leaflet)
library(sp)
library(maptools)
library(broom)
library(htmlwidgets)
library(webshot)

# Load stop and frisk data for 2003-2013
load(here("clean_data", "sqf_03_13.RData"))

# Load census data with race distributions on the precinct level
load(here("clean_data", "census_race_data.RData"))

# Load precinct shapefiles
load(here('clean_data', 'precinct_shape_file.RData'))

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

# Join stop rate ratios with precinct shape data
spatial_proportions <- geo_join(police_precincts, proportions, "Precinct", "precinct")

#Map the results:

mypopup <- paste0("Precinct: ", spatial_proportions$Precinct, "<br>", 
                  "Ratio Black to White Stop Rates: ", round(spatial_proportions$proportion, 2))

mypal <- colorNumeric(
  palette = "Spectral",
  domain = c(-log10(150), log10(150)),
  reverse = TRUE
)

per_capita_stop_rates <- leaflet(spatial_proportions) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal(log10(spatial_proportions$proportion)),
              fillOpacity = .9,
              weight = 1,
              popup = mypopup) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal, 
            values = log10(spatial_proportions$proportion),
            #labFormat = mylabformat,
            labFormat = labelFormat(transform = function(x) signif(10^x, 1)),
            position = "topleft",
            title = "Stop Rate<br>Ratio")

per_capita_stop_rates

saveWidget(per_capita_stop_rates, 
           "../figures/per_capita_stop_rates.html", 
           selfcontained = FALSE)
webshot("../figures/per_capita_stop_rates.html",
        file = "../figures/per_capita_stop_rates.png",
        cliprect = "viewport")


sessionInfo()

# Map explanation: Dark red areas have high levels of discrimination against blacks
# white areas show no  discrimination, green/blue areas are biased against whites.
# Precinct 121 is gray because it was only created in 2013, after the 2010 census.