library(pacman)
library(here)
library(tidyverse)
library(leaflet)
library(tigris)
library(tmap)
library(maptools)
library(tmaptools)
library(sp)
library(webshot)
library(htmlwidgets)


# Load stop and frisk data for 2003-2013
load(here("clean_data", "sqf_03_13.RData"))

# Load census data with race distributions on the precinct level
load(here("clean_data", "census_race_data.RData"))

# Load precinct shapefiles
load(here('clean_data', 'precinct_shape_file.RData'))

# Rename and summarize SQF data similarly
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

# Join the data frames
joint <- left_join(precinct_race, sqf_race_dist) %>%
  mutate(stop_rate = sqf_count/census_count)

# Create separate data frames with only White and Black race data
white_rates <- joint %>% filter(race == "White") %>% filter(precinct != 22)
black_rates <- joint %>% filter(race == "Black") %>% filter(precinct != 22)

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

# Create a map of NYC with the color of each precinct indicating the
# probability of being stopped there for a white person
# Note: Coloring is on a log scale, but the popups and legend are not
# (This was done for increased human-readability)
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

#white_stop_rates

saveWidget(white_stop_rates, 
           here("figures", "white_stop_rates_by_precinct.html"),
           selfcontained = FALSE)
webshot(here("figures", "white_stop_rates_by_precinct.html"),
        file = here("figures", "white_stop_rates_by_precinct.png"),
        cliprect = "viewport")

# Same as above, but for a black person
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

#black_stop_rates

saveWidget(black_stop_rates, 
           here("figures", "black_stop_rates_by_precinct.html"),
           selfcontained = FALSE)
webshot(here("figures", "black_stop_rates_by_precinct.html"),
        file = here("figures", "black_stop_rates_by_precinct.png"),
        cliprect = "viewport")

sessionInfo()