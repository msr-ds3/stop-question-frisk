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


census_api_key('5365371ad843ba3249f2e88162f10edcfe529d87', install = TRUE)
readRenviron("~/.Renviron")

#variables to load from census data
vars = c("P003001", "P003002", "P003003", "P003004", "P003005", "P003006", "P003007", "P003008")

#counties to load from census data
counties = c("Richmond", "Kings", "New York", "Queens", "Bronx")

#variable names
label = c("Total", "White alone", "Black or African American alone",
          "American Indian and Alaska Native alone", "Asian alone",
          "Native Hawaiian and Other Pacific Islander alone",
          "Some Other Race alone", "Two or More Races")

#load census data
census <- get_decennial(geography = "block", variables = vars, state = "NY", 
                        county = counties, year = 2010, geometry = TRUE, tigris_use_cache = TRUE)

#summarize census data
totals <- census %>% group_by(variable) %>% summarize(total = sum(value)) %>% select(total)
totals_by_race <- data.frame(vars, label, totals)

#spread census data by race
data <- spread(census, variable, value)

#rename columns for clarity
colnames(data) = c("GEOID", "Block Name", "Total", "White", "Black_African_American", 
                   "American_Indian_Alaska_Native", 
                   "Asian", "Native_Hawaiian_Pacific_Islander", "Other", "Two_Or_More_Races")

#convert counts to fractions of total
data <- data %>% mutate(frac_white = White/Total, frac_black = Black_African_American/Total,
                        frac_alaskan = American_Indian_Alaska_Native/Total, frac_asian = Asian/Total, 
                        frac_islander = Native_Hawaiian_Pacific_Islander/Total,
                        frac_other = Other/Total, frac_mixed = Two_Or_More_Races/Total)


census %>%
  ggplot(aes(fill = value)) +
  facet_grid(~variable,) +
  geom_sf(color = NA) +
  coord_sf(crs = 26915) + 
  scale_fill_viridis_c()


r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

leaflet(census) %>%
  addTiles() %>% 
  addPolygons(popup = ~neighborhood) %>%
  addProviderTiles("CartoDB.Positron")
