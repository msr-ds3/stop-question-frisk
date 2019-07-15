library(tidyverse)
library(dplyr)
library(tidycensus)
library(totalcensus)
library(sf) 
library(ggplot2)
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

#Cencus API key retrieved from ACS website
census_api_key("5365371ad843ba3249f2e88162f10edcfe529d87", install=TRUE)
readRenviron("~/.Renviron")

#Vectors of arguments to be passed to get decennial census for NYC at block level
var_race <- c("P003001","P003002","P003003","P003004","P003005","P003006","P003007",
              "P003008")
counties <- c("Richmond", "Kings", "New York", "Queens", "Bronx")


#Loading the census
census <- get_decennial(geography = "block", variables = var_race,
                        state = "NY", county = counties,
                        year = 2010, geometry = TRUE)


label = c("Total", "White alone", "Black or African American alone",
          "American Indian and Alaska Native alone", "Asian alone",
          "Native Hawaiian and Other Pacific Islander alone",
          "Some Other Race alone", "Two or More Races")

totals <- census %>% group_by(variable) %>% summarize(total = sum(value)) %>% select(total)
totals
totals_by_race <- data.frame(var_race, label, totals)

totals_by_race

census %>%
  ggplot(aes(fill = value)) +
  facet_wrap(~variable) +
  geom_sf(color = NA) +
  coord_sf(crs = 26915) + 
  scale_fill_viridis_c()



#Vectors of arguments to be passed to get decennial census for NYC at block level
var_race <- c("P003001","P003002","P003003","P003004","P003005","P003006","P003007",
              "P003008")
counties <- c("Richmond", "Kings", "New York", "Queens", "Bronx")



wob <- c("P003002","P003003")
counties <- c("New York", "Kings", "Bronx")


#Loading the census
census <- get_decennial(geography = "block", variables = wob,
                        state = "NY", county = counties,
                        year = 2010, geometry = TRUE, tigris_use_cache = TRUE)

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



