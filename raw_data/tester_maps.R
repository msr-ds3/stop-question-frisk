#Installing and loading packages
install.packages("tidycensus")
install.packages("totalcensus")
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
library(RgoogleMaps)

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
                        year = 2010, geometry = TRUE, tigris_use_cache = TRUE)


label = c("Total", "White alone", "Black or African American alone",
          "American Indian and Alaska Native alone", "Asian alone",
          "Native Hawaiian and Other Pacific Islander alone",
          "Some Other Race alone", "Two or More Races")

totals <- census %>% group_by(variable) %>% summarize(total = sum(value)) %>% select(total)

totals_by_race <- data.frame(var_race, label, totals)


#Vectors of arguments to be passed to get decennial census for NYC at block level
var_race <- c("P003001","P003002","P003003","P003004","P003005","P003006","P003007",
              "P003008")
counties <- c("Richmond", "Kings", "New York", "Queens", "Bronx")



wob <- c("P003002","P003003")
counties <- c("New York", "Kings", "Bronx")


#Loading the census
census_wob <- get_decennial(geography = "block", variables = wob,
                            state = "NY", county = counties,
                            year = 2010, geometry = TRUE, tigris_use_cache = TRUE)
#Plotting areas of white vs black populated area 
census %>%
  ggplot(aes(fill = value)) +
  facet_grid(~variable,) +
  geom_sf(color = NA) +
  coord_sf(crs = 26915) + 
  scale_fill_viridis_c()

#spreading the data
data <- spread(census, variable, value)

colnames(data) = c("GEO_ID", "Block Name","Total", "White alone", "Black or African American alone",
                   "American Indian and Alaska Native alone", "Asian alone",
                   "Native Hawaiian and Other Pacific Islander alone",
                   "Some Other Race alone", "Two or More Races")

View(head(data))


wob <- c("P003002","P003003")
counties <- c("New York", "Kings", "Bronx")

census_wob <- get_decennial(geography = "block", variables = wob,
                            state = "NY", county = counties,
                            year = 2010, geometry = TRUE, tigris_use_cache = TRUE)

census_wob <- spread(data, variable, value)

census_wob %>% 
  ggplot(aes(fill = value)) +
  facet_grid(~variable,) +
  geom_sf(color = NA) +
  coord_sf(crs = 26915) + 
  scale_fill_viridis_c()


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

#Plotting the Census data heatmap
census %>%
  ggplot(aes(fill = value)) +
  facet_grid(~variable,) +
  geom_sf(color = NA) +
  coord_sf(crs = 26915) + 
  scale_fill_viridis_c()


ggmap::register_google(key = "AIzaSyC43nsgttIA4Kp9LYUHXKU3UfNLdjCK1Eo")

source = "google"
get_map("New York City", source=source)

ny_acs <- get_acs(geography = "tract", 
              variables = "B19013_001", 
              state = "NY", 
              county = "New York", 
              geometry = TRUE)

ny <- get_decennial(geography = "tract", variables = vars, state = "NY", 
                        county = counties, year = 2010, tigris_use_cache = TRUE, geometry = TRUE)
colnames(ny)
colnames(ny_acs)
         
mapview(ny, zcol = "variable", legend = TRUE)




mapview(breweries)





