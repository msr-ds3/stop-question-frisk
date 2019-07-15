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
library(raster)
library(tibble)
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





#Getting and reading police precincts JSON file
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nypp/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

glimpse(police_precincts)

#tidying the police precincts
police_precincts <- tidy(police_precincts)


#Creating points from lat and long from police_precincts

lats<- police_precincts$lat
lngs<-police_precincts$long
points <- data.frame(lats, lngs)

View(points)

points_spdf <- points
coordinates(points_spdf) <- ~lngs + lats 

proj4string(points_spdf) <- proj4string(police_precincts)

matches <- over(points_spdf, police_precincts)
points <- cbind(points, matches)
points



#Creating spatial ggplot of the police_precinct data
ggplot() + 
  geom_polygon(data=police_precincts, aes(x=long, y=lat, group=group))

#Testing leaflet package
leaflet(police_precincts) %>%
  addTiles() %>% 
  addPolygons(popup = ~neighborhood) %>%
  addProviderTiles("CartoDB.Positron")


leaflet(census) %>%
  addTiles() %>% 
  addPolygons(popup = ~Precinct) %>%
  addProviderTiles("CartoDB.Positron")



