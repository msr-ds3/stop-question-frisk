library(tidyverse)
library(dplyr)
library(tidycensus)
library(totalcensus)
library(sf) 
library(tmap)
library(tmaptools)
library(tigris)
library(leaflet)
library(sp)
library(ggplot2)
library(ggmap)
library(broom)
library(httr)
library(rgdal)
library(rgeos)
library(maptools)


#Geolocating NYC
nyc_map <- get_map(location = c(lon = -74.1, lat = 41.1), maptype = "terrain", zoom = 11)

ggmap(nyc_map)



#Getting and reading police precincts JSON file
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nypp/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

summary(police_precincts)

#tidying the police precincts
police_precincts <- tidy(police_precincts)


ggmap(nyc_map) + 
  geom_polygon(data=police_precincts, aes(x=long, y=lat, group=group), color="blue", fill=NA)




set.seed(42)
lats <- 40.7544882 + rnorm(10)/100
lngs <- -73.9879923 + rnorm(10)/200
points <- data.frame(lat=lats, lng=lngs)
points


#Creating points from lat and long from police_precincts

lats<- police_precincts$lat
lngs<-police_precincts$long
points <- data.frame(lats, lngs)

#Creating spatial joins with inbuilt functions (make sure to load libraries on top)
points_spdf <- points
coordinates(points_spdf) <- ~lngs + lats 
proj4string(points_spdf) <- proj4string(police_precincts)
police_precincts$Precinct <- as.factor(police_precincts$Precinct)
matches <- over(points_spdf, police_precincts)
points <- cbind(points, matches)
points


#There are NA Values in precinct, this here will remove them and also count all precinct points
points_by_precinct<- points %>% filter(!is.na(Precinct))%>% 
  group_by(Precinct) %>%
  summarize(num_points=n()) 


#Tried to plot the precinct data however this needs follow up (Google API)
plot_data <- tidy(police_precincts, region="Precinct") %>%
  left_join(., points_by_precinct, by=c("id"="Precinct")) %>%
  filter(!is.na(num_points))


ggmap(nyc_map) + 
  geom_polygon(data=plot_data, aes(x=long, y=lat, group=group, fill=num_points), alpha=0.75)



#Using leaflect to plot the precinct area polygons
leaflet(police_precincts) %>%
  addTiles() %>% 
  addPolygons(popup = ~Precinct) %>%
  addProviderTiles("CartoDB.Positron")


#Creating spatial ggplot of the police_precinct data
ggmap() + 
  geom_polygon(data=police_precincts, aes(x=long, y=lat, group=group))


ggmap::register_google(key = "AIzaSyBDkCtkNhKl1YZAZGC-lOPOYNNdTcP3QNA")

nyc_map <- get_map(location = c(lon = -74.00, lat = 40.71), maptype = "terrain", zoom = 11)
ggmap(nyc_map)


police_precincts <- tidy(police_precincts)

black_proportions <- black_proportions %>% mutate(precinct = as.character(precinct))

jp <- police_precincts %>%
  left_join(black_proportions, by=c("id"="precinct"))

nyc_map <- get_map(location = c(lon = -74.00, lat = 40.71), maptype = "terrain", zoom = 11)

mypal <- colorNumeric(
  palette = "YlOrRd",
  domain = jp$props
)

ggmap(nyc_map) + 
  geom_polygon(data=jp, aes(x=long, y=lat, group=group, fill = props), color = "")



