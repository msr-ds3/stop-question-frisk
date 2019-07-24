library(tidyverse)
library(httr)
library(rgdal)

load("log_model.RData")

r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nypp/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

precincts <- police_precincts$Precinct
tally <- rep(1, 77)
gender <- rep("M", 77)
tog <- data.frame(races, tally, gender)