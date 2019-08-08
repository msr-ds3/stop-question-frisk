library(pacman)
library(httr)
library(rgdal)
library(here)

# get the police precinct shapefiles
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nypp/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

# save shapefiles
save(police_precincts, file = here('clean_data', 'precinct_shape_file.RData'))
