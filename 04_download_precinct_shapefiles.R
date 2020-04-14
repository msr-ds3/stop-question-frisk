if (!require("pacman")) install.packages("pacman")
pacman::p_load(httr, rgdal, here)

# get the police precinct shapefiles
r <- GET('https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_POLICE_PRECINCTS/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

# save shapefiles
save(police_precincts, file = here('clean_data', 'precinct_shape_file.RData'))
