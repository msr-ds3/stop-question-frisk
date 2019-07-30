library(htmlwidgets)

r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nypp/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

precinct_map <- leaflet(police_precincts) %>%
  addTiles() %>%
  addPolygons(fillOpacity = .1, weight = 1)  %>% 
  addProviderTiles("CartoDB.Positron")

precinct_map

saveWidget(precinct_map, 
           "../figures/precinct_map.png", 
           selfcontained = FALSE)

counties = c("Richmond", "Kings", "New York", "Queens", "Bronx")

nyc_blocks <- blocks(state = "NY", county = counties, year = 2010)
plot(nyc_blocks)

leaflet(nyc_blocks) %>%
    addTiles() %>%
    addPolygons(fillOpacity = .1, weight = 1)  %>% 
    addProviderTiles("CartoDB.Positron")
