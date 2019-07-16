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

load("sqf_03_18.RData")

census_api_key('5365371ad843ba3249f2e88162f10edcfe529d87', install = TRUE)
readRenviron("~/.Renviron")

#variables to load from census data
vars = c("P003004", "P003005", "P003006", "P003007", "P003008",
         "P005003", "P005004", "P005011", "P005012")

#counties to load from census data
counties = c("Richmond", "Kings", "New York", "Queens", "Bronx")

#variable names
label = c("White alone", "Black or African American alone",
          "American Indian and Alaska Native alone", "Asian alone",
          "Native Hawaiian and Other Pacific Islander alone",
          "Some Other Race alone", "Two or More Races")

#load census data
census <- get_decennial(geography = "tract", variables = vars, state = "NY", 
                        county = counties, year = 2010, tigris_use_cache = TRUE)

census <- mutate(census, variable = as.factor(variable))

census$variable <- recode(census$variable, P003004 = "American_Indian_Alaska_Native",
        P003005 = "Asian", P003006 = "Native_Hawaiian_Pacific_Islander",
        P003007 = "Other", P003008 = "Two_Or_More_Races",
        P005003 = "White_other", P005004 = "Black_other",
        P005011 = "White_Hispanic_Latino", P005012 = "Black_Hispanic_Latino")

census_plus <- census %>%
  group_by(GEOID) %>%
  filter(value == max(value)) %>%
  mutate(majority_race = variable) %>%
  select(GEOID, majority_race)

majorities <- left_join(data.frame(census), data.frame(census_plus), by = c("GEOID", "GEOID"))

#spread census data by race
data <- data.frame(spread(majorities, variable, value))

nyc <- tracts(state = "NY", county = counties, year = 2010)

joint <- geo_join(nyc, data, "GEOID10", "GEOID")

df <- joint

mypal <- colorFactor(
  palette = "YlGnBu",
  domain = df$majority_race
)

mypopup <- paste0("GEOID: ", df$GEOID, "<br>", "Majority race: ", df$majority_race)

mymap <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = df, 
              fillColor = ~mypal(df$majority_race), 
              color = "#b2aeae",
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = mypopup) %>%
  addLegend(pal = mypal, 
            values = df$majority_race, 
            position = "bottomright", 
            title = "Majority Race",
            labFormat = labelFormat(prefix = ""))

mymap
#Getting and reading police precincts JSON file
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nypp/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

#tidying the police precincts
police_precinct <- tidy(police_precincts)

#Using leaflet to plot the precinct area polygons
leaflet(police_precincts) %>%
  addTiles() %>% 
  addPolygons(popup = ~Precinct) %>%
  addProviderTiles("CartoDB.Positron")



