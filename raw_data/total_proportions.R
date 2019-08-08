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

########## LOAD AND CREATE/CLEAN DATAFRAMES ##########

# Load stop and frisk data for 2003-2013
load("sqf_03_13.RData")

# Load census data with race distributions on the precinct level
load("census_race_data.RData")

# Load precinct shapefiles
load(here('clean_data', 'precinct_shape_file.RData'))

total_pop <- precinct_race %>% group_by(precinct) %>% summarize(total_p = sum(total))

# find the proportion of each precinct that is Black/African American (Hispanic or not)
# (filter out N/A's - blocks with no corresponding precint - 
# this is justified because no people live in these blocks (population 0))
total_sqf_pop <- sf_data1 %>%
            group_by(pct) %>% summarize(total_pop = sum(pct)) 
 
total_proportion <- left_join(total_pop, total_sqf_pop, by = c("precinct"="pct")) 

total_props <- total_proportion %>% 
  mutate(props = (total_p/total_pop)) %>% na.omit(total_props$precinct)


# Join the precinct shape data with the data about the precincts
joint_prop_total <- geo_join(police_precincts, total_props, "Precinct", "precinct")

mypopup <- paste0("Precinct: ", joint_prop_total$Precinct, "<br>", 
                   "Total Pop SQF: ", joint_prop_total$props)

mypal <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_prop_total$props
)

leaflet(joint_prop_total) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal(joint_prop_total$props),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal, 
            values = joint_prop_total$props, 
            position = "topleft", 
            title = "Total Pop SQF")

