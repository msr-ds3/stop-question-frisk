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

load("census_race_data.RData")


#intensities <- sf_data1 %>% gather(sf_data1, `pf_hands``, `pf_hcuff``, `pf_wall", "pf_grnd", "pf_drwep", "pf_ptwep", "pf_pepsp", "pf_baton", key = "type_f_used", value = "force_used")  

low_intensity <-sf_data1 %>%
  mutate(pf_low = paste(pf_hands, pf_wall,pf_hcuff, sep = ""),
         pf_low = if_else(grepl("Y", pf_low), 1, 0))


high_intensity <- sf_data1 %>% mutate(pf_high = paste(pf_grnd, pf_drwep, pf_ptwep,
                                                             pf_baton, pf_pepsp, sep = ""),
                                      pf_high = if_else(grepl("Y",pf_high), 1, 0))

low_intensity_props <- low_intensity %>% group_by(addrpct) %>% summarize(props_low_force = mean(pf_low))


high_intensity_props <- high_intensity %>% group_by(addrpct) %>% summarize(props_high_force = mean(pf_high))


total_pop <- precinct_race %>% group_by(precinct) %>% summarize(total_p = sum(total))



# find the proportion of each precinct 
# (filter out N/A's - blocks with no corresponding precint - 
# this is justified because no people live in these blocks (population 0))
total_sqf_pop <- sf_data1 %>%
  group_by(pct) %>% summarize(total_pop = sum(pct)) 

total_proportion <- left_join(total_pop, total_sqf_pop, by = c("precinct"="pct")) 

total_props <- total_proportion %>% 
  mutate(props = (total_p/total_pop)) %>% na.omit(total_props$precinct)


# read in police precinct shape data
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nypp/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

# Join the precinct shape data with the data about the precincts
joint_prop_low <- geo_join(police_precincts, low_intensity_props, "Precinct", "addrpct")

joint_prop_high <- geo_join(police_precincts, high_intensity_props, "Precinct", "addrpct")


#Low intensity proportions for total population
mypopup <- paste0("Precinct: ", joint_prop_low$addrpct, "<br>", 
                  "Total Low Intensity Prop: ", joint_prop_low$props_low_force)

mypal <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_prop_low$props_low_force
)

leaflet(joint_prop_low) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal(joint_prop_low$props_low_force),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal, 
            values = joint_prop_low$props_low_force, 
            position = "topleft", 
            title = "Low Intensity Prop")


#High Intensity Proportions for total population
mypopup2 <- paste0("Precinct: ", joint_prop_high$addrpct, "<br>", 
                  "Total High Intensity Prop: ", joint_prop_high$props_high_force)

mypal2 <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_prop_high$props_high_force
)

leaflet(joint_prop_high) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal2(joint_prop_high$props_high_force),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup2) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal2, 
            values = joint_prop_high$props_high_force, 
            position = "topleft", 
            title = "High Intensity Prop")
