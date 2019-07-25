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

#Filter out only white population
precinct_white <- precinct_race %>% filter(variable == "White_other" | variable == "White_Hispanic_Latino")


# find the proportion of each precinct that is White
white_proportions <- precinct_race %>%
                     group_by(precinct) %>%
                     filter(!(is.na(precinct))) %>%
                     mutate(props = total/sum(total)) %>%
                     filter(variable == "White_other" |
                     variable == "White_Hispanic_Latino") %>%
                     summarize(prop = sum(props)) %>%
                     select(precinct, prop) %>%
                     ungroup()

#Low intensity 
low_intensity <- sf_data1 %>%
                 mutate(pf_low = paste(pf_hands, pf_wall,pf_hcuff, sep = ""),
                 pf_low = if_else(grepl("Y", pf_low), 1, 0))

View(low_intensity)

high_intensity <- sf_data1 %>% mutate(pf_high = paste(pf_grnd, pf_drwep, pf_ptwep,
                               pf_baton, pf_pepsp, sep = ""),
                               pf_high = if_else(grepl("Y",pf_high), 1, 0))


low_intensity_race <- low_intensity %>% filter(pf_low == 1)%>% group_by(race, addrpct) %>% 
  select(pf_low, race, addrpct)


View(low_intensity_race)

#Low proportions white
low_props_white <- low_intensity %>% filter(pf_low == 1) %>% 
  mutate(tally = 1) %>%
  group_by(addrpct, race) %>% 
  summarize(total = sum(tally)) %>%
  ungroup() %>% group_by(addrpct) %>%
  mutate(prop = total/sum(total)) %>%
  filter(race == "W") %>%
  summarize(low_force_prop_white = sum(prop)) 



high_props_white <- high_intensity %>% filter(pf_high == 1) %>% 
  mutate(tally = 1) %>%
  group_by(addrpct, race) %>% 
  summarize(total = sum(tally)) %>%
  ungroup() %>% group_by(addrpct) %>% 
  mutate(prop = total/sum(total)) %>%
  filter(race == "W") %>%
  summarize(high_force_prop_white = sum(prop))



# read in police precinct shape data
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nypp/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

# Join the precinct shape data with the data about the precincts
joint_prop_low_white <- geo_join(police_precincts, low_props_white, "Precinct", "addrpct")

joint_prop_high_white <- geo_join(police_precincts, high_props_white, "Precinct", "addrpct")


#Low intensity proportions for total population
mypopup <- paste0("Precinct: ", joint_prop_low_white$addrpct, "<br>", 
                  "White Low Intensity Prop: ", joint_prop_low_white$low_force_prop_white)

mypal <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_prop_low_white$low_force_prop_white
)

leaflet(joint_prop_low_white) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal(joint_prop_low_white$low_force_prop_white),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal, 
            values = joint_prop_low_white$low_force_prop_white, 
            position = "topleft", 
            title = "Low Intensity Prop")


#High Intensity Proportions for total population
mypopup2 <- paste0("Precinct: ", joint_prop_high_white$addrpct, "<br>", 
                   "White High Intensity Prop: ", joint_prop_high_white$high_force_prop_white)

mypal2 <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_prop_high_white$high_force_prop_white
)

leaflet(joint_prop_high) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal2(joint_prop_high_white$high_force_prop_white),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup2) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal2, 
            values = joint_prop_high_white$high_force_prop_white, 
            position = "topleft", 
            title = "High Intensity Prop")




#Feature with Radio buttons on map
mypopup <- paste0("Precinct: ", joint_prop_low_white$addrpct, "<br>", 
                  "White Low Intensity Prop: ", joint_prop_low_white$low_force_prop_white)

mypal <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_prop_low_white$low_force_prop_white
)

mypopup2 <- paste0("Precinct: ", joint_prop_high_white$addrpct, "<br>", 
                   "White High Intensity Prop: ", joint_prop_high_white$high_force_prop_white)

mypal2 <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_prop_high_white$high_force_prop_white
)

map_white_intensities <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>%
                   
  addPolygons(data=joint_prop_low_white,
              fillColor = ~mypal(joint_prop_low_white$low_force_prop_white),
              weight = 2,
              opacity = 1,
              color = "blue",
              dashArray = "3",
              fillOpacity = 0.7,
              popup = mypopup, group="Low") %>%
  addPolygons(data=joint_prop_high_white,
              fillColor = ~mypal2(joint_prop_high_white$high_force_prop_white),
              weight = 2,
              opacity = 1,
              color = "blue",
              dashArray = "3",
              fillOpacity = 0.7,
              popup = mypopup2, group="High")

map_white_intensities %>% addLayersControl(c("Low", "High"),
                                options = layersControlOptions(collapsed = FALSE))
