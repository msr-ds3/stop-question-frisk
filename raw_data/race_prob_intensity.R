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
library(dplyr)
########## LOAD AND CREATE/CLEAN DATAFRAMES ##########

# Load stop and frisk data for 2003-2013

load("sqf_03_13.RData")


#Splitting intensity Low and High
low_intensity <-sf_data1 %>%
  mutate(pf_low = paste(pf_hands, pf_wall,pf_hcuff, sep = ""),
         pf_low = if_else(grepl("Y", pf_low), 1, 0))


high_intensity <- sf_data1 %>% 
  mutate(pf_high = paste(pf_grnd, pf_drwep, pf_ptwep,
                         pf_baton, pf_pepsp, sep = ""),
         pf_high = if_else(grepl("Y",pf_high), 1, 0))


# the probability of having low intensity force used on you,
# given your race and precinct, conditional on being stopped
prob_low_intensity_given_race <- low_intensity %>%
    filter(race != "U") %>%
    mutate(race = recode_factor(race,"P" = "B", "I" = "Z"), 
           race = recode_factor(race, "W" = "White", "B" = "Black",  
           "Q" ="Hispanic",  "A" = "Asian", "Z" = "Other")) %>%
    group_by(addrpct, race) %>%
    summarize(prob = mean(pf_low)) 
  

prob_high_intensity_given_race <- high_intensity %>%
  filter(race != "U") %>%
  mutate(race = recode_factor(race,"P" = "B", "I" = "Z"), 
         race = recode_factor(race, "W" = "White", "B" = "Black",  
         "Q" ="Hispanic",  "A" = "Asian", "Z" = "Other")) %>%
  group_by(addrpct, race) %>%
  summarize(prob = mean(pf_high))


#Splitting Intensities into different dataframes given race
prob_low_white <- prob_low_intensity_given_race  %>% filter(race == "White")
prob_low_black <- prob_low_intensity_given_race  %>% filter(race == "Black")

prob_high_white <- prob_high_intensity_given_race  %>% filter(race == "White")
prob_high_black <- prob_high_intensity_given_race  %>% filter(race == "Black")



# read in police precinct shape data
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nypp/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)


# Join the precinct shape data with the data about the precincts
joint_prop_low_white <- geo_join(police_precincts, prob_low_white, "Precinct", "addrpct")

joint_prop_low_black <- geo_join(police_precincts, prob_low_black, "Precinct", "addrpct")

joint_prop_high_white <- geo_join(police_precincts, prob_high_white, "Precinct", "addrpct")

joint_prop_high_black <- geo_join(police_precincts, prob_high_black, "Precinct", "addrpct")



#Feature with Radio buttons on map

#Low White
mypopup <- paste0("Precinct: ", joint_prop_low_white$addrpct, "<br>", 
                  "White Low Intensity Prop: ", joint_prop_low_white$prob)

mypal <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_prop_low_white$prob
)

#Low Black
mypopup2 <- paste0("Precinct: ", joint_prop_low_black$addrpct, "<br>", 
                   "Black High Intensity Prop: ", joint_prop_low_black$prob)

mypal2 <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_prop_low_black$prob
)

#High White
mypopup3 <- paste0("Precinct: ", joint_prop_high_white$addrpct, "<br>", 
                  "White Low Intensity Prop: ", joint_prop_high_white$prob)

mypal3 <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_prop_high_white$prob
)

#High Black
mypopup4 <- paste0("Precinct: ", joint_prop_high_black$addrpct, "<br>", 
                   "Black High Intensity Prop: ", joint_prop_high_black$prob)

mypal4 <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_prop_high_black$prob
)


#Generating leaflet map with all the options
leafletmap <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=joint_prop_low_white,
              fillColor = ~mypal(joint_prop_low_white$prob),
              weight = 2,
              opacity = 1,
              fillOpacity = 0.7,
              popup = mypopup, group="Low-White") %>%
  addPolygons(data=joint_prop_low_black,
              fillColor = ~mypal2(joint_prop_low_black$prob),
              weight = 2,
              opacity = 1,
              fillOpacity = 0.7,
              popup = mypopup2, group="Low-Black") %>%
  addPolygons(data=joint_prop_high_white,
              fillColor = ~mypal3(joint_prop_high_white$prob),
              weight = 2,
              opacity = 1,
              fillOpacity = 0.7,
              popup = mypopup3, group="High-White") %>%
  addPolygons(data=joint_prop_high_black,
              fillColor = ~mypal4(joint_prop_high_black$prob),
              weight = 2,
              opacity = 1,
              fillOpacity = 0.7,
              popup = mypopup4, group="High-Black")


leafletmap %>% addLayersControl(c("Low-White", "Low-Black", "High-White", "High-Black"),
                                options = layersControlOptions(collapsed = FALSE))



