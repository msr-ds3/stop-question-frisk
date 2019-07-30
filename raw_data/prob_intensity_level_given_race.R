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
library(htmlwidgets)
library(webshot)

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
    filter(race != "U" & race != "X" & race != " ") %>%
    mutate(race = recode_factor(race,"P" = "B", "I" = "Z"), 
           race = recode_factor(race, "W" = "White", "B" = "Black",  
           "Q" ="Hispanic",  "A" = "Asian", "Z" = "Other")) %>%
    group_by(addrpct, race) %>%
    summarize(prob = mean(pf_low)) 
  
# the probability of having high intensity force used on you,
# given your race and precinct, conditional on being stopped
prob_high_intensity_given_race <- high_intensity %>%
  filter(race != "U" & race != "X" & race != " ") %>%
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

View(prob_low_intensity_given_race)

comparing_low_intensity <- prob_low_intensity_given_race %>%
  spread(race, prob) %>% 
  mutate(B_over_W = (Black/White)) 

comparing_high_intensity <- prob_high_intensity_given_race %>%
  spread(race, prob) %>% 
  mutate(B_over_W = (Black/White)) 


# read in police precinct shape data
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nypp/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)


# Join the precinct shape data with the data about the precincts
joint_prop_low_white <- geo_join(police_precincts, prob_low_white, "Precinct", "addrpct")

joint_prop_low_black <- geo_join(police_precincts, prob_low_black, "Precinct", "addrpct")

joint_prop_high_white <- geo_join(police_precincts, prob_high_white, "Precinct", "addrpct")

joint_prop_high_black <- geo_join(police_precincts, prob_high_black, "Precinct", "addrpct")




joint_low <- geo_join(police_precincts, comparing_low_intensity, "Precinct", "addrpct")
joint_high <- geo_join(police_precincts, comparing_high_intensity, "Precinct", "addrpct")


####### Computational Comparison of Blacks and Whites #######

# Is the probability of having low intensity force used on a stopped
# black civilian greater than the probability for a stopped white
# civilian in each precinct
comparing_low <- prob_low_intensity_given_race %>%
  spread(race, prob) %>%
  mutate(discrimination = Black > White)

# Is the probability of having low intensity force used on a stopped
# black civilian greater than the probability for a stopped white
# civilian in each precinct
comparing_high <- prob_high_intensity_given_race %>%
  spread(race, prob) %>%
  mutate(discrimination = Black > White)


####### Visual Comparison of Blacks and Whites #######

#Feature with Radio buttons on map

#Low White
mypopup <- paste0("Precinct: ", joint_prop_low_white$addrpct, "<br>", 
                  "White Low Intensity Prop: ", joint_prop_low_white$prob)

mypal <- colorNumeric(
  palette = "YlOrRd",
  domain = seq(0, .55, .01)
)

#Low Black
mypopup2 <- paste0("Precinct: ", joint_prop_low_black$addrpct, "<br>", 
                   "Black Low Intensity Prop: ", joint_prop_low_black$prob)

mypal2 <- colorNumeric(
  palette = "YlOrRd",
  domain = seq(0, .55, .01)
)

#High White
mypopup3 <- paste0("Precinct: ", joint_prop_high_white$addrpct, "<br>", 
                  "White High Intensity Prop: ", joint_prop_high_white$prob)

mypal3 <- colorNumeric(
  palette = "YlOrRd",
  domain = seq(0, .055, .001)
)

#High Black
mypopup4 <- paste0("Precinct: ", joint_prop_high_black$addrpct, "<br>", 
                   "Black High Intensity Prop: ", joint_prop_high_black$prob)

mypal4 <- colorNumeric(
  palette = "YlOrRd",
  domain = seq(0, .055, .001)
)


#Generating leaflet maps with all the options

#Leaflet Map for Low Intensity Force
leafletmaplow <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=joint_prop_low_white,
              fillColor = ~mypal(joint_prop_low_white$prob),
              weight = 1,
              fillOpacity = 0.7,
              popup = mypopup, group="Low-White") %>%
  addPolygons(data=joint_prop_low_black,
              fillColor = ~mypal2(joint_prop_low_black$prob),
              weight = 1,fillOpacity = 0.7,
              popup = mypopup2, group="Low-Black") %>%
  addLegend(position = "topleft", 
            pal = mypal, 
            values = c(0,0.5)) %>%
  addLayersControl(c("Low-White", "Low-Black"),
                                options = layersControlOptions(collapsed = FALSE))


leafletmaplow 


#Leaflet Map for High Intensity Force
leafletmaphigh <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>%

  addPolygons(data=joint_prop_high_white,
              fillColor = ~mypal3(joint_prop_high_white$prob),
              weight = 1,
              fillOpacity = 0.7,
              popup = mypopup3, group="High-White") %>%
  addPolygons(data=joint_prop_high_black,
              fillColor = ~mypal4(joint_prop_high_black$prob),
              weight = 1,
              fillOpacity = 0.7,
              popup = mypopup4, group="High-Black") %>%
  addLegend(position = "topleft", 
            pal = mypal3, 
            values = c(0,0.05)) %>%
  addLayersControl(c("High-White", "High-Black"),
                     options = layersControlOptions(collapsed = FALSE))


leafletmaphigh 




mypopup <- paste0("Precinct: ", joint_low$addrpct, "<br>", 
                  "Black/White Prop w/ Force Used: ", joint_low$B_over_W)

mypal <- colorNumeric(
  palette = "Spectral",
  domain = c(-2.5,2.5),
  reverse = TRUE
)



prob_low_force <- leaflet(joint_low) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal(joint_low$B_over_W),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal, 
            values = c(-2.5,2.5), 
            position = "topleft", 
            title = "Subject to Low-Intensity<br>Force
            Black vs White")


prob_low_force


mypopuphigh <- paste0("Precinct: ", joint_high$addrpct, "<br>", 
                  "Black/White Prop w/ Force Used: ", joint_high$B_over_W)

mypalhigh <- colorNumeric(
  palette = "Spectral",
  domain = c(-5,5),
  reverse = TRUE
)



prob_high_force <- leaflet(joint_high) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypalhigh(joint_high$B_over_W),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopuphigh) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypalhigh, 
            values = c(-5,5), 
            position = "topleft", 
            title = "Subject to High-Intensity<br>Force
            Black vs White")

prob_high_force


View(comparing_high_intensity)

#Saving interactive leaflet map
saveWidget(leafletmaphigh, "high-intensity.html", selfcontained = FALSE)

webshot("high-intensity.html", file = "high-white.png",
        cliprect = "viewport")


saveWidget(leafletmaplow, "low-intensity.html", selfcontained = FALSE)

webshot("low-intensity.html", file = "low-white.png",
        cliprect = "viewport")


