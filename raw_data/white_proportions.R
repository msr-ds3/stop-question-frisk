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


# find the proportion of each precinct that is White (Hispanic or not)
# (filter out N/A's - blocks with no corresponding precint - 
# this is justified because no people live in these blocks (population 0))
white_proportions <- precinct_race %>%
  group_by(precinct) %>%
  filter(!(is.na(precinct))) %>%
  mutate(props = total/sum(total)) %>%
  filter(variable == "White_other" |
           variable == "White_Hispanic_Latino") %>%
  summarize(prop = sum(props)) %>%
  select(precinct, prop) %>%
  ungroup()

View(white_proportions)

# Add precinct 121 to the data, using the value from precinct 122
# (Precinct 121 was created in 2013, used to be part of 122)
last_precinct_prop <- data.frame(c(121), c(0.82905935))
names(last_precinct_prop) = c("precinct", "prop")
white_proportions <- rbind(white_proportions, last_precinct_prop)

# read in police precinct shape data
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nypp/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

# Join the precinct shape data with the data about the precincts
joint_prop_white <- geo_join(police_precincts, white_proportions, "Precinct", "precinct")

sqf_race_dist_w <- sf_data1 %>% 
  select(addrpct, race)

sqf_white_prop <- sqf_race_dist_w %>%
  mutate(count = 1) %>%
  group_by(addrpct, race) %>%
  summarize(total = sum(count)) %>%
  filter(!(is.na(addrpct))) %>%
  mutate(props = total/sum(total)) %>%
  filter(race == "W") %>%
  select(addrpct, props)

joint_sqf_prop_w <- geo_join(police_precincts, sqf_white_prop, "Precinct", "addrpct")

# Proportion of stopped civilians that police pushed to wall that were white in each precinct
sqf_wall_white <- sf_data1 %>%
  select(addrpct, race, pf_wall) %>%
  filter(pf_wall == "Y") %>%
  select(addrpct, race) %>%
  mutate(count = 1) %>%
  group_by(addrpct, race) %>%
  summarize(total = sum(count)) %>%
  filter(!is.na(addrpct)) %>%
  mutate(props = total/sum(total)) %>%
  filter(race == "W") %>%
  select(addrpct, props)

joint_sqf_wall_w <- geo_join(police_precincts, sqf_wall_white, "Precinct", "addrpct")

# Proportion of stopped civilians that police used pepper spray against that were white in each precinct
sqf_pepsp_white <- sf_data1 %>%
  select(addrpct, race, pf_pepsp) %>%
  filter(pf_pepsp == "Y") %>%
  select(addrpct, race) %>%
  mutate(count = 1) %>%
  group_by(addrpct, race) %>%
  summarize(total = sum(count)) %>%
  filter(!is.na(addrpct)) %>%
  mutate(props = total/sum(total)) %>%
  filter(race == "W") %>%
  select(addrpct, props)

# Change NA's to 0
precincts <- data.frame(sort(unique(sf_data1$addrpct)))
colnames(precincts) <- c("addrpct")
sqf_pepsp_white <- left_join(precincts, sqf_pepsp_white)
sqf_pepsp_white$props[is.na(sqf_pepsp_white$props)] <- 0

joint_sqf_pepsp_white <- geo_join(police_precincts, sqf_pepsp_white, "Precinct", "addrpct")


#I think this one gives an error
precincts <- data.frame(sort(unique(sf_data1$addrpct)))
colnames(precincts) <- c("addrpct")
sqf_peps_w <- left_join(precincts, sqf_pepsp_white)
sqf_peps_w$props[is.na(sqf_peps_w$props)] <- 0

joint_sqf_pepsp_w <- geo_join(police_precincts, sqf_peps_w, "Precinct", "addrpct")


# Proportion of stopped civilians that police used baton on that were white in each precinct
sqf_baton_white <- sf_data1 %>%
  select(addrpct, race, pf_baton) %>%
  filter(pf_baton == "Y") %>%
  select(addrpct, race) %>%
  mutate(count = 1) %>%
  group_by(addrpct, race) %>%
  summarize(total = sum(count)) %>%
  filter(!is.na(addrpct)) %>%
  mutate(props = total/sum(total)) %>%
  filter(race == "W") %>%
  select(addrpct, props)

sqf_baton <- left_join(precincts, sqf_baton_white)
sqf_baton$props[is.na(sqf_baton$props)] <- 0

joint_sqf_baton_white <- geo_join(police_precincts, sqf_baton, "Precinct", "addrpct")

# Proportion of stopped civilians that police pointed weapon at that were white in each precinct
sqf_pwep_white <- sf_data1 %>%
  select(addrpct, race, pf_ptwep) %>%
  filter(pf_ptwep == "Y") %>%
  select(addrpct, race) %>%
  mutate(count = 1) %>%
  group_by(addrpct, race) %>%
  summarize(total = sum(count)) %>%
  filter(!is.na(addrpct)) %>%
  mutate(props = total/sum(total)) %>%
  filter(race == "W") %>%
  select(addrpct, props)

joint_sqf_pwep_white <- geo_join(police_precincts, sqf_pwep_white, "Precinct", "addrpct")


# Proportion of stopped civilians that police drew weapon against that were white in each precinct
sqf_dwep_white <- sf_data1 %>%
  select(addrpct, race, pf_drwep) %>%
  filter(pf_drwep == "Y") %>%
  select(addrpct, race) %>%
  mutate(count = 1) %>%
  group_by(addrpct, race) %>%
  summarize(total = sum(count)) %>%
  filter(!is.na(addrpct)) %>%
  mutate(props = total/sum(total)) %>%
  filter(race == "W") %>%
  select(addrpct, props)

# Change NA's to 0
precincts <- data.frame(sort(unique(sf_data1$addrpct)))
colnames(precincts) <- c("addrpct")
sqf_dwep <- left_join(precincts, sqf_dwep_white)
sqf_dwep$props[is.na(sqf_dwep$props)] <- 0

joint_sqf_dwep_white <- geo_join(police_precincts, sqf_dwep, "Precinct", "addrpct")


# Proportion of stopped civilians that police pushed to ground that were white in each precinct
sqf_ground_white <- sf_data1 %>%
  select(addrpct, race, pf_grnd) %>%
  filter(pf_grnd == "Y") %>%
  select(addrpct, race) %>%
  mutate(count = 1) %>%
  group_by(addrpct, race) %>%
  summarize(total = sum(count)) %>%
  filter(!is.na(addrpct)) %>%
  mutate(props = total/sum(total)) %>%
  filter(race == "W") %>%
  select(addrpct, props)

joint_sqf_grnd <- geo_join(police_precincts, sqf_ground_white, "Precinct", "addrpct")

# Proportion of stopped civilians that police used hands on that were white in each precinct
sqf_hands_white <- sf_data1 %>%
  select(addrpct, race, pf_hands) %>%
  filter(pf_hands == "Y") %>%
  select(addrpct, race) %>%
  mutate(count = 1) %>%
  group_by(addrpct, race) %>%
  summarize(total = sum(count)) %>%
  filter(!is.na(addrpct)) %>%
  mutate(props = total/sum(total)) %>%
  filter(race == "W") %>%
  select(addrpct, props)

joint_sqf_hands <- geo_join(police_precincts, sqf_hands_white, "Precinct", "addrpct")

# Proportion of stopped civilians that police handcuffed that were white in each precinct
sqf_cuffs_white <- sf_data1 %>%
  select(addrpct, race, pf_hcuff) %>%
  filter(pf_hcuff == "Y") %>%
  select(addrpct, race) %>%
  mutate(count = 1) %>%
  group_by(addrpct, race) %>%
  summarize(total = sum(count)) %>%
  filter(!is.na(addrpct)) %>%
  mutate(props = total/sum(total)) %>%
  filter(race == "W") %>%
  select(addrpct, props)

joint_sqf_cuff <- geo_join(police_precincts, sqf_cuffs_white, "Precinct", "addrpct")


########## CREATE MAPS OF RACE DISTRIBUTIONS ##########

# Map the proportion of each precinct that is White
mypopup <- paste0("Precinct: ", joint_prop_white$Precinct, "<br>", 
                  "Population Proportion White: ", joint_prop_white$prop)

mypal <- colorNumeric(
  palette = "YlOrRd",
  domain = 0:1
)

leaflet(joint_prop_white) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal(joint_prop_white$prop),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal, 
            values = joint_prop_white$prop, 
            position = "topleft", 
            title = "Population Proportion White")

#Map the proportion of civilians stopped in each precinct that are White
mypopup2 <- paste0("Precinct: ", joint_sqf_prop_w$Precinct, "<br>", 
                   "SQF Proportion White: ", joint_sqf_prop_w$props)

mypal2 <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_sqf_prop_w$props
)

leaflet(joint_sqf_prop_w) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal(joint_sqf_prop_w$props),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup2) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal2, 
            values = joint_sqf_prop_w$props, 
            position = "topleft", 
            title = "SQF Proportion White")

#DIFFEREENT LEVELS OF FORCE
#Map the proportion of stopped civilians hands were used on in each precinct that are White
mypopup3 <- paste0("Precinct: ", joint_sqf_hands$Precinct, "<br>", 
                   "Hands Proportion White: ", joint_sqf_hands$props)

mypal3 <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_sqf_hands$props
)

leaflet(joint_sqf_hands) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal3(joint_sqf_hands$props),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup3) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal3, 
            values = joint_sqf_hands$props, 
            position = "topleft", 
            title = "Hands Prop White")

#Map the proportion of stopped civilians handcuffed in each precinct that are White
mypopup4 <- paste0("Precinct: ", joint_sqf_cuff$Precinct, "<br>", 
                   "Handcuffs Proportion White: ", joint_sqf_cuff$props)

mypal4 <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_sqf_cuff$props
)

leaflet(joint_sqf_cuff) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal4(joint_sqf_cuff$props),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup4) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal4, 
            values = joint_sqf_cuff$props, 
            position = "topleft", 
            title = "Handcuffs Prop White")

#Map the proportion of stopped civilians pushed to a wall in each precinct that are white
mypopup5 <- paste0("Precinct: ", joint_sqf_wall$Precinct, "<br>", 
                   "Pushed to Wall Proportion White: ", joint_sqf_wall$props)

mypal5 <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_sqf_wall$props
)

leaflet(joint_sqf_wall) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal5(joint_sqf_wall$props),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup5) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal5, 
            values = joint_sqf_wall$props, 
            position = "topleft", 
            title = "Pushed to Wall Prop White")

#Map the proportion of stopped civilians pushed to the ground in each precinct that are white
mypopup6 <- paste0("Precinct: ", joint_sqf_grnd$Precinct, "<br>", 
                   "Pushed to Ground Proportion White: ", joint_sqf_grnd$props)

mypal6 <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_sqf_grnd$props
)

leaflet(joint_sqf_grnd) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal6(joint_sqf_grnd$props),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup6) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal6, 
            values = joint_sqf_grnd$props, 
            position = "topleft", 
            title = "Pushed to Ground Prop White")

#Map the proportion of stopped civilians police drew a weapon against in each precinct that are White
mypopup7 <- paste0("Precinct: ", joint_sqf_dwep_white$Precinct, "<br>", 
                   "Drew Weapon Proportion White: ", joint_sqf_dwep_white$props)

mypal7 <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_sqf_dwep_white$props
)

leaflet(joint_sqf_dwep_white) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal7(joint_sqf_dwep_white$props),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup7) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal7, 
            values = joint_sqf_dwep_white$props, 
            position = "topleft", 
            title = "Drew Weapon Prop White")

#Map the proportion of stopped civilians police pointed a weapon towards in each precinct that are white
mypopup8 <- paste0("Precinct: ", joint_sqf_pwep_white$Precinct, "<br>", 
                   "Pointed Weapon Proportion white: ", joint_sqf_pwep_white$props)

mypal8 <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_sqf_pwep_white$props
)

leaflet(joint_sqf_pwep_white) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal8(joint_sqf_pwep_white$props),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup8) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal8, 
            values = joint_sqf_pwep_white$props, 
            position = "topleft", 
            title = "Pointed Weapon Prop White")

#Map the proportion of stopped civilians sprayed with pepper spray in each precinct that are White
mypopup9 <- paste0("Precinct: ", joint_sqf_pepsp_white$Precinct, "<br>", 
                   "Pepper Sprayed Proportion White: ", joint_sqf_pepsp_white$props)

mypal9 <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_sqf_pepsp_white$props
)

leaflet(joint_sqf_pepsp_white) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal9(joint_sqf_pepsp_white$props),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup9) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal9, 
            values = joint_sqf_pepsp_white$props, 
            position = "topleft", 
            title = "Pepper Sprayed Prop White")

#Map the proportion of stopped civilians police used a baton on in each precinct that are black
mypopup10 <- paste0("Precinct: ", joint_sqf_baton_white$Precinct, "<br>", 
                    "Baton Proportion White: ", joint_sqf_baton_white$props)

mypal10 <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_sqf_baton_white$props
)

leaflet(joint_sqf_baton_white) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal10(joint_sqf_baton_white$props),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal10, 
            values = joint_sqf_baton_white$props, 
            position = "topleft", 
            title = "Baton Prop White")


