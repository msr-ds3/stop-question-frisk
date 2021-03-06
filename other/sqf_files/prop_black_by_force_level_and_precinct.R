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

# find the proportion of each precinct that is Black/African American (Hispanic or not)
# (filter out N/A's - blocks with no corresponding precint - 
# this is justified because no people live in these blocks (population 0))
black_proportions <- precinct_race %>%
  group_by(precinct) %>%
  filter(!(is.na(precinct))) %>%
  mutate(props = total/sum(total)) %>%
  filter(variable == "Black_or_African_American_other" |
           variable == "Black_or_African_American_Hispanic_Latino") %>%
  summarize(prop = sum(props)) %>%
  select(precinct, prop) %>%
  ungroup()

# Add precinct 121 to the data, using the value from precinct 122
# (Precinct 121 was created in 2013, used to be part of 122)
last_precinct_prop <- data.frame(c(121), c(0.0229286))
names(last_precinct_prop) = c("precinct", "prop")
black_proportions <- rbind(black_proportions, last_precinct_prop)

# read in police precinct shape data
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nypp/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

# Join the precinct shape data with the data about the precincts
joint_prop <- geo_join(police_precincts, black_proportions, "Precinct", "precinct")

sqf_race_dist <- sf_data1 %>% 
  select(addrpct, race)

sqf_black_prop <- sqf_race_dist %>%
  mutate(count = 1) %>%
  group_by(addrpct, race) %>%
  summarize(total = sum(count)) %>%
  filter(!(is.na(addrpct))) %>%
  mutate(props = total/sum(total)) %>%
  filter(race == "B" | race =="P") %>%
  select(addrpct, props)

joint_sqf_prop <- geo_join(police_precincts, sqf_black_prop, "Precinct", "addrpct")


sqf_black_prop <- sf_data1 %>%
  select(addrpct, race) %>%
  filter(race == "B" | race == "P") %>%
  group_by(addrpct) %>%
  filter(!is.na(addrpct)) %>%
  #mutate(pf_hcuff = as.factor(if_else(grepl("Y",pf_wall), 1, 0))) %>%
  summarize(props = mean(as.numeric(as.character(pf_wall))))

joint_sqf_prop <- geo_join(police_precincts, sqf_black_prop, "Precinct", "addrpct")




# Looking at different levels of force:

# Proportion of stopped civilians that police used hands on that were black in each precinct
sqf_hands_black <- sf_data1 %>%
  select(addrpct, race, pf_hands) %>%
  filter(pf_hands == "Y") %>%
  select(addrpct, race) %>%
  mutate(count = 1) %>%
  group_by(addrpct, race) %>%
  summarize(total = sum(count)) %>%
  filter(!is.na(addrpct)) %>%
  mutate(props = total/sum(total)) %>%
  filter(race == "B") %>%
  select(addrpct, props)

joint_sqf_hands <- geo_join(police_precincts, sqf_hands_black, "Precinct", "addrpct")



sqf_hands_black <- sf_data1 %>%
  select(addrpct, race, pf_hands) %>%
  filter(race == "B" | race == "P") %>%
  group_by(addrpct) %>%
  filter(!is.na(addrpct)) %>%
  mutate(pf_hands = as.factor(if_else(grepl("Y",pf_hands), 1, 0))) %>%
  summarize(props = mean(as.numeric(as.character(pf_hands))))

joint_sqf_hands <- geo_join(police_precincts, sqf_hands_black, "Precinct", "addrpct")



# Proportion of stopped civilians that police handcuffed that were black in each precinct

#Probability of being handcuffed given they are black and stopped
sqf_cuffs_black <- sf_data1 %>%
  select(addrpct, race, pf_hcuff) %>%
  filter(race == "B" | race == "P") %>%
  group_by(addrpct) %>%
  filter(!is.na(addrpct)) %>%
  mutate(pf_hcuff = as.factor(if_else(grepl("Y",pf_hcuff), 1, 0))) %>%
  summarize(prop = mean(as.numeric(as.character(pf_hcuff))))

joint_sqf_cuffs <- geo_join(police_precincts, sqf_cuffs_black, "Precinct", "addrpct")

# Proportion of stopped civilians that police pushed to wall that were black in each precinct
sqf_wall_black <- sf_data1 %>%
  select(addrpct, race, pf_wall) %>%
  filter(pf_wall == "Y") %>%
  select(addrpct, race) %>%
  mutate(count = 1) %>%
  group_by(addrpct, race) %>%
  summarize(total = sum(count)) %>%
  filter(!is.na(addrpct)) %>%
  mutate(props = total/sum(total)) %>%
  filter(race == "B") %>%
  select(addrpct, props)

joint_sqf_wall <- geo_join(police_precincts, sqf_wall_black, "Precinct", "addrpct")

#Probability of pushed to wall given they are black and stopped
sqf_wall_black <- sf_data1 %>%
  select(addrpct, race, pf_wall) %>%
  filter(race == "B" | race == "P") %>%
  group_by(addrpct) %>%
  filter(!is.na(addrpct)) %>%
  mutate(pf_wall = as.factor(if_else(grepl("Y",pf_wall), 1, 0))) %>%
  summarize(props = mean(as.numeric(as.character(pf_wall))))

joint_sqf_wall <- geo_join(police_precincts, sqf_wall_black, "Precinct", "addrpct")



# Proportion of stopped civilians that police pushed to ground that were black in each precinct
sqf_ground_black <- sf_data1 %>%
  select(addrpct, race, pf_grnd) %>%
  filter(pf_grnd == "Y") %>%
  select(addrpct, race) %>%
  mutate(count = 1) %>%
  group_by(addrpct, race) %>%
  summarize(total = sum(count)) %>%
  filter(!is.na(addrpct)) %>%
  mutate(props = total/sum(total)) %>%
  filter(race == "B") %>%
  select(addrpct, props)

joint_sqf_grnd <- geo_join(police_precincts, sqf_ground_black, "Precinct", "addrpct")


#Probability of pushed to ground given they are black and stopped
sqf_ground_black <- sf_data1 %>%
  select(addrpct, race, pf_grnd) %>%
  filter(race == "B" | race == "P") %>%
  group_by(addrpct) %>%
  filter(!is.na(addrpct)) %>%
  mutate(pf_grnd = as.factor(if_else(grepl("Y",pf_grnd), 1, 0))) %>%
  summarize(props = mean(as.numeric(as.character(pf_grnd))))

joint_sqf_grnd <- geo_join(police_precincts, sqf_ground_black, "Precinct", "addrpct")



# Proportion of stopped civilians that police drew weapon against that were black in each precinct
sqf_dwep_black <- sf_data1 %>%
  select(addrpct, race, pf_drwep) %>%
  filter(pf_drwep == "Y") %>%
  select(addrpct, race) %>%
  mutate(count = 1) %>%
  group_by(addrpct, race) %>%
  summarize(total = sum(count)) %>%
  filter(!is.na(addrpct)) %>%
  mutate(props = total/sum(total)) %>%
  filter(race == "B") %>%
  select(addrpct, props)


sqf_dwep_black <- sf_data1 %>%
  select(addrpct, race, pf_drwep) %>%
  filter(race == "B" | race == "P") %>%
  group_by(addrpct) %>%
  filter(!is.na(addrpct)) %>%
  mutate(pf_drwep = as.factor(if_else(grepl("Y",pf_drwep), 1, 0))) %>%
  summarize(props = mean(as.numeric(as.character(pf_drwep))))


# Change NA's to 0
precincts <- data.frame(sort(unique(sf_data1$addrpct)))
colnames(precincts) <- c("addrpct")
sqf_dwep <- left_join(precincts, sqf_dwep_black)
sqf_dwep$props[is.na(sqf_dwep$props)] <- 0

joint_sqf_dwep <- geo_join(police_precincts, sqf_dwep, "Precinct", "addrpct")




# Proportion of stopped civilians that police pointed weapon at that were black in each precinct
sqf_pwep_black <- sf_data1 %>%
  select(addrpct, race, pf_ptwep) %>%
  filter(pf_ptwep == "Y") %>%
  select(addrpct, race) %>%
  mutate(count = 1) %>%
  group_by(addrpct, race) %>%
  summarize(total = sum(count)) %>%
  filter(!is.na(addrpct)) %>%
  mutate(props = total/sum(total)) %>%
  filter(race == "B") %>%
  select(addrpct, props)


sqf_pwep_black <- sf_data1 %>%
  select(addrpct, race, pf_ptwep) %>%
  filter(race == "B" | race == "P") %>%
  group_by(addrpct) %>%
  filter(!is.na(addrpct)) %>%
  mutate(pf_ptwep = as.factor(if_else(grepl("Y",pf_ptwep), 1, 0))) %>%
  summarize(props = mean(as.numeric(as.character(pf_ptwep))))


joint_sqf_pwep <- geo_join(police_precincts, sqf_pwep_black, "Precinct", "addrpct")

# Proportion of stopped civilians that police used pepper spray against that were black in each precinct
sqf_pepsp_black <- sf_data1 %>%
  select(addrpct, race, pf_pepsp) %>%
  filter(pf_pepsp == "Y") %>%
  select(addrpct, race) %>%
  mutate(count = 1) %>%
  group_by(addrpct, race) %>%
  summarize(total = sum(count)) %>%
  filter(!is.na(addrpct)) %>%
  mutate(props = total/sum(total)) %>%
  filter(race == "B") %>%
  select(addrpct, props)



sqf_pepsp_black <- sf_data1 %>%
  select(addrpct, race, pf_pepsp) %>%
  filter(race == "B" | race == "P") %>%
  group_by(addrpct) %>%
  filter(!is.na(addrpct)) %>%
  mutate(pf_pepsp = as.factor(if_else(grepl("Y",pf_pepsp), 1, 0))) %>%
  summarize(props = mean(as.numeric(as.character(pf_pepsp))))

# Change NA's to 0
sqf_peps <- left_join(precincts, sqf_pepsp_black)
sqf_peps$props[is.na(sqf_peps$props)] <- 0

joint_sqf_pepsp <- geo_join(police_precincts, sqf_peps, "Precinct", "addrpct")


# Proportion of stopped civilians that police used baton on that were black in each precinct
sqf_baton_black <- sf_data1 %>%
  select(addrpct, race, pf_baton) %>%
  filter(pf_baton == "Y") %>%
  select(addrpct, race) %>%
  mutate(count = 1) %>%
  group_by(addrpct, race) %>%
  summarize(total = sum(count)) %>%
  filter(!is.na(addrpct)) %>%
  mutate(props = total/sum(total)) %>%
  filter(race == "B") %>%
  select(addrpct, props)


sqf_baton_black <- sf_data1 %>%
  select(addrpct, race, pf_baton) %>%
  filter(race == "B" | race == "P") %>%
  group_by(addrpct) %>%
  filter(!is.na(addrpct)) %>%
  mutate(pf_baton = as.factor(if_else(grepl("Y",pf_baton), 1, 0))) %>%
  summarize(props = mean(as.numeric(as.character(pf_baton))))


sqf_baton <- left_join(precincts, sqf_baton_black)
sqf_baton$props[is.na(sqf_baton$props)] <- 0

joint_sqf_baton <- geo_join(police_precincts, sqf_baton, "Precinct", "addrpct")



########## CREATE MAPS OF RACE DISTRIBUTIONS ##########

# Map the proportion of each precinct that is black
mypopup <- paste0("Precinct: ", joint_prop$Precinct, "<br>", 
                   "Population Proportion Black: ", joint_prop$prop)

mypal <- colorNumeric(
  palette = "YlOrRd",
  domain = 0:1
)

leaflet(joint_prop) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal(joint_prop$prop),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal, 
            values = joint_prop$prop, 
            position = "topleft", 
            title = "Population Proportion Black")

#Map the proportion of civilians stopped in each precinct that are black
mypopup2 <- paste0("Precinct: ", joint_sqf_prop$Precinct, "<br>", 
                   "SQF Proportion Black: ", joint_sqf_prop$props)

mypal2 <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_sqf_prop$props
)

leaflet(joint_sqf_prop) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal(joint_sqf_prop$props),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup2) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal2, 
            values = joint_sqf_prop$props, 
            position = "topleft", 
            title = "SQF Proportion Black")

#DIFFEREENT LEVELS OF FORCE
#Map the proportion of stopped civilians hands were used on in each precinct that are black
mypopup3 <- paste0("Precinct: ", joint_sqf_hands$Precinct, "<br>", 
                   "Hands Proportion Black: ", joint_sqf_hands$props)

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
            title = "Hands Prop Black")

#Map the proportion of stopped civilians handcuffed in each precinct that are black
mypopup4 <- paste0("Precinct: ", joint_sqf_cuffs$Precinct, "<br>", 
                   "Handcuffs Proportion Black: ", joint_sqf_cuffs$prop)

mypal4 <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_sqf_cuffs$prop
)

leaflet(joint_sqf_cuffs) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal4(joint_sqf_cuffs$prop),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup4) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal4, 
            values = joint_sqf_cuffs$prop, 
            position = "topleft", 
            title = "Handcuffs Prop Black")

#Map the proportion of stopped civilians pushed to a wall in each precinct that are black
mypopup5 <- paste0("Precinct: ", joint_sqf_wall$Precinct, "<br>", 
                   "Pushed to Wall Proportion Black: ", joint_sqf_wall$props)

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
            title = "Pushed to Wall Prop Black")

#Map the proportion of stopped civilians pushed to the ground in each precinct that are black
mypopup6 <- paste0("Precinct: ", joint_sqf_grnd$Precinct, "<br>", 
                   "Pushed to Ground Proportion Black: ", joint_sqf_grnd$props)

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
            title = "Pushed to Ground Prop Black")

#Map the proportion of stopped civilians police drew a weapon against in each precinct that are black
mypopup7 <- paste0("Precinct: ", joint_sqf_dwep$Precinct, "<br>", 
                   "Drew Weapon Proportion Black: ", joint_sqf_dwep$props)

mypal7 <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_sqf_dwep$props
)

leaflet(joint_sqf_dwep) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal7(joint_sqf_dwep$props),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup7) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal7, 
            values = joint_sqf_dwep$props, 
            position = "topleft", 
            title = "Drew Weapon Prop Black")

#Map the proportion of stopped civilians police pointed a weapon towards in each precinct that are black
mypopup8 <- paste0("Precinct: ", joint_sqf_pwep$Precinct, "<br>", 
                   "Pointed Weapon Proportion Black: ", joint_sqf_pwep$props)

mypal8 <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_sqf_pwep$props
)

leaflet(joint_sqf_pwep) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal8(joint_sqf_pwep$props),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup8) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal8, 
            values = joint_sqf_pwep$props, 
            position = "topleft", 
            title = "Pointed Weapon Prop Black")

#Map the proportion of stopped civilians sprayed with pepper spray in each precinct that are black
mypopup9 <- paste0("Precinct: ", joint_sqf_pepsp$Precinct, "<br>", 
                   "Pepper Sprayed Proportion Black: ", joint_sqf_pepsp$props)

mypal9 <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_sqf_pepsp$props
)

leaflet(joint_sqf_pepsp) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal9(joint_sqf_pepsp$props),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup9) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal9, 
            values = joint_sqf_pepsp$props, 
            position = "topleft", 
            title = "Pepper Sprayed Prop Black")

#Map the proportion of stopped civilians police used a baton on in each precinct that are black
mypopup10 <- paste0("Precinct: ", joint_sqf_baton$Precinct, "<br>", 
                   "Baton Proportion Black: ", joint_sqf_baton$props)

mypal10 <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_sqf_baton$props
)

leaflet(joint_sqf_baton) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal10(joint_sqf_baton$props),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal10, 
            values = joint_sqf_baton$props, 
            position = "topleft", 
            title = "Baton Prop Black")

