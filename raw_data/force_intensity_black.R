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

# Set up census data
census_api_key('5365371ad843ba3249f2e88162f10edcfe529d87', install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

#variables to load from census data
vars = c("P003004", "P003005", "P003006", "P003007", "P003008",
         "P005003", "P005004", "P005011", "P005012")

#counties to load from census data
counties = c("Richmond", "Kings", "New York", "Queens", "Bronx")

#load census data
census <- get_decennial(geography = "block", variables = vars, state = "NY", 
                        county = counties, year = 2010, tigris_use_cache = TRUE)

# set digits so converting geoid's to numbers will retain all sigfigs
options(digits = 15)

# convert GEOIDs to numbers, variables to a factor
census <- mutate(census, variable = as.factor(variable)) %>%
  mutate(geoid10 = as.numeric(GEOID)) %>% select(-GEOID)

# reset digits to the default
options(digits = 7)

# rename variables for clarity
census$variable <- recode(census$variable, P003004 = "American_Indian_and_Alaska_Native",
                          P003005 = "Asian", P003006 = "Native_Hawaiian_and_Pacific_Islander",
                          P003007 = "Other", P003008 = "Two_Or_More_Races",
                          P005003 = "White_other", P005004 = "Black_or_African_American_other",
                          P005011 = "White_Hispanic_Latino", P005012 = "Black_or_African_American_Hispanic_Latino")

# load file to map block-level data to precinct level
precinct_block_key <- read_csv("precinct_blocks_key.csv")


# add precinct numbers to census data
precinct_populations <- left_join(census, precinct_block_key)


# find the population of each race in each precinct
precinct_race <- precinct_populations %>% ungroup() %>%
  group_by(precinct, variable) %>%
  summarize(total = sum(value))

View(precinct_race)

precinct_black <- precinct_race %>% filter(variable == "Black_or_African_American_other" |
                           variable == "Black_or_African_American_Hispanic_Latino")


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



low_intensity <-sf_data1 %>%
                mutate(pf_low = paste(pf_hands, pf_wall,pf_hcuff, sep = ""),
                pf_low = if_else(grepl("Y", pf_low), 1, 0))



high_intensity <- sf_data1 %>% 
                  mutate(pf_high = paste(pf_grnd, pf_drwep, pf_ptwep,
                  pf_baton, pf_pepsp, sep = ""),
                  pf_high = if_else(grepl("Y",pf_high), 1, 0))


low_props_black <- low_intensity %>% filter(pf_low == 1) %>% 
                   mutate(tally = 1) %>%
                   group_by(addrpct, race) %>% 
                   summarize(total = sum(tally)) %>%
                   ungroup() %>% group_by(addrpct) %>%
                   mutate(prop = total/sum(total)) %>%
                   filter(race == "B" | race == "P") %>%
                   summarize(low_force_prop_black = sum(prop)) 



high_props_black <- high_intensity %>% filter(pf_high == 1) %>% 
                    mutate(tally = 1) %>%
                    group_by(addrpct, race) %>% 
                    summarize(total = sum(tally)) %>%
                    ungroup() %>% group_by(addrpct) %>% 
                    mutate(prop = total/sum(total)) %>%
                    filter(race == "B" | race == "P") %>%
                    summarize(high_force_prop_black = sum(prop))



# read in police precinct shape data
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nypp/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

# Join the precinct shape data with the data about the precincts
joint_prop_low_black <- geo_join(police_precincts, low_props_black, "Precinct", "addrpct")

joint_prop_high_black <- geo_join(police_precincts, high_props_black, "Precinct", "addrpct")


#Low intensity proportions for black
mypopup <- paste0("Precinct: ", joint_prop_low_black$addrpct, "<br>", 
                  "Black Low Intensity Prop: ", joint_prop_low_black$low_force_prop_black)

mypal <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_prop_low_black$low_force_prop_black
)

leaflet(joint_prop_low_black) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal(joint_prop_low_black$low_force_prop_black),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal, 
            values = joint_prop_low_black$low_force_prop_black, 
            position = "topleft", 
            title = "Low Intensity Prop")


#High Intensity Proportions for total population
mypopup2 <- paste0("Precinct: ", joint_prop_high_black$addrpct, "<br>", 
                   "Black High Intensity Prop: ", joint_prop_high_black$high_force_prop_black)

mypal2 <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_prop_high_black$high_force_prop_black
)

leaflet(joint_prop_high) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal2(joint_prop_high_black$high_force_prop_black),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup2) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal2, 
            values = joint_prop_high_black$high_force_prop_black, 
            position = "topleft", 
            title = "High Intensity Prop")


#Feature with Radio buttons on map
mypopup <- paste0("Precinct: ", joint_prop_low_black$addrpct, "<br>", 
                  "Black Low Intensity Prop: ", joint_prop_low_black$low_force_prop_black)

mypal <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_prop_low_black$low_force_prop_black
)

mypopup2 <- paste0("Precinct: ", joint_prop_high_black$addrpct, "<br>", 
                   "Black High Intensity Prop: ", joint_prop_high_black$high_force_prop_black)

mypal2 <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_prop_high_black$high_force_prop_black
)


leafletmap <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  
  addPolygons(data=joint_prop_low_black,
              fillColor = ~mypal(joint_prop_low_black$low_force_prop_black),
              weight = 2,
              opacity = 1,
              color = "blue",
              dashArray = "3",
              fillOpacity = 0.7,
              popup = mypopup, group="Low") %>%
  addPolygons(data=joint_prop_high_white,
              fillColor = ~mypal2(joint_prop_high_black$high_force_prop_black),
              weight = 2,
              opacity = 1,
              color = "blue",
              dashArray = "3",
              fillOpacity = 0.7,
              popup = mypopup2, group="High")

leafletmap %>% addLayersControl(c("Low", "High"),
                                options = layersControlOptions(collapsed = FALSE))



