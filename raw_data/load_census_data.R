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

sqf_race_dist

sqf_black_prop <- sqf_race_dist %>%
  mutate(count = 1) %>%
  group_by(addrpct, race) %>%
  summarize(total = sum(count)) %>%
  filter(!(is.na(addrpct))) %>%
  mutate(props = total/sum(total)) %>%
  filter(race == "B") %>%
  select(addrpct, props)

joint_sqf_prop <- geo_join(police_precincts, sqf_black_prop, "Precinct", "addrpct")

########## CREATE MAPS OF RACE DISTRIBUTIONS ##########

# Map the proportion of each precinct that is black

mypopup <- paste0("Precinct: ", joint_prop$Precinct, "<br>", 
                   "Population Proportion Black: ", joint_prop$prop)

mypal <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_prop$prop
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

#Map the proportion of people stopped in each precinct that are black
mypopup2 <- paste0("Precinct: ", joint_sqf_prop$Precinct, "<br>", 
                   "SQF Proportion Black: ", joint_sqf_prop$props)

mypal2 <- colorNumeric(
  palette = "YlOrRd",
  domain = joint_sqf_prop$props
)

leaflet(joint_sqf_prop) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal2(joint_sqf_prop$props),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup2) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal2, 
            values = joint_sqf_prop$props, 
            position = "topleft", 
            title = "SQF Proportion Black")

#Testing out ggmaps 
police_precincts <- tidy(police_precincts, region = "Precinct")

black_proportions <- black_proportions %>% mutate(precinct = as.character(precinct))

jp <- police_precincts %>%
  left_join(black_proportions, by=c("id"="precinct"))

nyc_map <- get_map(location = c(lon = -74.00, lat = 40.71), maptype = "terrain", zoom = 11)

mypal <- colorNumeric(
  palette = "YlOrRd",
  domain = jp$prop
)

jp
colors <- c("red")
jp %>% View()

?colorFactor()

install.packages("viridis")
library(viridis)

cp<- c("#ffffcc","#ffeda0", "#fed976","#feb24c","#fd8d3c", "#fc4e2a", "#e31a1c", "#b10026")


ggmap(nyc_map) + 
  geom_polygon(data=jp, aes(x=long, y=lat, group=group, fill = prop)) + 
  scale_fill_gradientn(colors = cp)

