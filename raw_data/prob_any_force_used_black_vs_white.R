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
library(htmlwidgets)
library(webshot)

########## LOAD AND CREATE/CLEAN DATAFRAMES ##########

# Load stop and frisk data for 2003-2013
load("sqf_03_13.RData")

# Load census data for race distributions by precinct
load("census_race_data.RData")

sqf_data <- sf_data1 %>%
  # filter out unknown races
  filter(race != " " & race != "U" & race != "X") %>%
  # recode Black Hispanic as Black, American Indian as Other
  mutate(race = recode_factor(race,"P" = "B", "I" = "Z"),
         # create an any_force_used column - ADDED pf_other TO THIS CALCULATION
         any_force_used = paste(pf_hands, pf_grnd, pf_wall, pf_drwep, pf_ptwep,
                                pf_hcuff,pf_baton, pf_pepsp, sep = ""),
         # create factors from columns
         any_force_used = if_else(grepl("Y",any_force_used), 1, 0),
         # recode race names for clarity
         race = recode_factor(race, "W" = "White", "B" = "Black",  "Q" ="Hispanic",
                              "A" = "Asian", "Z" = "Other")) %>%
  select(addrpct, race, any_force_used)

force_used <- sqf_data %>%
  group_by(addrpct, race) %>%
  summarize(prop_w_force_used = mean(any_force_used))

force_used

force_used_black <- force_used %>% filter(race == "Black")

force_used_white <- force_used %>% filter(race == "White")


comparing_races <- force_used %>%
  spread(race, prop_w_force_used) %>% 
  mutate(B_over_W = (Black/White)) 



########## MAP THE RESULTS ##########

# read in police precinct shape data
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nypp/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

# Join the precinct shape data with the data about the precincts
joint_black <- geo_join(police_precincts, force_used_black, "Precinct", "addrpct")
joint_white <- geo_join(police_precincts, force_used_white, "Precinct", "addrpct")
joint <- geo_join(police_precincts, comparing_races, "Precinct", "addrpct")


mypopup <- paste0("Precinct: ", joint$addrpct, "<br>", 
                  "Black/White Prop w/ Force Used: ", joint$B_over_W)

mypal <- colorNumeric(
  palette = "YlOrRd",
  domain = c(-1,2),
  reverse = TRUE
)


prob_force_used_b_over_w <- leaflet(joint) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal1(joint$prop_w_force_used),
              fillOpacity = 1,
              weight = 1,
              popup = mypopup) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal, 
            values = c(0, 2), 
            position = "topleft", 
            title = "Probability of <br> any force used <br>
            Black over White") 

prob_force_used_b_over_w

mypopup1 <- paste0("Precinct: ", joint_black$addrpct, "<br>", 
                  "Black/White Prop w/ Force Used: ", joint_black$prop_w_force_used)

mypal1 <- colorNumeric(
  palette = "Spectral",
  domain = c(-1,1),
  reverse = TRUE
)


prob_force_used_black <- leaflet(joint_black) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal1(joint_black$prop_w_force_used),
              fillOpacity = 1,
              weight = 1,
              popup = mypopup1,
              group = "Black") %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal1, 
            values = c(-1, 1), 
            position = "topleft", 
            title = "Probability of <br> any force used <br>
            given Black") 

prob_force_used_black



mypopup2 <- paste0("Precinct: ", joint_white$addrpct, "<br>", 
                   "Black/White Prop w/ Force Used: ", joint_white$prop_w_force_used)

mypal2 <- colorNumeric(
  palette = "Spectral",
  domain = c(-1,1),
  reverse = TRUE
)


prob_force_used_white <- leaflet(joint_white) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal(joint_white$prop_w_force_used),
              fillOpacity = 1,
              weight = 1,
              popup = mypopup2,
              group = "Black") %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal2, 
            values = c(-0,1), 
            position = "topleft", 
            title = "Probability of any force used <br>
            given White") 

prob_force_used_white


saveWidget(prob_force_used, 
           "../figures/prob_force_used.html", 
           selfcontained = FALSE)
webshot("../figures/prob_force_used.html",
        file = "../figures/prob_force_used.png",
        cliprect = "viewport")

