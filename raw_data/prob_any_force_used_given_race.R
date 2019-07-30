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

comparing_races <- force_used %>%
  spread(race, prop_w_force_used) %>%
  mutate(B_over_W = Black/White) %>%
  mutate(logBW = log(B_over_W)) %>%
  mutate(H_over_W = Hispanic/White) %>%
  mutate(logHW = log(H_over_W))


########## MAP THE RESULTS ##########

# read in police precinct shape data
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nypp/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

# Join the precinct shape data with the data about the precincts
joint <- geo_join(police_precincts, comparing_races, "Precinct", "addrpct")


mypopup <- paste0("Precinct: ", joint$addrpct, "<br>", 
                  "Black/White Prop w/ Force Used: ", joint$B_over_W)

mypal <- colorNumeric(
  palette = "Spectral",
  domain = c(-1,1),
  reverse = TRUE
)


mypopup2 <- paste0("Precinct: ", joint$addrpct, "<br>", 
                  "Hispanic/White Prop w/ Force Used: ", joint$H_over_W)

mypal2 <- colorNumeric(
  palette = "Spectral",
  domain = c(-1,1),
  reverse = TRUE
)

options(digits = 5)

prob_force_used <- leaflet(joint) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal(joint$logBW),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(#pal = mypal, 
            values = c(-1,1), 
            position = "topleft", 
            colors = c("#d7191c",
              "#fdae61",
              "#ffffbf",
              "#abdda4",
              "#2b83ba"),
            labels = signif(exp(seq(log(0.3678794), log(2.718282), length.out = 5)), 3),
            title = "Log Odds of Being<br>
            Subject to Force<br>
            vs White Baseline")

prob_force_used



saveWidget(prob_force_used, 
           "../figures/prob_force_used.html", 
           selfcontained = FALSE)
webshot("../figures/prob_force_used.html",
        file = "../figures/prob_force_used.png",
        cliprect = "viewport")

