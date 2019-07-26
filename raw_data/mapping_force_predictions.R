library(tidyverse)
library(httr)
library(rgdal)
library(ROCR)

# load the logistic model that uses year and precinct as controls
load("log_model.RData")

# load the stop and frisk data
load("sqf_03_13.RData")

# get the list of police precincts
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nypp/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

log_panel_data <- sf_data1 %>%
  # filter out unknown races, unknown genders, ages outside of the range 10-90,
  # and types of id equal to "other"
  filter(race != " " & race != "U" & race != "X" & sex != "Z" & typeofid != "O" &
           age > 10 & age <= 90) %>%
  # recode Black Hispanic as Black, American Indian as Other
  mutate(race = recode_factor(race,"P" = "B", "I" = "Z"),
         # create an any_force_used column - ADDED pf_other TO THIS CALCULATION
         any_force_used = paste(pf_hands, pf_grnd, pf_wall, pf_drwep, pf_ptwep,
                                pf_hcuff,pf_baton, pf_pepsp, sep = ""),
         # create factors from columns
         any_force_used = as.factor(if_else(grepl("Y",any_force_used), 1, 0)),
         sex = as.factor(if_else(grepl("M",sex), 1, 0)),
         inout = as.factor(if_else(grepl("I",inout), 1, 0)),
         timestop = as.numeric(timestop),
         daytime = as.factor(ifelse(600<=timestop & timestop<=1700,"1","0")),
         ac_incid = as.factor(if_else(grepl("Y",ac_incid), 1, 0)),
         ac_time = as.factor(if_else(grepl("Y",ac_time), 1, 0)),
         offunif = as.factor(if_else(grepl("Y",offunif), 1, 0)),
         othpers = as.factor(if_else(grepl("Y",othpers), 1, 0)),
         cs_objcs = as.factor(if_else(grepl("Y",cs_objcs), 1, 0)),
         cs_descr = as.factor(if_else(grepl("Y",cs_descr), 1, 0)),
         cs_casng = as.factor(if_else(grepl("Y",cs_casng), 1, 0)),
         cs_lkout = as.factor(if_else(grepl("Y",cs_lkout), 1, 0)),
         cs_cloth = as.factor(if_else(grepl("Y",cs_cloth), 1, 0)),
         cs_drgtr = as.factor(if_else(grepl("Y",cs_drgtr), 1, 0)),
         cs_furtv = as.factor(if_else(grepl("Y",cs_furtv), 1, 0)),
         cs_vcrim = as.factor(if_else(grepl("Y",cs_vcrim), 1, 0)),
         cs_bulge = as.factor(if_else(grepl("Y",cs_bulge), 1, 0)),
         cs_other = as.factor(if_else(grepl("Y",cs_other), 1, 0)),
         wepnfnd = paste(contrabn,asltweap,pistol,riflshot,knifcuti,machgun,othrweap, sep = ""),
         wepnfnd = as.factor(if_else(grepl("Y",wepnfnd), 1, 0)),
         pf_pp_spray_baton = paste(pf_pepsp, pf_baton,sep = ""),
         # CHANGED Y/N TO 1/0
         pf_pp_spray_baton = as.factor(ifelse(grepl("Y",pf_pp_spray_baton), 1, 0)),
         year = as.factor(year),
         # recode race names for clarity
         race = recode_factor(race, "W" = "White", "B" = "Black",  "Q" ="Hispanic",
                              "A" = "Asian", "Z" = "Other"))

log_panel_data <- log_panel_data %>% filter(typeofid != " " & year != 2003) %>%
  mutate(addrpct = as.factor(addrpct))

# add predictions to this test data using the logistic model
predictions <- predict(mylogit_prec, log_panel_data, type = "response")
test_predictions <- data.frame(log_panel_data, predictions)

# calculate how the predicted probabilities of being subject to police force
# compare between different races in each precinct
sqf_predictions <- test_predictions %>%
  select(addrpct, race, predictions) %>%
  filter(!is.na(addrpct)) %>%
  group_by(addrpct, race) %>%
  filter(!is.na(predictions)) %>%
  summarize(predictions = mean(predictions)) %>%
  spread(race, predictions) %>%
  mutate(H_over_W = Hispanic/White,
         logHW = log(H_over_W),
         B_over_W = Black/White,
         logBW = log(B_over_W))

########## MAP THE RESULTS ##########

# read in police precinct shape data
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nypp/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

# Join the precinct shape data with the data about the precincts
joint_pred <- geo_join(police_precincts, sqf_predictions, "Precinct", "addrpct")


mypopup3 <- paste0("Precinct: ", joint_pred$addrpct, "<br>", 
                  "Black/White Predicted Prop w/<br>Force Used: ", joint_pred$B_over_W)

mypal3 <- colorNumeric(
  palette = "Spectral",
  domain = c(-1,1),
  reverse = TRUE
)

mypopup4 <- paste0("Precinct: ", joint_pred$addrpct, "<br>", 
                   "Hispanic/White Predicted Prop w/<br>Force Used: ", joint_pred$H_over_W)

mypal4 <- colorNumeric(
  palette = "Spectral",
  domain = c(-1,1),
  reverse = TRUE
)

predicted_prob_force_used <- leaflet(joint_pred) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal4(joint_pred$logHW),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup4,
              group = "Hispanic") %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~mypal3(joint_pred$logBW),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup3,
              group = "Black") %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal3, 
            values = c(-1,1), 
            position = "topleft", 
            title = "Predicted Odds of Being<br>
            Subject to Force<br>
            vs White Baseline") %>%
  addLayersControl(c("Black", "Hispanic"),
                   options = layersControlOptions(collapsed = FALSE))

predicted_prob_force_used

saveWidget(predicted_prob_force_used, 
           "../figures/predicted_prob_force_used.html", 
           selfcontained = FALSE)
webshot("../figures/predicted_prob_force_used.html",
        file = "../figures/predicted_prob_force_used.png",
        cliprect = "viewport")
