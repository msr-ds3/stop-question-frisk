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

# create a table of test data with one row for each precinct race combination
# and using the most common outcome for every other field (column) - 
# see the appendix for the summary statistics used for determining majorities
race <- rep(c("White", "Black", "Hispanic", "Asian", "Other"), 77)
sex <- as.factor(rep(1, 385))
age <- rep(median(sf_data1$age, na.rm = TRUE), 385)
inout <- as.factor(rep(0, 385))
daytime <- as.factor(rep(0, 385))
ac_incid <- as.factor(rep(1, 385))
ac_time <- as.factor(rep(0, 385))
offunif <- as.factor(rep(1, 385))
typeofid <- as.factor(rep("P", 385))
othpers <- as.factor(rep(0, 385))
cs_bulge <- as.factor(rep(0, 385))
cs_cloth <- as.factor(rep(0, 385))
cs_casng <- as.factor(rep(0, 385))
cs_lkout <- as.factor(rep(0, 385))
cs_descr <- as.factor(rep(0, 385))
cs_drgtr <- as.factor(rep(0, 385))
cs_furtv <- as.factor(rep(1, 385))
cs_vcrim <- as.factor(rep(0, 385))
cs_objcs <- as.factor(rep(0, 385))
cs_other <- as.factor(rep(0, 385))
wepnfnd <- as.factor(rep(0, 385))
addrpct <- as.factor(rep(police_precincts$Precinct, 5))
year <- as.factor(rep(2008, 385))

test_data <- data.frame(race, sex, age, inout, 
                          daytime, ac_incid, ac_time, offunif, typeofid, othpers, 
                          cs_bulge, cs_cloth, cs_casng, cs_lkout, cs_descr, cs_drgtr, 
                          cs_furtv, cs_vcrim, cs_objcs, cs_other, wepnfnd, addrpct, 
                          year)

# add predictions to this test data using the logistic model
predictions <- predict(mylogit_prec, test_data, type = "response")
test_predictions <- data.frame(test_data, predictions)

# calculate how the predicted probabilities of being subject to police force
# compare between different races in each precinct
discrimination <- test_predictions %>%
  spread(race, predictions) %>%
  mutate(HgreaterW = Hispanic > White,
         BgreaterW = Black > White,
         AgreaterW = Asian > White,
         H_over_W = Hispanic/White,
         logHW = log(H_over_W),
         B_over_W = Black/White,
         logBW = log(B_over_W))

########## MAP THE RESULTS ##########

# read in police precinct shape data
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nypp/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
police_precincts <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

# Join the precinct shape data with the data about the precincts
joint_pred <- geo_join(police_precincts, discrimination, "Precinct", "addrpct")


mypopup <- paste0("Precinct: ", joint_pred$addrpct, "<br>", 
                  "Black/White Prop w/ Force Used: ", joint_pred$B_over_W)

mypal <- colorNumeric(
  palette = "Spectral",
  domain = c(-1,1),
  reverse = TRUE
)

mypopup2 <- paste0("Precinct: ", joint_pred$addrpct, "<br>", 
                   "Hispanic/White Prop w/ Force Used: ", joint_pred$H_over_W)

mypal2 <- colorNumeric(
  palette = "Spectral",
  domain = c(-1,1),
  reverse = TRUE
)

predicted_prob_force_used <- leaflet(joint_pred) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal2(joint_pred$logHW),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup2,
              group = "Hispanic") %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~mypal(joint_pred$logBW),
              fillOpacity = 0.7,
              weight = 1,
              popup = mypopup,
              group = "Black") %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal, 
            values = c(-1,1), 
            position = "topleft", 
            title = "Log Odds of Being<br>
            Subject to Force<br>
            vs White Baseline") %>%
  addLayersControl(c("Black", "Hispanic"),
                   options = layersControlOptions(collapsed = FALSE))

predicted_prob_force_used

saveWidget(prob_force_used, 
           "../figures/predicted_prob_force_used.html", 
           selfcontained = FALSE)
webshot("../figures/predicted_prob_force_used.html",
        file = "../figures/predicted_prob_force_used.png",
        cliprect = "viewport")
