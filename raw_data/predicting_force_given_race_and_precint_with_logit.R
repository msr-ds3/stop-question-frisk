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


test_predictions <- test_predictions %>%
  spread(race, predictions) %>%
  mutate(BoverW = Black/White, WoverW = White/White)

# calculate how the predicted probabilities of being subject to police force
# compare between different races in each precinct
discrimination <- test_predictions %>%
  spread(race, predictions) %>%
  mutate(HgreaterW = Hispanic > White,
         BgreaterW = Black > White,
         AgreaterW = Asian > White)

toy_predictions <- geo_join(police_precincts, test_predictions, "Precinct", "addrpct")

mypopup <- paste0("Precinct: ", toy_predictions$Precinct, "<br>", 
                   "B/W Odds: ", toy_predictions$BoverW)

mypal <- colorNumeric(
  palette = "Spectral",
  domain = c(-1,1),
  reverse = TRUE
)

mypal2 <- colorNumeric(
  palette = "Spectral",
  domain = exp(seq(log(0.3678794), log(2.718282), length.out = 10)),
  reverse = TRUE
)

leaflet(toy_predictions) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~mypal(log((toy_predictions$BoverW))),
              fillOpacity = .9,
              weight = 1,
              popup = mypopup) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = mypal2,
            #values = exp(c(-1,1)),
            labels = exp(c(-1,1)),
            values = exp(seq(log(0.3678794), log(2.718282), length.out = 10)),
            position = "topleft",
            title = "Predicted Odds of<br>Being Subject to Force")

