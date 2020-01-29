library(dplyr)
library(tidyr)
library(tidyverse)


officers <- read.csv("officers.csv")
View(officers)


individual <- read.csv("individuals.csv")
View(individual)


incidents <- read.csv("incidents.csv")
View(incidents)


#Changing column names to lower case
colnames(officers) <- tolower(colnames(officers))
colnames(individual) <- tolower(colnames(individual))
colnames(incidents) <- tolower(colnames(incidents))

View(officers)
individual


officers <- officers %>% rename("incident_id"="ï..incident_id")

individual <- individual %>% rename("incident_id"="ï..incident_id" )

incidents <- incidents %>% rename("incident_id"="ï..incident_id")

View(incidents)


df_1 <- left_join(officers, individual, by=c("incident_id" = "incident_id"))

View(df_1)

df_2 <- left_join(df_1, incidents, by=c("incident_id" = "incident_id"))

View(df_2)

#Seattle shootings
shootings <- read.csv("shooting_data.csv", header=TRUE, sep=",")

View(head(shootings))

total_shootings <- shootings %>% summarize(count=n())

total_shootings
s_by_race <- shootings %>% group_by(Subject.Race)



shootings_by_race <- shootings %>% group_by(Subject.Race) %>% summarize(count=n())
View(shootings_by_race)

shootings
