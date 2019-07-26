library(tidyverse)
load("merged_ppcs.RData")


logit <- glm(force ~ civilian_race, family = "binomial", data= merged_ppcs)
exp(coef(logit))
summary(logit)

logit2 <- glm(force ~ civilian_race + civilian_gender + civilian_income + civilian_employed + population_size + I(civilian_age^2), family='binomial', data = merged_ppcs)
summary(logit2)
exp(coef(logit2))

logit3 <- glm(force ~ civilian_race + civilian_gender + civilian_income + civilian_employed + population_size + I(civilian_age^2) + time_of_encounter + type_of_incident + off_hispanic + off_white + off_black + off_other + off_split, family='binomial', data = merged_ppcs)
summary(logit3)
exp(coef(logit3))

logit4 <- glm(force ~ civilian_race + civilian_gender + civilian_income + civilian_employed + population_size + I(civilian_age^2) + time_of_encounter + type_of_incident + off_hispanic + off_white + off_black + off_other + off_split + civilian_behavior, family='binomial', data = merged_ppcs)
summary(logit4)
exp(coef(logit4))

logit5 <- glm(force ~ civilian_race + civilian_gender + civilian_income + civilian_employed + population_size + I(civilian_age^2) + time_of_encounter + type_of_incident + off_hispanic + off_white + off_black + off_other + off_split + civilian_behavior + year, family='binomial', data = merged_ppcs)
summary(logit5)
exp(coef(logit5))
