library(tidyverse)
load("merged_ppcs.RData")
load("ppcs_2015.RData")

# training
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

# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------
# in-sample predictions
# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------



in_sample_ppcs <- data.frame(actual = merged_ppcs$force, 
                             prob = predict(logit, merged_ppcs, type = "response")) %>%
  mutate(pred = ifelse(prob > 0.03, 1, 0))

table(actual = in_sample_ppcs$actual, predicted = in_sample_ppcs$pred)

in_sample_ppcs %>%
  ggplot(aes(x = prob)) +
  geom_histogram(bins = 50)

# accuracy: fraction of correct classifications
in_sample_ppcs %>%
  summarize(acc = mean(pred == actual))

# precision: fraction of positive predictions that are actually true
in_sample_ppcs %>%
  filter(pred == 1) %>%
  summarize(prec = mean(actual == 1))

# recall: fraction of true examples that we predicted to be positive
# aka true positive rate, sensitivity
in_sample_ppcs %>%
  filter(actual == 1) %>%
  summarize(recall = mean(pred == 1))

# false positive rate: fraction of false examples that we predicted to be positive
in_sample_ppcs %>%
  filter(actual == 0) %>%
  summarize(fpr = mean(pred == 1))

### logit 2
in_sample_ppcs <- data.frame(actual = merged_ppcs$force, 
                             prob = predict(logit2, merged_ppcs, type = "response")) %>%
  mutate(pred = ifelse(prob > 0.03, 1, 0))

table(actual = in_sample_ppcs$actual, predicted = in_sample_ppcs$pred)


###
# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------
# out-of-sample prediction
# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------
ppcs_2015 <- ppcs_2015 %>%
  mutate(
    at_least_grabbed = ifelse(
      grab_push == 1 |
        handcuffed == 1|
        point_gun == 1 |
        hit_kick == 1  |
        pepper_stun == 1, 1, 0),
    
    at_least_handcuffed = ifelse(
      handcuffed == 1|
        point_gun == 1 |
        hit_kick == 1  |
        pepper_stun == 1, 1, 0),
    
    at_least_point_gun = ifelse(
      point_gun == 1 |
        hit_kick == 1  |
        pepper_stun == 1, 1, 0),
    
    at_least_kick_pepper_stun = ifelse(
      point_gun == 1 |
        hit_kick == 1  |
        pepper_stun == 1, 1, 0),
    
    force = ifelse(
      any_force == 1 |
        grab_push == 1 |
        handcuffed == 1|
        point_gun == 1 |
        hit_kick == 1  |
        pepper_stun == 1, 1, 0),
    year = as.factor(year),
    civilian_race = factor(civilian_race, levels = c("white", "black", "hispanic", "other")),
    civilian_gender = as.factor(civilian_gender),
    civilian_income = as.factor(civilian_income),
    civilian_employed = as.factor(civilian_employed),
    population_size = as.factor(population_size),
    time_of_encounter = as.factor(time_of_encounter),
    off_black = as.factor(off_black),
    off_white = as.factor(off_white),
    off_hispanic = as.factor(off_hispanic),
    off_other = as.factor(off_other),
    off_split = as.factor(off_split),
    type_of_incident = as.factor(type_of_incident),
    civilian_behavior = as.factor(civilian_behavior),
    civilian_searched = as.factor(civilian_searched),
    civilian_arrested = as.factor(civilian_arrested),
    civilian_guilty_of_illegal = as.factor(civilian_guilty_of_illegal),
    civilian_injured = as.factor(civilian_injured),
    excess_force = as.factor(excess_force),
    any_force = as.factor(any_force),
    grab_push = as.factor(grab_push),
    hit_kick = as.factor(hit_kick),
    point_gun = as.factor(point_gun),
    handcuffed = as.factor(handcuffed),
    pepper_stun = as.factor(pepper_stun),
    at_least_grabbed = as.factor(at_least_grabbed),
    at_least_handcuffed = as.factor(at_least_handcuffed),
    at_least_point_gun = as.factor(at_least_point_gun),
    at_least_kick_pepper_stun = as.factor(at_least_kick_pepper_stun),
    force = as.factor(force)
  ) 

out_sample_ppcs <- data.frame(actual = ppcs_2015$force, 
                              prob = predict(logit, ppcs_2015, type = "response")) %>%
  mutate(pred = ifelse(prob > 0.03, 1, 0))

table(actual = out_sample_ppcs$actual, predicted = out_sample_ppcs$pred)

# accuracy: fraction of correct classifications
out_sample_ppcs %>%
  summarize(acc = mean(pred == actual))

# precision: fraction of positive predictions that are actually true
out_sample_ppcs %>%
  filter(pred == 1) %>%
  summarize(prec = mean(actual == 1))

# recall: fraction of true examples that we predicted to be positive
# aka true positive rate, sensitivity
out_sample_ppcs %>%
  filter(actual == 1) %>%
  summarize(recall = mean(pred == 1))

# false positive rate: fraction of false examples that we predicted to be positive
out_sample_ppcs %>%
  filter(actual == 0) %>%
  summarize(fpr = mean(pred == 1))
