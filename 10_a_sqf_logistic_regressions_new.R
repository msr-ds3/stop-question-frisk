if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, gridExtra, naniar, scales, miceadds, here, pROC, ROCR)

#load the 2003-2018 dataset
#make sure you set the directory using "setwd" to the "stop-question-frisk" directory before loading the "here" library

load(here("clean_data","sqf_03_13.Rdata"))

#create a new data frame for the logistic regression model by converting the necessary columns to factors and creating new columns


log_data <- sf_data1 %>%
  mutate(any_force_used = paste(pf_hands, pf_grnd, pf_wall, pf_drwep, pf_ptwep,
                                pf_hcuff,pf_baton, pf_pepsp, sep = ""),
         wepnfnd = paste(contrabn,asltweap,pistol,riflshot,knifcuti,machgun,othrweap, sep = "")
  )%>%
  select(any_force_used,race,sex,age,inout,ac_incid,ac_time,timestop,
         offunif,typeofid,othpers,cs_bulge,cs_cloth,cs_casng,cs_lkout,
         cs_descr,cs_drgtr,cs_furtv,cs_vcrim,
         cs_objcs,cs_other,wepnfnd,pct,year)%>%
  filter(race != "X" & race != " " & typeofid != " ") %>%
  mutate(race = recode_factor(race,"P" = "B","Q" = "H"),
         any_force_used = as.factor(if_else(grepl("Y",any_force_used), 1, 0)),
         sex = as.factor(sex),
         inout = as.factor(if_else(grepl("I",inout), 1, 0)),
         timestop = as.numeric(gsub(":","",timestop)),
         daytime = as.factor(ifelse(600<=timestop & timestop<=1700,1,0)),
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
         wepnfnd = as.factor(if_else(grepl("Y",wepnfnd), 1, 0)),
         race = recode_factor(race, "W" = "White", "B" = "Black",  "H" ="Hispanic",
                              "A" = "Asian", "I" ="Other", "Z" = "Other"),
         year = as.factor(year),
         pct = as.factor(pct))


#model with precinct and year levels added as controls
model_full_control <- glm(any_force_used ~ race + sex + age + I(age^2) + inout + daytime + ac_incid + ac_time +
                            offunif + typeofid + othpers + cs_bulge + cs_cloth + cs_casng + cs_lkout +
                            cs_descr + cs_drgtr + cs_furtv + cs_vcrim +
                            cs_objcs + cs_other + wepnfnd + pct + year,
                          data = log_data,
                          family = "binomial", y=FALSE, model=FALSE)

#model with no control: race as the only predictor and Use of force as the outcome
model_no_race <- glm(any_force_used ~ sex + age + I(age^2) + inout + daytime + ac_incid + ac_time +
                       offunif + typeofid + othpers + cs_bulge + cs_cloth + cs_casng + cs_lkout +
                       cs_descr + cs_drgtr + cs_furtv + cs_vcrim +
                       cs_objcs + cs_other + wepnfnd + pct + year,
                     data = log_data,
                     family = "binomial", y=FALSE, model=FALSE)


#calculating AUC for model_ful_cntrl
#drop the missing values and create a new dataframe
new_log_data <- log_data %>%
  drop_na(daytime,race , sex , age, inout, ac_incid, ac_time,
          offunif, typeofid , othpers , cs_bulge , cs_cloth , cs_casng , cs_lkout ,
          cs_descr , cs_drgtr , cs_furtv , cs_vcrim ,
          cs_objcs , cs_other ,pct , year,
          any_force_used,
          wepnfnd)

#create a new data frame with two columns of actual force used and predicted probability of use of force
df <- data.frame(actual = new_log_data$any_force_used,
                 probs = predict(model_full_control, new_log_data, type = "response"))

#create numeric objects for predicted probabilities and actual values
pred_probs <- as.numeric(round(df$probs))
actual_values <- as.numeric(as.character(df$actual))

#calculate the area under the curve
roc_obj <- roc(pred_probs,actual_values)
auc(roc_obj)

#calculating AUC for model_no_race
#drop the missing values and create a new dataframe
new_data_no_race <- log_data %>%
  drop_na(daytime, sex , age, inout, ac_incid, ac_time,
          offunif, typeofid , othpers , cs_bulge , cs_cloth , cs_casng , cs_lkout ,
          cs_descr , cs_drgtr , cs_furtv , cs_vcrim ,
          cs_objcs , cs_other ,pct , year,
          any_force_used,
          wepnfnd)

#create a new data frame with two columns of actual force used and predicted probability of use of force
df_no_race <- data.frame(actual = new_data_no_race$any_force_used,
                         probs = predict(model_no_race, new_data_no_race, type = "response"))

#create numeric objects for predicted probabilities and actual values
pred_probs_no_race <- as.numeric(round(df_no_race$probs))
actual_values_no_race <- as.numeric(as.character(df_no_race$actual))

#calculate the area under the curve
roc_obj_no_race <- roc(pred_probs_no_race,actual_values_no_race)
auc(roc_obj_no_race)

#ROC Curve for model_ful_cntrl
pred_roc <- prediction(df$probs, df$actual)
eval <- performance(pred_roc,  measure='tpr', x.measure='fpr')

roc_eval <- data.frame(fpr=unlist(eval@x.values), tpr=unlist(eval@y.values))
ggplot(data=roc_eval, aes(x=fpr, y=tpr)) +
  theme_bw()+
  geom_line(color = "#451878") +
  geom_abline(linetype=2, color = "#451878") +
  scale_x_continuous(labels=percent, lim=c(0,1)) +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  xlab('Probability of a False Alarm') +
  ylab('Probability of Detecting Use of Force') +
  labs(title = "ROC Curve") +
  theme(axis.title.x = element_text(face = "bold", size=20, color = "#0d693e"),
        axis.title.y = element_text(face = "bold", size=20, color = "#0d693e"),
        axis.text.x = element_text(face = "bold", size=15),
        axis.text.y = element_text(face = "bold",size=18),
        plot.title = element_text(hjust = 0.5,size=25, color = "#374687",face = "bold"),
        legend.title = element_text(color = "#0d693e", face = "bold", size = 18 ),
        legend.text = element_text(size = 15 ))+
  ggsave(here("figures","sqf_roc_curve.png"),  width = 12, height = 10, dpi = 150, units = "in", device='png')


#ROC Curve for model_no_race
pred_roc_no_race <- prediction(df_no_race$probs, df_no_race$actual)
eval_no_race <- performance(pred_roc_no_race,  measure='tpr', x.measure='fpr')


roc_eval_no_race <- data.frame(fpr_no_race=unlist(eval_no_race@x.values),
                               tpr_no_race=unlist(eval_no_race@y.values))
ggplot(data=roc_eval_no_race, aes(x=fpr_no_race, y=tpr_no_race)) +
  theme_bw()+
  geom_line(color = "#451878") +
  geom_abline(linetype=2, color = "#451878") +
  scale_x_continuous(labels=percent, lim=c(0,1)) +
  scale_y_continuous(labels=percent, lim=c(0,1)) +
  xlab('Probability of a False Alarm') +
  ylab('Probability of Detecting Use of Force') +
  labs(title = "ROC Curve (No Race)") +
  theme(axis.title.x = element_text(face = "bold", size=20, color = "#0d693e"),
        axis.title.y = element_text(face = "bold", size=20, color = "#0d693e"),
        axis.text.x = element_text(face = "bold", size=15),
        axis.text.y = element_text(face = "bold",size=18),
        plot.title = element_text(hjust = 0.5,size=25, color = "#374687",face = "bold"),
        legend.title = element_text(color = "#0d693e", face = "bold", size = 18 ),
        legend.text = element_text(size = 15 ))+
  ggsave(here("figures","sqf_roc_curve_no_race.png"),  width = 12, height = 10, dpi = 150, units = "in", device='png')

save(model_full_control,file = 'model.rda')
saveRDS(log_data, file = here("log_data2.rds"))




