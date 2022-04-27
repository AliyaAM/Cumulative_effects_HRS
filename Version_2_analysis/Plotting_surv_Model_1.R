
library("survival")
library("survminer")
library("dplyr")




#Model 1: age and sex, wealth  [basis adjustment]
Model_1 = c("continious_age", "wealth_noIRA", "sex_1_2")
#Model 2: age, sex, wealth, BMI, hypertension  [basic adjustment + diabetes risk factors]
Model_2 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "hypertension_new_bin")
#Model 3: age, sex, wealth, physical activity, smoking (yes/no), and alcohol (days/week) [basic adjustment + health behaviours]
Model_3 = c("continious_age", "wealth_noIRA", "sex_1_2","alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin')
#Model 4: age, sex, wealth, CVD  [basic adjustment + CVD most common diabetes co_morbidity]
Model_4 = c("continious_age", "wealth_noIRA", "sex_1_2","CVD")
#Model 5: age, sex, wealth, depression  [basic adjustment + depression best researched psychosocial factor in diabetes ]
Model_5 = c("continious_age","wealth_noIRA", "sex_1_2","checklist_depression_bin")
#Model 6: age, sex, wealth, BMI, hypertension, CVD  [basic adjustment + diabetes risk factors+ CVD]
Model_6 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "hypertension_new_bin", "CVD")
#Model 7: age, sex, wealth, BMI, hypertension, depression  [basic adjustment + diabetes risk factors+ Depression]
Model_7 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "hypertension_new_bin", "checklist_depression_bin")


#Model 1: age and sex, wealth  [basis adjustment]
Model_1_nosex = c("continious_age", "wealth_noIRA")
#Model 2: age, sex, wealth, BMI, hypertension  [basic adjustment + diabetes risk factors]
Model_2_nosex = c("continious_age", "wealth_noIRA",  "assessed_BMI", "hypertension_new_bin")
#Model 3: age, sex, wealth, physical activity, smoking (yes/no), and alcohol (days/week) [basic adjustment + health behaviours]
Model_3_nosex = c("continious_age", "wealth_noIRA", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin')
#Model 4: age, sex, wealth, CVD  [basic adjustment + CVD most common diabetes co_morbidity]
Model_4_nosex = c("continious_age", "wealth_noIRA","CVD")
#Model 5: age, sex, wealth, depression  [basic adjustment + depression best researched psychosocial factor in diabetes ]
Model_5_nosex = c("continious_age","wealth_noIRA", "checklist_depression_bin")
#Model 6: age, sex, wealth, BMI, hypertension, CVD  [basic adjustment + diabetes risk factors+ CVD]
Model_6_nosex = c("continious_age", "wealth_noIRA",  "assessed_BMI", "hypertension_new_bin", "CVD")
#Model 7: age, sex, wealth, BMI, hypertension, depression  [basic adjustment + diabetes risk factors+ Depression]
Model_7_nosex = c("continious_age", "wealth_noIRA",  "assessed_BMI", "hypertension_new_bin", "checklist_depression_bin")


######


#Model 1: age and sex, wealth  [basis adjustment]
Model_noBMIcov_1 = c("continious_age", "wealth_noIRA", "sex_1_2")
#Model 2: age, sex, wealth, BMI, hypertension  [basic adjustment + diabetes risk factors]
Model_noBMIcov_2 = c("continious_age", "wealth_noIRA", "sex_1_2", "hypertension_new_bin")
#Model 3: age, sex, wealth, physical activity, smoking (yes/no), and alcohol (days/week) [basic adjustment + health behaviours]
Model_noBMIcov_3 = c("continious_age", "wealth_noIRA", "sex_1_2","alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin')
#Model 4: age, sex, wealth, CVD  [basic adjustment + CVD most common diabetes co_morbidity]
Model_noBMIcov_4 = c("continious_age", "wealth_noIRA", "sex_1_2","CVD")
#Model 5: age, sex, wealth, depression  [basic adjustment + depression best researched psychosocial factor in diabetes ]
Model_noBMIcov_5 = c("continious_age","wealth_noIRA", "sex_1_2","checklist_depression_bin")
#Model 6: age, sex, wealth, BMI, hypertension, CVD  [basic adjustment + diabetes risk factors+ CVD]
Model_noBMIcov_6 = c("continious_age", "wealth_noIRA", "sex_1_2", "hypertension_new_bin", "CVD")
#Model 7: age, sex, wealth, BMI, hypertension, depression  [basic adjustment + diabetes risk factors+ Depression]
Model_noBMIcov_7 = c("continious_age", "wealth_noIRA", "sex_1_2",  "hypertension_new_bin", "checklist_depression_bin")

###### DATA:

#cumulative_effects_dat = read.csv("/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/DATA_FOR_PLOT/all_waves_nodiabatbaseline_DIAB.csv")
cumulative_effects_dat = read.csv("/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/DATA_FOR_PLOT/all_waves_nodiabatbaseline_DIAB.csv")

###### Adding variables to the main dataset:

#outcome:  cumulative_effects_dat$diabetes_new_bin
#exposure: cumulative_effects_dat$discrim_bin
#time: cumulative_effects_dat$stop_new

###### Adding variables to the main dataset:
###### Adding variables to the main dataset:

cumulative_effects_dat$discrimination = cumulative_effects_dat$discrim_bin
cumulative_effects_dat$years = 2 *cumulative_effects_dat$timepoints_indiv

#1 = 2 year 
#2 = 4 years 
#3 = 6 years 

#coxph(Surv(t1, t2, stat) ∼ (age + surgery)* transplant) – time dependent covariates.

unique(cumulative_effects_dat$timepoints_indiv)
unique(cumulative_effects_dat$start_new)

cumulative_effects_dat$diabetes_new_bin_reversed = case_when(cumulative_effects_dat$diabetes_new_bin == 1 ~ 0, 
                                                             cumulative_effects_dat$diabetes_new_bin == 0 ~ 1) 

#Subset datasets:
data_male = subset(cumulative_effects_dat, cumulative_effects_dat$sex_1_2 ==1) 
data_female = subset(cumulative_effects_dat, cumulative_effects_dat$sex_1_2 ==2) 
data_race = subset(cumulative_effects_dat, cumulative_effects_dat$race_white == 0) 
data_BMI = subset(cumulative_effects_dat, cumulative_effects_dat$assessed_BMI > 30) 

#cumulative_effects_dat$sex_1_2
#cumulative_effects_dat$race_white
#cumulative_effects_dat$race_white
#cumulative_effects_dat$national_origin_ousideUS_bin
#cumulative_effects_dat$religion_bin
#cumulative_effects_dat$assessed_BMI


#### plot for the entire dataset: 

fit <- survfit(Surv(years, diabetes_new_bin_reversed)~ (discrimination + continious_age + wealth_noIRA + sex_1_2), data = cumulative_effects_dat)
summary_all = summary(fit)


### output below: 

#summary_all_table = summary_all$table
#subset_all = rep("All", time=nrow(summary_all_table))
#summary_all_table = cbind(subset_all, summary_all_table)

#write.csv(summary_all_table, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/summary_all_table.csv")

Number_events_all = fit$n.event
Number_risk_all = fit$n.risk
Number_censored_all = fit$n.censor
probability_for_nodiab_all = fit$surv
probability_SE_all = fit$std.err
probability_lower_CI_all = fit$lower
probability_upper_CI_all = fit$upper
cumhaz_output_all =  fit$cumhaz
cumhaz_output_std_all = fit$std.chaz




####
### output below: 
All_results_Model_1 = cbind(Number_events_all,
                               Number_risk_all,
                               Number_censored_all, 
                               probability_for_nodiab_all, 
                               probability_SE_all, 
                               probability_lower_CI_all, 
                               probability_upper_CI_all, 
                               cumhaz_output_all, 
                               cumhaz_output_std_all) 

subset_all_undj  = rep("All", time=nrow(All_results_Model_1)) 
All_results_Model_1 = cbind(subset_all_undj, All_results_Model_1) 
write.csv(All_results_Model_1, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/All_results_Model_1.csv")


plot_all = ggsurvplot(fit,
                      pval = TRUE, conf.int = TRUE,
                      risk.table = FALSE, # Add risk table
                      risk.table.col = "strata", # Change risk table color by groups
                      linetype = "strata", # Change line type by groups
                      surv.median.line = "hv", # Specify median survival
                      ggtheme = theme_bw(), # Change ggplot2 theme
                      palette = c("#E7B800", "#2E9FDF"))

print(plot_all)  


########
########
########
########

#### plot for female dataset: 

fit_female <- survfit(Surv(years, diabetes_new_bin_reversed)~ (discrimination + continious_age + wealth_noIRA + sex_1_2), data = data_female)
summary_female = summary(fit_female)


### output below: 
subset_female = rep("female", time=nrow(fit_female))
summary_female_table = summary_female$table
summary_female_table = cbind(subset_female, summary_female_table)

write.csv(summary_female_table, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/summary_female_table.csv")

Number_events_female = fit_female$n.event
Number_risk_female = fit_female$n.risk
Number_censored_female = fit_female$n.censor
probability_for_nodiab_female = fit_female$surv
probability_SE_female = fit_female$std.err
probability_lower_CI_female = fit_female$lower
probability_upper_CI_female = fit_female$upper
cumhaz_output_female =  fit_female$cumhaz
cumhaz_output_std_female = fit_female$std.chaz




####
### output below: 
female_results_Model_1 = cbind(Number_events_female,
                                  Number_risk_female,
                                  Number_censored_female, 
                                  probability_for_nodiab_female, 
                                  probability_SE_female, 
                                  probability_lower_CI_female, 
                                  probability_upper_CI_female, 
                                  cumhaz_output_female, 
                                  cumhaz_output_std_female) 

subset_female_undj = rep("female", time=nrow(female_results_Model_1))
female_results_Model_1 = cbind(subset_female_undj, female_results_Model_1) 
write.csv(female_results_Model_1, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/female_results_Model_1.csv")

plot_female = ggsurvplot(fit_female,
                         pval = TRUE, conf.int = TRUE,
                         risk.table = FALSE, # Add risk table
                         risk.table.col = "strata", # Change risk table color by groups
                         linetype = "strata", # Change line type by groups
                         surv.median.line = "hv", # Specify median survival
                         ggtheme = theme_bw(), # Change ggplot2 theme
                         palette = c("#E7B800", "#2E9FDF"))

print(plot_female) 

########
########
########
########

#### plot for male dataset: 

fit_male <- survfit(Surv(years, diabetes_new_bin_reversed)~ discrimination + continious_age + wealth_noIRA + sex_1_2, data = data_male)
summary_male = summary(fit_male)


### output below: 
subset_male = rep("male", time=nrow(fit_male))
summary_male_table = summary_male$table
summary_male_table = cbind(subset_male, summary_male_table)

write.csv(summary_male_table, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/summary_male_table.csv")

Number_events_male = fit_male$n.event
Number_risk_male = fit_male$n.risk
Number_censored_male = fit_male$n.censor
probability_for_nodiab_male = fit_male$surv
probability_SE_male = fit_male$std.err
probability_lower_CI_male = fit_male$lower
probability_upper_CI_male = fit_male$upper
cumhaz_output_male =  fit_male$cumhaz
cumhaz_output_std_male = fit_male$std.chaz




####
### output below: 
male_results_Model_1 = cbind(Number_events_male,
                                Number_risk_male,
                                Number_censored_male, 
                                probability_for_nodiab_male, 
                                probability_SE_male, 
                                probability_lower_CI_male, 
                                probability_upper_CI_male, 
                                cumhaz_output_male, 
                                cumhaz_output_std_male) 

subset_male_undj = rep("male", time=nrow(male_results_Model_1))
male_results_Model_1 = cbind(subset_male_undj, male_results_Model_1) 
write.csv(male_results_Model_1, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/male_results_Model_1.csv")

plot_male = ggsurvplot(fit_male,
                       pval = TRUE, conf.int = TRUE,
                       risk.table = FALSE, # Add risk table
                       risk.table.col = "strata", # Change risk table color by groups
                       linetype = "strata", # Change line type by groups
                       surv.median.line = "hv", # Specify median survival
                       ggtheme = theme_bw(), # Change ggplot2 theme
                       palette = c("#E7B800", "#2E9FDF"))

print(plot_male)  


########
########
########
########

#### plot for race dataset: 

fit_race <- survfit(Surv(years, diabetes_new_bin_reversed)~ discrimination + continious_age + wealth_noIRA + sex_1_2, data = data_race)
summary_race = summary(fit_race)


### output below: 
subset_race = rep("race", time=nrow(fit_race))
summary_race_table = summary_race$table
summary_race_table = cbind(subset_race, summary_race_table)

write.csv(summary_race_table, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/summary_race_table.csv")

Number_events_race = fit_race$n.event
Number_risk_race = fit_race$n.risk
Number_censored_race = fit_race$n.censor
probability_for_nodiab_race = fit_race$surv
probability_SE_race = fit_race$std.err
probability_lower_CI_race = fit_race$lower
probability_upper_CI_race = fit_race$upper
cumhaz_output_race =  fit_race$cumhaz
cumhaz_output_std_race = fit_race$std.chaz




####
### output below: 
race_results_Model_1 = cbind(Number_events_race,
                                Number_risk_race,
                                Number_censored_race, 
                                probability_for_nodiab_race, 
                                probability_SE_race, 
                                probability_lower_CI_race, 
                                probability_upper_CI_race, 
                                cumhaz_output_race, 
                                cumhaz_output_std_race) 

subset_race_undj = rep("race", time=nrow(race_results_Model_1))
race_results_Model_1 = cbind(subset_race_undj, race_results_Model_1) 
write.csv(race_results_Model_1, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/race_results_Model_1.csv")

plot_race = ggsurvplot(fit_race,
                       pval = TRUE, conf.int = TRUE,
                       risk.table = FALSE, # Add risk table
                       risk.table.col = "strata", # Change risk table color by groups
                       linetype = "strata", # Change line type by groups
                       surv.median.line = "hv", # Specify median survival
                       ggtheme = theme_bw(), # Change ggplot2 theme
                       palette = c("#E7B800", "#2E9FDF"))

print(plot_race)  

########
########
########
########

#### plot for BMI dataset: 

fit_BMI <- survfit(Surv(years, diabetes_new_bin_reversed)~ discrimination + continious_age + wealth_noIRA + sex_1_2, data = data_BMI)
summary_BMI = summary(fit_BMI)


### output below: 
subset_BMI = rep("BMI", time=nrow(fit_BMI))
summary_BMI_table = summary_BMI$table
summary_BMI_table = cbind(subset_BMI, summary_BMI_table)

write.csv(summary_BMI_table, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/summary_BMI_table.csv")

Number_events_BMI = fit_BMI$n.event
Number_risk_BMI = fit_BMI$n.risk
Number_censored_BMI = fit_BMI$n.censor
probability_for_nodiab_BMI = fit_BMI$surv
probability_SE_BMI = fit_BMI$std.err
probability_lower_CI_BMI = fit_BMI$lower
probability_upper_CI_BMI = fit_BMI$upper
cumhaz_output_BMI =  fit_BMI$cumhaz
cumhaz_output_std_BMI = fit_BMI$std.chaz




####
### output below: 
BMI_results_Model_1 = cbind(Number_events_BMI,
                               Number_risk_BMI,
                               Number_censored_BMI, 
                               probability_for_nodiab_BMI, 
                               probability_SE_BMI, 
                               probability_lower_CI_BMI, 
                               probability_upper_CI_BMI, 
                               cumhaz_output_BMI, 
                               cumhaz_output_std_BMI) 

subset_BMI_undj = rep("BMI", time=nrow(BMI_results_Model_1))
BMI_results_Model_1 = cbind(subset_BMI_undj, BMI_results_Model_1) 

write.csv(BMI_results_Model_1, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/BMI_results_Model_1.csv")


plot_BMI = ggsurvplot(fit_BMI,
                      pval = TRUE, conf.int = TRUE,
                      risk.table = FALSE, # Add risk table
                      risk.table.col = "strata", # Change risk table color by groups
                      linetype = "strata", # Change line type by groups
                      surv.median.line = "hv", # Specify median survival
                      ggtheme = theme_bw(), # Change ggplot2 theme
                      palette = c("#E7B800", "#2E9FDF"))

print(plot_BMI)  