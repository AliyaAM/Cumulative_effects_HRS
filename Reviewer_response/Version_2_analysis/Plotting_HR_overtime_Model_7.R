#https://cran.r-project.org/web/packages/casebase/vignettes/plotsmoothHazard.html

library(casebase)
library(visreg)
library(splines)
library(ggplot2)

cumulative_effects_dat = read.csv("/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/DATA_FOR_PLOT/all_waves_nodiabatbaseline_DIAB.csv")


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
#Model_7 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "hypertension_new_bin", "checklist_depression_bin")


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


#outcome:  cumulative_effects_dat$diabetes_new_bin

#exposure: cumulative_effects_dat$discrim_bin

cumulative_effects_dat$discrimination = cumulative_effects_dat$discrim_bin

#time: cumulative_effects_dat$stop_new

#cumulative_effects_dat$sex_1_2

#cumulative_effects_dat$race_white

#cumulative_effects_dat$race_white
#cumulative_effects_dat$national_origin_ousideUS_bin
#cumulative_effects_dat$religion_bin


#cumulative_effects_dat$assessed_BMI

cumulative_effects_dat$years = 2 *cumulative_effects_dat$timepoints_indiv

#1 = 2 year 
#2 = 4 years 
#3 = 6 years 

unique(cumulative_effects_dat$timepoints_indiv)
unique(cumulative_effects_dat$start_new)

#####

alpha_level = 0.85



##### everyone ####### data = cumulative_effects_dat
mod_cb <- fitSmoothHazard(diabetes_new_bin ~ log(years) + discrimination + continious_age + wealth_noIRA + sex_1_2 + assessed_BMI + hypertension_new_bin + checklist_depression_bin,
                          data = cumulative_effects_dat,
                          time = "timepoints_indiv")

# plot exposed to and not exposed to discrimination next to each other: 
plot(mod_cb,
     hazard.params = list(xvar = "years",
                          by = "discrimination",
                          alpha = alpha_level,
                          ylab = "Hazard"))


#######
#######
##### female ####### data = cumulative_effects_dat

data_female = subset(cumulative_effects_dat, cumulative_effects_dat$sex_1_2 ==2) 

mod_cb_female <- fitSmoothHazard(diabetes_new_bin ~ log(years) + discrimination + continious_age + wealth_noIRA  + assessed_BMI + hypertension_new_bin + checklist_depression_bin,
                                 data = data_female,
                                 time = "timepoints_indiv")

# plot exposed to and not exposed to discrimination next to each other: 
plot(mod_cb_female,
     hazard.params = list(xvar = "years",
                          by = "discrimination",
                          alpha = alpha_level,
                          ylab = "Hazard"))



############

data_race = subset(cumulative_effects_dat, cumulative_effects_dat$race_white == 0) 

mod_cb_race <- fitSmoothHazard(diabetes_new_bin ~ log(years) + discrimination + continious_age + wealth_noIRA + sex_1_2 + assessed_BMI + hypertension_new_bin + checklist_depression_bin,
                               data = data_race,
                               time = "timepoints_indiv")

# plot exposed to and not exposed to discrimination next to each other: 
plot(mod_cb_race,
     hazard.params = list(xvar = "years",
                          by = "discrimination",
                          alpha = alpha_level,
                          ylab = "Hazard"))


############

data_BMI = subset(cumulative_effects_dat, cumulative_effects_dat$assessed_BMI > 30) 

mod_cb_BMI <- fitSmoothHazard(diabetes_new_bin ~ log(years) + discrimination + continious_age + + continious_age + wealth_noIRA + sex_1_2  + hypertension_new_bin + checklist_depression_bin,
                              data = data_BMI,
                              time = "timepoints_indiv")

# plot exposed to and not exposed to discrimination next to each other: 
plot(mod_cb_BMI,
     hazard.params = list(xvar = "years",
                          by = "discrimination",
                          alpha = alpha_level,
                          ylab = "Hazard"))


