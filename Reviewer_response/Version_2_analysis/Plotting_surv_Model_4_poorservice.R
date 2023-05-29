
library("survival")
library("survminer")
library("dplyr")

library("powerSurvEpi")



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
# below is the entire dataset, not subseted to anyone: 

cumulative_effects_dat = read.csv("/Users/aliya/my_docs/proj/Cumulative_effects_HRS/data_files/all_waves_nodiabatbaseline_DIAB.csv")



#cumulative_effects_dat = read.csv("/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/DATA_FOR_PLOT/all_waves_nodiabatbaseline_DIAB.csv")
#cumulative_effects_dat = read.csv("/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/DATA_FOR_PLOT/all_waves_nodiab_at_two_first_waves_DIAB_discrim_recoded.csv")



###### Adding variables to the main dataset:

#we adjusted for verified medical history (no/yes) of cardiovascular diseases (CVD) such as "heart condition", "stroke", "angina", "heart failure", and "heart attack”. 
##### recode below: 

cumulative_effects_dat$CVD[cumulative_effects_dat$angina_new_bin ==1 | cumulative_effects_dat$heartfailure2yrs_bin == 1 | cumulative_effects_dat$heartattack_ever_bin == 1 | cumulative_effects_dat$heartattack_new_bin == 1] <-1
cumulative_effects_dat$CVD[cumulative_effects_dat$angina_new_bin ==0 & cumulative_effects_dat$heartfailure2yrs_bin == 0 & cumulative_effects_dat$heartattack_ever_bin == 0 & cumulative_effects_dat$heartattack_new_bin == 0] <-0

unique(cumulative_effects_dat$CVD)


cumulative_effects_dat$CVD_ever[cumulative_effects_dat$heartfailure2yrs_bin == 1 | cumulative_effects_dat$heartattack_ever_bin == 1 ] <-1
cumulative_effects_dat$CVD_ever[cumulative_effects_dat$heartfailure2yrs_bin == 0 & cumulative_effects_dat$heartattack_ever_bin == 0 ] <-0

unique(cumulative_effects_dat$CVD_ever)


cumulative_effects_dat$CVD_new[cumulative_effects_dat$angina_new_bin ==1 | cumulative_effects_dat$heartattack_new_bin == 1] <-1
cumulative_effects_dat$CVD_new[cumulative_effects_dat$angina_new_bin ==0 & cumulative_effects_dat$heartattack_new_bin == 0] <-0



unique(cumulative_effects_dat$CVD_new)


###### Adding variables to the main dataset:








cumulative_effects_dat$discrim_poorerservice_bin = case_when(cumulative_effects_dat$discrim_poorerservice == 1 ~ 1, 
                                                             cumulative_effects_dat$discrim_poorerservice == 2 ~ 1, 
                                                             cumulative_effects_dat$discrim_poorerservice == 3 ~ 1, 
                                                             cumulative_effects_dat$discrim_poorerservice == 4 ~ 1, 
                                                             cumulative_effects_dat$discrim_poorerservice == 5 ~ 0, 
                                                             cumulative_effects_dat$discrim_poorerservice == 6 ~ 0) 




cumulative_effects_dat$discrim_afraidothers_bin = case_when(cumulative_effects_dat$discrim_afraidothers == 1 ~ 1,
                                                            cumulative_effects_dat$discrim_afraidothers == 2 ~ 1, 
                                                            cumulative_effects_dat$discrim_afraidothers == 3 ~ 1, 
                                                            cumulative_effects_dat$discrim_afraidothers == 4 ~ 1, 
                                                            cumulative_effects_dat$discrim_afraidothers == 5 ~ 0, 
                                                            cumulative_effects_dat$discrim_afraidothers == 6 ~ 0,
                                                            cumulative_effects_dat$discrim_afraidothers == 0 ~ 0) 



cumulative_effects_dat$discrim_bin = case_when(cumulative_effects_dat$discrim_poorerservice_bin == 1 | cumulative_effects_dat$discrim_poorerservice_bin == 1 | cumulative_effects_dat$discrim_poorerservice_bin  == 1 | cumulative_effects_dat$discrim_poorerservice_bin == 1 | cumulative_effects_dat$discrim_afraidothers_bin == 1 | cumulative_effects_dat$discrim_poorerservice_bin == 1 ~ 1, 
                                               cumulative_effects_dat$discrim_poorerservice_bin == 0 & cumulative_effects_dat$discrim_poorerservice_bin == 0 & cumulative_effects_dat$discrim_poorerservice_bin  == 0 & cumulative_effects_dat$discrim_poorerservice_bin == 0 & cumulative_effects_dat$discrim_afraidothers_bin == 0 & cumulative_effects_dat$discrim_poorerservice_bin == 0 ~ 0) 



#outcome:  cumulative_effects_dat$diabetes_new_bin
#exposure: cumulative_effects_dat$discrim_bin
#time: cumulative_effects_dat$stop_new

###### Adding variables to the main dataset:
###### Adding variables to the main dataset:

cumulative_effects_dat$discrim_bin


cumulative_effects_dat$time_point = cumulative_effects_dat$start_new

cumulative_effects_dat$years = 2 * cumulative_effects_dat$start_new

cumulative_effects_dat$months = 12 * cumulative_effects_dat$years

cumulative_effects_dat$follow_up = cumulative_effects_dat$years


cumulative_effects_dat$diabetes_new_bin = case_when(cumulative_effects_dat$diabetes_new == 1 ~ 1, 
                                                    cumulative_effects_dat$diabetes_new == 0 ~ 0) 


cumulative_effects_dat$diabetes_new_bin_reversed = case_when(cumulative_effects_dat$diabetes_new_bin == 1 ~ 0, 
                                                             cumulative_effects_dat$diabetes_new_bin == 0 ~ 1) 


#outcome:  cumulative_effects_dat$diabetes_new_bin
#exposure: cumulative_effects_dat$discrim_bin
#time: cumulative_effects_dat$stop_new

###### Adding variables to the main dataset:
###### Adding variables to the main dataset:

unique(cumulative_effects_dat$discrim_poorerservice_bin_cat)


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


########

#### plot for All dataset: 

fit_all <- coxph(Surv(follow_up, diabetes_new_bin)~ discrim_poorerservice_bin + continious_age + wealth_noIRA + sex_1_2 + CVD , data = cumulative_effects_dat)
summary_all = summary(fit_all)

# coeffcients for discrim_poorerservice_bin: 
summary_all$coefficients[1,]
# exp (HR), and 95% CI: 
summary_all$conf.int[1,]
summary_all$waldtest
summary_all$logtest[1]
summary_all$n
summary_all$nevent


####
### output below: 
ALL_results_Model_4_non_interact = data.frame("Model_4_non_interact")
ALL_results_Model_4_non_interact$subset  = c("ALL")
ALL_results_Model_4_non_interact$coef  = c(summary_all$conf.int[1,1])
ALL_results_Model_4_non_interact$lower_CI = c(summary_all$conf.int[1,3])
ALL_results_Model_4_non_interact$upper_CI = c(summary_all$conf.int[1,4])
ALL_results_Model_4_non_interact$logtest = summary_all$logtest[1]
ALL_results_Model_4_non_interact$df = summary_all$logtest[2]
ALL_results_Model_4_non_interact$p_value = summary_all$logtest[3]

print(ALL_results_Model_4_non_interact)
########


########


########

#### plot for female dataset: 

fit_female <- coxph(Surv(follow_up, diabetes_new_bin)~ discrim_poorerservice_bin + continious_age + wealth_noIRA + sex_1_2 + CVD , data = data_female)
summary_female = summary(fit_female)

# coeffcients for discrim_poorerservice_bin: 
summary_female$coefficients[1,]
# exp (HR), and 95% CI: 
summary_female$conf.int[1,]
summary_female$waldtest
summary_female$logtest[1]
summary_female$n
summary_female$nevent


####
### output below: 
Female_results_Model_4_non_interact = data.frame("Model_4_non_interact")
Female_results_Model_4_non_interact$subset  = c("Female")
Female_results_Model_4_non_interact$coef  = c(summary_female$conf.int[1,1])
Female_results_Model_4_non_interact$lower_CI = c(summary_female$conf.int[1,3])
Female_results_Model_4_non_interact$upper_CI = c(summary_female$conf.int[1,4])
Female_results_Model_4_non_interact$logtest = summary_female$logtest[1]
Female_results_Model_4_non_interact$df = summary_female$logtest[2]
Female_results_Model_4_non_interact$p_value = summary_female$logtest[3]

print(Female_results_Model_4_non_interact)
########


########



fit_male <- coxph(Surv(follow_up, diabetes_new_bin)~ discrim_poorerservice_bin + continious_age + wealth_noIRA + sex_1_2 + CVD , data = data_male)
summary_male = summary(fit_male)

# coeffcients for discrim_poorerservice_bin: 
summary_male$coefficients[1,]
# exp (HR), and 95% CI: 
summary_male$conf.int[1,]
summary_male$waldtest
summary_male$logtest[1]
summary_male$n
summary_male$nevent


####
### output below: 
male_results_Model_4_non_interact = data.frame("Model_4_non_interact")
male_results_Model_4_non_interact$subset  = c("Female")
male_results_Model_4_non_interact$coef  = c(summary_male$conf.int[1,1])
male_results_Model_4_non_interact$lower_CI = c(summary_male$conf.int[1,3])
male_results_Model_4_non_interact$upper_CI = c(summary_male$conf.int[1,4])
male_results_Model_4_non_interact$logtest = summary_male$logtest[1]
male_results_Model_4_non_interact$df = summary_male$logtest[2]
male_results_Model_4_non_interact$p_value = summary_male$logtest[3]

print(male_results_Model_4_non_interact)
########

########

#### plot for race dataset: 

fit_race <- coxph(Surv(follow_up, diabetes_new_bin)~ discrim_poorerservice_bin + continious_age + wealth_noIRA + sex_1_2 + CVD , data = data_race)
summary_race = summary(fit_race)

# coeffcients for discrim_poorerservice_bin: 
summary_all$coefficients[1,]
# exp (HR), and 95% CI: 
summary_race$conf.int[1,]
summary_race$waldtest
summary_race$logtest[1]
summary_race$n
summary_race$nevent


####
### output below: 
race_results_Model_4_non_interact = data.frame("Model_4_non_interact")
race_results_Model_4_non_interact$subset  = c("race")
race_results_Model_4_non_interact$coef  = c(summary_race$conf.int[1,1])
race_results_Model_4_non_interact$lower_CI = c(summary_race$conf.int[1,3])
race_results_Model_4_non_interact$upper_CI = c(summary_race$conf.int[1,4])
race_results_Model_4_non_interact$logtest = summary_race$logtest[1]
race_results_Model_4_non_interact$df = summary_race$logtest[2]
race_results_Model_4_non_interact$p_value = summary_race$logtest[3]

print(race_results_Model_4_non_interact)

#write.csv(race_results_Model_4_non_interact, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/race_results_Model_4_non_interact.csv")


########
########
########
########

#### plot for BMI dataset: 

fit_BMI <- coxph(Surv(follow_up, diabetes_new_bin)~ discrim_poorerservice_bin + continious_age + wealth_noIRA + sex_1_2 + CVD , data = data_BMI)
summary_BMI = summary(fit_BMI)

# coeffcients for discrim_poorerservice_bin: 
summary_BMI$coefficients[1,]
# exp (HR), and 95% CI: 
summary_BMI$conf.int[1,]
summary_BMI$waldtest
summary_BMI$logtest[1]
summary_BMI$n
summary_BMI$nevent


####
### output below: 
BMI_results_Model_4_non_interact = data.frame("Model_4_non_interact")
BMI_results_Model_4_non_interact$subset  = c("BMI")
BMI_results_Model_4_non_interact$coef  = c(summary_BMI$conf.int[1,1])
BMI_results_Model_4_non_interact$lower_CI = c(summary_BMI$conf.int[1,3])
BMI_results_Model_4_non_interact$upper_CI = c(summary_BMI$conf.int[1,4])
BMI_results_Model_4_non_interact$logtest = summary_BMI$logtest[1]
BMI_results_Model_4_non_interact$df = summary_BMI$logtest[2]
BMI_results_Model_4_non_interact$p_value = summary_BMI$logtest[3]

print(BMI_results_Model_4_non_interact)

#write.csv(BMI_results_Model_4_non_interact, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/BMI_results_Model_4_non_interact.csv")

Model_4_non_interact_results = rbind(ALL_results_Model_4_non_interact, 
                                     Female_results_Model_4_non_interact, 
                                     male_results_Model_4_non_interact, 
                                     race_results_Model_4_non_interact, 
                                     BMI_results_Model_4_non_interact) 


write.csv(Model_4_non_interact_results, "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/Results/situations/Model_4_discrim_poorerservice_bin_results.csv")




numb_case_all = nobs(fit_all)
AIC_all = AIC(fit_all)
BIC_all = BIC(fit_all) 

numb_case_female = nobs(fit_female)
AIC_female = AIC(fit_female)
BIC_female = BIC(fit_female)

numb_cases_race = nobs(fit_race)
AIC_race = AIC(fit_race)
BIC_race = BIC(fit_race) 

numb_cases_BMI = nobs(fit_BMI)
AIC_BMI = AIC(fit_BMI)
BIC_BMI = BIC(fit_BMI) 



MODEL_4_diagnostics = rbind(numb_case_all, 
                            AIC_all, 
                            BIC_all, 
                            
                            numb_case_female,
                            AIC_female,
                            BIC_female,
                            
                            numb_cases_race,
                            AIC_race,
                            BIC_race,
                            
                            numb_cases_BMI,
                            AIC_BMI,
                            BIC_BMI)

#write.csv(Model_4_non_interact_results, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/Model_4_non_interact_results.csv")


#The powerSurvEpi package provides power and sample size calculation for survival analysis (with a focus towards epidemiological studies).



#The powerSurvEpi package provides power and sample size calculation for survival analysis (with a focus towards epidemiological studies).


n_all = fit_all$n

HR_all = c(summary_all$conf.int[1,1])

# example on page 803 of Palta M and Amini SB. (1985).
res.power_all <- power.stratify(
  n = n_all,
  timeUnit = 6,
  gVec = c(0.5, 0.5),
  PVec = c(0.5, 0.5),
  HR = HR_all,
  lambda0Vec = c(HR_all, HR_all),
  power.ini = 0.8,
  power.low = 0.001,
  power.upp = 0.999,
  alpha = 0.05)



#######

n_female = fit_female$n

HR_female = c(summary_female$conf.int[1,1])

# example on page 803 of Palta M and Amini SB. (1985).
res.power_female <- power.stratify(
  n = n_female,
  timeUnit = 6,
  gVec = c(0.5, 0.5),
  PVec = c(0.5, 0.5),
  HR = HR_female,
  lambda0Vec = c(HR_all, HR_all),
  power.ini = 0.8,
  power.low = 0.001,
  power.upp = 0.999,
  alpha = 0.05)


n_race = fit_race$n
HR_race = c(summary_race$conf.int[1,1])

# example on page 803 of Palta M and Amini SB. (1985).
res.power_race <- power.stratify(
  n = n_race,
  timeUnit = 6,
  gVec = c(0.5, 0.5),
  PVec = c(0.5, 0.5),
  HR = HR_race,
  lambda0Vec = c(HR_race, HR_race),
  power.ini = 0.8,
  power.low = 0.001,
  power.upp = 0.999,
  alpha = 0.05)




n_BMI = fit_BMI$n
HR_BMI  = c(summary_BMI $conf.int[1,1])

# example on page 803 of Palta M and Amini SB. (1985).
res.power_BMI <- power.stratify(
  n = n_BMI,
  timeUnit = 6,
  gVec = c(0.5, 0.5),
  PVec = c(0.5, 0.5),
  HR = HR_BMI,
  lambda0Vec = c(HR_race, HR_race),
  power.ini = 0.8,
  power.low = 0.001,
  power.upp = 0.999,
  alpha = 0.05)
res.power_race$power



Model_4_analysed_n = rbind(n_all,  
                           n_female, 
                           n_race, 
                           n_BMI) 


Model_4_numb_case = rbind(numb_case_all,   
                          numb_case_female, 
                          numb_cases_race,
                          numb_cases_BMI) 


Model_4_power = rbind(res.power_all$power,  
                      res.power_female$power, 
                      res.power_race$power, 
                      res.power_BMI$power) 


Model_4_power_results = data.frame(Model_4_analysed_n,
                                   Model_4_numb_case,
                                   Model_4_power)

#write.csv(Model_4_power_results, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/Model_4_power.csv")

