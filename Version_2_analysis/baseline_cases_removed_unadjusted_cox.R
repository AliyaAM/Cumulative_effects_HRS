
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
Unadjusted_nosex = c("continious_age", "wealth_noIRA")
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

cumulative_effects_dat = read.csv("/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/DATA_FOR_PLOT/all_waves_nodiabatbaseline_DIAB.csv")
#cumulative_effects_dat = read.csv("/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/DATA_FOR_PLOT/all_waves_nodiabatbaseline_DIAB.csv")
#cumulative_effects_dat = read.csv("/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/DATA_FOR_PLOT/all_waves_nodiab_at_two_first_waves_DIAB_discrim_recoded.csv")
#cumulative_effects_dat = read.csv("/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/DATA_FOR_PLOT/all_waves_nodiabatbaseline_DIAB_discrim_recoded.csv")

###### Adding variables to the main dataset:

#outcome:  cumulative_effects_dat$diabetes_new_bin
#exposure: cumulative_effects_dat$discrim_bin
#time: cumulative_effects_dat$stop_new

###### Adding variables to the main dataset:
###### Adding variables to the main dataset:

cumulative_effects_dat$discrim_bin
unique(cumulative_effects_dat$discrimination_cat)

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
#survfit.coxph

#cfit <- coxph(Surv(futime, death) ~ sex + age*hgb, data=mgus2)

fit <- coxph(Surv(years, diabetes_new_bin)~ discrimination, data = cumulative_effects_dat)
summary_all = summary(fit)

# coeffcients for discrimination: 
summary_all$coefficients[1,]
# exp (HR), and 95% CI: 
summary_all$conf.int[1,]
summary_all$waldtest
summary_all$logtest[1]
summary_all$n
summary_all$nevent


####
### output below: 
All_results_Unadjusted = data.frame("Unadjusted")
All_results_Unadjusted$subset  = c("All")
All_results_Unadjusted$coef  = c(summary_all$conf.int[1,1])
All_results_Unadjusted$lower_CI = c(summary_all$conf.int[1,3])
All_results_Unadjusted$upper_CI = c(summary_all$conf.int[1,4])
All_results_Unadjusted$logtest = summary_all$logtest[1]
All_results_Unadjusted$df = summary_all$logtest[2]
All_results_Unadjusted$p_value = summary_all$logtest[3]

print(All_results_Unadjusted)

#write.csv(All_results_Unadjusted, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/All_results_Unadjusted.csv")



########
########
########
########

#### plot for female dataset: 

fit_female <- coxph(Surv(years, diabetes_new_bin)~ discrimination, data = data_female)
summary_female = summary(fit_female)

# coeffcients for discrimination: 
summary_all$coefficients[1,]
# exp (HR), and 95% CI: 
summary_female$conf.int[1,]
summary_female$waldtest
summary_female$logtest[1]
summary_female$n
summary_female$nevent


####
### output below: 
Female_results_Unadjusted = data.frame("Unadjusted")
Female_results_Unadjusted$subset  = c("Female")
Female_results_Unadjusted$coef  = c(summary_female$conf.int[1,1])
Female_results_Unadjusted$lower_CI = c(summary_female$conf.int[1,3])
Female_results_Unadjusted$upper_CI = c(summary_female$conf.int[1,4])
Female_results_Unadjusted$logtest = summary_female$logtest[1]
Female_results_Unadjusted$df = summary_female$logtest[2]
Female_results_Unadjusted$p_value = summary_female$logtest[3]

print(Female_results_Unadjusted)

#write.csv(Female_results_Unadjusted, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/Female_results_Unadjusted.csv")



### output below: 


########
########
########
########

#### plot for male dataset: 


fit_male <- coxph(Surv(years, diabetes_new_bin)~ discrimination, data = data_male)
summary_male = summary(fit_male)

# coeffcients for discrimination: 
summary_all$coefficients[1,]
# exp (HR), and 95% CI: 
summary_male$conf.int[1,]
summary_male$waldtest
summary_male$logtest[1]
summary_male$n
summary_male$nevent


####
### output below: 
male_results_Unadjusted = data.frame("Unadjusted")
male_results_Unadjusted$subset  = c("male")
male_results_Unadjusted$coef  = c(summary_male$conf.int[1,1])
male_results_Unadjusted$lower_CI = c(summary_male$conf.int[1,3])
male_results_Unadjusted$upper_CI = c(summary_male$conf.int[1,4])
male_results_Unadjusted$logtest = summary_male$logtest[1]
male_results_Unadjusted$df = summary_male$logtest[2]
male_results_Unadjusted$p_value = summary_male$logtest[3]

print(male_results_Unadjusted)

#write.csv(male_results_Unadjusted, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/male_results_Unadjusted.csv")



########
########
########
########

#### plot for race dataset: 

fit_race <- coxph(Surv(years, diabetes_new_bin)~ discrimination, data = data_race)
summary_race = summary(fit_race)

# coeffcients for discrimination: 
summary_all$coefficients[1,]
# exp (HR), and 95% CI: 
summary_race$conf.int[1,]
summary_race$waldtest
summary_race$logtest[1]
summary_race$n
summary_race$nevent


####
### output below: 
race_results_Unadjusted = data.frame("Unadjusted")
race_results_Unadjusted$subset  = c("race")
race_results_Unadjusted$coef  = c(summary_race$conf.int[1,1])
race_results_Unadjusted$lower_CI = c(summary_race$conf.int[1,3])
race_results_Unadjusted$upper_CI = c(summary_race$conf.int[1,4])
race_results_Unadjusted$logtest = summary_race$logtest[1]
race_results_Unadjusted$df = summary_race$logtest[2]
race_results_Unadjusted$p_value = summary_race$logtest[3]

print(race_results_Unadjusted)

#write.csv(race_results_Unadjusted, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/race_results_Unadjusted.csv")


########
########
########
########

#### plot for BMI dataset: 

fit_BMI <- coxph(Surv(years, diabetes_new_bin)~ discrimination, data = data_BMI)
summary_BMI = summary(fit_BMI)

# coeffcients for discrimination: 
summary_BMI$coefficients[1,]
# exp (HR), and 95% CI: 
summary_BMI$conf.int[1,]
summary_BMI$waldtest
summary_BMI$logtest[1]
summary_BMI$n
summary_BMI$nevent


####
### output below: 
BMI_results_Unadjusted = data.frame("Unadjusted")
BMI_results_Unadjusted$subset  = c("BMI")
BMI_results_Unadjusted$coef  = c(summary_BMI$conf.int[1,1])
BMI_results_Unadjusted$lower_CI = c(summary_BMI$conf.int[1,3])
BMI_results_Unadjusted$upper_CI = c(summary_BMI$conf.int[1,4])
BMI_results_Unadjusted$logtest = summary_BMI$logtest[1]
BMI_results_Unadjusted$df = summary_BMI$logtest[2]
BMI_results_Unadjusted$p_value = summary_BMI$logtest[3]

print(BMI_results_Unadjusted)

#write.csv(BMI_results_Unadjusted, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/BMI_results_Unadjusted.csv")

Unadjusted_results = rbind(All_results_Unadjusted, 
                           Female_results_Unadjusted, 
                           male_results_Unadjusted, 
                           race_results_Unadjusted, 
                           BMI_results_Unadjusted) 

print(Unadjusted_results)
#write.csv(Unadjusted_results, "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/Unadjusted_results_nobaseline_discrim_bin.csv")
