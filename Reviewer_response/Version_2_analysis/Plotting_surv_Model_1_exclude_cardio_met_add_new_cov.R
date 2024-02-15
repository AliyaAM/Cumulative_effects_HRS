
library("survival")
library("survminer")
library("dplyr")


current_directory = "/Users/k2147340/OneDrive - King's College London/Documents/"

#current_directory = "/Users/aliya/my_docs/"
#current_directory = "/Users/aliyaamirova/proj/Cumulative_effects_HRS"

OUTPUT_ROOT = paste(current_directory, "proj/Cumulative_effects_HRS/Reviewer_response/Version_2_analysis/RESULTS/", sep="")
SOURCE_ROOT = paste(current_directory, "proj/Cumulative_effects_HRS/Reviewer_response/Version_2_analysis/", sep="")
DATA_ROOT = paste(current_directory, "/ELSA_HRS/Data_analysis/", sep = "") 



#### "education_level", "national_origin_ousideUS_bin", "race_white"




###### Adding variables to the main dataset:


###### DATA:
# below is the entire dataset, not subseted to anyone: 


###### DATA:
# below is the entire dataset, not subseted to anyone: 

#cumulative_effects_dat = read.csv(paste(OUTPUT_ROOT, "all_waves_nodiabatbaseline_DIAB.csv", sep =""))

cumulative_effects_dat_initial <- read.csv(paste(OUTPUT_ROOT, "data_flow_chart_withoutbaselineCVD.csv", sep=""))

unique_ids_cumulative = unique(cumulative_effects_dat_initial$HHIDPN)
number_in_cumulative = length(unique_ids_cumulative)


data_compared_v2_table1 = read.csv(paste(OUTPUT_ROOT, "data_compared_v2_for_table_1.csv", sep = ""))

unique_ids_table1 = unique(data_compared_v2_table1$HHIDPN)
number_ids_table_1 = length(unique_ids_table1)

cumulative_effects_dat_initial <- subset(cumulative_effects_dat_initial, cumulative_effects_dat_initial$HHIDPN %in% unique_ids_table1)


length(unique(cumulative_effects_dat_initial$HHIDPN))

#exclude participants with cardiometabolic disease at baseline: 

cases_with_CVD = subset(cumulative_effects_dat_initial,  CVD_ever == 1 & start_new == 0)

exclude_ids = unique(cases_with_CVD$HHIDPN)


cumulative_effects_dat <- subset(cumulative_effects_dat_initial,  !(HHIDPN %in% exclude_ids))


unique(cumulative_effects_dat$CVD_ever)
unique(cumulative_effects_dat_initial$start_new)
nrow(cumulative_effects_dat)
nrow(cumulative_effects_dat_initial)


#### There were only 27 people who disclosed their national origin in our subsample. 

unique(cumulative_effects_dat_initial$national_origin_ousideUS_bin)
unique(cumulative_effects_dat$national_origin_ousideUS_bin)



cumulative_effects_dat$discrim_harassed_bin = case_when(cumulative_effects_dat$discrim_harassed == 1 ~ 1, 
                                                        cumulative_effects_dat$discrim_harassed == 2 ~ 1, 
                                                        cumulative_effects_dat$discrim_harassed == 3 ~ 1, 
                                                        cumulative_effects_dat$discrim_harassed == 4 ~ 1, 
                                                        cumulative_effects_dat$discrim_harassed == 5 ~ 0, 
                                                        cumulative_effects_dat$discrim_harassed == 6 ~ 0,
                                                        cumulative_effects_dat$discrim_harassed == 0 ~ 0) 



cumulative_effects_dat$discrim_lessrespect_bin = case_when(cumulative_effects_dat$discrim_lessrespect == 1 ~ 1, 
                                                           cumulative_effects_dat$discrim_lessrespect == 2 ~ 1, 
                                                           cumulative_effects_dat$discrim_lessrespect == 3 ~ 1, 
                                                           cumulative_effects_dat$discrim_lessrespect == 4 ~ 1, 
                                                           cumulative_effects_dat$discrim_lessrespect == 5 ~ 0, 
                                                           cumulative_effects_dat$discrim_lessrespect == 6 ~ 0,
                                                           cumulative_effects_dat$discrim_lessrespect == 0 ~ 0) 



cumulative_effects_dat$discrim_medical_bin = case_when(cumulative_effects_dat$discrim_medical == 1 ~ 1, 
                                                       cumulative_effects_dat$discrim_medical == 2 ~ 1, 
                                                       cumulative_effects_dat$discrim_medical == 3 ~ 1, 
                                                       cumulative_effects_dat$discrim_medical == 4 ~ 1, 
                                                       cumulative_effects_dat$discrim_medical == 5 ~ 0, 
                                                       cumulative_effects_dat$discrim_medical == 6 ~ 0,
                                                       cumulative_effects_dat$discrim_medical == 0 ~ 0) 





cumulative_effects_dat$discrim_notclever_bin = case_when(cumulative_effects_dat$discrim_notclever == 1 ~ 1, 
                                                         cumulative_effects_dat$discrim_notclever == 2 ~ 1, 
                                                         cumulative_effects_dat$discrim_notclever == 3 ~ 1, 
                                                         cumulative_effects_dat$discrim_notclever == 4 ~ 1, 
                                                         cumulative_effects_dat$discrim_notclever == 5 ~ 0, 
                                                         cumulative_effects_dat$discrim_notclever == 6 ~ 0,
                                                         cumulative_effects_dat$discrim_notclever == 0 ~ 0) 






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



cumulative_effects_dat$discrim_bin = case_when(cumulative_effects_dat$discrim_harassed_bin == 1 | cumulative_effects_dat$discrim_lessrespect_bin == 1 | cumulative_effects_dat$discrim_medical_bin  == 1 | cumulative_effects_dat$discrim_notclever_bin == 1 | cumulative_effects_dat$discrim_afraidothers_bin == 1 | cumulative_effects_dat$discrim_poorerservice_bin == 1 ~ 1, 
                                               cumulative_effects_dat$discrim_harassed_bin == 0 & cumulative_effects_dat$discrim_lessrespect_bin == 0 & cumulative_effects_dat$discrim_medical_bin  == 0 & cumulative_effects_dat$discrim_notclever_bin == 0 & cumulative_effects_dat$discrim_afraidothers_bin == 0 & cumulative_effects_dat$discrim_poorerservice_bin == 0 ~ 0) 



#outcome:  cumulative_effects_dat$diabetes_new_bin
#exposure: cumulative_effects_dat$discrim_bin
#time: cumulative_effects_dat$stop_new

###### Adding variables to the main dataset:
###### Adding variables to the main dataset:

cumulative_effects_dat$discrim_bin

cumulative_effects_dat$discrimination = cumulative_effects_dat$discrim_bin

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

unique(cumulative_effects_dat$discrimination_cat)


#1 = 2 year 
#2 = 4 years 
#3 = 6 years 

#coxph(Surv(t1, t2, stat) ∼ (age + surgery)* transplant) – time dependent covariates.

unique(cumulative_effects_dat$timepoints_indiv)
unique(cumulative_effects_dat$start_new)

cumulative_effects_dat$diabetes_new_bin_reversed = case_when(cumulative_effects_dat$diabetes_new_bin == 1 ~ 0, 
                                                             cumulative_effects_dat$diabetes_new_bin == 0 ~ 1) 



#cumulative_effects_dat$sex_1_2
#cumulative_effects_dat$race_white
#cumulative_effects_dat$race_white
#cumulative_effects_dat$national_origin_ousideUS_bin
#cumulative_effects_dat$religion_bin
#cumulative_effects_dat$assessed_BMI


#### plot for the entire dataset:
#survfit.coxph

#cfit <- coxph(Surv(futime, death) ~ sex + age*hgb, data=mgus2)

fit <- coxph(Surv(follow_up, diabetes_new_bin)~ discrimination + continious_age + wealth_noIRA + sex_1_2 + race_white, data = cumulative_effects_dat)
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
All_results_Model_1 = data.frame("Model_1")
All_results_Model_1$subset  = c("All")
All_results_Model_1$coef  = c(summary_all$conf.int[1,1])
All_results_Model_1$lower_CI = c(summary_all$conf.int[1,3])
All_results_Model_1$upper_CI = c(summary_all$conf.int[1,4])
All_results_Model_1$logtest = summary_all$logtest[1]
All_results_Model_1$df = summary_all$logtest[2]
All_results_Model_1$p_value = summary_all$logtest[3]

print(All_results_Model_1)

write.csv(All_results_Model_1, paste(OUTPUT_ROOT, "Model_1_nocardiometdis_race_educat.csv"))

#Model 1: age and sex, wealth  [basis adjustment]
##############################  Model_1 = c("continious_age", "wealth_noIRA", "sex_1_2",   "education_level", "national_origin_ousideUS_bin", "race_white")
#Model 2: age, sex, wealth, BMI, hypertension  [basic adjustment + diabetes risk factors]
##############################  Model_2 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "hypertension_new_bin",   "education_level", "national_origin_ousideUS_bin", "race_white")
#Model_2 = c("continious_age", "wealth_noIRA", "sex_1_2",  "hypertension_new_bin")

#Model_2 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI")

#Model 3: age, sex, wealth, physical activity, smoking (yes/no), and alcohol (days/week) [basic adjustment + health behaviours]
##############################  Model_3 = c("continious_age", "wealth_noIRA", "sex_1_2",   "vigarious_physical_activity_new",   "education_level", "national_origin_ousideUS_bin", "race_white" )

#Model_3 = c("continious_age", "wealth_noIRA", "sex_1_2")

#Model 4: age, sex, wealth, CVD  [basic adjustment + CVD most common diabetes co_morbidity]
##############################  Model_4 = c("continious_age", "wealth_noIRA", "sex_1_2", "CVD",   "education_level", "national_origin_ousideUS_bin", "race_white" )
#Model 5: age, sex, wealth, depression  [basic adjustment + depression best researched psychosocial factor in diabetes ]
##############################  Model_5 = c("continious_age","wealth_noIRA", "sex_1_2", "checklist_depression_bin",   "education_level", "national_origin_ousideUS_bin", "race_white")



############################## Model 2: 
############################## "education_level", "national_origin_ousideUS_bin", "race_white"
##############################

fit_Model_2 <- coxph(Surv(follow_up, diabetes_new_bin) ~ discrimination + continious_age + wealth_noIRA + sex_1_2 + assessed_BMI + hypertension_new_bin  + race_white, data = cumulative_effects_dat)
summary_all_model_2 = summary(fit_Model_2)

# coeffcients for discrimination: 
summary_all_model_2$coefficients[1,]
# exp (HR), and 95% CI: 
summary_all_model_2$conf.int[1,]
summary_all_model_2$waldtest
summary_all_model_2$logtest[1]
summary_all_model_2$n
summary_all_model_2$nevent


####
### output below: 
All_results_Model_2 = data.frame("Model_2")
All_results_Model_2$subset  = c("All")
All_results_Model_2$coef  = c(summary_all_model_2$conf.int[1,1])
All_results_Model_2$lower_CI = c(summary_all_model_2$conf.int[1,3])
All_results_Model_2$upper_CI = c(summary_all_model_2$conf.int[1,4])
All_results_Model_2$logtest = summary_all_model_2$logtest[1]
All_results_Model_2$df = summary_all_model_2$logtest[2]
All_results_Model_2$p_value = summary_all_model_2$logtest[3]

print(All_results_Model_2)

write.csv(All_results_Model_2, paste(OUTPUT_ROOT, "Model_2_nocardiometdis_race_educat.csv"))


############################## Model 3: 
############################## "education_level", "national_origin_ousideUS_bin", "race_white"
##############################   Model_3 = c("continious_age", "wealth_noIRA", "sex_1_2",   "vigarious_physical_activity_new",   "education_level", "national_origin_ousideUS_bin", "race_white" )


fit_Model_3 <- coxph(Surv(follow_up, diabetes_new_bin) ~ discrimination + continious_age + wealth_noIRA + sex_1_2 +  vigarious_physical_activity_new  + race_white, data = cumulative_effects_dat)
summary_all_model_3 = summary(fit_Model_3)

# coeffcients for discrimination: 
summary_all_model_3$coefficients[1,]
# exp (HR), and 95% CI: 
summary_all_model_3$conf.int[1,]
summary_all_model_3$waldtest
summary_all_model_3$logtest[1]
summary_all_model_3$n
summary_all_model_3$nevent


####
### output below: 
All_results_Model_3 = data.frame("Model_3")
All_results_Model_3$subset  = c("All")
All_results_Model_3$coef  = c(summary_all_model_3$conf.int[1,1])
All_results_Model_3$lower_CI = c(summary_all_model_3$conf.int[1,3])
All_results_Model_3$upper_CI = c(summary_all_model_3$conf.int[1,4])
All_results_Model_3$logtest = summary_all_model_3$logtest[1]
All_results_Model_3$df = summary_all_model_3$logtest[2]
All_results_Model_3$p_value = summary_all_model_3$logtest[3]

print(All_results_Model_3)

write.csv(All_results_Model_3, paste(OUTPUT_ROOT, "Model_3_nocardiometdis_race_educat.csv"))



print("FULL LIST OF HEALTH BEHAVIOURS IS BELOW: ")
print("FULL LIST OF HEALTH BEHAVIOURS IS BELOW: ")
print("FULL LIST OF HEALTH BEHAVIOURS IS BELOW: ")
print("FULL LIST OF HEALTH BEHAVIOURS IS BELOW: ")
print("FULL LIST OF HEALTH BEHAVIOURS IS BELOW: ")
print("FULL LIST OF HEALTH BEHAVIOURS IS BELOW: ")


############################## Model 3_full: 
############################## "education_level", "national_origin_ousideUS_bin", "race_white"
##############################   Model_3 = c("continious_age", "wealth_noIRA", "sex_1_2",   "vigarious_physical_activity_new",   "education_level", "national_origin_ousideUS_bin", "race_white" )
#### we already dropped all NAs in this var so, the below is true:
#### check an error in the data cleaning file

if (any(is.na(cumulative_effects_dat$smokes_now_bin))) {
  cumulative_effects_dat$smokes_now_bin[is.na(cumulative_effects_dat$smokes_now_bin)] <- 0
}

unique(cumulative_effects_dat$smokes_now_bin) 

unique(cumulative_effects_dat$alcohol_days_week_new)

fit_Model_3_full <- coxph(Surv(follow_up, diabetes_new_bin) ~ discrimination + continious_age + wealth_noIRA + sex_1_2 +  vigarious_physical_activity_new + smokes_now_bin  + + alcohol_days_week_new +  race_white, data = cumulative_effects_dat)
summary_all_Model_3_full = summary(fit_Model_3)

# coeffcients for discrimination: 
summary_all_Model_3_full$coefficients[1,]
# exp (HR), and 95% CI: 
summary_all_Model_3_full$conf.int[1,]
summary_all_Model_3_full$waldtest
summary_all_Model_3_full$logtest[1]
summary_all_Model_3_full$n
summary_all_Model_3_full$nevent


####
### output below: 
All_results_Model_3_full = data.frame("Model_3_full")
All_results_Model_3_full$subset  = c("All")
All_results_Model_3_full$coef  = c(summary_all_Model_3_full$conf.int[1,1])
All_results_Model_3_full$lower_CI = c(summary_all_Model_3_full$conf.int[1,3])
All_results_Model_3_full$upper_CI = c(summary_all_Model_3_full$conf.int[1,4])
All_results_Model_3_full$logtest = summary_all_Model_3_full$logtest[1]
All_results_Model_3_full$df = summary_all_Model_3_full$logtest[2]
All_results_Model_3_full$p_value = summary_all_Model_3_full$logtest[3]

print(All_results_Model_3_full)

write.csv(All_results_Model_3_full, paste(OUTPUT_ROOT, "Model_3_full_nocardiometdis_race_educat.csv"))



###############
###############
############### Model 4: no covariate such as CVD since we are excuding cases on the basis of this var: instead model 4 is model 5 now which is "continious_age","wealth_noIRA", "sex_1_2", "checklist_depression_bin",   "education_level", "national_origin_ousideUS_bin", "race_white"


fit_Model_4 <- coxph(Surv(follow_up, diabetes_new_bin)~ discrimination + continious_age + wealth_noIRA + sex_1_2 + checklist_depression_bin + race_white, data = cumulative_effects_dat)
summary_all_Model_4 = summary(fit_Model_4)

# coeffcients for discrimination: 
summary_all_Model_4$coefficients[1,]
# exp (HR), and 95% CI: 
summary_all_Model_4$conf.int[1,]
summary_all_Model_4$waldtest
summary_all_Model_4$logtest[1]
summary_all_Model_4$n
summary_all_Model_4$nevent


####
### output below: 
All_results_Model_4 = data.frame("Model_4")
All_results_Model_4$subset  = c("All")
All_results_Model_4$coef  = c(summary_all_Model_4$conf.int[1,1])
All_results_Model_4$lower_CI = c(summary_all_Model_4$conf.int[1,3])
All_results_Model_4$upper_CI = c(summary_all_Model_4$conf.int[1,4])
All_results_Model_4$logtest = summary_all_Model_4$logtest[1]
All_results_Model_4$df = summary_all_Model_4$logtest[2]
All_results_Model_4$p_value = summary_all_Model_4$logtest[3]

print(All_results_Model_4)

write.csv(All_results_Model_4, paste(OUTPUT_ROOT, "Model_4_nocardiometdis_race_educat.csv"))



