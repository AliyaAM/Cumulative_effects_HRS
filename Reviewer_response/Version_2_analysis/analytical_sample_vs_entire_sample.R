
library("dplyr")
library("tidyr")
library("bain")
library("ggplot2")
library("stats")

current_directory = "/Users/k2147340/OneDrive - King's College London/Documents/"
data_directory = "/Users/k2147340/OneDrive - King's College London/Desktop/"
#C:\Users\k2147340\OneDrive - King's College London\Desktop

#current_directory = "/Users/aliya/my_docs/"
#current_directory = "/Users/aliyaamirova/proj/Cumulative_effects_HRS"

OUTPUT_ROOT = paste(current_directory, "proj/Cumulative_effects_HRS/Reviewer_response/Version_2_analysis/RESULTS/", sep="")
SOURCE_ROOT = paste(current_directory, "proj/Cumulative_effects_HRS/Reviewer_response/Version_2_analysis/", sep="")
DATA_ROOT = paste(current_directory, "/ELSA_HRS/Data_analysis/", sep = "") 
DATA2_ROOT = paste(current_directory, "proj/Cumulative_effects_HRS/Reviewer_response/Version_2_analysis/RESULTS/", sep="")



source((paste(SOURCE_ROOT, "participant_char_function.R", sep="")))


# ###### DATA:
# below is the entire dataset, not subseted to anyone:

cumulative_effects_dat_initial = read.csv(paste(data_directory, "data_flow_chart_withoutbaselineCVD.csv", sep=""))


cumulative_effects_dat_initial$RAHISPAN


#cumulative_effects_dat_initial = read.csv(paste(OUTPUT_ROOT, "all_waves_nodiabatbaseline_DIAB.csv", sep =""))
nrow(cumulative_effects_dat_initial)


#exclude participants with cardiometabolic disease at baseline:

cases_with_CVD = subset(cumulative_effects_dat_initial,  CVD_ever == 1 & start_new == 0)

exclude_ids = unique(cases_with_CVD$HHIDPN)



analytical_sample_COX <- subset(cumulative_effects_dat_initial,  !(HHIDPN %in% exclude_ids))




#####
# check if the rows with NAs in dicrimin_bin were excluded at each timepoint ANSWER: for cox the NAs in discrim_bin at baseline were excluded and for WCE NAs in discrim_bin at any point were excluded. 
# check: Rows with missing data (NAs) in the discrimination variable (yes/no) at any time point (2008-2018) were excluded from the analyses for the WCE analysis BUT not for the COX model.  
#Check analytical sample for Cox vs WCE (should be different as different NAs are excluded): 
# ANSWER: individual-timepoints rows in COX = 89691: individual-timepoints rows. (indvidual_1 & baseline is the first row, individual_1 and follow-up is second row etc)
# ANSWER: individual-timepoints rows in BMI = 10199: individual-timepoints rows. (indvidual_1 & baseline is the first row, individual_1 and follow-up is second row etc)
#ANSWER: analytical sample for COX: 22731 individuals 

COX_unique_id = unique(analytical_sample_COX$HHIDPN)
#COX_unique_id_n = length(COX_unique_id) 

#######
#WCE_unique_id_n = length(WCE_unique_id)

# recode into single var  discrim_bin


#create wealth quantile variable 

analytical_sample_COX$wealth_noIRA

quantiles_first_wealth_noIRA = quantile(analytical_sample_COX$wealth_noIRA,
                                        probs = seq(0, 0.25), na.rm = TRUE)


quantiles_second_wealth_noIRA = quantile(analytical_sample_COX$wealth_noIRA,
                                         probs = seq(0.50, 0.75), na.rm = TRUE)

quantiles_third_wealth_noIRA = quantile(analytical_sample_COX$wealth_noIRA,
                                        probs = seq(0.25, 0.5), na.rm = TRUE)


quantiles_fourth_wealth_noIRA = quantile(analytical_sample_COX$wealth_noIRA,
                                         probs = seq(0.75, 1), na.rm = TRUE)



q1_wealth_noIRA_baseline_all = subset(analytical_sample_COX, analytical_sample_COX$wealth_noIRA > quantiles_first_wealth_noIRA & analytical_sample_COX$wealth_noIRA <= quantiles_second_wealth_noIRA)
n_q1 = nrow(q1_wealth_noIRA_baseline_all)


q2_wealth_noIRA_baseline_all = subset(analytical_sample_COX, analytical_sample_COX$wealth_noIRA > quantiles_third_wealth_noIRA & analytical_sample_COX$wealth_noIRA <= quantiles_second_wealth_noIRA)
n_q2 = nrow(q2_wealth_noIRA_baseline_all)



q3_wealth_noIRA_baseline_all = subset(analytical_sample_COX, analytical_sample_COX$wealth_noIRA > quantiles_second_wealth_noIRA & analytical_sample_COX$wealth_noIRA <= quantiles_fourth_wealth_noIRA)
n_q3 = nrow(q3_wealth_noIRA_baseline_all)


q4_wealth_noIRA_baseline_all = subset(analytical_sample_COX, analytical_sample_COX$wealth_noIRA > quantiles_fourth_wealth_noIRA)
n_q4 = nrow(q4_wealth_noIRA_baseline_all)

# create a new variable for CVD 

analytical_sample_COX$CVD[analytical_sample_COX$angina_new_bin ==1 | analytical_sample_COX$heartfailure2yrs_bin == 1 | analytical_sample_COX$heartattack_ever_bin == 1 | analytical_sample_COX$heartattack_new_bin == 1] <-1
analytical_sample_COX$CVD[analytical_sample_COX$angina_new_bin ==0 & analytical_sample_COX$heartfailure2yrs_bin == 0 & analytical_sample_COX$heartattack_ever_bin == 0 & analytical_sample_COX$heartattack_new_bin == 0] <-0

unique(analytical_sample_COX$CVD)

analytical_sample_COX$CVD_ever[analytical_sample_COX$heartfailure2yrs_bin == 1 | analytical_sample_COX$heartattack_ever_bin == 1 ] <-1
analytical_sample_COX$CVD_ever[analytical_sample_COX$heartfailure2yrs_bin == 0 & analytical_sample_COX$heartattack_ever_bin == 0 ] <-0

unique(analytical_sample_COX$CVD_ever)


analytical_sample_COX$CVD_new[analytical_sample_COX$angina_new_bin ==1 | analytical_sample_COX$heartattack_new_bin == 1] <-1
analytical_sample_COX$CVD_new[analytical_sample_COX$angina_new_bin ==0 & analytical_sample_COX$heartattack_new_bin == 0] <-0



unique(analytical_sample_COX$CVD_ever)


analytical_sample_COX$discrim_harassed_bin = case_when(analytical_sample_COX$discrim_harassed == 1 ~ 1, 
                                                       analytical_sample_COX$discrim_harassed == 2 ~ 1, 
                                                       analytical_sample_COX$discrim_harassed == 3 ~ 1, 
                                                       analytical_sample_COX$discrim_harassed == 4 ~ 1, 
                                                       analytical_sample_COX$discrim_harassed == 5 ~ 0, 
                                                       analytical_sample_COX$discrim_harassed == 6 ~ 0,
                                                       analytical_sample_COX$discrim_harassed == 0 ~ 0) 



analytical_sample_COX$discrim_lessrespect_bin = case_when(analytical_sample_COX$discrim_lessrespect == 1 ~ 1, 
                                                          analytical_sample_COX$discrim_lessrespect == 2 ~ 1, 
                                                          analytical_sample_COX$discrim_lessrespect == 3 ~ 1, 
                                                          analytical_sample_COX$discrim_lessrespect == 4 ~ 1, 
                                                          analytical_sample_COX$discrim_lessrespect == 5 ~ 0, 
                                                          analytical_sample_COX$discrim_lessrespect == 6 ~ 0,
                                                          analytical_sample_COX$discrim_lessrespect == 0 ~ 0) 



analytical_sample_COX$discrim_medical_bin = case_when(analytical_sample_COX$discrim_medical == 1 ~ 1, 
                                                      analytical_sample_COX$discrim_medical == 2 ~ 1, 
                                                      analytical_sample_COX$discrim_medical == 3 ~ 1, 
                                                      analytical_sample_COX$discrim_medical == 4 ~ 1, 
                                                      analytical_sample_COX$discrim_medical == 5 ~ 0, 
                                                      analytical_sample_COX$discrim_medical == 6 ~ 0,
                                                      analytical_sample_COX$discrim_medical == 0 ~ 0) 





analytical_sample_COX$discrim_notclever_bin = case_when(analytical_sample_COX$discrim_notclever == 1 ~ 1, 
                                                        analytical_sample_COX$discrim_notclever == 2 ~ 1, 
                                                        analytical_sample_COX$discrim_notclever == 3 ~ 1, 
                                                        analytical_sample_COX$discrim_notclever == 4 ~ 1, 
                                                        analytical_sample_COX$discrim_notclever == 5 ~ 0, 
                                                        analytical_sample_COX$discrim_notclever == 6 ~ 0,
                                                        analytical_sample_COX$discrim_notclever == 0 ~ 0) 






analytical_sample_COX$discrim_poorerservice_bin = case_when(analytical_sample_COX$discrim_poorerservice == 1 ~ 1, 
                                                            analytical_sample_COX$discrim_poorerservice == 2 ~ 1, 
                                                            analytical_sample_COX$discrim_poorerservice == 3 ~ 1, 
                                                            analytical_sample_COX$discrim_poorerservice == 4 ~ 1, 
                                                            analytical_sample_COX$discrim_poorerservice == 5 ~ 0, 
                                                            analytical_sample_COX$discrim_poorerservice == 6 ~ 0) 




analytical_sample_COX$discrim_afraidothers_bin = case_when(analytical_sample_COX$discrim_afraidothers == 1 ~ 1,
                                                           analytical_sample_COX$discrim_afraidothers == 2 ~ 1, 
                                                           analytical_sample_COX$discrim_afraidothers == 3 ~ 1, 
                                                           analytical_sample_COX$discrim_afraidothers == 4 ~ 1, 
                                                           analytical_sample_COX$discrim_afraidothers == 5 ~ 0, 
                                                           analytical_sample_COX$discrim_afraidothers == 6 ~ 0,
                                                           analytical_sample_COX$discrim_afraidothers == 0 ~ 0) 



analytical_sample_COX$discrim_bin = case_when(analytical_sample_COX$discrim_harassed_bin == 1 | analytical_sample_COX$discrim_lessrespect_bin == 1 | analytical_sample_COX$discrim_medical_bin  == 1 | analytical_sample_COX$discrim_notclever_bin == 1 | analytical_sample_COX$discrim_afraidothers_bin == 1 | analytical_sample_COX$discrim_poorerservice_bin == 1 ~ 1, 
                                              analytical_sample_COX$discrim_harassed_bin == 0 & analytical_sample_COX$discrim_lessrespect_bin == 0 & analytical_sample_COX$discrim_medical_bin  == 0 & analytical_sample_COX$discrim_notclever_bin == 0 & analytical_sample_COX$discrim_afraidothers_bin == 0 & analytical_sample_COX$discrim_poorerservice_bin == 0 ~ 0) 



unique(analytical_sample_COX$discrim_bin)






#make table 2 (The results of the Cox regression run on the unrestricted sample)

#Unadjusted_cox = read.csv("/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/Cox_results_correct_13July_2022/Unadjusted_results_nobaseline_discrim_bin.csv")
#Model_1_cox = read.csv("/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/Cox_results_correct_13July_2022/Model_1_results_no_diab_at_baseline_discrim_bin.csv")
#Model_2_cox =read.csv("/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/Cox_results_correct_13July_2022/Model_2_non_interact_results_no_diab_at_baseline_discrim_bin.csv")
#Model_3_cox =read.csv("/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/Cox_results_correct_13July_2022/Model_3_non_interact_results.csv")
#Model_4_cox =read.csv("/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/Cox_results_correct_13July_2022/Model_4_non_interact_results.csv")
#Model_5_cox =read.csv("/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/Cox_results_correct_13July_2022/Model_5_non_interact_results.csv")

#RESULTS_COX = cbind(Unadjusted_cox, 
                    #Model_1_cox, 
                    #Model_2_cox, 
                    #Model_3_cox, 
                    #Model_4_cox, 
                    #Model_5_cox)


#RESULTS_COX$coef = round(RESULTS_COX$coef, 2)

#RESULTS_COX$lower_CI = round(RESULTS_COX$lower_CI, 2)

#RESULTS_COX$upper_CI = round(RESULTS_COX$upper_CI, 2)

#RESULTS_COX$p_value = round(RESULTS_COX$p_value, 4)

#write.csv(RESULTS_COX, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/RESULTS_COX.csv")



#MODELS_diagnostics = cbind(MODEL_unadjusted_diagnostics, 
                           # MODEL_1_diagnostics, 
                            #MODEL_2_diagnostics, 
                           # MODEL_3_diagnostics, 
                           # MODEL_4_diagnostics, 
                           # MODEL_5_diagnostics) 

#write.csv(MODELS_diagnostics, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/MODELS_diagnostics.csv")



#####

##### 
##### #####  #####  ##### #####    participant characteristics ##### #####  #####  ##### ##### 



#Subset datasets:
#unique(baseline_data_COX$sex_1_2)
#unique(baseline_data_COX$race_white)
#min(baseline_data_COX$assessed_BMI, na.rm = TRUE)
#max(baseline_data_COX$assessed_BMI, na.rm = TRUE)
#unique(baseline_data_COX$assessed_BMI)


print("baseline participants are above")

analytical_sample_COX_baseline = subset(analytical_sample_COX, analytical_sample_COX$start_new == 0) 
analytical_sample_COX_baseline_ids = unique(analytical_sample_COX_baseline$HHIDPN)
nrow(analytical_sample_COX_baseline_ids)


#####

#subset to diabetes = 1, diabetes = 0 throughout the follow-up

non_diabetes_baseline = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 0) 
non_diabetes_followup_1 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 1) 
non_diabetes_followup_2 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 2) 
non_diabetes_followup_3 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 3) 
non_diabetes_followup_4 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 4)
non_diabetes_followup_5 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 5)


#non_diabetes_baseline_and1 = inner_join(non_diabetes_baseline, 
#                                        non_diabetes_followup_1,
#                                        copy = TRUE,    
#                                        keep = FALSE, 
#                                        suffix = c("", "_new"), 
#                                        
#                                        by = "HHIDPN")

non_diabetes_baseline_and1_2 = inner_join(non_diabetes_followup_1, 
                                          non_diabetes_followup_2,
                                          copy = TRUE, 
                                          keep = FALSE, 
                                          suffix = c("", "_new"), 
                                          
                                          by = "HHIDPN")


non_diabetes_throughout_the_study = non_diabetes_baseline_and1_2


##########
##########




diabetes_baseline = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 0) 
diabetes_followup_1 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 1 & analytical_sample_COX$start_new == 1) 
diabetes_followup_2 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 1 & analytical_sample_COX$start_new == 2) 
diabetes_followup_3 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 1 & analytical_sample_COX$start_new == 3) 
diabetes_followup_4 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 1 & analytical_sample_COX$start_new == 4)
diabetes_followup_5 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 1 & analytical_sample_COX$start_new == 5)

table(diabetes_followup_3$discrim_bin) 

#diabetes_baseline_and1 = inner_join( diabetes_baseline, 
#                                       diabetes_followup_1,

#                                       suffix = c("", "_new"), 

#                                      by = "HHIDPN")

diabetes_baseline_and1_2 = full_join( diabetes_followup_1, 
                                      diabetes_followup_2,
                                      
                                      suffix = c("", "_new"), 
                                      
                                      by = "HHIDPN")

diabetes_baseline_and1_2_3 = full_join( diabetes_baseline_and1_2, 
                                        diabetes_followup_3,
                                        
                                        suffix = c("", "_new"), 
                                        
                                        by = "HHIDPN")

diabetes_baseline_and1_2_3_4 = full_join( diabetes_baseline_and1_2_3, 
                                          diabetes_followup_4,
                                          
                                          suffix = c("", "_new"), 
                                          
                                          by = "HHIDPN")

diabetes_throughout_the_study = full_join( diabetes_baseline_and1_2_3_4, 
                                           diabetes_followup_5,
                                           
                                           suffix = c("", "_new"), 
                                           by = "HHIDPN")


subsets_n_percent = cbind(nrow(analytical_sample_COX_baseline), 
                      nrow(analytical_sample_COX_baseline), 
                      
                      nrow(diabetes_throughout_the_study), 
                      #(nrow(diabetes_throughout_the_study)/nrow(analytical_sample_COX)*100), 
                      (nrow(diabetes_throughout_the_study)/nrow(analytical_sample_COX_baseline)*100), 
                      
                      nrow(non_diabetes_throughout_the_study), 
                      (nrow(non_diabetes_throughout_the_study)/nrow(analytical_sample_COX_baseline)*100))

sd(as.double(non_diabetes_throughout_the_study$alcohol_days_week_new), na.rm = TRUE)
mean(non_diabetes_throughout_the_study$alcohol_days_week_new)


all_participant_char          = participant_char_function(data = analytical_sample_COX_baseline) 
diabetic_participant_char     = participant_char_function(data = diabetes_throughout_the_study)
non_diabetic_participant_char = participant_char_function(data = non_diabetes_throughout_the_study) 


subset_name = c(   "all",
                   "all",
                   "diabetes",
                   "diabetes",
                   "no diabetes", 
                   "no diabetes")

participant_characteristics = cbind(all_participant_char,
                                    diabetic_participant_char,
                                    non_diabetic_participant_char) 
                                    

participant_characteristics = rbind(subset_name,
                                    
                                    subsets_n_percent,
                                  
                                    participant_characteristics)


participant_characteristics

write.csv(participant_characteristics, paste(OUTPUT_ROOT, "participant_characteristics_TABLE_1.csv", sep = ""))


#####                                                                                                                                                                                      
#Add wealth quantiles to table 1.


#####                                                                                                                                                                                      
#Add ethnic minorites to table 1.


#####
###### #####  #####  ##### #####  compare included sample to the overall sample. ##### #####  #####  ##### ##### 






#####
#Check how many women, ethnic minority, BMI>30, were exposed to everyday discrimination every 2- year internal. 
#Assess how many were exposed to discrimination every 2-year intervals (prolonged exposures in each subset all, women, ethnic minorities, BM5> 30 
#This might help explain why we found a cumulative effect in ethnic minorities but did not find it in others. 





###### ##### #####  #####  ##### #####  Check the time window (median follow-up in years) ##### #####  #####  ##### ##### 


##### ##### #####  #####  ##### ##### #Run t-tests comparing diff. sample characteristics between those who developed T2DM and those who did not

diabetes_throughout_the_study$developed_diabetes = rep(1, times = nrow(diabetes_throughout_the_study))
non_diabetes_throughout_the_study$developed_diabetes = rep(0, times = nrow(non_diabetes_throughout_the_study)) 
nrow(non_diabetes_throughout_the_study)

case = c(diabetes_throughout_the_study$developed_diabetes, non_diabetes_throughout_the_study$developed_diabetes)
unique(case)


age = c(diabetes_throughout_the_study$continious_age, non_diabetes_throughout_the_study$continious_age)
sex = c(diabetes_throughout_the_study$sex_1_2, non_diabetes_throughout_the_study$sex_1_2)

race = c(diabetes_throughout_the_study$race_white, non_diabetes_throughout_the_study$race_white)
#BMI kg/m2, mean (SD)

BMI = c(diabetes_throughout_the_study$assessed_BMI, non_diabetes_throughout_the_study$assessed_BMI)

#CVD,  n (%)

CVD = c(diabetes_throughout_the_study$CVD, non_diabetes_throughout_the_study$CVD)

#Hypertension, n (%)

hypertension = c(diabetes_throughout_the_study$hypertension_new_bin, non_diabetes_throughout_the_study$hypertension_new_bin)


#Depression, n (%)

depression = c(diabetes_throughout_the_study$checklist_depression_bin, non_diabetes_throughout_the_study$checklist_depression_bin)


#Alcohol consumption (days/week), Mean (SD)

Alcohol_consumption  = c(diabetes_throughout_the_study$alcohol_days_week, non_diabetes_throughout_the_study$alcohol_days_week)
Alcohol_consumption = as.numeric(Alcohol_consumption)

#Smoker status, n (%)

Smoking_status  = c(diabetes_throughout_the_study$smokes_now_bin, non_diabetes_throughout_the_study$smokes_now_bin)


#MVPA frequency, median
MVPA  = c(diabetes_throughout_the_study$vigarious_physical_activity, non_diabetes_throughout_the_study$vigarious_physical_activity)


wealth = c(diabetes_throughout_the_study$wealth_noIRA, 
           non_diabetes_throughout_the_study$wealth_noIRA)

data_ttest = data.frame(case, 
                    age, 
                   race, 
                    sex, 
                    BMI,
                    #CVD,  n (%)
                    CVD,
                    #Hypertension, n (%)
                    hypertension, 
                    #Depression, n (%)
                    depression,
                    #Alcohol consumption (days/week), Mean (SD)
                    #Alcohol_consumption,  
                    #Smoker status, n (%)
                    Smoking_status,
                    MVPA,
                   wealth)


age_diff <- t_test(age ~ case, data = data_ttest)
BMI_diff <- t_test(BMI ~ case, data = data_ttest)

#BMI_diff <- t_test(Alcohol_consumption ~ case, data = data_ttest)


wealth_diff <- t_test(wealth ~ case, data = data_ttest)

unique(case)
unique(Smoking_status)

chisq.test(case, sex)
chisq.test(case, CVD)
chisq.test(case, hypertension)
chisq.test(case, depression)
#chisq.test(case, Alcohol_consumption)
#chisq.test(case, Smoking_status)
chisq.test(case, MVPA)

chisq.test(case, race)
#MVPA frequency, median




##### #Compare AIC and BIC across the 5 included models

#wealth 
median(analytical_sample_COX_baseline$wealth_noIRA, na.rm = TRUE)
analytical_sample_COX_baseline$ses = case_when(analytical_sample_COX_baseline$wealth_noIRA <=median(analytical_sample_COX_baseline$wealth_noIRA, na.rm = TRUE) ~ 1,
                                               analytical_sample_COX_baseline$wealth_noIRA > median(analytical_sample_COX_baseline$wealth_noIRA, na.rm = TRUE) ~ 2)
unique(analytical_sample_COX_baseline$ses)



median(diabetes_throughout_the_study$wealth_noIRA, na.rm = TRUE)
diabetes_throughout_the_study$ses = case_when(diabetes_throughout_the_study$wealth_noIRA <=median(diabetes_throughout_the_study$wealth_noIRA, na.rm = TRUE) ~ 1,
                                              diabetes_throughout_the_study$wealth_noIRA > median(diabetes_throughout_the_study$wealth_noIRA, na.rm = TRUE) ~ 2)
unique(diabetes_throughout_the_study$ses)




median(non_diabetes_throughout_the_study$wealth_noIRA, na.rm = TRUE)
non_diabetes_throughout_the_study$ses = case_when(non_diabetes_throughout_the_study$wealth_noIRA <=median(non_diabetes_throughout_the_study$wealth_noIRA, na.rm = TRUE) ~ 1,
                                                  non_diabetes_throughout_the_study$wealth_noIRA > median(non_diabetes_throughout_the_study$wealth_noIRA, na.rm = TRUE) ~ 2)
unique(non_diabetes_throughout_the_study$ses)




ses = c(diabetes_throughout_the_study$ses,
        non_diabetes_throughout_the_study$ses)
nrow(ses)

diabetes_throughout_the_study$developed_diabetes = rep(1, times = nrow(diabetes_throughout_the_study))
non_diabetes_throughout_the_study$developed_diabetes = rep(0, times = nrow(non_diabetes_throughout_the_study)) 


table(diabetes_throughout_the_study$discrim_bin)


chisq.test(case, ses)
table(case, ses)

chisq.test(case, race)
table(case, race)

################

# summary_stat = summary(diabetes_throughout_the_study$wealth_noIRA)


# diabetes_throughout_the_study$wealth_quantile = case_when(diabetes_throughout_the_study$wealth_noIRA <summary_stat[2] ~ 1, 
#                                                           diabetes_throughout_the_study$wealth_noIRA >summary_stat[2] & diabetes_throughout_the_study$wealth_noIRA < summary_stat[3] ~ 2,
#                                                           diabetes_throughout_the_study$wealth_noIRA>summary_stat[3] & diabetes_throughout_the_study$wealth_noIRA < summary_stat[5] ~ 3, 
#                                                           diabetes_throughout_the_study$wealth_noIRA>summary_stat[5] ~ 4)



# 
# 
# summary_stat_no_diab = summary(non_diabetes_throughout_the_study$wealth_noIRA)
# 
# 
# non_diabetes_throughout_the_study$wealth_quantile = case_when(non_diabetes_throughout_the_study$wealth_noIRA <summary_stat_no_diab[2] ~ 1, 
#                                                           non_diabetes_throughout_the_study$wealth_noIRA >summary_stat_no_diab[2] & non_diabetes_throughout_the_study$wealth_noIRA < summary_stat_no_diab[3] ~ 2,
#                                                           non_diabetes_throughout_the_study$wealth_noIRA>summary_stat_no_diab[3] & non_diabetes_throughout_the_study$wealth_noIRA < summary_stat_no_diab[5] ~ 3, 
#                                                           non_diabetes_throughout_the_study$wealth_noIRA>summary_stat_no_diab[5] ~ 4)
# 
# wealth_quantile = c(diabetes_throughout_the_study$wealth_quantile, 
#                     non_diabetes_throughout_the_study$wealth_quantile)
# 
# 
# chisq.test(case, wealth_quantile)
# 
# 
