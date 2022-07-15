
library("dplyr")
library("tidyr")
library("bain")

current_directory = "/Users/aliya/my_docs/"


DATAIN_ROOT = (paste(current_directory, "KCL_postDoc/Data_analysis/", sep="")) 
SOURCE_ROOT = (paste(current_directory, "proj/Cumulative_effects_HRS/Version_2_analysis/", sep=""))
OUTPUT_ROOT =(paste(current_directory, "KCL_postDoc/Cumulative_effects/", sep=""))


source((paste(SOURCE_ROOT, "participant_char_function.R", sep="")))

#data for each year that has all cases but only the relevent columns (vars), hense called short. OLD stands for the way the diabetes outcome was extracted (which is correct) 

HRS2008_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2008_data/old/HRS2008_data_short_OLD.csv", sep=""))
HRS2010_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2010_data/old/HRS2010_data_short_OLD.csv", sep=""))
HRS2012_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2012_data/old/HRS2012_data_short_OLD.csv", sep=""))
HRS2014_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2014_data/old/HRS2014_data_short_OLD.csv", sep=""))
HRS2016_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2016_data/old/HRS2016_data_short_OLD.csv", sep=""))
HRS2018_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2018_data/old/HRS2018_data_short_OLD.csv", sep=""))


# the analytical sample for the WCE (and the subset of it is in COX), which includes all years and each year is labeled by a new variable (start_new: 0 for baseline, 1 for the first follow-up, 2 for the second follow-up etc, at 2-year intervals)
# the analytical sample does not include the baseline diabetes. 

analytical_sample_COX = read.csv("/Users/aliya/my_docs/proj/Cumulative_effects_HRS/data_files/all_waves_nodiabatbaseline_DIAB.csv")

#n_all = length(analytical_sample_COX_baseline_ids) 
#this below is for those with the BMI >30 kg/m2
analytical_sample_BMI = read.csv("/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/DATA_FOR_PLOT/all_waves_nodiabatbaseline_DIAB.csv")


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
WCE_unique_id = unique(analytical_sample_BMI$HHIDPN)
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

analytical_sample_COX_baseline = subset(analytical_sample_COX, analytical_sample_COX$start_new == 0) 
analytical_sample_COX_baseline_ids = unique(analytical_sample_COX_baseline$HHIDPN)

data_male = subset(analytical_sample_COX_baseline, analytical_sample_COX_baseline$sex_1_2 ==1) 
data_female = subset(analytical_sample_COX_baseline, analytical_sample_COX_baseline$sex_1_2 ==2) 
data_race = subset(analytical_sample_COX_baseline, analytical_sample_COX_baseline$race_white == 0) 
data_BMI = subset(analytical_sample_COX_baseline, analytical_sample_COX_baseline$assessed_BMI > 30) 

unique(data_BMI$CVD_ever)

#####

#subset to diabetes = 1, diabetes = 0 throughout the follow-up

non_diabetes_baseline = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 0) 
non_diabetes_followup_1 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 1) 
non_diabetes_followup_2 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 2) 
non_diabetes_followup_3 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 3) 
non_diabetes_followup_4 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 4)
non_diabetes_followup_5 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 5)


non_diabetes_baseline_and1 = inner_join(non_diabetes_baseline, 
                                        non_diabetes_followup_1,
                                        copy = TRUE,    
                                        keep = FALSE, 
                                        suffix = c("", "_new"), 
                                        
                                        by = "HHIDPN")

non_diabetes_baseline_and1_2 = inner_join(non_diabetes_baseline_and1, 
                                          non_diabetes_followup_2,
                                          copy = TRUE, 
                                          keep = FALSE, 
                                          suffix = c("", "_new"), 
                                          
                                          by = "HHIDPN")

non_diabetes_baseline_and1_2_3 = inner_join(non_diabetes_baseline_and1_2, 
                                            non_diabetes_followup_3,
                                            copy = TRUE,     
                                            keep = FALSE, 
                                            suffix = c("", "_new"), 
                                            
                                            by = "HHIDPN")

non_diabetes_baseline_and1_2_3_4 = inner_join(non_diabetes_baseline_and1_2_3, 
                                              non_diabetes_followup_4,
                                              copy = TRUE,  
                                              keep = FALSE, 
                                              suffix = c("", "_new"), 
                                              
                                              by = "HHIDPN")

non_diabetes_throughout_the_study = inner_join(non_diabetes_baseline_and1_2_3_4, 
                                               non_diabetes_followup_5,
                                               copy = TRUE,
                                               keep = FALSE, 
                                               suffix = c("", "_new"), 
                                               by = "HHIDPN")


##########
##########




diabetes_baseline = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 0) 
diabetes_followup_1 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 1 & analytical_sample_COX$start_new == 1) 
diabetes_followup_2 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 1 & analytical_sample_COX$start_new == 2) 
diabetes_followup_3 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 1 & analytical_sample_COX$start_new == 3) 
diabetes_followup_4 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 1 & analytical_sample_COX$start_new == 4)
diabetes_followup_5 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 1 & analytical_sample_COX$start_new == 5)

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
                      (nrow(non_diabetes_throughout_the_study)/nrow(analytical_sample_COX_baseline)*100), 
                      
                      nrow(data_female), 
                      (nrow(data_female)/nrow(analytical_sample_COX_baseline)*100), 
                      
                      
                      nrow(data_race), 
                      nrow(data_race)/nrow(data_race)*100, 
                      
                      
                      nrow(data_BMI), 
                      (nrow(data_BMI)/nrow(analytical_sample_COX_baseline)*100))

sd(as.double(non_diabetes_throughout_the_study$alcohol_days_week_new), na.rm = TRUE)
mean(non_diabetes_throughout_the_study$alcohol_days_week_new)


all_participant_char          = participant_char_function(data = analytical_sample_COX_baseline) 
diabetic_participant_char     = participant_char_function(data = diabetes_throughout_the_study)
non_diabetic_participant_char = participant_char_function(data = non_diabetes_throughout_the_study) 
female_participant_char       = participant_char_function(data = data_female) 
race_participant_char         = participant_char_function(data = data_race) 
BMI_participant_char          = participant_char_function(data = data_BMI) 

subset_name = c(   "all",
                   "all",
                   "diabetes",
                   "diabetes",
                   "no diabetes", 
                   "no diabetes", 
                   "female",
                   "female",
                   "race",
                   "race",
                   "BMI",
                   "BMI")

participant_characteristics = cbind(all_participant_char,
                                    diabetic_participant_char,
                                    non_diabetic_participant_char,
                                    female_participant_char,
                                    race_participant_char,
                                    BMI_participant_char) 
                                    

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

case = c(diabetes_throughout_the_study$developed_diabete, non_diabetes_throughout_the_study$developed_diabetes)
age = c(diabetes_throughout_the_study$continious_age, non_diabetes_throughout_the_study$continious_age)
sex = c(diabetes_throughout_the_study$sex_1_2, non_diabetes_throughout_the_study$sex_1_2)

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


data_ttest = cbind(case, 
                    age, 
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
                    MVPA)

data_ttest = as.data.frame(data_ttest)

data_ttest$age

age_diff <- t_test(age ~ case, data = data_ttest)
BMI_diff <- t_test(BMI ~ case, data = data_ttest)
#BMI_diff <- t_test(Alcohol_consumption ~ case, data = data_ttest)

unique(case)
unique(Smoking_status)

chisq.test(case, sex)
chisq.test(case, CVD)
chisq.test(case, hypertension)
chisq.test(case, depression)
#chisq.test(case, Alcohol_consumption)
#chisq.test(case, Smoking_status)
chisq.test(case, MVPA)

#MVPA frequency, median




##### #Compare AIC and BIC across the 5 included models









