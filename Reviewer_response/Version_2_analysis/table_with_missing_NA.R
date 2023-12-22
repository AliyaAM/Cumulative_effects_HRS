

# Load necessary packages


library("dplyr")
library("tidyr")
library("ggplot2")
library("stats")
library("summarytools")
library("devtools")

# install.packages("devtools")
#devtools::install_github("ewenharrison/finalfit")
library("finalfit")



#current_directory = "/Users/k2147340/OneDrive - King's College London/Documents/"

#data_directory = "/Users/aliya/Downloads/"

#data_directory = "/Users/k2147340/OneDrive - King's College London/Desktop/"
#C:\Users\k2147340\OneDrive - King's College London\Desktop

#current_directory = "/Users/aliya/my_docs/"
current_directory = "/Users/aliyaamirova/proj/Cumulative_effects_HRS"

#OUTPUT_ROOT = paste(current_directory, "proj/Cumulative_effects_HRS/Reviewer_response/Version_2_analysis/RESULTS/", sep="")
#SOURCE_ROOT = paste(current_directory, "proj/Cumulative_effects_HRS/Reviewer_response/Version_2_analysis/", sep="")
OUTPUT_ROOT = paste(current_directory, "/Reviewer_response/Version_2_analysis/RESULTS/", sep="")
SOURCE_ROOT = paste(current_directory, "/Reviewer_response/Version_2_analysis/", sep="")

DATA_ROOT = paste(current_directory, "/ELSA_HRS/Data_analysis/", sep = "") 
#DATA2_ROOT = paste(current_directory, "proj/Cumulative_effects_HRS/Reviewer_response/Version_2_analysis/RESULTS/", sep="")



source((paste(SOURCE_ROOT, "participant_char_function.R", sep="")))


# ###### DATA:
# below is the entire dataset, not subseted to anyone:

#cumulative_effects_dat_initial = read.csv(paste(data_directory, "data_flow_chart_withoutbaselineCVD.csv", sep=""))
cumulative_effects_dat_initial = read.csv(paste(current_directory, "/data_files/data_flow_chart_withoutbaselineCVD.csv", sep=""))


cumulative_effects_dat_initial$RAHISPAN
unique(cumulative_effects_dat_initial$education_level)
table(cumulative_effects_dat_initial$education_level)

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



analytical_sample_COX_baseline = subset(analytical_sample_COX, analytical_sample_COX$start_new == 0) 
analytical_sample_COX_baseline_ids = unique(analytical_sample_COX_baseline$HHIDPN)
nrow(analytical_sample_COX_baseline)

#####

#####

#subset to diabetes = 1, diabetes = 0 throughout the follow-up

non_diabetes_baseline = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 0) 
non_diabetes_followup_1 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 1) 
non_diabetes_followup_2 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 2) 
non_diabetes_followup_3 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 3) 
non_diabetes_followup_4 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 4)
non_diabetes_followup_5 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 5)


#non_diabetes_baseline_and1 = inner_join( non_diabetes_baseline, 
#                                      non_diabetes_followup_1,

#                                       suffix = c("", "_new"), 

#                                      by = "HHIDPN")

non_diabetes_baseline_and1_2 = full_join(non_diabetes_followup_1, 
                                         non_diabetes_followup_2,
                                         
                                         suffix = c("", "_new"), 
                                         
                                         by = "HHIDPN")

non_diabetes_baseline_and1_2_3 = full_join(non_diabetes_baseline_and1_2, 
                                           non_diabetes_followup_3,
                                           
                                           suffix = c("", "_new"), 
                                           
                                           by = "HHIDPN")

non_diabetes_baseline_and1_2_3_4 = full_join(non_diabetes_baseline_and1_2_3, 
                                             non_diabetes_followup_4,
                                             
                                             suffix = c("", "_new"), 
                                             
                                             by = "HHIDPN")

non_diabetes_throughout_the_study = full_join(non_diabetes_baseline_and1_2_3_4, 
                                              non_diabetes_followup_5,
                                              
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
##########
##########


diabetes_throughout_the_study$developed_diabetes = rep(1, times = nrow(diabetes_throughout_the_study))
non_diabetes_throughout_the_study$developed_diabetes = rep(0, times = nrow(non_diabetes_throughout_the_study)) 
nrow(non_diabetes_throughout_the_study)

case = c(diabetes_throughout_the_study$developed_diabetes,
         non_diabetes_throughout_the_study$developed_diabetes)

unique(case)

education = c(diabetes_throughout_the_study$education_level, 
              non_diabetes_throughout_the_study$education_level)

wealth = c(diabetes_throughout_the_study$wealth_noIRA, 
           non_diabetes_throughout_the_study$wealth_noIRA) 

age = c(diabetes_throughout_the_study$continious_age, 
        non_diabetes_throughout_the_study$continious_age)

sex = c(diabetes_throughout_the_study$sex_1_2, 
        non_diabetes_throughout_the_study$sex_1_2)


diabetes_throughout_the_study$sex

race = c(diabetes_throughout_the_study$race_white, 
         non_diabetes_throughout_the_study$race_white)

#BMI kg/m2, mean (SD)

BMI = c(diabetes_throughout_the_study$assessed_BMI, 
        non_diabetes_throughout_the_study$assessed_BMI)

#CVD,  n (%)

CVD = c(diabetes_throughout_the_study$CVD, 
        non_diabetes_throughout_the_study$CVD)

#Hypertension, n (%)

hypertension = c(diabetes_throughout_the_study$hypertension_new_bin, 
                 non_diabetes_throughout_the_study$hypertension_new_bin)


#Depression, n (%)

depression = c(diabetes_throughout_the_study$checklist_depression_bin, 
               non_diabetes_throughout_the_study$checklist_depression_bin)


#Alcohol consumption (days/week), Mean (SD)

Alcohol_consumption  = c(diabetes_throughout_the_study$alcohol_days_week, 
                         non_diabetes_throughout_the_study$alcohol_days_week)

Alcohol_consumption = as.numeric(Alcohol_consumption)

#Smoker status, n (%)

Smoking_status  = c(diabetes_throughout_the_study$smokes_now_bin, 
                    non_diabetes_throughout_the_study$smokes_now_bin)


#MVPA frequency, median
MVPA  = c(diabetes_throughout_the_study$vigarious_physical_activity, 
          non_diabetes_throughout_the_study$vigarious_physical_activity)


wealth = c(diabetes_throughout_the_study$wealth_noIRA, 
           non_diabetes_throughout_the_study$wealth_noIRA)

data_ttest = data.frame(case, 
                        age, 
                        race, 
                        sex, 
                        BMI,
                        education,
                        
                        #CVD,  n (%)
                        CVD,
                        #Hypertension, n (%)
                        hypertension, 
                        #Depression, n (%)
                        depression,
                        #Alcohol consumption (days/week), Mean (SD)
                        Alcohol_consumption,  
                        #Smoker status, n (%)
                        Smoking_status,
                        MVPA,
                        wealth)


head(data_ttest)
unique(data_ttest$case)


# Subset the data
group1 <- data_ttest %>% filter(case == 1)
nrow(group1)
group2 <- data_ttest %>% filter(case == 0)


### subset each dataset furhter into those with NAs and with complete follow-up: data_ttest, group1, group2



#write table detailing the summary statistic for the following variables: 
### Age (years, mean SD), Female (n, %),
### Race ethnicity (n, %), including Black (n, %), Hispanic (n, %), Other (n, %), White (n, %), 
### Wealth quantile, (n, %), including  Wealth quantile (0) (n, %), Wealth quantile (1) (n, %), Wealth quantile (2) (n, %), Wealth quantile (3) (n, %), Wealth quantile (4) (n, %), 
### Education (n, %),including  Less than upper secondary (n, %), Upper secondary & vocational (n, %), Tertiary education (n, %)
### BMI (kg/m2, mean, SD), Hypertension, (n, %), Depression, (n, %), Alcohol units (days/week, mean SD), Smoker status, (n, %), MVPA frequency (median, IQR)

### subgroup the information by: baseline (N, mean (SD)/n(%)), lost to follow-up and by entire sample, developed diabetes, did not develop diabetes. 
# each one further divided into the following: retained sample at first follow-up, at second follow-up, at third follow-up 




# Calculate summary statistics for Age (years, mean, SD) for the following data subsets: data_ttest, group1, group2

### age: missing data and complete cases: 

data_ttest_with_na_age <- data_ttest[is.na(data_ttest$age), ]

  
age_summary <- descr(group1$age, stats = c("mean", "sd"), 
                     transpose = TRUE, headings  = FALSE)



# Explanatory or confounding variables
explanatory = c("age", 
                "race", 
                "sex", 
                "BMI",
                "education")


#CVD,  n (%)
#"CVD",
#Hypertension, n (%)
#"hypertension", 
#Depression, n (%)
#"depression",
#Alcohol consumption (days/week), Mean (SD)
#"Alcohol_consumption",  
#Smoker status, n (%)
#"Smoking_status",
#"MVPA",
#"wealth"

dependent = "case" # Bowel obstruction

data_ttest %>% 
  finalfit::summary_factorlist(dependent, explanatory, 
                               na_include=TRUE, p=TRUE)


developed_diabetes_ids = unique(diabetes_throughout_the_study$HHIDPN)

did_not_develop_diabetes_ids = unique(non_diabetes_throughout_the_study$HHIDPN)



baseline_included_cohort_developed_diabetes <- subset(analytical_sample_COX, analytical_sample_COX$start_new == 0 & (HHIDPN %in% developed_diabetes_ids))

baseline_included_cohort_didnot_develop_diabetes <- subset(analytical_sample_COX, analytical_sample_COX$start_new == 0 & (HHIDPN %in% did_not_develop_diabetes_ids))

analytical_sample_COX_baseline

##############
##############
##############


baseline_included_cohort_developed_diabetes$developed_diabetes = rep(1, times = nrow(baseline_included_cohort_developed_diabetes))
baseline_included_cohort_didnot_develop_diabetes$developed_diabetes = rep(0, times = nrow(baseline_included_cohort_didnot_develop_diabetes)) 


dataset_compared = rbind(baseline_included_cohort_developed_diabetes, baseline_included_cohort_didnot_develop_diabetes)

dataset_compared$developed_diabetes = as.factor(dataset_compared$developed_diabetes)

unique(dataset_compared$smokes_ever_bin) 
unique(dataset_compared$smokes_now_bin)

# Explanatory or confounding variables
explanatory = c("continious_age", 
                "sex_1_2", 
                "race_white", 
                "education_level", 
                "wealth_noIRA", 
                "assessed_BMI", 
                 "hypertension_new_bin", 
                 "checklist_depression_bin", 
                 "alcohol_days_week", 
               # "smokes_now_bin"
                 "vigarious_physical_activity") 
                # 


dependent = "developed_diabetes" # Bowel obstruction

table1_final = dataset_compared %>%
  summary_factorlist(dependent, explanatory, p = TRUE, na_include = TRUE,
                   total_col = TRUE,
                   add_row_total = TRUE) -> t

write.csv(table1_final, file = paste(OUTPUT_ROOT, "table1_final.csv", sep = ""))

