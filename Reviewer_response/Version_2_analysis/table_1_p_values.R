# Load necessary packages



library("dplyr")
library("tidyr")
library("ggplot2")
library("stats")

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
nrow(analytical_sample_COX_baseline_ids)

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

# t-tests for continuous variables
age_pvalue <- t.test(group1$age, group2$age)$p.value
bmi_pvalue <- t.test(group1$BMI, group2$BMI)$p.value
alcohol_units_pvalue <- t.test(group1$Alcohol_consumption, group2$Alcohol_consumption)$p.value

# Mann-Whitney U test for non-parametric data
mvpa_pvalue <- wilcox.test(group1$MVPA, group2$MVPA)$p.value



# chi-square tests for categorical variables
table_sex_diabetes = table(data_ttest$case, data_ttest$sex)

#   # Calculate NA counts
 na_coun_sex <- sum(is.na(data_ttest$sex))

#   # Calculate percentages
percentages_sex <- prop.table(table_sex_diabetes, 1) * 100



#######
#######
#######

# Create a contingency table including NAs
table_sex_diabetes <- table(data_ttest$case, data_ttest$sex, useNA = "ifany")

# Calculate total counts including NAs
total_counts <- rowSums(table_sex_diabetes)

# Calculate percentages including NAs
percentages_sex <- sweep(table_sex_diabetes, 1, total_counts, FUN = "/") * 100

# Calculate NA counts and percentages
na_count_sex <- sum(is.na(data_ttest$sex))
na_percentage_sex <- (na_count_sex / length(data_ttest$sex)) * 100

# Add NA percentages as a new column to the percentages table
percentages_sex <- cbind(percentages_sex, `NAs (%)` = na_percentage_sex)



#######
#######
#######


# Calculate total counts including NAs
total_counts <- rowSums(table_sex_diabetes)

# Calculate percentages including NAs
# Create a contingency table including NAs
table_sex_diabetes <- table(data_ttest$case, data_ttest$sex, useNA = "ifany")
percentages_sex <- sweep(table_sex_diabetes, 1, total_counts, FUN = "/") * 100
female_pvalue <- chisq.test(table(data_ttest$case, data_ttest$sex))$p.value

print(table_sex_diabetes)
print(percentages_sex)
print(female_pvalue)


# Combine counts and percentages
formatted_table <- apply(table_sex_diabetes, c(1, 2), function(x, y) sprintf("%d (%.2f%%)", x, y[x]), y = percentages_sex)

# Display the formatted table
formatted_table


#######
#######
#######




#### race 
table_race_black_diabetes <- table(data_ttest$case, data_ttest$race, useNA = "ifany")
percentages_race_black <- sweep(table_race_black_diabetes, 1, total_counts, FUN = "/") * 100
race_black_pvalue <- chisq.test(table(data_ttest$case, data_ttest$race))$p.value

#### hypertension 
table_hypertension_diabetes <- table(data_ttest$case, data_ttest$hypertension, useNA = "ifany")
percentages_hypertension <- sweep(table_hypertension_diabetes, 1, total_counts, FUN = "/") * 100
hypertension_pvalue <- chisq.test(table(data_ttest$case, data_ttest$hypertension))$p.value

#### depression 
table_depression_diabetes <- table(data_ttest$case, data_ttest$depression, useNA = "ifany")
percentages_depression <- sweep(table_depression_diabetes, 1, total_counts, FUN = "/") * 100
depression_pvalue <- chisq.test(table(data_ttest$case, data_ttest$depression))$p.value

#### smoker
table_smoker_status_diabetes <- table(data_ttest$case, data_ttest$Smoking_status, useNA = "ifany")
percentages_smoker_status <- sweep(table_smoker_status_diabetes, 1, total_counts, FUN = "/") * 100
smoker_status_pvalue <- chisq.test(table(data_ttest$case, data_ttest$Smoking_status))$p.value

table(data_ttest$case, useNA = "ifany")

#### education 
table_education_diabetes <- table(data_ttest$case, data_ttest$education, useNA = "ifany")
percentages_education <- sweep(table_education_diabetes, 1, total_counts, FUN = "/") * 100
education_pvalue <- chisq.test(table(data_ttest$case, data_ttest$education))$p.value

#### wealth (this needs to change to wealth quantile)
table_wealth_diabetes <- table(data_ttest$case, data_ttest$wealth, useNA = "ifany")
percentages_wealth <- sweep(table_wealth_diabetes, 1, total_counts, FUN = "/") * 100
wealth_pvalue <- chisq.test(table(data_ttest$case, data_ttest$wealth))$p.value


# Print p-values 
cat("Age p-value:", age_pvalue, "\n")
cat("BMI p-value:", bmi_pvalue, "\n")
cat("Alcohol Units p-value:", alcohol_units_pvalue, "\n")
cat("MVPA p-value:", mvpa_pvalue, "\n")
# 
# # Function to calculate counts, percentages, NAs and perform chi-square test
# calculate_categorical_stats <- function(variable) {
#   # Create a contingency table
#   table_var_diabetes <- table(data_ttest$diabetes, data_ttest[[variable]], useNA = "ifany")
#   
#   # Calculate NA counts
#   na_count <- sum(is.na(data_ttest[[variable]]))
#   
#   # Calculate percentages
#   percentages <- prop.table(table_var_diabetes, 1) * 100
#   
#   # Chi-square test
#   chi_pvalue <- if(all(table_var_diabetes > 5)) {
#     chisq.test(table_var_diabetes)$p.value
#   } else {
#     # Use Fisher's Exact Test if any expected count is less than 5
#     fisher.test(table_var_diabetes)$p.value
#   }
#   
#   list(
#     counts = table_var_diabetes,
#     percentages = percentages,
#     pvalue = chi_pvalue,
#     na_count = na_count
#   )
# }
# 
# # Calculate stats for race_black
# race_black_stats <- calculate_categorical_stats("race")
# 
# sex_stats <- calculate_categorical_stats("sex")
# 
# 
# 
# # ... Add the other variables similarly ...
# 
# # Print results including NA counts
# print("Race Black Stats")
# print(race_black_stats$counts)
# print(race_black_stats$percentages)
# cat("Race Black Chi-square p-value:", race_black_stats$pvalue, "\n")
# cat("Race Black NAs:", race_black_stats$na_count, "\n\n")
# 
# # Repeat printing for other variables, including the NA counts
# 
# 
# 

print("check why there are no NAs in sex variable for the group that never developed diabetes but there are NAs in the group that did develop diabetes (29% also)")
print("I am not sure how to present this data but I checked and all is correct we are just not reporting the missing data. The total N = 917 for those who developed diabetes. Please find the screenshot attached. How do I include NAs in this table? For sex, NA=273 (29.78%) among those who developed diabetes and 0 for those who did not. The rest is exactly as reported in Table 1 already.We would also need to report the males as well as NAs in each group (diabetes vs not diabetes) and then the table will have another two rows per variable, it is already a confusing table. Can we add NAs for each variable in supplemental materials and then state in the notes that there is missing data (see supplemental materials)?")