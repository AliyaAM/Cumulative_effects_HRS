library(WCE)
library(survival)
library(dplyr)
library(car)
library(tidyverse)
library(tidyr)

library(epiDisplay) #tab1 function to make a frequency table 
library(foreign)
library(rms) # Used to extract p-value from logistic model
library(ggplot2) #plots 
library(corrplot)
library(gridExtra) 
library(sjPlot)
library(knitr)
library(lme4)
library(lattice)
library(Hmisc)




## Set the root directory to look for source code.
#laptop: 
#/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/
#"/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"

# plots: 
# https://adibender.github.io/pammtools/articles/cumulative-effects.html

#https://rpkgs.datanovia.com/ggpubr/reference/stat_regline_equation.html Adding text (coefficients etc) to the plot next to the regression line 


OUTPUT_ROOT ="/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/Results/"
SOURCE_ROOT = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/WCE_analysis/"

SOURCE_data_ROOT  = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/"

# function that subsets and srts dataset for a particular var (eg., age == 1)
source((paste(SOURCE_ROOT, "subset_sort.R", sep="")))

source((paste(SOURCE_ROOT, "subset_sort_reason.R", sep="")))



#function that sorts out the data into a dataframe where each participant x wave pair is one row. Here I also add starting and stopping points to identify each wave
source((paste(SOURCE_ROOT, "sort_timepoints.R", sep="")))

#function that runs WCE analysis
#source((paste(SOURCE_ROOT, "WCE_analysis.R", sep="")))

#function that runs WCE analysis
source((paste(SOURCE_ROOT, "summary_score_WCE_analysis.R", sep="")))

# function that samples bootstrapped CIs
source((paste(SOURCE_ROOT, "summary_score_Bootstrapped_CI.R", sep="")))


# function that runs WCE analysis and CIs sampling for a specified subset and with a specified model 
source((paste(SOURCE_ROOT, "discrim_bin_model_func.R", sep="")))


#Model 1: age and sex, wealth  [basis adjustment]
Model_1 = c("continious_age", "wealth_noIRA", "sex_1_2")

#Model 2: age, sex, wealth, BMI, hypertension  [basic adjustment + diabetes risk factors]
Model_2 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "hypertension_new_bin")

#Model 3: age, sex, wealth, physical activity, smoking (yes/no), and alcohol (days/week) [basic adjustment + health behaviours]
Model_3 = c("continious_age", "wealth_noIRA", "sex_1_2","alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin')

#Model 4: age, sex, wealth, CVD  [basic adjustment + CVD most common diabetes co-morbidity]
Model_4 = c("continious_age", "wealth_noIRA", "sex_1_2","CVD")

#Model 5: age, sex, wealth, depression  [basic adjustment + depression best researched psychosocial factor in diabetes ]
Model_5 = c("continious_age","wealth_noIRA", "sex_1_2","checklist_depression_bin")

#Model 6: age, sex, wealth, BMI, hypertension, CVD  [basic adjustment + diabetes risk factors+ CVD]
Model_6 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "hypertension_new_bin", "CVD")

#Model 7: age, sex, wealth, BMI, hypertension, depression  [basic adjustment + diabetes risk factors+ Depression]
Model_7 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "hypertension_new_bin", "checklist_depression_bin")
            

########
########

Model_noBMIcov_2 = c( "continious_age")
Model_noBMIcov_3 = c("continious_age", "wealth_noIRA")
Model_noBMIcov_4 = c("continious_age", "hypertension_new_bin")
Model_noBMIcov_5 = c( "continious_age", "hypertension_new_bin", "wealth_noIRA")
Model_noBMIcov_6 = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin')
Model_noBMIcov_7 = c( "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin')
Model_noBMIcov_8 = c( "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", "smokes_now_bin", "hypertension_new_bin")
Model_noBMIcov_9 = c("continious_age", "CVD")
Model_noBMIcov_10 = c( "continious_age", "CVD", "hypertension_new_bin")
Model_noBMIcov_11 = c( "continious_age", "CVD","alcohol_days_week_new",  "vigarious_physical_activity_new", "smokes_now_bin")
Model_noBMIcov_12 = c( "continious_age", "CVD","alcohol_days_week_new",  "vigarious_physical_activity_new", "smokes_now_bin", "hypertension_new_bin")
Model_noBMIcov_13 = c( "continious_age", "checklist_depression_bin")
Model_noBMIcov_14 = c( "continious_age", "checklist_depression_bin", "alcohol_days_week_new",  "vigarious_physical_activity_new", "smokes_now_bin")
Model_noBMIcov_15 = c( "continious_age", "hypertension_new_bin", "checklist_depression_bin")
Model_noBMIcov_16 = c( "continious_age", "hypertension_new_bin", "CVD", "checklist_depression_bin")
Model_noBMIcov_17 = c("continious_age", "hypertension_new_bin", "CVD", "checklist_depression_bin", "alcohol_days_week_new",  "vigarious_physical_activity_new", "smokes_now_bin")

#self-reported
#HRS2008_data_subset = subset(HRS2008_data, HRS2008_data[ , subset_var1] >= subset_value1 | HRS2008_data[ , subset_var2] <= subset_value2)
# 1 group 

#reason_discrim1_reason_age

age_dataset = subset_sort_reason(subset_var1 = "reason_discrim1_reason_age", 
                                                  subset_value1 = 1) 






age_Model_1_discrim_bin = discrim_bin_model_func(data_wce_subset = age_dataset, 
                                                 Model_n = Model_1, 
                                                 Model_name = "Model_1")



age_Model_2_discrim_bin = discrim_bin_model_func(data_wce_subset = age_dataset, 
                                                               Model_n = Model_2, 
                                                               Model_name = "Model_2")


age_Model_3_discrim_bin = discrim_bin_model_func(data_wce_subset = age_dataset, 
                                                               Model_n = Model_3)



age_Model_4_discrim_bin = discrim_bin_model_func(data_wce_subset = age_dataset, 
                                                               Model_n = Model_4)



age_Model_5_discrim_bin = discrim_bin_model_func(data_wce_subset = age_dataset, 
                                                               Model_n = Model_5)


age_Model_6_discrim_bin = discrim_bin_model_func(data_wce_subset = age_dataset, 
                                                               Model_n = Model_6)


age_Model_7_discrim_bin = discrim_bin_model_func(data_wce_subset = age_dataset, 
                                                               Model_n = Model_7)



age_results_discrim_bin = rbind(age_Model_1_discrim_bin,
                                age_Model_2_discrim_bin,
                                        age_Model_3_discrim_bin,
                                        age_Model_4_discrim_bin,
                                        age_Model_5_discrim_bin,
                                        age_Model_6_discrim_bin,
                                        age_Model_7_discrim_bin)

write.csv(age_results_discrim_bin, paste(OUTPUT_ROOT, "age_discrim_bin_reason_noage_V2.csv", sep=""))



age_results_discrim_bin_table_col= cbind(age_Model_1_discrim_bin,
                                         age_Model_2_discrim_bin,
                                                 age_Model_3_discrim_bin,
                                                 age_Model_4_discrim_bin,
                                                 age_Model_5_discrim_bin,
                                                 age_Model_6_discrim_bin,
                                                 age_Model_7_discrim_bin)

write.csv(age_results_discrim_bin_table_col, paste(OUTPUT_ROOT, "age_results_discrim_bin_table_col_reason_noage_V2.csv", sep=""))


#########

#
#####