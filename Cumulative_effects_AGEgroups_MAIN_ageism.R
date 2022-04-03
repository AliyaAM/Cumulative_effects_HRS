
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



SOURCE_data_ROOT = "/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"
SOURCE_ROOT =  "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/"
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/Results/"

# function that subsets and srts dataset for a particular var (eg., age50_60yrs == 1)
source((paste(SOURCE_ROOT, "subset_sort.R", sep="")))

source((paste(SOURCE_ROOT, "subset_sort_ageism.R", sep="")))

source((paste(SOURCE_ROOT, "subset_sort_AGEgroups.R", sep="")))

source((paste(SOURCE_ROOT, "subset_sort_BMI.R", sep="")))

source((paste(SOURCE_ROOT, "subset_sort_BMI_ageism.R", sep="")))


#function that sorts out the data into a dataframe where each participant x wave pair is one row. Here I also add starting and stopping points to identify each wave
source((paste(SOURCE_ROOT, "sort_timepoints.R", sep="")))

#function that runs WCE analysis
#source((paste(SOURCE_ROOT, "WCE_analysis.R", sep="")))

#function that runs WCE analysis
source((paste(SOURCE_ROOT, "summary_score_WCE_analysis.R", sep="")))

# function that samples bootstrapped CIs
source((paste(SOURCE_ROOT, "summary_score_Bootstrapped_CI.R", sep="")))


# function that runs WCE analysis and CIs sampling for a specified subset and with a specified model 
source((paste(SOURCE_ROOT, "discrim_bin_model_func_ageism.R", sep="")))


#Models below are a set of covariates 
Model_1 = c("continious_age")
Model_2 = c("assessed_BMI", "continious_age")
Model_3 = c("assessed_BMI", "continious_age", "wealth_noIRA")
Model_4 = c("assessed_BMI", "continious_age", "hypertension_new_bin")
Model_5 = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA")
Model_6 = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin')
Model_7 = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin')
Model_8 = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", "smokes_now_bin", "hypertension_new_bin")
Model_9 = c("assessed_BMI", "continious_age", "CVD")
Model_10 = c("assessed_BMI", "continious_age", "CVD", "hypertension_new_bin")
Model_11 = c("assessed_BMI", "continious_age", "CVD","alcohol_days_week_new",  "vigarious_physical_activity_new", "smokes_now_bin")
Model_12 = c("assessed_BMI", "continious_age", "CVD","alcohol_days_week_new",  "vigarious_physical_activity_new", "smokes_now_bin", "hypertension_new_bin")
Model_13 = c("assessed_BMI", "continious_age", "checklist_depression_bin")
Model_14 = c("assessed_BMI", "continious_age", "checklist_depression_bin", "alcohol_days_week_new",  "vigarious_physical_activity_new", "smokes_now_bin")
Model_15 = c("assessed_BMI", "continious_age", "hypertension_new_bin", "checklist_depression_bin")
Model_16 = c("assessed_BMI", "continious_age", "hypertension_new_bin", "CVD", "checklist_depression_bin")
Model_17 = c("assessed_BMI", "continious_age", "hypertension_new_bin", "CVD", "checklist_depression_bin", "alcohol_days_week_new",  "vigarious_physical_activity_new", "smokes_now_bin")

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
age50_60yrs_dataset = subset_sort_AGEgroups(subset_var1 = "continious_age", 
                                    subset_value1 = 50, 
                                    subset_var2 = "continious_age", 
                                    subset_value2 = 60) 


# 2 group 
age60_70yrs_dataset = subset_sort_AGEgroups(subset_var1 = "continious_age", 
                                            subset_value1 = 61, 
                                            subset_var2 = "continious_age", 
                                            subset_value2 = 70) 


# 3 group 
age70_80yrs_dataset = subset_sort_AGEgroups(subset_var1 = "continious_age", 
                                            subset_value1 = 71, 
                                            subset_var2 = "continious_age", 
                                            subset_value2 = 80) 


# 4 group 
age80above_dataset = subset_sort_AGEgroups(subset_var1 = "continious_age", 
                                         subset_value1 = 81, 
                                         subset_var2 = "continious_age", 
                                         subset_value2 = 100) 

###### run all models for age50_60yrs 
age50_60yrs_Model_1_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age50_60yrs_dataset, 
                                                           Model_n = Model_1, 
                                                           Model_name = "Model_1")


age50_60yrs_Model_2_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age50_60yrs_dataset, 
                                                           Model_n = Model_2, 
                                                           Model_name = "Model_2")


age50_60yrs_Model_3_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age50_60yrs_dataset, 
                                                           Model_n = Model_3)



age50_60yrs_Model_4_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age50_60yrs_dataset, 
                                                           Model_n = Model_4)



age50_60yrs_Model_5_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age50_60yrs_dataset, 
                                                           Model_n = Model_5)


age50_60yrs_Model_6_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age50_60yrs_dataset, 
                                                           Model_n = Model_6)


age50_60yrs_Model_7_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age50_60yrs_dataset, 
                                                           Model_n = Model_7)



age50_60yrs_Model_8_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age50_60yrs_dataset, 
                                                           Model_n = Model_8)


age50_60yrs_Model_9_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age50_60yrs_dataset, 
                                                           Model_n = Model_9)


age50_60yrs_Model_10_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age50_60yrs_dataset, 
                                                            Model_n = Model_10)


age50_60yrs_Model_11_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age50_60yrs_dataset, 
                                                            Model_n = Model_11)


age50_60yrs_Model_12_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age50_60yrs_dataset, 
                                                            Model_n = Model_12)


age50_60yrs_Model_13_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age50_60yrs_dataset, 
                                                            Model_n = Model_13)


age50_60yrs_Model_14_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age50_60yrs_dataset, 
                                                            Model_n = Model_14)

age50_60yrs_Model_15_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age50_60yrs_dataset, 
                                                            Model_n = Model_15)

age50_60yrs_Model_16_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age50_60yrs_dataset, 
                                                            Model_n = Model_16)


age50_60yrs_Model_17_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age50_60yrs_dataset, 
                                                            Model_n = Model_17)

age50_60yrs_results_discrim_bin = rbind(age50_60yrs_Model_1_discrim_bin,
                                   age50_60yrs_Model_2_discrim_bin,
                                   age50_60yrs_Model_3_discrim_bin,
                                   age50_60yrs_Model_4_discrim_bin,
                                   age50_60yrs_Model_5_discrim_bin,
                                   age50_60yrs_Model_6_discrim_bin,
                                   age50_60yrs_Model_7_discrim_bin,
                                   age50_60yrs_Model_8_discrim_bin,
                                   age50_60yrs_Model_9_discrim_bin,
                                   age50_60yrs_Model_10_discrim_bin,
                                   age50_60yrs_Model_11_discrim_bin,
                                   age50_60yrs_Model_12_discrim_bin,
                                   age50_60yrs_Model_13_discrim_bin,
                                   age50_60yrs_Model_14_discrim_bin,
                                   age50_60yrs_Model_15_discrim_bin,
                                   age50_60yrs_Model_16_discrim_bin,
                                   age50_60yrs_Model_17_discrim_bin)

write.csv(age50_60yrs_results_discrim_bin, paste(OUTPUT_ROOT, "age50_60yrs_results_discrim_bin_ageism.csv", sep=""))



age50_60yrs_results_discrim_bin_table_col= cbind(age50_60yrs_Model_1_discrim_bin,
                                            age50_60yrs_Model_2_discrim_bin,
                                            age50_60yrs_Model_3_discrim_bin,
                                            age50_60yrs_Model_4_discrim_bin,
                                            age50_60yrs_Model_5_discrim_bin,
                                            age50_60yrs_Model_6_discrim_bin,
                                            age50_60yrs_Model_7_discrim_bin,
                                            age50_60yrs_Model_8_discrim_bin,
                                            age50_60yrs_Model_9_discrim_bin,
                                            age50_60yrs_Model_10_discrim_bin,
                                            age50_60yrs_Model_11_discrim_bin,
                                            age50_60yrs_Model_12_discrim_bin,
                                            age50_60yrs_Model_13_discrim_bin,
                                            age50_60yrs_Model_14_discrim_bin,
                                            age50_60yrs_Model_15_discrim_bin,
                                            age50_60yrs_Model_16_discrim_bin,
                                            age50_60yrs_Model_17_discrim_bin)

write.csv(age50_60yrs_results_discrim_bin_table_col, paste(OUTPUT_ROOT, "age50_60yrs_results_discrim_bin_table_col_ageism.csv", sep=""))


#########

###### run all models for age60_70yrs 
age60_70yrs_Model_1_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age60_70yrs_dataset, 
                                                           Model_n = Model_1, 
                                                           Model_name = "Model_1")


age60_70yrs_Model_2_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age60_70yrs_dataset, 
                                                           Model_n = Model_2, 
                                                           Model_name = "Model_2")


age60_70yrs_Model_3_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age60_70yrs_dataset, 
                                                           Model_n = Model_3)



age60_70yrs_Model_4_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age60_70yrs_dataset, 
                                                           Model_n = Model_4)



age60_70yrs_Model_5_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age60_70yrs_dataset, 
                                                           Model_n = Model_5)


age60_70yrs_Model_6_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age60_70yrs_dataset, 
                                                           Model_n = Model_6)


age60_70yrs_Model_7_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age60_70yrs_dataset, 
                                                           Model_n = Model_7)



age60_70yrs_Model_8_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age60_70yrs_dataset, 
                                                           Model_n = Model_8)


age60_70yrs_Model_9_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age60_70yrs_dataset, 
                                                           Model_n = Model_9)


age60_70yrs_Model_10_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age60_70yrs_dataset, 
                                                            Model_n = Model_10)


age60_70yrs_Model_11_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age60_70yrs_dataset, 
                                                            Model_n = Model_11)


age60_70yrs_Model_12_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age60_70yrs_dataset, 
                                                            Model_n = Model_12)


age60_70yrs_Model_13_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age60_70yrs_dataset, 
                                                            Model_n = Model_13)


age60_70yrs_Model_14_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age60_70yrs_dataset, 
                                                            Model_n = Model_14)

age60_70yrs_Model_15_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age60_70yrs_dataset, 
                                                            Model_n = Model_15)

age60_70yrs_Model_16_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age60_70yrs_dataset, 
                                                            Model_n = Model_16)


age60_70yrs_Model_17_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age60_70yrs_dataset, 
                                                            Model_n = Model_17)

age60_70yrs_results_discrim_bin = rbind(age60_70yrs_Model_1_discrim_bin,
                                   age60_70yrs_Model_2_discrim_bin,
                                   age60_70yrs_Model_3_discrim_bin,
                                   age60_70yrs_Model_4_discrim_bin,
                                   age60_70yrs_Model_5_discrim_bin,
                                   age60_70yrs_Model_6_discrim_bin,
                                   age60_70yrs_Model_7_discrim_bin,
                                   age60_70yrs_Model_8_discrim_bin,
                                   age60_70yrs_Model_9_discrim_bin,
                                   age60_70yrs_Model_10_discrim_bin,
                                   age60_70yrs_Model_11_discrim_bin,
                                   age60_70yrs_Model_12_discrim_bin,
                                   age60_70yrs_Model_13_discrim_bin,
                                   age60_70yrs_Model_14_discrim_bin,
                                   age60_70yrs_Model_15_discrim_bin,
                                   age60_70yrs_Model_16_discrim_bin,
                                   age60_70yrs_Model_17_discrim_bin)

write.csv(age60_70yrs_results_discrim_bin, paste(OUTPUT_ROOT, "age60_70yrs_results_discrim_bin_ageism.csv", sep=""))



age60_70yrs_results_discrim_bin_table_col= cbind(age60_70yrs_Model_1_discrim_bin,
                                            age60_70yrs_Model_2_discrim_bin,
                                            age60_70yrs_Model_3_discrim_bin,
                                            age60_70yrs_Model_4_discrim_bin,
                                            age60_70yrs_Model_5_discrim_bin,
                                            age60_70yrs_Model_6_discrim_bin,
                                            age60_70yrs_Model_7_discrim_bin,
                                            age60_70yrs_Model_8_discrim_bin,
                                            age60_70yrs_Model_9_discrim_bin,
                                            age60_70yrs_Model_10_discrim_bin,
                                            age60_70yrs_Model_11_discrim_bin,
                                            age60_70yrs_Model_12_discrim_bin,
                                            age60_70yrs_Model_13_discrim_bin,
                                            age60_70yrs_Model_14_discrim_bin,
                                            age60_70yrs_Model_15_discrim_bin,
                                            age60_70yrs_Model_16_discrim_bin,
                                            age60_70yrs_Model_17_discrim_bin)

write.csv(age60_70yrs_results_discrim_bin_table_col, paste(OUTPUT_ROOT, "age60_70yrs_results_discrim_bin_table_col_ageism.csv", sep=""))

#####
#####

###### run all models for age70_80yrs 
age70_80yrs_Model_1_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age70_80yrs_dataset, 
                                                           Model_n = Model_1, 
                                                           Model_name = "Model_1")


age70_80yrs_Model_2_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age70_80yrs_dataset, 
                                                           Model_n = Model_2, 
                                                           Model_name = "Model_2")


age70_80yrs_Model_3_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age70_80yrs_dataset, 
                                                           Model_n = Model_3)



age70_80yrs_Model_4_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age70_80yrs_dataset, 
                                                           Model_n = Model_4)



age70_80yrs_Model_5_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age70_80yrs_dataset, 
                                                           Model_n = Model_5)


age70_80yrs_Model_6_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age70_80yrs_dataset, 
                                                           Model_n = Model_6)


age70_80yrs_Model_7_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age70_80yrs_dataset, 
                                                           Model_n = Model_7)



age70_80yrs_Model_8_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age70_80yrs_dataset, 
                                                           Model_n = Model_8)


age70_80yrs_Model_9_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age70_80yrs_dataset, 
                                                           Model_n = Model_9)


age70_80yrs_Model_10_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age70_80yrs_dataset, 
                                                            Model_n = Model_10)


age70_80yrs_Model_11_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age70_80yrs_dataset, 
                                                            Model_n = Model_11)


age70_80yrs_Model_12_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age70_80yrs_dataset, 
                                                            Model_n = Model_12)


age70_80yrs_Model_13_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age70_80yrs_dataset, 
                                                            Model_n = Model_13)


age70_80yrs_Model_14_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age70_80yrs_dataset, 
                                                            Model_n = Model_14)

age70_80yrs_Model_15_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age70_80yrs_dataset, 
                                                            Model_n = Model_15)

age70_80yrs_Model_16_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age70_80yrs_dataset, 
                                                            Model_n = Model_16)


age70_80yrs_Model_17_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age70_80yrs_dataset, 
                                                            Model_n = Model_17)

age70_80yrs_results_discrim_bin = rbind(age70_80yrs_Model_1_discrim_bin,
                                   age70_80yrs_Model_2_discrim_bin,
                                   age70_80yrs_Model_3_discrim_bin,
                                   age70_80yrs_Model_4_discrim_bin,
                                   age70_80yrs_Model_5_discrim_bin,
                                   age70_80yrs_Model_6_discrim_bin,
                                   age70_80yrs_Model_7_discrim_bin,
                                   age70_80yrs_Model_8_discrim_bin,
                                   age70_80yrs_Model_9_discrim_bin,
                                   age70_80yrs_Model_10_discrim_bin,
                                   age70_80yrs_Model_11_discrim_bin,
                                   age70_80yrs_Model_12_discrim_bin,
                                   age70_80yrs_Model_13_discrim_bin,
                                   age70_80yrs_Model_14_discrim_bin,
                                   age70_80yrs_Model_15_discrim_bin,
                                   age70_80yrs_Model_16_discrim_bin,
                                   age70_80yrs_Model_17_discrim_bin)

write.csv(age70_80yrs_results_discrim_bin, paste(OUTPUT_ROOT, "age70_80yrs_results_discrim_bin_ageism.csv", sep=""))



age70_80yrs_results_discrim_bin_table_col= cbind(age70_80yrs_Model_1_discrim_bin,
                                            age70_80yrs_Model_2_discrim_bin,
                                            age70_80yrs_Model_3_discrim_bin,
                                            age70_80yrs_Model_4_discrim_bin,
                                            age70_80yrs_Model_5_discrim_bin,
                                            age70_80yrs_Model_6_discrim_bin,
                                            age70_80yrs_Model_7_discrim_bin,
                                            age70_80yrs_Model_8_discrim_bin,
                                            age70_80yrs_Model_9_discrim_bin,
                                            age70_80yrs_Model_10_discrim_bin,
                                            age70_80yrs_Model_11_discrim_bin,
                                            age70_80yrs_Model_12_discrim_bin,
                                            age70_80yrs_Model_13_discrim_bin,
                                            age70_80yrs_Model_14_discrim_bin,
                                            age70_80yrs_Model_15_discrim_bin,
                                            age70_80yrs_Model_16_discrim_bin,
                                            age70_80yrs_Model_17_discrim_bin)

write.csv(age70_80yrs_results_discrim_bin_table_col, paste(OUTPUT_ROOT, "age70_80yrs_results_discrim_bin_table_col_ageism.csv", sep=""))

#####
#####

###### run all models for age80above 
age80above_Model_1_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age80above_dataset, 
                                                           Model_n = Model_1, 
                                                           Model_name = "Model_1")


age80above_Model_2_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age80above_dataset, 
                                                           Model_n = Model_2, 
                                                           Model_name = "Model_2")


age80above_Model_3_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age80above_dataset, 
                                                           Model_n = Model_3)



age80above_Model_4_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age80above_dataset, 
                                                           Model_n = Model_4)



age80above_Model_5_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age80above_dataset, 
                                                           Model_n = Model_5)


age80above_Model_6_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age80above_dataset, 
                                                           Model_n = Model_6)


age80above_Model_7_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age80above_dataset, 
                                                           Model_n = Model_7)



age80above_Model_8_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age80above_dataset, 
                                                           Model_n = Model_8)


age80above_Model_9_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age80above_dataset, 
                                                           Model_n = Model_9)


age80above_Model_10_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age80above_dataset, 
                                                            Model_n = Model_10)


age80above_Model_11_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age80above_dataset, 
                                                            Model_n = Model_11)


age80above_Model_12_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age80above_dataset, 
                                                            Model_n = Model_12)


age80above_Model_13_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age80above_dataset, 
                                                            Model_n = Model_13)


age80above_Model_14_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age80above_dataset, 
                                                            Model_n = Model_14)

age80above_Model_15_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age80above_dataset, 
                                                            Model_n = Model_15)

age80above_Model_16_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age80above_dataset, 
                                                            Model_n = Model_16)


age80above_Model_17_discrim_bin = discrim_bin_model_func_ageism(data_wce_subset = age80above_dataset, 
                                                            Model_n = Model_17)

age80above_results_discrim_bin = rbind(age80above_Model_1_discrim_bin,
                                   age80above_Model_2_discrim_bin,
                                   age80above_Model_3_discrim_bin,
                                   age80above_Model_4_discrim_bin,
                                   age80above_Model_5_discrim_bin,
                                   age80above_Model_6_discrim_bin,
                                   age80above_Model_7_discrim_bin,
                                   age80above_Model_8_discrim_bin,
                                   age80above_Model_9_discrim_bin,
                                   age80above_Model_10_discrim_bin,
                                   age80above_Model_11_discrim_bin,
                                   age80above_Model_12_discrim_bin,
                                   age80above_Model_13_discrim_bin,
                                   age80above_Model_14_discrim_bin,
                                   age80above_Model_15_discrim_bin,
                                   age80above_Model_16_discrim_bin,
                                   age80above_Model_17_discrim_bin)

write.csv(age80above_results_discrim_bin, paste(OUTPUT_ROOT, "age80above_results_discrim_bin_ageism.csv", sep=""))



age80above_results_discrim_bin_table_col= cbind(age80above_Model_1_discrim_bin,
                                            age80above_Model_2_discrim_bin,
                                            age80above_Model_3_discrim_bin,
                                            age80above_Model_4_discrim_bin,
                                            age80above_Model_5_discrim_bin,
                                            age80above_Model_6_discrim_bin,
                                            age80above_Model_7_discrim_bin,
                                            age80above_Model_8_discrim_bin,
                                            age80above_Model_9_discrim_bin,
                                            age80above_Model_10_discrim_bin,
                                            age80above_Model_11_discrim_bin,
                                            age80above_Model_12_discrim_bin,
                                            age80above_Model_13_discrim_bin,
                                            age80above_Model_14_discrim_bin,
                                            age80above_Model_15_discrim_bin,
                                            age80above_Model_16_discrim_bin,
                                            age80above_Model_17_discrim_bin)

write.csv(age80above_results_discrim_bin_table_col, paste(OUTPUT_ROOT, "age80above_results_discrim_bin_table_col_ageism.csv", sep=""))

#####
#####

