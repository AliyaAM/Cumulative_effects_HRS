
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

# function that subsets and srts dataset for a particular var (eg., female == 1)
source((paste(SOURCE_ROOT, "subset_sort.R", sep="")))

source((paste(SOURCE_ROOT, "subset_sort_version_2.R", sep="")))


source((paste(SOURCE_ROOT, "subset_sort_BMI.R", sep="")))

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


##### 1. •race + national origin• 2. race + religion. 3. national origin + religion, 4. •national origin + religion + race•

#self-reported
#national_origin_dataset = subset_sort(subset_var = "national_origin_ousideUS_bin", 
 #                                     subset_value = 1) 

#self-reported
#race_dataset = subset_sort(subset_var = "race_white", 
 #                          subset_value = 0) 




national_race_dataset = subset_sort_version_2(subset_var1 = "race_white", 
                      subset_value1 = 0,
                      subset_var2 = "national_origin_ousideUS_bin", 
                      subset_value2 = 1)
#self-reported
#religion_dataset = subset_sort(subset_var = "religion_bin", 
#                               subset_value = 1) 



###### run all models for national_origin 
national_origin_Model_1_discrim_bin = discrim_bin_model_func(data_wce_subset = national_race_dataset, 
                                                             Model_n = Model_1, 
                                                             Model_name = "Model_1")


national_origin_Model_2_discrim_bin = discrim_bin_model_func(data_wce_subset = national_race_dataset, 
                                                             Model_n = Model_2, 
                                                             Model_name = "Model_2")


national_origin_Model_3_discrim_bin = discrim_bin_model_func(data_wce_subset = national_race_dataset, 
                                                             Model_n = Model_3)



national_origin_Model_4_discrim_bin = discrim_bin_model_func(data_wce_subset = national_race_dataset, 
                                                             Model_n = Model_4)



national_origin_Model_5_discrim_bin = discrim_bin_model_func(data_wce_subset = national_race_dataset, 
                                                             Model_n = Model_5)


national_origin_Model_6_discrim_bin = discrim_bin_model_func(data_wce_subset = national_race_dataset, 
                                                             Model_n = Model_6)


national_origin_Model_7_discrim_bin = discrim_bin_model_func(data_wce_subset = national_race_dataset, 
                                                             Model_n = Model_7)



national_origin_Model_8_discrim_bin = discrim_bin_model_func(data_wce_subset = national_race_dataset, 
                                                             Model_n = Model_8)


national_origin_Model_9_discrim_bin = discrim_bin_model_func(data_wce_subset = national_race_dataset, 
                                                             Model_n = Model_9)


national_origin_Model_10_discrim_bin = discrim_bin_model_func(data_wce_subset = national_race_dataset, 
                                                              Model_n = Model_10)


national_origin_Model_11_discrim_bin = discrim_bin_model_func(data_wce_subset = national_race_dataset, 
                                                      Model_n = Model_11)

national_origin_Model_11_discrim_bin = cbind(NA, NA, NA)
colnames(national_origin_Model_11_discrim_bin) = c("hazard ratio",    "5% CI",  "95% CI") 

national_origin_Model_12_discrim_bin = discrim_bin_model_func(data_wce_subset = national_race_dataset, 
                                                      Model_n = Model_12)


national_origin_Model_12_discrim_bin = cbind(NA, NA, NA)
colnames(national_origin_Model_12_discrim_bin) = c("hazard ratio",    "5% CI",  "95% CI") 

national_origin_Model_13_discrim_bin = discrim_bin_model_func(data_wce_subset = national_race_dataset, 
                                                              Model_n = Model_13)


national_origin_Model_14_discrim_bin = discrim_bin_model_func(data_wce_subset = national_race_dataset, 
                                                              Model_n = Model_14)

national_origin_Model_15_discrim_bin = discrim_bin_model_func(data_wce_subset = national_race_dataset, 
                                                              Model_n = Model_15)

national_origin_Model_16_discrim_bin = discrim_bin_model_func(data_wce_subset = national_race_dataset, 
                                                              Model_n = Model_16)


national_origin_Model_17_discrim_bin = discrim_bin_model_func(data_wce_subset = national_race_dataset, 
                                                     Model_n = Model_17)


national_origin_Model_17_discrim_bin = cbind(NA, NA, NA)
colnames(national_origin_Model_17_discrim_bin) = c("hazard ratio",    "5% CI",  "95% CI") 


national_AND_race_results_discrim_bin = rbind(national_origin_Model_1_discrim_bin,
                                            national_origin_Model_2_discrim_bin,
                                            national_origin_Model_3_discrim_bin,
                                            national_origin_Model_4_discrim_bin,
                                            national_origin_Model_5_discrim_bin,
                                            national_origin_Model_6_discrim_bin,
                                            national_origin_Model_7_discrim_bin,
                                            national_origin_Model_8_discrim_bin,
                                            national_origin_Model_9_discrim_bin,
                                            national_origin_Model_10_discrim_bin,
                                            national_origin_Model_11_discrim_bin,
                                            national_origin_Model_12_discrim_bin,
                                            national_origin_Model_13_discrim_bin,
                                            national_origin_Model_14_discrim_bin,
                                            national_origin_Model_15_discrim_bin,
                                            national_origin_Model_16_discrim_bin,
                                            national_origin_Model_17_discrim_bin)




write.csv(national_AND_race_results_discrim_bin, paste(OUTPUT_ROOT, "national_AND_race_results_discrim_bin.csv", sep=""))


national_AND_race_results_discrim_bin_table_col = cbind(national_origin_Model_1_discrim_bin,
                                                      national_origin_Model_2_discrim_bin,
                                                      national_origin_Model_3_discrim_bin,
                                                      national_origin_Model_4_discrim_bin,
                                                      national_origin_Model_5_discrim_bin,
                                                      national_origin_Model_6_discrim_bin,
                                                      national_origin_Model_7_discrim_bin,
                                                      national_origin_Model_8_discrim_bin,
                                                      national_origin_Model_9_discrim_bin,
                                                      national_origin_Model_10_discrim_bin,
                                                      national_origin_Model_11_discrim_bin,
                                                      national_origin_Model_12_discrim_bin,
                                                      national_origin_Model_13_discrim_bin,
                                                      national_origin_Model_14_discrim_bin,
                                                      national_origin_Model_15_discrim_bin,
                                                      national_origin_Model_16_discrim_bin,
                                                      national_origin_Model_17_discrim_bin)

write.csv(national_AND_race_results_discrim_bin_table_col, paste(OUTPUT_ROOT, "national_AND_race_results_discrim_bin_table_col.csv", sep=""))


