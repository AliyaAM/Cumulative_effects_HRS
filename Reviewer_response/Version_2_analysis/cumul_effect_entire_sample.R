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

source((paste(SOURCE_ROOT, "sort_only.R", sep="")))

source((paste(SOURCE_ROOT, "subset_2008_baseline.R", sep="")))

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


#self-reported
#HRS2008_data_subset = subset(HRS2008_data, HRS2008_data[ , subset_var1] >= subset_value1 | HRS2008_data[ , subset_var2] <= subset_value2)
# 1 group 

#reason_discrim1_reason_age

entire_dataset = sort_only(entire_dataset = 1) 

age_Model_1_discrim_bin = discrim_bin_model_func(data_wce_subset = entire_dataset, 
                                                 Model_n = Model_1, 
                                                 Model_name = "Model_1")



age_Model_2_discrim_bin = discrim_bin_model_func(data_wce_subset = entire_dataset, 
                                                 Model_n = Model_2, 
                                                 Model_name = "Model_2")


age_Model_3_discrim_bin = discrim_bin_model_func(data_wce_subset = entire_dataset, 
                                                 Model_n = Model_3)



age_Model_4_discrim_bin = discrim_bin_model_func(data_wce_subset = entire_dataset, 
                                                 Model_n = Model_4)



age_Model_5_discrim_bin = discrim_bin_model_func(data_wce_subset = entire_dataset, 
                                                 Model_n = Model_5)


age_Model_6_discrim_bin = discrim_bin_model_func(data_wce_subset = entire_dataset, 
                                                 Model_n = Model_6)


age_Model_7_discrim_bin = discrim_bin_model_func(data_wce_subset = entire_dataset, 
                                                 Model_n = Model_7)



entire_sample_discrim_bin = rbind(age_Model_1_discrim_bin,
                                age_Model_2_discrim_bin,
                                age_Model_3_discrim_bin,
                                age_Model_4_discrim_bin,
                                age_Model_5_discrim_bin,
                                age_Model_6_discrim_bin,
                                age_Model_7_discrim_bin)

write.csv(entire_sample_discrim_bin, paste(OUTPUT_ROOT, "entire_dataset_discrim_bin_V2.csv", sep=""))



entire_sample_discrim_bin_table_col= cbind(age_Model_1_discrim_bin,
                                         age_Model_2_discrim_bin,
                                         age_Model_3_discrim_bin,
                                         age_Model_4_discrim_bin,
                                         age_Model_5_discrim_bin,
                                         age_Model_6_discrim_bin,
                                         age_Model_7_discrim_bin)

write.csv(entire_sample_discrim_bin_table_col, paste(OUTPUT_ROOT, "entire_dataset_discrim_bin_table_col_V2.csv", sep=""))


#########

#
#####