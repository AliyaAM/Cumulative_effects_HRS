
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



###### run all models for religion other than protestant or catholic 
religion_Model_1_discrim_bin = discrim_bin_model_func(subset_var = "religion_bin", 
                                                      subset_value = 1, 
                                                      Model_n = Model_1, 
                                                      Model_name = "Model_1")


religion_Model_2_discrim_bin = discrim_bin_model_func(subset_var = "religion_bin", 
                                                      subset_value = 1, 
                                                      Model_n = Model_2, 
                                                      Model_name = "Model_2")


religion_Model_3_discrim_bin = discrim_bin_model_func(subset_var = "religion_bin", 
                                                      subset_value = 1, 
                                                      Model_n = Model_3)



religion_Model_4_discrim_bin = discrim_bin_model_func(subset_var = "religion_bin", 
                                                      subset_value = 1, 
                                                      Model_n = Model_4)



religion_Model_5_discrim_bin = discrim_bin_model_func(subset_var = "religion_bin", 
                                                      subset_value = 1, 
                                                      Model_n = Model_5)


religion_Model_6_discrim_bin = discrim_bin_model_func(subset_var = "religion_bin", 
                                                      subset_value = 1, 
                                                      Model_n = Model_6)


religion_Model_7_discrim_bin = discrim_bin_model_func(subset_var = "religion_bin", 
                                                      subset_value = 1, 
                                                      Model_n = Model_7)



religion_Model_8_discrim_bin = discrim_bin_model_func(subset_var = "religion_bin", 
                                                      subset_value = 1, 
                                                      Model_n = Model_8)


religion_Model_9_discrim_bin = discrim_bin_model_func(subset_var = "religion_bin", 
                                                      subset_value = 1, 
                                                      Model_n = Model_9)


religion_Model_10_discrim_bin = discrim_bin_model_func(subset_var = "religion_bin", 
                                                      subset_value = 1, 
                                                      Model_n = Model_10)


religion_Model_11_discrim_bin = discrim_bin_model_func(subset_var = "religion_bin", 
                                                       subset_value = 1, 
                                                       Model_n = Model_11)


religion_Model_12_discrim_bin = discrim_bin_model_func(subset_var = "religion_bin", 
                                                       subset_value = 1, 
                                                       Model_n = Model_12)


religion_Model_13_discrim_bin = discrim_bin_model_func(subset_var = "religion_bin", 
                                                       subset_value = 1, 
                                                       Model_n = Model_13)


religion_Model_14_discrim_bin = discrim_bin_model_func(subset_var = "religion_bin", 
                                                       subset_value = 1, 
                                                       Model_n = Model_14)

religion_Model_15_discrim_bin = discrim_bin_model_func(subset_var = "religion_bin", 
                                                       subset_value = 1, 
                                                       Model_n = Model_15)

religion_Model_16_discrim_bin = discrim_bin_model_func(subset_var = "religion_bin", 
                                                       subset_value = 1, 
                                                       Model_n = Model_16)


religion_Model_17_discrim_bin = discrim_bin_model_func(subset_var = "religion_bin", 
                                                       subset_value = 1, 
                                                       Model_n = Model_17)
