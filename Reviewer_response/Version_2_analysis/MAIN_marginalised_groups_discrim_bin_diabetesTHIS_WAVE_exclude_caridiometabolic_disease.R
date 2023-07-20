
#library(here)

#setwd('~/proj/Cumulative_effects_HRS/')

#here::i_am()
#current_directory = here()
#print(current_directory)
#print(file.path())

library(WCE)
library(survival)
library(dplyr)
library(car)
library(tidyverse)
#library(tidyr)

library(epiDisplay) #tab1 function to make a frequency table 
library(foreign)
library(rms) # Used to extract p_value from logistic model
#library(ggplot2) #plots 
library(corrplot)
library(gridExtra) 
library(sjPlot)
library(knitr)
library(lme4)
library(lattice)
library(Hmisc)


library(arsenal)

#library(riskRegression)


# plots: 
# https://adibender.github.io/pammtools/articles/cumulative_effects.html
#https://rpkgs.datanovia.com/ggpubr/reference/stat_regline_equation.html Adding text (coefficients etc) to the plot next to the regression line 

current_directory = "/Users/k2147340/OneDrive - King's College London/Documents/"

#current_directory = "/Users/aliya/my_docs/"
#current_directory = "/Users/aliyaamirova/proj/Cumulative_effects_HRS"

OUTPUT_ROOT = paste(current_directory, "proj/Cumulative_effects_HRS/Reviewer_response/Version_2_analysis/RESULTS/", sep="")
SOURCE_ROOT = paste(current_directory, "proj/Cumulative_effects_HRS/Reviewer_response/Version_2_analysis/", sep="")
DATA_ROOT = paste(current_directory, "/ELSA_HRS/Data_analysis/", sep = "") 

#DATA_ROOT = paste(current_directory, "KCL_postDoc/Data_analysis/", sep = "")

# function that subsets and srts dataset for a particular var (eg., female == 1)

#/Users/aliyaamirova/proj/Cumulative_effects_HRS/Version_2_analysis
source((paste(SOURCE_ROOT, "subset_func.R", sep="")))
source((paste(SOURCE_ROOT, "clean_recode_keyvars.R", sep="")))
#source((paste(SOURCE_ROOT, "subset_sort_BMI.R", sep="")))

#function that sorts out the data into a dataframe where each participant x wave pair is one row. Here I also add starting and stopping points to identify each wave
source((paste(SOURCE_ROOT, "sort_timepoints_drop_baseline.R", sep="")))

source((paste(SOURCE_ROOT, "clean_recode_sort.R", sep="")))

#function that runs WCE analysis
source((paste(SOURCE_ROOT, "summary_score_WCE_analysis.R", sep="")))
# function that samples bootstrapped CIs
source((paste(SOURCE_ROOT, "summary_score_Bootstrapped_CI.R", sep="")))
# function that runs WCE analysis and CIs sampling for a specified subset and with a specified model 
source((paste(SOURCE_ROOT, "HRs_CIs_analysis.R", sep="")))

source((paste(SOURCE_ROOT, "Seven_models_drop_baseline.R", sep="")))

source((paste(SOURCE_ROOT, "Seven_models_drop_baseline_no_sex.R", sep="")))

source((paste(SOURCE_ROOT, "Seven_models_drop_baseline_no_BMI.R", sep="")))

source((paste(SOURCE_ROOT, "p_value_func.R", sep="")))

#data 



HRS2008_data_initial = read.csv(paste(DATA_ROOT, "HRS_2008_data/HRS2008_data_short_education.csv", sep=""))
HRS2010_data_initial = read.csv(paste(DATA_ROOT, "HRS_2010_data/HRS2010_data_short_education.csv", sep=""))
HRS2012_data_initial = read.csv(paste(DATA_ROOT, "HRS_2012_data/HRS2012_data_short_education.csv", sep=""))
HRS2014_data_initial = read.csv(paste(DATA_ROOT, "HRS_2014_data/HRS2014_data_short_education.csv", sep=""))
HRS2016_data_initial = read.csv(paste(DATA_ROOT, "HRS_2016_data/HRS2016_data_short_education.csv", sep=""))
HRS2018_data_initial = read.csv(paste(DATA_ROOT, "HRS_2018_data/HRS2018_data_short_education.csv", sep=""))


HRS2008_data_initial_old = read.csv(paste(DATA_ROOT, "HRS_2008_data/HRS2008_data_short_OLD.csv", sep =""))

ls(HRS2008_data_initial_old)

table(HRS2010_data_initial$RAHISPAN)

#data 

HRS2008_data_intermediate = HRS2008_data_initial
HRS2010_data_intermediate = HRS2010_data_initial
HRS2012_data_intermediate = HRS2012_data_initial
HRS2014_data_intermediate = HRS2014_data_initial
HRS2016_data_intermediate = HRS2016_data_initial
HRS2018_data_intermediate = HRS2018_data_initial

nrow(HRS2008_data_intermediate) 
nrow(HRS2010_data_intermediate)
nrow(HRS2012_data_intermediate)
nrow(HRS2014_data_intermediate)
nrow(HRS2016_data_intermediate)
nrow(HRS2018_data_intermediate)


HRS2008_data_initial$HHIDPN
HRS2010_data_initial$HHIDPN
HRS2012_data_initial$HHIDPN
HRS2014_data_initial$HHIDPN
HRS2016_data_initial$HHIDPN
HRS2018_data_initial$HHIDPN



drop_var = c("X.2")
HRS2008_data = HRS2008_data_intermediate[ , !(names(HRS2008_data_intermediate) %in% drop_var)]

HRS2010_data = HRS2010_data_intermediate
HRS2012_data = HRS2012_data_intermediate
HRS2014_data = HRS2014_data_intermediate
HRS2016_data = HRS2016_data_intermediate
HRS2018_data = HRS2018_data_intermediate




summary(comparedf(x = HRS2008_data, y = HRS2018_data))


list_var_2008 = ls(HRS2008_data)
list_var_2018 = ls(HRS2018_data)




#########

Model = c(1, 2, 3, 4, 5, 6, 7)


# ####### # ####### # ####### # ####### EXPOSURE # ####### # ####### # ####### # #######
exposure = "discrim_bin"
outcome = "diabetes_new_bin"



# ####### # ####### # ####### # ####### OUTCOME # ####### # ####### # ####### # #######
##### DIAB DIAB DIAB DIAB DIAB DIAB 
##### DIAB DIAB DIAB DIAB DIAB DIAB 
# this is DIAB below (from older files, where diab this wave was coded as diabetes)


unique(HRS2008_data$diabetes_new) 
unique(HRS2010_data$diabetes_new) 
unique(HRS2012_data$diabetes_new)
unique(HRS2014_data$diabetes_new)
unique(HRS2016_data$diabetes_new)
unique(HRS2018_data$diabetes_new)

##### DIAB DIAB DIAB DIAB DIAB DIAB recode to DIAB_bin
##### DIAB DIAB DIAB DIAB DIAB DIAB 

HRS2008_data$diabetes_new_bin = case_when(HRS2008_data$diabetes_new == 1 ~ 1,
                                          HRS2008_data$diabetes_new == 0 ~ 0, 
                                          HRS2008_data$diabetes_new == 3 ~ 1, 
                                          HRS2008_data$diabetes_new == 4 ~ 0)    


HRS2010_data$diabetes_new_bin = case_when(HRS2010_data$diabetes_new == 1 ~ 1,
                                          HRS2010_data$diabetes_new == 0 ~ 0, 
                                          HRS2010_data$diabetes_new == 3 ~ 1, 
                                          HRS2010_data$diabetes_new == 4 ~ 0)   

##### DIAB DIAB DIAB DIAB DIAB DIAB 
##### DIAB DIAB DIAB DIAB DIAB DIAB 



HRS2012_data$diabetes_new_bin = case_when(HRS2012_data$diabetes_new == 1 ~ 1,
                                          HRS2012_data$diabetes_new == 0 ~ 0, 
                                          HRS2012_data$diabetes_new == 3 ~ 1, 
                                          HRS2012_data$diabetes_new == 4 ~ 0)

##### DIAB DIAB DIAB DIAB DIAB DIAB 
##### DIAB DIAB DIAB DIAB DIAB DIAB 



HRS2014_data$diabetes_new_bin = case_when(HRS2014_data$diabetes_new == 1 ~ 1,
                                          HRS2014_data$diabetes_new == 0 ~ 0, 
                                          HRS2014_data$diabetes_new == 3 ~ 1, 
                                          HRS2014_data$diabetes_new == 4 ~ 0) 

##### DIAB DIAB DIAB DIAB DIAB DIAB 
##### DIAB DIAB DIAB DIAB DIAB DIAB 


HRS2016_data$diabetes_new_bin = case_when(HRS2016_data$diabetes_new == 1 ~ 1,
                                          HRS2016_data$diabetes_new == 0 ~ 0, 
                                          HRS2016_data$diabetes_new == 3 ~ 1, 
                                          HRS2016_data$diabetes_new == 4 ~ 0) 
unique(HRS2016_data$diabetes_new_bin)


##### DIAB DIAB DIAB DIAB DIAB DIAB 
##### DIAB DIAB DIAB DIAB DIAB DIAB 

HRS2018_data$diabetes_new_bin = case_when(HRS2018_data$diabetes_new == 1 ~ 1,
                                          HRS2018_data$diabetes_new == 0 ~ 0, 
                                          HRS2018_data$diabetes_new == 3 ~ 1, 
                                          HRS2018_data$diabetes_new == 4 ~ 0) 

unique(HRS2018_data$diabetes_new_bin)

##### DIAB DIAB DIAB DIAB DIAB DIAB 
##### DIAB DIAB DIAB DIAB DIAB DIAB 
##### DIAB DIAB DIAB DIAB DIAB DIAB 
##### DIAB DIAB DIAB DIAB DIAB DIAB 
##### DIAB DIAB DIAB DIAB DIAB DIAB 
##### DIAB DIAB DIAB DIAB DIAB DIAB 

# this is DIAB below (from older files, where diab this wave was coded as diabetes)
#outcome = "diabetes_new_bin"


HRS2008_data_initial$angina_new_bin
unique(HRS2012_data_initial$diabetes_new_bin)
unique(HRS2008_data_initial$stroke_new_bin) 
HRS2008_data_initial$heartcondition_new_bin
HRS2008_data_initial$heartfailure2yrs_bin
HRS2008_data_initial$heartattack_new_bin
HRS2008_data_initial$smokes_now_bin
unique(HRS2008_data_initial$vigarious_physical_activity)
HRS2008_data_initial$alcohol_days_week
HRS2008_data_initial$checklist_depression_bin


all_discrim_bin_diabetes_thisWAVE_7models = Seven_models_drop_baseline(subset_var1 = "NA",
                                                                       subset_value1 = "NA",
                                                                       
                                                                       subset_BMI = "NA",
                                                                       subset_BMI_value  = "NA",
                                                                       
                                                                       subset_var2 = "NA",
                                                                       subset_value2 = "NA",
                                                                       
                                                                       subset_var3= "NA",
                                                                       subset_value3 = "NA",
                                                                       
                                                                       subset_name = "ALL",
                                                                       
                                                                       subset_reason1 = "NA",
                                                                       subset_reason1_value = "NA",
                                                                       
                                                                       subset_reason2 = "NA",
                                                                       subset_reason2_value =  "NA",
                                                                       
                                                                       
                                                                       subset_reason3 = "NA",
                                                                       subset_reason3_value = "NA",
                                                                       
                                                                       HRS2008_data = HRS2008_data,
                                                                       HRS2010_data = HRS2010_data,
                                                                       HRS2012_data = HRS2012_data,
                                                                       HRS2014_data = HRS2014_data,
                                                                       HRS2016_data = HRS2016_data,
                                                                       HRS2018_data = HRS2018_data,
                                                                       
                                                                       exposure = exposure,
                                                                       outcome = outcome)



write.csv(all_discrim_bin_diabetes_thisWAVE_7models, paste(OUTPUT_ROOT, "all_discrim_bin_diabetes_new_bin_race_education_exclude_cardiometabolic_disorder.csv", sep=""))


all_results = read.csv(paste(OUTPUT_ROOT, "all_discrim_bin_diabetes_new_bin_race_education_exclude_cardiometabolic_disorder.csv", sep=""))


results = rbind(all_results[1:7,]) 

write.csv(results, paste(OUTPUT_ROOT, "result_table_diabetes_new_bin_race_education_exclude_cardiometabolic_disorder.csv", sep=""))


#  all_results = read.csv(paste(OUTPUT_ROOT, "All_clean_data_HRsonly_diabetes_new_bin.csv", sep=""))
#  female_results =  read.csv(paste(OUTPUT_ROOT, "Female_clean_data_HRsonly_diabetes_new_bin.csv", sep=""))
#  race_results = read.csv(paste(OUTPUT_ROOT, "Race_clean_data_HRsonly_diabetes_new_bin.csv", sep=""))
#  combo_results = read.csv(paste(OUTPUT_ROOT, "Combo_clean_data_HRsonly_diabetes_new_bin.csv", sep=""))
#  BMI_results = read.csv(paste(OUTPUT_ROOT, "BMI_clean_data_HRsonly_diabetes_new_bin.csv", sep=""))
# 
# Model = c(1, 2, 3, 4, 5, 6, 7)
# 
# 
# 
# all_results = cbind(Model, 
#                     all_results)
# 
# 
# female_results = cbind(Model, 
#                     female_results)
# 
# 
# race_results = cbind(Model, 
#                     race_results)
# 
# combo_results = cbind(Model, 
#                     combo_results)
# 
# BMI_results = cbind(Model, 
#                     BMI_results)
# 
# 
# results = rbind(all_results[1:7,],
#                 female_results[1:7,],
#                 race_results[1:7,],
#                 combo_results[1:7,],
#                 BMI_results[1:7,]) 
# 
# Model_1= subset(results, results$Model == 1)
# Model_2= subset(results, results$Model == 2)
# Model_3= subset(results, results$Model == 3)
# Model_4= subset(results, results$Model == 4)
# Model_5= subset(results, results$Model == 5)
# Model_6= subset(results, results$Model == 6)
# Model_7= subset(results, results$Model == 7)
# 
# results_col = cbind(Model_1, 
#                     Model_2, 
#                     Model_3, 
#                     Model_4, 
#                     Model_5, 
#                     Model_6, 
#                     Model_7)
# 
# results_col = results_col[,c(3:10,18:20, 28:30, 38:40, 48:50, 58:60, 68:70)]
# write.csv(results_col, paste(OUTPUT_ROOT, "done/result_table_diabetes_new_bin.csv", sep=""))
# 
# 
