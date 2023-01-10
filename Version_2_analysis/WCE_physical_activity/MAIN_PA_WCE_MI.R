
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

library(riskRegression)


# plots: 
# https://adibender.github.io/pammtools/articles/cumulative_effects.html
#https://rpkgs.datanovia.com/ggpubr/reference/stat_regline_equation.html Adding text (coefficients etc) to the plot next to the regression line 


#current_directory = "/Users/aliya/my_docs/"
current_directory = "/Users/aliyaamirova/"

OUTPUT_ROOT =(paste(current_directory, "proj/Cumulative_effects_HRS/Results/myocardial_PA/", sep=""))
SOURCE_ROOT = (paste(current_directory, "proj/Cumulative_effects_HRS/Version_2_analysis/", sep=""))
DATAIN_ROOT = (paste(current_directory, "Documents/KCL_postDoc/Data_analysis/", sep="")) 

#/Users/aliyaamirova/proj/Cumulative_effects_HRS/Version_2_analysis

#subset func is subsetting to people with particular BMI or female or ethnic minority
# function that subsets and srts dataset for a particular var (eg., female == 1)

source((paste(SOURCE_ROOT, "subset_func.R", sep="")))

source((paste(SOURCE_ROOT, "WCE_physical_activity/clean_recode_keyvars_MI.R", sep="")))
#source((paste(SOURCE_ROOT, "subset_sort_BMI.R", sep="")))

#function that sorts out the data into a dataframe where each participant x wave pair is one row. Here I also add starting and stopping points to identify each wave
#drop baseline MI cases: 

source((paste(SOURCE_ROOT, "WCE_physical_activity/sort_timepoints_drop_bsln_MI.R", sep="")))

source((paste(SOURCE_ROOT, "clean_recode_sort.R", sep="")))

#function that runs WCE analysis
source((paste(SOURCE_ROOT, "summary_score_WCE_analysis.R", sep="")))
# function that samples bootstrapped CIs
source((paste(SOURCE_ROOT, "WCE_physical_activity/summary_score_Bootstrapped_CI_MI.R", sep="")))
# function that runs WCE analysis and CIs sampling for a specified subset and with a specified model 
source((paste(SOURCE_ROOT, "HRs_CIs_analysis.R", sep="")))

source((paste(SOURCE_ROOT, "Seven_models_drop_baseline.R", sep="")))

source((paste(SOURCE_ROOT, "Seven_models_drop_baseline_no_sex.R", sep="")))

source((paste(SOURCE_ROOT, "Seven_models_drop_baseline_no_BMI.R", sep="")))

source((paste(SOURCE_ROOT, "p_value_func.R", sep="")))

#data 

print("more data is available in all_HRS_by_years_PGS, but then it is not in the right format for the WCE, chexk later")

HRS_2008_data_with_PGS = read.csv(paste(DATAIN_ROOT, "HRS_2008_data/HRS_2008_data_with_PGS.csv", sep=""))
all_HRS_by_years_PGS = read.csv(paste(DATAIN_ROOT, "HRS_2008_data/all_HRS_by_years_PGS.csv", sep=""))


HRS2008_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2008_data/HRS2008_data_short.csv", sep=""))
HRS2010_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2010_data/HRS2010_data_short.csv", sep=""))
HRS2012_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2012_data/HRS2012_data_short.csv", sep=""))
HRS2014_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2014_data/HRS2014_data_short.csv", sep=""))
HRS2016_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2016_data/HRS2016_data_short.csv", sep=""))
HRS2018_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2018_data/HRS2018_data_short.csv", sep=""))

########
#data 

HRS2008_data_intermediate = HRS2008_data_initial
HRS2010_data_intermediate = HRS2010_data_initial
HRS2012_data_intermediate = HRS2012_data_initial
HRS2014_data_intermediate = HRS2014_data_initial
HRS2016_data_intermediate = HRS2016_data_initial
HRS2018_data_intermediate = HRS2018_data_initial



HRS2008_data = HRS2008_data_intermediate
HRS2010_data = HRS2010_data_intermediate
HRS2012_data = HRS2012_data_intermediate
HRS2014_data = HRS2014_data_intermediate
HRS2016_data = HRS2016_data_intermediate
HRS2018_data = HRS2018_data_intermediate

#########

Model = c(1, 2, 3, 4, 5, 6, 7)


# ####### # ####### # ####### # ####### EXPOSURE # ####### # ####### # ####### # #######
print("change the name of exposure to the outcome of interest below")


print("PA (reverse below, so: 5-point Likert scale: 5 - every day; 4 - more than once per week; 3 -  once per week; 2 - once-three times per month; 1 - never")

exposure = "vigarious_physical_activity_bin"
outcome = "heartattack_new_bin"



print("recode so it is: (1) sedentary vs mild, (2) sedentary vs vagarious, (3) mild vs vagarious")


############### EXPOSURE ############### 
HRS2008_data$vigarious_physical_activity_bin = case_when(HRS2008_data$vigarious_physical_activity == 1 ~ 1,
                                                         HRS2008_data$vigarious_physical_activity == 2 ~ 1, 
                                                         HRS2008_data$vigarious_physical_activity == 3 ~ 0, 
                                                         HRS2008_data$vigarious_physical_activity == 4 ~ 0,
                                                         HRS2008_data$vigarious_physical_activity == 5 ~ 0)    


HRS2010_data$vigarious_physical_activity_bin = case_when(HRS2010_data$vigarious_physical_activity == 1 ~ 1,
                                                         HRS2010_data$vigarious_physical_activity == 2 ~ 1, 
                                                         HRS2010_data$vigarious_physical_activity == 3 ~ 0, 
                                                         HRS2010_data$vigarious_physical_activity == 4 ~ 0,
                                                         HRS2010_data$vigarious_physical_activity == 5 ~ 0)   

##### MI MI MI MI MI MI 
##### MI MI MI MI MI MI 


HRS2012_data$vigarious_physical_activity_bin = case_when(HRS2012_data$vigarious_physical_activity == 1 ~ 1,
                                                         HRS2012_data$vigarious_physical_activity == 2 ~ 1, 
                                                         HRS2012_data$vigarious_physical_activity == 3 ~ 0, 
                                                         HRS2012_data$vigarious_physical_activity == 4 ~ 0,
                                                         HRS2012_data$vigarious_physical_activity == 5 ~ 0)

##### MI MI MI MI MI MI 
##### MI MI MI MI MI MI 



HRS2014_data$vigarious_physical_activity_bin = case_when(HRS2014_data$vigarious_physical_activity == 1 ~ 1,
                                                         HRS2014_data$vigarious_physical_activity == 2 ~ 1, 
                                                         HRS2014_data$vigarious_physical_activity == 3 ~ 0, 
                                                         HRS2014_data$vigarious_physical_activity == 4 ~ 0,
                                                         HRS2014_data$vigarious_physical_activity == 5 ~ 0) 

##### MI MI MI MI MI MI 
##### MI MI MI MI MI MI 


HRS2016_data$vigarious_physical_activity_bin = case_when(HRS2016_data$vigarious_physical_activity == 1 ~ 1,
                                                         HRS2016_data$vigarious_physical_activity == 2 ~ 1, 
                                                         HRS2016_data$vigarious_physical_activity == 3 ~ 0, 
                                                         HRS2016_data$vigarious_physical_activity == 4 ~ 0,
                                                         HRS2016_data$vigarious_physical_activity == 5 ~ 0) 

unique(HRS2016_data$vigarious_physical_activity_bin)


##### MI MI MI MI MI MI 
##### MI MI MI MI MI MI 

HRS2018_data$vigarious_physical_activity_bin = case_when(HRS2018_data$vigarious_physical_activity == 1 ~ 1,
                                                         HRS2018_data$vigarious_physical_activity == 2 ~ 1, 
                                                         HRS2018_data$vigarious_physical_activity == 3 ~ 0, 
                                                         HRS2018_data$vigarious_physical_activity == 4 ~ 0,
                                                         HRS2018_data$vigarious_physical_activity == 5 ~ 0) 

unique(HRS2018_data$vigarious_physical_activity_bin)

# recode so it is: (1) sedentary vs mild, (2) sedentary vs vagarious, (3) mild vs vagarious

unique(HRS2016_data$vigarious_physical_activity_bin)
#1  2  3  5  4 NA  1

###############
###############


# ####### # ####### # ####### # ####### OUTCOME # ####### # ####### # ####### # #######
##### MI MI MI MI MI MI 
##### MI MI MI MI MI MI 
# this is MI below (from older files, where MI this wave was coded as myocardial)


unique(HRS2008_data$heartattack_new_bin) 
unique(HRS2010_data$heartattack_new_bin) 
unique(HRS2012_data$heartattack_new_bin)
unique(HRS2014_data$heartattack_new_bin)
unique(HRS2016_data$heartattack_new_bin)
unique(HRS2018_data$heartattack_new_bin)

##### MI MI MI MI MI MI recode to MI_bin
##### MI MI MI MI MI MI 


print("change the name below var for each year's MI outcome: called YEAR_mi_bin in the all_HRS_by_years_PGS file")


HRS2008_data$heartattack_new_bin = case_when(HRS2008_data$heartattack_new_bin == 1 ~ 1,
                                          HRS2008_data$heartattack_new_bin == 0 ~ 0, 
                                          HRS2008_data$heartattack_new_bin == 3 ~ 1, 
                                          HRS2008_data$heartattack_new_bin == 4 ~ 0)    


HRS2010_data$heartattack_new_bin = case_when(HRS2010_data$heartattack_new_bin == 1 ~ 1,
                                          HRS2010_data$heartattack_new_bin == 0 ~ 0, 
                                          HRS2010_data$heartattack_new_bin == 3 ~ 1, 
                                          HRS2010_data$heartattack_new_bin == 4 ~ 0)   

##### MI MI MI MI MI MI 
##### MI MI MI MI MI MI 



HRS2012_data$heartattack_new_bin = case_when(HRS2012_data$heartattack_new_bin == 1 ~ 1,
                                          HRS2012_data$heartattack_new_bin == 0 ~ 0, 
                                          HRS2012_data$heartattack_new_bin == 3 ~ 1, 
                                          HRS2012_data$heartattack_new_bin == 4 ~ 0)

##### MI MI MI MI MI MI 
##### MI MI MI MI MI MI 



HRS2014_data$heartattack_new_bin = case_when(HRS2014_data$heartattack_new_bin == 1 ~ 1,
                                          HRS2014_data$heartattack_new_bin == 0 ~ 0, 
                                          HRS2014_data$heartattack_new_bin == 3 ~ 1, 
                                          HRS2014_data$heartattack_new_bin == 4 ~ 0) 

##### MI MI MI MI MI MI 
##### MI MI MI MI MI MI 


HRS2016_data$heartattack_new_bin = case_when(HRS2016_data$heartattack_new_bin == 1 ~ 1,
                                          HRS2016_data$heartattack_new_bin == 0 ~ 0, 
                                          HRS2016_data$heartattack_new_bin == 3 ~ 1, 
                                          HRS2016_data$heartattack_new_bin == 4 ~ 0) 
unique(HRS2016_data$heartattack_new_bin)


##### MI MI MI MI MI MI 
##### MI MI MI MI MI MI 

HRS2018_data$heartattack_new_bin = case_when(HRS2018_data$heartattack_new_bin == 1 ~ 1,
                                          HRS2018_data$heartattack_new_bin == 0 ~ 0, 
                                          HRS2018_data$heartattack_new_bin == 3 ~ 1, 
                                          HRS2018_data$heartattack_new_bin == 4 ~ 0) 

unique(HRS2018_data$heartattack_new_bin)

##### MI MI MI MI MI MI 
##### MI MI MI MI MI MI 
##### MI MI MI MI MI MI 
##### MI MI MI MI MI MI 
##### MI MI MI MI MI MI 
##### MI MI MI MI MI MI 

# this is MI below (from older files, where MI this wave was coded as myocardial)
#outcome = "heartattack_new_bin"


HRS2008_data_initial$angina_new_bin
unique(HRS2012_data_initial$heartattack_new_bin)
unique(HRS2008_data_initial$stroke_new_bin) 
HRS2008_data_initial$heartcondition_new_bin
HRS2008_data_initial$heartfailure2yrs_bin
HRS2008_data_initial$heartattack_new_bin
HRS2008_data_initial$smokes_now_bin
unique(HRS2008_data_initial$vigarious_physical_activity)
HRS2008_data_initial$alcohol_days_week
HRS2008_data_initial$checklist_depression_bin


all_vigarious_physical_activity_myocardial_thisWAVE_7models = Seven_models_drop_baseline(subset_var1 = "NA",
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



write.csv(all_vigarious_physical_activity_myocardial_thisWAVE_7models, paste(OUTPUT_ROOT, "all_vigarious_physical_activity_heartattack_new_bin.csv", sep=""))


# all_vigarious_physical_activity_myocardial_thisWAVE_7models_pvalues = p_value_func(data = all_vigarious_physical_activity_myocardial_thisWAVE_7models,
#                                                             subset_name = "All",
#                                                             Model = Model)


female_vigarious_physical_activity_myocardial_thisWAVE_7models = Seven_models_drop_baseline_no_sex(subset_var1 = "sex_1_2",
                                                                                 subset_value1 = 1,
                                                                                 
                                                                                 
                                                                                 subset_BMI = "NA",
                                                                                 subset_BMI_value  = "NA",
                                                                                 
                                                                                 subset_var2 = "NA",
                                                                                 subset_value2 = "NA",
                                                                                 
                                                                                 subset_var3= "NA",
                                                                                 subset_value3 = "NA",
                                                                                 
                                                                                 subset_reason1 = "NA",
                                                                                 subset_reason1_value = "NA",
                                                                                 
                                                                                 subset_reason2 = "NA",
                                                                                 subset_reason2_value =  "NA",
                                                                                 
                                                                                 
                                                                                 subset_reason3 = "NA",
                                                                                 subset_reason3_value = "NA",
                                                                                 
                                                                                 subset_name = "FEMALE",
                                                                                 
                                                                                 HRS2008_data = HRS2008_data,
                                                                                 HRS2010_data = HRS2010_data,
                                                                                 HRS2012_data = HRS2012_data,
                                                                                 HRS2014_data = HRS2014_data,
                                                                                 HRS2016_data = HRS2016_data,
                                                                                 HRS2018_data = HRS2018_data,
                                                                                 
                                                                                 exposure = exposure,
                                                                                 outcome = outcome)



write.csv(female_vigarious_physical_activity_myocardial_thisWAVE_7models, paste(OUTPUT_ROOT, "female_vigarious_physical_activity_heartattack_new_bin.csv", sep=""))
# 
# 
# 
# # female_vigarious_physical_activity_myocardial_thisWAVE_7models_pvalues = p_value_func(data = female_vigarious_physical_activity_myocardial_thisWAVE_7models,
# #                                                                subset_name = "Female", 
# #                                                                Model = Model)
# 
# 
# 
# 
# 
# race_vigarious_physical_activity_myocardial_thisWAVE_7models = Seven_models_drop_baseline(subset_var1 = "race_white", 
#                                                      subset_value1 = 0, 
#                                                      
#                                                      subset_BMI = "NA", 
#                                                      subset_BMI_value  = "NA", 
#                                                      
#                                                      subset_var2 = "NA", 
#                                                      subset_value2 = "NA",  
#                                                      
#                                                      subset_var3= "NA", 
#                                                      subset_value3 = "NA", 
#                                                      
#                                                      subset_reason1 = "NA", 
#                                                      subset_reason1_value = "NA", 
#                                                      
#                                                      subset_reason2 = "NA", 
#                                                      subset_reason2_value =  "NA", 
#                                                      
#                                                      
#                                                      subset_reason3 = "NA", 
#                                                      subset_reason3_value = "NA", 
#                                                      
#                                                      subset_name = "RACE", 
#                                                      
#                                                      HRS2008_data = HRS2008_data, 
#                                                      HRS2010_data = HRS2010_data, 
#                                                      HRS2012_data = HRS2012_data, 
#                                                      HRS2014_data = HRS2014_data, 
#                                                      HRS2016_data = HRS2016_data, 
#                                                      HRS2018_data = HRS2018_data, 
#                                                      
#                                                      exposure = exposure, 
#                                                      outcome = outcome) 
# 
# 
# 
# 
# write.csv(race_vigarious_physical_activity_myocardial_thisWAVE_7models, paste(OUTPUT_ROOT, "race_vigarious_physical_activity_heartattack_new_bin.csv", sep=""))
# 
# 
# 
# # race_vigarious_physical_activity_myocardial_thisWAVE_7models_pvalues = p_value_func(data = race_vigarious_physical_activity_myocardial_thisWAVE_7models,
# #                                                              subset_name = "Race", 
# #                                                              Model = Model)
# 
# 
# 
# combo_vigarious_physical_activity_myocardial_thisWAVE_7models = Seven_models_drop_baseline(subset_var1 = "race_white", 
#                                                       subset_value1 = 0, 
#                                                       
#                                                       subset_BMI = "NA", 
#                                                       subset_BMI_value  = "NA", 
#                                                       
#                                                       subset_var2 = "religion_bin", 
#                                                       subset_value2 = 1,  
#                                                       
#                                                       subset_var3= "national_origin_ousideUS_bin", 
#                                                       subset_value3 = 1, 
#                                                       
#                                                       subset_reason1 = "NA", 
#                                                       subset_reason1_value = "NA", 
#                                                       
#                                                       subset_reason2 = "NA", 
#                                                       subset_reason2_value =  "NA", 
#                                                       
#                                                       
#                                                       subset_reason3 = "NA", 
#                                                       subset_reason3_value = "NA", 
#                                                       
#                                                       subset_name = "COMBO", 
#                                                       
#                                                       HRS2008_data = HRS2008_data, 
#                                                       HRS2010_data = HRS2010_data, 
#                                                       HRS2012_data = HRS2012_data, 
#                                                       HRS2014_data = HRS2014_data, 
#                                                       HRS2016_data = HRS2016_data, 
#                                                       HRS2018_data = HRS2018_data, 
#                                                       
#                                                       exposure = exposure, 
#                                                       outcome = outcome) 
# 
# 
# 
# 
# write.csv(combo_vigarious_physical_activity_myocardial_thisWAVE_7models, paste(OUTPUT_ROOT, "combo_vigarious_physical_activity_heartattack_new_bin.csv", sep=""))
# 
# 
# 
# 
# # Combo_vigarious_physical_activity_myocardial_thisWAVE_7models_pvalues = p_value_func(data = combo_vigarious_physical_activity_myocardial_thisWAVE_7models,
# #                                                               subset_name = "Combo", 
# #                                                               Model = Model)


BMI_vigarious_physical_activity_myocardial_thisWAVE_7models = Seven_models_drop_baseline_no_BMI(subset_var1 = "NA", 
                                                                              subset_value1 = "NA", 
                                                                              
                                                                              subset_BMI = "assessed_BMI", 
                                                                              subset_BMI_value = 30, 
                                                                              
                                                                              subset_var2 = "NA", 
                                                                              subset_value2 = "NA", 
                                                                              
                                                                              subset_var3= "NA", 
                                                                              subset_value3 = "NA", 
                                                                              
                                                                              subset_reason1 = "NA", 
                                                                              subset_reason1_value = "NA", 
                                                                              
                                                                              subset_reason2 = "NA", 
                                                                              subset_reason2_value =  "NA", 
                                                                              
                                                                              
                                                                              subset_reason3 = "NA", 
                                                                              subset_reason3_value = "NA", 
                                                                              
                                                                              subset_name = "BMI", 
                                                                              
                                                                              HRS2008_data = HRS2008_data, 
                                                                              HRS2010_data = HRS2010_data, 
                                                                              HRS2012_data = HRS2012_data, 
                                                                              HRS2014_data = HRS2014_data, 
                                                                              HRS2016_data = HRS2016_data, 
                                                                              HRS2018_data = HRS2018_data, 
                                                                              
                                                                              exposure = exposure, 
                                                                              outcome = outcome) 



write.csv(BMI_vigarious_physical_activity_myocardial_thisWAVE_7models, paste(OUTPUT_ROOT, "BMI_vigarious_physical_activity_heartattack_new_bin.csv", sep=""))

all_vigarious_physical_activity_heartattack_new_bin.csv
female_vigarious_physical_activity_heartattack_new_bin.csv
race_vigarious_physical_activity_heartattack_new_bin.csv
combo_vigarious_physical_activity_heartattack_new_bin.csv
BMI_vigarious_physical_activity_heartattack_new_bin.csv

# BMI_vigarious_physical_activity_myocardial_thisWAVE_7models_pvalues = p_value_func(data = BMI_vigarious_physical_activity_myocardial_thisWAVE_7models,
#                                                             subset_name = "BMI", 
#                                                             Model = Model)

all_results = read.csv(paste(OUTPUT_ROOT, "done/all_vigarious_physical_activity_heartattack_new_bin.csv", sep=""))
female_results =  read.csv(paste(OUTPUT_ROOT, "done/female_vigarious_physical_activity_heartattack_new_bin.csv", sep=""))
race_results = read.csv(paste(OUTPUT_ROOT, "done/race_vigarious_physical_activity_heartattack_new_bin.csv", sep=""))
combo_results = read.csv(paste(OUTPUT_ROOT, "done/combo_vigarious_physical_activity_heartattack_new_bin.csv", sep=""))
BMI_results = read.csv(paste(OUTPUT_ROOT, "done/BMI_vigarious_physical_activity_heartattack_new_bin.csv", sep=""))


results = rbind(all_results[1:7,],
                female_results[1:7,],
                race_results[1:7,],
                combo_results[1:7,],
                BMI_results[1:7,]) 

write.csv(results, paste(OUTPUT_ROOT, "done/result_table_heartattack_new_bin.csv", sep=""))


# all_results = read.csv(paste(OUTPUT_ROOT, "All_clean_data_HRsonly_heartattack_new_bin.csv", sep=""))
# female_results =  read.csv(paste(OUTPUT_ROOT, "Female_clean_data_HRsonly_heartattack_new_bin.csv", sep=""))
# race_results = read.csv(paste(OUTPUT_ROOT, "Race_clean_data_HRsonly_heartattack_new_bin.csv", sep=""))
# combo_results = read.csv(paste(OUTPUT_ROOT, "Combo_clean_data_HRsonly_heartattack_new_bin.csv", sep=""))
# BMI_results = read.csv(paste(OUTPUT_ROOT, "BMI_clean_data_HRsonly_heartattack_new_bin.csv", sep=""))

Model = c(1, 2, 3, 4, 5, 6, 7)



all_results = cbind(Model, 
                    all_results)


female_results = cbind(Model, 
                       female_results)


race_results = cbind(Model, 
                     race_results)

combo_results = cbind(Model, 
                      combo_results)

BMI_results = cbind(Model, 
                    BMI_results)


results = rbind(all_results[1:7,],
                female_results[1:7,],
                race_results[1:7,],
                combo_results[1:7,],
                BMI_results[1:7,]) 

Model_1= subset(results, results$Model == 1)
Model_2= subset(results, results$Model == 2)
Model_3= subset(results, results$Model == 3)
Model_4= subset(results, results$Model == 4)
Model_5= subset(results, results$Model == 5)
Model_6= subset(results, results$Model == 6)
Model_7= subset(results, results$Model == 7)

results_col = cbind(Model_1, 
                    Model_2, 
                    Model_3, 
                    Model_4, 
                    Model_5, 
                    Model_6, 
                    Model_7)

results_col = results_col[,c(3:10,18:20, 28:30, 38:40, 48:50, 58:60, 68:70)]
write.csv(results_col, paste(OUTPUT_ROOT, "done/result_table_heartattack_new_bin.csv", sep=""))


