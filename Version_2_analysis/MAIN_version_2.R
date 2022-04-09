

library(WCE)
library(survival)
library(dplyr)
library(car)
library(tidyverse)
library(tidyr)

library(epiDisplay) #tab1 function to make a frequency table 
library(foreign)
library(rms) # Used to extract p_value from logistic model
library(ggplot2) #plots 
library(corrplot)
library(gridExtra) 
library(sjPlot)
library(knitr)
library(lme4)
library(lattice)
library(Hmisc)



# plots: 
# https://adibender.github.io/pammtools/articles/cumulative_effects.html
#https://rpkgs.datanovia.com/ggpubr/reference/stat_regline_equation.html Adding text (coefficients etc) to the plot next to the regression line 


library("here")


current_directory = here()


OUTPUT_ROOT =(paste(current_directory, "/data_files/Results/", sep=""))
SOURCE_ROOT = (paste(current_directory, "/Version_2_analysis/", sep=""))
DATAIN_ROOT = (paste(current_directory, "/data_files/", sep="")) 

#data 
HRS2008_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2008_data_short.csv", sep=""))
HRS2010_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2010_data_short.csv", sep=""))
HRS2012_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2012_data_short.csv", sep=""))
HRS2014_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2014_data_short.csv", sep=""))
HRS2016_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2016_data_short.csv", sep=""))
HRS2018_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2018_data_short.csv", sep=""))

#######

HRS2008_data_intermediate = HRS2008_data_initial
HRS2010_data_intermediate = HRS2010_data_initial
HRS2012_data_intermediate = HRS2012_data_initial
HRS2014_data_intermediate = HRS2014_data_initial
HRS2016_data_intermediate = HRS2016_data_initial
HRS2018_data_intermediate = HRS2018_data_initial


# function that subsets and srts dataset for a particular var (eg., female == 1)

#/Users/aliyaamirova/proj/Cumulative_effects_HRS/Version_2_analysis
source((paste(SOURCE_ROOT, "subset_func.R", sep="")))
source((paste(SOURCE_ROOT, "clean_recode_keyvars.R", sep="")))
#source((paste(SOURCE_ROOT, "subset_sort_BMI.R", sep="")))

#function that sorts out the data into a dataframe where each participant x wave pair is one row. Here I also add starting and stopping points to identify each wave
source((paste(SOURCE_ROOT, "sort_timepoints.R", sep="")))

source((paste(SOURCE_ROOT, "clean_recode_sort.R", sep="")))

#function that runs WCE analysis
source((paste(SOURCE_ROOT, "summary_score_WCE_analysis.R", sep="")))
# function that samples bootstrapped CIs
source((paste(SOURCE_ROOT, "summary_score_Bootstrapped_CI.R", sep="")))
# function that runs WCE analysis and CIs sampling for a specified subset and with a specified model 
source((paste(SOURCE_ROOT, "models_func.R", sep="")))


#Model 1: age and sex, wealth  [basis adjustment]
Model_1 = c("continious_age", "wealth_noIRA", "sex_1_2")
#Model 2: age, sex, wealth, BMI, hypertension  [basic adjustment + diabetes risk factors]
Model_2 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "hypertension_new_bin")
#Model 3: age, sex, wealth, physical activity, smoking (yes/no), and alcohol (days/week) [basic adjustment + health behaviours]
Model_3 = c("continious_age", "wealth_noIRA", "sex_1_2","alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin')
#Model 4: age, sex, wealth, CVD  [basic adjustment + CVD most common diabetes co_morbidity]
Model_4 = c("continious_age", "wealth_noIRA", "sex_1_2","CVD")
#Model 5: age, sex, wealth, depression  [basic adjustment + depression best researched psychosocial factor in diabetes ]
Model_5 = c("continious_age","wealth_noIRA", "sex_1_2","checklist_depression_bin")
#Model 6: age, sex, wealth, BMI, hypertension, CVD  [basic adjustment + diabetes risk factors+ CVD]
Model_6 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "hypertension_new_bin", "CVD")
#Model 7: age, sex, wealth, BMI, hypertension, depression  [basic adjustment + diabetes risk factors+ Depression]
Model_7 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "hypertension_new_bin", "checklist_depression_bin")


#Model 1: age and sex, wealth  [basis adjustment]
Model_1_nosex = c("continious_age", "wealth_noIRA")
#Model 2: age, sex, wealth, BMI, hypertension  [basic adjustment + diabetes risk factors]
Model_2_nosex = c("continious_age", "wealth_noIRA",  "assessed_BMI", "hypertension_new_bin")
#Model 3: age, sex, wealth, physical activity, smoking (yes/no), and alcohol (days/week) [basic adjustment + health behaviours]
Model_3_nosex = c("continious_age", "wealth_noIRA", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin')
#Model 4: age, sex, wealth, CVD  [basic adjustment + CVD most common diabetes co_morbidity]
Model_4_nosex = c("continious_age", "wealth_noIRA","CVD")
#Model 5: age, sex, wealth, depression  [basic adjustment + depression best researched psychosocial factor in diabetes ]
Model_5_nosex = c("continious_age","wealth_noIRA", "checklist_depression_bin")
#Model 6: age, sex, wealth, BMI, hypertension, CVD  [basic adjustment + diabetes risk factors+ CVD]
Model_6_nosex = c("continious_age", "wealth_noIRA",  "assessed_BMI", "hypertension_new_bin", "CVD")
#Model 7: age, sex, wealth, BMI, hypertension, depression  [basic adjustment + diabetes risk factors+ Depression]
Model_7_nosex = c("continious_age", "wealth_noIRA",  "assessed_BMI", "hypertension_new_bin", "checklist_depression_bin")


######


#Model 1: age and sex, wealth  [basis adjustment]
Model_noBMIcov_1 = c("continious_age", "wealth_noIRA", "sex_1_2")
#Model 2: age, sex, wealth, BMI, hypertension  [basic adjustment + diabetes risk factors]
Model_noBMIcov_2 = c("continious_age", "wealth_noIRA", "sex_1_2", "hypertension_new_bin")
#Model 3: age, sex, wealth, physical activity, smoking (yes/no), and alcohol (days/week) [basic adjustment + health behaviours]
Model_noBMIcov_3 = c("continious_age", "wealth_noIRA", "sex_1_2","alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin')
#Model 4: age, sex, wealth, CVD  [basic adjustment + CVD most common diabetes co_morbidity]
Model_noBMIcov_4 = c("continious_age", "wealth_noIRA", "sex_1_2","CVD")
#Model 5: age, sex, wealth, depression  [basic adjustment + depression best researched psychosocial factor in diabetes ]
Model_noBMIcov_5 = c("continious_age","wealth_noIRA", "sex_1_2","checklist_depression_bin")
#Model 6: age, sex, wealth, BMI, hypertension, CVD  [basic adjustment + diabetes risk factors+ CVD]
Model_noBMIcov_6 = c("continious_age", "wealth_noIRA", "sex_1_2", "hypertension_new_bin", "CVD")
#Model 7: age, sex, wealth, BMI, hypertension, depression  [basic adjustment + diabetes risk factors+ Depression]
Model_noBMIcov_7 = c("continious_age", "wealth_noIRA", "sex_1_2",  "hypertension_new_bin", "checklist_depression_bin")


HRS2008_data_intermediate$race_white = as.factor(HRS2008_data_intermediate$race_white)
HRS2010_data_intermediate$race_white = as.factor(HRS2010_data_intermediate$race_white)
HRS2012_data_intermediate$race_white = as.factor(HRS2012_data_intermediate$race_white)
HRS2014_data_intermediate$race_white = as.factor(HRS2014_data_intermediate$race_white)
HRS2016_data_intermediate$race_white = as.factor(HRS2016_data_intermediate$race_white)
HRS2018_data_intermediate$race_white = as.factor(HRS2018_data_intermediate$race_white)


HRS2008_data = HRS2008_data_intermediate
HRS2010_data = HRS2010_data_intermediate
HRS2012_data = HRS2012_data_intermediate
HRS2014_data = HRS2014_data_intermediate
HRS2016_data = HRS2016_data_intermediate
HRS2018_data = HRS2018_data_intermediate


race_dataset = clean_recode_sort(subset_var = "race_white", 
                                 subset_value = 0, 
                                 HRS2008_data = HRS2008_data_intermediate,
                                 HRS2010_data = HRS2010_data_intermediate, 
                                 HRS2012_data = HRS2012_data_intermediate,
                                 HRS2014_data = HRS2014_data_intermediate, 
                                 HRS2016_data = HRS2016_data_intermediate,
                                 HRS2018_data = HRS2018_data_intermediate) 

###### run all models for race other than white

#  exposure = "discrim_bin", 
#outcome = "diabetes_new_bin",


###### drop NAs and weird strings like " NA", THE WCE ANALYSIS DOES NOT RUN WITH NAs
unique(race_dataset$diabetes_new)

unique(HRS2008_data_initial$diabetes_new)
unique(HRS2008_data_intermediate$diabetes_new)


race_dataset_noNAs = race_dataset %>% drop_na(diabetes_new)
unique(race_dataset_noNAs$diabetes_new)

race_dataset_noNAs = race_dataset_noNAs %>% drop_na(discrim_bin)
unique(race_dataset_noNAs$discrim_bin)


# sort out data and tag time points as start_new, stop_new
race_dataset_noNAs_timepoints = sort_timepoints(data = race_dataset_noNAs)

# diabetes_new is r2diabs:w2 r had diabetes since last iw 
# r3diabs:w3 r had diabetes since last iw  
# r4diabs:w4 r had diabetes since last iw  
# rndiabs:wn r had diabetes since last iw  

unique(HRS2008_data_initial$diabetes_new)

race_Model_1_discrim_bin = models_func(data_wce_subset = race_dataset_noNAs_timepoints, 
                                                  Model_n = Model_1, 
                                                  
                                                  exposure = "discrim_bin", 
                                                  outcome = "diabetes_new",
                                                
                                                  Model_name = "Model_1")



race_Model_2_discrim_bin = models_func(data_wce_subset = race_dataset_noNAs_timepoints, 
                                       Model_n = Model_3, 
                                       
                                       exposure = "discrim_bin", 
                                       outcome = "diabetes_new",
                                       
                                       Model_name = "Model_2")




race_Model_3_discrim_bin = models_func(data_wce_subset = race_dataset_noNAs_timepoints, 
                                       Model_n = Model_3, 
                                       
                                       exposure = "discrim_bin", 
                                       outcome = "diabetes_new",
                                       
                                       Model_name = "Model_3")



race_Model_4_discrim_bin = models_func(data_wce_subset = race_dataset_noNAs_timepoints, 
                                       Model_n = Model_4, 
                                       
                                       exposure = "discrim_bin", 
                                       outcome = "diabetes_new",
                                       
                                       Model_name = "Model_4")





race_Model_5_discrim_bin = models_func(data_wce_subset = race_dataset_noNAs_timepoints, 
                                       Model_n = Model_5, 
                                       
                                       exposure = "discrim_bin", 
                                       outcome = "diabetes_new",
                                       
                                       Model_name = "Model_5")




race_Model_6_discrim_bin = models_func(data_wce_subset = race_dataset_noNAs_timepoints, 
                                       Model_n = Model_5, 
                                       
                                       exposure = "discrim_bin", 
                                       outcome = "diabetes_new",
                                       
                                       Model_name = "Model_6")



race_Model_7_discrim_bin = models_func(data_wce_subset = race_dataset_noNAs_timepoints, 
                                       Model_n = Model_5, 
                                       
                                       exposure = "discrim_bin", 
                                       outcome = "diabetes_new",
                                       
                                       Model_name = "Model_7")




race_results_discrim_bin = rbind(race_Model_1_discrim_bin,
                                 race_Model_2_discrim_bin,
                                 race_Model_3_discrim_bin,
                                 race_Model_4_discrim_bin,
                                 race_Model_5_discrim_bin,
                                 race_Model_6_discrim_bin,
                                 race_Model_7_discrim_bin)

write.csv(race_results_discrim_bin, paste(OUTPUT_ROOT, "race_results_discrim_binrestricted__V4.csv", sep=""))


race_results_discrim_bin_table_col = cbind(race_Model_1_discrim_bin,
                                           race_Model_2_discrim_bin,
                                           race_Model_3_discrim_bin,
                                           race_Model_4_discrim_bin,
                                           race_Model_5_discrim_bin,
                                           race_Model_6_discrim_bin,
                                           race_Model_7_discrim_bin)

write.csv(race_results_discrim_bin_table_col, paste(OUTPUT_ROOT, "race_results_discrim_bin_table_colrestricted__V4.csv", sep=""))
