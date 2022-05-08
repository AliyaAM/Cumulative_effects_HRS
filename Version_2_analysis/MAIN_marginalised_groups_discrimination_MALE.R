


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



current_directory = "/Users/aliyaamirova/proj/Cumulative_effects_HRS"

OUTPUT_ROOT =(paste(current_directory, "/data_files/Results/DIAB_diabetes_this_wave_correct/", sep=""))
SOURCE_ROOT = (paste(current_directory, "/Version_2_analysis/", sep=""))
DATAIN_ROOT = (paste(current_directory, "/data_files/OLD_data_diabetes_this_wave/", sep="")) 


# function that subsets and srts dataset for a particular var (eg., male == 1)

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

source((paste(SOURCE_ROOT, "p_value_func.R", sep="")))



#data 
HRS2008_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2008_data_short_OLD.csv", sep=""))
HRS2010_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2010_data_short_OLD.csv", sep=""))
HRS2012_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2012_data_short_OLD.csv", sep=""))
HRS2014_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2014_data_short_OLD.csv", sep=""))
HRS2016_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2016_data_short_OLD.csv", sep=""))
HRS2018_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2018_data_short_OLD.csv", sep=""))

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
exposure = "discrim_bin"

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
outcome = "diabetes_new_bin"



male_discrim_bin_diabetes_thisWAVE_7models = Seven_models_drop_baseline(subset_var1 = "sex_1_2", 
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
                                                                          
                                                                          subset_name = "male", 
                                                                          
                                                                          HRS2008_data = HRS2008_data, 
                                                                          HRS2010_data = HRS2010_data, 
                                                                          HRS2012_data = HRS2012_data, 
                                                                          HRS2014_data = HRS2014_data, 
                                                                          HRS2016_data = HRS2016_data, 
                                                                          HRS2018_data = HRS2018_data, 
                                                                          
                                                                          exposure = exposure, 
                                                                          outcome = outcome) 



write.csv(male_discrim_bin_diabetes_thisWAVE_7models, paste(OUTPUT_ROOT, "male_discrim_bin_diabetes_thisWAVE.csv", sep=""))




male_discrim_bin_diabetes_thisWAVE_7models_pvalues = p_value_func(data = male_discrim_bin_diabetes_thisWAVE_7models,
                                                                    subset_name = "male", 
                                                                    Model = Model)

write.csv(male_discrim_bin_diabetes_thisWAVE_7models_pvalues, paste(OUTPUT_ROOT, "male_discrim_bin_diabetes_7models_pvalues.csv", sep=""))


male_results = read.csv(paste(OUTPUT_ROOT, "male_clean_data_HRsonly.csv", sep=""))


results = rbind(male_results[1:7,])

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
write.csv(results_col, paste(OUTPUT_ROOT, "result_table_diabetes_thisWAVE_MALE.csv", sep=""))



                
