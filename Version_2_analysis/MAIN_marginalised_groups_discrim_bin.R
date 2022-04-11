
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

OUTPUT_ROOT =(paste(current_directory, "/data_files/Results/", sep=""))
SOURCE_ROOT = (paste(current_directory, "/Version_2_analysis/", sep=""))
DATAIN_ROOT = (paste(current_directory, "/data_files/", sep="")) 


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
source((paste(SOURCE_ROOT, "HRs_CIs_analysis.R", sep="")))

source((paste(SOURCE_ROOT, "Seven_models.R", sep="")))

source((paste(SOURCE_ROOT, "p_value_func.R", sep="")))



#data 
HRS2008_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2008_data_short.csv", sep=""))
HRS2010_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2010_data_short.csv", sep=""))
HRS2012_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2012_data_short.csv", sep=""))
HRS2014_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2014_data_short.csv", sep=""))
HRS2016_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2016_data_short.csv", sep=""))
HRS2018_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2018_data_short.csv", sep=""))

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

outcome = "diabetes_new"

all_discrim_bin_diabetes_new_7models = Seven_models(subset_var1 = "NA", 
                                                    subset_value1 = "NA", 
                                                    
                                                    subset_BMI = "NA", 
                                                    subset_BMI_value  = "NA", 
                                                    
                                                    subset_var2 = "NA", 
                                                    subset_value2 = "NA",  
                                                    
                                                    subset_var3= "NA", 
                                                    subset_value3 = "NA", 
                                                    
                                                    subset_name = "ALL", 
                                                    
                                                    HRS2008_data = HRS2008_data, 
                                                    HRS2010_data = HRS2010_data, 
                                                    HRS2012_data = HRS2012_data, 
                                                    HRS2014_data = HRS2014_data, 
                                                    HRS2016_data = HRS2016_data, 
                                                    HRS2018_data = HRS2018_data, 
                                                    
                                                    exposure = exposure, 
                                                    outcome = outcome) 



write.csv(all_discrim_bin_diabetes_new_7models, paste(OUTPUT_ROOT, "all_discrim_bin_diabetes_new.csv", sep=""))


all_discrim_bin_diabetes_new_7models_pvalues = p_value_func(data = all_discrim_bin_diabetes_new_7models,
                                                             subset_name = "All", 
                                                             Model = Model)


female_discrim_bin_diabetes_new_7models = Seven_models(subset_var1 = "sex_1_2", 
                                                       subset_value1 = 2, 
                                                       
                                                       
                                                       subset_BMI = "NA", 
                                                       subset_BMI_value  = "NA", 
                                                       
                                                       subset_var2 = "NA", 
                                                       subset_value2 = "NA",  
                                                       
                                                       subset_var3= "NA", 
                                                       subset_value3 = "NA", 
                                                       
                                                       subset_name = "FEMALE", 
                                                       
                                                       HRS2008_data = HRS2008_data, 
                                                       HRS2010_data = HRS2010_data, 
                                                       HRS2012_data = HRS2012_data, 
                                                       HRS2014_data = HRS2014_data, 
                                                       HRS2016_data = HRS2016_data, 
                                                       HRS2018_data = HRS2018_data, 
                                                       
                                                       exposure = exposure, 
                                                       outcome = outcome) 



write.csv(female_discrim_bin_diabetes_new_7models, paste(OUTPUT_ROOT, "female_discrim_bin_diabetes_new.csv", sep=""))




female_discrim_bin_diabetes_new_7models_pvalues = p_value_func(data = female_discrim_bin_diabetes_new_7models,
                                                            subset_name = "Female", 
                                                            Model = Model)





race_discrim_bin_diabetes_new_7models = Seven_models(subset_var1 = "race_white", 
                                                     subset_value1 = 0, 
                                                     
                                                     subset_BMI = "NA", 
                                                     subset_BMI_value  = "NA", 
                                                     
                                                     subset_var2 = "NA", 
                                                     subset_value2 = "NA",  
                                                     
                                                     subset_var3= "NA", 
                                                     subset_value3 = "NA", 
                                                     
                                                     subset_name = "RACE", 
                                                     
                                                     HRS2008_data = HRS2008_data, 
                                                     HRS2010_data = HRS2010_data, 
                                                     HRS2012_data = HRS2012_data, 
                                                     HRS2014_data = HRS2014_data, 
                                                     HRS2016_data = HRS2016_data, 
                                                     HRS2018_data = HRS2018_data, 
                                                     
                                                     exposure = exposure, 
                                                     outcome = outcome) 




write.csv(race_discrim_bin_diabetes_new_7models, paste(OUTPUT_ROOT, "race_discrim_bin_diabetes_new.csv", sep=""))



race_discrim_bin_diabetes_new_7models_pvalues = p_value_func(data = race_discrim_bin_diabetes_new_7models,
                                                             subset_name = "Race", 
                                                             Model = Model)



combo_discrim_bin_diabetes_new_7models = Seven_models(subset_var1 = "race_white", 
                                                      subset_value1 = 0, 
                                                      
                                                      subset_BMI = "NA", 
                                                      subset_BMI_value  = "NA", 
                                                      
                                                      subset_var2 = "religion_bin", 
                                                      subset_value2 = 1,  
                                                      
                                                      subset_var3= "national_origin_ousideUS_bin", 
                                                      subset_value3 = 1, 
                                                      
                                                      subset_name = "COMBO", 
                                                      
                                                      HRS2008_data = HRS2008_data, 
                                                      HRS2010_data = HRS2010_data, 
                                                      HRS2012_data = HRS2012_data, 
                                                      HRS2014_data = HRS2014_data, 
                                                      HRS2016_data = HRS2016_data, 
                                                      HRS2018_data = HRS2018_data, 
                                                      
                                                      exposure = exposure, 
                                                      outcome = outcome) 




write.csv(combo_discrim_bin_diabetes_new_7models, paste(OUTPUT_ROOT, "combo_discrim_bin_diabetes_new.csv", sep=""))




Combo_discrim_bin_diabetes_new_7models_pvalues = p_value_func(data = combo_discrim_bin_diabetes_new_7models,
                                                             subset_name = "Combo", 
                                                             Model = Model)


BMI_discrim_bin_diabetes_new_7models = Seven_models(subset_var1 = "NA", 
                                                    subset_value1 = "NA", 
                                                    
                                                    subset_BMI = "assessed_BMI", 
                                                    subset_BMI_value = 30, 
                                                    
                                                    subset_var2 = "NA", 
                                                    subset_value2 = "NA", 
                                                    
                                                    subset_var3= "NA", 
                                                    subset_value3 = "NA", 
                                                    
                                                    
                                                    subset_name = "BMI", 
                                                    
                                                    HRS2008_data = HRS2008_data, 
                                                    HRS2010_data = HRS2010_data, 
                                                    HRS2012_data = HRS2012_data, 
                                                    HRS2014_data = HRS2014_data, 
                                                    HRS2016_data = HRS2016_data, 
                                                    HRS2018_data = HRS2018_data, 
                                                    
                                                    exposure = exposure, 
                                                    outcome = outcome) 



write.csv(BMI_discrim_bin_diabetes_new_7models, paste(OUTPUT_ROOT, "BMI_discrim_bin_diabetes_new.csv", sep=""))

BMI_discrim_bin_diabetes_new_7models_pvalues = p_value_func(data = BMI_discrim_bin_diabetes_new_7models,
                                                              subset_name = "BMI", 
                                                              Model = Model)

all_results = read.csv(paste(OUTPUT_ROOT, "All_clean_data_HRsonly.csv", sep=""))
female_results =  read.csv(paste(OUTPUT_ROOT, "Female_clean_data_HRsonly.csv", sep=""))
race_results = read.csv(paste(OUTPUT_ROOT, "Race_clean_data_HRsonly.csv", sep=""))
combo_results = read.csv(paste(OUTPUT_ROOT, "Combo_clean_data_HRsonly.csv", sep=""))
BMI_results = read.csv(paste(OUTPUT_ROOT, "BMI_clean_data_HRsonly.csv", sep=""))


results = rbind(all_results[1:7,],
                female_results[1:7,],
                race_results[1:7,],
                combo_results[1:7,],
                BMI_results[1:7,]) 

write.csv(results, paste(OUTPUT_ROOT, "result_table_diabetes_new.csv", sep=""))


all_results = read.csv(paste(OUTPUT_ROOT, "All_clean_data_HRsonly.csv", sep=""))
female_results =  read.csv(paste(OUTPUT_ROOT, "Female_clean_data_HRsonly.csv", sep=""))
race_results = read.csv(paste(OUTPUT_ROOT, "Race_clean_data_HRsonly.csv", sep=""))
combo_results = read.csv(paste(OUTPUT_ROOT, "Combo_clean_data_HRsonly.csv", sep=""))
BMI_results = read.csv(paste(OUTPUT_ROOT, "BMI_clean_data_HRsonly.csv", sep=""))


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
write.csv(results_col, paste(OUTPUT_ROOT, "result_table_diabetes_new.csv", sep=""))


