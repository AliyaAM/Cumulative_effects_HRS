
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


# function that subsets and srts dataset for a particular var (eg., combo == 1)

#/Users/aliyaamirova/proj/Cumulative_effects_HRS/Version_2_analysis
source((paste(SOURCE_ROOT, "subset_func.R", sep="")))

source((paste(SOURCE_ROOT, "clean_recode_keyvars.R", sep="")))

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


HRS2008_data_intermediate$combo_white = as.factor(HRS2008_data_intermediate$sex_1_2)
HRS2010_data_intermediate$combo_white = as.factor(HRS2010_data_intermediate$sex_1_2)
HRS2012_data_intermediate$combo_white = as.factor(HRS2012_data_intermediate$sex_1_2)
HRS2014_data_intermediate$combo_white = as.factor(HRS2014_data_intermediate$sex_1_2)
HRS2016_data_intermediate$combo_white = as.factor(HRS2016_data_intermediate$sex_1_2)
HRS2018_data_intermediate$combo_white = as.factor(HRS2018_data_intermediate$sex_1_2)


#exposure = "discrim_bin", 
#outcome = "diabetes_new_bin"

combo_discrim_bin_diabetes_new_7models = Seven_models(subset_var1 = "race_white", 
                                                    subset_value1 = 0, 
                                                    
                                                    subset_BMI = "NA", 
                                                    subset_BMI_value  = "NA", 
                                                    
                                                    subset_var2 = "religion_bin", 
                                                    subset_value2 = 1,  
                                                    
                                                    subset_var3= "national_origin_ousideUS_bin", 
                                                    subset_value3 = 1, 
                                                    
                                                    HRS2008_data = HRS2008_data, 
                                                    HRS2010_data = HRS2010_data, 
                                                    HRS2012_data = HRS2012_data, 
                                                    HRS2014_data = HRS2014_data, 
                                                    HRS2016_data = HRS2016_data, 
                                                    HRS2018_data = HRS2018_data, 
                                                    exposure = "discrim_bin", 
                                                    outcome = "diabetes_new") 




write.csv(combo_discrim_bin_diabetes_new_7models, paste(OUTPUT_ROOT, "combo_discrim_bin_diabetes_new.csv", sep=""))



