
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

#library(riskRegression)


# plots: 
# https://adibender.github.io/pammtools/articles/cumulative_effects.html
#https://rpkgs.datanovia.com/ggpubr/reference/stat_regline_equation.html Adding text (coefficients etc) to the plot next to the regression line 



current_directory =  "C:/Users/k2147340/OneDrive - King's College London/Documents/ELSA_HRS/Data_analysis/"

#current_directory = "/Users/aliya/my_docs/"
#current_directory = "/Users/aliyaamirova/proj/Cumulative_effects_HRS/"

#OUTPUT_ROOT =(paste(current_directory, "proj/Cumulative_effects_HRS/Results/diabetes_new_min_revised_nov2022/check_drop_12cases_at_follow_up4/", sep=""))
#SOURCE_ROOT = (paste(current_directory, "proj/Cumulative_effects_HRS/Version_2_analysis/", sep=""))
#DATAIN_ROOT = (paste(current_directory, "KCL_postDoc/Data_analysis/", sep="")) 



### education 
#data_v2 = read.csv(paste(DATAIN_ROOT, "HRS_2010_data/HRS_ALLData_originalVARNames.csv", sep=""))

HRS2010_data_initial = read.csv(paste(current_directory, "HRS_2010_data/HRS2010_data_short_education.csv", sep=""))

id_vector = unique(HRS2010_data_initial$HHIDPN) 

### There is hispan var in file below:  
data_v2 = read.csv("C:/Users/k2147340/OneDrive - King's College London/Desktop/randhrs1992_2018v1.csv")

data_v2$HHIDPN

#data_v2$HHIDPN = paste(data_v2$HHID...1, 0, data_v2$PN...2, sep = "") 



#data_v2 = subset(data_v2, data_v2$HHIDPN %in% id_vector) 
data_v2 = subset(data_v2, data_v2$HHIDPN %in% id_vector)

HRS2010_data_initial$RAHISPAN = data_v2$RAHISPAN



write.csv(HRS2010_data_initial, file = paste(current_directory, "HRS_2010_data/HRS2010_data_short_education.csv", sep=""))


