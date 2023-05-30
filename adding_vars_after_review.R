
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


current_directory = "/Users/aliya/my_docs/"
#current_directory = "/Users/aliyaamirova/proj/Cumulative_effects_HRS"

OUTPUT_ROOT =(paste(current_directory, "proj/Cumulative_effects_HRS/Results/diabetes_new_min_revised_nov2022/check_drop_12cases_at_follow_up4/", sep=""))
SOURCE_ROOT = (paste(current_directory, "proj/Cumulative_effects_HRS/Version_2_analysis/", sep=""))
DATAIN_ROOT = (paste(current_directory, "KCL_postDoc/Data_analysis/", sep="")) 



### education 
HRS2010_data_additional = read.csv(paste(DATAIN_ROOT, "HRS_2010_data/HRS_ALLData_originalVARNames.csv", sep=""))

HRS2010_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2010_data/old/HRS2010_data_short_OLD.csv", sep=""))

id_vector = unique(HRS2010_data_initial$HHIDPN) 



HRS2010_data_additional$HHIDPN = paste(HRS2010_data_additional$HHID...1, 0, HRS2010_data_additional$PN...2, sep = "") 


HRS2010_data_additional = subset(HRS2010_data_additional, HRS2010_data_additional$HHIDPN %in% id_vector) 

nrow(HRS2010_data_additional)
nrow(HRS2010_data_initial)

#unique(HRS2010_data_additional$MB014)
#0. No formal education " 0" 
#1-11. Grades  " 5"  " 6"  " 7"   " 8"  " 9"  " 10"  " 11" 
#12. High school " 12"
#13-15. Some college " 13" " 14" " 15"
#16. College grad  " 16"
#17. Post college (17+ years) " 17"
#97. Other " 97"
#98. Don't know; not ascertained
#99. Refused
#Blank. Inapplicable; partial interview #" NA" 

ls(HRS2010_data_additional) 



  unique(HRS2010_data_additional$MB014)

HRS2010_data_additional$education = case_when(HRS2010_data_additional$MB014 == " 0" ~ 0, 
                                              HRS2010_data_additional$MB014 == " 1" ~ 1, 
                                              HRS2010_data_additional$MB014 == " 2" ~ 2, 
                                              HRS2010_data_additional$MB014 == " 3" ~ 3, 
                                              HRS2010_data_additional$MB014 == " 4" ~ 4, 
                                              HRS2010_data_additional$MB014 == " 5" ~ 5, 
                                              HRS2010_data_additional$MB014 == " 6" ~ 6, 
                                              HRS2010_data_additional$MB014 == " 7" ~ 7, 
                                              HRS2010_data_additional$MB014 == " 8" ~ 8, 
                                              HRS2010_data_additional$MB014 == " 9" ~ 9, 
                                              HRS2010_data_additional$MB014 == " 10" ~ 10, 
                                              HRS2010_data_additional$MB014 == " 11" ~ 11, 
                                              HRS2010_data_additional$MB014 == " 12" ~ 12, 
                                              HRS2010_data_additional$MB014 == " 13" ~ 13, 
                                              HRS2010_data_additional$MB014 == " 14" ~ 14, 
                                              HRS2010_data_additional$MB014 == " 15" ~ 15, 
                                              HRS2010_data_additional$MB014 == " 16" ~ 16, 
                                              HRS2010_data_additional$MB014 == " 17" ~ 17, 
                                              HRS2010_data_additional$MB014 == " 97" ~ 97) 


HRS2010_data_initial$education_level = case_when(HRS2010_data_additional$education == 0 ~ 0,
                                                    
                                                    HRS2010_data_additional$education == 1 ~ 1, 
                                                    HRS2010_data_additional$education == 2 ~ 1,
                                                    HRS2010_data_additional$education == 3 ~ 1,
                                                    HRS2010_data_additional$education == 4 ~ 1,
                                                    HRS2010_data_additional$education == 5 ~ 1,
                                                    HRS2010_data_additional$education == 6 ~ 1,
                                                    HRS2010_data_additional$education == 7 ~ 1, 
                                                    HRS2010_data_additional$education == 8 ~ 1, 
                                                    HRS2010_data_additional$education == 9 ~ 1,
                                                    HRS2010_data_additional$education == 10 ~ 1,
                                                    HRS2010_data_additional$education == 11 ~ 1,
                                                    
                                                    
                                                    
                                                    HRS2010_data_additional$education == 12 ~ 2,
                                                    
                                                    HRS2010_data_additional$education == 13 ~ 3,
                                                    HRS2010_data_additional$education == 14 ~ 3,
                                                    HRS2010_data_additional$education == 15 ~ 3,
                                                    
                                                    HRS2010_data_additional$education == 16 ~ 4,
                                                    
                                                    HRS2010_data_additional$education == 17 ~ 5) 



unique(HRS2010_data_initial$education_level) 

write.csv(HRS2010_data_initial, file = paste(DATAIN_ROOT, "HRS_2010_data/old/HRS2010_data_short_OLD.csv", sep=""))


