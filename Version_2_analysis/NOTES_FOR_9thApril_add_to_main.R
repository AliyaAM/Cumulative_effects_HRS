


####### exposure is coded as: 
####### 1 Almost everyday
####### 2 At least once a week
####### 3 A few times a month
####### 4 A few times a year
####### 5 Less than once a year
####### 6 Never

####### HR 1vs6 is Almost everyday vs Never
####### HR 2vs6 is At least once a week vs Never
####### HR 3vs6 is A few times a month vs Never
####### HR 4vs6 is A few times a year vs Never
####### HR 5vs6 is Less than once a year vs Never



#################################### ######## ########################################################################


######
######
###### fix vars 



#diabetes_new is diabtes this wave 
#0.no
#1.yes
#3.disp prev record and has cond
#4.disp prev record and no cond
#.d=DK
#.r=RF      


# create binary CVD variable 


WCE_dataset_subset$CVD[WCE_dataset_subset$heartcondition_ever_bin == 1 | WCE_dataset_subset$heartcondition_new_bin == 1 | WCE_dataset_subset$angina_new_bin ==1 | WCE_dataset_subset$stroke_new_bin == 1 | WCE_dataset_subset$heartfailure2yrs_bin == 1 | WCE_dataset_subset$heartattack_ever_bin == 1 | WCE_dataset_subset$heartattack_new_bin == 1] <-1
WCE_dataset_subset$CVD[WCE_dataset_subset$heartcondition_ever_bin == 0 & WCE_dataset_subset$heartcondition_new_bin == 0 & WCE_dataset_subset$angina_new_bin ==0 & WCE_dataset_subset$stroke_new_bin == 0 & WCE_dataset_subset$heartfailure2yrs_bin == 0 & WCE_dataset_subset$heartattack_ever_bin == 0 & WCE_dataset_subset$heartattack_new_bin == 0] <-0

###### add binary esposure and binary outcome 


WCE_dataset_subset$diabetes_new_bin = case_when(WCE_dataset_subset$diabetes_new == 1 ~ 1, 
                                                WCE_dataset_subset$diabetes_new == 0 ~ 0, 
                                                WCE_dataset_subset$diabetes_new == 3 ~ 1, 
                                                WCE_dataset_subset$diabetes_new == 4 ~ 0) 



#physical activity (original): 1.every day; 2.>1 per week; 3.1 per week; 4.l-3 per mon; 5.never; .d=DK/NA; .r=RF

WCE_dataset_subset$vigarious_physical_activity_new = case_when(WCE_dataset_subset$vigarious_physical_activity == 1 ~ 5, 
                                                               WCE_dataset_subset$vigarious_physical_activity == 2 ~ 4, 
                                                               WCE_dataset_subset$vigarious_physical_activity == 3 ~ 3, 
                                                               WCE_dataset_subset$vigarious_physical_activity == 4 ~ 2, 
                                                               WCE_dataset_subset$vigarious_physical_activity == 5 ~ 1) 



WCE_dataset_subset$vigarious_physical_activity_bin = case_when(WCE_dataset_subset$vigarious_physical_activity_new == 5 ~ 1, 
                                                               WCE_dataset_subset$vigarious_physical_activity_new == 4 ~ 1, 
                                                               WCE_dataset_subset$vigarious_physical_activity_new == 3 ~ 1, 
                                                               WCE_dataset_subset$vigarious_physical_activity_new == 2 ~ 0, 
                                                               WCE_dataset_subset$vigarious_physical_activity_new == 1 ~ 0) 

unique(WCE_dataset_subset$alcohol_days_week)


WCE_dataset_subset$alcohol_days_week_new =  na_if(WCE_dataset_subset$alcohol_days_week, 8)
WCE_dataset_subset$alcohol_days_week_new = na_if(WCE_dataset_subset$alcohol_days_week_new, 9) 

unique(WCE_dataset_subset$alcohol_days_week_new)



###### recode into single var  discrim_bin


WCE_dataset_subset$discrim_harassed_bin = case_when(WCE_dataset_subset$discrim_harassed == 1 ~ 1, 
                                                    WCE_dataset_subset$discrim_harassed == 2 ~ 1, 
                                                    WCE_dataset_subset$discrim_harassed == 3 ~ 1, 
                                                    WCE_dataset_subset$discrim_harassed == 4 ~ 1, 
                                                    WCE_dataset_subset$discrim_harassed == 5 ~ 0, 
                                                    WCE_dataset_subset$discrim_harassed == 6 ~ 0,
                                                    WCE_dataset_subset$discrim_harassed == 0 ~ 0) 




WCE_dataset_subset$discrim_lessrespect_bin = case_when(WCE_dataset_subset$discrim_lessrespect == 1 ~ 1, 
                                                       WCE_dataset_subset$discrim_lessrespect == 2 ~ 1, 
                                                       WCE_dataset_subset$discrim_lessrespect == 3 ~ 1, 
                                                       WCE_dataset_subset$discrim_lessrespect == 4 ~ 1, 
                                                       WCE_dataset_subset$discrim_lessrespect == 5 ~ 0, 
                                                       WCE_dataset_subset$discrim_lessrespect == 6 ~ 0,
                                                       WCE_dataset_subset$discrim_lessrespect == 0 ~ 0) 


WCE_dataset_subset$discrim_medical_bin = case_when(WCE_dataset_subset$discrim_medical == 1 ~ 1, 
                                                   WCE_dataset_subset$discrim_medical == 2 ~ 1, 
                                                   WCE_dataset_subset$discrim_medical == 3 ~ 1, 
                                                   WCE_dataset_subset$discrim_medical == 4 ~ 1, 
                                                   WCE_dataset_subset$discrim_medical == 5 ~ 0, 
                                                   WCE_dataset_subset$discrim_medical == 6 ~ 0,
                                                   WCE_dataset_subset$discrim_medical == 0 ~ 0) 




WCE_dataset_subset$discrim_notclever_bin = case_when(WCE_dataset_subset$discrim_notclever == 1 ~ 1, 
                                                     WCE_dataset_subset$discrim_notclever == 2 ~ 1, 
                                                     WCE_dataset_subset$discrim_notclever == 3 ~ 1, 
                                                     WCE_dataset_subset$discrim_notclever == 4 ~ 1, 
                                                     WCE_dataset_subset$discrim_notclever == 5 ~ 0, 
                                                     WCE_dataset_subset$discrim_notclever == 6 ~ 0,
                                                     WCE_dataset_subset$discrim_notclever == 0 ~ 0) 



unique(WCE_dataset_subset$discrim_poorerservice)
WCE_dataset_subset$discrim_poorerservice = as.numeric(WCE_dataset_subset$discrim_poorerservice) 

WCE_dataset_subset$discrim_poorerservice_bin = case_when(WCE_dataset_subset$discrim_poorerservice == 1 ~ 1, 
                                                         WCE_dataset_subset$discrim_poorerservice == 2 ~ 1, 
                                                         WCE_dataset_subset$discrim_poorerservice == 3 ~ 1, 
                                                         WCE_dataset_subset$discrim_poorerservice == 4 ~ 1, 
                                                         WCE_dataset_subset$discrim_poorerservice == 5 ~ 0, 
                                                         WCE_dataset_subset$discrim_poorerservice == 6 ~ 0) 



WCE_dataset_subset$discrim_afraidothers_bin = case_when(WCE_dataset_subset$discrim_afraidothers == 1 ~ 1, 
                                                        WCE_dataset_subset$discrim_afraidothers == 2 ~ 1, 
                                                        WCE_dataset_subset$discrim_afraidothers == 3 ~ 1, 
                                                        WCE_dataset_subset$discrim_afraidothers == 4 ~ 1, 
                                                        WCE_dataset_subset$discrim_afraidothers == 5 ~ 0, 
                                                        WCE_dataset_subset$discrim_afraidothers == 6 ~ 0,
                                                        WCE_dataset_subset$discrim_afraidothers == 0 ~ 0) 


WCE_dataset_subset$discrim_bin = case_when(WCE_dataset_subset$discrim_harassed_bin == 1 | WCE_dataset_subset$discrim_lessrespect_bin == 1 | WCE_dataset_subset$discrim_medical_bin  == 1 | WCE_dataset_subset$discrim_notclever_bin == 1 | WCE_dataset_subset$discrim_afraidothers_bin == 1 | WCE_dataset_subset$discrim_poorerservice_bin == 1 ~ 1, 
                                           WCE_dataset_subset$discrim_harassed_bin == 0 & WCE_dataset_subset$discrim_lessrespect_bin == 0 & WCE_dataset_subset$discrim_medical_bin  == 0 & WCE_dataset_subset$discrim_notclever_bin == 0 & WCE_dataset_subset$discrim_afraidothers_bin == 0 & WCE_dataset_subset$discrim_poorerservice_bin == 0 ~ 0) 


############# 

###### drop NAs and weird strings like " NA", THE WCE ANALYSIS DOES NOT RUN WITH NAs
WCE_dataset_subset = WCE_dataset_subset %>% drop_na(diabetes_new_bin)
unique(WCE_dataset_subset$diabetes_new_bin)

WCE_dataset_subset = WCE_dataset_subset %>% drop_na(discrim_bin)


WCE_dataset_subset = subset(WCE_dataset_subset, HHIDPN != "3020")

WCE_dataset_subset = subset(WCE_dataset_subset , diabetes_new_bin != " NA")
unique(WCE_dataset_subset$diabetes_new)

WCE_dataset_subset = subset(WCE_dataset_subset , summary_mean_score_discrim != " NA")
unique(WCE_dataset_subset$summary_mean_score_discrim)

WCE_dataset_subset = subset(WCE_dataset_subset , discrim_harassed != " NA")
unique(WCE_dataset_subset$discrim_harassed)


WCE_dataset_subset = subset(WCE_dataset_subset , discrim_lessrespect != " NA")
unique(WCE_dataset_subset$discrim_lessrespect)

WCE_dataset_subset = subset(WCE_dataset_subset , discrim_medical != " NA")
unique(WCE_dataset_subset$discrim_medical)

WCE_dataset_subset = subset(WCE_dataset_subset , discrim_notclever != " NA")
unique(WCE_dataset_subset$discrim_notclever_bin)

WCE_dataset_subset = subset(WCE_dataset_subset , discrim_poorerservice != " NA")
unique(WCE_dataset_subset$discrim_poorerservice)


WCE_dataset_subset = subset(WCE_dataset_subset , discrim_afraidothers != " NA")
unique(WCE_dataset_subset$discrim_afraidothers)



WCE_dataset_subset$alcohol_days_week_new = as.numeric(WCE_dataset_subset$alcohol_days_week_new)
WCE_dataset_subset$vigarious_physical_activity_new = as.numeric(WCE_dataset_subset$vigarious_physical_activity_new)
WCE_dataset_subset$smokes_now_bin = as.numeric(WCE_dataset_subset$smokes_now_bin)
WCE_dataset_subset$checklist_depression_bin = as.numeric(WCE_dataset_subset$checklist_depression_bin)
WCE_dataset_subset$wealth_noIRA = as.numeric(WCE_dataset_subset$wealth_noIRA)




######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...



data_wce_subset = sort_timepoints(data = WCE_dataset_subset)

nrow(data_wce_subset)


######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018
#myvars <- c("HHIDPN", "timepoints_indiv", "start_new", "stop_new", "diabetes_new_bin", "continious_age",
#            "summary_mean_score_discrim_bin", "discrim_bin", "discrim_lessrespect_bin", "discrim_medical_bin", "discrim_notclever_bin", "discrim_poorerservice_bin", "discrim_afraidothers_bin")

#data_wce_subset <- data_wce_subset_before[myvars]

unique(data_wce_subset$discrim_bin)
unique(data_wce_subset$timepoints_indiv)
unique(data_wce_subset$start_new)
unique(data_wce_subset$stop_new)
unique(data_wce_subset$HHIDPN)
unique(data_wce_subset$diabetes_new_bin)
unique(data_wce_subset$discrim_bin)
unique(data_wce_subset$hypertension_new_bin)


#######
#######
#######
#######
#######


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




## Set the root directory to look for source code.
#laptop: 

#/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/
#"/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"

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

HRS2008_data_intermediate = subset(HRS2008_data_initial, HRS2008_data_initial$diabetes_ever == 0)
HRS2010_data_intermediate = subset(HRS2010_data_initial, HRS2010_data_initial$diabetes_ever == 0)
HRS2012_data_intermediate = subset(HRS2012_data_initial, HRS2012_data_initial$diabetes_ever == 0)
HRS2014_data_intermediate = subset(HRS2014_data_initial, HRS2014_data_initial$diabetes_ever == 0)
HRS2016_data_intermediate = subset(HRS2016_data_initial, HRS2016_data_initial$diabetes_ever == 0)
HRS2018_data_intermediate = subset(HRS2018_data_initial, HRS2018_data_initial$diabetes_ever == 0)


# function that subsets and srts dataset for a particular var (eg., female == 1)

#/Users/aliyaamirova/proj/Cumulative_effects_HRS/Version_2_analysis
source((paste(SOURCE_ROOT, "subset_func.R", sep="")))
source((paste(SOURCE_ROOT, "clean_recode_keyvars.R", sep="")))
#source((paste(SOURCE_ROOT, "subset_sort_BMI.R", sep="")))

#function that sorts out the data into a dataframe where each participant x wave pair is one row. Here I also add starting and stopping points to identify each wave
source((paste(SOURCE_ROOT, "sort_timepoints.R", sep="")))
#function that runs WCE analysis
#source((paste(SOURCE_ROOT, "WCE_analysis.R", sep="")))
#function that runs WCE analysis
#source((paste(SOURCE_ROOT, "summary_score_WCE_analysis.R", sep="")))
# function that samples bootstrapped CIs
#source((paste(SOURCE_ROOT, "summary_score_Bootstrapped_CI.R", sep="")))
# function that runs WCE analysis and CIs sampling for a specified subset and with a specified model 
#source((paste(SOURCE_ROOT, "discrim_bin_model_func.R", sep="")))

#"race_white" == 0




###### drop NAs and weird strings like " NA", THE WCE ANALYSIS DOES NOT RUN WITH NAs
dataset_clean = dataset_clean %>% drop_na(dataset_clean)
data_wce_subset = sort_timepoints(data = dataset_clean)




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





###### run all models for national_origin 
national_origin_Model_1_discrim_bin = discrim_bin_model_func(data_wce_subset = national_origin_dataset, 
                                                             Model_n = Model_1, 
                                                             Model_name = "Model_1")


national_origin_Model_2_discrim_bin = discrim_bin_model_func(data_wce_subset = national_origin_dataset, 
                                                             Model_n = Model_2, 
                                                             Model_name = "Model_2")


national_origin_Model_3_discrim_bin = discrim_bin_model_func(data_wce_subset = national_origin_dataset, 
                                                             Model_n = Model_3)



national_origin_Model_4_discrim_bin = discrim_bin_model_func(data_wce_subset = national_origin_dataset, 
                                                             Model_n = Model_4)



national_origin_Model_5_discrim_bin = discrim_bin_model_func(data_wce_subset = national_origin_dataset, 
                                                             Model_n = Model_5)


national_origin_Model_6_discrim_bin = discrim_bin_model_func(data_wce_subset = national_origin_dataset, 
                                                             Model_n = Model_6)


national_origin_Model_7_discrim_bin = discrim_bin_model_func(data_wce_subset = national_origin_dataset, 
                                                             Model_n = Model_7)



national_origin_results_discrim_bin = rbind(national_origin_Model_1_discrim_bin,
                                            national_origin_Model_2_discrim_bin,
                                            national_origin_Model_3_discrim_bin,
                                            national_origin_Model_4_discrim_bin,
                                            national_origin_Model_5_discrim_bin,
                                            national_origin_Model_6_discrim_bin,
                                            national_origin_Model_7_discrim_bin)




write.csv(national_origin_results_discrim_bin, paste(OUTPUT_ROOT, "national_origin_results_discrim_binrestricted__V4.csv", sep=""))


national_origin_results_discrim_bin_table_col = cbind(national_origin_Model_1_discrim_bin,
                                                      national_origin_Model_2_discrim_bin,
                                                      national_origin_Model_3_discrim_bin,
                                                      national_origin_Model_4_discrim_bin,
                                                      national_origin_Model_5_discrim_bin,
                                                      national_origin_Model_6_discrim_bin,
                                                      national_origin_Model_7_discrim_bin)

write.csv(national_origin_results_discrim_bin_table_col, paste(OUTPUT_ROOT, "national_origin_results_discrim_bin_table_colrestricted__V4.csv", sep=""))


###### run all models for female 
female_Model_1_discrim_bin = discrim_bin_model_func(data_wce_subset = female_dataset, 
                                                    Model_n = Model_1_nosex, 
                                                    Model_name = "Model_1")


female_Model_2_discrim_bin = discrim_bin_model_func(data_wce_subset = female_dataset, 
                                                    Model_n = Model_2_nosex, 
                                                    Model_name = "Model_2")


female_Model_3_discrim_bin = discrim_bin_model_func(data_wce_subset = female_dataset, 
                                                    Model_n = Model_3_nosex)



female_Model_4_discrim_bin = discrim_bin_model_func(data_wce_subset = female_dataset, 
                                                    Model_n = Model_4_nosex)



female_Model_5_discrim_bin = discrim_bin_model_func(data_wce_subset = female_dataset, 
                                                    Model_n = Model_5_nosex)


female_Model_6_discrim_bin = discrim_bin_model_func(data_wce_subset = female_dataset, 
                                                    Model_n = Model_6_nosex)


female_Model_7_discrim_bin = discrim_bin_model_func(data_wce_subset = female_dataset, 
                                                    Model_n = Model_7_nosex)




female_results_discrim_bin = rbind(female_Model_1_discrim_bin,
                                   female_Model_2_discrim_bin,
                                   female_Model_3_discrim_bin,
                                   female_Model_4_discrim_bin,
                                   female_Model_5_discrim_bin,
                                   female_Model_6_discrim_bin,
                                   female_Model_7_discrim_bin)

write.csv(female_results_discrim_bin, paste(OUTPUT_ROOT, "female_results_discrim_binrestricted__V4.csv", sep=""))



female_results_discrim_bin_table_col= cbind(female_Model_1_discrim_bin,
                                            female_Model_2_discrim_bin,
                                            female_Model_3_discrim_bin,
                                            female_Model_4_discrim_bin,
                                            female_Model_5_discrim_bin,
                                            female_Model_6_discrim_bin,
                                            female_Model_7_discrim_bin)

write.csv(female_results_discrim_bin_table_col, paste(OUTPUT_ROOT, "female_results_discrim_bin_table_colrestricted__V4.csv", sep=""))


#############


###### run all models for BMI_dataset 
BMI_Model_1_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                 Model_n = Model_1, 
                                                 Model_name = "Model_1")


BMI_Model_2_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                 Model_n = Model_2, 
                                                 Model_name = "Model_2")


BMI_Model_3_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                 Model_n = Model_3)



BMI_Model_4_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                 Model_n = Model_4)



BMI_Model_5_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                 Model_n = Model_5)


BMI_Model_6_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                 Model_n = Model_6)


BMI_Model_7_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                 Model_n = Model_7)



BMI_results_discrim_bin = rbind(BMI_Model_1_discrim_bin,
                                BMI_Model_2_discrim_bin,
                                BMI_Model_3_discrim_bin,
                                BMI_Model_4_discrim_bin,
                                BMI_Model_5_discrim_bin,
                                BMI_Model_6_discrim_bin,
                                BMI_Model_7_discrim_bin)

write.csv(BMI_results_discrim_bin, paste(OUTPUT_ROOT, "BMI_results_discrim_binrestricted__V4.csv", sep=""))

BMI_results_discrim_bin_table_col = cbind(BMI_Model_1_discrim_bin,
                                          BMI_Model_2_discrim_bin,
                                          BMI_Model_3_discrim_bin,
                                          BMI_Model_4_discrim_bin,
                                          BMI_Model_5_discrim_bin,
                                          BMI_Model_6_discrim_bin,
                                          BMI_Model_7_discrim_bin)

write.csv(BMI_results_discrim_bin_table_col, paste(OUTPUT_ROOT, "BMI_results_discrim_bin_table_colrestricted__V4.csv", sep=""))
#############



###### run all models for BMI_dataset 
BMI_Model_noBMIcov_1_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                          Model_n = Model_noBMIcov_1, 
                                                          Model_name = "Model_noBMIcov_1")


BMI_Model_noBMIcov_1_discrim_bin = cbind(NA, NA, NA)
colnames(BMI_Model_noBMIcov_1_discrim_bin) = c("hazard ratio",    "5% CI",  "95% CI") 

BMI_Model_noBMIcov_2_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                          Model_n = Model_noBMIcov_2, 
                                                          Model_name = "Model_noBMIcov_2")


BMI_Model_noBMIcov_3_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                          Model_n = Model_noBMIcov_3)



BMI_Model_noBMIcov_4_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                          Model_n = Model_noBMIcov_4)



BMI_Model_noBMIcov_5_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                          Model_n = Model_noBMIcov_5)


BMI_Model_noBMIcov_6_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                          Model_n = Model_noBMIcov_6)


BMI_Model_noBMIcov_7_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                          Model_n = Model_noBMIcov_7)


BMI_results_discrim_bin_noBMIcov = rbind(BMI_Model_noBMIcov_1_discrim_bin,
                                         BMI_Model_noBMIcov_2_discrim_bin,
                                         BMI_Model_noBMIcov_3_discrim_bin,
                                         BMI_Model_noBMIcov_4_discrim_bin,
                                         BMI_Model_noBMIcov_5_discrim_bin,
                                         BMI_Model_noBMIcov_6_discrim_bin,
                                         BMI_Model_noBMIcov_7_discrim_bin)

write.csv(BMI_results_discrim_bin_noBMIcov, paste(OUTPUT_ROOT, "BMI_results_discrim_bin_noBMIcovrestricted__V4.csv", sep=""))



BMI_results_discrim_bin_noBMIcov_table_col = cbind(BMI_Model_noBMIcov_1_discrim_bin,
                                                   BMI_Model_noBMIcov_2_discrim_bin,
                                                   BMI_Model_noBMIcov_3_discrim_bin,
                                                   BMI_Model_noBMIcov_4_discrim_bin,
                                                   BMI_Model_noBMIcov_5_discrim_bin,
                                                   BMI_Model_noBMIcov_6_discrim_bin,
                                                   BMI_Model_noBMIcov_7_discrim_bin)

write.csv(BMI_results_discrim_bin_noBMIcov_table_col, paste(OUTPUT_ROOT, "BMI_results_discrim_bin_noBMIcov_table_colrestricted__V4.csv", sep=""))




#############


###### run all models for race other than white
race_Model_1_discrim_bin = discrim_bin_model_func(data_wce_subset = race_dataset, 
                                                  Model_n = Model_1, 
                                                  Model_name = "Model_1")


race_Model_2_discrim_bin = discrim_bin_model_func(data_wce_subset = race_dataset, 
                                                  Model_n = Model_2, 
                                                  Model_name = "Model_2")


race_Model_3_discrim_bin = discrim_bin_model_func(data_wce_subset = race_dataset, 
                                                  Model_n = Model_3)



race_Model_4_discrim_bin = discrim_bin_model_func(data_wce_subset = race_dataset, 
                                                  Model_n = Model_4)



race_Model_5_discrim_bin = discrim_bin_model_func(data_wce_subset = race_dataset, 
                                                  Model_n = Model_5)


race_Model_6_discrim_bin = discrim_bin_model_func(data_wce_subset = race_dataset, 
                                                  Model_n = Model_6)


race_Model_7_discrim_bin = discrim_bin_model_func(data_wce_subset = race_dataset, 
                                                  Model_n = Model_7)



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

###### run all models for religion other than protestant or catholic 
religion_Model_1_discrim_bin = discrim_bin_model_func(data_wce_subset = religion_dataset, 
                                                      Model_n = Model_1, 
                                                      Model_name = "Model_1")


religion_Model_2_discrim_bin = discrim_bin_model_func(data_wce_subset = religion_dataset, 
                                                      Model_n = Model_2, 
                                                      Model_name = "Model_2")


religion_Model_3_discrim_bin = discrim_bin_model_func(data_wce_subset = religion_dataset, 
                                                      Model_n = Model_3)



religion_Model_4_discrim_bin = discrim_bin_model_func(data_wce_subset = religion_dataset, 
                                                      Model_n = Model_4)



religion_Model_5_discrim_bin = discrim_bin_model_func(data_wce_subset = religion_dataset, 
                                                      Model_n = Model_5)


religion_Model_6_discrim_bin = discrim_bin_model_func(data_wce_subset = religion_dataset, 
                                                      Model_n = Model_6)


religion_Model_7_discrim_bin = discrim_bin_model_func(data_wce_subset = religion_dataset, 
                                                      Model_n = Model_7)


religion_results_discrim_bin = rbind(religion_Model_1_discrim_bin,
                                     religion_Model_2_discrim_bin,
                                     religion_Model_3_discrim_bin,
                                     religion_Model_4_discrim_bin,
                                     religion_Model_5_discrim_bin,
                                     religion_Model_6_discrim_bin,
                                     religion_Model_7_discrim_bin)

write.csv(religion_results_discrim_bin, paste(OUTPUT_ROOT, "religion_results_discrim_binrestricted__V4.csv", sep=""))


religion_results_discrim_bin_table_col = cbind(religion_Model_1_discrim_bin,
                                               religion_Model_2_discrim_bin,
                                               religion_Model_3_discrim_bin,
                                               religion_Model_4_discrim_bin,
                                               religion_Model_5_discrim_bin,
                                               religion_Model_6_discrim_bin,
                                               religion_Model_7_discrim_bin)

write.csv(religion_results_discrim_bin_table_col, paste(OUTPUT_ROOT, "religion_results_discrim_bin_table_colrestricted__V4.csv", sep=""))

#############

