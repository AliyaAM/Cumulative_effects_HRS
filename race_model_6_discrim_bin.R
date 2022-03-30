
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


SOURCE_data_ROOT = "/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"
SOURCE_ROOT =  "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/"
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/Results/"

source((paste(SOURCE_ROOT, "sort_timepoints.R", sep="")))

source((paste(SOURCE_ROOT, "WCE_analysis.R", sep="")))
source((paste(SOURCE_ROOT, "summary_score_WCE_analysis.R", sep="")))

source((paste(SOURCE_ROOT, "summary_score_Bootstrapped_CI.R", sep="")))
source((paste(SOURCE_ROOT, "summary_score_Bootstrapped_CI.R", sep="")))

#WCE_dataset_race = read.csv(paste(SOURCE_data_ROOT, "WCE_dataset_race_diabetes.csv", sep=""))
#unique(WCE_dataset_race$diabetes_new_bin)
#WCE_dataset_race$discrim_bin

#WCE_dataset_race.csv 
#WCE_dataset_race.csv
#WCE_dataset_national_origin_ousideUS
#WCE_dataset_race.csv
#WCE_dataset_race.csv

#unique(WCE_dataset_race$summary_mean_score_discrim_bin)
#unique(WCE_dataset_race$discrim_bin)
#unique(WCE_dataset_race$discrim_lessrespect_bin)
#unique(WCE_dataset_race$discrim_medical_bin)
#unique(WCE_dataset_race$discrim_notclever_bin)
#unique(WCE_dataset_race$discrim_poorerservice_bin)
#unique(WCE_dataset_race$discrim_afraidothers_bin)


## number_reasons_discrimination

#################################### ######## ########################################################################


Model_6 = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin')
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



# Model 1: age      
# Model 2: age and BMI 
# Model 3: age, BMI, wealth,  (1)
# Model 4: age, BMI, hypertension  (2)
# Model 5: age, BMI, hypertension, wealth NAs 
# Model 6: smoking, physical activity, alcohol consumption
# Model 7: age, BMI,  smoking, physical activity, alcohol consumption  (3)
# Model 8:age, BMI, smoking, physical activity, alcohol consumption, hypertension
# Model 9: age, BMI, CVD  (4)
# Model 10: age, BMI,CVD, hypertension 
# Model 11: age, BMI, smoking, physical activity, alcohol consumption, CVD  NAs 
# Model 12: age, BMI, smoking, physical activity, alcohol consumption, CVD, hypertension  (5)  NAs 
# Model 13: age, BMI, depression  (6)
# Model 14: age, BMI, smoking, physical activity, alcohol consumption, depression (7)
# Model 15: age, BMI, hypertension, depression 
# Model 16: age, BMI,CVD, hypertension, depression 
# Model 17: age, BMI, smoking, physical activity, alcohol consumption, CVD, hypertension, depression  (8) NAs 
#######
#age, sex, wealth, ethnicity, smoking, physical activity, alcohol consumption, race, hypertension, CVD

#### CVDS are:
#"heartcondition_ever_bin", "heartcondition_new_bin", "angina_new_bin", "stroke_new_bin", "heartfailure2yrs_bin", "heartattack_ever_bin", "heartattack_new_bin"
#### Models with CVD are: 
# Model 9: age, BMI, CVD  (4)
# Model 10: age, BMI,CVD, hypertension 
# Model 11: age, BMI, smoking, physical activity, alcohol consumption, CVD  
# Model 12: age, BMI, smoking, physical activity, alcohol consumption, CVD, hypertension  (5)
# Model 16: age, BMI,CVD, hypertension, depression 
# Model 17: age, BMI, smoking, physical activity, alcohol consumption, CVD, hypertension, depression  (8)

#"heartcondition_ever_bin", "heartcondition_new_bin", "angina_new_bin", "stroke_new_bin", "heartfailure2yrs_bin", "heartattack_ever_bin", "heartattack_new_bin"
HRS2008_data = read.csv(paste(SOURCE_data_ROOT, "HRS_2008_data/HRS2008_data_short.csv", sep=""))
HRS2010_data = read.csv(paste(SOURCE_data_ROOT, "HRS_2010_data/HRS2010_data_short.csv", sep=""))
HRS2012_data = read.csv(paste(SOURCE_data_ROOT, "HRS_2012_data/HRS2012_data_short.csv", sep=""))
HRS2014_data = read.csv(paste(SOURCE_data_ROOT, "HRS_2014_data/HRS2014_data_short.csv", sep=""))
HRS2016_data = read.csv(paste(SOURCE_data_ROOT, "HRS_2016_data/HRS2016_data_short.csv", sep=""))
HRS2018_data = read.csv(paste(SOURCE_data_ROOT, "HRS_2018_data/HRS2018_data_short.csv", sep=""))

unique(HRS2008_data$smokes_now_bin)
unique(HRS2008_data$smokes_ever_bin)
unique(HRS2008_data$alcohol_days_week)
unique(HRS2008_data$vigarious_physical_activity)



unique(HRS2016_data$smokes_now_bin)
unique(HRS2016_data$smokes_ever_bin)
unique(HRS2016_data$alcohol_days_week)
unique(HRS2016_data$vigarious_physical_activity)

#physical activity: 1.every day; 2.>1 per week; 3.1 per week; 4.l-3 per mon; 5.never; .d=DK/NA; .r=RF

#################################### race ########################################################################
# # # # subset the data set to those with race
# # # # subset the data set to those with race
HRS2008_data_race = subset(HRS2008_data, HRS2008_data$race_white == 0) 
HRS2010_data_race  = subset(HRS2010_data, HRS2010_data$race_white == 0)
HRS2012_data_race  = subset(HRS2012_data, HRS2012_data$race_white == 0)
HRS2014_data_race  = subset(HRS2014_data, HRS2014_data$race_white == 0)
HRS2016_data_race  = subset(HRS2016_data, HRS2016_data$race_white == 0)
HRS2018_data_race  = subset(HRS2018_data, HRS2018_data$race_white == 0)

WCE_dataset_race = rbind(HRS2008_data_race,
                         HRS2010_data_race, 
                         HRS2012_data_race,
                         HRS2014_data_race, 
                         HRS2016_data_race,
                         HRS2018_data_race)


#diabetes_new is diabtes this wave 
#0.no
#1.yes
#3.disp prev record and has cond
#4.disp prev record and no cond
#.d=DK
#.r=RF      


# create binary CVD variable 


WCE_dataset_race$CVD[WCE_dataset_race$heartcondition_ever_bin == 1 | WCE_dataset_race$heartcondition_new_bin == 1 | WCE_dataset_race$angina_new_bin ==1 | WCE_dataset_race$stroke_new_bin == 1 | WCE_dataset_race$heartfailure2yrs_bin == 1 | WCE_dataset_race$heartattack_ever_bin == 1 | WCE_dataset_race$heartattack_new_bin == 1] <-1
WCE_dataset_race$CVD[WCE_dataset_race$heartcondition_ever_bin == 0 & WCE_dataset_race$heartcondition_new_bin == 0 & WCE_dataset_race$angina_new_bin ==0 & WCE_dataset_race$stroke_new_bin == 0 & WCE_dataset_race$heartfailure2yrs_bin == 0 & WCE_dataset_race$heartattack_ever_bin == 0 & WCE_dataset_race$heartattack_new_bin == 0] <-0

###### add binary esposure and binary outcome 


WCE_dataset_race$diabetes_new_bin = case_when(WCE_dataset_race$diabetes_new == 1 ~ 1, 
                                              WCE_dataset_race$diabetes_new == 0 ~ 0, 
                                              WCE_dataset_race$diabetes_new == 3 ~ 1, 
                                              WCE_dataset_race$diabetes_new == 4 ~ 0) 



#physical activity (original): 1.every day; 2.>1 per week; 3.1 per week; 4.l-3 per mon; 5.never; .d=DK/NA; .r=RF

WCE_dataset_race$vigarious_physical_activity_new = case_when(WCE_dataset_race$vigarious_physical_activity == 1 ~ 5, 
                                                             WCE_dataset_race$vigarious_physical_activity == 2 ~ 4, 
                                                             WCE_dataset_race$vigarious_physical_activity == 3 ~ 3, 
                                                             WCE_dataset_race$vigarious_physical_activity == 4 ~ 2, 
                                                             WCE_dataset_race$vigarious_physical_activity == 5 ~ 1) 



WCE_dataset_race$vigarious_physical_activity_bin = case_when(WCE_dataset_race$vigarious_physical_activity_new == 5 ~ 1, 
                                                             WCE_dataset_race$vigarious_physical_activity_new == 4 ~ 1, 
                                                             WCE_dataset_race$vigarious_physical_activity_new == 3 ~ 1, 
                                                             WCE_dataset_race$vigarious_physical_activity_new == 2 ~ 0, 
                                                             WCE_dataset_race$vigarious_physical_activity_new == 1 ~ 0) 

unique(WCE_dataset_race$alcohol_days_week)


WCE_dataset_race$alcohol_days_week_new =  na_if(WCE_dataset_race$alcohol_days_week, 8)
WCE_dataset_race$alcohol_days_week_new = na_if(WCE_dataset_race$alcohol_days_week_new, 9) 

unique(WCE_dataset_race$alcohol_days_week_new)



###### recode into single var  discrim_bin


WCE_dataset_race$discrim_harassed_bin = case_when(WCE_dataset_race$discrim_harassed == 1 ~ 1, 
                                                  WCE_dataset_race$discrim_harassed == 2 ~ 1, 
                                                  WCE_dataset_race$discrim_harassed == 3 ~ 1, 
                                                  WCE_dataset_race$discrim_harassed == 4 ~ 1, 
                                                  WCE_dataset_race$discrim_harassed == 5 ~ 0, 
                                                  WCE_dataset_race$discrim_harassed == 6 ~ 0,
                                                  WCE_dataset_race$discrim_harassed == 0 ~ 0) 




WCE_dataset_race$discrim_lessrespect_bin = case_when(WCE_dataset_race$discrim_lessrespect == 1 ~ 1, 
                                                     WCE_dataset_race$discrim_lessrespect == 2 ~ 1, 
                                                     WCE_dataset_race$discrim_lessrespect == 3 ~ 1, 
                                                     WCE_dataset_race$discrim_lessrespect == 4 ~ 1, 
                                                     WCE_dataset_race$discrim_lessrespect == 5 ~ 0, 
                                                     WCE_dataset_race$discrim_lessrespect == 6 ~ 0,
                                                     WCE_dataset_race$discrim_lessrespect == 0 ~ 0) 


########## discrim_medical_bin   #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...


WCE_dataset_race$discrim_medical_bin = case_when(WCE_dataset_race$discrim_medical == 1 ~ 1, 
                                                 WCE_dataset_race$discrim_medical == 2 ~ 1, 
                                                 WCE_dataset_race$discrim_medical == 3 ~ 1, 
                                                 WCE_dataset_race$discrim_medical == 4 ~ 1, 
                                                 WCE_dataset_race$discrim_medical == 5 ~ 0, 
                                                 WCE_dataset_race$discrim_medical == 6 ~ 0,
                                                 WCE_dataset_race$discrim_medical == 0 ~ 0) 




WCE_dataset_race$discrim_notclever_bin = case_when(WCE_dataset_race$discrim_notclever == 1 ~ 1, 
                                                   WCE_dataset_race$discrim_notclever == 2 ~ 1, 
                                                   WCE_dataset_race$discrim_notclever == 3 ~ 1, 
                                                   WCE_dataset_race$discrim_notclever == 4 ~ 1, 
                                                   WCE_dataset_race$discrim_notclever == 5 ~ 0, 
                                                   WCE_dataset_race$discrim_notclever == 6 ~ 0,
                                                   WCE_dataset_race$discrim_notclever == 0 ~ 0) 



unique(WCE_dataset_race$discrim_poorerservice)
WCE_dataset_race$discrim_poorerservice = as.numeric(WCE_dataset_race$discrim_poorerservice) 

WCE_dataset_race$discrim_poorerservice_bin = case_when(WCE_dataset_race$discrim_poorerservice == 1 ~ 1, 
                                                       WCE_dataset_race$discrim_poorerservice == 2 ~ 1, 
                                                       WCE_dataset_race$discrim_poorerservice == 3 ~ 1, 
                                                       WCE_dataset_race$discrim_poorerservice == 4 ~ 1, 
                                                       WCE_dataset_race$discrim_poorerservice == 5 ~ 0, 
                                                       WCE_dataset_race$discrim_poorerservice == 6 ~ 0) 



WCE_dataset_race$discrim_afraidothers_bin = case_when(WCE_dataset_race$discrim_afraidothers == 1 ~ 1, 
                                                      WCE_dataset_race$discrim_afraidothers == 2 ~ 1, 
                                                      WCE_dataset_race$discrim_afraidothers == 3 ~ 1, 
                                                      WCE_dataset_race$discrim_afraidothers == 4 ~ 1, 
                                                      WCE_dataset_race$discrim_afraidothers == 5 ~ 0, 
                                                      WCE_dataset_race$discrim_afraidothers == 6 ~ 0,
                                                      WCE_dataset_race$discrim_afraidothers == 0 ~ 0) 


WCE_dataset_race$discrim_bin = case_when(WCE_dataset_race$discrim_harassed_bin == 1 | WCE_dataset_race$discrim_lessrespect_bin == 1 | WCE_dataset_race$discrim_medical_bin  == 1 | WCE_dataset_race$discrim_notclever_bin == 1 | WCE_dataset_race$discrim_afraidothers_bin == 1 | WCE_dataset_race$discrim_poorerservice_bin == 1 ~ 1, 
                                         WCE_dataset_race$discrim_harassed_bin == 0 & WCE_dataset_race$discrim_lessrespect_bin == 0 & WCE_dataset_race$discrim_medical_bin  == 0 & WCE_dataset_race$discrim_notclever_bin == 0 & WCE_dataset_race$discrim_afraidothers_bin == 0 & WCE_dataset_race$discrim_poorerservice_bin == 0 ~ 0) 


###### drop NAs and weird strings like " NA", THE WCE ANALYSIS DOES NOT RUN WITH NAs
WCE_dataset_race = WCE_dataset_race %>% drop_na(diabetes_new_bin)
unique(WCE_dataset_race$diabetes_new_bin)

WCE_dataset_race = WCE_dataset_race %>% drop_na(discrim_bin)

WCE_dataset_race = WCE_dataset_race %>% drop_na(wealth_noIRA)


WCE_dataset_race = subset(WCE_dataset_race, HHIDPN != "3020")

WCE_dataset_race = subset(WCE_dataset_race , diabetes_new_bin != " NA")
unique(WCE_dataset_race$diabetes_new)

WCE_dataset_race = subset(WCE_dataset_race , summary_mean_score_discrim != " NA")
unique(WCE_dataset_race$summary_mean_score_discrim)

WCE_dataset_race = subset(WCE_dataset_race , discrim_harassed != " NA")
unique(WCE_dataset_race$discrim_harassed)


WCE_dataset_race = subset(WCE_dataset_race , discrim_lessrespect != " NA")
unique(WCE_dataset_race$discrim_lessrespect)

WCE_dataset_race = subset(WCE_dataset_race , discrim_medical != " NA")
unique(WCE_dataset_race$discrim_medical)

WCE_dataset_race = subset(WCE_dataset_race , discrim_notclever != " NA")
unique(WCE_dataset_race$discrim_notclever_bin)

WCE_dataset_race = subset(WCE_dataset_race , discrim_poorerservice != " NA")
unique(WCE_dataset_race$discrim_poorerservice)


WCE_dataset_race = subset(WCE_dataset_race , discrim_afraidothers != " NA")
unique(WCE_dataset_race$discrim_afraidothers)



WCE_dataset_race$alcohol_days_week_new = as.numeric(WCE_dataset_race$alcohol_days_week_new)
WCE_dataset_race$vigarious_physical_activity_new = as.numeric(WCE_dataset_race$vigarious_physical_activity_new)
WCE_dataset_race$smokes_now_bin = as.numeric(WCE_dataset_race$smokes_now_bin)
WCE_dataset_race$checklist_depression_bin = as.numeric(WCE_dataset_race$checklist_depression_bin)
WCE_dataset_race$wealth_noIRA = as.numeric(WCE_dataset_race$wealth_noIRA)

#################################### race ########################################################################

########## SUMMARY MEAN SCORE #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...
data_wce_race = sort_timepoints(data = WCE_dataset_race)

nrow(data_wce_race)
######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018
#myvars <- c("HHIDPN", "timepoints_indiv", "start_new", "stop_new", "diabetes_new_bin", "continious_age",
#            "summary_mean_score_discrim_bin", "discrim_bin", "discrim_lessrespect_bin", "discrim_medical_bin", "discrim_notclever_bin", "discrim_poorerservice_bin", "discrim_afraidothers_bin")

#data_wce_race <- data_wce_race_before[myvars]

unique(data_wce_race$discrim_bin)
unique(data_wce_race$timepoints_indiv)
unique(data_wce_race$start_new)
unique(data_wce_race$stop_new)
unique(data_wce_race$HHIDPN)
unique(data_wce_race$diabetes_new_bin)
unique(data_wce_race$discrim_bin)
unique(data_wce_race$hypertension_new_bin)


results = data.frame()

race_discrim_bin_Model_6 = summary_score_WCE_analysis(data_WCE = data_wce_race,
                                                      exposure = "discrim_bin", 
                                                      outcome = "diabetes_new_bin", 
                                                      covariates_list = Model_6)


race_discrim_bin_HR_Model_6 = race_discrim_bin_Model_6[1]

race_discrim_bin_Model_6_stats_recoded = race_discrim_bin_Model_6[2]


unique(data_wce_race$continious_age)
unique(data_wce_race$diabetes_new_bin)
unique(data_wce_race$summary_mean_score_discrim_bin)

######## bootstrapped CIs for the HRs from the above model 
race_discrim_bin_age_CI_Model_6  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_race,
                                                                 exposure = "discrim_bin", 
                                                                 outcome = "diabetes_new_bin",
                                                                 covariates_list = Model_6)


write.csv(race_discrim_bin_Model_6_stats_recoded, paste(OUTPUT_ROOT, "stats/race_discrim_BIN_Model_6_stats.csv", sep=""))


race_discrim_bin_HR_Model_6 = unlist(race_discrim_bin_HR_Model_6)
race_discrim_bin_results_Model_6 = cbind(race_discrim_bin_HR_Model_6, race_discrim_bin_age_CI_Model_6)

colnames(race_discrim_bin_results_Model_6) = c("hazard ratio", "5% CI", "95% CI")



write.csv(race_discrim_bin_results_Model_6, paste(OUTPUT_ROOT, "race_discrim_BIN_results_Model_6.csv", sep=""))



########
########
########
