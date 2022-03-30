
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
#WCE_dataset_race$discrim_harassed_bin

#WCE_dataset_race.csv 
#WCE_dataset_race.csv
#WCE_dataset_national_origin_ousideUS
#WCE_dataset_race.csv
#WCE_dataset_religion.csv

#unique(WCE_dataset_race$summary_mean_score_discrim_bin)
#unique(WCE_dataset_race$discrim_harassed_bin)
#unique(WCE_dataset_race$discrim_lessrespect_bin)
#unique(WCE_dataset_race$discrim_medical_bin)
#unique(WCE_dataset_race$discrim_notclever_bin)
#unique(WCE_dataset_race$discrim_poorerservice_bin)
#unique(WCE_dataset_race$discrim_afraidothers_bin)


## number_reasons_discrimination

#################################### ######## ########################################################################
# Model 1: age
# Model 2: age and race 
# Model 3: age, race, wealth,  (1)
# Model 4: age, race, hypertension  (2)
# Model 5: age, race, hypertension, wealth 
# Model 6: smoking, physical activity, alcohol consumption
# Model 7: age, race,  smoking, physical activity, alcohol consumption  (3)
# Model 8:age, race, smoking, physical activity, alcohol consumption, hypertension
# Model 9: age, race, CVD  (4)
# Model 10: age, race,CVD, hypertension 
# Model 11: age, race, smoking, physical activity, alcohol consumption, CVD  
# Model 12: age, race, smoking, physical activity, alcohol consumption, CVD, hypertension  (5)
# Model 13: age, race, depression  (6)
# Model 14: age, race, smoking, physical activity, alcohol consumption, depression (7)
# Model 15: age, race, hypertension, depression 
# Model 16: age, race,CVD, hypertension, depression 
# Model 17: age, race, smoking, physical activity, alcohol consumption, depression
# Model 18: age, race, smoking, physical activity, alcohol consumption, CVD, hypertension, depression  (8)
#######
#age, sex, wealth, ethnicity, smoking, physical activity, alcohol consumption, race, hypertension, CVD

#### CVDS are: 
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


###### drop NAs and weird strings like " NA", THE WCE ANALYSIS DOES NOT RUN WITH NAs

WCE_dataset_race = WCE_dataset_race %>% drop_na(diabetes_new_bin)
unique(WCE_dataset_race$diabetes_new_bin)


WCE_dataset_race = WCE_dataset_race %>% drop_na(discrim_harassed_bin)




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
#################################### race ########################################################################

########## SUMMARY MEAN SCORE #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...
data_wce_race = sort_timepoints(data = WCE_dataset_race)

nrow(data_wce_race)
######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018
#myvars <- c("HHIDPN", "timepoints_indiv", "start_new", "stop_new", "diabetes_new_bin", "continious_age",
#            "summary_mean_score_discrim_bin", "discrim_harassed_bin", "discrim_lessrespect_bin", "discrim_medical_bin", "discrim_notclever_bin", "discrim_poorerservice_bin", "discrim_afraidothers_bin")

#data_wce_race <- data_wce_race_before[myvars]

unique(data_wce_race$discrim_harassed_bin)


unique(data_wce_race$timepoints_indiv)
unique(data_wce_race$start_new)
unique(data_wce_race$stop_new)
unique(data_wce_race$HHIDPN)
unique(data_wce_race$diabetes_new_bin)
unique(data_wce_race$discrim_harassed_bin)

unique(data_wce_race$hypertension_new_bin)


race_all_results = data.frame()


data_wce_race$discrim_harassed_bin = case_when(data_wce_race$discrim_harassed == 1 ~ 1, 
                                                 data_wce_race$discrim_harassed == 2 ~ 1, 
                                                 data_wce_race$discrim_harassed == 3 ~ 1, 
                                                 data_wce_race$discrim_harassed == 4 ~ 1, 
                                                 data_wce_race$discrim_harassed == 5 ~ 0, 
                                                 data_wce_race$discrim_harassed == 6 ~ 0,
                                                 data_wce_race$discrim_harassed == 0 ~ 0) 

# Model 7: age, race,  smoking, physical activity, alcohol consumption  (3)

race_discrim_harassed_bin_age = summary_score_WCE_analysis(data_WCE = data_wce_race,
                                                             exposure = "discrim_harassed_bin", 
                                                             outcome = "diabetes_new_bin", 
                                                             #covariates_list = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))
                                                             #covariates_list = c("assessed_BMI", "continious_age", "heartcondition_ever_bin", "heartcondition_new_bin", "angina_new_bin", "stroke_new_bin", "heartfailure2yrs_bin", "heartattack_ever_bin", "heartattack_new_bin"))
                                                             #covariates_list = c("assessed_BMI", "continious_age", "checklist_depression_bin"))
                                                             covariates_list = c("assessed_BMI", "continious_age", "checklist_depression_bin", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", "smokes_now_bin", "hypertension_new_bin"))

#covariates_list = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))
#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA"))
#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin"))

#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

race_discrim_harassed_bin_age_HR = race_discrim_harassed_bin_age[1]

race_discrim_harassed_bin_model_14_stats_recoded = race_discrim_harassed_bin_age[2]


unique(data_wce_race$continious_age)
unique(data_wce_race$diabetes_new_bin)
unique(data_wce_race$summary_mean_score_discrim_bin)

######## bootstrapped CIs for the HRs from the above model 
race_discrim_harassed_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_race,
                                                                    exposure = "discrim_harassed_bin", 
                                                                    outcome = "diabetes_new_bin", 
                                                                    #covariates_list = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))
                                                                    #covariates_list = c("assessed_BMI", "continious_age", "heartcondition_ever_bin", "heartcondition_new_bin", "angina_new_bin", "stroke_new_bin", "heartfailure2yrs_bin", "heartattack_ever_bin", "heartattack_new_bin"))
                                                                    #covariates_list = c("assessed_BMI", "continious_age", "checklist_depression_bin"))
                                                                    covariates_list = c("assessed_BMI", "continious_age", "checklist_depression_bin", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", "smokes_now_bin", "hypertension_new_bin"))

#covariates_list = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA"))
#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin"))

#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

write.csv(race_discrim_harassed_bin_model_14_stats_recoded, paste(OUTPUT_ROOT, "stats/race_overal_discrim_model_14_stats_recoded.csv", sep=""))


race_discrim_harassed_bin_age_HR = unlist(race_discrim_harassed_bin_age_HR)
race_harassed_bin_age_results = cbind(race_discrim_harassed_bin_age_HR, race_discrim_harassed_bin_age_CI)

race_all_results = rbind(race_harassed_bin_age_results, race_all_results)
colnames(race_all_results) = c("hazard ratio", "5% CI", "95% CI")



##########

########## discrim_lessrespect_bin #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...



data_wce_race$discrim_lessrespect_bin = case_when(data_wce_race$discrim_lessrespect == 1 ~ 1, 
                                                    data_wce_race$discrim_lessrespect == 2 ~ 1, 
                                                    data_wce_race$discrim_lessrespect == 3 ~ 1, 
                                                    data_wce_race$discrim_lessrespect == 4 ~ 1, 
                                                    data_wce_race$discrim_lessrespect == 5 ~ 0, 
                                                    data_wce_race$discrim_lessrespect == 6 ~ 0,
                                                    data_wce_race$discrim_lessrespect == 0 ~ 0) 

######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018
unique(data_wce_race$discrim_lessrespect_bin)

race_discrim_lessrespect_bin_age = summary_score_WCE_analysis(data_WCE = data_wce_race,
                                                                exposure = "discrim_lessrespect_bin", 
                                                                outcome = "diabetes_new_bin", 
                                                                #covariates_list = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))
                                                                #covariates_list = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", "smokes_now_bin", "hypertension_new_bin"))
                                                                #covariates_list = c("assessed_BMI", "continious_age", "heartcondition_ever_bin", "heartcondition_new_bin", "angina_new_bin", "stroke_new_bin", "heartfailure2yrs_bin", "heartattack_ever_bin", "heartattack_new_bin"))
                                                                #covariates_list = c("assessed_BMI", "continious_age", "checklist_depression_bin"))
                                                                covariates_list = c("assessed_BMI", "continious_age", "checklist_depression_bin", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin')) 

#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA"))

#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin"))
#covariates_list = c("assessed_BMI", "continious_age"))
#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

race_discrim_lessrespect_bin_age_HR = race_discrim_lessrespect_bin_age[1]

race_discrim_lessrespect_bin_model_14_stats_recoded = race_discrim_lessrespect_bin_age[2]


######## bootstrapped CIs for the HRs from the above model 
race_discrim_lessrespect_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_race,
                                                                       exposure = "discrim_lessrespect_bin", 
                                                                       outcome = "diabetes_new_bin", 
                                                                       #covariates_list = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))
                                                                       #covariates_list = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", "smokes_now_bin", "hypertension_new_bin"))
                                                                       #covariates_list = c("assessed_BMI", "continious_age", "heartcondition_ever_bin", "heartcondition_new_bin", "angina_new_bin", "stroke_new_bin", "heartfailure2yrs_bin", "heartattack_ever_bin", "heartattack_new_bin"))
                                                                       #covariates_list = c("assessed_BMI", "continious_age", "checklist_depression_bin"))
                                                                       covariates_list = c("assessed_BMI", "continious_age", "checklist_depression_bin", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin')) 

#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA"))
#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin"))

#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

write.csv(race_discrim_lessrespect_bin_model_14_stats_recoded, paste(OUTPUT_ROOT, "stats/race_discrim_lessrespect_bin_model_14_stats_recoded.csv", sep=""))


race_discrim_lessrespect_bin_age_HR = unlist(race_discrim_lessrespect_bin_age_HR)
race_discrim_lessrespect_bin_age_results = cbind(race_discrim_lessrespect_bin_age_HR, race_discrim_lessrespect_bin_age_CI)


colnames(race_discrim_lessrespect_bin_age_results) = c("hazard ratio", "5% CI", "95% CI")
race_all_results = rbind(race_all_results, race_discrim_lessrespect_bin_age_results)



########## discrim_medical_bin   #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...


data_wce_race$discrim_medical_bin = case_when(data_wce_race$discrim_medical == 1 ~ 1, 
                                                data_wce_race$discrim_medical == 2 ~ 1, 
                                                data_wce_race$discrim_medical == 3 ~ 1, 
                                                data_wce_race$discrim_medical == 4 ~ 1, 
                                                data_wce_race$discrim_medical == 5 ~ 0, 
                                                data_wce_race$discrim_medical == 6 ~ 0,
                                                data_wce_race$discrim_medical == 0 ~ 0) 

######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018


race_discrim_medical_bin_age = summary_score_WCE_analysis(data_WCE = data_wce_race,
                                                            exposure = "discrim_medical_bin", 
                                                            outcome = "diabetes_new_bin", 
                                                            #covariates_list = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))
                                                            #covariates_list = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", "smokes_now_bin", "hypertension_new_bin"))
                                                            #covariates_list = c("assessed_BMI", "continious_age", "heartcondition_ever_bin", "heartcondition_new_bin", "angina_new_bin", "stroke_new_bin", "heartfailure2yrs_bin", "heartattack_ever_bin", "heartattack_new_bin"))
                                                            #covariates_list = c("assessed_BMI", "continious_age", "checklist_depression_bin"))
                                                            covariates_list = c("assessed_BMI", "continious_age", "checklist_depression_bin", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin')) 

#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA"))
#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

race_discrim_medical_bin_age_HR = race_discrim_medical_bin_age[1]

race_discrim_medical_bin_model_14_stats_recoded = race_discrim_medical_bin_age[2]



######## bootstrapped CIs for the HRs from the above model 
race_discrim_medical_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_race,
                                                                   exposure = "discrim_medical_bin", 
                                                                   outcome = "diabetes_new_bin", 
                                                                   #covariates_list = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))
                                                                   #covariates_list = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", "smokes_now_bin", "hypertension_new_bin"))
                                                                   #covariates_list = c("assessed_BMI", "continious_age", "heartcondition_ever_bin", "heartcondition_new_bin", "angina_new_bin", "stroke_new_bin", "heartfailure2yrs_bin", "heartattack_ever_bin", "heartattack_new_bin"))
                                                                   #covariates_list = c("assessed_BMI", "continious_age", "checklist_depression_bin"))
                                                                   covariates_list = c("assessed_BMI", "continious_age", "checklist_depression_bin", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin"))
#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA"))

#covariates_list = c("continious_age"))

write.csv(race_discrim_medical_bin_model_14_stats_recoded, paste(OUTPUT_ROOT, "stats/race_discrim_medical_bin_model_14_stats_recoded.csv", sep=""))



race_discrim_medical_bin_age_HR = unlist(race_discrim_medical_bin_age_HR)
race_discrim_medical_bin_age_results = cbind(race_discrim_medical_bin_age_HR, race_discrim_medical_bin_age_CI)


colnames(race_discrim_medical_bin_age_results) = c("hazard ratio", "5% CI", "95% CI")
race_all_results = rbind(race_all_results, race_discrim_medical_bin_age_results)

########## discrim_notclever_bin #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...


data_wce_race$discrim_notclever_bin = case_when(data_wce_race$discrim_notclever == 1 ~ 1, 
                                                  data_wce_race$discrim_notclever == 2 ~ 1, 
                                                  data_wce_race$discrim_notclever == 3 ~ 1, 
                                                  data_wce_race$discrim_notclever == 4 ~ 1, 
                                                  data_wce_race$discrim_notclever == 5 ~ 0, 
                                                  data_wce_race$discrim_notclever == 6 ~ 0,
                                                  data_wce_race$discrim_notclever == 0 ~ 0) 
######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018


race_discrim_notclever_bin_age = summary_score_WCE_analysis(data_WCE = data_wce_race,
                                                              exposure = "discrim_notclever_bin", 
                                                              outcome = "diabetes_new_bin", 
                                                              #covariates_list = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))
                                                              #covariates_list = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", "smokes_now_bin", "hypertension_new_bin"))
                                                              #covariates_list = c("assessed_BMI", "continious_age", "heartcondition_ever_bin", "heartcondition_new_bin", "angina_new_bin", "stroke_new_bin", "heartfailure2yrs_bin", "heartattack_ever_bin", "heartattack_new_bin"))
                                                              #covariates_list = c("assessed_BMI", "continious_age", "checklist_depression_bin"))
                                                              covariates_list = c("assessed_BMI", "continious_age", "checklist_depression_bin", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA"))
#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

race_discrim_notclever_bin_age_HR = race_discrim_notclever_bin_age[1]

race_discrim_notclever_bin_model_14_stats_recoded = race_discrim_notclever_bin_age[2]



######## bootstrapped CIs for the HRs from the above model 
race_discrim_notclever_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_race,
                                                                     exposure = "discrim_notclever_bin", 
                                                                     outcome = "diabetes_new_bin", 
                                                                     #covariates_list = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))
                                                                     #covariates_list = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", "smokes_now_bin", "hypertension_new_bin"))
                                                                     #covariates_list = c("assessed_BMI", "continious_age", "heartcondition_ever_bin", "heartcondition_new_bin", "angina_new_bin", "stroke_new_bin", "heartfailure2yrs_bin", "heartattack_ever_bin", "heartattack_new_bin"))
                                                                     #covariates_list = c("assessed_BMI", "continious_age", "checklist_depression_bin"))
                                                                     covariates_list = c("assessed_BMI", "continious_age", "checklist_depression_bin", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA"))

#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin"))
#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

write.csv(race_discrim_notclever_bin_model_14_stats_recoded, paste(OUTPUT_ROOT, "stats/race_discrim_notclever_bin_model_14_stats_recoded.csv", sep=""))




race_discrim_notclever_bin_age_HR = unlist(race_discrim_notclever_bin_age_HR)
race_discrim_notclever_bin_age_results = cbind(race_discrim_notclever_bin_age_HR, race_discrim_notclever_bin_age_CI)



colnames(race_discrim_notclever_bin_age_results) = c("hazard ratio", "5% CI", "95% CI")
race_all_results = rbind(race_all_results, race_discrim_notclever_bin_age_results)
########## discrim_poorerservice_bin #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...

unique(data_wce_race$discrim_poorerservice)
data_wce_race$discrim_poorerservice = as.numeric(data_wce_race$discrim_poorerservice) 

data_wce_race$discrim_poorerservice_bin = case_when(data_wce_race$discrim_poorerservice == 1 ~ 1, 
                                                      data_wce_race$discrim_poorerservice == 2 ~ 1, 
                                                      data_wce_race$discrim_poorerservice == 3 ~ 1, 
                                                      data_wce_race$discrim_poorerservice == 4 ~ 1, 
                                                      data_wce_race$discrim_poorerservice == 5 ~ 0, 
                                                      data_wce_race$discrim_poorerservice == 6 ~ 0) 
######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018


race_discrim_poorerservice_bin_age = summary_score_WCE_analysis(data_WCE = data_wce_race,
                                                                  exposure = "discrim_poorerservice_bin", 
                                                                  outcome = "diabetes_new_bin", 
                                                                  #covariates_list = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))
                                                                  #covariates_list = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", "smokes_now_bin", "hypertension_new_bin"))
                                                                  #covariates_list = c("assessed_BMI", "continious_age", "heartcondition_ever_bin", "heartcondition_new_bin", "angina_new_bin", "stroke_new_bin", "heartfailure2yrs_bin", "heartattack_ever_bin", "heartattack_new_bin"))
                                                                  #covariates_list = c("assessed_BMI", "continious_age", "checklist_depression_bin"))
                                                                  covariates_list = c("assessed_BMI", "continious_age", "checklist_depression_bin", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA"))
#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin"))

#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

race_discrim_poorerservice_bin_age_HR = race_discrim_poorerservice_bin_age[1]

race_discrim_poorerservice_bin_model_14_stats_recoded = race_discrim_poorerservice_bin_age[2]



######## bootstrapped CIs for the HRs from the above model 
race_discrim_poorerservice_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_race,
                                                                         exposure = "discrim_poorerservice_bin", 
                                                                         outcome = "diabetes_new_bin", 
                                                                         #covariates_list = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))
                                                                         #covariates_list = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", "smokes_now_bin", "hypertension_new_bin"))
                                                                         #covariates_list = c("assessed_BMI", "continious_age", "heartcondition_ever_bin", "heartcondition_new_bin", "angina_new_bin", "stroke_new_bin", "heartfailure2yrs_bin", "heartattack_ever_bin", "heartattack_new_bin"))
                                                                         #covariates_list = c("assessed_BMI", "continious_age", "checklist_depression_bin"))
                                                                         covariates_list = c("assessed_BMI", "continious_age", "checklist_depression_bin", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA"))
#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin"))

#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

write.csv(race_discrim_poorerservice_bin_model_14_stats_recoded, paste(OUTPUT_ROOT, "stats/race_discrim_poorerservice_bin_model_14_stats_recoded.csv", sep=""))





race_discrim_poorerservice_bin_age_HR = unlist(race_discrim_poorerservice_bin_age_HR)
race_discrim_poorerservice_bin_age_results = cbind(race_discrim_poorerservice_bin_age_HR, race_discrim_poorerservice_bin_age_CI)



colnames(race_discrim_poorerservice_bin_age_results) = c("hazard ratio", "5% CI", "95% CI")
race_all_results = rbind(race_all_results, race_discrim_poorerservice_bin_age_results)
########## AFRAID OTHERS #########

data_wce_race$discrim_afraidothers_bin = case_when(data_wce_race$discrim_afraidothers == 1 ~ 1, 
                                                     data_wce_race$discrim_afraidothers == 2 ~ 1, 
                                                     data_wce_race$discrim_afraidothers == 3 ~ 1, 
                                                     data_wce_race$discrim_afraidothers == 4 ~ 1, 
                                                     data_wce_race$discrim_afraidothers == 5 ~ 0, 
                                                     data_wce_race$discrim_afraidothers == 6 ~ 0,
                                                     data_wce_race$discrim_afraidothers == 0 ~ 0) 

######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018
race_afraid_others_age = summary_score_WCE_analysis(data_WCE = data_wce_race,
                                                      exposure = "discrim_afraidothers_bin", 
                                                      outcome = "diabetes_new_bin", 
                                                      #covariates_list = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))
                                                      #covariates_list = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", "smokes_now_bin", "hypertension_new_bin"))
                                                      #covariates_list = c("assessed_BMI", "continious_age", "heartcondition_ever_bin", "heartcondition_new_bin", "angina_new_bin", "stroke_new_bin", "heartfailure2yrs_bin", "heartattack_ever_bin", "heartattack_new_bin"))
                                                      #covariates_list = c("assessed_BMI", "continious_age", "checklist_depression_bin"))
                                                      covariates_list = c("assessed_BMI", "continious_age", "checklist_depression_bin", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA"))
#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

race_afraid_others_age_HR = race_afraid_others_age[1]

race_afraid_others_model_14_stats_recoded = race_afraid_others_age[2]



######## bootstrapped CIs for the HRs from the above model 
race_afraid_others_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_race,
                                                             exposure = "discrim_afraidothers_bin", 
                                                             outcome = "diabetes_new_bin", 
                                                             #covariates_list = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))
                                                             #covariates_list = c("assessed_BMI", "continious_age", "alcohol_days_week_new",  "vigarious_physical_activity_new", "smokes_now_bin", "hypertension_new_bin"))
                                                             #covariates_list = c("assessed_BMI", "continious_age", "heartcondition_ever_bin", "heartcondition_new_bin", "angina_new_bin", "stroke_new_bin", "heartfailure2yrs_bin", "heartattack_ever_bin", "heartattack_new_bin"))
                                                             #covariates_list = c("assessed_BMI", "continious_age", "checklist_depression_bin"))
                                                             covariates_list = c("assessed_BMI", "continious_age", "checklist_depression_bin", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA"))
#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin"))

#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

write.csv(race_afraid_others_model_14_stats_recoded, paste(OUTPUT_ROOT, "stats/race_afraid_others_model_14_stats_recoded.csv", sep=""))



race_afraid_others_age_HR = unlist(race_afraid_others_age_HR)
race_afraid_others_age_results = cbind(race_afraid_others_age_HR, race_afraid_others_age_CI)


colnames(race_afraid_others_age_results) = c("hazard ratio", "5% CI", "95% CI")
race_all_results = rbind(race_all_results, race_afraid_others_age_results)

variable = #c("summary mean score",
  c("harassed",  "less respect",   "medical", "not clever",  "poorer service", "afraid others") 



race_all_results  = cbind(variable, race_all_results)

write.csv(race_all_results, paste(OUTPUT_ROOT, "race_all_results_bin_model_14_test.csv", sep=""))

########
########
########
