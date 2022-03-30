
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

#WCE_dataset_national_origin = read.csv(paste(SOURCE_data_ROOT, "WCE_dataset_national_origin_diabetes.csv", sep=""))
#unique(WCE_dataset_national_origin$diabetes_new_bin)
#WCE_dataset_national_origin$discrim_harassed_bin

#WCE_dataset_national_origin.csv 
#WCE_dataset_national_origin.csv
#WCE_dataset_national_origin_ousideUS
#WCE_dataset_national_origin.csv
#WCE_dataset_national_origin.csv

#unique(WCE_dataset_national_origin$summary_mean_score_discrim_bin)
#unique(WCE_dataset_national_origin$discrim_harassed_bin)
#unique(WCE_dataset_national_origin$discrim_lessrespect_bin)
#unique(WCE_dataset_national_origin$discrim_medical_bin)
#unique(WCE_dataset_national_origin$discrim_notclever_bin)
#unique(WCE_dataset_national_origin$discrim_poorerservice_bin)
#unique(WCE_dataset_national_origin$discrim_afraidothers_bin)


## number_reasons_discrimination

#################################### ######## ########################################################################
# Model 1: age
# Model 2: age and BMI 
# Model 3: age, BMI, wealth,  (1)
# Model 4: age, BMI, hypertension  (2)
# Model 5: age, BMI, hypertension, wealth 
# Model 6: smoking, physical activity, alcohol consumption
# Model 7: age, BMI,  smoking, physical activity, alcohol consumption  (3)
# Model 8:age, BMI, smoking, physical activity, alcohol consumption, hypertension
# Model 9: age, BMI, CVD  (4)
# Model 10: age, BMI,CVD, hypertension 
# Model 11: age, BMI, smoking, physical activity, alcohol consumption, CVD  
# Model 12: age, BMI, smoking, physical activity, alcohol consumption, CVD, hypertension  (5)
# Model 13: age, BMI, depression  (6)
# Model 14: age, BMI, smoking, physical activity, alcohol consumption, depression (7)
# Model 15: age, BMI, hypertension, depression 
# Model 16: age, BMI,CVD, hypertension, depression 
# Model 17: age, BMI, smoking, physical activity, alcohol consumption, depression
# Model 18: age, BMI, smoking, physical activity, alcohol consumption, CVD, hypertension, depression  (8)
#######
#age, sex, wealth, ethnicity, smoking, physical activity, alcohol consumption, female, hypertension, CVD

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

#################################### national_origin ########################################################################
# # # # subset the data set to those with national_origin


HRS2008_data_national_origin_ousideUS = subset(HRS2008_data, HRS2008_data$national_origin_ousideUS == 1) 
HRS2010_data_national_origin_ousideUS = subset(HRS2010_data, HRS2010_data$national_origin_ousideUS == 1)
HRS2012_data_national_origin_ousideUS = subset(HRS2012_data, HRS2012_data$national_origin_ousideUS == 1)
HRS2014_data_national_origin_ousideUS = subset(HRS2014_data, HRS2014_data$national_origin_ousideUS == 1)
HRS2016_data_national_origin_ousideUS = subset(HRS2016_data, HRS2016_data$national_origin_ousideUS == 1)
HRS2018_data_national_origin_ousideUS = subset(HRS2018_data, HRS2018_data$national_origin_ousideUS == 1)


WCE_dataset_national_origin = rbind(HRS2008_data_national_origin_ousideUS,
                                    HRS2010_data_national_origin_ousideUS, 
                                    HRS2012_data_national_origin_ousideUS,
                                    HRS2014_data_national_origin_ousideUS, 
                                    HRS2016_data_national_origin_ousideUS,
                                    HRS2018_data_national_origin_ousideUS)


#diabetes_new is diabtes this wave 
#0.no
#1.yes
#3.disp prev record and has cond
#4.disp prev record and no cond
#.d=DK
#.r=RF      

###### add binary esposure and binary outcome 


WCE_dataset_national_origin$diabetes_new_bin = case_when(WCE_dataset_national_origin$diabetes_new == 1 ~ 1, 
                                                  WCE_dataset_national_origin$diabetes_new == 0 ~ 0, 
                                                  WCE_dataset_national_origin$diabetes_new == 3 ~ 1, 
                                                  WCE_dataset_national_origin$diabetes_new == 4 ~ 0) 



#physical activity (original): 1.every day; 2.>1 per week; 3.1 per week; 4.l-3 per mon; 5.never; .d=DK/NA; .r=RF

WCE_dataset_national_origin$vigarious_physical_activity_new = case_when(WCE_dataset_national_origin$vigarious_physical_activity == 1 ~ 5, 
                                                                 WCE_dataset_national_origin$vigarious_physical_activity == 2 ~ 4, 
                                                                 WCE_dataset_national_origin$vigarious_physical_activity == 3 ~ 3, 
                                                                 WCE_dataset_national_origin$vigarious_physical_activity == 4 ~ 2, 
                                                                 WCE_dataset_national_origin$vigarious_physical_activity == 5 ~ 1) 



WCE_dataset_national_origin$vigarious_physical_activity_bin = case_when(WCE_dataset_national_origin$vigarious_physical_activity_new == 5 ~ 1, 
                                                                 WCE_dataset_national_origin$vigarious_physical_activity_new == 4 ~ 1, 
                                                                 WCE_dataset_national_origin$vigarious_physical_activity_new == 3 ~ 1, 
                                                                 WCE_dataset_national_origin$vigarious_physical_activity_new == 2 ~ 0, 
                                                                 WCE_dataset_national_origin$vigarious_physical_activity_new == 1 ~ 0) 

unique(WCE_dataset_national_origin$alcohol_days_week)


WCE_dataset_national_origin$alcohol_days_week_new =  na_if(WCE_dataset_national_origin$alcohol_days_week, 8)
WCE_dataset_national_origin$alcohol_days_week_new = na_if(WCE_dataset_national_origin$alcohol_days_week_new, 9) 

unique(WCE_dataset_national_origin$alcohol_days_week_new)


###### drop NAs and weird strings like " NA", THE WCE ANALYSIS DOES NOT RUN WITH NAs

WCE_dataset_national_origin = WCE_dataset_national_origin %>% drop_na(diabetes_new_bin)
unique(WCE_dataset_national_origin$diabetes_new_bin)


WCE_dataset_national_origin = WCE_dataset_national_origin %>% drop_na(discrim_harassed_bin)




WCE_dataset_national_origin = subset(WCE_dataset_national_origin, HHIDPN != "3020")

WCE_dataset_national_origin = subset(WCE_dataset_national_origin , diabetes_new_bin != " NA")
unique(WCE_dataset_national_origin$diabetes_new)

WCE_dataset_national_origin = subset(WCE_dataset_national_origin , summary_mean_score_discrim != " NA")
unique(WCE_dataset_national_origin$summary_mean_score_discrim)

WCE_dataset_national_origin = subset(WCE_dataset_national_origin , discrim_harassed != " NA")
unique(WCE_dataset_national_origin$discrim_harassed)


WCE_dataset_national_origin = subset(WCE_dataset_national_origin , discrim_lessrespect != " NA")
unique(WCE_dataset_national_origin$discrim_lessrespect)

WCE_dataset_national_origin = subset(WCE_dataset_national_origin , discrim_medical != " NA")
unique(WCE_dataset_national_origin$discrim_medical)

WCE_dataset_national_origin = subset(WCE_dataset_national_origin , discrim_notclever != " NA")
unique(WCE_dataset_national_origin$discrim_notclever_bin)

WCE_dataset_national_origin = subset(WCE_dataset_national_origin , discrim_poorerservice != " NA")
unique(WCE_dataset_national_origin$discrim_poorerservice)


WCE_dataset_national_origin = subset(WCE_dataset_national_origin , discrim_afraidothers != " NA")
unique(WCE_dataset_national_origin$discrim_afraidothers)



WCE_dataset_national_origin$alcohol_days_week_new = as.numeric(WCE_dataset_national_origin$alcohol_days_week_new)
WCE_dataset_national_origin$vigarious_physical_activity_new = as.numeric(WCE_dataset_national_origin$vigarious_physical_activity_new)
WCE_dataset_national_origin$smokes_now_bin = as.numeric(WCE_dataset_national_origin$smokes_now_bin)

#################################### national_origin ########################################################################

########## SUMMARY MEAN SCORE #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...
data_wce_national_origin = sort_timepoints(data = WCE_dataset_national_origin)

nrow(data_wce_national_origin)
######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018
#myvars <- c("HHIDPN", "timepoints_indiv", "start_new", "stop_new", "diabetes_new_bin", "continious_age",
#            "summary_mean_score_discrim_bin", "discrim_harassed_bin", "discrim_lessrespect_bin", "discrim_medical_bin", "discrim_notclever_bin", "discrim_poorerservice_bin", "discrim_afraidothers_bin")

#data_wce_national_origin <- data_wce_national_origin_before[myvars]

unique(data_wce_national_origin$discrim_harassed_bin)


unique(data_wce_national_origin$timepoints_indiv)
unique(data_wce_national_origin$start_new)
unique(data_wce_national_origin$stop_new)
unique(data_wce_national_origin$HHIDPN)
unique(data_wce_national_origin$diabetes_new_bin)
unique(data_wce_national_origin$discrim_harassed_bin)

unique(data_wce_national_origin$hypertension_new_bin)


national_origin_all_results = data.frame()


data_wce_national_origin$discrim_harassed_bin = case_when(data_wce_national_origin$discrim_harassed == 1 ~ 1, 
                                                   data_wce_national_origin$discrim_harassed == 2 ~ 1, 
                                                   data_wce_national_origin$discrim_harassed == 3 ~ 1, 
                                                   data_wce_national_origin$discrim_harassed == 4 ~ 1, 
                                                   data_wce_national_origin$discrim_harassed == 5 ~ 0, 
                                                   data_wce_national_origin$discrim_harassed == 6 ~ 0,
                                                   data_wce_national_origin$discrim_harassed == 0 ~ 0) 


national_origin_discrim_harassed_bin_age = summary_score_WCE_analysis(data_WCE = data_wce_national_origin,
                                                               exposure = "discrim_harassed_bin", 
                                                               outcome = "diabetes_new_bin", 
                                                               covariates_list = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))
#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA"))
#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin"))

#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

national_origin_discrim_harassed_bin_age_HR = national_origin_discrim_harassed_bin_age[1]

national_origin_discrim_harassed_bin_model_6_stats_recoded = national_origin_discrim_harassed_bin_age[2]


unique(data_wce_national_origin$continious_age)
unique(data_wce_national_origin$diabetes_new_bin)
unique(data_wce_national_origin$summary_mean_score_discrim_bin)

######## bootstrapped CIs for the HRs from the above model 
national_origin_discrim_harassed_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_national_origin,
                                                                      exposure = "discrim_harassed_bin", 
                                                                      outcome = "diabetes_new_bin", 
                                                                      covariates_list = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA"))
#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin"))

#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

write.csv(national_origin_discrim_harassed_bin_model_6_stats_recoded, paste(OUTPUT_ROOT, "stats/national_origin_overal_discrim_model_6_stats_recoded.csv", sep=""))


national_origin_discrim_harassed_bin_age_HR = unlist(national_origin_discrim_harassed_bin_age_HR)
national_origin_harassed_bin_age_results = cbind(national_origin_discrim_harassed_bin_age_HR, national_origin_discrim_harassed_bin_age_CI)

national_origin_all_results = rbind(national_origin_harassed_bin_age_results, national_origin_all_results)
colnames(national_origin_all_results) = c("hazard ratio", "5% CI", "95% CI")



##########

########## discrim_lessrespect_bin #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...



data_wce_national_origin$discrim_lessrespect_bin = case_when(data_wce_national_origin$discrim_lessrespect == 1 ~ 1, 
                                                      data_wce_national_origin$discrim_lessrespect == 2 ~ 1, 
                                                      data_wce_national_origin$discrim_lessrespect == 3 ~ 1, 
                                                      data_wce_national_origin$discrim_lessrespect == 4 ~ 1, 
                                                      data_wce_national_origin$discrim_lessrespect == 5 ~ 0, 
                                                      data_wce_national_origin$discrim_lessrespect == 6 ~ 0,
                                                      data_wce_national_origin$discrim_lessrespect == 0 ~ 0) 

######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018
unique(data_wce_national_origin$discrim_lessrespect_bin)

national_origin_discrim_lessrespect_bin_age = summary_score_WCE_analysis(data_WCE = data_wce_national_origin,
                                                                  exposure = "discrim_lessrespect_bin", 
                                                                  outcome = "diabetes_new_bin", 
                                                                  covariates_list = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin')) 

#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA"))

#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin"))
#covariates_list = c("assessed_BMI", "continious_age"))
#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

national_origin_discrim_lessrespect_bin_age_HR = national_origin_discrim_lessrespect_bin_age[1]

national_origin_discrim_lessrespect_bin_model_6_stats_recoded = national_origin_discrim_lessrespect_bin_age[2]


######## bootstrapped CIs for the HRs from the above model 
national_origin_discrim_lessrespect_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_national_origin,
                                                                         exposure = "discrim_lessrespect_bin", 
                                                                         outcome = "diabetes_new_bin", 
                                                                         covariates_list = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin')) 

#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA"))
#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin"))

#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

write.csv(national_origin_discrim_lessrespect_bin_model_6_stats_recoded, paste(OUTPUT_ROOT, "stats/national_origin_discrim_lessrespect_bin_model_6_stats_recoded.csv", sep=""))


national_origin_discrim_lessrespect_bin_age_HR = unlist(national_origin_discrim_lessrespect_bin_age_HR)
national_origin_discrim_lessrespect_bin_age_results = cbind(national_origin_discrim_lessrespect_bin_age_HR, national_origin_discrim_lessrespect_bin_age_CI)


colnames(national_origin_discrim_lessrespect_bin_age_results) = c("hazard ratio", "5% CI", "95% CI")
national_origin_all_results = rbind(national_origin_all_results, national_origin_discrim_lessrespect_bin_age_results)



########## discrim_medical_bin   #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...


data_wce_national_origin$discrim_medical_bin = case_when(data_wce_national_origin$discrim_medical == 1 ~ 1, 
                                                  data_wce_national_origin$discrim_medical == 2 ~ 1, 
                                                  data_wce_national_origin$discrim_medical == 3 ~ 1, 
                                                  data_wce_national_origin$discrim_medical == 4 ~ 1, 
                                                  data_wce_national_origin$discrim_medical == 5 ~ 0, 
                                                  data_wce_national_origin$discrim_medical == 6 ~ 0,
                                                  data_wce_national_origin$discrim_medical == 0 ~ 0) 

######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018


national_origin_discrim_medical_bin_age = summary_score_WCE_analysis(data_WCE = data_wce_national_origin,
                                                              exposure = "discrim_medical_bin", 
                                                              outcome = "diabetes_new_bin", 
                                                              covariates_list = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin')) 

#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA"))
#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

national_origin_discrim_medical_bin_age_HR = national_origin_discrim_medical_bin_age[1]

national_origin_discrim_medical_bin_model_6_stats_recoded = national_origin_discrim_medical_bin_age[2]



######## bootstrapped CIs for the HRs from the above model 
national_origin_discrim_medical_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_national_origin,
                                                                     exposure = "discrim_medical_bin", 
                                                                     outcome = "diabetes_new_bin", 
                                                                     covariates_list = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin"))
#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA"))

#covariates_list = c("continious_age"))

write.csv(national_origin_discrim_medical_bin_model_6_stats_recoded, paste(OUTPUT_ROOT, "stats/national_origin_discrim_medical_bin_model_6_stats_recoded.csv", sep=""))



national_origin_discrim_medical_bin_age_HR = unlist(national_origin_discrim_medical_bin_age_HR)
national_origin_discrim_medical_bin_age_results = cbind(national_origin_discrim_medical_bin_age_HR, national_origin_discrim_medical_bin_age_CI)


colnames(national_origin_discrim_medical_bin_age_results) = c("hazard ratio", "5% CI", "95% CI")
national_origin_all_results = rbind(national_origin_all_results, national_origin_discrim_medical_bin_age_results)

########## discrim_notclever_bin #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...


data_wce_national_origin$discrim_notclever_bin = case_when(data_wce_national_origin$discrim_notclever == 1 ~ 1, 
                                                    data_wce_national_origin$discrim_notclever == 2 ~ 1, 
                                                    data_wce_national_origin$discrim_notclever == 3 ~ 1, 
                                                    data_wce_national_origin$discrim_notclever == 4 ~ 1, 
                                                    data_wce_national_origin$discrim_notclever == 5 ~ 0, 
                                                    data_wce_national_origin$discrim_notclever == 6 ~ 0,
                                                    data_wce_national_origin$discrim_notclever == 0 ~ 0) 
######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018


national_origin_discrim_notclever_bin_age = summary_score_WCE_analysis(data_WCE = data_wce_national_origin,
                                                                exposure = "discrim_notclever_bin", 
                                                                outcome = "diabetes_new_bin", 
                                                                covariates_list = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA"))
#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

national_origin_discrim_notclever_bin_age_HR = national_origin_discrim_notclever_bin_age[1]

national_origin_discrim_notclever_bin_model_6_stats_recoded = national_origin_discrim_notclever_bin_age[2]



######## bootstrapped CIs for the HRs from the above model 
national_origin_discrim_notclever_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_national_origin,
                                                                       exposure = "discrim_notclever_bin", 
                                                                       outcome = "diabetes_new_bin", 
                                                                       covariates_list = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA"))

#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin"))
#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

write.csv(national_origin_discrim_notclever_bin_model_6_stats_recoded, paste(OUTPUT_ROOT, "stats/national_origin_discrim_notclever_bin_model_6_stats_recoded.csv", sep=""))




national_origin_discrim_notclever_bin_age_HR = unlist(national_origin_discrim_notclever_bin_age_HR)
national_origin_discrim_notclever_bin_age_results = cbind(national_origin_discrim_notclever_bin_age_HR, national_origin_discrim_notclever_bin_age_CI)



colnames(national_origin_discrim_notclever_bin_age_results) = c("hazard ratio", "5% CI", "95% CI")
national_origin_all_results = rbind(national_origin_all_results, national_origin_discrim_notclever_bin_age_results)
########## discrim_poorerservice_bin #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...

unique(data_wce_national_origin$discrim_poorerservice)
data_wce_national_origin$discrim_poorerservice = as.numeric(data_wce_national_origin$discrim_poorerservice) 

data_wce_national_origin$discrim_poorerservice_bin = case_when(data_wce_national_origin$discrim_poorerservice == 1 ~ 1, 
                                                        data_wce_national_origin$discrim_poorerservice == 2 ~ 1, 
                                                        data_wce_national_origin$discrim_poorerservice == 3 ~ 1, 
                                                        data_wce_national_origin$discrim_poorerservice == 4 ~ 1, 
                                                        data_wce_national_origin$discrim_poorerservice == 5 ~ 0, 
                                                        data_wce_national_origin$discrim_poorerservice == 6 ~ 0) 
######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018


national_origin_discrim_poorerservice_bin_age = summary_score_WCE_analysis(data_WCE = data_wce_national_origin,
                                                                    exposure = "discrim_poorerservice_bin", 
                                                                    outcome = "diabetes_new_bin", 
                                                                    covariates_list = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA"))
#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin"))

#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

national_origin_discrim_poorerservice_bin_age_HR = national_origin_discrim_poorerservice_bin_age[1]

national_origin_discrim_poorerservice_bin_model_6_stats_recoded = national_origin_discrim_poorerservice_bin_age[2]



######## bootstrapped CIs for the HRs from the above model 
national_origin_discrim_poorerservice_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_national_origin,
                                                                           exposure = "discrim_poorerservice_bin", 
                                                                           outcome = "diabetes_new_bin", 
                                                                           covariates_list = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA"))
#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin"))

#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

write.csv(national_origin_discrim_poorerservice_bin_model_6_stats_recoded, paste(OUTPUT_ROOT, "stats/national_origin_discrim_poorerservice_bin_model_6_stats_recoded.csv", sep=""))





national_origin_discrim_poorerservice_bin_age_HR = unlist(national_origin_discrim_poorerservice_bin_age_HR)
national_origin_discrim_poorerservice_bin_age_results = cbind(national_origin_discrim_poorerservice_bin_age_HR, national_origin_discrim_poorerservice_bin_age_CI)



colnames(national_origin_discrim_poorerservice_bin_age_results) = c("hazard ratio", "5% CI", "95% CI")
national_origin_all_results = rbind(national_origin_all_results, national_origin_discrim_poorerservice_bin_age_results)
########## AFRAID OTHERS #########

data_wce_national_origin$discrim_afraidothers_bin = case_when(data_wce_national_origin$discrim_afraidothers == 1 ~ 1, 
                                                       data_wce_national_origin$discrim_afraidothers == 2 ~ 1, 
                                                       data_wce_national_origin$discrim_afraidothers == 3 ~ 1, 
                                                       data_wce_national_origin$discrim_afraidothers == 4 ~ 1, 
                                                       data_wce_national_origin$discrim_afraidothers == 5 ~ 0, 
                                                       data_wce_national_origin$discrim_afraidothers == 6 ~ 0,
                                                       data_wce_national_origin$discrim_afraidothers == 0 ~ 0) 

######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018
national_origin_afraid_others_age = summary_score_WCE_analysis(data_WCE = data_wce_national_origin,
                                                        exposure = "discrim_afraidothers_bin", 
                                                        outcome = "diabetes_new_bin", 
                                                        covariates_list = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA"))
#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

national_origin_afraid_others_age_HR = national_origin_afraid_others_age[1]

national_origin_afraid_others_model_6_stats_recoded = national_origin_afraid_others_age[2]



######## bootstrapped CIs for the HRs from the above model 
national_origin_afraid_others_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_national_origin,
                                                               exposure = "discrim_afraidothers_bin", 
                                                               outcome = "diabetes_new_bin", 
                                                               covariates_list = c("alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin'))

#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin", "wealth_noIRA"))
#covariates_list = c("assessed_BMI", "continious_age", "hypertension_new_bin"))

#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

write.csv(national_origin_afraid_others_model_6_stats_recoded, paste(OUTPUT_ROOT, "stats/national_origin_afraid_others_model_6_stats_recoded.csv", sep=""))



national_origin_afraid_others_age_HR = unlist(national_origin_afraid_others_age_HR)
national_origin_afraid_others_age_results = cbind(national_origin_afraid_others_age_HR, national_origin_afraid_others_age_CI)


colnames(national_origin_afraid_others_age_results) = c("hazard ratio", "5% CI", "95% CI")
national_origin_all_results = rbind(national_origin_all_results, national_origin_afraid_others_age_results)

variable = #c("summary mean score",
  c("harassed",  "less respect",   "medical", "not clever",  "poorer service", "afraid others") 



national_origin_all_results  = cbind(variable, national_origin_all_results)

write.csv(national_origin_all_results, paste(OUTPUT_ROOT, "national_origin_all_results_bin_model_6_test.csv", sep=""))

########
########
########
