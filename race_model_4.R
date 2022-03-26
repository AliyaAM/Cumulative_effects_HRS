
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

# WCE_dataset_race = read.csv(paste(SOURCE_data_ROOT, "WCE_race_BMI_diabetes_bin.csv", sep=""))
WCE_dataset_race = read.csv(paste(SOURCE_data_ROOT, "WCE_dataset_race.csv", sep=""))
unique(WCE_dataset_race$diabetes_new_bin)
WCE_dataset_race$discrim_harassed_bin

#WCE_dataset_race.csv 
#WCE_dataset_race.csv
#WCE_dataset_national_origin_ousideUS
#WCE_dataset_race.csv
#WCE_dataset_religion.csv

unique(WCE_dataset_race$summary_mean_score_discrim)
unique(WCE_dataset_race$discrim_harassed)
unique(WCE_dataset_race$discrim_lessrespect)
unique(WCE_dataset_race$discrim_medical)
unique(WCE_dataset_race$discrim_notclever)
unique(WCE_dataset_race$discrim_poorerservice)
unique(WCE_dataset_race$discrim_afraidothers)

unique(WCE_dataset_race$diabetes_new_bin)

unique(WCE_dataset_race$hypertension_new_bin)
## number_reasons_discrimination

#################################### ######## ########################################################################
#################################### race ########################################################################

########## SUMMARY MEAN SCORE #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...
data_wce_race_before = sort_timepoints(data = WCE_dataset_race)


######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018
myvars <- c("HHIDPN", 
            "timepoints_indiv", 
            "start_new", "stop_new", 
            "diabetes_new_bin",
            "continious_age", "assessed_BMI", "wealth_noIRA", 
            "summary_mean_score_discrim", "discrim_harassed", "discrim_lessrespect", "discrim_medical", "discrim_notclever", "discrim_poorerservice", "discrim_afraidothers")

data_wce_race <- data_wce_race_before[myvars]


data_wce_race$discrim_harassed_bin = case_when(data_wce_race$discrim_harassed == 1 ~ 1, 
                                                   data_wce_race$discrim_harassed == 2 ~ 1, 
                                                   data_wce_race$discrim_harassed == 3 ~ 1, 
                                                   data_wce_race$discrim_harassed == 4 ~ 1, 
                                                   data_wce_race$discrim_harassed == 5 ~ 0, 
                                                   data_wce_race$discrim_harassed == 6 ~ 0,
                                                   data_wce_race$discrim_harassed == 0 ~ 0) 

unique(data_wce_race$discrim_harassed_bin)

unique(data_wce_race$timepoints_indiv)
unique(data_wce_race$start_new)
unique(data_wce_race$stop_new)
unique(data_wce_race$HHIDPN)
unique(data_wce_race$diabetes_new_bin)
unique(data_wce_race$continious_age)




race_all_results = data.frame()
race_discrim_harassed_bin_age = summary_score_WCE_analysis(data_WCE = data_wce_race,
                                                               exposure = "discrim_harassed_bin", 
                                                               outcome = "diabetes_new_bin", 
                                                               #covariates_list = c("assessed_BMI", "continious_age",  "wealth_noIRA"))

#covariates_list = c("assessed_race", "continious_age", "wealth_noIRA"))
covariates_list = c("continious_age"))

race_discrim_harassed_bin_age_HR = race_discrim_harassed_bin_age[1]

race_discrim_harassed_bin_model_4_stats_recoded = race_discrim_harassed_bin_age[2]


unique(data_wce_race$continious_age)
unique(data_wce_race$diabetes_new_bin)
unique(data_wce_race$summary_mean_score_discrim_bin)

######## bootstrapped CIs for the HRs from the above model 
race_discrim_harassed_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_race,
                                                                      exposure = "discrim_harassed_bin", 
                                                                      outcome = "diabetes_new_bin", 
                                                                      #covariates_list = c("assessed_BMI", "continious_age",  "wealth_noIRA"))

#covariates_list = c("assessed_race", "continious_age", "wealth_noIRA"))
covariates_list = c("continious_age"))

write.csv(race_discrim_harassed_bin_model_4_stats_recoded, paste(OUTPUT_ROOT, "stats/race_overal_discrim_model_4_stats_recoded.csv", sep=""))


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
                                                                  #covariates_list = c("assessed_BMI", "continious_age"))
#covariates_list = c("assessed_race", "continious_age", "wealth_noIRA"))
covariates_list = c("continious_age"))

race_discrim_lessrespect_bin_age_HR = race_discrim_lessrespect_bin_age[1]

race_discrim_lessrespect_bin_model_4_stats_recoded = race_discrim_lessrespect_bin_age[2]


######## bootstrapped CIs for the HRs from the above model 
race_discrim_lessrespect_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_race,
                                                                         exposure = "discrim_lessrespect_bin", 
                                                                         outcome = "diabetes_new_bin", 
                                                                         #covariates_list = c("assessed_BMI", "continious_age",  "wealth_noIRA"))
#covariates_list = c("assessed_race", "continious_age", "wealth_noIRA"))
covariates_list = c("continious_age"))

write.csv(race_discrim_lessrespect_bin_model_4_stats_recoded, paste(OUTPUT_ROOT, "stats/race_discrim_lessrespect_bin_model_4_stats_recoded.csv", sep=""))


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
                                                              #covariates_list = c("assessed_BMI", "continious_age",  "wealth_noIRA"))
#covariates_list = c("assessed_race", "continious_age", "wealth_noIRA"))
covariates_list = c("continious_age"))

race_discrim_medical_bin_age_HR = race_discrim_medical_bin_age[1]

race_discrim_medical_bin_model_4_stats_recoded = race_discrim_medical_bin_age[2]



######## bootstrapped CIs for the HRs from the above model 
race_discrim_medical_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_race,
                                                                     exposure = "discrim_medical_bin", 
                                                                     outcome = "diabetes_new_bin", 
                                                                     #covariates_list = c("assessed_race", "continious_age", "wealth_noIRA"))
                                                                     #covariates_list = c("assessed_BMI", "continious_age",  "wealth_noIRA"))
covariates_list = c("continious_age"))

write.csv(race_discrim_medical_bin_model_4_stats_recoded, paste(OUTPUT_ROOT, "stats/race_discrim_medical_bin_model_4_stats_recoded.csv", sep=""))



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
                                                                #covariates_list = c("assessed_BMI", "continious_age",  "wealth_noIRA"))
#covariates_list = c("assessed_race", "continious_age", "wealth_noIRA"))
covariates_list = c("continious_age"))

race_discrim_notclever_bin_age_HR = race_discrim_notclever_bin_age[1]

race_discrim_notclever_bin_model_4_stats_recoded = race_discrim_notclever_bin_age[2]



######## bootstrapped CIs for the HRs from the above model 
race_discrim_notclever_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_race,
                                                                       exposure = "discrim_notclever_bin", 
                                                                       outcome = "diabetes_new_bin", 
                                                                       #covariates_list = c("assessed_BMI", "continious_age",  "wealth_noIRA"))
#covariates_list = c("assessed_race", "continious_age", "wealth_noIRA"))
covariates_list = c("continious_age"))

write.csv(race_discrim_notclever_bin_model_4_stats_recoded, paste(OUTPUT_ROOT, "stats/race_discrim_notclever_bin_model_4_stats_recoded.csv", sep=""))




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
                                                                    covariates_list = c("assessed_BMI", "continious_age",  "wealth_noIRA"))
#covariates_list = c("assessed_race", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

race_discrim_poorerservice_bin_age_HR = race_discrim_poorerservice_bin_age[1]

race_discrim_poorerservice_bin_model_4_stats_recoded = race_discrim_poorerservice_bin_age[2]



######## bootstrapped CIs for the HRs from the above model 
race_discrim_poorerservice_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_race,
                                                                           exposure = "discrim_poorerservice_bin", 
                                                                           outcome = "diabetes_new_bin", 
                                                                           covariates_list = c("assessed_BMI", "continious_age",  "wealth_noIRA"))
#covariates_list = c("assessed_race", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

write.csv(race_discrim_poorerservice_bin_model_4_stats_recoded, paste(OUTPUT_ROOT, "stats/race_discrim_poorerservice_bin_model_4_stats_recoded.csv", sep=""))





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
                                                        covariates_list = c("assessed_BMI", "continious_age",  "wealth_noIRA"))
#covariates_list = c("assessed_race", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

race_afraid_others_age_HR = race_afraid_others_age[1]

race_afraid_others_model_4_stats_recoded = race_afraid_others_age[2]



######## bootstrapped CIs for the HRs from the above model 
race_afraid_others_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_race,
                                                               exposure = "discrim_afraidothers_bin", 
                                                               outcome = "diabetes_new_bin", 
                                                               covariates_list = c("assessed_BMI", "continious_age",  "wealth_noIRA"))
#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

write.csv(race_afraid_others_model_4_stats_recoded, paste(OUTPUT_ROOT, "stats/race_afraid_others_model_4_stats_recoded.csv", sep=""))



race_afraid_others_age_HR = unlist(race_afraid_others_age_HR)
race_afraid_others_age_results = cbind(race_afraid_others_age_HR, race_afraid_others_age_CI)


colnames(race_afraid_others_age_results) = c("hazard ratio", "5% CI", "95% CI")
race_all_results = rbind(race_all_results, race_afraid_others_age_results)

variable = #c("summary mean score",
  c("harassed",  "less respect",   "medical", "not clever",  "poorer service", "afraid others")



race_all_results  = cbind(variable, race_all_results)

write.csv(race_all_results, paste(OUTPUT_ROOT, "race_all_results_bin_model_4.csv", sep=""))

########
########
########
