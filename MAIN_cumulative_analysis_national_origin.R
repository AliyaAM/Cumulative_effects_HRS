
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

SOURCE_data_ROOT = "/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"


SOURCE_ROOT =  "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/"
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/Results/"

source((paste(SOURCE_ROOT, "sort_timepoints.R", sep="")))

source((paste(SOURCE_ROOT, "WCE_analysis.R", sep="")))
source((paste(SOURCE_ROOT, "summary_score_WCE_analysis.R", sep="")))

source((paste(SOURCE_ROOT, "Bootstrapped_CI.R", sep="")))
source((paste(SOURCE_ROOT, "summary_score_Bootstrapped_CI.R", sep="")))

WCE_dataset_national_origin_ousideUS = read.csv(paste(SOURCE_data_ROOT, "WCE_dataset_national_origin_ousideUS.csv", sep=""))

#WCE_dataset_national_origin_ousideUS.csv 
#WCE_dataset_national_origin_ousideUS.csv
#WCE_dataset_national_origin_ousideUS
#WCE_dataset_race.csv
#WCE_dataset_religion.csv

## $summary_mean_score_discrim
## $discrim_harassed
## $discrim_lessrespect
## $discrim_medical
## $discrim_notclever
## $discrim_poorerservice
## $discrim_afraidothers


## number_reasons_discrimination

#################################### ######## ########################################################################
#################################### national_origin_ousideUS ########################################################################

########## SUMMARY MEAN SCORE #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...
data_wce_national_origin_ousideUS = sort_timepoints(data = WCE_dataset_national_origin_ousideUS)

######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018
#myvars <- c("HHIDPN", "timepoints_indiv", "start_new", "stop_new", "diabetes_new_bin", "continious_age",
#            "summary_mean_score_discrim_bin", "discrim_harassed_bin", "discrim_lessrespect_bin", "discrim_medical_bin", "discrim_notclever_bin", "discrim_poorerservice_bin", "discrim_afraidothers_bin")

#data_wce_national_origin_ousideUS <- data_wce_national_origin_ousideUS_before[myvars]


data_wce_national_origin_ousideUS$discrim_harassed_bin = case_when(data_wce_national_origin_ousideUS$discrim_harassed == 1 ~ 1, 
                                                   data_wce_national_origin_ousideUS$discrim_harassed == 2 ~ 1, 
                                                   data_wce_national_origin_ousideUS$discrim_harassed == 3 ~ 1, 
                                                   data_wce_national_origin_ousideUS$discrim_harassed == 4 ~ 1, 
                                                   data_wce_national_origin_ousideUS$discrim_harassed == 5 ~ 0, 
                                                   data_wce_national_origin_ousideUS$discrim_harassed == 6 ~ 0,
                                                   data_wce_national_origin_ousideUS$discrim_harassed == 0 ~ 0) 

unique(data_wce_national_origin_ousideUS$discrim_harassed_bin)

unique(data_wce_national_origin_ousideUS$timepoints_indiv)
unique(data_wce_national_origin_ousideUS$start_new)
unique(data_wce_national_origin_ousideUS$stop_new)
unique(data_wce_national_origin_ousideUS$HHIDPN)
unique(data_wce_national_origin_ousideUS$diabetes_new_bin)
unique(data_wce_national_origin_ousideUS$discrim_harassed)
unique(data_wce_national_origin_ousideUS$continious_age)


national_origin_ousideUS_all_results = data.frame()
national_origin_ousideUS_discrim_harassed_bin_age = summary_score_WCE_analysis(data_WCE = data_wce_national_origin_ousideUS,
                                                               exposure = "discrim_harassed_bin", 
                                                               outcome = "diabetes_new_bin", 
                                                               #covariates_list = c("assessed_national_origin_ousideUS", "continious_age", "wealth_noIRA"))
                                                               covariates_list = c("continious_age"))

national_origin_ousideUS_discrim_harassed_bin_age_HR = national_origin_ousideUS_discrim_harassed_bin_age[1]

national_origin_ousideUS_discrim_harassed_bin_stats_recoded = national_origin_ousideUS_discrim_harassed_bin_age[2]


unique(data_wce_national_origin_ousideUS$continious_age)
unique(data_wce_national_origin_ousideUS$diabetes_new_bin)
unique(data_wce_national_origin_ousideUS$summary_mean_score_discrim_bin)

######## bootstrapped CIs for the HRs from the above model 
national_origin_ousideUS_discrim_harassed_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_national_origin_ousideUS,
                                                                      exposure = "discrim_harassed_bin", 
                                                                      outcome = "diabetes_new_bin", 
                                                                      #covariates_list = c("assessed_national_origin_ousideUS", "continious_age", "wealth_noIRA"))
                                                                      covariates_list = c("continious_age"))

write.csv(national_origin_ousideUS_discrim_harassed_bin_stats_recoded, paste(OUTPUT_ROOT, "stats/national_origin_ousideUS_overal_discrim_stats_recoded.csv", sep=""))


national_origin_ousideUS_discrim_harassed_bin_age_HR = unlist(national_origin_ousideUS_discrim_harassed_bin_age_HR)
national_origin_ousideUS_harassed_bin_age_results = cbind(national_origin_ousideUS_discrim_harassed_bin_age_HR, national_origin_ousideUS_discrim_harassed_bin_age_CI)

national_origin_ousideUS_all_results = rbind(national_origin_ousideUS_harassed_bin_age_results, national_origin_ousideUS_all_results)
colnames(national_origin_ousideUS_all_results) = c("hazard ratio", "5% CI", "95% CI")



##########

########## discrim_lessrespect_bin #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...



data_wce_national_origin_ousideUS$discrim_lessrespect_bin = case_when(data_wce_national_origin_ousideUS$discrim_lessrespect == 1 ~ 1, 
                                                      data_wce_national_origin_ousideUS$discrim_lessrespect == 2 ~ 1, 
                                                      data_wce_national_origin_ousideUS$discrim_lessrespect == 3 ~ 1, 
                                                      data_wce_national_origin_ousideUS$discrim_lessrespect == 4 ~ 1, 
                                                      data_wce_national_origin_ousideUS$discrim_lessrespect == 5 ~ 0, 
                                                      data_wce_national_origin_ousideUS$discrim_lessrespect == 6 ~ 0,
                                                      data_wce_national_origin_ousideUS$discrim_lessrespect == 0 ~ 0) 

######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018
unique(data_wce_national_origin_ousideUS$discrim_lessrespect_bin)

national_origin_ousideUS_discrim_lessrespect_bin_age = summary_score_WCE_analysis(data_WCE = data_wce_national_origin_ousideUS,
                                                                  exposure = "discrim_lessrespect_bin", 
                                                                  outcome = "diabetes_new_bin", 
                                                                  #covariates_list = c("assessed_national_origin_ousideUS", "continious_age", "wealth_noIRA"))
                                                                  covariates_list = c("continious_age"))

national_origin_ousideUS_discrim_lessrespect_bin_age_HR = national_origin_ousideUS_discrim_lessrespect_bin_age[1]

national_origin_ousideUS_discrim_lessrespect_bin_stats_recoded = national_origin_ousideUS_discrim_lessrespect_bin_age[2]


######## bootstrapped CIs for the HRs from the above model 
national_origin_ousideUS_discrim_lessrespect_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_national_origin_ousideUS,
                                                                         exposure = "discrim_lessrespect_bin", 
                                                                         outcome = "diabetes_new_bin", 
                                                                         #covariates_list = c("assessed_national_origin_ousideUS", "continious_age", "wealth_noIRA"))
                                                                         covariates_list = c("continious_age"))

write.csv(national_origin_ousideUS_discrim_lessrespect_bin_stats_recoded, paste(OUTPUT_ROOT, "stats/national_origin_ousideUS_discrim_lessrespect_bin_age_stats_recoded.csv", sep=""))


national_origin_ousideUS_discrim_lessrespect_bin_age_HR = unlist(national_origin_ousideUS_discrim_lessrespect_bin_age_HR)
national_origin_ousideUS_discrim_lessrespect_bin_age_results = cbind(national_origin_ousideUS_discrim_lessrespect_bin_age_HR, national_origin_ousideUS_discrim_lessrespect_bin_age_CI)


colnames(national_origin_ousideUS_discrim_lessrespect_bin_age_results) = c("hazard ratio", "5% CI", "95% CI")
national_origin_ousideUS_all_results = rbind(national_origin_ousideUS_all_results, national_origin_ousideUS_discrim_lessrespect_bin_age_results)



########## discrim_medical_bin   #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...


data_wce_national_origin_ousideUS$discrim_medical_bin = case_when(data_wce_national_origin_ousideUS$discrim_medical == 1 ~ 1, 
                                                  data_wce_national_origin_ousideUS$discrim_medical == 2 ~ 1, 
                                                  data_wce_national_origin_ousideUS$discrim_medical == 3 ~ 1, 
                                                  data_wce_national_origin_ousideUS$discrim_medical == 4 ~ 1, 
                                                  data_wce_national_origin_ousideUS$discrim_medical == 5 ~ 0, 
                                                  data_wce_national_origin_ousideUS$discrim_medical == 6 ~ 0,
                                                  data_wce_national_origin_ousideUS$discrim_medical == 0 ~ 0) 

######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018


national_origin_ousideUS_discrim_medical_bin_age = summary_score_WCE_analysis(data_WCE = data_wce_national_origin_ousideUS,
                                                              exposure = "discrim_medical_bin", 
                                                              outcome = "diabetes_new_bin", 
                                                              #covariates_list = c("assessed_national_origin_ousideUS", "continious_age", "wealth_noIRA"))
                                                              covariates_list = c("continious_age"))

national_origin_ousideUS_discrim_medical_bin_age_HR = national_origin_ousideUS_discrim_medical_bin_age[1]

national_origin_ousideUS_discrim_medical_bin_stats_recoded = national_origin_ousideUS_discrim_medical_bin_age[2]



######## bootstrapped CIs for the HRs from the above model 
national_origin_ousideUS_discrim_medical_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_national_origin_ousideUS,
                                                                     exposure = "discrim_medical_bin", 
                                                                     outcome = "diabetes_new_bin", 
                                                                     #covariates_list = c("assessed_national_origin_ousideUS", "continious_age", "wealth_noIRA"))
                                                                     covariates_list = c("continious_age"))

write.csv(national_origin_ousideUS_discrim_medical_bin_stats_recoded, paste(OUTPUT_ROOT, "stats/national_origin_ousideUS_discrim_medical_bin_stats_recoded.csv", sep=""))



national_origin_ousideUS_discrim_medical_bin_age_HR = unlist(national_origin_ousideUS_discrim_medical_bin_age_HR)
national_origin_ousideUS_discrim_medical_bin_age_results = cbind(national_origin_ousideUS_discrim_medical_bin_age_HR, national_origin_ousideUS_discrim_medical_bin_age_CI)


colnames(national_origin_ousideUS_discrim_medical_bin_age_results) = c("hazard ratio", "5% CI", "95% CI")
national_origin_ousideUS_all_results = rbind(national_origin_ousideUS_all_results, national_origin_ousideUS_discrim_medical_bin_age_results)

########## discrim_notclever_bin #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...


data_wce_national_origin_ousideUS$discrim_notclever_bin = case_when(data_wce_national_origin_ousideUS$discrim_notclever == 1 ~ 1, 
                                                    data_wce_national_origin_ousideUS$discrim_notclever == 2 ~ 1, 
                                                    data_wce_national_origin_ousideUS$discrim_notclever == 3 ~ 1, 
                                                    data_wce_national_origin_ousideUS$discrim_notclever == 4 ~ 1, 
                                                    data_wce_national_origin_ousideUS$discrim_notclever == 5 ~ 0, 
                                                    data_wce_national_origin_ousideUS$discrim_notclever == 6 ~ 0,
                                                    data_wce_national_origin_ousideUS$discrim_notclever == 0 ~ 0) 
######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018


national_origin_ousideUS_discrim_notclever_bin_age = summary_score_WCE_analysis(data_WCE = data_wce_national_origin_ousideUS,
                                                                exposure = "discrim_notclever_bin", 
                                                                outcome = "diabetes_new_bin", 
                                                                #covariates_list = c("assessed_national_origin_ousideUS", "continious_age", "wealth_noIRA"))
                                                                covariates_list = c("continious_age"))

national_origin_ousideUS_discrim_notclever_bin_age_HR = national_origin_ousideUS_discrim_notclever_bin_age[1]

national_origin_ousideUS_discrim_notclever_bin_stats_recoded = national_origin_ousideUS_discrim_notclever_bin_age[2]



######## bootstrapped CIs for the HRs from the above model 
national_origin_ousideUS_discrim_notclever_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_national_origin_ousideUS,
                                                                       exposure = "discrim_notclever_bin", 
                                                                       outcome = "diabetes_new_bin", 
                                                                       #covariates_list = c("assessed_national_origin_ousideUS", "continious_age", "wealth_noIRA"))
                                                                       covariates_list = c("continious_age"))

write.csv(national_origin_ousideUS_discrim_notclever_bin_stats_recoded, paste(OUTPUT_ROOT, "stats/national_origin_ousideUS_discrim_notclever_bin_stats_recoded.csv", sep=""))




national_origin_ousideUS_discrim_notclever_bin_age_HR = unlist(national_origin_ousideUS_discrim_notclever_bin_age_HR)
national_origin_ousideUS_discrim_notclever_bin_age_results = cbind(national_origin_ousideUS_discrim_notclever_bin_age_HR, national_origin_ousideUS_discrim_notclever_bin_age_CI)



colnames(national_origin_ousideUS_discrim_notclever_bin_age_results) = c("hazard ratio", "5% CI", "95% CI")
national_origin_ousideUS_all_results = rbind(national_origin_ousideUS_all_results, national_origin_ousideUS_discrim_notclever_bin_age_results)
########## discrim_poorerservice_bin #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...

unique(data_wce_national_origin_ousideUS$discrim_poorerservice)
data_wce_national_origin_ousideUS$discrim_poorerservice = as.numeric(data_wce_national_origin_ousideUS$discrim_poorerservice) 

data_wce_national_origin_ousideUS$discrim_poorerservice_bin = case_when(data_wce_national_origin_ousideUS$discrim_poorerservice == 1 ~ 1, 
                                                        data_wce_national_origin_ousideUS$discrim_poorerservice == 2 ~ 1, 
                                                        data_wce_national_origin_ousideUS$discrim_poorerservice == 3 ~ 1, 
                                                        data_wce_national_origin_ousideUS$discrim_poorerservice == 4 ~ 1, 
                                                        data_wce_national_origin_ousideUS$discrim_poorerservice == 5 ~ 0, 
                                                        data_wce_national_origin_ousideUS$discrim_poorerservice == 6 ~ 0) 
######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018


national_origin_ousideUS_discrim_poorerservice_bin_age = summary_score_WCE_analysis(data_WCE = data_wce_national_origin_ousideUS,
                                                                    exposure = "discrim_poorerservice_bin", 
                                                                    outcome = "diabetes_new_bin", 
                                                                    #covariates_list = c("assessed_national_origin_ousideUS", "continious_age", "wealth_noIRA"))
                                                                    covariates_list = c("continious_age"))

national_origin_ousideUS_discrim_poorerservice_bin_age_HR = national_origin_ousideUS_discrim_poorerservice_bin_age[1]

national_origin_ousideUS_discrim_poorerservice_bin_stats_recoded = national_origin_ousideUS_discrim_poorerservice_bin_age[2]



######## bootstrapped CIs for the HRs from the above model 
national_origin_ousideUS_discrim_poorerservice_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_national_origin_ousideUS,
                                                                           exposure = "discrim_poorerservice_bin", 
                                                                           outcome = "diabetes_new_bin", 
                                                                           #covariates_list = c("assessed_national_origin_ousideUS", "continious_age", "wealth_noIRA"))
                                                                           covariates_list = c("continious_age"))

write.csv(national_origin_ousideUS_discrim_poorerservice_bin_stats_recoded, paste(OUTPUT_ROOT, "stats/national_origin_ousideUS_discrim_poorerservice_bin_stats_recoded.csv", sep=""))





national_origin_ousideUS_discrim_poorerservice_bin_age_HR = unlist(national_origin_ousideUS_discrim_poorerservice_bin_age_HR)
national_origin_ousideUS_discrim_poorerservice_bin_age_results = cbind(national_origin_ousideUS_discrim_poorerservice_bin_age_HR, national_origin_ousideUS_discrim_poorerservice_bin_age_CI)



colnames(national_origin_ousideUS_discrim_poorerservice_bin_age_results) = c("hazard ratio", "5% CI", "95% CI")
national_origin_ousideUS_all_results = rbind(national_origin_ousideUS_all_results, national_origin_ousideUS_discrim_poorerservice_bin_age_results)
########## AFRAID OTHERS #########

data_wce_national_origin_ousideUS$discrim_afraidothers_bin = case_when(data_wce_national_origin_ousideUS$discrim_afraidothers == 1 ~ 1, 
                                                       data_wce_national_origin_ousideUS$discrim_afraidothers == 2 ~ 1, 
                                                       data_wce_national_origin_ousideUS$discrim_afraidothers == 3 ~ 1, 
                                                       data_wce_national_origin_ousideUS$discrim_afraidothers == 4 ~ 1, 
                                                       data_wce_national_origin_ousideUS$discrim_afraidothers == 5 ~ 0, 
                                                       data_wce_national_origin_ousideUS$discrim_afraidothers == 6 ~ 0,
                                                       data_wce_national_origin_ousideUS$discrim_afraidothers == 0 ~ 0) 

######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018
national_origin_ousideUS_afraid_others_age = summary_score_WCE_analysis(data_WCE = data_wce_national_origin_ousideUS,
                                                        exposure = "discrim_afraidothers_bin", 
                                                        outcome = "diabetes_new_bin", 
                                                        #covariates_list = c("assessed_national_origin_ousideUS", "continious_age", "wealth_noIRA"))
                                                        covariates_list = c("continious_age"))

national_origin_ousideUS_afraid_others_age_HR = national_origin_ousideUS_afraid_others_age[1]

national_origin_ousideUS_afraid_others_age_stats_recoded = national_origin_ousideUS_afraid_others_age[2]



######## bootstrapped CIs for the HRs from the above model 
national_origin_ousideUS_afraid_others_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_national_origin_ousideUS,
                                                               exposure = "discrim_afraidothers_bin", 
                                                               outcome = "diabetes_new_bin", 
                                                               #covariates_list = c("assessed_national_origin_ousideUS", "continious_age", "wealth_noIRA"))
                                                               covariates_list = c("continious_age"))

write.csv(national_origin_ousideUS_afraid_others_age_stats_recoded, paste(OUTPUT_ROOT, "stats/national_origin_ousideUS_afraid_others_age_stats_recoded.csv", sep=""))



national_origin_ousideUS_afraid_others_age_HR = unlist(national_origin_ousideUS_afraid_others_age_HR)
national_origin_ousideUS_afraid_others_age_results = cbind(national_origin_ousideUS_afraid_others_age_HR, national_origin_ousideUS_afraid_others_age_CI)


colnames(national_origin_ousideUS_afraid_others_age_results) = c("hazard ratio", "5% CI", "95% CI")
national_origin_ousideUS_all_results = rbind(national_origin_ousideUS_all_results, national_origin_ousideUS_afraid_others_age_results)

variable = #c("summary mean score",
  c("harassed",  "less respect",   "medical", "not clever",  "poorer service", "afraid others")



national_origin_ousideUS_all_results  = cbind(variable, national_origin_ousideUS_all_results)

write.csv(national_origin_ousideUS_all_results, paste(OUTPUT_ROOT, "national_origin_ousideUS_all_results_recoded_1vs0.csv", sep=""))

########
########
########
