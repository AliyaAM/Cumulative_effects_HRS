
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

#WCE_dataset_religion = read.csv(paste(SOURCE_data_ROOT, "WCE_dataset_religion_depression.csv", sep=""))
#WCE_dataset_female = read.csv(paste(SOURCE_data_ROOT, "WCE_dataset_female.csv", sep=""))
#WCE_dataset_lim_cond = read.csv(paste(SOURCE_data_ROOT, "WCE_dataset_lim_cond.csv", sep=""))
#WCE_dataset_national_origin_ousideUS = read.csv(paste(SOURCE_data_ROOT, "WCE_dataset_national_origin_ousideUS.csv", sep=""))
WCE_dataset_religion = read.csv(paste(SOURCE_data_ROOT, "WCE_dataset_religion_depression_bin.csv", sep=""))
#WCE_dataset_religion = read.csv(paste(SOURCE_data_ROOT, "WCE_dataset_religion.csv", sep=""))


#WCE_dataset_religion.csv 
#WCE_dataset_lim_cond.csv
#WCE_dataset_national_origin_ousideUS
#WCE_dataset_religion.csv
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
#################################### religion ########################################################################

########## SUMMARY MEAN SCORE #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...
data_wce_religion = sort_timepoints(data = WCE_dataset_religion)

######## main analysis producing HR for developing depression_bin as aresult of cumulative effects of discriminaiton over years 2008 - 2018

unique(data_wce_religion$checklist_depression_bin)

religion_all_results = data.frame()
religion_overal_discrim_age = summary_score_WCE_analysis(data_WCE = data_wce_religion,
                                                     exposure = "summary_mean_score_discrim", 
                                                     outcome = "checklist_depression_bin", 
                                                     #covariates_list = c("assessed_religion", "continious_age", "wealth_noIRA"))
                                                     covariates_list = c("continious_age"))

religion_overal_discrim_age_HR = religion_overal_discrim_age[1]

religion_overal_discrim_stats = religion_overal_discrim_age[2]

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


######## bootstrapped CIs for the HRs from the above model 
religion_overal_discrim_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_religion,
                                                            exposure = "summary_mean_score_discrim", 
                                                            outcome = "checklist_depression_bin", 
                                                            #covariates_list = c("assessed_religion", "continious_age", "wealth_noIRA"))
                                                            covariates_list = c("continious_age"))

write.csv(religion_overal_discrim_stats, paste(OUTPUT_ROOT, "stats/religion_overal_discrim_stats_depression.csv", sep=""))


religion_overal_discrim_age_HR = unlist(religion_overal_discrim_age_HR)
religion_overal_discrim_age_results = cbind(religion_overal_discrim_age_HR, religion_overal_discrim_age_CI)

religion_all_results = rbind(religion_overal_discrim_age_results, religion_all_results)
colnames(religion_all_results) = c("hazard ratio", "5% CI", "95% CI")


########## discrim_harassed #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...
data_wce_religion = sort_timepoints(data = WCE_dataset_religion)

######## main analysis producing HR for developing depression_bin as aresult of cumulative effects of discriminaiton over years 2008 - 2018

religion_discrim_harassed_age = WCE_analysis(data_WCE = data_wce_religion,
                                         exposure = "discrim_harassed", 
                                         outcome = "checklist_depression_bin", 
                                         #covariates_list = c("assessed_religion", "continious_age", "wealth_noIRA"))
                                         covariates_list = c("continious_age"))

religion_discrim_harassed_age_HR = religion_discrim_harassed_age[1]

religion_discrim_harassed_stats = religion_discrim_harassed_age[2]

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


######## bootstrapped CIs for the HRs from the above model 
religion_discrim_harassed_age_CI  = Bootstrapped_CI(WCE_data_CI = data_wce_religion,
                                                exposure = "discrim_harassed", 
                                                outcome = "checklist_depression_bin", 
                                                #covariates_list = c("assessed_religion", "continious_age", "wealth_noIRA"))
                                                covariates_list = c("continious_age"))

write.csv(religion_discrim_harassed_stats, paste(OUTPUT_ROOT, "stats/religion_discrim_harassed_age_stats_depression.csv", sep=""))


religion_discrim_harassed_age_HR = unlist(religion_discrim_harassed_age_HR)
religion_discrim_harassed_age_results = cbind(religion_discrim_harassed_age_HR, religion_discrim_harassed_age_CI)

colnames(religion_discrim_harassed_age_results) = c("hazard ratio", "5% CI", "95% CI")
religion_all_results = rbind(religion_all_results, religion_discrim_harassed_age_results)



##########

########## discrim_lessrespect #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...
data_wce_religion = sort_timepoints(data = WCE_dataset_religion)

######## main analysis producing HR for developing depression_bin as aresult of cumulative effects of discriminaiton over years 2008 - 2018
unique(data_wce_religion$discrim_lessrespect)

religion_discrim_lessrespect_age = WCE_analysis(data_WCE = data_wce_religion,
                                            exposure = "discrim_lessrespect", 
                                            outcome = "checklist_depression_bin", 
                                            #covariates_list = c("assessed_religion", "continious_age", "wealth_noIRA"))
                                            covariates_list = c("continious_age"))

religion_discrim_lessrespect_age_HR = religion_discrim_lessrespect_age[1]

religion_discrim_lessrespect_stats = religion_discrim_lessrespect_age[2]

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


######## bootstrapped CIs for the HRs from the above model 
religion_discrim_lessrespect_age_CI  = Bootstrapped_CI(WCE_data_CI = data_wce_religion,
                                                   exposure = "discrim_lessrespect", 
                                                   outcome = "checklist_depression_bin", 
                                                   #covariates_list = c("assessed_religion", "continious_age", "wealth_noIRA"))
                                                   covariates_list = c("continious_age"))

write.csv(religion_discrim_lessrespect_stats, paste(OUTPUT_ROOT, "stats/religion_discrim_lessrespect_age_stats_depression.csv", sep=""))


religion_discrim_lessrespect_age_HR = unlist(religion_discrim_lessrespect_age_HR)
religion_discrim_lessrespect_age_results = cbind(religion_discrim_lessrespect_age_HR, religion_discrim_lessrespect_age_CI)


colnames(religion_discrim_lessrespect_age_results) = c("hazard ratio", "5% CI", "95% CI")
religion_all_results = rbind(religion_all_results, religion_discrim_lessrespect_age_results)



########## discrim_medical   #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...
data_wce_religion = sort_timepoints(data = WCE_dataset_religion)

######## main analysis producing HR for developing depression_bin as aresult of cumulative effects of discriminaiton over years 2008 - 2018


religion_discrim_medical_age = WCE_analysis(data_WCE = data_wce_religion,
                                        exposure = "discrim_medical", 
                                        outcome = "checklist_depression_bin", 
                                        #covariates_list = c("assessed_religion", "continious_age", "wealth_noIRA"))
                                        covariates_list = c("continious_age"))

religion_discrim_medical_age_HR = religion_discrim_medical_age[1]

religion_discrim_medical_stats = religion_discrim_medical_age[2]

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


######## bootstrapped CIs for the HRs from the above model 
religion_discrim_medical_age_CI  = Bootstrapped_CI(WCE_data_CI = data_wce_religion,
                                               exposure = "discrim_medical", 
                                               outcome = "checklist_depression_bin", 
                                               #covariates_list = c("assessed_religion", "continious_age", "wealth_noIRA"))
                                               covariates_list = c("continious_age"))

write.csv(religion_discrim_medical_stats, paste(OUTPUT_ROOT, "stats/religion_discrim_medical_stats_depression.csv", sep=""))



religion_discrim_medical_age_HR = unlist(religion_discrim_medical_age_HR)
religion_discrim_medical_age_results = cbind(religion_discrim_medical_age_HR, religion_discrim_medical_age_CI)


colnames(religion_discrim_medical_age_results) = c("hazard ratio", "5% CI", "95% CI")
religion_all_results = rbind(religion_all_results, religion_discrim_medical_age_results)

########## discrim_notclever #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...
data_wce_religion = sort_timepoints(data = WCE_dataset_religion)

######## main analysis producing HR for developing depression_bin as aresult of cumulative effects of discriminaiton over years 2008 - 2018


religion_discrim_notclever_age = WCE_analysis(data_WCE = data_wce_religion,
                                          exposure = "discrim_notclever", 
                                          outcome = "checklist_depression_bin", 
                                          #covariates_list = c("assessed_religion", "continious_age", "wealth_noIRA"))
                                          covariates_list = c("continious_age"))

religion_discrim_notclever_age_HR = religion_discrim_notclever_age[1]

religion_discrim_notclever_stats = religion_discrim_notclever_age[2]

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


######## bootstrapped CIs for the HRs from the above model 
religion_discrim_notclever_age_CI  = Bootstrapped_CI(WCE_data_CI = data_wce_religion,
                                                 exposure = "discrim_notclever", 
                                                 outcome = "checklist_depression_bin", 
                                                 #covariates_list = c("assessed_religion", "continious_age", "wealth_noIRA"))
                                                 covariates_list = c("continious_age"))

write.csv(religion_discrim_notclever_stats, paste(OUTPUT_ROOT, "stats/religion_discrim_notclever_stats_depression.csv", sep=""))




religion_discrim_notclever_age_HR = unlist(religion_discrim_notclever_age_HR)
religion_discrim_notclever_age_results = cbind(religion_discrim_notclever_age_HR, religion_discrim_notclever_age_CI)



colnames(religion_discrim_notclever_age_results) = c("hazard ratio", "5% CI", "95% CI")
religion_all_results = rbind(religion_all_results, religion_discrim_notclever_age_results)
########## discrim_poorerservice #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...
data_wce_religion = sort_timepoints(data = WCE_dataset_religion)

######## main analysis producing HR for developing depression_bin as aresult of cumulative effects of discriminaiton over years 2008 - 2018


religion_discrim_poorerservice_age = WCE_analysis(data_WCE = data_wce_religion,
                                              exposure = "discrim_poorerservice", 
                                              outcome = "checklist_depression_bin", 
                                              #covariates_list = c("assessed_religion", "continious_age", "wealth_noIRA"))
                                              covariates_list = c("continious_age"))

religion_discrim_poorerservice_age_HR = religion_discrim_poorerservice_age[1]

religion_discrim_poorerservice_stats = religion_discrim_poorerservice_age[2]

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


######## bootstrapped CIs for the HRs from the above model 
religion_discrim_poorerservice_age_CI  = Bootstrapped_CI(WCE_data_CI = data_wce_religion,
                                                     exposure = "discrim_poorerservice", 
                                                     outcome = "checklist_depression_bin", 
                                                     #covariates_list = c("assessed_religion", "continious_age", "wealth_noIRA"))
                                                     covariates_list = c("continious_age"))

write.csv(religion_discrim_poorerservice_stats, paste(OUTPUT_ROOT, "stats/MI_discrim_poorerservice_stats_depression.csv", sep=""))





religion_discrim_poorerservice_age_HR = unlist(religion_discrim_poorerservice_age_HR)
religion_discrim_poorerservice_age_results = cbind(religion_discrim_poorerservice_age_HR, religion_discrim_poorerservice_age_CI)



colnames(religion_discrim_poorerservice_age_results) = c("hazard ratio", "5% CI", "95% CI")
religion_all_results = rbind(religion_all_results, religion_discrim_poorerservice_age_results)
########## AFRAID OTHERS #########

######## main analysis producing HR for developing depression_bin as aresult of cumulative effects of discriminaiton over years 2008 - 2018
religion_afraid_others_age = WCE_analysis(data_WCE = data_wce_religion,
                                      exposure = "discrim_afraidothers", 
                                      outcome = "checklist_depression_bin", 
                                      #covariates_list = c("assessed_religion", "continious_age", "wealth_noIRA"))
                                      covariates_list = c("continious_age"))

religion_afraid_others_age_HR = religion_afraid_others_age[1]

religion_afraid_others_age_stats = religion_afraid_others_age[2]

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


######## bootstrapped CIs for the HRs from the above model 
religion_afraid_others_age_CI  = Bootstrapped_CI(WCE_data_CI = data_wce_religion,
                                             exposure = "discrim_afraidothers", 
                                             outcome = "checklist_depression_bin", 
                                             #covariates_list = c("assessed_religion", "continious_age", "wealth_noIRA"))
                                             covariates_list = c("continious_age"))

write.csv(religion_afraid_others_age_stats, paste(OUTPUT_ROOT, "stats/religion_afraid_others_age_stats_depression.csv", sep=""))



religion_afraid_others_age_HR = unlist(religion_afraid_others_age_HR)
religion_afraid_others_age_results = cbind(religion_afraid_others_age_HR, religion_afraid_others_age_CI)


colnames(religion_afraid_others_age_results) = c("hazard ratio", "5% CI", "95% CI")
religion_all_results = rbind(religion_all_results, religion_afraid_others_age_results)

variable = c("summary mean score, 2",
             "summary mean score, 3",
             "summary mean score, 4",
             "summary mean score, 5",
             "summary mean score, 6",
             
             "harassed, almost everyday", 
             "harassed, at least once a week", 
             "harassed, a few times a month", 
             "harassed, a few times a year", 
             "harassed, less than once a year", 
             
             "less respect, almost everyday", 
             "less respect, at least once a week", 
             "less respect, a few times a month", 
             "less respect, a few times a year", 
             "less respect, less than once a year", 
             
             "medical, almost everyday", 
             "medical, at least once a week",  
             "medical, a few times a month", 
             "medical, a few times a year", 
             "medical, less than once a year", 
             
             "not clever, almost everyday", 
             "not clever, at least once a week", 
             "not clever, a few times a month", 
             "not clever, a few times a year", 
             "not clever, less than once a year", 
             
             "poorer service, almost everyday",
             "poorer service, at least once a week", 
             "poorer service, a few times a month",
             "poorer service, a few times a year",
             "poorer service, less than once a year", 
             
             "afraid others, almost everyday", 
             "afraid others, at least once a week",
             "afraid others, a few times a month",
             "afraid others, a few times a year",
             "afraid others, less than once a year")



religion_all_results  = cbind(variable, religion_all_results)

write.csv(religion_all_results, paste(OUTPUT_ROOT, "religion_all_results_depression.csv", sep=""))

########
########
########
