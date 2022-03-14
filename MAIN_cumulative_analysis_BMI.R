
library(WCE)
library(survival)
library(dplyr)
library(car)
library(tidyverse)
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
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/"

source((paste(SOURCE_ROOT, "sort_timepoints.R", sep="")))

source((paste(SOURCE_ROOT, "WCE_analysis.R", sep="")))
source((paste(SOURCE_ROOT, "summary_score_WCE_analysis.R", sep="")))

source((paste(SOURCE_ROOT, "Bootstrapped_CI.R", sep="")))
source((paste(SOURCE_ROOT, "summary_score_Bootstrapped_CI.R", sep="")))


WCE_dataset_BMI = read.csv(paste(SOURCE_data_ROOT, "WCE_dataset_BMI.csv", sep=""))

WCE_dataset_female = read.csv(paste(SOURCE_data_ROOT, "WCE_dataset_female.csv", sep=""))
WCE_dataset_lim_cond = read.csv(paste(SOURCE_data_ROOT, "WCE_dataset_lim_cond.csv", sep=""))
WCE_dataset_national_origin_ousideUS = read.csv(paste(SOURCE_data_ROOT, "WCE_dataset_national_origin_ousideUS.csv", sep=""))
WCE_dataset_race = read.csv(paste(SOURCE_data_ROOT, "WCE_dataset_race.csv", sep=""))
WCE_dataset_religion = read.csv(paste(SOURCE_data_ROOT, "WCE_dataset_religion.csv", sep=""))

#WCE_dataset_BMI.csv 
#WCE_dataset_lim_cond.csv
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
#################################### BMI ########################################################################

########## SUMMARY MEAN SCORE #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...
data_wce_BMI = sort_timepoints(data = WCE_dataset_BMI)

######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018


BMI_all_results = data.frame()
BMI_overal_discrim_age = summary_score_WCE_analysis(data_WCE = data_wce_BMI,
                                                   exposure = "summary_mean_score_discrim", 
                                                   outcome = "diabetes_new_bin", 
                                                   #covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
                                                   covariates_list = c("continious_age"))

BMI_overal_discrim_age_HR = BMI_overal_discrim_age[1]

BMI_overal_discrim_stats = BMI_overal_discrim_age[2]

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
BMI_overal_discrim_age_CI  = summary_score_Bootstrapped_CI(data_WCE = data_wce_BMI,
                                                            exposure = "summary_mean_score_discrim", 
                                                            outcome = "diabetes_new_bin", 
                                                            #covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
                                                            covariates_list = c("continious_age"))

#Steps to obtain the P value from the CI for an estimate of effect (Est)
#calculate the standard error: SE = (u − l)/(2×1.96)
#calculate the test statistic: z = Est/SE.
#calculate the P value2: P = exp(−0.717×z − 0.416×z2).


#1.9333720 1.3344550 1.921868 
#Est = 1.9333720
#SE = (1.921868 - 1.3344550 )/(2*1.96)
#z = 1.9333720/SE
#p = exp(−0.717×z − 0.416×z2)
#1.9333720 1.3344550 1.921868 


write.csv(BMI_overal_discrim_stats, paste(SOURCE_ROOT, "BMI_overal_discrim_stats.csv", sep=""))
write.csv(BMI_overal_discrim_age_HR, paste(SOURCE_ROOT, "BMI_overal_discrim_age_HR.csv", sep=""))
write.csv(BMI_overal_discrim_age_CI, paste(SOURCE_ROOT, "BMI_overal_discrim_age_CI.csv", sep=""))


BMI_overal_discrim_age_HR = unlist(BMI_overal_discrim_age_HR)
BMI_overal_discrim_age_results = cbind(BMI_overal_discrim_age_HR, BMI_overal_discrim_age_CI)

BMI_all_results = rbind(BMI_overal_discrim_age_results, BMI_all_results)
colnames(BMI_all_results) = c("hazard ratio", "5% CI", "95% CI")



########## discrim_harassed #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...
data_wce_BMI = sort_timepoints(data = WCE_dataset_BMI)

######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018

BMI_discrim_harassed_age = WCE_analysis(data_WCE = data_wce_BMI,
                                      exposure = "discrim_harassed", 
                                      outcome = "diabetes_new_bin", 
                                      #covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
                                      covariates_list = c("continious_age"))

BMI_discrim_harassed_age_HR = BMI_discrim_harassed_age[1]

BMI_discrim_harassed_stats = BMI_discrim_harassed_age[2]

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
BMI_discrim_harassed_age_CI  = Bootstrapped_CI(WCE_data_CI = data_wce_BMI,
                                             exposure = "discrim_harassed", 
                                             outcome = "diabetes_new_bin", 
                                             #covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
                                             covariates_list = c("continious_age"))

write.csv(BMI_discrim_harassed_stats, paste(SOURCE_ROOT, "BMI_discrim_harassed_age_stats.csv", sep=""))
write.csv(BMI_discrim_harassed_age_HR, paste(SOURCE_ROOT, "BMI_discrim_harassed_age_HR.csv", sep=""))
write.csv(BMI_discrim_harassed_age_CI, paste(SOURCE_ROOT, "BMI_discrim_harassed_age_CI.csv", sep=""))

BMI_discrim_harassed_age_HR = unlist(BMI_discrim_harassed_age_HR)
BMI_discrim_harassed_age_results = cbind(BMI_discrim_harassed_age_HR, BMI_discrim_harassed_age_CI)

colnames(BMI_discrim_harassed_age_results) = c("hazard ratio", "5% CI", "95% CI")
BMI_all_results = rbind(BMI_all_results, BMI_discrim_harassed_age_results)



##########

########## discrim_lessrespect #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...
data_wce_BMI = sort_timepoints(data = WCE_dataset_BMI)

######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018
unique(data_wce_BMI$discrim_lessrespect)

BMI_discrim_lessrespect_age = WCE_analysis(data_WCE = data_wce_BMI,
                                        exposure = "discrim_lessrespect", 
                                        outcome = "diabetes_new_bin", 
                                        #covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
                                        covariates_list = c("continious_age"))

BMI_discrim_lessrespect_age_HR = BMI_discrim_lessrespect_age[1]

BMI_discrim_lessrespect_stats = BMI_discrim_lessrespect_age[2]

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
BMI_discrim_lessrespect_age_CI  = Bootstrapped_CI(WCE_data_CI = data_wce_BMI,
                                               exposure = "discrim_lessrespect", 
                                               outcome = "diabetes_new_bin", 
                                               #covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
                                               covariates_list = c("continious_age"))

write.csv(BMI_discrim_lessrespect_stats, paste(SOURCE_ROOT, "BMI_discrim_lessrespect_age_stats.csv", sep=""))
write.csv(BMI_discrim_lessrespect_age_HR, paste(SOURCE_ROOT, "BMI_discrim_lessrespect_age_HR.csv", sep=""))
write.csv(BMI_discrim_lessrespect_age_CI, paste(SOURCE_ROOT, "BMI_discrim_lessrespect_age_CI.csv", sep=""))

BMI_discrim_lessrespect_age_HR = unlist(BMI_discrim_lessrespect_age_HR)
BMI_discrim_lessrespect_age_results = cbind(BMI_discrim_lessrespect_age_HR, BMI_discrim_lessrespect_age_CI)


colnames(BMI_discrim_lessrespect_age_results) = c("hazard ratio", "5% CI", "95% CI")
BMI_all_results = rbind(BMI_all_results, BMI_discrim_lessrespect_age_results)



########## discrim_medical   #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...
data_wce_BMI = sort_timepoints(data = WCE_dataset_BMI)

######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018


BMI_discrim_medical_age = WCE_analysis(data_WCE = data_wce_BMI,
                                      exposure = "discrim_medical", 
                                      outcome = "diabetes_new_bin", 
                                      #covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
                                      covariates_list = c("continious_age"))

BMI_discrim_medical_age_HR = BMI_discrim_medical_age[1]

BMI_discrim_medical_stats = BMI_discrim_medical_age[2]

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
BMI_discrim_medical_age_CI  = Bootstrapped_CI(WCE_data_CI = data_wce_BMI,
                                             exposure = "discrim_medical", 
                                             outcome = "diabetes_new_bin", 
                                             #covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
                                             covariates_list = c("continious_age"))

write.csv(BMI_discrim_medical_stats, paste(SOURCE_ROOT, "BMI_discrim_medical_stats.csv", sep=""))
write.csv(BMI_discrim_medical_age_HR, paste(SOURCE_ROOT, "BMI_discrim_medical_age_HR.csv", sep=""))
write.csv(BMI_discrim_medical_age_CI, paste(SOURCE_ROOT, "BMI_discrim_medical_age_CI.csv", sep=""))


BMI_discrim_medical_age_HR = unlist(BMI_discrim_medical_age_HR)
BMI_discrim_medical_age_results = cbind(BMI_discrim_medical_age_HR, BMI_discrim_medical_age_CI)


colnames(BMI_discrim_medical_age_results) = c("hazard ratio", "5% CI", "95% CI")
BMI_all_results = rbind(BMI_all_results, BMI_discrim_medical_age_results)

########## discrim_notclever #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...
data_wce_BMI = sort_timepoints(data = WCE_dataset_BMI)

######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018


BMI_discrim_notclever_age = WCE_analysis(data_WCE = data_wce_BMI,
                                      exposure = "discrim_notclever", 
                                      outcome = "diabetes_new_bin", 
                                      #covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
                                      covariates_list = c("continious_age"))

BMI_discrim_notclever_age_HR = BMI_discrim_notclever_age[1]

BMI_discrim_notclever_stats = BMI_discrim_notclever_age[2]

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
BMI_discrim_notclever_age_CI  = Bootstrapped_CI(WCE_data_CI = data_wce_BMI,
                                             exposure = "discrim_notclever", 
                                             outcome = "diabetes_new_bin", 
                                             #covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
                                             covariates_list = c("continious_age"))

write.csv(BMI_discrim_notclever_stats, paste(SOURCE_ROOT, "BMI_discrim_notclever_stats.csv", sep=""))
write.csv(BMI_discrim_notclever_age_HR, paste(SOURCE_ROOT, "BMI_discrim_notclever_age_HR.csv", sep=""))
write.csv(BMI_discrim_notclever_age_CI, paste(SOURCE_ROOT, "BMI_discrim_notclever_age_CI.csv", sep=""))



BMI_discrim_notclever_age_HR = unlist(BMI_discrim_notclever_age_HR)
BMI_discrim_notclever_age_results = cbind(BMI_discrim_notclever_age_HR, BMI_discrim_notclever_age_CI)



colnames(BMI_discrim_notclever_age_results) = c("hazard ratio", "5% CI", "95% CI")
BMI_all_results = rbind(BMI_all_results, BMI_discrim_notclever_age_results)
########## discrim_poorerservice #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...
data_wce_BMI = sort_timepoints(data = WCE_dataset_BMI)

######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018


BMI_discrim_poorerservice_age = WCE_analysis(data_WCE = data_wce_BMI,
                                      exposure = "discrim_poorerservice", 
                                      outcome = "diabetes_new_bin", 
                                      #covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
                                      covariates_list = c("continious_age"))

BMI_discrim_poorerservice_age_HR = BMI_discrim_poorerservice_age[1]

BMI_discrim_poorerservice_stats = BMI_discrim_poorerservice_age[2]

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
BMI_discrim_poorerservice_age_CI  = Bootstrapped_CI(WCE_data_CI = data_wce_BMI,
                                             exposure = "discrim_poorerservice", 
                                             outcome = "diabetes_new_bin", 
                                             #covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
                                             covariates_list = c("continious_age"))

write.csv(BMI_discrim_poorerservice_stats, paste(SOURCE_ROOT, "BMI_discrim_poorerservice_stats.csv", sep=""))
write.csv(BMI_discrim_poorerservice_age_HR, paste(SOURCE_ROOT, "BMI_discrim_poorerservice_age_HR.csv", sep=""))
write.csv(BMI_discrim_poorerservice_age_CI, paste(SOURCE_ROOT, "BMI_discrim_poorerservice_age_CI.csv", sep=""))




BMI_discrim_poorerservice_age_HR = unlist(BMI_discrim_poorerservice_age_HR)
BMI_discrim_poorerservice_age_results = cbind(BMI_discrim_poorerservice_age_HR, BMI_discrim_poorerservice_age_CI)



colnames(BMI_discrim_poorerservice_age_results) = c("hazard ratio", "5% CI", "95% CI")
BMI_all_results = rbind(BMI_all_results, BMI_discrim_poorerservice_age_results)
########## AFRAID OTHERS #########

######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018
BMI_afraid_others_age = WCE_analysis(data_WCE = data_wce_BMI,
                                        exposure = "discrim_afraidothers", 
                                        outcome = "diabetes_new_bin", 
                                        #covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
                                        covariates_list = c("continious_age"))

BMI_afraid_others_age_HR = BMI_afraid_others_age[1]

BMI_afraid_others_age_stats = BMI_afraid_others_age[2]

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
BMI_afraid_others_age_CI  = Bootstrapped_CI(WCE_data_CI = data_wce_BMI,
                                               exposure = "discrim_afraidothers", 
                                               outcome = "diabetes_new_bin", 
                                               #covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
                                               covariates_list = c("continious_age"))

write.csv(BMI_afraid_others_age_stats, paste(SOURCE_ROOT, "BMI_afraid_others_age_stats.csv", sep=""))
write.csv(BMI_afraid_others_age_HR, paste(SOURCE_ROOT, "BMI_afraid_others_age_HR.csv", sep=""))
write.csv(BMI_afraid_others_age_CI, paste(SOURCE_ROOT, "BMI_afraid_others_age_CI.csv", sep=""))


BMI_afraid_others_age_HR = unlist(BMI_afraid_others_age_HR)
BMI_afraid_others_age_results = cbind(BMI_afraid_others_age_HR, BMI_afraid_others_age_CI)


colnames(BMI_afraid_others_age_results) = c("hazard ratio", "5% CI", "95% CI")
BMI_all_results = rbind(BMI_all_results, BMI_afraid_others_age_results)

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



BMI_all_results  = cbind(variable, BMI_all_results)

########
########
########

write.csv(BMI_all_results, paste(SOURCE_ROOT, "BMI_all_results.csv", sep=""))

