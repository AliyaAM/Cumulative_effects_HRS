
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

WCE_dataset_BMI = read.csv(paste(SOURCE_data_ROOT, "WCE_dataset_BMI.csv", sep=""))


unique(WCE_dataset_BMI$diabetes_new_bin)
WCE_dataset_BMI$discrim_harassed_bin

#WCE_dataset_BMI.csv 
#WCE_dataset_BMI.csv
#WCE_dataset_BMI
#WCE_dataset_BMI.csv
#WCE_dataset_BMI.csv

unique(WCE_dataset_BMI$summary_mean_score_discrim_bin)
unique(WCE_dataset_BMI$discrim_harassed_bin)
unique(WCE_dataset_BMI$discrim_lessrespect_bin)
unique(WCE_dataset_BMI$discrim_medical_bin)
unique(WCE_dataset_BMI$discrim_notclever_bin)
unique(WCE_dataset_BMI$discrim_poorerservice_bin)
unique(WCE_dataset_BMI$discrim_afraidothers_bin)


## number_reasons_discrimination

#################################### ######## ########################################################################
#################################### BMI ########################################################################

########## SUMMARY MEAN SCORE #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...
data_wce_BMI = sort_timepoints(data = WCE_dataset_BMI)


######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018
#myvars <- c("HHIDPN", "timepoints_indiv", "start_new", "stop_new", "diabetes_new_bin", "continious_age",
#            "summary_mean_score_discrim_bin", "discrim_harassed_bin", "discrim_lessrespect_bin", "discrim_medical_bin", "discrim_notclever_bin", "discrim_poorerservice_bin", "discrim_afraidothers_bin")

#data_wce_BMI <- data_wce_BMI_before[myvars]


data_wce_BMI$discrim_harassed_bin = case_when(data_wce_BMI$discrim_harassed == 1 ~ 1, 
                                                 data_wce_BMI$discrim_harassed == 2 ~ 1, 
                                                 data_wce_BMI$discrim_harassed == 3 ~ 1, 
                                                 data_wce_BMI$discrim_harassed == 4 ~ 1, 
                                                 data_wce_BMI$discrim_harassed == 5 ~ 0, 
                                                 data_wce_BMI$discrim_harassed == 6 ~ 0,
                                                 data_wce_BMI$discrim_harassed == 0 ~ 0) 

unique(data_wce_BMI$discrim_harassed_bin)

unique(data_wce_BMI$timepoints_indiv)
unique(data_wce_BMI$start_new)
unique(data_wce_BMI$stop_new)
unique(data_wce_BMI$HHIDPN)
unique(data_wce_BMI$diabetes_new_bin)
unique(data_wce_BMI$discrim_harassed)
unique(data_wce_BMI$continious_age)


BMI_all_results = data.frame()
BMI_discrim_harassed_bin_age = summary_score_WCE_analysis(data_WCE = data_wce_BMI,
                                                             exposure = "discrim_harassed_bin", 
                                                             outcome = "diabetes_new_bin", 
                                                             covariates_list = c("assessed_BMI", "continious_age"))

#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

BMI_discrim_harassed_bin_age_HR = BMI_discrim_harassed_bin_age[1]

BMI_discrim_harassed_bin_model2_stats_recoded = BMI_discrim_harassed_bin_age[2]


unique(data_wce_BMI$continious_age)
unique(data_wce_BMI$diabetes_new_bin)
unique(data_wce_BMI$summary_mean_score_discrim_bin)

######## bootstrapped CIs for the HRs from the above model 
BMI_discrim_harassed_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_BMI,
                                                                    exposure = "discrim_harassed_bin", 
                                                                    outcome = "diabetes_new_bin", 
                                                                    covariates_list = c("assessed_BMI", "continious_age"))

#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

write.csv(BMI_discrim_harassed_bin_model2_stats_recoded, paste(OUTPUT_ROOT, "stats/BMI_overal_discrim_model2_stats_recoded.csv", sep=""))


BMI_discrim_harassed_bin_age_HR = unlist(BMI_discrim_harassed_bin_age_HR)
BMI_harassed_bin_age_results = cbind(BMI_discrim_harassed_bin_age_HR, BMI_discrim_harassed_bin_age_CI)

BMI_all_results = rbind(BMI_harassed_bin_age_results, BMI_all_results)
colnames(BMI_all_results) = c("hazard ratio", "5% CI", "95% CI")



##########

########## discrim_lessrespect_bin #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...



data_wce_BMI$discrim_lessrespect_bin = case_when(data_wce_BMI$discrim_lessrespect == 1 ~ 1, 
                                                    data_wce_BMI$discrim_lessrespect == 2 ~ 1, 
                                                    data_wce_BMI$discrim_lessrespect == 3 ~ 1, 
                                                    data_wce_BMI$discrim_lessrespect == 4 ~ 1, 
                                                    data_wce_BMI$discrim_lessrespect == 5 ~ 0, 
                                                    data_wce_BMI$discrim_lessrespect == 6 ~ 0,
                                                    data_wce_BMI$discrim_lessrespect == 0 ~ 0) 

######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018
unique(data_wce_BMI$discrim_lessrespect_bin)

BMI_discrim_lessrespect_bin_age = summary_score_WCE_analysis(data_WCE = data_wce_BMI,
                                                                exposure = "discrim_lessrespect_bin", 
                                                                outcome = "diabetes_new_bin", 
                                                                covariates_list = c("assessed_BMI", "continious_age"))
#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

BMI_discrim_lessrespect_bin_age_HR = BMI_discrim_lessrespect_bin_age[1]

BMI_discrim_lessrespect_bin_model2_stats_recoded = BMI_discrim_lessrespect_bin_age[2]


######## bootstrapped CIs for the HRs from the above model 
BMI_discrim_lessrespect_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_BMI,
                                                                       exposure = "discrim_lessrespect_bin", 
                                                                       outcome = "diabetes_new_bin", 
                                                                       covariates_list = c("assessed_BMI", "continious_age"))
#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

write.csv(BMI_discrim_lessrespect_bin_model2_stats_recoded, paste(OUTPUT_ROOT, "stats/BMI_discrim_lessrespect_bin_model2_stats_recoded.csv", sep=""))


BMI_discrim_lessrespect_bin_age_HR = unlist(BMI_discrim_lessrespect_bin_age_HR)
BMI_discrim_lessrespect_bin_age_results = cbind(BMI_discrim_lessrespect_bin_age_HR, BMI_discrim_lessrespect_bin_age_CI)


colnames(BMI_discrim_lessrespect_bin_age_results) = c("hazard ratio", "5% CI", "95% CI")
BMI_all_results = rbind(BMI_all_results, BMI_discrim_lessrespect_bin_age_results)



########## discrim_medical_bin   #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...


data_wce_BMI$discrim_medical_bin = case_when(data_wce_BMI$discrim_medical == 1 ~ 1, 
                                                data_wce_BMI$discrim_medical == 2 ~ 1, 
                                                data_wce_BMI$discrim_medical == 3 ~ 1, 
                                                data_wce_BMI$discrim_medical == 4 ~ 1, 
                                                data_wce_BMI$discrim_medical == 5 ~ 0, 
                                                data_wce_BMI$discrim_medical == 6 ~ 0,
                                                data_wce_BMI$discrim_medical == 0 ~ 0) 

######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018


BMI_discrim_medical_bin_age = summary_score_WCE_analysis(data_WCE = data_wce_BMI,
                                                            exposure = "discrim_medical_bin", 
                                                            outcome = "diabetes_new_bin", 
                                                            covariates_list = c("assessed_BMI", "continious_age"))
#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

BMI_discrim_medical_bin_age_HR = BMI_discrim_medical_bin_age[1]

BMI_discrim_medical_bin_model2_stats_recoded = BMI_discrim_medical_bin_age[2]



######## bootstrapped CIs for the HRs from the above model 
BMI_discrim_medical_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_BMI,
                                                                   exposure = "discrim_medical_bin", 
                                                                   outcome = "diabetes_new_bin", 
                                                                   #covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
                                                                   covariates_list = c("assessed_BMI", "continious_age"))
#covariates_list = c("continious_age"))

write.csv(BMI_discrim_medical_bin_model2_stats_recoded, paste(OUTPUT_ROOT, "stats/BMI_discrim_medical_bin_model2_stats_recoded.csv", sep=""))



BMI_discrim_medical_bin_age_HR = unlist(BMI_discrim_medical_bin_age_HR)
BMI_discrim_medical_bin_age_results = cbind(BMI_discrim_medical_bin_age_HR, BMI_discrim_medical_bin_age_CI)


colnames(BMI_discrim_medical_bin_age_results) = c("hazard ratio", "5% CI", "95% CI")
BMI_all_results = rbind(BMI_all_results, BMI_discrim_medical_bin_age_results)

########## discrim_notclever_bin #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...


data_wce_BMI$discrim_notclever_bin = case_when(data_wce_BMI$discrim_notclever == 1 ~ 1, 
                                                  data_wce_BMI$discrim_notclever == 2 ~ 1, 
                                                  data_wce_BMI$discrim_notclever == 3 ~ 1, 
                                                  data_wce_BMI$discrim_notclever == 4 ~ 1, 
                                                  data_wce_BMI$discrim_notclever == 5 ~ 0, 
                                                  data_wce_BMI$discrim_notclever == 6 ~ 0,
                                                  data_wce_BMI$discrim_notclever == 0 ~ 0) 
######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018


BMI_discrim_notclever_bin_age = summary_score_WCE_analysis(data_WCE = data_wce_BMI,
                                                              exposure = "discrim_notclever_bin", 
                                                              outcome = "diabetes_new_bin", 
                                                              covariates_list = c("assessed_BMI", "continious_age"))
#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

BMI_discrim_notclever_bin_age_HR = BMI_discrim_notclever_bin_age[1]

BMI_discrim_notclever_bin_model2_stats_recoded = BMI_discrim_notclever_bin_age[2]



######## bootstrapped CIs for the HRs from the above model 
BMI_discrim_notclever_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_BMI,
                                                                     exposure = "discrim_notclever_bin", 
                                                                     outcome = "diabetes_new_bin", 
                                                                     covariates_list = c("assessed_BMI", "continious_age"))
#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

write.csv(BMI_discrim_notclever_bin_model2_stats_recoded, paste(OUTPUT_ROOT, "stats/BMI_discrim_notclever_bin_model2_stats_recoded.csv", sep=""))




BMI_discrim_notclever_bin_age_HR = unlist(BMI_discrim_notclever_bin_age_HR)
BMI_discrim_notclever_bin_age_results = cbind(BMI_discrim_notclever_bin_age_HR, BMI_discrim_notclever_bin_age_CI)



colnames(BMI_discrim_notclever_bin_age_results) = c("hazard ratio", "5% CI", "95% CI")
BMI_all_results = rbind(BMI_all_results, BMI_discrim_notclever_bin_age_results)
########## discrim_poorerservice_bin #########

######## sort data in teh right way where starting point of  wave 1 is 0 and stopping point is 1, for wave 2: 1 and 2, for wave 3: 3 and 4...

unique(data_wce_BMI$discrim_poorerservice)
data_wce_BMI$discrim_poorerservice = as.numeric(data_wce_BMI$discrim_poorerservice) 

data_wce_BMI$discrim_poorerservice_bin = case_when(data_wce_BMI$discrim_poorerservice == 1 ~ 1, 
                                                      data_wce_BMI$discrim_poorerservice == 2 ~ 1, 
                                                      data_wce_BMI$discrim_poorerservice == 3 ~ 1, 
                                                      data_wce_BMI$discrim_poorerservice == 4 ~ 1, 
                                                      data_wce_BMI$discrim_poorerservice == 5 ~ 0, 
                                                      data_wce_BMI$discrim_poorerservice == 6 ~ 0) 
######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018


BMI_discrim_poorerservice_bin_age = summary_score_WCE_analysis(data_WCE = data_wce_BMI,
                                                                  exposure = "discrim_poorerservice_bin", 
                                                                  outcome = "diabetes_new_bin", 
                                                                  covariates_list = c("assessed_BMI", "continious_age"))
#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

BMI_discrim_poorerservice_bin_age_HR = BMI_discrim_poorerservice_bin_age[1]

BMI_discrim_poorerservice_bin_model2_stats_recoded = BMI_discrim_poorerservice_bin_age[2]



######## bootstrapped CIs for the HRs from the above model 
BMI_discrim_poorerservice_bin_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_BMI,
                                                                         exposure = "discrim_poorerservice_bin", 
                                                                         outcome = "diabetes_new_bin", 
                                                                         covariates_list = c("assessed_BMI", "continious_age"))
#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

write.csv(BMI_discrim_poorerservice_bin_model2_stats_recoded, paste(OUTPUT_ROOT, "stats/BMI_discrim_poorerservice_bin_model2_stats_recoded.csv", sep=""))





BMI_discrim_poorerservice_bin_age_HR = unlist(BMI_discrim_poorerservice_bin_age_HR)
BMI_discrim_poorerservice_bin_age_results = cbind(BMI_discrim_poorerservice_bin_age_HR, BMI_discrim_poorerservice_bin_age_CI)



colnames(BMI_discrim_poorerservice_bin_age_results) = c("hazard ratio", "5% CI", "95% CI")
BMI_all_results = rbind(BMI_all_results, BMI_discrim_poorerservice_bin_age_results)
########## AFRAID OTHERS #########

data_wce_BMI$discrim_afraidothers_bin = case_when(data_wce_BMI$discrim_afraidothers == 1 ~ 1, 
                                                     data_wce_BMI$discrim_afraidothers == 2 ~ 1, 
                                                     data_wce_BMI$discrim_afraidothers == 3 ~ 1, 
                                                     data_wce_BMI$discrim_afraidothers == 4 ~ 1, 
                                                     data_wce_BMI$discrim_afraidothers == 5 ~ 0, 
                                                     data_wce_BMI$discrim_afraidothers == 6 ~ 0,
                                                     data_wce_BMI$discrim_afraidothers == 0 ~ 0) 

######## main analysis producing HR for developing diabetes as aresult of cumulative effects of discriminaiton over years 2008 - 2018
BMI_afraid_others_age = summary_score_WCE_analysis(data_WCE = data_wce_BMI,
                                                      exposure = "discrim_afraidothers_bin", 
                                                      outcome = "diabetes_new_bin", 
                                                      covariates_list = c("assessed_BMI", "continious_age"))
#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

BMI_afraid_others_age_HR = BMI_afraid_others_age[1]

BMI_afraid_others_model2_stats_recoded = BMI_afraid_others_age[2]



######## bootstrapped CIs for the HRs from the above model 
BMI_afraid_others_age_CI  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_BMI,
                                                             exposure = "discrim_afraidothers_bin", 
                                                             outcome = "diabetes_new_bin", 
                                                             covariates_list = c("assessed_BMI", "continious_age"))
#covariates_list = c("assessed_BMI", "continious_age", "wealth_noIRA"))
#covariates_list = c("continious_age"))

write.csv(BMI_afraid_others_model2_stats_recoded, paste(OUTPUT_ROOT, "stats/BMI_afraid_others_model2_stats_recoded.csv", sep=""))



BMI_afraid_others_age_HR = unlist(BMI_afraid_others_age_HR)
BMI_afraid_others_age_results = cbind(BMI_afraid_others_age_HR, BMI_afraid_others_age_CI)


colnames(BMI_afraid_others_age_results) = c("hazard ratio", "5% CI", "95% CI")
BMI_all_results = rbind(BMI_all_results, BMI_afraid_others_age_results)

variable = #c("summary mean score",
  c("harassed",  "less respect",   "medical", "not clever",  "poorer service", "afraid others")



BMI_all_results  = cbind(variable, BMI_all_results)

write.csv(BMI_all_results, paste(OUTPUT_ROOT, "BMI_all_results_bin_model2.csv", sep=""))

########
########
########
