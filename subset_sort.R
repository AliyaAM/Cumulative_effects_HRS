

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


#WCE_dataset_subset = read.csv(paste(SOURCE_data_ROOT, "WCE_dataset_subset_diabetes.csv", sep=""))
#unique(WCE_dataset_subset$diabetes_new_bin)
#WCE_dataset_subset$discrim_bin

#WCE_dataset_subset.csv 
#WCE_dataset_subset.csv
#WCE_dataset_national_origin_ousideUS
#WCE_dataset_subset.csv
#WCE_dataset_subset.csv

#unique(WCE_dataset_subset$summary_mean_score_discrim_bin)
#unique(WCE_dataset_subset$discrim_bin)
#unique(WCE_dataset_subset$discrim_lessrespect_bin)
#unique(WCE_dataset_subset$discrim_medical_bin)
#unique(WCE_dataset_subset$discrim_notclever_bin)
#unique(WCE_dataset_subset$discrim_poorerservice_bin)
#unique(WCE_dataset_subset$discrim_afraidothers_bin)


## number_reasons_discrimination

#################################### ######## ########################################################################


subset_sort = function (subset_var, subset_value){
  
  
  
  # subset 1: age      
  # subset 2: age and BMI 
  # subset 3: age, BMI, wealth,  (1)
  # subset 4: age, BMI, hypertension  (2)
  # subset 5: age, BMI, hypertension, wealth NAs 
  # subset 6: smoking, physical activity, alcohol consumption
  # subset 7: age, BMI,  smoking, physical activity, alcohol consumption  (3)
  # subset 8:age, BMI, smoking, physical activity, alcohol consumption, hypertension
  # subset 9: age, BMI, CVD  (4)
  # subset 10: age, BMI,CVD, hypertension 
  # subset 11: age, BMI, smoking, physical activity, alcohol consumption, CVD  NAs 
  # subset 12: age, BMI, smoking, physical activity, alcohol consumption, CVD, hypertension  (5)  NAs 
  # subset 13: age, BMI, depression  (6)
  # subset 14: age, BMI, smoking, physical activity, alcohol consumption, depression (7)
  # subset 15: age, BMI, hypertension, depression 
  # subset 16: age, BMI,CVD, hypertension, depression 
  # subset 17: age, BMI, smoking, physical activity, alcohol consumption, CVD, hypertension, depression  (8) NAs 
  #######
  #age, sex, wealth, ethnicity, smoking, physical activity, alcohol consumption, subset, hypertension, CVD
  
  #### CVDS are:
  #"heartcondition_ever_bin", "heartcondition_new_bin", "angina_new_bin", "stroke_new_bin", "heartfailure2yrs_bin", "heartattack_ever_bin", "heartattack_new_bin"
  #### subsets with CVD are: 
  # subset 9: age, BMI, CVD  (4)
  # subset 10: age, BMI,CVD, hypertension 
  # subset 11: age, BMI, smoking, physical activity, alcohol consumption, CVD  
  # subset 12: age, BMI, smoking, physical activity, alcohol consumption, CVD, hypertension  (5)
  # subset 16: age, BMI,CVD, hypertension, depression 
  # subset 17: age, BMI, smoking, physical activity, alcohol consumption, CVD, hypertension, depression  (8)
  
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
  
  #################################### subset ########################################################################
  # # # # subset the data set to those with subset
  # # # # subset the data set to those with subset
  
  HRS2008_data_subset = subset(HRS2008_data, HRS2008_data[ , subset_var] == subset_value)
  
  HRS2010_data_subset  = subset(HRS2010_data, HRS2010_data[ , subset_var] == subset_value)
  
  HRS2012_data_subset  = subset(HRS2012_data, HRS2012_data[ , subset_var] == subset_value)
  
  HRS2014_data_subset  = subset(HRS2014_data, HRS2014_data[ , subset_var] == subset_value)
  
  HRS2016_data_subset  = subset(HRS2016_data, HRS2016_data[ , subset_var] == subset_value)
  
  HRS2018_data_subset  = subset(HRS2018_data, HRS2018_data[ , subset_var] == subset_value)
  
  
  WCE_dataset_subset = rbind(HRS2008_data_subset,
                             HRS2010_data_subset, 
                             HRS2012_data_subset,
                             HRS2014_data_subset, 
                             HRS2016_data_subset,
                             HRS2018_data_subset)
  
  
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
  
  #################################### subset ########################################################################
  
  ########## SUMMARY MEAN SCORE #########
  
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
  
  return(data_wce_subset)
}