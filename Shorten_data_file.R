
SOURCE_ROOT = "/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"
OUTPUT_ROOT = "/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"


#other sources of stress, shall we include them as covariates: job strain etc, a lit of HRS stress items are here: https://g2aging.org/?section=concordance-search&sWords=stress&interval=1992%2C2016&page=1&per_page=50&af_src=1

# other literature considers cumulatie effects as: cumulative stressors (global, weekly and major life events)
#https://bmcpublichealth.biomedcentral.com/articles/10.1186/s12889-020-08573-0 

#think how we are going to restrict to a particular type of discrimination, 
#which wave shall we use? 
#shall we state that every wave they listed disability (ie subset HRS2004_discrim_disability == 1 & HRS2008_discrim_disability ==1 etc every wave up to 2018) 

#add 2004, 2006, 2008 

#from RAND below we beed to find: #wealth # in harmonised rnad file 2008 year wealth var is: #H9ATOTW 

#RAND_data = read.csv("/Users/aliya/my_docs/KCL_postDoc/Data_analysis/randhrs1992_2018v1.csv")


#coxph

#covariates: Fixed confounding factors at baseline will include SES, sex, 
########################################################################### and for the onset of a particular  disease a history of that disease or diseases known to be a precuisite prior study recruitment (i.e. baseline)
#HRS2018_race_hispanic_latino

#HRS2018_race_white
#HRS2018_race_black
#LGB_2016
#Straight_2016
#sex_1_0_2018
#yearsof_education2018

#Time-dependent covariates will include current age 
#age_groups2018
#continious_age2018

#bind rows with bind_rows in dplyr 


# add continious age variable to 2008 dataset 
#HRS2008_data$HRS2008


HRS2008_data = read.csv(paste(SOURCE_ROOT, "HRS_2008_data/HRS2008_data_discrimination_dataset_april_2022.csv", sep=""))
HRS2010_data = read.csv(paste(SOURCE_ROOT, "HRS_2010_data/HRS2010_data_discrimination_dataset_april_2022.csv", sep=""))
HRS2012_data = read.csv(paste(SOURCE_ROOT, "HRS_2012_data/HRS2012_data_discrimination_dataset_april_2022.csv", sep=""))
HRS2014_data = read.csv(paste(SOURCE_ROOT, "HRS_2014_data/HRS2014_data_discrimination_dataset_april_2022.csv", sep=""))
HRS2016_data = read.csv(paste(SOURCE_ROOT, "HRS_2016_data/HRS2016_data_discrimination_dataset_april_2022.csv", sep=""))
HRS2018_data = read.csv(paste(SOURCE_ROOT, "HRS_2018_data/HRS2018_data_discrimination_dataset_april_2022.csv", sep=""))



colnames_all = c('HHIDPN',
                 "diabetes_new",
                 "diabetes_ever",
                 "sex_1_2",
                 "wealth_noIRA",
                 "start",
                 "stop",
                 "reason_discrim1_reason_age",
                 "reason_discrim1_reason_disability",
                 
                 "summary_mean_score_discrim", 
                 "checklist_depression_bin", 
                 "reason_discirim_financial", 
                 "reason_discirim_sexuality", 
                 "number_reasons_discrimination", 
                 
                 "reason_discrim1_reason_gender", 
                "reason_discrim1_reason_national", 
                 "reason_discrim1_reason_race", 
                 "reason_discrim1_reason_religion", 
                 "reason_discrim1_reason_weight", 
                 
                 "discrim_harassed",
                 "discrim_lessrespect",
                 "discrim_medical",
                 "discrim_notclever",
                 "discrim_poorerservice",
                 "discrim_afraidothers",
                
                "continious_age",
                "sex_1_2", 
                "assessed_BMI", 
                
                "limiting_condition_bin", 
                "national_origin_ousideUS_bin", 
                "religion_bin", 
                "race_white", 
                
                "year")


nrow_2008 = nrow(HRS2008_data) 
HRS2008_data$year = rep(2008, times = nrow_2008)


nrow_2010 = nrow(HRS2010_data) 
HRS2010_data$year = rep(2010, times = nrow_2010)

nrow_2012 = nrow(HRS2012_data) 
HRS2012_data$year = rep(2012, times = nrow_2012)

nrow_2014 = nrow(HRS2014_data) 
HRS2014_data$year= rep(2014, times = nrow_2014)

nrow_2016 = nrow(HRS2016_data) 
HRS2016_data$year = rep(2016, times = nrow_2016)

nrow_2018 = nrow(HRS2018_data) 
HRS2018_data$year = rep(2018, times = nrow_2018)



HRS2008_data_short = cbind(HRS2008_data$HHIDPN,
                              HRS2008_data$diabetes_new,
                              HRS2008_data$diabetes_ever,
                              
                              HRS2008_data$sex_1_2,
                              HRS2008_data$wealth_noIRA,
                              HRS2008_data$start,
                              HRS2008_data$stop,
                              
                              HRS2008_data$reason_discrim1_reason_age, 
                              HRS2008_data$reason_discrim1_reason_disability,
                              
                              HRS2008_data$summary_mean_score_discrim, 
                              HRS2008_data$checklist_depression_bin, 
                              HRS2008_data$reason_discirim_financial, 
                              HRS2008_data$reason_discirim_sexuality, 
                              HRS2008_data$number_reasons_discrimination, 
                           
                           HRS2008_data$reason_discrim1_reason_gender, 
                           HRS2008_data$reason_discrim1_reason_national, 
                           HRS2008_data$reason_discrim1_reason_race, 
                           HRS2008_data$reason_discrim1_reason_religion, 
                           HRS2008_data$reason_discrim1_reason_weight, 
                              
                              HRS2008_data$discrim_harassed,
                              HRS2008_data$discrim_lessrespect,
                              HRS2008_data$discrim_medical,
                              HRS2008_data$discrim_notclever,
                              HRS2008_data$discrim_poorerservice,
                              HRS2008_data$discrim_afraidothers,
                              
                           HRS2008_data$continious_age,
                           HRS2008_data$sex_1_2,
                           HRS2008_data$assessed_BMI, 
                           HRS2008_data$limiting_condition_bin, 
                           HRS2008_data$national_origin_ousideUS, 
                           HRS2008_data$religion_bin, 
                           HRS2008_data$race_white, 
                           
                           
                           
                           HRS2008_data$year)


HRS2008_data_short = as.data.frame(HRS2008_data_short)

colnames(HRS2008_data_short) = colnames_all


HRS2010_data_short = cbind(HRS2010_data$HHIDPN,
                              HRS2010_data$diabetes_new,
                              HRS2010_data$diabetes_ever,
                              HRS2010_data$sex_1_2,
                              HRS2010_data$wealth_noIRA,
                              HRS2010_data$start,
                              HRS2010_data$stop,
                           
                           HRS2010_data$reason_discrim1_reason_age, 
                           HRS2010_data$reason_discrim1_reason_disability,
                              
                              HRS2010_data$summary_mean_score_discrim, 
                              HRS2010_data$checklist_depression_bin, 
                              HRS2010_data$reason_discirim_financial, 
                              HRS2010_data$reason_discirim_sexuality, 
                              HRS2010_data$number_reasons_discrimination, 
                              
                           HRS2010_data$reason_discrim1_reason_gender, 
                           HRS2010_data$reason_discrim1_reason_national, 
                           HRS2010_data$reason_discrim1_reason_race, 
                           HRS2010_data$reason_discrim1_reason_religion, 
                           HRS2010_data$reason_discrim1_reason_weight, 
                              
                              HRS2010_data$discrim_harassed,
                              HRS2010_data$discrim_lessrespect,
                              HRS2010_data$discrim_medical,
                              HRS2010_data$discrim_notclever,
                              HRS2010_data$discrim_poorerservice,
                              
                              HRS2010_data$discrim_afraidothers,
                           
                           HRS2010_data$continious_age,
                           HRS2010_data$sex_1_2,
                           HRS2010_data$assessed_BMI, 
                           HRS2010_data$limiting_condition_bin, 
                           HRS2010_data$national_origin_ousideUS, 
                           HRS2010_data$religion_bin, 
                           HRS2010_data$race_white, 
                           
                           
                           
                           HRS2010_data$year)


HRS2010_data_short = as.data.frame(HRS2010_data_short)

colnames(HRS2010_data_short) = colnames_all



HRS2012_data_short = cbind(HRS2012_data$HHIDPN,
                              HRS2012_data$diabetes_new,
                              HRS2012_data$diabetes_ever,
                              HRS2012_data$sex_1_2,
                              HRS2012_data$wealth_noIRA,
                              HRS2012_data$start,
                              HRS2012_data$stop,
                           
                           HRS2012_data$reason_discrim1_reason_age, 
                           
                              HRS2012_data$reason_discrim1_reason_disability,
                              
                              
                              HRS2012_data$summary_mean_score_discrim, 
                              HRS2012_data$checklist_depression_bin, 
                              HRS2012_data$reason_discirim_financial, 
                              HRS2012_data$reason_discirim_sexuality, 
                              HRS2012_data$number_reasons_discrimination, 
                           
                          
                           HRS2012_data$reason_discrim1_reason_gender, 
                           HRS2012_data$reason_discrim1_reason_national, 
                           HRS2012_data$reason_discrim1_reason_race, 
                           HRS2012_data$reason_discrim1_reason_religion, 
                           HRS2012_data$reason_discrim1_reason_weight, 
                              
                              HRS2012_data$discrim_harassed,
                              HRS2012_data$discrim_lessrespect,
                              HRS2012_data$discrim_medical,
                              HRS2012_data$discrim_notclever,
                              HRS2012_data$discrim_poorerservice,
                              HRS2012_data$discrim_afraidothers,
                              
                           HRS2012_data$continious_age,
                           HRS2012_data$sex_1_2,
                           HRS2012_data$assessed_BMI, 
                           HRS2012_data$limiting_condition_bin, 
                           HRS2012_data$national_origin_ousideUS, 
                           HRS2012_data$religion_bin, 
                           HRS2012_data$race_white, 
                           
                           
                          
                           HRS2012_data$year)



HRS2012_data_short = as.data.frame(HRS2012_data_short)

colnames(HRS2012_data_short) = colnames_all



HRS2014_data_short = cbind(HRS2014_data$HHIDPN,
                              HRS2014_data$diabetes_new,
                              HRS2014_data$diabetes_ever,
                              HRS2014_data$sex_1_2,
                              HRS2014_data$wealth_noIRA,
                              HRS2014_data$start,
                              HRS2014_data$stop,
                           
                           HRS2014_data$reason_discrim1_reason_age, 
                           HRS2014_data$reason_discrim1_reason_disability,
                              
                              HRS2014_data$summary_mean_score_discrim, 
                              HRS2014_data$checklist_depression_bin, 
                              HRS2014_data$reason_discirim_financial, 
                              HRS2014_data$reason_discirim_sexuality, 
                              HRS2014_data$number_reasons_discrimination, 
                              
                           HRS2014_data$reason_discrim1_reason_gender, 
                           HRS2014_data$reason_discrim1_reason_national, 
                           HRS2014_data$reason_discrim1_reason_race, 
                           HRS2014_data$reason_discrim1_reason_religion, 
                           HRS2014_data$reason_discrim1_reason_weight, 
                              
                              
                              HRS2014_data$discrim_harassed,
                              HRS2014_data$discrim_lessrespect,
                              HRS2014_data$discrim_medical,
                              HRS2014_data$discrim_notclever,
                              HRS2014_data$discrim_poorerservice,
                              HRS2014_data$discrim_afraidothers,
                              
                           HRS2014_data$continious_age,
                           HRS2014_data$sex_1_2,
                           HRS2014_data$assessed_BMI, 
                           HRS2014_data$limiting_condition_bin, 
                           HRS2014_data$national_origin_ousideUS, 
                           HRS2014_data$religion_bin, 
                           HRS2014_data$race_white, 
                           
                           
                           
                           HRS2014_data$year)


HRS2014_data_short = as.data.frame(HRS2014_data_short)

colnames(HRS2014_data_short) = colnames_all


#HRS2016_data$HHIDPN = HRS2016_data$HHIDPN_HRS2016

HRS2016_data_short = cbind(HRS2016_data$HHIDPN,
                              HRS2016_data$diabetes_new,
                              HRS2016_data$diabetes_ever,
                              
                              HRS2016_data$sex_1_2,
                              HRS2016_data$wealth_noIRA,
                              HRS2016_data$start,
                              HRS2016_data$stop,
                              
                           HRS2016_data$reason_discrim1_reason_age, 
                           
                              HRS2016_data$reason_discrim1_reason_disability,
                              
                              HRS2016_data$summary_mean_score_discrim, 
                              HRS2016_data$checklist_depression_bin, 
                              HRS2016_data$reason_discirim_financial, 
                              HRS2016_data$reason_discirim_sexuality, 
                              HRS2016_data$number_reasons_discrimination, 
                              
                           HRS2016_data$reason_discrim1_reason_gender, 
                           HRS2016_data$reason_discrim1_reason_national, 
                           HRS2016_data$reason_discrim1_reason_race, 
                           HRS2016_data$reason_discrim1_reason_religion, 
                           HRS2016_data$reason_discrim1_reason_weight, 
                              
                              HRS2016_data$discrim_harassed,
                              HRS2016_data$discrim_lessrespect,
                              HRS2016_data$discrim_medical,
                              HRS2016_data$discrim_notclever,
                              HRS2016_data$discrim_poorerservice,
                              HRS2016_data$discrim_afraidothers,
                              
                           HRS2016_data$continious_age,
                           HRS2016_data$sex_1_2,
                           HRS2016_data$assessed_BMI, 
                           HRS2016_data$limiting_condition_bin, 
                           HRS2016_data$national_origin_ousideUS, 
                           HRS2016_data$religion_bin, 
                           HRS2016_data$race_white, 
                           
                           
                           
                           HRS2016_data$year)




HRS2016_data_short = as.data.frame(HRS2016_data_short)

colnames(HRS2016_data_short) = colnames_all


HRS2018_data_short = cbind(HRS2018_data$HHIDPN,
                              HRS2018_data$diabetes_new,
                              HRS2018_data$diabetes_ever,
                              HRS2018_data$sex_1_2,
                              HRS2018_data$wealth_noIRA,
                              
                              HRS2018_data$start,
                              HRS2018_data$stop,
                              
                           HRS2018_data$reason_discrim1_reason_age, 
                           
                              HRS2018_data$reason_discrim1_reason_disability,
                              
                              HRS2018_data$summary_mean_score_discrim, 
                              HRS2018_data$checklist_depression_bin, 
                           
                              HRS2018_data$reason_discirim_financial, 
                              HRS2018_data$reason_discirim_sexuality, 
                              HRS2018_data$number_reasons_discrimination, 
                           
                           
                           HRS2018_data$reason_discrim1_reason_gender, 
                           HRS2018_data$reason_discrim1_reason_national, 
                           HRS2018_data$reason_discrim1_reason_race, 
                           HRS2018_data$reason_discrim1_reason_religion, 
                           HRS2018_data$reason_discrim1_reason_weight, 
                              
                              
                        
                              HRS2018_data$discrim_harassed,
                              HRS2018_data$discrim_lessrespect,
                              HRS2018_data$discrim_medical,
                              HRS2018_data$discrim_notclever,
                              HRS2018_data$discrim_poorerservice,
                              HRS2018_data$discrim_afraidothers,
                              
                           HRS2018_data$continious_age,
                           HRS2018_data$sex_1_2,
                           HRS2018_data$assessed_BMI, 
                           HRS2018_data$limiting_condition_bin, 
                           HRS2018_data$national_origin_ousideUS, 
                           HRS2018_data$religion_bin, 
                           HRS2018_data$race_white, 
                           
                           
                           
                           HRS2018_data$year)


HRS2018_data_short = as.data.frame(HRS2018_data_short)
colnames(HRS2018_data_short) = colnames_all



write.csv(HRS2008_data_short, paste(SOURCE_ROOT, "HRS_2008_data/HRS2008_data_short.csv", sep=""))
write.csv(HRS2010_data_short, paste(SOURCE_ROOT, "HRS_2010_data/HRS2010_data_short.csv", sep=""))
write.csv(HRS2012_data_short, paste(SOURCE_ROOT, "HRS_2012_data/HRS2012_data_short.csv", sep=""))
write.csv(HRS2014_data_short, paste(SOURCE_ROOT, "HRS_2014_data/HRS2014_data_short.csv", sep=""))
write.csv(HRS2016_data_short, paste(SOURCE_ROOT, "HRS_2016_data/HRS2016_data_short.csv", sep=""))
write.csv(HRS2018_data_short, paste(SOURCE_ROOT, "HRS_2018_data/HRS2018_data_short.csv", sep=""))


