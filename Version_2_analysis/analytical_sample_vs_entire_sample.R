library(dplyr)




DATAIN_ROOT = (paste("/Users/aliya/my_docs/KCL_postDoc/Data_analysis/", sep="")) 


#data for each year that has all cases but only the relevent columns (vars), hense called short. OLD stands for the way the diabetes outcome was extracted (which is correct) 

HRS2008_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2008_data/old/HRS2008_data_short_OLD.csv", sep=""))
HRS2010_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2010_data/old/HRS2010_data_short_OLD.csv", sep=""))
HRS2012_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2012_data/old/HRS2012_data_short_OLD.csv", sep=""))
HRS2014_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2014_data/old/HRS2014_data_short_OLD.csv", sep=""))
HRS2016_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2016_data/old/HRS2016_data_short_OLD.csv", sep=""))
HRS2018_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2018_data/old/HRS2018_data_short_OLD.csv", sep=""))


# the analytical sample for the WCE (and the subset of it is in COX), which includes all years and each year is labeled by a new variable (start_new: 0 for baseline, 1 for the first follow-up, 2 for the second follow-up etc, at 2-year intervals)
# the analytical sample does not include the baseline diabetes. 

analytical_sample_COX = read.csv("/Users/aliya/my_docs/proj/Cumulative_effects_HRS/data_files/all_waves_nodiabatbaseline_DIAB.csv")

#this below is for those with the BMI >30 kg/m2
analytical_sample_BMI = read.csv("/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/DATA_FOR_PLOT/all_waves_nodiabatbaseline_DIAB.csv")




#####
# check if the rows with NAs in dicrimin_bin were excluded at each timepoint 
# check: Rows with missing data (NAs) in the discrimination variable (yes/no) at any time point (2008-2018) were excluded from the analyses for the WCE analysis BUT not for the COX model.  
#Check analytical sample for Cox vs WCE (should be different as different NAs are excluded): 
# ANSWER: individual-timepoints rows in COX = 89691: individual-timepoints rows. (indvidual_1 & baseline is the first row, individual_1 and follow-up is second row etc)
# ANSWER: individual-timepoints rows in BMI = 10199: individual-timepoints rows. (indvidual_1 & baseline is the first row, individual_1 and follow-up is second row etc)
#ANSWER: analytical sample for COX: 22731 individuals 

COX_unique_id = unique(analytical_sample_COX$HHIDPN)
COX_unique_id_n = length(COX_unique_id) 

#######
WCE_unique_id = unique(analytical_sample_BMI$HHIDPN)
WCE_unique_id_n = length(WCE_unique_id) 
# recode into single var  discrim_bin


analytical_sample_COX$discrim_harassed_bin = case_when(analytical_sample_COX$discrim_harassed == 1 ~ 1, 
                                                       analytical_sample_COX$discrim_harassed == 2 ~ 1, 
                                                       analytical_sample_COX$discrim_harassed == 3 ~ 1, 
                                                       analytical_sample_COX$discrim_harassed == 4 ~ 1, 
                                                       analytical_sample_COX$discrim_harassed == 5 ~ 0, 
                                                       analytical_sample_COX$discrim_harassed == 6 ~ 0,
                                                       analytical_sample_COX$discrim_harassed == 0 ~ 0) 



analytical_sample_COX$discrim_lessrespect_bin = case_when(analytical_sample_COX$discrim_lessrespect == 1 ~ 1, 
                                                          analytical_sample_COX$discrim_lessrespect == 2 ~ 1, 
                                                          analytical_sample_COX$discrim_lessrespect == 3 ~ 1, 
                                                          analytical_sample_COX$discrim_lessrespect == 4 ~ 1, 
                                                          analytical_sample_COX$discrim_lessrespect == 5 ~ 0, 
                                                          analytical_sample_COX$discrim_lessrespect == 6 ~ 0,
                                                          analytical_sample_COX$discrim_lessrespect == 0 ~ 0) 



analytical_sample_COX$discrim_medical_bin = case_when(analytical_sample_COX$discrim_medical == 1 ~ 1, 
                                                      analytical_sample_COX$discrim_medical == 2 ~ 1, 
                                                      analytical_sample_COX$discrim_medical == 3 ~ 1, 
                                                      analytical_sample_COX$discrim_medical == 4 ~ 1, 
                                                      analytical_sample_COX$discrim_medical == 5 ~ 0, 
                                                      analytical_sample_COX$discrim_medical == 6 ~ 0,
                                                      analytical_sample_COX$discrim_medical == 0 ~ 0) 





analytical_sample_COX$discrim_notclever_bin = case_when(analytical_sample_COX$discrim_notclever == 1 ~ 1, 
                                                        analytical_sample_COX$discrim_notclever == 2 ~ 1, 
                                                        analytical_sample_COX$discrim_notclever == 3 ~ 1, 
                                                        analytical_sample_COX$discrim_notclever == 4 ~ 1, 
                                                        analytical_sample_COX$discrim_notclever == 5 ~ 0, 
                                                        analytical_sample_COX$discrim_notclever == 6 ~ 0,
                                                        analytical_sample_COX$discrim_notclever == 0 ~ 0) 






analytical_sample_COX$discrim_poorerservice_bin = case_when(analytical_sample_COX$discrim_poorerservice == 1 ~ 1, 
                                                            analytical_sample_COX$discrim_poorerservice == 2 ~ 1, 
                                                            analytical_sample_COX$discrim_poorerservice == 3 ~ 1, 
                                                            analytical_sample_COX$discrim_poorerservice == 4 ~ 1, 
                                                            analytical_sample_COX$discrim_poorerservice == 5 ~ 0, 
                                                            analytical_sample_COX$discrim_poorerservice == 6 ~ 0) 




analytical_sample_COX$discrim_afraidothers_bin = case_when(analytical_sample_COX$discrim_afraidothers == 1 ~ 1,
                                                           analytical_sample_COX$discrim_afraidothers == 2 ~ 1, 
                                                           analytical_sample_COX$discrim_afraidothers == 3 ~ 1, 
                                                           analytical_sample_COX$discrim_afraidothers == 4 ~ 1, 
                                                           analytical_sample_COX$discrim_afraidothers == 5 ~ 0, 
                                                           analytical_sample_COX$discrim_afraidothers == 6 ~ 0,
                                                           analytical_sample_COX$discrim_afraidothers == 0 ~ 0) 



analytical_sample_COX$discrim_bin = case_when(analytical_sample_COX$discrim_harassed_bin == 1 | analytical_sample_COX$discrim_lessrespect_bin == 1 | analytical_sample_COX$discrim_medical_bin  == 1 | analytical_sample_COX$discrim_notclever_bin == 1 | analytical_sample_COX$discrim_afraidothers_bin == 1 | analytical_sample_COX$discrim_poorerservice_bin == 1 ~ 1, 
                                              analytical_sample_COX$discrim_harassed_bin == 0 & analytical_sample_COX$discrim_lessrespect_bin == 0 & analytical_sample_COX$discrim_medical_bin  == 0 & analytical_sample_COX$discrim_notclever_bin == 0 & analytical_sample_COX$discrim_afraidothers_bin == 0 & analytical_sample_COX$discrim_poorerservice_bin == 0 ~ 0) 



unique(analytical_sample_COX$discrim_bin)



unique(analytical_sample_COX$start_new)
baseline_data_COX = subset(analytical_sample_COX, analytical_sample_COX$start_new == 0)
last_follow_up_COX = subset(analytical_sample_COX, analytical_sample_COX$start_new == 5)
unique(baseline_data_COX$diabetes_new)
unique(last_follow_up_COX$diabetes_new)
unique(baseline_data_COX$discrim_bin)

unique(baseline_data_COX$sex_1_2)
unique(baseline_data_COX$race_white)
min(baseline_data_COX$assessed_BMI, na.rm = TRUE)
max(baseline_data_COX$assessed_BMI, na.rm = TRUE)
unique(baseline_data_COX$assessed_BMI)
#####
# compare included sample to the overall sample.






#####
#Check how many women, ethnic minority, BMI>30, were exposed to everyday discrimination every 2- year internal. 
#Assess how many were exposed to discrimination every 2-year intervals (prolonged exposures in each subset all, women, ethnic minorities, BM5> 30 
#This might help explain why we found a cumulative effect in ethnic minorities but did not find it in others. 





#####
# Check the time window (median follow-up in years)


#####
#Run t-tests comparing diff. sample characteristics between those who developed T2DM and those who did not


#####
#Compare AIC and BIC across the 5 included models




#####                                                                                                                                                                                      
#Add wealth quantiles to table 1.






