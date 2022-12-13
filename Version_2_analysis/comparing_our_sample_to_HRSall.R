

library("dplyr")
library("tidyr")
library("bain")

current_directory = "/Users/aliya/my_docs/"



DATAIN_ROOT = (paste(current_directory, "KCL_postDoc/Data_analysis/", sep="")) 
SOURCE_ROOT = (paste(current_directory, "proj/Cumulative_effects_HRS/Version_2_analysis/", sep=""))
OUTPUT_ROOT =(paste(current_directory, "KCL_postDoc/Cumulative_effects/", sep=""))


source((paste(SOURCE_ROOT, "sort_timepoints_allHRS.R", sep="")))



#data for each year that has all cases but only the relevent columns (vars), hense called short. OLD stands for the way the diabetes outcome was extracted (which is correct) 

HRS2008_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2008_data/HRS2008_dataset_latest.csv", sep=""))
HRS2010_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2010_data/HRS2010_dataset_latest.csv", sep=""))
HRS2012_data_initial =  read.csv(paste(DATAIN_ROOT, "HRS_2012_data/HRS2012_dataset_latest.csv", sep=""))
HRS2014_data_initial =  read.csv(paste(DATAIN_ROOT, "HRS_2014_data/HRS2014_dataset_latest.csv", sep=""))
HRS2016_data_initial =  read.csv(paste(DATAIN_ROOT, "HRS_2016_data/HRS2016_dataset_latest.csv", sep=""))
HRS2018_data_initial =  read.csv(paste(DATAIN_ROOT, "HRS_2018_data/HRS2018_dataset_latest.csv", sep=""))


# the analytical sample for the WCE (and the subset of it is in COX), which includes all years and each year is labeled by a new variable (start_new: 0 for baseline, 1 for the first follow-up, 2 for the second follow-up etc, at 2-year intervals)
# the analytical sample does not include the baseline diabetes. 

#wealth 
median(HRS2010_data_initial$wealth_noIRA, na.rm = TRUE)
HRS2010_data_initial$ses = case_when(HRS2010_data_initial$wealth_noIRA <=median(HRS2010_data_initial$wealth_noIRA, na.rm = TRUE) ~ 1,
                                     HRS2010_data_initial$wealth_noIRA > median(HRS2010_data_initial$wealth_noIRA, na.rm = TRUE) ~ 2)



median(HRS2012_data_initial$wealth_noIRA, na.rm = TRUE)
HRS2012_data_initial$ses = case_when(HRS2012_data_initial$wealth_noIRA <=median(HRS2012_data_initial$wealth_noIRA, na.rm = TRUE) ~ 1,
                                     HRS2012_data_initial$wealth_noIRA > median(HRS2012_data_initial$wealth_noIRA, na.rm = TRUE) ~ 2)


median(HRS2014_data_initial$wealth_noIRA, na.rm = TRUE)
HRS2014_data_initial$ses = case_when(HRS2014_data_initial$wealth_noIRA <=median(HRS2014_data_initial$wealth_noIRA, na.rm = TRUE) ~ 1,
                                     HRS2014_data_initial$wealth_noIRA > median(HRS2014_data_initial$wealth_noIRA, na.rm = TRUE) ~ 2)



median(HRS2016_data_initial$wealth_noIRA, na.rm = TRUE)
HRS2016_data_initial$ses = case_when(HRS2016_data_initial$wealth_noIRA <=median(HRS2016_data_initial$wealth_noIRA, na.rm = TRUE) ~ 1,
                                     HRS2016_data_initial$wealth_noIRA > median(HRS2016_data_initial$wealth_noIRA, na.rm = TRUE) ~ 2)

###############
###############
###############
###############


HRS2010_data = cbind(HRS2010_data_initial$HHIDPN, 
                     
                     HRS2010_data_initial$diabetes_new, 
                     HRS2010_data_initial$diabetes_ever, 
                     
                     HRS2010_data_initial$continious_age, 
                     HRS2010_data_initial$sex_1_2, 
                     
                     HRS2010_data_initial$ses, 
                     
                     HRS2010_data_initial$angina2yrs_bin, 
                     HRS2010_data_initial$HRS2010_heartattack_bin, 
                  
                     
                     HRS2010_data_initial$hypertension_new_bin, 
                     HRS2010_data_initial$assessed_BMI, 
                     HRS2010_data_initial$checklist_depression_bin, 
                     HRS2010_data_initial$alcohol_days_week, 
                     HRS2010_data_initial$smokes_now_bin, 
                     HRS2010_data_initial$vigarious_physical_activity)

HRS2012_data_initial$angina_new_bin = case_when(HRS2012_data_initial$angina_new == 1 ~1,
                                                HRS2012_data_initial$angina_new == 0 ~ 0)                   


HRS2012_data = cbind(HRS2012_data_initial$HHIDPN, 
                     
                     HRS2012_data_initial$diabetes_new, 
                     HRS2012_data_initial$diabetes_ever, 
                     
                     HRS2012_data_initial$continious_age, 
                     HRS2012_data_initial$sex_1_2, 
                     
                     HRS2012_data_initial$ses, 
                     
                     HRS2012_data_initial$angina_new_bin,
                     HRS2012_data_initial$heartattack_ever_bin, 
                    
                     HRS2012_data_initial$hypertension_new_bin, 
                     HRS2012_data_initial$assessed_BMI, 
                     HRS2012_data_initial$checklist_depression_bin, 
                     HRS2012_data_initial$alcohol_days_week, 
                     HRS2012_data_initial$smokes_now_bin, 
                     HRS2012_data_initial$vigarious_physical_activity)



HRS2014_data = cbind(HRS2014_data_initial$HHIDPN, 
                     
                     HRS2014_data_initial$diabetes_new, 
                     HRS2014_data_initial$diabetes_ever, 
                     
                     HRS2014_data_initial$continious_age, 
                     HRS2014_data_initial$sex_1_2, 
                     
                     HRS2014_data_initial$ses, 
                     
                     
                     HRS2014_data_initial$angina_new_bin,
                     HRS2014_data_initial$heartattack_ever_bin, 
                     
                     HRS2014_data_initial$hypertension_new_bin, 
                     HRS2014_data_initial$assessed_BMI, 
                     HRS2014_data_initial$checklist_depression_bin, 
                     HRS2014_data_initial$alcohol_days_week, 
                     HRS2014_data_initial$smokes_now_bin, 
                     HRS2014_data_initial$vigarious_physical_activity)

HRS2016_data_initial$angina2yrs_bin = case_when(HRS2016_data_initial$angina2yrs_bin == 1 ~ 1, 
                                                HRS2016_data_initial$angina2yrs_bin == 0 ~ 0)


HRS2016_data = cbind(HRS2016_data_initial$HHIDPN, 
                     
                     HRS2016_data_initial$diabetes_new, 
                     HRS2016_data_initial$diabetes_ever, 
                     
                     HRS2016_data_initial$continious_age, 
                     HRS2016_data_initial$sex_1_2, 
                     
                     HRS2016_data_initial$ses, 
                     
                     
                     HRS2016_data_initial$angina2yrs_bin,
                     HRS2016_data_initial$heartattack_ever_bin, 
                     
                     
                     HRS2016_data_initial$hypertension_new_bin, 
                     
                     HRS2016_data_initial$assessed_BMI, 
                     
                     HRS2016_data_initial$checklist_depression_bin, 
                     HRS2016_data_initial$alcohol_days_week, 
                     HRS2016_data_initial$smokes_now_bin, 
                     HRS2016_data_initial$vigarious_physical_activity)



  
data_initial = rbind(HRS2010_data, 
                     HRS2012_data, 
                     HRS2014_data, 
                     HRS2016_data)


colnames(data_initial) = c("HHIDPN", 
                           "diabetes_new", 
                           "diabetes_ever",
                           
                           "continious_age", 
                           
                           "sex_1_2", 
                           
                           "ses",
                           
                           "angina_new_bin", 
                           "heart_attack_ever_bin", 
                           
                           "hypertension_new_bin", 
                           "assessed_BMI", 
                           'checklist_depression_bin', 
                           "alcohol_days_week", 
                           'smokes_now_bin', 
                            'vigarious_physical_activity')

data_initial = as.data.frame(data_initial)

all_data_HRS = sort_timepoints_allHRS(data = data_initial)

nrow(all_data_HRS)

write.csv(all_data_HRS, paste(OUTPUT_ROOT, "all_data_HRS_2010_2016.csv", sep = ""))
