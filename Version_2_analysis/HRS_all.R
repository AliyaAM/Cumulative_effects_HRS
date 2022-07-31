

library("dplyr")
library("tidyr")
library("bain")

current_directory = "/Users/aliya/my_docs/"



DATAIN_ROOT = (paste(current_directory, "KCL_postDoc/Data_analysis/", sep="")) 
SOURCE_ROOT = (paste(current_directory, "proj/Cumulative_effects_HRS/Version_2_analysis/", sep=""))
OUTPUT_ROOT =(paste(current_directory, "KCL_postDoc/Cumulative_effects/", sep=""))


source((paste(SOURCE_ROOT, "sort_timepoints_allHRS.R", sep="")))

source((paste(SOURCE_ROOT, "sort_timepoints_drop_baseline.R", sep="")))


#data for each year that has all cases but only the relevent columns (vars), hense called short. OLD stands for the way the diabetes outcome was extracted (which is correct) 

HRS2008_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2008_data/HRS2008_dataset_latest.csv", sep=""))
HRS2010_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2010_data/HRS2010_dataset_latest.csv", sep=""))
HRS2012_data_initial =  read.csv(paste(DATAIN_ROOT, "HRS_2012_data/HRS2012_dataset_latest.csv", sep=""))
HRS2014_data_initial =  read.csv(paste(DATAIN_ROOT, "HRS_2014_data/HRS2014_dataset_latest.csv", sep=""))
HRS2016_data_initial =  read.csv(paste(DATAIN_ROOT, "HRS_2016_data/HRS2016_dataset_latest.csv", sep=""))
HRS2018_data_initial =  read.csv(paste(DATAIN_ROOT, "HRS_2018_data/HRS2018_dataset_latest.csv", sep=""))


# the analytical sample for the WCE (and the subset of it is in COX), which includes all years and each year is labeled by a new variable (start_new: 0 for baseline, 1 for the first follow-up, 2 for the second follow-up etc, at 2-year intervals)
# the analytical sample does not include the baseline diabetes. 

###############
###############
###############
###############
HRS2008_data = cbind(HRS2008_data_initial$HHIDPN, 
                     HRS2008_data_initial$diabetes_new)


HRS2010_data = cbind(HRS2010_data_initial$HHIDPN, 
                     HRS2010_data_initial$diabetes_new)

HRS2012_data = cbind(HRS2012_data_initial$HHIDPN, 
                     HRS2012_data_initial$diabetes_new)

HRS2014_data = cbind(HRS2014_data_initial$HHIDPN, 
                     HRS2014_data_initial$diabetes_new)

HRS2016_data = cbind(HRS2016_data_initial$HHIDPN, 
                     HRS2016_data_initial$diabetes_new)

HRS2018_data = cbind(HRS2018_data_initial$HHIDPN, 
                     HRS2018_data_initial$diabetes_new)
                     #HRS2018_data_initial$diabetes_ever)

data_initial = rbind(HRS2008_data, 
                     HRS2010_data, 
                     HRS2012_data, 
                     HRS2014_data, 
                     HRS2016_data, 
                     HRS2018_data)


colnames(data_initial) = c("HHIDPN")

data_initial = as.data.frame(data_initial)

ID = unique(data_initial$HHIDPN)


no_baseline_diabetes = sort_timepoints_drop_baseline(data_initial)
unique(no_baseline_diabetes$HHIDPN)


wave_1 = subset(no_baseline_diabetes,  no_baseline_diabetes$start_new == 0)
wave_1_diabetes = as.data.frame(wave_1_diabetes)
diabetes_wave_1_unique = unique(wave_1_diabetes$HHIDPN)


all_waves_no_diab_baseline <- no_baseline_diabetes[ !(no_baseline_diabetes$HHIDPN %in% c(diabetes_wave_1_unique)), ]
IDno_baseline_diabetes = unique(all_waves_no_diab_baseline$HHIDPN)



