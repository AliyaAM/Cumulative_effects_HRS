
library("dplyr")
library("tidyr")
library("bain")

current_directory = "/Users/aliya/my_docs/"


DATAIN_ROOT = (paste(current_directory, "KCL_postDoc/Data_analysis/", sep="")) 
SOURCE_ROOT = (paste(current_directory, "proj/Cumulative_effects_HRS/Version_2_analysis/", sep=""))
OUTPUT_ROOT =(paste(current_directory, "KCL_postDoc/Cumulative_effects/", sep=""))


source((paste(SOURCE_ROOT, "participant_char_function.R", sep="")))

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

