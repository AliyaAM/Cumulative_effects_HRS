
current_directory = "/Users/aliyaamirova/"
DATA_ROOT = paste(current_directory, "KCL_postDoc/Data_analysis/", sep = "")

DATA_ROOT = paste(current_directory, "ELSA_HRS/Data_analysis/", sep = "") 

HRS2008_data_initial = read.csv(paste(DATA_ROOT, "HRS_2008_data/HRS2008_data_short_education.csv", sep=""))
HRS2010_data_initial = read.csv(paste(DATA_ROOT, "HRS_2010_data/HRS2010_data_short_education.csv", sep=""))
HRS2012_data_initial = read.csv(paste(DATA_ROOT, "HRS_2012_data/HRS2012_data_short_education.csv", sep=""))
HRS2014_data_initial = read.csv(paste(DATA_ROOT, "HRS_2014_data/HRS2014_data_short_education.csv", sep=""))
HRS2016_data_initial = read.csv(paste(DATA_ROOT, "HRS_2016_data/HRS2016_data_short_education.csv", sep=""))
HRS2018_data_initial = read.csv(paste(DATA_ROOT, "HRS_2018_data/HRS2018_data_short_education.csv", sep=""))

add_hispanic = read.csv("/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/randhrs1992_2018v1.csv")
unique(add_hispanic$HHIDPN)

nrow(add_hispanic)
add_hispanic$RAHISPAN


