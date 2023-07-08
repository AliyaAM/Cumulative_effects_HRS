


directory = "/Users/k2147340/OneDrive - King's College London/Documents/"

SOURCE_ROOT = paste(directory, "proj/Cumulative_effects_HRS/", sep = "")

#DATA_ROOT = paste(directory, "proj/Cumulative_effects_HRS/data_files/", sep = "") 


DATA_ROOT = paste(directory, "/ELSA_HRS/Data_analysis/", sep = "") 


### There is no 
data_v2 = read.csv("C:/Users/k2147340/OneDrive - King's College London/Desktop/randhrs1992_2018v1.csv")

data_v2$RAHISPAN





H_HRS_data = read.csv(paste(DATA_ROOT, "H_HRS_c.csv", sep = ""))
RAND_HRS_longitudional_file = read.csv(paste(DATA_ROOT, "randhrs1992_2018v1.csv", sep=""))
#HHIDPN = RAND_HRS_longitudional_file$HHIDPN
unique(H_HRS_data$rahispan)
unique(RAND_HRS_longitudional_file$rahispan)
ls(H_HRS_data)
unique(H_HRS_data$RAHISPAN)
unique(H_HRS_data$LB028)


#ls(H_HRS_data)

#H_HRS_data$r8

#data 
#HRS2008_data_initial$HRS2008_hispanic_bin

HRS2008_data_initial = read.csv(paste(DATA_ROOT, "HRS_2008_data/HRS2008_data_short_OLD.csv", sep=""))
HRS2010_data_initial = read.csv(paste(DATA_ROOT, "HRS_2010_data/HRS2010_data_short_OLD.csv", sep=""))
HRS2012_data_initial = read.csv(paste(DATA_ROOT, "HRS_2012_data/HRS2012_data_short_OLD.csv", sep=""))
HRS2014_data_initial = read.csv(paste(DATA_ROOT, "HRS_2014_data/HRS2014_data_short_OLD.csv", sep=""))
HRS2016_data_initial = read.csv(paste(DATA_ROOT, "HRS_2016_data/HRS2016_data_short_OLD.csv", sep=""))
HRS2018_data_initial = read.csv(paste(DATA_ROOT, "HRS_2018_data/HRS2018_data_short_OLD.csv", sep=""))
#cross_waves = data.frame(HHIDPN) 

ids_vector_2008 = unique(HRS2008_data_initial$HHIDPN) #example: 17707041
ids_vector_2010 = unique(HRS2010_data_initial$HHIDPN)
ids_vector_2012 = unique(HRS2012_data_initial$HHIDPN)
ids_vector_2014 = unique(HRS2014_data_initial$HHIDPN)
ids_vector_2016 = unique(HRS2016_data_initial$HHIDPN)
ids_vector_2018 = unique(HRS2018_data_initial$HHIDPN)

#unique(H_HRS_data$hhidpn)  #example: 14783010

H_HRS_data_2008 = subset(H_HRS_data, H_HRS_data$hhidpn %in% ids_vector_2008) 
nrow(H_HRS_data_2008)

H_HRS_data_2010 = subset(H_HRS_data, H_HRS_data$hhidpn %in% ids_vector_2010) 
nrow(H_HRS_data_2010)

H_HRS_data_2012 = subset(H_HRS_data, H_HRS_data$hhidpn %in% ids_vector_2012) 
nrow(H_HRS_data_2012)

H_HRS_data_2014 = subset(H_HRS_data, H_HRS_data$hhidpn %in% ids_vector_2014) 
nrow(H_HRS_data_2014)

H_HRS_data_2016 = subset(H_HRS_data, H_HRS_data$hhidpn %in% ids_vector_2016) 
nrow(H_HRS_data_2016)

H_HRS_data_2018 = subset(H_HRS_data, H_HRS_data$hhidpn %in% ids_vector_2018) 
nrow(H_HRS_data_2018)


HRS2008_data_initial$education_level = H_HRS_data_2008$raeducl
HRS2010_data_initial$education_level = H_HRS_data_2010$raeducl
HRS2012_data_initial$education_level = H_HRS_data_2012$raeducl
HRS2014_data_initial$education_level = H_HRS_data_2014$raeducl
HRS2016_data_initial$education_level = H_HRS_data_2016$raeducl
HRS2018_data_initial$education_level = H_HRS_data_2018$raeducl


write.csv(HRS2008_data_initial, paste(DATA_ROOT, "HRS_2008_data/HRS2008_data_short_education.csv", sep=""))
write.csv(HRS2010_data_initial, paste(DATA_ROOT, "HRS_2010_data/HRS2010_data_short_education.csv", sep=""))
write.csv(HRS2012_data_initial, paste(DATA_ROOT, "HRS_2012_data/HRS2012_data_short_education.csv", sep=""))
write.csv(HRS2014_data_initial, paste(DATA_ROOT, "HRS_2014_data/HRS2014_data_short_education.csv", sep=""))
write.csv(HRS2016_data_initial, paste(DATA_ROOT, "HRS_2016_data/HRS2016_data_short_education.csv", sep=""))
write.csv(HRS2018_data_initial, paste(DATA_ROOT, "HRS_2018_data/HRS2018_data_short_education.csv", sep=""))



H_HRS_data$hhidpn
ls(H_HRS_data)

#raeducl: r harmonized education level
#Response type:
  #Enumerated, Responses: 1.less than upper secondary, 2.upper secondary and vocational training, 3.tertiary, .m:Missing