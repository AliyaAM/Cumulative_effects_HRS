
dataset = read.csv(file = "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/Results/dataset_noNAs_timepoints_TEST_DELETE_AFTER_debugging_24nov2022.csv")

were_present_at_four_points_ids = c(16602020,
                                    35607020,
                                    78026021,
                                    145768020,
                                    210031010,
                                    210661010,	
                                    501617010,	
                                    501870010,
                                    502417020,
                                    57714040,	
                                    916299010)

ID_4tp = unique(were_present_at_four_points_ids)


dataset_ftp_timepoints<-dataset[(dataset$HHIDPN %in% ID_4tp),]

dataset_ftp_timepoints_baseline = dataset_ftp_timepoints[(dataset_ftp_timepoints$start_new == 0),]
summary(dataset_ftp_timepoints_baseline)



dataset_noftp_timepoints<-dataset[!(dataset$HHIDPN %in% ID_4tp),]



HRS2008_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2008_data/old/HRS2008_data_short_OLD.csv", sep=""))
HRS2010_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2010_data/old/HRS2010_data_short_OLD.csv", sep=""))
HRS2012_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2012_data/old/HRS2012_data_short_OLD.csv", sep=""))
HRS2014_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2014_data/old/HRS2014_data_short_OLD.csv", sep=""))
HRS2016_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2016_data/old/HRS2016_data_short_OLD.csv", sep=""))
HRS2018_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2018_data/old/HRS2018_data_short_OLD.csv", sep=""))


dataset_initial = rbind(HRS2008_data_initial, 
                        HRS2010_data_initial, 
                        HRS2012_data_initial, 
                        HRS2014_data_initial, 
                        HRS2016_data_initial, 
                        HRS2018_data_initial)


dataset_ftp_timepoints<-dataset_initial[(dataset_initial$HHIDPN %in% ID_4tp),]


dataset_ftp_timepoints_baseline = subset(dataset_ftp_timepoints, dataset_ftp_timepoints$start == 0)

nrow(dataset_ftp_timepoints_baseline)
#dataset_initial[(dataset_initial$HHIDPN %in% ID_4tp),]


table(dataset_ftp_timepoints_baseline$sex_1_2)


ID916299010_char = subset(dataset_initial, dataset_initial$HHIDPN == 916299010) #female, no diabetes, 52 at baseline. BMI = 24, no limiting disability at baseline 


table(dataset_ftp_timepoints_baseline$limiting_condition_bin) 

# no one as disability, # 11 people to exclude: 5 women 6 men,  # two people above BMI>30


table(dataset_ftp_timepoints_baseline$assessed_BMI) 





