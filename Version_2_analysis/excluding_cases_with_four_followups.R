
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


