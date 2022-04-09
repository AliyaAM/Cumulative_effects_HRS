


library("here")
library("Hmisc")


current_directory = here()


OUTPUT_ROOT =(paste(current_directory, "/data_files/Results/", sep=""))
SOURCE_ROOT = (paste(current_directory, "/Version_2_analysis/", sep=""))
DATAIN_ROOT = (paste(current_directory, "/data_files/", sep="")) 


data = read.csv(paste(DATAIN_ROOT, "HRS2008_data_short.csv", sep=""))

data$discrim_harassed_num = as.numeric(data$discrim_harassed)


table(data$discrim_harassed_num)

length(data$discrim_harassed)

table(data$discrim_harassed)



unique(data$discrim_harassed_num)

unique(data$discrim_harassed)



data$discrim_harassed_bin = case_when(data$discrim_harassed_num == 1 ~ 1, 
                                      data$discrim_harassed_num == 2 ~ 1, 
                                      data$discrim_harassed_num == 3 ~ 1, 
                                      data$discrim_harassed_num == 4 ~ 1, 
                                      data$discrim_harassed_num == 5 ~ 0, 
                                      data$discrim_harassed_num == 6 ~ 0,
                                      data$discrim_harassed_num == 0 ~ 0) 

table(data$discrim_harassed_bin)


data$discrim_harassed =  case_when(data$discrim_harassed==" NA"  ~NA, 
                                   data$discrim_harassed=="6"~6, 
                                   data$discrim_harassed=="5"~5, 
                                   data$discrim_harassed=="4"~4, 
                                   data$discrim_harassed=="3"~3, 
                                   data$discrim_harassed=="2"~2, 
                                   data$discrim_harassed=="1"~1, 
                                   data$discrim_harassed=="0"~0,
                                   data$discrim_harassed==NA~NA)





data$discrim_lessrespect =  case_when(data$discrim_lessrespect==" NA"  ~NA, 
                                      data$discrim_lessrespect=="6"~6, 
                                      data$discrim_lessrespect=="5"~5, 
                                      data$discrim_lessrespect=="4"~4, 
                                      data$discrim_lessrespect=="3"~3, 
                                      data$discrim_lessrespect=="2"~2, 
                                      data$discrim_lessrespect=="1"~1, 
                                      data$discrim_lessrespect=="0"~0,
                                      data$discrim_lessrespect==NA~NA)





data$discrim_medical =  case_when(data$discrim_medical==" NA"  ~NA, 
                                  data$discrim_medical=="6"~6, 
                                  data$discrim_medical=="5"~5, 
                                  data$discrim_medical=="4"~4, 
                                  data$discrim_medical=="3"~3, 
                                  data$discrim_medical=="2"~2, 
                                  data$discrim_medical=="1"~1, 
                                  data$discrim_medical=="0"~0,
                                  data$discrim_medical==NA~NA)

data$discrim_notclever =  case_when(data$discrim_notclever==" NA"  ~NA, 
                                    data$discrim_notclever=="6"~6, 
                                    data$discrim_notclever=="5"~5, 
                                    data$discrim_notclever=="4"~4, 
                                    data$discrim_notclever=="3"~3, 
                                    data$discrim_notclever=="2"~2, 
                                    data$discrim_notclever=="1"~1, 
                                    data$discrim_notclever=="0"~0,
                                    data$discrim_notclever==NA~NA)



