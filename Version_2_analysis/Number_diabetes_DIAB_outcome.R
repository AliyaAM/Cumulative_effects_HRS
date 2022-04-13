
library(dplyr)
library(tidyr)

current_directory = "/Users/aliya/my_docs/proj/Cumulative_effects_HRS"


OUTPUT_ROOT =(paste(current_directory, "/data_files/", sep=""))
SOURCE_ROOT = (paste(current_directory, "/Version_2_analysis/", sep=""))

#DATAIN_ROOT = (paste(current_directory, "/data_files/", sep="")) 


DATAIN_ROOT = (paste("/Users/aliya/my_docs/KCL_postDoc/Data_analysis/", sep="")) 

total_n_proportion = data.frame()
#"heartcondition_ever_bin", "heartcondition_new_bin", "angina_new_bin", "stroke_new_bin", "heartfailure2yrs_bin", "heartattack_ever_bin", "heartattack_new_bin"
#data 
HRS2008_data = read.csv(paste(DATAIN_ROOT, "HRS_2008_data/old/HRS2008_data_short_OLD.csv", sep=""))
HRS2010_data = read.csv(paste(DATAIN_ROOT, "HRS_2010_data/old/HRS2010_data_short_OLD.csv", sep=""))
HRS2012_data = read.csv(paste(DATAIN_ROOT, "HRS_2012_data/old/HRS2012_data_short_OLD.csv", sep=""))
HRS2014_data = read.csv(paste(DATAIN_ROOT, "HRS_2014_data/old/HRS2014_data_short_OLD.csv", sep=""))
HRS2016_data = read.csv(paste(DATAIN_ROOT, "HRS_2016_data/old/HRS2016_data_short_OLD.csv", sep=""))
HRS2018_data = read.csv(paste(DATAIN_ROOT, "HRS_2018_data/old/HRS2018_data_short_OLD.csv", sep=""))


#HRS2008_data = subset(HRS2008_data, HRS2008_data$diabetes_ever == 0)
#HRS2010_data = subset(HRS2010_data, HRS2010_data$diabetes_ever == 0)
#HRS2012_data = subset(HRS2012_data, HRS2012_data$diabetes_ever == 0)
#HRS2014_data = subset(HRS2014_data, HRS2014_data$diabetes_ever == 0)
#HRS2016_data = subset(HRS2016_data, HRS2016_data$diabetes_ever == 0)
#HRS2018_data = subset(HRS2018_data, HRS2018_data$diabetes_ever == 0)



unique(HRS2008_data$diabetes_new) 
unique(HRS2010_data$diabetes_new) 
unique(HRS2012_data$diabetes_new)
unique(HRS2014_data$diabetes_new)
unique(HRS2016_data$diabetes_new)
unique(HRS2018_data$diabetes_new)

##### DIAB DIAB DIAB DIAB DIAB DIAB recode to DIAB_bin
##### DIAB DIAB DIAB DIAB DIAB DIAB 



HRS2008_data$diabetes_new_bin = case_when(HRS2008_data$diabetes_new == 1 ~ 1,
                                          HRS2008_data$diabetes_new == 0 ~ 0, 
                                          HRS2008_data$diabetes_new == 3 ~ 1, 
                                          HRS2008_data$diabetes_new == 4 ~ 0)    


HRS2010_data$diabetes_new_bin = case_when(HRS2010_data$diabetes_new == 1 ~ 1,
                                          HRS2010_data$diabetes_new == 0 ~ 0, 
                                          HRS2010_data$diabetes_new == 3 ~ 1, 
                                          HRS2010_data$diabetes_new == 4 ~ 0)   

##### DIAB DIAB DIAB DIAB DIAB DIAB 
##### DIAB DIAB DIAB DIAB DIAB DIAB 



HRS2012_data$diabetes_new_bin = case_when(HRS2012_data$diabetes_new == 1 ~ 1,
                                          HRS2012_data$diabetes_new == 0 ~ 0, 
                                          HRS2012_data$diabetes_new == 3 ~ 1, 
                                          HRS2012_data$diabetes_new == 4 ~ 0)

##### DIAB DIAB DIAB DIAB DIAB DIAB 
##### DIAB DIAB DIAB DIAB DIAB DIAB 



HRS2014_data$diabetes_new_bin = case_when(HRS2014_data$diabetes_new == 1 ~ 1,
                                          HRS2014_data$diabetes_new == 0 ~ 0, 
                                          HRS2014_data$diabetes_new == 3 ~ 1, 
                                          HRS2014_data$diabetes_new == 4 ~ 0) 

##### DIAB DIAB DIAB DIAB DIAB DIAB 
##### DIAB DIAB DIAB DIAB DIAB DIAB 


HRS2016_data$diabetes_new_bin = case_when(HRS2016_data$diabetes_new == 1 ~ 1,
                                          HRS2016_data$diabetes_new == 0 ~ 0, 
                                          HRS2016_data$diabetes_new == 3 ~ 1, 
                                          HRS2016_data$diabetes_new == 4 ~ 0) 
unique(HRS2016_data$diabetes_new_bin)


##### DIAB DIAB DIAB DIAB DIAB DIAB 
##### DIAB DIAB DIAB DIAB DIAB DIAB 

HRS2018_data$diabetes_new_bin = case_when(HRS2018_data$diabetes_new == 1 ~ 1,
                                          HRS2018_data$diabetes_new == 0 ~ 0, 
                                          HRS2018_data$diabetes_new == 3 ~ 1, 
                                          HRS2018_data$diabetes_new == 4 ~ 0) 

unique(HRS2018_data$diabetes_new_bin)

##### DIAB DIAB DIAB DIAB DIAB DIAB 
##### DIAB DIAB DIAB DIAB DIAB DIAB 
##### DIAB DIAB DIAB DIAB DIAB DIAB 
##### DIAB DIAB DIAB DIAB DIAB DIAB 
##### DIAB DIAB DIAB DIAB DIAB DIAB 
##### DIAB DIAB DIAB DIAB DIAB DIAB 


entire_dataset = rbind(HRS2008_data, 
                       HRS2010_data,
                       HRS2012_data,
                       HRS2014_data,
                       HRS2016_data,
                       HRS2018_data)


  
  
entire_dataset = entire_dataset %>% drop_na(diabetes_new_bin)
unique(entire_dataset$diabetes_new)

entire_dataset = entire_dataset %>% drop_na(exposure)
unique(entire_dataset$exposure)

entire_dataset_n = unique(entire_dataset$HHIDPN) 
entire_dataset_Nvalue = length(entire_dataset_n) 

total_n_proportion = rbind(total_n_proportion, 
                           entire_dataset_Nvalue)


  ID = unique(entire_dataset$HHIDPN)
  
  #print(isTRUE(entire_dataset$HHIDPN == ID[1]))
  #entire_dataset = entire_dataset %>% drop_na()
  
  participant_wave_df = data.frame()
  
  n = 1
  for (id in ID){
    
    
    participant_wave = subset(entire_dataset, entire_dataset$HHIDPN == id)
    
    if (nrow(participant_wave)== 1){
      
      participant_wave$timepoints_indiv = 1
      
      
      participant_wave$start_new = c(0)
      participant_wave$stop_new = c(1)
      
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    }
    
    if (nrow(participant_wave) ==2){
      
      participant_wave$timepoints_indiv = 2
      
      participant_wave$start_new = c(0, 1)
      participant_wave$stop_new = c(1, 2)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    }
    
    if (nrow(participant_wave)==3){
      
      participant_wave$timepoints_indiv = 3
      
      participant_wave$start_new = c(0, 1, 2)
      participant_wave$stop_new = c(1, 2, 3)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    } 
    
    
    if (nrow(participant_wave)==4){
      
      participant_wave$timepoints_indiv = 4
      
      participant_wave$start_new = c(0, 1, 2, 3)
      participant_wave$stop_new = c(1, 2, 3, 4)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    } 
    
    if (nrow(participant_wave)==5){
      
      participant_wave$timepoints_indiv = 5
      
      participant_wave$start_new = c(0, 1, 2, 3, 4)
      participant_wave$stop_new = c(1, 2, 3, 4, 5)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    } 
    
    if (nrow(participant_wave)==6){
      
      participant_wave$timepoints_indiv = 6
      participant_wave$start_new = c(0, 1, 2, 3, 4, 5)
      participant_wave$stop_new = c(1, 2, 3, 4, 5, 6)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    }
    n = n + 1
  }


wave_1 = subset(participant_wave_df,  participant_wave_df$start_new == 0)
wave_1_diabetes  = subset(wave_1, wave_1$diabetes_new == 1)


diabetes_wave_1_unique = unique(wave_1_diabetes$HHIDPN)
new_diabetes_wave1_dataset = length(diabetes_wave_1_unique)
#wave 1: 224 cases 


wave_2 = subset(participant_wave_df, participant_wave_df$start_new == 1)
wave_2_diabetes  = subset(wave_2, wave_2$diabetes_new == 1)

diabetes_wave_2_unique = unique(wave_2_diabetes$HHIDPN)
new_diabetes_wave2_dataset = length(diabetes_wave_2_unique)
#wave 2: 318 cases 


wave_3 = subset(participant_wave_df, participant_wave_df$start_new == 2)
wave_3_diabetes  = subset(wave_3, wave_3$diabetes_new == 1)


diabetes_wave_3_unique = unique(wave_3_diabetes$HHIDPN)
new_diabetes_wave3_dataset = length(diabetes_wave_3_unique)
#wave 3: 233 cases 


wave_4 = subset(participant_wave_df, participant_wave_df$start_new == 3)
wave_4_diabetes  = subset(wave_4, wave_4$diabetes_new == 1)


diabetes_wave_4_unique = unique(wave_4_diabetes$HHIDPN)
new_diabetes_wave4_dataset = length(diabetes_wave_4_unique)
#wave 4: 174 cases 


wave_5 = subset(participant_wave_df, participant_wave_df$start_new == 4)
wave_5_diabetes  = subset(wave_5, wave_5$diabetes_new == 1)


diabetes_wave_5_unique = unique(wave_5_diabetes$HHIDPN)
new_diabetes_wave5_dataset = length(diabetes_wave_5_unique)
#

wave_6 = subset(participant_wave_df, participant_wave_df$start_new == 5)
wave_6_diabetes  = subset(wave_6, wave_6$diabetes_new == 1)


diabetes_wave_6_unique = unique(wave_6_diabetes$HHIDPN)
new_diabetes_wave6_dataset = length(diabetes_wave_6_unique)



new_diabetes_each_wave = rbind(new_diabetes_wave2_dataset,
                               new_diabetes_wave3_dataset,
                               new_diabetes_wave4_dataset,
                               new_diabetes_wave5_dataset,
                               new_diabetes_wave6_dataset)

write.csv(new_diabetes_each_wave, (paste(OUTPUT_ROOT, "new_diabetes_each_wave_DIAB.csv", sep="")))

all_waves = rbind(wave_1, 
                  wave_2, 
                  wave_3, 
                  wave_4, 
                  wave_5, 
                  wave_6)


all_waves_no_diab_baseline <- all_waves[ !(all_waves$HHIDPN %in% c(diabetes_wave_1_unique)), ]

write.csv(all_waves_no_diab_baseline, (paste(OUTPUT_ROOT, "all_waves_nodiabatbaseline_DIAB.csv", sep="")))

all_waves_no_diab_baseline
# number of rows are: 112895 
all_waves_unique_id = unique(all_waves$HHIDPN)

all_waves_unique_id_value = length(all_waves_unique_id) 

diabetes_all_waves = rbind(wave_2_diabetes, 
                           wave_3_diabetes, 
                           wave_4_diabetes, 
                           wave_5_diabetes, 
                           wave_6_diabetes)

diabetes_all_waves_unique = unique(diabetes_all_waves$HHIDPN)



new_diabetes_participant_wave_df = length(diabetes_all_waves_unique)


total_n_proportion = rbind( entire_dataset_Nvalue, 
                           all_waves_unique_id_value, 
                           new_diabetes_participant_wave_df)
######################
