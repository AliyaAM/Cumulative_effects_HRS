


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



#subset everything to diabetes_ever == 0 

entire_dataset = rbind(HRS2008_data, 
                       HRS2010_data,
                       HRS2012_data,
                       HRS2014_data,
                       HRS2016_data,
                       HRS2018_data)



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



new_diabetes_each_wave = rbind(new_diabetes_wave1_dataset, 
                               new_diabetes_wave2_dataset,
                               new_diabetes_wave3_dataset,
                               new_diabetes_wave4_dataset,
                               new_diabetes_wave5_dataset,
                               new_diabetes_wave6_dataset)

write.csv(new_diabetes_each_wave, (paste(OUTPUT_ROOT, "new_diabetes_each_wave_DIAB.csv", sep="")))


diabetes_all_waves = rbind(wave_1_diabetes, 
                           wave_2_diabetes, 
                           wave_3_diabetes, 
                           wave_4_diabetes, 
                           wave_5_diabetes, 
                           wave_6_diabetes)

diabetes_all_waves_unique = unique(diabetes_all_waves$HHIDPN)



new_diabetes_participant_wave_df = length(diabetes_all_waves_unique)

total_n_proportion = data.frame()
total_n_proportion = rbind(total_n_proportion, 
                           new_diabetes_participant_wave_df)
######################

#female 
HRS2008_data_female = subset(HRS2008_data, HRS2008_data$sex_1_2==2)
HRS2010_data_female = subset(HRS2010_data, HRS2010_data$sex_1_2==2)
HRS2012_data_female = subset(HRS2012_data, HRS2012_data$sex_1_2==2)
HRS2014_data_female = subset(HRS2014_data, HRS2014_data$sex_1_2==2)
HRS2016_data_female = subset(HRS2016_data, HRS2016_data$sex_1_2==2)
HRS2018_data_female = subset(HRS2018_data, HRS2018_data$sex_1_2==2)


female_dataset = rbind(HRS2008_data_female, 
                       HRS2010_data_female,
                       HRS2012_data_female,
                       HRS2014_data_female,
                       HRS2016_data_female,
                       HRS2018_data_female)

wave_female_1 = subset(female_dataset,  female_dataset$start_new == 0)
wave_female_1_diabetes_female  = subset(wave_female_1, wave_female_1$diabetes_new == 1)

wave_female_2 = subset(female_dataset, female_dataset$start_new == 1)
wave_female_2_diabetes_female  = subset(wave_female_2, wave_female_2$diabetes_new == 1)

wave_female_3 = subset(female_dataset, female_dataset$start_new == 2)
wave_female_3_diabetes_female  = subset(wave_female_3, wave_female_3$diabetes_new == 1)

wave_female_4 = subset(female_dataset, female_dataset$start_new == 3)
wave_female_4_diabetes_female  = subset(wave_female_4, wave_female_4$diabetes_new == 1)

wave_female_5 = subset(female_dataset, female_dataset$start_new == 4)
wave_female_5_diabetes_female  = subset(wave_female_5, wave_female_5$diabetes_new == 1)

wave_female_6 = subset(female_dataset, female_dataset$start_new == 5)
wave_female_6_diabetes_female  = subset(wave_female_6, wave_female_6$diabetes_new == 1)


diabetes_female_all_wave_females = rbind(wave_female_1_diabetes_female, 
                                         wave_female_2_diabetes_female, 
                                         wave_female_3_diabetes_female, 
                                         wave_female_4_diabetes_female, 
                                         wave_female_5_diabetes_female, 
                                         wave_female_6_diabetes_female)

diabetes_female_all_wave_females_unique = unique(diabetes_female_all_wave_females$HHIDPN)



female_dataset_n = unique(female_dataset$HHIDPN) 
female_dataset_Nvalue = length(female_dataset_n)

total_n_proportion = rbind(total_n_proportion, 
                           female_dataset_Nvalue)

new_diabetes_female_female_dataset = length(diabetes_female_all_wave_females_unique)

total_n_proportion = rbind(total_n_proportion, 
                           new_diabetes_female_female_dataset)



######################
HRS2008_data$race_white == 0
#race 
HRS2008_data_race = subset(HRS2008_data, HRS2008_data$race_white==0)
HRS2010_data_race = subset(HRS2010_data, HRS2010_data$race_white==0)
HRS2012_data_race = subset(HRS2012_data, HRS2012_data$race_white==0)
HRS2014_data_race = subset(HRS2014_data, HRS2014_data$race_white==0)
HRS2016_data_race = subset(HRS2016_data, HRS2016_data$race_white==0)
HRS2018_data_race = subset(HRS2018_data, HRS2018_data$race_white==0)


race_dataset = rbind(HRS2008_data_race, 
                     HRS2010_data_race,
                     HRS2012_data_race,
                     HRS2014_data_race,
                     HRS2016_data_race,
                     HRS2018_data_race)

wave_race_1 = subset(race_dataset,  race_dataset$start_new == 0)
wave_race_1_diabetes_race  = subset(wave_race_1, wave_race_1$diabetes_new == 1)

wave_race_2 = subset(race_dataset, race_dataset$start_new == 1)
wave_race_2_diabetes_race  = subset(wave_race_2, wave_race_2$diabetes_new == 1)

wave_race_3 = subset(race_dataset, race_dataset$start_new == 2)
wave_race_3_diabetes_race  = subset(wave_race_3, wave_race_3$diabetes_new == 1)

wave_race_4 = subset(race_dataset, race_dataset$start_new == 3)
wave_race_4_diabetes_race  = subset(wave_race_4, wave_race_4$diabetes_new == 1)

wave_race_5 = subset(race_dataset, race_dataset$start_new == 4)
wave_race_5_diabetes_race  = subset(wave_race_5, wave_race_5$diabetes_new == 1)

wave_race_6 = subset(race_dataset, race_dataset$start_new == 5)
wave_race_6_diabetes_race  = subset(wave_race_6, wave_race_6$diabetes_new == 1)


diabetes_race_all_wave_races = rbind(wave_race_1_diabetes_race, 
                                     wave_race_2_diabetes_race, 
                                     wave_race_3_diabetes_race, 
                                     wave_race_4_diabetes_race, 
                                     wave_race_5_diabetes_race, 
                                     wave_race_6_diabetes_race)

diabetes_race_all_wave_races_unique = unique(diabetes_race_all_wave_races$HHIDPN)


#####
#####

race_dataset_n = unique(race_dataset$HHIDPN) 
race_dataset_Nvalue = length(race_dataset_n)


total_n_proportion = rbind(total_n_proportion, 
                           race_dataset_Nvalue)


new_diabetes_race_race_dataset = length(diabetes_race_all_wave_races_unique)


total_n_proportion = rbind(total_n_proportion, 
                           new_diabetes_race_race_dataset)
########################################################
########################################################



#race_combo 
HRS2008_data_race_combo = subset(HRS2008_data, HRS2008_data$race_white==0 | HRS2008_data$national_origin_ousideUS_bin == 1 | HRS2008_data$religion_bin == 1) 
HRS2010_data_race_combo = subset(HRS2010_data, HRS2010_data$race_white==0 | HRS2010_data$national_origin_ousideUS_bin == 1 | HRS2010_data$religion_bin == 1) 
HRS2012_data_race_combo = subset(HRS2012_data, HRS2012_data$race_white==0 | HRS2012_data$national_origin_ousideUS_bin == 1 | HRS2012_data$religion_bin == 1) 
HRS2014_data_race_combo = subset(HRS2014_data, HRS2014_data$race_white==0 | HRS2014_data$national_origin_ousideUS_bin == 1 | HRS2014_data$religion_bin == 1) 
HRS2016_data_race_combo = subset(HRS2016_data, HRS2016_data$race_white==0 | HRS2016_data$national_origin_ousideUS_bin == 1 | HRS2016_data$religion_bin == 1) 
HRS2018_data_race_combo = subset(HRS2018_data, HRS2018_data$race_white==0 | HRS2018_data$national_origin_ousideUS_bin == 1 | HRS2018_data$religion_bin == 1) 


race_combo_dataset = rbind(HRS2008_data_race_combo, 
                           HRS2010_data_race_combo,
                           HRS2012_data_race_combo,
                           HRS2014_data_race_combo,
                           HRS2016_data_race_combo,
                           HRS2018_data_race_combo)

wave_race_combo_1 = subset(race_combo_dataset,  race_combo_dataset$start_new == 0)
wave_race_combo_1_diabetes_race_combo  = subset(wave_race_combo_1, wave_race_combo_1$diabetes_new == 1)

wave_race_combo_2 = subset(race_combo_dataset, race_combo_dataset$start_new == 1)
wave_race_combo_2_diabetes_race_combo  = subset(wave_race_combo_2, wave_race_combo_2$diabetes_new == 1)

wave_race_combo_3 = subset(race_combo_dataset, race_combo_dataset$start_new == 2)
wave_race_combo_3_diabetes_race_combo  = subset(wave_race_combo_3, wave_race_combo_3$diabetes_new == 1)

wave_race_combo_4 = subset(race_combo_dataset, race_combo_dataset$start_new == 3)
wave_race_combo_4_diabetes_race_combo  = subset(wave_race_combo_4, wave_race_combo_4$diabetes_new == 1)

wave_race_combo_5 = subset(race_combo_dataset, race_combo_dataset$start_new == 4)
wave_race_combo_5_diabetes_race_combo  = subset(wave_race_combo_5, wave_race_combo_5$diabetes_new == 1)

wave_race_combo_6 = subset(race_combo_dataset, race_combo_dataset$start_new == 5)
wave_race_combo_6_diabetes_race_combo  = subset(wave_race_combo_6, wave_race_combo_6$diabetes_new == 1)


diabetes_race_combo_all_wave_race_combos = rbind(wave_race_combo_1_diabetes_race_combo, 
                                                 wave_race_combo_2_diabetes_race_combo, 
                                                 wave_race_combo_3_diabetes_race_combo, 
                                                 wave_race_combo_4_diabetes_race_combo, 
                                                 wave_race_combo_5_diabetes_race_combo, 
                                                 wave_race_combo_6_diabetes_race_combo)

diabetes_race_combo_all_wave_race_combos_unique = unique(diabetes_race_combo_all_wave_race_combos$HHIDPN)


#####
#####
race_combo_dataset_n = unique(race_combo_dataset$HHIDPN) 
race_combo_dataset_Nvalue = length(race_combo_dataset_n)

new_diabetes_race_combo_race_combo_dataset = length(diabetes_race_combo_all_wave_race_combos_unique)



total_n_proportion = rbind(total_n_proportion, 
                           race_combo_dataset_n, 
                           new_diabetes_race_combo_race_combo_dataset)
########################################################

########################################################
########################################################



#BMI 
HRS2008_data_BMI = subset(HRS2008_data, HRS2008_data$assessed_BMI>30)  
HRS2010_data_BMI = subset(HRS2010_data, HRS2010_data$assessed_BMI>30)   
HRS2012_data_BMI = subset(HRS2012_data, HRS2012_data$assessed_BMI>30)   
HRS2014_data_BMI = subset(HRS2014_data, HRS2014_data$assessed_BMI>30)   
HRS2016_data_BMI = subset(HRS2016_data, HRS2016_data$assessed_BMI>30)   
HRS2018_data_BMI = subset(HRS2018_data, HRS2018_data$assessed_BMI>30)   


BMI_dataset = rbind(HRS2008_data_BMI, 
                    HRS2010_data_BMI,
                    HRS2012_data_BMI,
                    HRS2014_data_BMI,
                    HRS2016_data_BMI,
                    HRS2018_data_BMI)

wave_BMI_1 = subset(BMI_dataset,  BMI_dataset$start_new == 0)
wave_BMI_1_diabetes_BMI  = subset(wave_BMI_1, wave_BMI_1$diabetes_new == 1)

wave_BMI_2 = subset(BMI_dataset, BMI_dataset$start_new == 1)
wave_BMI_2_diabetes_BMI  = subset(wave_BMI_2, wave_BMI_2$diabetes_new == 1)

wave_BMI_3 = subset(BMI_dataset, BMI_dataset$start_new == 2)
wave_BMI_3_diabetes_BMI  = subset(wave_BMI_3, wave_BMI_3$diabetes_new == 1)

wave_BMI_4 = subset(BMI_dataset, BMI_dataset$start_new == 3)
wave_BMI_4_diabetes_BMI  = subset(wave_BMI_4, wave_BMI_4$diabetes_new == 1)

wave_BMI_5 = subset(BMI_dataset, BMI_dataset$start_new == 4)
wave_BMI_5_diabetes_BMI  = subset(wave_BMI_5, wave_BMI_5$diabetes_new == 1)

wave_BMI_6 = subset(BMI_dataset, BMI_dataset$start_new == 5)
wave_BMI_6_diabetes_BMI  = subset(wave_BMI_6, wave_BMI_6$diabetes_new == 1)


diabetes_BMI_all_wave_BMIs = rbind(wave_BMI_1_diabetes_BMI, 
                                   wave_BMI_2_diabetes_BMI, 
                                   wave_BMI_3_diabetes_BMI, 
                                   wave_BMI_4_diabetes_BMI, 
                                   wave_BMI_5_diabetes_BMI, 
                                   wave_BMI_6_diabetes_BMI)

diabetes_BMI_all_wave_BMIs_unique = unique(diabetes_BMI_all_wave_BMIs$HHIDPN)


#####
#####
BMI_dataset_n = unique(BMI_dataset$HHIDPN) 
BMI_dataset_Nvalue = length(BMI_dataset_n)

new_diabetes_BMI_BMI_dataset = length(diabetes_BMI_all_wave_BMIs_unique)



total_N = rbind(participant_wave_df_Nvalue,
                
                female_dataset_Nvalue, 
                
                race_dataset_Nvalue, 
                
                race_combo_dataset_Nvalue, 
                
                BMI_dataset_Nvalue)



diabetes_cases = rbind(new_diabetes_participant_wave_df, 
                       new_diabetes_female_female_dataset, 
                       new_diabetes_race_race_dataset, 
                       new_diabetes_race_combo_race_combo_dataset,
                       new_diabetes_BMI_BMI_dataset) 

percentage_diabetes = (diabetes_cases/total_N) * 100

numbers = cbind(total_N, 
                diabetes_cases, 
                percentage_diabetes)

numbers = as.data.frame(numbers)
colnames(numbers) = c("total N", "new diabetes cases", "%")

write.csv(numbers, (paste(OUTPUT_ROOT, "diabetes_new_cases_DIAB_in_each_subset.csv", sep="")))

########################################################
########################################################

