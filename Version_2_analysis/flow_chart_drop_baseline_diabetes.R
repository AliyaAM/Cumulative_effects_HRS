flow_chart_drop_baseline_diabetes = function (data){
  
  
  ID = unique(data$HHIDPN)
  
  #print(isTRUE(data$HHIDPN == ID[1]))
  #data = data %>% drop_na()
  
  participant_wave_df = data.frame()
  
  n = 1
  for (id in ID){
    
    
    participant_wave = subset(data, data$HHIDPN == id)
    
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
  #wave_4_diabetes  = subset(wave_4, wave_4$diabetes_new == 1)
  
  
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
  
  #write.csv(new_diabetes_each_wave, (paste(OUTPUT_ROOT, "new_diabetes_each_wave_DIAB.csv", sep="")))
  
  all_waves = rbind(wave_1, 
                    wave_2, 
                    wave_3, 
                    wave_4, 
                    wave_5, 
                    wave_6)
  
  
  
  return(new_diabetes_each_wave)
  
}