

sort_timepoints_drop_baseline = function (data){
  print("Runnning sort_timepoints_drop_baseline.")
  
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
  print("Finished loop.")
  
  
  wave_1 = subset(participant_wave_df,  participant_wave_df$start_new == 0)
  wave_1_heartattack  = subset(wave_1, wave_1$heartattack_new_bin == 1)
  
  wave_1_heartattack_ever  = subset(wave_1, wave_1$heartattack_ever == 1)
  
  heartattack_wave_1_unique = unique(wave_1_heartattack$HHIDPN)
  
  heartattack_ever_wave_1_unique = unique(wave_1_heartattack_ever$HHIDPN)
  
  #new_heartattack_wave1_dataset = length(heartattack_wave_1_unique)
  #wave 1: 224 cases 
  
  
  wave_2 = subset(participant_wave_df, participant_wave_df$start_new == 1)
  #wave_2_heartattack  = subset(wave_2, wave_2$heartattack_new == 1)
  
  #heartattack_wave_2_unique = unique(wave_2_heartattack$HHIDPN)
  #new_heartattack_wave2_dataset = length(heartattack_wave_2_unique)
  #wave 2: 318 cases 
  
  
  wave_3 = subset(participant_wave_df, participant_wave_df$start_new == 2)
  #wave_3_heartattack  = subset(wave_3, wave_3$heartattack_new == 1)
  
  
  #heartattack_wave_3_unique = unique(wave_3_heartattack$HHIDPN)
  #new_heartattack_wave3_dataset = length(heartattack_wave_3_unique)
  #wave 3: 233 cases 
  
  
  wave_4 = subset(participant_wave_df, participant_wave_df$start_new == 3)
  #wave_4_heartattack  = subset(wave_4, wave_4$heartattack_new == 1)
  
  
  #heartattack_wave_4_unique = unique(wave_4_heartattack$HHIDPN)
  #new_heartattack_wave4_dataset = length(heartattack_wave_4_unique)
  #wave 4: 174 cases 
  
  
  wave_5 = subset(participant_wave_df, participant_wave_df$start_new == 4)
  #wave_5_heartattack  = subset(wave_5, wave_5$heartattack_new == 1)
  
  
  #heartattack_wave_5_unique = unique(wave_5_heartattack$HHIDPN)
  #new_heartattack_wave5_dataset = length(heartattack_wave_5_unique)
  #
  
  wave_6 = subset(participant_wave_df, participant_wave_df$start_new == 5)
  #wave_6_heartattack  = subset(wave_6, wave_6$heartattack_new == 1)
  
  
  #heartattack_wave_6_unique = unique(wave_6_heartattack$HHIDPN)
  #new_heartattack_wave6_dataset = length(heartattack_wave_6_unique)
  
  
  
  #new_heartattack_each_wave = rbind(new_heartattack_wave2_dataset,
  # new_heartattack_wave3_dataset,
  # new_heartattack_wave4_dataset,
  # new_heartattack_wave5_dataset,
  # new_heartattack_wave6_dataset)
  
  #write.csv(new_heartattack_each_wave, (paste(OUTPUT_ROOT, "new_heartattack_each_wave_MI.csv", sep="")))
  
  print("About to rbind all waves together.")
  all_waves = rbind(wave_1, 
                    wave_2, 
                    wave_3, 
                    wave_4, 
                    wave_5, 
                    wave_6)
  print("Finished making all_waves.")
  
  
  all_waves_no_MI_baseline <- all_waves[ !(all_waves$HHIDPN %in% c(heartattack_wave_1_unique)), ]
  #all_waves_no_MI_ever <- all_waves_no_MI_baseline[ !(all_waves_no_MI_baseline$HHIDPN %in% c(heartattack_ever_wave_1_unique)), ]
  
  
  #write.csv(all_waves_no_MI_baseline, (paste(OUTPUT_ROOT, "all_waves_noMIatbaseline_MI.csv", sep="")))
  
  #all_waves_no_MI_baseline_unique = unique(all_waves_no_MI_baseline$HHIDPN)
  #all_waves_no_MI_baseline_unique_values = length(all_waves_no_MI_baseline_unique)
  
  # number of rows are: 112895 
  #all_waves_unique_id = unique(all_waves$HHIDPN)
  
  # all_waves_unique_id_value = length(all_waves_unique_id) 
  
  #heartattack_all_waves = rbind(wave_2_heartattack, 
  #wave_3_heartattack, 
  #wave_4_heartattack, 
  #wave_5_heartattack, 
  #wave_6_heartattack)
  
  #heartattack_all_waves_unique = unique(heartattack_all_waves$HHIDPN)
  #new_heartattack_participant_wave_df = length(heartattack_all_waves_unique)
  
  
  #total_n_proportion = rbind(all_waves_unique_id_value, 
  #all_waves_no_MI_baseline_unique_values, 
  #new_heartattack_participant_wave_df)
  
  #write.csv(total_n_proportion, (paste(OUTPUT_ROOT, "total_n_noMIatbaseline_MI_values.csv", sep="")))
  
  
  return(all_waves_no_MI_baseline)
  
}