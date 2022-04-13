

sort_timepoints_drop_baseline = function (data, 
                                          outcome){
  
  ID = unique(data$HHIDPN)
  
  #print(isTRUE(data$HHIDPN == ID[1]))
  #data = data %>% drop_na()
  
  participant_wave_df = data.frame()
  
  n = 1
  for (id in ID){
    
    
    participant_wave = subset(data, data$HHIDPN == id)
    
    #excluding those who had the outcome at their baseline: 
    #dataset_noNAs_timepoints = subset(dataset_noNAs_timepoints, dataset_noNAs_timepoints$start_new  != 0 & dataset_noNAs_timepoints$outcome != 1) 
    
    #nrow_dataset_noNAs_timepoints = nrow(dataset_noNAs_timepoints)
    
    #print("nrow_dataset_noNAs_timepoints: ")
    #print(nrow_dataset_noNAs_timepoints) 
    
    
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
      
      participant_wave = subset(participant_wave, participant_wave$start_new  != 0 & participant_wave$outcome != 1) 
      
      participant_wave_nrow = nrow(participant_wave)
      
      
      print("participant_wave_nrow: ")
      print(participant_wave_nrow) 
      
      
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    }
    
    if (nrow(participant_wave)==3){
      
      participant_wave$timepoints_indiv = 3
      
      participant_wave$start_new = c(0, 1, 2)
      participant_wave$stop_new = c(1, 2, 3)
      
      participant_wave = subset(participant_wave, participant_wave$start_new  != 0 & participant_wave$outcome != 1) 
      
      participant_wave_nrow = nrow(participant_wave)
      
      print("participant_wave_nrow: ")
      print(participant_wave_nrow) 
      
      
  
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    } 
    
    
    if (nrow(participant_wave)==4){
      
      participant_wave$timepoints_indiv = 4
      
      participant_wave$start_new = c(0, 1, 2, 3)
      participant_wave$stop_new = c(1, 2, 3, 4)
      
      participant_wave = subset(participant_wave, participant_wave$start_new  != 0 & participant_wave$outcome != 1) 
      
      participant_wave_nrow = nrow(participant_wave)
      
      print("participant_wave_nrow: ")
      print(participant_wave_nrow) 
      
      
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    } 
    
    if (nrow(participant_wave)==5){
      
      participant_wave$timepoints_indiv = 5
      
      participant_wave$start_new = c(0, 1, 2, 3, 4)
      participant_wave$stop_new = c(1, 2, 3, 4, 5)
      
      participant_wave = subset(participant_wave, participant_wave$start_new  != 0 & participant_wave$outcome != 1) 
      
      participant_wave_nrow = nrow(participant_wave)
      
      print("participant_wave_nrow: ")
      print(participant_wave_nrow) 
      
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    } 
    
    if (nrow(participant_wave)==6){
      
      participant_wave$timepoints_indiv = 6
      participant_wave$start_new = c(0, 1, 2, 3, 4, 5)
      participant_wave$stop_new = c(1, 2, 3, 4, 5, 6)
      
      participant_wave = subset(participant_wave, participant_wave$start_new  != 0 & participant_wave$outcome != 1) 
      
      participant_wave_nrow = nrow(participant_wave)
      
      print("participant_wave_nrow: ")
      print(participant_wave_nrow) 
      
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    }
    n = n + 1
  }
  return(participant_wave_df)
}