

sort_timepoints = function (data){
  
ID = unique(data$HHIDPN)

#print(isTRUE(data$HHIDPN == ID[1]))
#data = data %>% drop_na()

participant_wave_df = data.frame()

n = 1
for (id in ID){

  print(n)
  print(id)
  
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
return(participant_wave_df)
}