
current_directory = "/Users/aliyaamirova/proj/Cumulative_effects_HRS"

SOURCE_ROOT = (paste(current_directory, "/Version_2_analysis/", sep=""))
DATAIN_ROOT = (paste(current_directory, "/data_files/OLD_data_diabetes_this_wave/", sep="")) 

OUTPUT_ROOT =(paste(current_directory, "/data_files/Results/Hypertension_new_bin/", sep=""))


source((paste(SOURCE_ROOT, "clean_recode_keyvars.R", sep="")))



#data 
HRS2008_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2008_data_short_OLD.csv", sep=""))
HRS2010_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2010_data_short_OLD.csv", sep=""))
HRS2012_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2012_data_short_OLD.csv", sep=""))
HRS2014_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2014_data_short_OLD.csv", sep=""))
HRS2016_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2016_data_short_OLD.csv", sep=""))
HRS2018_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2018_data_short_OLD.csv", sep=""))

########

#data 

HRS2008_data_intermediate = HRS2008_data_initial
HRS2010_data_intermediate = HRS2010_data_initial
HRS2012_data_intermediate = HRS2012_data_initial
HRS2014_data_intermediate = HRS2014_data_initial
HRS2016_data_intermediate = HRS2016_data_initial
HRS2018_data_intermediate = HRS2018_data_initial



HRS2008_data = HRS2008_data_intermediate
HRS2010_data = HRS2010_data_intermediate
HRS2012_data = HRS2012_data_intermediate
HRS2014_data = HRS2014_data_intermediate
HRS2016_data = HRS2016_data_intermediate
HRS2018_data = HRS2018_data_intermediate


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


#### add discrimination here for each year separately 

dataset = rbind(HRS2008_data = HRS2008_data,
                HRS2010_data = HRS2010_data, 
                HRS2012_data = HRS2012_data,
                HRS2014_data = HRS2014_data, 
                HRS2016_data = HRS2016_data,
                HRS2018_data = HRS2018_data) 



###### drop NAs and weird strings like " NA", THE WCE ANALYSIS DOES NOT RUN WITH NAs

data  = dataset 
###### turn to numeric vectors, coercing NAs 

data$discrim_harassed = as.numeric(data$discrim_harassed)
data$discrim_lessrespect = as.numeric(data$discrim_lessrespect)
data$discrim_medical = as.numeric(data$discrim_medical)
data$discrim_notclever = as.numeric(data$discrim_notclever)
data$discrim_poorerservice = as.numeric(data$discrim_poorerservice)
data$discrim_afraidothers = as.numeric(data$discrim_afraidothers)


data$heartcondition_ever_bin = as.numeric(data$heartcondition_ever_bin)
data$heartcondition_new_bin = as.numeric(data$heartcondition_new_bin)
data$angina_new_bin = as.numeric(data$angina_new_bin)
data$stroke_new_bin = as.numeric(data$stroke_new_bin)
data$heartfailure2yrs_bin = as.numeric(data$heartfailure2yrs_bin)
data$heartattack_ever_bin = as.numeric(data$heartattack_ever_bin)
data$heartattack_new_bin = as.numeric(data$heartattack_new_bin)

data$vigarious_physical_activity = as.numeric(data$vigarious_physical_activity)


data$alcohol_days_week = as.numeric(data$alcohol_days_week)
data$smokes_now_bin = as.numeric(data$smokes_now_bin)
data$checklist_depression_bin = as.numeric(data$checklist_depression_bin)
data$wealth_noIRA = as.numeric(data$wealth_noIRA)


data$diabetes_new = as.numeric(data$diabetes_new)
data$diabetes_new_bin = as.numeric(data$diabetes_new_bin)



##### recode below: 

#physical activity (original): 1.every day; 2.>1 per week; 3.1 per week; 4.l-3 per mon; 5.never; .d=DK/NA; .r=RF


data$vigarious_physical_activity_new = case_when(data$vigarious_physical_activity == 1 ~ 5, 
                                                 data$vigarious_physical_activity == 2 ~ 4, 
                                                 data$vigarious_physical_activity == 3 ~ 3, 
                                                 data$vigarious_physical_activity == 4 ~ 2, 
                                                 data$vigarious_physical_activity == 5 ~ 1) 




data$vigarious_physical_activity_bin = case_when(data$vigarious_physical_activity_new == 5 ~ 1, 
                                                 data$vigarious_physical_activity_new == 4 ~ 1, 
                                                 data$vigarious_physical_activity_new == 3 ~ 1, 
                                                 data$vigarious_physical_activity_new == 2 ~ 0, 
                                                 data$vigarious_physical_activity_new == 1 ~ 0) 





data$alcohol_days_week_new =  na_if(data$alcohol_days_week, 8)
data$alcohol_days_week_new = na_if(data$alcohol_days_week_new, 9) 




###### recode into single var  discrim_bin


data$discrim_harassed_bin = case_when(data$discrim_harassed == 1 ~ 1, 
                                      data$discrim_harassed == 2 ~ 1, 
                                      data$discrim_harassed == 3 ~ 1, 
                                      data$discrim_harassed == 4 ~ 1, 
                                      data$discrim_harassed == 5 ~ 0, 
                                      data$discrim_harassed == 6 ~ 0,
                                      data$discrim_harassed == 0 ~ 0) 



data$discrim_lessrespect_bin = case_when(data$discrim_lessrespect == 1 ~ 1, 
                                         data$discrim_lessrespect == 2 ~ 1, 
                                         data$discrim_lessrespect == 3 ~ 1, 
                                         data$discrim_lessrespect == 4 ~ 1, 
                                         data$discrim_lessrespect == 5 ~ 0, 
                                         data$discrim_lessrespect == 6 ~ 0,
                                         data$discrim_lessrespect == 0 ~ 0) 



data$discrim_medical_bin = case_when(data$discrim_medical == 1 ~ 1, 
                                     data$discrim_medical == 2 ~ 1, 
                                     data$discrim_medical == 3 ~ 1, 
                                     data$discrim_medical == 4 ~ 1, 
                                     data$discrim_medical == 5 ~ 0, 
                                     data$discrim_medical == 6 ~ 0,
                                     data$discrim_medical == 0 ~ 0) 





data$discrim_notclever_bin = case_when(data$discrim_notclever == 1 ~ 1, 
                                       data$discrim_notclever == 2 ~ 1, 
                                       data$discrim_notclever == 3 ~ 1, 
                                       data$discrim_notclever == 4 ~ 1, 
                                       data$discrim_notclever == 5 ~ 0, 
                                       data$discrim_notclever == 6 ~ 0,
                                       data$discrim_notclever == 0 ~ 0) 






data$discrim_poorerservice_bin = case_when(data$discrim_poorerservice == 1 ~ 1, 
                                           data$discrim_poorerservice == 2 ~ 1, 
                                           data$discrim_poorerservice == 3 ~ 1, 
                                           data$discrim_poorerservice == 4 ~ 1, 
                                           data$discrim_poorerservice == 5 ~ 0, 
                                           data$discrim_poorerservice == 6 ~ 0) 




data$discrim_afraidothers_bin = case_when(data$discrim_afraidothers == 1 ~ 1,
                                          data$discrim_afraidothers == 2 ~ 1, 
                                          data$discrim_afraidothers == 3 ~ 1, 
                                          data$discrim_afraidothers == 4 ~ 1, 
                                          data$discrim_afraidothers == 5 ~ 0, 
                                          data$discrim_afraidothers == 6 ~ 0,
                                          data$discrim_afraidothers == 0 ~ 0) 




data$discrim_bin = case_when(data$discrim_harassed_bin == 1 | data$discrim_lessrespect_bin == 1 | data$discrim_medical_bin  == 1 | data$discrim_notclever_bin == 1 | data$discrim_afraidothers_bin == 1 | data$discrim_poorerservice_bin == 1 ~ 1, 
                             data$discrim_harassed_bin == 0 & data$discrim_lessrespect_bin == 0 & data$discrim_medical_bin  == 0 & data$discrim_notclever_bin == 0 & data$discrim_afraidothers_bin == 0 & data$discrim_poorerservice_bin == 0 ~ 0) 


data$start
data$stop


data$discrim_bin
ID = unique(data$HHIDPN)

data$discrim_bin[3]
head(data)
#print(isTRUE(data$HHIDPN == ID[1]))
#data = data %>% drop_na()

participant_wave_df = data.frame()



n = 1
for (id in ID){
  
  
  participant_wave = subset(data, data$HHIDPN == id)
  
  if (nrow(participant_wave) == 1 ){
    
    participant_wave$timepoints_indiv = 1
    
    
    participant_wave$start_new = c(0)
    participant_wave$stop_new = c(1)
    
    participant_wave$discrimination_cat = participant_wave$discrim_bin
    participant_wave_df = rbind(participant_wave_df, participant_wave) 
    

  }
  

  if (nrow(participant_wave) ==2){
    
    participant_wave$timepoints_indiv = 2
    
    participant_wave$start_new = c(0, 1)
    participant_wave$stop_new = c(1, 2)
    
    if (is.na(participant_wave$discrim_bin[1]) & !is.na(participant_wave$discrim_bin[2])){
      
      participant_wave$discrimination_cat = c(participant_wave$discrim_bin[2], participant_wave$discrim_bin[2])
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    }
    
    
    if (is.na(participant_wave$discrim_bin[1]) & is.na(participant_wave$discrim_bin[2])){
      
      participant_wave$discrimination_cat = c(NA, NA)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    }
    
    
    if (!is.na(participant_wave$discrim_bin[1]) & !is.na(participant_wave$discrim_bin[2])){
      
      discrimination_cat_value = participant_wave$discrim_bin[1] + participant_wave$discrim_bin[2]
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    }
    

  
    }
       
       
  if (nrow(participant_wave)==3){
    
    participant_wave$timepoints_indiv = 3
    
    participant_wave$start_new = c(0, 1, 2)
    participant_wave$stop_new = c(1, 2, 3)
    
    if (is.na(participant_wave$discrim_bin[1]) & !is.na(participant_wave$discrim_bin[2]) & !is.na(participant_wave$discrim_bin[3])){
      
      discrimination_cat_value = participant_wave$discrim_bin[2] + participant_wave$discrim_bin[3]
      
      participant_wave$discrimination_cat =  c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    }
    
    if (!is.na(participant_wave$discrim_bin[1]) & is.na(participant_wave$discrim_bin[2]) & !is.na(participant_wave$discrim_bin[3])){
      
      discrimination_cat_value = participant_wave$discrim_bin[1] + participant_wave$discrim_bin[3]
      
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      

    }
    
    if (!is.na(participant_wave$discrim_bin[1]) & !is.na(participant_wave$discrim_bin[2]) & is.na(participant_wave$discrim_bin[3])){
      
      discrimination_cat_value = participant_wave$discrim_bin[1] + participant_wave$discrim_bin[2]
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      

    }
    
    if (is.na(participant_wave$discrim_bin[1]) & is.na(participant_wave$discrim_bin[2]) & !is.na(participant_wave$discrim_bin[3])){
      
      discrimination_cat_value =  participant_wave$discrim_bin[3]
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      

    }
    
    if (is.na(participant_wave$discrim_bin[1]) & is.na(participant_wave$discrim_bin[2]) & is.na(participant_wave$discrim_bin[3])){
      
      discrimination_cat_value = NA
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      

    }
    
    
    if (!is.na(participant_wave$discrim_bin[1]) & !is.na(participant_wave$discrim_bin[2]) & !is.na(participant_wave$discrim_bin[3])){
      
      discrimination_cat_value = participant_wave$discrim_bin[1] + participant_wave$discrim_bin[2] + participant_wave$discrim_bin[3] 
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
      
    }    



  } 
  
  
  if (nrow(participant_wave)==4){
    
    participant_wave$timepoints_indiv = 4
    
    participant_wave$start_new = c(0, 1, 2, 3)
    participant_wave$stop_new = c(1, 2, 3, 4)
    
    if (is.na(participant_wave$discrim_bin[1]) & !is.na(participant_wave$discrim_bin[2]) & !is.na(participant_wave$discrim_bin[3]) & !is.na(participant_wave$discrim_bin[4])){
      
      discrimination_cat_value = participant_wave$discrim_bin[2] + participant_wave$discrim_bin[3] +  participant_wave$discrim_bin[4] 
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      

    }
    
    if (!is.na(participant_wave$discrim_bin[1]) & is.na(participant_wave$discrim_bin[2]) & !is.na(participant_wave$discrim_bin[3]) & !is.na(participant_wave$discrim_bin[4])){
      
      discrimination_cat_value = participant_wave$discrim_bin[1] + participant_wave$discrim_bin[3] +  participant_wave$discrim_bin[4] 
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      

    }
    
    if (!is.na(participant_wave$discrim_bin[1]) & !is.na(participant_wave$discrim_bin[2]) & is.na(participant_wave$discrim_bin[3])  & is.na(participant_wave$discrim_bin[4])){
      
      discrimination_cat_value = participant_wave$discrim_bin[1] + participant_wave$discrim_bin[2]
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      

    }
    
    if (!is.na(participant_wave$discrim_bin[1]) & !is.na(participant_wave$discrim_bin[2]) & is.na(participant_wave$discrim_bin[3])  & !is.na(participant_wave$discrim_bin[4])){
      
      discrimination_cat_value = participant_wave$discrim_bin[1] + participant_wave$discrim_bin[2] + participant_wave$discrim_bin[4]
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      

    }
    
    
    if (is.na(participant_wave$discrim_bin[1]) & is.na(participant_wave$discrim_bin[2]) & !is.na(participant_wave$discrim_bin[3]) & is.na(participant_wave$discrim_bin[4])){
      
      discrimination_cat_value =  participant_wave$discrim_bin[3]
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      

    }
    
    if (!is.na(participant_wave$discrim_bin[1]) & !is.na(participant_wave$discrim_bin[2]) & !is.na(participant_wave$discrim_bin[3]) & !is.na(participant_wave$discrim_bin[4])){
      
      discrimination_cat_value = NA
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
      

    }
    
    
    if (!is.na(participant_wave$discrim_bin[1]) & !is.na(participant_wave$discrim_bin[2]) & !is.na(participant_wave$discrim_bin[3]) & !is.na(participant_wave$discrim_bin[4])){
      
      discrimination_cat_value = participant_wave$discrim_bin[1] + participant_wave$discrim_bin[2] + participant_wave$discrim_bin[3] + participant_wave$discrim_bin[4]
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      

      
    }    


    
  } 
  
  if (nrow(participant_wave)==5){
    
    participant_wave$timepoints_indiv = 5
    
    participant_wave$start_new = c(0, 1, 2, 3, 4)
    participant_wave$stop_new = c(1, 2, 3, 4, 5)
    if (is.na(participant_wave$discrim_bin[1]) & !is.na(participant_wave$discrim_bin[2]) & !is.na(participant_wave$discrim_bin[3]) & !is.na(participant_wave$discrim_bin[4])){
      
      discrimination_cat_value = participant_wave$discrim_bin[2] + participant_wave$discrim_bin[3] +  participant_wave$discrim_bin[4] 
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      

    }
    
    if (!is.na(participant_wave$discrim_bin[1]) & is.na(participant_wave$discrim_bin[2]) & !is.na(participant_wave$discrim_bin[3]) & !is.na(participant_wave$discrim_bin[4])){
      
      discrimination_cat_value = participant_wave$discrim_bin[1] + participant_wave$discrim_bin[3] +  participant_wave$discrim_bin[4] 
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      

    }
    
    if (!is.na(participant_wave$discrim_bin[1]) & !is.na(participant_wave$discrim_bin[2]) & is.na(participant_wave$discrim_bin[3])  & is.na(participant_wave$discrim_bin[4])){
      
      discrimination_cat_value = participant_wave$discrim_bin[1] + participant_wave$discrim_bin[2]
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      

    }
    
    if (!is.na(participant_wave$discrim_bin[1])  & !is.na(participant_wave$discrim_bin[2]) & is.na(participant_wave$discrim_bin[3])  & !is.na(participant_wave$discrim_bin[4])){
      
      discrimination_cat_value = participant_wave$discrim_bin[1] + participant_wave$discrim_bin[2] + participant_wave$discrim_bin[4]
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      

    }
    
    
    if (is.na(participant_wave$discrim_bin[1]) & is.na(participant_wave$discrim_bin[2]) & !is.na(participant_wave$discrim_bin[3])  & is.na(participant_wave$discrim_bin[4])){
      
      discrimination_cat_value =  participant_wave$discrim_bin[3]
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      

      
    }
    
    if (is.na(participant_wave$discrim_bin[1]) & is.na(participant_wave$discrim_bin[2]) & is.na(participant_wave$discrim_bin[3]) & is.na(participant_wave$discrim_bin[4])){
      
      discrimination_cat_value = NA
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      

      
    }
    
    
    if (!is.na(participant_wave$discrim_bin[1]) & !is.na(participant_wave$discrim_bin[2]) & !is.na(participant_wave$discrim_bin[3]) & !is.na(participant_wave$discrim_bin[4])){
      
      discrimination_cat_value = participant_wave$discrim_bin[1] + participant_wave$discrim_bin[2] + participant_wave$discrim_bin[3] + participant_wave$discrim_bin[4]
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
      
    }    

  }
  
  if (nrow(participant_wave)==6){
    
    participant_wave$timepoints_indiv = 6
    participant_wave$start_new = c(0, 1, 2, 3, 4, 5)
    participant_wave$stop_new = c(1, 2, 3, 4, 5, 6)

    if (is.na(participant_wave$discrim_bin[1]) & !is.na(participant_wave$discrim_bin[2]) & !is.na(participant_wave$discrim_bin[3]) & !is.na(participant_wave$discrim_bin[4])){
      
      discrimination_cat_value = participant_wave$discrim_bin[2] + participant_wave$discrim_bin[3] +  participant_wave$discrim_bin[4] 
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    }
    
    if (!is.na(participant_wave$discrim_bin[1]) & is.na(participant_wave$discrim_bin[2]) & !is.na(participant_wave$discrim_bin[3]) & !is.na(participant_wave$discrim_bin[4])){
      
      discrimination_cat_value = participant_wave$discrim_bin[1] + participant_wave$discrim_bin[3] +  participant_wave$discrim_bin[4] 
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    }
    
    if (!is.na(participant_wave$discrim_bin[1]) & !is.na(participant_wave$discrim_bin[2]) & is.na(participant_wave$discrim_bin[3])  & is.na(participant_wave$discrim_bin[4])){
      
      discrimination_cat_value = participant_wave$discrim_bin[1] + participant_wave$discrim_bin[2]
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    }
    
    if (!is.na(participant_wave$discrim_bin[1]) & !is.na(participant_wave$discrim_bin[2]) & is.na(participant_wave$discrim_bin[3])  & !is.na(participant_wave$discrim_bin[4])){
      
      discrimination_cat_value = participant_wave$discrim_bin[1] + participant_wave$discrim_bin[2] + participant_wave$discrim_bin[4]
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    }
    
    
    if (is.na(participant_wave$discrim_bin[1]) & is.na(participant_wave$discrim_bin[2]) & !is.na(participant_wave$discrim_bin[3])  & is.na(participant_wave$discrim_bin[4])){
      
      discrimination_cat_value =  participant_wave$discrim_bin[3]
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    }
    
    if (is.na(participant_wave$discrim_bin[1]) & is.na(participant_wave$discrim_bin[2]) & is.na(participant_wave$discrim_bin[3]) & is.na(participant_wave$discrim_bin[4])){
      
      discrimination_cat_value = NA
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    }
    
    
    if (!is.na(participant_wave$discrim_bin[1]) & !is.na(participant_wave$discrim_bin[2]) & !is.na(participant_wave$discrim_bin[3]) & !is.na(participant_wave$discrim_bin[4])){
      
      discrimination_cat_value = participant_wave$discrim_bin[1] + participant_wave$discrim_bin[2] + participant_wave$discrim_bin[3] + participant_wave$discrim_bin[4]
      participant_wave$discrimination_cat = c(discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value, discrimination_cat_value)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
      
    }    

    participant_wave_df
  }


  n = n + 1
}





unique(participant_wave_df$discrimination_cat)

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

#write.csv(new_diabetes_each_wave, (paste(OUTPUT_ROOT, "new_diabetes_each_wave_DIAB.csv", sep="")))

all_waves = rbind(wave_1, 
                  wave_2, 
                  wave_3, 
                  wave_4, 
                  wave_5, 
                  wave_6)


all_waves_no_diab_baseline <- all_waves[ !(all_waves$HHIDPN %in% c(diabetes_wave_1_unique)), ]

write.csv(all_waves_no_diab_baseline, (paste(OUTPUT_ROOT, "all_waves_nodiabatbaseline_DIAB_discrim_recoded.csv", sep="")))

all_waves_no_diab_baseline_unique = unique(all_waves_no_diab_baseline$HHIDPN)
all_waves_no_diab_baseline_unique_values = length(all_waves_no_diab_baseline_unique)

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


total_n_proportion = rbind(all_waves_unique_id_value, 
                           all_waves_no_diab_baseline_unique_values, 
                           new_diabetes_participant_wave_df)

#write.csv(total_n_proportion, (paste(OUTPUT_ROOT, "total_n_nodiabatbaseline_DIAB_values.csv", sep="")))



all_waves_no_diab_baseline$CVD = case_when(all_waves_no_diab_baseline$heartattack_new_bin ==1 | all_waves_no_diab_baseline$heartcondition_new_bin == 1 | all_waves_no_diab_baseline$heartfailure2yrs_bin == 1 ~ 1, 
                                           all_waves_no_diab_baseline$heartattack_new_bin ==0 & all_waves_no_diab_baseline$heartcondition_new_bin == 0 & all_waves_no_diab_baseline$heartfailure2yrs_bin == 0 ~ 0)


############# 
