
library(dplyr)
library(tidyr)

current_directory = "/Users/aliya/my_docs/proj/Cumulative_effects_HRS"

OUTPUT_ROOT =(paste(current_directory, "/data_files/Results/", sep=""))
SOURCE_ROOT = (paste(current_directory, "/Version_2_analysis/", sep=""))

source((paste(SOURCE_ROOT, "participant_char_function.R", sep="")))

data = read.csv("/Users/aliya/my_docs/proj/Cumulative_effects_HRS/data_files/all_waves_nodiabatbaseline_DIAB.csv")

unique(data$start_new)
unique(data$diabetes_new)

data$discrim_bin = case_when(data$discrim_harassed_bin == 1 | data$discrim_lessrespect_bin == 1 | data$discrim_medical_bin  == 1 | data$discrim_notclever_bin == 1 | data$discrim_afraidothers_bin == 1 | data$discrim_poorerservice_bin == 1 ~ 1, 
                             data$discrim_harassed_bin == 0 & data$discrim_lessrespect_bin == 0 & data$discrim_medical_bin  == 0 & data$discrim_notclever_bin == 0 & data$discrim_afraidothers_bin == 0 & data$discrim_poorerservice_bin == 0 ~ 0) 

unique(data$discrim_bin)


data = data %>% drop_na(diabetes_new)
unique(data$diabetes_new)

data = data %>% drop_na(discrim_bin)
unique(data$discrim_bin)

data_baseline = subset(data, data$start_new == 0)
all_participant_char = participant_char_function(data = data_baseline)

write.csv(all_participant_char, paste(OUTPUT_ROOT, "all_participant_char.csv"))

#CVD_ever in all participants = 872 
#n_data_CVD_new in all participants = 1042

non_diabetes_baseline = subset(data, data$diabetes_new == 0 & data$start_new == 0) 
non_diabetes_followup_1 = subset(data, data$diabetes_new == 0 & data$start_new == 1) 
non_diabetes_followup_2 = subset(data, data$diabetes_new == 0 & data$start_new == 2) 
non_diabetes_followup_3 = subset(data, data$diabetes_new == 0 & data$start_new == 3) 
non_diabetes_followup_4 = subset(data, data$diabetes_new == 0 & data$start_new == 4)
non_diabetes_followup_5 = subset(data, data$diabetes_new == 0 & data$start_new == 5)


non_diabetes_baseline_and1 = inner_join(non_diabetes_baseline, 
                                        non_diabetes_followup_1,
                                        copy = TRUE,    
                                        keep = FALSE, 
                                        suffix = c("", "_new"), 

                                        by = "HHIDPN")

non_diabetes_baseline_and1_2 = inner_join(non_diabetes_baseline_and1, 
                                          non_diabetes_followup_2,
                                          copy = TRUE, 
                                          keep = FALSE, 
                                          suffix = c("", "_new"), 

                                          by = "HHIDPN")

non_diabetes_baseline_and1_2_3 = inner_join(non_diabetes_baseline_and1_2, 
                                          non_diabetes_followup_3,
                                          copy = TRUE,     
                                          keep = FALSE, 
                                          suffix = c("", "_new"), 

                                          by = "HHIDPN")

non_diabetes_baseline_and1_2_3_4 = inner_join(non_diabetes_baseline_and1_2_3, 
                                           non_diabetes_followup_4,
                                           copy = TRUE,  
                                           keep = FALSE, 
                                           suffix = c("", "_new"), 

                                           by = "HHIDPN")

non_diabetes_throughout_the_study = inner_join(non_diabetes_baseline_and1_2_3_4, 
                                              non_diabetes_followup_5,
                                              copy = TRUE,
                                              keep = FALSE, 
                                              suffix = c("", "_new"), 
                                              by = "HHIDPN")


##########
##########

##### recode below: 

CV_cases = function(data){

data$CVD[data$angina_new_bin ==1 | data$heartfailure2yrs_bin == 1 | data$heartattack_ever_bin == 1 | data$heartattack_new_bin == 1] <-1
data$CVD[data$angina_new_bin ==0 & data$heartfailure2yrs_bin == 0 & data$heartattack_ever_bin == 0 & data$heartattack_new_bin == 0] <-0

unique(data$CVD)


data$CVD_ever[data$heartfailure2yrs_bin == 1 | data$heartattack_ever_bin == 1 ] <-1
data$CVD_ever[data$heartfailure2yrs_bin == 0 & data$heartattack_ever_bin == 0 ] <-0

unique(data$CVD_ever)


data$CVD_new[data$angina_new_bin ==1 | data$heartattack_new_bin == 1] <-1
data$CVD_new[data$angina_new_bin ==0 & data$heartattack_new_bin == 0] <-0



unique(data$CVD_new)

data_CVD_new = subset(data, data$CVD_new == 1)
n_data_CVD_new = nrow(data_CVD_new)

data_CVD_ever = subset(data, data$CVD_ever == 1)
n_data_CVD_ever = nrow(data_CVD_ever)

unique(data$CVD_new)

n_CVD_baseline =  n_data_CVD_ever + data_CVD_new

return(n_CVD_baseline)

}

CVD_cases_non_diabetes = CV_cases(data = non_diabetes_throughout_the_study)
non_diabetes_participant_char = participant_char_function(data = non_diabetes_throughout_the_study)

 write.csv(non_diabetes_participant_char, paste(OUTPUT_ROOT, "non_diabetes_participant_char.csv"))


#####################



diabetes_baseline = subset(data, data$diabetes_new == 0 & data$start_new == 0) 
diabetes_followup_1 = subset(data, data$diabetes_new == 1 & data$start_new == 1) 
diabetes_followup_2 = subset(data, data$diabetes_new == 1 & data$start_new == 2) 
diabetes_followup_3 = subset(data, data$diabetes_new == 1 & data$start_new == 3) 
diabetes_followup_4 = subset(data, data$diabetes_new == 1 & data$start_new == 4)
diabetes_followup_5 = subset(data, data$diabetes_new == 1 & data$start_new == 5)

 #diabetes_baseline_and1 = inner_join( diabetes_baseline, 
  #                                       diabetes_followup_1,
                                       
 #                                       suffix = c("", "_new"), 
                                        
  #                                      by = "HHIDPN")

 diabetes_baseline_and1_2 = full_join( diabetes_followup_1, 
                                           diabetes_followup_2,
                                         
                                          suffix = c("", "_new"), 
                                          
                                          by = "HHIDPN")

 diabetes_baseline_and1_2_3 = full_join( diabetes_baseline_and1_2, 
                                             diabetes_followup_3,
                                           
                                            suffix = c("", "_new"), 
                                            
                                            by = "HHIDPN")
 
 diabetes_baseline_and1_2_3_4 = full_join( diabetes_baseline_and1_2_3, 
                                               diabetes_followup_4,
                                            
                                              suffix = c("", "_new"), 
                                              
                                              by = "HHIDPN")

 diabetes_throughout_the_study = full_join( diabetes_baseline_and1_2_3_4, 
                                                diabetes_followup_5,
                                              
                                               suffix = c("", "_new"), 
                                               by = "HHIDPN")

 
 diabetes_participant_char = participant_char_function(data = diabetes_throughout_the_study)
 CVD_cases_non_diabetes = CV_cases(data = diabetes_throughout_the_study)
 
 
 write.csv(diabetes_participant_char, paste(OUTPUT_ROOT, "diabetes_participant_char.csv"))
 
 
