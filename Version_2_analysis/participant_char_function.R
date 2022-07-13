
participant_char_function =  function (data){
  
  data = data 

## All sex

data$sex_1_2
data_baseline_female = subset(data, data$sex_1_2 == 2)
n_female_all = nrow(data_baseline_female)


## ALL wealth 

data$wealth_noIRA

quantiles_first_wealth_noIRA = quantile(data$wealth_noIRA,
                                        probs = seq(0, 0.25), na.rm = TRUE)

quantiles_second_wealth_noIRA = quantile(data$wealth_noIRA,
                                         probs = seq(0.50, 0.75), na.rm = TRUE)

quantiles_third_wealth_noIRA = quantile(data$wealth_noIRA,
                                        probs = seq(0.25, 0.5), na.rm = TRUE)


quantiles_fourth_wealth_noIRA = quantile(data$wealth_noIRA,
                                         probs = seq(0.75, 1), na.rm = TRUE)


q1_wealth_noIRA_baseline_all = subset(data_baseline, data$wealth_noIRA <= quantiles_first_wealth_noIRA)
n_q1 = nrow(q1_wealth_noIRA_baseline_all)



q2_wealth_noIRA_baseline_all = subset(data_baseline, data$wealth_noIRA > quantiles_first_wealth_noIRA & data$wealth_noIRA <= quantiles_second_wealth_noIRA)
n_q2 = nrow(q2_wealth_noIRA_baseline_all)


q3_wealth_noIRA_baseline_all = subset(data_baseline, data$wealth_noIRA > quantiles_second_wealth_noIRA & data$wealth_noIRA <= quantiles_third_wealth_noIRA)
n_q3 = nrow(q3_wealth_noIRA_baseline_all)


q4_wealth_noIRA_baseline_all = subset(data_baseline, data$wealth_noIRA > quantiles_third_wealth_noIRA & data$wealth_noIRA <= quantiles_fourth_wealth_noIRA)
n_q4 = nrow(q4_wealth_noIRA_baseline_all)


q5_wealth_noIRA_baseline_all = subset(data_baseline, data$wealth_noIRA > quantiles_fourth_wealth_noIRA)
n_q5 = nrow(q5_wealth_noIRA_baseline_all)

## All age 

mean_age_all = mean(data$continious_age) 
sd_age_all = sd(data$continious_age)
range_age = range(data$continious_age)


## All CVD

#data$angina_new_bin
#data_baseline_angina = subset(data_baseline, data$angina_new_bin == 1)
#n_angina_all = nrow(data_baseline_angina)


#data$hypertension_new_bin
#data_baseline_hypertension_all = subset(data_baseline, data$hypertension_new_bin == 1)
#n_baseline_hypertension_all = nrow(data_baseline_hypertension_all)

#data$stroke_new_bin
#data_baseline_stroke_all = subset(data_baseline, data$stroke_new_bin == 1)
#n_baseline_stroke_all = nrow(data_baseline_stroke_all)

##data$heartcondition_new_bin
#data_baseline_heartcondition_all = subset(data_baseline, data$heartcondition_new_bin == 1)
#n_baseline_heartcondition_all = nrow(data_baseline_heartcondition_all)

#data$heartfailure2yrs_bin
#data_baseline_heartfailure2yrs_bin_all = subset(data_baseline, data$heartfailure2yrs_bin == 1)
#n_baseline_heartfailure2yrs_bin_all = nrow(data_baseline_heartfailure2yrs_bin_all)

#data$heartattack_ever_bin
#data_baseline_heartattack_ever_bin_all = subset(data_baseline, data$heartattack_ever_bin == 1)
#n_baseline_heartattack_ever_bin_all = nrow(data_baseline_heartattack_ever_bin_all)


##### recode into CVD below: 
  
  #data$CVD_new = case_when(data$angina_new_bin ==1 | data$heartfailure2yrs_bin == 1 | data$heartattack_new_bin == 1 ~ 1, 
                           #data$angina_new_bin ==0 & data$heartfailure2yrs_bin == 0  & data$heartattack_new_bin == 0 ~ 0) 
  
  
  #unique(data$CVD)
  
  
  #data$CVD_ever = case_when(data$heartfailure2yrs_bin == 1 | data$heartattack_ever_bin == 1 ~ 1, 
                            #data$heartfailure2yrs_bin == 0 & data$heartattack_ever_bin == 0 ~ 0) 
  
  #unique(data$CVD_ever)
  
##### recode below: 

#data$CVD[data$angina_new_bin ==1 | data$heartfailure2yrs_bin == 1 | data$heartattack_ever_bin == 1 | data$heartattack_new_bin == 1] <-1
#data$CVD[data$angina_new_bin ==0 & data$heartfailure2yrs_bin == 0 & data$heartattack_ever_bin == 0 & data$heartattack_new_bin == 0] <-0

#unique(data$CVD)


#data$CVD_ever[data$heartfailure2yrs_bin == 1 | data$heartattack_ever_bin == 1 ] <-1
#data$CVD_ever[data$heartfailure2yrs_bin == 0 & data$heartattack_ever_bin == 0 ] <-0

#unique(data$CVD_ever)


#data$CVD_new[data$angina_new_bin ==1 | data$heartattack_new_bin == 1] <-1
#data$CVD_new[data$angina_new_bin ==0 & data$heartattack_new_bin == 0] <-0



#unique(data$CVD_new)
  
  #data_CVD_new = subset(data, data$CVD_new == 1)
  #n_data_CVD_new = nrow(data_CVD_new)
  
  #data_CVD_ever = subset(data, data$CVD_ever == 1)
  #n_data_CVD_ever = nrow(data_CVD_ever)
  
 # unique(data$CVD_new)
  


#n_CVD_baseline =  n_data_CVD_ever + data_CVD_new


## All healh behaviours

mean_days_alchohol_baseline_all = mean(data$alcohol_days_week, na.rm = TRUE)
sd_days_alchohol_baseline_all = sd(data$alcohol_days_week, na.rm = TRUE)
range_alchohol_baseline_all = range(data$alcohol_days_week, na.rm = TRUE) 


data$smokes_now_bin
data_baseline_smokes_now_bin_all = subset(data_baseline, data$smokes_now_bin == 1)
n_baseline_smokes_now_bin_all = nrow(data_baseline_smokes_now_bin_all)


median_PA_baseline_all = median(data$vigarious_physical_activity, na.rm = TRUE)
q2_PA_baseline_all = quantile(data$vigarious_physical_activity,
                              probs = seq(0.25, 0.5), na.rm = TRUE)

q3_PA_baseline_all = quantile(data$vigarious_physical_activity,
                              probs = seq(0.5, 0.75), na.rm = TRUE)

range_PA_baseline_all = range(data$vigarious_physical_activity, na.rm = TRUE)



## All CVD risks

data$hypertension_new_bin
data_baseline_hypertension_bin_all = subset(data_baseline, data$hypertension_new_bin == 1)
n_baseline_hypertension_bin_all = nrow(data_baseline_hypertension_bin_all)



## All BMI 

mean_BMI_all = mean(data$assessed_BMI, na.rm = TRUE)
sd_BMI_all =  sd(data$assessed_BMI, na.rm = TRUE)
range_BMI_all = range(data$assessed_BMI, na.rm = TRUE) 



## All depression 
data$checklist_depression_bin
data_baseline_depression_bin_all = subset(data_baseline, data$checklist_depression_bin == 1)
n_baseline_depression_bin_all = nrow(data_baseline_depression_bin_all)

result_participant_char = cbind(mean_age_all, 
                            sd_age_all, 
                            range_age, 
                            
                            n_female_all, 
                            
                            
                            n_q1, 
                            n_q2, 
                            n_q3, 
                            n_q4, 
                            n_q5, 
                            
                            #n_data_CVD_new, 
                           # n_data_CVD_ever,
                           # n_CVD_baseline, 
                            
                            mean_days_alchohol_baseline_all, 
                            sd_days_alchohol_baseline_all, 
                            range_alchohol_baseline_all, 
                            n_baseline_smokes_now_bin_all, 
                            q2_PA_baseline_all, 
                            q3_PA_baseline_all, 
                            range_PA_baseline_all, 
                            
                            n_baseline_hypertension_bin_all, 
                            mean_BMI_all, 
                            sd_BMI_all, 
                            
                            n_baseline_depression_bin_all)

return(result_participant_char)
                  
                 
}
