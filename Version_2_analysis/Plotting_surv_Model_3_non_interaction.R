

SOURCE_ROOT = "/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"

library("survival")
library("survminer")
library("dplyr")
library("tibble")



#Model 1: age and sex, wealth  [basis adjustment]
#Model 2: age, sex, wealth, BMI, hypertension  [basic adjustment + diabetes risk factors]
Model_2 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "hypertension_new_bin")
#Model 3: age, sex, wealth, physical activity, smoking (yes/no), and alcohol (days/week) [basic adjustment + health behaviours]
Model_3 = c("continious_age", "wealth_noIRA", "sex_1_2","alcohol_days_week",  "vigarious_physical_activity", 'smokes_now_bin')
#Model 4: age, sex, wealth, CVD  [basic adjustment + CVD most common diabetes co_morbidity]
Model_4 = c("continious_age", "wealth_noIRA", "sex_1_2","CVD")
#Model 5: age, sex, wealth, depression  [basic adjustment + depression best researched psychosocial factor in diabetes ]
Model_5 = c("continious_age","wealth_noIRA", "sex_1_2","checklist_depression_bin")
#Model 6: age, sex, wealth, BMI, hypertension, CVD  [basic adjustment + diabetes risk factors+ CVD]
Model_6 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "hypertension_new_bin", "CVD")
#Model 7: age, sex, wealth, BMI, hypertension, depression  [basic adjustment + diabetes risk factors+ Depression]
Model_7 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "hypertension_new_bin", "checklist_depression_bin")


#Model 1: age and sex, wealth  [basis adjustment]
Model_2_non_interact_nosex = c("continious_age", "wealth_noIRA")
#Model 2: age, sex, wealth, BMI, hypertension  [basic adjustment + diabetes risk factors]
Model_2_non_interact_nosex = c("continious_age", "wealth_noIRA",  "assessed_BMI", "hypertension_new_bin")
#Model 3: age, sex, wealth, physical activity, smoking (yes/no), and alcohol (days/week) [basic adjustment + health behaviours]
Model_3_nosex = c("continious_age", "wealth_noIRA", "alcohol_days_week",  "vigarious_physical_activity", 'smokes_now_bin')
#Model 4: age, sex, wealth, CVD  [basic adjustment + CVD most common diabetes co_morbidity]
Model_4_nosex = c("continious_age", "wealth_noIRA","CVD")
#Model 5: age, sex, wealth, depression  [basic adjustment + depression best researched psychosocial factor in diabetes ]
Model_5_nosex = c("continious_age","wealth_noIRA", "checklist_depression_bin")
#Model 6: age, sex, wealth, BMI, hypertension, CVD  [basic adjustment + diabetes risk factors+ CVD]
Model_6_nosex = c("continious_age", "wealth_noIRA",  "assessed_BMI", "hypertension_new_bin", "CVD")
#Model 7: age, sex, wealth, BMI, hypertension, depression  [basic adjustment + diabetes risk factors+ Depression]
Model_7_nosex = c("continious_age", "wealth_noIRA",  "assessed_BMI", "hypertension_new_bin", "checklist_depression_bin")


######


#Model 1: age and sex, wealth  [basis adjustment]
Model_noBMIcov_1 = c("continious_age", "wealth_noIRA", "sex_1_2")
#Model 2: age, sex, wealth, BMI, hypertension  [basic adjustment + diabetes risk factors]
Model_noBMIcov_2 = c("continious_age", "wealth_noIRA", "sex_1_2", "hypertension_new_bin")
#Model 3: age, sex, wealth, physical activity, smoking (yes/no), and alcohol (days/week) [basic adjustment + health behaviours]
Model_noBMIcov_3 = c("continious_age", "wealth_noIRA", "sex_1_2","alcohol_days_week",  "vigarious_physical_activity", 'smokes_now_bin')
#Model 4: age, sex, wealth, CVD  [basic adjustment + CVD most common diabetes co_morbidity]
Model_noBMIcov_4 = c("continious_age", "wealth_noIRA", "sex_1_2","CVD")
#Model 5: age, sex, wealth, depression  [basic adjustment + depression best researched psychosocial factor in diabetes ]
Model_noBMIcov_5 = c("continious_age","wealth_noIRA", "sex_1_2","checklist_depression_bin")
#Model 6: age, sex, wealth, BMI, hypertension, CVD  [basic adjustment + diabetes risk factors+ CVD]
Model_noBMIcov_6 = c("continious_age", "wealth_noIRA", "sex_1_2", "hypertension_new_bin", "CVD")
#Model 7: age, sex, wealth, BMI, hypertension, depression  [basic adjustment + diabetes risk factors+ Depression]
Model_noBMIcov_7 = c("continious_age", "wealth_noIRA", "sex_1_2",  "hypertension_new_bin", "checklist_depression_bin")

###### DATA:

original_data = read.csv("/Users/aliya/my_docs/proj/Cumulative_effects_HRS/data_files/all_waves_nodiabatbaseline_DIAB.csv")



###### Adding variables to the main dataset:

###### Adding variables to the main dataset:


original_data$discrim_harassed = as.numeric(original_data$discrim_harassed)
original_data$discrim_lessrespect = as.numeric(original_data$discrim_lessrespect)
original_data$discrim_medical = as.numeric(original_data$discrim_medical)
original_data$discrim_notclever = as.numeric(original_data$discrim_notclever)
original_data$discrim_poorerservice = as.numeric(original_data$discrim_poorerservice)
original_data$discrim_afraidothers = as.numeric(original_data$discrim_afraidothers)


original_data$heartcondition_ever_bin = as.numeric(original_data$heartcondition_ever_bin)
original_data$heartcondition_new_bin = as.numeric(original_data$heartcondition_new_bin)
original_data$angina_new_bin = as.numeric(original_data$angina_new_bin)
original_data$stroke_new_bin = as.numeric(original_data$stroke_new_bin)
original_data$heartfailure2yrs_bin = as.numeric(original_data$heartfailure2yrs_bin)
original_data$heartattack_ever_bin = as.numeric(original_data$heartattack_ever_bin)
original_data$heartattack_new_bin = as.numeric(original_data$heartattack_new_bin)

original_data$vigarious_physical_activity = as.numeric(original_data$vigarious_physical_activity)


original_data$alcohol_days_week = as.numeric(original_data$alcohol_days_week)
original_data$checklist_depression_bin = as.numeric(original_data$checklist_depression_bin)
original_data$wealth_noIRA = as.numeric(original_data$wealth_noIRA)


original_data$diabetes_new = as.numeric(original_data$diabetes_new)




##### recode below: 

original_data$CVD[original_data$angina_new_bin ==1 | original_data$heartfailure2yrs_bin == 1 | original_data$heartattack_ever_bin == 1 | original_data$heartattack_new_bin == 1] <-1
original_data$CVD[original_data$angina_new_bin ==0 & original_data$heartfailure2yrs_bin == 0 & original_data$heartattack_ever_bin == 0 & original_data$heartattack_new_bin == 0] <-0

unique(original_data$CVD)


original_data$CVD_ever[original_data$heartfailure2yrs_bin == 1 | original_data$heartattack_ever_bin == 1 ] <-1
original_data$CVD_ever[original_data$heartfailure2yrs_bin == 0 & original_data$heartattack_ever_bin == 0 ] <-0

unique(original_data$CVD_ever)


original_data$CVD_new[original_data$angina_new_bin ==1 | original_data$heartattack_new_bin == 1] <-1
original_data$CVD_new[original_data$angina_new_bin ==0 & original_data$heartattack_new_bin == 0] <-0



unique(original_data$CVD_new)




#physical activity (original): 1.every day; 2.>1 per week; 3.1 per week; 4.l-3 per mon; 5.never; .d=DK/NA; .r=RF

original_data$vigarious_physical_activity_new = case_when(original_data$vigarious_physical_activity == 1 ~ 5, 
                                                 original_data$vigarious_physical_activity == 2 ~ 4, 
                                                 original_data$vigarious_physical_activity == 3 ~ 3, 
                                                 original_data$vigarious_physical_activity == 4 ~ 2, 
                                                 original_data$vigarious_physical_activity == 5 ~ 1) 




original_data$vigarious_physical_activity_bin = case_when(original_data$vigarious_physical_activity_new == 5 ~ 1, 
                                                 original_data$vigarious_physical_activity_new == 4 ~ 1, 
                                                 original_data$vigarious_physical_activity_new == 3 ~ 1, 
                                                 original_data$vigarious_physical_activity_new == 2 ~ 0, 
                                                 original_data$vigarious_physical_activity_new == 1 ~ 0) 





original_data$alcohol_days_week_new =  na_if(original_data$alcohol_days_week, 8)
original_data$alcohol_days_week_new = na_if(original_data$alcohol_days_week_new, 9) 


###### add smoking 

HRS_2008_data = read.csv(paste(SOURCE_ROOT, "HRS_2008_data/HRS2008_dataset_latest.csv", sep=""))
HRS_2010_data = read.csv(paste(SOURCE_ROOT, "HRS_2010_data/HRS2010_dataset_latest.csv", sep=""))
HRS_2012_data =  read.csv(paste(SOURCE_ROOT, "HRS_2012_data/HRS2012_dataset_latest.csv", sep=""))
HRS_2014_data =  read.csv(paste(SOURCE_ROOT, "HRS_2014_data/HRS2014_dataset_latest.csv", sep=""))
HRS_2016_data =  read.csv(paste(SOURCE_ROOT, "HRS_2016_data/HRS2016_dataset_latest.csv", sep=""))
HRS_2018_data =  read.csv(paste(SOURCE_ROOT, "HRS_2018_data/HRS2018_dataset_latest.csv", sep=""))


HRS_2008_data$HHIDPN
HRS_2008_data$HRS2008_emo_psychiat_prob_ever
HRS_2008_data$HRS2008_emo_psychiat_prob_bin

HRS_2008_data$HRS2008_checklist_depression_bin


df_2008 <- tibble(HRS_2008_data$HHIDPN, HRS_2008_data$smokes_now_bin)

df_2010 <- tibble(HRS_2010_data$HHIDPN, HRS_2010_data$smokes_now_bin)

df_2012 <- tibble(HRS_2012_data$HHIDPN, HRS_2012_data$smokes_now_bin)

df_2014 <- tibble(HRS_2014_data$HHIDPN, HRS_2014_data$smokes_now_bin)

df_2016 <- tibble(HRS_2016_data$HHIDPN, HRS_2016_data$smokes_now_bin)

df_2018 <- tibble(HRS_2018_data$HHIDPN, HRS_2018_data$smokes_now_bin)

df_1 = df_2008

df_2 <- df_1  %>%  bind_rows(df_2010)

df_3 <- df_2 %>%  bind_rows(df_2012)

df_4 <- df_3 %>%  bind_rows(df_2014)

df_5 <- df_4 %>%  bind_rows(df_2016)

data <- df_5 %>%  bind_rows(df_2018)

data = tibble(data$`HRS_2008_data$HHIDPN`, data$`HRS_2008_data$smokes_now_bin`) 

colnames(data) = c("HHIDPN",
                   "smokes_now_bin")


ID = unique(data$HHIDPN)

#print(isTRUE(data$HHIDPN == ID[1]))
#data = data %>% drop_na()

participant_wave_df = data.frame()

n = 1

for (id in ID){
  
  
  participant_wave = subset(data, data$HHIDPN == id)
  
  if (nrow(participant_wave)== 1){
    

    
    participant_wave$start_new = c(0)
    participant_wave$stop_new = c(1)
    
    participant_wave_df = rbind(participant_wave_df, participant_wave) 
    
  }
  
  if (nrow(participant_wave) ==2){
    

    participant_wave$start_new = c(0, 1)
    participant_wave$stop_new = c(1, 2)
    participant_wave_df = rbind(participant_wave_df, participant_wave) 
    
  }
  
  if (nrow(participant_wave)==3){
    

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
    

    participant_wave$start_new = c(0, 1, 2, 3, 4)
    participant_wave$stop_new = c(1, 2, 3, 4, 5)
    participant_wave_df = rbind(participant_wave_df, participant_wave) 
    
  } 
  
  if (nrow(participant_wave)==6){
    
    participant_wave$start_new = c(0, 1, 2, 3, 4, 5)
    participant_wave$stop_new = c(1, 2, 3, 4, 5, 6)
    participant_wave_df = rbind(participant_wave_df, participant_wave) 
    
  }
  n = n + 1
}

unique(participant_wave_df$start_new)




all_waves_smokes_now_bin_added  = participant_wave_df

unique(all_waves_smokes_now_bin_added$start_new)
unique(all_waves_smokes_now_bin_added$smokes_now_bin)



#######################
#######################



cumulative_effects_dat$discrim_harassed_bin = case_when(cumulative_effects_dat$discrim_harassed == 1 ~ 1, 
                                                        cumulative_effects_dat$discrim_harassed == 2 ~ 1, 
                                                        cumulative_effects_dat$discrim_harassed == 3 ~ 1, 
                                                        cumulative_effects_dat$discrim_harassed == 4 ~ 1, 
                                                        cumulative_effects_dat$discrim_harassed == 5 ~ 0, 
                                                        cumulative_effects_dat$discrim_harassed == 6 ~ 0,
                                                        cumulative_effects_dat$discrim_harassed == 0 ~ 0) 



cumulative_effects_dat$discrim_lessrespect_bin = case_when(cumulative_effects_dat$discrim_lessrespect == 1 ~ 1, 
                                                           cumulative_effects_dat$discrim_lessrespect == 2 ~ 1, 
                                                           cumulative_effects_dat$discrim_lessrespect == 3 ~ 1, 
                                                           cumulative_effects_dat$discrim_lessrespect == 4 ~ 1, 
                                                           cumulative_effects_dat$discrim_lessrespect == 5 ~ 0, 
                                                           cumulative_effects_dat$discrim_lessrespect == 6 ~ 0,
                                                           cumulative_effects_dat$discrim_lessrespect == 0 ~ 0) 



cumulative_effects_dat$discrim_medical_bin = case_when(cumulative_effects_dat$discrim_medical == 1 ~ 1, 
                                                       cumulative_effects_dat$discrim_medical == 2 ~ 1, 
                                                       cumulative_effects_dat$discrim_medical == 3 ~ 1, 
                                                       cumulative_effects_dat$discrim_medical == 4 ~ 1, 
                                                       cumulative_effects_dat$discrim_medical == 5 ~ 0, 
                                                       cumulative_effects_dat$discrim_medical == 6 ~ 0,
                                                       cumulative_effects_dat$discrim_medical == 0 ~ 0) 





cumulative_effects_dat$discrim_notclever_bin = case_when(cumulative_effects_dat$discrim_notclever == 1 ~ 1, 
                                                         cumulative_effects_dat$discrim_notclever == 2 ~ 1, 
                                                         cumulative_effects_dat$discrim_notclever == 3 ~ 1, 
                                                         cumulative_effects_dat$discrim_notclever == 4 ~ 1, 
                                                         cumulative_effects_dat$discrim_notclever == 5 ~ 0, 
                                                         cumulative_effects_dat$discrim_notclever == 6 ~ 0,
                                                         cumulative_effects_dat$discrim_notclever == 0 ~ 0) 






cumulative_effects_dat$discrim_poorerservice_bin = case_when(cumulative_effects_dat$discrim_poorerservice == 1 ~ 1, 
                                                             cumulative_effects_dat$discrim_poorerservice == 2 ~ 1, 
                                                             cumulative_effects_dat$discrim_poorerservice == 3 ~ 1, 
                                                             cumulative_effects_dat$discrim_poorerservice == 4 ~ 1, 
                                                             cumulative_effects_dat$discrim_poorerservice == 5 ~ 0, 
                                                             cumulative_effects_dat$discrim_poorerservice == 6 ~ 0) 




cumulative_effects_dat$discrim_afraidothers_bin = case_when(cumulative_effects_dat$discrim_afraidothers == 1 ~ 1,
                                                            cumulative_effects_dat$discrim_afraidothers == 2 ~ 1, 
                                                            cumulative_effects_dat$discrim_afraidothers == 3 ~ 1, 
                                                            cumulative_effects_dat$discrim_afraidothers == 4 ~ 1, 
                                                            cumulative_effects_dat$discrim_afraidothers == 5 ~ 0, 
                                                            cumulative_effects_dat$discrim_afraidothers == 6 ~ 0,
                                                            cumulative_effects_dat$discrim_afraidothers == 0 ~ 0) 



cumulative_effects_dat$discrim_bin = case_when(cumulative_effects_dat$discrim_harassed_bin == 1 | cumulative_effects_dat$discrim_lessrespect_bin == 1 | cumulative_effects_dat$discrim_medical_bin  == 1 | cumulative_effects_dat$discrim_notclever_bin == 1 | cumulative_effects_dat$discrim_afraidothers_bin == 1 | cumulative_effects_dat$discrim_poorerservice_bin == 1 ~ 1, 
                                               cumulative_effects_dat$discrim_harassed_bin == 0 & cumulative_effects_dat$discrim_lessrespect_bin == 0 & cumulative_effects_dat$discrim_medical_bin  == 0 & cumulative_effects_dat$discrim_notclever_bin == 0 & cumulative_effects_dat$discrim_afraidothers_bin == 0 & cumulative_effects_dat$discrim_poorerservice_bin == 0 ~ 0) 


unique(cumulative_effects_dat$start_new)

#outcome:  cumulative_effects_dat$diabetes_new_bin
#exposure: cumulative_effects_dat$discrim_bin
#time: cumulative_effects_dat$stop_new

###### Adding variables to the main cumulative_effects_datset:
###### Adding variables to the main cumulative_effects_datset:

cumulative_effects_dat$discrim_bin

cumulative_effects_dat$discrimination = cumulative_effects_dat$discrim_bin

cumulative_effects_dat$time_point = cumulative_effects_dat$start_new

cumulative_effects_dat$years = 2 * cumulative_effects_dat$start_new

cumulative_effects_dat$months = 12 * cumulative_effects_dat$years

cumulative_effects_dat$follow_up = cumulative_effects_dat$years


cumulative_effects_dat$diabetes_new_bin = case_when(cumulative_effects_dat$diabetes_new == 1 ~ 1, 
                                                    cumulative_effects_dat$diabetes_new == 0 ~ 0) 


cumulative_effects_dat$diabetes_new_bin_reversed = case_when(cumulative_effects_dat$diabetes_new_bin == 1 ~ 0, 
                                                             cumulative_effects_dat$diabetes_new_bin == 0 ~ 1) 


#outcome:  cumulative_effects_dat$diabetes_new_bin
#exposure: cumulative_effects_dat$discrim_bin
#time: cumulative_effects_dat$stop_new

###### Adding variables to the main cumulative_effects_datset:
###### Adding variables to the main cumulative_effects_datset:

unique(cumulative_effects_dat$discrimination_cat)

unique(cumulative_effects_dat$discrimination_cat)


cumulative_effects_dat$discrimination = cumulative_effects_dat$discrim_bin
#1 = 2 year 
#2 = 4 years 
#3 = 6 years 

#coxph(Surv(t1, t2, stat) ∼ (age + surgery)* transplant) – time dependent covariates.

unique(cumulative_effects_dat$timepoints_indiv)
unique(cumulative_effects_dat$start_new)

cumulative_effects_dat$diabetes_new_bin_reversed = case_when(cumulative_effects_dat$diabetes_new_bin == 1 ~ 0, 
                                                             cumulative_effects_dat$diabetes_new_bin == 0 ~ 1) 

#Subset cumulative_effects_datsets:
data_male = subset(cumulative_effects_dat, cumulative_effects_dat$sex_1_2 ==1) 

unique(cumulative_effects_dat$sex_1_2.1)
head(cumulative_effects_dat) 

data_female = subset(cumulative_effects_dat, cumulative_effects_dat$sex_1_2.1 == 2) 


data_race = subset(cumulative_effects_dat, cumulative_effects_dat$race_white == 0) 
data_BMI = subset(cumulative_effects_dat, cumulative_effects_dat$assessed_BMI > 30) 


#cumulative_effects_dat$sex_1_2
#cumulative_effects_dat$race_white
#cumulative_effects_dat$race_white
#cumulative_effects_dat$national_origin_ousideUS_bin
#cumulative_effects_dat$religion_bin
#cumulative_effects_dat$assessed_BMI


#### plot for the entire dataset:
#survfit.coxph

#cfit <- coxph(Surv(futime, death) ~ sex + age*hgb, data=mgus2)


unique(cumulative_effects_dat$alcohol_days_week_new)
unique(cumulative_effects_dat$smokes_now_bin)
unique(cumulative_effects_dat$vigarious_physical_activity)

fit <- coxph(Surv(follow_up, diabetes_new_bin)~ discrimination + continious_age + wealth_noIRA + sex_1_2 + alcohol_days_week_new +  vigarious_physical_activity_new + smokes_now_bin , data = cumulative_effects_dat)
summary_all = summary(fit)

# coeffcients for discrimination: 
summary_all$coefficients[1,]
# exp (HR), and 95% CI: 
summary_all$conf.int[1,]
summary_all$waldtest
summary_all$logtest[1]
summary_all$n
summary_all$nevent


####
### output below: 
All_results_Model_3_non_interact = data.frame("Model_3_non_interact")
All_results_Model_3_non_interact$subset  = c("All")
All_results_Model_3_non_interact$coef  = c(summary_all$conf.int[1,1])
All_results_Model_3_non_interact$lower_CI = c(summary_all$conf.int[1,3])
All_results_Model_3_non_interact$upper_CI = c(summary_all$conf.int[1,4])
All_results_Model_3_non_interact$logtest = summary_all$logtest[1]
All_results_Model_3_non_interact$df = summary_all$logtest[2]
All_results_Model_3_non_interact$p_value = summary_all$logtest[3]

print(All_results_Model_3_non_interact)

#write.csv(All_results_Model_3_non_interact, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/All_results_Model_3_non_interact.csv")



########
########
########
########

#### plot for female dataset: 
unique(data_female$smokes_now_bin)

fit_female <- coxph(Surv(follow_up, diabetes_new_bin)~ discrimination + continious_age + wealth_noIRA + alcohol_days_week_new +  vigarious_physical_activity_new + smokes_now_bin  , data = data_female)
summary_female = summary(fit_female)

# coeffcients for discrimination: 
summary_all$coefficients[1,]
# exp (HR), and 95% CI: 
summary_female$conf.int[1,]
summary_female$waldtest
summary_female$logtest[1]
summary_female$n
summary_female$nevent


####
### output below: 
Female_results_Model_3_non_interact = data.frame("Model_3_non_interact")
Female_results_Model_3_non_interact$subset  = c("Female")
Female_results_Model_3_non_interact$coef  = c(summary_female$conf.int[1,1])
Female_results_Model_3_non_interact$lower_CI = c(summary_female$conf.int[1,3])
Female_results_Model_3_non_interact$upper_CI = c(summary_female$conf.int[1,4])
Female_results_Model_3_non_interact$logtest = summary_female$logtest[1]
Female_results_Model_3_non_interact$df = summary_female$logtest[2]
Female_results_Model_3_non_interact$p_value = summary_female$logtest[3]

print(Female_results_Model_3_non_interact)

#write.csv(Female_results_Model_3_non_interact, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/Female_results_Model_3_non_interact.csv")



### output below: 


########
########
########
########

#### plot for male dataset: 


fit_male <- coxph(Surv(follow_up, diabetes_new_bin)~ discrimination + continious_age + wealth_noIRA + alcohol_days_week_new +  vigarious_physical_activity_new + smokes_now_bin  , data = data_male)
summary_male = summary(fit_male)

# coeffcients for discrimination: 
summary_all$coefficients[1,]
# exp (HR), and 95% CI: 
summary_male$conf.int[1,]
summary_male$waldtest
summary_male$logtest[1]
summary_male$n
summary_male$nevent


####
### output below: 
male_results_Model_3_non_interact = data.frame("Model_3_non_interact")
male_results_Model_3_non_interact$subset  = c("male")
male_results_Model_3_non_interact$coef  = c(summary_male$conf.int[1,1])
male_results_Model_3_non_interact$lower_CI = c(summary_male$conf.int[1,3])
male_results_Model_3_non_interact$upper_CI = c(summary_male$conf.int[1,4])
male_results_Model_3_non_interact$logtest = summary_male$logtest[1]
male_results_Model_3_non_interact$df = summary_male$logtest[2]
male_results_Model_3_non_interact$p_value = summary_male$logtest[3]

print(male_results_Model_3_non_interact)

#write.csv(male_results_Model_3_non_interact, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/male_results_Model_3_non_interact.csv")



########
########
########
########

#### plot for race dataset: 

fit_race <- coxph(Surv(follow_up, diabetes_new_bin)~ discrimination + continious_age + wealth_noIRA + sex_1_2 + alcohol_days_week_new +  vigarious_physical_activity_new + smokes_now_bin , data = data_race)
summary_race = summary(fit_race)

# coeffcients for discrimination: 
summary_all$coefficients[1,]
# exp (HR), and 95% CI: 
summary_race$conf.int[1,]
summary_race$waldtest
summary_race$logtest[1]
summary_race$n
summary_race$nevent


####
### output below: 
race_results_Model_3_non_interact = data.frame("Model_3_non_interact")
race_results_Model_3_non_interact$subset  = c("race")
race_results_Model_3_non_interact$coef  = c(summary_race$conf.int[1,1])
race_results_Model_3_non_interact$lower_CI = c(summary_race$conf.int[1,3])
race_results_Model_3_non_interact$upper_CI = c(summary_race$conf.int[1,4])
race_results_Model_3_non_interact$logtest = summary_race$logtest[1]
race_results_Model_3_non_interact$df = summary_race$logtest[2]
race_results_Model_3_non_interact$p_value = summary_race$logtest[3]

print(race_results_Model_3_non_interact)

#write.csv(race_results_Model_3_non_interact, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/race_results_Model_3_non_interact.csv")


########
########
########
########

#### plot for BMI dataset: 

data_BMI$alcohol_days_week

fit_BMI <- coxph(Surv(follow_up, diabetes_new_bin)~ discrimination + continious_age + wealth_noIRA + sex_1_2 + alcohol_days_week_new +  vigarious_physical_activity_new + smokes_now_bin , data = data_BMI)
summary_BMI = summary(fit_BMI)

# coeffcients for discrimination: 
summary_BMI$coefficients[1,]
# exp (HR), and 95% CI: 
summary_BMI$conf.int[1,]
summary_BMI$waldtest
summary_BMI$logtest[1]
summary_BMI$n
summary_BMI$nevent


####
### output below: 
BMI_results_Model_3_non_interact = data.frame("Model_3_non_interact")
BMI_results_Model_3_non_interact$subset  = c("BMI")
BMI_results_Model_3_non_interact$coef  = c(summary_BMI$conf.int[1,1])
BMI_results_Model_3_non_interact$lower_CI = c(summary_BMI$conf.int[1,3])
BMI_results_Model_3_non_interact$upper_CI = c(summary_BMI$conf.int[1,4])
BMI_results_Model_3_non_interact$logtest = summary_BMI$logtest[1]
BMI_results_Model_3_non_interact$df = summary_BMI$logtest[2]
BMI_results_Model_3_non_interact$p_value = summary_BMI$logtest[3]

print(BMI_results_Model_3_non_interact)

#write.csv(BMI_results_Model_3_non_interact, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/BMI_results_Model_3_non_interact.csv")

Model_3_non_interact_results = rbind(All_results_Model_3_non_interact, 
                                     Female_results_Model_3_non_interact, 
                                     male_results_Model_3_non_interact, 
                                     race_results_Model_3_non_interact, 
                                     BMI_results_Model_3_non_interact) 


write.csv(Model_3_non_interact_results, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/Model_3_non_interact_results.csv")

