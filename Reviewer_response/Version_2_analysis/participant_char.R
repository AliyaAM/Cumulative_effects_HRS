library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

library(dplyr)
library(stats)

#data = read.csv("/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/DATA_FOR_PLOT/all_waves_nodiabatbaseline_DIAB.csv") 

data = read.csv("C:/Users/k2147340/OneDrive - King's College London/Desktop/dataset_noNAs_timepoints_TEST_DELETE_AFTER_debugging_24nov2022.csv")



data_baseline = subset(data, data$start_new == 0)

## All sex

data_baseline$sex_1_2
data_baseline_female = subset(data_baseline, data_baseline$sex_1_2 == 2)
n_female_all = nrow(data_baseline_female)


## ALL wealth 

data_baseline$wealth_noIRA

quantiles_first_wealth_noIRA = quantile(data_baseline$wealth_noIRA,
                 probs = seq(0, 0.25), na.rm = TRUE)

quantiles_second_wealth_noIRA = quantile(data_baseline$wealth_noIRA,
                                         probs = seq(0.50, 0.75), na.rm = TRUE)

quantiles_third_wealth_noIRA = quantile(data_baseline$wealth_noIRA,
                                         probs = seq(0.25, 0.5), na.rm = TRUE)


quantiles_fourth_wealth_noIRA = quantile(data_baseline$wealth_noIRA,
                                        probs = seq(0.75, 1), na.rm = TRUE)


q1_wealth_noIRA_baseline_all = subset(data_baseline, data_baseline$wealth_noIRA <= quantiles_first_wealth_noIRA)
nrow(q1_wealth_noIRA_baseline_all)


q2_wealth_noIRA_baseline_all = subset(data_baseline, data_baseline$wealth_noIRA > quantiles_first_wealth_noIRA & data_baseline$wealth_noIRA <= quantiles_second_wealth_noIRA)
nrow(q2_wealth_noIRA_baseline_all)


q3_wealth_noIRA_baseline_all = subset(data_baseline, data_baseline$wealth_noIRA > quantiles_second_wealth_noIRA & data_baseline$wealth_noIRA <= quantiles_third_wealth_noIRA)
nrow(q3_wealth_noIRA_baseline_all)


q4_wealth_noIRA_baseline_all = subset(data_baseline, data_baseline$wealth_noIRA > quantiles_third_wealth_noIRA & data_baseline$wealth_noIRA <= quantiles_fourth_wealth_noIRA)
nrow(q4_wealth_noIRA_baseline_all)


q5_wealth_noIRA_baseline_all = subset(data_baseline, data_baseline$wealth_noIRA > quantiles_fourth_wealth_noIRA)
nrow(q5_wealth_noIRA_baseline_all)

## All age 

mean_age_all = mean(data_baseline$continious_age) 
sd_age_all = sd(data_baseline$continious_age)


## All CVD

data_baseline$angina_new_bin
data_baseline_angina = subset(data_baseline, data_baseline$angina_new_bin == 1)
n_angina_all = nrow(data_baseline_angina)


data_baseline$hypertension_new_bin
data_baseline_hypertension_all = subset(data_baseline, data_baseline$hypertension_new_bin == 1)
n_baseline_hypertension_all = nrow(data_baseline_hypertension_all)

data_baseline$stroke_new_bin
data_baseline_stroke_all = subset(data_baseline, data_baseline$stroke_new_bin == 1)
n_baseline_stroke_all = nrow(data_baseline_stroke_all)

data_baseline$heartcondition_new_bin
data_baseline_heartcondition_all = subset(data_baseline, data_baseline$heartcondition_new_bin == 1)
n_baseline_heartcondition_all = nrow(data_baseline_heartcondition_all)

data_baseline$heartfailure2yrs_bin
data_baseline_heartfailure2yrs_bin_all = subset(data_baseline, data_baseline$heartfailure2yrs_bin == 1)
n_baseline_heartfailure2yrs_bin_all = nrow(data_baseline_heartfailure2yrs_bin_all)

data_baseline$heartattack_ever_bin
data_baseline_heartattack_ever_bin_all = subset(data_baseline, data_baseline$heartattack_ever_bin == 1)
n_baseline_heartattack_ever_bin_all = nrow(data_baseline_heartattack_ever_bin_all)


##### recode into CVD below: 

clean_recode_keyvars =  function (data){
  
  data = data 

data$CVD_new[data$angina_new_bin ==1 | data$heartfailure2yrs_bin == 1 | data$heartattack_new_bin == 1] <-1
data$CVD_new[data$angina_new_bin ==0 & data$heartfailure2yrs_bin == 0  & data$heartattack_new_bin == 0] <-0

unique(data$CVD)


data$CVD_ever[data$heartfailure2yrs_bin == 1 | data$heartattack_ever_bin == 1 ] <-1
data$CVD_ever[data$heartfailure2yrs_bin == 0 & data$heartattack_ever_bin == 0 ] <-0

unique(data$CVD_ever)



data$heartattack_ever_bin
data_CVD_new = subset(data, data$CVD_new == 1)
n_data_CVD_new = nrow(data_CVD_new)

data_CVD_ever = subset(data, data$CVD_ever == 1)
n_data_CVD_ever = nrow(data_CVD_ever)

unique(data$CVD_new)

return(params = c(n_data_CVD_new, 
                  n_data_CVD_ever))
}

n_CVD_baseline_new_AND_ever = clean_recode_keyvars(data = data_baseline)
n_CVD_baseline = n_CVD_baseline_new_AND_ever[1] + n_CVD_baseline_new_AND_ever[2]

## All healh behaviours

mean_days_alchohol_baseline_all = mean(data_baseline$alcohol_days_week)
sd_days_alchohol_baseline_all = sd(data_baseline$alcohol_days_week)

data_baseline$smokes_now_bin
data_baseline_smokes_now_bin_all = subset(data_baseline, data_baseline$smokes_now_bin == 1)
n_baseline_smokes_now_bin_all = nrow(data_baseline_smokes_now_bin_all)

median_PA_baseline_all = median(data_baseline$vigarious_physical_activity, na.rm = TRUE)
q2_PA_baseline_all = quantile(data_baseline$vigarious_physical_activity,
         probs = seq(0.25, 0.5), na.rm = TRUE)

q3_PA_baseline_all = quantile(data_baseline$vigarious_physical_activity,
                 probs = seq(0.5, 0.75), na.rm = TRUE)

range_PA_baseline_all = range(data_baseline$vigarious_physical_activity, na.rm = TRUE)

## All CVD risks

data_baseline$hypertension_new_bin
data_baseline_hypertension_bin_all = subset(data_baseline, data_baseline$hypertension_new_bin == 1)
n_baseline_hypertension_bin_all = nrow(data_baseline_hypertension_bin_all)

## All BMI 

mean_BMI_all = mean(data_baseline$assessed_BMI)
sd_BMI_all =  sd(data_baseline$assessed_BMI)

## All depression 
data_baseline$checklist_depression_bin
data_baseline_depression_bin_all = subset(data_baseline, data_baseline$checklist_depression_bin == 1)
n_baseline_depression_bin_all = nrow(data_baseline_depression_bin_all)




##############
##############

developed_diabetes_later = subset(data, data$diabetes_new == 1)

developed_diabetes_later_baseline = subset(developed_diabetes_later, developed_diabetes_later$start_new == 0)

developed_diabetes_later_baseline$sex_1_2
developed_diabetes_later_baseline$wealth_noIRA
developed_diabetes_later_baseline$continious_age
developed_diabetes_later_baseline$assessed_BMI

developed_diabetes_later_baseline$angina_new_bin
developed_diabetes_later_baseline$hypertension_new_bin
developed_diabetes_later_baseline$stroke_new_bin
developed_diabetes_later_baseline$heartcondition_new_bin
developed_diabetes_later_baseline$heartfailure2yrs_bin
developed_diabetes_later_baseline$heartattack_ever_bin

developed_diabetes_later_baseline$alcohol_days_week
developed_diabetes_later_baseline$smokes_now_bin
developed_diabetes_later_baseline$vigarious_physical_activity
developed_diabetes_later_baseline$smokes_now_bin


developed_diabetes_later_baseline$hypertension_new_bin
developed_diabetes_later_baseline$checklist_depression_bin

frequency(developed_diabetes_later_baseline$discrim_bin)
unique(developed_diabetes_later_baseline$discrim_bin)

##############
##############

non_diabetes = subset(data, data$diabetes_new == 0)

non_diabetes_baseline = subset(non_diabetes, non_diabetes$start_new == 0)

non_diabetes_baseline$sex_1_2
non_diabetes_baseline$wealth_noIRA
non_diabetes_baseline$continious_age
non_diabetes_baseline$assessed_BMI

non_diabetes_baseline$angina_new_bin
non_diabetes_baseline$hypertension_new_bin
non_diabetes_baseline$stroke_new_bin
non_diabetes_baseline$heartcondition_new_bin
non_diabetes_baseline$heartfailure2yrs_bin
non_diabetes_baseline$heartattack_ever_bin

non_diabetes_baseline$smokes_now_bin
non_diabetes_baseline$vigarious_physical_activity
non_diabetes_baseline$smokes_now_bin


non_diabetes_baseline$hypertension_new_bin
non_diabetes_baseline$checklist_depression_bin



all = rbind(developed_diabetes_later, non_diabetes)
all = as.data.frame(all)

 chisq.test(all$diabetes_new, all$race_white)


# all_timepoint0 = subset(all, all$start_new == 0)
# chisq.test(all_timepoint0$diabetes_new, all_timepoint0$discrim_bin)
# table(all_timepoint0$diabetes_new, all_timepoint0$discrim_bin)

all_timepoint1 = subset(all, all$start_new == 1)
chisq.test(all_timepoint1$diabetes_new, all_timepoint1$discrim_bin)
table(all_timepoint1$diabetes_new, all_timepoint1$discrim_bin)

all_timepoint2 = subset(all, all$start_new == 2)
chisq.test(all_timepoint2$diabetes_new, all_timepoint2$discrim_bin)
table(all_timepoint2$diabetes_new, all_timepoint2$discrim_bin)

all_timepoint3 = subset(all, all$start_new == 3)
chisq.test(all_timepoint3$diabetes_new, all_timepoint3$discrim_bin)
table(all_timepoint3$diabetes_new, all_timepoint3$discrim_bin)

#all %>% group_by(start_new) %>% chisq.test(all$diabetes_new, all$discrim_bin)


#all = rbind(developed_diabetes_later, non_diabetes)
#all %>% group_by(all$start_new) %>% table(all$diabetes_new, all$discrim_bin)



##############
##############

data_baseline_female = subset(data_baseline, data_baseline$sex_1_2 == 2)
nrow(data_baseline_female)

data_baseline_female$sex_1_2
data_baseline_female$wealth_noIRA
data_baseline_female$continious_age
data_baseline_female$assessed_BMI

data_baseline_female$angina_new_bin
data_baseline_female$hypertension_new_bin
data_baseline_female$stroke_new_bin
data_baseline_female$heartcondition_new_bin
data_baseline_female$heartfailure2yrs_bin
data_baseline_female$heartattack_ever_bin

data_baseline_female$alcohol_days_week
data_baseline_female$smokes_now_bin
data_baseline_female$vigarious_physical_activity
data_baseline_female$smokes_now_bin


data_baseline_female$hypertension_new_bin
data_baseline_female$checklist_depression_bin

##############
##############

### MARGINALISED GROUPS 

##############
##############

data_baseline_obese = subset(data_baseline, data_baseline$assessed_BMI >30)


data_baseline_obese$sex_1_2
data_baseline_obese$wealth_noIRA
data_baseline_obese$continious_age
data_baseline_obese$assessed_BMI

data_baseline_obese$angina_new_bin
data_baseline_obese$hypertension_new_bin
data_baseline_obese$stroke_new_bin
data_baseline_obese$heartcondition_new_bin
data_baseline_obese$heartfailure2yrs_bin
data_baseline_obese$heartattack_ever_bin

data_baseline_obese$alcohol_days_week
data_baseline_obese$smokes_now_bin
data_baseline_obese$vigarious_physical_activity
data_baseline_obese$smokes_now_bin


data_baseline_obese$hypertension_new_bin
data_baseline_obese$checklist_depression_bin
