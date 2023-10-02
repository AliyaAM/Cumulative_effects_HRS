



library("survival")
library("survminer")
library("dplyr")
library("ggplot2")


#Model 1: age and sex, wealth  [basis adjustment]
Model_1 = c("continious_age", "wealth_noIRA", "sex_1_2")
#Model 2: age, sex, wealth, BMI, hypertension  [basic adjustment + diabetes risk factors]
Model_2 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "hypertension_new_bin")
#Model 3: age, sex, wealth, physical activity, smoking (yes/no), and alcohol (days/week) [basic adjustment + health behaviours]
Model_3 = c("continious_age", "wealth_noIRA", "sex_1_2","alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin')
#Model 4: age, sex, wealth, CVD  [basic adjustment + CVD most common diabetes co_morbidity]
Model_4 = c("continious_age", "wealth_noIRA", "sex_1_2","CVD")
#Model 5: age, sex, wealth, depression  [basic adjustment + depression best researched psychosocial factor in diabetes ]
Model_5 = c("continious_age","wealth_noIRA", "sex_1_2","checklist_depression_bin")
#Model 6: age, sex, wealth, BMI, hypertension, CVD  [basic adjustment + diabetes risk factors+ CVD]
Model_6 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "hypertension_new_bin", "CVD")
#Model 7: age, sex, wealth, BMI, hypertension, depression  [basic adjustment + diabetes risk factors+ Depression]
Model_7 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "hypertension_new_bin", "checklist_depression_bin")


#Model 1: age and sex, wealth  [basis adjustment]
Model_1_nosex = c("continious_age", "wealth_noIRA")
#Model 2: age, sex, wealth, BMI, hypertension  [basic adjustment + diabetes risk factors]
Model_2_nosex = c("continious_age", "wealth_noIRA",  "assessed_BMI", "hypertension_new_bin")
#Model 3: age, sex, wealth, physical activity, smoking (yes/no), and alcohol (days/week) [basic adjustment + health behaviours]
Model_3_nosex = c("continious_age", "wealth_noIRA", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin')
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
Model_noBMIcov_3 = c("continious_age", "wealth_noIRA", "sex_1_2","alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin')
#Model 4: age, sex, wealth, CVD  [basic adjustment + CVD most common diabetes co_morbidity]
Model_noBMIcov_4 = c("continious_age", "wealth_noIRA", "sex_1_2","CVD")
#Model 5: age, sex, wealth, depression  [basic adjustment + depression best researched psychosocial factor in diabetes ]
Model_noBMIcov_5 = c("continious_age","wealth_noIRA", "sex_1_2","checklist_depression_bin")
#Model 6: age, sex, wealth, BMI, hypertension, CVD  [basic adjustment + diabetes risk factors+ CVD]
Model_noBMIcov_6 = c("continious_age", "wealth_noIRA", "sex_1_2", "hypertension_new_bin", "CVD")
#Model 7: age, sex, wealth, BMI, hypertension, depression  [basic adjustment + diabetes risk factors+ Depression]
Model_noBMIcov_7 = c("continious_age", "wealth_noIRA", "sex_1_2",  "hypertension_new_bin", "checklist_depression_bin")

###### DATA:
# below is the entire dataset, not subseted to anyone: 

cumulative_effects_dat = read.csv("/Users/aliya/my_docs/proj/Cumulative_effects_HRS/data_files/all_waves_nodiabatbaseline_DIAB.csv")

###### Adding variables to the main dataset:

#outcome:  cumulative_effects_dat$diabetes_new_bin 
#exposure: cumulative_effects_dat$discrim 
#time: cumulative_effects_dat$stop_new

###### Adding variables to the main dataset:
###### Adding variables to the main dataset:



DATA_ROOT = "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/Results/situations/"



unique(cumulative_effects_dat$start_new)

cumulative_effects_dat$time_point = cumulative_effects_dat$start_new

cumulative_effects_dat$years = 2 * cumulative_effects_dat$start_new

cumulative_effects_dat$months = 12 * cumulative_effects_dat$years

cumulative_effects_dat$follow_up = cumulative_effects_dat$years


#1 = 2 year 
#2 = 4 follow_up 
#3 = 6 follow_up 


unique(cumulative_effects_dat$start_new)

sd(cumulative_effects_dat$start_new)

cumulative_effects_dat$diabetes_new_bin  = case_when(cumulative_effects_dat$diabetes_new_bin == 1 ~ 1, 
                                                    cumulative_effects_dat$diabetes_new_bin == 0 ~ 0) 


cumulative_effects_dat$diabetes_new_bin_reversed = case_when(cumulative_effects_dat$diabetes_new_bin  == 1 ~ 0, 
                                                             cumulative_effects_dat$diabetes_new_bin  == 0 ~ 1) 



#### plot for the entire dataset: 

fit <- survfit(Surv(follow_up, diabetes_new_bin ) ~ discrim_harassed , data = cumulative_effects_dat)
summary_all = summary(fit)
print(summary_all)


### output below: 

summary_all_table = summary_all$table
subset_all = rep("All", time=nrow(summary_all_table))
summary_all_table = cbind(subset_all, summary_all_table)

write.csv(summary_all_table, paste(DATA_ROOT, "/summary_all_table_discrim_harassed_cat.csv", sep = ""))

Number_events_all = fit$n.event
Number_risk_all = fit$n.risk
Number_censored_all = fit$n.censor
probability_for_nodiab_all = fit$surv
probability_SE_all = fit$std.err
probability_lower_CI_all = fit$lower
probability_upper_CI_all = fit$upper
cumhaz_output_all =  fit$cumhaz
cumhaz_output_std_all = fit$std.chaz




####
### output below: 
All_results_unadjusted = cbind(Number_events_all,
                               Number_risk_all,
                               Number_censored_all, 
                               probability_for_nodiab_all, 
                               probability_SE_all, 
                               probability_lower_CI_all, 
                               probability_upper_CI_all, 
                               cumhaz_output_all, 
                               cumhaz_output_std_all) 

subset_all_undj  = rep("All", time=nrow(All_results_unadjusted)) 
All_results_unadjusted = cbind(subset_all_undj, All_results_unadjusted) 
#write.csv(All_results_unadjusted, "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/All_results_unadjusted_discrim_harassed_cat.csv")

write.csv(All_results_unadjusted, paste(DATA_ROOT, "/All_results_unadjusted_discrim_harassed_cat.csv", sep = ""))

plot_all = ggsurvplot(fit,
                      pval = TRUE, conf.int = TRUE,
                      risk.table = FALSE, # Add risk table
                      risk.table.col = "strata", # Change risk table color by groups
                      linetype = "strata", # Change line type by groups
                      surv.median.line = "hv", # Specify median survival
                      
                      surv.scale = "percent",
                      
                      ggtheme = theme_bw(),
                      ylim = c(0.5, 1),
                      pval.coord = c(0, 0.95),
                      
                      xlab = "Years from baseline", 
                      
                      ylab = "proportion of cases free of diabetes", 
                      
                      # Change ggplot2 theme
                      palette = c("#E7B800", "#2E9FDF"))

plot_all
print(plot_all)  
