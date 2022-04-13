
Seven_models_drop_baseline = function (subset_var1, 
                         subset_value1, 
                         
                         subset_BMI, 
                         subset_BMI_value, 
                         
                         subset_var2, 
                         subset_value2,
                         
                         
                         subset_var3, 
                         subset_value3,
                         
                         
                         subset_reason1, 
                         subset_reason1_value, 
                         
                         subset_reason2, 
                         subset_reason2_value, 
                         
                         
                         subset_reason3, 
                         subset_reason3_value, 
                         
                         exposure, 
                         
                         outcome, 
                         
                         subset_name, 
                         
                         HRS2008_data,
                         HRS2010_data, 
                         HRS2012_data,
                         HRS2014_data, 
                         HRS2016_data,
                         HRS2018_data){
  
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
  
  
  
  dataset = clean_recode_sort(subset_var1, 
                              subset_value1, 
                              
                              subset_BMI, 
                              subset_BMI_value, 
                              
                              subset_var2, 
                              subset_value2,
                              
                              
                              subset_var3, 
                              subset_value3,
                              
                              
                              subset_reason1, 
                              subset_reason1_value, 
                              
                              subset_reason2, 
                              subset_reason2_value, 
                              
                              
                              subset_reason3, 
                              subset_reason3_value, 
                              
                              HRS2008_data = HRS2008_data,
                              HRS2010_data = HRS2010_data, 
                              HRS2012_data = HRS2012_data,
                              HRS2014_data = HRS2014_data, 
                              HRS2016_data = HRS2016_data,
                              HRS2018_data = HRS2018_data) 
  
  ###### run all models 
  
  
  
  ###### drop NAs and weird strings like " NA", THE WCE ANALYSIS DOES NOT RUN WITH NAs
  

  dataset_noNAs = dataset %>% drop_na(outcome)
  unique(dataset_noNAs$outcome)
  
  dataset_noNAs = dataset_noNAs %>% drop_na(exposure)
  unique(dataset_noNAs$exposure)
  
  
  # sort out data and tag time points as start_new, stop_new
  dataset_noNAs_timepoints = sort_timepoints_drop_baseline(data = dataset_noNAs)
  
  
  Model_1_exposure = HRs_CIs_analysis(data_wce_subset = dataset_noNAs_timepoints, 
                                      Model_n = Model_1, 
                                      
                                      exposure = exposure, 
                                      outcome = outcome,
                                      
                                      
                                      subset_name = subset_name, 
                                      
                                      Model_name = "Model_1")
  
  
  Model_2_exposure =  HRs_CIs_analysis(data_wce_subset = dataset_noNAs_timepoints, 
                                       Model_n = Model_2, 
                                       
                                       exposure = exposure, 
                                       outcome = outcome,
                                       
                                       subset_name = subset_name, 
                                       
                                       
                                       Model_name = "Model_2")
  
  
  
  
  Model_3_exposure = HRs_CIs_analysis(data_wce_subset = dataset_noNAs_timepoints, 
                                      Model_n = Model_3, 
                                      
                                      exposure = exposure, 
                                      outcome = outcome,
                                      
                                      subset_name = subset_name, 
                                      
                                      
                                      Model_name = "Model_3")
  
  
  
  Model_4_exposure = HRs_CIs_analysis(data_wce_subset = dataset_noNAs_timepoints, 
                                      Model_n = Model_4, 
                                      
                                      exposure = exposure, 
                                      outcome = outcome,
                                      
                                      subset_name = subset_name, 
                                      
                                      
                                      Model_name = "Model_4")
  
  
  
  
  
  Model_5_exposure = HRs_CIs_analysis(data_wce_subset = dataset_noNAs_timepoints, 
                                      Model_n = Model_5, 
                                      
                                      exposure = exposure, 
                                      outcome = outcome,
                                      
                                      subset_name = subset_name, 
                                      
                                      
                                      Model_name = "Model_5")
  
  
  
  
  Model_6_exposure = HRs_CIs_analysis(data_wce_subset = dataset_noNAs_timepoints, 
                                      Model_n = Model_6, 
                                      
                                      exposure = exposure, 
                                      outcome = outcome,
                                      
                                      subset_name = subset_name, 
                                      
                                      
                                      Model_name = "Model_6")
  
  
  
  Model_7_exposure = HRs_CIs_analysis(data_wce_subset = dataset_noNAs_timepoints, 
                                      Model_n = Model_7, 
                                      
                                      exposure = exposure, 
                                      outcome = outcome,
                                      subset_name = subset_name, 
                                      
                                      
                                      Model_name = "Model_7")
  
  
  
  results_exposure = rbind(Model_1_exposure,
                           Model_2_exposure,
                           Model_3_exposure,
                           Model_4_exposure,
                           Model_5_exposure,
                           Model_6_exposure,
                           Model_7_exposure)
  
  
  
  results_exposure_table_col = cbind(Model_1_exposure,
                                     Model_2_exposure,
                                     Model_3_exposure,
                                     Model_4_exposure,
                                     Model_5_exposure,
                                     Model_6_exposure,
                                     Model_7_exposure)
  

  
  return(results_exposure)
}
