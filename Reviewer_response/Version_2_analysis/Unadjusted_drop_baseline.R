
Unadjusted_drop_baseline = function (subset_var1, 
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
  
  
  Model_unadjusted = HRs_CIs_analysis(data_wce_subset = dataset_noNAs_timepoints, 
                                      Model_n = NULL, 
                                      
                                      exposure = exposure, 
                                      outcome = outcome,
                                      
                                      
                                      subset_name = subset_name, 
                                      
                                      Model_name = "Unadjusted")
  
  
  
  
  
  results_exposure = rbind(Model_unadjusted)
  
  
  
  
  return(results_exposure)
}
