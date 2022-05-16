
HRs_CIs_analysis = function (data_wce_subset,
                        
                             Model_n,
                        
                             exposure, 
                             outcome, 
                             
                             subset_name,
                             Model_name){
  
  
  exposure = exposure
  outcome = outcome
  
  results = data.frame()
  
  subset__HR_subset = summary_score_WCE_analysis(data_WCE = data_wce_subset,
                                                 exposure = exposure, 
                                                 outcome = outcome, 
                                                 subset_name = subset_name,
                                                 Model_name = Model_name, 
                                                 covariates_list = Model_n)
  
  
   
  

  
  
  ######## bootstrapped CIs for the HRs from the above model 
  subset__age_CI_subset  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_subset,
                                                                    exposure = exposure, 
                                                                    outcome = outcome,
                                                                    covariates_list = Model_n)
  
  

  #folder_1 = paste(subset_var, "/", sep = "")
  #folder_2 = paste(Model_name, "/", sep = "")
  
  #path = paste(OUTPUT_ROOT, folder_1, folder_2, sep = "")
  
  #dir.create(paste(path, sep = ""))
  
  #write.csv(subset__subset_stats_recoded, paste(OUTPUT_ROOT, folder_1, folder_2, "subset__subset_stats.csv", sep=""))
  
  
  subset_results_HR_CIs = cbind(subset__HR_subset, subset__age_CI_subset)
  
  #colnames(subset_results_HR_CIs) = c("analysed n", 
                                      #"diabetes onset (n)",
                                      #"max. timepoints", 
                                      #"median timepoint",
                                      #"BIC", 
                                      #"hazard ratio",
                                      #"5% CI",
                                      #"95% CI")
  
  colnames(subset_results_HR_CIs) = c("analysed_n", 
                                      "diabetes_onset_n",
                                      "max_timepoints", 
                                      "median_timepoint",
                                      "BIC", 
                                      "hazard_ratio",
                                      "CI_5",
                                      "CI_95")
  
  
  #subset_results_subset = add total n = start_new = 0, number of cases,coefficient table for each model, BIC,
  
  return(subset_results_HR_CIs)
}

########
########
########
