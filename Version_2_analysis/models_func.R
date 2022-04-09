
models_func = function (data_wce_subset,
                        Model_n,
                        Model_name, 
                        exposure, 
                        outcome){
  
  
  exposure = exposure
  outcome = outcome
  
  results = data.frame()
  
  subset__subset = summary_score_WCE_analysis(data_WCE = data_wce_subset,
                                                         exposure = exposure, 
                                                         outcome = outcome, 
                                                         covariates_list = Model_n)
  
  
  subset__HR_subset = subset__subset[1]
  
  subset__subset_stats_recoded = subset__subset[2]
  
  
  unique(data_wce_subset$continious_age)
  unique(data_wce_subset$diabetes_new_bin)
  unique(data_wce_subset$summary_mean_score_)
  
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
  
  
  subset__HR_subset = unlist(subset__HR_subset)
  subset_results_subset = cbind(subset__HR_subset, subset__age_CI_subset)
  
  colnames(subset_results_subset) = c("hazard ratio", "5% CI", "95% CI")
  
  
  return(subset_results_subset)
}

########
########
########
