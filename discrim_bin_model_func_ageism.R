
discrim_bin_model_func_ageism = function (data_wce_subset, Model_n, Model_name){
  
  
  results = data.frame()
  
  subset_reason_discrim1_reason_age_subset = summary_score_WCE_analysis(data_WCE = data_wce_subset,
                                                         exposure = "reason_discrim1_reason_age", 
                                                         outcome = "diabetes_new_bin", 
                                                         covariates_list = Model_n)
  
  
  subset_reason_discrim1_reason_age_HR_subset = subset_reason_discrim1_reason_age_subset[1]
  
  subset_reason_discrim1_reason_age_subset_stats_recoded = subset_reason_discrim1_reason_age_subset[2]
  
  
  unique(data_wce_subset$continious_age)
  unique(data_wce_subset$diabetes_new_bin)
  unique(data_wce_subset$summary_mean_score_reason_discrim1_reason_age)
  
  ######## bootstrapped CIs for the HRs from the above model 
  subset_reason_discrim1_reason_age_age_CI_subset  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_subset,
                                                                    exposure = "reason_discrim1_reason_age", 
                                                                    outcome = "diabetes_new_bin",
                                                                    covariates_list = Model_n)
  
  
  
  #folder_1 = paste(subset_var, "/", sep = "")
  #folder_2 = paste(Model_name, "/", sep = "")
  
  #path = paste(OUTPUT_ROOT, folder_1, folder_2, sep = "")
  
  #dir.create(paste(path, sep = ""))
  
  #write.csv(subset_reason_discrim1_reason_age_subset_stats_recoded, paste(OUTPUT_ROOT, folder_1, folder_2, "subset_discrim_BIN_subset_stats.csv", sep=""))
  
  
  subset_reason_discrim1_reason_age_HR_subset = unlist(subset_reason_discrim1_reason_age_HR_subset)
  subset_reason_discrim1_reason_age_results_subset = cbind(subset_reason_discrim1_reason_age_HR_subset, subset_reason_discrim1_reason_age_age_CI_subset)
  
  colnames(subset_reason_discrim1_reason_age_results_subset) = c("hazard ratio", "5% CI", "95% CI")
  
  
  return(subset_reason_discrim1_reason_age_results_subset)
}

########
########
########
