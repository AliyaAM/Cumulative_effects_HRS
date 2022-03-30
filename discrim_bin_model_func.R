
  
  
  results = data.frame()
  
  subset_discrim_bin_subset = summary_score_WCE_analysis(data_WCE = data_wce_subset,
                                                         exposure = "discrim_bin", 
                                                         outcome = "diabetes_new_bin", 
                                                         covariates_list = Model_n)
  
  
  subset_discrim_bin_HR_subset = subset_discrim_bin_subset[1]
  
  subset_discrim_bin_subset_stats_recoded = subset_discrim_bin_subset[2]
  
  
  unique(data_wce_subset$continious_age)
  unique(data_wce_subset$diabetes_new_bin)
  unique(data_wce_subset$summary_mean_score_discrim_bin)
  
  ######## bootstrapped CIs for the HRs from the above model 
  subset_discrim_bin_age_CI_subset  = summary_score_Bootstrapped_CI(WCE_data_CI = data_wce_subset,
                                                                    exposure = "discrim_bin", 
                                                                    outcome = "diabetes_new_bin",
                                                                    covariates_list = Model_n)
  
  

  #folder_1 = paste(subset_var, "/", sep = "")
  #folder_2 = paste(Model_name, "/", sep = "")
  
  #path = paste(OUTPUT_ROOT, folder_1, folder_2, sep = "")
  
  #dir.create(paste(path, sep = ""))
  
  #write.csv(subset_discrim_bin_subset_stats_recoded, paste(OUTPUT_ROOT, folder_1, folder_2, "subset_discrim_BIN_subset_stats.csv", sep=""))
  
  
  subset_discrim_bin_HR_subset = unlist(subset_discrim_bin_HR_subset)
  subset_discrim_bin_results_subset = cbind(subset_discrim_bin_HR_subset, subset_discrim_bin_age_CI_subset)
  
  colnames(subset_discrim_bin_results_subset) = c("hazard ratio", "5% CI", "95% CI")
  
  
  return(subset_discrim_bin_results_subset)
}

########
########
########
