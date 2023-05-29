

SOURCE_ROOT = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/Results/Version_2_analysis_results/"
OUTPUT_ROOT = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/Results/Version_2_analysis_results/"



restricted_to_discrim_reason_results = read.csv(paste(OUTPUT_ROOT, "restricted_V2_HRs_only.csv", sep=""))
results = read.csv(paste(OUTPUT_ROOT, "V2_HRs_only.csv", sep=""))

restricted_to_discrim_reason_results_model_1 = subset(restricted_to_discrim_reason_results, Model == 1) 

restricted_to_discrim_reason_results_model_2 = subset(restricted_to_discrim_reason_results, Model == 2) 

restricted_to_discrim_reason_results_model_3 = subset(restricted_to_discrim_reason_results, Model == 3) 

restricted_to_discrim_reason_results_model_4 = subset(restricted_to_discrim_reason_results, Model == 4) 

restricted_to_discrim_reason_results_model_5 = subset(restricted_to_discrim_reason_results, Model == 5) 

restricted_to_discrim_reason_results_model_6 = subset(restricted_to_discrim_reason_results, Model == 6) 

restricted_to_discrim_reason_results_model_7 = subset(restricted_to_discrim_reason_results, Model == 7)

results_restricted_cols = cbind(restricted_to_discrim_reason_results_model_1, 
                                 restricted_to_discrim_reason_results_model_2, 
                                 restricted_to_discrim_reason_results_model_3, 
                                 restricted_to_discrim_reason_results_model_4, 
                                 restricted_to_discrim_reason_results_model_5, 
                                 restricted_to_discrim_reason_results_model_6, 
                                 restricted_to_discrim_reason_results_model_7) 
      


write.csv(results_restricted_cols, paste(OUTPUT_ROOT, "results_restricted_V2_HRs_only_cols.csv", sep=""))

########


results_model_1 = subset(results, Model == 1) 
results_model_2 = subset(results, Model == 2) 

results_model_3 = subset(results, Model == 3) 

results_model_4 = subset(results, Model == 4) 

results_model_5 = subset(results, Model == 5) 

results_model_6 = subset(results, Model == 6) 

results_model_7 = subset(results, Model == 7)

results_cols = cbind(results_model_1,
                     results_model_2, 
                     results_model_3, 
                     results_model_4, 
                     results_model_5, 
                     results_model_6, 
                     results_model_7) 



write.csv(results_cols, paste(OUTPUT_ROOT, "results_V2_HRs_only_cols.csv", sep=""))


