

SOURCE_ROOT = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/Results/"
OUTPUT_ROOT = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/Results/"



entire_dataset = read.csv(paste(OUTPUT_ROOT, "entire_dataset_V2_HRs_only.csv", sep=""))

entire_dataset_model_1 = subset(entire_dataset, Model == 1) 

entire_dataset_model_2 = subset(entire_dataset, Model == 2) 

entire_dataset_model_3 = subset(entire_dataset, Model == 3) 

entire_dataset_model_4 = subset(entire_dataset, Model == 4) 

entire_dataset_model_5 = subset(entire_dataset, Model == 5) 

entire_dataset_model_6 = subset(entire_dataset, Model == 6) 

entire_dataset_model_7 = subset(entire_dataset, Model == 7)

results_restricted_cols = cbind(entire_dataset_model_1, 
                                entire_dataset_model_2, 
                                entire_dataset_model_3, 
                                entire_dataset_model_4, 
                                entire_dataset_model_5, 
                                entire_dataset_model_6, 
                                entire_dataset_model_7) 



write.csv(results_restricted_cols, paste(OUTPUT_ROOT, "entire_dataset_V2_HRs_only_cols_transposed.csv", sep=""))

########

