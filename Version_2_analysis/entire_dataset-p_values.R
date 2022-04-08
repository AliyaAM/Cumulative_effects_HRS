#SOURCE_ROOT =  "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/"
#OUTPUT_ROOT = "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/Results/"


#OUTPUT_ROOT ="/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/Results/"
#SOURCE_ROOT = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/WCE_analysis/"
SOURCE_ROOT = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/Results/"
OUTPUT_ROOT = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/Results/"


#SOURCE_data_ROOT  = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/"

# read.csv(paste(OUTPUT_ROOT, "entire_dataset_discrim_bin_norace_national_origin_religioncov_table_colrestricted_reason.csv", sep=""))
# read.csv(paste(OUTPUT_ROOT, "entire_dataset_discrim_bin_table_colrestricted_reason.csv", sep=""))
#= read.csv(paste(OUTPUT_ROOT, "race_national_origin_results_discrim_bin_table_colrestricted_reason.csv", sep=""))

# fouldlers: 
#discrim_bin_version_2
#restricted_discrim_reason
#joint_version2

entire_dataset = read.csv(paste(OUTPUT_ROOT, "entire_dataset_discrim_bin_V2.csv", sep=""))
entire_dataset$Model = c(1, 2, 3, 4, 5, 6, 7)
entire_dataset$subset = rep("entire_dataset", times = nrow(entire_dataset))


#######
#######


diabetes_results_discrim_bin = rbind(entire_dataset) 


#age80above_results_discrim_bin_table_col_reason.csv


diabetes_results_discrim_bin$SE = (diabetes_results_discrim_bin$X95..CI-diabetes_results_discrim_bin$X5..CI)/(2*1.96)
diabetes_results_discrim_bin$z_value = diabetes_results_discrim_bin$hazard.ratio/diabetes_results_discrim_bin$SE 
diabetes_results_discrim_bin$p_value = exp(-0.717*diabetes_results_discrim_bin$z_value -0.416*((diabetes_results_discrim_bin$z_value)^2))


#df2[,c(1,3,2,4)]

as.numeric(diabetes_results_discrim_bin$hazard.ratio)
as.numeric(diabetes_results_discrim_bin$X5..CI)
as.numeric(diabetes_results_discrim_bin$X95..CI)
as.numeric(diabetes_results_discrim_bin$SE)
as.numeric(diabetes_results_discrim_bin$z_value)
as.numeric(diabetes_results_discrim_bin$p_value)

diabetes_results_discrim_bin = cbind(diabetes_results_discrim_bin$subset,
                                     diabetes_results_discrim_bin$Model,
                                     diabetes_results_discrim_bin$hazard.ratio,
                                     diabetes_results_discrim_bin$X5..CI,
                                     diabetes_results_discrim_bin$X95..CI,
                                     diabetes_results_discrim_bin$SE,
                                     diabetes_results_discrim_bin$z_value,
                                     diabetes_results_discrim_bin$p_value)

colnames(diabetes_results_discrim_bin) = c("subset", 
                                           "Model",
                                           
                                           "hazard.ratio", 
                                           "X5..CI",
                                           "X95..CI", 
                                           "SE", 
                                           "z_value",
                                           "p_value")

write.csv(diabetes_results_discrim_bin, paste(OUTPUT_ROOT, "entire_dataset_V2_p_values.csv", sep=""))


diabetes_results_HRs_only = read.csv(paste(OUTPUT_ROOT, "entire_dataset_V2_p_values.csv", sep=""))

head(diabetes_results_HRs_only)
diabetes_results_HRs_only$p_value
diabetes_results_HRs_only$X5..CI = round(diabetes_results_HRs_only$X5..CI, digits = 2)
diabetes_results_HRs_only$X95..CI = round(diabetes_results_HRs_only$X95..CI, digits = 2)
diabetes_results_HRs_only$hazard.ratio = round(diabetes_results_HRs_only$hazard.ratio, digits = 2)

head(diabetes_results_HRs_only)
diabetes_results_HRs_only$p_value
diabetes_results_HRs_only$asterix_mark = case_when(diabetes_results_HRs_only$p_value <=0.05 & diabetes_results_HRs_only$p_value > 0.01 ~ "*",
                                                   diabetes_results_HRs_only$p_value <=0.01 &  diabetes_results_HRs_only$p_value > 0.001 ~ "**",
                                                   diabetes_results_HRs_only$p_value <=0.001 ~ "***")

diabetes_results_HRs_only$CI_both = paste("[", diabetes_results_HRs_only$X5..CI, ";" , diabetes_results_HRs_only$X95..CI, "]", sep="")


diabetes_results_HRs_only = cbind(diabetes_results_HRs_only$subset,
                                  diabetes_results_HRs_only$Model, 
                                  diabetes_results_HRs_only$hazard.ratio,
                                  diabetes_results_HRs_only$asterix_mark,
                                  diabetes_results_HRs_only$CI_both)

colnames(diabetes_results_HRs_only) = c("subset", 
                                        "Model",
                                        "HR", 
                                        "p-value level",
                                        "95% CI")

write.csv(diabetes_results_HRs_only, paste(OUTPUT_ROOT, "entire_dataset_V2_HRs_only.csv", sep=""))

