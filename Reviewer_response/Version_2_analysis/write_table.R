
current_directory = "/Users/k2147340/OneDrive - King's College London/Documents/"

#current_directory = "/Users/aliya/my_docs/"
#current_directory = "/Users/aliyaamirova/proj/Cumulative_effects_HRS"

OUTPUT_ROOT = paste(current_directory, "proj/Cumulative_effects_HRS/Reviewer_response/Version_2_analysis/RESULTS/", sep="")
SOURCE_ROOT = paste(current_directory, "proj/Cumulative_effects_HRS/Reviewer_response/Version_2_analysis/", sep="")
DATA_ROOT = paste(current_directory, "/ELSA_HRS/Data_analysis/", sep = "") 


#SOURCE_ROOT = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/Results/Version_2_analysis_results/"
#OUTPUT_ROOT = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/Results/Version_2_analysis_results/"


results_reviewers = read.csv(paste(SOURCE_ROOT, "discrim_bin_version_2/results_reviewers_discrim_binrestricted__V2.csv", sep=""))

results_reviewers$Model = c(1, 2, 3, 4, 5, 6, 7)
results_reviewers$subset = rep("All", times = nrow(results_reviewers))




# rbind all results:                                   
diabetes_results_discrim_bin = rbind(results_reviewers) 



diabetes_results_discrim_bin$SE = (diabetes_results_discrim_bin$X95..CI-diabetes_results_discrim_bin$X5..CI)/(2*1.96)
diabetes_results_discrim_bin$z_value = diabetes_results_discrim_bin$hazard.ratio/diabetes_results_discrim_bin$SE 
#diabetes_results_discrim_bin$p_value = exp(-0.717*diabetes_results_discrim_bin$z_value -0.416*((diabetes_results_discrim_bin$z_value)^2))


#df2[,c(1,3,2,4)]

as.numeric(diabetes_results_discrim_bin$hazard.ratio)
as.numeric(diabetes_results_discrim_bin$X5..CI)
as.numeric(diabetes_results_discrim_bin$X95..CI)
as.numeric(diabetes_results_discrim_bin$SE)
as.numeric(diabetes_results_discrim_bin$z_value)
#as.numeric(diabetes_results_discrim_bin$p_value)

diabetes_results_discrim_bin = cbind(diabetes_results_discrim_bin$subset,
                                     diabetes_results_discrim_bin$Model,
                                     diabetes_results_discrim_bin$hazard.ratio,
                                     diabetes_results_discrim_bin$X5..CI,
                                     diabetes_results_discrim_bin$X95..CI,
                                     diabetes_results_discrim_bin$SE,
                                     diabetes_results_discrim_bin$z_value) 
                                     #diabetes_results_discrim_bin$p_value)

colnames(diabetes_results_discrim_bin) = c("subset", 
                                           "Model",
                                           
                                           "hazard.ratio", 
                                           "X5..CI",
                                           "X95..CI", 
                                           "SE", 
                                           "z_value")

write.csv(diabetes_results_discrim_bin, paste(OUTPUT_ROOT, "V2_p_values.csv", sep=""))


diabetes_results_HRs_only = read.csv(paste(OUTPUT_ROOT, "V2_p_values.csv", sep=""))

head(diabetes_results_HRs_only)
#diabetes_results_HRs_only$p_value
diabetes_results_HRs_only$X5..CI = round(diabetes_results_HRs_only$X5..CI, digits = 2)
diabetes_results_HRs_only$X95..CI = round(diabetes_results_HRs_only$X95..CI, digits = 2)
diabetes_results_HRs_only$hazard.ratio = round(diabetes_results_HRs_only$hazard.ratio, digits = 2)

head(diabetes_results_HRs_only)
# diabetes_results_HRs_only$p_value
# diabetes_results_HRs_only$asterix_mark = case_when(diabetes_results_HRs_only$p_value <=0.05 & diabetes_results_HRs_only$p_value > 0.01 ~ "*",
#                                                    diabetes_results_HRs_only$p_value <=0.01 &  diabetes_results_HRs_only$p_value > 0.001 ~ "**",
#                                                    diabetes_results_HRs_only$p_value <=0.001 ~ "***")

diabetes_results_HRs_only$CI_both = paste("[", diabetes_results_HRs_only$X5..CI, ";" , diabetes_results_HRs_only$X95..CI, "]", sep="")


diabetes_results_HRs_only = cbind(diabetes_results_HRs_only$subset,
                                  diabetes_results_HRs_only$Model, 
                                  diabetes_results_HRs_only$hazard.ratio,
                                  #diabetes_results_HRs_only$asterix_mark,
                                  diabetes_results_HRs_only$CI_both)

colnames(diabetes_results_HRs_only) = c("subset", 
                                        "Model",
                                        "HR", 
                                        "p-value level",
                                        "95% CI")

write.csv(diabetes_results_HRs_only, paste(OUTPUT_ROOT, "V2_HRs_only.csv", sep=""))

