#SOURCE_ROOT =  "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/"
#OUTPUT_ROOT = "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/Results/"


#OUTPUT_ROOT ="/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/Results/"
#SOURCE_ROOT = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/WCE_analysis/"
SOURCE_ROOT = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/Results/Version_2_analysis_results/"
OUTPUT_ROOT = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/Results/Version_2_analysis_results/"


  
#SOURCE_data_ROOT  = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/"

# read.csv(paste(OUTPUT_ROOT, "race_national_origin_religion_results_discrim_bin_norace_national_origin_religioncov_table_colrestricted_reason.csv", sep=""))
# read.csv(paste(OUTPUT_ROOT, "race_national_origin_religion_results_discrim_bin_table_colrestricted_reason.csv", sep=""))
#= read.csv(paste(OUTPUT_ROOT, "race_national_origin_results_discrim_bin_table_colrestricted_reason.csv", sep=""))

# fouldlers: 
#discrim_bin_version_2
#restricted_discrim_reason
#joint_version2

female_results = read.csv(paste(SOURCE_ROOT, "discrim_bin_version_2/female_results_discrim_binrestricted__V2.csv", sep=""))

female_results$Model = c(1, 2, 3, 4, 5, 6, 7)
female_results$subset = rep("female_results", times = nrow(female_results))

national_origin_results = read.csv(paste(SOURCE_ROOT,"discrim_bin_version_2/national_origin_results_discrim_binrestricted__V2.csv", sep=""))

national_origin_results$Model = c(1, 2, 3, 4, 5, 6, 7)
national_origin_results$subset = rep("national_origin_results", times = nrow(national_origin_results))#SOURCE_ROOT =  "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/"
#OUTPUT_ROOT = "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/Results/"


#OUTPUT_ROOT ="/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/Results/"
#SOURCE_ROOT = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/WCE_analysis/"
SOURCE_ROOT = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/Results/Version_2_analysis_results/"
OUTPUT_ROOT = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/Results/Version_2_analysis_results/"



#SOURCE_data_ROOT  = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/"

# read.csv(paste(OUTPUT_ROOT, "race_national_origin_religion_results_discrim_bin_norace_national_origin_religioncov_table_colrestricted_reason.csv", sep=""))
# read.csv(paste(OUTPUT_ROOT, "race_national_origin_religion_results_discrim_bin_table_colrestricted_reason.csv", sep=""))
#= read.csv(paste(OUTPUT_ROOT, "race_national_origin_results_discrim_bin_table_colrestricted_reason.csv", sep=""))

# fouldlers: 
#discrim_bin_version_2
#restricted_discrim_reason
#joint_version2

female_results = read.csv(paste(SOURCE_ROOT, "discrim_bin_version_2/female_results_discrim_binrestricted__V2.csv", sep=""))

female_results$Model = c(1, 2, 3, 4, 5, 6, 7)
female_results$subset = rep("female_results", times = nrow(female_results))

national_origin_results = read.csv(paste(SOURCE_ROOT,"discrim_bin_version_2/national_origin_results_discrim_binrestricted__V2.csv", sep=""))

national_origin_results$Model = c(1, 2, 3, 4, 5, 6, 7)
national_origin_results$subset = rep("national_origin_results", times = nrow(national_origin_results))

race_results= read.csv(paste(SOURCE_ROOT,"discrim_bin_version_2/race_results_discrim_binrestricted__V2.csv", sep=""))

race_results$Model = c(1, 2, 3, 4, 5, 6, 7)
race_results$subset = rep("race_results", times = nrow(race_results))

religion_results = read.csv(paste(SOURCE_ROOT, "discrim_bin_version_2/religion_results_discrim_binrestricted__V2.csv", sep=""))


religion_results$Model = c(1, 2, 3, 4, 5, 6, 7)
religion_results$subset = rep("religion_results", times = nrow(religion_results))


BMI_results = read.csv(paste(SOURCE_ROOT,"discrim_bin_version_2/BMI_results_discrim_binrestricted__V2.csv", sep=""))

BMI_results$Model = c(1, 2, 3, 4, 5, 6, 7)
BMI_results$subset = rep("BMI_results", times = nrow(BMI_results))

BMI_noBMIcov_results = read.csv(paste(SOURCE_ROOT,"discrim_bin_version_2/BMI_results_discrim_bin_noBMIcovrestricted__V2.csv", sep=""))


BMI_noBMIcov_results$Model = c(1, 2, 3, 4, 5, 6, 7)
BMI_noBMIcov_results$subset = rep("BMI_noBMIcov_results", times = nrow(BMI_noBMIcov_results))


#######
#######


#######
#######
#restricted to attributed reason 
#BMI_results_discrim_bin_noBMIcov_table_colrestricted_reason_V2.csv
ageism_results = read.csv(paste(SOURCE_ROOT, "restricted_discrim_reason/age_discrim_bin_reason_noage_V2.csv", sep=""))
ageism_results$Model = c(1, 2, 3, 4, 5, 6, 7)
ageism_results$subset = rep("ageism_results", times = nrow(ageism_results))


#age_discrim_bin_reason_noage_V2_HRs_only.csv
#age_discrim_bin_reason_noage_V2_HRs_only.numbers

#age_discrim_bin_reason_noage_V2_p_values.csv

#age_results_discrim_bin_table_col_reason_noage_V2.csv

#female_results_discrim_bin_table_colrestricted_reason_V2.csv

female_sexism_results = read.csv(paste(SOURCE_ROOT, "restricted_discrim_reason/female_results_discrim_binrestricted_reason_V2.csv", sep=""))
female_sexism_results$Model = c(1, 2, 3, 4, 5, 6, 7)
female_sexism_results$subset = rep("female_sexism_results", times = nrow(female_sexism_results))


#race_national_origin_religion_results_discrim_bin_table_colrestricted_reason_V2.csv
race_nat_origin_religion_discrim_reason_results = read.csv(paste(SOURCE_ROOT, "joint_version2/race_national_origin_religion_results_discrim_binrestricted_reason_V2.csv", sep=""))
race_nat_origin_religion_discrim_reason_results$Model = c(1, 2, 3, 4, 5, 6, 7)
race_nat_origin_religion_discrim_reason_results$subset = rep("race_nat_origin_religion_discrim_reason_results", times = nrow(race_nat_origin_religion_discrim_reason_results))

#race_national_origin_results_discrim_bin_table_colrestricted_reason_V2.csv
race_nat_origin_discrim_reason_results =  read.csv(paste(SOURCE_ROOT, "joint_version2/race_national_origin_results_discrim_binrestricted_reason_V2.csv", sep=""))
race_nat_origin_discrim_reason_results$Model = c(1, 2, 3, 4, 5, 6, 7)
race_nat_origin_discrim_reason_results$subset = rep("race_nat_origin_discrim_reason_results", times = nrow(race_nat_origin_discrim_reason_results))


BMI_weight_discrim_noBMIcov_results = read.csv(paste(SOURCE_ROOT, "restricted_discrim_reason/BMI_results_discrim_bin_noBMIcovrestricted_reason_V2.csv", sep=""))
BMI_weight_discrim_noBMIcov_results$Model = c(1, 2, 3, 4, 5, 6, 7)
BMI_weight_discrim_noBMIcov_results$subset = rep("BMI_weight_discrim_noBMIcov_results", times = nrow(BMI_weight_discrim_noBMIcov_results))

#BMI_results_discrim_bin_table_colrestricted_reason_V2.csv

BMI_weight_discrim_results= read.csv(paste(SOURCE_ROOT, "restricted_discrim_reason/BMI_results_discrim_binrestricted_reason_V2.csv", sep=""))
BMI_weight_discrim_results$Model = c(1, 2, 3, 4, 5, 6, 7)
BMI_weight_discrim_results$subset = rep("BMI_weight_discrim_results", times = nrow(BMI_weight_discrim_results))


#####
#####

# not restricted to a reason:                                   
diabetes_results_discrim_bin = rbind(female_results,
                                     national_origin_results, 
                                     race_results, 
                                     religion_results, 
                                     BMI_results, 
                                     BMI_noBMIcov_results) 


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

write.csv(diabetes_results_discrim_bin, paste(OUTPUT_ROOT, "V2_p_values.csv", sep=""))


diabetes_results_HRs_only = read.csv(paste(OUTPUT_ROOT, "V2_p_values.csv", sep=""))

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

write.csv(diabetes_results_HRs_only, paste(OUTPUT_ROOT, "V2_HRs_only.csv", sep=""))


########
########


diabetes_results_discrim_bin_resitricted = rbind(ageism_results, 
                                                 female_sexism_results, 
                                                 race_nat_origin_discrim_reason_results, 
                                                 race_nat_origin_religion_discrim_reason_results, 
                                                 BMI_weight_discrim_results) 




diabetes_results_discrim_bin_resitricted$SE = (diabetes_results_discrim_bin_resitricted$X95..CI-diabetes_results_discrim_bin_resitricted$X5..CI)/(2*1.96)
diabetes_results_discrim_bin_resitricted$z_value = diabetes_results_discrim_bin_resitricted$hazard.ratio/diabetes_results_discrim_bin_resitricted$SE 
diabetes_results_discrim_bin_resitricted$p_value = exp(-0.717*diabetes_results_discrim_bin_resitricted$z_value -0.416*((diabetes_results_discrim_bin_resitricted$z_value)^2))


#df2[,c(1,3,2,4)]

as.numeric(diabetes_results_discrim_bin_resitricted$hazard.ratio)
as.numeric(diabetes_results_discrim_bin_resitricted$X5..CI)
as.numeric(diabetes_results_discrim_bin_resitricted$X95..CI)
as.numeric(diabetes_results_discrim_bin_resitricted$SE)
as.numeric(diabetes_results_discrim_bin_resitricted$z_value)
as.numeric(diabetes_results_discrim_bin_resitricted$p_value)

diabetes_results_discrim_bin_resitricted = cbind(diabetes_results_discrim_bin_resitricted$subset,
                                                 diabetes_results_discrim_bin_resitricted$Model,
                                                 diabetes_results_discrim_bin_resitricted$hazard.ratio,
                                                 diabetes_results_discrim_bin_resitricted$X5..CI,
                                                 diabetes_results_discrim_bin_resitricted$X95..CI,
                                                 diabetes_results_discrim_bin_resitricted$SE,
                                                 diabetes_results_discrim_bin_resitricted$z_value,
                                                 diabetes_results_discrim_bin_resitricted$p_value)

colnames(diabetes_results_discrim_bin_resitricted) = c("subset", 
                                                       "Model",
                                                       
                                                       "hazard.ratio", 
                                                       "X5..CI",
                                                       "X95..CI", 
                                                       "SE", 
                                                       "z_value",
                                                       "p_value")

write.csv(diabetes_results_discrim_bin_resitricted, paste(OUTPUT_ROOT, "resitricted_V2_p_values.csv", sep=""))


diabetes_results_HRs_only_restricted = read.csv(paste(OUTPUT_ROOT, "resitricted_V2_p_values.csv", sep=""))

head(diabetes_results_HRs_only_restricted)
diabetes_results_HRs_only_restricted$p_value
diabetes_results_HRs_only_restricted$X5..CI = round(diabetes_results_HRs_only_restricted$X5..CI, digits = 2)
diabetes_results_HRs_only_restricted$X95..CI = round(diabetes_results_HRs_only_restricted$X95..CI, digits = 2)
diabetes_results_HRs_only_restricted$hazard.ratio = round(diabetes_results_HRs_only_restricted$hazard.ratio, digits = 2)

head(diabetes_results_HRs_only_restricted)
diabetes_results_HRs_only_restricted$p_value
diabetes_results_HRs_only_restricted$asterix_mark = case_when(diabetes_results_HRs_only_restricted$p_value <=0.05 & diabetes_results_HRs_only_restricted$p_value > 0.01 ~ "*",
                                                              diabetes_results_HRs_only_restricted$p_value <=0.01 &  diabetes_results_HRs_only_restricted$p_value > 0.001 ~ "**",
                                                              diabetes_results_HRs_only_restricted$p_value <=0.001 ~ "***")

diabetes_results_HRs_only_restricted$CI_both = paste("[", diabetes_results_HRs_only_restricted$X5..CI, ";" , diabetes_results_HRs_only_restricted$X95..CI, "]", sep="")


diabetes_results_HRs_only_restricted = cbind(diabetes_results_HRs_only_restricted$subset,
                                             diabetes_results_HRs_only_restricted$Model, 
                                             diabetes_results_HRs_only_restricted$hazard.ratio,
                                             diabetes_results_HRs_only_restricted$asterix_mark,
                                             diabetes_results_HRs_only_restricted$CI_both)

colnames(diabetes_results_HRs_only_restricted) = c("subset", 
                                                   "Model",
                                                   "HR", 
                                                   "p-value level",
                                                   "95% CI")

write.csv(diabetes_results_HRs_only_restricted, paste(OUTPUT_ROOT, "restricted_V2_HRs_only.csv", sep=""))


race_results= read.csv(paste(SOURCE_ROOT,"discrim_bin_version_2/race_results_discrim_binrestricted__V2.csv", sep=""))

race_results$Model = c(1, 2, 3, 4, 5, 6, 7)
race_results$subset = rep("race_results", times = nrow(race_results))

religion_results = read.csv(paste(SOURCE_ROOT, "discrim_bin_version_2/religion_results_discrim_binrestricted__V2.csv", sep=""))


religion_results$Model = c(1, 2, 3, 4, 5, 6, 7)
religion_results$subset = rep("religion_results", times = nrow(religion_results))


BMI_results = read.csv(paste(SOURCE_ROOT,"discrim_bin_version_2/BMI_results_discrim_binrestricted__V2.csv", sep=""))

BMI_results$Model = c(1, 2, 3, 4, 5, 6, 7)
BMI_results$subset = rep("BMI_results", times = nrow(BMI_results))

BMI_noBMIcov_results = read.csv(paste(SOURCE_ROOT,"discrim_bin_version_2/BMI_results_discrim_bin_noBMIcovrestricted__V2.csv", sep=""))


BMI_noBMIcov_results$Model = c(1, 2, 3, 4, 5, 6, 7)
BMI_noBMIcov_results$subset = rep("BMI_noBMIcov_results", times = nrow(BMI_noBMIcov_results))


#######
#######


# not restricted to a reason:                                   
diabetes_results_discrim_bin = rbind(female_results,
                                     national_origin_results, 
                                     race_results, 
                                     religion_results, 
                                     BMI_results, 
                                     BMI_noBMIcov_results) 
                                     
                                     
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

write.csv(diabetes_results_discrim_bin, paste(OUTPUT_ROOT, "V2_p_values.csv", sep=""))


diabetes_results_HRs_only = read.csv(paste(OUTPUT_ROOT, "V2_p_values.csv", sep=""))

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

write.csv(diabetes_results_HRs_only, paste(OUTPUT_ROOT, "V2_HRs_only.csv", sep=""))

