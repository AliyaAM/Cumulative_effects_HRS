OUTPUT_ROOT = "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/Results/"

female_results_discrim_bin = read.csv(paste(OUTPUT_ROOT, "female_results_discrim_bin.csv", sep=""))
female_results_discrim_bin$subset = rep("female", nrow(female_results_discrim_bin))

national_origin_ousideUS_results_discrim_bin = read.csv(paste(OUTPUT_ROOT, "national_origin_results_discrim_bin.csv", sep=""))
national_origin_ousideUS_results_discrim_bin$subset = rep("national origin", nrow(national_origin_ousideUS_results_discrim_bin))

race_results_discrim_bin = read.csv(paste(OUTPUT_ROOT, "race_results_discrim_bin.csv", sep=""))
race_results_discrim_bin$subset = rep("race", nrow(race_results_discrim_bin))

religion_results_discrim_bin = read.csv(paste(OUTPUT_ROOT, "religion_results_discrim_bin.csv", sep=""))
religion_results_discrim_bin$subset = rep("religion", nrow(religion_results_discrim_bin))

BMI_results_discrim_bin = read.csv(paste(OUTPUT_ROOT, "BMI_results_discrim_bin.csv", sep=""))
BMI_results_discrim_bin$subset = rep("BMI", nrow(BMI_results_discrim_bin))


BMI_results_discrim_bin_noBMIcov = read.csv(paste(OUTPUT_ROOT, "BMI_results_discrim_bin_noBMIcov.csv", sep=""))
BMI_results_discrim_bin_noBMIcov$subset = rep("BMI, noBMIcov", nrow(BMI_results_discrim_bin_noBMIcov))


diabetes_results_discrim_bin = rbind(female_results_discrim_bin, 
                                     national_origin_ousideUS_results_discrim_bin, 
                                    race_results_discrim_bin,
                                    religion_results_discrim_bin, 
                                    BMI_results_discrim_bin, 
                                    BMI_results_discrim_bin_noBMIcov)

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
                                    diabetes_results_discrim_bin$hazard.ratio,
                                    diabetes_results_discrim_bin$X5..CI,
                                    diabetes_results_discrim_bin$X95..CI,
                                    diabetes_results_discrim_bin$SE,
                                    diabetes_results_discrim_bin$z_value,
                                    diabetes_results_discrim_bin$p_value)

colnames(diabetes_results_discrim_bin) = c("subset", 
                                  
                                          "hazard.ratio", 
                                          "X5..CI",
                                          "X95..CI", 
                                          "SE", 
                                          "z_value",
                                          "p_value")

write.csv(diabetes_results_discrim_bin, paste(OUTPUT_ROOT, "diabetes_results_p_value_discrim_bin.csv", sep=""))
