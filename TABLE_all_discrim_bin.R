SOURCE_ROOT =  "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/"
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


TABLE_1 = read.csv(paste(SOURCE_ROOT, "diabetes_results_p_value_discrim_bin_HRs_only.csv", sep=""))

model_1_result = subset(TABLE_1, TABLE_1$Model == 1)
model_2_result = subset(TABLE_1, TABLE_1$Model == 2)
model_3_result = subset(TABLE_1, TABLE_1$Model == 3)
model_4_result = subset(TABLE_1, TABLE_1$Model == 4)
model_5_result = subset(TABLE_1, TABLE_1$Model == 5)
model_6_result = subset(TABLE_1, TABLE_1$Model == 6)
model_7_result = subset(TABLE_1, TABLE_1$Model == 7)
model_8_result = subset(TABLE_1, TABLE_1$Model == 8)
model_9_result = subset(TABLE_1, TABLE_1$Model == 9)
model_10_result = subset(TABLE_1, TABLE_1$Model == 10)
model_11_result = subset(TABLE_1, TABLE_1$Model == 11)
model_12_result = subset(TABLE_1, TABLE_1$Model == 12)
model_13_result = subset(TABLE_1, TABLE_1$Model == 13)
model_14_result = subset(TABLE_1, TABLE_1$Model == 14)
model_15_result = subset(TABLE_1, TABLE_1$Model == 15)
model_16_result = subset(TABLE_1, TABLE_1$Model == 16)
model_17_result = subset(TABLE_1, TABLE_1$Model == 17)

TABLE_1_main_models = cbind(model_1_result,
                            model_2_result,
                            model_3_result, 
                            model_4_result,
               
                            model_7_result,
                            model_9_result,
                      
                            model_12_result,
                            model_13_result,
                            model_14_result,
                            model_15_result,
                            model_16_result,
                            model_17_result)


write.csv(TABLE_1_main_models, paste(OUTPUT_ROOT, "TABLE_1_main_models.csv", sep=""))
