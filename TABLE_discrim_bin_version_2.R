SOURCE_ROOT =  "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/"
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/Results/"


diabetes_results_discrim_bin = read.csv(paste(OUTPUT_ROOT, "national_AND_race_results_discrim_bin.csv", sep=""))
diabetes_results_discrim_bin$subset = rep("race, national origin", nrow(diabetes_results_discrim_bin))


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


write.csv(diabetes_results_discrim_bin, paste(OUTPUT_ROOT, "diabetes_results_p_value_discrim_bin_national_race.csv", sep=""))

diabetes_results_HRs_only = diabetes_results_discrim_bin

diabetes_results_discrim_bin$p_value

diabetes_results_HRs_only$X5..CI = round(diabetes_results_HRs_only$X5..CI, digits = 2)
diabetes_results_HRs_only$X95..CI = round(diabetes_results_HRs_only$X95..CI, digits = 2)
diabetes_results_HRs_only$hazard.ratio = round(diabetes_results_HRs_only$hazard.ratio, digits = 2)

diabetes_results_HRs_only$asterix_mark = case_when(diabetes_results_HRs_only$p_value <=0.05 & diabetes_results_HRs_only$p_value > 0.01 ~ "*",
                                                   diabetes_results_HRs_only$p_value <=0.01 &  diabetes_results_HRs_only$p_value > 0.001 ~ "**",
                                                   diabetes_results_HRs_only$p_value <=0.001 ~ "***")
                                                      
diabetes_results_HRs_only$CI_both = paste("[", diabetes_results_HRs_only$X5..CI, ";" , diabetes_results_HRs_only$X95..CI, "]", sep="")


diabetes_results_HRs_only = cbind(diabetes_results_HRs_only$subset,
                                  diabetes_results_HRs_only$hazard.ratio,
                                  diabetes_results_HRs_only$asterix_mark,
                                  diabetes_results_HRs_only$CI_both)

colnames(diabetes_results_HRs_only) = c("subset", 
                                        "HR", 
                                        "p-value level",
                                        "95% CI")

write.csv(diabetes_results_HRs_only, paste(OUTPUT_ROOT, "diabetes_results_national_AND_race_p_value_discrim_bin_HRs_only.csv", sep=""))


diabetes_results_HRs_only$Model = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)

TABLE_1 = read.csv(paste(OUTPUT_ROOT, "diabetes_results_national_AND_race_p_value_discrim_bin_HRs_only.csv", sep=""))

TABLE_1$Model = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)



model_1_result = subset(TABLE_1, TABLE_1$Model == 1)
Model_1 = paste(model_1_result$HR, model_1_result$p.value.level, model_1_result$X95..CI)

model_2_result = subset(TABLE_1, TABLE_1$Model == 2)
Model_2 = paste(model_2_result$HR, model_2_result$p.value.level, model_2_result$X95..CI)

model_3_result = subset(TABLE_1, TABLE_1$Model == 3)
Model_3 = paste(model_3_result$HR, model_3_result$p.value.level, model_3_result$X95..CI)

model_4_result = subset(TABLE_1, TABLE_1$Model == 4)
Model_4 = paste(model_4_result$HR, model_4_result$p.value.level, model_4_result$X95..CI)

model_5_result = subset(TABLE_1, TABLE_1$Model == 5)
Model_5 = paste(model_5_result$HR, model_5_result$p.value.level, model_5_result$X95..CI)

model_6_result = subset(TABLE_1, TABLE_1$Model == 6)
Model_6 = paste(model_6_result$HR, model_6_result$p.value.level, model_6_result$X95..CI)

model_7_result = subset(TABLE_1, TABLE_1$Model == 7)
Model_7 = paste(model_7_result$HR, model_7_result$p.value.level, model_7_result$X95..CI)

model_8_result = subset(TABLE_1, TABLE_1$Model == 8)
Model_8 = paste(model_8_result$HR, model_8_result$p.value.level, model_8_result$X95..CI)

model_9_result = subset(TABLE_1, TABLE_1$Model == 9)
Model_9 = paste(model_9_result$HR, model_9_result$p.value.level, model_9_result$X95..CI)

model_10_result = subset(TABLE_1, TABLE_1$Model == 10)
Model_10 = paste(model_10_result$HR, model_10_result$p.value.level, model_10_result$X95..CI)

model_11_result = subset(TABLE_1, TABLE_1$Model == 11)
Model_11 = paste(model_11_result$HR, model_11_result$p.value.level, model_11_result$X95..CI)

model_12_result = subset(TABLE_1, TABLE_1$Model == 12)
Model_12 = paste(model_12_result$HR, model_12_result$p.value.level, model_12_result$X95..CI)

model_13_result = subset(TABLE_1, TABLE_1$Model == 13)
Model_13 = paste(model_13_result$HR, model_13_result$p.value.level, model_13_result$X95..CI)

model_14_result = subset(TABLE_1, TABLE_1$Model == 14)
Model_14 = paste(model_14_result$HR, model_14_result$p.value.level, model_14_result$X95..CI)

model_15_result = subset(TABLE_1, TABLE_1$Model == 15)
Model_15 = paste(model_15_result$HR, model_15_result$p.value.level, model_15_result$X95..CI)

model_16_result = subset(TABLE_1, TABLE_1$Model == 16)
Model_16 = paste(model_16_result$HR, model_16_result$p.value.level, model_16_result$X95..CI)

model_17_result = subset(TABLE_1, TABLE_1$Model == 17)
Model_17 = paste(model_17_result$HR, model_17_result$p.value.level, model_17_result$X95..CI)


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



TABLE_1_main_models_edited = cbind(Model_1,
                                   Model_2,
                                   Model_3, 
                            Model_4,
                            
                            Model_7,
                            Model_9,
                            
                            Model_12,
                            Model_13,
                            Model_14,
                            Model_15,
                            Model_16,
                            Model_17)

write.csv(TABLE_1_main_models, paste(OUTPUT_ROOT, "TABLE_1_main_models_race_national_origin.csv", sep=""))
write.csv(TABLE_1_main_models_edited, paste(OUTPUT_ROOT, "TABLE_1_main_models_race_national_origin_edited.csv", sep=""))

