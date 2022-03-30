
SOURCE_ROOT = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/Results/binary/"
## Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/Results/binary/"

SOURCE_data_ROOT = "/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"
SOURCE_ROOT =  "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/"
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/Results/"



#BMI_results = read.csv(paste(SOURCE_ROOT, "Results/BMI_all_results_bin_model_3.csv", sep=""))
#BMI_results$subset = rep("Weight, BMI>30 kg/m2", nrow(BMI_results))

#female_all_results = read.csv(paste(SOURCE_ROOT, "Results/female_all_results_bin_model_3.csv", sep=""))
#female_all_results$subset = rep("Female", nrow(female_all_results))

#national_origin_ousideUS_all_results = read.csv(paste(SOURCE_ROOT, "Results/national_origin_ousideUS_all_results_bin_model_3.csv", sep=""))
#national_origin_ousideUS_all_results$subset = c(rep("National origin", nrow(national_origin_ousideUS_all_results)))

#race_all_results = read.csv(paste(SOURCE_ROOT, "Results/race_all_results_bin_model_3.csv", sep=""))
#race_all_results$subset = rep("Ethnic minority", nrow(race_all_results))


#religion_all_results = read.csv(paste(SOURCE_ROOT, "Results/religion_all_results_bin_model_3.csv", sep=""))
#religion_all_results$subset = rep("Religion", nrow(religion_all_results))

#BMI_all_results.csv

#BMI_all_results_bin_model2.csv

# model 4
BMI_all_results_bin_model_4 = read.csv(paste(SOURCE_ROOT, "Results/BMI_all_results_bin_model_4_test.csv", sep="")) 
BMI_all_results_bin_model_4$subset = rep("BMI model 4", nrow(BMI_all_results_bin_model_4)) 

#model 5 NAs
#BMI_all_results_bin_model_5 = read.csv(paste(SOURCE_ROOT, "Results/BMI_all_results_bin_model_5_test.csv", sep="")) 
#BMI_all_results_bin_model_5$subset = rep("BMI model 5", nrow(BMI_all_results_bin_model_5)) 

#model 6
BMI_all_results_bin_model_6 = read.csv(paste(SOURCE_ROOT, "Results/BMI_all_results_bin_model_6_test.csv", sep="")) 
BMI_all_results_bin_model_6$subset = rep("BMI model 6", nrow(BMI_all_results_bin_model_6)) 

#BMI_all_results_bin_model_7_test.csv
BMI_all_results_bin_model_7 = read.csv(paste(SOURCE_ROOT, "Results/BMI_all_results_bin_model_7_test.csv", sep="")) 
BMI_all_results_bin_model_7$subset = rep("BMI model 7", nrow(BMI_all_results_bin_model_7)) 

#BMI_all_results_bin_model_8_test.csv
BMI_all_results_bin_model_8 = read.csv(paste(SOURCE_ROOT, "Results/BMI_all_results_bin_model_8_test.csv", sep="")) 
BMI_all_results_bin_model_8$subset = rep("BMI model 8", nrow(BMI_all_results_bin_model_8)) 

#BMI_all_results_bin_model_9_test.csv
BMI_all_results_bin_model_9 = read.csv(paste(SOURCE_ROOT, "Results/BMI_all_results_bin_model_8_test.csv", sep="")) 
BMI_all_results_bin_model_9$subset = rep("BMI model 9", nrow(BMI_all_results_bin_model_9)) 

#BMI_all_results_bin_model_10_test.csv
BMI_all_results_bin_model_10 = read.csv(paste(SOURCE_ROOT, "Results/BMI_all_results_bin_model_10_test.csv", sep="")) 
BMI_all_results_bin_model_10$subset = rep("BMI model 10", nrow(BMI_all_results_bin_model_10)) 

#BMI_all_results_bin_model_11_test.csv NAs 
BMI_all_results_bin_model_11 = read.csv(paste(SOURCE_ROOT, "Results/BMI_all_results_bin_model_11_test.csv", sep="")) 
BMI_all_results_bin_model_11$subset = rep("BMI model 11", nrow(BMI_all_results_bin_model_11)) 

#BMI_all_results_bin_model_12_test.csv NAs
BMI_all_results_bin_model_12 = read.csv(paste(SOURCE_ROOT, "Results/BMI_all_results_bin_model_12_test.csv", sep="")) 
BMI_all_results_bin_model_12$subset = rep("BMI model 12", nrow(BMI_all_results_bin_model_12)) 

#BMI_all_results_bin_model_13_test.csv
BMI_all_results_bin_model_13 = read.csv(paste(SOURCE_ROOT, "Results/BMI_all_results_bin_model_13_test.csv", sep="")) 
BMI_all_results_bin_model_13$subset = rep("BMI model 13", nrow(BMI_all_results_bin_model_13)) 

#BMI_all_results_bin_model_14_test.csv
BMI_all_results_bin_model_14 = read.csv(paste(SOURCE_ROOT, "Results/BMI_all_results_bin_model_14_test.csv", sep="")) 
BMI_all_results_bin_model_14$subset = rep("BMI model 14", nrow(BMI_all_results_bin_model_14)) 

#BMI_all_results_bin_model_15_test.csv
BMI_all_results_bin_model_15 = read.csv(paste(SOURCE_ROOT, "Results/BMI_all_results_bin_model_15_test.csv", sep="")) 
BMI_all_results_bin_model_15$subset = rep("BMI model 15", nrow(BMI_all_results_bin_model_15)) 

#BMI_all_results_bin_model_16_test.csv
BMI_all_results_bin_model_16 = read.csv(paste(SOURCE_ROOT, "Results/BMI_all_results_bin_model_16_test.csv", sep="")) 
BMI_all_results_bin_model_16$subset = rep("BMI model 16", nrow(BMI_all_results_bin_model_16)) 

#BMI_all_results_bin_model_17_test.csv NAs
BMI_all_results_bin_model_17 = read.csv(paste(SOURCE_ROOT, "Results/BMI_all_results_bin_model_17_test.csv", sep="")) 
BMI_all_results_bin_model_17$subset = rep("BMI model 17", nrow(BMI_all_results_bin_model_17)) 


BMI_results_model_main = rbind(BMI_all_results_bin_model_4, 
                               #BMI_all_results_bin_model_5, 
                               #BMI_all_results_bin_model_6, 
                               BMI_all_results_bin_model_7, 
                               #BMI_all_results_bin_model_8, 
                               BMI_all_results_bin_model_9, 
                               #BMI_all_results_bin_model_10, 
                               #BMI_all_results_bin_model_11, 
                               #BMI_all_results_bin_model_12, 
                               BMI_all_results_bin_model_13, 
                               BMI_all_results_bin_model_14, 
                               #BMI_all_results_bin_model_15, 
                               BMI_all_results_bin_model_16) 
#BMI_all_results_bin_model_17)

#BMI_all_results_depression.csv
#BMI_all_results_p_values.xlsx
#BMI_all_results_recoded_1vs0.csv
#BMI_stats
#female_all_results.csv
#female_all_results_bin_model2.csv

###########################
#female_all_results_bin_model_4_test.csv NAs
female_all_results_bin_model_4 = read.csv(paste(SOURCE_ROOT, "Results/female_all_results_bin_model_4_test.csv", sep="")) 
female_all_results_bin_model_4$subset = rep("Female model 4", nrow(female_all_results_bin_model_4)) 


#female_model_5
#female_all_results_bin_model_6_test.csv
female_all_results_bin_model_6 = read.csv(paste(SOURCE_ROOT, "Results/female_all_results_bin_model_6_test.csv", sep="")) 
female_all_results_bin_model_6$subset = rep("Female model 6", nrow(female_all_results_bin_model_6)) 

#female_all_results_bin_model_7_test.csv
female_all_results_bin_model_7 = read.csv(paste(SOURCE_ROOT, "Results/female_all_results_bin_model_7_test.csv", sep="")) 
female_all_results_bin_model_7$subset = rep("Female model 7", nrow(female_all_results_bin_model_7)) 

#female_all_results_bin_model_8_test.csv
female_all_results_bin_model_8 = read.csv(paste(SOURCE_ROOT, "Results/female_all_results_bin_model_8_test.csv", sep="")) 
female_all_results_bin_model_8$subset = rep("Female model 8", nrow(female_all_results_bin_model_8)) 

#female_all_results_bin_model_9_test.csv
female_all_results_bin_model_9 = read.csv(paste(SOURCE_ROOT, "Results/female_all_results_bin_model_9_test.csv", sep="")) 
female_all_results_bin_model_9$subset = rep("Female model 9", nrow(female_all_results_bin_model_9)) 

#female_all_results_bin_model_10_test.csv
female_all_results_bin_model_10 = read.csv(paste(SOURCE_ROOT, "Results/female_all_results_bin_model_10_test.csv", sep="")) 
female_all_results_bin_model_10$subset = rep("Female model 10", nrow(female_all_results_bin_model_10)) 

#female_all_results_bin_model_11_test.csv NAs
female_all_results_bin_model_11 = read.csv(paste(SOURCE_ROOT, "Results/female_all_results_bin_model_11_test.csv", sep="")) 
female_all_results_bin_model_11$subset = rep("Female model 11", nrow(female_all_results_bin_model_11)) 

#female_all_results_bin_model_12_test.csv NAs
female_all_results_bin_model_12 = read.csv(paste(SOURCE_ROOT, "Results/female_all_results_bin_model_12_test.csv", sep="")) 
female_all_results_bin_model_12$subset = rep("Female model 12", nrow(female_all_results_bin_model_12)) 

#female_all_results_bin_model_13_test.csv
female_all_results_bin_model_13 = read.csv(paste(SOURCE_ROOT, "Results/female_all_results_bin_model_13_test.csv", sep="")) 
female_all_results_bin_model_13$subset = rep("Female model 13", nrow(female_all_results_bin_model_13)) 

#female_all_results_bin_model_14_test.csv
female_all_results_bin_model_14 = read.csv(paste(SOURCE_ROOT, "Results/female_all_results_bin_model_14_test.csv", sep="")) 
female_all_results_bin_model_14$subset = rep("Female model 14", nrow(female_all_results_bin_model_14)) 

#female_all_results_bin_model_15_test.csv
female_all_results_bin_model_15 = read.csv(paste(SOURCE_ROOT, "Results/female_all_results_bin_model_15_test.csv", sep="")) 
female_all_results_bin_model_15$subset = rep("Female model 15", nrow(female_all_results_bin_model_15)) 

#female_all_results_bin_model_16_test.csv
female_all_results_bin_model_16 = read.csv(paste(SOURCE_ROOT, "Results/female_all_results_bin_model_16_test.csv", sep="")) 
female_all_results_bin_model_16$subset = rep("Female model 16", nrow(female_all_results_bin_model_16)) 

#female_all_results_bin_model_17_test.csv NAs
female_all_results_bin_model_17 = read.csv(paste(SOURCE_ROOT, "Results/female_all_results_bin_model_17_test.csv", sep="")) 
female_all_results_bin_model_17$subset = rep("Female model 17", nrow(female_all_results_bin_model_17))



female_results_model_main = rbind(female_all_results_bin_model_4,
                                  #female_all_results_bin_model_5, 
                                  #female_all_results_bin_model_6, 
                                  female_all_results_bin_model_7, 
                                  #female_all_results_bin_model_8, 
                                  female_all_results_bin_model_9, 
                                  #female_all_results_bin_model_10, 
                                  #female_all_results_bin_model_11, 
                                  #female_all_results_bin_model_12, 
                                  female_all_results_bin_model_13, 
                                  female_all_results_bin_model_14, 
                                  #female_all_results_bin_model_15, 
                                  female_all_results_bin_model_16) 
#female_all_results_bin_model_17)

###########################
#national_origin_ousideUS_all_results.csv

#national_origin_ousideUS_bin_all_results_bin_model_2_29march_test.csv
#national_origin_ousideUS_all_results_bin_model2.csv
#national_origin_ousideUS_all_results_bin_model_3.csv

#national_origin_ousideUS_all_results_bin_model_4_test.csv
national_origin_ousideUS_all_results_bin_model_4 = read.csv(paste(SOURCE_ROOT, "Results/national_origin_ousideUS_all_results_bin_model_4_test.csv", sep="")) 
national_origin_ousideUS_all_results_bin_model_4$subset = rep("national origin 4", nrow(national_origin_ousideUS_all_results_bin_model_4))

#national_origin_model_5

#national_origin_all_results_bin_model_6_test.csv
national_origin_ousideUS_all_results_bin_model_6 = read.csv(paste(SOURCE_ROOT, "Results/national_origin_all_results_bin_model_6_test.csv", sep="")) 
national_origin_ousideUS_all_results_bin_model_6$subset = rep("national origin 6", nrow(national_origin_ousideUS_all_results_bin_model_6))

#national_origin_ousideUS_all_results_bin_model_7_test.csv
national_origin_ousideUS_all_results_bin_model_7 = read.csv(paste(SOURCE_ROOT, "Results/national_origin_ousideUS_all_results_bin_model_7_test.csv", sep="")) 
national_origin_ousideUS_all_results_bin_model_7$subset = rep("national origin 7", nrow(national_origin_ousideUS_all_results_bin_model_7))

#national_origin_ousideUS_all_results_bin_model_8_test.csv
national_origin_ousideUS_all_results_bin_model_8 = read.csv(paste(SOURCE_ROOT, "Results/national_origin_ousideUS_all_results_bin_model_8_test.csv", sep="")) 
national_origin_ousideUS_all_results_bin_model_8$subset = rep("national origin 8", nrow(national_origin_ousideUS_all_results_bin_model_8))

#national_origin_ousideUS_bin_all_results_bin_model_9_test.csv
national_origin_ousideUS_all_results_bin_model_9 = read.csv(paste(SOURCE_ROOT, "Results/national_origin_ousideUS_bin_all_results_bin_model_9_test.csv", sep="")) 
national_origin_ousideUS_all_results_bin_model_9$subset = rep("national origin 9", nrow(national_origin_ousideUS_all_results_bin_model_9))

#NA small sample model 10 
national_origin_ousideUS_all_results_bin_model_10 = read.csv(paste(SOURCE_ROOT, "Results/national_origin_ousideUS_bin_all_results_bin_model_10_test.csv", sep="")) 
national_origin_ousideUS_all_results_bin_model_10$subset = rep("national origin 10", nrow(national_origin_ousideUS_all_results_bin_model_10))

#NA small sample model 11 
#NA small sample model 12
#national_origin_ousideUS_all_results_bin_model_13_test.csv
national_origin_ousideUS_all_results_bin_model_13 = read.csv(paste(SOURCE_ROOT, "Results/national_origin_ousideUS_all_results_bin_model_13_test.csv", sep="")) 
national_origin_ousideUS_all_results_bin_model_13$subset = rep("national origin 13", nrow(national_origin_ousideUS_all_results_bin_model_13))

#national_origin_ousideUS_all_results_bin_model_14_test.csv
national_origin_ousideUS_all_results_bin_model_14 = read.csv(paste(SOURCE_ROOT, "Results/national_origin_ousideUS_all_results_bin_model_14_test.csv", sep="")) 
national_origin_ousideUS_all_results_bin_model_14$subset = rep("national origin 14", nrow(national_origin_ousideUS_all_results_bin_model_14))

#model 15  
national_origin_ousideUS_all_results_bin_model_15 = read.csv(paste(SOURCE_ROOT, "Results/national_origin_ousideUS_bin_all_results_bin_model_15_test.csv", sep="")) 
national_origin_ousideUS_all_results_bin_model_15$subset = rep("national origin 15", nrow(national_origin_ousideUS_all_results_bin_model_15))

#national_origin_ousideUS_bin_all_results_bin_model_16_test.csv
national_origin_ousideUS_all_results_bin_model_16 = read.csv(paste(SOURCE_ROOT, "Results/national_origin_ousideUS_bin_all_results_bin_model_16_test.csv", sep="")) 
national_origin_ousideUS_all_results_bin_model_16$subset = rep("national origin 16", nrow(national_origin_ousideUS_all_results_bin_model_16))

#national origin model 17 

national_origin_ousideUS_results_model_main = rbind(national_origin_ousideUS_all_results_bin_model_4,
                                                    #national_origin_ousideUS_all_results_bin_model_5, 
                                                    #national_origin_ousideUS_all_results_bin_model_6, 
                                                    national_origin_ousideUS_all_results_bin_model_7, 
                                                    #national_origin_ousideUS_all_results_bin_model_8, 
                                                    national_origin_ousideUS_all_results_bin_model_9, 
                                                    #national_origin_ousideUS_all_results_bin_model_10, 
                                                    #national_origin_ousideUS_all_results_bin_model_11, 
                                                    #national_origin_ousideUS_all_results_bin_model_12, 
                                                    national_origin_ousideUS_all_results_bin_model_13, 
                                                    national_origin_ousideUS_all_results_bin_model_14, 
                                                    #national_origin_ousideUS_all_results_bin_model_15, 
                                                    national_origin_ousideUS_all_results_bin_model_16) 
#female_all_results_bin_model_17)

ncol(national_origin_ousideUS_results_model_main)
#############################################
#race_all_results.csv
#race_all_results_bin_model2.csv

#race_all_results_bin_model_4.csv
race_all_results_bin_model_4 = read.csv(paste(SOURCE_ROOT, "Results/race_all_results_bin_model_4.csv", sep="")) 
race_all_results_bin_model_4$subset = rep("race model 4", nrow(race_all_results_bin_model_4))
ncol(race_all_results_bin_model_4)
#race_model_5

#race_all_results_bin_model_6_test.csv
race_all_results_bin_model_6 = read.csv(paste(SOURCE_ROOT, "Results/race_all_results_bin_model_6_test.csv", sep="")) 
race_all_results_bin_model_6$subset = rep("race model 6", nrow(race_all_results_bin_model_6))
ncol(race_all_results_bin_model_6)

#race_all_results_bin_model_7_test.csv
race_all_results_bin_model_7 = read.csv(paste(SOURCE_ROOT, "Results/race_all_results_bin_model_7_test.csv", sep="")) 
race_all_results_bin_model_7$subset = rep("race model 7", nrow(race_all_results_bin_model_7))
ncol(race_all_results_bin_model_7)

#race_all_results_bin_model_8_test.csv
race_all_results_bin_model_8 = read.csv(paste(SOURCE_ROOT, "Results/race_all_results_bin_model_8_test.csv", sep="")) 
race_all_results_bin_model_8$subset = rep("race model 8", nrow(race_all_results_bin_model_8))
ncol(race_all_results_bin_model_8)

#race_all_results_bin_model_9_test.csv
race_all_results_bin_model_9 = read.csv(paste(SOURCE_ROOT, "Results/race_all_results_bin_model_9_test.csv", sep="")) 
race_all_results_bin_model_9$subset = rep("race model 9", nrow(race_all_results_bin_model_9))
ncol(race_all_results_bin_model_9)

#race_all_results_bin_model_10_test.csv
race_all_results_bin_model_10 = read.csv(paste(SOURCE_ROOT, "Results/race_all_results_bin_model_10_test.csv", sep="")) 
race_all_results_bin_model_10$subset = rep("race model 10", nrow(race_all_results_bin_model_10))
ncol(race_all_results_bin_model_10)

#race_all_results_bin_model_11_test.csv
race_all_results_bin_model_11 = read.csv(paste(SOURCE_ROOT, "Results/race_all_results_bin_model_11_test.csv", sep="")) 
race_all_results_bin_model_11$subset = rep("race model 11", nrow(race_all_results_bin_model_11))
ncol(race_all_results_bin_model_11)

#race_all_results_bin_model_12_test.csv
race_all_results_bin_model_12 = read.csv(paste(SOURCE_ROOT, "Results/race_all_results_bin_model_12_test.csv", sep="")) 
race_all_results_bin_model_12$subset = rep("race model 12", nrow(race_all_results_bin_model_12))
ncol(race_all_results_bin_model_12)

#race_all_results_bin_model_13_test.csv
race_all_results_bin_model_13 = read.csv(paste(SOURCE_ROOT, "Results/race_all_results_bin_model_13_test.csv", sep="")) 
race_all_results_bin_model_13$subset = rep("race model 13", nrow(race_all_results_bin_model_13))
ncol(race_all_results_bin_model_13)

#race_all_results_bin_model_14_test.csv
race_all_results_bin_model_14 = read.csv(paste(SOURCE_ROOT, "Results/race_all_results_bin_model_14_test.csv", sep="")) 
race_all_results_bin_model_14$subset = rep("race model 14", nrow(race_all_results_bin_model_14))
ncol(race_all_results_bin_model_14)

#race_model_15 ADD
#race_all_results_bin_model_15_test.csv
race_all_results_bin_model_15 = read.csv(paste(SOURCE_ROOT, "Results/race_all_results_bin_model_15_test.csv", sep="")) 
race_all_results_bin_model_15$subset = rep("race model 15", nrow(race_all_results_bin_model_15))
ncol(race_all_results_bin_model_15)

#race_all_results_bin_model_16_test.csv
race_all_results_bin_model_16 = read.csv(paste(SOURCE_ROOT, "Results/race_all_results_bin_model_16_test.csv", sep="")) 
race_all_results_bin_model_16$subset = rep("race model 16", nrow(race_all_results_bin_model_16))
ncol(race_all_results_bin_model_16)

#race_model_17
race_results_model_main = rbind(race_all_results_bin_model_4,
                                #race_all_results_bin_model_5, 
                                #race_all_results_bin_model_6, 
                                race_all_results_bin_model_7, 
                                #race_all_results_bin_model_8, 
                                race_all_results_bin_model_9, 
                                #race_all_results_bin_model_10, 
                                #race_all_results_bin_model_11, 
                                #race_all_results_bin_model_12, 
                                race_all_results_bin_model_13, 
                                race_all_results_bin_model_14, 
                                #race_all_results_bin_model_15, 
                                race_all_results_bin_model_16) 
#race_all_results_bin_model_17)
ncol(race_results_model_main)
#############################################

#race_all_results_bin_model_4.csv
religion_all_results_bin_model_4 = read.csv(paste(SOURCE_ROOT, "Results/religion_all_results_bin_model_4_test.csv", sep="")) 
religion_all_results_bin_model_4$subset = rep("religion model 4", nrow(religion_all_results_bin_model_4))

#religion_model_5

#religion_all_results_bin_model_6_test.csv
religion_all_results_bin_model_6 = read.csv(paste(SOURCE_ROOT, "Results/religion_all_results_bin_model_6_test.csv", sep="")) 
religion_all_results_bin_model_6$subset = rep("religion model 6", nrow(religion_all_results_bin_model_6))

#religion_all_results_bin_model_7_test.csv
religion_all_results_bin_model_7 = read.csv(paste(SOURCE_ROOT, "Results/religion_all_results_bin_model_7_test.csv", sep="")) 
religion_all_results_bin_model_7$subset = rep("religion model 7", nrow(religion_all_results_bin_model_7))

#religion_all_results_bin_model_8_test.csv
religion_all_results_bin_model_8 = read.csv(paste(SOURCE_ROOT, "Results/religion_all_results_bin_model_8_test.csv", sep="")) 
religion_all_results_bin_model_8$subset = rep("religion model 8", nrow(religion_all_results_bin_model_8))

#religion_all_results_bin_model_9_test.csv
religion_all_results_bin_model_9 = read.csv(paste(SOURCE_ROOT, "Results/religion_all_results_bin_model_9_test.csv", sep="")) 
religion_all_results_bin_model_9$subset = rep("religion model 9", nrow(religion_all_results_bin_model_9))

#religion_all_results_bin_model_10_test.csv
religion_all_results_bin_model_10 = read.csv(paste(SOURCE_ROOT, "Results/religion_all_results_bin_model_10_test.csv", sep="")) 
religion_all_results_bin_model_10$subset = rep("religion model 10", nrow(religion_all_results_bin_model_10))

#religion_all_results_bin_model_11_test.csv
religion_all_results_bin_model_11 = read.csv(paste(SOURCE_ROOT, "Results/religion_all_results_bin_model_11_test.csv", sep="")) 
religion_all_results_bin_model_11$subset = rep("religion model 11", nrow(religion_all_results_bin_model_11))

#religion_all_results_bin_model_12_test.csv
religion_all_results_bin_model_12 = read.csv(paste(SOURCE_ROOT, "Results/religion_all_results_bin_model_12_test.csv", sep="")) 
religion_all_results_bin_model_12$subset = rep("religion model 12", nrow(religion_all_results_bin_model_12))

#religion_all_results_bin_model_13_test.csv
religion_all_results_bin_model_13 = read.csv(paste(SOURCE_ROOT, "Results/religion_all_results_bin_model_13_test.csv", sep="")) 
religion_all_results_bin_model_13$subset = rep("religion model 13", nrow(religion_all_results_bin_model_13))

#religion_all_results_bin_model_14_test.csv
religion_all_results_bin_model_14 = read.csv(paste(SOURCE_ROOT, "Results/religion_all_results_bin_model_14_test.csv", sep="")) 
religion_all_results_bin_model_14$subset = rep("religion model 14", nrow(religion_all_results_bin_model_14))

#religion_model_15 ADD
#religion_all_results_bin_model_15_test.csv
religion_all_results_bin_model_15 = read.csv(paste(SOURCE_ROOT, "Results/religion_all_results_bin_model_15_test.csv", sep="")) 
religion_all_results_bin_model_15$subset = rep("religion model 15", nrow(religion_all_results_bin_model_15))

#religion_all_results_bin_model_16_test.csv
religion_all_results_bin_model_16 = read.csv(paste(SOURCE_ROOT, "Results/religion_all_results_bin_model_16_test.csv", sep="")) 
religion_all_results_bin_model_16$subset = rep("religion model 16", nrow(religion_all_results_bin_model_16))
#religion_model_17



religion_results_model_main = rbind(religion_all_results_bin_model_4,
                                    #religion_all_results_bin_model_5, 
                                    #religion_all_results_bin_model_6, 
                                    religion_all_results_bin_model_7, 
                                    #religion_all_results_bin_model_8, 
                                    religion_all_results_bin_model_9, 
                                    #religion_all_results_bin_model_10, 
                                    #religion_all_results_bin_model_11, 
                                    #religion_all_results_bin_model_12, 
                                    religion_all_results_bin_model_13, 
                                    religion_all_results_bin_model_14, 
                                    #religion_all_results_bin_model_15, 
                                    religion_all_results_bin_model_16) 
#religion_all_results_bin_model_17)
ncol(religion_results_model_main)

#############
#############



#race_all_results_depression.csv
#race_all_results_recoded_1vs0.csv
#religion_afraid_others_age_stats.csv
#religion_all_results.csv
#religion_all_results_depression.csv
#religion_all_results_recoded_1vs0.csv


#female_all_results_depression.csv
#female_all_results_p_value.xlsx
#female_all_results_recoded_1vs0.csv
#female_race_all_results.csv
#lim_cond_all_results.csv

#lim_cond_all_results_bin_model2.csv
#lim_cond_all_results_bin_model_4.csv
#lim_cond_all_results_bin_model_4_test.csv
#lim_cond_all_results_recoded.csv
#lim_cond_all_results_recoded_1vs0.csv
#lim_cond_all_results.csv

#national_origin_ousideUS_all_results_recoded_1vs0.csv
# intersectional 

#female_race_all_results.csv


#Steps to obtain the P value from the CI for an estimate of effect (Est)
#If the upper and lower limits of a 95% CI are u and l respectively:
#1 calculate the standard error: SE = (u − l)/(2×1.96)
#2 calculate the test statistic: z = Est/SE
#3 calculate the P value2: P = exp(−0.717×z − 0.416×z2)

diabetes_results_models_main = rbind(female_results_model_main, 
                                    race_results_model_main,
                                    national_origin_ousideUS_results_model_main, 
                                    religion_results_model_main, 
                                    BMI_results_model_main)

diabetes_results_models_main$SE = (diabetes_results_models_main$X95..CI-diabetes_results_models_main$X5..CI)/(2*1.96)
diabetes_results_models_main$z_value = diabetes_results_models_main$hazard.ratio/diabetes_results_models_main$SE 
diabetes_results_models_main$p_value = exp(-0.717*diabetes_results_models_main$z_value -0.416*((diabetes_results_models_main$z_value)^2))


#df2[,c(1,3,2,4)]

as.numeric(diabetes_results_models_main$hazard.ratio)
as.numeric(diabetes_results_models_main$X5..CI)
as.numeric(diabetes_results_models_main$X95..CI)
as.numeric(diabetes_results_models_main$SE)
as.numeric(diabetes_results_models_main$z_value)
as.numeric(diabetes_results_models_main$p_value)

diabetes_results_models_main = cbind(diabetes_results_models_main$subset,
                                    diabetes_results_models_main$variable,
                                    diabetes_results_models_main$hazard.ratio,
                                    diabetes_results_models_main$X5..CI,
                                    diabetes_results_models_main$X95..CI,
                                    diabetes_results_models_main$SE,
                                    diabetes_results_models_main$z_value,
                                    diabetes_results_models_main$p_value)

colnames(diabetes_results_models_main) = c("subset", 
                                          "variable", 
                                          "hazard.ratio", 
                                          "X5..CI",
                                          "X95..CI", 
                                          "SE", 
                                          "z_value",
                                          "p_value")

write.csv(diabetes_results_models_main, paste(OUTPUT_ROOT, "diabetes_results_p_value_models_main.csv", sep=""))


# Model 1: age
# Model 2: age and BMI 
# Model 3: age, BMI, wealth,  (1)
# Model 4: age, BMI, hypertension  (2)
# Model 5: age, BMI, hypertension, wealth 
# Model 6: smoking, physical activity, alcohol consumption
# Model 7: age, BMI,  smoking, physical activity, alcohol consumption  (3)
# Model 8:age, BMI, smoking, physical activity, alcohol consumption, hypertension
# Model 9: age, BMI, CVD  (4)
# Model 10: age, BMI,CVD, hypertension 
# Model 11: age, BMI, smoking, physical activity, alcohol consumption, CVD  
# Model 12: age, BMI, smoking, physical activity, alcohol consumption, CVD, hypertension  (5)
# Model 13: age, BMI, depression  (6)
# Model 14: age, BMI, smoking, physical activity, alcohol consumption, depression (7)
# Model 15: age, BMI, hypertension, depression 
# Model 16: age, BMI,CVD, hypertension, depression 
# Model 17: age, BMI, smoking, physical activity, alcohol consumption, CVD, hypertension, depression  (8)

####################

### demographics alone 
# Model 3: age, BMI, wealth,  (1)
### demographics and hypertention  
# Model 4: age, BMI, hypertension  (2)
### demographics and health behaviours   
# Model 7: age, BMI,  smoking, physical activity, alcohol consumption  (3)
### demographics and CVD
# Model 9: age, BMI, CVD  (4)
### demographics, hypertension, CVD and health behaviours  
# Model 12: age, BMI, smoking, physical activity, alcohol consumption, CVD, hypertension  (5)
### demographics and depression 
# Model 13: age, BMI, depression  (6)
### demographics, health behaviours, and depression 
# Model 14: age, BMI, smoking, physical activity, alcohol consumption, depression (7)
### demographics, CVD, hypertension, health behaviours, and depression 
# Model 17: age, BMI, smoking, physical activity, alcohol consumption, CVD, hypertension, depression  (8)

