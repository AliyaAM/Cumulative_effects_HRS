
library("dplyr")

SOURCE_ROOT = "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/Results/situations/"


#import raw output data from the analysis
harrased_1 = read.csv(paste(SOURCE_ROOT, "Model1_discrim_harassed_bin_cat.csv", sep = ""))
harrased_2 =read.csv(paste(SOURCE_ROOT,"Model_2_discrim_harassed_bin_results.csv", sep = ""))
harrased_3 =read.csv(paste(SOURCE_ROOT,"Model_3_discrim_harassed_bin_results.csv", sep = ""))
harrased_4 =read.csv(paste(SOURCE_ROOT,"Model_4_discrim_harassed_bin_results.csv", sep = ""))
harrased_5 =read.csv(paste(SOURCE_ROOT,"Model_5_discrim_harassed_bin_results.csv", sep = ""))
               
lessrespect_1 = read.csv(paste(SOURCE_ROOT,"Model1_discrim_lessrespect_bin_cat.csv", sep = ""))
lessrespect_2 = read.csv(paste(SOURCE_ROOT,"Model_2_discrim_lessrespect_bin_results.csv", sep = ""))
lessrespect_3 = read.csv(paste(SOURCE_ROOT,"Model_3_discrim_lessrespect_bin_results.csv", sep = ""))
lessrespect_4 = read.csv(paste(SOURCE_ROOT,"Model_4_discrim_lessrespect_bin_results.csv", sep = ""))
lessrespect_5 = read.csv(paste(SOURCE_ROOT,"Model_5_discrim_lessrespect_bin_results.csv", sep = ""))

medical_1 = read.csv(paste(SOURCE_ROOT,"Model1_discrim_medical_bin_cat.csv", sep = ""))
medical_2 = read.csv(paste(SOURCE_ROOT,"Model_2_discrim_medical_bin_results.csv", sep = ""))
medical_3 = read.csv(paste(SOURCE_ROOT,"Model_3_discrim_medical_bin_results.csv", sep = ""))
medical_4 = read.csv(paste(SOURCE_ROOT,"Model_4_discrim_medical_bin_results.csv", sep = ""))
medical_5 = read.csv(paste(SOURCE_ROOT,"Model_5_discrim_medical_bin_results.csv", sep = ""))

notclever_1 = read.csv(paste(SOURCE_ROOT,"Model1_discrim_notclever_bin_cat.csv", sep = ""))
notclever_2 = read.csv(paste(SOURCE_ROOT,"Model_2_discrim_notclever_bin_results.csv", sep = ""))
notclever_3 = read.csv(paste(SOURCE_ROOT,"Model_3_discrim_notclever_bin_results.csv", sep = ""))
notclever_4 = read.csv(paste(SOURCE_ROOT,"Model_4_discrim_notclever_bin_results.csv", sep = ""))
notclever_5 = read.csv(paste(SOURCE_ROOT,"Model_5_discrim_notclever_bin_results.csv", sep = ""))

poorerservice_1 = read.csv(paste(SOURCE_ROOT,"Model1_discrim_poorerservice_bin_cat.csv", sep = ""))
poorerservice_2 = read.csv(paste(SOURCE_ROOT,"Model_2_discrim_poorerservice_bin_results.csv", sep = ""))
poorerservice_3 = read.csv(paste(SOURCE_ROOT,"Model_3_discrim_poorerservice_bin_results.csv", sep = ""))
poorerservice_4 = read.csv(paste(SOURCE_ROOT,"Model_4_discrim_poorerservice_bin_results.csv", sep = ""))
poorerservice_5 = read.csv(paste(SOURCE_ROOT,"Model_5_discrim_poorerservice_bin_results.csv", sep = ""))


#select relevent columns 
harrased_1 
harrased_2 = dplyr::select(harrased_2, coef, lower_CI, upper_CI, p_value) 
harrased_3 = dplyr::select(harrased_3, coef, lower_CI, upper_CI, p_value) 
harrased_4 = dplyr::select(harrased_4, coef, lower_CI, upper_CI, p_value) 
harrased_5 = dplyr::select(harrased_5, coef, lower_CI, upper_CI, p_value) 


lessrespect_1
lessrespect_2 = dplyr::select(lessrespect_2, coef, lower_CI, upper_CI, p_value) 
lessrespect_3 = dplyr::select(lessrespect_3, coef, lower_CI, upper_CI, p_value) 
lessrespect_4 = dplyr::select(lessrespect_4, coef, lower_CI, upper_CI, p_value) 
lessrespect_5 = dplyr::select(lessrespect_5, coef, lower_CI, upper_CI, p_value) 


medical_1
medical_2 = dplyr::select(medical_2, coef, lower_CI, upper_CI, p_value) 
medical_3 = dplyr::select(medical_3, coef, lower_CI, upper_CI, p_value) 
medical_4 = dplyr::select(medical_4, coef, lower_CI, upper_CI, p_value) 
medical_5 = dplyr::select(medical_5, coef, lower_CI, upper_CI, p_value) 

notclever_1
notclever_2 = dplyr::select(notclever_2, coef, lower_CI, upper_CI, p_value) 
notclever_3 = dplyr::select(notclever_3, coef, lower_CI, upper_CI, p_value) 
notclever_4 = dplyr::select(notclever_4, coef, lower_CI, upper_CI, p_value) 
notclever_5 = dplyr::select(notclever_5, coef, lower_CI, upper_CI, p_value) 

poorerservice_1
poorerservice_2 = dplyr::select(poorerservice_2, coef, lower_CI, upper_CI, p_value) 
poorerservice_3 = dplyr::select(poorerservice_3, coef, lower_CI, upper_CI, p_value) 
poorerservice_4 = dplyr::select(poorerservice_4, coef, lower_CI, upper_CI, p_value) 
poorerservice_5 = dplyr::select(poorerservice_5, coef, lower_CI, upper_CI, p_value) 




#writing tables (function)
table_writing_function = function(data){
  coef = round(data$coef, digits = 2)
  
  data_output = data.frame(coef)
  
  data_output$asterix_mark = case_when(data$p_value <=0.05 & data$p_value > 0.01 ~ "*",
                                       data$p_value <=0.01 &  data$p_value > 0.001 ~ "**",
                                       data$p_value <=0.001 ~ "***")
  
  
  data$lower_CI = round(data$lower_CI, digits = 2)
  data$upper_CI = round(data$upper_CI, digits = 2)
  
  
  data_output$CI_both = paste("[", data$lower_CI, ";" , data$upper_CI, "]", sep="")
  
  
  data_output$p_value = round(data$p_value, digits = 4)
  
  return(data_output)
  
}


#write tables for each situation_model



harrased_1 = table_writing_function(harrased_1)
harrased_2 = table_writing_function(harrased_2)
harrased_3 = table_writing_function(harrased_3)
harrased_4 = table_writing_function(harrased_4)
harrased_5 = table_writing_function(harrased_5)


lessrespect_1 = table_writing_function(lessrespect_1)
lessrespect_2 = table_writing_function(lessrespect_2)
lessrespect_3 = table_writing_function(lessrespect_3)
lessrespect_4 = table_writing_function(lessrespect_4)
lessrespect_5 = table_writing_function(lessrespect_5)

medical_1 = table_writing_function(medical_1)
medical_2 = table_writing_function(medical_2)
medical_3 = table_writing_function(medical_3)
medical_4 = table_writing_function(medical_4)
medical_5 = table_writing_function(medical_5)


notclever_1 = table_writing_function(notclever_1)
notclever_2 = table_writing_function(notclever_2)
notclever_3 = table_writing_function(notclever_3)
notclever_4 = table_writing_function(notclever_4)
notclever_5 = table_writing_function(notclever_5)

poorerservice_1 = table_writing_function(poorerservice_1)
poorerservice_2 = table_writing_function(poorerservice_2)
poorerservice_3 = table_writing_function(poorerservice_3)
poorerservice_4 = table_writing_function(poorerservice_4)
poorerservice_5 = table_writing_function(poorerservice_5)



harrased_results = cbind(harrased_1, 
                         harrased_2,
                         harrased_3,
                         harrased_4,
                         harrased_5)


lessrespect_results = cbind(lessrespect_1,
                         lessrespect_2,
                         lessrespect_3,
                         lessrespect_4, 
                         lessrespect_5)



medical_results = cbind(medical_1,
                        medical_2,
                        medical_3,
                        medical_4, 
                        medical_5)


notclever_results = cbind(notclever_1,
                          notclever_2,
                          notclever_3,
                          notclever_4, 
                          notclever_5)


poorerservice_results = cbind(poorerservice_1,
                              poorerservice_2,
                              poorerservice_3,
                              poorerservice_4, 
                              poorerservice_5)


results_by_situation =  rbind(harrased_results, 
                              lessrespect_results, 
                              medical_results,
                              notclever_results,
                              poorerservice_results)

# add: harrassed, less respect, etc col
# within that add All, Female, male, race, BMI, etc) 

write.csv(results_by_situation, file = paste(SOURCE_ROOT, "results_by_situation.csv"))

# 
# colnames(harrased_1) <- paste("Model_1", colnames(harrased_1), sep = "_")
# colnames(harrased_2) <- paste("Model_2", colnames(harrased_2), sep = "_")
# colnames(harrased_3) <- paste("Model_3", colnames(harrased_3), sep = "_")
# colnames(harrased_4) <- paste("Model_4", colnames(harrased_4), sep = "_")
# colnames(harrased_5) <- paste("Model_5", colnames(harrased_5), sep = "_")
# 
# 
# colnames(lessrespect_1) <- paste("Model_1", colnames(lessrespect_1), sep = "_")
# colnames(lessrespect_2) <- paste("Model_2", colnames(lessrespect_2), sep = "_")
# colnames(lessrespect_3) <- paste("Model_3", colnames(lessrespect_3), sep = "_")
# colnames(lessrespect_4) <- paste("Model_4", colnames(lessrespect_4), sep = "_")
# colnames(lessrespect_5) <- paste("Model_5", colnames(lessrespect_5), sep = "_")
# 
# 
# colnames(medical_1) <- paste("Model_1", colnames(medical_1), sep = "_")
# colnames(medical_2) <- paste("Model_2", colnames(medical_2), sep = "_")
# colnames(medical_3) <- paste("Model_3", colnames(medical_3), sep = "_")
# colnames(medical_4) <- paste("Model_4", colnames(medical_4), sep = "_")
# colnames(medical_5) <- paste("Model_5", colnames(medical_5), sep = "_")
# 
# 
# colnames(notclever_1) <- paste("Model_1", colnames(notclever_1), sep = "_")
# colnames(notclever_2) <- paste("Model_2", colnames(notclever_2), sep = "_")
# colnames(notclever_3) <- paste("Model_3", colnames(notclever_3), sep = "_")
# colnames(notclever_4) <- paste("Model_4", colnames(notclever_4), sep = "_")
# colnames(notclever_5) <- paste("Model_5", colnames(notclever_5), sep = "_")
# 
# 
# colnames(poorerservice_1) <- paste("Model_1", colnames(poorerservice_1), sep = "_")
# colnames(poorerservice_2) <- paste("Model_2", colnames(poorerservice_2), sep = "_")
# colnames(poorerservice_3) <- paste("Model_3", colnames(poorerservice_3), sep = "_")
# colnames(poorerservice_4) <- paste("Model_4", colnames(poorerservice_4), sep = "_")
# colnames(poorerservice_5) <- paste("Model_5", colnames(poorerservice_5), sep = "_")
# 
