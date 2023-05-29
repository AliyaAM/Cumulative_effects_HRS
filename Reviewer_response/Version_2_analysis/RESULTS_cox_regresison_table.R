library(tidyverse)

Model_1_restricted_results = read.csv("/Users/aliya/my_docs/KCL_postDoc/Data_analysis/DISCRIMINATIONAL_CATEGORICAL_/Model_1_results.csv")
Model_2_restricted_non_interact_results = read.csv("/Users/aliya/my_docs/KCL_postDoc/Data_analysis/DISCRIMINATIONAL_CATEGORICAL_/Model_2_non_interact_results.csv")
Model_3_restricted_non_interact_results =  read.csv("/Users/aliya/my_docs/KCL_postDoc/Data_analysis/DISCRIMINATIONAL_CATEGORICAL_/Model_3_non_interact_results.csv")
Model_4_restricted_non_interact_results = read.csv("/Users/aliya/my_docs/KCL_postDoc/Data_analysis/DISCRIMINATIONAL_CATEGORICAL_/Model_4_non_interact_results.csv")
Model_5_restricted_non_interact_results =  read.csv("/Users/aliya/my_docs/KCL_postDoc/Data_analysis/DISCRIMINATIONAL_CATEGORICAL_/Model_5_non_interact_results.csv")

#Model_6_non_interact_results is NA

Model_7_restricted_non_interact_results = read.csv("/Users/aliya/my_docs/KCL_postDoc/Data_analysis/DISCRIMINATIONAL_CATEGORICAL_/Model_7_non_interact_results.csv")


RESULTS_cox_regression_discrim_atribution = cbind(Model_1_restricted_results, 
                                                    Model_2_restricted_non_interact_results, 
                                                    Model_3_restricted_non_interact_results, 
                                                    Model_4_restricted_non_interact_results, 
                                                    Model_5_restricted_non_interact_results, 
                                                    #Model_6_restricted_non_interact_results, 
                                                    Model_7_restricted_non_interact_results) 



write.csv(RESULTS_cox_regression_discrim_atribution, "/Users/aliya/my_docs/KCL_postDoc/Data_analysis/DISCRIMINATIONAL_CATEGORICAL_/RESULTS_cox_regression_discrim_atribution_unedited_categorical.csv")



data = read.csv("/Users/aliya/my_docs/KCL_postDoc/Data_analysis/DISCRIMINATIONAL_CATEGORICAL_/RESULTS_cox_regression_discrim_atribution_categorical.csv")



data$lower_CI = round(as.numeric(data$lower_CI), digits = 2)
data$upper_CI = round(as.numeric(data$upper_CI), digits = 2)

data$coef = round(as.numeric(data$coef), digits = 2)
data$logtest = round(as.numeric(data$logtest), digits = 2)


data$asterix_mark = case_when(data$p_value <=0.05 & data$p_value > 0.01 ~ "*",
                                    data$p_value <=0.01 &  data$p_value > 0.001 ~ "**",
                                    data$p_value <=0.001 ~ "***")

data$CI_both = paste("[", data$lower_CI, ";" , data$upper_CI, "]", sep="")




write.csv(data, "/Users/aliya/my_docs/KCL_postDoc/Data_analysis/DISCRIMINATIONAL_CATEGORICAL_//RESULTS_cox_model_data_HRsonly_restricted_discrimination_categorical.csv") 


                                                    
      
      