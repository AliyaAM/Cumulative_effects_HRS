library(tidyverse)


RESULTS_cox_regression_marginalised_groups = rbind(Model_1_results, 
                                                    Model_2_non_interact_results, 
                                                    Model_3_non_interact_results, 
                                                    Model_4_non_interact_results, 
                                                    Model_5_non_interact_results, 
                                                    Model_6_non_interact_results, 
                                                    Model_7_non_interact_results) 



write.csv(RESULTS_cox_regression_marginalised_groups, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/RESULTS_cox_regression_marginalised_groups.csv")



data = read.csv("/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/RESULTS_cox_regression_marginalised_groups_col.csv")



data$lower_CI = round(as.numeric(data$lower_CI), digits = 2)
data$upper_CI = round(as.numeric(data$upper_CI), digits = 2)

data$coef = round(as.numeric(data$coef), digits = 2)
data$logtest = round(as.numeric(data$logtest), digits = 2)


data$asterix_mark = case_when(data$p_value <=0.05 & data$p_value > 0.01 ~ "*",
                                    data$p_value <=0.01 &  data$p_value > 0.001 ~ "**",
                                    data$p_value <=0.001 ~ "***")

data$CI_both = paste("[", data$lower_CI, ";" , data$upper_CI, "]", sep="")




write.csv(data, "/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/RESULTS_cox_model_data_HRsonly.csv") 


                                                    
      
      