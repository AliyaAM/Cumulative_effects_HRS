
p_value_func = function (data, subset_name, Model){

Model_col = rep(Model, times = nrow(data))

subset_name_col = rep(subset_name, times = nrow(data))


data = cbind(subset_name_col, Model, data)
data = as.data.frame(data)


data$hazard_ratio = as.numeric(data$hazard_ratio)
data$CI_5 = as.numeric(data$CI_5)
data$CI_95 = as.numeric(data$CI_95)


data$SE = (data$CI_5 - data$CI_95)/(2*1.96)
data$z_value = data$hazard_ratio/data$SE 
data$p_value = exp(-0.717*data$z_value -0.416*((data$z_value)^2))

#######

write.csv(data, paste(OUTPUT_ROOT, subset_name, "data_pvalues.csv", sep=""))

clean_data = as.data.frame(data)


clean_data$CI_5 = round(as.numeric(clean_data$CI_5), digits = 2)
clean_data$CI_95 = round(as.numeric(clean_data$CI_95), digits = 2)
clean_data$hazard_ratio = round(as.numeric(clean_data$hazard_ratio), digits = 2)
clean_data$BIC = round(as.numeric(clean_data$BIC), digits = 2)


clean_data$asterix_mark = case_when(clean_data$p_value <=0.05 & clean_data$p_value > 0.01 ~ "*",
                                                                    clean_data$p_value <=0.01 &  clean_data$p_value > 0.001 ~ "**",
                                                                    clean_data$p_value <=0.001 ~ "***")

clean_data$CI_both = paste("[", clean_data$CI_5, ";" , clean_data$CI_95, "]", sep="")


clean_data_HRsonly = cbind(Model_col,
                           subset_name_col, 
                           clean_data$analysed_n,
                           clean_data$diabetes_onset_n,
                           clean_data$median_timepoint, 
                           clean_data$BIC, 
                           clean_data$hazard_ratio, 
                           clean_data$asterix_mark, 
                           clean_data$CI_both)

colnames(clean_data_HRsonly) = c("Model",
                                 "subset",
                                 "analysed_n",
                                  "diabetes_onset_n",
                                  "median_timepoint", 
                                  "BIC", 
                                  "hazard_ratio", 
                                  "asterix_mark", 
                                  "CI_both")


write.csv(clean_data_HRsonly, paste(OUTPUT_ROOT, subset_name, "_clean_data_HRsonly.csv", sep=""))

return(clean_data_HRsonly)
}
