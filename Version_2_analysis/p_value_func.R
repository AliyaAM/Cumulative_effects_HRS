
p_value_func = function(data, subset_name, Model){
  
Model = c(1, 2, 3, 4, 5, 6, 7)

subset_name = rep(subset_name, times = nrow(data))


data = cbind(subset_name, Model, data)
data = as.data.frame(data)

hazard_ratio = as.numeric(data$`hazard ratio`)
CI_5 = as.numeric(data$`5% CI`)
CI_95 = as.numeric(data$`95% CI`)


SE = (CI_5 - CI_95)/(2*1.96)
z_value = hazard_ratio/SE 
p_value = exp(-0.717*z_value -0.416*((z_value)^2))

data_pvalues = cbind(data, SE, z_value, p_value)
data_pvalues = as.data.frame(data)
#######
#######

#write.csv(data_pvalues, paste(c(OUTPUT_ROOT, subset_name, "data_pvalues.csv", sep="")))

clean_data = as.data.frame(data)


clean_data$`5% CI` = round(as.numeric(clean_data$`5% CI`), digits = 2)
clean_data$`95% CI` = round(as.numeric(clean_data$`95% CI`), digits = 2)
clean_data$`hazard ratio` = round(as.numeric(clean_data$`hazard ratio`), digits = 2)
clean_data$BIC = round(as.numeric(clean_data$BIC), digits = 2)


clean_data$asterix_mark = case_when(clean_data$p_value <=0.05 & clean_data$p_value > 0.01 ~ "*",
                                                                    clean_data$p_value <=0.01 &  clean_data$p_value > 0.001 ~ "**",
                                                                    clean_data$p_value <=0.001 ~ "***")

clean_data$CI_both = paste("[", clean_data$`5% CI`, ";" , clean_data$`95% CI`, "]", sep="")


clean_data_HRsonly = cbind(clean_data$`analysed n`,
                           clean_data$`diabetes onset (n)`,
                           clean_data$`median timepoint`, 
                           clean_data$BIC, 
                           clean_data$`hazard ratio`, 
                           clean_data$asterix_mark, 
                           clean_data$CI_both)

#write.csv(clean_data_HRsonly, paste(c(OUTPUT_ROOT, subset_name, "clean_data_HRsonly.csv", sep="")))

return(clean_data)
}