library(WCE)
library(survival)
library(dplyr)
library(car)
library(tidyverse)
library(tidyr)

library(epiDisplay) #tab1 function to make a frequency table 
library(foreign)
library(rms) # Used to extract p_value from logistic model
library(ggplot2) #plots 
library(corrplot)
library(gridExtra) 
library(sjPlot)
library(knitr)
library(lme4)
library(lattice)
library(Hmisc)




## Set the root directory to look for source code.
#laptop: 
#/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/
#"/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"

# plots: 
# https://adibender.github.io/pammtools/articles/cumulative_effects.html

#https://rpkgs.datanovia.com/ggpubr/reference/stat_regline_equation.html Adding text (coefficients etc) to the plot next to the regression line 



OUTPUT_ROOT ="/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/Results/"
SOURCE_ROOT = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/WCE_analysis/"

SOURCE_data_ROOT  = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/"

#SOURCE_data_ROOT = "/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"
#SOURCE_ROOT =  "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/"
#OUTPUT_ROOT = "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/Results/"

# function that subsets and srts dataset for a particular var (eg., female == 1)


source((paste(SOURCE_ROOT, "subset_sort.R", sep="")))

source((paste(SOURCE_ROOT, "subset_sort_BMI.R", sep="")))




#function that sorts out the data into a dataframe where each participant x wave pair is one row. Here I also add starting and stopping points to identify each wave
source((paste(SOURCE_ROOT, "sort_timepoints.R", sep="")))

#function that runs WCE analysis
#source((paste(SOURCE_ROOT, "WCE_analysis.R", sep="")))

#function that runs WCE analysis
source((paste(SOURCE_ROOT, "summary_score_WCE_analysis.R", sep="")))

# function that samples bootstrapped CIs
source((paste(SOURCE_ROOT, "summary_score_Bootstrapped_CI.R", sep="")))


# function that runs WCE analysis and CIs sampling for a specified subset and with a specified model 
source((paste(SOURCE_ROOT, "discrim_bin_model_func.R", sep="")))


#Model 1: age and sex, wealth  [basis adjustment]
Model_1 = c("continious_age", "wealth_noIRA", "sex_1_2")

#Model 2: age, sex, wealth, BMI, hypertension  [basic adjustment + diabetes risk factors]
Model_2 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "hypertension_new_bin")

#Model 3: age, sex, wealth, physical activity, smoking (yes/no), and alcohol (days/week) [basic adjustment + health behaviours]
Model_3 = c("continious_age", "wealth_noIRA", "sex_1_2","alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin')

#Model 4: age, sex, wealth, CVD  [basic adjustment + CVD most common diabetes co_morbidity]
Model_4 = c("continious_age", "wealth_noIRA", "sex_1_2","CVD")

#Model 5: age, sex, wealth, depression  [basic adjustment + depression best researched psychosocial factor in diabetes ]
Model_5 = c("continious_age","wealth_noIRA", "sex_1_2","checklist_depression_bin")

#Model 6: age, sex, wealth, BMI, hypertension, CVD  [basic adjustment + diabetes risk factors+ CVD]
Model_6 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "hypertension_new_bin", "CVD")

#Model 7: age, sex, wealth, BMI, hypertension, depression  [basic adjustment + diabetes risk factors+ Depression]
Model_7 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "hypertension_new_bin", "checklist_depression_bin")


#Model 1: age and sex, wealth  [basis adjustment]
Model_1_nosex = c("continious_age", "wealth_noIRA")

#Model 2: age, sex, wealth, BMI, hypertension  [basic adjustment + diabetes risk factors]
Model_2_nosex = c("continious_age", "wealth_noIRA",  "assessed_BMI", "hypertension_new_bin")

#Model 3: age, sex, wealth, physical activity, smoking (yes/no), and alcohol (days/week) [basic adjustment + health behaviours]
Model_3_nosex = c("continious_age", "wealth_noIRA", "alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin')

#Model 4: age, sex, wealth, CVD  [basic adjustment + CVD most common diabetes co_morbidity]
Model_4_nosex = c("continious_age", "wealth_noIRA","CVD")

#Model 5: age, sex, wealth, depression  [basic adjustment + depression best researched psychosocial factor in diabetes ]
Model_5_nosex = c("continious_age","wealth_noIRA", "checklist_depression_bin")

#Model 6: age, sex, wealth, BMI, hypertension, CVD  [basic adjustment + diabetes risk factors+ CVD]
Model_6_nosex = c("continious_age", "wealth_noIRA",  "assessed_BMI", "hypertension_new_bin", "CVD")

#Model 7: age, sex, wealth, BMI, hypertension, depression  [basic adjustment + diabetes risk factors+ Depression]
Model_7_nosex = c("continious_age", "wealth_noIRA",  "assessed_BMI", "hypertension_new_bin", "checklist_depression_bin")


######


#Model 1: age and sex, wealth  [basis adjustment]
Model_noBMIcov_1 = c("continious_age", "wealth_noIRA", "sex_1_2")

#Model 2: age, sex, wealth, BMI, hypertension  [basic adjustment + diabetes risk factors]
Model_noBMIcov_2 = c("continious_age", "wealth_noIRA", "sex_1_2", "hypertension_new_bin")

#Model 3: age, sex, wealth, physical activity, smoking (yes/no), and alcohol (days/week) [basic adjustment + health behaviours]
Model_noBMIcov_3 = c("continious_age", "wealth_noIRA", "sex_1_2","alcohol_days_week_new",  "vigarious_physical_activity_new", 'smokes_now_bin')

#Model 4: age, sex, wealth, CVD  [basic adjustment + CVD most common diabetes co_morbidity]
Model_noBMIcov_4 = c("continious_age", "wealth_noIRA", "sex_1_2","CVD")

#Model 5: age, sex, wealth, depression  [basic adjustment + depression best researched psychosocial factor in diabetes ]
Model_noBMIcov_5 = c("continious_age","wealth_noIRA", "sex_1_2","checklist_depression_bin")

#Model 6: age, sex, wealth, BMI, hypertension, CVD  [basic adjustment + diabetes risk factors+ CVD]
Model_noBMIcov_6 = c("continious_age", "wealth_noIRA", "sex_1_2", "hypertension_new_bin", "CVD")

#Model 7: age, sex, wealth, BMI, hypertension, depression  [basic adjustment + diabetes risk factors+ Depression]
Model_noBMIcov_7 = c("continious_age", "wealth_noIRA", "sex_1_2",  "hypertension_new_bin", "checklist_depression_bin")



#self_reported
# also restricting to those who attributed discrimination to specific reasons as follows: 
#reason_discrim1__national
#reason_discrim1__race
#reason_discrim1__religion
#reason_discrim1__weight

#age50_60yrs_dataset$reason_discrim1__age is in a separate file

female_dataset = subset_sort(subset_var = "sex_1_2", 
                                           subset_value = 2) 


#objectively measured weight and height from which the BMi was calculated
BMI_dataset = subset_sort_BMI(subset_var = "assessed_BMI", 
                                        subset_value = 30) 



#self_reported
national_origin_dataset = subset_sort(subset_var = "national_origin_ousideUS_bin", 
                                                    subset_value = 1) 

#self_reported
race_dataset = subset_sort(subset_var = "race_white", 
                                         subset_value = 0) 

#self_reported
religion_dataset = subset_sort(subset_var = "religion_bin", 
                                             subset_value = 1) 


###### run all models for female 
female_Model_1_discrim_bin = discrim_bin_model_func(data_wce_subset = female_dataset, 
                                                    Model_n = Model_1_nosex, 
                                                    Model_name = "Model_1")


female_Model_2_discrim_bin = discrim_bin_model_func(data_wce_subset = female_dataset, 
                                                    Model_n = Model_2_nosex, 
                                                    Model_name = "Model_2")


female_Model_3_discrim_bin = discrim_bin_model_func(data_wce_subset = female_dataset, 
                                                    Model_n = Model_3_nosex)



female_Model_4_discrim_bin = discrim_bin_model_func(data_wce_subset = female_dataset, 
                                                    Model_n = Model_4_nosex)



female_Model_5_discrim_bin = discrim_bin_model_func(data_wce_subset = female_dataset, 
                                                    Model_n = Model_5_nosex)


female_Model_6_discrim_bin = discrim_bin_model_func(data_wce_subset = female_dataset, 
                                                    Model_n = Model_6_nosex)


female_Model_7_discrim_bin = discrim_bin_model_func(data_wce_subset = female_dataset, 
                                                    Model_n = Model_7_nosex)




female_results_discrim_bin = rbind(female_Model_1_discrim_bin,
                                   female_Model_2_discrim_bin,
                                   female_Model_3_discrim_bin,
                                   female_Model_4_discrim_bin,
                                   female_Model_5_discrim_bin,
                                   female_Model_6_discrim_bin,
                                   female_Model_7_discrim_bin)

write.csv(female_results_discrim_bin, paste(OUTPUT_ROOT, "female_results_discrim_binrestricted__V4.csv", sep=""))



female_results_discrim_bin_table_col= cbind(female_Model_1_discrim_bin,
                                            female_Model_2_discrim_bin,
                                            female_Model_3_discrim_bin,
                                            female_Model_4_discrim_bin,
                                            female_Model_5_discrim_bin,
                                            female_Model_6_discrim_bin,
                                            female_Model_7_discrim_bin)

write.csv(female_results_discrim_bin_table_col, paste(OUTPUT_ROOT, "female_results_discrim_bin_table_colrestricted__V4.csv", sep=""))


#############


###### run all models for BMI_dataset 
BMI_Model_1_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                 Model_n = Model_1, 
                                                 Model_name = "Model_1")


BMI_Model_2_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                 Model_n = Model_2, 
                                                 Model_name = "Model_2")


BMI_Model_3_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                 Model_n = Model_3)



BMI_Model_4_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                 Model_n = Model_4)



BMI_Model_5_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                 Model_n = Model_5)


BMI_Model_6_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                 Model_n = Model_6)


BMI_Model_7_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                 Model_n = Model_7)



BMI_results_discrim_bin = rbind(BMI_Model_1_discrim_bin,
                                BMI_Model_2_discrim_bin,
                                BMI_Model_3_discrim_bin,
                                BMI_Model_4_discrim_bin,
                                BMI_Model_5_discrim_bin,
                                BMI_Model_6_discrim_bin,
                                BMI_Model_7_discrim_bin)

write.csv(BMI_results_discrim_bin, paste(OUTPUT_ROOT, "BMI_results_discrim_binrestricted__V4.csv", sep=""))

BMI_results_discrim_bin_table_col = cbind(BMI_Model_1_discrim_bin,
                                          BMI_Model_2_discrim_bin,
                                          BMI_Model_3_discrim_bin,
                                          BMI_Model_4_discrim_bin,
                                          BMI_Model_5_discrim_bin,
                                          BMI_Model_6_discrim_bin,
                                          BMI_Model_7_discrim_bin)

write.csv(BMI_results_discrim_bin_table_col, paste(OUTPUT_ROOT, "BMI_results_discrim_bin_table_colrestricted__V4.csv", sep=""))
#############



###### run all models for BMI_dataset 
BMI_Model_noBMIcov_1_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                          Model_n = Model_noBMIcov_1, 
                                                          Model_name = "Model_noBMIcov_1")


BMI_Model_noBMIcov_1_discrim_bin = cbind(NA, NA, NA)
colnames(BMI_Model_noBMIcov_1_discrim_bin) = c("hazard ratio",    "5% CI",  "95% CI") 

BMI_Model_noBMIcov_2_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                          Model_n = Model_noBMIcov_2, 
                                                          Model_name = "Model_noBMIcov_2")


BMI_Model_noBMIcov_3_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                          Model_n = Model_noBMIcov_3)



BMI_Model_noBMIcov_4_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                          Model_n = Model_noBMIcov_4)



BMI_Model_noBMIcov_5_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                          Model_n = Model_noBMIcov_5)


BMI_Model_noBMIcov_6_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                          Model_n = Model_noBMIcov_6)


BMI_Model_noBMIcov_7_discrim_bin = discrim_bin_model_func(data_wce_subset = BMI_dataset, 
                                                          Model_n = Model_noBMIcov_7)


BMI_results_discrim_bin_noBMIcov = rbind(BMI_Model_noBMIcov_1_discrim_bin,
                                         BMI_Model_noBMIcov_2_discrim_bin,
                                         BMI_Model_noBMIcov_3_discrim_bin,
                                         BMI_Model_noBMIcov_4_discrim_bin,
                                         BMI_Model_noBMIcov_5_discrim_bin,
                                         BMI_Model_noBMIcov_6_discrim_bin,
                                         BMI_Model_noBMIcov_7_discrim_bin)

write.csv(BMI_results_discrim_bin_noBMIcov, paste(OUTPUT_ROOT, "BMI_results_discrim_bin_noBMIcovrestricted__V4.csv", sep=""))



BMI_results_discrim_bin_noBMIcov_table_col = cbind(BMI_Model_noBMIcov_1_discrim_bin,
                                                   BMI_Model_noBMIcov_2_discrim_bin,
                                                   BMI_Model_noBMIcov_3_discrim_bin,
                                                   BMI_Model_noBMIcov_4_discrim_bin,
                                                   BMI_Model_noBMIcov_5_discrim_bin,
                                                   BMI_Model_noBMIcov_6_discrim_bin,
                                                   BMI_Model_noBMIcov_7_discrim_bin)

write.csv(BMI_results_discrim_bin_noBMIcov_table_col, paste(OUTPUT_ROOT, "BMI_results_discrim_bin_noBMIcov_table_colrestricted__V4.csv", sep=""))



###### run all models for national_origin 
national_origin_Model_1_discrim_bin = discrim_bin_model_func(data_wce_subset = national_origin_dataset, 
                                                             Model_n = Model_1, 
                                                             Model_name = "Model_1")


national_origin_Model_2_discrim_bin = discrim_bin_model_func(data_wce_subset = national_origin_dataset, 
                                                             Model_n = Model_2, 
                                                             Model_name = "Model_2")


national_origin_Model_3_discrim_bin = discrim_bin_model_func(data_wce_subset = national_origin_dataset, 
                                                             Model_n = Model_3)



national_origin_Model_4_discrim_bin = discrim_bin_model_func(data_wce_subset = national_origin_dataset, 
                                                             Model_n = Model_4)



national_origin_Model_5_discrim_bin = discrim_bin_model_func(data_wce_subset = national_origin_dataset, 
                                                             Model_n = Model_5)


national_origin_Model_6_discrim_bin = discrim_bin_model_func(data_wce_subset = national_origin_dataset, 
                                                             Model_n = Model_6)


national_origin_Model_7_discrim_bin = discrim_bin_model_func(data_wce_subset = national_origin_dataset, 
                                                             Model_n = Model_7)



national_origin_results_discrim_bin = rbind(national_origin_Model_1_discrim_bin,
                                            national_origin_Model_2_discrim_bin,
                                            national_origin_Model_3_discrim_bin,
                                            national_origin_Model_4_discrim_bin,
                                            national_origin_Model_5_discrim_bin,
                                            national_origin_Model_6_discrim_bin,
                                            national_origin_Model_7_discrim_bin)




write.csv(national_origin_results_discrim_bin, paste(OUTPUT_ROOT, "national_origin_results_discrim_binrestricted__V4.csv", sep=""))


national_origin_results_discrim_bin_table_col = cbind(national_origin_Model_1_discrim_bin,
                                                      national_origin_Model_2_discrim_bin,
                                                      national_origin_Model_3_discrim_bin,
                                                      national_origin_Model_4_discrim_bin,
                                                      national_origin_Model_5_discrim_bin,
                                                      national_origin_Model_6_discrim_bin,
                                                      national_origin_Model_7_discrim_bin)

write.csv(national_origin_results_discrim_bin_table_col, paste(OUTPUT_ROOT, "national_origin_results_discrim_bin_table_colrestricted__V4.csv", sep=""))


#############


###### run all models for race other than white
race_Model_1_discrim_bin = discrim_bin_model_func(data_wce_subset = race_dataset, 
                                                  Model_n = Model_1, 
                                                  Model_name = "Model_1")


race_Model_2_discrim_bin = discrim_bin_model_func(data_wce_subset = race_dataset, 
                                                  Model_n = Model_2, 
                                                  Model_name = "Model_2")


race_Model_3_discrim_bin = discrim_bin_model_func(data_wce_subset = race_dataset, 
                                                  Model_n = Model_3)



race_Model_4_discrim_bin = discrim_bin_model_func(data_wce_subset = race_dataset, 
                                                  Model_n = Model_4)



race_Model_5_discrim_bin = discrim_bin_model_func(data_wce_subset = race_dataset, 
                                                  Model_n = Model_5)


race_Model_6_discrim_bin = discrim_bin_model_func(data_wce_subset = race_dataset, 
                                                  Model_n = Model_6)


race_Model_7_discrim_bin = discrim_bin_model_func(data_wce_subset = race_dataset, 
                                                  Model_n = Model_7)



race_results_discrim_bin = rbind(race_Model_1_discrim_bin,
                                 race_Model_2_discrim_bin,
                                 race_Model_3_discrim_bin,
                                 race_Model_4_discrim_bin,
                                 race_Model_5_discrim_bin,
                                 race_Model_6_discrim_bin,
                                 race_Model_7_discrim_bin)

write.csv(race_results_discrim_bin, paste(OUTPUT_ROOT, "race_results_discrim_binrestricted__V4.csv", sep=""))


race_results_discrim_bin_table_col = cbind(race_Model_1_discrim_bin,
                                           race_Model_2_discrim_bin,
                                           race_Model_3_discrim_bin,
                                           race_Model_4_discrim_bin,
                                           race_Model_5_discrim_bin,
                                           race_Model_6_discrim_bin,
                                           race_Model_7_discrim_bin)

write.csv(race_results_discrim_bin_table_col, paste(OUTPUT_ROOT, "race_results_discrim_bin_table_colrestricted__V4.csv", sep=""))

###### run all models for religion other than protestant or catholic 
religion_Model_1_discrim_bin = discrim_bin_model_func(data_wce_subset = religion_dataset, 
                                                      Model_n = Model_1, 
                                                      Model_name = "Model_1")


religion_Model_2_discrim_bin = discrim_bin_model_func(data_wce_subset = religion_dataset, 
                                                      Model_n = Model_2, 
                                                      Model_name = "Model_2")


religion_Model_3_discrim_bin = discrim_bin_model_func(data_wce_subset = religion_dataset, 
                                                      Model_n = Model_3)



religion_Model_4_discrim_bin = discrim_bin_model_func(data_wce_subset = religion_dataset, 
                                                      Model_n = Model_4)



religion_Model_5_discrim_bin = discrim_bin_model_func(data_wce_subset = religion_dataset, 
                                                      Model_n = Model_5)


religion_Model_6_discrim_bin = discrim_bin_model_func(data_wce_subset = religion_dataset, 
                                                      Model_n = Model_6)


religion_Model_7_discrim_bin = discrim_bin_model_func(data_wce_subset = religion_dataset, 
                                                      Model_n = Model_7)


religion_results_discrim_bin = rbind(religion_Model_1_discrim_bin,
                                     religion_Model_2_discrim_bin,
                                     religion_Model_3_discrim_bin,
                                     religion_Model_4_discrim_bin,
                                     religion_Model_5_discrim_bin,
                                     religion_Model_6_discrim_bin,
                                     religion_Model_7_discrim_bin)

write.csv(religion_results_discrim_bin, paste(OUTPUT_ROOT, "religion_results_discrim_binrestricted__V4.csv", sep=""))


religion_results_discrim_bin_table_col = cbind(religion_Model_1_discrim_bin,
                                               religion_Model_2_discrim_bin,
                                               religion_Model_3_discrim_bin,
                                               religion_Model_4_discrim_bin,
                                               religion_Model_5_discrim_bin,
                                               religion_Model_6_discrim_bin,
                                               religion_Model_7_discrim_bin)

write.csv(religion_results_discrim_bin_table_col, paste(OUTPUT_ROOT, "religion_results_discrim_bin_table_colrestricted__V4.csv", sep=""))

#############
