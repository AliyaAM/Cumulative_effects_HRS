cumulative_effects_dat = read.csv("/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/DATA_FOR_PLOT/all_waves_nodiab_at_two_first_waves_DIAB_discrim_recoded.csv")




data_male = subset(cumulative_effects_dat, cumulative_effects_dat$sex_1_2 ==1) 
data_female = subset(cumulative_effects_dat, cumulative_effects_dat$sex_1_2 ==2) 
data_race = subset(cumulative_effects_dat, cumulative_effects_dat$race_white == 0) 
data_BMI = subset(cumulative_effects_dat, cumulative_effects_dat$assessed_BMI > 30) 


nrow(data_male) #9352



all_n = unique(cumulative_effects_dat$HHIDPN) 8461
male_n = unique(data_male$HHIDPN) 3833
female_n = unique(data_female$HHIDPN) 4647
race_n = unique(data_race$HHIDPN) 3014
BMI_n = unique(data_BMI$HHIDPN) 2163


all_diabetes = subset(cumulative_effects_dat, cumulative_effects_dat$diabetes_new == 1) #476
all_diabetes_n = unique(all_diabetes$HHIDPN) 266 

male_diabetes = subset(data_male, data_male$diabetes_new == 1) #232
male_diabetes_n = unique(male_diabetes$HHIDPN)  131 

female_diabetes = subset(data_female, data_female$diabetes_new == 1)  #244
female_diabetes_n = unique(female_diabetes$HHIDPN)  135 

race_diabetes = subset(data_race, data_race$diabetes_new == 1)  #250 
race_diabetes_n = unique(race_diabetes$HHIDPN)  137 


BMI_diabetes = subset(data_BMI, data_BMI$diabetes_new == 1) #92 
BMI_diabetes_n = unique(BMI_diabetes$HHIDPN)  82 



#####################################################################
names= c("All", 
         "female",
         "male", 
         "race", 
         "BMI")

total_n = c(8461, 
            4647,
            3833,
            3014, 
            2163) 

#####################################################################
diabetes_n = c(266, 
               135, 
               131, 
               137, 
               82) 


results = data.frame(names, total_n, diabetes_n)

results$percentage = (results$diabetes_n/results$total_n)*100

write.csv(results, "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/results_number_diabetes_two_waves_removed.csv")




mean(cumulative_effects_dat$timepoints_indiv) #3.4
mean(data_male$timepoints_indiv) #3.4
mean(data_female$timepoints_indiv) #3.4
mean(data_race$timepoints_indiv) #3.6
mean(data_BMI$timepoints_indiv) #3.5

