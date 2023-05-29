
library("dplyr")
library("tidyr")

###### Adding variables to the main dataset:

#outcome:  cumulative_effects_dat$diabetes_new_bin
#exposure: cumulative_effects_dat$discrim_bin
#time: cumulative_effects_dat$stop_new

###### Adding variables to the main dataset:
###### Adding variables to the main dataset:

unique(cumulative_effects_dat$discrimination_cat)

cumulative_effects_dat$discrimination = cumulative_effects_dat$discrimination_cat
cumulative_effects_dat$years = 2 *cumulative_effects_dat$timepoints_indiv

cumulative_effects_dat$assessed_BMI = as.numeric(cumulative_effects_dat$assessed_BMI)

#cumulative_effects_dat_mean_BMI$assessed_BMI = subset(cumulative_effects_dat, cumulative_effects_dat$assessed_BMI != "NA")


#cumulative_effects_dat = subset(cumulative_effects_dat, cumulative_effects_dat$start_new == 0)
#1 = 2 year 
#2 = 4 years 
#3 = 6 years 


unique(cumulative_effects_dat$timepoints_indiv)
unique(cumulative_effects_dat$start_new)

cumulative_effects_dat$diabetes_new_bin_reversed = case_when(cumulative_effects_dat$diabetes_new_bin == 1 ~ 0, 
                                                             cumulative_effects_dat$diabetes_new_bin == 0 ~ 1) 

#Subset datasets:
data_male = subset(cumulative_effects_dat, cumulative_effects_dat$sex_1_2 ==1) 
data_female = subset(cumulative_effects_dat, cumulative_effects_dat$sex_1_2 ==2) 
data_race = subset(cumulative_effects_dat, cumulative_effects_dat$race_white == 0) 
data_BMI = subset(cumulative_effects_dat, cumulative_effects_dat$assessed_BMI > 30) 

#####

subset_name = c("all",
                "female",
                "race",
                "BMI")

age_all = mean(cumulative_effects_dat$continious_age)
age_female = mean(data_female$continious_age)
age_ethnicity = mean(data_race$continious_age)
age_BMI = mean(data_BMI$continious_age)

age_mean = c(age_all, 
             age_female, 
             age_ethnicity, 
             age_BMI)


age_all_sd = sd(cumulative_effects_dat$continious_age)
age_female_sd = sd(data_female$continious_age)
age_ethnicity_sd = sd(data_race$continious_age)
age_BMI_sd = sd(data_BMI$continious_age)

age_sd = c(age_all_sd, 
             age_female_sd, 
             age_ethnicity_sd, 
             age_BMI_sd)

#####

data_race_female = subset(data_race, data_race$sex_1_2 ==2) 
data_BMI_female = subset(data_BMI, data_BMI$sex_1_2 ==2) 

female_race_n = nrow(data_race_female)
female_BMI_n = nrow(data_BMI_female)

female_n = c("NA", 
             "NA",
             female_race_n,
             female_BMI_n)

percent_race_female = nrow(data_race_female)/nrow(data_race)*100
percent_BMI_female = nrow(data_BMI_female)/nrow(data_BMI)*100


female_percent = c("NA", 
             "NA",
             percent_race_female,
             percent_BMI_female)

####

wealth_all = mean(cumulative_effects_dat$wealth_noIRA) 
wealth_female = mean(data_female$wealth_noIRA)
wealth_ethnicity = mean(data_race$wealth_noIRA)
wealth_BMI = mean(data_BMI$wealth_noIRA)

wealth_mean = c(wealth_all, 
                wealth_female,
                wealth_ethnicity, 
                wealth_BMI) 

wealth_all_sd = sd(cumulative_effects_dat$wealth_noIRA)
wealth_female_sd = sd(data_female$wealth_noIRA)
wealth_ethnicity_sd = sd(data_race$wealth_noIRA)
wealth_BMI_sd = sd(data_BMI$wealth_noIRA)

wealth_sd = c(wealth_all_sd, 
              wealth_female_sd,
              wealth_ethnicity_sd, 
              wealth_BMI_sd) 

#####
#cumulative_effects_dat = na.omit(cumulative_effects_dat)

#cumulative_effects_dat$assessed_BMI<-cumulative_effects_dat$cumulative_effects_dat[!is.na(cumulative_effects_dat$assessed_BMI)]

cumulative_effects_dat$assessed_BMI = as.integer(cumulative_effects_dat$assessed_BMI)
cumulative_effects_dat = tibble(cumulative_effects_dat)
cumulative_effects_dat = cumulative_effects_dat %>% drop_na(assessed_BMI)
unique(cumulative_effects_dat$assessed_BMI)

cumulative_effects_dat_obese = subset(cumulative_effects_dat, cumulative_effects_dat$assessed_BMI >30) 
cumulative_effects_dat_overweight = subset(cumulative_effects_dat, cumulative_effects_dat$assessed_BMI >=25 & cumulative_effects_dat$assessed_BMI <=30) 
cumulative_effects_dat_healthy_weight = subset(cumulative_effects_dat, cumulative_effects_dat$assessed_BMI <25 & cumulative_effects_dat$assessed_BMI >18) 
cumulative_effects_dat_under_weight = subset(cumulative_effects_dat, cumulative_effects_dat$assessed_BMI <18) 

data_female_drop_bmi_dat = subset(cumulative_effects_dat, cumulative_effects_dat$sex_1_2 == 2)
data_female_drop_bmi_dat_obese = subset(data_female_drop_bmi_dat, data_female_drop_bmi_dat$assessed_BMI >30) 
data_female_drop_bmi_dat_overweight = subset(data_female_drop_bmi_dat, data_female_drop_bmi_dat$assessed_BMI >=25 & data_female_drop_bmi_dat$assessed_BMI <=30) 
data_female_drop_bmi_dat_healthy_weight = subset(data_female_drop_bmi_dat, data_female_drop_bmi_dat$assessed_BMI <25 & data_female_drop_bmi_dat$assessed_BMI >18) 
data_female_drop_bmi_dat_under_weight = subset(data_female_drop_bmi_dat, data_female_drop_bmi_dat$assessed_BMI <18) 

data_data_ethnicity_drop_bmi_dat = subset(cumulative_effects_dat, cumulative_effects_dat$race_white == 0)
data_data_ethnicity_drop_bmi_dat_obese = subset(data_data_ethnicity_drop_bmi_dat, data_data_ethnicity_drop_bmi_dat$assessed_BMI >30) 
data_data_ethnicity_drop_bmi_dat_overweight = subset(data_data_ethnicity_drop_bmi_dat, data_data_ethnicity_drop_bmi_dat$assessed_BMI >=25 & data_data_ethnicity_drop_bmi_dat$assessed_BMI <=30) 
data_data_ethnicity_drop_bmi_dat_healthy_weight = subset(data_data_ethnicity_drop_bmi_dat, data_data_ethnicity_drop_bmi_dat$assessed_BMI <25 & data_data_ethnicity_drop_bmi_dat$assessed_BMI >18) 
data_data_ethnicity_drop_bmi_dat_under_weight = subset(data_data_ethnicity_drop_bmi_dat, data_data_ethnicity_drop_bmi_dat$assessed_BMI <18) 

cumulative_effects_dat_obese = 
data_data_BMI_drop_bmi_dat = subset(cumulative_effects_dat, cumulative_effects_dat$assessed_BMI >30)


nrow_obese = nrow(cumulative_effects_dat_obese)
nrow_female_obese = nrow(data_female_drop_bmi_dat_obese)
nrow_ethnicity_obese = nrow(data_data_ethnicity_drop_bmi_dat_obese)
nrow_bmi_obese = nrow(cumulative_effects_dat_obese)

n_obese = c(nrow_obese, 
            nrow_female_obese, 
            nrow_ethnicity_obese, 
            nrow_bmi_obese)

nrow_overweight = nrow(cumulative_effects_dat_overweight)
nrow_female_overweight = nrow(data_female_drop_bmi_dat_overweight)
nrow_ethnicity_overweight = nrow(data_data_ethnicity_drop_bmi_dat_overweight)



n_overweight = c(nrow_overweight, 
                 nrow_female_overweight, 
                 nrow_ethnicity_overweight, 
                 "NA")

nrow_healthy_weight = nrow(cumulative_effects_dat_healthy_weight)
nrow_female_healthy_weight = nrow(data_female_drop_bmi_dat_healthy_weight)
nrow_ethnicity_healthy_weight = nrow(data_data_ethnicity_drop_bmi_dat_healthy_weight)

n_healthy_weight = c(nrow_healthy_weight, 
                 nrow_female_healthy_weight, 
                 nrow_ethnicity_healthy_weight, 
                 "NA")

nrow_under_weight = nrow(cumulative_effects_dat_under_weight)
nrow_female_under_weight = nrow(data_female_drop_bmi_dat_under_weight)
nrow_ethnicity_under_weight = nrow(data_data_ethnicity_drop_bmi_dat_under_weight)


n_under_weight = c(nrow_under_weight, 
                     nrow_female_under_weight, 
                     nrow_ethnicity_under_weight, 
                     "NA")



percent_all_obese = (nrow(cumulative_effects_dat_obese)/nrow(cumulative_effects_dat))*100
percent_female_obese = (nrow(data_data_ethnicity_drop_bmi_dat_obese)/nrow(data_data_ethnicity_drop_bmi_dat))*100
percent_ethnicity_obese = (nrow(data_data_ethnicity_drop_bmi_dat_obese)/nrow(data_data_ethnicity_drop_bmi_dat))*100

percent_obese  = c(percent_all_obese, 
                    percent_female_obese, 
                    percent_ethnicity_obese, 
                    "NA")

percent_all_overweight = (nrow(cumulative_effects_dat_overweight)/nrow(cumulative_effects_dat))*100
percent_female_overweight = (nrow(data_female_drop_bmi_dat_overweight)/nrow(data_female_drop_bmi_dat))*100
percent_ethnicity_overweight = (nrow(data_data_ethnicity_drop_bmi_dat_overweight)/nrow(data_data_ethnicity_drop_bmi_dat))*100


percent_overweight  = c(percent_all_overweight, 
                   percent_female_overweight, 
                   percent_ethnicity_overweight, 
                   "NA")

percent_all_healthy_weight = (nrow(cumulative_effects_dat_healthy_weight)/nrow(cumulative_effects_dat))*100
percent_female_healthy_weight = (nrow(data_female_drop_bmi_dat_healthy_weight)/nrow(data_female_drop_bmi_dat))*100
percent_ethnicity_healthy_weight = (nrow(data_data_ethnicity_drop_bmi_dat_healthy_weight)/nrow(data_data_ethnicity_drop_bmi_dat))*100


percent_healthy_weight  = c(percent_all_healthy_weight, 
                        percent_female_healthy_weight, 
                        percent_ethnicity_healthy_weight, 
                        "NA")

percent_all_under_weight = (nrow(cumulative_effects_dat_under_weight)/nrow(cumulative_effects_dat))*100
percent_female_under_weight = (nrow(data_female_drop_bmi_dat_under_weight)/nrow(data_female_drop_bmi_dat))*100
percent_ethnicity_under_weight = (nrow(data_data_ethnicity_drop_bmi_dat_under_weight)/nrow(data_data_ethnicity_drop_bmi_dat))*100

percent_under_weight  = c(percent_all_under_weight, 
                             percent_female_under_weight, 
                             percent_ethnicity_under_weight, 
                             "NA")


BMI_all = mean(cumulative_effects_dat$assessed_BMI) 
BMI_female = mean(data_female_drop_bmi_dat$assessed_BMI)
BMI_ethnicity = mean(data_data_ethnicity_drop_bmi_dat$assessed_BMI)
BMI_BMI = mean(data_data_BMI_drop_bmi_dat$assessed_BMI)

BMI_mean = c(BMI_all, 
           BMI_female, 
           BMI_ethnicity, 
           BMI_BMI)

BMI_all_sd = sd(cumulative_effects_dat$assessed_BMI)
BMI_female_sd = sd(data_female_drop_bmi_dat$assessed_BMI)
BMI_ethnicity_sd = sd(data_data_ethnicity_drop_bmi_dat$assessed_BMI)
BMI_BMI_sd = sd(data_data_BMI_drop_bmi_dat$assessed_BMI)

BMI_sd = c(BMI_all_sd, 
             BMI_female_sd, 
             BMI_ethnicity_sd, 
             BMI_BMI_sd)





demographics = cbind(subset_name, 
                     age_mean,
                     age_sd, 
                     
                     female_n, 
                     female_percent, 
                     
                     wealth_mean, 
                     wealth_sd, 
                     
                     BMI_mean, 
                     BMI_sd, 
                     
                     n_obese, 
                     percent_obese, 
                     
                     n_overweight, 
                     percent_overweight, 
                     
                     n_healthy_weight, 
                     percent_healthy_weight, 
                     
                     n_under_weight, 
                     percent_under_weight)


#write.csv(demographics, "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/demographics.csv")

