
# plots: 
# https://adibender.github.io/pammtools/articles/cumulative_effects.html
#https://rpkgs.datanovia.com/ggpubr/reference/stat_regline_equation.html Adding text (coefficients etc) to the plot next to the regression line 

#current_directory = "/Users/k2147340/OneDrive - King's College London/Documents/"

current_directory = "/Users/aliya/my_docs/"
#current_directory = "/Users/aliyaamirova/proj/Cumulative_effects_HRS"

OUTPUT_ROOT = paste(current_directory, "proj/Cumulative_effects_HRS/Reviewer_response/Version_2_analysis/RESULTS/", sep="")
SOURCE_ROOT = paste(current_directory, "proj/Cumulative_effects_HRS/Reviewer_response/Version_2_analysis/", sep="")
DATA_ROOT = paste(current_directory, "/ELSA_HRS/Data_analysis/", sep = "") 



library("survival")
library("survminer")
library("dplyr")
library("ggplot2")


###### DATA:
# below is the entire dataset, not subseted to anyone: 

cumulative_effects_dat_initial = read.csv(paste(OUTPUT_ROOT, "all_waves_nodiabatbaseline_DIAB.csv", sep =""))
nrow(cumulative_effects_dat_initial)


#exclude participants with cardiometabolic disease at baseline: 

cases_with_CVD = subset(cumulative_effects_dat_initial,  CVD_ever == 1 & start_new == 0)

exclude_ids = unique(cases_with_CVD$HHIDPN)


cumulative_effects_dat <- subset(cumulative_effects_dat_initial,  !(HHIDPN %in% exclude_ids))


unique(cumulative_effects_dat$CVD_ever)
unique(cumulative_effects_dat_initial$start_new)
nrow(cumulative_effects_dat)
nrow(cumulative_effects_dat_initial)



###### Adding variables to the main dataset:

#outcome:  cumulative_effects_dat$diabetes_new_bin
#exposure: cumulative_effects_dat$discrim_bin
#time: cumulative_effects_dat$stop_new

###### Adding variables to the main dataset:
###### Adding variables to the main dataset:


# cumulative_effects_dat$discrim_harassed_bin = case_when(cumulative_effects_dat$discrim_harassed == 1 ~ 1, 
#                                                         cumulative_effects_dat$discrim_harassed == 2 ~ 1, 
#                                                         cumulative_effects_dat$discrim_harassed == 3 ~ 1, 
#                                                         cumulative_effects_dat$discrim_harassed == 4 ~ 1, 
#                                                         cumulative_effects_dat$discrim_harassed == 5 ~ 0, 
#                                                         cumulative_effects_dat$discrim_harassed == 6 ~ 0,
#                                                         cumulative_effects_dat$discrim_harassed == 0 ~ 0) 
# 
# 
# 
# cumulative_effects_dat$discrim_lessrespect_bin = case_when(cumulative_effects_dat$discrim_lessrespect == 1 ~ 1, 
#                                                            cumulative_effects_dat$discrim_lessrespect == 2 ~ 1, 
#                                                            cumulative_effects_dat$discrim_lessrespect == 3 ~ 1, 
#                                                            cumulative_effects_dat$discrim_lessrespect == 4 ~ 1, 
#                                                            cumulative_effects_dat$discrim_lessrespect == 5 ~ 0, 
#                                                            cumulative_effects_dat$discrim_lessrespect == 6 ~ 0,
#                                                            cumulative_effects_dat$discrim_lessrespect == 0 ~ 0) 
# 
# 
# 
# cumulative_effects_dat$discrim_medical_bin = case_when(cumulative_effects_dat$discrim_medical == 1 ~ 1, 
#                                                        cumulative_effects_dat$discrim_medical == 2 ~ 1, 
#                                                        cumulative_effects_dat$discrim_medical == 3 ~ 1, 
#                                                        cumulative_effects_dat$discrim_medical == 4 ~ 1, 
#                                                        cumulative_effects_dat$discrim_medical == 5 ~ 0, 
#                                                        cumulative_effects_dat$discrim_medical == 6 ~ 0,
#                                                        cumulative_effects_dat$discrim_medical == 0 ~ 0) 
# 
# 
# 
# 
# 
# cumulative_effects_dat$discrim_notclever_bin = case_when(cumulative_effects_dat$discrim_notclever == 1 ~ 1, 
#                                                          cumulative_effects_dat$discrim_notclever == 2 ~ 1, 
#                                                          cumulative_effects_dat$discrim_notclever == 3 ~ 1, 
#                                                          cumulative_effects_dat$discrim_notclever == 4 ~ 1, 
#                                                          cumulative_effects_dat$discrim_notclever == 5 ~ 0, 
#                                                          cumulative_effects_dat$discrim_notclever == 6 ~ 0,
#                                                          cumulative_effects_dat$discrim_notclever == 0 ~ 0) 
# 
# 
# 
# 
# 
# 
# cumulative_effects_dat$discrim_poorerservice_bin = case_when(cumulative_effects_dat$discrim_poorerservice == 1 ~ 1, 
#                                                              cumulative_effects_dat$discrim_poorerservice == 2 ~ 1, 
#                                                              cumulative_effects_dat$discrim_poorerservice == 3 ~ 1, 
#                                                              cumulative_effects_dat$discrim_poorerservice == 4 ~ 1, 
#                                                              cumulative_effects_dat$discrim_poorerservice == 5 ~ 0, 
#                                                              cumulative_effects_dat$discrim_poorerservice == 6 ~ 0) 
# 
# 
# 
# 
# cumulative_effects_dat$discrim_afraidothers_bin = case_when(cumulative_effects_dat$discrim_afraidothers == 1 ~ 1,
#                                                             cumulative_effects_dat$discrim_afraidothers == 2 ~ 1, 
#                                                             cumulative_effects_dat$discrim_afraidothers == 3 ~ 1, 
#                                                             cumulative_effects_dat$discrim_afraidothers == 4 ~ 1, 
#                                                             cumulative_effects_dat$discrim_afraidothers == 5 ~ 0, 
#                                                             cumulative_effects_dat$discrim_afraidothers == 6 ~ 0,
#                                                             cumulative_effects_dat$discrim_afraidothers == 0 ~ 0) 
# 
# 
# 
# cumulative_effects_dat$discrim_bin = case_when(cumulative_effects_dat$discrim_harassed_bin == 1 | cumulative_effects_dat$discrim_lessrespect_bin == 1 | cumulative_effects_dat$discrim_medical_bin  == 1 | cumulative_effects_dat$discrim_notclever_bin == 1 | cumulative_effects_dat$discrim_afraidothers_bin == 1 | cumulative_effects_dat$discrim_poorerservice_bin == 1 ~ 1, 
#                                                cumulative_effects_dat$discrim_harassed_bin == 0 & cumulative_effects_dat$discrim_lessrespect_bin == 0 & cumulative_effects_dat$discrim_medical_bin  == 0 & cumulative_effects_dat$discrim_notclever_bin == 0 & cumulative_effects_dat$discrim_afraidothers_bin == 0 & cumulative_effects_dat$discrim_poorerservice_bin == 0 ~ 0) 
# 
# 
# 
# 
# cumulative_effects_dat$discrimination = cumulative_effects_dat$discrim_bin 
# 
# 
# 
# cumulative_effects_dat$discrimination_reversed = case_when(cumulative_effects_dat$discrim_bin == 1 ~ 0,
#                                                            cumulative_effects_dat$discrim_bin == 0 ~ 1)

unique(cumulative_effects_dat$start_new)

cumulative_effects_dat$time_point = cumulative_effects_dat$start_new

cumulative_effects_dat$years = 2 * cumulative_effects_dat$start_new

cumulative_effects_dat$months = 12 * cumulative_effects_dat$years

cumulative_effects_dat$follow_up = cumulative_effects_dat$years


#1 = 2 year 
#2 = 4 follow_up 
#3 = 6 follow_up 


unique(cumulative_effects_dat$start_new)

sd(cumulative_effects_dat$start_new)

cumulative_effects_dat$diabetes_new_bin = case_when(cumulative_effects_dat$diabetes_new == 1 ~ 1, 
                                                    cumulative_effects_dat$diabetes_new == 0 ~ 0) 


cumulative_effects_dat$diabetes_new_bin_reversed = case_when(cumulative_effects_dat$diabetes_new_bin == 1 ~ 0, 
                                                             cumulative_effects_dat$diabetes_new_bin == 0 ~ 1) 


#cumulative_effects_dat$sex_1_2
#cumulative_effects_dat$race_white
#cumulative_effects_dat$race_white
#cumulative_effects_dat$national_origin_ousideUS_bin
#cumulative_effects_dat$religion_bin
#cumulative_effects_dat$assessed_BMI

##### discrim_harassed, discrim_lessrespect, discrim_medical, discrim_notclever, discrim_poorerservice, 


cor_matrix <- cor(cumulative_effects_dat[,c("discrim_harassed", "discrim_lessrespect", "discrim_medical", "discrim_notclever", "discrim_poorerservice")], use="complete.obs")
print(cor_matrix)


cumulative_effects_dat$mean_discrimination <- rowMeans(cumulative_effects_dat[,c("discrim_harassed", "discrim_lessrespect", "discrim_medical", "discrim_notclever", "discrim_poorerservice")], na.rm=TRUE)


# unadjusted: 
fit_mean_discrim <- coxph(Surv(follow_up, diabetes_new_bin) ~ mean_discrimination, data = cumulative_effects_dat)
summary(fit_mean_discrim)




## Model 1: age, sex, wealth, education, race (Black, Hispanic, Other, White) (basic adjustment)
Model_1 = c("continious_age", "wealth_noIRA", "sex_1_2", 
            "education_level",
            #"national_origin_ousideUS_bin",
            "race_white")

fit_mean_discrim_model_1 <- coxph(Surv(follow_up, diabetes_new_bin) ~ mean_discrimination + continious_age + wealth_noIRA + sex_1_2 + race_white + education_level, data = cumulative_effects_dat)
summary(fit_mean_discrim)


## Model 2: age, sex, wealth, education, race (Black, Hispanic, Other, White)  BMI, hypertension (basic adjustment + known type 2 diabetes risk factors)
Model_2 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "hypertension_new_bin",
            "education_level", 
            #"national_origin_ousideUS_bin",
            "race_white")


fit_mean_discrim_model_2 <- coxph(Surv(follow_up, diabetes_new_bin) ~ mean_discrimination + continious_age + wealth_noIRA + sex_1_2 + assessed_BMI + hypertension_new_bin + race_white + education_level, data = cumulative_effects_dat)
summary(fit_mean_discrim)


## Model 3: age, sex, wealth, education, race (Black, Hispanic, Other, White) physical activity, smoking (yes/no), and alcohol (days/week)(basic adjustment + health behaviours)
Model_3 = c("continious_age", "wealth_noIRA", "sex_1_2",   "vigarious_physical_activity_new", 
            "education_level", 
            #"national_origin_ousideUS_bin", 
            "race_white" )

fit_mean_discrim_model_3 <- coxph(Surv(follow_up, diabetes_new_bin) ~ mean_discrimination + continious_age + wealth_noIRA + sex_1_2 + vigarious_physical_activity_new  + race_white + education_level, data = cumulative_effects_dat)
summary(fit_mean_discrim)


## Model 4: age, sex, wealth, education, race (Black, Hispanic, Other, White), depression (basic adjustment + depression)
Model_4 = c("continious_age","wealth_noIRA", "sex_1_2", "checklist_depression_bin", 
            "education_level", 
            #"national_origin_ousideUS_bin", 
            "race_white")

fit_mean_discrim_model_4 <- coxph(Surv(follow_up, diabetes_new_bin) ~ mean_discrimination + continious_age + wealth_noIRA + sex_1_2 + checklist_depression_bin  + race_white + education_level, data = cumulative_effects_dat)
summary(fit_mean_discrim)


########
########
########
########

