
library("dplyr")
library("tidyr")
library("bain")

current_directory = "/Users/aliya/my_docs/"


DATAIN_ROOT = (paste(current_directory, "KCL_postDoc/Data_analysis/", sep="")) 
SOURCE_ROOT = (paste(current_directory, "proj/Cumulative_effects_HRS/Version_2_analysis/", sep=""))
OUTPUT_ROOT =(paste(current_directory, "KCL_postDoc/Cumulative_effects/", sep=""))


##### #Compare our analytic sample and the total HRS sample 
analytical_sample_COX = read.csv("/Users/aliya/my_docs/proj/Cumulative_effects_HRS/data_files/all_waves_nodiabatbaseline_DIAB.csv")


analytical_sample_COX = subset(analytical_sample_COX, analytical_sample_COX$start_new == 0)
HRS2010_data_initial = subset(HRS2010_data_initial, HRS2010_data_initial$start_new == 0)

nrow(analytical_sample_COX)
nrow(HRS2010_data_initial)

HRS2010_data_initial = read.csv(paste(DATAIN_ROOT, "HRS_2010_data/old/HRS2010_data_short_OLD.csv", sep=""))

nrow(HRS2010_data_initial)
##### ##### #####  #####  ##### ##### #Run t-tests comparing diff. sample characteristics between those who developed T2DM and those who did not

analytical_sample_COX$study_source = rep(1, times = nrow(analytical_sample_COX))
HRS2010_data_initial$study_source = rep(0, times = nrow(HRS2010_data_initial)) 

case = c(analytical_sample_COX$study_source, HRS2010_data_initial$study_source)
age = c(analytical_sample_COX$continious_age, HRS2010_data_initial$continious_age)
sex = c(analytical_sample_COX$sex_1_2, HRS2010_data_initial$sex_1_2)

#BMI kg/m2, mean (SD)

BMI = c(analytical_sample_COX$assessed_BMI, HRS2010_data_initial$assessed_BMI)

#CVD,  n (%)

CVD = c(analytical_sample_COX$CVD, HRS2010_data_initial$CVD)

#Hypertension, n (%)

hypertension = c(analytical_sample_COX$hypertension_new_bin, HRS2010_data_initial$hypertension_new_bin)


#Depression, n (%)

depression = c(analytical_sample_COX$checklist_depression_bin, HRS2010_data_initial$checklist_depression_bin)


#Alcohol consumption (days/week), Mean (SD)

Alcohol_consumption  = c(analytical_sample_COX$alcohol_days_week, HRS2010_data_initial$alcohol_days_week)
Alcohol_consumption = as.numeric(Alcohol_consumption)

#Smoker status, n (%)

Smoking_status  = c(analytical_sample_COX$smokes_now_bin, HRS2010_data_initial$smokes_now_bin)


#MVPA frequency, median
MVPA  = c(analytical_sample_COX$vigarious_physical_activity, HRS2010_data_initial$vigarious_physical_activity)



data_ttest = cbind(case, 
                   age, 
                   sex, 
                   BMI,
                   #CVD,  n (%)
                   CVD,
                   #Hypertension, n (%)
                   hypertension, 
                   #Depression, n (%)
                   depression,
                   #Alcohol consumption (days/week), Mean (SD)
                   #Alcohol_consumption,  
                   #Smoker status, n (%)
                   Smoking_status,
                   MVPA)

data_ttest = as.data.frame(data_ttest)

nrow(data_ttest)

data_ttest$age

# 64.32641 vs      63.52677, p-value >0.000, t = 7.77 (df = 48320)
#our sample was younger by a year on average

#BMI = 29.95  vs    BMI = 29.19 t = 8.31 (df = 18288), p-value < 0.000, our sample had a slighly lower BMI but still significantly different. 

age_diff <- t_test(age ~ case, data = data_ttest)
BMI_diff <- t_test(BMI ~ case, data = data_ttest)

#Alcohol_consumption_diff: t = 3.5297, df = 14015, p-value = 0.0004173; 1.983630 vs 1.873899 (our sample consumed alcohol fewer days than the total HRS sample)
Alcohol_consumption_diff <- t_test(Alcohol_consumption ~ case, data = data_ttest)

#MVPA: t = 4.484, df = 47815, p-value = 7.343e-06, 3.927857   vs    3.873475
MVPA_diff <- t_test(MVPA ~ case, data = data_ttest)


chisq.test(case, sex)
table(case, sex)
# sex: chi-squared = 2.65 (df = 1), p-value = 0.1034, women in the total sample: 15220; women in our sample = 13084 

#chisq.test(case, CVD)
#table(case, CVD)

# CVD: X-squared = 26782, df = 49512, p-value = 1

chisq.test(case, hypertension)
table(case, hypertension)

# hypertension: X-squared = 59.46, df = 1, p-value = 1.248e-14, in the total sample: 2802; in our sample: 1758

chisq.test(case, depression)
table(case, depression)

#depression: X-squared = 7.0715, df = 1, p-value = 0.007832; in the total sample: 3505; in our sample: 2815

chisq.test(case, Alcohol_consumption)
table(case, Alcohol_consumption)
#Alcohol_consumption (days/week): X-squared = 71.336, df = 9, p-value = 8.324e-12 ; 

#Smoking_status: X-squared = 12105, df = 1, p-value < 2.2e-16, 
chisq.test(case, Smoking_status)
table(case, Smoking_status)

#MVPA: X-squared = 26.194, df = 4, p-value = 2.892e-05
chisq.test(case, MVPA)
table(case, MVPA)

