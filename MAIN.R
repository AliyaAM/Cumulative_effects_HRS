
#/Users/aliya/my_docs/KCL_postDoc/Data_analysis/HRS_2010_data/HRS_ALLData_originalVARNames.csv

library(WCE)
library(survival)
library(dplyr)
library(car)
library(tidyverse)
library(epiDisplay) #tab1 function to make a frequency table 
library(foreign)
library(rms) # Used to extract p-value from logistic model
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
"/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"

SOURCE_ROOT = "/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"
OUTPUT_ROOT = "/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"


#other sources of stress, shall we include them as covariates: job strain etc, a lit of HRS stress items are here: https://g2aging.org/?section=concordance-search&sWords=stress&interval=1992%2C2016&page=1&per_page=50&af_src=1

# other literature considers cumulatie effects as: cumulative stressors (global, weekly and major life events)
#https://bmcpublichealth.biomedcentral.com/articles/10.1186/s12889-020-08573-0 

#think how we are going to restrict to a particular type of discrimination, 
#which wave shall we use? 
#shall we state that every wave they listed disability (ie subset HRS2004_discrim_disability == 1 & HRS2008_discrim_disability ==1 etc every wave up to 2018) 

#add 2004, 2006, 2008 

#from RAND below we beed to find: #wealth # in harmonised rnad file 2008 year wealth var is: #H9ATOTW 

#RAND_data = read.csv("/Users/aliya/my_docs/KCL_postDoc/Data_analysis/randhrs1992_2018v1.csv")


#coxph

#covariates: Fixed confounding factors at baseline will include SES, sex, 
########################################################################### and for the onset of a particular  disease a history of that disease or diseases known to be a precuisite prior study recruitment (i.e. baseline)
#HRS2018_race_hispanic_latino

#HRS2018_race_white
#HRS2018_race_black
#LGB_2016
#Straight_2016
#sex_1_0_2018
#yearsof_education2018

#Time-dependent covariates will include current age 
#age_groups2018
#continious_age2018

#bind rows with bind_rows in dplyr 


# add continious age variable to 2008 dataset 
#HRS2008_data$HRS2008


HRS2008_data = read.csv(paste(SOURCE_ROOT, "HRS_2008_data/HRS2008_discrimination_dataset_march2022.csv", sep=""))
HRS2010_data = read.csv(paste(SOURCE_ROOT, "HRS_2010_data/HRS2010_discrimination_dataset_march2022.csv", sep=""))
HRS2012_data = read.csv(paste(SOURCE_ROOT, "HRS_2012_data/HRS2012_discrimination_dataset_march2022.csv", sep=""))
HRS2014_data = read.csv(paste(SOURCE_ROOT, "HRS_2014_data/HRS2014_discrimination_dataset_march2022.csv", sep=""))
HRS2016_data = read.csv(paste(SOURCE_ROOT, "HRS_2016_data/HRS2016_discrimination_dataset_march2022.csv", sep=""))
HRS2018_data = read.csv(paste(SOURCE_ROOT, "HRS_2018_data/HRS2018_discrimination_dataset_march2022.csv", sep=""))


#exposure: binary whether did experience discrimination or not (five different types, age, disability, etc)
# cumulative effects of exposure to discrimination on outcomes 
HRS2008_data$reason_discrim1_reason_disability = HRS2008_data$HRS2008_reason_discrim1_reason_disability #add to the dataset, code is on the laptop
HRS2010_data$reason_discrim1_reason_disability = HRS2010_data$HRS2010_reason_discrim1_reason_disability
HRS2012_data$reason_discrim1_reason_disability = HRS2012_data$HRS2012_reason_discrim1_reason_disability
HRS2014_data$reason_discrim1_reason_disability = HRS2014_data$HRS2014_reason_discrim1_reason_disability
HRS2016_data$reason_discrim1_reason_disability = HRS2016_data$HRS2016_reason_discrim1_reason_disability
HRS2018_data$reason_discrim1_reason_disability = HRS2018_data$HRS2018_reason_discrim1_reason_disability


#dose: the frequency of exposure, So a person who reports daily discrimination vs yearly discrimination at 1 time point
#separately for each discriinatory situation 

#####: rename the variables so they are called the same across waves

# 2008 add disability and subset the 2008 dataset above 

HRS2008_data$discrim_harassed = HRS2008_data$HRS2008_discrim_harassed 
HRS2008_data$discrim_lessrespect = HRS2008_data$HRS2008_discrim_lessrespect 
HRS2008_data$discrim_poorerservice = HRS2008_data$HRS2008_discrim_poorerservice 
HRS2008_data$discrim_notclever = HRS2008_data$HRS2008_discrim_notclever
HRS2008_data$discrim_medical = HRS2008_data$HRS2008_discrim_medical 
HRS2008_data$discrim_afraidothers = HRS2008_data$HRS2008_discrim_afraidothers 

# 2010 
HRS2010_data$discrim_harassed = HRS2010_data$HRS2010_discrim_harassed 
HRS2010_data$discrim_lessrespect = HRS2010_data$HRS2010_discrim_lessrespect 
HRS2010_data$discrim_poorerservice = HRS2010_data$HRS2010_discrim_poorerservice 
HRS2010_data$discrim_notclever = HRS2010_data$HRS2010_discrim_notclever
HRS2010_data$discrim_medical = HRS2010_data$HRS2010_discrim_medical 
HRS2010_data$discrim_afraidothers = HRS2010_data$HRS2010_discrim_afraidothers 


# 2012
HRS2012_data$discrim_harassed = HRS2012_data$HRS2012_discrim_harassed 
HRS2012_data$discrim_lessrespect = HRS2012_data$HRS2012_discrim_lessrespect 
HRS2012_data$discrim_poorerservice = HRS2012_data$HRS2012_discrim_poorerservice 
HRS2012_data$discrim_notclever = HRS2012_data$HRS2012_discrim_notclever
HRS2012_data$discrim_medical = HRS2012_data$HRS2012_discrim_medical 
HRS2012_data$discrim_afraidothers = HRS2012_data$HRS2012_discrim_afraidothers 


# 2014
HRS2014_data$discrim_harassed = HRS2014_data$HRS2014_discrim_harassed 
HRS2014_data$discrim_lessrespect = HRS2014_data$HRS2014_discrim_lessrespect 
HRS2014_data$discrim_poorerservice = HRS2014_data$HRS2014_discrim_poorerservice 
HRS2014_data$discrim_notclever = HRS2014_data$HRS2014_discrim_notclever
HRS2014_data$discrim_medical = HRS2014_data$HRS2014_discrim_medical 
HRS2014_data$discrim_afraidothers = HRS2014_data$HRS2014_discrim_afraidothers 


# 2016
HRS2016_data$discrim_harassed = HRS2016_data$HRS2016_discrim_harassed 
HRS2016_data$discrim_lessrespect = HRS2016_data$HRS2016_discrim_lessrespect 
HRS2016_data$discrim_poorerservice = HRS2016_data$HRS2016_discrim_poorerservice 
HRS2016_data$discrim_notclever = HRS2016_data$HRS2016_discrim_notclever
HRS2016_data$discrim_medical = HRS2016_data$HRS2016_discrim_medical 
HRS2016_data$discrim_afraidothers = HRS2016_data$HRS2016_discrim_afraidothers 


# 2018
HRS2018_data$discrim_harassed = HRS2018_data$HRS2018_discrim_harassed 
HRS2018_data$discrim_lessrespect = HRS2018_data$HRS2018_discrim_lessrespect 
HRS2018_data$discrim_poorerservice = HRS2018_data$HRS2018_discrim_poorerservice 
HRS2018_data$discrim_notclever = HRS2018_data$HRS2018_discrim_notclever
HRS2018_data$discrim_medical = HRS2018_data$HRS2018_discrim_medical 
HRS2018_data$discrim_afraidothers = HRS2018_data$HRS2018_discrim_afraidothers 

#composite discrimination 
## ## ## ## ADD


#create a dummy variable for the follow-up period 

HRS2008_data_n = nrow(HRS2008_data)
HRS2010_data_n = nrow(HRS2010_data)
HRS2012_data_n = nrow(HRS2012_data)
HRS2014_data_n = nrow(HRS2014_data)
HRS2016_data_n = nrow(HRS2016_data)
HRS2018_data_n = nrow(HRS2018_data)


HRS2008_data$start = rep(0, times = HRS2008_data_n)
HRS2010_data$start = rep(1, times = HRS2010_data_n)
HRS2012_data$start = rep(2, times = HRS2012_data_n)
HRS2014_data$start = rep(3, times = HRS2014_data_n)
HRS2016_data$start = rep(4, times = HRS2016_data_n)
HRS2018_data$start = rep(5, times = HRS2018_data_n)
  
HRS2008_data$stop = rep(1, times = HRS2008_data_n)
HRS2010_data$stop = rep(2, times = HRS2010_data_n)
HRS2012_data$stop = rep(3, times = HRS2012_data_n)
HRS2014_data$stop = rep(4, times = HRS2014_data_n)
HRS2016_data$stop = rep(5, times = HRS2016_data_n)
HRS2018_data$stop = rep(6, times = HRS2018_data_n)


#bind rows with bind_rows in dplyr 

#exclude partners that are younger than 50 years old: 
#there is no current age var in HRS 2008, in the codebook it says LA019 is current age but there is no such variable  therefore we could not subset HRS2008 to those younger than 50 HRS2008_data = subset(HRS2008_data, HRS2008_data$continious_age >=50)
HRS2008_data = subset(HRS2008_data, HRS2008_data$continious_age >=50)
HRS2010_data = subset(HRS2010_data, HRS2010_data$continious_age >=50)
HRS2012_data = subset(HRS2012_data, HRS2012_data$continious_age >=50)
HRS2014_data = subset(HRS2014_data, HRS2014_data$continious_age >=50)
HRS2016_data = subset(HRS2016_data, HRS2016_data$continious_age >=50)
HRS2018_data = subset(HRS2018_data, HRS2018_data$continious_age >=50)




#subset to those with a disability and those who experienced discrimination due to disability throught out the years 

HRS2008_data_lim_cond = subset(HRS2008_data, HRS2008_data$limiting_condition_bin == 1) 
HRS2010_data_lim_cond  = subset(HRS2010_data, HRS2010_data$limiting_condition_bin == 1)
HRS2012_data_lim_cond  = subset(HRS2012_data, HRS2012_data$limiting_condition_bin == 1)
HRS2014_data_lim_cond  = subset(HRS2014_data, HRS2014_data$limiting_condition_bin == 1)
HRS2016_data_lim_cond  = subset(HRS2016_data, HRS2016_data$limiting_condition_bin == 1)
HRS2018_data_lim_cond  = subset(HRS2018_data, HRS2018_data$limiting_condition_bin == 1)


#HRS2008_data = subset(HRS2008_data, HRS2008_reason_discrim1_reason_disability == 1) 
#HRS2010_data = subset(HRS2010_data, HRS2010_reason_discrim1_reason_disability == 1)
#HRS2012_data = subset(HRS2012_data, HRS2012_reason_discrim1_reason_disability == 1)
#HRS2014_data = subset(HRS2014_data, HRS2014_reason_discrim1_reason_disability == 1)
#HRS2016_data = subset(HRS2016_data, HRS2016_reason_discrim1_reason_disability == 1)
#HRS2018_data = subset(HRS2018_data, HRS2018_reason_discrim1_reason_disability == 1)

HRS2008_data_female = subset(HRS2008_data, HRS2008_data$sex_1_2 == 2) 
HRS2010_data_female = subset(HRS2010_data, HRS2010_data$sex_1_2 == 2)
HRS2012_data_female = subset(HRS2012_data, HRS2012_data$sex_1_2 == 2)
HRS2014_data_female = subset(HRS2014_data, HRS2014_data$sex_1_2 == 2)
HRS2016_data_female = subset(HRS2016_data, HRS2016_data$sex_1_2 == 2)
HRS2018_data_female = subset(HRS2018_data, HRS2018_data$sex_1_2 == 2)



HRS2008_data_religion = subset(HRS2008_data, HRS2008_data$religion_bin == 1) 
HRS2010_data_religion = subset(HRS2010_data, HRS2010_data$religion_bin == 1)
HRS2012_data_religion = subset(HRS2012_data, HRS2012_data$religion_bin == 1)
HRS2014_data_religion = subset(HRS2014_data, HRS2014_data$religion_bin == 1)
HRS2016_data_religion = subset(HRS2016_data, HRS2016_data$religion_bin == 1)
HRS2018_data_religion = subset(HRS2018_data, HRS2018_data$religion_bin == 1)




HRS2008_data_national_origin_ousideUS = subset(HRS2008_data, HRS2008_data$national_origin_ousideUS == 1) 
HRS2010_data_national_origin_ousideUS = subset(HRS2010_data, HRS2010_data$national_origin_ousideUS == 1)
HRS2012_data_national_origin_ousideUS = subset(HRS2012_data, HRS2012_data$national_origin_ousideUS == 1)
HRS2014_data_national_origin_ousideUS = subset(HRS2014_data, HRS2014_data$national_origin_ousideUS == 1)
HRS2016_data_national_origin_ousideUS = subset(HRS2016_data, HRS2016_data$national_origin_ousideUS == 1)
HRS2018_data_national_origin_ousideUS = subset(HRS2018_data, HRS2018_data$national_origin_ousideUS == 1)



HRS2008_data_race = subset(HRS2008_data, HRS2008_data$race_white == 0) 
HRS2010_data_race = subset(HRS2010_data, HRS2010_data$race_white == 0)
HRS2012_data_race = subset(HRS2012_data, HRS2012_data$race_white == 0)
HRS2014_data_race = subset(HRS2014_data, HRS2014_data$race_white == 0)
HRS2016_data_race = subset(HRS2016_data, HRS2016_data$race_white == 0)
HRS2018_data_race = subset(HRS2018_data, HRS2018_data$race_white == 0)


#assessed_BMI_2008
HRS2008_data_BMI = subset(HRS2008_data, HRS2008_data$assessed_BMI > 30) 
HRS2010_data_BMI = subset(HRS2010_data, HRS2010_data$assessed_BMI > 30)
HRS2012_data_BMI = subset(HRS2012_data, HRS2012_data$assessed_BMI > 30)
HRS2014_data_BMI = subset(HRS2014_data, HRS2014_data$assessed_BMI > 30)
HRS2016_data_BMI = subset(HRS2016_data, HRS2016_data$assessed_BMI > 30)
HRS2018_data_BMI = subset(HRS2018_data, HRS2018_data$assessed_BMI > 30)


#outcomes 
#event in WCE: 
##### onset of physical conditions (bin)
#diabetes 

HRS2008_data_lim_cond = data_frame(HRS2008_data_lim_cond$HHIDPN,
                                   HRS2008_data_lim_cond$diabetes_new,
                                   HRS2008_data_lim_cond$diabetes_ever,
                                   HRS2008_data_lim_cond$sex_1_2,
                                   HRS2008_data_lim_cond$wealth_noIRA,
                                   HRS2008_data_lim_cond$start,
                                   HRS2008_data_lim_cond$stop,
                                   HRS2008_data_lim_cond$reason_discrim1_reason_disability,
                                   
                                   HRS2008_data_lim_cond$discrim_harassed,
                                   HRS2008_data_lim_cond$discrim_lessrespect,
                                   HRS2008_data_lim_cond$discrim_medical,
                                   HRS2008_data_lim_cond$discrim_notclever,
                                   HRS2008_data_lim_cond$discrim_poorerservice,
                                   HRS2008_data_lim_cond$discrim_afraidothers,
                                   
                                   HRS2008_data_lim_cond$continious_age,
                                   HRS2008_data_lim_cond$assessed_BMI)

nrow_2008 = nrow(HRS2008_data_lim_cond) 
HRS2008_data_lim_cond$year_2008 = rep(2010, times = nrow_2008)


HRS2010_data_lim_cond = data_frame(HRS2010_data_lim_cond$HHIDPN,
                                   HRS2010_data_lim_cond$diabetes_new,
                                   HRS2010_data_lim_cond$diabetes_ever,
                                   HRS2010_data_lim_cond$sex_1_2,
                                   HRS2010_data_lim_cond$wealth_noIRA,
                                   HRS2010_data_lim_cond$start,
                                   HRS2010_data_lim_cond$stop,
                                   HRS2010_data_lim_cond$reason_discrim1_reason_disability,
                                   
                                   HRS2010_data_lim_cond$discrim_harassed,
                                   HRS2010_data_lim_cond$discrim_lessrespect,
                                   HRS2010_data_lim_cond$discrim_medical,
                                   HRS2010_data_lim_cond$discrim_notclever,
                                   HRS2010_data_lim_cond$discrim_poorerservice,
                                   
                                   HRS2010_data_lim_cond$discrim_afraidothers,
                                   HRS2010_data_lim_cond$continious_age,
                                   HRS2010_data_lim_cond$assessed_BMI)

nrow_2010 = nrow(HRS2010_data_lim_cond) 
HRS2010_data_lim_cond$year_2010 = rep(2010, times = nrow_2010)

HRS2012_data_lim_cond = data_frame(HRS2012_data_lim_cond$HHIDPN,
                                   HRS2012_data_lim_cond$diabetes_new,
                                   HRS2012_data_lim_cond$diabetes_ever,
                                   HRS2012_data_lim_cond$sex_1_2,
                                   HRS2012_data_lim_cond$wealth_noIRA,
                                   HRS2012_data_lim_cond$start,
                                   HRS2012_data_lim_cond$stop,
                                   HRS2012_data_lim_cond$reason_discrim1_reason_disability,
                                   
                                   HRS2012_data_lim_cond$discrim_harassed,
                                   HRS2012_data_lim_cond$discrim_lessrespect,
                                   HRS2012_data_lim_cond$discrim_medical,
                                   HRS2012_data_lim_cond$discrim_notclever,
                                   HRS2012_data_lim_cond$discrim_poorerservice,
                                   HRS2012_data_lim_cond$discrim_afraidothers,
                                   
                                   HRS2012_data_lim_cond$continious_age,
                                   HRS2012_data_lim_cond$assessed_BMI)

nrow_2012 = nrow(HRS2012_data_lim_cond) 
HRS2012_data_lim_cond$year_2012 = rep(2012, times = nrow_2012)


HRS2014_data_lim_cond = data_frame(HRS2014_data_lim_cond$HHIDPN,
                                   HRS2014_data_lim_cond$diabetes_new,
                                   HRS2014_data_lim_cond$diabetes_ever,
                                   HRS2014_data_lim_cond$sex_1_2,
                                   HRS2014_data_lim_cond$wealth_noIRA,
                                   HRS2014_data_lim_cond$start,
                                   HRS2014_data_lim_cond$stop,
                                   HRS2014_data_lim_cond$reason_discrim1_reason_disability,
                                   
                                   HRS2014_data_lim_cond$discrim_harassed,
                                   HRS2014_data_lim_cond$discrim_lessrespect,
                                   HRS2014_data_lim_cond$discrim_medical,
                                   HRS2014_data_lim_cond$discrim_notclever,
                                   HRS2014_data_lim_cond$discrim_poorerservice,
                                   HRS2014_data_lim_cond$discrim_afraidothers,
                                   
                                   HRS2014_data_lim_cond$continious_age,
                                   HRS2014_data_lim_cond$assessed_BMI)


nrow_2014 = nrow(HRS2014_data_lim_cond) 
HRS2014_data_lim_cond$year_2014 = rep(2014, times = nrow_2014)

HRS2016_data_lim_cond$HHIDPN_HRS2016 = HRS2016_data_lim_cond$HHIDPN


HRS2016_data_lim_cond = data_frame(HRS2016_data_lim_cond$HHIDPN,
                                   HRS2016_data_lim_cond$diabetes_new,
                                   HRS2016_data_lim_cond$diabetes_ever,
                                   HRS2016_data_lim_cond$sex_1_2,
                                   HRS2016_data_lim_cond$wealth_noIRA,
                                   
                                   HRS2016_data_lim_cond$start,
                                   HRS2016_data_lim_cond$stop,
                                  
                                   HRS2016_data_lim_cond$reason_discrim1_reason_disability,
                                   
                                   HRS2016_data_lim_cond$discrim_harassed,
                                   HRS2016_data_lim_cond$discrim_lessrespect,
                                   HRS2016_data_lim_cond$discrim_medical,
                                   HRS2016_data_lim_cond$discrim_notclever,
                                   HRS2016_data_lim_cond$discrim_poorerservice,
                                   HRS2016_data_lim_cond$discrim_afraidothers,
                                   
                                   HRS2016_data_lim_cond$continious_age,
                                   HRS2016_data_lim_cond$assessed_BMI)

nrow_2016 = nrow(HRS2016_data_lim_cond) 
HRS2016_data_lim_cond$year_2016 = rep(2016, times = nrow_2016)


HRS2018_data_lim_cond$HHIDPN = HRS2018_data_lim_cond$HHIDPN_HRS2018


HRS2018_data_lim_cond = data_frame(HRS2018_data_lim_cond$HHIDPN,
                                   HRS2018_data_lim_cond$diabetes_new,
                                   HRS2018_data_lim_cond$diabetes_ever,
                                   HRS2018_data_lim_cond$sex_1_2,
                                   HRS2018_data_lim_cond$wealth_noIRA,
                                   
                                   HRS2018_data_lim_cond$start,
                                   HRS2018_data_lim_cond$stop,
                                   
                                   HRS2018_data_lim_cond$reason_discrim1_reason_disability,
                                   
                                   HRS2018_data_lim_cond$discrim_harassed,
                                   HRS2018_data_lim_cond$discrim_lessrespect,
                                   HRS2018_data_lim_cond$discrim_medical,
                                   HRS2018_data_lim_cond$discrim_notclever,
                                   HRS2018_data_lim_cond$discrim_poorerservice,
                                   HRS2018_data_lim_cond$discrim_afraidothers,
                                   
                                   HRS2018_data_lim_cond$continious_age, 
                                   HRS2018_data_lim_cond$assessed_BMI)

nrow_2018 = nrow(HRS2018_data_lim_cond) 
HRS2018_data_lim_cond$year_2018= rep(2018, times = nrow_2018)


colnames_all = c('HHIDPN',
         "diabetes_new",
         "diabetes_ever",
         "sex_1_2",
         "wealth_noIRA",
         "start",
         "stop",
         "reason_discrim1_reason_disability",
         "discrim_harassed",
         "discrim_lessrespect",
         "discrim_medical",
         "discrim_notclever",
         "discrim_poorerservice",
         "discrim_afraidothers",
         "continious_age",
         "assessed_BMI")

colnames(HRS2008_data_lim_cond) = colnames_all
colnames(HRS2010_data_lim_cond) = colnames_all
colnames(HRS2012_data_lim_cond) = colnames_all
colnames(HRS2014_data_lim_cond) = colnames_all
colnames(HRS2016_data_lim_cond) = colnames_all
colnames(HRS2018_data_lim_cond) = colnames_all

nrow(HRS2008_data_lim_cond)
nrow(HRS2010_data_lim_cond)
nrow(HRS2012_data_lim_cond)
nrow(HRS2014_data_lim_cond)
nrow(HRS2016_data_lim_cond)
nrow(HRS2018_data_lim_cond)


#HRS2018_data_lim_cond = subset(HRS2018_data_lim_cond, HHIDPN %in% HRS2016_data_lim_cond$HHIDPN)
#HRS2016_data_lim_cond = subset(HRS2016_data_lim_cond, HHIDPN %in% HRS2014_data_lim_cond$HHIDPN)
#HRS2014_data_lim_cond = subset(HRS2014_data_lim_cond, HHIDPN %in% HRS2012_data_lim_cond$HHIDPN)

#HRS2012_data_lim_cond = subset(HRS2012_data_lim_cond, HHIDPN %in% HRS2010_data_lim_cond$HHIDPN)
#HRS2010_data_lim_cond = subset(HRS2010_data_lim_cond, HHIDPN %in% HRS2018_data_lim_cond$HHIDPN)

#HRS2016_data_lim_cond =  subset(HRS2016_data_lim_cond, HHIDPN %in% HRS2012_data_lim_cond$HHIDPN)
#HRS2018_data_lim_cond = subset(HRS2018_data_lim_cond, HHIDPN %in% HRS2012_data_lim_cond$HHIDPN)


#data_1 = HRS2008_data_lim_cond %>% inner_join(HRS2010_data_lim_cond,by="HHIDPN")
#data_2 = data_1 %>% inner_join(HRS2012_data_lim_cond,by="HHIDPN")
#data_3 = data_2 %>% inner_join(HRS2014_data_lim_cond,by="HHIDPN")
#data_4 = data_3 %>% inner_join(HRS2016_data_lim_cond,by="HHIDPN")
#reduced_data_frame_lim = data_4 %>% inner_join(HRS2018_data_lim_cond,by="HHIDPN")



WCE_dataset_lim_cond = rbind(HRS2008_data_lim_cond, HRS2010_data_lim_cond, HRS2012_data_lim_cond, HRS2014_data_lim_cond, HRS2016_data_lim_cond, HRS2018_data_lim_cond)


WCE_dataset_lim_cond = rbind(HRS2008_data_lim_cond, HRS2010_data_lim_cond, HRS2012_data_lim_cond, HRS2014_data_lim_cond, HRS2016_data_lim_cond, HRS2018_data_lim_cond)

WCE_dataset_lim_cond = WCE_dataset_lim_cond %>% drop_na()
WCE_dataset_lim_cond$diabetes_new_bin = case_when(WCE_dataset_lim_cond$diabetes_new ==0 ~ 0, 
                                                  WCE_dataset_lim_cond$diabetes_new ==1 ~ 1) 




WCE_lim_cond = as.data.frame(WCE_dataset_lim_cond$HHIDPN)
colnames(WCE_lim_cond) = "HHIDPN"

WCE_lim_cond$Start = as.numeric(WCE_dataset_lim_cond$start)
WCE_lim_cond$Stop = as.numeric(WCE_dataset_lim_cond$stop)

WCE_lim_cond$discrim_harassed_num = as.numeric(WCE_dataset_lim_cond$discrim_harassed)
WCE_lim_cond$diabetes_new_bin_num = as.numeric(WCE_dataset_lim_cond$diabetes_new_bin)
WCE_lim_cond$assessed_BMI_num = as.numeric(WCE_dataset_lim_cond$assessed_BMI)
WCE_lim_cond$continious_age_num = as.numeric(WCE_dataset_lim_cond$continious_age)


table(by(WCE_lim_cond$Start, WCE_lim_cond$HHIDPN, min)) 

apply(is.na(WCE_lim_cond)==1, 2, sum)
apply(is.na(WCE_dataset_lim_cond)==1, 2, sum)


unique(WCE_lim_cond$diabetes_new_bin_num)

WCE_lim_cond = WCE_lim_cond %>% drop_na()

write.csv(WCE_lim_cond, paste(SOURCE_ROOT, "WCE_lim_cond.csv", sep=""))
write.csv(WCE_dataset_lim_cond, paste(SOURCE_ROOT, "WCE_dataset_lim_cond.csv", sep=""))

