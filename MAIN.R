
library(WCE)
library(survival)
#/Users/aliya/my_docs/KCL_postDoc/Data_analysis/HRS_2010_data/HRS_ALLData_originalVARNames.csv


library(WCE)
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

#other sources of stress, shall we include them as covariates: job strain etc, a lit of HRS stress items are here: https://g2aging.org/?section=concordance-search&sWords=stress&interval=1992%2C2016&page=1&per_page=50&af_src=1

# other literature considers cumulatie effects as: cumulative stressors (global, weekly and major life events)
#https://bmcpublichealth.biomedcentral.com/articles/10.1186/s12889-020-08573-0 

#think how we are going to restrict to a particular type of discrimination, 
#which wave shall we use? 
#shall we state that every wave they listed disability (ie subset HRS2004_discrim_disability == 1 & HRS2008_discrim_disability ==1 etc every wave up to 2018) 

#add 2004, 2006, 2008 


#RAND_data = read.csv("/Users/aliya/my_docs/KCL_postDoc/Data_analysis/randhrs1992_2018v1.csv")
#coxph
#/Users/aliya/my_docs/KCL_postDoc/Data_analysis/HRS_2008_data/HRS2008_discrimination_dataset.csv
HRS2008_data = read.csv("/Users/aliya/my_docs/KCL_postDoc/Data_analysis/HRS_2008_data/HRS2008_discrimination_dataset.csv")
HRS2010_data = read.csv("/Users/aliya/my_docs/KCL_postDoc/Data_analysis/HRS_2010_data/HRS2010_discrimination_dataset_new.csv")
HRS2012_data = read.csv("/Users/aliya/my_docs/KCL_postDoc/Data_analysis/HRS_2012_data/HRS2012_discrimination_dataset_new.csv")
HRS2014_data = read.csv("/Users/aliya/my_docs/KCL_postDoc/Data_analysis/HRS_2014_data/HRS2014_discrimination_dataset_new.csv")
HRS2016_data = read.csv("/Users/aliya/my_docs/KCL_postDoc/Data_analysis/HRS_2016_data/HRS2016_discrimination_dataset_new.csv")
HRS2018_data = read.csv("/Users/aliya/my_docs/KCL_postDoc/Data_analysis/HRS_2018_data/HRS2018_discrimination_dataset_new.csv")

# add continious age variable to 2008 dataset 
#HRS2008_data$HRS2008
head(HRS2008_data)

# drop variables that we are not using in 2010 dataset 
#ls(HRS2010_data)

#exclude partners that are younger than 50 years old: 
#there is no current age var in HRS 2008, in the codebook it says LA019 is current age but there is no such variable  therefore we could not subset HRS2008 to those younger than 50 HRS2008_data = subset(HRS2008_data, HRS2008_data$continious_age >=50)
HRS2010_data = subset(HRS2010_data, HRS2010_data$continious_age >=50)
HRS2012_data = subset(HRS2012_data, HRS2012_data$continious_age >=50)
HRS2014_data = subset(HRS2014_data, HRS2014_data$continious_age >=50)
HRS2016_data = subset(HRS2016_data, HRS2016_data$continious_age >=50)
HRS2018_data = subset(HRS2018_data, HRS2018_data$continious_age >=50)


########### match by id each wave
HRS2008_data$HHIDPN = HRS2008_data$PN
HRS2010_data$HHIDPN = HRS2010_data$HHIDPN
HRS2012_data$HHIDPN = HRS2012_data$HHIDPN_HRS2012
HRS2014_data$HHIDPN = HRS2014_data$HHIDPN_HRS2014
HRS2016_data$HHIDPN = HRS2016_data$HHIDPN_HRS2016
HRS2018_data$HHIDPN = HRS2018_data$HHIDPN_HRS2018

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


#subset to those with a disability and those who experienced discrimination due to disability throught out the years 
HRS2008_reason_discrim

#HRS2008_data = subset(HRS2008_data, HRS2008_reason_discrim1_reason_disability == 1) #add to the dataset, code is on the laptop
HRS2010_data = subset(HRS2010_data, HRS2010_reason_discrim1_reason_disability == 1)
HRS2012_data = subset(HRS2012_data, HRS2012_reason_discrim1_reason_disability == 1)
HRS2014_data = subset(HRS2014_data, HRS2014_reason_discrim1_reason_disability == 1)
HRS2016_data = subset(HRS2016_data, HRS2016_reason_discrim1_reason_disability == 1)
HRS2018_data = subset(HRS2018_data, HRS2018_reason_discrim1_reason_disability == 1)

print("stop 1")
assert:: stop 
print("stop 2")

#exposure: binary whether did experience discrimination or not (five different types, age, disability, etc)
# cumulative effects of exposure to discrimination on outcomes 
#HRS2008_data$reason_discrim1_reason_disability = HRS2008_data$HRS2008_reason_discrim1_reason_disability #add to the dataset, code is on the laptop
HRS2010_data$reason_discrim1_reason_disability = HRS2010_data$HRS2010_reason_discrim1_reason_disability
HRS2012_data$reason_discrim1_reason_disability = HRS2012_data$HRS2012_reason_discrim1_reason_disability
HRS2014_data$reason_discrim1_reason_disability = HRS2014_data$HRS2014_reason_discrim1_reason_disability
HRS2016_data$reason_discrim1_reason_disability = HRS2016_data$HRS2016_reason_discrim1_reason_disability
HRS2018_data$reason_discrim1_reason_disability = HRS2018_data$HRS2018_reason_discrim1_reason_disability


#dose: the frequency of exposure, So a person who reports daily discrimination vs yearly discrimination at 1 time point
#separately for each discriinatory situation 

#####: rename the variables so they are calle dthe same across waves

# 2008 add disability and subset the 2008 dataset above 

#HRS2008_data$discrim_harassed = HRS2008_data$HRS2008_discrim_harassed 
#HRS2008_data$discrim_lessrespect = HRS2008_data$HRS2008_discrim_lessrespect 
#HRS2008_data$discrim_poorerservice = HRS2008_data$HRS2008_discrim_poorerservice 
#HRS2008_data$discrim_notclever = HRS2008_data$HRS2008_discrim_notclever
#HRS2008_data$discrim_medical = HRS2008_data$HRS2008_discrim_medical 
#HRS2008_data$discrim_afraidothers = HRS2008_data$HRS2008_discrim_afraidothers 

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

#outcomes 
#event in WCE: 
##### onset of physical conditions:
##### #####  heart attack, 
HRS2008_data$heartattack2yrs_bin = HRS2008_data$HRS2008_heartattack2yrs_bin


HRS2010_data$HRS2010_heartattack2yrs_bin

#add below
#HRS2012_data$HRS2012_heartattack2yrs_bin
#HRS2014_data$HRS2014_heartattack2yrs_bin
#HRS2016_data$HRS2016_heartattack2yrs_bin
#HRS2018_data$HRS2018_heartattack2yrs_bin



##### ##### angina, 
HRS2008_data$angina2yrs_bin = HRS2008_data$HRS2008_angina2yrs_bin
HRS2010_data$angina2yrs_bin = HRS2010_data$HRS2010_angina2yrs_bin

#add below
#HRS2014_data$HRS2014_angina2yrs_bin 
#HRS2016_data$HRS2016_angina2yrs_bin
#HRS2018_data$HRS2018_angina2yrs_bin



##### ##### hypertension, 
HRS2008_data$HRS2008_hypertension_new
HRS2010_data$HRS2010_hypertension_new
#add below
#HRS2012_data$HRS2012_hypertension_new
#HRS2014_data$HRS2014_hypertension_new
#HRS2016_data$HRS2016_hypertension_new
#HRS2018_data$HRS2018_hypertension_new


##### ##### diabetes,
HRS2008_data$HRS2008_diabetes_new
HRS2010_data$HRS2010_diabetes_new
#add below

#HRS2012_data$HRS2012_diabetes_new
#HRS2014_data$HRS2014_diabetes_new
#HRS2016_data$HRS2016_diabetes_new
#HRS2018_data$HRS2018_diabetes_new

##### ##### stroke, 
HRS2008_data$HRS2008_stroke_new
HRS2010_data$HRS2010_stroke_new

#dd below
#HRS2012_data$HRS2012_stroke_new
#HRS2014_data$HRS2014_stroke_new
#HRS2016_data$HRS2016_stroke_new
#HRS2018_data$HRS2018_stroke_new


##### ##### heart failure,
HRS2008_data$HRS2008_heartfailure2yrs_bin
HRS2010_data$HRS2010_heartfailure2yrs_bin

#add below
#HRS2012_data$HRS2012_heartfailure2yrs_bin
#HRS2014_data$HRS2014_heartfailure2yrs_bin
#HRS2016_data$HRS2016_heartfailure2yrs_bin
#HRS2018_data$HRS2018_heartfailure2yrs_bin


#other heart conditions 

HRS2008_data$HRS2008_heartcondition_new

HRS2010_data$HRS2010_heartcondition_new

#add below 
#HRS2012_data$HRS2012_heartcondition_new
#HRS2014_data$HRS2014_heartcondition_new
#HRS2016_data$HRS2016_heartcondition_new
#HRS2018_data$HRS2018_heartcondition_new

HRS2008_data$HRS2008_heartcondition_bin

HRS2010_data$HRS2010_heartcondition_new

#add below 
#HRS2012_data$HRS2012_heartcondition_new
#HRS2014_data$HRS2014_heartcondition_new
#HRS2016_data$HRS2016_heartcondition_new
#HRS2018_data$HRS2018_heartcondition_new

HRS2008_data$HRS2008_heartcondition_ever
HRS2010_data$HRS2010_heartcondition_ever

#add below 
#HRS2012_data$HRS2012_heartcondition_ever
#HRS2014_data$HRS2014_heartcondition_ever
#HRS2016_data$HRS2016_heartcondition_ever
#HRS2018_data$HRS2018_heartcondition_ever

#HRS2012_data$HRS2012_
#HRS2014_data$HRS2014_
#HRS2016_data$HRS2016_
#HRS2018_data$HRS2018_

##### ##### #####  ##### #####   other conditions 

##### ##### arthritis, 
HRS2008_data$HRS2008_arthritis_new
#HRS2012_data$HRS2012_
#HRS2014_data$HRS2014_
#HRS2016_data$HRS2016_
#HRS2018_data$HRS2018_

##### ##### lung disease, 
HRS2008_data$HRS2008_lungdisease_new

#HRS2012_data$HRS2012_
#HRS2014_data$HRS2014_
#HRS2016_data$HRS2016_
#HRS2018_data$HRS2018_

##### ##### cancer,
HRS2008_data$HRS2008_cancer_new

#HRS2012_data$HRS2012_
#HRS2014_data$HRS2014_
#HRS2016_data$HRS2016_
#HRS2018_data$HRS2018_



##### ##### depression 
HRS2008_data$HRS2008_checklist_depression_bin

#HRS2012_data$HRS2012_
#HRS2014_data$HRS2014_
#HRS2016_data$HRS2016_
#HRS2018_data$HRS2018_

##### and psychiatric disorders.
HRS2008_data$HRS2008_emo_psychiat_prob_new
HRS2008_data$HRS2008_emo_psychiat_prob_bin

#HRS2012_data$HRS2012_
#HRS2014_data$HRS2014_
#HRS2016_data$HRS2016_
#HRS2018_data$HRS2018_


##### ##### #####  ##### #####continious outcomes 
##### self-rated health,
HRS2008_srh

#HRS2012_data$HRS2012_
#HRS2014_data$HRS2014_
#HRS2016_data$HRS2016_
#HRS2018_data$HRS2018_

##### weight,
HRS2018_weight_kg

HRS2018obese_bin
HRS2018_underweight_bin
HRS2018_normalweight_bin
HRS2018_BMI

##### blood pressure, 
## ## ## ## ADD

##### depressive symptoms (CES-D), 
## ## ## ## ADD

##### life satisfaction (SWLS), 
## ## ## ## ADD

########################################################################
########################################################################



#covariates: Fixed confounding factors at baseline will include SES, sex, 
########################################################################### and for the onset of a particular  disease a history of that disease or diseases known to be a precuisite prior study recruitment (i.e. baseline)
HRS2018_race_hispanic_latino

HRS2018_race_white
HRS2018_race_black
LGB_2016
Straight_2016
sex_1_0_2018
yearsof_education2018

#Time-dependent covariates will include current age 
age_groups2018
continious_age2018
  