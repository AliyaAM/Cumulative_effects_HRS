
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




HRS2008_data_female = subset(HRS2008_data, HRS2008_data$sex_1_2 == 2) 
HRS2010_data_female = subset(HRS2010_data, HRS2010_data$sex_1_2 == 2)
HRS2012_data_female = subset(HRS2012_data, HRS2012_data$sex_1_2 == 2)
HRS2014_data_female = subset(HRS2014_data, HRS2014_data$sex_1_2 == 2)
HRS2016_data_female = subset(HRS2016_data, HRS2016_data$sex_1_2 == 2)
HRS2018_data_female = subset(HRS2018_data, HRS2018_data$sex_1_2 == 2)


HRS2008_data_female = data_frame(HRS2008_data_female$HHIDPN,
                                   HRS2008_data_female$diabetes_new,
                                   HRS2008_data_female$diabetes_ever,
                                   HRS2008_data_female$sex_1_2,
                                   HRS2008_data_female$wealth_noIRA,
                                   HRS2008_data_female$start,
                                   HRS2008_data_female$stop,
                                   HRS2008_data_female$reason_discrim1_reason_disability,
                                   
                                   HRS2008_data_female$discrim_harassed,
                                   HRS2008_data_female$discrim_lessrespect,
                                   HRS2008_data_female$discrim_medical,
                                   HRS2008_data_female$discrim_notclever,
                                   HRS2008_data_female$discrim_poorerservice,
                                   HRS2008_data_female$discrim_afraidothers,
                                   
                                   HRS2008_data_female$continious_age,
                                   HRS2008_data_female$assessed_BMI)

nrow_2008 = nrow(HRS2008_data_female) 
HRS2008_data_female$year_2008 = rep(2010, times = nrow_2008)

HRS2008_data_female <- na.omit(HRS2008_data_female)


HRS2010_data_female = data_frame(HRS2010_data_female$HHIDPN,
                                   HRS2010_data_female$diabetes_new,
                                   HRS2010_data_female$diabetes_ever,
                                   HRS2010_data_female$sex_1_2,
                                   HRS2010_data_female$wealth_noIRA,
                                   HRS2010_data_female$start,
                                   HRS2010_data_female$stop,
                                   HRS2010_data_female$reason_discrim1_reason_disability,
                                   
                                   HRS2010_data_female$discrim_harassed,
                                   HRS2010_data_female$discrim_lessrespect,
                                   HRS2010_data_female$discrim_medical,
                                   HRS2010_data_female$discrim_notclever,
                                   HRS2010_data_female$discrim_poorerservice,
                                   
                                   HRS2010_data_female$discrim_afraidothers,
                                   HRS2010_data_female$continious_age,
                                   HRS2010_data_female$assessed_BMI)

nrow_2010 = nrow(HRS2010_data_female) 
HRS2010_data_female$year_2010 = rep(2010, times = nrow_2010)

HRS2010_data_female <- na.omit(HRS2010_data_female)

HRS2012_data_female = data_frame(HRS2012_data_female$HHIDPN,
                                   HRS2012_data_female$diabetes_new,
                                   HRS2012_data_female$diabetes_ever,
                                   HRS2012_data_female$sex_1_2,
                                   HRS2012_data_female$wealth_noIRA,
                                   HRS2012_data_female$start,
                                   HRS2012_data_female$stop,
                                   HRS2012_data_female$reason_discrim1_reason_disability,
                                   
                                   HRS2012_data_female$discrim_harassed,
                                   HRS2012_data_female$discrim_lessrespect,
                                   HRS2012_data_female$discrim_medical,
                                   HRS2012_data_female$discrim_notclever,
                                   HRS2012_data_female$discrim_poorerservice,
                                   HRS2012_data_female$discrim_afraidothers,
                                   
                                   HRS2012_data_female$continious_age,
                                   HRS2012_data_female$assessed_BMI)


nrow_2012 = nrow(HRS2012_data_female) 
HRS2012_data_female$year_2012 = rep(2012, times = nrow_2012)

HRS2012_data_female <- na.omit(HRS2012_data_female)


HRS2014_data_female = data_frame(HRS2014_data_female$HHIDPN,
                                   HRS2014_data_female$diabetes_new,
                                   HRS2014_data_female$diabetes_ever,
                                   HRS2014_data_female$sex_1_2,
                                   HRS2014_data_female$wealth_noIRA,
                                   HRS2014_data_female$start,
                                   HRS2014_data_female$stop,
                                   HRS2014_data_female$reason_discrim1_reason_disability,
                                   
                                   HRS2014_data_female$discrim_harassed,
                                   HRS2014_data_female$discrim_lessrespect,
                                   HRS2014_data_female$discrim_medical,
                                   HRS2014_data_female$discrim_notclever,
                                   HRS2014_data_female$discrim_poorerservice,
                                   HRS2014_data_female$discrim_afraidothers,
                                   
                                   HRS2014_data_female$continious_age,
                                   HRS2014_data_female$assessed_BMI)


nrow_2014 = nrow(HRS2014_data_female) 
HRS2014_data_female$year_2014 = rep(2014, times = nrow_2014)

HRS2014_data_female <- na.omit(HRS2014_data_female)

HRS2016_data_female$HHIDPN_HRS2016 = HRS2016_data_female$HHIDPN


HRS2016_data_female = data_frame(HRS2016_data_female$HHIDPN,
                                   HRS2016_data_female$diabetes_new,
                                   HRS2016_data_female$diabetes_ever,
                                   HRS2016_data_female$sex_1_2,
                                   HRS2016_data_female$wealth_noIRA,
                                   
                                   HRS2016_data_female$start,
                                   HRS2016_data_female$stop,
                                   
                                   HRS2016_data_female$reason_discrim1_reason_disability,
                                   
                                   HRS2016_data_female$discrim_harassed,
                                   HRS2016_data_female$discrim_lessrespect,
                                   HRS2016_data_female$discrim_medical,
                                   HRS2016_data_female$discrim_notclever,
                                   HRS2016_data_female$discrim_poorerservice,
                                   HRS2016_data_female$discrim_afraidothers,
                                   
                                   HRS2016_data_female$continious_age,
                                   HRS2016_data_female$assessed_BMI)

nrow_2016 = nrow(HRS2016_data_female) 
HRS2016_data_female$year_2016 = rep(2016, times = nrow_2016)

HRS2016_data_female <- na.omit(HRS2016_data_female)



HRS2018_data_female$HHIDPN = HRS2018_data_female$HHIDPN_HRS2018


HRS2018_data_female = data_frame(HRS2018_data_female$HHIDPN,
                                   HRS2018_data_female$diabetes_new,
                                   HRS2018_data_female$diabetes_ever,
                                   HRS2018_data_female$sex_1_2,
                                   HRS2018_data_female$wealth_noIRA,
                                   
                                   HRS2018_data_female$start,
                                   HRS2018_data_female$stop,
                                   
                                   HRS2018_data_female$reason_discrim1_reason_disability,
                                   
                                   HRS2018_data_female$discrim_harassed,
                                   HRS2018_data_female$discrim_lessrespect,
                                   HRS2018_data_female$discrim_medical,
                                   HRS2018_data_female$discrim_notclever,
                                   HRS2018_data_female$discrim_poorerservice,
                                   HRS2018_data_female$discrim_afraidothers,
                                   
                                   HRS2018_data_female$continious_age, 
                                   HRS2018_data_female$assessed_BMI)

nrow_2018 = nrow(HRS2018_data_female) 
HRS2018_data_female$year_2018= rep(2018, times = nrow_2018)

HRS2018_data_female <- na.omit(HRS2018_data_female)



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

colnames(HRS2008_data_female) = colnames_all
colnames(HRS2010_data_female) = colnames_all
colnames(HRS2012_data_female) = colnames_all
colnames(HRS2014_data_female) = colnames_all
colnames(HRS2016_data_female) = colnames_all
colnames(HRS2018_data_female) = colnames_all



WCE_dataset_female = rbind(HRS2008_data_female, HRS2010_data_female, HRS2012_data_female, HRS2014_data_female, HRS2016_data_female, HRS2018_data_female)


WCE_dataset_female$diabetes_new_bin = case_when(WCE_dataset_female$diabetes_new ==0 ~ 0, 
                                                  WCE_dataset_female$diabetes_new ==1 ~ 1) 



WCE_dataset_female = subset(WCE_dataset_female, HHIDPN != "3020")

WCE_dataset_female = subset(WCE_dataset_female, discrim_afraidothers != " NA")

unique(WCE_dataset_female$discrim_afraidothers)

write.csv(WCE_dataset_female, paste(SOURCE_ROOT, "WCE_dataset_female.csv", sep=""))

#which(WCE_dataset_female, WCE_dataset_female$HHIDPN== 3020)

nrow(WCE_dataset_female)
head(WCE_dataset_female)

ID = unique(WCE_dataset_female$HHIDPN)

#print(isTRUE(WCE_dataset_female$HHIDPN == ID[1]))
WCE_dataset_female = WCE_dataset_female %>% drop_na()

participant_wave_df = data.frame()

n = 1
for (id in ID){
  print(n)
  print(id)
  participant_wave = subset(WCE_dataset_female, WCE_dataset_female$HHIDPN == id)
  
  if (nrow(participant_wave)== 1){
    
    participant_wave$timepoints_indiv = 1
    
    
    participant_wave$start_new = c(0)
    participant_wave$stop_new = c(1)
    
    participant_wave_df = rbind(participant_wave_df, participant_wave) 
    
  }
  
  if (nrow(participant_wave) ==2){
    
    participant_wave$timepoints_indiv = 2
    
    participant_wave$start_new = c(0, 1)
    participant_wave$stop_new = c(1, 2)
    participant_wave_df = rbind(participant_wave_df, participant_wave) 
    
  }
  
  if (nrow(participant_wave)==3){
    
    participant_wave$timepoints_indiv = 3
    
    participant_wave$start_new = c(0, 1, 2)
    participant_wave$stop_new = c(1, 2, 3)
    participant_wave_df = rbind(participant_wave_df, participant_wave) 
    
  } 
  
  
  if (nrow(participant_wave)==4){
    
    participant_wave$timepoints_indiv = 4
    
    participant_wave$start_new = c(0, 1, 2, 3)
    participant_wave$stop_new = c(1, 2, 3, 4)
    participant_wave_df = rbind(participant_wave_df, participant_wave) 
    
  } 
  
  if (nrow(participant_wave)==5){
    
    participant_wave$timepoints_indiv = 5
    
    participant_wave$start_new = c(0, 1, 2, 3, 4)
    participant_wave$stop_new = c(1, 2, 3, 4, 5)
    participant_wave_df = rbind(participant_wave_df, participant_wave) 
    
  } 
  
  if (nrow(participant_wave)==6){
    
    participant_wave$timepoints_indiv = 6
    participant_wave$start_new = c(0, 1, 2, 3, 4, 5)
    participant_wave$stop_new = c(1, 2, 3, 4, 5, 6)
    participant_wave_df = rbind(participant_wave_df, participant_wave) 
    
  }
  n = n + 1
}


participant_wave_df$discrim_lessrespect
participant_wave_df$discrim_medical
participant_wave_df$discrim_notclever
participant_wave_df$discrim_poorerservice
participant_wave_df$discrim_afraidothers


participant_wave_df$diabetes_new_bin = as.numeric(participant_wave_df$diabetes_new_bin)
participant_wave_df$start_new = as.numeric(participant_wave_df$start_new)
participant_wave_df$stop_new = as.numeric(participant_wave_df$stop_new)
participant_wave_df$discrim_afraidothers = as.numeric(participant_wave_df$discrim_afraidothers)
participant_wave_df$assessed_BMI = as.numeric(participant_wave_df$assessed_BMI)
participant_wave_df$continious_age = as.numeric(participant_wave_df$continious_age)

table(by(participant_wave_df$start_new,  participant_wave_df$HHIDPN, min)) 

table(participant_wave_df$start_new)
 

##participant_wave_df = participant_wave_df %>% drop_na()
#participant_wave_df <- na.omit(participant_wave_df)

checkWCE(participant_wave_df,
         id = "HHIDPN", 
         event = "diabetes_new_bin", 
         start = "start_new",
         stop = "stop_new",
         expos = "discrim_afraidothers") 

# check that the minumum start of time point 0 and min for stop is 1
table(by(participant_wave_df$start_new,  participant_wave_df$HHIDPN, min)) 
table(by(participant_wave_df$start,  participant_wave_df$HHIDPN, min)) 
table(by(participant_wave_df$stop,  participant_wave_df$HHIDPN, min)) 
table(by(participant_wave_df$stop_new,  participant_wave_df$HHIDPN, min)) 

#check how may people were in each wave 
table(participant_wave_df$start_new)

#check how many people took part in multiple waves 
n_timepoints_list = unique(participant_wave_df$timepoints_indiv)
participant_wave_df$n_timepoints_max = max(n_timepoints_list)

#take the maximum number of time points value to be specified for the cut off point in WCE analysis below
n_timepoints_max = max(participant_wave_df$n_timepoints_max)

head(participant_wave_df)
nrow(participant_wave_df)
length(unique(participant_wave_df$HHIDPN))

#check the unique values in the discrim variable, this are original form the HRS files. 
####### coded as: 
####### 1 Almost everyday
####### 2 At least once a week
####### 3 A few times a month
####### 4 A few times a year
####### 5 Less than once a year
####### 6 Never

unique(participant_wave_df$discrim_afraidothers)

wce_age =  WCE(data = participant_wave_df,
               analysis = "Cox", 
               nknots = 1:3, cutoff = n_timepoints_max, 
               constrained = "R", aic = FALSE, MatchedSet = NULL, 
               id = "HHIDPN", 
               event = "diabetes_new_bin", 
               start = "start_new", 
               stop = "stop_new", 
               expos = "discrim_afraidothers",
               covariates = c("continious_age"))
wce_age
summary(wce_age)

wce_age_BMI <- WCE(data = participant_wave_df, 
                   analysis = "Cox",
                   nknots = 1:3, 
                   cutoff = n_timepoints_max, 
                   constrained = "R", 
                   aic = FALSE, 
                   MatchedSet = NULL,
                   id = "HHIDPN", 
                   event = "diabetes_new_bin", 
                   start = "start_new", 
                   stop = "stop_new", 
                   expos = "discrim_afraidothers",
                   covariates = c("assessed_BMI", "continious_age")) 
wce_age_BMI
summary(wce_age_BMI)

wce_BMI_age_wealth <- WCE(participant_wave_df, 
                          analysis = "Cox",
                          nknots = 1:3, 
                          cutoff = n_timepoints_max, 
                          constrained = "R",
                          aic = FALSE, 
                          MatchedSet = NULL, 
                          id = "HHIDPN",
                          event = "diabetes_new_bin", 
                          start = "start_new",
                          stop = "stop_new", 
                          expos = "discrim_afraidothers",
                          covariates = c("assessed_BMI", "continious_age", "wealth_noIRA")) 
wce_BMI_age_wealth
summary(wce_BMI_age_wealth)

# Example from the R library HR.WCE: 
######## Exposed at a dose of 1 (constant) vs. unexposed over the time window of 90 days
######## scenario1 <- rep(1, 90)
######## scenario2 <- rep(0, 90)
######## HR.WCE(wce, vecnum = scenario1, vecdenom = scenario2)

#producing hazard ratios of experiencing discrimination Almost everyday (=1) to never (=6) on the onset of diabetes type 2. 
unique(participant_wave_df$discrim_afraidothers)

scenario1 <- rep(1, n_timepoints_max)
scenario2 <- rep(6, n_timepoints_max) # for all models 
HR.WCE(wce_age, vecnum = scenario1, vecdenom = scenario2, allres = TRUE)


#producing hazard ratios of experiencing discrimination At least once a week (=2) to never (=6) on the onset of diabetes type 2. 
scenario3 <- rep(2, n_timepoints_max)
scenario2 <- rep(6, n_timepoints_max) # for all models 
HR.WCE(wce_age, vecnum = scenario3, vecdenom = scenario2, allres = TRUE)

#producing hazard ratios of experiencing discrimination A few times a month (=3) to never (=6) on the onset of diabetes type 2. 
scenario4 <- rep(3, n_timepoints_max)
scenario2 <- rep(6, n_timepoints_max) # for all models 
HR.WCE(wce_age, vecnum = scenario4, vecdenom = scenario2, allres = TRUE)


#producing hazard ratios of experiencing discrimination A few times a year (=4) to never (=6) on the onset of diabetes type 2. 
scenario5 <- rep(4, n_timepoints_max)
scenario2 <- rep(6, n_timepoints_max) # for all models 
HR.WCE(wce_age, vecnum = scenario5, vecdenom = scenario2, allres = TRUE)


#producing hazard ratios of experiencing discrimination less than once a year  (=5) to never (=6) on the onset of diabetes type 2. 
scenario6 <- rep(5, n_timepoints_max)
scenario2 <- rep(6, n_timepoints_max) # for all models 
HR.WCE(wce_age, vecnum = scenario6, vecdenom = scenario2, allres = TRUE)



ID <- unique(participant_wave_df$HHIDPN)

write.csv(participant_wave_df, paste(SOURCE_ROOT, "participant_wave_df_female.csv", sep=""))


coef.WCE(wce_age)
