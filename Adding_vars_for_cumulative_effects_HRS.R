

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



## Set the root directory to look for source code.

#/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/
#"/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"
SOURCE_ROOT = "/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"
OUTPUT_ROOT = "/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"

#SOURCE_ROOT = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/"
## Set the root location on the user's local machine to save output files.
#OUTPUT_ROOT = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/"


#ead HRS all waves harmonised data:
#Harmonised_data_all_waves_HRS/
harmonised_data_all_waves = read.csv(paste(SOURCE_ROOT, "H_HRS_c.csv", sep=""))


#other sources of stress, shall we include them as covariates: job strain etc, a lit of HRS stress items are here: https://g2aging.org/?section=concordance-search&sWords=stress&interval=1992%2C2016&page=1&per_page=50&af_src=1

# other literature considers cumulatie effects as: cumulative stressors (global, weekly and major life events)
#https://bmcpublichealth.biomedcentral.com/articles/10.1186/s12889-020-08573-0 

#think how we are going to restrict to a particular type of discrimination, 
#which wave shall we use? 
#shall we state that every wave they listed disability (ie subset HRS2004_discrim_disability == 1 & HRS2008_discrim_disability ==1 etc every wave up to 2018) 

#add 2004, 2006, 2008 

#from RAND below we beed to find: #wealth # in harmonised rnad file 2008 year wealth var is: #H9ATOTW 

#RAND_data = read.csv("/Users/aliya/my_docs/KCL_postDoc/Data_analysis/randhrs1992_2018v1.csv")

#data files with original variables 

HRS2008_data_RAND = read.csv(paste(SOURCE_ROOT, "HRS_2008_data/HRS2008_ALLData_originalVARNames.csv", sep=""))
HRS2010_data_RAND  = read.csv(paste(SOURCE_ROOT, "HRS_2010_data/HRS2010_rand_harmonisedFile.csv", sep=""))
HRS2012_data_RAND  = read.csv(paste(SOURCE_ROOT, "HRS_2012_data/HRS2012_rand_harmonisedFile.csv", sep=""))
HRS2014_data_RAND  = read.csv(paste(SOURCE_ROOT, "HRS_2014_data/HRS2014_rand_harmonisedFile.csv", sep=""))
HRS2016_data_RAND  = read.csv(paste(SOURCE_ROOT, "HRS_2016_data/HRS2016_rand_harmonisedFile.csv", sep=""))
HRS2018_data_RAND  = read.csv(paste(SOURCE_ROOT, "HRS_2018_data/HRS2018_rand_harmonisedFile.csv", sep=""))


#coxph
#/Users/aliya/my_docs/KCL_postDoc/Data_analysis/HRS_2008_data/HRS2008_discrimination_dataset.csv
HRS2008_data = read.csv(paste(SOURCE_ROOT, "HRS_2008_data/HRS2008_discrimination_dataset_new.csv",  sep=""))
HRS2010_data = read.csv(paste(SOURCE_ROOT, "HRS_2010_data/HRS2010_discrimination_dataset_new.csv",  sep=""))
HRS2012_data = read.csv(paste(SOURCE_ROOT, "HRS_2012_data/HRS2012_discrimination_dataset_new.csv",  sep=""))
HRS2014_data = read.csv(paste(SOURCE_ROOT, "HRS_2014_data/HRS2014_discrimination_dataset_new.csv",  sep=""))
HRS2016_data = read.csv(paste(SOURCE_ROOT, "HRS_2016_data/HRS2016_discrimination_dataset_new.csv",  sep=""))
HRS2018_data = read.csv(paste(SOURCE_ROOT, "HRS_2018_data/HRS2018_discrimination_dataset_new.csv",  sep=""))


########### match by id each wave
HRS2010_data$HHIDPN = HRS2010_data$HHIDPN
HRS2012_data$HHIDPN = HRS2012_data$HHIDPN_HRS2012
HRS2014_data$HHIDPN = HRS2014_data$HHIDPN_HRS2014
HRS2016_data$HHIDPN = HRS2016_data$HHIDPN_HRS2016
HRS2018_data$HHIDPN = HRS2018_data$HHIDPN_HRS2018

######
######
######################
######################

### Add data

#HRS2010_data$HRS2010_heartattack2yrs_bin


#ever heart attack HRS2012: NC257, 1 = Yes, 5 = No, unique(HRS2012_data_RAND$NC257)
HRS2012_data$HRS2012_heartattack_ever = HRS2012_data_RAND$NC257
HRS2012_data$heartattack_ever = HRS2012_data$HRS2012_heartattack_ever
#hrs 2014: have you had heart attack ever: OC257, unique(HRS2014_data_RAND$OC257)
HRS2014_data$HRS2014_heartattack_ever = HRS2014_data_RAND$OC257
HRS2014_data$heartattack_ever = HRS2014_data$HRS2014_heartattack_ever
# hrs 2016: ever had heart attack: PC257, unique(HRS2016_data_RAND$PC257)
HRS2016_data$HRS2016_heartattack_ever = HRS2016_data_RAND$PC257

HRS2016_data$heartattack_ever = HRS2016_data$HRS2016_heartattack_ever

#hrs 2018: have you had heart attack ever: C257 (missing, null vector)
HRS2018_data$HRS2018_heartattack_ever = HRS2018_data_RAND$C040
HRS2018_data$heartattack_ever = HRS2018_data$HRS2018_heartattack_ever


######
#hrs 2014: have you had other heart attack: OC040 unique(HRS2014_data_RAND$OC040)
#hrs 2016 had  heart attacks:  PC040 unique(HRS2016_data_RAND$PC040)
#hrs 2018: have you had heart attack: C040 unique(HRS2018_data_RAND$C040)


HRS2014_data$HRS2014_heartattack2yrs_bin = HRS2014_data_RAND$HRS2014_data_RAND$OC040
HRS2014_data$heartattack2yrs_bin = HRS2014_data$HRS2014_heartattack2yrs_bin 

HRS2016_data$HRS2016_heartattack2yrs_bin = HRS2016_data_RAND$HRS2016_data_RAND$PC040
HRS2016_data$heartattack2yrs_bin = HRS2016_data$HRS2016_heartattack2yrs_bin

HRS2018_data$HRS2018_heartattack2yrs_bin = HRS2018_data_RAND$HRS2018_data_RAND$C040
HRS2018_data$heartattack2yrs_bin = HRS2018_data$HRS2018_heartattack2yrs_bin

#####
#had other heart attacks HRS2012: NC274, unique(HRS2012_data_RAND$NC274)
# hrs 2014 had other heart attacks: OC274, unique(HRS2014_data_RAND$OC274)
# hrs 2016 had other heart attacks:  PC274, unique(HRS2016_data_RAND$PC274)
######
# number of heart attacks in the past: NC275, HRS2012_data_RAND$NC275, unique(HRS2012_data_RAND$NC275)
#####
#unique(HRS2018_data_RAND$C257)
# hrs 2018 had other heart attacks: C274 (missing, null vector)
#unique(HRS2018_data_RAND$C274)





##### ##### angina, 
HRS2008_data$angina2yrs_bin = HRS2008_data$HRS2008_angina2yrs_bin

HRS2010_data$angina2yrs_bin = HRS2010_data$HRS2010_angina2yrs_bin
#HRS2014_data$HRS2014_angina2yrs_bin: R7ANGIN, C045_
HRS2014_data$HRS2014_angina2yrs_bin = HRS2014_data_RAND$R7ANGIN

HRS2014_data$angina2yrs_bin = HRS2014_data$HRS2014_angina2yrs_bin

#HRS2016_data$HRS2016_angina2yrs_bin: R13ANGIN, 
HRS2016_data$HRS2016_angina2yrs_bin = HRS2016_data_RAND$R13ANGIN
HRS2016_data$angina2yrs_bin = HRS2016_data$HRS2016_angina2yrs_bin

#HRS2018_data$HRS2018_angina2yrs_bin: R9ANGIN
HRS2018_data$HRS2018_angina2yrs_bin = HRS2018_data_RAND$R9ANGIN
HRS2018_data$angina2yrs_bin = HRS2018_data$HRS2018_angina2yrs_bin 


##### Angina new: 
#2014: since last wave: R12ANGIN 
HRS2014_data$HRS2014_angina_new = HRS2014_data_RAND$R12ANGIN
HRS2014_data$angina_new = HRS2014_data$HRS2014_angina_new

#HRS 2018:angina since last wave: R14ANGIN
HRS2018_data$HRS2018_angina_new = HRS2018_data_RAND$R14ANGIN
HRS2018_data$angina_new = HRS2018_data$HRS2018_angina_new


##### Angina ever: 
# 2014: ever had angina: R12ANGINE, C260_
HRS2014_data$HRS2014_angina_ever = HRS2014_data_RAND$R12ANGINE
HRS2014_data$angina_ever = HRS2014_data$HRS2014_angina_ever

#HRS2016: ever had angina: R13ANGINE varified: ZC260
HRS2016_data$HRS2016_angina_ever = HRS2016_data_RAND$R13ANGINE
HRS2016_data$angina_ever  = HRS2016_data$HRS2016_angina_ever

#HRS 2018: ever had angina: R14ANGINE
HRS2018_data$HRS2018_angina_ever = HRS2018_data_RAND$R14ANGINE
HRS2018_data$angina_ever = HRS2018_data$HRS2018_angina_ever


#are you limitting usual activities because of angina: PH204

##### ##### hypertension, 
HRS2008_data$hypertension_new = HRS2008_data$HRS2008_hypertension_new 
HRS2010_data$hypertension_new = HRS2010_data$HRS2010_hypertension_new

#HRS2012: confirmed medical diagnosis: high blood preasure NC005
HRS2012_data$HRS2012_hypertension_new = HRS2012_data_RAND$NC005
HRS2012_data$hypertension_new = HRS2012_data$HRS2012_hypertension_new 

#HRS 2014: high blood preassure: OC005
HRS2014_data$HRS2014_hypertension_new = HRS2014_data_RAND$OC005
HRS2014_data$hypertension_new = HRS2014_data$HRS2014_hypertension_new 

#HRS2016_data$HRS2016_hypertension_new: not measured 
#HRS binary high blood preassure: PC005

HRS2016_data$HRS2016_hypertension_new = HRS2016_data_RAND$PC005
HRS2016_data$hypertension_new = HRS2016_data$HRS2016_hypertension_new
  
#check below!!!!!!! 
#HRS2018_data$HRS2018_hypertension_new: not measured 
#HRS: 2018 hypertension ever: C005
HRS2018_data$HRS2018_hypertension_ever = HRS2018_data_RAND$C005
HRS2018_data$hypertension_ever = HRS2018_data$HRS2018_hypertension_ever 


##### ##### diabetes new 
HRS2008_data$diabetes_new = HRS2008_data$HRS2008_diabetes_new
HRS2010_data$diabetes_new = HRS2010_data$HRS2010_diabetes_new

#HRS2012_data$HRS2012_diabetes_new: R11DIABS, R11DIAB, R11DIABQ
HRS2012_data$HRS2012_diabetes_new = HRS2012_data_RAND$R11DIABS
HRS2012_data$diabetes_new = HRS2012_data$HRS2012_diabetes_new 

#HRS2014_data$HRS2014_diabetes_new: R12DIABS, RAXDIAB
HRS2014_data$HRS2014_diabetes_new = HRS2014_data_RAND$R12DIABS
HRS2014_data$diabetes_new = HRS2014_data$HRS2014_diabetes_new 

#HRS2016_data$HRS2016_diabetes_new R13DIABS
HRS2016_data$HRS2016_diabetes_new = HRS2016_data_RAND$R13DIABS
HRS2016_data$diabetes_new = HRS2016_data$HRS2016_diabetes_new

#HRS 2018 diabetes sine last wave, new diabetes: R14DIABS
HRS2018_data$HRS2018_diabetes_new = HRS2016_data_RAND$R14DIABS
HRS2018_data$diabetes_new = HRS2018_data$HRS2018_diabetes_new 

##### ##### ever had diabetes  
# HRS2012: ever had diabetes: R11DIABE, R3DIAB, RAXDIABE
HRS2012_data$HRS2012_diabetes_ever  = HRS2012_data_RAND$R11DIABE
HRS2012_data$diabetes_ever = HRS2012_data$HRS2012_diabetes_ever 

#HRS 2014: ever had diabetes R12DIABE, ever had diabetes (doctor diagnosed) RALDIABE
HRS2014_data$HRS2014_diabetes_ever  = HRS2014_data_RAND$RALDIABE
HRS2014_data$diabetes_ever = HRS2014_data$HRS2014_diabetes_ever

#HRS 2016: ever had diabetes: R13DIABE (doctor diagnosed), R7DIABE (doctor diagnosed)
HRS2016_data$HRS2016_diabetes_ever  = HRS2016_data_RAND$R7DIABE
HRS2016_data$diabetes_ever  = HRS2016_data$HRS2016_diabetes_ever  

#HRS 2018: ever had diabetes; R14DIABE
HRS2018_data$HRS2018_diabetes_ever  = HRS2018_data_RAND$R14DIABE
HRS2018_data$diabetes_ever = HRS2018_data$HRS2018_diabetes_ever  

#######

#HRS 2014 reports diabetes this wave: R12DIAB
HRS2014_data$HRS2014_diabetes_thisWave  = HRS2014_data_RAND$R12DIAB
HRS2014_data$diabetes_thisWave  = HRS2014_data$HRS2014_diabetes_thisWave 

#HRS 2016: reports diabetes this wave: R13DIAB (doctor diagnosed) 
HRS2016_data$HRS2016_diabetes_thisWave  = HRS2016_data_RAND$R13DIAB
HRS2016_data$diabetes_thisWave = HRS2016_data$HRS2016_diabetes_thisWave 

#HRS 2018: reports diabetes this wave: R14DIAB
HRS2018_data$HRS2018_diabetes_thisWave  = HRS2018_data_RAND$R14DIAB
HRS2018_data$diabetes_thisWave = HRS2018_data$HRS2018_diabetes_thisWave 


# HRS 2012: childhood diabtes: RACHDIAB
HRS2012_data$HRS2012_diabetes_childhoodDiabetes  = HRS2012_data_RAND$RACHDIAB
HRS2012_data$diabetes_childhoodDiabetes = HRS2012_data$HRS2012_diabetes_childhoodDiabetes

#HRS 2014: childhood diabetes: S7CHDIAB
HRS2014_data$HRS2014_diabetes_childhoodDiabetes  = HRS2014_data_RAND$S7CHDIAB
HRS2014_data$diabetes_childhoodDiabetes = HRS2014_data$HRS2014_diabetes_childhoodDiabetes

#HRS 2016: childhood diabetes: RACHDIAB
HRS2016_data$HRS2016_diabetes_childhoodDiabetes  = HRS2016_data_RAND$RACHDIAB
HRS2016_data$diabetes_childhoodDiabetes = HRS2016_data$HRS2016_diabetes_childhoodDiabetes

#hrs: 2012: ever diabetes limits your activities R4DIABLMT
HRS2012_data$HRS2012_diabetes_limitsActivities  = HRS2012_data_RAND$R4DIABLMT
HRS2012_data$diabetes_limitsActivities  = HRS2012_data$HRS2012_diabetes_limitsActivities

#HRS  2012: age first diagnosed with diabtes: RADIAGDIAB
HRS2012_data$HRS2012_diabetes_age_diagnosed = HRS2012_data_RAND$RADIAGDIAB
HRS2012_data$diabetes_age_diagnosed  = HRS2012_data$HRS2012_diabetes_age_diagnosed 

#HRS 2014: age first diagnosed with diabetes: RADIAGDIAB
#HRS2014_data$HRS2014_diabetes_age_diagnosed = HRS2014_data_RAND$RADIAGDIAB
#HRS 2016:  age first diagnosed with diabetes: RADIAGDIAB
#HRS2012_data$HRS2012_diabetes_age_diagnosed = HRS2012_data_RAND$RADIAGDIAB





#HRS 2014: diabetes limits activities: R5DIABLMT
HRS2014_data$HRS2014_diabetes_limitsActivities  = HRS2014_data_RAND$R5DIABLMT
HRS2014_data$diabetes_limitsActivities = HRS2014_data$HRS2014_diabetes_limitsActivities

#HRS diabetes limits activities: R7DIABLMT
HRS2016_data$HRS2016_diabetes_limitsActivities  = HRS2016_data_RAND$R7DIABLMT
HRS2016_data$diabetes_limitsActivities = HRS2016_data$HRS2016_diabetes_limitsActivities

#HRS before you were 16 years old did you have diabetes: B106

##### ##### stroke, 
HRS2008_data$stroke_new = HRS2008_data$HRS2008_stroke_new
HRS2010_data$stroke_new = HRS2010_data$HRS2010_stroke_new

#dd below
#HRS2012_data$HRS2012_stroke_new
#HRS 2012: another stroke since previous wave: NC062
HRS2012_data$HRS2012_stroke_new = HRS2012_data_RAND$NC062
HRS2012_data$stroke_new  = HRS2012_data$HRS2012_stroke_new  

# #HRS2014_ another storke since previous wave: OC062
HRS2014_data$HRS2014_stroke_new = HRS2014_data_RAND$OC062
HRS2014_data$stroke_new = HRS2014_data$HRS2014_stroke_new 
  
# #HRS2016_ another storke since previous wave: PC062
HRS2016_data$HRS2016_stroke_new = HRS2016_data_RAND$PC062
HRS2016_data$stroke_new = HRS2016_data$HRS2016_stroke_new

#has a doctor ever told you you have stroke C053
HRS2018_data$HRS2018_stroke_new = HRS2018_data_RAND$C053
HRS2018_data$stroke_new = HRS2018_data$HRS2018_stroke_new 

  
#NC053	HRS 2012	C. Physical Health	
#Description: STROKE Text:  <font color=purple>^FLC053 ^FLC053B ^FLIWER</font> <font color=blue>Def:</font> (Medical doctors include specialists such as Dermatologists, Psychiatrists, Ophthalmologists, Osteopaths, Cardiologists, as well as family doctors, internists and physicians' assistants. Also include diagnoses made by Nurses and Nurse Practitioners.)
#                                                                                   Response type:
#                                                                                   Enumerated
#                                                                                   Responses:
#                                                                                   1 Yes
#                                                                                   2 [VOL] Possible stroke or tia (transient ischemic attack)
#                                                                                   3 DISPUTES PREVIOUS WAVE RECORD, BUT NOW HAS CONDITION
#                                                                                   4 DISPUTES PREVIOUS WAVE RECORD, DOES NOT HAVE CONDITION
#                                                                                   5 NO






##### ##### heart failure,

HRS2008_data$heartfailure2yrs_bin = HRS2008_data$HRS2008_heartfailure2yrs_bin 
HRS2010_data$heartfailure2yrs_bin = HRS2010_data$HRS2010_heartfailure2yrs_bin

#HRS2012_data$HRS2012_heartfailure2yrs_bin: XC048
HRS2012_data$HRS2012_heartfailure2yrs_bin = HRS2012_data_RAND$XC048
HRS2012_data$heartfailure2yrs_bin = HRS2012_data$HRS2012_heartfailure2yrs_bin
  
#HRS2014_data$HRS2014_heartfailure2yrs_bin: OC048
HRS2014_data$HRS2014_heartfailure2yrs_bin = HRS2014_data_RAND$OC048
HRS2014_data$heartfailure2yrs_bin = HRS2014_data$HRS2014_heartfailure2yrs_bin 
  
#HRS2016_data$HRS2016_heartfailure2yrs_bin: 
HRS2016_data$HRS2016_heartfailure2yrs_bin = HRS2016_data_RAND$PC048
HRS2016_data$heartfailure2yrs_bin = HRS2016_data$HRS2016_heartfailure2yrs_bin 

#HRS2018_data$HRS2018_heartfailure2yrs_bin: C048
HRS2018_data$HRS2018_heartfailure2yrs_bin = HRS2018_data_RAND$C048
HRS2018_data$heartfailure2yrs_bin = HRS2018_data$HRS2018_heartfailure2yrs_bin 

#HRS 2012: HF bin: NC048


#HRS2012 ever had heart failure: R11CONHRTFE, R6CONHRTFE
#HRS 2014: ever had HF, R12CONHRTFE
#HRS 2016: ever had HF: PC263


#2012 hospitalise ddue to HF: NC049
#HRS 2016: hospitalised due to HF: PC049


#HRS 2014: new HF: R12CONHRTF
#HRS 2012: new heart failure: R11CONHRTF


#HRS 2012: age first diagnosed with HF: RADIAGCHF


###### other heart conditions 
HRS2008_data$heartcondition_new = HRS2008_data$HRS2008_heartcondition_new

HRS2010_data$heartcondition_new = HRS2010_data$HRS2010_heartcondition_new

#add below 
#HRS2012_data$HRS2012_heartcondition_new
#HRS heart condition ever: XC036
HRS2012_data_RAND
#HRS2014_data$HRS2014_heartcondition_new

#HRS2016_data$HRS2016_heartcondition_new
#HRS2016 heart condition ever:C033
#HRS2018_data$HRS2018_heartcondition_new
#HRS 2018 heart condiiton ever: C036

HRS2008_data$heartcondition_bin = HRS2008_data$HRS2008_heartcondition_bin
HRS2010_data$heartcondition_new = HRS2010_data$HRS2010_heartcondition_new

#add below 
#HRS2012_data$HRS2012_heartcondition_new
HRS2012_data_RAND
#HRS2014_data$HRS2014_heartcondition_new
#2014: heart condition: #: YC036,  Did a doctor ever tell [First Name] that [he/she] had a heart attack, coronary heart disease, angina, congestive heart failure, or other heart problems? PREVIOUS WAVE: [YES] Our records (from [his/her] interview [[in [Prev Wave IW Month], [Prev Wave IW Year]/in [Prev Wave IW Year]) show that [he/she] had a heart problem. PREVIOUS WAVE: [NO] (Since [his/her] interview in [[Prev Wave IW Month], [Prev Wave IW Year]/in [Prev Wave IW Year],) did a doctor tell [First Name] that [he/she] had) A heart attack, (had) coronary heart disease, angina, congestive heart failure, or other heart problems?
#HRS2016_data$HRS2016_heartcondition_new
#HRS2018_data$HRS2018_heartcondition_new

HRS2008_data$HRS2008_heartcondition_ever
HRS2008_data$heartcondition_ever = HRS2008_data$HRS2008_heartcondition_ever

HRS2010_data$HRS2010_heartcondition_ever

HRS2010_data$heartcondition_ever = HRS2010_data$HRS2010_heartcondition_ever



#add below 
#HRS2012_data$HRS2012_heartcondition_ever
HRS2012_data_RAND
#HRS2014_data$HRS2014_heartcondition_ever
#HRS2016_data$HRS2016_heartcondition_ever
#HRS2018_data$HRS2018_heartcondition_ever

#HRS2012_data$HRS2012_
#HRS2014_data$HRS2014_
#HRS2016_data$HRS2016_
#HRS2018_data$HRS2018_

##### ##### #####  ##### #####  #### #### #### #### #### #### #### #### #### other conditions 

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
#HRS2008_srh

#HRS2012_data$HRS2012_
#HRS2014_data$HRS2014_
#HRS2016_data$HRS2016_
#HRS2018_data$HRS2018_

##### weight,
#HRS2018_weight_kg

#HRS2018obese_bin
#HRS2018_underweight_bin
#HRS2018_normalweight_bin
#HRS2018_BMI

##### blood pressure, 
## ## ## ## ADD

##### depressive symptoms (CES-D), 
## ## ## ## ADD

##### life satisfaction (SWLS), 
## ## ## ## ADD

########################################################################
########################################################################

##### ##### ##### physical activity 

##### ##### ##### health mobile app 

##### ##### ##### polygenetic scores 

##### ##### ##### life event stress 
##### ##### ##### job strain 
##### ##### ##### saliva cortisol 
##### ##### ##### hair cortisol 

################
################
################ ADD COVARIATES BELOW 
ls(HRS2010_data)

#covariates: Fixed confounding factors at baseline will include SES, sex, #deprivation index 
########################################################################### and for the onset of a particular  disease a history of that disease or diseases known to be a precuisite prior study recruitment (i.e. baseline)


##### sex, in oroginal files it is coded as 1 = male and 2 = female 

#HRS2008_data_RAND$GENDER

#HRS2010_data_RAND$GENDER

HRS2012_data$HRS2012_sex_1_2 = HRS2012_data_RAND$GENDER

HRS2014_data$HRS2014_sex_1_2 = HRS2014_data_RAND$GENDER
HRS2016_data$HRS2016_sex_1_2 = HRS2016_data_RAND$GENDER
HRS2018_data$HRS2018_sex_1_2 = HRS2018_data_RAND$GENDER


#lim_physical_cond 

#2008: LM007 : 1. Yes, 5. No, 8. Don't know; not ascertained, 9. Refused, Blank. Inapplicable; partial interview

HRS2008_data$HRS2008_limiting_condition = HRS2008_data_RAND$LM007
HRS2008_data$HRS2008_limiting_condition_bin = case_when(HRS2008_data_RAND$LM007 == " 1" ~ 1, 
                                                HRS2008_data_RAND$LM007 == " 5" ~ 0)



#2008: harmonised data: R9LIMIMPAR
HRS2012_data$limiting_condition = HRS2012_data_RAND$NM007
unique(HRS2012_data$HRS2008_limiting_condition)

HRS2012_data$HRS2012_limiting_condition_bin = case_when(HRS2012_data_RAND$NM007 == 1 ~ 1, 
                                                HRS2012_data_RAND$NM007 == 5 ~ 0)

#2014:  harmonised data:R12LIMIMPAR
HRS2014_data$HRS2014_limiting_condition_bin = case_when(HRS2014_data_RAND$OM007 == 1 ~ 1, 
                                                HRS2014_data_RAND$OM007 == 5 ~ 0)

#2016:  harmonised data:R13LIMIMPAR

#2018:  harmonised data:R14LIMIMPAR
#bellow is an empty vector, despite the website suggesting that this variable is the correct one. 
HRS2018_data$HRS2018_limiting_condition_bin = case_when(HRS2018_data_RAND$M007 == 1 ~ 1, 
                                                HRS2018_data_RAND$M007 == 5 ~ 0)



#weight
#MC139      WEIGHT IN POUNDS
#####  weight (lb) / [height (in)]2 x 703
#HRS2010_weight_pounds 
#HRS2010_weight_kg = 0.453592 * HRS2010_data$HRS2010_weight_pounds



#2008: objective (scale): LI841
#self-reported: LC139, HRS2008_data_RAND$LC139

#HRS2008_data$HRS2008_weight_pounds = HRS2008_data_RAND$LC139
#HRS2008_data$HRS2008_weight_kg = 0.453592 * HRS2008_data$HRS2008_weight_pounds

#2012: weight in pound objective (scale): NV841, rand: R11PMWGHT, self-report: NC139
#2012: weight in kg R11MWEIGHT

#HRS2012_data$HRS2012_weight_pounds = HRS2012_data_RAND$NC139
#HRS2012_data$HRS2012_weight_kg = 0.453592 * HRS2012_data$HRS2012_weight_pounds

#2014: weight in pounds objective (scale): OI841, self-reported: OC139 
#HRS2014_data$HRS2014_weight_pounds = HRS2014_data_RAND$OC139
#HRS2014_data$HRS2014_weight_kg = 0.453592 * HRS2014_data$HRS2014_weight_pounds

#2016: weight in pounds objective (scale): PI841,  self-report: PC139
#HRS2016_data$HRS2016_weight_pounds = HRS2016_data_RAND$PC139
#HRS2016_data$HRS2016_weight_kg = 0.453592 * HRS2016_data$HRS2016_weight_pounds


#2018: weight in pounds objective (scale): I841,  self-report: C139
#HRS2018_data$HRS2018_weight_pounds = HRS2018_data_RAND$C139
#HRS2018_data$HRS2018_weight_kg = 0.453592 * HRS2018_data$HRS2018_weight_pounds



#height


#HRS2010_height_feet 
#MC142      HEIGHT INCHES
#HRS2010_height_inches = RAND_HRS2010$MC142
#HRS2010_height_meters = 0.3048 * HRS2010_data$HRS2010_height_feet

#2008: height feet: LC141, inches: LC142, objective physical measurement (inches): LI834
#HRS2008_data$HRS2008_height_inches = HRS2008_data_RAND$LC142


#2012: feet: NC141, inches: NC142, objective measurement (inches): NV834 
#HRS2012_height_inches = HRS2012_data_RAND$NC142
#change, check the formula: HRS2012_height_meters =  HRS2012_height_inches * 39.37

#HRS2012_height_meters = HRS2012_data_RAND$R11MHEIGHT

#2014: feet: OC141, inches: OC142,  objective physical measurement (inches) OI834
#HRS2014_height_inches = HRS2014_data_RAND$OI834
#2016:feet: , inches:  , objective physical measurement (inches) PI834
#HRS2016_height_inches = HRS2016_data_RAND$PI834
#2018: feet: PC141, inches:  PC142, objective physical measurement (inches) I834 
#HRS2018_height_inches = HRS2018_data_RAND$I834



#BMI: calculate 
#HRS2012_data$HRS2012_BMI = HRS2012_data$HRS2012_weight_kg/((HRS2012_data$HRS2012_height_meters)^2) 
#BMI: objective measurement: 
#2008: objective measurement:  R9PMBMI, self-reported: R9BMI
#HRS2008_data_RAND$R9PMBMI: null 
#HRS2008_data_RAND$R9BMI: null
#2012: objective measurement:  R11PMBMI, self-reported: R11BMI
#HRS2012_data_RAND$R11PMBMI
#HRS2012_data_RAND$R11BMI
#2014: objective measurement:  , self-reported: 
#2016: objective measurement:  , self-reported: 
#2018: objective measurement:  , self-reported: 

#### BMI from harmonised file: 

HRS2008_data$PN
#BMI: 2008, wave 9: 
harmonised_data_all_waves_2008 = subset(harmonised_data_all_waves, hhidpn %in% HRS2008_data$PN)
#BMI categories: 
#HRS2008_data$HRS2008_BMI_cat = harmonised_data_all_waves$r9bmicat
#HRS2008_data$HRS2008_BMI = harmonised_data_all_waves$r9mbmi


#BMI: 2010, wave 10: 
harmonised_data_all_waves_2010 = subset(harmonised_data_all_waves, hhidpn %in% HRS2010_data$HHIDPN)
HRS2010_data$HRS2010_BMI_cat = harmonised_data_all_waves_2010$r10bmicat
HRS2010_data$HRS2010_BMI = harmonised_data_all_waves_2010$r10mbmi


#BMI: 2012, wave 11: 
harmonised_data_all_waves_2012 = subset(harmonised_data_all_waves, hhidpn %in% HRS2012_data$HHIDPN)
HRS2012_data$HRS2012_BMI_cat = harmonised_data_all_waves_2012$r11bmicat 
HRS2012_data$HRS2012_BMI = harmonised_data_all_waves_2012$r11mbmi

#BMI: 2014, wave 12: 
harmonised_data_all_waves_2014 = subset(harmonised_data_all_waves, hhidpn %in% HRS2014_data$HHIDPN)
HRS2014_data$HRS2014_BMI_cat = harmonised_data_all_waves_2014$r12bmicat 
HRS2014_data$HRS2014_BMI = harmonised_data_all_waves_2014$r12mbmi


#BMI: 2016, wave 13:
harmonised_data_all_waves_2016 = subset(harmonised_data_all_waves, hhidpn %in% HRS2016_data$HHIDPN)
HRS2016_data$HRS2016_BMI_cat = harmonised_data_all_waves_2016$r13bmicat 
HRS2016_data$HRS2016_BMI = harmonised_data_all_waves_2016$r13mbmi


#BMI: 2018, wave 14: 
harmonised_data_all_waves_2018 = subset(harmonised_data_all_waves, hhidpn %in% HRS2018_data$HHIDPN)
HRS2018_data$HRS2018_BMI_cat = harmonised_data_all_waves_2018$r14bmicat 
HRS2018_data$HRS2018_BMI = harmonised_data_all_waves_2018$r14mbmi



#wealth add for: HRS2008_data, HRS 2012, HRS 2014, 2016, 2018 
#HRS2008_data$wealth_noIRA = HRS2008_data_RAND$H9ATOTW

HRS2010_data$wealth_noIRA = HRS2010_data$wealth_noIRA_HRS2010
#2012: H11ATOTW
nrow(harmonised_data_all_waves_2012)

HRS2012_data$wealth_noIRA_HRS2012 = harmonised_data_all_waves_2012$h11atotw
HRS2012_data$wealth_noIRA_HRS2012 = HRS2012_data$wealth_noIRA

#2014: H12ATOTW
HRS2014_data$wealth_noIRA_HRS2014 = harmonised_data_all_waves_2014$h12atotw
HRS2014_data$wealth_noIRA = HRS2014_data$wealth_noIRA_HRS2014 

#2016: H13ATOTW
HRS2016_data$wealth_noIRA_HRS2016 = HRS2016_data_RAND$h13atotw
HRS2016_data$wealth_noIRA = HRS2016_data$wealth_noIRA_HRS2016 
#2018: H14ATOTW
HRS2018_data$wealth_noIRA_HRS2018 = HRS2018_data_RAND$h14atotw
HRS2018_data$wealth_noIRA = HRS2018_data$wealth_noIRA_HRS2018 

#change in wealth: 
#2008: H9ATOTWC
#2010: H10ATOTWC
#2012: H11ATOTWC
#2014: H12ATOTWC
#2016: H13ATOTWC
#2018: H14ATOTWC


#religion
#RARELIG - 2008 
#1.protestant, 2.catholic, 3.jewish, 4.none/no pref, 5.other, .d=DK, .m=Oth missin, .r=RF

#HRS2012_data$religion = harmonised_data_all_waves_2012$rarelig - null 


#national origin 
#rabplacf - 2008 
#HRS2012_data$national_origin = harmonised_data_all_waves_2012$rabplacf 
#0.no discrepancy: 1.trk=us/rabplace=outside us, 2.trk=not us/rabplace=us, 3.trk=miss/rabplace=valid census division

#race: 2008, RARACEM: 1.white/caucasian, 2.black/african american, 3.other, .m=Oth missing
#race 2008: rahispan, 0.not hispanic, 1.hispanic, .m=Oth missing
#HRS2008_data$race = harmonised_data_all_waves_2012$raracem 


#HRS2018_race_hispanic_latino


#HRS2018_race_white

#HRS2018_race_black

#LGB_2016

#Straight_2016


#yearsof_education2018

#Time-dependent covariates will include current age 


#age_groups2018

#continious_age2018

#deprivation index 

#bind rows with bind_rows in dplyr 

HRS2012_data$continious_age =  HRS2012_data$continious_age2012
HRS2014_data$continious_age = HRS2014_data$continious_age2014
HRS2016_data$continious_age = HRS2016_data$continious_age2016
HRS2018_data$continious_age = HRS2018_data$continious_age2018

#######


# add continious age variable to 2008 dataset 
#HRS2008_data$HRS2008
# drop variables that we are not using in 2010 dataset 
#ls(HRS2010_data)







write.csv(HRS2008_data, paste(SOURCE_ROOT, "HRS_2008_data/HRS2008_discrimination_dataset_march2022.csv", sep=""))
write.csv(HRS2010_data, paste(SOURCE_ROOT, "HRS_2010_data/HRS2010_discrimination_dataset_march2022.csv", sep=""))
write.csv(HRS2012_data, paste(SOURCE_ROOT, "HRS_2012_data/HRS2012_discrimination_dataset_march2022.csv", sep=""))
write.csv(HRS2014_data, paste(SOURCE_ROOT, "HRS_2014_data/HRS2014_discrimination_dataset_march2022.csv", sep=""))
write.csv(HRS2016_data, paste(SOURCE_ROOT, "HRS_2016_data/HRS2016_discrimination_dataset_march2022.csv", sep=""))
write.csv(HRS2018_data, paste(SOURCE_ROOT, "HRS_2018_data/HRS2018_discrimination_dataset_march2022.csv", sep=""))


print("use non-capital letters for vars for harmonised data")
print("organise by PN, ID, (match)") 
print("add covariates: BMI from harmonised data") 
print("add HRs_year to all var names, and also save the name without the HRS_year in the var name for the cumulative effects data analysis")
