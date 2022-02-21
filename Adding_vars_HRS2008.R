
library(dplyr)


HRS2008_data = read.csv("/Users/aliya/my_docs/KCL_postDoc/Data_analysis/HRS_2008_data/HRS2008_discrimination_dataset.csv")
Original_vars = read.csv("/Users/aliya/my_docs/KCL_postDoc/Data_analysis/HRS_2008_data/HRS2008_ALLData_originalVARNames.csv")

head(HRS2008_data)


#MA019
#Original_vars

#Original_vars$L

#age NO AGE VAR, could not find online
#LA044
#Original_vars$LLB001A


#Original_vars$LA019

#name in the hRS harmonised dataset: Original_vars$R9AGEY_B

#LB016 Description:	  R college degree:	(Did you get a college degree?)  1. Yes, 5. No, 8. Don't know; not ascertained, 9. Refused, Blank. Inapplicable; partial interview

#disability 
#Original_vars$LM019

#race: LB089, 1. White/caucasian, 2. Black/african american, 97. Other (specify) - masked version includes american indian, alaskan native, asian, and pacific islander, 98. Don't know; not ascertained, 99. Refused, Blank. Inapplicable; partial interview
#Original_vars$LB089

#sex

#SES
#wealth
# in harmonised rnad file: #H9ATOTW Original_vars$H9ATOTW

#religion 

#ancestery LB002Description:	  Born in us Were you born in the United States?Answer choices:	  1. Yes, 5. No, 8. Don't know; not ascertained, 9. Refused, Blank. Inapplicable; partial interview

#US citizenship: LB085 



#age_discrim
#financial_status discrim 
#gender/sex  discrim
#sexual orientation discrim
#race discrim
#weight discrim
#religion discrim
#ancestary discrim 

#check what other types of discriminaiton 
#make sure the names are consisitent with the rest 

#physical activity 
#actigraph 


#SES
#wealth


#sex (1 = male, 2 = female)
#below is not correct, the var name speficied in the code book on global ageing is incorrect (LX060_R)
#HRS2008_data$sex_1_2 =  Original_vars$LX060_R

#bmi
HRS2008_data$HRS2008_weight_pounds  = Original_vars$LC139

HRS2008_data$HRS2008_weight_kg = 0.453592 * HRS2008_data$HRS2008_weight_pounds
#      HEIGHT FEET
HRS2008_data$HRS2008_height_feet = Original_vars$LC141
HRS2008_data$HRS2008_height_feet = as.numeric(HRS2008_data$HRS2008_height_feet)

HRS2008_data$HRS2008_height_meters = 0.3048 * HRS2008_data$HRS2008_height_feet

############### ••• BMI ••• ###############
HRS2008_data$HRS2008_BMI = HRS2008_data$HRS2008_weight_kg /((HRS2008_data$HRS2008_height_meters)^2) 

#race
#sexual orientation has to be subsetted to those reporting in 2016, since only one wave asked about that 


# HRS2008_reason_discrim1_reason_disability == 1)

#HRS2008_data$discrim_harassed = HRS2008_data$HRS2008_discrim_harassed 
HRS2008_data$HRS2008_discrim_harassed = Original_vars$LLB030E

#HRS2008_data$discrim_lessrespect = HRS2008_data$HRS2008_discrim_lessrespect 
HRS2008_data$HRS2008_discrim_lessrespect  = Original_vars$LLB030A

#HRS2008_data$discrim_poorerservice = HRS2008_data$HRS2008_discrim_poorerservice 
HRS2008_data$HRS2008_discrim_poorerservice = Original_vars$LLB030B

#HRS2008_data$discrim_notclever = HRS2008_data$HRS2008_discrim_notclever
HRS2008_data$HRS2008_discrim_notclever = Original_vars$LLB030C

#HRS2008_data$discrim_medical = HRS2008_data$HRS2008_discrim_medical 
HRS2008_data$HRS2008_discrim_medical = Original_vars$LLB030F

#HRS2008_data$discrim_afraidothers = HRS2008_data$HRS2008_discrim_afraidothers 
HRS2008_data$HRS2008_discrim_afraidothers  = Original_vars$LLB030D


# Q31.WHY EXPERIENCES HAPPENED TO YOU - 1

HRS2008_data$HRS2008_reason_discrim1  = Original_vars$LLB031M1         
HRS2008_data$HRS2008_reason_discrim1 = as.numeric(HRS2008_data$HRS2008_reason_discrim1)

#         1.  YOUR ANCESTRY OR NATIONAL ORIGIN
#           2.  YOUR GENDER
#           3.  YOUR RACE
#           4.  YOUR AGE
#           5.  YOUR RELIGION
#           6.  YOUR WEIGHT
#          7.  A PHYSICAL DISABILITY
#           8.  AN ASPECT OF YOUR PHYSICAL APPEARANCE
#           9.  YOUR SEXUAL ORIENTATION
#         10.  YOUR FINANCIAL STATUS
#          11.  OTHER


HRS2008_data$HRS2008_reason_discrim1_reason_age = case_when(HRS2008_data$HRS2008_reason_discrim1 == 1 ~ 0,
                                                            HRS2008_data$HRS2008_reason_discrim1 == 2 ~ 0, 
                                                            HRS2008_data$HRS2008_reason_discrim1 == 3 ~ 0,
                                                            HRS2008_data$HRS2008_reason_discrim1 == 4 ~ 1,
                                                            HRS2008_data$HRS2008_reason_discrim1 == 5 ~ 0,
                                                            HRS2008_data$HRS2008_reason_discrim1 == 6 ~ 0, 
                                                            HRS2008_data$HRS2008_reason_discrim1 == 7 ~ 0, 
                                                            HRS2008_data$HRS2008_reason_discrim1 == 8 ~ 0,
                                                            HRS2008_data$HRS2008_reason_discrim1 == 9 ~ 0,
                                                            HRS2008_data$HRS2008_reason_discrim1 == 10 ~ 0,
                                                            HRS2008_data$HRS2008_reason_discrim1 == 11 ~ 0)

unique(HRS2008_data$HRS2008_reason_discrim1_reason_age)


HRS2008_data$HRS2008_reason_discrim1_reason_disability = case_when(HRS2008_data$HRS2008_reason_discrim1 == 1 ~ 0,
                                                                   HRS2008_data$HRS2008_reason_discrim1 == 2 ~ 0, 
                                                                   HRS2008_data$HRS2008_reason_discrim1 == 3 ~ 0,
                                                                   HRS2008_data$HRS2008_reason_discrim1 == 4 ~ 0,
                                                                   HRS2008_data$HRS2008_reason_discrim1 == 5 ~ 0,
                                                                   HRS2008_data$HRS2008_reason_discrim1 == 6 ~ 0, 
                                                                   HRS2008_data$HRS2008_reason_discrim1 == 7 ~ 1, 
                                                                   HRS2008_data$HRS2008_reason_discrim1 == 8 ~ 0,
                                                                   HRS2008_data$HRS2008_reason_discrim1 == 9 ~ 0,
                                                                   HRS2008_data$HRS2008_reason_discrim1 == 10 ~ 0,
                                                                   HRS2008_data$HRS2008_reason_discrim1 == 11 ~ 0)

HRS2008_data$HRS2008_reason_discrim1_reason_financial = case_when(HRS2008_data$HRS2008_reason_discrim1 == 1 ~ 0,
                                                                  HRS2008_data$HRS2008_reason_discrim1 == 2 ~ 0, 
                                                                  HRS2008_data$HRS2008_reason_discrim1 == 3 ~ 0,
                                                                  HRS2008_data$HRS2008_reason_discrim1 == 4 ~ 0,
                                                                  HRS2008_data$HRS2008_reason_discrim1 == 5 ~ 0,
                                                                  HRS2008_data$HRS2008_reason_discrim1 == 6 ~ 0, 
                                                                  HRS2008_data$HRS2008_reason_discrim1 == 7 ~ 0, 
                                                                  HRS2008_data$HRS2008_reason_discrim1 == 8 ~ 0,
                                                                  HRS2008_data$HRS2008_reason_discrim1 == 9 ~ 0,
                                                                  HRS2008_data$HRS2008_reason_discrim1 == 10 ~ 0,
                                                                  HRS2008_data$HRS2008_reason_discrim1 == 11 ~ 1)

HRS2008_data$HRS2008_reason_discrim1_reason_gender = case_when(HRS2008_data$HRS2008_reason_discrim1 == 1 ~ 0,
                                                               HRS2008_data$HRS2008_reason_discrim1 == 2 ~ 1, 
                                                               HRS2008_data$HRS2008_reason_discrim1 == 3 ~ 0,
                                                               HRS2008_data$HRS2008_reason_discrim1 == 4 ~ 0,
                                                               HRS2008_data$HRS2008_reason_discrim1 == 5 ~ 0,
                                                               HRS2008_data$HRS2008_reason_discrim1 == 6 ~ 0, 
                                                               HRS2008_data$HRS2008_reason_discrim1 == 7 ~ 0, 
                                                               HRS2008_data$HRS2008_reason_discrim1 == 8 ~ 0,
                                                               HRS2008_data$HRS2008_reason_discrim1 == 9 ~ 0,
                                                               HRS2008_data$HRS2008_reason_discrim1 == 10 ~ 0,
                                                               HRS2008_data$HRS2008_reason_discrim1 == 11 ~ 0)

HRS2008_data$HRS2008_reason_discrim1_reason_race = case_when(HRS2008_data$HRS2008_reason_discrim1 == 1 ~ 0,
                                                             HRS2008_data$HRS2008_reason_discrim1 == 2 ~ 0, 
                                                             HRS2008_data$HRS2008_reason_discrim1 == 3 ~ 1,
                                                             HRS2008_data$HRS2008_reason_discrim1 == 4 ~ 0,
                                                             HRS2008_data$HRS2008_reason_discrim1 == 5 ~ 0,
                                                             HRS2008_data$HRS2008_reason_discrim1 == 6 ~ 0, 
                                                             HRS2008_data$HRS2008_reason_discrim1 == 7 ~ 0, 
                                                             HRS2008_data$HRS2008_reason_discrim1 == 8 ~ 0,
                                                             HRS2008_data$HRS2008_reason_discrim1 == 9 ~ 0,
                                                             HRS2008_data$HRS2008_reason_discrim1 == 10 ~ 0,
                                                             HRS2008_data$HRS2008_reason_discrim1 == 11 ~ 0)

HRS2008_data$HRS2008_reason_discrim1_reason_sexuality = case_when(HRS2008_data$HRS2008_reason_discrim1 == 1 ~ 0,
                                                                  HRS2008_data$HRS2008_reason_discrim1 == 2 ~ 0, 
                                                                  HRS2008_data$HRS2008_reason_discrim1 == 3 ~ 0,
                                                                  HRS2008_data$HRS2008_reason_discrim1 == 4 ~ 0,
                                                                  HRS2008_data$HRS2008_reason_discrim1 == 5 ~ 0,
                                                                  HRS2008_data$HRS2008_reason_discrim1 == 6 ~ 0, 
                                                                  HRS2008_data$HRS2008_reason_discrim1 == 7 ~ 0, 
                                                                  HRS2008_data$HRS2008_reason_discrim1 == 8 ~ 0,
                                                                  HRS2008_data$HRS2008_reason_discrim1 == 9 ~ 1,
                                                                  HRS2008_data$HRS2008_reason_discrim1 == 10 ~ 0,
                                                                  HRS2008_data$HRS2008_reason_discrim1 == 11 ~ 0)

HRS2008_data$HRS2008_reason_discrim1_reason_weight = case_when(HRS2008_data$HRS2008_reason_discrim1 == 1 ~ 0,
                                                               HRS2008_data$HRS2008_reason_discrim1 == 2 ~ 0, 
                                                               HRS2008_data$HRS2008_reason_discrim1 == 3 ~ 0,
                                                               HRS2008_data$HRS2008_reason_discrim1 == 4 ~ 0,
                                                               HRS2008_data$HRS2008_reason_discrim1 == 5 ~ 0,
                                                               HRS2008_data$HRS2008_reason_discrim1 == 6 ~ 1, 
                                                               HRS2008_data$HRS2008_reason_discrim1 == 7 ~ 0, 
                                                               HRS2008_data$HRS2008_reason_discrim1 == 8 ~ 0,
                                                               HRS2008_data$HRS2008_reason_discrim1 == 9 ~ 0,
                                                               HRS2008_data$HRS2008_reason_discrim1 == 10 ~ 0,
                                                               HRS2008_data$HRS2008_reason_discrim1 == 11 ~ 0)

HRS2008_data$HRS2008_reason_discrim1_reason_national = case_when(HRS2008_data$HRS2008_reason_discrim1 == 1 ~ 1,
                                                                 HRS2008_data$HRS2008_reason_discrim1 == 2 ~ 0, 
                                                                 HRS2008_data$HRS2008_reason_discrim1 == 3 ~ 0,
                                                                 HRS2008_data$HRS2008_reason_discrim1 == 4 ~ 0,
                                                                 HRS2008_data$HRS2008_reason_discrim1 == 5 ~ 0,
                                                                 HRS2008_data$HRS2008_reason_discrim1 == 6 ~ 0, 
                                                                 HRS2008_data$HRS2008_reason_discrim1 == 7 ~ 0, 
                                                                 HRS2008_data$HRS2008_reason_discrim1 == 8 ~ 0,
                                                                 HRS2008_data$HRS2008_reason_discrim1 == 9 ~ 0,
                                                                 HRS2008_data$HRS2008_reason_discrim1 == 10 ~ 0,
                                                                 HRS2008_data$HRS2008_reason_discrim1 == 11 ~ 0)

HRS2008_data$HRS2008_reason_discrim1_reason_religion = case_when(HRS2008_data$HRS2008_reason_discrim1 == 1 ~ 0,
                                                                 HRS2008_data$HRS2008_reason_discrim1 == 2 ~ 0, 
                                                                 HRS2008_data$HRS2008_reason_discrim1 == 3 ~ 0,
                                                                 HRS2008_data$HRS2008_reason_discrim1 == 4 ~ 0,
                                                                 HRS2008_data$HRS2008_reason_discrim1 == 5 ~ 1,
                                                                 HRS2008_data$HRS2008_reason_discrim1 == 6 ~ 0, 
                                                                 HRS2008_data$HRS2008_reason_discrim1 == 7 ~ 0, 
                                                                 HRS2008_data$HRS2008_reason_discrim1 == 8 ~ 0,
                                                                 HRS2008_data$HRS2008_reason_discrim1 == 9 ~ 0,
                                                                 HRS2008_data$HRS2008_reason_discrim1 == 10 ~ 0,
                                                                 HRS2008_data$HRS2008_reason_discrim1 == 11 ~ 0)


write.csv(HRS2008_data, file = "/Users/aliya/my_docs/KCL_postDoc/Data_analysis/HRS_2008_data/HRS2008_discrimination_dataset_new.csv")

