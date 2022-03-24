## Set the root directory to look for source code.
#laptop: 
#/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/
#"/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"
"/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"

library(dplyr)

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
#we created a binary variable to indicate whether participants had experienced discrimination in the past year 
#(a few times or more a year vs. less than once a year or never)

#1 = almost everyday
#2 = at least once a week
#3 = a few times a month
#4 = a few times a year
#5 = less than once a year
#6 = never 

HRS2008_data$discrim_harassed = HRS2008_data$HRS2008_discrim_harassed
HRS2008_data$discrim_lessrespect  = HRS2008_data$HRS2008_discrim_lessrespect 
HRS2008_data$discrim_poorerservice = HRS2008_data$HRS2008_discrim_poorerservice 
HRS2008_data$discrim_notclever = HRS2008_data$HRS2008_discrim_notclever
HRS2008_data$discrim_medical = HRS2008_data$HRS2008_discrim_medical 
HRS2008_data$discrim_afraidothers = HRS2008_data$HRS2008_discrim_afraidothers 


HRS2008_data$discrim_harassed_bin = case_when(HRS2008_data$HRS2008_discrim_harassed == 1 ~ 1, 
                                          HRS2008_data$HRS2008_discrim_harassed == 2 ~ 1, 
                                          HRS2008_data$HRS2008_discrim_harassed == 3 ~ 1, 
                                          HRS2008_data$HRS2008_discrim_harassed == 4 ~ 1, 
                                          HRS2008_data$HRS2008_discrim_harassed == 5 ~ 0, 
                                          HRS2008_data$HRS2008_discrim_harassed == 6 ~ 0) 



HRS2008_data$discrim_lessrespect_bin  = case_when(HRS2008_data$HRS2008_discrim_lessrespect == 1 ~ 1, 
                                                  HRS2008_data$HRS2008_discrim_lessrespect == 2 ~ 1, 
                                                  HRS2008_data$HRS2008_discrim_lessrespect == 3 ~ 1, 
                                                  HRS2008_data$HRS2008_discrim_lessrespect == 4 ~ 1, 
                                                  HRS2008_data$HRS2008_discrim_lessrespect == 5 ~ 0, 
                                                  HRS2008_data$HRS2008_discrim_lessrespect == 6 ~ 0) 



HRS2008_data$discrim_poorerservice_bin  = case_when(HRS2008_data$HRS2008_discrim_poorerservice == 1 ~ 1, 
                                                    HRS2008_data$HRS2008_discrim_poorerservice == 2 ~ 1, 
                                                    HRS2008_data$HRS2008_discrim_poorerservice == 3 ~ 1, 
                                                    HRS2008_data$HRS2008_discrim_poorerservice == 4 ~ 1, 
                                                    HRS2008_data$HRS2008_discrim_poorerservice == 5 ~ 0, 
                                                    HRS2008_data$HRS2008_discrim_poorerservice == 6 ~ 0) 
  
HRS2008_data$discrim_notclever_bin = case_when(HRS2008_data$HRS2008_discrim_notclever == 1 ~ 1, 
                                               HRS2008_data$HRS2008_discrim_notclever == 2 ~ 1, 
                                               HRS2008_data$HRS2008_discrim_notclever == 3 ~ 1, 
                                               HRS2008_data$HRS2008_discrim_notclever == 4 ~ 1, 
                                               HRS2008_data$HRS2008_discrim_notclever == 5 ~ 0, 
                                               HRS2008_data$HRS2008_discrim_notclever == 6 ~ 0) 
  
  


HRS2008_data$discrim_medical_bin =  case_when(HRS2008_data$HRS2008_discrim_medical == 1 ~ 1, 
                                              HRS2008_data$HRS2008_discrim_medical == 2 ~ 1, 
                                              HRS2008_data$HRS2008_discrim_medical == 3 ~ 1, 
                                              HRS2008_data$HRS2008_discrim_medical == 4 ~ 1, 
                                              HRS2008_data$HRS2008_discrim_medical == 5 ~ 0, 
                                              HRS2008_data$HRS2008_discrim_medical == 6 ~ 0) 
  

HRS2008_data$discrim_afraidothers_bin = case_when(HRS2008_data$HRS2008_discrim_afraidothers == 1 ~ 1, 
                                                  HRS2008_data$HRS2008_discrim_afraidothers == 2 ~ 1, 
                                                  HRS2008_data$HRS2008_discrim_afraidothers == 3 ~ 1, 
                                                  HRS2008_data$HRS2008_discrim_afraidothers == 4 ~ 1, 
                                                  HRS2008_data$HRS2008_discrim_afraidothers == 5 ~ 0, 
                                                  HRS2008_data$HRS2008_discrim_afraidothers == 6 ~ 0) 
  

# 2010 
HRS2010_data$discrim_harassed = HRS2010_data$HRS2010_discrim_harassed 
HRS2010_data$discrim_lessrespect = HRS2010_data$HRS2010_discrim_lessrespect 
HRS2010_data$discrim_poorerservice = HRS2010_data$HRS2010_discrim_poorerservice 
HRS2010_data$discrim_notclever = HRS2010_data$HRS2010_discrim_notclever
HRS2010_data$discrim_medical = HRS2010_data$HRS2010_discrim_medical 
HRS2010_data$discrim_afraidothers = HRS2010_data$HRS2010_discrim_afraidothers 

HRS2010_data$discrim_harassed_bin = case_when(HRS2010_data$HRS2010_discrim_harassed == 1 ~ 1, 
                                              HRS2010_data$HRS2010_discrim_harassed == 2 ~ 1, 
                                              HRS2010_data$HRS2010_discrim_harassed == 3 ~ 1, 
                                              HRS2010_data$HRS2010_discrim_harassed == 4 ~ 1, 
                                              HRS2010_data$HRS2010_discrim_harassed == 5 ~ 0, 
                                              HRS2010_data$HRS2010_discrim_harassed == 6 ~ 0) 



HRS2010_data$discrim_lessrespect_bin  = case_when(HRS2010_data$HRS2010_discrim_lessrespect == 1 ~ 1, 
                                                  HRS2010_data$HRS2010_discrim_lessrespect == 2 ~ 1, 
                                                  HRS2010_data$HRS2010_discrim_lessrespect == 3 ~ 1, 
                                                  HRS2010_data$HRS2010_discrim_lessrespect == 4 ~ 1, 
                                                  HRS2010_data$HRS2010_discrim_lessrespect == 5 ~ 0, 
                                                  HRS2010_data$HRS2010_discrim_lessrespect == 6 ~ 0) 



HRS2010_data$discrim_poorerservice_bin  = case_when(HRS2010_data$HRS2010_discrim_poorerservice == 1 ~ 1, 
                                                    HRS2010_data$HRS2010_discrim_poorerservice == 2 ~ 1, 
                                                    HRS2010_data$HRS2010_discrim_poorerservice == 3 ~ 1, 
                                                    HRS2010_data$HRS2010_discrim_poorerservice == 4 ~ 1, 
                                                    HRS2010_data$HRS2010_discrim_poorerservice == 5 ~ 0, 
                                                    HRS2010_data$HRS2010_discrim_poorerservice == 6 ~ 0) 

HRS2010_data$discrim_notclever_bin = case_when(HRS2010_data$HRS2010_discrim_notclever == 1 ~ 1, 
                                               HRS2010_data$HRS2010_discrim_notclever == 2 ~ 1, 
                                               HRS2010_data$HRS2010_discrim_notclever == 3 ~ 1, 
                                               HRS2010_data$HRS2010_discrim_notclever == 4 ~ 1, 
                                               HRS2010_data$HRS2010_discrim_notclever == 5 ~ 0, 
                                               HRS2010_data$HRS2010_discrim_notclever == 6 ~ 0) 




HRS2010_data$discrim_medical_bin =  case_when(HRS2010_data$HRS2010_discrim_medical == 1 ~ 1, 
                                              HRS2010_data$HRS2010_discrim_medical == 2 ~ 1, 
                                              HRS2010_data$HRS2010_discrim_medical == 3 ~ 1, 
                                              HRS2010_data$HRS2010_discrim_medical == 4 ~ 1, 
                                              HRS2010_data$HRS2010_discrim_medical == 5 ~ 0, 
                                              HRS2010_data$HRS2010_discrim_medical == 6 ~ 0) 


HRS2010_data$discrim_afraidothers_bin = case_when(HRS2010_data$HRS2010_discrim_afraidothers == 1 ~ 1, 
                                                  HRS2010_data$HRS2010_discrim_afraidothers == 2 ~ 1, 
                                                  HRS2010_data$HRS2010_discrim_afraidothers == 3 ~ 1, 
                                                  HRS2010_data$HRS2010_discrim_afraidothers == 4 ~ 1, 
                                                  HRS2010_data$HRS2010_discrim_afraidothers == 5 ~ 0, 
                                                  HRS2010_data$HRS2010_discrim_afraidothers == 6 ~ 0) 

# 2012
HRS2012_data$discrim_harassed = HRS2012_data$HRS2012_discrim_harassed


HRS2012_data$discrim_harassed_bin = case_when(HRS2012_data$HRS2012_discrim_harassed == 1 ~ 1, 
                                              HRS2012_data$HRS2012_discrim_harassed == 2 ~ 1, 
                                              HRS2012_data$HRS2012_discrim_harassed == 3 ~ 1, 
                                              HRS2012_data$HRS2012_discrim_harassed == 4 ~ 1, 
                                              HRS2012_data$HRS2012_discrim_harassed == 5 ~ 0, 
                                              HRS2012_data$HRS2012_discrim_harassed == 6 ~ 0)  

HRS2012_data$discrim_lessrespect = HRS2012_data$HRS2012_discrim_lessrespect 


HRS2012_data$discrim_lessrespect_bin = case_when(HRS2012_data$HRS2012_discrim_lessrespect == 1 ~ 1, 
                                              HRS2012_data$HRS2012_discrim_lessrespect == 2 ~ 1, 
                                              HRS2012_data$HRS2012_discrim_lessrespect == 3 ~ 1, 
                                              HRS2012_data$HRS2012_discrim_lessrespect == 4 ~ 1, 
                                              HRS2012_data$HRS2012_discrim_lessrespect == 5 ~ 0, 
                                              HRS2012_data$HRS2012_discrim_lessrespect == 6 ~ 0)   

HRS2012_data$discrim_poorerservice = HRS2012_data$HRS2012_discrim_poorerservice 

HRS2012_data$discrim_poorerservice_bin = case_when(HRS2012_data$HRS2012_discrim_poorerservice == 1 ~ 1, 
                                                 HRS2012_data$HRS2012_discrim_poorerservice == 2 ~ 1, 
                                                 HRS2012_data$HRS2012_discrim_poorerservice == 3 ~ 1, 
                                                 HRS2012_data$HRS2012_discrim_poorerservice == 4 ~ 1, 
                                                 HRS2012_data$HRS2012_discrim_poorerservice == 5 ~ 0, 
                                                 HRS2012_data$HRS2012_discrim_poorerservice == 6 ~ 0)    


HRS2012_data$discrim_notclever = HRS2012_data$HRS2012_discrim_notclever

HRS2012_data$discrim_notclever_bin = case_when(HRS2012_data$HRS2012_discrim_notclever == 1 ~ 1, 
                                                   HRS2012_data$HRS2012_discrim_notclever == 2 ~ 1, 
                                                   HRS2012_data$HRS2012_discrim_notclever == 3 ~ 1, 
                                                   HRS2012_data$HRS2012_discrim_notclever == 4 ~ 1, 
                                                   HRS2012_data$HRS2012_discrim_notclever == 5 ~ 0, 
                                                   HRS2012_data$HRS2012_discrim_notclever == 6 ~ 0)    


HRS2012_data$discrim_medical = HRS2012_data$HRS2012_discrim_medical

HRS2012_data$discrim_medical_bin = case_when(HRS2012_data$HRS2012_discrim_medical == 1 ~ 1, 
                                               HRS2012_data$HRS2012_discrim_medical == 2 ~ 1, 
                                               HRS2012_data$HRS2012_discrim_medical == 3 ~ 1, 
                                               HRS2012_data$HRS2012_discrim_medical == 4 ~ 1, 
                                               HRS2012_data$HRS2012_discrim_medical == 5 ~ 0, 
                                               HRS2012_data$HRS2012_discrim_medical == 6 ~ 0)    

HRS2012_data$discrim_afraidothers = HRS2012_data$HRS2012_discrim_afraidothers 

HRS2012_data$discrim_afraidothers_bin = case_when(HRS2012_data$HRS2012_discrim_afraidothers == 1 ~ 1, 
                                             HRS2012_data$HRS2012_discrim_afraidothers == 2 ~ 1, 
                                             HRS2012_data$HRS2012_discrim_afraidothers == 3 ~ 1, 
                                             HRS2012_data$HRS2012_discrim_afraidothers == 4 ~ 1, 
                                             HRS2012_data$HRS2012_discrim_afraidothers == 5 ~ 0, 
                                             HRS2012_data$HRS2012_discrim_afraidothers == 6 ~ 0)    

# 2014
HRS2014_data$discrim_harassed = HRS2014_data$HRS2014_discrim_harassed 

HRS2014_data$discrim_harassed_bin = case_when(HRS2014_data$HRS2014_discrim_harassed == 1 ~ 1, 
                                          HRS2014_data$HRS2014_discrim_harassed == 2 ~ 1, 
                                          HRS2014_data$HRS2014_discrim_harassed == 3 ~ 1, 
                                          HRS2014_data$HRS2014_discrim_harassed == 4 ~ 1, 
                                          HRS2014_data$HRS2014_discrim_harassed == 5 ~ 0, 
                                          HRS2014_data$HRS2014_discrim_harassed == 6 ~ 0)    


HRS2014_data$discrim_lessrespect = HRS2014_data$HRS2014_discrim_lessrespect 

HRS2014_data$discrim_lessrespect_bin = case_when(HRS2014_data$HRS2014_discrim_lessrespect == 1 ~ 1, 
                                          HRS2014_data$HRS2014_discrim_lessrespect == 2 ~ 1, 
                                          HRS2014_data$HRS2014_discrim_lessrespect == 3 ~ 1, 
                                          HRS2014_data$HRS2014_discrim_lessrespect == 4 ~ 1, 
                                          HRS2014_data$HRS2014_discrim_lessrespect == 5 ~ 0, 
                                          HRS2014_data$HRS2014_discrim_lessrespect == 6 ~ 0)    


HRS2014_data$discrim_poorerservice = HRS2014_data$HRS2014_discrim_poorerservice 

HRS2014_data$discrim_poorerservice_bin = case_when(HRS2014_data$HRS2014_discrim_poorerservice == 1 ~ 1, 
                                                 HRS2014_data$HRS2014_discrim_poorerservice == 2 ~ 1, 
                                                 HRS2014_data$HRS2014_discrim_poorerservice == 3 ~ 1, 
                                                 HRS2014_data$HRS2014_discrim_poorerservice == 4 ~ 1, 
                                                 HRS2014_data$HRS2014_discrim_poorerservice == 5 ~ 0, 
                                                 HRS2014_data$HRS2014_discrim_poorerservice == 6 ~ 0)    


HRS2014_data$discrim_notclever = HRS2014_data$HRS2014_discrim_notclever

HRS2014_data$discrim_notclever_bin = case_when(HRS2014_data$HRS2014_discrim_notclever == 1 ~ 1, 
                                                   HRS2014_data$HRS2014_discrim_notclever == 2 ~ 1, 
                                                   HRS2014_data$HRS2014_discrim_notclever == 3 ~ 1, 
                                                   HRS2014_data$HRS2014_discrim_notclever == 4 ~ 1, 
                                                   HRS2014_data$HRS2014_discrim_notclever == 5 ~ 0, 
                                                   HRS2014_data$HRS2014_discrim_notclever == 6 ~ 0)    

HRS2014_data$discrim_medical = HRS2014_data$HRS2014_discrim_medical 


HRS2014_data$discrim_medical_bin = case_when(HRS2014_data$HRS2014_discrim_medical == 1 ~ 1, 
                                               HRS2014_data$HRS2014_discrim_medical  == 2 ~ 1, 
                                               HRS2014_data$HRS2014_discrim_medical  == 3 ~ 1, 
                                               HRS2014_data$HRS2014_discrim_medical == 4 ~ 1, 
                                               HRS2014_data$HRS2014_discrim_medical == 5 ~ 0, 
                                               HRS2014_data$HRS2014_discrim_medical == 6 ~ 0)  

HRS2014_data$discrim_afraidothers = HRS2014_data$HRS2014_discrim_afraidothers 

HRS2014_data$discrim_afraidothers_bin = case_when(HRS2014_data$HRS2014_discrim_afraidothers == 1 ~ 1, 
                                             HRS2014_data$HRS2014_discrim_afraidothers  == 2 ~ 1, 
                                             HRS2014_data$HRS2014_discrim_afraidothers  == 3 ~ 1, 
                                             HRS2014_data$HRS2014_discrim_afraidothers == 4 ~ 1, 
                                             HRS2014_data$HRS2014_discrim_afraidothers == 5 ~ 0, 
                                             HRS2014_data$HRS2014_discrim_afraidothers == 6 ~ 0)  

# 2016
HRS2016_data$discrim_harassed = HRS2016_data$HRS2016_discrim_harassed 
HRS2016_data$discrim_lessrespect = HRS2016_data$HRS2016_discrim_lessrespect 
HRS2016_data$discrim_poorerservice = HRS2016_data$HRS2016_discrim_poorerservice 
HRS2016_data$discrim_notclever = HRS2016_data$HRS2016_discrim_notclever
HRS2016_data$discrim_medical = HRS2016_data$HRS2016_discrim_medical 
HRS2016_data$discrim_afraidothers = HRS2016_data$HRS2016_discrim_afraidothers 


HRS2016_data$discrim_harassed_bin = case_when(HRS2016_data$HRS2016_discrim_harassed == 1 ~ 1, 
                                              HRS2016_data$HRS2016_discrim_harassed == 2 ~ 1, 
                                              HRS2016_data$HRS2016_discrim_harassed == 3 ~ 1, 
                                              HRS2016_data$HRS2016_discrim_harassed == 4 ~ 1, 
                                              HRS2016_data$HRS2016_discrim_harassed == 5 ~ 0, 
                                              HRS2016_data$HRS2016_discrim_harassed == 6 ~ 0)    



HRS2016_data$discrim_lessrespect_bin = case_when(HRS2016_data$HRS2016_discrim_lessrespect == 1 ~ 1, 
                                                 HRS2016_data$HRS2016_discrim_lessrespect == 2 ~ 1, 
                                                 HRS2016_data$HRS2016_discrim_lessrespect == 3 ~ 1, 
                                                 HRS2016_data$HRS2016_discrim_lessrespect == 4 ~ 1, 
                                                 HRS2016_data$HRS2016_discrim_lessrespect == 5 ~ 0, 
                                                 HRS2016_data$HRS2016_discrim_lessrespect == 6 ~ 0)    



HRS2016_data$discrim_poorerservice_bin = case_when(HRS2016_data$HRS2016_discrim_poorerservice == 1 ~ 1, 
                                                   HRS2016_data$HRS2016_discrim_poorerservice == 2 ~ 1, 
                                                   HRS2016_data$HRS2016_discrim_poorerservice == 3 ~ 1, 
                                                   HRS2016_data$HRS2016_discrim_poorerservice == 4 ~ 1, 
                                                   HRS2016_data$HRS2016_discrim_poorerservice == 5 ~ 0, 
                                                   HRS2016_data$HRS2016_discrim_poorerservice == 6 ~ 0)    



HRS2016_data$discrim_notclever_bin = case_when(HRS2016_data$HRS2016_discrim_notclever == 1 ~ 1, 
                                               HRS2016_data$HRS2016_discrim_notclever == 2 ~ 1, 
                                               HRS2016_data$HRS2016_discrim_notclever == 3 ~ 1, 
                                               HRS2016_data$HRS2016_discrim_notclever == 4 ~ 1, 
                                               HRS2016_data$HRS2016_discrim_notclever == 5 ~ 0, 
                                               HRS2016_data$HRS2016_discrim_notclever == 6 ~ 0)    


HRS2016_data$discrim_medical_bin = case_when(HRS2016_data$HRS2016_discrim_medical == 1 ~ 1, 
                                             HRS2016_data$HRS2016_discrim_medical  == 2 ~ 1, 
                                             HRS2016_data$HRS2016_discrim_medical  == 3 ~ 1, 
                                             HRS2016_data$HRS2016_discrim_medical == 4 ~ 1, 
                                             HRS2016_data$HRS2016_discrim_medical == 5 ~ 0, 
                                             HRS2016_data$HRS2016_discrim_medical == 6 ~ 0)  


HRS2016_data$discrim_afraidothers_bin = case_when(HRS2016_data$HRS2016_discrim_afraidothers == 1 ~ 1, 
                                                  HRS2016_data$HRS2016_discrim_afraidothers  == 2 ~ 1, 
                                                  HRS2016_data$HRS2016_discrim_afraidothers  == 3 ~ 1, 
                                                  HRS2016_data$HRS2016_discrim_afraidothers == 4 ~ 1, 
                                                  HRS2016_data$HRS2016_discrim_afraidothers == 5 ~ 0, 
                                                  HRS2016_data$HRS2016_discrim_afraidothers == 6 ~ 0)  

# 2018
HRS2018_data$discrim_harassed = HRS2018_data$HRS2018_discrim_harassed 
HRS2018_data$discrim_lessrespect = HRS2018_data$HRS2018_discrim_lessrespect 
HRS2018_data$discrim_poorerservice = HRS2018_data$HRS2018_discrim_poorerservice 
HRS2018_data$discrim_notclever = HRS2018_data$HRS2018_discrim_notclever
HRS2018_data$discrim_medical = HRS2018_data$HRS2018_discrim_medical 
HRS2018_data$discrim_afraidothers = HRS2018_data$HRS2018_discrim_afraidothers 


HRS2018_data$discrim_harassed_bin = case_when(HRS2018_data$HRS2018_discrim_harassed == 1 ~ 1, 
                                              HRS2018_data$HRS2018_discrim_harassed == 2 ~ 1, 
                                              HRS2018_data$HRS2018_discrim_harassed == 3 ~ 1, 
                                              HRS2018_data$HRS2018_discrim_harassed == 4 ~ 1, 
                                              HRS2018_data$HRS2018_discrim_harassed == 5 ~ 0, 
                                              HRS2018_data$HRS2018_discrim_harassed == 6 ~ 0)    



HRS2018_data$discrim_lessrespect_bin = case_when(HRS2018_data$HRS2018_discrim_lessrespect == 1 ~ 1, 
                                                 HRS2018_data$HRS2018_discrim_lessrespect == 2 ~ 1, 
                                                 HRS2018_data$HRS2018_discrim_lessrespect == 3 ~ 1, 
                                                 HRS2018_data$HRS2018_discrim_lessrespect == 4 ~ 1, 
                                                 HRS2018_data$HRS2018_discrim_lessrespect == 5 ~ 0, 
                                                 HRS2018_data$HRS2018_discrim_lessrespect == 6 ~ 0)    



HRS2018_data$discrim_poorerservice_bin = case_when(HRS2018_data$HRS2018_discrim_poorerservice == 1 ~ 1, 
                                                   HRS2018_data$HRS2018_discrim_poorerservice == 2 ~ 1, 
                                                   HRS2018_data$HRS2018_discrim_poorerservice == 3 ~ 1, 
                                                   HRS2018_data$HRS2018_discrim_poorerservice == 4 ~ 1, 
                                                   HRS2018_data$HRS2018_discrim_poorerservice == 5 ~ 0, 
                                                   HRS2018_data$HRS2018_discrim_poorerservice == 6 ~ 0)    



HRS2018_data$discrim_notclever_bin = case_when(HRS2018_data$HRS2018_discrim_notclever == 1 ~ 1, 
                                               HRS2018_data$HRS2018_discrim_notclever == 2 ~ 1, 
                                               HRS2018_data$HRS2018_discrim_notclever == 3 ~ 1, 
                                               HRS2018_data$HRS2018_discrim_notclever == 4 ~ 1, 
                                               HRS2018_data$HRS2018_discrim_notclever == 5 ~ 0, 
                                               HRS2018_data$HRS2018_discrim_notclever == 6 ~ 0)    


HRS2018_data$discrim_medical_bin = case_when(HRS2018_data$HRS2018_discrim_medical == 1 ~ 1, 
                                             HRS2018_data$HRS2018_discrim_medical  == 2 ~ 1, 
                                             HRS2018_data$HRS2018_discrim_medical  == 3 ~ 1, 
                                             HRS2018_data$HRS2018_discrim_medical == 4 ~ 1, 
                                             HRS2018_data$HRS2018_discrim_medical == 5 ~ 0, 
                                             HRS2018_data$HRS2018_discrim_medical == 6 ~ 0)  


HRS2018_data$discrim_afraidothers_bin = case_when(HRS2018_data$HRS2018_discrim_afraidothers == 1 ~ 1, 
                                                  HRS2018_data$HRS2018_discrim_afraidothers  == 2 ~ 1, 
                                                  HRS2018_data$HRS2018_discrim_afraidothers  == 3 ~ 1, 
                                                  HRS2018_data$HRS2018_discrim_afraidothers == 4 ~ 1, 
                                                  HRS2018_data$HRS2018_discrim_afraidothers == 5 ~ 0, 
                                                  HRS2018_data$HRS2018_discrim_afraidothers == 6 ~ 0)  

#composite discrimination 
## ## ## ## ADD

#discrimination summary mean score
HRS2008_data$summary_mean_score_discrim = HRS2008_data$summary_mean_score_discrim_2008 

HRS2008_data$summary_mean_score_discrim_bin = case_when(HRS2008_data$summary_mean_score_discrim_2008 == 1 ~ 1, 
                                                  HRS2008_data$summary_mean_score_discrim_2008  == 2 ~ 1, 
                                                  HRS2008_data$summary_mean_score_discrim_2008  == 3 ~ 1, 
                                                  HRS2008_data$summary_mean_score_discrim_2008 == 4 ~ 1, 
                                                  HRS2008_data$summary_mean_score_discrim_2008 == 5 ~ 0, 
                                                  HRS2008_data$summary_mean_score_discrim_2008 == 6 ~ 0)  

HRS2010_data$summary_mean_score_discrim = HRS2010_data$summary_mean_score_discrim_2010 

HRS2010_data$summary_mean_score_discrim_bin = case_when(HRS2010_data$summary_mean_score_discrim_2010 == 1 ~ 1, 
                                                  HRS2010_data$summary_mean_score_discrim_2010  == 2 ~ 1, 
                                                  HRS2010_data$summary_mean_score_discrim_2010  == 3 ~ 1, 
                                                  HRS2010_data$summary_mean_score_discrim_2010 == 4 ~ 1, 
                                                  HRS2010_data$summary_mean_score_discrim_2010 == 5 ~ 0, 
                                                  HRS2010_data$summary_mean_score_discrim_2010 == 6 ~ 0)  

HRS2012_data$summary_mean_score_discrim = HRS2012_data$summary_mean_score_discrim_2012 

HRS2012_data$summary_mean_score_discrim_bin = case_when(HRS2012_data$summary_mean_score_discrim_2012 == 1 ~ 1, 
                                                        HRS2012_data$summary_mean_score_discrim_2012  == 2 ~ 1, 
                                                        HRS2012_data$summary_mean_score_discrim_2012  == 3 ~ 1, 
                                                        HRS2012_data$summary_mean_score_discrim_2012 == 4 ~ 1, 
                                                        HRS2012_data$summary_mean_score_discrim_2012 == 5 ~ 0, 
                                                        HRS2012_data$summary_mean_score_discrim_2012 == 6 ~ 0)  

HRS2014_data$summary_mean_score_discrim = HRS2014_data$summary_mean_score_discrim_2014 

HRS2014_data$summary_mean_score_discrim_bin = case_when(HRS2014_data$summary_mean_score_discrim_2014 == 1 ~ 1, 
                                                        HRS2014_data$summary_mean_score_discrim_2014  == 2 ~ 1, 
                                                        HRS2014_data$summary_mean_score_discrim_2014  == 3 ~ 1, 
                                                        HRS2014_data$summary_mean_score_discrim_2014 == 4 ~ 1, 
                                                        HRS2014_data$summary_mean_score_discrim_2014 == 5 ~ 0, 
                                                        HRS2014_data$summary_mean_score_discrim_2014 == 6 ~ 0)  


HRS2016_data$summary_mean_score_discrim = HRS2016_data$summary_mean_score_discrim_2016

HRS2016_data$summary_mean_score_discrim_bin = case_when(HRS2016_data$summary_mean_score_discrim_2016 == 1 ~ 1, 
                                                        HRS2016_data$summary_mean_score_discrim_2016  == 2 ~ 1, 
                                                        HRS2016_data$summary_mean_score_discrim_2016  == 3 ~ 1, 
                                                        HRS2016_data$summary_mean_score_discrim_2016 == 4 ~ 1, 
                                                        HRS2016_data$summary_mean_score_discrim_2016 == 5 ~ 0, 
                                                        HRS2016_data$summary_mean_score_discrim_2016 == 6 ~ 0)  

HRS2018_data$summary_mean_score_discrim = HRS2018_data$summary_mean_score_discrim_2018 

HRS2018_data$summary_mean_score_discrim_bin = case_when(HRS2018_data$summary_mean_score_discrim_2018 == 1 ~ 1, 
                                                        HRS2018_data$summary_mean_score_discrim_2018  == 2 ~ 1, 
                                                        HRS2018_data$summary_mean_score_discrim_2018  == 3 ~ 1, 
                                                        HRS2018_data$summary_mean_score_discrim_2018 == 4 ~ 1, 
                                                        HRS2018_data$summary_mean_score_discrim_2018 == 5 ~ 0, 
                                                        HRS2018_data$summary_mean_score_discrim_2018 == 6 ~ 0)  

#financial status discrim

HRS2008_data$reason_discirim_financial = HRS2008_data$reason_discirim_financial_2008 
HRS2010_data$reason_discirim_financial = HRS2010_data$reason_discirim_financial_2010 
HRS2012_data$reason_discirim_financial = HRS2012_data$reason_discirim_financial_2012 
HRS2014_data$reason_discirim_financial = HRS2014_data$reason_discirim_financial_2014 
HRS2016_data$reason_discirim_financial = HRS2016_data$reason_discirim_financial_2016 
HRS2018_data$reason_discirim_financial = HRS2018_data$reason_discirim_financial_2018 


# sexual orientation discrim 
#HRS2008_data$reason_discirim_sexuality_2006 = harmonised_data_all_waves$r8dcsxori
HRS2008_data$reason_discirim_sexuality = HRS2008_data$reason_discirim_sexuality_2008 
HRS2010_data$reason_discirim_sexuality = HRS2010_data$reason_discirim_sexuality_2010
HRS2012_data$reason_discirim_sexuality = HRS2012_data$reason_discirim_sexuality_2012 
HRS2014_data$reason_discirim_sexuality = HRS2014_data$reason_discirim_sexuality_2014 
HRS2016_data$reason_discirim_sexuality = HRS2016_data$reason_discirim_sexuality_2016 
HRS2018_data$reason_discirim_sexuality = HRS2018_data$reason_discirim_sexuality_2018

##### discrimination reason age
HRS2008_data$reason_discrim1_reason_age = HRS2008_data$HRS2008_reason_discrim1_reason_age
HRS2010_data$reason_discrim1_reason_age = HRS2010_data$HRS2010_reason_discrim1_reason_age
HRS2012_data$reason_discrim1_reason_age = HRS2012_data$HRS2012_reason_discrim1_reason_age
HRS2014_data$reason_discrim1_reason_age = HRS2014_data$HRS2014_reason_discrim1_reason_age
HRS2016_data$reason_discrim1_reason_age = HRS2016_data$HRS2016_reason_discrim1_reason_age
HRS2018_data$reason_discrim1_reason_age = HRS2018_data$HRS2018_reason_discrim1_reason_age

##### discrim reason gender
HRS2008_data$reason_discrim1_reason_gender = HRS2008_data$HRS2008_reason_discrim1_reason_gender
HRS2010_data$reason_discrim1_reason_gender = HRS2010_data$HRS2010_reason_discrim1_reason_gender
HRS2012_data$reason_discrim1_reason_gender = HRS2012_data$HRS2012_reason_discrim1_reason_gender
HRS2014_data$reason_discrim1_reason_gender = HRS2014_data$HRS2014_reason_discrim1_reason_gender
HRS2016_data$reason_discrim1_reason_gender = HRS2016_data$HRS2016_reason_discrim1_reason_gender
HRS2018_data$reason_discrim1_reason_gender = HRS2018_data$HRS2018_reason_discrim1_reason_gender

##### discrim reason national
HRS2008_data$reason_discrim1_reason_national = HRS2008_data$HRS2008_reason_discrim1_reason_national
HRS2010_data$reason_discrim1_reason_national = HRS2010_data$HRS2010_reason_discrim1_reason_national
HRS2012_data$reason_discrim1_reason_national = HRS2012_data$HRS2012_reason_discrim1_reason_national
HRS2014_data$reason_discrim1_reason_national = HRS2014_data$HRS2014_reason_discrim1_reason_national
HRS2016_data$reason_discrim1_reason_national = HRS2016_data$HRS2016_reason_discrim1_reason_national
HRS2018_data$reason_discrim1_reason_national = HRS2018_data$HRS2018_reason_discrim1_reason_national


##### discrim reason race
HRS2008_data$reason_discrim1_reason_race = HRS2008_data$HRS2008_reason_discrim1_reason_race
HRS2010_data$reason_discrim1_reason_race = HRS2010_data$HRS2010_reason_discrim1_reason_race
HRS2012_data$reason_discrim1_reason_race = HRS2012_data$HRS2012_reason_discrim1_reason_race
HRS2014_data$reason_discrim1_reason_race = HRS2014_data$HRS2014_reason_discrim1_reason_race
HRS2016_data$reason_discrim1_reason_race = HRS2016_data$HRS2016_reason_discrim1_reason_race
HRS2018_data$reason_discrim1_reason_race = HRS2018_data$HRS2018_reason_discrim1_reason_race


##### discrim reason religion
HRS2008_data$reason_discrim1_reason_religion = HRS2008_data$HRS2008_reason_discrim1_reason_religion
HRS2010_data$reason_discrim1_reason_religion = HRS2010_data$HRS2010_reason_discrim1_reason_religion
HRS2012_data$reason_discrim1_reason_religion = HRS2012_data$HRS2012_reason_discrim1_reason_religion
HRS2014_data$reason_discrim1_reason_religion = HRS2014_data$HRS2014_reason_discrim1_reason_religion
HRS2016_data$reason_discrim1_reason_religion = HRS2016_data$HRS2016_reason_discrim1_reason_religion
HRS2018_data$reason_discrim1_reason_religion = HRS2018_data$HRS2018_reason_discrim1_reason_religion




##### discrim reason religion
HRS2008_data$reason_discrim1_reason_weight = HRS2008_data$HRS2008_reason_discrim1_reason_weight
HRS2010_data$reason_discrim1_reason_weight = HRS2010_data$HRS2010_reason_discrim1_reason_weight
HRS2012_data$reason_discrim1_reason_weight = HRS2012_data$HRS2012_reason_discrim1_reason_weight
HRS2014_data$reason_discrim1_reason_weight = HRS2014_data$HRS2014_reason_discrim1_reason_weight
HRS2016_data$reason_discrim1_reason_weight = HRS2016_data$HRS2016_reason_discrim1_reason_weight
HRS2018_data$reason_discrim1_reason_weight = HRS2018_data$HRS2018_reason_discrim1_reason_weight


# number of reasons for discrim 
#HRS2008_data$number_reasons_discrimination_2006 = harmonised_data_all_waves$r8dcreas
HRS2008_data$number_reasons_discrimination = HRS2008_data$number_reasons_discrimination_2008 
HRS2010_data$number_reasons_discrimination = HRS2010_data$number_reasons_discrimination_2010 
HRS2012_data$number_reasons_discrimination = HRS2012_data$number_reasons_discrimination_2012 
HRS2014_data$number_reasons_discrimination = HRS2014_data$number_reasons_discrimination_2014
HRS2016_data$number_reasons_discrimination = HRS2016_data$number_reasons_discrimination_2016
HRS2018_data$number_reasons_discrimination = HRS2018_data$number_reasons_discrimination_2018 



####hypertension 

HRS2008_data$hypertension_new_bin =  case_when(HRS2008_data$hypertension_new == 1 ~ 1, 
                                               HRS2008_data$hypertension_new == 5 ~ 0,
                                               HRS2008_data$hypertension_new == 3 ~ 1)

HRS2010_data$hypertension_new_bin =  case_when(HRS2010_data$hypertension_new == 1 ~ 1, 
                                               HRS2010_data$hypertension_new == 5 ~ 0,
                                               HRS2010_data$hypertension_new == 3 ~ 1)

HRS2012_data$hypertension_new_bin =  case_when(HRS2012_data$hypertension_new == 1 ~ 1, 
                                               HRS2012_data$hypertension_new == 5 ~ 0,
                                               HRS2012_data$hypertension_new == 3 ~ 1)



HRS2014_data$hypertension_new_bin =  case_when(HRS2014_data$hypertension_new == 1 ~ 1, 
                                               HRS2014_data$hypertension_new == 5 ~ 0,
                                               HRS2014_data$hypertension_new == 3 ~ 1)



HRS2016_data$hypertension_new_bin =  case_when(HRS2016_data$hypertension_new == 1 ~ 1, 
                                               HRS2016_data$hypertension_new == 5 ~ 0,
                                               HRS2016_data$hypertension_new == 3 ~ 1)



#####
#####
#####




HRS2014_data$angina_new_bin = case_when(HRS2014_data$angina_new == 1 ~ 1, 
                                        HRS2014_data$angina_new == 5 ~ 0,
                                        HRS2014_data$angina_new == 3 ~ 1)



HRS2018_data$angina_new_bin = case_when(HRS2018_data$angina_new == 1 ~ 1, 
                                        HRS2018_data$angina_new == 5 ~ 0,
                                        HRS2018_data$angina_new == 3 ~ 1)






HRS2008_data$stroke_new_bin = case_when(HRS2008_data$stroke_new == 1 ~ 1, 
                                        HRS2008_data$stroke_new == 5 ~ 0,
                                        HRS2008_data$stroke_new == 3 ~ 1)


HRS2010_data$stroke_new_bin = case_when(HRS2010_data$stroke_new == 1 ~ 1, 
                                        HRS2010_data$stroke_new == 5 ~ 0,
                                        HRS2010_data$stroke_new == 3 ~ 1)


HRS2012_data$stroke_new_bin = case_when(HRS2012_data$stroke_new == 1 ~ 1, 
                                        HRS2012_data$stroke_new == 5 ~ 0,
                                        HRS2012_data$stroke_new == 3 ~ 1)


HRS2014_data$stroke_new_bin = case_when(HRS2014_data$stroke_new == 1 ~ 1, 
                                        HRS2014_data$stroke_new == 5 ~ 0,
                                        HRS2014_data$stroke_new == 3 ~ 1)

HRS2016_data$stroke_new_bin = case_when(HRS2016_data$stroke_new == 1 ~ 1, 
                                        HRS2016_data$stroke_new == 5 ~ 0,
                                        HRS2016_data$stroke_new == 3 ~ 1)




HRS2008_data$heartcondition_new_bin = case_when(HRS2008_data$heartcondition_new == 1 ~ 1, 
                                        HRS2008_data$heartcondition_new == 5 ~ 0,
                                        HRS2008_data$heartcondition_new == 3 ~ 1)


HRS2010_data$heartcondition_new_bin = case_when(HRS2010_data$heartcondition_new == 1 ~ 1, 
                                        HRS2010_data$heartcondition_new == 5 ~ 0,
                                        HRS2010_data$heartcondition_new == 3 ~ 1)









HRS2008_data$heartcondition_ever_bin = case_when(HRS2008_data$heartcondition_ever == 1 ~ 1, 
                                                HRS2008_data$heartcondition_ever == 5 ~ 0,
                                                HRS2008_data$heartcondition_ever == 3 ~ 1)


HRS2010_data$heartcondition_ever_bin = case_when(HRS2010_data$heartcondition_ever == 1 ~ 1, 
                                                HRS2010_data$heartcondition_ever == 5 ~ 0,
                                                HRS2010_data$heartcondition_ever == 3 ~ 1)









HRS2012_data$heartattack_ever_bin = case_when(HRS2012_data$heartattack_ever == 1 ~ 1, 
                                                 HRS2012_data$heartattack_ever == 5 ~ 0,
                                                 HRS2012_data$heartattack_ever == 3 ~ 1)


HRS2014_data$heartattack_ever_bin = case_when(HRS2014_data$heartattack_ever == 1 ~ 1, 
                                                 HRS2014_data$heartattack_ever == 5 ~ 0,
                                                 HRS2014_data$heartattack_ever == 3 ~ 1)

HRS2016_data$heartattack_ever_bin = case_when(HRS2016_data$heartattack_ever == 1 ~ 1, 
                                                 HRS2016_data$heartattack_ever == 5 ~ 0,
                                                 HRS2016_data$heartattack_ever == 3 ~ 1)







HRS2008_data$smokes_ever_bin = case_when(HRS2008_data$smokes_ever == 1 ~ 1, 
                                             HRS2008_data$smokes_ever == 5 ~ 0,
                                             HRS2008_data$smokes_ever == 3 ~ 1)


HRS2010_data$smokes_ever_bin = case_when(HRS2010_data$smokes_ever == 1 ~ 1, 
                                             HRS2010_data$smokes_ever == 5 ~ 0,
                                             HRS2010_data$smokes_ever == 3 ~ 1)


HRS2012_data$smokes_ever_bin = case_when(HRS2012_data$smokes_ever == 1 ~ 1, 
                                             HRS2012_data$smokes_ever == 5 ~ 0,
                                             HRS2012_data$smokes_ever == 3 ~ 1)


HRS2014_data$smokes_ever_bin = case_when(HRS2014_data$smokes_ever == 1 ~ 1, 
                                             HRS2014_data$smokes_ever == 5 ~ 0,
                                             HRS2014_data$smokes_ever == 3 ~ 1)

HRS2016_data$smokes_ever_bin = case_when(HRS2016_data$smokes_ever == 1 ~ 1, 
                                             HRS2016_data$smokes_ever == 5 ~ 0,
                                             HRS2016_data$smokes_ever == 3 ~ 1)

HRS2018_data$smokes_ever_bin = case_when(HRS2018_data$smokes_ever == 1 ~ 1, 
                                             HRS2018_data$smokes_ever == 5 ~ 0,
                                             HRS2018_data$smokes_ever == 3 ~ 1)



HRS2008_data$smokes_now_bin = case_when(HRS2008_data$smokes_now == 1 ~ 1, 
                                         HRS2008_data$smokes_now == 5 ~ 0,
                                         HRS2008_data$smokes_now == 3 ~ 1)


HRS2010_data$smokes_now_bin = case_when(HRS2010_data$smokes_now == 1 ~ 1, 
                                         HRS2010_data$smokes_now == 5 ~ 0,
                                         HRS2010_data$smokes_now == 3 ~ 1)


HRS2012_data$smokes_now_bin = case_when(HRS2012_data$smokes_now == 1 ~ 1, 
                                         HRS2012_data$smokes_now == 5 ~ 0,
                                         HRS2012_data$smokes_now == 3 ~ 1)


HRS2014_data$smokes_now_bin = case_when(HRS2014_data$smokes_now == 1 ~ 1, 
                                         HRS2014_data$smokes_now == 5 ~ 0,
                                         HRS2014_data$smokes_now == 3 ~ 1)

HRS2016_data$smokes_now_bin = case_when(HRS2016_data$smokes_now == 1 ~ 1, 
                                         HRS2016_data$smokes_now == 5 ~ 0,
                                         HRS2016_data$smokes_now == 3 ~ 1)

HRS2018_data$smokes_now_bin = case_when(HRS2018_data$smokes_now == 1 ~ 1, 
                                         HRS2018_data$smokes_now == 5 ~ 0,
                                         HRS2018_data$smokes_now == 3 ~ 1)



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



write.csv(HRS2008_data, paste(SOURCE_ROOT, "HRS_2008_data/HRS2008_data_discrimination_dataset_april_2022.csv", sep=""))
write.csv(HRS2010_data, paste(SOURCE_ROOT, "HRS_2010_data/HRS2010_data_discrimination_dataset_april_2022.csv", sep=""))
write.csv(HRS2012_data, paste(SOURCE_ROOT, "HRS_2012_data/HRS2012_data_discrimination_dataset_april_2022.csv", sep=""))
write.csv(HRS2014_data, paste(SOURCE_ROOT, "HRS_2014_data/HRS2014_data_discrimination_dataset_april_2022.csv", sep=""))
write.csv(HRS2016_data, paste(SOURCE_ROOT, "HRS_2016_data/HRS2016_data_discrimination_dataset_april_2022.csv", sep=""))
write.csv(HRS2018_data, paste(SOURCE_ROOT, "HRS_2018_data/HRS2018_data_discrimination_dataset_april_2022.csv", sep=""))