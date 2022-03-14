
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

SOURCE_data_ROOT = "/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"
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
#HRS2018_BMI_hispanic_latino

#HRS2018_BMI_white
#HRS2018_BMI_black
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

HRS2008_data = read.csv(paste(SOURCE_data_ROOT, "HRS_2008_data/HRS2008_data_short.csv", sep=""))
HRS2010_data = read.csv(paste(SOURCE_data_ROOT, "HRS_2010_data/HRS2010_data_short.csv", sep=""))
HRS2012_data = read.csv(paste(SOURCE_data_ROOT, "HRS_2012_data/HRS2012_data_short.csv", sep=""))
HRS2014_data = read.csv(paste(SOURCE_data_ROOT, "HRS_2014_data/HRS2014_data_short.csv", sep=""))
HRS2016_data = read.csv(paste(SOURCE_data_ROOT, "HRS_2016_data/HRS2016_data_short.csv", sep=""))
HRS2018_data = read.csv(paste(SOURCE_data_ROOT, "HRS_2018_data/HRS2018_data_short.csv", sep=""))


#assessed_BMI_2008
HRS2008_data_BMI = subset(HRS2008_data, HRS2008_data$assessed_BMI > 30) 
HRS2010_data_BMI = subset(HRS2010_data, HRS2010_data$assessed_BMI > 30)
HRS2012_data_BMI = subset(HRS2012_data, HRS2012_data$assessed_BMI > 30)
HRS2014_data_BMI = subset(HRS2014_data, HRS2014_data$assessed_BMI > 30)
HRS2016_data_BMI = subset(HRS2016_data, HRS2016_data$assessed_BMI > 30)
HRS2018_data_BMI = subset(HRS2018_data, HRS2018_data$assessed_BMI > 30)




HRS2018_data_BMI = na.omit(HRS2018_data_BMI)
HRS2016_data_BMI = na.omit(HRS2016_data_BMI)
HRS2014_data_BMI = na.omit(HRS2014_data_BMI)
HRS2012_data_BMI = na.omit(HRS2012_data_BMI)
HRS2010_data_BMI = na.omit(HRS2010_data_BMI)
HRS2008_data_BMI = na.omit(HRS2008_data_BMI)


WCE_dataset_BMI = rbind(HRS2008_data_BMI, 
                        HRS2010_data_BMI,
                        HRS2012_data_BMI, 
                        HRS2014_data_BMI, 
                        HRS2016_data_BMI, 
                        HRS2018_data_BMI)


WCE_dataset_BMI$diabetes_new_bin = case_when(WCE_dataset_BMI$diabetes_new ==0 ~ 0, 
                                              WCE_dataset_BMI$diabetes_new ==1 ~ 1) 



WCE_dataset_BMI = subset(WCE_dataset_BMI, HHIDPN != "3020")

WCE_dataset_BMI = subset(WCE_dataset_BMI , summary_mean_score_discrim != " NA")
unique(WCE_dataset_BMI$summary_mean_score_discrim)

WCE_dataset_BMI = subset(WCE_dataset_BMI , discrim_harassed != " NA")
unique(WCE_dataset_BMI$discrim_harassed)


WCE_dataset_BMI = subset(WCE_dataset_BMI , discrim_lessrespect != " NA")
unique(WCE_dataset_BMI$discrim_lessrespect)

WCE_dataset_BMI = subset(WCE_dataset_BMI , discrim_medical != " NA")
unique(WCE_dataset_BMI$discrim_medical)

WCE_dataset_BMI = subset(WCE_dataset_BMI , discrim_notclever != " NA")
unique(WCE_dataset_BMI$discrim_notclever)

WCE_dataset_BMI = subset(WCE_dataset_BMI , discrim_poorerservice != " NA")
unique(WCE_dataset_BMI$discrim_poorerservice)


WCE_dataset_BMI = subset(WCE_dataset_BMI , discrim_afraidothers != " NA")
unique(WCE_dataset_BMI$discrim_afraidothers)

write.csv(WCE_dataset_BMI, paste(SOURCE_data_ROOT, "WCE_dataset_BMI.csv", sep=""))

