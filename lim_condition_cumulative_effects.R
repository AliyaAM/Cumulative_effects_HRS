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

WCE_dataset_lim_cond = read.csv(paste(SOURCE_ROOT, "WCE_dataset_lim_cond.csv", sep=""))

WCE_dataset_lim_cond = WCE_dataset_lim_cond %>% drop_na()


nrow(WCE_dataset_lim_cond)
head(WCE_dataset_lim_cond)

ID = unique(WCE_dataset_lim_cond$HHIDPN)

#print(isTRUE(WCE_dataset_lim_cond$HHIDPN == ID[1]))

participant_wave_df = data.frame()

n = 1
for (id in ID){
  print(n)
  print(id)
participant_wave = subset(WCE_dataset_lim_cond, WCE_dataset_lim_cond$HHIDPN == id)

if (nrow(participant_wave)== 1){
  
  participant_wave$start_new = c(0)
  participant_wave$stop_new = c(1)
  
  participant_wave_df = rbind(participant_wave_df, participant_wave) 
  
}

if (nrow(participant_wave) ==2){
 

  participant_wave$start_new = c(0, 1)
  participant_wave$stop_new = c(1, 2)
  participant_wave_df = rbind(participant_wave_df, participant_wave) 
  
   }
 
if (nrow(participant_wave)==3){

  
  participant_wave$start_new = c(0, 1, 2)
 participant_wave$stop_new = c(1, 2, 3)
 participant_wave_df = rbind(participant_wave_df, participant_wave) 
  
  } 


if (nrow(participant_wave)==4){
  
  participant_wave$start_new = c(0, 1, 2, 3)
  participant_wave$stop_new = c(1, 2, 3, 4)
  participant_wave_df = rbind(participant_wave_df, participant_wave) 
  
  } 

if (nrow(participant_wave)==5){
  
  participant_wave$start_new = c(0, 1, 2, 3, 4)
  participant_wave$stop_new = c(1, 2, 3, 4, 5)
  participant_wave_df = rbind(participant_wave_df, participant_wave) 
  
  } 

if (nrow(participant_wave)==6){
  
  participant_wave$start_new = c(0, 1, 2, 3, 4, 5)
  participant_wave$stop_new = c(1, 2, 3, 4, 5, 6)
  participant_wave_df = rbind(participant_wave_df, participant_wave) 
  
}
n = n + 1
}


participant_wave_df$diabetes_new_bin = as.numeric(participant_wave_df$diabetes_new_bin)
participant_wave_df$start_new = as.numeric(participant_wave_df$start_new)
participant_wave_df$stop_new = as.numeric(participant_wave_df$stop_new)
participant_wave_df$discrim_harassed = as.numeric(participant_wave_df$discrim_harassed)
participant_wave_df$assessed_BMI = as.numeric(participant_wave_df$assessed_BMI)
participant_wave_df$continious_age = as.numeric(participant_wave_df$continious_age)



print("is NA:")

checkWCE(participant_wave_df, id = "HHIDPN", event = "diabetes_new_bin", start = "start_new", stop = "stop_new", expos = "discrim_harassed") 

head(participant_wave_df)

nrow(participant_wave_df)

length(unique(participant_wave_df$HHIDPN))

table(by(participant_wave_df$start_new, participant_wave_df$HHIDPN, min)) 

apply(is.na(participant_wave_df)==1, 2, sum)
      
participant_wave_df$wealth_noIRA


wce_age =  WCE(data = participant_wave_df, analysis = "Cox", nknots = 1:3, cutoff = 4, constrained = "R", aic = FALSE, MatchedSet = NULL, id = "HHIDPN", event = "diabetes_new_bin", start = "start_new", stop = "stop_new", expos = "discrim_harassed", covariates = c( "continious_age"))
wce_age
summary(wce_age)

wce_age_BMI <- WCE(data = participant_wave_df, analysis = "Cox", nknots = 1:3, cutoff = 4, constrained = "R", aic = FALSE, MatchedSet = NULL, id = "HHIDPN", event = "diabetes_new_bin", start = "start_new", stop = "stop_new", expos = "discrim_harassed", covariates = c("assessed_BMI", "continious_age")) 
wce_age_BMI
summary(wce_age_BMI)

wce_BMI_age_wealth <- WCE(participant_wave_df, analysis = "Cox", nknots = 1:3, cutoff = 4, constrained = "R", aic = FALSE, MatchedSet = NULL, id = "HHIDPN", event = "diabetes_new_bin", start = "start_new", stop = "stop_new", expos = "discrim_harassed", covariates = c("assessed_BMI", "continious_age", "wealth_noIRA")) 
wce_BMI_age_wealth
summary(wce_BMI_age_wealth)

scenario1 <- rep(6, 4)
scenario2 <- rep(0, 4) # for all models 
HR.WCE(wce_age, vecnum = scenario1, vecdenom = scenario2, allres = TRUE)


scenario3 <- rep(5, 4)
scenario2 <- rep(0, 4) # for all models 
HR.WCE(wce_age, vecnum = scenario3, vecdenom = scenario2, allres = TRUE)


scenario4 <- rep(4, 4)
scenario2 <- rep(0, 4) # for all models 
HR.WCE(wce_age, vecnum = scenario4, vecdenom = scenario2, allres = TRUE)


scenario5 <- rep(3, 4)
scenario2 <- rep(0, 4) # for all models 
HR.WCE(wce_age, vecnum = scenario5, vecdenom = scenario2, allres = TRUE)



scenario6 <- rep(2, 4)
scenario2 <- rep(0, 4) # for all models 
HR.WCE(wce_age, vecnum = scenario6, vecdenom = scenario2, allres = TRUE)


scenario7 <- rep(1, 4)
scenario2 <- rep(0, 4) # for all models 
HR.WCE(wce_age, vecnum = scenario7, vecdenom = scenario2, allres = TRUE)

log (994705.1)
ID <- unique(participant_wave_df$HHIDPN)

coef.WCE(wce_age)


write.csv(participant_wave_df)

write.csv(participant_wave_df, paste(SOURCE_ROOT, "participant_wave_df_lim_cond.csv", sep=""))

