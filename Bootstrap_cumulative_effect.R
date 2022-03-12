
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

print("Add forloop for replacing the ids") 

## Set the root directory to look for source code.
#laptop: 
#/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/
#"/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"
"/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"

SOURCE_ROOT = "/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"
OUTPUT_ROOT = "/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"


#participant_wave_df = read.csv(paste(SOURCE_ROOT, "participant_wave_df_female.csv", sep=""))
#participant_wave_df = read.csv(paste(SOURCE_ROOT, "participant_wave_df_lim_cond.csv", sep=""))
#participant_wave_df = read.csv(paste(SOURCE_ROOT, "participant_wave_df_religion.csv", sep=""))
#participant_wave_df = read.csv(paste(SOURCE_ROOT, "participant_wave_df_national_origin.csv", sep=""))
#participant_wave_df = read.csv(paste(SOURCE_ROOT, "participant_wave_df_national_race.csv", sep=""))
participant_wave_df = read.csv(paste(SOURCE_ROOT, "participant_wave_df_BMI.csv", sep=""))



participant_wave_df = na.omit(participant_wave_df)
#should be between 300 and 100, the more the better but runs slower. to test the analysis I will set it to 5 for now. 

 bootstraps_samples = 100
 Num_time_points = 4
 
#Prepare vectors to extract estimated weight function and (if relevant) HRs for each bootstrap resample: 
 
 boot.WCE <- matrix(NA, ncol = Num_time_points, nrow = bootstraps_samples) # to store estimated weight functions 
 boot.HR <- rep(NA, bootstraps_samples) # to store estimated HRs
 
 #Sample IDs with replacement:
 ID <- unique(participant_wave_df$HHIDPN) 
 
 for (i in 1:bootstraps_samples){ 
   ID.resamp <- sort(sample(ID, replace=FALSE))
   datab <- participant_wave_df[participant_wave_df$HHIDPN %in% ID.resamp,]  # select obs. but duplicated Id are ignored
  

   

   mod <- WCE(data = datab, 
              analysis = "Cox", nknots = 1:3, cutoff = Num_time_points,
              constrained = "R", aic = FALSE, MatchedSet = NULL, 
              id = "HHIDPN", 
              event = "diabetes_new_bin", 
              start = "start_new", 
              stop = "stop_new", 
              expos = "discrim_afraidothers", 
              covariates = c("continious_age"))
   
   
   # return best WCE estimates and corresponding HR 
   best <- which.min(mod$info.criterion) 
   boot.WCE[i,] <- mod$WCEmat[best,] 
   boot.HR[i] <- HR.WCE(mod, rep(1, Num_time_points), rep(0, Num_time_points)) 
   
 } 
 
 
 # estimated weight functions 
 estimated_weight_functions  = apply(boot.WCE, 1, quantile, p = c(0.05, 0.95))
 
 # estimated HR 
HR_CI =  quantile(boot.HR, p = c(0.05, 0.95)) 



unique(datab$HHIDPN)
head(datab$HHIDPN)
tail(datab$HHIDPN)
datab$HHIDPN