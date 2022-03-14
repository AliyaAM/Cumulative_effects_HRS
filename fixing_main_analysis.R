

library(WCE)
library(survival)
library(dplyr)
library(car)
library(tidyverse)
library(tidyr)

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



SOURCE_ROOT =  "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/"
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/"



  data_wce_BMI$diabetes_new_bin = as.numeric(data_wce_BMI$diabetes_new_bin)
  data_wce_BMI$start_new = as.numeric(data_wce_BMI$start_new)
  data_wce_BMI$stop_new = as.numeric(data_wce_BMI$stop_new)
  
  data_wce_BMI$summary_mean_score_discrim = as.numeric(data_wce_BMI$summary_mean_score_discrim)
  
  data_wce_BMI$discrim_harassed = as.numeric(data_wce_BMI$discrim_harassed)
  data_wce_BMI$discrim_lessrespect = as.numeric(data_wce_BMI$discrim_lessrespect)
  data_wce_BMI$discrim_medical = as.numeric(data_wce_BMI$discrim_medical)
  data_wce_BMI$discrim_notclever = as.numeric(data_wce_BMI$discrim_notclever)
  data_wce_BMI$discrim_poorerservice = as.numeric(data_wce_BMI$discrim_poorerservice)
  data_wce_BMI$discrim_afraidothers = as.numeric(data_wce_BMI$discrim_afraidothers)
  

  data_wce_BMI$wealth_noIRA = as.numeric(data_wce_BMI$wealth_noIRA)
  data_wce_BMI$assessed_BMI = as.numeric(data_wce_BMI$assessed_BMI)
  data_wce_BMI$continious_age = as.numeric(data_wce_BMI$continious_age)
  data_wce_BMI$timepoints_indiv = as.numeric(data_wce_BMI$timepoints_indiv)
  
  
  checkWCE(data_wce_BMI,
          id = "HHIDPN", 
          event = "diabetes_new_bin", 
         start = "start_new",
          stop = "stop_new",
         expos = "summary_mean_score_discrim") 
  
  # check that the minumum start of time point 0 and min for stop is 1
  #table(by(data_wce_BMI$start_new,  data_wce_BMI$HHIDPN, min)) 
  #table(by(data_wce_BMI$start,  data_wce_BMI$HHIDPN, min)) 
  #table(by(data_wce_BMI$stop,  data_wce_BMI$HHIDPN, min)) 
  #table(by(data_wce_BMI$stop_new,  data_wce_BMI$HHIDPN, min)) 
  
  #check how may people were in each wave 
  table(data_wce_BMI$start_new)
  
  #check how many people took part in multiple waves 
  n_timepoints_list = unique(data_wce_BMI$timepoints_indiv)
  data_wce_BMI$n_timepoints_max = max(n_timepoints_list)
  
  #take the maximum number of time points value to be specified for the cut off point in WCE analysis below
  n_timepoints_max = max(data_wce_BMI$n_timepoints_max)
  
  
  
  wce =  WCE(data = data_wce_BMI,
             analysis = "Cox", 
             nknots = 1:3, cutoff = n_timepoints_max, 
             constrained = "R", aic = FALSE, MatchedSet = NULL, 
             id = "HHIDPN", 
             event = "diabetes_new_bin", 
             start = "start_new", 
             stop = "stop_new", 
             expos = "summary_mean_score_discrim",
             covariates = c("continious_age"))
  wce
  summary(wce)
  
  coef.WCE(wce)
  
  mean = mean(data_wce_BMI$summary_mean_score_discrim)
  max = max(data_wce_BMI$summary_mean_score_discrim)
  min = min(data_wce_BMI$summary_mean_score_discrim)
  


  
  
  scenario1 <- rep(2, n_timepoints_max)
  scenario2 <- rep(1, n_timepoints_max) # for all models 
  HR_value_1vs2 = HR.WCE(wce, vecnum = scenario1, vecdenom = scenario2, allres = TRUE)
  hazard_ratio_1 = HR_value_1vs2[1]
  
  scenario1 <- rep(3, n_timepoints_max)
  scenario3 <- rep(1, n_timepoints_max) # for all models 
  HR_value_1vs3 = HR.WCE(wce, vecnum = scenario1, vecdenom = scenario3, allres = TRUE)
  hazard_ratio_2  = HR_value_1vs3[1]
  
  
  scenario1 <- rep(4, n_timepoints_max)
  scenario3 <- rep(1, n_timepoints_max) # for all models 
  HR_value_1vs3 = HR.WCE(wce, vecnum = scenario1, vecdenom = scenario3, allres = TRUE)
  hazard_ratio_3  = HR_value_1vs3[1]
  
  
  
  
  scenario1 <- rep(5, n_timepoints_max)
  scenario3 <- rep(1, n_timepoints_max) # for all models 
  HR_value_1vs3 = HR.WCE(wce, vecnum = scenario1, vecdenom = scenario3, allres = TRUE)
  hazard_ratio_4 = HR_value_1vs3[1]
  
  
  scenario1 <- rep(6, n_timepoints_max)
  scenario3 <- rep(1, n_timepoints_max) # for all models 
  HR_value_1vs3 = HR.WCE(wce, vecnum = scenario1, vecdenom = scenario3, allres = TRUE)
  hazard_ratio_5  = HR_value_1vs3[1]
  
  
  
  mat_t1_value = wce$WCEmat[1,1]
  mat_t2_value = wce$WCEmat[1,2]
  mat_t3_value = wce$WCEmat[1,3]
  
  loglik_value = wce$loglik[1]
  
  info_criterion_value = wce$info.criterion[1]
  
  est_value_all = wce$est
  est_value_D1 = est_value_all$`1 knot(s)`[1]
  est_value_D2 = est_value_all$`1 knot(s)`[2]
  est_value_D3 = est_value_all$`1 knot(s)`[3]
  est_value_D4 = est_value_all$`2 knot(s)`[4]
  est_value_D5 = est_value_all$`3 knot(s)`[5]
  
  
  
  
  results_HR_WCE = rbind(hazard_ratio_1,
                         hazard_ratio_2,
                         hazard_ratio_3, 
                         hazard_ratio_4, 
                         hazard_ratio_5) 
  
  results_stats_WCE= cbind(mat_t1_value, 
                           mat_t2_value,
                           mat_t3_value,
                           
                           loglik_value,
                           
                           info_criterion_value,
                           
                           est_value_D1, 
                           est_value_D2, 
                           est_value_D3, 
                           est_value_D4, 
                           est_value_D5)
  
  
