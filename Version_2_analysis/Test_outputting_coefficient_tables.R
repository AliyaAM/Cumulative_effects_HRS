


#library(here)

#setwd('~/proj/Cumulative_effects_HRS/')

#here::i_am()
#current_directory = here()
#print(current_directory)
#print(file.path())

library(WCE)
library(survival)
library(dplyr)
library(car)
library(tidyverse)
library(tidyr)

library(epiDisplay) #tab1 function to make a frequency table 
library(foreign)
library(rms) # Used to extract p_value from logistic model
library(ggplot2) #plots 
library(corrplot)
library(gridExtra) 
library(sjPlot)
library(knitr)
library(lme4)
library(lattice)
library(Hmisc)



# plots: 
# https://adibender.github.io/pammtools/articles/cumulative_effects.html
#https://rpkgs.datanovia.com/ggpubr/reference/stat_regline_equation.html Adding text (coefficients etc) to the plot next to the regression line 



current_directory = "/Users/aliyaamirova/proj/Cumulative_effects_HRS"

OUTPUT_ROOT =(paste(current_directory, "/data_files/Results/", sep=""))
SOURCE_ROOT = (paste(current_directory, "/Version_2_analysis/", sep=""))
DATAIN_ROOT = (paste(current_directory, "/data_files/", sep="")) 


# function that subsets and srts dataset for a particular var (eg., combo == 1)

#/Users/aliyaamirova/proj/Cumulative_effects_HRS/Version_2_analysis
source((paste(SOURCE_ROOT, "subset_func.R", sep="")))

source((paste(SOURCE_ROOT, "clean_recode_keyvars.R", sep="")))

#function that sorts out the data into a dataframe where each participant x wave pair is one row. Here I also add starting and stopping points to identify each wave
source((paste(SOURCE_ROOT, "sort_timepoints.R", sep="")))

source((paste(SOURCE_ROOT, "clean_recode_sort.R", sep="")))

#function that runs WCE analysis
source((paste(SOURCE_ROOT, "summary_score_WCE_analysis.R", sep="")))
# function that samples bootstrapped CIs
source((paste(SOURCE_ROOT, "summary_score_Bootstrapped_CI.R", sep="")))
# function that runs WCE analysis and CIs sampling for a specified subset and with a specified model 
source((paste(SOURCE_ROOT, "HRs_CIs_analysis.R", sep="")))

source((paste(SOURCE_ROOT, "Seven_models.R", sep="")))


#data 
HRS2008_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2008_data_short.csv", sep=""))
HRS2010_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2010_data_short.csv", sep=""))
HRS2012_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2012_data_short.csv", sep=""))
HRS2014_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2014_data_short.csv", sep=""))
HRS2016_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2016_data_short.csv", sep=""))
HRS2018_data_initial = read.csv(paste(DATAIN_ROOT, "HRS2018_data_short.csv", sep=""))

########

#data 

HRS2008_data_intermediate = HRS2008_data_initial
HRS2010_data_intermediate = HRS2010_data_initial
HRS2012_data_intermediate = HRS2012_data_initial
HRS2014_data_intermediate = HRS2014_data_initial
HRS2016_data_intermediate = HRS2016_data_initial
HRS2018_data_intermediate = HRS2018_data_initial



HRS2008_data = HRS2008_data_intermediate
HRS2010_data = HRS2010_data_intermediate
HRS2012_data = HRS2012_data_intermediate
HRS2014_data = HRS2014_data_intermediate
HRS2016_data = HRS2016_data_intermediate
HRS2018_data = HRS2018_data_intermediate


exposure = "discrim_bin"
outcome = "diabetes_new"

Model_n=  c("continious_age", "wealth_noIRA", "sex_1_2")


dataset = clean_recode_sort(
  
  subset_var1 = "race_white", 
  subset_value1 = 0, 
  
  subset_BMI = "NA", 
  subset_BMI_value  = "NA", 
  
  subset_var2 = "religion_bin", 
  subset_value2 = 1,  
  
  subset_var3= "national_origin_ousideUS_bin", 
  subset_value3 = 1, 
  
  HRS2008_data = HRS2008_data, 
  HRS2010_data = HRS2010_data, 
  HRS2012_data = HRS2012_data, 
  HRS2014_data = HRS2014_data, 
  HRS2016_data = HRS2016_data, 
  HRS2018_data = HRS2018_data)




dataset_noNAs = dataset %>% drop_na(diabetes_new)
unique(dataset_noNAs$diabetes_new)

dataset_noNAs = dataset_noNAs %>% drop_na(exposure)
unique(dataset_noNAs$exposure)



dataset_noNAs_timepoints = sort_timepoints(data = dataset_noNAs)




#Model_1_exposure = HRs_CIs_analysis(data_wce_subset = dataset_noNAs_timepoints, 
#                                    Model_n = Model_1, 
#                                    
#                                    exposure = exposure, 
#                                    outcome = outcome,
                                    
#                                    Model_name = "Model_1")

#####




#all values have to be numeric for this analysis 
data_WCE = dataset_noNAs_timepoints
covariates_list = Model_n

  #exposure = as.numeric(exposure)
  #outcome = as.numeric(outcome)
  
  checkWCE(data_WCE, 
           id = "HHIDPN", 
           event = outcome, 
           start = "start_new",
           stop = "stop_new",
           expos = exposure) 
  
  # check that the minumum start of time point 0 and min for stop is 1
  start_new_check = table(by(data_WCE$start_new,  data_WCE$HHIDPN, min)) 

  
  stop_new_check = table(by(data_WCE$stop_new,  data_WCE$HHIDPN, min)) 

  #check how may people were in each wave 
  
  
  
  #check how many people took part in multiple waves 
  
  n_timepoints_list = unique(data_WCE$timepoints_indiv)
 
  
  
  #take the maximum number of time points value to be specified for the cut off point in WCE analysis below
 
   
  wce =  WCE(data = data_WCE,
             analysis = "Cox", 
             nknots = 1, cutoff = n_timepoints_max, 
             constrained = "R", aic = FALSE, MatchedSet = NULL, 
             id = "HHIDPN", 
             event = outcome, 
             start = "start_new", 
             stop = "stop_new", 
             expos = exposure,
             covariates = covariates_list)
  
  
  summary(wce)
 
 
  wce
  
  coef.WCE(wce)
  wce$est
  wce$knotsmat
  wce$loglik
  wce$vcovmat
  wce$SED
  
  

  #mean = mean(data_WCE$summary_mean_score_discrim)
  #max = max(data_WCE$summary_mean_score_discrim)
  #min = min(data_WCE$summary_mean_score_discrim)
  
  #lower_quantile = quantile(data_WCE$summary_mean_score_discrim, p = 0.25)
  
  #upper_quantile = quantile(data_WCE$summary_mean_score_discrim, p = 0.75)
  
  #median = quantile(data_WCE$summary_mean_score_discrim, p = 0.5)
  
  
  #scenario1 <- rep(1, n_timepoints_max)
  #scenario2 <- rep(0, n_timepoints_max) # for all models 
  
  #we created a binary variable to indicate whether participants had experienced discrimination in the past year 
  #(a few times or more a year vs. less than once a year or never)
  
  #1 = almost everyday
  #2 = at least once a week
  #3 = a few times a month
  #4 = a few times a year
  
  #5 = less than once a year
  #6 = never 
  
  
  scenario1 <- c(rep(1, n_timepoints_max))
  scenario2 <- c(rep(0, n_timepoints_max))
  
  HR_value_1vs0 = HR.WCE(wce, vecnum = scenario1, vecdenom = scenario2, allres = TRUE)
  
  print("hazard ratio within summary WCE analysis:")
  print(HR_value_1vs0)
  
  hazard_ratio_1vs0 = HR_value_1vs0[1]
  print("hazard ratio within summary WCE analysis, first value:")
  print(hazard_ratio_1vs0)
  
  #scenario_lower_quantile <- rep(lower_quantile, n_timepoints_max)
  #scenario_upper_quantile <- rep(upper_quantile, n_timepoints_max) # for all models 
  #HR_value_quantiles = HR.WCE(wce, vecnum = scenario_upper_quantile, vecdenom = scenario_lower_quantile, allres = TRUE)
  #hazard_ratio_quantiles  = HR_value_quantiles[1]
  
  
  #scenario1 <- rep(2, n_timepoints_max)
  #scenario2 <- rep(1, n_timepoints_max) # for all models 
  #HR_value_1vs2 = HR.WCE(wce, vecnum = scenario1, vecdenom = scenario2, allres = TRUE)
  #hazard_ratio_1 = HR_value_1vs2[1]
  
  #scenario1 <- rep(3, n_timepoints_max)
  #scenario3 <- rep(1, n_timepoints_max) # for all models 
  #HR_value_1vs3 = HR.WCE(wce, vecnum = scenario1, vecdenom = scenario3, allres = TRUE)
  #hazard_ratio_2  = HR_value_1vs3[1]
  
  
  #scenario1 <- rep(4, n_timepoints_max)
  #scenario3 <- rep(1, n_timepoints_max) # for all models 
  #HR_value_1vs3 = HR.WCE(wce, vecnum = scenario1, vecdenom = scenario3, allres = TRUE)
  #hazard_ratio_3  = HR_value_1vs3[1]
  
  
  
  
  #scenario1 <- rep(5, n_timepoints_max)
  #scenario3 <- rep(1, n_timepoints_max) # for all models 
  #HR_value_1vs3 = HR.WCE(wce, vecnum = scenario1, vecdenom = scenario3, allres = TRUE)
  #hazard_ratio_4 = HR_value_1vs3[1]
  
  
  #scenario1 <- rep(6, n_timepoints_max)
  #scenario3 <- rep(1, n_timepoints_max) # for all models 
  #HR_value_1vs3 = HR.WCE(wce, vecnum = scenario1, vecdenom = scenario3, allres = TRUE)
  #hazard_ratio_5  = HR_value_1vs3[1]
  
  
  
  mat_t1_value = wce$WCEmat[1,1]
  mat_t2_value = wce$WCEmat[1,2]
  mat_t3_value = wce$WCEmat[1,3]
  
  #loglik_value = wce$loglik[1]
  
  info_criterion_value = wce$info.criterion[1]
  
  est_value_all = wce$est
  est_value_D1 = est_value_all$`1 knot(s)`[1]
  est_value_D2 = est_value_all$`1 knot(s)`[2]
  est_value_D3 = est_value_all$`1 knot(s)`[3]
  est_value_D4 = est_value_all$`2 knot(s)`[4]
  est_value_D5 = est_value_all$`3 knot(s)`[5]
  
  
  
  
  results_HR_WCE = rbind(hazard_ratio_1vs0) 
  
  results_stats_WCE= cbind(mat_t1_value, 
                           mat_t2_value,
                           mat_t3_value,
                           
                           # loglik_value,
                           
                           info_criterion_value,
                           
                           est_value_D1, 
                           est_value_D2, 
                           est_value_D3, 
                           est_value_D4, 
                           est_value_D5)
  
  



#####
#######################
#######################

subset__subset = summary_score_WCE_analysis(data_WCE = dataset_noNAs_timepoints,
                                            exposure = exposure, 
                                            outcome = outcome, 
                                            covariates_list = Model_n)


subset__HR_subset = subset__subset[1]

subset__subset_stats_recoded = subset__subset[2]



######## bootstrapped CIs for the HRs from the above model 
subset__age_CI_subset  = summary_score_Bootstrapped_CI(WCE_data_CI = dataset_noNAs_timepoints,
                                                       exposure = exposure, 
                                                       outcome = outcome,
                                                       covariates_list = Model_n)



#folder_1 = paste(subset_var, "/", sep = "")
#folder_2 = paste(Model_name, "/", sep = "")

#path = paste(OUTPUT_ROOT, folder_1, folder_2, sep = "")

#dir.create(paste(path, sep = ""))

#write.csv(subset__subset_stats_recoded, paste(OUTPUT_ROOT, folder_1, folder_2, "subset__subset_stats.csv", sep=""))


subset__HR_subset = unlist(subset__HR_subset)
subset_results_HR_CIs = cbind(subset__HR_subset, subset__age_CI_subset)

colnames(subset_results_HR_CIs) = c("hazard ratio", "5% CI", "95% CI")

#subset_results_subset = add total n = start_new = 0, number of cases, coefficient table for each model, BIC,
