
#all values have to be numeric for this analysis 

data_wce_BMI$diabetes_new_bin = as.numeric(data_wce_BMI$diabetes_new_bin)
data_wce_BMI$start_new = as.numeric(data_wce_BMI$start_new)
data_wce_BMI$stop_new = as.numeric(data_wce_BMI$stop_new)

data_wce_BMI$discrim_harassed = as.numeric(data_wce_BMI$summary_mean_score_discrim)

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
table(by(data_wce_BMI$start_new,  data_wce_BMI$HHIDPN, min)) 
table(by(data_wce_BMI$start,  data_wce_BMI$HHIDPN, min)) 
table(by(data_wce_BMI$stop,  data_wce_BMI$HHIDPN, min)) 
table(by(data_wce_BMI$stop_new,  data_wce_BMI$HHIDPN, min)) 

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
           covariates = "continious_age")
wce
summary(wce)

coef.WCE(wce)

mean = mean(data_wce_BMI$summary_mean_score_discrim)
max = max(data_wce_BMI$summary_mean_score_discrim)
min = min(data_wce_BMI$summary_mean_score_discrim)

lower_quantile = quantile(data_wce_BMI$summary_mean_score_discrim, p = 0.25)

upper_quantile = quantile(data_wce_BMI$summary_mean_score_discrim, p = 0.75)

median = quantile(data_wce_BMI$summary_mean_score_discrim, p = 0.5)


scenario_lower_quantile <- rep(lower_quantile, n_timepoints_max)
scenario_upper_quantile <- rep(upper_quantile, n_timepoints_max) # for all models 
HR_value_quantiles = HR.WCE(wce, vecnum = scenario_upper_quantile, vecdenom = scenario_lower_quantile, allres = TRUE)
hazard_ratio_quantiles  = HR_value_quantiles[1]


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


scenario1 <- rep(4, n_timepoints_max)
scenario3 <- rep(1, n_timepoints_max) # for all models 
HR_value_1vs3 = HR.WCE(wce, vecnum = scenario1, vecdenom = scenario3, allres = TRUE)
hazard_ratio_4  = HR_value_1vs3[1]



scenario1 <- rep(5, n_timepoints_max)
scenario3 <- rep(1, n_timepoints_max) # for all models 
HR_value_1vs3 = HR.WCE(wce, vecnum = scenario1, vecdenom = scenario3, allres = TRUE)
hazard_ratio_5  = HR_value_1vs3[1]


scenario1 <- rep(6, n_timepoints_max)
scenario3 <- rep(1, n_timepoints_max) # for all models 
HR_value_1vs3 = HR.WCE(wce, vecnum = scenario1, vecdenom = scenario3, allres = TRUE)
hazard_ratio_6  = HR_value_1vs3[1]
