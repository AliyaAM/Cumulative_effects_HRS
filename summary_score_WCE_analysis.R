
#all values have to be numeric for this analysis 


summary_score_WCE_analysis = function(data_WCE, exposure, outcome, covariates_list){
  
  
data_WCE$diabetes_new_bin = as.numeric(data_WCE$diabetes_new_bin)
data_WCE$start_new = as.numeric(data_WCE$start_new)
data_WCE$stop_new = as.numeric(data_WCE$stop_new)

data_WCE$summary_mean_score_discrim = as.numeric(data_WCE$summary_mean_score_discrim)

data_WCE$discrim_harassed = as.numeric(data_WCE$discrim_harassed)
data_WCE$discrim_lessrespect = as.numeric(data_WCE$discrim_lessrespect)
data_WCE$discrim_medical = as.numeric(data_WCE$discrim_medical)
data_WCE$discrim_notclever = as.numeric(data_WCE$discrim_notclever)
data_WCE$discrim_poorerservice = as.numeric(data_WCE$discrim_poorerservice)
data_WCE$discrim_afraidothers = as.numeric(data_WCE$discrim_afraidothers)

data_WCE$wealth_noIRA = as.numeric(data_WCE$wealth_noIRA)
data_WCE$assessed_BMI = as.numeric(data_WCE$assessed_BMI)
data_WCE$continious_age = as.numeric(data_WCE$continious_age)

data_WCE$timepoints_indiv = as.numeric(data_WCE$timepoints_indiv)


checkWCE(data_WCE,
         id = "HHIDPN", 
         event = outcome, 
         start = "start_new",
         stop = "stop_new",
         expos = exposure) 

# check that the minumum start of time point 0 and min for stop is 1
table(by(data_WCE$start_new,  data_WCE$HHIDPN, min)) 
table(by(data_WCE$start,  data_WCE$HHIDPN, min)) 
table(by(data_WCE$stop,  data_WCE$HHIDPN, min)) 
table(by(data_WCE$stop_new,  data_WCE$HHIDPN, min)) 

#check how may people were in each wave 
table(data_WCE$start_new)

#check how many people took part in multiple waves 
n_timepoints_list = unique(data_WCE$timepoints_indiv)
data_WCE$n_timepoints_max = max(n_timepoints_list)

#take the maximum number of time points value to be specified for the cut off point in WCE analysis below
n_timepoints_max = max(data_WCE$n_timepoints_max)



wce =  WCE(data = data_WCE,
           analysis = "Cox", 
           nknots = 1:3, cutoff = n_timepoints_max, 
           constrained = "R", aic = FALSE, MatchedSet = NULL, 
           id = "HHIDPN", 
           event = outcome, 
           start = "start_new", 
           stop = "stop_new", 
           expos = exposure,
           covariates = covariates_list)
wce
summary(wce)

coef.WCE(wce)

mean = mean(data_WCE$summary_mean_score_discrim)
max = max(data_WCE$summary_mean_score_discrim)
min = min(data_WCE$summary_mean_score_discrim)

lower_quantile = quantile(data_WCE$summary_mean_score_discrim, p = 0.25)

upper_quantile = quantile(data_WCE$summary_mean_score_discrim, p = 0.75)

median = quantile(data_WCE$summary_mean_score_discrim, p = 0.5)


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

#loglik_value = wce$loglik[1]

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
                         
                        # loglik_value,
                         
                         info_criterion_value,
                         
                         est_value_D1, 
                         est_value_D2, 
                         est_value_D3, 
                         est_value_D4, 
                         est_value_D5)


return(params = list(results_HR_WCE, results_stats_WCE))
}
