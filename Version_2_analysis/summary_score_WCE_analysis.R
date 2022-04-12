
#all values have to be numeric for this analysis 


summary_score_WCE_analysis = function(data_WCE, exposure, outcome, covariates_list, Model_name, subset_name){
  
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
  print(start_new_check) 
  
  

  stop_new_check = table(by(data_WCE$stop_new,  data_WCE$HHIDPN, min)) 
  print(stop_new_check) 
  
#check how may people were in each wave 
table(data_WCE$start_new)

#check how many people took part in multiple waves 
n_timepoints_list = unique(data_WCE$timepoints_indiv)
data_WCE$n_timepoints_max = max(n_timepoints_list)

print(n_timepoints_list)

#take the maximum number of time points value to be specified for the cut off point in WCE analysis below
n_timepoints_max = max(data_WCE$n_timepoints_max)
print("maximu number of points")
print(n_timepoints_max)

n_timepoints_max = as.numeric(n_timepoints_max)

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
wce
summary(wce)

coef.WCE(wce)

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
#output: 
numb_new_events = wce$nevents
analysed_n =  start_new_check 
n_timepoints_max = max(n_timepoints_list)
median_timepoints = median(n_timepoints_list)
BIC_information_criterion = wce$info.criterion


results = cbind(analysed_n, numb_new_events,n_timepoints_max, median_timepoints, BIC_information_criterion, hazard_ratio_1vs0)
colnames(results) = c("analysed n", "diabetes onset (n)", "max. timepoints", "median timepoint", "BIC", "hazard ratio")

#changes with number of waves: 
people_timepoints_n = table(data_WCE$timepoints_indiv)
analysed_each_wave_n = table(data_WCE$start_new)
WCEmat = wce$WCEmat

wave_info = rbind(people_timepoints_n, 
                  analysed_each_wave_n, 
                  WCEmat)

address_wave_info = paste(OUTPUT_ROOT, subset_name, Model_name, "wave_info.csv", sep = "")
print(address_wave_info)

write.csv(wave_info, paste(OUTPUT_ROOT, subset_name, Model_name, "wave_info.csv", sep=""))

#changes with number of covariates: 
wce$beta.hat.covariates
wce$se.covariates

covar_coef_se = rbind(wce$beta.hat.covariates, wce$se.covariates)

write.csv(covar_coef_se, paste(OUTPUT_ROOT, subset_name, Model_name, "covar_coef_se.csv", sep=""))


return(results)
}
