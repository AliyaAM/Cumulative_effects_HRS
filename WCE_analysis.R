
WCE_analysis = function(data_WCE, exposure, outcome, covariates_list){

#event = "type in event"
#covariates_list = c("covariate name", "covariate name")
#outcome = "outcome name"
  
  
  #all values have to be numeric for this analysis 
  
  data_WCE$checklist_depression_bin = as.numeric(data_WCE$checklist_depression_bin)
  data_WCE$start_new = as.numeric(data_WCE$start_new)
  data_WCE$stop_new = as.numeric(data_WCE$stop_new)
  
  data_WCE$discrim_harassed = as.numeric(data_WCE$summary_mean_score_discrim)

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

head(data_WCE)
nrow(data_WCE)
length(unique(data_WCE$HHIDPN))

#check the unique values in the discrim variable, this are original form the HRS files. 
####### coded as: 
####### 1 Almost everyday
####### 2 At least once a week
####### 3 A few times a month
####### 4 A few times a year
####### 5 Less than once a year
####### 6 Never


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


# Example from the R library HR.WCE: 
######## Exposed at a dose of 1 (constant) vs. unexposed over the time window of 90 days
######## scenario1 <- rep(1, 90)
######## scenario2 <- rep(0, 90)
######## HR.WCE(wce, vecnum = scenario1, vecdenom = scenario2)

#producing hazard ratios of experiencing discrimination A few times a year or more (=1) to less than once a year and never (=0) on the onset of diabetes type 2. 

scenario1 <- rep(1, n_timepoints_max)
scenario2 <- rep(0, n_timepoints_max) # for all models 
HR_value_1vs0 = HR.WCE(wce, vecnum = scenario1, vecdenom = scenario2, allres = TRUE)
hazard_ratio_1vs0 = HR_value_1vs0[1]



ID <- unique(data_WCE$HHIDPN)


coef.WCE(wce)



results_HR_WCE = rbind(hazard_ratio_1vs0) 
                    
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


return(params = list(results_HR_WCE, results_stats_WCE))
}



