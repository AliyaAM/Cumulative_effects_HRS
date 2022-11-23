

summary_score_Bootstrapped_CI = function (WCE_data_CI, outcome, exposure, covariates_list){
  
  
  #all values have to be numeric for this analysis 
  
  #all values have to be numeric for this analysis 
  
 #WCE_data_CI$diabetes_new_bin = as.numeric(WCE_data_CI$diabetes_new_bin)
#WCE_data_CI$checklist_depression_bin = as.numeric(WCE_data_CI$checklist_depression_bin)
  
  #WCE_data_CI$start_new = as.numeric(WCE_data_CI$start_new)
 # WCE_data_CI$stop_new = as.numeric(WCE_data_CI$stop_new)
  
 # WCE_data_CI$discrim_harassed_bin = as.numeric(WCE_data_CI$discrim_harassed_bin)
  #WCE_data_CI$discrim_lessrespect_bin = as.numeric(WCE_data_CI$discrim_lessrespect_bin)
 # WCE_data_CI$discrim_medical_bin = as.numeric(WCE_data_CI$discrim_medical_bin)
 # WCE_data_CI$discrim_notclever_bin = as.numeric(WCE_data_CI$discrim_notclever_bin)
#  WCE_data_CI$discrim_poorerservice_bin = as.numeric(WCE_data_CI$discrim_poorerservice_bin)
 # WCE_data_CI$discrim_afraidothers_bin = as.numeric(WCE_data_CI$discrim_afraidothers_bin)
  
#  WCE_data_CI$wealth_noIRA = as.numeric(WCE_data_CI$wealth_noIRA)
 # WCE_data_CI$assessed_BMI = as.numeric(WCE_data_CI$assessed_BMI)
 # WCE_data_CI$continious_age = as.numeric(WCE_data_CI$continious_age)
  
  WCE_data_CI$timepoints_indiv = as.numeric(WCE_data_CI$timepoints_indiv)
  
  
  
  #bootstraps_samples should be between 300 and 100, the more the better but runs slower. to test the analysis I will set it to 5 for now. 
  
  bootstraps_samples = 100
  Num_time_points = max(WCE_data_CI$timepoints_indiv)

  #Prepare vectors to extract estimated weight function and (if relevant) HRs for each bootstrap resample: 
  
  boot.WCE <- matrix(NA, ncol = Num_time_points, nrow = bootstraps_samples) # to store estimated weight functions 
  boot.HR <- rep(NA, bootstraps_samples)
  #boot.HR_2vs6 <- rep(NA, bootstraps_samples)
  #boot.HR_3vs6 <- rep(NA, bootstraps_samples)
  #boot.HR_4vs6 <- rep(NA, bootstraps_samples)
  #boot.HR_5vs6 <- rep(NA, bootstraps_samples)
  
  #Sample IDs with replacement:
  ID <- unique(WCE_data_CI$HHIDPN) 
  
  for (i in 1:bootstraps_samples){ 
    ID.resamp <- sort(sample(ID, size = 1000, replace=TRUE))
    
    rows_check = WCE_data_CI$HHIDPN %in% ID.resamp
    
    datab <- WCE_data_CI[WCE_data_CI$HHIDPN %in% ID.resamp,]  # select obs. but duplicated Id are ignored
   
     print(rows_check)
     
     print("rows_check above in summary_score_Bootstrapped_CI")
    
    print(datab)
    
    print("datab above in summary_score_Bootstrapped_CI")
    
    #crash 
    # deal with duplicated HHIDPN and assign them new HHIDPN 
    step <- 1 
    repeat {
      # select duplicated HHIDPN in ID.resamp 
      ID.resamp <- ID.resamp[duplicated(ID.resamp)==TRUE]
      if (length(ID.resamp)==0) break # stop when no more duplicated HHIDPN to deal with 
      # select obs. but remaining duplicated HHIDPN are ignored 
      subset.dup <- WCE_data_CI[WCE_data_CI$HHIDPN %in% ID.resamp,] 
      # assign new HHIDPN to duplicates 
      subset.dup$HHIDPN <- subset.dup$HHIDPN + step * 10^ceiling(log10(max(WCE_data_CI$HHIDPN))) 
      # 10^ceiling(log10(max(WCE_data_CI$HHIDPN)) is the power of 10 
      #above the maximum HHIDPN from original data
      datab <- rbind(datab, subset.dup) 
      step <- step+1 
    }
    
    #change 
    start_stop = c("start_new", "stop_new")
    
    
    datab %>% dplyr::select(HHIDPN, covariates_list, outcome, exposure, start_new, stop_new)
    
    datab = na.omit(datab)
    
    print(datab)
    
    Num_time_points_datab = max(datab$timepoints_indiv)
    
    #Prepare vectors to extract estimated weight function and (if relevant) HRs for each bootstrap resample: 
    
    boot.WCE <- matrix(NA, ncol = Num_time_points_datab, nrow = bootstraps_samples) # to store estimated weight functions 
    boot.HR <- rep(NA, bootstraps_samples)
    
    mod <- WCE(data = datab, 
               analysis = "Cox", nknots = 1, cutoff = Num_time_points_datab,
               constrained = "R", aic = FALSE, MatchedSet = NULL, 
               id = "HHIDPN", 
               event = outcome, 
               start = "start_new", 
               stop = "stop_new", 
               expos = exposure,
               covariates = covariates_list)
    
    
    # return best WCE estimates and corresponding HR 
    
    best <- which.min(mod$info.criterion) 
    best = as.numeric(best)
    boot.WCE[i,] <- mod$WCEmat[best,] 
   
    #boot.HR_1vs6[i] <- HR.WCE(mod, rep(2, Num_time_points), rep(1, Num_time_points)) 
   # boot.HR_2vs6[i] <- HR.WCE(mod, rep(3, Num_time_points), rep(1, Num_time_points)) 
   # boot.HR_3vs6[i] <- HR.WCE(mod, rep(4, Num_time_points), rep(1, Num_time_points)) 
    #boot.HR_4vs6[i] <- HR.WCE(mod, rep(5, Num_time_points), rep(1, Num_time_points)) 
    #boot.HR_5vs6[i] <- HR.WCE(mod, rep(6, Num_time_points), rep(1, Num_time_points)) 
    
    #scenario1 <- c(rep(1, Num_time_points))
    #scenario2 <- c(rep(0, Num_time_points))
    
    boot.HR[i] <- HR.WCE(mod, rep(1, Num_time_points_datab), rep(0, Num_time_points_datab)) 
    } 
  
  boot.HR = as.numeric(boot.HR) 
  print("boot.HR:")
  print(boot.HR)
  
  boot.WCE = as.numeric(boot.WCE)
  print("boot.WCE")
  print(boot.WCE)
  # estimated weight functions 
  #estimated_weight_functions  = apply(boot.WCE, 2, quantile, p = c(0.05, 0.95))
  

  # estimated HR 
  #quantile(as.numeric(x), probs=c(.25, .75), na.rm = TRUE)
  boot.HR = na.omit(boot.HR)
  HR_CI1vs0_lower =  quantile(boot.HR, probs=0.05) 
  
  #HR_CI1vs6_lower =  quantile(boot.HR_1vs6, p = 0.05) 
  #HR_CI2vs6_lower =  quantile(boot.HR_2vs6, p = 0.05) 
  #HR_CI3vs6_lower =  quantile(boot.HR_3vs6, p = 0.05) 
  #HR_CI4vs6_lower =  quantile(boot.HR_4vs6, p = 0.05) 
  #HR_CI5vs6_lower =  quantile(boot.HR_5vs6, p = 0.05) 
  
  
  HR_CI1vs0_upper =  quantile(boot.HR, p  = 0.95) 
  
  
  #HR_CI1vs6_upper =  quantile(boot.HR_1vs6, p  = 0.95) 
  #HR_CI2vs6_upper =  quantile(boot.HR_2vs6, p  = 0.95)  
  #HR_CI3vs6_upper =  quantile(boot.HR_3vs6, p  = 0.95) 
  #HR_CI4vs6_upper =  quantile(boot.HR_4vs6, p  = 0.95) 
  #HR_CI5vs6_upper =  quantile(boot.HR_5vs6, p  = 0.95) 
  
  
  
  HR_CIs_lower = rbind(HR_CI1vs0_lower)
  
  
  HR_CIs_upper = rbind(HR_CI1vs0_upper)
  
  HR_CIs_all = cbind(HR_CIs_lower, 
                     HR_CIs_upper)
  
  return(HR_CIs_all) 
}