# line to standardize specific columns provided in a concatenated list.
# for (covariate in covariates_list){sample_df[covariate] = scale(sample_df[covariate])}

summary_score_Bootstrapped_CI = function (data_wce_subset,
                                          
                                          Model_n, 
                                      
                                          subset_name,
                                          Model_name,
                                          
                                          outcome, 
                                          exposure){
  
  covariates_list = Model_n 
  
  print(paste("covariates_list", covariates_list, sep=": "))
  
  print(paste("nrow(dataset) before dropping nas", nrow(data_wce_subset), sep=" = "))
  
  data_wce_subset = data_wce_subset %>% dplyr::select(HHIDPN, all_of(covariates_list), outcome, exposure, start_new, stop_new, timepoints_indiv) 
  data_wce_subset = data_wce_subset %>% drop_na("HHIDPN", all_of(covariates_list), outcome, exposure, "start_new", "stop_new")
  
  data_wce_subset = data_wce_subset %>% drop_na(all_of(covariates_list))
  
  print(paste("nrow(dataset) after dropping nas", nrow(data_wce_subset), sep=" = "))
  
  #bootstraps_samples should be between 300 and 100, the more the better but runs slower. to test the analysis I will set it to 5 for now. 
  
  bootstraps_samples = 100
  Num_time_points = 3
  #Prepare vectors to extract estimated weight function and (if relevant) HRs for each bootstrap resample: 
  
  boot.WCE <- matrix(NA, ncol = Num_time_points, nrow = bootstraps_samples) # to store estimated weight functions 
  boot.HR <- rep(NA, bootstraps_samples)
  #boot.HR_2vs6 <- rep(NA, bootstraps_samples)
  #boot.HR_3vs6 <- rep(NA, bootstraps_samples)
  #boot.HR_4vs6 <- rep(NA, bootstraps_samples)
  #boot.HR_5vs6 <- rep(NA, bootstraps_samples)
  
  #Sample IDs with replacement:
  ID <- unique(data_wce_subset$HHIDPN) 
  
  for (i in 1:bootstraps_samples){ 
    
    print(paste("i", i, sep=": "))
    ID.resamp <- sort(sample(ID, size = 1000, replace=TRUE))
    
    sample_df <- data_wce_subset[data_wce_subset$HHIDPN %in% ID.resamp,]  # select obs. but duplicated Id are ignored

    # deal with duplicated HHIDPN and assign them new HHIDPN 
    step <- 1
    repeat {
      # select duplicated HHIDPN in ID.resamp
      ID.resamp <- ID.resamp[duplicated(ID.resamp)==TRUE]
      if (length(ID.resamp)==0) break # stop when no more duplicated HHIDPN to deal with
      # select obs. but remaining duplicated HHIDPN are ignored
      subset.dup <- data_wce_subset[data_wce_subset$HHIDPN %in% ID.resamp,]
      # assign new HHIDPN to duplicates
      subset.dup$HHIDPN <- subset.dup$HHIDPN + step * 10^ceiling(log10(max(data_wce_subset$HHIDPN)))
      # 10^ceiling(log10(max(data_wce_subset$HHIDPN)) is the power of 10
      #above the maximum HHIDPN from original data
      sample_df <- rbind(sample_df, subset.dup)
      step <- step+1
    }
    
    num_indiv_points_sample_df =  max(sample_df$timepoints_indiv)
    
    print(paste("Number of rows in sample_df", nrow(sample_df) , sep=" = "))
    
    
    # The following line standardizes specific columns provided in a concatenated list (i.e. covariates_list).
    for (covariate in covariates_list){sample_df[covariate] = scale(sample_df[covariate])}
    
    #sample_df[outcome] = scale(sample_df[outcome])
    #sample_df[exposure] = scale(sample_df[exposure])

    print("About to call WCE.")
    mod <- WCE(data = sample_df,
        analysis = "Cox", nknots = 1, cutoff = Num_time_points,
        constrained = "R", aic = FALSE,
        id = "HHIDPN",
        event = outcome,
        start = "start_new",
        stop = "stop_new",
        expos = exposure,
        covariates = all_of(covariates_list))
    
    print("About to print summary(mod): ")
    print(summary(mod))
    print("done printing summary")
    
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
    
    boot.HR[i] <- HR.WCE(mod, rep(1, Num_time_points), rep(0, Num_time_points))
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
  
  boot_HR_value = quantile(boot.HR, probs=0.5) 
  print("boot.HR_value = ")
  print(boot_HR_value)
  
  HR_CI1vs0_lower =  quantile(boot.HR, probs=0.05) 
  print("HR_CI1vs0_lower = ")
  print(HR_CI1vs0_lower)
  
  #HR_CI1vs6_lower =  quantile(boot.HR_1vs6, p = 0.05) 
  #HR_CI2vs6_lower =  quantile(boot.HR_2vs6, p = 0.05) 
  #HR_CI3vs6_lower =  quantile(boot.HR_3vs6, p = 0.05) 
  #HR_CI4vs6_lower =  quantile(boot.HR_4vs6, p = 0.05) 
  #HR_CI5vs6_lower =  quantile(boot.HR_5vs6, p = 0.05) 
  
  
  HR_CI1vs0_upper =  quantile(boot.HR, p  = 0.95) 
  print("HR_CI1vs0_upper= ")
  print(HR_CI1vs0_upper)
  
  
  #HR_CI1vs6_upper =  quantile(boot.HR_1vs6, p  = 0.95) 
  #HR_CI2vs6_upper =  quantile(boot.HR_2vs6, p  = 0.95)  
  #HR_CI3vs6_upper =  quantile(boot.HR_3vs6, p  = 0.95) 
  #HR_CI4vs6_upper =  quantile(boot.HR_4vs6, p  = 0.95) 
  #HR_CI5vs6_upper =  quantile(boot.HR_5vs6, p  = 0.95) 
  
  
  print("about to rbind")
  
  HR_CIs_lower = rbind(HR_CI1vs0_lower)
  HR_CIs_upper = rbind(HR_CI1vs0_upper)
  HR_CIs_all = cbind(HR_CIs_lower, 
                     HR_CIs_upper)
  
  
  
  # check that the minumum start of time point 0 and min for stop is 1
  start_new_check = table(by(data_wce_subset$start_new,  data_wce_subset$HHIDPN, min)) 
  print(start_new_check)
  
  #######################
  
  numb_new_events = mod$nevents
  print(paste("numb_new_events ", numb_new_events , sep=" = "))
  print("test 5")
  
  analysed_n =  start_new_check 
  print(paste("analysed_n ", analysed_n , sep=" = "))
  print("test 6")
  
  n_timepoints_list = unique(data_wce_subset$timepoints_indiv)
  
  n_timepoints_max = max(n_timepoints_list)
  print(paste("n_timepoints_max ", n_timepoints_max , sep=" = "))
  
  
  median_timepoints = median(n_timepoints_list)
  print(paste("median_timepoints ", median_timepoints , sep=" = "))
  
  BIC_information_criterion = mod$info.criterion
  print(paste("BIC_information_criterion ", BIC_information_criterion , sep=" = "))
  
  
  print("test 7")
  
  results = cbind(analysed_n, 
                  numb_new_events, 
                  n_timepoints_max, 
                  median_timepoints,
                  BIC_information_criterion,
                  boot_HR_value, 
                  HR_CIs_all)
  

  print("test 8")
  
  results = data.frame(results)
  print(typeof(results)) 
  
  print("test 9")
  
  colnames(results) = c("analysed n",
                        "diabetes onset (n)",
                        "max. timepoints",
                        "median timepoint",
                        "BIC",             
                        "hazard ratio", 
                        "95 % CI")
  

  print("result = ")
  print(results)
  
  print("done with this function!")
  return(results) 
}
