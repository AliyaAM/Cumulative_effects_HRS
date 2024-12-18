

# line to standardize specific columns provided in a concatenated list.
# for (covariate in covariates_list){sample_df[covariate] = scale(sample_df[covariate])}

summary_score_Bootstrapped_CI = function (WCE_data_CI, 
                                          outcome, 
                                          exposure, 
                                          covariates_list){
  
  
  print(paste("nrow(dataset) before dropping nas", nrow(WCE_data_CI), sep=" = "))
  
  WCE_data_CI = WCE_data_CI %>% dplyr::select(HHIDPN, all_of(covariates_list), outcome, exposure, start_new, stop_new, timepoints_indiv) 
  
  #vars = c("HHIDPN", Model_1, outcome, exposure, "start_new", "stop_new") 
  
  WCE_data_CI = WCE_data_CI %>% na.omit(WCE_data_CI)
                                        
  print(paste("nrow(dataset) after dropping nas", nrow(WCE_data_CI), sep=" = "))
  
  
  #all values have to be numeric for this analysis 
  
  
  #number_cov = length(covariates_list) 

  
  #print(paste("covariates_list", covariates_list, sep=": "))
  
  #all values have to be numeric for this analysis 
  
  #WCE_data_CI$checklist_depression_bin = as.numeric(WCE_data_CI$checklist_depression_bin)
  
  WCE_data_CI$start_new = as.numeric(WCE_data_CI$start_new)
  WCE_data_CI$stop_new = as.numeric(WCE_data_CI$stop_new)
  
  # WCE_data_CI$discrim_harassed_bin = as.numeric(WCE_data_CI$discrim_harassed_bin)
  #WCE_data_CI$discrim_lessrespect_bin = as.numeric(WCE_data_CI$discrim_lessrespect_bin)
  # WCE_data_CI$discrim_medical_bin = as.numeric(WCE_data_CI$discrim_medical_bin)
  # WCE_data_CI$discrim_notclever_bin = as.numeric(WCE_data_CI$discrim_notclever_bin)
  #  WCE_data_CI$discrim_poorerservice_bin = as.numeric(WCE_data_CI$discrim_poorerservice_bin)
  # WCE_data_CI$discrim_afraidothers_bin = as.numeric(WCE_data_CI$discrim_afraidothers_bin)
  
  WCE_data_CI$sex_1_2 = as.numeric(WCE_data_CI$sex_1_2)
  WCE_data_CI$wealth_noIRA = as.numeric(WCE_data_CI$wealth_noIRA)
  # WCE_data_CI$assessed_BMI = as.numeric(WCE_data_CI$assessed_BMI)
  WCE_data_CI$continious_age = as.numeric(WCE_data_CI$continious_age)
  
  WCE_data_CI$timepoints_indiv = as.numeric(WCE_data_CI$timepoints_indiv)
  
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
  ID <- unique(WCE_data_CI$HHIDPN) 
  
  for (i in 1:bootstraps_samples){ 
    
    outcome = outcome
    exposure = exposure
    
    print(paste("i", i, sep=": "))
    ID.resamp <- sort(sample(ID, size = 1000, replace=TRUE))
    
    sample_df <- WCE_data_CI[WCE_data_CI$HHIDPN %in% ID.resamp,]  # select obs. but duplicated Id are ignored
    
    #print("rows_check above in summary_score_Bootstrapped_CI")
    
    #print(sample_df)
    
    #print("sample_df above in summary_score_Bootstrapped_CI")
    
    #crash 
    # deal with duplicated HHIDPN and assign them new HHIDPN 
    # step <- 1 
    # repeat {
    #   # select duplicated HHIDPN in ID.resamp 
    #   ID.resamp <- ID.resamp[duplicated(ID.resamp)==TRUE]
    #   if (length(ID.resamp)==0) break # stop when no more duplicated HHIDPN to deal with 
    #   # select obs. but remaining duplicated HHIDPN are ignored 
    #   subset.dup <- WCE_data_CI[WCE_data_CI$HHIDPN %in% ID.resamp,] 
    #   # assign new HHIDPN to duplicates 
    #   subset.dup$HHIDPN <- subset.dup$HHIDPN + step * 10^ceiling(log10(max(WCE_data_CI$HHIDPN))) 
    #   # 10^ceiling(log10(max(WCE_data_CI$HHIDPN)) is the power of 10 
    #   #above the maximum HHIDPN from original data
    #   sample_df <- rbind(sample_df, subset.dup) 
    #   step <- step+1 
    # }
    
    #sample_df = sample_df %>% drop_na("HHIDPN", all_of(covariates_list), outcome, exposure, "start_new", "stop_new")
    
    #print(sample_df)
    
    #print(unique(sample_df$outcome))
    
    num_indiv_points_sample_df =  max(sample_df$timepoints_indiv)
    
    if (num_indiv_points_sample_df != Num_time_points) {
      print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
      print(paste("num_indiv_points_sample_df", num_indiv_points_sample_df, sep=" = "))
      print(paste("Num_time_points", Num_time_points, sep=" = "))
      print("--------------------------------------")
    }
    
    print(paste("Number of rows in sample_df", nrow(sample_df) , sep=" = "))
    
    
    #Num_time_points
    
    #print("num_indiv_points_sample_df:")
    #print(num_indiv_points_sample_df)
    
    # The following line standardizes specific columns provided in a concatenated list (i.e. covariates_list).
    for (covariate in covariates_list){sample_df[covariate] = scale(sample_df[covariate])}
    
    #sample_df[outcome] = scale(sample_df[outcome])
    #sample_df[exposure] = scale(sample_df[exposure])
    
    
    #print("mod is below: WCE(data = ...")
    print("About to call WCE.")
    
    mod <- tryCatch(
      {
        # Just to highlight: if you want to use more than one 
        # R expression in the "try" part then you'll have to 
        # use curly brackets.
        # 'tryCatch()' will return the last evaluated expression 
        # in case the "try" part was completed successfully
        
        # The result to be returned goes on the next line, it must be an expression.
        sample_df = sample_df %>% na.omit(sample_df)
        
        WCE(data = sample_df,
            analysis = "Cox", nknots = 1, cutoff = Num_time_points,
            constrained = "R", aic = FALSE,
            id = "HHIDPN",
            event = "diabetes_new_bin",
            start = "start_new",
            stop = "stop_new",
            expos = "discrim_bin",
            covariates = all_of(covariates_list))
      },
      error=function(cond) {
        message("it seems we caused an error")
        message("Here's the original error message:")
        message(cond)
        
        message("---")
        message("dim(sample_df): ")
        print(dim(sample_df))
        message("---")
        #message(sample_df)
        #message(paste("summary(sample_df)", summary(sample_df), sep=": "))
        message("---")
        message(paste("nrow(sample_df)", nrow(sample_df), sep=": "))
        message("---")
        
        #cov_1 = covariate[1]
        #cov_2 = covariate[2]
        #cov_3 = covariate[3]
        
        #cov_1_vector = sample_df[, cov_1]
        
        message("---")
        message(paste("length(sample_df[continious_age])", length(sample_df$continious_age), sep=": "))
        
        message("---")
        message(paste("length(sample_df[wealth_noIRA])", length(sample_df$wealth_noIRA), sep=": "))
        
        message("---")
        message(paste("length(sample_df[sex_1_2])", length(sample_df$sex_1_2), sep=": "))
        
        message("---")
        message(paste("length(sample_df[diabetes_new_bin])", length(sample_df$diabetes_new_bin), sep=": "))
        
        message("---")
        message(paste("length(sample_df[discrim_bin])", length(sample_df$discrim_bin), sep=": "))
        
        message("---")
        message(paste("length(sample_df[start_new])", length(sample_df$start_new), sep=": "))
        
        message("---")
        message(paste("length(sample_df[stop_new])", length(sample_df$stop_new), sep=": "))
        
        message("---")
        message(paste("length(sample_df[HHIDPN])", length(sample_df$HHIDPN), sep=": "))
        
        message("---")
        print(length(unique(sample_df$HHIDPN)))
        
        # for (covariate in covariates_list){
        #   print(covariate)
        #   message(paste("class(sample_df[covariate])", class(sample_df[,covariate]), sep=": "))
        # }
        message("---")
        print(class(sample_df$diabetes_new_bin))
        message("---")
        print(class(sample_df$discrim_bin))
        message("---")
        print(class(sample_df$continious_age))
        message("---")
        print(class(sample_df$wealth_noIRA))
        message("---")
        print(class(sample_df$sex_1_2))
        message("---")
        print(class(sample_df$start_new))
        message("---")
        print(class(sample_df$stop_new))
        message("---")
        print(class(sample_df$HHIDPN))
        
        message("---")
        print(unique(sample_df$diabetes_new_bin))
        
        message("---")
        print(summary(sample_df))
        
        message("---$")
        print(sample_df$HHIDPN)
        
        message("---")
        message(paste("here is the original error again: ", cond, sep=": "))
        message("---")
        print(checkWCE(sample_df, id = "HHIDPN", event = "diabetes_new_bin",  start = "start_new", stop = "stop_new", expos = "discrim_bin"))
        message("---")
        message(paste("crash was in i", i, sep=": "))
        
        # Choose a return value in case of error
        return(NA)
      }
    )
    
    print("About to print summary(mod): ")
    print(summary(mod))
    print("done printing summary")
    
    # return best WCE estimates and corresponding HR 
    #print("finished: mod = WCE(data = ...")
    
    
    #mod
    #summary(mod)
    #print("above printed: mod = WCE(data = ...")
    
    
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
  
  boot.HR_value = quantile(boot.HR, probs=0.5) 
  
  print("boot.HR_value = ")
  print(boot.HR_value)
  
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
  
  results = cbind(boot.HR_value, HR_CIs_all)
  
  print("result = ")
  print(results)
  
  print("done with this function!")
  return(results) 
}


