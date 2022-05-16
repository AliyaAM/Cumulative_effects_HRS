library(boot)
library(boot.pval)
library(WCE)


cumulative_effects_dat = read.csv("/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/DATA_FOR_PLOT/all_waves_nodiabatbaseline_DIAB.csv")
#cumulative_effects_dat = read.csv("/Users/aliya/my_docs/KCL_postDoc/Cumulative_effects/DATA_FOR_PLOT/all_waves_nodiabatbaseline_DIAB.csv")
#cumulative_effects_dat = read.csv("/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/DATA_FOR_PLOT/all_waves_nodiab_at_two_first_waves_DIAB_discrim_recoded.csv")

Model_1 = c("continious_age", "wealth_noIRA", "sex_1_2")

outcome = "diabetes_new_bin"
exposure = "discrim_bin"

covariates_list = Model_1

Num_time_points = max(cumulative_effects_dat$timepoints_indiv)


#Sample IDs with replacement:
ID <- unique(WCE_data_CI$HHIDPN) 

for (i in 1:bootstraps_samples){ 
  ID.resamp <- sort(sample(ID, replace=TRUE))
  datab <- WCE_data_CI[WCE_data_CI$HHIDPN %in% ID.resamp,]  # select obs. but duplicated Id are ignored
  
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
  
  mod <- WCE(data = datab, 
             analysis = "Cox", nknots = 1, cutoff = Num_time_points,
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
  
  boot.HR_1vs0[i] <- HR.WCE(mod, rep(1, Num_time_points), rep(0, Num_time_points)) 
}




boot_coxph = boot(boot.HR_1vs0, 
                  statistic = mod, 
                  R = 100) 
           
boot.pval(boot_coxph)


