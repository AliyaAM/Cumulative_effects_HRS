

Bootstrapped_CI = function (participant_wave_df, outcome, exposure, covariates_list){


participant_wave_df = na.omit(participant_wave_df)
#should be between 300 and 100, the more the better but runs slower. to test the analysis I will set it to 5 for now. 

bootstraps_samples = 100
Num_time_points = max(participant_wave$timepoints_indiv)

#Prepare vectors to extract estimated weight function and (if relevant) HRs for each bootstrap resample: 

boot.WCE <- matrix(NA, ncol = Num_time_points, nrow = bootstraps_samples) # to store estimated weight functions 
boot.HR <- rep(NA, bootstraps_samples) # to store estimated HRs

#Sample IDs with replacement:
ID <- unique(participant_wave_df$HHIDPN) 

for (i in 1:bootstraps_samples){ 
  ID.resamp <- sort(sample(ID, replace=FALSE))
  datab <- participant_wave_df[participant_wave_df$HHIDPN %in% ID.resamp,]  # select obs. but duplicated Id are ignored
  
  
  mod <- WCE(data = datab, 
             analysis = "Cox", nknots = 1:3, cutoff = Num_time_points,
             constrained = "R", aic = FALSE, MatchedSet = NULL, 
             id = "HHIDPN", 
             event = outcome, 
             start = "start_new", 
             stop = "stop_new", 
             expos = exposure,
             covariates = covariates_list)
  
  
  # return best WCE estimates and corresponding HR 
  best <- which.min(mod$info.criterion) 
  boot.WCE[i,] <- mod$WCEmat[best,] 
  boot.HR[i] <- HR.WCE(mod, rep(1, Num_time_points), rep(0, Num_time_points)) 
  
} 


# estimated weight functions 
estimated_weight_functions  = apply(boot.WCE, 1, quantile, p = c(0.05, 0.95))

# estimated HR 
HR_CI =  quantile(boot.HR, p = c(0.05, 0.95)) 

unique(datab$HHIDPN)
head(datab$HHIDPN)
tail(datab$HHIDPN)
datab$HHIDPN

return(HR_CI)
}