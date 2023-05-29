# line to standardize specific columns provided in a concatenated list.
# for (covariate in covariates_list){sample_df[covariate] = scale(sample_df[covariate])}

summary_score_Bootstrapped_CI = function (WCE_data_CI, 
                                          outcome, 
                                          exposure, 
                                          covariates_list){
  
  
  outcome = outcome
  
  exposure = exposure
  
  print(paste("covariates_list", covariates_list, sep=": "))
  
  print(paste("nrow(dataset) before dropping nas", nrow(WCE_data_CI), sep=" = "))
  
  WCE_data_CI = WCE_data_CI %>% dplyr::select(HHIDPN, all_of(covariates_list), outcome, exposure, start_new, stop_new, timepoints_indiv) 
  #WCE_data_CI = WCE_data_CI %>% drop_na("HHIDPN", all_of(covariates_list), outcome, exposure, "start_new", "stop_new")
  
  print(paste("nrow(dataset) after dropping nas", nrow(WCE_data_CI), sep=" = "))
  
  #all values have to be numeric for this analysis 
  
  #all values have to be numeric for this analysis 
  
  WCE_data_CI$diabetes_new_bin = as.numeric(WCE_data_CI$diabetes_new_bin)
  WCE_data_CI$discrim_bin = as.numeric(WCE_data_CI$discrim_bin)
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


# DELETE THIS LATER!!!
Model_1 = c("continious_age", "wealth_noIRA", "sex_1_2", "checklist_depression_bin")

#"vigarious_physical_activity_new", 'smokes_now_bin')


#Model_3 = c("continious_age", "wealth_noIRA", "sex_1_2",   "vigarious_physical_activity_new", 'smokes_now_bin')
#Model 4: age, sex, wealth, CVD  [basic adjustment + CVD most common diabetes co_morbidity]
#Model_4 = c("continious_age", "wealth_noIRA", "sex_1_2","CVD")
#Model 5: age, sex, wealth, depression  [basic adjustment + depression best researched psychosocial factor in diabetes ]
#Model_5 = c("continious_age","wealth_noIRA", "sex_1_2","checklist_depression_bin")
#Model 6: age, sex, wealth, BMI, hypertension, CVD  [basic adjustment + diabetes risk factors+ CVD]

#Model_6 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "hypertension_new_bin", "CVD")
#Model_6 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "CVD")

#Model 7: age, sex, wealth, BMI, hypertension, depression  [basic adjustment + diabetes risk factors+ Depression]
#Model_7 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "hypertension_new_bin", "checklist_depression_bin")
#Model_7 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "checklist_depression_bin")


exposure = "discrim_bin"
outcome = "diabetes_new_bin"
loaded_df = read_csv("/Users/aliya/my_docs/proj/Cumulative_effects_HRS/Results/dataset_noNAs_timepoints_TEST_DELETE_AFTER_debugging_24nov2022.csv")


# Remove NAs that slipped in anyway


sex_na = c(138300010)
#  Remove participant with sneaky NA in their BMI variable.

BMI_null_ids = c(18525020,  20634010,  24040011,  50158020,  53033031,  57630010,
                 58974010,  81751020,  82648040,  85482020,  85630010,  87287020,
                 87549040, 115355020, 153089010, 153479010, 184591010, 500498010,
                 500582020, 502068011, 502328010, 502490010,  13635010,  39357040,
                 42943020,  46558020,  47582011,  57644040,  72327040,  77744020,
                 139930020, 141144020, 210253010, 500281020, 500286020, 501717010,
                 502021010, 502226020, 502364020, 521251010, 524179010, 527123010,
                 529360010, 532259020, 536403020, 537264020, 537283010, 902489020,
                 915508010, 915508020, 920200020,  14578040, 501219020, 526342020,
                 527180010, 526578010, 529828010, 912068010, 917056010, 917702010,
                 204659010, 212361010, 902805020, 914269010, 915940020,  11948020,
                 21675030,  47602020,  78026021,  80353040, 117310011, 200683020,
                 202973010, 205110021, 501646010,  12458020,  12745020,  13734020,
                 17234040,  18285030,  46641020,  59258030,  60232040,  60290030,
                 71012020, 138205010, 160233010, 212572010, 900500010, 919052010)


#  Remove participant with sneaky NA in their hypertension variable.

hypertension_null_ids =  c(10475010,  10645031,  11611010,  12135030,  12401040,  12512020,
                           13784010,  13814040,  13817011,  14167040,  14830020,  15195010,
                           15440040,  15657010,  16035010,  16035040,  16566020,  17363010,
                           19047020,  19458040,  19754010,  20777040,  21012030,  21058020,
                           21078010,  21717010,  22245010,  22544010,  22659011,  22665010,
                           23212010,  23756010,  25556010,  30644010,  31766020,  32634030,
                           33527030,  34231010,  35364020,  35364030,  35421010,  35849010,
                           35901030,  36179020,  36218020,  36236010,  36257010,  36787010,
                           36796040,  37106010,  37184030,  37735010,  38496020,  39101020,
                           39183020,  39279040,  40158021,  40276010,  40918020,  41129010,
                           41220020,  42493010,  42814010,  43512020,  44406010,  44647011,
                           45217010,  45992040,  46187010,  46443010,  46559010,  46697011,
                           46787010,  46821010,  47168010,  47320020,  47495010,  47707010,
                           47707020,  48406020,  48440040,  48622020,  48683010,  48825020,
                           48904010,  49219020,  49242010,  49396010,  49470010,  50173040,
                           50364040,  50385020,  50784010,  50822010,  50930010,  51003041,
                           51405020,  51937010,  51961010,  52234010,  52703040,  55394010,
                           55574020,  56076010,  56955011,  59063010,  60311010,  60547040,
                           60788010,  60956030,  61230010,  64609040,  65015010,  65428010,
                           65761010,  72544010,  73050020,  73878011,  75031010,  76081020,
                           76598020,  76870010,  76941010,  77697010,  77866010,  78495010,
                           78687020,  79180011,  79673010,  79900020,  80000010,  80116020,
                           80339011,  80593010,  81563020,  82314040,  83108010,  84218010,
                           84433020,  84454021,  84641020,  85992040,  86203010,  86354011,
                           86508011,  87402010,  87542020,  95650010, 111597020, 114508010,
                           114651010, 117988010, 121006020, 121178010, 121197011, 121480020,
                           122410010, 122520010, 131580010, 139864020, 140382010, 140753010,
                           141187010, 146110010, 146250010, 147661020, 148951010, 148951020,
                           149115010, 149570020, 151304020, 152575010, 153089020, 155821010,
                           157407010, 157483010, 159013010, 160030020, 162284010, 173877010,
                           173877020, 174031010, 178602020, 178905010, 178976010, 179898010,
                           183634010, 186920010, 200799020, 200823020, 200834020, 201328020,
                           201363020, 203311020, 204393010, 204415020, 204956010, 205001011,
                           205427010, 205537020, 205628020, 206478010, 206674011, 207459020,
                           207516010, 207901010, 210538010, 211000010, 211036010, 211120010,
                           211157010, 211185020, 211239010, 211296020, 211299020, 211972010,
                           212028020, 212198020, 212606010, 212840010, 212964020, 212993010,
                           213049011, 213117010, 213126020, 213189020, 213233010, 500009010,
                           500017020, 500138010, 500233010, 500249010, 500355010, 500370010,
                           500490010, 500498020, 500568020, 500580010, 500597010, 500605020,
                           500648010, 500649010, 500672010, 500719010, 500742010, 500800010,
                           500916010, 500921010, 500926010, 500956020, 501177010, 501187010,
                           501266020, 501331010, 501352010, 501359020, 501375010, 501439010,
                           501451010, 501511020, 501652010, 501659010, 501762020, 501840010,
                           501875020, 501939010, 501966010, 502088020, 502185010, 502221010,
                           502293020, 502314010, 502318010, 502336020, 502356010, 502370010,
                           502395020, 502514020, 502523020, 502671010, 502750010,  10109030,
                           12586010,  12611010,  12695030,  13552030,  13899041,  14188020,
                           14247010,  14270040,  14420010,  15225020,  15236010,  15516010,
                           15834010,  16048010,  16705020,  17075010,  17075020,  17952040,
                           21269010,  21629040,  22137020,  22243020,  22275010,  22738020,
                           22837040,  22861040,  23119030,  24500020,  25081020,  31633020,
                           32048010,  32481010,  32929010,  35330010,  36408030,  37865030,
                           38685020,  39753030,  39776040,  40552010,  40795030,  41046020,
                           41047010,  41880010,  41906010,  41942040,  42844010,  43179010,
                           43494010,  44391010,  44580011,  44994020,  45081011,  45500010,
                           45746040,  45984010,  46138010,  47578010,  47751010,  48615010,
                           48623021,  48982010,  48982020,  49542040,  50176040,  50365030,
                           50528020,  51228010,  52470010,  52672010,  53029011,  53266010,
                           53813010,  54082020,  56010010,  58752010,  58958010,  60136040,
                           60300010,  60594040,  60637010,  60656030,  65878040,  72327010,
                           73625010,  75035041,  76933010,  77532010,  77566010,  77788020,
                           77912040,  77921010,  77969011,  78465020,  78585020,  78747010,
                           78987020,  79760010,  79825010,  79867020,  79907020,  81113040,
                           81443020,  81553010,  81554020,  82179020,  83217020,  83494010,
                           83543010,  83904010,  84465010,  85577030,  85581040,  87200010,
                           110427010, 112435010, 112575010, 112747010, 115627010, 115842010,
                           118324010, 122704010, 124573020, 124773010, 130654020, 131774010,
                           131880010, 135681010, 136573010, 137464010, 137882010, 142813020,
                           143433020, 147421010, 150297010, 150370010, 151409010, 151986010,
                           154552010, 155507010, 155934010, 155965010, 161196020, 164374020,
                           164974020, 172868010, 173675020, 174335020, 178095020, 178447020,
                           178865010, 180872020, 182026020, 184830020, 185285010, 185285020,
                           186047020, 186438010, 203309020, 204330020, 204509020, 205095011,
                           205371020, 205966011, 206273010, 207430021, 207903020, 211367010,
                           211376010, 211418010, 211462010, 211814020, 211912020, 211936020,
                           211970010, 212080010, 212197010, 212200010, 212699010, 212931010,
                           213416010, 500070010, 500191010, 500204010, 500236012, 500273020,
                           500309010, 500311020, 500322020, 500367010, 500387010, 500391020,
                           500416021, 500438010, 500503010, 500663020, 500678010, 500721010,
                           500723010, 500759010, 500784020, 500900020, 500934010, 501045010,
                           501087021, 501131010, 501155010, 501299020, 501400020, 501405010,
                           501440010, 501448010, 501459020, 501530010, 501543010, 501550010,
                           501684010, 501686020, 501692020, 501699010, 501978010, 502021020,
                           502117020, 502146010, 502226020, 502349010, 502414020, 502433020,
                           502497010, 502529010, 502556010, 502627010, 502642010, 502657020,
                           502723020, 520084010, 520619010, 521672010, 521714020, 521849010,
                           521849020, 523768020, 523964010, 523970020, 524071020, 524076010,
                           524145010, 524213020, 524322010, 524371010, 524912010, 525037010,
                           525707020, 525764020, 526173010, 526336020, 526423010, 526689020,
                           529519020, 529782010, 530980020, 532544010, 533243010, 533605010,
                           533628020, 534426010, 534897010, 534984010, 536262020, 537500020,
                           545474010, 900269020, 900290020, 900777010, 901135020, 901174010,
                           902392020, 902971020, 903300010, 903529020, 905375010, 906543020,
                           907306020, 907690020, 908928010, 909841020, 911152010, 914265020,
                           914637010, 915367020, 915508010, 916600020, 918312010, 918388020,
                           918529010, 920200010, 527143020, 527208020, 904047010, 904025010,
                           10458020,  34086011,  40323020,  44169031,  50365031,  58493010,
                           73927010,  81198020,  82483040, 131812010, 140958010, 150525020,
                           185442010, 205363020, 501446010, 501839011, 520384011, 521317010,
                           522011020, 522123020, 523221020, 524037020, 528439010, 532097010,
                           901180011, 901477010, 902740010, 909569020, 909624011, 909889010,
                           914305020, 915988020) 


#  Remove participant with sneaky NA in their PA variable.

physical_activity_null_ids  = c(85577030, 522550010)


#  Remove participant with sneaky NA in their smoking bin variable (all NA)


# no NA in CVD and checklist_depression bin variable 


all_nas = c(sex_na, physical_activity_null_ids, hypertension_null_ids, BMI_null_ids)

ID_nas = unique(all_nas)


ID_nas = unique(all_nas)

length(ID_nas)
#dataset_noNAs_timepoints = loaded_df[,!(names(loaded_df) %in% drop)]


dataset_noNAs_timepoints<-loaded_df[!(loaded_df$HHIDPN %in% ID_nas),]


nrow(dataset_noNAs_timepoints)
nrow(loaded_df)


#loaded_df<-loaded_df[!(loaded_df$HHIDPN==BMI_null_ids),]
Model_1 = c("continious_age", "wealth_noIRA", "sex_1_2", "assessed_BMI", "hypertension_new_bin")
result = summary_score_Bootstrapped_CI(dataset_noNAs_timepoints, outcome=outcome, exposure=exposure, covariates_list=Model_1)


