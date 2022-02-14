Cumulative_effects = function(data_HRS2008,
                              data_HRS2010, 
                              data_HRS2012,
                              data_HRS2014, 
                              data_HRS2016, 
                              data_HRS2018, 
                              
                              #same arguments as in the fucntions used in cross-nat analysis to subset the data: 
                              analysis_variable_name,
                               subsetting_VAR1_HRS, 
                               HRS_var1_value, 
                               subsetting_VAR2_HRS,
                               HRS_var2_value,
                               discrimination_VAR_hrs,
                              
                              #arguments that specify dose and event for the WCE
                              dose,  #harrassed, lessrespect etc 0-6 scale 
                              Event #diabetes_new, heartattack etc
                              ){
  
  
  
  if (subsetting_VAR1_HRS == "NA" & subsetting_VAR2_HRS == "NA"){
    
    data_HRS2008_subset = data_HRS2008
    data_HRS2010_subset = data_HRS2010
    data_HRS2012_subset = data_HRS2012
    data_HRS2014_subset = data_HRS2014
    data_HRS2016_subset = data_HRS2016
    data_HRS2018_subset = data_HRS2018
    
  } 
  
  if ( subsetting_VAR1_HRS != "NA" & subsetting_VAR2_HRS == "NA"){
    
    data_HRS2008_subset = subset(data_HRS2008, data_HRS2008[ , subsetting_VAR1_HRS] == HRS_var1_value)
    data_HRS2010_subset = subset(data_HRS2010, data_HRS2010[ , subsetting_VAR1_HRS] == HRS_var1_value)
    data_HRS2012_subset = subset(data_HRS2012, data_HRS2012[ , subsetting_VAR1_HRS] == HRS_var1_value)
    data_HRS2014_subset = subset(data_HRS2014, data_HRS2014[ , subsetting_VAR1_HRS] == HRS_var1_value)
    data_HRS2016_subset = subset(data_HRS2016, data_HRS2016[ , subsetting_VAR1_HRS] == HRS_var1_value)
    data_HRS2018_subset = subset(data_HRS2018, data_HRS2018[ , subsetting_VAR1_HRS] == HRS_var1_value)
    
    
  } 
  
  if (subsetting_VAR1_HRS != "NA" & subsetting_VAR2_HRS != "NA"){
    
    data_HRS2008_subset = subset(data_HRS2008, data_HRS2008[ , subsetting_VAR1_HRS] == HRS_var1_value & data_HRS2008[ ,subsetting_VAR2_HRS] == HRS_var2_value)
    data_HRS2010_subset = subset(data_HRS2010, data_HRS2010[ , subsetting_VAR1_HRS] == HRS_var1_value & data_HRS2010[ ,subsetting_VAR2_HRS] == HRS_var2_value)
    
    data_HRS2012_subset = subset(data_HRS2012, data_HRS2012[ , subsetting_VAR1_HRS] == HRS_var1_value & data_HRS2012[ ,subsetting_VAR2_HRS] == HRS_var2_value)
    data_HRS2014_subset = subset(data_HRS2014, data_HRS2014[ , subsetting_VAR1_HRS] == HRS_var1_value & data_HRS2014[ ,subsetting_VAR2_HRS] == HRS_var2_value)
    data_HRS2016_subset = subset(data_HRS2016, data_HRS2016[ , subsetting_VAR1_HRS] == HRS_var1_value & data_HRS2016[ ,subsetting_VAR2_HRS] == HRS_var2_value)
    data_HRS2018_subset = subset(data_HRS2018, data_HRS2018[ , subsetting_VAR1_HRS] == HRS_var1_value & data_HRS2018[ ,subsetting_VAR2_HRS] == HRS_var2_value)
    
  }
  
  
  #first subset data as above above 
  #second rename all variablesso they match names across waves (important for the third step below) (this step an be done outside this funciton)
  #third merge data where each time x participant is one raw, but then then all variables have to be the same name  
  
  data_HRS_subset_all
  
  wce.obj <- WCE(data = data_HRS_subset_all,
                 analysis = "Cox",
                 nknots = 1:3, 
                 cutoff = XX, #figure out this 
                 constrained = "R",
                 aic = FALSE, 
                 MatchedSet = NULL, 
                 id = "Id", 
                 event = "Event", #these do not yet exist in the dataframe, pick the var name from the arguments 
                 start = "Start", 
                 stop = "Stop", 
                 expos = "dose", #these do not yet exist in the dataframe, pick the var name from the arguments 
                 covariates = c("sex", "age")) #these do not yet exist in the dataframe, pick the var name from the arguments 
}
  
  
  #################### data_HRS = HRS2010_discrimination_dataset_before_subsetting,   #analysis_variable_name = "disability", #subsetting_VAR1_HRS = "limiting_condition_bin", #HRS_var1_value = 1, #subsetting_VAR2_HRS =   "NA",  #HRS_var2_value = "NA", #discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_disability"