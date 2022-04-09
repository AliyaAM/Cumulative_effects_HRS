

subset_func = function(subset_var, 
                       subset_value,
                       HRS2008_data, 
                       HRS2010_data, 
                       HRS2012_data, 
                       HRS2014_data, 
                       HRS2016_data, 
                       HRS2018_data){
  
 
  #################################### subset ########################################################################
  # # # # subset the datasets 
  
  HRS2008_data_subset = subset(HRS2008_data, HRS2008_data[ , subset_var] == subset_value)
  
  HRS2010_data_subset  = subset(HRS2010_data, HRS2010_data[ , subset_var] == subset_value)
  
  HRS2012_data_subset  = subset(HRS2012_data, HRS2012_data[ , subset_var] == subset_value)
  
  HRS2014_data_subset  = subset(HRS2014_data, HRS2014_data[ , subset_var] == subset_value)
  
  HRS2016_data_subset  = subset(HRS2016_data, HRS2016_data[ , subset_var] == subset_value)
  
  HRS2018_data_subset  = subset(HRS2018_data, HRS2018_data[ , subset_var] == subset_value)
  
  
  subset_dataset = rbind(HRS2008_data_subset,
                             HRS2010_data_subset, 
                             HRS2012_data_subset,
                             HRS2014_data_subset, 
                             HRS2016_data_subset,
                             HRS2018_data_subset)
  
  
  return(subset_dataset)
}