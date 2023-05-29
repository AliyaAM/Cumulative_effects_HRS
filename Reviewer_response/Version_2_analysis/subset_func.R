

subset_func = function(subset_var1, 
                       subset_value1, 
                       
                       subset_BMI, 
                       subset_BMI_value, 
                       
                       subset_var2, 
                       subset_value2,
                       
                       
                       subset_var3, 
                       subset_value3,
                       
                       
                       subset_reason1, 
                       subset_reason1_value, 
                       
                       subset_reason2, 
                       subset_reason2_value, 
                       
                       
                       subset_reason3, 
                       subset_reason3_value, 
                       
                       HRS2008_data, 
                       HRS2010_data, 
                       HRS2012_data, 
                       HRS2014_data, 
                       HRS2016_data, 
                       HRS2018_data){
  
 
  #################################### subset ########################################################################
  # # # # subset the datasets 
  
  
  if (subset_reason1 =="NA" & subset_reason2 == "NA" & subset_reason3 == "NA"  & subset_var1 =="NA" & subset_BMI =="NA" & subset_var2 =="NA" & subset_var3 =="NA"){
    
    
    HRS2008_data_subset  = HRS2008_data
    HRS2010_data_subset  = HRS2010_data
    HRS2012_data_subset  = HRS2012_data
    HRS2014_data_subset  = HRS2014_data
    HRS2016_data_subset  = HRS2016_data
    HRS2018_data_subset  = HRS2018_data
    
  }
  
  
  if (subset_reason1 !="NA" & subset_reason2 == "NA" & subset_reason3 == "NA"  & subset_var1 =="NA" & subset_BMI =="NA" & subset_var2 =="NA" & subset_var3 =="NA"){
    
    
    HRS2008_data_subset  = subset(HRS2008_data, HRS2008_data[ , subset_reason1] == subset_reason1_value)
    HRS2010_data_subset  = subset(HRS2010_data, HRS2010_data[ , subset_reason1] == subset_reason1_value)
    HRS2012_data_subset  = subset(HRS2012_data, HRS2012_data[ , subset_reason1] == subset_reason1_value)
    HRS2014_data_subset  = subset(HRS2014_data, HRS2014_data[ , subset_reason1] == subset_reason1_value)
    HRS2016_data_subset  = subset(HRS2016_data, HRS2016_data[ , subset_reason1] == subset_reason1_value)
    HRS2018_data_subset  = subset(HRS2018_data, HRS2018_data[ , subset_reason1] == subset_reason1_value)
  }
  
  if (subset_reason1 =="NA"  & subset_reason2 == "NA" & subset_reason3 == "NA"  & subset_var1 !="NA" & subset_BMI =="NA" & subset_var2 =="NA" & subset_var3 =="NA"){
    
  
  HRS2008_data_subset  = subset(HRS2008_data, HRS2008_data[ , subset_var1] == subset_value1)
  HRS2010_data_subset  = subset(HRS2010_data, HRS2010_data[ , subset_var1] == subset_value1)
  HRS2012_data_subset  = subset(HRS2012_data, HRS2012_data[ , subset_var1] == subset_value1)
  HRS2014_data_subset  = subset(HRS2014_data, HRS2014_data[ , subset_var1] == subset_value1)
  HRS2016_data_subset  = subset(HRS2016_data, HRS2016_data[ , subset_var1] == subset_value1)
  HRS2018_data_subset  = subset(HRS2018_data, HRS2018_data[ , subset_var1] == subset_value1)
  
  }
  
  
  if (subset_reason1 =="NA" & subset_reason2 == "NA" & subset_reason3 == "NA"  & subset_var1 =="NA" & subset_BMI !="NA" & subset_var2 =="NA" & subset_var3 =="NA"){
    
    
    HRS2008_data_subset  = subset(HRS2008_data, HRS2008_data[ , subset_BMI] > subset_BMI_value)
    HRS2010_data_subset  = subset(HRS2010_data, HRS2010_data[ , subset_BMI] > subset_BMI_value)
    HRS2012_data_subset  = subset(HRS2012_data, HRS2012_data[ , subset_BMI] > subset_BMI_value)
    HRS2014_data_subset  = subset(HRS2014_data, HRS2014_data[ , subset_BMI] > subset_BMI_value)
    HRS2016_data_subset  = subset(HRS2016_data, HRS2016_data[ , subset_BMI] > subset_BMI_value)
    HRS2018_data_subset  = subset(HRS2018_data, HRS2018_data[ , subset_BMI] > subset_BMI_value)
    }
    
    
  
  
  if (subset_reason1 =="NA" & subset_reason2 == "NA" & subset_reason3 == "NA"  & subset_var1 !="NA" &  subset_BMI =="NA" & subset_var2 !="NA" & subset_var3 !="NA"){
    
    
    HRS2008_data_subset = subset(HRS2008_data, HRS2008_data[ , subset_var1] == subset_value1 | HRS2008_data[, subset_var2] == subset_value2 | HRS2008_data[, subset_var3] == subset_value3)
    HRS2010_data_subset = subset(HRS2010_data, HRS2010_data[ , subset_var1] == subset_value1 | HRS2010_data[, subset_var2] == subset_value2 | HRS2010_data[, subset_var3] == subset_value3)
    HRS2012_data_subset = subset(HRS2012_data, HRS2012_data[ , subset_var1] == subset_value1 | HRS2012_data[, subset_var2] == subset_value2 | HRS2012_data[, subset_var3] == subset_value3)
    HRS2014_data_subset = subset(HRS2014_data, HRS2014_data[ , subset_var1] == subset_value1 | HRS2014_data[, subset_var2] == subset_value2 | HRS2014_data[, subset_var3] == subset_value3)
    HRS2016_data_subset = subset(HRS2016_data, HRS2016_data[ , subset_var1] == subset_value1 | HRS2016_data[, subset_var2] == subset_value2 | HRS2016_data[, subset_var3] == subset_value3)
    HRS2018_data_subset = subset(HRS2018_data, HRS2018_data[ , subset_var1] == subset_value1 | HRS2018_data[, subset_var2] == subset_value2 | HRS2018_data[, subset_var3] == subset_value3)
  }
  
  if (subset_reason1 !="NA" & subset_reason2 == "NA" & subset_reason3 == "NA"  & subset_var1 !="NA" &  subset_BMI =="NA" & subset_var2 =="NA" & subset_var3 =="NA"){
    
    
    HRS2008_data_subset = subset(HRS2008_data, HRS2008_data[ , subset_var1] == subset_value1 & HRS2008_data[ , subset_reason1] == subset_reason1_value)
    HRS2010_data_subset = subset(HRS2010_data, HRS2010_data[ , subset_var1] == subset_value1 & HRS2010_data[ , subset_reason1] == subset_reason1_value)
    HRS2012_data_subset = subset(HRS2012_data, HRS2012_data[ , subset_var1] == subset_value1 & HRS2012_data[ , subset_reason1] == subset_reason1_value)
    HRS2014_data_subset = subset(HRS2014_data, HRS2014_data[ , subset_var1] == subset_value1 & HRS2014_data[ , subset_reason1] == subset_reason1_value)
    HRS2016_data_subset = subset(HRS2016_data, HRS2016_data[ , subset_var1] == subset_value1 & HRS2016_data[ , subset_reason1] == subset_reason1_value)
    HRS2018_data_subset = subset(HRS2018_data, HRS2018_data[ , subset_var1] == subset_value1 & HRS2018_data[ , subset_reason1] == subset_reason1_value)
  }
  
  
  if (subset_reason1 !="NA" & subset_reason2 == "NA" & subset_reason3 == "NA"  & subset_var1 =="NA" &  subset_BMI !="NA" & subset_var2 =="NA" & subset_var3 =="NA"){
    
    
    HRS2008_data_subset = subset(HRS2008_data, HRS2008_data[ , subset_BMI] == subset_BMI_value & HRS2008_data[ , subset_reason1] == subset_reason1_value)
    HRS2010_data_subset = subset(HRS2010_data, HRS2010_data[ , subset_BMI] == subset_BMI_value & HRS2010_data[ , subset_reason1] == subset_reason1_value)
    HRS2012_data_subset = subset(HRS2012_data, HRS2012_data[ , subset_BMI] == subset_BMI_value & HRS2012_data[ , subset_reason1] == subset_reason1_value)
    HRS2014_data_subset = subset(HRS2014_data, HRS2014_data[ , subset_BMI] == subset_BMI_value & HRS2014_data[ , subset_reason1] == subset_reason1_value)
    HRS2016_data_subset = subset(HRS2016_data, HRS2016_data[ , subset_BMI] == subset_BMI_value & HRS2016_data[ , subset_reason1] == subset_reason1_value)
    HRS2018_data_subset = subset(HRS2018_data, HRS2018_data[ , subset_BMI] == subset_BMI_value & HRS2018_data[ , subset_reason1] == subset_reason1_value)
  }
  
  
  if (subset_reason1 !="NA" & subset_reason2 != "NA" & subset_reason3 != "NA"  & subset_var1 !="NA" &  subset_BMI =="NA" & subset_var2 !="NA" & subset_var3 !="NA"){
    
    HRS2008_data_subset_combo = subset(HRS2008_data, HRS2008_data[ , subset_var1] == subset_value1 | HRS2008_data[, subset_var2] == subset_value2 | HRS2008_data[, subset_var3] == subset_value3)
    HRS2010_data_subset_combo  = subset(HRS2010_data, HRS2010_data[ , subset_var1] == subset_value1 | HRS2010_data[, subset_var2] == subset_value2 | HRS2010_data[, subset_var3] == subset_value3)
    HRS2012_data_subset_combo  = subset(HRS2012_data, HRS2012_data[ , subset_var1] == subset_value1 | HRS2012_data[, subset_var2] == subset_value2 | HRS2012_data[, subset_var3] == subset_value3)
    HRS2014_data_subset_combo  = subset(HRS2014_data, HRS2014_data[ , subset_var1] == subset_value1 | HRS2014_data[, subset_var2] == subset_value2 | HRS2014_data[, subset_var3] == subset_value3)
    HRS2016_data_subset_combo  = subset(HRS2016_data, HRS2016_data[ , subset_var1] == subset_value1 | HRS2016_data[, subset_var2] == subset_value2 | HRS2016_data[, subset_var3] == subset_value3)
    HRS2018_data_subset_combo  = subset(HRS2018_data, HRS2018_data[ , subset_var1] == subset_value1 | HRS2018_data[, subset_var2] == subset_value2 | HRS2018_data[, subset_var3] == subset_value3)
    
    
    
    HRS2008_data_subset = subset(HRS2008_data_subset_combo, HRS2008_data_subset_combo[ , subset_reason1] == subset_reason1_value | HRS2008_data_subset_combo[ , subset_reason2] == subset_reason2_value | HRS2008_data_subset_combo[ , subset_reason3] == subset_reason3_value)
    HRS2010_data_subset = subset(HRS2010_data_subset_combo, HRS2010_data_subset_combo[ , subset_reason1] == subset_reason1_value | HRS2010_data_subset_combo[ , subset_reason2] == subset_reason2_value | HRS2010_data_subset_combo[ , subset_reason3] == subset_reason3_value)
    HRS2012_data_subset = subset(HRS2012_data_subset_combo, HRS2012_data_subset_combo[ , subset_reason1] == subset_reason1_value | HRS2012_data_subset_combo[ , subset_reason2] == subset_reason2_value | HRS2012_data_subset_combo[ , subset_reason3] == subset_reason3_value)
    HRS2014_data_subset = subset(HRS2014_data_subset_combo, HRS2014_data_subset_combo[ , subset_reason1] == subset_reason1_value | HRS2014_data_subset_combo[ , subset_reason2] == subset_reason2_value | HRS2014_data_subset_combo[ , subset_reason3] == subset_reason3_value)
    HRS2016_data_subset = subset(HRS2016_data_subset_combo, HRS2016_data_subset_combo[ , subset_reason1] == subset_reason1_value | HRS2016_data_subset_combo[ , subset_reason2] == subset_reason2_value | HRS2016_data_subset_combo[ , subset_reason3] == subset_reason3_value)
    HRS2018_data_subset = subset(HRS2018_data_subset_combo, HRS2018_data_subset_combo[ , subset_reason1] == subset_reason1_value | HRS2018_data_subset_combo[ , subset_reason2] == subset_reason2_value | HRS2018_data_subset_combo[ , subset_reason3] == subset_reason3_value)
    
    
  }
      
    subset_dataset = rbind(HRS2008_data_subset,
                           HRS2010_data_subset, 
                           HRS2012_data_subset,
                           HRS2014_data_subset, 
                           HRS2016_data_subset,
                           HRS2018_data_subset)
  
  
  
  
  return(subset_dataset)
}