
  clean_recode_sort = function (subset_var1, 
                                subset_value1, 
                                
                                subset_BMI, 
                                subset_BMI_value, 
                                
                                subset_var2, 
                                subset_value2,
                                
                                
                                subset_var3, 
                                subset_value3,
                                
                                HRS2008_data, 
                                HRS2010_data, 
                                HRS2012_data, 
                                HRS2014_data, 
                                HRS2016_data, 
                                HRS2018_data){
    
  
    
    HRS2008_data = HRS2008_data
    HRS2010_data = HRS2010_data
    HRS2012_data = HRS2012_data
    HRS2014_data = HRS2014_data 
    HRS2016_data = HRS2016_data
    HRS2018_data = HRS2018_data
    
    dataset = subset_func(subset_var1 = subset_var1,
                           subset_value1 = subset_value1, 
                           
                        
                          subset_BMI = subset_BMI, 
                          subset_BMI_value = subset_BMI_value, 
                          
                          subset_var2 = subset_var2,
                          subset_value2 = subset_value2, 
                          
                          
                          subset_var3 = subset_var3, 
                          subset_value3 = subset_value3,
                    
                           HRS2008_data = HRS2008_data, 
                           HRS2010_data = HRS2010_data, 
                           HRS2012_data = HRS2012_data, 
                           HRS2014_data = HRS2014_data, 
                           HRS2016_data = HRS2016_data, 
                           HRS2018_data = HRS2018_data) 
    
    dataset_clean = clean_recode_keyvars(data = dataset)

    
    return(dataset_clean)
}

#######
#######

