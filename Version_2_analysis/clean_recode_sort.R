
  clean_recode_sort = function (subset_var,
                                subset_value, 
                                HRS2008_data, 
                                HRS2010_data, 
                                HRS2012_data, 
                                HRS2014_data, 
                                HRS2016_data, 
                                HRS2018_data){
    
    subset_var = subset_var
    subset_value = subset_value
    
    HRS2008_data = HRS2008_data
    HRS2010_data = HRS2010_data
    HRS2012_data = HRS2012_data
    HRS2014_data = HRS2014_data 
    HRS2016_data = HRS2016_data
    HRS2018_data = HRS2018_data
    
    dataset = subset_func(subset_var = subset_var,
                           subset_value = subset_value, 
                           
                    
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

