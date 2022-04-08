## Set the root directory to look for source code.
#laptop: 
#/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/
#"/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"

# plots: 
# https://adibender.github.io/pammtools/articles/cumulative-effects.html

#https://rpkgs.datanovia.com/ggpubr/reference/stat_regline_equation.html Adding text (coefficients etc) to the plot next to the regression line 

####### exposure is coded as: 
####### 1 Almost everyday
####### 2 At least once a week
####### 3 A few times a month
####### 4 A few times a year
####### 5 Less than once a year
####### 6 Never

####### HR 1vs6 is Almost everyday vs Never
####### HR 2vs6 is At least once a week vs Never
####### HR 3vs6 is A few times a month vs Never
####### HR 4vs6 is A few times a year vs Never
####### HR 5vs6 is Less than once a year vs Never


#WCE_dataset_subset = read.csv(paste(SOURCE_data_ROOT, "WCE_dataset_subset_diabetes.csv", sep=""))
#unique(WCE_dataset_subset$diabetes_new_bin)
#WCE_dataset_subset$discrim_bin

#WCE_dataset_subset.csv 
#WCE_dataset_subset.csv
#WCE_dataset_national_origin_ousideUS
#WCE_dataset_subset.csv
#WCE_dataset_subset.csv

#unique(WCE_dataset_subset$summary_mean_score_discrim_bin)
#unique(WCE_dataset_subset$discrim_bin)
#unique(WCE_dataset_subset$discrim_lessrespect_bin)
#unique(WCE_dataset_subset$discrim_medical_bin)
#unique(WCE_dataset_subset$discrim_notclever_bin)
#unique(WCE_dataset_subset$discrim_poorerservice_bin)
#unique(WCE_dataset_subset$discrim_afraidothers_bin)


## number_reasons_discrimination

#################################### ######## ########################################################################


subset_sort = function(subset_var, subset_value,
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
  
  
  WCE_dataset_subset = rbind(HRS2008_data_subset,
                             HRS2010_data_subset, 
                             HRS2012_data_subset,
                             HRS2014_data_subset, 
                             HRS2016_data_subset,
                             HRS2018_data_subset)
  
  
  return(WCE_dataset_subset)
}