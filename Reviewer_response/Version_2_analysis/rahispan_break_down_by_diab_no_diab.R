#current_directory = "/Users/aliyaamirova/"

current_directory =  "/Users/aliya/my_docs/"
  
  
DATA_ROOT = paste(current_directory, "KCL_postDoc/Data_analysis/", sep = "")

#DATA_ROOT = paste(current_directory, "ELSA_HRS/Data_analysis/", sep = "")

add_hispanic = read.csv(paste(DATA_ROOT, "randhrs1992_2018v1.csv",sep=""))

unique(add_hispanic$HHIDPN)
nrow(add_hispanic)
add_hispanic$RAHISPAN


HRS2008_data_initial = read.csv(paste(DATA_ROOT, "HRS_2008_data/HRS2008_data_short_education.csv", sep=""))
HRS2010_data_initial = read.csv(paste(DATA_ROOT, "HRS_2010_data/HRS2010_data_short_education.csv", sep=""))
HRS2012_data_initial = read.csv(paste(DATA_ROOT, "HRS_2012_data/HRS2012_data_short_education.csv", sep=""))
HRS2014_data_initial = read.csv(paste(DATA_ROOT, "HRS_2014_data/HRS2014_data_short_education.csv", sep=""))
HRS2016_data_initial = read.csv(paste(DATA_ROOT, "HRS_2016_data/HRS2016_data_short_education.csv", sep=""))
HRS2018_data_initial = read.csv(paste(DATA_ROOT, "HRS_2018_data/HRS2018_data_short_education.csv", sep=""))

#add_hispanic = read.csv("/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/randhrs1992_2018v1.csv")




# Select HHIDPN and RAHISPAN columns from add_hispanic
add_hispanic_selected <- add_hispanic[, c('HHIDPN', 'RAHISPAN', "RARACEM", "RAEDEGRM")]

# Merge add_hispanic_selected with all other dataframes
HRS2008_data_initial <- merge(HRS2008_data_initial, add_hispanic_selected, by = 'HHIDPN', all = TRUE)
HRS2010_data_initial <- merge(HRS2010_data_initial, add_hispanic_selected, by = 'HHIDPN', all = TRUE)
HRS2012_data_initial <- merge(HRS2012_data_initial, add_hispanic_selected, by = 'HHIDPN', all = TRUE)
HRS2014_data_initial <- merge(HRS2014_data_initial, add_hispanic_selected, by = 'HHIDPN', all = TRUE)
HRS2016_data_initial <- merge(HRS2016_data_initial, add_hispanic_selected, by = 'HHIDPN', all = TRUE)
HRS2018_data_initial <- merge(HRS2018_data_initial, add_hispanic_selected, by = 'HHIDPN', all = TRUE)



ids = unique(all_years$HHIDPN)

HRS2008_data_initial$RAHISPAN
HRS2010_data_initial$RAHISPAN
HRS2012_data_initial$RAHISPAN
HRS2014_data_initial$RAHISPAN
HRS2016_data_initial$RAHISPAN
HRS2018_data_initial$RAHISPAN

HRS2008_data_initial <- subset(HRS2008_data_initial, select = c(HHIDPN, diabetes_new, RAHISPAN, RARACEM, RAEDEGRM))
HRS2010_data_initial <- subset(HRS2010_data_initial, select = c(HHIDPN, diabetes_new, RAHISPAN, RARACEM, RAEDEGRM))
HRS2012_data_initial <- subset(HRS2012_data_initial, select = c(HHIDPN, diabetes_new, RAHISPAN, RARACEM, RAEDEGRM))
HRS2014_data_initial <- subset(HRS2014_data_initial, select = c(HHIDPN, diabetes_new, RAHISPAN, RARACEM, RAEDEGRM))
HRS2016_data_initial <- subset(HRS2016_data_initial, select = c(HHIDPN, diabetes_new, RAHISPAN, RARACEM, RAEDEGRM))
HRS2018_data_initial <- subset(HRS2018_data_initial, select = c(HHIDPN, diabetes_new, RAHISPAN, RARACEM, RAEDEGRM))



all_years = rbind(HRS2008_data_initial, 
                  HRS2010_data_initial, 
                  HRS2012_data_initial, 
                  HRS2014_data_initial, 
                  HRS2016_data_initial, 
                  HRS2018_data_initial)



flow_chart_drop_baseline_diabetes = function (data){
  
  
  ID = unique(data$HHIDPN)
  
  #print(isTRUE(data$HHIDPN == ID[1]))
  #data = data %>% drop_na()
  
  participant_wave_df = data.frame()
  
  n = 1
  for (id in ID){
    
    
    participant_wave = subset(data, data$HHIDPN == id)
    
    if (nrow(participant_wave)== 1){
      
      participant_wave$timepoints_indiv = 1
      
      
      participant_wave$start_new = c(0)
      participant_wave$stop_new = c(1)
      
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    }
    
    if (nrow(participant_wave) ==2){
      
      participant_wave$timepoints_indiv = 2
      
      participant_wave$start_new = c(0, 1)
      participant_wave$stop_new = c(1, 2)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    }
    
    if (nrow(participant_wave)==3){
      
      participant_wave$timepoints_indiv = 3
      
      participant_wave$start_new = c(0, 1, 2)
      participant_wave$stop_new = c(1, 2, 3)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    } 
    
    
    if (nrow(participant_wave)==4){
      
      participant_wave$timepoints_indiv = 4
      
      participant_wave$start_new = c(0, 1, 2, 3)
      participant_wave$stop_new = c(1, 2, 3, 4)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    } 
    
    if (nrow(participant_wave)==5){
      
      participant_wave$timepoints_indiv = 5
      
      participant_wave$start_new = c(0, 1, 2, 3, 4)
      participant_wave$stop_new = c(1, 2, 3, 4, 5)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    } 
    
    if (nrow(participant_wave)==6){
      
      participant_wave$timepoints_indiv = 6
      participant_wave$start_new = c(0, 1, 2, 3, 4, 5)
      participant_wave$stop_new = c(1, 2, 3, 4, 5, 6)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    }
    n = n + 1
  }
  
  wave_1 = subset(participant_wave_df,  participant_wave_df$start_new == 0)
  wave_1_diabetes  = subset(wave_1, wave_1$diabetes_new == 1)
  
  
  diabetes_wave_1_unique = unique(wave_1_diabetes$HHIDPN)
  new_diabetes_wave1_dataset = length(diabetes_wave_1_unique)
  #wave 1: 224 cases 
  
  
  wave_2 = subset(participant_wave_df, participant_wave_df$start_new == 1)
  wave_2_diabetes  = subset(wave_2, wave_2$diabetes_new == 1)
  
  diabetes_wave_2_unique = unique(wave_2_diabetes$HHIDPN)
  new_diabetes_wave2_dataset = length(diabetes_wave_2_unique)
  #wave 2: 318 cases 
  
  
  wave_3 = subset(participant_wave_df, participant_wave_df$start_new == 2)
  wave_3_diabetes  = subset(wave_3, wave_3$diabetes_new == 1)
  
  
  diabetes_wave_3_unique = unique(wave_3_diabetes$HHIDPN)
  new_diabetes_wave3_dataset = length(diabetes_wave_3_unique)
  #wave 3: 233 cases 
  
  
  #wave_4 = subset(participant_wave_df, participant_wave_df$start_new == 3)
  #wave_4_diabetes  = subset(wave_4, wave_4$diabetes_new == 1)
  
  
  #diabetes_wave_4_unique = unique(wave_4_diabetes$HHIDPN)
  #new_diabetes_wave4_dataset = length(diabetes_wave_4_unique)
  #wave 4: 174 cases 
  
  
  #wave_5 = subset(participant_wave_df, participant_wave_df$start_new == 4)
  #wave_5_diabetes  = subset(wave_5, wave_5$diabetes_new == 1)
  
  
  #diabetes_wave_5_unique = unique(wave_5_diabetes$HHIDPN)
  #new_diabetes_wave5_dataset = length(diabetes_wave_5_unique)
  #
  
  #wave_6 = subset(participant_wave_df, participant_wave_df$start_new == 5)
  #wave_6_diabetes  = subset(wave_6, wave_6$diabetes_new == 1)
  
  
  #diabetes_wave_6_unique = unique(wave_6_diabetes$HHIDPN)
  #new_diabetes_wave6_dataset = length(diabetes_wave_6_unique)
  
  
  
  new_diabetes_each_wave = rbind(new_diabetes_wave2_dataset,
                                 new_diabetes_wave3_dataset) 
  #new_diabetes_wave4_dataset,
  #new_diabetes_wave5_dataset,
  #new_diabetes_wave6_dataset)
  
  #write.csv(new_diabetes_each_wave, (paste(OUTPUT_ROOT, "new_diabetes_each_wave_DIAB.csv", sep="")))
  
  all_waves = rbind(participant_wave_df) 
  #wave_4, 
  #wave_5, 
  #wave_6)
  
  
  
  return(participant_wave_df)
  
}


arranged_data = flow_chart_drop_baseline_diabetes(data = all_years)
nrow(arranged_data)

baseline_diabetes = subset(arranged_data, arranged_data$diabetes_new ==1 & arranged_data$start_new ==0) 
nrow(baseline_diabetes)

exclude_ids = unique(baseline_diabetes$HHIDPN)


subset_no_diabetes <- subset(arranged_data, !(HHIDPN %in% exclude_ids))
nrow(subset_no_diabetes)

subset_no_diabetes$start_new
subset_no_diabetes$stop_new
subset_no_diabetes$diabetes_new
subset_no_diabetes$RAHISPAN

all_hispanic = subset(subset_no_diabetes, subset_no_diabetes$RAHISPAN == 1)
all_hispanic_ids = unique(all_hispanic$HHIDPN)
length(all_hispanic_ids)

diabetes_hispanic = subset(subset_no_diabetes, subset_no_diabetes$diabetes_new == 1 & subset_no_diabetes$RAHISPAN == 1)  
diabetes_hispanic_ids = unique(diabetes_hispanic$HHIDPN)
length(diabetes_hispanic_ids)

no_diabetes_hispanic = subset(subset_no_diabetes, subset_no_diabetes$diabetes_new == 0 & subset_no_diabetes$RAHISPAN == 1)  
no_diabetes_hispanic_ids = unique(no_diabetes_hispanic$HHIDPN)
length(no_diabetes_hispanic_ids)

3116+9836


all_black = subset(subset_no_diabetes, subset_no_diabetes$RARACEM == 2)
all_black_ids = unique(all_black$HHIDPN)
length(all_black_ids)

diabetes_black = subset(subset_no_diabetes, subset_no_diabetes$diabetes_new == 1 & subset_no_diabetes$RARACEM == 2)  
diabetes_black_ids = unique(diabetes_black$HHIDPN)
length(diabetes_black_ids)

no_diabetes_black = subset(subset_no_diabetes, subset_no_diabetes$diabetes_new == 0 & subset_no_diabetes$RARACEM == 2)  
no_diabetes_black_ids = unique(no_diabetes_black$HHIDPN)
length(no_diabetes_black_ids)


3116+9836


#0.No degree 
#1.GED 
#2.HS 
#3.HS/GED 
#4.AA/ Lt BA 
#5.BA 
#6.MA/MBA 
#7.Law/MD/PhD 
#8.Other 

#For the descriptive stats, I suggest combining them as no degree (0), high_school/ (1-3), some college (4), undergraduate and above (5-7), and Other (8).
unique(subset_no_diabetes$RAEDEGRM)

no_degree = subset(subset_no_diabetes, subset_no_diabetes$RAEDEGRM == 0)
no_degree_ids = unique(no_degree$HHIDPN)
length(no_degree_ids)

diabetes_no_degree = subset(subset_no_diabetes, subset_no_diabetes$diabetes_new == 1 & subset_no_diabetes$RAEDEGRM == 0)  
diabetes_no_degree_ids = unique(diabetes_no_degree$HHIDPN)
length(diabetes_no_degree_ids)

no_diabetes_high_school = subset(subset_no_diabetes, subset_no_diabetes$diabetes_new == 0 & subset_no_diabetes$RAEDEGRM == 1)  
no_diabetes_high_school_ids = unique(no_diabetes_high_school$HHIDPN)
length(no_diabetes_high_school_ids)



some_college = subset(subset_no_diabetes, subset_no_diabetes$RAEDEGRM == 3)
some_college_ids = unique(some_college$HHIDPN)
length(some_college_ids)

diabetes_some_college = subset(subset_no_diabetes, subset_no_diabetes$diabetes_new == 1 & subset_no_diabetes$RAEDEGRM == 3)  
diabetes_some_college_ids = unique(diabetes_some_college$HHIDPN)
length(diabetes_some_college_ids)

no_diabetes_some_college = subset(subset_no_diabetes, subset_no_diabetes$diabetes_new == 0 & subset_no_diabetes$RAEDEGRM == 3)  
no_diabetes_some_college_ids = unique(no_diabetes_some_college$HHIDPN)
length(no_diabetes_some_college_ids)

undergraduate_and_above = subset(subset_no_diabetes, subset_no_diabetes$RAEDEGRM == 4)
undergraduate_and_above_ids = unique(undergraduate_and_above$HHIDPN)
length(undergraduate_and_above_ids)


diabetes_undergraduate_and_above = subset(subset_no_diabetes, subset_no_diabetes$diabetes_new == 1 & subset_no_diabetes$RAEDEGRM == 4)  
diabetes_undergraduate_and_above_ids = unique(diabetes_undergraduate_and_above$HHIDPN)
length(diabetes_undergraduate_and_above_ids)



###########

other_degree = subset(subset_no_diabetes, subset_no_diabetes$RAEDEGRM == 5)
other_degree_ids = unique(other_degree$HHIDPN)
length(other_degree_ids)


diabetes_other_degree = subset(subset_no_diabetes, subset_no_diabetes$diabetes_new == 1 & subset_no_diabetes$RAEDEGRM == 5)  
diabetes_other_degree_ids = unique(diabetes_other_degree$HHIDPN)
length(diabetes_other_degree_ids)

