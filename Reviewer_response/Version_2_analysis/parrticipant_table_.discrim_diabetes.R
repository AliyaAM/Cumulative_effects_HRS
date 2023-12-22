# Load necessary packages
library("dplyr")
library("tidyr")
library("ggplot2")
library("stats")
library("summarytools")
# library("devtools") # Uncomment if devtools is required
# install.packages("devtools") # Uncomment if devtools isn't installed
# devtools::install_github("ewenharrison/finalfit") # Uncomment if finalfit isn't installed
library("finalfit")

# Setting up directories for data storage and analysis
current_directory <- "/Users/aliyaamirova/proj/Cumulative_effects_HRS"
OUTPUT_ROOT <- paste(current_directory, "/Reviewer_response/Version_2_analysis/RESULTS/", sep="")
SOURCE_ROOT <- paste(current_directory, "/Reviewer_response/Version_2_analysis/", sep="")
DATA_ROOT <- paste(current_directory, "/ELSA_HRS/Data_analysis/", sep = "") 



# Load and preprocess data
cumulative_effects_dat_initial <- read.csv(paste(current_directory, "/data_files/data_flow_chart_withoutbaselineCVD.csv", sep=""))

head(cumulative_effects_dat_initial)


print("add hispanic as a variable in a different file, redo the flow chart excluding baseline cvd and diabetes")

#hispanic_data <- read.csv("path_to_hispanic_data.csv")
#cumulative_effects_dat_initial <- merge(cumulative_effects_dat_initial, hispanic_data, by = "common_identifier")




# Check for unique IDs and missing values
print(paste("Number of unique IDs:", length(unique(cumulative_effects_dat_initial$HHIDPN))))

# Subset for participants diagnosed with diabetes (diabetes_new = 1)
diabetes_subset <- subset(cumulative_effects_dat_initial, diabetes_new == 1)
# Print the number of unique IDs in this subset
print(paste("Number of unique IDs with diabetes:", length(unique(diabetes_subset$HHIDPN))))

# Subset for participants not diagnosed with diabetes (diabetes_new = 0)
non_diabetes_subset <- subset(cumulative_effects_dat_initial,  diabetes_new == 0)
# Print the number of unique IDs in this subset
print(paste("Number of unique IDs without diabetes:", length(unique(non_diabetes_subset$HHIDPN))))

# CVD status recoding
cumulative_effects_dat_initial$CVD <- with(cumulative_effects_dat_initial, ifelse(angina_new_bin == 1 | heartfailure2yrs_bin == 1 | heartattack_ever_bin == 1 | heartattack_new_bin == 1, 1, 0))
cumulative_effects_dat_initial$CVD_ever <- with(cumulative_effects_dat_initial, ifelse(heartfailure2yrs_bin == 1 | heartattack_ever_bin == 1, 1, 0))

# Ensuring that all categories are covered in the recoding
if (any(is.na(cumulative_effects_dat_initial$CVD))) {
  warning("There are NAs in the CVD variable.")
}


# Exclude participants with cardiometabolic disease at baseline
cases_with_CVD <- subset(cumulative_effects_dat_initial, CVD_ever == 1 & start_new == 0)
exclude_ids <- unique(cases_with_CVD$HHIDPN)
analytical_sample_COX <- subset(cumulative_effects_dat_initial, !(HHIDPN %in% exclude_ids))
print(paste("Number of unique IDs:", length(unique(analytical_sample_COX$HHIDPN))))


# Visualizing Age Distribution
ggplot(analytical_sample_COX, aes(x = continious_age)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Age Distribution in the Analytical Sample", x = "Age", y = "Frequency")


# Data Visualization: Distribution of BMI
ggplot(analytical_sample_COX, aes(x = assessed_BMI)) +
  geom_density(fill = "green", alpha = 0.5) +
  theme_minimal() +
  labs(title = "BMI Distribution in the Analytical Sample", x = "BMI", y = "Density")


# Function to create wealth quantiles
create_wealth_quantiles <- function(wealth_data) {
  quantiles <- quantile(wealth_data, probs = seq(0, 1, by = 0.25), na.rm = TRUE)
  cut(wealth_data, breaks = quantiles, include.lowest = TRUE, labels = FALSE)
}

# create wealth quantile variable

analytical_sample_COX$wealth_quantile <- as.factor(create_wealth_quantiles(analytical_sample_COX$wealth_noIRA))





# Subsetting data for two  diabetes statuses at each follow-up

diabetes_baseline = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 1 & analytical_sample_COX$start_new == 0) 
nrow(diabetes_baseline) # this should be 0, as we are excluding people who had diabetes at the baseline 
diabetes_followup_1 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 1 & analytical_sample_COX$start_new == 1) 
diabetes_followup_2 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 1 & analytical_sample_COX$start_new == 2) 
diabetes_followup_3 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 1 & analytical_sample_COX$start_new == 3) 
diabetes_followup_4 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 1 & analytical_sample_COX$start_new == 4)
diabetes_followup_5 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 1 & analytical_sample_COX$start_new == 5)

non_diabetes_baseline = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 0) 
non_diabetes_followup_1 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 1) 
non_diabetes_followup_2 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 2) 
non_diabetes_followup_3 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 3) 
non_diabetes_followup_4 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 4)
non_diabetes_followup_5 = subset(analytical_sample_COX, analytical_sample_COX$diabetes_new == 0 & analytical_sample_COX$start_new == 5)




# Merging datasets (those who developed diabetes at some point, and those who did not develop diabetes)
diabetes_data_merged <- rbind(diabetes_followup_1, 
                              diabetes_followup_2, 
                              diabetes_followup_3,
                              diabetes_followup_4, 
                              diabetes_followup_5)

non_diabetes_data_merged <- rbind(non_diabetes_baseline,
                                  non_diabetes_followup_1,
                                  non_diabetes_followup_2, 
                                  non_diabetes_followup_3, 
                                  non_diabetes_followup_4, 
                                  non_diabetes_followup_5)


non_diabetes_data_merged_no_baseline <- rbind(non_diabetes_followup_1,
                                              non_diabetes_followup_2, 
                                              non_diabetes_followup_3, 
                                              non_diabetes_followup_4, 
                                              non_diabetes_followup_5)



#unique ids of people who developed diabetes and who did not: 
ids_developed_diabetes <- unique(diabetes_data_merged$HHIDPN)
ids_did_not_develop_diabetes <- unique(non_diabetes_data_merged$HHIDPN)

# include only those with follow-up data 
length(unique(non_diabetes_data_merged_no_baseline$HHIDPN)) #8561 

# Subset baseline data (ie., start_new = 0) into two groups: developed diabetes and did not: 
baseline_data_group_developed_diabetes <- subset(analytical_sample_COX, analytical_sample_COX$start_new == 0 & HHIDPN %in% ids_developed_diabetes)
baseline_data_group_did_not_develop_diabetes <- subset(analytical_sample_COX, analytical_sample_COX$start_new == 0 & HHIDPN %in% ids_did_not_develop_diabetes)

# data checking: 
head(baseline_data_group_developed_diabetes)
#number of people who never developed diabetes during th follow-up 
length(ids_did_not_develop_diabetes)
nrow(baseline_data_group_did_not_develop_diabetes)

#number of people who developed diabetes at some point during follow-up
length(ids_developed_diabetes)
nrow(baseline_data_group_developed_diabetes)


# assign values to new variable describing whether ind developed diabetes during the span of the study or not: 
baseline_data_group_developed_diabetes$developed_diabetes = rep(1, times = nrow(baseline_data_group_developed_diabetes))
baseline_data_group_did_not_develop_diabetes$developed_diabetes = rep(0, times = nrow(baseline_data_group_did_not_develop_diabetes)) 
nrow(baseline_data_group_did_not_develop_diabetes)


#create new clean data frame: 
developed_diabetes = c(baseline_data_group_developed_diabetes$developed_diabetes,
         baseline_data_group_did_not_develop_diabetes$developed_diabetes)

unique(developed_diabetes)


HHIDPN = c(baseline_data_group_developed_diabetes$HHIDPN, 
          baseline_data_group_did_not_develop_diabetes$HHIDPN)

education = c(baseline_data_group_developed_diabetes$education_level , 
              baseline_data_group_did_not_develop_diabetes$education_level )

wealth = c(baseline_data_group_developed_diabetes$wealth_quantile , 
           baseline_data_group_did_not_develop_diabetes$wealth_quantile ) 

age = c(baseline_data_group_developed_diabetes$continious_age , 
        baseline_data_group_did_not_develop_diabetes$continious_age )

sex = c(baseline_data_group_developed_diabetes$sex_1_2 , 
        baseline_data_group_did_not_develop_diabetes$sex_1_2 )


race = c(baseline_data_group_developed_diabetes$race_white , 
         baseline_data_group_did_not_develop_diabetes$race_white )

#BMI kg/m2, mean (SD)

BMI = c(baseline_data_group_developed_diabetes$assessed_BMI , 
        baseline_data_group_did_not_develop_diabetes$assessed_BMI )


#Hypertension, n (%)

hypertension = c(baseline_data_group_developed_diabetes$hypertension_new_bin , 
                 baseline_data_group_did_not_develop_diabetes$hypertension_new_bin )


#Depression, n (%)

depression = c(baseline_data_group_developed_diabetes$checklist_depression_bin , 
               baseline_data_group_did_not_develop_diabetes$checklist_depression_bin )


#Alcohol consumption (days/week), Mean (SD)

Alcohol_consumption  = c(baseline_data_group_developed_diabetes$alcohol_days_week , 
                         baseline_data_group_did_not_develop_diabetes$alcohol_days_week )

Alcohol_consumption = as.numeric(Alcohol_consumption)

#Smoker status, n (%)


baseline_data_group_did_not_develop_diabetes$smokes_now_bin <- ifelse(
  is.na(baseline_data_group_did_not_develop_diabetes$smokes_now_bin), 
  0, 
  baseline_data_group_did_not_develop_diabetes$smokes_now_bin
)

baseline_data_group_developed_diabetes$smokes_now_bin <- ifelse(
  is.na(baseline_data_group_developed_diabetes$smokes_now_bin), 
  0, 
  baseline_data_group_developed_diabetes$smokes_now_bin
)


Smoking_status  = c(baseline_data_group_developed_diabetes$smokes_now_bin , 
                    baseline_data_group_did_not_develop_diabetes$smokes_now_bin )



#MVPA frequency, median
MVPA  = c(baseline_data_group_developed_diabetes$vigarious_physical_activity , 
          baseline_data_group_did_not_develop_diabetes$vigarious_physical_activity )


data_compared = data.frame(HHIDPN, 
                           developed_diabetes, # whether developed diabetes or not during the span of the study 
                        age, 
                        race, 
                        sex, 
                        BMI,
                        education,
                        #Hypertension, n (%)
                        hypertension, 
                        #Depression, n (%)
                        depression,
                        #Alcohol consumption (days/week), Mean (SD)
                        Alcohol_consumption,  
                        #Smoker status, n (%)
                        Smoking_status,
                        MVPA,
                        wealth)

nrow(data_compared)
head(data_compared)
unique(data_compared$case)
table(data_compared$case)


# Ensuring merged data integrity
if (any(duplicated(data_compared$HHIDPN))) {
  stop("Duplicate IDs found in merged diabetes data.")
}


#ensure that the variables are in the correct format 
data_compared$developed_diabetes = as.factor(data_compared$developed_diabetes)

nrow(data_compared)
head(data_compared)

data_compared$developed_diabetes = as.factor(data_compared$developed_diabetes)
data_compared$age = as.double(data_compared$age)
data_compared$sex = as.factor(data_compared$sex)
data_compared$race = as.factor(data_compared$race)
data_compared$BMI = as.double(data_compared$BMI)
data_compared$education = as.factor(data_compared$education)
data_compared$depression = as.factor(data_compared$depression)
data_compared$hypertension = as.factor(data_compared$hypertension)
data_compared$Alcohol_consumption = as.factor(data_compared$Alcohol_consumption)
data_compared$Smoking_status = as.factor(data_compared$Smoking_status)
data_compared$MVPA = as.factor(data_compared$MVPA)
data_compared$wealth = as.factor(data_compared$wealth) # wealth quantiles



summary(data_compared)
str(data_compared)

unique(data_compared$developed_diabetes)
unique(data_compared$age)
unique(race)
unique(sex)
unique(BMI)
unique(education)
unique(hypertension)
unique(depression)
unique(Alcohol_consumption)
unique(MVPA)
unique(wealth)

###### Participant table: 

# Explanatory or confounding variables
explanatory = c("age", 
                "race", 
                "sex", 
                "BMI",
                "education",
                "hypertension", 
                "depression",
                "Alcohol_consumption",  
                "Smoking_status",
                "MVPA",
                "wealth") 
# 

dependent = "developed_diabetes" 


data_compared$race <- droplevels(data_compared$race)
data_compared$sex <- droplevels(data_compared$sex)
data_compared$education <- droplevels(data_compared$education)
data_compared$hypertension <- droplevels(data_compared$hypertension)
data_compared$depression <- droplevels(data_compared$depression)
data_compared$Alcohol_consumption <- droplevels(data_compared$Alcohol_consumption)
data_compared$Smoking_status <- droplevels(data_compared$Smoking_status)
data_compared$MVPA <- droplevels(data_compared$MVPA)
data_compared$wealth <- droplevels(wealth)


summary(data_compared)
str(data_compared)


# Generating summary table for participant characteristics
table1_final = data_compared %>%
  summary_factorlist(dependent, explanatory, p = TRUE, na_include = TRUE,
                     total_col = TRUE,
                     add_row_total = TRUE) -> t

write.csv(table1_final, file = paste(OUTPUT_ROOT, "table1_final.csv", sep = ""))

##### more detailed table: 

#### separately report the total number of participants for each time points, also include the participant characteristics at baseline for those who had more than 1 timepoint and for those who were in the study for the baseline only")
#### add three tables: baseline participant characteristics for the included sample,participant characteristics for those who were lost to the first follow-up, lost to the second follow-up, lost to the third follow-up, the characteristics of the total final sample per each variable")

# Assuming your dataset is named analytical_sample_COX and has columns HHIDPN (participant ID) and start_new (indicating follow-up time)

# Create a table of counts of follow-up visits for each participant
followup_counts <- table(data_compared$HHIDPN)

# IDs of participants who attended more than one follow-up
ids_more_than_one_followup <- names(followup_counts[followup_counts > 1])

# IDs of participants who attended only the baseline (no follow-ups)
ids_only_one_followup <- names(followup_counts[followup_counts == 1])

# Generating a list of IDs for each specific follow-up
ids_followup_f <- lapply(0:5, function(f) {
  unique(data_compared$HHIDPN[data_compared$start_new == f])
})

# If you want to see the IDs for a specific follow-up, you can access them like this:
# ids_followup_1 <- ids_followup_f[[2]] # For follow-up 1 (index 2 because indexing starts at 1 in R and 0 represents the baseline)


# Counting the number of participants at each follow-up
participants_per_followup <- sapply(0:5, function(f) {
  sum(data_compared$start_new == f)
})
print("Number of participants at each follow-up:")
print(participants_per_followup)

# Separating participants based on number of follow-ups
baseline_more_than_one_followup <- subset(data_compared, start_new == 0 & HHIDPN %in% ids_more_than_one_followup)
baseline_only_one_followup <- subset(data_compared, start_new == 0 & HHIDPN %in% ids_only_one_followup)

# Create function to generate participant characteristics table
generate_participant_table <- function(data, variables) {
  summary_factorlist(dependent, variables, data = data, p = TRUE, na_include = TRUE,
                     total_col = TRUE, add_row_total = TRUE)
}

# Baseline participant characteristics for the included sample
table1 <- generate_participant_table(data_compared[data_compared$start_new == 0, ], explanatory)

# Characteristics of participants lost to each follow-up
f
table1_more_than_one_followup = baseline_more_than_one_followup %>%
  summary_factorlist(dependent, explanatory, p = TRUE, na_include = TRUE,
                     total_col = TRUE,
                     add_row_total = TRUE) -> t




table1_more_than_one_followup = baseline_more_than_one_followup %>%
  summary_factorlist(dependent, explanatory, p = TRUE, na_include = TRUE,
                     total_col = TRUE,
                     add_row_total = TRUE) -> t
# Characteristics of the total final sample per each variable
table3 <- generate_participant_table(analytical_sample_COX, explanatory)

# Exporting the tables
write.csv(table1, file = paste(OUTPUT_ROOT, "table1_baseline_characteristics.csv", sep = ""))
write.csv(table2, file = paste(OUTPUT_ROOT, "table2_lost_to_followup.csv", sep = ""))
write.csv(table3, file = paste(OUTPUT_ROOT, "table3_final_sample_characteristics.csv", sep = ""))


print("separately report the total number of participants for each time points, also include the participant characteristics at baseline for those who had more than 1 timepoint and for those who were in the study for the baseline only")

participants_per_followup <- table(analytical_sample_COX$start_new)


print("add three tables: baseline participant characteristics for the included sample,participant characteristics for those who were lost to the first follow-up, lost to the second follow-up, lost to the third follow-up, the characteristics of the total final sample per each variable")

