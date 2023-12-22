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

add_hispanic = read.csv("/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/randhrs1992_2018v1.csv")
unique(add_hispanic$HHIDPN)

nrow(add_hispanic)
add_hispanic$RAHISPAN

#hispanic_data <- read.csv("path_to_hispanic_data.csv")
#cumulative_effects_dat_initial <- merge(cumulative_effects_dat_initial, hispanic_data, by = "common_identifier")

# Merging the two dataframes
cumulative_effects_dat_initial <- merge(cumulative_effects_dat_initial, add_hispanic[c("HHIDPN", "RAHISPAN")], by = "HHIDPN", all.x = TRUE)

# Checking the first few rows of the updated dataframe
head(cumulative_effects_dat_initial)


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

start_new = c(baseline_data_group_developed_diabetes$start_new, 
              baseline_data_group_did_not_develop_diabetes$start_new)

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

hispanic = c(baseline_data_group_developed_diabetes$RAHISPAN, 
             baseline_data_group_did_not_develop_diabetes$RAHISPAN)

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
                           start_new, 
                           developed_diabetes, # whether developed diabetes or not during the span of the study 
                        age, 
                        race,
                        hispanic, 
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


# Ensuring merged data integrity
if (any(duplicated(data_compared$HHIDPN))) {
  stop("Duplicate IDs found in merged diabetes data.")
}



# Find duplicated rows
duplicated_rows <- duplicated(data_compared$HHIDPN) | duplicated(data_compared$HHIDPN, fromLast = TRUE)

# Print all duplicated rows
print(data_compared[duplicated_rows, ])

# dublicates cases: 79+1710 

# Keep only the rows where HHIDPN is not duplicated
data_compared <- data_compared[!duplicated(data_compared$HHIDPN), ]


#ensure that the variables are in the correct format 

nrow(data_compared)
head(data_compared)

data_compared$developed_diabetes = as.factor(data_compared$developed_diabetes)
data_compared$age = as.double(data_compared$age)
data_compared$sex = as.factor(data_compared$sex)
data_compared$race = as.factor(data_compared$race)
data_compared$hispanic = as.factor(data_compared$hispanic)
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
                "hispanic",
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

dependent = c("developed_diabetes")


data_compared$race <- droplevels(data_compared$race)
data_compared$hispanic = droplevels(data_compared$hispanic)
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


data_compared_v2 <- data_compared

data_compared_v2$HHIDPN = data_compared$HHIDPN

data_compared_v2$start_new = data_compared$start_new

data_compared_v2$developed_diabetes = as.factor(data_compared$developed_diabetes)
data_compared_v2$age = as.double(data_compared$age)
data_compared_v2$sex = as.factor(data_compared$sex)
data_compared_v2$race = as.factor(data_compared$race)
data_compared_v2$hispanic = as.factor(data_compared$hispanic)
data_compared_v2$BMI = as.double(data_compared$BMI)
data_compared_v2$education = as.factor(data_compared$education)
data_compared_v2$depression = as.factor(data_compared$depression)
data_compared_v2$hypertension = as.factor(data_compared$hypertension)
data_compared_v2$Alcohol_consumption = as.double(data_compared$Alcohol_consumption)
data_compared_v2$Smoking_status = as.factor(data_compared$Smoking_status)
data_compared_v2$MVPA = as.double(data_compared$MVPA)
data_compared_v2$wealth = as.factor(data_compared$wealth) # wealth quantiles



###### Participant table: 

# Explanatory or confounding variables
explanatory = c("age",
                "race",
                "hispanic",
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

dependent = c("developed_diabetes")


data_compared_v2$race <- droplevels(data_compared_v2$race)
data_compared_v2$hispanic = droplevels(data_compared_v2$hispanic)
data_compared_v2$sex <- droplevels(data_compared_v2$sex)
data_compared_v2$education <- droplevels(data_compared_v2$education)
data_compared_v2$hypertension <- droplevels(data_compared_v2$hypertension)
data_compared_v2$depression <- droplevels(data_compared_v2$depression)
data_compared_v2$Smoking_status <- droplevels(data_compared_v2$Smoking_status)
data_compared_v2$wealth <- droplevels(data_compared_v2$wealth)


summary(data_compared_v2)
str(data_compared_v2)


# Generating summary table for participant characteristics
table1_final_v2 = data_compared_v2 %>%
  summary_factorlist(dependent, explanatory, p = TRUE, na_include = TRUE,
                     total_col = TRUE,
                     add_row_total = TRUE) -> t

write.csv(table1_final_v2, file = paste(OUTPUT_ROOT, "table1_final_v2.csv", sep = ""))

##### more detailed table: 

#### separately report the total number of participants for each time points, also include the participant characteristics at baseline for those who were lost to follow-up and complete cases 
#### add three tables: baseline participant characteristics for the included sample,participant characteristics for those lost to follow-up, the characteristics of the total final sample per each variable")


# Ensure 'start_new' is numeric
# Ensure 'start_new' is numeric
data_compared_v2$start_new <- as.numeric(as.character(data_compared_v2$start_new))

# Identify baseline participants
baseline_participants <- unique(subset(data_compared_v2, start_new == 0)$HHIDPN)

# Function to check if participants are present in follow-up
check_presence_in_followup <- function(time_point, baseline_ids) {
  followup_ids <- unique(subset(data_compared_v2, start_new == time_point)$HHIDPN)
  absent_ids <- setdiff(baseline_ids, followup_ids)
  return(absent_ids)
}

# Checking for each follow-up
lost_to_followup <- lapply(1:max(data_compared_v2$start_new), check_presence_in_followup, baseline_participants)
all_lost_ids <- unique(unlist(lost_to_followup))

# Extract Baseline Characteristics of Lost Participants
lost_baseline_data <- subset(data_compared_v2, HHIDPN %in% all_lost_ids & start_new == 0)

# Adjust 'dependent' to be a single string, not a vector
dependent = "developed_diabetes"

# Create the Summary Table
lost_to_followup_summary <- summary_factorlist(lost_baseline_data, explanatory, dependent, p = TRUE)

# View or save the summary table
print(lost_to_followup_summary)


# Assign 'dependent' as a single string directly
dependent <- "developed_diabetes"

# Re-check the structure of 'dependent'
print(paste("Type of 'dependent':", class(dependent)))
print(paste("Length of 'dependent':", length(dependent)))

# Create the Summary Table
lost_to_followup_summary <- summary_factorlist(lost_baseline_data, explanatory, dependent, p = TRUE)

# View or save the summary table
print(lost_to_followup_summary)



# 
# 
# 
# # Create a table of counts of follow-up visits for each participant
# followup_counts <- table(data_compared$HHIDPN)
# 
# # IDs of participants who attended more than one follow-up
# ids_more_than_one_followup <- names(followup_counts[followup_counts > 1])
# 
# # IDs of participants who attended only the baseline (no follow-ups)
# ids_only_one_followup <- names(followup_counts[followup_counts == 1])
# 
# # Generating a list of IDs for each specific follow-up
# ids_followup_f <- lapply(0:5, function(f) {
#   unique(data_compared$HHIDPN[data_compared$start_new == f])
# })
# 
# # If you want to see the IDs for a specific follow-up, you can access them like this:
# # ids_followup_1 <- ids_followup_f[[2]] # For follow-up 1 (index 2 because indexing starts at 1 in R and 0 represents the baseline)
# 
# 
# # Counting the number of participants at each follow-up
# participants_per_followup <- sapply(0:5, function(f) {
#   sum(data_compared$start_new == f)
# })
# print("Number of participants at each follow-up:")
# print(participants_per_followup)
# 
# # Separating participants based on number of follow-ups
# baseline_more_than_one_followup <- subset(data_compared, start_new == 0 & HHIDPN %in% ids_more_than_one_followup)
# baseline_only_one_followup <- subset(data_compared, start_new == 0 & HHIDPN %in% ids_only_one_followup)
# 
# # Create function to generate participant characteristics table
# generate_participant_table <- function(data, variables) {
#   summary_factorlist(dependent, variables, data = data, p = TRUE, na_include = TRUE,
#                      total_col = TRUE, add_row_total = TRUE)
# }
# 
# # Baseline participant characteristics for the included sample
# 
# table1 <- generate_participant_table(data_compared[data_compared$start_new == 0, ], explanatory)
# 
# # Characteristics of participants lost to each follow-up
# # head(baseline_more_than_one_followup)
# 
# baseline_more_than_one_followup$developed_diabetes = as.factor(baseline_more_than_one_followup$developed_diabetes)
# baseline_more_than_one_followup$age = as.double(baseline_more_than_one_followup$age)
# baseline_more_than_one_followup$sex = as.factor(baseline_more_than_one_followup$sex)
# baseline_more_than_one_followup$race = as.factor(baseline_more_than_one_followup$race)
# baseline_more_than_one_followup$hispanic = as.factor(baseline_more_than_one_followup$hispanic)
# baseline_more_than_one_followup$BMI = as.double(baseline_more_than_one_followup$BMI)
# baseline_more_than_one_followup$education = as.factor(baseline_more_than_one_followup$education)
# baseline_more_than_one_followup$depression = as.factor(baseline_more_than_one_followup$depression)
# baseline_more_than_one_followup$hypertension = as.factor(baseline_more_than_one_followup$hypertension)
# baseline_more_than_one_followup$Alcohol_consumption = as.factor(baseline_more_than_one_followup$Alcohol_consumption)
# baseline_more_than_one_followup$Smoking_status = as.factor(baseline_more_than_one_followup$Smoking_status)
# baseline_more_than_one_followup$MVPA = as.factor(baseline_more_than_one_followup$MVPA)
# baseline_more_than_one_followup$wealth = as.factor(baseline_more_than_one_followup$wealth) # wealth quantiles
# 
# 
# 
# baseline_more_than_one_followup$race <- droplevels(baseline_more_than_one_followup$race)
# baseline_more_than_one_followup$hispanic = droplevels(baseline_more_than_one_followup$hispanic)
# baseline_more_than_one_followup$sex <- droplevels(baseline_more_than_one_followup$sex)
# baseline_more_than_one_followup$education <- droplevels(baseline_more_than_one_followup$education)
# baseline_more_than_one_followup$hypertension <- droplevels(baseline_more_than_one_followup$hypertension)
# baseline_more_than_one_followup$depression <- droplevels(baseline_more_than_one_followup$depression)
# baseline_more_than_one_followup$Alcohol_consumption <- droplevels(baseline_more_than_one_followup$Alcohol_consumption)
# baseline_more_than_one_followup$Smoking_status <- droplevels(baseline_more_than_one_followup$Smoking_status)
# baseline_more_than_one_followup$MVPA <- droplevels(baseline_more_than_one_followup$MVPA)
# baseline_more_than_one_followup$wealth <- droplevels(baseline_more_than_one_followup$wealth)
# 
# summary(baseline_more_than_one_followup)
# str(baseline_more_than_one_followup)
# 
# table1_more_than_one_followup = baseline_more_than_one_followup %>%
#   summary_factorlist(dependent, explanatory, p = TRUE, na_include = TRUE,
#                      total_col = TRUE,
#                      add_row_total = TRUE) -> t
# 
# 
# table1_more_than_one_followup <- generate_participant_table(data = baseline_more_than_one_followup,variables = explanatory)
# 
# 
# table1_baseline_only_one_followup = baseline_only_one_followup %>%
#   summary_factorlist(dependent, explanatory, p = TRUE, na_include = TRUE,
#                      total_col = TRUE,
#                      add_row_total = TRUE) -> t
# # Characteristics of the total final sample per each variable
# table3 <- generate_participant_table(analytical_sample_COX, explanatory)
# 
# # Exporting the tables
# write.csv(table1, file = paste(OUTPUT_ROOT, "table1_baseline_characteristics.csv", sep = ""))
# write.csv(table1_baseline_only_one_followup, file = paste(OUTPUT_ROOT, "table2_lost_to_followup.csv", sep = ""))
# write.csv(table1_more_than_one_followup, file = paste(OUTPUT_ROOT, "table3_final_sample_characteristics.csv", sep = ""))
# 
# 
# print("separately report the total number of participants for each time points, also include the participant characteristics at baseline for those who had more than 1 timepoint and for those who were in the study for the baseline only")
# 
# participants_per_followup <- table(analytical_sample_COX$start_new)
# 
# 
# print("add three tables: baseline participant characteristics for the included sample,participant characteristics for those who were lost to the first follow-up, lost to the second follow-up, lost to the third follow-up, the characteristics of the total final sample per each variable")
# 15754-1789
