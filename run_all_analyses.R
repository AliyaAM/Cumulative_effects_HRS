

library(WCE)
library(survival)
library(dplyr)
library(car)
library(tidyverse)
library(tidyr)

library(epiDisplay) #tab1 function to make a frequency table 
library(foreign)
library(rms) # Used to extract p-value from logistic model
library(ggplot2) #plots 
library(corrplot)
library(gridExtra) 
library(sjPlot)
library(knitr)
library(lme4)
library(lattice)
library(Hmisc)


## Set the root directory to look for source code.
#laptop: 
#/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/
#"/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"

SOURCE_data_ROOT = "/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"


SOURCE_ROOT =  "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/"
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/Cumulative_effects_HRS/Results/"

#source((paste(SOURCE_ROOT, "MAIN_cumulative_analysis_BMI.R", sep="")))
source((paste(SOURCE_ROOT, "MAIN_cumulative_analysis_FEMALE.R", sep="")))
source((paste(SOURCE_ROOT, "MAIN_cumulative_analysis_lim_cond.R", sep="")))
source((paste(SOURCE_ROOT, "MAIN_cumulative_analysis_national_origin.R", sep="")))
source((paste(SOURCE_ROOT, "MAIN_cumulative_analysis_race.R", sep="")))
source((paste(SOURCE_ROOT, "MAIN_cumulative_anaysis_religion.R", sep="")))


