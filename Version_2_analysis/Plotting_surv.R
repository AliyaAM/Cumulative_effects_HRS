
library("survival")
library("survminer")
library("dplyr")


  
cumulative_effects_dat = read.csv("/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/DATA_FOR_PLOT/all_waves_nodiabatbaseline_DIAB.csv")

#outcome:  cumulative_effects_dat$diabetes_new_bin

#exposure: cumulative_effects_dat$discrim_bin

cumulative_effects_dat$discrimination = cumulative_effects_dat$discrim_bin

#time: cumulative_effects_dat$stop_new

#cumulative_effects_dat$sex_1_2

#cumulative_effects_dat$race_white

#cumulative_effects_dat$race_white
#cumulative_effects_dat$national_origin_ousideUS_bin
#cumulative_effects_dat$religion_bin


#cumulative_effects_dat$assessed_BMI

cumulative_effects_dat$years = 2 *cumulative_effects_dat$timepoints_indiv

#1 = 2 year 
#2 = 4 years 
#3 = 6 years 

unique(cumulative_effects_dat$timepoints_indiv)
unique(cumulative_effects_dat$start_new)

cumulative_effects_dat$diabetes_new_bin_reversed = case_when(cumulative_effects_dat$diabetes_new_bin == 1 ~ 0, 
                                                       cumulative_effects_dat$diabetes_new_bin == 0 ~ 1) 

fit <- survfit(Surv(years, diabetes_new_bin_reversed) ~ discrimination, data = cumulative_effects_dat)



plot_all = ggsurvplot(fit,
           pval = TRUE, conf.int = TRUE,
           risk.table = FALSE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))

print(plot_all)  