
# plots: 
# https://adibender.github.io/pammtools/articles/cumulative_effects.html
#https://rpkgs.datanovia.com/ggpubr/reference/stat_regline_equation.html Adding text (coefficients etc) to the plot next to the regression line 

current_directory = "/Users/k2147340/OneDrive - King's College London/Documents/"

#current_directory = "/Users/aliya/my_docs/"
#current_directory = "/Users/aliyaamirova/proj/Cumulative_effects_HRS"

OUTPUT_ROOT = paste(current_directory, "proj/Cumulative_effects_HRS/Reviewer_response/Version_2_analysis/RESULTS/", sep="")
SOURCE_ROOT = paste(current_directory, "proj/Cumulative_effects_HRS/Reviewer_response/Version_2_analysis/", sep="")
DATA_ROOT = paste(current_directory, "/ELSA_HRS/Data_analysis/", sep = "") 



library("survival")
library("survminer")
library("dplyr")
library("ggplot2")


###### DATA:
# below is the entire dataset, not subseted to anyone: 

cumulative_effects_dat_initial = read.csv(paste(OUTPUT_ROOT, "all_waves_nodiabatbaseline_DIAB.csv", sep =""))
nrow(cumulative_effects_dat_initial)


#exclude participants with cardiometabolic disease at baseline: 

cases_with_CVD = subset(cumulative_effects_dat_initial,  CVD_ever == 1 & start_new == 0)

exclude_ids = unique(cases_with_CVD$HHIDPN)


cumulative_effects_dat <- subset(cumulative_effects_dat_initial,  !(HHIDPN %in% exclude_ids))

 
unique(cumulative_effects_dat$CVD_ever)
unique(cumulative_effects_dat_initial$start_new)
nrow(cumulative_effects_dat)
nrow(cumulative_effects_dat_initial)



###### Adding variables to the main dataset:

#outcome:  cumulative_effects_dat$diabetes_new_bin
#exposure: cumulative_effects_dat$discrim_bin
#time: cumulative_effects_dat$stop_new

###### Adding variables to the main dataset:
###### Adding variables to the main dataset:



cumulative_effects_dat$discrim_harassed_bin = case_when(cumulative_effects_dat$discrim_harassed == 1 ~ 1, 
                                                        cumulative_effects_dat$discrim_harassed == 2 ~ 1, 
                                                        cumulative_effects_dat$discrim_harassed == 3 ~ 1, 
                                                        cumulative_effects_dat$discrim_harassed == 4 ~ 1, 
                                                        cumulative_effects_dat$discrim_harassed == 5 ~ 0, 
                                                        cumulative_effects_dat$discrim_harassed == 6 ~ 0,
                                                        cumulative_effects_dat$discrim_harassed == 0 ~ 0) 



cumulative_effects_dat$discrim_lessrespect_bin = case_when(cumulative_effects_dat$discrim_lessrespect == 1 ~ 1, 
                                                           cumulative_effects_dat$discrim_lessrespect == 2 ~ 1, 
                                                           cumulative_effects_dat$discrim_lessrespect == 3 ~ 1, 
                                                           cumulative_effects_dat$discrim_lessrespect == 4 ~ 1, 
                                                           cumulative_effects_dat$discrim_lessrespect == 5 ~ 0, 
                                                           cumulative_effects_dat$discrim_lessrespect == 6 ~ 0,
                                                           cumulative_effects_dat$discrim_lessrespect == 0 ~ 0) 



cumulative_effects_dat$discrim_medical_bin = case_when(cumulative_effects_dat$discrim_medical == 1 ~ 1, 
                                                       cumulative_effects_dat$discrim_medical == 2 ~ 1, 
                                                       cumulative_effects_dat$discrim_medical == 3 ~ 1, 
                                                       cumulative_effects_dat$discrim_medical == 4 ~ 1, 
                                                       cumulative_effects_dat$discrim_medical == 5 ~ 0, 
                                                       cumulative_effects_dat$discrim_medical == 6 ~ 0,
                                                       cumulative_effects_dat$discrim_medical == 0 ~ 0) 





cumulative_effects_dat$discrim_notclever_bin = case_when(cumulative_effects_dat$discrim_notclever == 1 ~ 1, 
                                                         cumulative_effects_dat$discrim_notclever == 2 ~ 1, 
                                                         cumulative_effects_dat$discrim_notclever == 3 ~ 1, 
                                                         cumulative_effects_dat$discrim_notclever == 4 ~ 1, 
                                                         cumulative_effects_dat$discrim_notclever == 5 ~ 0, 
                                                         cumulative_effects_dat$discrim_notclever == 6 ~ 0,
                                                         cumulative_effects_dat$discrim_notclever == 0 ~ 0) 






cumulative_effects_dat$discrim_poorerservice_bin = case_when(cumulative_effects_dat$discrim_poorerservice == 1 ~ 1, 
                                                             cumulative_effects_dat$discrim_poorerservice == 2 ~ 1, 
                                                             cumulative_effects_dat$discrim_poorerservice == 3 ~ 1, 
                                                             cumulative_effects_dat$discrim_poorerservice == 4 ~ 1, 
                                                             cumulative_effects_dat$discrim_poorerservice == 5 ~ 0, 
                                                             cumulative_effects_dat$discrim_poorerservice == 6 ~ 0) 




cumulative_effects_dat$discrim_afraidothers_bin = case_when(cumulative_effects_dat$discrim_afraidothers == 1 ~ 1,
                                                            cumulative_effects_dat$discrim_afraidothers == 2 ~ 1, 
                                                            cumulative_effects_dat$discrim_afraidothers == 3 ~ 1, 
                                                            cumulative_effects_dat$discrim_afraidothers == 4 ~ 1, 
                                                            cumulative_effects_dat$discrim_afraidothers == 5 ~ 0, 
                                                            cumulative_effects_dat$discrim_afraidothers == 6 ~ 0,
                                                            cumulative_effects_dat$discrim_afraidothers == 0 ~ 0) 



cumulative_effects_dat$discrim_bin = case_when(cumulative_effects_dat$discrim_harassed_bin == 1 | cumulative_effects_dat$discrim_lessrespect_bin == 1 | cumulative_effects_dat$discrim_medical_bin  == 1 | cumulative_effects_dat$discrim_notclever_bin == 1 | cumulative_effects_dat$discrim_afraidothers_bin == 1 | cumulative_effects_dat$discrim_poorerservice_bin == 1 ~ 1, 
                                               cumulative_effects_dat$discrim_harassed_bin == 0 & cumulative_effects_dat$discrim_lessrespect_bin == 0 & cumulative_effects_dat$discrim_medical_bin  == 0 & cumulative_effects_dat$discrim_notclever_bin == 0 & cumulative_effects_dat$discrim_afraidothers_bin == 0 & cumulative_effects_dat$discrim_poorerservice_bin == 0 ~ 0) 




cumulative_effects_dat$discrimination = cumulative_effects_dat$discrim_bin 



cumulative_effects_dat$discrimination_reversed = case_when(cumulative_effects_dat$discrim_bin == 1 ~ 0,
                                                           cumulative_effects_dat$discrim_bin == 0 ~ 1)

unique(cumulative_effects_dat$start_new)

cumulative_effects_dat$time_point = cumulative_effects_dat$start_new

cumulative_effects_dat$years = 2 * cumulative_effects_dat$start_new

cumulative_effects_dat$months = 12 * cumulative_effects_dat$years

cumulative_effects_dat$follow_up = cumulative_effects_dat$years


#1 = 2 year 
#2 = 4 follow_up 
#3 = 6 follow_up 


unique(cumulative_effects_dat$start_new)

sd(cumulative_effects_dat$start_new)

cumulative_effects_dat$diabetes_new_bin = case_when(cumulative_effects_dat$diabetes_new == 1 ~ 1, 
                                                    cumulative_effects_dat$diabetes_new == 0 ~ 0) 


cumulative_effects_dat$diabetes_new_bin_reversed = case_when(cumulative_effects_dat$diabetes_new_bin == 1 ~ 0, 
                                                             cumulative_effects_dat$diabetes_new_bin == 0 ~ 1) 


#cumulative_effects_dat$sex_1_2
#cumulative_effects_dat$race_white
#cumulative_effects_dat$race_white
#cumulative_effects_dat$national_origin_ousideUS_bin
#cumulative_effects_dat$religion_bin
#cumulative_effects_dat$assessed_BMI


#### plot for the entire dataset: 

fit <- survfit(Surv(follow_up, diabetes_new_bin) ~ discrimination, data = cumulative_effects_dat)
summary_all = summary(fit)
print(summary_all)

summary_all$lower
summary_all$upper

summary_all$surv
### output below: 

summary_all_table = summary_all$table
subset_all = rep("All", time=nrow(summary_all_table))
summary_all_table = cbind(subset_all, summary_all_table)

write.csv(summary_all_table, paste(OUTPUT_ROOT, "summary_all_table_discrimination_cat_no_cardiomet_disease.csv")) 

Number_events_all = fit$n.event
Number_risk_all = fit$n.risk
Number_censored_all = fit$n.censor
probability_for_nodiab_all = fit$surv
probability_SE_all = fit$std.err
probability_lower_CI_all = fit$lower
probability_upper_CI_all = fit$upper
cumhaz_output_all =  fit$cumhaz
cumhaz_output_std_all = fit$std.chaz




####
### output below: 
All_results_unadjusted = cbind(Number_events_all,
                               Number_risk_all,
                               Number_censored_all, 
                               probability_for_nodiab_all, 
                               probability_SE_all, 
                               probability_lower_CI_all, 
                               probability_upper_CI_all, 
                               cumhaz_output_all, 
                               cumhaz_output_std_all) 

subset_all_undj  = rep("All", time=nrow(All_results_unadjusted)) 
All_results_unadjusted = cbind(subset_all_undj, All_results_unadjusted) 
write.csv(All_results_unadjusted, paste(OUTPUT_ROOT, "All_results_unadjusted_discrimination_cat_no_cardiomet_disease.csv", sep = "")) 


plot_all = ggsurvplot(fit,
                      pval = TRUE, conf.int = TRUE,
                      risk.table = FALSE, # Add risk table
                      risk.table.col = "strata", # Change risk table color by groups
                      linetype = "strata", # Change line type by groups
                      surv.median.line = "hv", # Specify median survival
                      
                      surv.scale = "percent",
                      
                      ggtheme = theme_bw(),
                      ylim = c(0.5, 1),
                      pval.coord = c(0, 0.95),
                      
                      xlab = "Years from baseline", 
                      
                      ylab = "proportion of cases free of diabetes", 
                      
                      # Change ggplot2 theme
                      palette = c("#E7B800", "#2E9FDF"))

plot_all
print(plot_all)  


########
########
########
########

