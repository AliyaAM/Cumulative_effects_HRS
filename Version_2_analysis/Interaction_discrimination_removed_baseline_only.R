cumulative_effects_dat = read.csv("/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Cumulative_effects_laptop/DATA_FOR_PLOT/all_waves_nodiabatbaseline_DIAB_discrim_recoded.csv")



data_male = subset(cumulative_effects_dat, cumulative_effects_dat$sex_1_2 ==1) 
data_female = subset(cumulative_effects_dat, cumulative_effects_dat$sex_1_2 ==2) 
data_race = subset(cumulative_effects_dat, cumulative_effects_dat$race_white == 0) 
data_BMI = subset(cumulative_effects_dat, cumulative_effects_dat$assessed_BMI > 30) 

cumulative_effects_dat$diabetes_new

cumulative_effects_dat$sex_1_2
cumulative_effects_dat$continious_age
cumulative_effects_dat$wealth_noIRA

cumulative_effects_dat$discrim_bin


discrimination_sex <- glm(diabetes_new_bin ~ discrim_bin*sex_1_2, data=cumulative_effects_dat, family=binomial)
discrimination_sex_summary = summary(discrimination_sex)
discrimination_sex_summary$coefficients



discrimination_age <- glm(diabetes_new_bin ~ discrim_bin*continious_age, data=cumulative_effects_dat, family=binomial)
discrimination_age_summary = summary(discrimination_age)
discrimination_age_summary$coefficients



discrimination_wealth <- glm(diabetes_new_bin ~ discrim_bin*wealth_noIRA, data=cumulative_effects_dat, family=binomial)
discrimination_wealth_summary = summary(discrimination_wealth)
discrimination_wealth_summary$coefficients


all_interaction_coef = rbind(discrimination_sex_summary$coefficients, 
                             discrimination_age_summary$coefficients,
                             discrimination_wealth_summary$coefficients) 



######## adjusting for hypertension 

cumulative_effects_dat$hypertension_new_bin

discrimination_sex_adj_hptn <- glm(diabetes_new_bin ~ discrim_bin*sex_1_2 + hypertension_new_bin, data=cumulative_effects_dat, family=binomial)
discrimination_sex_adj_hptn_summary = summary(discrimination_sex_adj_hptn)
discrimination_sex_adj_hptn_summary$coefficients



discrimination_age_adj_hptn <- glm(diabetes_new_bin ~ discrim_bin*continious_age + hypertension_new_bin, data=cumulative_effects_dat, family=binomial)
discrimination_age_adj_hptn_summary = summary(discrimination_age_adj_hptn)
discrimination_age_adj_hptn_summary$coefficients



discrimination_wealth_adj_hptn <- glm(diabetes_new_bin ~ discrim_bin*wealth_noIRA + hypertension_new_bin, data=cumulative_effects_dat, family=binomial)
discrimination_wealth_adj_hptn_summary = summary(discrimination_wealth_adj_hptn)
discrimination_wealth_adj_hptn_summary$coefficients


all_interaction_coef__adj_hptn = rbind(discrimination_sex_adj_hptn_summary$coefficients, 
                                       discrimination_age_adj_hptn_summary$coefficients,
                                       discrimination_wealth_adj_hptn_summary$coefficients) 

# sig sex x discrimination interaction adjusting for hypertension 

######## adjusting for CVD 

# sig sex x discrimination interaction adjusting for CVD 

######## adjusting for depression 

cumulative_effects_dat$checklist_depression_bin
discrimination_sex_adj_depres <- glm(diabetes_new_bin ~ discrim_bin*sex_1_2 + checklist_depression_bin, data=cumulative_effects_dat, family=binomial)
discrimination_sex_adj_depres_summary = summary(discrimination_sex_adj_depres)
discrimination_sex_adj_depres_summary$coefficients


discrimination_age_adj_depres <- glm(diabetes_new_bin ~ discrim_bin*continious_age + checklist_depression_bin, data=cumulative_effects_dat, family=binomial)
discrimination_age_adj_depres_summary = summary(discrimination_age_adj_depres)
discrimination_age_adj_depres_summary$coefficients


discrimination_wealth_adj_depres <- glm(diabetes_new_bin ~ discrim_bin*wealth_noIRA + checklist_depression_bin, data=cumulative_effects_dat, family=binomial)
discrimination_wealth_adj_depres_summary = summary(discrimination_wealth_adj_depres)
discrimination_wealth_adj_depres_summary$coefficients


all_interaction_coef__adj_depres = rbind(discrimination_sex_adj_depres_summary$coefficients, 
                                         discrimination_age_adj_depres_summary$coefficients,
                                         discrimination_wealth_adj_depres_summary$coefficients) 


#the interaction effect between discrimination and sex disappeared when adjusting for depression  


######## adjusting for BMI 
cumulative_effects_dat$assessed_BMI
discrimination_sex_adj_BMI <- glm(diabetes_new_bin ~ discrim_bin*sex_1_2 + assessed_BMI, data=cumulative_effects_dat, family=binomial)
discrimination_sex_adj_BMI_summary = summary(discrimination_sex_adj_BMI)
discrimination_sex_adj_BMI_summary$coefficients


discrimination_age_adj_BMI <- glm(diabetes_new_bin ~ discrim_bin*continious_age + assessed_BMI, data=cumulative_effects_dat, family=binomial)
discrimination_age_adj_BMI_summary = summary(discrimination_age_adj_BMI)
discrimination_age_adj_BMI_summary$coefficients


discrimination_wealth_adj_BMI <- glm(diabetes_new_bin ~ discrim_bin*wealth_noIRA + assessed_BMI, data=cumulative_effects_dat, family=binomial)
discrimination_wealth_adj_BMI_summary = summary(discrimination_wealth_adj_BMI)
discrimination_wealth_adj_BMI_summary$coefficients


#the interaction effect between discrimination and sex disappeared when adjusting for BMI 

all_interaction_coef__adj_BMI = rbind(discrimination_sex_adj_BMI_summary$coefficients, 
                                      discrimination_age_adj_BMI_summary$coefficients,
                                      discrimination_wealth_adj_BMI_summary$coefficients) 


######## adjusting for health behaviours 









# perform test for hetorescedacity 
# the assumtion of heotrescedacity was not violated 

#The image above shows the “Residual vs. Fitted”-plot and the “Scale-Location”-plot for a regression model without heteroscedastic residuals. In other words, the variance of the residuals is the same for all values of the fitted values.
#Although the lines in both plots are not flat, the variability among the (square root of the standardized) residuals seems stable. The variability does neither increase nor decrease with the fitted values. Therefore, we can assume that this regression model does not violate the homoscedasticity assumption.
#discrimination_sex_adj_depres <- glm(diabetes_new_bin ~ discrim_bin*sex_1_2 + checklist_depression_bin, data=cumulative_effects_dat, family=binomial)


hetorescedacity_test_discrim_diabetes <- lm(diabetes_new_bin~discrim_bin, data = cumulative_effects_dat)
par(mfrow = c(2, 2))
plot(hetorescedacity_test_discrim_diabetes)

library(lmtest)

# the Breusch-Pagan test 
# The interpretation of the Breusch-Pagan test for heteroscedasticity is simple. Because the test statistic (BP) is small and the p-value is not significant (i.e., >0.05), we do not reject the null hypothesis. Therefore, we assume that the residuals are homoscedastic.
lmtest::bptest(hetorescedacity_test_discrim_diabetes)
#BP = 0.035053, df = 1, p-value = 0.8515

# if violated perform weighted regression 

#https://support.minitab.com/en-us/minitab/18/help-and-how-to/modeling-statistics/regression/supporting-topics/basics/weighted-regression/#:~:text=Weighted%20regression%20is%20a%20method,a%20constant%20variance%20(homoscedasticity).



hetorescedacity_test_discrim_diabetes_male <- lm(diabetes_new_bin~discrim_bin, data = data_male)
lmtest::bptest(hetorescedacity_test_discrim_diabetes_male)

hetorescedacity_test_discrim_diabetes_female <- lm(diabetes_new_bin~discrim_bin, data = data_female)
lmtest::bptest(hetorescedacity_test_discrim_diabetes_female)

hetorescedacity_test_discrim_diabetes_race <- lm(diabetes_new_bin~discrim_bin, data = data_race)
lmtest::bptest(hetorescedacity_test_discrim_diabetes_race)

hetorescedacity_test_discrim_diabetes_BMI<- lm(diabetes_new_bin~discrim_bin, data = data_BMI)
lmtest::bptest(hetorescedacity_test_discrim_diabetes_BMI)


#logistic regression assumptions 

#First, binary logistic regression requires the dependent variable to be binary and ordinal logistic regression requires the dependent variable to be ordinal.
#Second, logistic regression requires the observations to be independent of each other.  In other words, the observations should not come from repeated measurements or matched data.
#Third, logistic regression requires there to be little or no multicollinearity among the independent variables.  This means that the independent variables should not be too highly correlated with each other.
#Fourth, logistic regression assumes linearity of independent variables and log odds.  although this analysis does not require the dependent and independent variables to be related linearly, it requires that the independent variables are linearly related to the log odds.
#Finally, logistic regression typically requires a large sample size.  A general guideline is that you need at minimum of 10 cases with the least frequent outcome for each independent variable in your model. For example, if you have 5 independent variables and the expected probability of your least frequent outcome is .10, then you would need a minimum sample size of 500 (10*5 / .10).


#The Cox proportional hazards model makes two assumptions: (1) survival curves for different strata must have hazard functions that are proportional over the time t and (2) the relationship between the log hazard and each covariate is linear, which can be verified with residual plots.

