
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



#diabetes_new is diabtes this wave 
#0.no
#1.yes
#3.disp prev record and has cond
#4.disp prev record and no cond
#.d=DK
#.r=RF      

# create binary CVD variable 

clean_recode_keyvars =  function (data){

  data = data 
  
  ###### turn to numeric vectors, coercing NAs 
  
  data$discrim_harassed = as.numeric(data$discrim_harassed)
  data$discrim_lessrespect = as.numeric(data$discrim_lessrespect)
  data$discrim_medical = as.numeric(data$discrim_medical)
  data$discrim_notclever = as.numeric(data$discrim_notclever)
  data$discrim_poorerservice = as.numeric(data$discrim_poorerservice)
  data$discrim_afraidothers = as.numeric(data$discrim_afraidothers)
  
  
  data$heartcondition_ever_bin = as.numeric(data$heartcondition_ever_bin)
  data$heartcondition_new_bin = as.numeric(data$heartcondition_new_bin)
  data$angina_new_bin = as.numeric(data$angina_new_bin)
  data$stroke_new_bin = as.numeric(data$stroke_new_bin)
  data$heartfailure2yrs_bin = as.numeric(data$heartfailure2yrs_bin)
  data$heartattack_ever_bin = as.numeric(data$heartattack_ever_bin)
  data$heartattack_new_bin = as.numeric(data$heartattack_new_bin)
  
  data$vigarious_physical_activity = as.numeric(data$vigarious_physical_activity)

  
  data$alcohol_days_week = as.numeric(data$alcohol_days_week)
  data$smokes_now_bin = as.numeric(data$smokes_now_bin)
  data$checklist_depression_bin = as.numeric(data$checklist_depression_bin)
  data$wealth_noIRA = as.numeric(data$wealth_noIRA)
  
  
  data$diabetes_new = as.numeric(data$diabetes_new)
  data$diabetes_new_bin = as.numeric(data$diabetes_new_bin)
  
  
  
  ##### recode below: 
  
  data$CVD[data$angina_new_bin ==1 | data$heartfailure2yrs_bin == 1 | data$heartattack_ever_bin == 1 | data$heartattack_new_bin == 1] <-1
  data$CVD[data$angina_new_bin ==0 & data$heartfailure2yrs_bin == 0 & data$heartattack_ever_bin == 0 & data$heartattack_new_bin == 0] <-0
  
  unique(data$CVD)
  
  
  data$CVD_ever[data$heartfailure2yrs_bin == 1 | data$heartattack_ever_bin == 1 ] <-1
  data$CVD_ever[data$heartfailure2yrs_bin == 0 & data$heartattack_ever_bin == 0 ] <-0
  
  unique(data$CVD_ever)
  
  
  data$CVD_new[data$angina_new_bin ==1 | data$heartattack_new_bin == 1] <-1
  data$CVD_new[data$angina_new_bin ==0 & data$heartattack_new_bin == 0] <-0
  
  
  
  unique(data$CVD_new)




#physical activity (original): 1.every day; 2.>1 per week; 3.1 per week; 4.l-3 per mon; 5.never; .d=DK/NA; .r=RF


  data$vigarious_physical_activity_new = case_when(data$vigarious_physical_activity == 1 ~ 5, 
                                                   data$vigarious_physical_activity == 2 ~ 4, 
                                                   data$vigarious_physical_activity == 3 ~ 3, 
                                                   data$vigarious_physical_activity == 4 ~ 2, 
                                                   data$vigarious_physical_activity == 5 ~ 1) 




  data$vigarious_physical_activity_bin = case_when(data$vigarious_physical_activity_new == 5 ~ 1, 
                                                   data$vigarious_physical_activity_new == 4 ~ 1, 
                                                   data$vigarious_physical_activity_new == 3 ~ 1, 
                                                   data$vigarious_physical_activity_new == 2 ~ 0, 
                                                   data$vigarious_physical_activity_new == 1 ~ 0) 





  data$alcohol_days_week_new =  na_if(data$alcohol_days_week, 8)
  data$alcohol_days_week_new = na_if(data$alcohol_days_week_new, 9) 




###### recode into single var  discrim_bin
 

  data$discrim_harassed_bin = case_when(data$discrim_harassed == 1 ~ 1, 
                                        data$discrim_harassed == 2 ~ 1, 
                                        data$discrim_harassed == 3 ~ 1, 
                                        data$discrim_harassed == 4 ~ 1, 
                                        data$discrim_harassed == 5 ~ 0, 
                                        data$discrim_harassed == 6 ~ 0,
                                        data$discrim_harassed == 0 ~ 0) 


  
  data$discrim_lessrespect_bin = case_when(data$discrim_lessrespect == 1 ~ 1, 
                                           data$discrim_lessrespect == 2 ~ 1, 
                                           data$discrim_lessrespect == 3 ~ 1, 
                                           data$discrim_lessrespect == 4 ~ 1, 
                                           data$discrim_lessrespect == 5 ~ 0, 
                                           data$discrim_lessrespect == 6 ~ 0,
                                           data$discrim_lessrespect == 0 ~ 0) 



  data$discrim_medical_bin = case_when(data$discrim_medical == 1 ~ 1, 
                                       data$discrim_medical == 2 ~ 1, 
                                       data$discrim_medical == 3 ~ 1, 
                                       data$discrim_medical == 4 ~ 1, 
                                       data$discrim_medical == 5 ~ 0, 
                                       data$discrim_medical == 6 ~ 0,
                                       data$discrim_medical == 0 ~ 0) 





  data$discrim_notclever_bin = case_when(data$discrim_notclever == 1 ~ 1, 
                                         data$discrim_notclever == 2 ~ 1, 
                                         data$discrim_notclever == 3 ~ 1, 
                                         data$discrim_notclever == 4 ~ 1, 
                                         data$discrim_notclever == 5 ~ 0, 
                                         data$discrim_notclever == 6 ~ 0,
                                         data$discrim_notclever == 0 ~ 0) 






  data$discrim_poorerservice_bin = case_when(data$discrim_poorerservice == 1 ~ 1, 
                                             data$discrim_poorerservice == 2 ~ 1, 
                                             data$discrim_poorerservice == 3 ~ 1, 
                                             data$discrim_poorerservice == 4 ~ 1, 
                                             data$discrim_poorerservice == 5 ~ 0, 
                                             data$discrim_poorerservice == 6 ~ 0) 




  data$discrim_afraidothers_bin = case_when(data$discrim_afraidothers == 1 ~ 1,
                                            data$discrim_afraidothers == 2 ~ 1, 
                                            data$discrim_afraidothers == 3 ~ 1, 
                                            data$discrim_afraidothers == 4 ~ 1, 
                                            data$discrim_afraidothers == 5 ~ 0, 
                                            data$discrim_afraidothers == 6 ~ 0,
                                            data$discrim_afraidothers == 0 ~ 0) 



  data$discrim_bin = case_when(data$discrim_harassed_bin == 1 | data$discrim_lessrespect_bin == 1 | data$discrim_medical_bin  == 1 | data$discrim_notclever_bin == 1 | data$discrim_afraidothers_bin == 1 | data$discrim_poorerservice_bin == 1 ~ 1, 
                                           data$discrim_harassed_bin == 0 & data$discrim_lessrespect_bin == 0 & data$discrim_medical_bin  == 0 & data$discrim_notclever_bin == 0 & data$discrim_afraidothers_bin == 0 & data$discrim_poorerservice_bin == 0 ~ 0) 


  
  
############# 


return(data)
}


