
#SOURCE_ROOT = "/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"
#OUTPUT_ROOT = "/Users/aliya/my_docs/KCL_postDoc/Data_analysis/"
SOURCE_ROOT = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/"


RAND_HRS_longitudional_file = read.csv(paste(SOURCE_ROOT, "randhrs1992_2018v1.csv", sep=""))

harmonised_data_all_waves = read.csv(paste(SOURCE_ROOT, "H_HRS_c.csv", sep=""))


HHIDPN = RAND_HRS_longitudional_file$HHIDPN

cross_waves = data.frame(HHIDPN) 

#sex
cross_waves$sex_1_2 = RAND_HRS_longitudional_file$RAGENDER	

#age at each wave (years)
cross_waves$continious_age_1992 = RAND_HRS_longitudional_file$R1AGEY_B
cross_waves$continious_age_1994 = RAND_HRS_longitudional_file$R2AGEY_B
cross_waves$continious_age_1996 = RAND_HRS_longitudional_file$R3AGEY_B
cross_waves$continious_age_1998 = RAND_HRS_longitudional_file$R4AGEY_B
cross_waves$continious_age_2000 = RAND_HRS_longitudional_file$R5AGEY_B
cross_waves$continious_age_2002 = RAND_HRS_longitudional_file$R6AGEY_B
cross_waves$continious_age_2004 = RAND_HRS_longitudional_file$R7AGEY_B
cross_waves$continious_age_2006 = RAND_HRS_longitudional_file$R8AGEY_B
cross_waves$continious_age_2008 = RAND_HRS_longitudional_file$R9AGEY_B
cross_waves$continious_age_2010 = RAND_HRS_longitudional_file$R10AGEY_B
cross_waves$continious_age_2012 = RAND_HRS_longitudional_file$R11AGEY_B
cross_waves$continious_age_2014 = RAND_HRS_longitudional_file$R12AGEY_B
cross_waves$continious_age_2016 = RAND_HRS_longitudional_file$R13AGEY_B
cross_waves$continious_age_2018 = RAND_HRS_longitudional_file$R14AGEY_B



#religion
#1.protestant, 2.catholic, 3.jewish, 4.none/no pref, 5.other, .d=DK, .m=Oth missin, .r=RF

cross_waves$religion = RAND_HRS_longitudional_file$RARELIG 

cross_waves$religion_bin = case_when(cross_waves$religion == 1 ~ 0, 
                                     cross_waves$religion == 2 ~ 0, 
                                     cross_waves$religion == 4 ~ 0, 
                                     cross_waves$religion == 3 ~ 1, 
                                     cross_waves$religion == 3 ~ 1, 
                                     cross_waves$religion == 5 ~ 1)
                                     


#national_origin 

cross_waves$national_origin_ousideUS  = case_when(RAND_HRS_longitudional_file$RABPLACF == 1 ~ 1, 
                                                  RAND_HRS_longitudional_file$RABPLACF == 2 ~ 0)

#race 2008: rahispan, 0.not hispanic, 1.hispanic, .m=Oth missing
#HRS2008_data$race = harmonised_data_all_waves_2012$raracem 
#HRS2018_race_hispanic_latino
#HRS2018_race_white
#HRS2018_race_black

#race: 2008, RARACEM: 1.white/caucasian, 2.black/african american, 3.other, .m=Oth missing

cross_waves$race_white  = case_when(RAND_HRS_longitudional_file$RARACEM == 1 ~ 1, 
                                    RAND_HRS_longitudional_file$RARACEM == 2 ~ 0, 
                                    RAND_HRS_longitudional_file$RARACEM == 3 ~ 0)


#LGB_2016

#Straight_2016


#cross_waves$limiting_condition not included in the rand harmonised cleaned file 

#cross_waves$limiting_condition_bin not included in the rand harmonised cleaned file 

#R1BMI - self-reported BMI 
cross_waves$BMI_1992 = RAND_HRS_longitudional_file$R1BMI
cross_waves$BMI_1994 = RAND_HRS_longitudional_file$R2BMI
cross_waves$BMI_1996 = RAND_HRS_longitudional_file$R3BMI
cross_waves$BMI_1998 = RAND_HRS_longitudional_file$R4BMI
cross_waves$BMI_2000 = RAND_HRS_longitudional_file$R5BMI
cross_waves$BMI_2002 = RAND_HRS_longitudional_file$R6BMI
cross_waves$BMI_2004 = RAND_HRS_longitudional_file$R7BMI
cross_waves$BMI_2006 = RAND_HRS_longitudional_file$R8BMI
cross_waves$BMI_2008 = RAND_HRS_longitudional_file$R9BMI
cross_waves$BMI_2010 = RAND_HRS_longitudional_file$R10BMI
cross_waves$BMI_2012 = RAND_HRS_longitudional_file$R11BMI
cross_waves$BMI_2014 = RAND_HRS_longitudional_file$R12BMI
cross_waves$BMI_2016 = RAND_HRS_longitudional_file$R13BMI
cross_waves$BMI_2018 = RAND_HRS_longitudional_file$R14BMI

#R1PMBMI	- objectively assessed BMI 

cross_waves$assessed_BMI_1992 = RAND_HRS_longitudional_file$R1PMBMI
cross_waves$assessed_BMI_1994 = RAND_HRS_longitudional_file$R2PMBMI
cross_waves$assessed_BMI_1996 = RAND_HRS_longitudional_file$R3PMBMI
cross_waves$assessed_BMI_1998 = RAND_HRS_longitudional_file$R4PMBMI
cross_waves$assessed_BMI_2000 = RAND_HRS_longitudional_file$R5PMBMI
cross_waves$assessed_BMI_2002 = RAND_HRS_longitudional_file$R6PMBMI
cross_waves$assessed_BMI_2004 = RAND_HRS_longitudional_file$R7PMBMI
cross_waves$assessed_BMI_2006 = RAND_HRS_longitudional_file$R8PMBMI
cross_waves$assessed_BMI_2008 = RAND_HRS_longitudional_file$R9PMBMI
cross_waves$assessed_BMI_2010 = RAND_HRS_longitudional_file$R10PMBMI
cross_waves$assessed_BMI_2012 = RAND_HRS_longitudional_file$R11PMBMI
cross_waves$assessed_BMI_2014 = RAND_HRS_longitudional_file$R12PMBMI
cross_waves$assessed_BMI_2016 = RAND_HRS_longitudional_file$R13PMBMI
cross_waves$assessed_BMI_2018 = RAND_HRS_longitudional_file$R14PMBMI

#cross_waves$wealth_noIRA_HRS2012 = RAND_HRS_longitudional_file_2012$h11atotw

cross_waves$wealth_noIRA_HRS1992 = RAND_HRS_longitudional_file$H1ATOTW
cross_waves$wealth_noIRA_HRS1994 = RAND_HRS_longitudional_file$H2ATOTW
cross_waves$wealth_noIRA_HRS1996 = RAND_HRS_longitudional_file$H3ATOTW
cross_waves$wealth_noIRA_HRS1998 = RAND_HRS_longitudional_file$H4ATOTW
cross_waves$wealth_noIRA_HRS2000 = RAND_HRS_longitudional_file$H5ATOTW
cross_waves$wealth_noIRA_HRS2002 = RAND_HRS_longitudional_file$H6ATOTW
cross_waves$wealth_noIRA_HRS2004 = RAND_HRS_longitudional_file$H7ATOTW
cross_waves$wealth_noIRA_HRS2006 = RAND_HRS_longitudional_file$H8ATOTW
cross_waves$wealth_noIRA_HRS2008 = RAND_HRS_longitudional_file$H9ATOTW
cross_waves$wealth_noIRA_HRS2010 = RAND_HRS_longitudional_file$H10ATOTW
cross_waves$wealth_noIRA_HRS2012 = RAND_HRS_longitudional_file$H11ATOTW
cross_waves$wealth_noIRA_HRS2014 = RAND_HRS_longitudional_file$H12ATOTW
cross_waves$wealth_noIRA_HRS2016 = RAND_HRS_longitudional_file$H13ATOTW
cross_waves$wealth_noIRA_HRS2018 = RAND_HRS_longitudional_file$H14ATOTW

#RAND_HRS_longitudional_file$r10rxdepres #takes medication for depression 

cross_waves$depression_bin_1992 = RAND_HRS_longitudional_file$R1DEPRES
cross_waves$depression_bin_1994 = RAND_HRS_longitudional_file$R2DEPRES
cross_waves$depression_bin_1996 = RAND_HRS_longitudional_file$R3DEPRES
cross_waves$depression_bin_1998 = RAND_HRS_longitudional_file$R4DEPRES
cross_waves$depression_bin_2000 = RAND_HRS_longitudional_file$R5DEPRES
cross_waves$depression_bin_2002 = RAND_HRS_longitudional_file$R6DEPRES
cross_waves$depression_bin_2004 = RAND_HRS_longitudional_file$R7DEPRES
cross_waves$depression_bin_2006 = RAND_HRS_longitudional_file$R8DEPRES
cross_waves$depression_bin_2008 = RAND_HRS_longitudional_file$R9DEPRES
cross_waves$depression_bin_2010 = RAND_HRS_longitudional_file$R10DEPRES
cross_waves$depression_bin_2012 = RAND_HRS_longitudional_file$R11DEPRES
cross_waves$depression_bin_2014 = RAND_HRS_longitudional_file$R12DEPRES
cross_waves$depression_bin_2016 = RAND_HRS_longitudional_file$R13DEPRES
cross_waves$depression_bin_2018 = RAND_HRS_longitudional_file$R14DEPRES



#heart condition ever 
cross_waves$heartcondition_ever_1992 = RAND_HRS_longitudional_file$R1HEARTE
cross_waves$heartcondition_ever_1994 = RAND_HRS_longitudional_file$R2HEARTE
cross_waves$heartcondition_ever_1996 = RAND_HRS_longitudional_file$R3HEARTE
cross_waves$heartcondition_ever_1998 = RAND_HRS_longitudional_file$R4HEARTE
cross_waves$heartcondition_ever_2000 = RAND_HRS_longitudional_file$R5HEARTE
cross_waves$heartcondition_ever_2002 = RAND_HRS_longitudional_file$R6HEARTE
cross_waves$heartcondition_ever_2004 = RAND_HRS_longitudional_file$R7HEARTE
cross_waves$heartcondition_ever_2006 = RAND_HRS_longitudional_file$R8HEARTE
cross_waves$heartcondition_ever_2008 = RAND_HRS_longitudional_file$R9HEARTE
cross_waves$heartcondition_ever_2010 = RAND_HRS_longitudional_file$R10HEARTE
cross_waves$heartcondition_ever_2012 = RAND_HRS_longitudional_file$R11HEARTE
cross_waves$heartcondition_ever_2014 = RAND_HRS_longitudional_file$R12HEARTE
cross_waves$heartcondition_ever_2016 = RAND_HRS_longitudional_file$R13HEARTE
cross_waves$heartcondition_ever_2018 = RAND_HRS_longitudional_file$R14HEARTE


# stroke this wave: 
cross_waves$stroke_new_1992 = RAND_HRS_longitudional_file$R1STROK
cross_waves$stroke_new_1994 = RAND_HRS_longitudional_file$R2STROK
cross_waves$stroke_new_1996 = RAND_HRS_longitudional_file$R3STROK
cross_waves$stroke_new_1998 = RAND_HRS_longitudional_file$R4STROK
cross_waves$stroke_new_2000 = RAND_HRS_longitudional_file$R5STROK
cross_waves$stroke_new_2002 = RAND_HRS_longitudional_file$R6STROK
cross_waves$stroke_new_2004 = RAND_HRS_longitudional_file$R7STROK
cross_waves$stroke_new_2006 = RAND_HRS_longitudional_file$R8STROK
cross_waves$stroke_new_2008 = RAND_HRS_longitudional_file$R9STROK
cross_waves$stroke_new_2010 = RAND_HRS_longitudional_file$R10STROK
cross_waves$stroke_new_2012 = RAND_HRS_longitudional_file$R11STROK
cross_waves$stroke_new_2014 = RAND_HRS_longitudional_file$R12STROK
cross_waves$stroke_new_2016 = RAND_HRS_longitudional_file$R13STROK
cross_waves$stroke_new_2018 = RAND_HRS_longitudional_file$R14STROK


#stroke ever: 
cross_waves$stroke_ever_1992 = RAND_HRS_longitudional_file$R1STROKE
cross_waves$stroke_ever_1994 = RAND_HRS_longitudional_file$R2STROKE
cross_waves$stroke_ever_1996 = RAND_HRS_longitudional_file$R3STROKE
cross_waves$stroke_ever_1998 = RAND_HRS_longitudional_file$R4STROKE
cross_waves$stroke_ever_2000 = RAND_HRS_longitudional_file$R5STROKE
cross_waves$stroke_ever_2002 = RAND_HRS_longitudional_file$R6STROKE
cross_waves$stroke_ever_2004 = RAND_HRS_longitudional_file$R7STROKE
cross_waves$stroke_ever_2006 = RAND_HRS_longitudional_file$R8STROKE
cross_waves$stroke_ever_2008 = RAND_HRS_longitudional_file$R9STROKE
cross_waves$stroke_ever_2010 = RAND_HRS_longitudional_file$R10STROKE
cross_waves$stroke_ever_2012 = RAND_HRS_longitudional_file$R11STROKE
cross_waves$stroke_ever_2014 = RAND_HRS_longitudional_file$R12STROKE
cross_waves$stroke_ever_2016 = RAND_HRS_longitudional_file$R13STROKE
cross_waves$stroke_ever_2018 = RAND_HRS_longitudional_file$R14STROKE


# diabetes this wave: 
cross_waves$diabetes_new_1992 = RAND_HRS_longitudional_file$R1DIAB
cross_waves$diabetes_new_1994 = RAND_HRS_longitudional_file$R2DIAB
cross_waves$diabetes_new_1996 = RAND_HRS_longitudional_file$R3DIAB
cross_waves$diabetes_new_1998 = RAND_HRS_longitudional_file$R4DIAB
cross_waves$diabetes_new_2000 = RAND_HRS_longitudional_file$R5DIAB
cross_waves$diabetes_new_2002 = RAND_HRS_longitudional_file$R6DIAB
cross_waves$diabetes_new_2004 = RAND_HRS_longitudional_file$R7DIAB
cross_waves$diabetes_new_2006 = RAND_HRS_longitudional_file$R8DIAB
cross_waves$diabetes_new_2008 = RAND_HRS_longitudional_file$R9DIAB
cross_waves$diabetes_new_2010 = RAND_HRS_longitudional_file$R10DIAB
cross_waves$diabetes_new_2012 = RAND_HRS_longitudional_file$R11DIAB
cross_waves$diabetes_new_2014 = RAND_HRS_longitudional_file$R12DIAB
cross_waves$diabetes_new_2016 = RAND_HRS_longitudional_file$R13DIAB
cross_waves$diabetes_new_2018 = RAND_HRS_longitudional_file$R14DIAB


# diabetes ever
cross_waves$diabetes_ever_1992 = RAND_HRS_longitudional_file$R1DIABE
cross_waves$diabetes_ever_1994 = RAND_HRS_longitudional_file$R2DIABE
cross_waves$diabetes_ever_1996 = RAND_HRS_longitudional_file$R3DIABE
cross_waves$diabetes_ever_1998 = RAND_HRS_longitudional_file$R4DIABE
cross_waves$diabetes_ever_2000 = RAND_HRS_longitudional_file$R5DIABE
cross_waves$diabetes_ever_2002 = RAND_HRS_longitudional_file$R6DIABE
cross_waves$diabetes_ever_2004 = RAND_HRS_longitudional_file$R7DIABE
cross_waves$diabetes_ever_2006 = RAND_HRS_longitudional_file$R8DIABE
cross_waves$diabetes_ever_2008 = RAND_HRS_longitudional_file$R9DIABE
cross_waves$diabetes_ever_2010 = RAND_HRS_longitudional_file$R10DIABE
cross_waves$diabetes_ever_2012 = RAND_HRS_longitudional_file$R11DIABE
cross_waves$diabetes_ever_2014 = RAND_HRS_longitudional_file$R12DIABE
cross_waves$diabetes_ever_2016 = RAND_HRS_longitudional_file$R13DIABE
cross_waves$diabetes_ever_2018 = RAND_HRS_longitudional_file$R14DIABE

#R1BPHIGH high blood pressure this wave

#diastolic blood pressure R8BPDIA
cross_waves$diastolic_blood_pressure_1992 = RAND_HRS_longitudional_file$R1BPDIA
cross_waves$diastolic_blood_pressure_1994 = RAND_HRS_longitudional_file$R2BPDIA
cross_waves$diastolic_blood_pressure_1996 = RAND_HRS_longitudional_file$R3BPDIA
cross_waves$diastolic_blood_pressure_1998 = RAND_HRS_longitudional_file$R4BPDIA
cross_waves$diastolic_blood_pressure_2000 = RAND_HRS_longitudional_file$R5BPDIA
cross_waves$diastolic_blood_pressure_2002 = RAND_HRS_longitudional_file$R6BPDIA
cross_waves$diastolic_blood_pressure_2004 = RAND_HRS_longitudional_file$R7BPDIA
cross_waves$diastolic_blood_pressure_2006 = RAND_HRS_longitudional_file$R8BPDIA
cross_waves$diastolic_blood_pressure_2008 = RAND_HRS_longitudional_file$R9BPDIA
cross_waves$diastolic_blood_pressure_2010 = RAND_HRS_longitudional_file$R10BPDIA
cross_waves$diastolic_blood_pressure_2012 = RAND_HRS_longitudional_file$R11BPDIA
cross_waves$diastolic_blood_pressure_2014 = RAND_HRS_longitudional_file$R12PDIA
cross_waves$diastolic_blood_pressure_2016 = RAND_HRS_longitudional_file$R13BPDIA
cross_waves$diastolic_blood_pressure_2018 = RAND_HRS_longitudional_file$R14BPDIA


#systolic blood pressure: R12BPSYS
cross_waves$systolic_blood_pressure_1992 = RAND_HRS_longitudional_file$R1BPSYS
cross_waves$systolic_blood_pressure_1994 = RAND_HRS_longitudional_file$R2BPSYS
cross_waves$systolic_blood_pressure_1996 = RAND_HRS_longitudional_file$R3BPSYS
cross_waves$systolic_blood_pressure_1998 = RAND_HRS_longitudional_file$R4BPSYS
cross_waves$systolic_blood_pressure_2000 = RAND_HRS_longitudional_file$R5BPSYS
cross_waves$systolic_blood_pressure_2002 = RAND_HRS_longitudional_file$R6BPSYS
cross_waves$systolic_blood_pressure_2004 = RAND_HRS_longitudional_file$R7BPSYS
cross_waves$systolic_blood_pressure_2006 = RAND_HRS_longitudional_file$R8BPSYS
cross_waves$systolic_blood_pressure_2008 = RAND_HRS_longitudional_file$R9BPSYS
cross_waves$systolic_blood_pressure_2010 = RAND_HRS_longitudional_file$R10BPSYS
cross_waves$systolic_blood_pressure_2012 = RAND_HRS_longitudional_file$R11BPSYS
cross_waves$systolic_blood_pressure_2014 = RAND_HRS_longitudional_file$R12BPSYS
cross_waves$systolic_blood_pressure_2016 = RAND_HRS_longitudional_file$R13BPSYS
cross_waves$systolic_blood_pressure_2018 = RAND_HRS_longitudional_file$R14BPSYS


#high blood pressure ever
cross_waves$hypertension_ever_1992 = RAND_HRS_longitudional_file$R1HIBPE
cross_waves$hypertension_ever_1994 = RAND_HRS_longitudional_file$R2HIBPE
cross_waves$hypertension_ever_1996 = RAND_HRS_longitudional_file$R3HIBPE
cross_waves$hypertension_ever_1998 = RAND_HRS_longitudional_file$R4HIBPE
cross_waves$hypertension_ever_2000 = RAND_HRS_longitudional_file$R5HIBPE
cross_waves$hypertension_ever_2002 = RAND_HRS_longitudional_file$R6HIBPE
cross_waves$hypertension_ever_2004 = RAND_HRS_longitudional_file$R7HIBPE
cross_waves$hypertension_ever_2006 = RAND_HRS_longitudional_file$R8HIBPE
cross_waves$hypertension_ever_2008 = RAND_HRS_longitudional_file$R9HIBPE
cross_waves$hypertension_ever_2010 = RAND_HRS_longitudional_file$R10HIBPE
cross_waves$hypertension_ever_2002 = RAND_HRS_longitudional_file$R11HIBPE
cross_waves$hypertension_ever_2014 = RAND_HRS_longitudional_file$R12HIBPE
cross_waves$hypertension_ever_2006 = RAND_HRS_longitudional_file$R13HIBPE
cross_waves$hypertension_ever_2018 = RAND_HRS_longitudional_file$R14HIBPE



# heart attack new: all waves 
cross_waves$heartattack_new_1992 = RAND_HRS_longitudional_file$R1HEARTS
cross_waves$heartattack_new_1994 = RAND_HRS_longitudional_file$R2HEARTS
cross_waves$heartattack_new_1996 = RAND_HRS_longitudional_file$R3HEARTS
cross_waves$heartattack_new_1998 = RAND_HRS_longitudional_file$R4HEARTS
cross_waves$heartattack_new_2000 = RAND_HRS_longitudional_file$R5HEARTS
cross_waves$heartattack_new_2002 = RAND_HRS_longitudional_file$R6HEARTS
cross_waves$heartattack_new_2004 = RAND_HRS_longitudional_file$R7HEARTS
cross_waves$heartattack_new_2006 = RAND_HRS_longitudional_file$R8HEARTS
cross_waves$heartattack_new_2008 = RAND_HRS_longitudional_file$R9HEARTS
cross_waves$heartattack_new_2010 = RAND_HRS_longitudional_file$R10HEARTS
cross_waves$heartattack_new_2012 = RAND_HRS_longitudional_file$R11HEARTS
cross_waves$heartattack_new_2014 = RAND_HRS_longitudional_file$R12HEARTS
cross_waves$heartattack_new_2016 = RAND_HRS_longitudional_file$R13HEARTS
cross_waves$heartattack_new_2018 = RAND_HRS_longitudional_file$R14HEARTS



# R9SMOKEV smokes ever 
cross_waves$smokes_ever_1992 = RAND_HRS_longitudional_file$R1SMOKEV 
cross_waves$smokes_ever_1994 = RAND_HRS_longitudional_file$R2SMOKEV 
cross_waves$smokes_ever_1996 = RAND_HRS_longitudional_file$R3SMOKEV 
cross_waves$smokes_ever_1998 = RAND_HRS_longitudional_file$R4SMOKEV 
cross_waves$smokes_ever_2000 = RAND_HRS_longitudional_file$R5SMOKEV 
cross_waves$smokes_ever_2002 = RAND_HRS_longitudional_file$R6SMOKEV 
cross_waves$smokes_ever_2004 = RAND_HRS_longitudional_file$R7SMOKEV 
cross_waves$smokes_ever_2006 = RAND_HRS_longitudional_file$R8SMOKEV 
cross_waves$smokes_ever_2008 = RAND_HRS_longitudional_file$R9SMOKEV 
cross_waves$smokes_ever_2010 = RAND_HRS_longitudional_file$R10SMOKEV 
cross_waves$smokes_ever_2012 = RAND_HRS_longitudional_file$R11SMOKEV 
cross_waves$smokes_ever_2014 = RAND_HRS_longitudional_file$R12SMOKEV 
cross_waves$smokes_ever_2016 = RAND_HRS_longitudional_file$R13SMOKEV 
cross_waves$smokes_ever_2018 = RAND_HRS_longitudional_file$R14SMOKEV 

# R9SMOKEN Smokes now 

cross_waves$smokes_now_1992 = RAND_HRS_longitudional_file$R1SMOKEN 
cross_waves$smokes_now_1994 = RAND_HRS_longitudional_file$R2SMOKEN 
cross_waves$smokes_now_1996 = RAND_HRS_longitudional_file$R3SMOKEN 
cross_waves$smokes_now_1998 = RAND_HRS_longitudional_file$R4SMOKEN 
cross_waves$smokes_now_2000 = RAND_HRS_longitudional_file$R5SMOKEN 
cross_waves$smokes_now_2002 = RAND_HRS_longitudional_file$R6SMOKEN 
cross_waves$smokes_now_2004 = RAND_HRS_longitudional_file$R7SMOKEN 
cross_waves$smokes_now_2006 = RAND_HRS_longitudional_file$R8SMOKEN
cross_waves$smokes_now_2008 = RAND_HRS_longitudional_file$R9SMOKEN 
cross_waves$smokes_now_2010 = RAND_HRS_longitudional_file$R10SMOKEN 
cross_waves$smokes_now_2012 = RAND_HRS_longitudional_file$R11SMOKEN 
cross_waves$smokes_now_2014 = RAND_HRS_longitudional_file$R12SMOKEN 
cross_waves$smokes_now_2016 = RAND_HRS_longitudional_file$R13SMOKEN 
cross_waves$smokes_now_2018 = RAND_HRS_longitudional_file$R14SMOKEN 





write.csv(cross_waves, paste(SOURCE_ROOT, "cross_waves_march2022.csv", sep=""))

head(cross_waves)

ls(cross_waves)