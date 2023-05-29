
data = HRS2008_data



data$CVD[data$angina_new_bin ==1 | data$heartfailure2yrs_bin == 1 | data$heartattack_ever_bin == 1 | data$heartattack_new_bin == 1] <-1
data$CVD[data$angina_new_bin ==0 & data$heartfailure2yrs_bin == 0 & data$heartattack_ever_bin == 0 & data$heartattack_new_bin == 0] <-0

unique(data$CVD)


data$CVD_ever[data$heartfailure2yrs_bin == 1 | data$heartattack_ever_bin == 1 ] <-1
data$CVD_ever[data$heartfailure2yrs_bin == 0 & data$heartattack_ever_bin == 0 ] <-0
#data$CVD_ever[data$heartfailure2yrs_bin == " NA" & data$heartattack_ever_bin == " NA" ] <-0

unique(data$CVD_ever)


data$CVD_new[data$angina_new_bin ==1 | data$heartattack_new_bin == 1] <-1
data$CVD_new[data$angina_new_bin ==0 & data$heartattack_new_bin == 0] <-0



unique(data$CVD_new)