
data_test = data.frame()
name = c("A", "B", "C", "D", "E", "F")

outcome = c(1, 0,  1,    1,   0,  1 )

data_test = rbind(name, outcome)


data_test <- data.frame(name = c("A", "B", "C", "D", "E", "F"),
                      outcome = c(1,   0,   1,   1,   0,   1 ))


# in data2 we should drop A, C, D, F, or recode them as old diabetes. 

data2_test = data.frame(name = c("A", "B", "E", "F", "G", "H"), 
                      outcome = c(1,   1,   0,   1,   0,   1)) 


data_test_drop_no_diabetes = subset(data_test, data_test$outcome == 0) 


#only keeps cases that are in the right dataframe
#data2_test_new = right_join(data_test, data2_test)


#test_right_join = right_join(data_test_drop_no_diabetes, data2_test)



# left_join lets us keep all the old cases (left) 
data2_new  = left_join(data_test_drop_no_diabetes, data_test)

#data2_new = subset(data2_new, )