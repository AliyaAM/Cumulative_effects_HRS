> data = read.csv("/Users/aliya/my_docs/proj/Cumulative_effects_HRS/Results/dataset_noNAs_timepoints_TEST_DELETE_AFTER_debugging_24nov2022.csv")
> subset_time_1 = subset(data, data$start_new = 0)
Error: unexpected '=' in "subset_time_1 = subset(data, data$start_new ="
> subset_time_1 = subset(data, data$start_new == 0)
> subset_time_2 = subset(data, data$start_new == 1)
> subset_time_3 = subset(data, data$start_new == 2)
> subset_time_4 = subset(data, data$start_new == 3)
> subset_time_5 = subset(data, data$start_new == 4)
> subset_time_6 = subset(data, data$start_new == 5)
> subset_time_1$discrim_bin
[1] 1 0 1 1 0 1 0 0 0 0 1 0 0 1 0 0 0 1 0 0 1 1 1 1 1 0 1 0 0 1 0 1 1 1 1 1 0 1 0 0 0 1 1 0 0 1 0 1 0 0 0 0 0 1 1 0 0 1 1 0 0 0 0 1 1 0 0 1 1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 0 0 1 1 0 0 0 1
[92] 1 0 1 0 0 0 0 1 0 1 0 0 0 0 0 0 0 1 0 0 1 1 1 0 1 1 1 0 0 1 0 0 0 0 1 1 0 0 0 0 0 1 0 1 1 0 0 1 1 0 0 0 1 1 0 1 1 1 1 0 0 0 1 0 0 1 1 1 1 0 0 0 0 0 0 0 1 0 1 0 0 0 1 1 0 0 1 1 0 1 0
[183] 1 0 1 0 1 0 0 1 0 0 1 1 0 1 1 1 0 0 0 0 1 0 0 1 1 1 0 0 1 0 0 0 0 0 1 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 1 1 0 1 1 1 1 0 1 1 0 0 0 1 0 1 0 0 1 0 0 1 1 1 0 0 1 1 0 1 0 0 0 0 0 0 0 0 0 1 1
[274] 0 0 1 1 0 1 0 1 1 0 0 0 0 0 1 1 1 0 1 0 1 1 0 1 0 0 1 0 1 1 0 1 1 1 1 0 1 0 0 0 0 0 0 1 0 1 0 0 0 1 1 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 1 1 0 1 1 1 0 0 0
[365] 1 1 0 0 1 0 0 0 0 0 1 1 0 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1 0 1 0 1 1 1 1 1 1 0 1 1 0 0 1 0 1 0 0 1 0 1 1 0 1 0 0 0 1 0 0 0 0 0 0 1 0 1 0 1 1 1 0 1 0 1 1 0 0 0 1 0 1 0 0 0 1 0 1 0 0 0 0
[456] 0 0 1 0 0 0 1 1 0 0 1 1 1 0 0 1 0 0 0 0 1 1 0 0 0 1 1 0 1 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 1 0 1 1 0 0 0 0 0 0 1 1 0 0 0 0 1 1 1 0 1 0 0 0 1 1 1 1 0 0 1 1 0 1 0 0 0 1 0 1 1 0 1 0 0 0 0
[547] 1 1 0 1 1 0 0 0 1 0 1 1 0 0 1 1 0 0 1 0 0 0 1 1 1 1 1 0 1 0 0 0 1 0 0 0 0 1 0 0 0 0 1 1 1 1 0 1 1 1 0 0 0 1 0 0 1 1 1 0 0 1 0 0 0 0 1 1 0 0 0 1 0 0 1 1 0 1 0 1 0 0 0 0 0 1 1 0 0 0 0
[638] 0 0 1 1 0 1 0 1 0 1 1 0 1 1 1 1 0 0 0 1 1 1 0 0 1 0 1 0 0 1 1 0 0 1 1 0 1 0 0 0 0 1 0 0 0 1 0 1 1 0 1 0 0 0 0 1 1 0 1 0 0 0 1 0 0 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 1 1 0 0 0 1
[729] 0 0 0 1 0 0 0 1 0 0 0 1 0 1 1 1 0 1 1 1 0 1 0 1 0 1 1 1 0 0 0 0 0 0 1 1 1 0 0 1 0 1 1 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 0 1 1 0 1 0 0 1 0 0 1 0 0 0 1 0 1 0 1 0 1 1 1 0
[820] 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 1 0 1 1 0 1 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 1 1 1 1 0 0 0 1 0 0 0 0 1 1 1 0 0 0 0 1 0 1 1 1 0 1 0 1 1 0 0 0 1 0 1 0 0 0 0 1
[911] 1 1 0 0 0 1 0 0 0 0 0 1 0 0 0 1 1 0 0 0 0 0 0 1 1 0 0 0 0 0 1 1 0 1 1 0 0 0 0 0 0 1 0 1 0 1 1 0 0 0 0 0 0 0 0 1 0 1 1 0 0 1 0 1 0 0 1 0 0 1 1 1 1 1 1 1 0 1 1 0 0 1 0 0 0 1 1 1 0 1
[ reached getOption("max.print") -- omitted 15256 entries ]
> summary(subset_time_1$discrim_bin)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.0000  0.0000  0.0000  0.4852  1.0000  1.0000 
> frequency(subset_time_1$discrim_bin)
[1] 1
> summary(subset_time_1$discrim_bin)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.0000  0.0000  0.0000  0.4852  1.0000  1.0000 
> table(subset_time_1$discrim_bin)

0    1 
8368 7888 
> > table(subset_time_2$discrim_bin)
Error: unexpected '>' in ">"
> 
  >  table(subset_time_2$discrim_bin)

0    1 
4027 6225 
> 
  >  table(subset_time_3$discrim_bin)

0    1 
3307 1857 
> 
  >  table(subset_time_4$discrim_bin)

0 1 
9 2 
> 
  >  table(subset_time_5$discrim_bin)
< table of extent 0 >
  > 
  >