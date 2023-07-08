
current_directory = "/Users/k2147340/OneDrive - King's College London/Documents/"

OUTPUT_ROOT = "C:/Users/k2147340/OneDrive - King's College London/Desktop/"
dataset_noNAs_timepoints = read.csv(paste(OUTPUT_ROOT, "dataset_noNAs_timepoints_TEST_DELETE_AFTER_debugging_24nov2022.csv",  sep = ""))
#write.csv(dataset_noNAs_timepoints, "/Users/aliyaamirova/Desktop/removing_NAs_PA_MI/dataset_noNAs_timepoints_TEST_DELETE_AFTER_debugging_15jan2023_PA_MI.csv")
#dataset_noNAs_timepoints = read.csv("/Users/aliyaamirova/Desktop/removing_NAs_PA_MI/dataset_noNAs_timepoints_TEST_DELETE_AFTER_debugging_15jan2023_PA_MI.csv")

# Remove participant with sneaky NA in their sex variable.
SOURCE_ROOT = paste(current_directory, "proj/Cumulative_effects_HRS/Reviewer_response/Version_2_analysis/", sep="")
source((paste(SOURCE_ROOT, "participant_char_function.R", sep="")))

### There is no 
#data_v2 = read.csv("C:/Users/k2147340/OneDrive - King's College London/Desktop/randhrs1992_2018v1.csv")

"C:/Users/k2147340/OneDrive - King's College London/Desktop"


dataset_noNAs_timepoints<-dataset_noNAs_timepoints[!(dataset_noNAs_timepoints$HHIDPN==138300010),]

sex_na = c(138300010)
#  Remove participant with sneaky NA in their BMI variable.

BMI_null_ids = c(18525020,  20634010,  24040011,  50158020,  53033031,  57630010,
                 58974010,  81751020,  82648040,  85482020,  85630010,  87287020,
                 87549040, 115355020, 153089010, 153479010, 184591010, 500498010,
                 500582020, 502068011, 502328010, 502490010,  13635010,  39357040,
                 42943020,  46558020,  47582011,  57644040,  72327040,  77744020,
                 139930020, 141144020, 210253010, 500281020, 500286020, 501717010,
                 502021010, 502226020, 502364020, 521251010, 524179010, 527123010,
                 529360010, 532259020, 536403020, 537264020, 537283010, 902489020,
                 915508010, 915508020, 920200020,  14578040, 501219020, 526342020,
                 527180010, 526578010, 529828010, 912068010, 917056010, 917702010,
                 204659010, 212361010, 902805020, 914269010, 915940020,  11948020,
                 21675030,  47602020,  78026021,  80353040, 117310011, 200683020,
                 202973010, 205110021, 501646010,  12458020,  12745020,  13734020,
                 17234040,  18285030,  46641020,  59258030,  60232040,  60290030,
                 71012020, 138205010, 160233010, 212572010, 900500010, 919052010)


#  Remove participant with sneaky NA in their hypertension variable.

hypertension_null_ids =  c(10475010,  10645031,  11611010,  12135030,  12401040,  12512020,
                           13784010,  13814040,  13817011,  14167040,  14830020,  15195010,
                           15440040,  15657010,  16035010,  16035040,  16566020,  17363010,
                           19047020,  19458040,  19754010,  20777040,  21012030,  21058020,
                           21078010,  21717010,  22245010,  22544010,  22659011,  22665010,
                           23212010,  23756010,  25556010,  30644010,  31766020,  32634030,
                           33527030,  34231010,  35364020,  35364030,  35421010,  35849010,
                           35901030,  36179020,  36218020,  36236010,  36257010,  36787010,
                           36796040,  37106010,  37184030,  37735010,  38496020,  39101020,
                           39183020,  39279040,  40158021,  40276010,  40918020,  41129010,
                           41220020,  42493010,  42814010,  43512020,  44406010,  44647011,
                           45217010,  45992040,  46187010,  46443010,  46559010,  46697011,
                           46787010,  46821010,  47168010,  47320020,  47495010,  47707010,
                           47707020,  48406020,  48440040,  48622020,  48683010,  48825020,
                           48904010,  49219020,  49242010,  49396010,  49470010,  50173040,
                           50364040,  50385020,  50784010,  50822010,  50930010,  51003041,
                           51405020,  51937010,  51961010,  52234010,  52703040,  55394010,
                           55574020,  56076010,  56955011,  59063010,  60311010,  60547040,
                           60788010,  60956030,  61230010,  64609040,  65015010,  65428010,
                           65761010,  72544010,  73050020,  73878011,  75031010,  76081020,
                           76598020,  76870010,  76941010,  77697010,  77866010,  78495010,
                           78687020,  79180011,  79673010,  79900020,  80000010,  80116020,
                           80339011,  80593010,  81563020,  82314040,  83108010,  84218010,
                           84433020,  84454021,  84641020,  85992040,  86203010,  86354011,
                           86508011,  87402010,  87542020,  95650010, 111597020, 114508010,
                           114651010, 117988010, 121006020, 121178010, 121197011, 121480020,
                           122410010, 122520010, 131580010, 139864020, 140382010, 140753010,
                           141187010, 146110010, 146250010, 147661020, 148951010, 148951020,
                           149115010, 149570020, 151304020, 152575010, 153089020, 155821010,
                           157407010, 157483010, 159013010, 160030020, 162284010, 173877010,
                           173877020, 174031010, 178602020, 178905010, 178976010, 179898010,
                           183634010, 186920010, 200799020, 200823020, 200834020, 201328020,
                           201363020, 203311020, 204393010, 204415020, 204956010, 205001011,
                           205427010, 205537020, 205628020, 206478010, 206674011, 207459020,
                           207516010, 207901010, 210538010, 211000010, 211036010, 211120010,
                           211157010, 211185020, 211239010, 211296020, 211299020, 211972010,
                           212028020, 212198020, 212606010, 212840010, 212964020, 212993010,
                           213049011, 213117010, 213126020, 213189020, 213233010, 500009010,
                           500017020, 500138010, 500233010, 500249010, 500355010, 500370010,
                           500490010, 500498020, 500568020, 500580010, 500597010, 500605020,
                           500648010, 500649010, 500672010, 500719010, 500742010, 500800010,
                           500916010, 500921010, 500926010, 500956020, 501177010, 501187010,
                           501266020, 501331010, 501352010, 501359020, 501375010, 501439010,
                           501451010, 501511020, 501652010, 501659010, 501762020, 501840010,
                           501875020, 501939010, 501966010, 502088020, 502185010, 502221010,
                           502293020, 502314010, 502318010, 502336020, 502356010, 502370010,
                           502395020, 502514020, 502523020, 502671010, 502750010,  10109030,
                           12586010,  12611010,  12695030,  13552030,  13899041,  14188020,
                           14247010,  14270040,  14420010,  15225020,  15236010,  15516010,
                           15834010,  16048010,  16705020,  17075010,  17075020,  17952040,
                           21269010,  21629040,  22137020,  22243020,  22275010,  22738020,
                           22837040,  22861040,  23119030,  24500020,  25081020,  31633020,
                           32048010,  32481010,  32929010,  35330010,  36408030,  37865030,
                           38685020,  39753030,  39776040,  40552010,  40795030,  41046020,
                           41047010,  41880010,  41906010,  41942040,  42844010,  43179010,
                           43494010,  44391010,  44580011,  44994020,  45081011,  45500010,
                           45746040,  45984010,  46138010,  47578010,  47751010,  48615010,
                           48623021,  48982010,  48982020,  49542040,  50176040,  50365030,
                           50528020,  51228010,  52470010,  52672010,  53029011,  53266010,
                           53813010,  54082020,  56010010,  58752010,  58958010,  60136040,
                           60300010,  60594040,  60637010,  60656030,  65878040,  72327010,
                           73625010,  75035041,  76933010,  77532010,  77566010,  77788020,
                           77912040,  77921010,  77969011,  78465020,  78585020,  78747010,
                           78987020,  79760010,  79825010,  79867020,  79907020,  81113040,
                           81443020,  81553010,  81554020,  82179020,  83217020,  83494010,
                           83543010,  83904010,  84465010,  85577030,  85581040,  87200010,
                           110427010, 112435010, 112575010, 112747010, 115627010, 115842010,
                           118324010, 122704010, 124573020, 124773010, 130654020, 131774010,
                           131880010, 135681010, 136573010, 137464010, 137882010, 142813020,
                           143433020, 147421010, 150297010, 150370010, 151409010, 151986010,
                           154552010, 155507010, 155934010, 155965010, 161196020, 164374020,
                           164974020, 172868010, 173675020, 174335020, 178095020, 178447020,
                           178865010, 180872020, 182026020, 184830020, 185285010, 185285020,
                           186047020, 186438010, 203309020, 204330020, 204509020, 205095011,
                           205371020, 205966011, 206273010, 207430021, 207903020, 211367010,
                           211376010, 211418010, 211462010, 211814020, 211912020, 211936020,
                           211970010, 212080010, 212197010, 212200010, 212699010, 212931010,
                           213416010, 500070010, 500191010, 500204010, 500236012, 500273020,
                           500309010, 500311020, 500322020, 500367010, 500387010, 500391020,
                           500416021, 500438010, 500503010, 500663020, 500678010, 500721010,
                           500723010, 500759010, 500784020, 500900020, 500934010, 501045010,
                           501087021, 501131010, 501155010, 501299020, 501400020, 501405010,
                           501440010, 501448010, 501459020, 501530010, 501543010, 501550010,
                           501684010, 501686020, 501692020, 501699010, 501978010, 502021020,
                           502117020, 502146010, 502226020, 502349010, 502414020, 502433020,
                           502497010, 502529010, 502556010, 502627010, 502642010, 502657020,
                           502723020, 520084010, 520619010, 521672010, 521714020, 521849010,
                           521849020, 523768020, 523964010, 523970020, 524071020, 524076010,
                           524145010, 524213020, 524322010, 524371010, 524912010, 525037010,
                           525707020, 525764020, 526173010, 526336020, 526423010, 526689020,
                           529519020, 529782010, 530980020, 532544010, 533243010, 533605010,
                           533628020, 534426010, 534897010, 534984010, 536262020, 537500020,
                           545474010, 900269020, 900290020, 900777010, 901135020, 901174010,
                           902392020, 902971020, 903300010, 903529020, 905375010, 906543020,
                           907306020, 907690020, 908928010, 909841020, 911152010, 914265020,
                           914637010, 915367020, 915508010, 916600020, 918312010, 918388020,
                           918529010, 920200010, 527143020, 527208020, 904047010, 904025010,
                           10458020,  34086011,  40323020,  44169031,  50365031,  58493010,
                           73927010,  81198020,  82483040, 131812010, 140958010, 150525020,
                           185442010, 205363020, 501446010, 501839011, 520384011, 521317010,
                           522011020, 522123020, 523221020, 524037020, 528439010, 532097010,
                           901180011, 901477010, 902740010, 909569020, 909624011, 909889010,
                           914305020, 915988020) 


#remove cases that were included at all four follow-ups: 

were_present_at_four_points_ids = c(16602020,
                                    35607020,
                                    78026021,
                                    145768020,
                                    210031010,
                                    210661010,	
                                    501617010,	
                                    501870010,
                                    502417020,
                                    57714040,	
                                    916299010)



# no NA in CVD and checklist_depression bin variable 
#MI_null_ids = c(17661030,  19298020,  22150010,  57921010,  78509010, 138300010,
#172749010, 184925020, 211537020, 212253020, 212764010, 212904010,
#501848010)

#all_nas = c(sex_na, physical_activity_null_ids, hypertension_null_ids, BMI_null_ids)
all_nas = c(sex_na, hypertension_null_ids)

ID_nas = unique(all_nas)

length(ID_nas)


dataset_noNAs_timepoints<-dataset_noNAs_timepoints[!(dataset_noNAs_timepoints$HHIDPN %in% ID_nas),]

cases_with_CVD = subset(dataset_noNAs_timepoints,  CVD_ever == 1 & start_new == 0)

exclude_ids = unique(cases_with_CVD$HHIDPN)

data_flow_chart_withoutCVD <- subset(dataset_noNAs_timepoints,  !(HHIDPN %in% exclude_ids))


unique(data_flow_chart_withoutCVD$CVD_ever)
unique(data_flow_chart_withoutCVD$start_new)
nrow(data_flow_chart_withoutCVD)
nrow(data_flow_chart_withoutCVD)

length(unique(data_flow_chart_withoutCVD$HHIDPN))


data_flow_chart_withoutCVD_baseline = subset(data_flow_chart_withoutCVD, data_flow_chart_withoutCVD$start_new == 0)
nrow(data_flow_chart_withoutCVD_baseline)

#14901 vs 22731 

subset_withoutCVD_n = 14901
total_n = 22731+615

percent_without_cvd = (subset_withoutCVD_n*100)/total_n



write.csv(data_flow_chart_withoutCVD, file = (paste(OUTPUT_ROOT, "data_flow_chart_withoutCVD.csv", sep="")))

baseline_data_withoutCVD = subset(data_flow_chart_withoutCVD, data_flow_chart_withoutCVD$start_new == 0) 
followup1_data_withoutCVD = subset(data_flow_chart_withoutCVD, data_flow_chart_withoutCVD$start_new == 1) 
followup2_data_withoutCVD = subset(data_flow_chart_withoutCVD, data_flow_chart_withoutCVD$start_new == 2) 

withoutCVD = nrow(baseline_data_withoutCVD)
withoutCVD_followup1 = nrow(followup1_data_withoutCVD)
withoutCVD_followup2 = nrow(followup2_data_withoutCVD)


#######
#######
#######


flow_chart_drop_baseline_diabetes = function (data){
  
  
  ID = unique(data$HHIDPN)
  
  #print(isTRUE(data$HHIDPN == ID[1]))
  #data = data %>% drop_na()
  
  participant_wave_df = data.frame()
  
  n = 1
  for (id in ID){
    
    
    participant_wave = subset(data, data$HHIDPN == id)
    
    if (nrow(participant_wave)== 1){
      
      participant_wave$timepoints_indiv = 1
      
      
      participant_wave$start_new = c(0)
      participant_wave$stop_new = c(1)
      
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    }
    
    if (nrow(participant_wave) ==2){
      
      participant_wave$timepoints_indiv = 2
      
      participant_wave$start_new = c(0, 1)
      participant_wave$stop_new = c(1, 2)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    }
    
    if (nrow(participant_wave)==3){
      
      participant_wave$timepoints_indiv = 3
      
      participant_wave$start_new = c(0, 1, 2)
      participant_wave$stop_new = c(1, 2, 3)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    } 
    
    
    if (nrow(participant_wave)==4){
      
      participant_wave$timepoints_indiv = 4
      
      participant_wave$start_new = c(0, 1, 2, 3)
      participant_wave$stop_new = c(1, 2, 3, 4)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    } 
    
    if (nrow(participant_wave)==5){
      
      participant_wave$timepoints_indiv = 5
      
      participant_wave$start_new = c(0, 1, 2, 3, 4)
      participant_wave$stop_new = c(1, 2, 3, 4, 5)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    } 
    
    if (nrow(participant_wave)==6){
      
      participant_wave$timepoints_indiv = 6
      participant_wave$start_new = c(0, 1, 2, 3, 4, 5)
      participant_wave$stop_new = c(1, 2, 3, 4, 5, 6)
      participant_wave_df = rbind(participant_wave_df, participant_wave) 
      
    }
    n = n + 1
  }
  
  wave_1 = subset(participant_wave_df,  participant_wave_df$start_new == 0)
  wave_1_diabetes  = subset(wave_1, wave_1$diabetes_new == 1)
  
  
  diabetes_wave_1_unique = unique(wave_1_diabetes$HHIDPN)
  new_diabetes_wave1_dataset = length(diabetes_wave_1_unique)
  #wave 1: 224 cases 
  
  
  wave_2 = subset(participant_wave_df, participant_wave_df$start_new == 1)
  wave_2_diabetes  = subset(wave_2, wave_2$diabetes_new == 1)
  
  diabetes_wave_2_unique = unique(wave_2_diabetes$HHIDPN)
  new_diabetes_wave2_dataset = length(diabetes_wave_2_unique)
  #wave 2: 318 cases 
  
  
  wave_3 = subset(participant_wave_df, participant_wave_df$start_new == 2)
  wave_3_diabetes  = subset(wave_3, wave_3$diabetes_new == 1)
  
  
  diabetes_wave_3_unique = unique(wave_3_diabetes$HHIDPN)
  new_diabetes_wave3_dataset = length(diabetes_wave_3_unique)
  #wave 3: 233 cases 
  
  
  #wave_4 = subset(participant_wave_df, participant_wave_df$start_new == 3)
  #wave_4_diabetes  = subset(wave_4, wave_4$diabetes_new == 1)
  
  
  #diabetes_wave_4_unique = unique(wave_4_diabetes$HHIDPN)
  #new_diabetes_wave4_dataset = length(diabetes_wave_4_unique)
  #wave 4: 174 cases 
  
  
  #wave_5 = subset(participant_wave_df, participant_wave_df$start_new == 4)
  #wave_5_diabetes  = subset(wave_5, wave_5$diabetes_new == 1)
  
  
  #diabetes_wave_5_unique = unique(wave_5_diabetes$HHIDPN)
  #new_diabetes_wave5_dataset = length(diabetes_wave_5_unique)
  #
  
  #wave_6 = subset(participant_wave_df, participant_wave_df$start_new == 5)
  #wave_6_diabetes  = subset(wave_6, wave_6$diabetes_new == 1)
  
  
  #diabetes_wave_6_unique = unique(wave_6_diabetes$HHIDPN)
  #new_diabetes_wave6_dataset = length(diabetes_wave_6_unique)
  
  
  
  new_diabetes_each_wave = rbind(new_diabetes_wave2_dataset,
                                 new_diabetes_wave3_dataset) 
  #new_diabetes_wave4_dataset,
  #new_diabetes_wave5_dataset,
  #new_diabetes_wave6_dataset)
  
  write.csv(new_diabetes_each_wave, (paste(OUTPUT_ROOT, "new_diabetes_each_wave_DIAB.csv", sep="")))
  
  all_waves = rbind(wave_1, 
                    wave_2, 
                    wave_3) 
  #wave_4, 
  #wave_5, 
  #wave_6)
  
  
  
  return(all_waves)
  
}
  
### cases with CVD: 
data_flow_chart = flow_chart_drop_baseline_diabetes(data = data_flow_chart_withoutCVD) 
write.csv(data_flow_chart, file = (paste(OUTPUT_ROOT, "data_flow_chart_withoutbaselineCVD.csv", sep="")))

unique(data_flow_chart$start_new)
baseline_data = subset(data_flow_chart, data_flow_chart$start_new == 0) 
followup1_data = subset(data_flow_chart, data_flow_chart$start_new == 1) 
followup2_data = subset(data_flow_chart, data_flow_chart$start_new == 2) 

baseline_n = nrow(baseline_data)
baseline_n_followup1 = nrow(followup1_data)
baseline_n_followup2 = nrow(followup2_data)

wce_subset_3 = subset(data_flow_chart, data_flow_chart$stop_new ==3)
ids_t3 = unique(wce_subset_3$HHIDPN)

wce_subset_2 = subset(data_flow_chart, data_flow_chart$stop_new ==2)
ids_t2 = unique(wce_subset_2$HHIDPN)


wce_subset_1 = subset(data_flow_chart, data_flow_chart$stop_new ==1)
ids_t1 = unique(wce_subset_1$HHIDPN)

ids_accross_all_ts = cbind(ids_t3,
                           ids_t2, 
                           ids_t1)

diff_ids = setdiff(ids_t3,  
                   ids_t2)


diff_ids2 = setdiff(ids_t2,
                    ids_t1)



intersect(intersect(ids_t3,ids_t2),ids_t1)
wce_total = Reduce(intersect, list(ids_t3,ids_t2,ids_t1))
wce_total_n = length(wce_total)

time_point = c("0", "1", "2", "wce_present_at_3ts")

total_n = c(baseline_n, baseline_n_followup1, baseline_n_followup2, wce_total_n)
table = cbind(time_point, total_n) 

write.csv(table, file = (paste(OUTPUT_ROOT, "n_flow_chart_withoutbaselineCVD.csv", sep="")))




unique(data_flow_chart$CVD_ever)



##### calculate the percentages for the drop in cases in CVD: at baseline, at follow-up and at second follow-up.

percent_withCVD_baseline =  (withoutCVD*100)/baseline_n
withoutCVD_followup1 =  (withoutCVD_followup1*100)/baseline_n_followup1
withoutCVD_followup2 =  (withoutCVD_followup2*100)/baseline_n_followup2