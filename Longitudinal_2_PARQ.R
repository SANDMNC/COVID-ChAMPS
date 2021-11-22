# Read in the longitudinal dataset

longitudinal <- read.csv("scored_data/longitudinalset.csv", header=TRUE, 
                          stringsAsFactors = FALSE)

# put into a smaller dataframe the items from T2, the matching ones from T1 plus the missing one

PARQ_for_imp<- longitudinal[,c("ch_parenting.1_1_num","ch_parenting.1_4_num","ch_parenting.1_11_num","ch_parenting.1_15_num",
  "ch_parenting.1_21_num","ch_parenting.1_23_num","ch_parenting.1_27_num","ch_parenting.1_29_num", 
  "ch_parenting.1_5_num","ch_parenting.1_8_num","ch_parenting.1_12_num","ch_parenting.1_17_num", 
  "ch_parenting.1_22_num","ch_parenting.1_24_num","ch_parenting_f_1_num_f","ch_parenting_f_2_num_f", 
  "ch_parenting_f_5_num_f", "ch_parenting_f_7_num_f","ch_parenting_f_10_num_f","ch_parenting_f_12_num_f",
  "ch_parenting_f_13_num_f", "ch_parenting_f_3_num_f","ch_parenting_f_4_num_f","ch_parenting_f_6_num_f",
   "ch_parenting_f_8_num_f","ch_parenting_f_9_num_f","ch_parenting_f_11_num_f")]

# What's missing
# ch1_parenting#1_21 not repeated at followup
# "I make my child feel that what (s)he does is important"
#This will be w5 missing in longitudinal

# Append the names to have warmth on the end and a number to match T1 and T2
# w = warmth, h = hostile, t1  = baseline, t2 = followup

colnames(PARQ_for_imp) <- c("ch_parenting.1_1_num_T1w1","ch_parenting.1_4_num_T1w2","ch_parenting.1_11_num_T1w3",
                            "ch_parenting.1_15_num_T1w4", "ch_parenting.1_21_num_T1w5","ch_parenting.1_23_num_T1w6",
                            "ch_parenting.1_27_num_T1w7", "ch_parenting.1_29_num_T1w8", 
                            "ch_parenting.1_5_num_T1h1","ch_parenting.1_8_num_T1h2","ch_parenting.1_12_num_T1h3",
                            "ch_parenting.1_17_num_T1h4", "ch_parenting.1_22_num_T1h5","ch_parenting.1_24_num_T1h6",
                            "ch_parenting_f_1_num_f_T2w1","ch_parenting_f_2_num_f_T2w2",  "ch_parenting_f_5_num_f_T2w3",
                            "ch_parenting_f_7_num_f_T2w4","ch_parenting_f_10_num_f_T2w6","ch_parenting_f_12_num_f_T2w7",
                            "ch_parenting_f_13_num_f_T2w8",
                            "ch_parenting_f_3_num_f_T2h1","ch_parenting_f_4_num_f_T2h2","ch_parenting_f_6_num_f_T2h3",
                            "ch_parenting_f_8_num_f_T2h4","ch_parenting_f_9_num_f_T2h5","ch_parenting_f_11_num_f_T2h6")

#Add in a missing placeholder variable
PARQ_for_imp$ch_parenting_f_T2w5<- rep(NA, nrow(PARQ_for_imp))

#reference info
# T1
# PARQwarmth=c("-ch_parenting.1_1_num","-ch_parenting.1_4_num","-ch_parenting.1_11_num",
#              "-ch_parenting.1_15_num","-ch_parenting.1_21_num","-ch_parenting.1_23_num",
#               "-ch_parenting.1_27_num","-ch_parenting.1_29_num"), 
# PARQhostile=c("ch_parenting.1_5_num","ch_parenting.1_8_num","ch_parenting.1_12_num",
#                "ch_parenting.1_17_num","ch_parenting.1_22_num","ch_parenting.1_24_num")
#                       
# T2
# PARQwarmth=c("-ch_parenting.f_1_num","-ch_parenting.f_2_num", "-ch_parenting.f_5_num",
#               "-ch_parenting.f_7_num","-ch_parenting.f_10_num","-ch_parenting.f_12_num",
#                "-ch_parenting.f_13_num"), 
# PARQhostile=c("ch_parenting.f_3_num","ch_parenting.f_4_num","ch_parenting.f_6_num",
#                "ch_parenting.f_8_num","ch_parenting.f_9_num","ch_parenting.f_11_num"))
# 
# 

