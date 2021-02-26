
#Script made with R version 3.5.2 (2018-12-20) - edits made with R version 4.0.2 (2020-06-22)

# Inputs:
# csv file exported from Qualtrics
# Exported with "choice text" rather than numeric for meantime.
# Manually deleted the email column to deidentify, and the 2nd and 3rd rows with additional qualtrics headings.

# Outputs:
# Csv files with combination of raw and scored data, for use in the missing and analyses scripts.
# First csv each parent is a separate row.
# Second csv each child is a separate row.

# Open script after opening the project file - COVID-ChAMPS.Rproj


# Still to do... 
# imputation of any missing if there are

# Load packages --------------------------------------------------------------------
library(car) # for recode
library(dplyr) # for %>%
library(tidyselect) # for starts_with and contains
library(psych) #for scoreItems etc.


# General cleaning----------------------------------------------------------------------------------------------

# Read in the csv file
covid_data <- read.csv("raw_data/covid_data_fu.csv", header=TRUE, stringsAsFactors = FALSE)

#Add unique ID for each participant
id <- rownames(covid_data)
covid_data <- cbind(id=id, covid_data)

#Age variable
#Make the age variables numeric rather than characters
covid_data$ch1_age <- as.numeric(covid_data$ch1_age_f) 
covid_data$ch2_age <- as.numeric(covid_data$ch2_age_f) 
covid_data$ch3_age <- as.numeric(covid_data$ch3_age_f)

#Missing values - change them from "" to NA to make easier to work with
covid_data[(covid_data=="")] <- NA

# Inclusion criteria ---------------------------------------------------------------------------------
## Participants should have completed at least 70% of either SDQ or PTSD for at least 1 child 

#Examine the PTSD and SDQ items
varnames <- names(covid_data)
#How many entries have at least 70% of the SDQ or PTSD answers
## Must have greater than or equal to 10/15 for SDQ1 and 11/15 for PTSD
nrow(covid_data[which(rowSums(!is.na(covid_data[, grep("ch1_SDQ_f", varnames)]))>=10), ])

nrow(covid_data[which(rowSums(!is.na(covid_data[, grep("ch1_PTSD_f", varnames)]))>=11), ])

#How many have either the SDQ or the PTSD criteria
nrow(covid_data[which(rowSums(!is.na(covid_data[, grep("ch1_SDQ_f", varnames)]))>=10 | rowSums(!is.na(covid_data[, grep("ch1_PTSD_f", varnames)]))>=11), ])

#cutting ineligible people based on SDQ and PSD out of larger sample
covid_data <- covid_data[which(rowSums(!is.na(covid_data[, grep("ch1_SDQ_f", varnames)]))>=10 | rowSums(!is.na(covid_data[, grep("ch1_PTSD_f", varnames)]))>=11), ]

# Demographics parents-------------------------------------------------------------------------------------------

#Now recode the income to the midpoint 
covid_data$income_mid<- ifelse(covid_data$income_change == "Less than AU$699 per week (AU$36,400 or GBPÂ£20,500 per year)", 18200, 
                               ifelse(covid_data$income_change == "AU$700 to AU$999 per week (up to AU$52,000 or GBPÂ£29,280 per year)", 44200,
                                      ifelse(covid_data$income_change == "AU$1,000 to AU$1,730 per week (up to AU$90,000 or GBPÂ£50,680 per year)", 71000, 
                                             ifelse(covid_data$income_change == "AU$1,731 to AU$2,700 per week (up to AU$140,000 or GBPÂ£78,800 per year)", 115000,
                                                    ifelse(covid_data$income_change == "More than AU$2,700 per week (AU$140,000 or GBPÂ£78,800 per year)", 140000, NA
                                                    )))))


#Use this later after the merge
# #Midpoint of bracket divided by the sqrt of people in the house
# covid_data$income_famsize_f<-covid_data$income_f_mid/sqrt(as.numeric(covid_data$home_num))

#Other demographics relating to each child - contained in child section
# COVID impact ------------------------------------------------------------------------------------------

##known 
covid_data$other_covid <- as.factor(covid_data$other_covid)

##known hospitalized, 
covid_data$other_hosp <- as.factor(covid_data$other_hosp)
covid_data$other_hosp_num<- ifelse(covid_data$other_hosp == "No", 0,1)

##known died, 
covid_data$other_died <- as.factor(covid_data$other_died)

# COVID distress ------------------------------------------------------------


# Includes financial problems - covid_finance, uncertainty stress - covid_uncertain, stress related to existing plans disruption - covid_plans,
# infection worry personal, friends/family - covid_worry_1 and covid_worry_2, mental emotional health impact and life changes worry - covid_worry_3,
# negative life impact and positive - could reverse score for a distress measure - covid_neg and covid_pos

#Grab the relevant variable names
covid_vars <- vars_select(varnames, starts_with("covid"))
# Not all covid variables relevant - just pick the ones to score the distress
covid_vars <- covid_vars[c(2:8, 11)]

#blank dataframe
covidnumvars=data.frame(matrix(ncol=length(covid_vars),nrow=nrow(covid_data[,covid_vars])))

#recodes as ordinal
covidnumvars <-ifelse(covid_data[,covid_vars] == "Not at all", 0, 
                      ifelse(covid_data[,covid_vars] == "Very Slightly", 1, 
                             ifelse(covid_data[,covid_vars] == "Slightly", 2,
                                    ifelse(covid_data[,covid_vars] == "Moderately", 3,
                                           ifelse(covid_data[,covid_vars] == "Quite a Bit", 4,
                                                  5)))))
#Put new variable names in
colnames(covidnumvars)<- c("covid_finance_num", "covid_uncertain_num", "covid_plans_num", "covid_worry_1_num", 
                           "covid_worry_2_num", "covid_worry_3_num", "covid_neg_num", "covid_pos_num")

#compute Cronbach's alpha for neg scale (std.alpha is 0.84)
covid_vars_neg <- subset(covidnumvars, select = -covid_pos_num)
psych::alpha(covid_vars_neg)

#join this dataframe to the main one
covid_data<- cbind(covid_data, covidnumvars)

#Scoring the covid distress - need to add the >70% impute missing
#scoring code from psych package. Note "-" before items reverse scored.
# Need to rearrange this in a better way than than just 1 total
keys.list.cov.dist <- list(totalcov_dist=c("covid_finance_num", "covid_uncertain_num", "covid_plans_num", "covid_worry_1_num", 
                                           "covid_worry_2_num", "covid_worry_3_num", "covid_neg_num", "-covid_pos_num"))

cov_dist_scored <- scoreItems(keys.list.cov.dist, covid_data, impute= "mean", totals=TRUE, min=0,max=5) # note - FALSE:average, TRUE:sum

cov_dist_scored$scores #The scores themselves
cov_dist_totals <- as.data.frame(cov_dist_scored$scores)#just the total scores
covid_data <- cbind(covid_data, cov_dist_totals)#totals and raw scores

#Get rid of participants total scores who have less than 70% of items (need 6/8)
## This line tells you how people had more than 2 missing (but not the whole scale missing)

##SW: Up to here with follow-up scoring 22/2/21, 2.30pm

# Parent mental health-------------------------------------------------------------------------------------------

# DASS scoring -------------------------------------------------------------------------------------------

DASSvars <- vars_select(varnames, starts_with("DASS"))

#Creates a blank dataframe to put the scores into 
DASSnumvars=data.frame(matrix(ncol=21,nrow=nrow(covid_data[,DASSvars])))

#recodes the words to numbers
DASSnumvars<- ifelse(covid_data[,DASSvars] == "Never", 0, 
                     ifelse(covid_data[,DASSvars] == "Sometimes", 1, 
                            ifelse(covid_data[,DASSvars] == "Often", 2, 3)))

#Put the names of the new variables in
colnames(DASSnumvars)<- c("DASS_1_num", "DASS_2_num", "DASS_3_num", "DASS_4_num", "DASS_5_num", "DASS_6_num", "DASS_7_num", "DASS_8_num", "DASS_9_num", "DASS_10_num",
                          "DASS_11_num", "DASS_12_num", "DASS_13_num", "DASS_14_num", "DASS_15_num", "DASS_16_num", "DASS_17_num", "DASS_18_num", "DASS_19_num", "DASS_20_num",
                          "DASS_21_num" )

#join this dataframe to the main one
covid_data<- cbind(covid_data, DASSnumvars)

#Score each and also total
keys.listDASS <- list(totalDASS=c("DASS_1_num", "DASS_2_num", "DASS_3_num", "DASS_4_num", "DASS_5_num", "DASS_6_num", "DASS_7_num", "DASS_8_num", "DASS_9_num", "DASS_10_num",
                                  "DASS_11_num", "DASS_12_num", "DASS_13_num", "DASS_14_num", "DASS_15_num", "DASS_16_num", "DASS_17_num", "DASS_18_num", "DASS_19_num", "DASS_20_num",
                                  "DASS_21_num"),
                      DASSDep=c("DASS_3_num","DASS_5_num","DASS_10_num","DASS_13_num","DASS_16_num","DASS_17_num","DASS_21_num"), 
                      DASSAnx=c("DASS_2_num","DASS_4_num","DASS_7_num","DASS_9_num","DASS_15_num","DASS_19_num","DASS_20_num"),
                      DASSStress=c("DASS_1_num","DASS_6_num","DASS_8_num","DASS_11_num","DASS_12_num","DASS_14_num","DASS_18_num"))

DASSscored <- scoreItems(keys.listDASS, covid_data, impute= "mean", totals=TRUE, min=0,max=3) # note - FALSE:average, TRUE:sum
DASSscored$scores #The scores themselves
DASS_totals <- as.data.frame(DASSscored$scores)#just the total scores
covid_data <- cbind(covid_data, DASS_totals)#totals and raw scores


#Section to remove people who are missing more than 70% of their DASS scores (need 15/21) - so if more than 6 missing get rid of (or 5/7 for subscales so no more than 2 missing)
nrow(covid_data[rowSums(is.na(covid_data[, keys.listDASS$totalDASS]))>6,])
covid_data[which(rowSums(is.na(covid_data[, keys.listDASS$totalDASS]))>6),"totalDASS"] <- NA
nrow(covid_data[rowSums(is.na(covid_data[, keys.listDASS$DASSStress]))>2,])
covid_data[which(rowSums(is.na(covid_data[, keys.listDASS$DASSDep]))>2),"DASSDep"] <- NA
nrow(covid_data[rowSums(is.na(covid_data[, keys.listDASS$DASSAnx]))>2,])
covid_data[which(rowSums(is.na(covid_data[, keys.listDASS$DASSAnx]))>2),"DASSAnx"] <- NA
nrow(covid_data[rowSums(is.na(covid_data[, keys.listDASS$DASSStress]))>2,])
covid_data[which(rowSums(is.na(covid_data[, keys.listDASS$DASSStress]))>2),"DASSStress"] <- NA

#Create new categorical variable for each subscale putting them in categories
covid_data$DepCat <- as.factor(car::recode(covid_data$DASSDep, "NA=NA; 0:4='Normal';5:6='Mild';7:10='Moderate'; 11:13='Severe'; 
                                           else='Ex_severe'"))

covid_data$AnxCat <- as.factor(car::recode(covid_data$DASSAnx, "NA=NA; 0:3='Normal';4:5='Mild';6:7='Moderate'; 8:9='Severe'; 
                                           else='Ex_severe'"))

covid_data$StressCat <- as.factor(car::recode(covid_data$DASSStress, "NA=NA; 0:7='Normal';8:9='Mild';10:12='Moderate'; 13:16='Severe'; 
                                              else='Ex_severe'"))



# FES cohesion scoring -------------------------------------------------------------------------------------------
FESvars <- vars_select(varnames, starts_with("cohesion"))

#Creates a blank dataframe to put the scores into
FESnumvars=data.frame(matrix(ncol=9,nrow=nrow(covid_data[,FESvars])))

#recodes the words to numbers
FESnumvars<- ifelse(covid_data[,FESvars] == "Disagree", 0, 1)                  

#Put the names of the new variables in
colnames(FESnumvars)<- c("cohesion_1_num", "cohesion_2_num", "cohesion_3_num", "cohesion_4_num", "cohesion_5_num", "cohesion_6_num", "cohesion_7_num", "cohesion_8_num", "cohesion_9_num")

#join this dataframe to the main one
covid_data<- cbind(covid_data, FESnumvars)

#Scoring the FES - need to add the >70% impute missing
#scoring code from psych package. Note "-" before items reverse scored.
keys.listFES <- list(totalFES=c("cohesion_1_num" ,  "cohesion_2_num" , "-cohesion_3_num" , "cohesion_4_num"  , "cohesion_5_num" , "cohesion_6_num" ,  "-cohesion_7_num" , "-cohesion_8_num" , "-cohesion_9_num"))

FESscored <- scoreItems(keys.listFES, covid_data, impute= "mean", totals=TRUE, min=0,max=1) # note - FALSE:average, TRUE:sum

FESscored$scores #The scores themselves
FES_totals <- as.data.frame(FESscored$scores)#just the total scores
covid_data <- cbind(covid_data, FES_totals)#totals and raw scores

#Get rid of participants total scores who have less than 70% of items (need 7/9)
## This line tells you how people had more than 2 missing (but not the whole scale missing)
nrow(covid_data[rowSums(is.na(covid_data[, colnames(FESnumvars)]))>2 & rowSums(is.na(covid_data[, colnames(FESnumvars)])) <9,])
#This line should replace all missing more than 2 with NA to the total 
covid_data[which(rowSums(is.na(covid_data[, colnames(FESnumvars)]))>2),"totalFES"] <- NA


# Children ----------------------------------------------------------------------------------------------
#Create separate dataframe for children (stack all of the children on top of each other)

#Collect all the variables relevant for each child

ch1vars <- vars_select(varnames, contains("ch1"))
ch2vars <- vars_select(varnames, contains("ch2"))
ch3vars <- vars_select(varnames, contains("ch3"))
ch4vars <- vars_select(varnames, contains("ch4"))
ch5vars <- vars_select(varnames, contains("ch5"))
ch6vars <- vars_select(varnames, contains("ch6"))

#Create a dataframe for each set of children, by cutting out the other childrens variables
covid_data_ch1<- covid_data[, !(names(covid_data) %in% c(ch2vars, ch3vars, ch4vars, ch5vars, ch6vars))]
covid_data_ch2<- covid_data[, !(names(covid_data) %in% c(ch1vars, ch3vars, ch4vars, ch5vars, ch6vars))]
covid_data_ch3<- covid_data[, !(names(covid_data) %in% c(ch1vars, ch2vars, ch4vars, ch5vars, ch6vars))]
covid_data_ch4<- covid_data[, !(names(covid_data) %in% c(ch1vars, ch2vars, ch3vars, ch5vars, ch6vars))]
covid_data_ch5<- covid_data[, !(names(covid_data) %in% c(ch1vars, ch2vars, ch3vars, ch4vars, ch6vars))]
covid_data_ch6<- covid_data[, !(names(covid_data) %in% c(ch1vars, ch2vars, ch3vars, ch4vars, ch5vars))]

#Getting rid of the child number from the variable name to prepare variables to be merged, e.g. ch1_age and ch2_age become ch_age
names(covid_data_ch1) <- sub("ch1_", "ch_", names(covid_data_ch1))
names(covid_data_ch2) <- sub("ch2_", "ch_", names(covid_data_ch2))
names(covid_data_ch3) <- sub("ch3_", "ch_", names(covid_data_ch3))
names(covid_data_ch4) <- sub("ch4_", "ch_", names(covid_data_ch4))
names(covid_data_ch5) <- sub("ch5_", "ch_", names(covid_data_ch5))
names(covid_data_ch6) <- sub("ch6_", "ch_", names(covid_data_ch6))

#Add in dummy trigger question for first child to allow easier merging
covid_data_ch1$ch_trigger <- NA
#Change it to a character type variable otherwise it won't match the original variable for the merge
covid_data_ch1$ch_trigger<- as.character(covid_data_ch1$ch_trigger)

#Delete blank children by use of Yes in trigger column

covid_data_ch2<-covid_data_ch2[covid_data_ch2$ch_trigger=="Yes" & !is.na(covid_data_ch2$ch_trigger),]
covid_data_ch3<-covid_data_ch3[covid_data_ch3$ch_trigger=="Yes" & !is.na(covid_data_ch3$ch_trigger),]
covid_data_ch4<-covid_data_ch4[covid_data_ch4$ch_trigger=="Yes" & !is.na(covid_data_ch4$ch_trigger),]
covid_data_ch5<-covid_data_ch5[covid_data_ch5$ch_trigger=="Yes" & !is.na(covid_data_ch5$ch_trigger),]
covid_data_ch6<-covid_data_ch6[covid_data_ch6$ch_trigger=="Yes" & !is.na(covid_data_ch6$ch_trigger),]


#See how many children were entered in the additional rounds
#How many parents entered at least 1 child?
nrow(covid_data)
#at least 2 children entered
nrow(covid_data_ch2)
#at least 3 children
nrow(covid_data_ch3)
#at least 4 children
nrow(covid_data_ch4)
#at least 5 children
nrow(covid_data_ch5)
#at least 6 children
nrow(covid_data_ch6)
#Total children
nrow(covid_data)+nrow(covid_data_ch2)+nrow(covid_data_ch3)+nrow(covid_data_ch4)+nrow(covid_data_ch5)+nrow(covid_data_ch6)


#ways to check if dataframes have same variables before  merge
#identical(covid_data_ch1, covid_data_ch2)
#summary(comparedf(covid_data_ch1, covid_data_ch2)) #this function is from the arsenal package)



# There was an error in the collection of child 2 onwards - more data than intended was collected
# Ch1 had only hyperactivity, emotion, and conduct subscales, ch2 onwards had extra items not removed
# Firstly get rid of extra items from ch2 onwards to make it uniform across the children, 
# and change the names to be matching with ch1

relabel <- function(my_data){
  
  drops <- c("ch_SDQ_f_7","ch_SDQ_f_4","ch_SDQ_f_9","ch_SDQ_f_12","ch_SDQ_f_16","ch_SDQ_f_19")
  my_data <- my_data[ , !(names(my_data) %in% drops)]
  
  my_data %>% 
    rename(
      ch_SDQ_f_4 = ch_SDQ_f_5,
      ch_SDQ_f_5 = ch_SDQ_f_6,
      ch_SDQ_f_6 = ch_SDQ_f_8,
      ch_SDQ_f_7 = ch_SDQ_f_10,
      ch_SDQ_f_8 = ch_SDQ_f_11,
      ch_SDQ_f_9 = ch_SDQ_f_13,
      ch_SDQ_f_10 = ch_SDQ_f_14,
      ch_SDQ_f_11 = ch_SDQ_f_15,
      ch_SDQ_f_12 = ch_SDQ_f_17,
      ch_SDQ_f_13 = ch_SDQ_f_18,
      ch_SDQ_f_14 = ch_SDQ_f_20,
      ch_SDQ_f_15 = ch_SDQ_f_21,
      )

  
}

covid_data_ch2 <- relabel(covid_data_ch2)
covid_data_ch3 <- relabel(covid_data_ch3)

# #Found that this variable was different - inconsistency in data asked between different children
# names(covid_data_ch1)[names(covid_data_ch1)=="ch_med_source_7_TEXT"] <- "ch_med_source_6_TEXT"

#Merge all the different children into 1 dataset
covid_data_child<- rbind(covid_data_ch1, covid_data_ch2, covid_data_ch3)

#Add on a second ID - to give each child a unique identifier
covid_data_child$id2<- 1:nrow(covid_data_child)


# Demographics for children -------------------------------------------------------------------------------------------------
#Will check this out soon - KB

# ## Gender of child
#   # Note that 1 transgender child, but not male or female described, one child Other - 'X' - put into NA
# covid_data_child$ch_gender <- as.factor(covid_data_child$ch_gender)

# There are 2 children labeled as both males and female - on further investigation, using the earlier child table 
# found that one of them was initially labeled male [row 195] and one female [457]
#Check the row numbers for the children
# which(covid_data_child$ch_gender == "Male,Female")
# # Hardcode the gender for these 2 children
# covid_data_child[195,"ch_gender"] <- "Male"
# covid_data_child[457,"ch_gender"] <- "Female"
# #Recode to male, female and NA
# covid_data_child$ch_gender_num<- ifelse(covid_data_child$ch_gender == "Female", 1,
#                                         ifelse(covid_data_child$ch_gender == "Male", 2,
#                                                ifelse(covid_data_child$ch_gender == "Male,Prefer not to say", 2,
#                                                       NA)))


# ## Age of child
# #Note: this indicates some children yonger than 5 - need to remove them
# covid_data_child[covid_data_child$ch_age<5 & !is.na(covid_data_child$ch_age), "ch_age"]
# #Remove these children from dataset
# covid_data_child<- covid_data_child[covid_data_child$ch_age>=5 , ]
# 


# COVID communication -----------------------------------------------------------------------------------------------------

childvarnames <- names(covid_data_child)
#Grab the relevant variable names
comm_vars <- vars_select(childvarnames, starts_with("ch_talk"))

#blank dataframe
commnumvars=data.frame(matrix(ncol=length(comm_vars),nrow=nrow(covid_data_child[,comm_vars])))

#recodes as ordinal
commnumvars <-ifelse(covid_data_child[,comm_vars] == "Not at all", 0, 
                     ifelse(covid_data_child[,comm_vars] == "A little", 1, 
                            ifelse(covid_data_child[,comm_vars] == "Moderately", 2,3)))

#Put new variable names in
colnames(commnumvars)<- c("ch_talk_num", "ch_talk_about_1_num", "ch_talk_about_2_num", "ch_talk_about_3_num", 
                          "ch_talk_about_4_num", "ch_talk_about_5_num", "ch_talk_about_6_num", "ch_talk_about_7_num",
                          "ch_talk_about_8_num")

#join this dataframe to the main one
covid_data_child<- cbind(covid_data_child, commnumvars)



#Scoring the communication - need to add the >70% impute missing
#scoring code from psych package. 
# Items grouped according to correlations between them and inline with Wilson, et al (2010). Journal of Clinical Child & Adolescent Psychology, 39(4), 445-459.
# While Wilson et al. separated emotion and reassurance, for us, reassurance correlates highly with emotion items (.67 with child's emotions)
keys.list.cov.comm <- list(facts_comm=c("ch_talk_about_1_num", "ch_talk_about_2_num"), 
                           emotion_comm=c("ch_talk_about_3_num", "ch_talk_about_4_num", 
                                          "ch_talk_about_5_num", "ch_talk_about_6_num"), 
                           self_comm=c("ch_talk_about_7_num", "ch_talk_about_8_num"))

cov_comm_scored <- scoreItems(keys.list.cov.comm, covid_data_child, impute= "mean", totals=TRUE, min=0,max=3) # note - FALSE:average, TRUE:sum

cov_comm_scored$scores #The scores themselves
cov_comm_totals <- as.data.frame(cov_comm_scored$scores)#just the total scores
covid_data_child <- cbind(covid_data_child, cov_comm_totals)#totals and raw scores

#Get rid of participants total scores who have less than 70% of items
## This line tells you how people had more than X missing (but not the whole scale missing)
nrow(covid_data_child[rowSums(is.na(covid_data_child[, c( "ch_talk_about_1_num", "ch_talk_about_2_num")]))>1 & rowSums(is.na(covid_data_child[, c( "ch_talk_about_1_num", "ch_talk_about_2_num")])) <2,]) 
nrow(covid_data_child[rowSums(is.na(covid_data_child[, c( "ch_talk_about_3_num", "ch_talk_about_4_num", "ch_talk_about_5_num", "ch_talk_about_6_num")]))>1 & rowSums(is.na(covid_data_child[, c( "ch_talk_about_3_num", "ch_talk_about_4_num", "ch_talk_about_5_num", "ch_talk_about_6_num")])) <3,]) 
nrow(covid_data_child[rowSums(is.na(covid_data_child[, c( "ch_talk_about_7_num", "ch_talk_about_8_num")]))>1 & rowSums(is.na(covid_data_child[, c( "ch_talk_about_7_num", "ch_talk_about_8_num")])) <2,]) 
#This line should replace all missing more than 0, 1, 0 for each scale, respetively with NA to the total 
covid_data_child[which(rowSums(is.na(covid_data_child[, c("ch_talk_about_1_num", "ch_talk_about_2_num")]))>0),"facts_comm"] <- NA
covid_data_child[which(rowSums(is.na(covid_data_child[, c("ch_talk_about_3_num", "ch_talk_about_4_num", "ch_talk_about_5_num", "ch_talk_about_6_num")]))>1),"emotion_comm"] <- NA
covid_data_child[which(rowSums(is.na(covid_data_child[, c("ch_talk_about_7_num", "ch_talk_about_8_num")]))>0),"self_comm"] <- NA


# SDQ scoring --------------------------------------------------------------------------------------------

ch_SDQ_f_4

childvarnames <- names(covid_data_child)
SDQvars <- vars_select(childvarnames, contains("SDQ"))


#Creates a blank dataframe to put the scores into 
SDQnumvars=data.frame(matrix(ncol=15,nrow=nrow(covid_data_child[,SDQvars])))

#recodes the words to numbers
SDQnumvars<- ifelse(covid_data_child[,SDQvars] == "Not true", 0, 
                     ifelse(covid_data_child[,SDQvars] == "Somewhat true", 1, 2))

#Put the names of the new variables in
colnames(SDQnumvars)<- c("ch_SDQ_f_1_num" , "ch_SDQ_f_2_num" , "ch_SDQ_f_3_num" , "ch_SDQ_f_4_num" , "ch_SDQ_f_5_num" , "ch_SDQ_f_6_num" , "ch_SDQ_f_7_num" ,
                          "ch_SDQ_f_8_num" , "ch_SDQ_f_9_num" , "ch_SDQ_f_10_num" , "ch_SDQ_f_11_num" , "ch_SDQ_f_12_num" , "ch_SDQ_f_13_num" , "ch_SDQ_f_14_num" ,
                          "ch_SDQ_f_15_num")


#join this dataframe to the main one
covid_data_child<- cbind(covid_data_child, SDQnumvars)

#Scoring the SDQ subscales 
#scoring code from psych package.Note "-" before items reverse scored.
keys.listSDQ <- list(SDQhyp=c("ch_SDQ_f_1_num","ch_SDQ_f_6_num","ch_SDQ_f_9_num","-ch_SDQ_f_12_num"  , "-ch_SDQ_f_15_num"),
                     SDQemo=c("ch_SDQ_f_2_num","ch_SDQ_f_5_num","ch_SDQ_f_8_num" ,"ch_SDQ_f_10_num" , "ch_SDQ_f_14_num") , 
                     SDQcon=c("ch_SDQ_f_3_num" ,"-ch_SDQ_f_4_num","ch_SDQ_f_7_num","ch_SDQ_f_11_num" , "ch_SDQ_f_13_num"))

SDQscored <- scoreItems(keys.listSDQ, covid_data_child, impute= "mean", totals=TRUE, min=0,max=2) # note - FALSE:average, TRUE:sum
SDQscored$scores #The scores themselves
SDQ_totals <- as.data.frame(SDQscored$scores)#just the total scores
covid_data_child <- cbind(covid_data_child, SDQ_totals)#totals and raw scores


#Get rid of participants total scores who have less than 70% of items (need 18/25) - no more than 7 missing
## This line tells you how people had more than 7 missing (but not the whole scale missing)
#Same 2 steps for total plus subscales
#Need to make this less error prone
#Subscale - SDQemo
rowSums(is.na(covid_data_child[, c("ch_SDQ_f_2_num","ch_SDQ_f_5_num","ch_SDQ_f_8_num" ,"ch_SDQ_f_10_num" , "ch_SDQ_f_14_num")]))
#This line should replace all missing more than 2 with NA to the total 
covid_data_child[which(rowSums(is.na(covid_data_child[, c("ch_SDQ_f_2_num","ch_SDQ_f_5_num","ch_SDQ_f_8_num" ,"ch_SDQ_f_10_num" , "ch_SDQ_f_14_num")]))>1),"SDQemo"] <- NA
#Subscale -  SDQcon
rowSums(is.na(covid_data_child[, c("ch_SDQ_f_3_num" ,"ch_SDQ_f_4_num","ch_SDQ_f_7_num","ch_SDQ_f_11_num" , "ch_SDQ_f_13_num")]))
#This line should replace all missing more than 2 with NA to the total 
covid_data_child[which(rowSums(is.na(covid_data_child[, c("ch_SDQ_f_3_num" ,"ch_SDQ_f_4_num","ch_SDQ_f_7_num","ch_SDQ_f_11_num" , "ch_SDQ_f_13_num")]))>1),"SDQcon"] <- NA
#Subscale -  SDQhyp
rowSums(is.na(covid_data_child[, c("ch_SDQ_f_1_num","ch_SDQ_f_6_num","ch_SDQ_f_9_num","ch_SDQ_f_12_num"  , "ch_SDQ_f_15_num")]))
#This line should replace all missing more than 2 with NA to the total 
covid_data_child[which(rowSums(is.na(covid_data_child[, c("ch_SDQ_f_1_num","ch_SDQ_f_6_num","ch_SDQ_f_9_num","ch_SDQ_f_12_num"  , "ch_SDQ_f_15_num")]))>1),"SDQhyp"] <- NA


#Create new categorical variable for each subscale putting them in categories
covid_data_child$SDQemo_cat <- as.factor(car::recode(covid_data_child$SDQemo, "NA=NA; 0:3='Average';4='S_raised'; else='High'"))
covid_data_child$SDQcon_cat <- as.factor(car::recode(covid_data_child$SDQcon, "NA=NA; 0:2='Average';3='S_raised';else='High'"))
covid_data_child$SDQhyp_cat <- as.factor(car::recode(covid_data_child$SDQhyp, "NA=NA; 0:5='Average';6='S_raised';else='High'"))

# PTSD scoring --------------------------------------------------------------------------------------------

childvarnames <- names(covid_data_child)
PTSDvars <- vars_select(childvarnames, contains("ch_PTSD"))

#Creates a blank dataframe to put the scores into 
PTSDnumvars=data.frame(matrix(ncol=15,nrow=nrow(covid_data_child[,PTSDvars])))

#recodes the words to numbers
PTSDnumvars<- ifelse(covid_data_child[,PTSDvars] == "Yes", 1, 0)

#Put the names of the new variables in
colnames(PTSDnumvars)<- c("ch_PTSD_1_num" , "ch_PTSD_2_num" , "ch_PTSD_3_num" , "ch_PTSD_4_num" , "ch_PTSD_5_num" , "ch_PTSD_6_num" , "ch_PTSD_7_num" ,
                          "ch_PTSD_8_num" , "ch_PTSD_9_num" , "ch_PTSD_10_num" , "ch_PTSD_11_num" , "ch_PTSD_12_num" , "ch_PTSD_13_num" , "ch_PTSD_14_num" ,
                          "ch_PTSD_15_num")

#join this dataframe to the main one
covid_data_child<- cbind(covid_data_child, PTSDnumvars)

#Scoring the total ch PTSD
#scoring code from psych package. 
keys.listPTSD <- list(totalPTSD=c("ch_PTSD_1_num" , "ch_PTSD_2_num" , "ch_PTSD_3_num" , "ch_PTSD_4_num" , "ch_PTSD_5_num" , "ch_PTSD_6_num" , "ch_PTSD_7_num" ,
                                  "ch_PTSD_8_num" , "ch_PTSD_9_num" , "ch_PTSD_10_num" , "ch_PTSD_11_num" , "ch_PTSD_12_num" , "ch_PTSD_13_num" , "ch_PTSD_14_num" ,
                                  "ch_PTSD_15_num")
)

PTSDscored <- scoreItems(keys.listPTSD, covid_data_child, impute= "mean", totals=TRUE, min=0,max=1) # note - FALSE:average, TRUE:sum
PTSDscored$scores #The scores themselves
PTSD_totals <- as.data.frame(PTSDscored$scores)#just the total scores
covid_data_child <- cbind(covid_data_child, PTSD_totals)#totals and raw scores

#Get rid of participants total scores who have less than 70% of items (need 11/15) - no more than 4 missing
## This line tells you how people had more than 4 missing (but not the whole scale missing)
# TotalPTSD
nrow(covid_data_child[rowSums(is.na(covid_data_child[, c( "ch_PTSD_1_num" , "ch_PTSD_2_num" , "ch_PTSD_3_num" , "ch_PTSD_4_num" , "ch_PTSD_5_num" , "ch_PTSD_6_num" , "ch_PTSD_7_num" ,
                                                          "ch_PTSD_8_num" , "ch_PTSD_9_num" , "ch_PTSD_10_num" , "ch_PTSD_11_num" , "ch_PTSD_12_num" , "ch_PTSD_13_num" , "ch_PTSD_14_num" ,
                                                          "ch_PTSD_15_num")]))>1 & rowSums(is.na(covid_data_child[, c( "ch_PTSD_1_num" , "ch_PTSD_2_num" , "ch_PTSD_3_num" , "ch_PTSD_4_num" , "ch_PTSD_5_num" , "ch_PTSD_6_num" , "ch_PTSD_7_num" ,
                                                                                                                       "ch_PTSD_8_num" , "ch_PTSD_9_num" , "ch_PTSD_10_num" , "ch_PTSD_11_num" , "ch_PTSD_12_num" , "ch_PTSD_13_num" , "ch_PTSD_14_num" ,
                                                                                                                       "ch_PTSD_15_num")])) <11,]) 
#This line should replace all missing more than 4 with NA to the total 
covid_data_child[which(rowSums(is.na(covid_data_child[, c("ch_PTSD_1_num" , "ch_PTSD_2_num" , "ch_PTSD_3_num" , "ch_PTSD_4_num" , "ch_PTSD_5_num" , "ch_PTSD_6_num" , "ch_PTSD_7_num" ,
                                                          "ch_PTSD_8_num" , "ch_PTSD_9_num" , "ch_PTSD_10_num" , "ch_PTSD_11_num" , "ch_PTSD_12_num" , "ch_PTSD_13_num" , "ch_PTSD_14_num" ,
                                                          "ch_PTSD_15_num")]))>4),"totalPTSD"] <- NA



# PARQ scoring --------------------------------------------------------------------------------------------

childvarnames <- names(covid_data_child)
PARQvars <- vars_select(childvarnames, contains("parenting_f"))

#Creates a blank dataframe to put the scores into 
PARQnumvars=data.frame(matrix(ncol=13,nrow=nrow(covid_data_child[,PARQvars])))


#recodes the words to numbers
PARQnumvars<- ifelse(covid_data_child[,PARQvars] == "Almost never true", 1, 
                      ifelse(covid_data_child[,PARQvars] == "Rarely true", 2, 
                             ifelse(covid_data_child[,PARQvars] == "Sometimes true", 3,4)))


#Put the names of the new variables in
colnames(PARQnumvars)<- c("ch_parenting_f_1_num" , "ch_parenting_f_2_num" , "ch_parenting_f_3_num" ,
                           "ch_parenting_f_4_num" , "ch_parenting_f_5_num" , "ch_parenting_f_6_num" , 
                           "ch_parenting_f_7_num" , "ch_parenting_f_8_num" , "ch_parenting_f_9_num" ,
                           "ch_parenting_f_10_num", "ch_parenting_f_11_num", "ch_parenting_f_12_num",
                           "ch_parenting_f_13_num")

#join this dataframe to the main one
covid_data_child<- cbind(covid_data_child, PARQnumvars)

# Yet to sort through the old PARQ scoring

# #Scoring the total PARQ and subscales - need to add the >70% impute missing
# #scoring code from psych package. Note "-" before items reverse scored.
# keys.listPARQ <- list(totalPARQ=c("-ch_parenting.1_1_num" , "ch_parenting.1_2_num" , "-ch_parenting.1_4_num" , "ch_parenting.1_5_num" , "ch_parenting.1_6_num" , 
#                                   "ch_parenting.1_8_num" , "ch_parenting.1_9_num" , "ch_parenting.1_10_num" , "-ch_parenting.1_11_num" , "ch_parenting.1_12_num" , "ch_parenting.1_13_num" , "ch_parenting.1_14_num" ,
#                                   "-ch_parenting.1_15_num" , "-ch_parenting.1_16_num" , "ch_parenting.1_17_num" , "ch_parenting.1_18_num" , "ch_parenting.1_19_num" , 
#                                   "-ch_parenting.1_21_num" , "ch_parenting.1_22_num" , "-ch_parenting.1_23_num" , "ch_parenting.1_24_num" , "ch_parenting.1_25_num", "-ch_parenting.1_27_num", "ch_parenting.1_28_num", "-ch_parenting.1_29_num"),
#                       PARQwarmth=c("-ch_parenting.1_1_num","-ch_parenting.1_4_num","-ch_parenting.1_11_num","-ch_parenting.1_15_num","-ch_parenting.1_21_num","-ch_parenting.1_23_num","-ch_parenting.1_27_num","-ch_parenting.1_29_num"), 
#                       PARQhostile=c("ch_parenting.1_5_num","ch_parenting.1_8_num","ch_parenting.1_12_num","ch_parenting.1_17_num","ch_parenting.1_22_num","ch_parenting.1_24_num"),
#                       PARQneglect=c("ch_parenting.1_2_num","ch_parenting.1_9_num","ch_parenting.1_13_num","-ch_parenting.1_16_num","ch_parenting.1_18_num","ch_parenting.1_28_num"),
#                       PARQundiff=c("ch_parenting.1_6_num","ch_parenting.1_10_num","ch_parenting.1_19_num","ch_parenting.1_25_num"),
#                       PARQcontrol=c("ch_parenting.1_3_num","ch_parenting.1_7_num","ch_parenting.1_14_num","-ch_parenting.1_20_num","ch_parenting.1_26_num"))
# 
# PARQscored <- scoreItems(keys.listPARQ, covid_data_child, impute= "mean", totals=TRUE, min=1,max=4) # note - FALSE:average, TRUE:sum
# PARQscored$scores #The scores themselves
# PARQ_totals <- as.data.frame(PARQscored$scores)#just the total scores
# covid_data_child <- cbind(covid_data_child, PARQ_totals)#totals and raw scores
# 
# # Get rid of any px that have less than 70% of the scales missing
# # totalPARQ
# # Put the variable names in
# totalPARQvars<- c("ch_parenting.1_1_num" , "ch_parenting.1_2_num" , "ch_parenting.1_4_num" , "ch_parenting.1_5_num" , "ch_parenting.1_6_num" ,                                 "ch_parenting.1_8_num" , "ch_parenting.1_9_num" , "ch_parenting.1_10_num" , "ch_parenting.1_11_num" , "ch_parenting.1_12_num" ,"ch_parenting.1_13_num" , "ch_parenting.1_14_num" ,
#                   "ch_parenting.1_15_num" , "ch_parenting.1_16_num" , "ch_parenting.1_17_num" , "ch_parenting.1_18_num" , "ch_parenting.1_19_num" , 
#                   "ch_parenting.1_21_num" , "ch_parenting.1_22_num" , "ch_parenting.1_23_num" , "ch_parenting.1_24_num" , "ch_parenting.1_25_num", "ch_parenting.1_27_num", "ch_parenting.1_28_num", "ch_parenting.1_29_num")
# # PARQwarmth
# PARQwarmthvars <- c("ch_parenting.1_1_num","ch_parenting.1_4_num","ch_parenting.1_11_num","ch_parenting.1_15_num","ch_parenting.1_21_num","ch_parenting.1_23_num","ch_parenting.1_27_num","ch_parenting.1_29_num")
# #PARQhostile
# PARQhostilevars <- c("ch_parenting.1_5_num","ch_parenting.1_8_num","ch_parenting.1_12_num","ch_parenting.1_17_num","ch_parenting.1_22_num","ch_parenting.1_24_num")
# #PARQneglect
# PARQneglectvars <- c("ch_parenting.1_2_num","ch_parenting.1_9_num","ch_parenting.1_13_num","ch_parenting.1_16_num","ch_parenting.1_18_num","ch_parenting.1_28_num")
# #PARQundiff
# PARQundiffvars <- c("ch_parenting.1_6_num","ch_parenting.1_10_num","ch_parenting.1_19_num","ch_parenting.1_25_num")
# #PARQcontrol
# PARQcontrolvars <- c("ch_parenting.1_3_num","ch_parenting.1_7_num","ch_parenting.1_14_num","ch_parenting.1_20_num","ch_parenting.1_26_num")
# 
# #Need 21 out of 29 - can't have more than 8 missing (or for subscales 6/8 , no more than 2 missing, 5/6, 3/4, 4/5, no more than 1 missing)
# ## How mnay will be cut
# nrow(covid_data_child[rowSums(is.na(covid_data_child[, totalPARQvars]))>8,]) 
# #This line should replace all missing more than 8 with NA to the total 
# covid_data_child[which(rowSums(is.na(covid_data_child[, totalPARQvars]))>8),"totalPARQ"] <- NA
# ## How mnay will be cut
# nrow(covid_data_child[rowSums(is.na(covid_data_child[, PARQwarmthvars]))>2,]) 
# #This line should replace all missing more than 2 with NA to the total 
# covid_data_child[which(rowSums(is.na(covid_data_child[, PARQwarmthvars]))>2),"PARQwarmth"] <- NA
# ## How mnay will be cut
# nrow(covid_data_child[rowSums(is.na(covid_data_child[, PARQhostilevars]))>1,]) 
# #This line should replace all missing more than 1 with NA to the total 
# covid_data_child[which(rowSums(is.na(covid_data_child[, PARQhostilevars]))>1),"PARQhostile"] <- NA
# ## How mnay will be cut
# nrow(covid_data_child[rowSums(is.na(covid_data_child[, PARQneglectvars]))>1,]) 
# #This line should replace all missing more than 1 with NA to the total 
# covid_data_child[which(rowSums(is.na(covid_data_child[, PARQneglectvars]))>1),"PARQneglect"] <- NA
# ## How mnay will be cut
# nrow(covid_data_child[rowSums(is.na(covid_data_child[, PARQundiffvars]))>1,]) 
# #This line should replace all missing more than 1 with NA to the total 
# covid_data_child[which(rowSums(is.na(covid_data_child[, PARQundiffvars]))>1),"PARQundiff"] <- NA
# ## How mnay will be cut
# nrow(covid_data_child[rowSums(is.na(covid_data_child[, PARQcontrolvars]))>1,]) 
# #This line should replace all missing more than 1 with NA to the total 
# covid_data_child[which(rowSums(is.na(covid_data_child[, PARQcontrolvars]))>1),"PARQcontrol"] <- NA
# 
# 


# Create centered variables --------------------------------------------------------------------------------

# create function or loop through all the variables

# #Create centred variables for analysis
# PARQhostile.mean <- mean(covid_data_child$PARQhostile, na.rm = TRUE)
# totalSDQ.mean <- mean(covid_data_child$totalSDQ, na.rm = TRUE)
# ch_age.mean <- mean(covid_data_child$ch_age, na.rm = TRUE)
# totalFES.mean <- mean(covid_data_child$totalFES, na.rm = TRUE)
# income_famsize.mean <- mean(covid_data_child$income_famsize, na.rm = TRUE)
# SDQemo.mean <- mean(covid_data_child$SDQemo, na.rm = TRUE)
# SDQhyp.mean <- mean(covid_data_child$SDQhyp, na.rm = TRUE)
# SDQcon.mean <- mean(covid_data_child$SDQcon, na.rm = TRUE)
# SDQpeer.mean <- mean(covid_data_child$SDQpeer, na.rm = TRUE)
# DASSDep.mean <- mean(covid_data_child$DASSDep, na.rm = TRUE)
# DASSAnx.mean <- mean(covid_data_child$DASSAnx, na.rm = TRUE)
# DASSStress.mean <- mean(covid_data_child$DASSStress, na.rm = TRUE)
# totalcov_dist.mean <- mean(covid_data_child$totalcov_dist, na.rm = TRUE)
# covid_pos_num.mean <- mean(covid_data_child$covid_pos_num, na.rm = TRUE)
# totalFES.mean <- mean(covid_data_child$totalFES, na.rm = TRUE)
# covid_finance_num.mean <- mean(covid_data_child$covid_finance_num, na.rm = TRUE)
# PARQcontrol.mean <- mean(covid_data_child$PARQcontrol, na.rm = TRUE)
# PARQwarmth.mean <- mean(covid_data_child$PARQwarmth, na.rm = TRUE)
# facts_comm.mean <- mean(covid_data_child$facts_comm, na.rm = TRUE)
# emotion_comm.mean <- mean(covid_data_child$emotion_comm, na.rm = TRUE)
# self_comm.mean <- mean(covid_data_child$self_comm, na.rm = TRUE)
# ch_talk_about_6_num.mean <- mean(covid_data_child$ch_talk_about_6_num, na.rm = TRUE)
# covid_data_child$ch_talk_about_6_num.c <- (covid_data_child$ch_talk_about_6_num-ch_talk_about_6_num.mean)
# covid_data_child$facts_comm.c <- (covid_data_child$facts_comm-facts_comm.mean)
# covid_data_child$emotion_comm.c <- (covid_data_child$emotion_comm-emotion_comm.mean)
# covid_data_child$self_comm.c <- (covid_data_child$self_comm-self_comm.mean)
# covid_data_child$PARQwarmth.c <- (covid_data_child$PARQwarmth-PARQwarmth.mean)
# covid_data_child$PARQcontrol.c <- (covid_data_child$PARQcontrol-PARQcontrol.mean)
# covid_data_child$covid_finance_num.c <- (covid_data_child$covid_finance_num-covid_finance_num.mean)
# covid_data_child$totalFES.c <- (covid_data_child$totalFES-totalFES.mean)
# covid_data_child$covid_pos_num.c <- (covid_data_child$covid_pos_num-covid_pos_num.mean)
# covid_data_child$totalcov_dist.c <- (covid_data_child$totalcov_dist-totalcov_dist.mean)
# covid_data_child$DASSStress.c <- (covid_data_child$DASSStress-DASSStress.mean)
# covid_data_child$DASSAnx.c <- (covid_data_child$DASSAnx-DASSAnx.mean)
# covid_data_child$DASSDep.c <- (covid_data_child$DASSDep-DASSDep.mean)
# covid_data_child$SDQpeer.c <- (covid_data_child$SDQpeer-SDQpeer.mean)
# covid_data_child$SDQcon.c <- (covid_data_child$SDQcon-SDQcon.mean)
# covid_data_child$SDQhyp.c <- (covid_data_child$SDQhyp-SDQhyp.mean)
# covid_data_child$SDQemo.c <- (covid_data_child$SDQemo-SDQemo.mean)
# covid_data_child$totalSDQ.c <- (covid_data_child$totalSDQ-totalSDQ.mean)
# covid_data_child$PARQhostile.c <- (covid_data_child$PARQhostile-PARQhostile.mean)
# covid_data_child$totalFES.c <- (covid_data_child$totalFES-totalFES.mean)
# covid_data_child$ch_age.c <- (covid_data_child$ch_age-ch_age.mean)
# covid_data_child$income_famsize.c <- (covid_data_child$income_famsize-income_famsize.mean)


#prepare a smaller subset of the data to use for MI

# covid_data_child_for_MI <- covid_data_child[,c("id", "par_age", "country_cat","par_gender_num", "income_mid" ,"income_famsize.c",
#                                                "par_ed_ord" , "other_hosp_num","iso", "par_past_mh_num", "DASSStress", "id2",
#                                                "SDQpro","totalSDQ2", "SDQemo2" ,"SDQcon2","SDQhyp2", "SDQpeer2" , 
#                                                "PARQcontrol","facts_comm.c" ,"emotion_comm.c", "self_comm.c" ,
#                                                "PARQwarmth.c", "PARQcontrol.c" , "covid_finance_num.c","totalFES.c" ,"covid_pos_num.c",
#                                                "totalcov_dist.c", "DASSStress.c", "DASSAnx.c" ,  "DASSDep.c" , "SDQpeer.c" , "SDQcon.c" ,
#                                                "SDQhyp.c" , "SDQemo.c","totalSDQ.c" ,"PARQhostile.c" ,"totalPTSD", "other_par_num")]
# 

# Put a _f on the end of all the variables

names(covid_data) <- paste(names(covid_data), "_f", sep="")
names(covid_data_child) <- paste(names(covid_data_child), "_f", sep="")

# Writing the scored files ----------------------------------------

write.csv(covid_data, file = "scored_data/covid_data_scored_fu.csv")
write.csv(covid_data_child, file = "scored_data/covid_data_child_scored_fu.csv")
#write.csv(covid_data_child_for_MI, file = "scored_data/covid_data_child_scored_for_MI.csv")

# Save an Rdata file to store a bunch of useful stuff for later, including alpha, and names of 
# variable groupings
# save(alpha_df, DASSnumvars,  keys.listDASS, FESnumvars, keys.listFES, PARQ1numvars, keys.listPARQ,
#      keys.listSDQ, SDQ1numvars, PTSDnumvars, keys.listPTSD, commnumvars, keys.list.cov.comm,
#      file = "scored_data/covid_scoring_extras.RData")

