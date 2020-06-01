---
title: "COVID_scoring"
author: "Kate Bray"
date: "1 June 2020"
output: html_document

# This script currently takes a csv file exported from Qualtrics. In future may need to adapt to take a file exported from ACCESS database.

# Inputs:
# csv file exported from Qualtrics
# Exported with "choice text" rather than numeric for meantime.
# Manually deleted the email column to deidentify, and the 2nd and 3rd rows with additional qualtrics headings.

# Outputs:
# Two csv files with combination of raw and scored data.
# First csv each parent is a seperate row.
# Second csv each child is a seperate row.

---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

# Set working directory to where the data is.
setwd("/Volumes/Groups/Adaptdir/COVID-CHAMPS/Data")


# Load packages (install them first if don't have them using install.packages("package_name") )
library(car) # for recode
library(dplyr) # for %>%
library(tidyselect) # for starts_with and contains
library(psych) #for scoreItems etc.
library(naniar) #for examining missing
library(ggplot2)
library(summarytools) #for freq - frequency tables
library(corrplot) #For correlation plot

```

```{r}
# General cleaning----------------------------------------------------------------------------------------------

# Read in the csv file
covid_data <- read.csv("covid_data.csv", header=TRUE, stringsAsFactors = FALSE)


#Make the age variables numeric rather than characters

covid_data$ch2_age <- as.numeric(covid_data$ch2_age)
covid_data$ch3_age <- as.numeric(covid_data$ch3_age)
covid_data$ch4_age <- as.numeric(covid_data$ch4_age)
covid_data$ch5_age <- as.numeric(covid_data$ch5_age)
covid_data$ch6_age <- as.numeric(covid_data$ch6_age)

#Add unique ID on
id <- rownames(covid_data)
covid_data <- cbind(id=id, covid_data)

#Missing values - change them from "" to NA to make easier to work with
covid_data[(covid_data=="")] <- NA


# Inclusion criteria ---------------------------------------------------------------------------------
## Participants must have given consent
## Participants should have completed at least 70% of either SDQ or PTSD for at least 1 child 


#how many people consented properly to each question
##Consent q 1.
nrow(covid_data[!is.na(covid_data[covid_data$Q259=="Both","Q259"]), ])
##Consent q 2.
nrow(covid_data[!is.na(covid_data[covid_data$Q260=="15-20 minutes for one child" ,"Q260"]), ])
##Consent q 3.
nrow(covid_data[!is.na(covid_data[covid_data$Q261=="On a secure, password-protected server","Q261"]), ])

#Cut those who didn't consent out of the dataframe
covid_data <- covid_data[(covid_data$Q259=="Both" & covid_data$Q260=="15-20 minutes for one child" & 
                   covid_data$Q261=="On a secure, password-protected server") ,]

covid_data <- covid_data[!is.na(covid_data$Q259) & !is.na(covid_data$Q260) &!is.na(covid_data$Q261), ]

#Examine the PTSD and SDQ items
varnames <- names(covid_data)
#How many entries have at least 70% of the SDQ or PTSD answers
## Must have greater than or equal to 18/25 for SDQ1 and 11/15 for PTSD
nrow(covid_data[which(rowSums(!is.na(covid_data[, grep("ch1_SDQ.1", varnames)]))>=18), ])
#nrow(covid_data[which(rowSums(!is.na(covid_data[, grep("ch1_SDQ.2", varnames)]))>=18), ])
nrow(covid_data[which(rowSums(!is.na(covid_data[, grep("ch1_PTSD", varnames)]))>=11), ])

#How mnay have either the SDQ or the PTSD criteria
nrow(covid_data[which(rowSums(!is.na(covid_data[, grep("ch1_SDQ.1", varnames)]))>=18
                      #| rowSums(!is.na(covid_data[, grep("ch1_SDQ.2", varnames)]))>=18
                      | rowSums(!is.na(covid_data[, grep("ch1_PTSD", varnames)]))>=11), ])


#cutting inelgible people based on SDQ and PSD out of larger sample
covid_data <- covid_data[which(rowSums(!is.na(covid_data[, grep("ch1_SDQ.1", varnames)]))>=18
                     # | rowSums(!is.na(covid_data[, grep("ch1_SDQ.2", varnames)]))>=18
                      | rowSums(!is.na(covid_data[, grep("ch1_PTSD", varnames)]))>=11), ]



#Have a look at validity items
#Any person who answers top 2 for both
summarytools::freq(as.factor(covid_data$validity_1), order = "freq")
summarytools::freq(as.factor(covid_data$validity_2), order = "freq")

which(covid_data$validity_1=="Agree a lot" | covid_data$validity_1=="Agree")
which(covid_data$validity_2=="Yes")

#These participants are not the same so neither gets excluded


# Missing data display ---------------------------------------------------------

#Of those remaining display how much of the dataset is missing 
#This is just for first half of dataset  - split up into readable section. 
#Not very meaningful to get overall missing as many of the questions would be coded -3, NA - can look into this for future if required.
#please see child section of script for child missing data

#Broken up into 2 halfs for better visualisation - please note the 'TEXT' items make it appear that a lot is missing
#Perhaps cut the TEXT variables out for this visualisation?
vis_miss(covid_data[,which(colnames(covid_data)=="country"):which(colnames(covid_data)=="child_details.3_6")]) +
  theme(axis.text.x = element_text(size=6, angle=90))

vis_miss(covid_data[,which(colnames(covid_data)=="par_age"):which(colnames(covid_data)=="ethnicity_15_TEXT")]) +
  theme(axis.text.x = element_text(size=6, angle=90))

#COVID-related
vis_miss(covid_data[,which(colnames(covid_data)=="par_tested"):which(colnames(covid_data)=="covid_pos_why_6_TEXT")]) +
  theme(axis.text.x = element_text(size=6, angle=90))

#family variables etc.
vis_miss(covid_data[,which(colnames(covid_data)=="cohesion_1"):which(colnames(covid_data)=="validity_2")]) +
  theme(axis.text.x = element_text(size=6, angle=90))

#DASS missing
vis_miss(covid_data[,which(colnames(covid_data)=="DASS_1"):which(colnames(covid_data)=="DASS_21")]) +
  theme(axis.text.x = element_text(size=6, angle=90))

#"Progres" through the survey
hist(covid_data$Progress)

# Demographics parents-------------------------------------------------------------------------------------------

## Country family lives in
# Turns the text data to a factor
covid_data$country <- as.factor(covid_data$country)

# Country frequency and percentage (% Valid is excluding NAs, % Total includes NAs)
summarytools::freq(covid_data$country, order = "freq")

countrypiedata<- summarytools::freq(covid_data$country, order = "freq", totals=FALSE)
pie(countrypiedata[1:13,1])

#Make a number variable just in case want to use it analysis. Just first 5 countries all others are other
covid_data$country_num<- ifelse(covid_data$country == "Australia ",1,
                                ifelse(covid_data$country == "United Kingdom ",2,
                                       ifelse(covid_data$country == "Canada ",3,
                                              ifelse(covid_data$country == "New Zealand ",4,5))))
                                                     

#Ethnicity
# Some of the other could be placed in a group (a lot of White British identifying as other rather than choosing western european)
#Look at the large "other" group reassign 'English', 'British', 'british' and 'Welsh'

a <- grep("English|British|british|Welsh", covid_data$ethnicity_15_TEXT)
b <- which(covid_data$ethnicity=="Other (describe):")
covid_data[intersect(a,b), "ethnicity"] <-  "European - Western"


summarytools::freq(as.factor(covid_data$ethnicity), order = "freq")

ethpiedata<- summarytools::freq(as.factor(covid_data$ethnicity), order = "freq", totals=FALSE)
pie(ethpiedata[1:27,1])

## Gender of parent
covid_data$par_gender <- as.factor(covid_data$par_gender)
# frequency and percentage (% Valid is excluding NAs, % Total includes NAs)
summarytools::freq(covid_data$par_gender, order = "freq")
covid_data$par_gender_num<- ifelse(covid_data$par_gender == "Female", 1, 2)

## Age of parent
#the following displays m, median, range, sd
summary(covid_data$par_age)
sd(covid_data$par_age, na.rm =TRUE)
hist(covid_data$par_age)

## Occupation
#This is a bit messy as people could choose more than 1  category - maybe for descriptives place the small numbers of multiple options into larger categories?
#I should be able to make totals if required
summarytools::freq(covid_data$par_occ_status, order = "freq")


## Relative Family Income
# Gross weekly family income was made equivalent to household size by taking the midpoint of each of the 15 income brackets in the LSAC data set and dividing by the square root of the number of people residing in the house. - ref: Bradbury. Family size equivalence scales and survey evaluation of income and well-being. J Soc Policy, 18 (1989), pp. 383-408

# First step is recode the 6 or more to the number in the text

#When I looked at the '6 or more' variable - 2 cases were NA. I filled these in with 6, as I thought the most likely answer they didn't fill it was that they had 6 - let me know if I shoudld change this.
#This selects the 2 NAs and puts a 6 in them
covid_data[which(covid_data$home_num == "6 or more: please indicate number in text box" & is.na(covid_data$home_num_6_TEXT)),"home_num_6_TEXT"] <- 6
#Now replace the number from TEXT variable into the 6 or more option in home number variable
covid_data[which(covid_data$home_num == "6 or more: please indicate number in text box"),"home_num"]   <- covid_data[which(covid_data$home_num == "6 or more: please indicate number in text box"),"home_num_6_TEXT"]

#Now recode the income to the midpoint
covid_data$income_mid<- ifelse(covid_data$income == "Less than AU$36,400 (US$23,230) per year", 18200, 
                               ifelse(covid_data$income == "Up to AU$699 per week (AU$36,400 or US$23,230 per year)", 26000,
                                  ifelse(covid_data$income == "AU$700 to AU$999 per week (up to AU$52,000 or US$33,168 per year)", 44200, 
                                    ifelse(covid_data$income == "AU$1,000 to AU$1,730 per week (up to AU$90,000 or US$57,407 per year)", 71000,
                                      ifelse(covid_data$income == "AU$1,731 to AU$2,700 per week (up to AU$140,000 or US$89,307 per year)", 115000, 140000
                                        )))))

#Midpoint of bracket divided by the sqrt of people in the house
covid_data$income_famsize<-covid_data$income_mid/sqrt(as.numeric(covid_data$home_num))

summary(covid_data$income_famsize)
sd(covid_data$income_famsize, na.rm =TRUE)
hist(covid_data$income_famsize)

#Education - at the moment this is ordinal
#recoding as ordinal data
covid_data$par_ed_ord<- ifelse(covid_data$par_ed == "Partial primary / elementary school", 1, 
                               ifelse(covid_data$par_ed == "Completed primary / elementary school (Grade 6, typically age ~11)", 2,
                                  ifelse(covid_data$par_ed == "Partial high school / upper secondary school", 3 ,
                                    ifelse(covid_data$par_ed == "Completed high school / upper secondary school (Grade 12, typically age ~18)", 4,
                                      ifelse(covid_data$par_ed == "TAFE (Technical And Further Education) / Vocational training course",5,
                                             ifelse(covid_data$par_ed == "Partial University/ College" , 6,
                                                    ifelse(covid_data$par_ed == "Graduated three-year University / College (Bachelor's) degree", 7,
                                                           ifelse(covid_data$par_ed == "Honours (four year University / College degree)", 8,
                                                                  ifelse(covid_data$par_ed == "Partial graduate school (Master's, PhD / doctorate, etc.)", 9, 10 )))))))))

#Frequency and percentage of people in the different groups
summarytools::freq(as.factor(covid_data$par_ed_ord), order = "freq")
hist(covid_data$par_ed_ord)

#Other demographics - contained in child section
##Parent-type (biol versus other)

# COVID impact ------------------------------------------------------------------------------------------
# Frequencies/% for COVID 
## tested, 
summarytools::freq(as.factor(covid_data$par_tested), order = "freq")
##hospitalized, 
summarytools::freq(as.factor(covid_data$par_hosp), order = "freq")
##known, 
summarytools::freq(as.factor(covid_data$other_covid), order = "freq")
##known hospitalized, 
summarytools::freq(as.factor(covid_data$other_hosp), order = "freq")
##known died, 
summarytools::freq(as.factor(covid_data$other_died), order = "freq")
##stay at home (Y/N), 
summarytools::freq(as.factor(covid_data$stay_home), order = "freq")
##stay at home length, 
#to calulate this would do date implemented to date done survey if "no", or date lifted if "yes"

covid_data$datdiff_lifted <- as.vector(as.Date(as.character(covid_data$leave_home_date), format="%d/%m/%Y")-
                  as.Date(as.character(covid_data$stay_home_date), format="%d/%m/%Y"))

covid_data$datdiff_stillhome <- as.vector(as.Date(as.character(covid_data$StartDate), format="%d/%m/%Y")-
                  as.Date(as.character(covid_data$stay_home_date), format="%d/%m/%Y"))

#Create new variable time_iso which is mostly the date for those still at home, but for those who restrictions have been relaxed add that number in there
covid_data$iso <- covid_data$datdiff_stillhome
covid_data[covid_data$leave_home=="Yes" & !is.na(covid_data$leave_home),"iso"] <- covid_data[covid_data$leave_home=="Yes" & !is.na(covid_data$leave_home),"datdiff_lifted"]

summary(covid_data$iso)
hist(covid_data$iso)

#There is an error in one date - to be removed - -332
covid_data[covid_data$iso<0, "iso"]

##restriction lifted?, 
summarytools::freq(as.factor(covid_data$leave_home), order = "freq")
##follow rules, 
summarytools::freq(as.factor(covid_data$soc_dist), order = "freq")
##why?, people pick multiples
summarytools::freq(as.factor(covid_data$soc_dist_why), order = "freq")
##job change, this has a lot of chnages there refer to job related ones
summarytools::freq(as.factor(covid_data$covid_change), order = "freq")

# COVID distress ------------------------------------------------------------


# Inlcudes financial problems - covid_finance, uncertainty stress - covid_uncertain, stress related to existing plans disruption - covid_plans,
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
 nrow(covid_data[rowSums(is.na(covid_data[, colnames(covidnumvars)]))>2 & rowSums(is.na(covid_data[, colnames(covidnumvars)])) <8,])
#This line should replace all missing more than 2 with NA to the total 
covid_data[which(rowSums(is.na(covid_data[, colnames(covidnumvars)]))>2),"totalcov_dist"] <- NA

#Means, sds, range, histogram
summary(covid_data[,c("totalcov_dist")])
summary(covidnumvars)
sd(covid_data$totalcov_dist, na.rm =TRUE)
sd(covid_data$covid_finance_num, na.rm =TRUE)
sd(covid_data$covid_uncertain_num, na.rm =TRUE)
sd(covid_data$covid_plans_num, na.rm =TRUE)
sd(covid_data$covid_worry_1_num, na.rm =TRUE)
sd(covid_data$covid_worry_2_num, na.rm =TRUE)
sd(covid_data$covid_worry_3_num, na.rm =TRUE)
sd(covid_data$covid_neg_num, na.rm =TRUE)
sd(covid_data$covid_pos_num, na.rm =TRUE)
hist(covid_data$totalcov_dist)
hist(covid_data$covid_finance_num)
hist(covid_data$covid_uncertain_num)
hist(covid_data$covid_plans_num)
hist(covid_data$covid_worry_1_num)
hist(covid_data$covid_worry_2_num)
hist(covid_data$covid_worry_3_num)
hist(covid_data$covid_neg_num)
hist(covid_data$covid_pos_num)

# Parent mental health-------------------------------------------------------------------------------------------

covid_data$par_past_mh_num<- ifelse(covid_data$par_past_mh == "Yes (describe:)", 1,2)
summarytools::freq(as.factor(covid_data$par_past_mh_num), order = "freq")

# DASS scoring -------------------------------------------------------------------------------------------
#DASS still needs <70% items removed

#Subscale scores (already scored within qualtrics). Mean, sd, range of scores and histograms
summary(covid_data$Dep)
sd(covid_data$Dep, na.rm =TRUE)
hist(covid_data$Dep)
summary(covid_data$Anx)
sd(covid_data$Anx, na.rm =TRUE)
hist(covid_data$Anx)
summary(covid_data$Stress)
sd(covid_data$Stress, na.rm =TRUE)
hist(covid_data$Stress)

#Create new categorical variable for each subscale putting them in categories
covid_data$DepCat <- as.factor(car::recode(covid_data$Dep, "NA=NA; 0:4='Normal';5:6='Mild';7:10='Moderate'; 11:13='Severe'; 
                                           else='Ex_severe'"))
table(covid_data$DepCat, useNA = "ifany")
covid_data$AnxCat <- as.factor(car::recode(covid_data$Anx, "NA=NA; 0:3='Normal';4:5='Mild';6:7='Moderate'; 8:9='Severe'; 
                                           else='Ex_severe'"))
table(covid_data$AnxCat, useNA = "ifany")
covid_data$StressCat <- as.factor(car::recode(covid_data$Stress, "NA=NA; 0:7='Normal';8:9='Mild';10:12='Moderate'; 13:16='Severe'; 
                                              else='Ex_severe'"))
table(covid_data$StressCat, useNA = "ifany")

#This section is for checking the scoring of individual items, and still to be added the 70% imputated section

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

#Score each and also total



#join this dataframe to the main one
covid_data<- cbind(covid_data, DASSnumvars)

#Section to remove people who are missing more than 70% of their DASS scores (need 15/21) - so if more than 6 missing, goodbye
#...still to make this section


# FES cohesion scoring -------------------------------------------------------------------------------------------

#Don't need this twice
#varnames <- names(covid_data)
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

#Means, sds, range, histogram
summary(covid_data[,c("totalFES")])

sd(covid_data$totalFES, na.rm =TRUE)

hist(covid_data$totalFES)


# Children ----------------------------------------------------------------------------------------------
#Create seperate dataframe for children (stack all of the children on top of each other)


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
covid_data_ch2<- covid_data_ch2[(complete.cases(covid_data_ch2$ch_trigger=="Yes")),]
covid_data_ch3<- covid_data_ch3[(complete.cases(covid_data_ch3$ch_trigger=="Yes")),]
covid_data_ch4<- covid_data_ch4[(complete.cases(covid_data_ch4$ch_trigger=="Yes")),]
covid_data_ch5<- covid_data_ch5[(complete.cases(covid_data_ch5$ch_trigger=="Yes")),]
covid_data_ch6<- covid_data_ch6[(complete.cases(covid_data_ch6$ch_trigger=="Yes")),]


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

#Found that this variable was different - inconsistenciy in data asked between different children
names(covid_data_ch1)[names(covid_data_ch1)=="ch_med_source_7_TEXT"] <- "ch_med_source_6_TEXT"

#Merge all the different children into 1 dataset
covid_data_child<- rbind(covid_data_ch1, covid_data_ch2, covid_data_ch3, covid_data_ch4, covid_data_ch5, covid_data_ch6)

#Missing data for children - split up more for better viewing
vis_miss(covid_data_child[,which(colnames(covid_data_child)=="ch_age"):which(colnames(covid_data_child)=="ch_concern")])  +
  theme(axis.text.x = element_text(size=6, angle=90))
vis_miss(covid_data_child[,which(colnames(covid_data_child)=="ch_SDQ.1_1"):which(colnames(covid_data_child)=="ch_SDQ.2_25")])  +
  theme(axis.text.x = element_text(size=6, angle=90))
vis_miss(covid_data_child[,which(colnames(covid_data_child)=="ch_PTSD_1"):which(colnames(covid_data_child)=="ch_parenting.1_29")])  +
  theme(axis.text.x = element_text(size=6, angle=90))

# Demographics for children -------------------------------------------------------------------------------------------------

## Gender of child
covid_data_child$ch_gender <- as.factor(covid_data_child$ch_gender)
summarytools::freq(covid_data_child$ch_gender, order = "freq")
covid_data_child$ch_gender_num<- ifelse(covid_data_child$ch_gender == "Female", 1, 2)

## Age of child
#Note: this indicates some children yonger than 5 - need to remove them
covid_data_child[covid_data_child$ch_age<5 & !is.na(covid_data_child$ch_age), "ch_age"]
#Remove these children from dataset
covid_data_child<- covid_data_child[covid_data_child$ch_age>=5 , ]

#the following displays m, median, range, sd
summary(covid_data_child$ch_age)
sd(covid_data_child$ch_age, na.rm =TRUE)
hist(covid_data_child$ch_age)


## Type of parent
#This line tries to isolate the parent-type for each child (up to 6), and also exclude those that didn't answer on the first child
##Just to examine the data - most seem the same for each child with a few exceptions
parent_type <- covid_data[which(!is.na(covid_data_child$child_details.3_1)),which(colnames(covid_data)=="child_details.3_1"):which(colnames(covid_data)=="child_details.3_6")]
#Doing this so I can stack all the columns togethert
parent_type <- stack(parent_type)
##Show frequency
summarytools::freq(as.factor(parent_type$values), order = "freq")


# SDQ scoring --------------------------------------------------------------------------------------------

childvarnames <- names(covid_data_child)
SDQ1vars <- vars_select(childvarnames, contains("SDQ.1"))
SDQ2vars <- vars_select(childvarnames, contains("SDQ.2"))

#Creates a blank dataframe to put the scores into 
SDQ1numvars=data.frame(matrix(ncol=25,nrow=nrow(covid_data_child[,SDQ1vars])))
SDQ2numvars=data.frame(matrix(ncol=25,nrow=nrow(covid_data_child[,SDQ2vars])))


#recodes the words to numbers
SDQ1numvars<- ifelse(covid_data_child[,SDQ1vars] == "Not true", 0, 
                     ifelse(covid_data_child[,SDQ1vars] == "Somewhat true", 1, 2))
SDQ2numvars<- ifelse(covid_data_child[,SDQ2vars] == "Less since COVID", -1, 
                     ifelse(covid_data_child[,SDQ2vars] == "Same since COVID", 0, 1))

#Put the names of the new variables in
colnames(SDQ1numvars)<- c("ch_SDQ.1_1_num" , "ch_SDQ.1_2_num" , "ch_SDQ.1_3_num" , "ch_SDQ.1_4_num" , "ch_SDQ.1_5_num" , "ch_SDQ.1_6_num" , "ch_SDQ.1_7_num" ,
                          "ch_SDQ.1_8_num" , "ch_SDQ.1_9_num" , "ch_SDQ.1_10_num" , "ch_SDQ.1_11_num" , "ch_SDQ.1_12_num" , "ch_SDQ.1_13_num" , "ch_SDQ.1_14_num" ,
                          "ch_SDQ.1_15_num" , "ch_SDQ.1_16_num" , "ch_SDQ.1_17_num" , "ch_SDQ.1_18_num" , "ch_SDQ.1_19_num" , "ch_SDQ.1_20_num" ,
                          "ch_SDQ.1_21_num" , "ch_SDQ.1_22_num" , "ch_SDQ.1_23_num" , "ch_SDQ.1_24_num" , "ch_SDQ.1_25_num")
colnames(SDQ2numvars)<- c("ch_SDQ.2_1_num" , "ch_SDQ.2_2_num" , "ch_SDQ.2_3_num" , "ch_SDQ.2_4_num" , "ch_SDQ.2_5_num" , "ch_SDQ.2_6_num" , "ch_SDQ.2_7_num" ,
                          "ch_SDQ.2_8_num" , "ch_SDQ.2_9_num" , "ch_SDQ.2_10_num" , "ch_SDQ.2_11_num" , "ch_SDQ.2_12_num" , "ch_SDQ.2_13_num" , "ch_SDQ.2_14_num" ,
                          "ch_SDQ.2_15_num" , "ch_SDQ.2_16_num" , "ch_SDQ.2_17_num" , "ch_SDQ.2_18_num" , "ch_SDQ.2_19_num" , "ch_SDQ.2_20_num" ,
                          "ch_SDQ.2_21_num" , "ch_SDQ.2_22_num" , "ch_SDQ.2_23_num" , "ch_SDQ.2_24_num" , "ch_SDQ.2_25_num")


#join this dataframe to the main one
covid_data_child<- cbind(covid_data_child, SDQ1numvars, SDQ2numvars)

#Scoring the total SDQ and subscales 
#scoring code from psych package.Note "-" before items reverse scored.Note pro not included in total SDQ.
keys.listSDQ <- list(totalSDQ=c("ch_SDQ.1_2_num" , "ch_SDQ.1_3_num" , "ch_SDQ.1_5_num" , "ch_SDQ.1_6_num" , "-ch_SDQ.1_7_num" ,
                                  "ch_SDQ.1_8_num" , "ch_SDQ.1_10_num" , "-ch_SDQ.1_11_num" , "ch_SDQ.1_12_num" , "ch_SDQ.1_13_num" , "-ch_SDQ.1_14_num" ,
                                  "ch_SDQ.1_15_num" , "ch_SDQ.1_16_num" , "ch_SDQ.1_18_num" , "ch_SDQ.1_19_num" ,
                                  "-ch_SDQ.1_21_num" , "ch_SDQ.1_22_num" , "ch_SDQ.1_23_num" , "ch_SDQ.1_24_num" , "-ch_SDQ.1_25_num"),
                      SDQemo=c("ch_SDQ.1_3_num","ch_SDQ.1_8_num","ch_SDQ.1_13_num" ,"ch_SDQ.1_16_num" , "ch_SDQ.1_24_num") , 
                      SDQcon=c("ch_SDQ.1_5_num" ,"-ch_SDQ.1_7_num","ch_SDQ.1_12_num","ch_SDQ.1_18_num" , "ch_SDQ.1_22_num") ,
                      SDQhyp=c("ch_SDQ.1_2_num","ch_SDQ.1_10_num","ch_SDQ.1_15_num","-ch_SDQ.1_21_num"  , "-ch_SDQ.1_25_num") ,
                      SDQpeer=c("ch_SDQ.1_6_num","-ch_SDQ.1_11_num","-ch_SDQ.1_14_num" ,"ch_SDQ.1_19_num" , "ch_SDQ.1_23_num") ,
                      SDQpro=c("ch_SDQ.1_1_num" ,"ch_SDQ.1_4_num","ch_SDQ.1_9_num" ,"ch_SDQ.1_17_num" , "ch_SDQ.1_20_num"))

SDQscored <- scoreItems(keys.listSDQ, covid_data_child, impute= "mean", totals=TRUE, min=0,max=2) # note - FALSE:average, TRUE:sum
SDQscored$scores #The scores themselves
SDQ_totals <- as.data.frame(SDQscored$scores)#just the total scores
covid_data_child <- cbind(covid_data_child, SDQ_totals)#totals and raw scores

#Scoring the total SDQ2 and subscales 
#scoring code from psych package.Note "-" before items reverse scored.
keys.listSDQ2 <- list(totalSDQ2=c("ch_SDQ.2_2_num" , "ch_SDQ.2_3_num" , "ch_SDQ.2_5_num" , "ch_SDQ.2_6_num" , "-ch_SDQ.2_7_num" ,
                                  "ch_SDQ.2_8_num" , "ch_SDQ.2_10_num" , "-ch_SDQ.2_11_num" , "ch_SDQ.2_12_num" , "ch_SDQ.2_13_num" , "-ch_SDQ.2_14_num" ,
                                  "ch_SDQ.2_15_num" , "ch_SDQ.2_16_num" , "ch_SDQ.2_18_num" , "ch_SDQ.2_19_num" , 
                                  "-ch_SDQ.2_21_num" , "ch_SDQ.2_22_num" , "ch_SDQ.2_23_num" , "ch_SDQ.2_24_num" , "-ch_SDQ.2_25_num"),
                      SDQemo2=c("ch_SDQ.2_3_num","ch_SDQ.2_8_num","ch_SDQ.2_13_num" ,"ch_SDQ.2_16_num" , "ch_SDQ.2_24_num") , 
                      SDQcon2=c("ch_SDQ.2_5_num" ,"-ch_SDQ.2_7_num","ch_SDQ.2_12_num","ch_SDQ.2_18_num" , "ch_SDQ.2_22_num") ,
                      SDQhyp2=c("ch_SDQ.2_2_num","ch_SDQ.2_10_num","ch_SDQ.2_15_num","-ch_SDQ.2_21_num"  , "-ch_SDQ.2_25_num") ,
                      SDQpeer2=c("ch_SDQ.2_6_num","-ch_SDQ.2_11_num","-ch_SDQ.2_14_num" ,"ch_SDQ.2_19_num" , "ch_SDQ.2_23_num") ,
                      SDQpro2=c("ch_SDQ.2_1_num" ,"ch_SDQ.2_4_num","ch_SDQ.2_9_num" ,"ch_SDQ.2_17_num" , "ch_SDQ.2_20_num"))

SDQscored2 <- scoreItems(keys.listSDQ2, covid_data_child, impute= "mean", totals=TRUE, min=-1,max=1) # note - FALSE:average, TRUE:sum
SDQscored2$scores #The scores themselves
SDQ_totals2 <- as.data.frame(SDQscored2$scores)#just the total scores
covid_data_child <- cbind(covid_data_child, SDQ_totals2)#totals and raw scores

#Get rid of participants total scores who have less than 70% of items (need 18/25) - no more than 7 missing
## This line tells you how people had more than 7 missing (but not the whole scale missing)
#Same 2 steps for total plus subscales
#Need to make this less error prone
# TotalSDQ
nrow(covid_data_child[rowSums(is.na(covid_data_child[, c( "ch_SDQ.1_5_num" ,"ch_SDQ.1_7_num","ch_SDQ.1_12_num","ch_SDQ.1_18_num" , "ch_SDQ.1_22_num" , "ch_SDQ.1_2_num","ch_SDQ.1_10_num","ch_SDQ.1_15_num","ch_SDQ.1_21_num"  , "ch_SDQ.1_25_num" , "ch_SDQ.1_6_num","ch_SDQ.1_11_num","ch_SDQ.1_14_num" ,"ch_SDQ.1_19_num" , "ch_SDQ.1_23_num")]))>1 & rowSums(is.na(covid_data_child[, c( "ch_SDQ.1_5_num" ,"ch_SDQ.1_7_num","ch_SDQ.1_12_num","ch_SDQ.1_18_num" , "ch_SDQ.1_22_num" , "ch_SDQ.1_2_num","ch_SDQ.1_10_num","ch_SDQ.1_15_num","ch_SDQ.1_21_num"  , "ch_SDQ.1_25_num" , "ch_SDQ.1_6_num","ch_SDQ.1_11_num","ch_SDQ.1_14_num" ,"ch_SDQ.1_19_num" , "ch_SDQ.1_23_num")])) <20,]) 
nrow(covid_data_child[rowSums(is.na(covid_data_child[, c( "ch_SDQ.2_5_num" ,"ch_SDQ.2_7_num","ch_SDQ.2_12_num","ch_SDQ.2_18_num" , "ch_SDQ.2_22_num" , "ch_SDQ.2_2_num","ch_SDQ.2_10_num","ch_SDQ.2_15_num","ch_SDQ.2_21_num"  , "ch_SDQ.2_25_num" , "ch_SDQ.2_6_num","ch_SDQ.2_11_num","ch_SDQ.2_14_num" ,"ch_SDQ.2_19_num" , "ch_SDQ.2_23_num")]))>1 & rowSums(is.na(covid_data_child[, c( "ch_SDQ.2_5_num" ,"ch_SDQ.2_7_num","ch_SDQ.2_12_num","ch_SDQ.2_18_num" , "ch_SDQ.2_22_num" , "ch_SDQ.2_2_num","ch_SDQ.2_10_num","ch_SDQ.2_15_num","ch_SDQ.2_21_num"  , "ch_SDQ.2_25_num" , "ch_SDQ.2_6_num","ch_SDQ.2_11_num","ch_SDQ.2_14_num" ,"ch_SDQ.2_19_num" , "ch_SDQ.2_23_num")])) <20,]) 
#This line should replace all missing more than 2 with NA to the total 
covid_data_child[which(rowSums(is.na(covid_data_child[, c("ch_SDQ.1_5_num" ,"ch_SDQ.1_7_num","ch_SDQ.1_12_num","ch_SDQ.1_18_num" , "ch_SDQ.1_22_num" , "ch_SDQ.1_2_num","ch_SDQ.1_10_num","ch_SDQ.1_15_num","ch_SDQ.1_21_num"  , "ch_SDQ.1_25_num" , "ch_SDQ.1_6_num","ch_SDQ.1_11_num","ch_SDQ.1_14_num" ,"ch_SDQ.1_19_num" , "ch_SDQ.1_23_num")]))>7),"totalSDQ"] <- NA
covid_data_child[which(rowSums(is.na(covid_data_child[, c("ch_SDQ.2_5_num" ,"ch_SDQ.2_7_num","ch_SDQ.2_12_num","ch_SDQ.2_18_num" , "ch_SDQ.2_22_num" , "ch_SDQ.2_2_num","ch_SDQ.2_10_num","ch_SDQ.2_15_num","ch_SDQ.2_21_num"  , "ch_SDQ.2_25_num" , "ch_SDQ.2_6_num","ch_SDQ.2_11_num","ch_SDQ.2_14_num" ,"ch_SDQ.2_19_num" , "ch_SDQ.2_23_num")]))>7),"totalSDQ2"] <- NA
#Subscale - SDQemo
 nrow(covid_data_child[rowSums(is.na(covid_data_child[, c("ch_SDQ.1_3_num","ch_SDQ.1_8_num","ch_SDQ.1_13_num" ,"ch_SDQ.1_16_num" , "ch_SDQ.1_24_num")]))>1 & rowSums(is.na(covid_data_child[, c("ch_SDQ.1_3_num","ch_SDQ.1_8_num","ch_SDQ.1_13_num" ,"ch_SDQ.1_16_num" , "ch_SDQ.1_24_num")])) <5,])
 nrow(covid_data_child[rowSums(is.na(covid_data_child[, c("ch_SDQ.2_3_num","ch_SDQ.2_8_num","ch_SDQ.2_13_num" ,"ch_SDQ.2_16_num" , "ch_SDQ.2_24_num")]))>1 & rowSums(is.na(covid_data_child[, c("ch_SDQ.2_3_num","ch_SDQ.2_8_num","ch_SDQ.2_13_num" ,"ch_SDQ.2_16_num" , "ch_SDQ.2_24_num")])) <5,])
#This line should replace all missing more than 2 with NA to the total 
covid_data_child[which(rowSums(is.na(covid_data_child[, c("ch_SDQ.1_3_num","ch_SDQ.1_8_num","ch_SDQ.1_13_num" ,"ch_SDQ.1_16_num" , "ch_SDQ.1_24_num")]))>1),"SDQemo"] <- NA
covid_data_child[which(rowSums(is.na(covid_data_child[, c("ch_SDQ.2_3_num","ch_SDQ.2_8_num","ch_SDQ.2_13_num" ,"ch_SDQ.2_16_num" , "ch_SDQ.2_24_num")]))>1),"SDQemo2"] <- NA
#Subscale -  SDQcon
 nrow(covid_data_child[rowSums(is.na(covid_data_child[, c("ch_SDQ.1_5_num" ,"ch_SDQ.1_7_num","ch_SDQ.1_12_num","ch_SDQ.1_18_num" , "ch_SDQ.1_22_num")]))>1 & rowSums(is.na(covid_data_child[, c("ch_SDQ.1_5_num" ,"ch_SDQ.1_7_num","ch_SDQ.1_12_num","ch_SDQ.1_18_num" , "ch_SDQ.1_22_num")])) <5,])
 nrow(covid_data_child[rowSums(is.na(covid_data_child[, c("ch_SDQ.2_5_num" ,"ch_SDQ.2_7_num","ch_SDQ.2_12_num","ch_SDQ.2_18_num" , "ch_SDQ.2_22_num")]))>1 & rowSums(is.na(covid_data_child[, c("ch_SDQ.2_5_num" ,"ch_SDQ.2_7_num","ch_SDQ.2_12_num","ch_SDQ.2_18_num" , "ch_SDQ.2_22_num")])) <5,])
#This line should replace all missing more than 2 with NA to the total 
covid_data_child[which(rowSums(is.na(covid_data_child[, c("ch_SDQ.1_5_num" ,"ch_SDQ.1_7_num","ch_SDQ.1_12_num","ch_SDQ.1_18_num" , "ch_SDQ.1_22_num")]))>1),"SDQcon"] <- NA
covid_data_child[which(rowSums(is.na(covid_data_child[, c("ch_SDQ.2_5_num" ,"ch_SDQ.2_7_num","ch_SDQ.2_12_num","ch_SDQ.2_18_num" , "ch_SDQ.2_22_num")]))>1),"SDQcon2"] <- NA
#Subscale -  SDQhyp
 nrow(covid_data_child[rowSums(is.na(covid_data_child[, c("ch_SDQ.1_2_num","ch_SDQ.1_10_num","ch_SDQ.1_15_num","ch_SDQ.1_21_num"  , "ch_SDQ.1_25_num")]))>1 & rowSums(is.na(covid_data_child[, c("ch_SDQ.1_2_num","ch_SDQ.1_10_num","ch_SDQ.1_15_num","ch_SDQ.1_21_num"  , "ch_SDQ.1_25_num")])) <5,])
 nrow(covid_data_child[rowSums(is.na(covid_data_child[, c("ch_SDQ.2_2_num","ch_SDQ.2_10_num","ch_SDQ.2_15_num","ch_SDQ.2_21_num"  , "ch_SDQ.2_25_num")]))>1 & rowSums(is.na(covid_data_child[, c("ch_SDQ.2_2_num","ch_SDQ.2_10_num","ch_SDQ.2_15_num","ch_SDQ.2_21_num"  , "ch_SDQ.2_25_num")])) <5,])
#This line should replace all missing more than 2 with NA to the total 
covid_data_child[which(rowSums(is.na(covid_data_child[, c("ch_SDQ.1_2_num","ch_SDQ.1_10_num","ch_SDQ.1_15_num","ch_SDQ.1_21_num","ch_SDQ.1_25_num")]))>1),"SDQhyp"] <- NA
covid_data_child[which(rowSums(is.na(covid_data_child[, c("ch_SDQ.2_2_num","ch_SDQ.2_10_num","ch_SDQ.2_15_num","ch_SDQ.2_21_num","ch_SDQ.2_25_num")]))>1),"SDQhyp2"] <- NA
#Subscale -  SDQpeer
 nrow(covid_data_child[rowSums(is.na(covid_data_child[, c("ch_SDQ.1_6_num","ch_SDQ.1_11_num","ch_SDQ.1_14_num" ,"ch_SDQ.1_19_num" , "ch_SDQ.1_23_num")]))>1 & rowSums(is.na(covid_data_child[, c("ch_SDQ.1_6_num","ch_SDQ.1_11_num","ch_SDQ.1_14_num" ,"ch_SDQ.1_19_num" , "ch_SDQ.1_23_num")])) <5,])
 nrow(covid_data_child[rowSums(is.na(covid_data_child[, c("ch_SDQ.2_6_num","ch_SDQ.2_11_num","ch_SDQ.2_14_num" ,"ch_SDQ.2_19_num" , "ch_SDQ.2_23_num")]))>1 & rowSums(is.na(covid_data_child[, c("ch_SDQ.2_6_num","ch_SDQ.2_11_num","ch_SDQ.2_14_num" ,"ch_SDQ.2_19_num" , "ch_SDQ.2_23_num")])) <5,])
#This line should replace all missing more than 2 with NA to the total 
covid_data_child[which(rowSums(is.na(covid_data_child[, c("ch_SDQ.1_6_num","ch_SDQ.1_11_num","ch_SDQ.1_14_num" ,"ch_SDQ.1_19_num","ch_SDQ.1_23_num")]))>1),"SDQpeer"] <- NA
covid_data_child[which(rowSums(is.na(covid_data_child[, c("ch_SDQ.2_6_num","ch_SDQ.2_11_num","ch_SDQ.2_14_num" ,"ch_SDQ.2_19_num","ch_SDQ.2_23_num")]))>1),"SDQpeer2"] <- NA
#Subscale -  SDQpro
 nrow(covid_data_child[rowSums(is.na(covid_data_child[, c("ch_SDQ.1_1_num" ,"ch_SDQ.1_4_num","ch_SDQ.1_9_num" ,"ch_SDQ.1_17_num" , "ch_SDQ.1_20_num")]))>1 & rowSums(is.na(covid_data_child[, c("ch_SDQ.1_1_num" ,"ch_SDQ.1_4_num","ch_SDQ.1_9_num" ,"ch_SDQ.1_17_num" , "ch_SDQ.1_20_num")])) <5,])
 nrow(covid_data_child[rowSums(is.na(covid_data_child[, c("ch_SDQ.2_1_num" ,"ch_SDQ.2_4_num","ch_SDQ.2_9_num" ,"ch_SDQ.2_17_num" , "ch_SDQ.2_20_num")]))>1 & rowSums(is.na(covid_data_child[, c("ch_SDQ.2_1_num" ,"ch_SDQ.2_4_num","ch_SDQ.2_9_num" ,"ch_SDQ.2_17_num" , "ch_SDQ.2_20_num")])) <5,])
#This line should replace all missing more than 2 with NA to the total 
covid_data_child[which(rowSums(is.na(covid_data_child[, c("ch_SDQ.1_1_num" ,"ch_SDQ.1_4_num","ch_SDQ.1_9_num","ch_SDQ.1_17_num","ch_SDQ.1_20_num")]))>1),"SDQpro"] <- NA
covid_data_child[which(rowSums(is.na(covid_data_child[, c("ch_SDQ.2_1_num" ,"ch_SDQ.2_4_num","ch_SDQ.2_9_num","ch_SDQ.2_17_num","ch_SDQ.2_20_num")]))>1),"SDQpro2"] <- NA




#Means, sds, range, histogram
summary(covid_data_child[,c("totalSDQ", "SDQemo" ,  "SDQcon", "SDQhyp", "SDQpeer", "SDQpro")])
summary(covid_data_child[,c("totalSDQ2", "SDQemo2" ,  "SDQcon2", "SDQhyp2", "SDQpeer2", "SDQpro2")])

sd(covid_data_child$totalSDQ)
sd(covid_data_child$SDQemo)
sd(covid_data_child$SDQcon)
sd(covid_data_child$SDQhyp)
sd(covid_data_child$SDQpeer)
sd(covid_data_child$SDQpro)

sd(covid_data_child$totalSDQ2)
sd(covid_data_child$SDQemo2)
sd(covid_data_child$SDQcon2)
sd(covid_data_child$SDQhyp2)
sd(covid_data_child$SDQpeer2)
sd(covid_data_child$SDQpro2)

hist(covid_data_child$totalSDQ)
hist(covid_data_child$SDQemo)
hist(covid_data_child$SDQcon)
hist(covid_data_child$SDQhyp)
hist(covid_data_child$SDQpeer)
hist(covid_data_child$SDQpro)

hist(covid_data_child$totalSDQ2)
hist(covid_data_child$SDQemo2)
hist(covid_data_child$SDQcon2)
hist(covid_data_child$SDQhyp2)
hist(covid_data_child$SDQpeer2)
hist(covid_data_child$SDQpro2)


#Create new categorical variable for each subscale putting them in categories
covid_data_child$totalSDQ_cat <- as.factor(car::recode(covid_data_child$totalSDQ, "NA=NA; 0:13='Average';14:16='S_raised'; else='High'"))
table(covid_data_child$totalSDQ_cat, useNA = "ifany")
covid_data_child$SDQemo_cat <- as.factor(car::recode(covid_data_child$SDQemo, "NA=NA; 0:3='Average';4='S_raised'; else='High'"))
table(covid_data_child$SDQemo_cat, useNA = "ifany")
covid_data_child$SDQcon_cat <- as.factor(car::recode(covid_data_child$SDQcon, "NA=NA; 0:2='Average';3='S_raised';else='High'"))
table(covid_data_child$SDQcon_cat, useNA = "ifany")
covid_data_child$SDQhyp_cat <- as.factor(car::recode(covid_data_child$SDQhyp, "NA=NA; 0:5='Average';6='S_raised';else='High'"))
table(covid_data_child$SDQhyp_cat, useNA = "ifany")
covid_data_child$SDQpeer_cat <- as.factor(car::recode(covid_data_child$SDQpeer, "NA=NA; 0:2='Average';3='S_raised';else='High'"))
table(covid_data_child$SDQpeer_cat, useNA = "ifany")
covid_data_child$SDQpro_cat <- as.factor(car::recode(covid_data_child$SDQpro, "NA=NA; 6:10='Average';5='S_low';else='Low'"))
table(covid_data_child$SDQpro_cat, useNA = "ifany")

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

#Means, sds, range, histogram
summary(covid_data_child[,c("totalPTSD")])
sd(covid_data_child$totalPTSD)
hist(covid_data_child$totalPTSD)

# PARQ scoring --------------------------------------------------------------------------------------------

childvarnames <- names(covid_data_child)
PARQ1vars <- vars_select(childvarnames, contains("parenting.1"))
PARQ2vars <- vars_select(childvarnames, contains("parenting.2"))

#Creates a blank dataframe to put the scores into 
PARQ1numvars=data.frame(matrix(ncol=29,nrow=nrow(covid_data_child[,PARQ1vars])))
PARQ2numvars=data.frame(matrix(ncol=29,nrow=nrow(covid_data_child[,PARQ2vars])))


#recodes the words to numbers
PARQ1numvars<- ifelse(covid_data_child[,PARQ1vars] == "Almost never true", 1, 
                     ifelse(covid_data_child[,PARQ1vars] == "Rarely true", 2, 
                        ifelse(covid_data_child[,PARQ1vars] == "Sometimes true", 3,4)))
PARQ2numvars<- ifelse(covid_data_child[,PARQ2vars] == "Less since COVID", -1, 
                     ifelse(covid_data_child[,PARQ2vars] == "Same since COVID", 0, 1))

#Put the names of the new variables in
colnames(PARQ1numvars)<- c("ch_parenting.1_1_num" , "ch_parenting.1_2_num" , "ch_parenting.1_3_num" , "ch_parenting.1_4_num" , "ch_parenting.1_5_num" , "ch_parenting.1_6_num" , "ch_parenting.1_7_num" ,
                          "ch_parenting.1_8_num" , "ch_parenting.1_9_num" , "ch_parenting.1_10_num" , "ch_parenting.1_11_num" , "ch_parenting.1_12_num" , "ch_parenting.1_13_num" , "ch_parenting.1_14_num" ,
                          "ch_parenting.1_15_num" , "ch_parenting.1_16_num" , "ch_parenting.1_17_num" , "ch_parenting.1_18_num" , "ch_parenting.1_19_num" , "ch_parenting.1_20_num" ,
                          "ch_parenting.1_21_num" , "ch_parenting.1_22_num" , "ch_parenting.1_23_num" , "ch_parenting.1_24_num" , "ch_parenting.1_25_num",
"ch_parenting.1_26_num" , "ch_parenting.1_27_num" , "ch_parenting.1_28_num" , "ch_parenting.1_29_num")
colnames(PARQ2numvars)<- c("ch_parenting.2_1_num" , "ch_parenting.2_2_num" , "ch_parenting.2_3_num" , "ch_parenting.2_4_num" , "ch_parenting.2_5_num" , "ch_parenting.2_6_num" , "ch_parenting.2_7_num" ,
                          "ch_parenting.2_8_num" , "ch_parenting.2_9_num" , "ch_parenting.2_10_num" , "ch_parenting.2_11_num" , "ch_parenting.2_12_num" , "ch_parenting.2_13_num" , "ch_parenting.2_14_num" ,
                          "ch_parenting.2_15_num" , "ch_parenting.2_16_num" , "ch_parenting.2_17_num" , "ch_parenting.2_18_num" , "ch_parenting.2_19_num" , "ch_parenting.2_20_num" ,
                          "ch_parenting.2_21_num" , "ch_parenting.2_22_num" , "ch_parenting.2_23_num" , "ch_parenting.2_24_num" , "ch_parenting.2_25_num",
"ch_parenting.2_26_num" , "ch_parenting.2_27_num" , "ch_parenting.2_28_num" , "ch_parenting.2_29_num")


#join this dataframe to the main one
covid_data_child<- cbind(covid_data_child, PARQ1numvars, PARQ2numvars)

#Scoring the total PARQ and subscales - need to add the >70% impute missing
#scoring code from psych package. Note "-" before items reverse scored.
keys.listPARQ <- list(totalPARQ=c("-ch_parenting.1_1_num" , "ch_parenting.1_2_num" , "-ch_parenting.1_4_num" , "ch_parenting.1_5_num" , "ch_parenting.1_6_num" , 
                                  "ch_parenting.1_8_num" , "ch_parenting.1_9_num" , "ch_parenting.1_10_num" , "-ch_parenting.1_11_num" , "ch_parenting.1_12_num" , "ch_parenting.1_13_num" , "ch_parenting.1_14_num" ,
                                  "-ch_parenting.1_15_num" , "-ch_parenting.1_16_num" , "ch_parenting.1_17_num" , "ch_parenting.1_18_num" , "ch_parenting.1_19_num" , 
                                  "-ch_parenting.1_21_num" , "ch_parenting.1_22_num" , "-ch_parenting.1_23_num" , "ch_parenting.1_24_num" , "ch_parenting.1_25_num", "-ch_parenting.1_27_num", "ch_parenting.1_28_num", "-ch_parenting.1_29_num"),
                      PARQwarmth=c("-ch_parenting.1_1_num","-ch_parenting.1_4_num","-ch_parenting.1_11_num","-ch_parenting.1_15_num","-ch_parenting.1_21_num","-ch_parenting.1_23_num","-ch_parenting.1_27_num","-ch_parenting.1_29_num"), 
                      PARQhostile=c("ch_parenting.1_5_num","ch_parenting.1_8_num","ch_parenting.1_12_num","ch_parenting.1_17_num","ch_parenting.1_22_num","ch_parenting.1_24_num"),
                      PARQneglect=c("ch_parenting.1_2_num","ch_parenting.1_9_num","ch_parenting.1_13_num","-ch_parenting.1_16_num","ch_parenting.1_18_num","ch_parenting.1_28_num"),
                      PARQundiff=c("ch_parenting.1_6_num","ch_parenting.1_10_num","ch_parenting.1_19_num","ch_parenting.1_25_num"),
                      PARQcontrol=c("ch_parenting.1_3_num","ch_parenting.1_7_num","ch_parenting.1_14_num","-ch_parenting.1_20_num","ch_parenting.1_26_num"))

PARQscored <- scoreItems(keys.listPARQ, covid_data_child, impute= "mean", totals=TRUE, min=1,max=4) # note - FALSE:average, TRUE:sum
PARQscored$scores #The scores themselves
PARQ_totals <- as.data.frame(PARQscored$scores)#just the total scores
covid_data_child <- cbind(covid_data_child, PARQ_totals)#totals and raw scores

#Means, sds, range, histogram
summary(covid_data_child[,c("totalPARQ", "PARQwarmth" ,  "PARQhostile", "PARQneglect", "PARQundiff", "PARQcontrol")])

sd(covid_data_child$totalPARQ)
sd(covid_data_child$PARQwarmth)
sd(covid_data_child$PARQhostile)
sd(covid_data_child$PARQneglect)
sd(covid_data_child$PARQundiff)
sd(covid_data_child$PARQcontrol)

hist(covid_data_child$totalPARQ)
hist(covid_data_child$ PARQwarmth)
hist(covid_data_child$ PARQhostile)
hist(covid_data_child$ PARQneglect)
hist(covid_data_child$ PARQundiff)
hist(covid_data_child$ PARQcontrol)

# Correlation plot --------------------------------------------------------------------------- 
##Make the matrix

corrmatrix <-  dplyr::select(covid_data_child, par_age, par_ed_ord, par_gender_num, ch_age, ch_gender_num, income_famsize,
                             totalFES, Dep, Anx, Stress,
                             totalPARQ, PARQwarmth, PARQhostile, PARQneglect, PARQundiff, PARQcontrol, 
                             totalSDQ, SDQemo, SDQcon, SDQhyp, SDQpeer, SDQpro,
                             totalSDQ2, SDQemo2, SDQcon2, SDQhyp2, SDQpeer2, SDQpro2)


M <- cor(corrmatrix, use = "complete.obs")
res1 <- cor.mtest(corrmatrix, use = "complete.obs", 
                  conf.level = .95)

corrplot.mixed(M,  order = "hclust", lower.col = "black", number.cex = .6,
               tl.col = "black", tl.srt = 45,tl.cex = 0.8, tl.pos= "lt",
               p.mat = res1$p, sig.level = .05, insig = "blank") 


# Followups -----------------------------------------------------------------------------------------------------
# how many people interested in followup survey?

summarytools::freq(as.factor(covid_data$fllwup), order = "freq")


# Writing the scored files ----------------------------------------
# This will currently save in the working directory - you can set the path to save where you want

write.csv(covid_data, file = "covid_data_test.csv")
write.csv(covid_data_child, file = "covid_data_child_scored.csv")

```

