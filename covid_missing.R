#Script to examine missing data

# This script looks at missing data from the main variables examined in the analyses (sw_analyses).
# Takes as input the scored datafile.
# Note this only examines score totals, or subscale totals - not at the individual item level.
# And the scoring script has most of the scales and subscales already using mean substitution if more than 70% of scale is there.
# 
# Source of code and plots: https://datascienceplus.com/imputing-missing-data-with-r-mice-package/

#set to appropriate working directory
setwd("/Volumes/Groups/Adaptdir/COVID-CHAMPS/Data")
covid_data_child <- read.csv("covid_data_child_scored.csv", header=TRUE, stringsAsFactors = FALSE)
# Select variables for MI
covid_data_child_for_MI <- covid_data_child[c(1:298,300:368,370:373,375:410,412:435,438:457,459:463,466:469,471,473:474,477,479,482:497),c(1,44,295:300,303,313,338,353,422:427,449,514:533)]

#Load packages
library(dplyr)
library(naniar) #for examining missing
library(ggplot2)
library(mice) #missing data imputation 
library(VIM)
library(finalfit) #another package which has functions to examine missing


#Selecting the variables used in analysis
covid_child_missing<- dplyr::select(covid_data_child_for_MI, id, 
                             ch_age.c, par_age, par_ed_ord, income_famsize, country_cat, ch_gender_num,
                             PARQhostile.c, PARQcontrol.c, PARQwarmth.c, par_gender_num,
                             totalSDQ.c,SDQemo.c, SDQhyp.c, SDQcon.c, SDQpeer.c,
                             totalSDQ2, SDQemo2, SDQhyp2, SDQcon2, SDQpeer2,
                             DASSDep.c, DASSAnx.c, DASSStress.c,
                             totalFES.c, totalPTSD, totalDASS, iso,
                             totalcov_dist.c, covid_pos_num.c, covid_finance_num.c, 
                             facts_comm.c, emotion_comm.c, self_comm.c,
                             par_past_mh_num, other_hosp_num)

#visualising overall main data
vis_miss(covid_child_missing) + theme(axis.text.x = element_text(size=6, angle=90))


#Function that looks at percentage of missing data
pMiss <- function(x){sum(is.na(x))/length(x)*100}
#Use that function to examine which missing data in columns (2), then rows (1))
apply(covid_child_missing,2,pMiss)
apply(covid_child_missing,1,pMiss)


aggr_plot <- aggr(covid_child_missing, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(covid_child_missing), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#Can use this to look at particular relationship and how missing from one varibales impacts the spread of the other variable. Change the variables as desired - 1 example below
marginplot(covid_child_missing[c("totalSDQ2","DASSStress")])

#compare missing and non-missing
explanatory = c("ch_age", "par_age", "par_ed_ord", "income_famsize", "country_cat", "ch_gender_num",
                             "PARQhostile", "PARQcontrol", "PARQwarmth",
                             "totalSDQ", "SDQemo", "SDQhyp", "SDQcon", "SDQpeer",
                             "DASSDep", "DASSAnx", "DASSStress",
                             "totalFES", "totalPTSD",
                             "totalcov_dist", "covid_pos_num", "covid_finance_num", 
                             "facts_comm", "emotion_comm", "self_comm",
                             "par_past_mh_num")
dependent = "totalSDQ2"
covid_child_missing %>% 
  missing_compare(dependent, explanatory) %>% 
    knitr::kable(row.names=FALSE, align = c("l", "l", "r", "r", "r")) # Omit when you run




## Creating an imputed dataset

# Use function mice.
# m=5 refers to the number of imputed datasets. Five is the default value.
# meth='pmm' refers to the imputation method. In this case we are using predictive mean matching as imputation method. Other imputation methods can be used, type methods(mice) for a list of the available imputation methods.

miData <- mice(covid_child_missing,m=50,maxit=50,meth='pmm',seed=500)
summary(miData)

#Shows imputed datset and observed (blue)
densityplot(miData)

#Another look at distributions
stripplot(miData, pch = 20, cex = 1.2)

# Makes complete dataset based on imputed dataset (the number is the set it will use)
completedData <- complete(miData,2)

