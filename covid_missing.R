#Script to examine missing data

# This script looks at missing data from the main variables examined in the analyses (sw_analyses).
# Takes as input the scored datafile.
# Note this only examines score totals, or subscale totals - not at the individual item level.
# And the scoring script has most of the scales and subscales already using mean substitution if more than 70% of scale is there.
# 
# Source of code and plots: https://datascienceplus.com/imputing-missing-data-with-r-mice-package/

#set to appropriate working directory
#setwd("/Users/sarah/Dropbox/2020/COVID_parenting/COVID_script/")

#Load packages
library(dplyr)
library(naniar) #for examining missing
library(ggplot2)
library(mice) #missing data imputation 
library(VIM)
library(finalfit) #another package which has functions to examine missing


#Selecting the variables used in analysis
covid_child_missing<- dplyr::select(covid_data_child, 
                             ch_age, par_age, par_ed_ord, income_famsize, country_num, ch_gender_num,
                             PARQhostile, PARQcontrol, PARQwarmth,
                             totalSDQ,SDQemo, SDQhyp, SDQcon, SDQpeer,
                             totalSDQ2, SDQemo2, SDQhyp2, SDQcon2, SDQpeer2,
                             DASSDep, DASSAnx, DASSStress,
                             totalFES, totalPTSD,
                             totalcov_dist, covid_pos_num, covid_finance_num, 
                             facts_comm, emotion_comm, self_comm,
                             ch_talk_about_6_num, par_past_mh_num)

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
explanatory = c("ch_age", "par_age", "par_ed_ord", "income_famsize", "country_num", "ch_gender_num",
                             "PARQhostile", "PARQcontrol", "PARQwarmth",
                             "totalSDQ", "SDQemo", "SDQhyp", "SDQcon", "SDQpeer",
                             "DASSDep", "DASSAnx", "DASSStress",
                             "totalFES", "totalPTSD",
                             "totalcov_dist", "covid_pos_num", "covid_finance_num", 
                             "facts_comm", "emotion_comm", "self_comm",
                             "ch_talk_about_6_num", "par_past_mh_num")
dependent = "totalSDQ2"
covid_child_missing %>% 
  missing_compare(dependent, explanatory) %>% 
    knitr::kable(row.names=FALSE, align = c("l", "l", "r", "r", "r")) # Omit when you run




## Creating an imputed dataset

# Use function mice.
# m=5 refers to the number of imputed datasets. Five is the default value.
# meth='pmm' refers to the imputation method. In this case we are using predictive mean matching as imputation method. Other imputation methods can be used, type methods(mice) for a list of the available imputation methods.

tempData <- mice(covid_child_missing,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)

#Shows imputed datset and observed (blue)
densityplot(tempData)

#Another look at distributions
stripplot(tempData, pch = 20, cex = 1.2)

# Makes complete dataset based on imputed dataset (the number is the set it will use)
completedData <- complete(tempData,1)