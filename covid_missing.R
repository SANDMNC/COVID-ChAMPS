#Script to examine missing data

# This script looks at missing data from the main variables examined in the analyses (sw_analyses).
# Takes as input the scored datafile.
# Note this only examines score totals, or subscale totals - not at the individual item level.
# And the scoring script has most of the scales and subscales already using mean substitution if more than 70% of scale is there.
# 
# Source of code and plots: https://datascienceplus.com/imputing-missing-data-with-r-mice-package/

#read in data
covid_data_child_for_MI <- read.csv("/scored_data/covid_data_child_scored_for_MI.csv", header=TRUE, stringsAsFactors = FALSE)


#Load packages
library(dplyr) #v1.0.0
library(naniar) #for examining missing #v0.5.1
library(ggplot2) #v3.3.1
library(mice) #missing data imputation #v3.9.0
library(VIM) #v6.0.0
library(finalfit) #another package which has functions to examine missing #v1.0.1


#Selecting the variables used in analysis
covid_child_missing<- dplyr::select(covid_data_child_for_MI, id, ch_age.c, par_age, par_ed_ord, income_famsize.c, country_cat, 
                             ch_gender_num, PARQhostile.c, PARQcontrol.c, PARQwarmth.c, par_gender_num,
                             totalSDQ.c, SDQemo.c, SDQhyp.c, SDQcon.c, SDQpeer.c,
                             totalSDQ2, SDQemo2, SDQhyp2, SDQcon2, SDQpeer2,
                             totalDASS.c, DASSDep.c, DASSAnx.c, DASSStress.c,
                             totalFES.c, totalPTSD, iso,
                             totalcov_dist.c, covid_pos_num.c, covid_finance_num.c, 
                             facts_comm.c, emotion_comm.c, self_comm.c,
                             par_past_mh_num, other_hosp_num, other_par_num)


#visualising overall main data
vis_miss(covid_child_missing) + theme(axis.text.x = element_text(size=6, angle=90))


#Function that looks at percentage of missing data
pMiss <- function(x){sum(is.na(x))/length(x)*100}
#Use that function to examine which missing data in columns (2), then rows (1))
apply(covid_child_missing,2,pMiss)
apply(covid_child_missing,1,pMiss)


aggr_plot <- aggr(covid_child_missing, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(covid_child_missing), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#Can use this to look at particular relationship and how missing from one varibales impacts the spread of the other variable. Change the variables as desired - 1 example below
# marginplot(covid_child_missing[c("totalSDQ2","DASSStress")])

# #compare missing and non-missing
# explanatory = c("ch_age", "par_age", "par_ed_ord", "income_famsize", "country_cat", "ch_gender_num",
#                              "PARQhostile", "PARQcontrol", "PARQwarmth",
#                              "totalSDQ", "SDQemo", "SDQhyp", "SDQcon", "SDQpeer",
#                              "DASSDep", "DASSAnx", "DASSStress",
#                              "totalFES", "totalPTSD",
#                              "totalcov_dist", "covid_pos_num", "covid_finance_num", 
#                              "facts_comm", "emotion_comm", "self_comm",
#                              "par_past_mh_num")
# dependent = "totalSDQ2"
# covid_child_missing %>% 
#   missing_compare(dependent, explanatory) %>% 
#     knitr::kable(row.names=FALSE, align = c("l", "l", "r", "r", "r")) # Omit when you run




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


#export data
save(miData, file = "/scored_data/miData.rda")

