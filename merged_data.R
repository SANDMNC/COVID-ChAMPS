# Script to merge baseline and followup data

#
library(rlang)
library(ggplot2)
library(ggstatsplot)

# Read in the csv file
# Baseline
covid_data_bl <- read.csv("scored_data/covid_data_child_scored.csv", header=TRUE, 
                          stringsAsFactors = FALSE)
# Follow-up
covid_data_fu <- read.csv("scored_data/covid_data_child_scored_fu.csv", header=TRUE, 
                          stringsAsFactors = FALSE)

#Put into one variable all the ages of the children
covid_data_bl$allchildage<- paste(round(covid_data_bl[,27],0),round(covid_data_bl[,28],0),
                                  round(covid_data_bl[,29],0),round(covid_data_bl[,30],0), 
                                  round(covid_data_bl[,31],0), round(covid_data_bl[,32],0))
#Put into one variable the gender of child
covid_data_bl$allchildgender<- paste(covid_data_bl[,33],covid_data_bl[,34],covid_data_bl[,35],
                                  covid_data_bl[,36],covid_data_bl[,37],covid_data_bl[,38])

#DASS, covid stress, parental hostility/warmth
# "DASSAnx"  "DASSDep"  "DASSStress"
# [511] "totalPARQ"             "PARQwarmth"           
# [513] "PARQhostile"           "PARQneglect"          
# [515] "PARQundiff"            "PARQcontrol"
# 



# Select variables to merge
bl_select <- covid_data_bl[,c("ResponseId","ch_age","ch_gender", "child_num", "allchildage","allchildgender",
                              "SDQemo","SDQhyp", "SDQcon", "totalPTSD", 
                              "PARQwarmth",  "PARQhostile",  "PARQneglect", "PARQcontrol","PARQundiff" ,
                              "DASSAnx", "DASSDep" , "DASSStress",
                              "totalcov_dist" ,
                              "country", "aus_postcode","EndDate", "Progress")]

bl_select$ch_age <- round(bl_select$ch_age, 0)

fu_select <- covid_data_fu[,c("Response_ID_f","ch_age_f_f", "ch_gender_f_f","SDQemo_f",
                              "SDQhyp_f","SDQcon_f","totalPTSD_f", 
                             
                              "DASSAnx_f", "DASSDep_f" , "DASSStress_f",
                              "totalcov_dist_f" ,
                              "aus_postcode_f", "EndDate_f", "Progress_f")]

# "PARQwarmth_f",  "PARQhostile_f",  "PARQneglect_f", "PARQcontrol_f","PARQundiff_f" ,
#don't think it is scored

# Missing data checks-------------------------------------------------------------------------------


which(is.na(bl_select[,1:3]), arr.ind=TRUE)
# no missing data in the baseline


which(is.na(fu_select[,1:7]), arr.ind=TRUE)
# There is missing data in follow up which was messing up the loop below

# Cut these children with missing data out of the dataframe
fu_select <- fu_select[-which(is.na(fu_select[,1:7]), arr.ind=TRUE)[,1],]
# number of children so far
nrow(fu_select[-which(is.na(fu_select[,1:7]), arr.ind=TRUE)[,1],])


# Going to put them into list format for easier viewing of children in the same family -------------
 
length_of_list <- length(unique(fu_select$Response_ID_f))
nested_list <- vector(mode = "list", length = length_of_list)

# Loop through each parent response and put the baseline and follow-up data in 
for (k in 1:length_of_list) {
  
  nested_list[[k]] <- vector(mode = "list", length = 3)
  
  nested_list[[k]][[1]] <- bl_select[(bl_select$ResponseId==(unique(fu_select$Response_ID_f)[k])),]
  
  nested_list[[k]][[2]] <- fu_select[(fu_select$Response_ID_f==(unique(fu_select$Response_ID_f)[k])),]
  
}
nested_list

edited_nested_list <- nested_list  

# Investigations------------------------------------------------------------------------------------
# Noticed when examining the list above that some families have multiple entries at followup
# Visualise better just the ones who appear to be double-ups

# Make an empty data frame, and add the columns headings
doubleupdata_toclean <- data.frame(matrix(ncol = 11, nrow = length(edited_nested_list)))
colnames(doubleupdata_toclean) <- c("ID", "repeat", "progress", "date_complete", "child_num", 
                                    "allchildage", "allchildgender", "child_age_bl", "child_gender_bl",
                                    "child_age_fu", "child_gender_fu")
#loop through the participants
for (i in 1:length(edited_nested_list)) {
  
  doubleupdata_toclean[i,1] <- edited_nested_list[[i]][[1]]$ResponseId[1]
  
  if(length(unique(edited_nested_list[[i]][[2]]$EndDate_f))>1){
    
    doubleupdata_toclean[i,"repeat"] <- "repeat"
    doubleupdata_toclean[i,"progress"] <- paste(edited_nested_list[[i]][[2]]$Progress_f, collapse = " ")
    doubleupdata_toclean[i,"date_complete"] <- paste(edited_nested_list[[i]][[2]]$EndDate_f, collapse = " ")
    
    doubleupdata_toclean[i,"child_num"] <- edited_nested_list[[i]][[1]]$child_num[1]
    doubleupdata_toclean[i,"allchildage"] <- edited_nested_list[[i]][[1]]$allchildage[1]
    doubleupdata_toclean[i,"allchildgender"] <- edited_nested_list[[i]][[1]]$allchildgender[1]
    
    doubleupdata_toclean[i,"child_age_bl"]  <- paste(edited_nested_list[[i]][[1]]$ch_age, collapse = " ")
    doubleupdata_toclean[i,"child_gender_bl"]  <- paste(edited_nested_list[[i]][[1]]$ch_gender, collapse = " ")
    doubleupdata_toclean[i,"child_age_fu"]  <- paste(edited_nested_list[[i]][[2]]$ch_age_f_f, collapse = " ")
    doubleupdata_toclean[i,"child_gender_fu"]  <- paste(edited_nested_list[[i]][[2]]$ch_gender_f_f, collapse = " ")
  
  }
}
# View the double ups in one dataframe
doubleupdata_toclean

# Cleaning -----------------------------------------------------------------------------------------
#Remove double up entries

for (i in 1:length(edited_nested_list)) {
  
  if(length(unique(edited_nested_list[[i]][[2]]$EndDate_f))>1){
    
    #sort the unique date as the earliest first, and select only the first one
    first_date<- sort(as.Date(unique(edited_nested_list[[i]][[2]]$EndDate_f), format="%d/%m/%y"))[1]
    #identify the positions of the first date and store
    positions<- as.Date(edited_nested_list[[i]][[2]]$EndDate_f, format="%d/%m/%y")==first_date
    #subset
    edited_nested_list[[i]][[2]] <- edited_nested_list[[i]][[2]][positions,]
    
  }
}
edited_nested_list

#Match the remaining children to their baselines----------------------------------------------------

for (i in 1:length(edited_nested_list)) {
  
  # Add in a third element to the list, which is for the children which did both baseline and followup and could be merged
  edited_nested_list[[i]][[3]] <- data.frame(matrix(ncol = 50, nrow = nrow(edited_nested_list[[i]][[2]])))
  
  # Loop through 
  for (j in 1:nrow(edited_nested_list[[i]][[2]])) {
      
    for (k in 1:nrow(edited_nested_list[[i]][[1]])) {     
      # If, across baseline and follow-up, both the gender and the age 
      # (also age plus 1, to allow for the time passing) of the child match, then merge them
      if(edited_nested_list[[i]][[2]][j,]$ch_gender_f_f == edited_nested_list[[i]][[1]][k,]$ch_gender & 
        (edited_nested_list[[i]][[2]][j,]$ch_age_f_f == edited_nested_list[[i]][[1]][k,]$ch_age | 
        edited_nested_list[[i]][[2]][j,]$ch_age_f_f == edited_nested_list[[i]][[1]][k,]$ch_age + 1)){
        edited_nested_list[[i]][[3]][j,] <- merge(edited_nested_list[[i]][[1]][k,], 
                                                  edited_nested_list[[i]][[2]][j,]) 
      }
    } #add in column names
    colnames(edited_nested_list[[i]][[3]]) <- colnames(merge(edited_nested_list[[i]][[1]][k,], 
                                                             edited_nested_list[[i]][[2]][j,]) )
  }
}
edited_nested_list

# problems still
# 7 (R_1QzsoNkSLf5mIKF) maybe answered for same child twice at time 2?

edited_nested_list[[7]]
#Look at the 11 year old at time 2

# Numbers of children follow-up---------------------------------------------------------------------
# How many children we have for time 1 and time 2, that have the data present for the SDQ etc

# create empty vectors
numchildren <- vector() 
numchildrentotal <- vector() 

for (l in 1:length(edited_nested_list)) {
  
  numchildren[l] <- sum(!is.na(edited_nested_list[[l]][[3]]$ResponseId))
  numchildrentotal[l] <- nrow(edited_nested_list[[l]][[2]])
}

# Total children answered for in follow up
fu_total_n <- sum(numchildrentotal)
# Total children who have both baseline and follow up for
fu_n <- sum(numchildren)


# Merge kids who have both into one data frame for analyses-----------------------------------------
# Create empty data frame with the number of rows and columns
followup <- data.frame(matrix(ncol = ncol(edited_nested_list[[1]][[3]]), nrow = fu_n))
colnames(followup) <- colnames(edited_nested_list[[1]][[3]])
# Loop through the nested list and pull the merged baseline-followups into new data frame
for (i in 1:length(edited_nested_list)) {
  followup[((sum(numchildren[1:i]))-(numchildren[i]-1)):(sum(numchildren[1:i])),] <- 
    edited_nested_list[[i]][[3]][!is.na(edited_nested_list[[i]][[3]]$ResponseId),]
}
followup

# Country

table(followup$country)

# Remove some unnecessary columns for data analysis
followup[,c("child_num","allchildage","allchildgender","ch_gender_f_f", "Progress_f", "Progress", 
            "Response_ID_f")] <- NULL

# Reshape from wide to long ------------------------------------------------------------------------
reshaped <- reshape(followup, varying=list(SDQemo= c("SDQemo", "SDQemo_f"), 
                                           SDQhyp= c("SDQhyp","SDQhyp_f"), 
                                           SDQcon= c("SDQcon","SDQcon_f"),
                                           totalPTSD= c("totalPTSD", "totalPTSD_f"),
                                           DASSAnx= c("DASSAnx","DASSAnx_f"),
                                           DASSDep= c("DASSDep","DASSDep_f"),
                                           DASSStress= c("DASSStress","DASSStress_f"),
                                           totalcov_dist= c("totalcov_dist","totalcov_dist_f")), 
        v.names=c("SDQemo", "SDQhyp", "SDQcon", "totalPTSD", "DASSAnx", "DASSDep", "DASSStress",
                  "totalcov_dist"), 
        direction="long",  
        times=1:2,        # substitutes number for T1 and T2
        timevar="times")  # to name the time col

#First basic look at T1 and T2 symptoms change over time
interaction.plot(reshaped$times,reshaped$id, reshaped$SDQemo,
                 xlab="time", ylab="SDQemo", col=c(1:10), legend=F) 
interaction.plot(reshaped$times,reshaped$id, reshaped$SDQhyp,
                 xlab="time", ylab="SDQhyp", col=c(1:10), legend=F) 
interaction.plot(reshaped$times,reshaped$id, reshaped$SDQcon,
                 xlab="time", ylab="SDQcon", col=c(1:10), legend=F)
interaction.plot(reshaped$times,reshaped$id, reshaped$totalPTSD,
                 xlab="time", ylab="totalPTSD", col=c(1:10), legend=F) 
interaction.plot(reshaped$times,reshaped$id, reshaped$DASSAnx,
                 xlab="time", ylab="DASSAnx", col=c(1:10), legend=F) 

  
# Box plots and group differences-------------------------------------------------------------------

# ggplot paired boxplots


myviolinplot <- function(mydf, myxcol, myycol){
  ggplot(mydf, aes(x={{myxcol}}, y={{myycol}})) + 
  geom_violin() +
  geom_boxplot(width=0.1) +
  stat_compare_means(method = "wilcox.test", paired = TRUE,
                     label.x = 1.5,
                     label.y.npc = "top") +
  stat_compare_means(method = "t.test", paired = TRUE,
                     label.x = 0.5,
                     label.y.npc = "top")
  
}


reshaped$times<- as.factor(reshaped$times)


p1 <- myviolinplot(reshaped, times, SDQemo)
p2 <- myviolinplot(reshaped, times, SDQhyp)
p3 <- myviolinplot(reshaped, times, SDQcon)

desiredid <- reshaped[is.na(reshaped$totalPTSD),"id"]
p4 <- myviolinplot(reshaped[!reshaped$id==desiredid,], times, totalPTSD)

p5 <- myviolinplot(reshaped, times, DASSAnx)
p6 <- myviolinplot(reshaped, times, DASSDep)
p7 <- myviolinplot(reshaped, times, DASSStress)
p8 <- myviolinplot(reshaped, times, totalcov_dist)


ggarrange(p1, p2, p3, p4,
          ncol = 2, nrow = 2)

ggarrange(p5, p6, p7,p8,
          ncol = 2, nrow = 2)

#ttest
t.test(SDQemo ~ times, data = reshaped, paired = TRUE)
t.test(SDQhyp ~ times, data = reshaped, paired = TRUE)
t.test(SDQcon ~ times, data = reshaped, paired = TRUE)
t.test(totalPTSD ~ times, data = reshaped, paired = TRUE)
t.test(DASSAnx ~ times, data = reshaped, paired = TRUE)
t.test(DASSDep ~ times, data = reshaped, paired = TRUE)
t.test(DASSStress ~ times, data = reshaped, paired = TRUE)



