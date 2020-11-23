# Script to grab the info needed to generate the follow-up survey email 

# Download from original survey the datafile (text responses rather than numeric)

#Read it in
ph1 <- read.csv("raw_data/covid_data_ph1.csv")
#Get rid of first 2 extra text rows
ph1<- ph1[-(1:2),]
#Turn all blanks to NAs
ph1[(ph1=="")] <- NA

#There is 1 child who labeled Male, Female. Looks like a  mistake based on they previously 
#described their 10 year old as Male
#Hard coding this change now
ph1[ph1$ResponseId=="R_OydrwcZcDtWK7ND","ch1_gender"] <- "Male"

#Recode capital letter to little letters
ph1$ch1_gender <- ifelse(ph1$ch1_gender == "Female", "female",
       ifelse(ph1$ch1_gender == "Male", "male", NA))
ph1$ch2_gender <- ifelse(ph1$ch2_gender == "Female", "female",
                         ifelse(ph1$ch2_gender == "Male", "male", NA))
ph1$ch3_gender <- ifelse(ph1$ch3_gender == "Female", "female",
                         ifelse(ph1$ch3_gender == "Male", "male", NA))
#Create blank vectors
ph1$ch1_details <- rep(NA, nrow(ph1))
ph1$ch2_details <- rep(NA, nrow(ph1))
ph1$ch3_details <- rep(NA, nrow(ph1))
ph1$ch4_details <- rep(NA, nrow(ph1))

#Change the NA to No to allow easier working with logic
ph1[is.na(ph1$ch2_trigger),"ch2_trigger"]  <- "No"
ph1[is.na(ph1$ch3_trigger),"ch3_trigger"]  <- "No"
ph1[is.na(ph1$ch4_trigger),"ch4_trigger"]  <- "No"

#Go through each child (only if the trigger question was answered yes), 
#and paste together the age and gender
for (i in 3:nrow(ph1)) {
  
  ph1$ch1_details[i]<- paste(ph1$ch1_age[i], "year old", ph1$ch1_gender[i])

if(ph1$ch2_trigger[i]=="Yes"){
  ph1$ch2_details[i]<- paste(ph1$ch2_age[i], "year old", 
                                                         ph1$ch2_gender[i])}

if(ph1$ch3_trigger[i]=="Yes"){ph1$ch3_details[i]<- paste(ph1$ch3_age[i], "year old", 
                                                         ph1$ch3_gender[i])}

if(ph1$ch4_trigger[i]=="Yes"){ph1$ch4_details[i]<- paste(ph1$ch4_age[i], "year old", 
                                                         ph1$ch4_gender[i])}
}

#Make some more blank vectors
ph1[grep("NA", ph1$ch1_details), "ch1_details"] <- NA
ph1[grep("NA", ph1$ch2_details), "ch2_details"] <- NA
ph1[grep("NA", ph1$ch3_details), "ch3_details"] <- NA
ph1[grep("NA", ph1$ch4_details), "ch4_details"] <- NA


#Grab only those participants who wanted to be part of ph2
ph2<- ph1[which(ph1$fllwup=="I am interested in being sent a follow-up survey in the future" |
            ph1$fllwup=="Both please!"), c("email", "ResponseId", "ch1_details", "ch2_details",
                                           "ch3_details", "ch4_details") ]

#And only for people who have an email there
ph2<- ph2[!is.na(ph2$email),]

#The group which we have only has max 3 children answered for

ph2$Children_T1_info <- rep(NA, nrow(ph2))

for (i in 1:nrow(ph2)){
  if(!is.na(ph2$ch1_details[i]) & is.na(ph2$ch2_details[i])){
    ph2$Children_T1_info[i] <- ph2$ch1_details[i]
  }
  
  if(!is.na(ph2$ch1_details[i]) & !is.na(ph2$ch2_details[i])){
    ph2$Children_T1_info[i] <- paste(ph2$ch1_details[i], "and", ph2$ch2_details[i])
    
    if(!is.na(ph2$ch3_details[i])){
      ph2$Children_T1_info[i] <- paste(ph2$Children_T1_info[i], "and", ph2$ch3_details[i])
      
      if(!is.na(ph2$ch4_details[i])){
        ph2$Children_T1_info[i] <- paste(ph2$Children_T1_info[i], "and", ph2$ch4_details[i])
      }
    }
  }
}



# Do the same thing with the people who interested in survey results
#Grab only those participants who wanted to be part of ph2
ph2_summary<- ph1[which(ph1$fllwup=="I am interested in being sent a summary of findings once available"), c("email", "ResponseId", "ch1_details", "ch2_details",
                                                 "ch3_details", "ch4_details") ]

#And only for people who have an email there
ph2_summary<- ph2_summary[!is.na(ph2_summary$email),]

#The group which we have only has max 3 children answered for

ph2_summary$Children_T1_info <- rep(NA, nrow(ph2_summary))

for (i in 1:nrow(ph2_summary)){
  if(!is.na(ph2_summary$ch1_details[i]) & is.na(ph2_summary$ch2_details[i])){
    ph2_summary$Children_T1_info[i] <- ph2_summary$ch1_details[i]
  }
  
  if(!is.na(ph2_summary$ch1_details[i]) & !is.na(ph2_summary$ch2_details[i])){
    ph2_summary$Children_T1_info[i] <- paste(ph2_summary$ch1_details[i], "and", ph2_summary$ch2_details[i])
    
    if(!is.na(ph2_summary$ch3_details[i])){
      ph2_summary$Children_T1_info[i] <- paste(ph2_summary$Children_T1_info[i], "and", ph2_summary$ch3_details[i])
      
      if(!is.na(ph2_summary$ch4_details[i])){
        ph2_summary$Children_T1_info[i] <- paste(ph2_summary$Children_T1_info[i], "and", ph2_summary$ch4_details[i])
      }
    }
  }
}

#Rename the variables and just export the 3 columns needed to put into Qualtrics
colnames(ph2)[colnames(ph2)=="email"] <- "Email"
colnames(ph2_summary)[colnames(ph2_summary)=="email"] <- "Email"
colnames(ph2)[colnames(ph2)=="ResponseId"] <- "Response_ID"
colnames(ph2_summary)[colnames(ph2_summary)=="ResponseId"] <- "Response_ID"
ph2$ch1_details <- NULL
ph2$ch2_details <- NULL
ph2$ch3_details <- NULL
ph2$ch4_details <- NULL
ph2_summary$ch1_details <- NULL
ph2_summary$ch2_details <- NULL
ph2_summary$ch3_details <- NULL
ph2_summary$ch4_details <- NULL

# Export--------------------------------------------------------------------------------------------
write.csv(ph2, "scored_data/COVID_child_details.csv")
write.csv(ph2_summary, "scored_data/COVID_child_details_summary.csv")
