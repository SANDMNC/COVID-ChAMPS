# Script to merge baseline and followup dataframes


# Read in the csv file
covid_data_bl <- read.csv("scored_data/covid_data_child_scored.csv", header=TRUE, stringsAsFactors = FALSE)
covid_data_fu <- read.csv("scored_data/covid_data_child_scored_fu.csv", header=TRUE, stringsAsFactors = FALSE)

# Select variables to merge
covid_data_bl <- covid_data_bl[,c("ResponseId","SDQemo","SDQhyp","SDQcon","totalPTSD")]

covid_data_fu <- covid_data_fu[,c("Response_ID_f","SDQemo_f","SDQhyp_f","SDQcon_f","totalPTSD_f")]

names(covid_data_fu)[names(covid_data_fu) == "Response_ID_f"] <- "ResponseId"


list.of.data.frames = list(covid_data_bl, covid_data_fu)
merged_data= Reduce(function(...) merge(..., by = "ResponseId", all=T), list.of.data.frames)

# Writing the scored files ----------------------------------------
write.csv(merged_data, file = "scored_data/merged_data.csv")
