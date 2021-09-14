source("Recapse_Ultility.R")

#onHPC
data_dir <- "/recapse/intermediate_data/4_RecurrDates_Outcome_Info/"
outdir <- "/recapse/intermediate_data/4_RecurrDates_Outcome_Info/"


#local
data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/4_RecurrDates_Outcome_Info/"
outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/4_RecurrDates_Outcome_Info/"

###########################################################################
#3. Load all event df
###########################################################################
All_event_df <- read.xlsx(paste0(outdir,"4_All_event_df.xlsx"),sheet = 1)


############################################################################################
#4. Get analysis ID
############################################################################################
analysis_IDs <- unique(All_event_df$study_id)

####################################################################################################
#8. Add SBCE outcome flag
#'@1.Definition: 
#' Has 2nd event (Either 1. Recurrence of the first primary breast cancer  
#'                Or     2. the diagnosis of a second primary breast cancer
#'                Or     3. first primary BC related death
#'#'@NOTE: there is a bug in previous code, Primary_C509$$$C502	death site C509, was not count for First priamry bc death
####################################################################################################
#1.definition flag
All_event_df$SBCE <- 0
sbce_idexes <- which(is.na(All_event_df[,"Date_2nd_Event"])==F)
All_event_df[sbce_idexes,"SBCE"] <- 1
table(All_event_df$SBCE) #    0:35485  1:4839  
outcome_df <- All_event_df[,c("study_id","SBCE")]
write.xlsx(outcome_df,paste0(outdir,"4_SBCE_Label.xlsx"))


####################################################################################################
#Report numbers
####################################################################################################
#1.first primary died with recur or diagnoise of 2nd primary
All_event_df$Died_And_RecurOr2ndPrimary <- NA
both_died_andRecur_idxes <- which(is.na(All_event_df[,"Date_2nd_Event"])==F  & 
                                  is.na(All_event_df$Primary_1stBC_Death_Date) == F)
All_event_df[both_died_andRecur_idxes,"Died_And_RecurOr2ndPrimary"] <- 1
All_event_df[-both_died_andRecur_idxes,"Died_And_RecurOr2ndPrimary"] <- 0
table(All_event_df$Died_And_RecurOr2ndPrimary)     #39132  1192   

#2.first primary died without SBCE 
died_noRecur_idxes <- which(is.na(All_event_df$Primary_1stBC_Death_Date) == F & 
                            is.na(All_event_df[,"Date_2nd_Event"])== T)
length(died_noRecur_idxes) #0

#3.has 2nd event, no 3rd
only2nd_no3rd_idxes <- which(is.na(All_event_df[,"Date_2nd_Event"])==F & 
                             is.na(All_event_df[,"Date_3rd_Event"])==T)
length(only2nd_no3rd_idxes) #4318
only2nd_no3rd_df <- All_event_df[only2nd_no3rd_idxes,]
table(only2nd_no3rd_df[,"Type_2nd_Event"])
#3.1 1st->Recur -> no 3rd    #2850 (2826 + 11+12+1)
#3.2 1st->2nd Primary -> no 3rd    784


#4. both 2nd and 3rd
both23_idxes <- which(is.na(All_event_df[,"Date_2nd_Event"])==F & 
                      is.na(All_event_df[,"Date_3rd_Event"])==F)
length(both23_idxes) #521
both23_df <- All_event_df[both23_idxes,]
#4.1 2nd event = recur (145)
both23_2ndEvent_Recur_indxes <- which(grepl("1Recur",both23_df[,"Type_2nd_Event"]) == T)
both23_2ndEvent_Recur_df <- both23_df[both23_2ndEvent_Recur_indxes,]
#4.1 1 (1st -> Recur-> primary #247)
length(which(grepl("Primary",both23_2ndEvent_Recur_df[,"Type_3rd_Event"])==T)) 
#4.1 2 (1st -> Recur-> other #103)
length(which(grepl("Other",both23_2ndEvent_Recur_df[,"Type_3rd_Event"])==T)) 

#4.2 2nd event = 2nd Primary (142)
both23_2ndEvent_Primary_df <- both23_df[-both23_2ndEvent_Recur_indxes,]
#4.1 1 (1st -> 2nd Primary -> Recur #51)
third_event_recur_indxes <- which(grepl("1Recur",both23_2ndEvent_Primary_df[,"Type_3rd_Event"])==T)
length(third_event_recur_indxes)

#4.1 2 (1st -> 2nd Primary -> other #72)
third_event_other_indxes <- which(grepl("Other",both23_2ndEvent_Primary_df[,"Type_3rd_Event"])==T)
length(third_event_other_indxes)

#4.1 3 (1st -> 2nd Primary -> Primary #48)
third_event_primary_df <- both23_2ndEvent_Primary_df[-c(third_event_recur_indxes,third_event_other_indxes),]
nrow(third_event_primary_df)
