library(lubridate)

data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/"
out_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0225_21/"

uh3_kcr_df <- read.csv(paste0(data_dir,"uh3_kcrdata.csv"),stringsAsFactors = F)
unique_IDs <- unique(uh3_kcr_df$study_id)


#you need set your selection criteria for cancer sequence equals to ‘0’ or ‘1’
check <- uh3_kcr_df[which(uh3_kcr_df[,"CentralSequenceNumber"]  %in% c(0,1)),]



#Get first_primary and Second_primary date
primary_date_df <- as.data.frame(matrix(NA,nrow = length(unique_IDs),ncol = 4))
colnames(primary_date_df) <-c ("ID","first_primary_date","Second_primary_date","n_1stPrimary_records")

for (p in 1:length(unique_IDs)){
  #Get Id
  curr_id <- unique_IDs[p]
  primary_date_df[p,"ID"] <- curr_id
  
  #Get curr id data
  curr_df <- uh3_kcr_df[which(uh3_kcr_df[,"study_id"] == curr_id),]
  
  #Get first primary cases, set your selection criteria for cancer sequence equals to ‘0’ or ‘1’
  curr_first_primary_df <- curr_df[which(curr_df[,"CentralSequenceNumber"]  %in% c(0,1)),]
  #Get number of first primary records, check how many pts has 0 and 1
  primary_date_df[p,"n_1stPrimary_records"] <- nrow(curr_first_primary_df)
  
  if (nrow(curr_first_primary_df) > 0){
    curr_first_primary_date <- curr_first_primary_df[,"Date_dx"]
  }else{
    curr_first_primary_date <- NA
  }
  primary_date_df[p,"first_primary_date"] <- curr_first_primary_date

  #Get second primary: set your selection criteria for cancer sequence equals to ‘2’ 
  curr_second_primary_df <- curr_df[which(curr_df[,"CentralSequenceNumber"] == 2),]
  if (nrow(curr_second_primary_df) > 0){
    curr_second_primary_date <- curr_second_primary_df[,"Date_dx"]
  }else{
    curr_second_primary_date <- NA
  }
  primary_date_df[p,"Second_primary_date"] <- curr_second_primary_date
  
}


#Get non-recurrent patient idxes who has 1st , but not 2nd
non_recurrence_pt_indxes <-  which(is.na(primary_date_df[,"first_primary_date"])==F & is.na(primary_date_df[,"Second_primary_date"])==T)
#Get recurrent patient idxes who has 1st and 2nd
recurrence_pt_indxes <-  which(is.na(primary_date_df[,"first_primary_date"])==F & is.na(primary_date_df[,"Second_primary_date"])==F)

length(non_recurrence_pt_indxes) #39595
length(recurrence_pt_indxes) #1780

#Add recurrence flag
updated_primary_date_df <-primary_date_df
updated_primary_date_df$Recurrence <- NA
updated_primary_date_df$Recurrence[non_recurrence_pt_indxes] <- 0
updated_primary_date_df$Recurrence[recurrence_pt_indxes] <- 1

updated_primary_date_df <- updated_primary_date_df[-which(is.na(updated_primary_date_df$Recurrence)==T),]

#Check if first_primary_date < Second_primary_date
first_LT_2nd_indxes <- which(mdy(updated_primary_date_df$first_primary_date) < mdy(updated_primary_date_df$Second_primary_date))
first_equal_2nd_indxes <- which(mdy(updated_primary_date_df$first_primary_date) == mdy(updated_primary_date_df$Second_primary_date))
first_GT_2nd_indxes <- which(mdy(updated_primary_date_df$first_primary_date) > mdy(updated_primary_date_df$Second_primary_date))

length(first_LT_2nd_indxes) #1359
length(first_equal_2nd_indxes) #421
length(first_GT_2nd_indxes) #0


#Remove patients who has 1st  == 2nd date for final analysis 
updated_primary_date_df <-updated_primary_date_df[-first_equal_2nd_indxes,]
write.csv(updated_primary_date_df,paste0(out_dir,"primary_date_df.csv"))

