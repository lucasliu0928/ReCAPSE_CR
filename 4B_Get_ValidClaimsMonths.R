library(lubridate)
library(dplyr)
####Functions
#1.Convert column interger names to acutal date
convert_intCol_toDate <- function(enroll_df,month_col_indexes, min_date,max_date, date_unit){
  dates_seq <- seq(min_date,max_date,by = date_unit)
  colnames(enroll_df)[month_col_indexes] <- as.character(dates_seq)
  return(enroll_df)
}

#2.Check if enrolled ever
check_enrollment_claim_type <- function(claim_name, enroll_df_tocheck,id_colname, pt_id){
  curr_idex <- which(enroll_df_tocheck[,id_colname] == pt_id)
  
  if (length(curr_idex) > 0){
    enroll_flag <- paste0("Enrolled_",claim_name)
    curr_enroll_df <- enroll_df_tocheck[curr_idex,]
  }else{
    enroll_flag <- paste0("NOTEnrolled_",claim_name)
    curr_enroll_df <- NULL
  }
  return(list(enroll_flag,curr_enroll_df))
}

#File directorys
proj_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/"
raw_data_dir <- paste0(proj_dir,"ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/")
both_data_intermediate_dir <- paste0(proj_dir,"ReCAPSE_Intermediate_Data/0318_21/For_Both_Data/")
out_dir <- paste0(proj_dir,"ReCAPSE_Intermediate_Data/0318_21/For_Both_Data/")

#########################################################################################################
#I. Get Medicaid 
#IDs:  Study_id, id_medicaid 
#mon1-mon240 (from 1/2000-12/2019)
#########################################################################################################
enrollment_df <- read.csv(paste0(raw_data_dir,"kcr_medicaid_enroll_fb0015.csv"),stringsAsFactors = F)
#1. Get Month Columns
month_col_names <- paste0("mon", seq(1, 240,1)) 
month_col_indexes <- which(colnames(enrollment_df) %in% month_col_names)
#2. Convert Month column names from mon1-mon240 to (1/2000-12/2019)
enrollment_df <- convert_intCol_toDate(enrollment_df,month_col_indexes, my("1/2000"),my("12/2019"), "1 mon")
#3. remove all NA rows (All Month columns)
enrollment_df <- enrollment_df[complete.cases(enrollment_df[ ,month_col_indexes]),]

#4. Get IDs
Medicaid_IDs <- unique(enrollment_df[,"study_id"])
length(Medicaid_IDs) #13902

#########################################################################################################
#II. Get Medicare 
#Study_id , claim_id 
#Mon1-Mon324 (1/1991-12/2017)
#GHO1-GHO324 (HMO enrollment indicator)
#########################################################################################################
enrollment_df2 <- read.csv(paste0(raw_data_dir,"kcr_medicare_enroll_fb0015.csv"),stringsAsFactors = F)
#1. Remove GHO cols
enrollment_df2 <- enrollment_df2[, -which(grepl("GHO",colnames(enrollment_df2)) == T)]

#2.reorder ID column to be the 1st
enrollment_df2 <- relocate(enrollment_df2,"study_id")

#3. Get Month Columns
month_col_names <- paste0("MON", seq(1, 324,1)) 
month_col_indexes <- which(colnames(enrollment_df2) %in% month_col_names)

#4. Convert Month column names from Mon1-Mon324  to (1/1991-12/2017)
enrollment_df2 <- convert_intCol_toDate(enrollment_df2,month_col_indexes, my("1/1991"),my("12/2017"), "1 mon")

#5. remove all NA rows (All Month columns)
enrollment_df2 <- enrollment_df2[complete.cases(enrollment_df2[ , month_col_indexes]),]

#6.Recode enrollent 1,2,3 to 1 as enrolled
for (j in 1:length(month_col_indexes)){
  curr_col_idx <- month_col_indexes[j]
  enrollment_df2[which(enrollment_df2[,curr_col_idx] > 1),curr_col_idx] <- 1
}

#6. Get IDs
Medicare_IDs <- unique(enrollment_df2[,"study_id"])
length(Medicare_IDs) #33446

#########################################################################################################
#Load pateint event type and date data
#########################################################################################################
All_event_df <- read.csv(paste0(both_data_intermediate_dir,"updated_All_event_df.csv"),stringsAsFactors = F)
#1.Get number of IDs in event type files
event_type_IDs <- unique(All_event_df$ID) #41375

#2.Update IDs that in eventy type files and enrollment files
#2.1 Combine Ids in Medicaid_IDs and Medicare_IDs
#'@Question: 
#'9879 study_id are identical in medicaid and medicare, these pts might have both medicaid and medicare
#'#Keep them for now
both_enroll_IDs <- unique(c(Medicaid_IDs,Medicare_IDs)) #37469

#3. only process Ids in both event type data and enrollemnt  
analysis_Ids <- unique(intersect(both_enroll_IDs,event_type_IDs)) #32386
length(analysis_Ids)

#37469 pts has enrollmnet data, 32386 patients has both event type and enrollment type data
#########################################################################################################
#III.find valid claim month for each pts (the month date where enrollment = 1)
#########################################################################################################
options(warn=2) 
valid_claim_month_list <- list(NA)
for (i in 1:length(analysis_Ids)){
  if (i %% 1000 = 0){
    print(i)
  }
  curr_id <- analysis_Ids[i]
  
  #1.Check if enrolled in medicaid
  res <- check_enrollment_claim_type("Medicaid",enrollment_df,"study_id",curr_id)
  enroll_flag1 <- res[[1]]
  curr_enroll_df1 <- res[[2]]
  
  #2.Check if enrolled in medicare
  res <- check_enrollment_claim_type("Medicare",enrollment_df2,"study_id",curr_id)
  enroll_flag2 <- res[[1]]
  curr_enroll_df2 <- res[[2]]
  
  #3.Get all valid claim months
  Id_cols1 <- which(colnames(curr_enroll_df1) %in% c("study_id","Id_medicaid"))
  Id_cols2 <- which(colnames(curr_enroll_df2) %in% c("study_id"))
  
  if (enroll_flag1 == "Enrolled_Medicaid" & grepl("NOT",enroll_flag2) == T){ #if enrolled in medicaid only
    curr_valid_months <- colnames(curr_enroll_df1)[which(curr_enroll_df1==1)]
  }else if (grepl("NOT",enroll_flag1) == T & enroll_flag2 == "Enrolled_Medicare"){#if enrolled in medicare only
    curr_valid_months <- colnames(curr_enroll_df2)[which(curr_enroll_df2==1)]
  }else if (enroll_flag1 == "Enrolled_Medicaid" & enroll_flag2 == "Enrolled_Medicare"){ #if enrolled both
    curr_valid_months1 <- colnames(curr_enroll_df1)[which(curr_enroll_df1==1)]
    curr_valid_months2 <- colnames(curr_enroll_df2)[which(curr_enroll_df2==1)]
    curr_valid_months <- unique(c(curr_valid_months1,curr_valid_months2))
  }
  if (length(curr_valid_months) > 0){
    valid_claim_month_list[[i]]  <- curr_valid_months
  }else{
    valid_claim_month_list[[i]] <- NULL
  }
}

#########################################################################################################
#IV Truncate valid claim month for the duration of predefined starting and ending point
#1. Start point : the diagnosis date of the first primary breast cancer + 6 months
#2. End point:  2.1 without an SBCE :  end of recorded claims
#               2.2 with an SBCE, but no third event: end of recorded claims
#               2.3 with an SBCE, 
#                   the subsequent cancer is a non-breast primary cancer: 3 months before the registry-based diagnosis date or, 
#                   the subsequent is a breast cancer event (Recurrence or diagnose of breast cancer):  1 month before the first subsequent breast cancer event.

#########################################################################################################
truncated_valid_month <- list()
for (i in 1:length(analysis_Ids)){
    if (i %% 1000 == 0){
      print(i)
    }
    curr_id <- analysis_Ids[i]
    curr_valid_months <- valid_claim_month_list[[i]]
    if (length(curr_valid_months) > 0){ #if there are valid enrollemnts
          
          #sort enrollent monthes, get the end of claim month
          sorted_valid_months <- sort(ymd(curr_valid_months))
          last_valid_mon <- max(sorted_valid_months)
          
          #########curr event df
          curr_event_df <- All_event_df[which(All_event_df[,"ID"] == curr_id),]
          curr_1stPBC_date <- curr_event_df[,"Date_1st_Event"]
          
          #Start point
          curr_start <- mdy(curr_1stPBC_date) + months(6)
          
          ##End point
          #check if pt has the 3rd event
          curr_3rd_event_flag <- curr_event_df[,"Event3_Flag"]
          
          if (curr_3rd_event_flag == "Other Cancer"){ # diag dates - 3month
            curr_diag_date <- curr_event_df[,"Date_3rd_Event"]
            curr_end <- mdy(curr_diag_date) - months(3)
          }else if (curr_3rd_event_flag == "Primary BC" | curr_3rd_event_flag == "1Recur"){ #diag dates - 1month
            curr_diag_date <- curr_event_df[,"Date_3rd_Event"]
            curr_end <- mdy(curr_diag_date) - months(1)
          }else{ #use all valid claims
            curr_end <- last_valid_mon
          }
          
          #Valid claims between start and end point
          valid_months_idxes <- which(ymd(sorted_valid_months) >= curr_start & ymd(sorted_valid_months) <= curr_end )
          n_valid_months <- length(sorted_valid_months[valid_months_idxes])
          valid_month_df <- cbind.data.frame(rep(curr_id,n_valid_months),sorted_valid_months[valid_months_idxes])
          colnames(valid_month_df) <- c("ID","Valid_Month")
          truncated_valid_month[[i]] <- valid_month_df
        }else{
          truncated_valid_month[[i]] <- NULL
        }
}


all_truncated_valid_month_df <- do.call(rbind,truncated_valid_month)
write.csv(all_truncated_valid_month_df,paste0(out_dir,"All_truncated_valid_month_df.csv"),row.names = F)

#########################################################################################################
#V. Remove patients does not qualify the following: 
#'@Question1: and 3 months of claims before or after the SBCE  ( have calims at least 3 months  before SBCE or after))
#or any 6 months of claims for non-SBCE patients (At least 6 month data avaiable)
##'TODO.@Question2: ?Filter the claims table for patients with local or regional stage, 
#########################################################################################################
#Reload the following dataset, so that we do not have to run above function again.
#2.1 First load all event date file
All_event_df <- read.csv(paste0(both_data_intermediate_dir,"updated_All_event_df.csv"),stringsAsFactors = F)
#2.2 Load valid month df
all_valid_month_df <- read.csv(paste0(out_dir,"All_truncated_valid_month_df.csv"),stringsAsFactors = F)

#2.3 Find IDs that have both valid month data and event data
analysis_ids <- unique(intersect(all_valid_month_df$ID, All_event_df$ID))
length(analysis_ids) #29892

ct <- 1
ID_to_remove <- NA
for (i in 1:length(analysis_ids)){
  if (i %% 100 == 0){
    print(i)
  }
  curr_id <- analysis_ids[i]
  
  #event data
  curr_event_df <- All_event_df[which(All_event_df[,"ID"] == curr_id),]
  
  #Valid month data
  curr_idxes_in_valid_mondf <- which(all_valid_month_df[,"ID"] == curr_id)
  curr_valid_month_df <- all_valid_month_df[curr_idxes_in_valid_mondf, ]
  
  #check if patient has SBCE
  curr_SBCE_status <- curr_event_df$SBCE
  if (curr_SBCE_status == 0){ #if no recurrent
    #if not more than 6 month valid enrollment, exclude ID
    if (nrow(curr_valid_month_df) < 6){
      ID_to_remove[ct] <- curr_id
      ct <- ct+1
    }
  }else{ #if recurrent 
    curr_SBCE_date <- mdy(curr_event_df[,"Date_2nd_Event"])
    curr_SBCE_minus3Mon <- curr_SBCE_date - months(3)
    curr_SBCE_plus3Mon <- curr_SBCE_date + months(3)
    #check if this patient has valid month before curr_SBCE_minus3Mon or after curr_SBCE_plus3Mon
    n_month_before <- length(which(ymd(curr_valid_month_df[,"Valid_Month"]) < curr_SBCE_minus3Mon))
    n_month_after <-  length(which(ymd(curr_valid_month_df[,"Valid_Month"]) > curr_SBCE_plus3Mon))
    if (n_month_before == 0 & n_month_after == 0){ #if no claims before or after, exclude this ID
      ID_to_remove[ct] <- curr_id
      ct <- ct+1
    }

  }
}

updated_valid_month_df <- all_valid_month_df[-which(all_valid_month_df$ID %in% ID_to_remove), ]

write.csv(updated_valid_month_df,paste0(out_dir,"All_Final_Valid_month.csv"),row.names = F)
