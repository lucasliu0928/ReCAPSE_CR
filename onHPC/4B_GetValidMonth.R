library(lubridate)
library(dplyr)

#onHPC
data_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
outdir <- "/recapse/intermediate_data/"


#local
data_dir <- "/Volumes/LJL_ExtPro/Data/Testing data for UH3 - Dec 16 2020/"
outdir <- "/Users/lucasliu/Desktop/intermediate_data/"

#########################################################################################################
#Load enrollment month
#########################################################################################################
enrollment_start_end_df <- read.xlsx(paste0(outdir,"enrollment_start_end_df.xlsx"),sheet = 1)


#########################################################################################################
#Load pateint event type and date data
#########################################################################################################
All_event_df <- read.csv(paste0(outdir,"updated_All_event_df.csv"),stringsAsFactors = F)
#1.Get number of IDs in event type files
event_type_IDs <- unique(All_event_df$ID) #40329

#3. only process Ids in both event type data and enrollemnt  
analysis_Ids <- unique(intersect(enrollment_start_end_df$study_id,event_type_IDs)) #31266

#########################################################################################################
#IV Truncate valid claim month for the duration of predefined starting and ending point
#1. Start point : the diagnosis date of the first primary breast cancer + 6 months
#2. End point:  2.1 without an SBCE :  end of recorded claims
#               2.2 with an SBCE, but no third event: end of recorded claims
#               2.3 with an SBCE, 
#                   the subsequent cancer is a non-breast primary cancer: 3 months before the registry-based diagnosis date or, 
#                   the subsequent is a breast cancer event (Recurrence or diagnose of breast cancer):  1 month before the first subsequent breast cancer event.

#########################################################################################################
valid_month_df <- as.data.frame(matrix(NA, nrow = length(analysis_Ids), ncol = 3))
colnames(valid_month_df) <- c("study_id" , "Valid_Start","Valid_End")

for (i in 1:length(analysis_Ids)){ 
    if (i %% 1000 == 0){
      print(i)
    }
    curr_id <- analysis_Ids[i]
    #current enrollment
    curr_enroll_df <- enrollment_start_end_df[which(enrollment_start_end_df$study_id == curr_id),]
    last_enroll_mon <- curr_enroll_df[,"Enroll_End"]
    
    #########curr event df
    curr_event_df <- All_event_df[which(All_event_df[,"ID"] == curr_id),]
    curr_1stPBC_date <- curr_event_df[,"Date_1st_Event"]
    
    #Start point
    curr_start <- mdy(curr_1stPBC_date) + days(6*30)  #in the case of + 6months returns NA

    ##End point
    #check if pt has the 3rd event
    curr_3rd_event_flag <- curr_event_df[,"Event3_Flag"]
    
    if (curr_3rd_event_flag == "Other Cancer"){ # diag dates - 3month
      curr_diag_date <- curr_event_df[,"Date_3rd_Event"]
      curr_end <- mdy(curr_diag_date) - days(3*30) 

    }else if (curr_3rd_event_flag == "Primary BC" | curr_3rd_event_flag == "1Recur"){ #diag dates - 1month
      curr_diag_date <- curr_event_df[,"Date_3rd_Event"]
      curr_end <- mdy(curr_diag_date) - days(1*30) 

    }else{ #use all valid claims
      curr_end <- last_enroll_mon
    }
    
    valid_month_df[i,"study_id"] <- curr_id
    valid_month_df[i,"Valid_Start"] <- as.character(curr_start)
    valid_month_df[i,"Valid_End"] <- as.character(curr_end)
    
}

#Now it is possible the event has no dates recorded
exclude_idx <- which(is.na(valid_month_df$Valid_Start)==T | is.na(valid_month_df$Valid_End)==T)
valid_month_df <- valid_month_df[-exclude_idx,]

write.xlsx(valid_month_df,paste0(outdir,"valid_month_df.xlsx"))

#'@TODO
#########################################################################################################
#V. Remove patients does not qualify the following: 
#'@Question1: and 3 months of claims before or after the SBCE  ( have claims at least 3 months  before SBCE or after))
#or any 6 months of claims for non-SBCE patients (At least 6 month data avaiable)
##'TODO.@Question2: ?Filter the claims table for patients with local or regional stage, 
#########################################################################################################
# analysis_ids <- unique(intersect(valid_month_df$study_id, All_event_df$ID))
# length(analysis_ids) #29558
# 
# ct <- 1
# ID_to_remove <- NA
# for (i in 1:length(analysis_ids)){
#   i <- 1
#   if (i %% 100 == 0){
#     print(i)
#   }
#   curr_id <- analysis_ids[i]
#   
#   #event data
#   curr_event_df <- All_event_df[which(All_event_df[,"ID"] == curr_id),]
#   
#   #Valid month data
#   curr_idxes_in_valid_mondf <- which(valid_month_df[,"study_id"] == curr_id)
#   curr_valid_month_df <- valid_month_df[curr_idxes_in_valid_mondf, ]
#   
#   #check if patient has SBCE
#   curr_SBCE_status <- curr_event_df$SBCE
#   if (curr_SBCE_status == 0){ #if no recurrent
#     #if not more than 6 month valid enrollment, exclude ID
#     if (nrow(curr_valid_month_df) < 6){
#       ID_to_remove[ct] <- curr_id
#       ct <- ct+1
#     }
#   }else{ #if recurrent 
#     curr_SBCE_date <- mdy(curr_event_df[,"Date_2nd_Event"])
#     curr_SBCE_minus3Mon <- curr_SBCE_date - months(3)
#     curr_SBCE_plus3Mon <- curr_SBCE_date + months(3)
#     #check if this patient has valid month before curr_SBCE_minus3Mon or after curr_SBCE_plus3Mon
#     n_month_before <- length(which(ymd(curr_valid_month_df[,"Valid_Month"]) < curr_SBCE_minus3Mon))
#     n_month_after <-  length(which(ymd(curr_valid_month_df[,"Valid_Month"]) > curr_SBCE_plus3Mon))
#     if (n_month_before == 0 & n_month_after == 0){ #if no claims before or after, exclude this ID
#       ID_to_remove[ct] <- curr_id
#       ct <- ct+1
#     }
#     
#   }
# }
# 
# updated_valid_month_df <- all_valid_month_df[-which(all_valid_month_df$ID %in% ID_to_remove), ]
# 
# write.csv(updated_valid_month_df,paste0(out_dir,"All_Final_Valid_month.csv"),row.names = F)
