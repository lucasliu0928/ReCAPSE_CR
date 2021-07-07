library(lubridate)
library(dplyr)

#onHPC
data_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
outdir <- "/recapse/intermediate_data/"


#local
data_dir <- "/Volumes/LJL_ExtPro/Data/Testing data for UH3 - Dec 16 2020/"
outdir <- "/Users/lucasliu/Desktop/intermediate_data/"

#########################################################################################################
#1.Load pateint event type and date data
#########################################################################################################
All_event_df <- read.csv(paste0(outdir,"4_updated_All_event_df.csv"),stringsAsFactors = F)
#1.Get number of IDs in event type files
event_type_IDs <- unique(All_event_df$study_id) #39387

#########################################################################################################
#2. Load enrollment month
#########################################################################################################
enrollment_start_end_df <- read.xlsx(paste0(outdir,"5_enrollment_start_end_df.xlsx"),sheet = 1)
enrolmment_IDs <- unique(enrollment_start_end_df$study_id) #37149

#########################################################################################################
#3. only process Ids in both event type data and enrollemnt 
#########################################################################################################
analysis_Ids <- unique(intersect(enrolmment_IDs,event_type_IDs)) #30456
analysis_Ids <- sort(analysis_Ids)

#########################################################################################################
#IV Truncate valid claim month for the duration of predefined starting and ending point
#1. Start point : the diagnosis date of the first primary breast cancer + 6 months
#2. End point:  2.1 without an SBCE :  end of recorded claims
#               2.2 with an SBCE, but no third event: end of recorded claims
#               2.3 with an SBCE, 
#                   the subsequent cancer is a non-breast primary cancer: 3 months before the registry-based diagnosis date or, 
#                   the subsequent is a breast cancer event (Recurrence or diagnose of breast cancer):  1 month before the first subsequent breast cancer event.

#########################################################################################################
valid_month_df <- as.data.frame(matrix(NA, nrow = length(analysis_Ids), ncol = 6))
colnames(valid_month_df) <- c("study_id" , "Valid_Start","Valid_End","First_Primary_Start","Date_3rd_Event","Enrollment_End")

for (i in 1:length(analysis_Ids)){ 
    if (i %% 1000 == 0){
      print(i)
    }
    
    curr_id <- analysis_Ids[i]
    
    #current enrollment
    curr_enroll_df <- enrollment_start_end_df[which(enrollment_start_end_df[,"study_id"] == curr_id),]
    last_enroll_mon <- curr_enroll_df[,"Enroll_End"]

    #########curr event df
    curr_event_df <- All_event_df[which(All_event_df[,"study_id"] == curr_id),]
    curr_1stPBC_date <- mdy(curr_event_df[,"Date_1st_Event"])
    curr_3rd_event_date <- mdy(curr_event_df[,"Date_3rd_Event"])
    
    #Start point
    curr_start <- curr_1stPBC_date + days(6*30)  #in the case of + 6months returns NA
    
    #End point
    #1. Check if patient has SBCE
    curr_SBCE_flag <- curr_event_df[,"SBCE"]
    if (curr_SBCE_flag == 0){ #without an SBCE :  end of recorded claims
      curr_end <- last_enroll_mon
    }else {
      #2. check if pt has the 3rd event
      if (is.na(curr_3rd_event_date) == T){ #no third event: end of recorded claims
        curr_end <- last_enroll_mon
      }else{
        #3. Check type of 3rd event
        curr_3rd_event_type <- curr_event_df[,"Type_3rd_Event"]
        #3rd event is a breast cancer event (Recurrence or diagnose of breast cancer):  1 month before the first subsequent breast cancer event.
        if (grepl("Primary|1Recur",curr_3rd_event_type)==T){ #due to merging effect, as long as it contains primary/1Recur, it counts, it is possible that type = "Priamry$$$Other"
          curr_end <- curr_3rd_event_date - days(1*30) 
          
        }else if (curr_3rd_event_type == "Other"){# due to merging effect, consider exact match in this case
          #3rd event is a non-breast primary cancer: 3 months before the registry-based diagnosis date or, 
          curr_end <- curr_3rd_event_date - days(3*30) 
          
        }
  
        
      }
      
    }


    
    valid_month_df[i,"study_id"] <- curr_id
    valid_month_df[i,"Valid_Start"] <- as.character(curr_start)
    valid_month_df[i,"Valid_End"] <- as.character(curr_end)
    valid_month_df[i,"First_Primary_Start"] <- as.character(curr_1stPBC_date)
    valid_month_df[i,"Date_3rd_Event"] <- as.character(curr_3rd_event_date)
    valid_month_df[i,"Enrollment_End"] <- as.character(last_enroll_mon)
    
    
}

check_IDs <- All_event_df[which(All_event_df$SBCE == 1 & All_event_df$Type_3rd_Event == "Other"),"study_id"]

check_df <- valid_month_df[which(valid_month_df$study_id %in% check_IDs),]

write.xlsx(valid_month_df,paste0(outdir,"5_valid_month_df.xlsx"))

#'@TODO
#########################################################################################################
#V. Remove patients does not qualify the following: 
#'@Question1: 3 months of claims before or after the SBCE  ( have claims at least 3 months  before SBCE or after))
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
