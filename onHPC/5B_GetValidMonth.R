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
All_event_df <- read.xlsx(paste0(outdir,"4_updated_All_event_df.xlsx"),sheet = 1)
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


write.xlsx(valid_month_df,paste0(outdir,"5_valid_month_df.xlsx"))

