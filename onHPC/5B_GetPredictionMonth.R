source("Recapse_Ultility.R")

#onHPC
data_dir <- "/recapse/intermediate_data/4_RecurrDates_Outcome_Info/"
outdir <- "/recapse/intermediate_data/5_Enrollment_And_Prediction_Months/"


#local
data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/4_RecurrDates_Outcome_Info/"
outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/5_Enrollment_And_Prediction_Months/"

#########################################################################################################
#1.Load pateint event type and date data
#########################################################################################################
All_event_df <- read.xlsx(paste0(data_dir,"4_All_event_df.xlsx"),sheet = 1)
#1.Get number of IDs in event type files
event_type_IDs <- unique(All_event_df$study_id) # 40324

#########################################################################################################
#2. Load enrollment month
#########################################################################################################
enrollment_df <- read.xlsx(paste0(outdir,"5_enrollment_Months.xlsx"),sheet = 1)
enrolmment_IDs <- unique(enrollment_df$study_id) #37149

#########################################################################################################
#3. only process Ids in both event type data and enrollemnt 
#########################################################################################################
analysis_Ids <- unique(intersect(enrolmment_IDs,event_type_IDs)) #31262
analysis_Ids <- sort(analysis_Ids)

#########################################################################################################
#IV Get the prediction window as the predefined starting and ending point
#1. Start point : the diagnosis date of the first primary breast cancer + 6 months
#2. End point:  2.1 without an SBCE :  end of enrollment
#               2.2 with an SBCE, but no third event: end of enrollment
#               2.3 with an SBCE, 
#                   the 1st subsequent event is a non-breast primary cancer: 3 months before the registry-based diagnosis date or, 
#                   the 1st subsequent event is a breast cancer event:
#                   (Recurrence or diagnose of breast cancer or 1st primary BC death):  1 month before the subsequent event.

#########################################################################################################
prediction_month_df <- as.data.frame(matrix(NA, nrow = length(analysis_Ids), ncol = 7))
colnames(prediction_month_df) <- c("study_id" , "Prediction_Start","Prediction_End","First_Primary_Start","Date_3rd_Event","Enrollment_Start","Enrollment_End")

for (i in 1:length(analysis_Ids)){ 
    if (i %% 1000 == 0){
      print(i)
    }
    
    curr_id <- analysis_Ids[i]

    #current enrollment
    curr_enroll_df <- enrollment_df[which(enrollment_df[,"study_id"] == curr_id),]
    curr_enroll_months <- curr_enroll_df[,"Enrolled_Month"]
    last_enroll_mon  <- max(ymd(curr_enroll_months))
    first_enroll_mon <- min(ymd(curr_enroll_months))
    #########curr event df
    curr_event_df       <- All_event_df[which(All_event_df[,"study_id"] == curr_id),]
    curr_1stPBC_date    <- mdy(curr_event_df[,"Date_1st_Event"])
    curr_3rd_event_date <- mdy(curr_event_df[,"Date_3rd_Event"])
    
    #Start point
    curr_start <- curr_1stPBC_date + days(6*30)  #in the case of + 6months returns NA
    
    #End point
    if (is.na(curr_event_df[,"Type_2nd_Event"]) == T){ #if no 2nd event :  end of recorded claims
        curr_end <- last_enroll_mon
    }else {#if With 2nd event
      if (is.na(curr_3rd_event_date) == T){ #if no third event: end of recorded claims
        curr_end <- last_enroll_mon
      }else{#if with 3rd event 
        curr_3rd_event_type <- curr_event_df[,"Type_3rd_Event"] #get 3rd event type
        #3rd event is a breast cancer event (Recurrence or diagnose of breast cancer or death):  1 month before the first subsequent breast cancer event.
        if (grepl("Primary|1Recur|Death",curr_3rd_event_type)==T){ #due to merging effect, as long as it contains primary/1Recur, it counts, it is possible that type = "Priamry$$$Other"
          curr_end <- curr_3rd_event_date - days(1*30) 
          
        }else if (curr_3rd_event_type == "Other"){# due to merging effect, consider exact match in this case (Only has other)
          #3rd event is a non-breast primary cancer: 3 months before the registry-based diagnosis date or, 
          curr_end <- curr_3rd_event_date - days(3*30) 
          
        }
  
        
      }
      
    }


    
    prediction_month_df[i,"study_id"] <- curr_id
    prediction_month_df[i,"Prediction_Start"] <- as.character(curr_start)
    prediction_month_df[i,"Prediction_End"] <- as.character(curr_end)
    prediction_month_df[i,"First_Primary_Start"] <- as.character(curr_1stPBC_date)
    prediction_month_df[i,"Date_3rd_Event"] <- as.character(curr_3rd_event_date)
    prediction_month_df[i,"Enrollment_Start"] <- as.character(first_enroll_mon)
    prediction_month_df[i,"Enrollment_End"]   <- as.character(last_enroll_mon)
    
}


#########################################################################################################
#1.Exclude patient has enrollment end before prediction start
#########################################################################################################
exclude_idxes1 <- which(ymd(prediction_month_df[,"Enrollment_End"]) <= ymd(prediction_month_df[,"Prediction_Start"]))
updated_prediction_month_df <- prediction_month_df[-exclude_idxes1,] #-1105

#########################################################################################################
#2.Exclude patient has enrollment start after prediction end
#########################################################################################################
exclude_idxes2 <- which(ymd(updated_prediction_month_df[,"Enrollment_Start"]) >= ymd(updated_prediction_month_df[,"Prediction_End"]))
updated_prediction_month_df <- updated_prediction_month_df[-exclude_idxes2,] #-134

#########################################################################################################
#3.Exclude patient has prediction end before prediction start (because it is 1st bc date + 6 month is after 3rd event -3 month)
#########################################################################################################
updated_prediction_month_df$Prediction_Duration_inDays <- difftime(ymd(updated_prediction_month_df[,"Prediction_End"]) , 
                                                                   ymd(updated_prediction_month_df[,"Prediction_Start"]), units = "days")
#Exclude prediction duration < 0 days
exclude_indxes <- which(updated_prediction_month_df$Prediction_Duration_inDays <= 0 )
updated_prediction_month_df <- updated_prediction_month_df[-exclude_indxes,]

#Exclude prediction duration < 180 days (6 month) #(cuz Later will further exclude no claims within 3month before and after SBCE or 6 month for no SBCE patient)
exclude_indxes <- which(updated_prediction_month_df[,"Prediction_Duration_inDays"] < 180)
updated_prediction_month_df <- updated_prediction_month_df[-exclude_indxes,]

#########################################################################################################
#Update: if prediction start before enrollment start, then prediction start = enrollment start
#########################################################################################################
updated_indexes1 <- which(ymd(updated_prediction_month_df[,"Prediction_Start"]) < 
                          ymd(updated_prediction_month_df[,"Enrollment_Start"]))
updated_prediction_month_df[updated_indexes1,"Prediction_Start"] <- updated_prediction_month_df[updated_indexes1,"Enrollment_Start"]

#########################################################################################################
#Update: if prediction end after enrollment end, then prediction end = enrollment end
#########################################################################################################
updated_indexes2 <- which(ymd(updated_prediction_month_df[,"Prediction_End"]) > 
                          ymd(updated_prediction_month_df[,"Enrollment_End"]))
updated_prediction_month_df[updated_indexes2,"Prediction_End"] <- updated_prediction_month_df[updated_indexes2,"Enrollment_End"]


write.xlsx(updated_prediction_month_df,paste0(outdir,"5_prediction_month_df.xlsx"))

