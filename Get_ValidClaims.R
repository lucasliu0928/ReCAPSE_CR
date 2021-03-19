library(lubridate)
out_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0318_21/"
data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/"

All_event_df <- read.csv(paste0(out_dir,"updated_All_event_df.csv"),stringsAsFactors = F)
analysis_IDs <- unique(All_event_df$ID)


#get Medicaid enrollment
enrollment_df <- read.csv(paste0(data_dir,"kcr_medicaid_enroll_fb0015.csv"),stringsAsFactors = F)
#remove all NA rows
enrollment_df <- enrollment_df[complete.cases(enrollment_df[ , 3:ncol(enrollment_df)]),]


#Convert column names from mon1-mon240 to (1/2000-12/2019)
start_date <- my("1/2000")
end_date <- my("12/2019")
dates_seq <- seq(start_date,end_date,by = "1 mon")
colnames(enrollment_df)[3:ncol(enrollment_df)] <- as.character(dates_seq)

#filter out the Ids not in all_event_df 
updated_IDs <- unique(intersect(enrollment_df[,"study_id"],All_event_df[,"ID"]))
updated_enrollment_df <- enrollment_df[which(enrollment_df[,"study_id"] %in% updated_IDs), ] #12287
updated_All_event_df <- All_event_df[which(All_event_df[,"ID"] %in% updated_IDs), ]

#find valid month for each pts
options(warn=2) 
valid_months_list <- list(NA)
for (i in 1:length(updated_IDs)){
  curr_id <- updated_IDs[i]
  #########curr enroll
  curr_enroll_df <- updated_enrollment_df[which(updated_enrollment_df[,"study_id"] == curr_id),]
  curr_enroll_df <- curr_enroll_df[,-which(colnames(curr_enroll_df) %in% c("study_id","Id_medicaid"))] #drop ID
  
  curr_valid_months <- colnames(curr_enroll_df)[which(curr_enroll_df==1)]
  
  if (length(curr_valid_months) > 0){ #if there are valid enrollemnts
      sorted_valid_months <- sort(ymd(curr_valid_months))
      last_valid_mon <- max(sorted_valid_months)
      
      #########curr event df
      curr_event_df <- updated_All_event_df[which(updated_All_event_df[,"ID"] == curr_id),]
      curr_1stPBC_date <- curr_event_df[,"Date_1st_Event"]
      
      #Start point
      curr_start <- mdy(curr_1stPBC_date) + months(6)
      
      ##End point
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
      
      #Valid claims between start and end
      valid_months_idxes <- which(ymd(sorted_valid_months) >= curr_start & ymd(sorted_valid_months) <= curr_end )
      n_valid_months <- length(sorted_valid_months[valid_months_idxes])
      valid_month_df <- cbind.data.frame(rep(curr_id,n_valid_months),sorted_valid_months[valid_months_idxes])
      colnames(valid_month_df) <- c("ID","Valid_Month")
      valid_months_list[[i]] <- valid_month_df
  }else{
    valid_months_list[[i]] <- NULL
  }

}

all_valid_month_df <- do.call(rbind,valid_months_list)
write.csv(all_valid_month_df,paste0(out_dir,"all_valid_month_medicaid.csv"),row.names = F)

