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

#1.find valid month for each pts
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

#'2.@Question1: ?Filter the claims table for patients with local or regional stage, 
#'@Question2: and 3 months of claims before or after the SBCE  
#or any 6 months of claims for non-SBCE patients (At least 6 month data avaiable)
#2.1 First load all event date file
event_data_dir <- out_dir
event_date_df <- read.csv(paste0(event_data_dir,"updated_All_event_df.csv"),stringsAsFactors = F)

#2.2 Load valid month df
all_valid_month_df <- read.csv(paste0(event_data_dir,"all_valid_month_medicaid.csv"),stringsAsFactors = F)

#2.3 Find IDs that have both valid month data and event data
analysis_ids <- unique(intersect(all_valid_month_df$ID, event_date_df$ID))

updated_valid_month_df <- all_valid_month_df #initlize, so that we can remove unqualified IDs
removed_id <- NA
ct <- 1
for (i in 1:length(analysis_ids)){
  curr_id <- analysis_ids[i]
  #event data
  curr_event_df <- event_date_df[which(event_date_df[,"ID"] == curr_id),]
  #Valid month data
  curr_idxes_in_valid_mondf <- which(updated_valid_month_df[,"ID"] == curr_id)
  curr_valid_month_df <- updated_valid_month_df[curr_idxes_in_valid_mondf, ]
  #check if patient has SBCE
  curr_SBCE_status <- curr_event_df$SBCE
  if (curr_SBCE_status == 0){ #if no recurrent
    #if not more than 6 month valid enrollment, exclude ID
    if (nrow(curr_valid_month_df) < 6){
      updated_valid_month_df <- updated_valid_month_df[-curr_idxes_in_valid_mondf,]
      removed_id[ct] <- curr_id
      ct <- ct+1
    }
  }else{ #if recurrent (at least 3 months of claims before or after the SBCE)
    #Exclude: 3 month - SBCE  <  M < 3 month + SBCE
    curr_SBCE_date <- mdy(curr_event_df$Date_2nd_Event)
    curr_idxes<- which(ymd(curr_valid_month_df[,"Valid_Month"]) > curr_SBCE_date - months(3) & 
                           ymd(curr_valid_month_df[,"Valid_Month"]) < curr_SBCE_date + months(3))
    month_toexclude <- curr_valid_month_df[,"Valid_Month"][curr_idxes]
    if (length(month_toexclude) > 0){
      #remove these monthes from the valid month for all patients
      indx_toexclude<- which(updated_valid_month_df[,"ID"] == curr_id & 
                              updated_valid_month_df[,"Valid_Month"] %in% month_toexclude)
      updated_valid_month_df <- updated_valid_month_df[-indx_toexclude,]
    }
  }
}
write.csv(updated_valid_month_df,paste0(out_dir,"Filtered_all_valid_month_medicaid.csv"),row.names = F)

  