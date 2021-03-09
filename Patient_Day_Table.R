library(lubridate)

get_codes_per_day_func <- function(curr_day_df, column_names){
  curr_diag <- curr_day_df[,column_names]
  curr_unique_diag<- unique(as.vector(unlist(curr_diag)))
  empty_idxes <- which(curr_unique_diag == "")
  if (length(empty_idxes) > 0 ){
    curr_unique_diag <- curr_unique_diag[-empty_idxes] #remove "" codes
  }
  return(curr_unique_diag)
}


data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/"
intermediate_data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/"

out_dir <- paste0(intermediate_data_dir,"/0225_21/")

#date df 
date_df <- read.csv(paste0(intermediate_data_dir,"0225_21/primary_date_df.csv"),stringsAsFactors = F)

#Medicaid
medicaid <- read.csv(paste0(intermediate_data_dir,"0225_21/Filtered_Medicaid_df.csv"),stringsAsFactors = F)
medicaid_IDs <- unique(medicaid$study_id)



#get N days avaialble in claims data
N_days_avaiable <- NA
for (p in 1:length(medicaid_IDs)){
  if(p %% 100 == 0){
  print(p)
  }
  #claim data
  curr_id <- medicaid_IDs[p]
  curr_df <- medicaid[which(medicaid[,"study_id"] == curr_id),]
  
  #date data
  curr_date_df <- date_df[which(date_df[,"ID"] == curr_id),]
  curr_1st_date <-  mdy(curr_date_df[,"first_primary_date"]) 
  curr_2nd_date <-  mdy(curr_date_df[,"Second_primary_date"])
  
  curr_6mon_after_1st_date  <- curr_1st_date + days(30*6)
  start_date <- curr_6mon_after_1st_date
  
  if (is.na(curr_2nd_date) == F){ #If it has 2nd date
    curr_3mon_before_1st_date <- curr_2nd_date - days(30*3)
    end_date <- curr_3mon_before_1st_date
  }else{
    end_date <- max(mdy(curr_df[,"DTE_FIRST_SVC"]))
  }
  
  if (ymd(start_date) < ymd(end_date)){
     days_avaiable <- seq(start_date,end_date, by = "1 day")
     N_days_avaiable[p] <- length(days_avaiable)
  }else{
    N_days_avaiable[p] <- NA
  }
}




#filter out start date > end date (The diagnoise of 1st is later than the claim data)
na_idxes <- which(is.na(N_days_avaiable) == T) #1490
updated_medicaid_IDs <- medicaid_IDs[-na_idxes] #9821
max_dayavailable <- max(N_days_avaiable,na.rm = T) #7115 days, 19 years

#'@NOTE: Some patients only has claim data after end date(the 2nd date)
patient_day_allcode_tb_list <- list(NA)
for (p in 2408:length(updated_medicaid_IDs)){
      if(p %% 100 == 0){
        print(p)
      }
      #claim data
      curr_id <- updated_medicaid_IDs[p]
      curr_df <- medicaid[which(medicaid[,"study_id"] == curr_id),]
      
      #date data
      curr_date_df <- date_df[which(date_df[,"ID"] == curr_id),]
      curr_1st_date <-  mdy(curr_date_df[,"first_primary_date"]) 
      curr_2nd_date <-  mdy(curr_date_df[,"Second_primary_date"])
      
      curr_6mon_after_1st_date  <- curr_1st_date + days(30*6)
      start_date <- curr_6mon_after_1st_date
      
      if (is.na(curr_2nd_date) == F){ #If it has 2nd date
        curr_3mon_before_1st_date <- curr_2nd_date - days(30*3)
        end_date <- curr_3mon_before_1st_date
      }else{
        end_date <- max(mdy(curr_df[,"DTE_FIRST_SVC"]))
      }
      
      #Get claims data in timewindow
      curr_df_inTW <- curr_df[which(mdy(curr_df[,"DTE_FIRST_SVC"]) >= start_date 
                                    & mdy(curr_df[,"DTE_FIRST_SVC"]) <= end_date), ]
      if (nrow(curr_df_inTW) > 0){
      #Compute services time - start date
      time_fromstart <- difftime(mdy(curr_df_inTW[,"DTE_FIRST_SVC"]),start_date,units = "days") + 1
      sorted_uniquetime_fromstart <- sort(unique(time_fromstart),decreasing = F)
      curr_df_inTW$Days_FromStart <- time_fromstart
      
      curr_pt_perday_table_list <- list(NA) 
      #Get per day diag codes
      for (d in 1:length(sorted_uniquetime_fromstart)){
        curr_day <- sorted_uniquetime_fromstart[d]
        curr_day_df <- curr_df_inTW[which(curr_df_inTW[,"Days_FromStart"] == curr_day),]
      
        #Diag codes
        diag_col <- c("CDE_DIAG_PRIM", "CDE_DIAG_2","CDE_DIAG_3","CDE_DIAG_4")
        curr_unique_diag <- get_codes_per_day_func(curr_day_df,diag_col)
        pro_col <- c("CDE_PROC_PRIM")
        curr_unique_proc <- get_codes_per_day_func(curr_day_df,pro_col)
        
        curr_day_diag_df <- cbind.data.frame(curr_unique_diag,rep("Diagnostic",length(curr_unique_diag)),rep(curr_day,length(curr_unique_diag)))
        colnames(curr_day_diag_df) <- c("Code","Type","Day")
        curr_day_pro_df <- cbind.data.frame(curr_unique_proc,rep("Procedure",length(curr_unique_proc)),rep(curr_day,length(curr_unique_proc)))
        colnames(curr_day_pro_df) <- c("Code","Type","Day")
        curr_day_allcodes_df <- rbind(curr_day_diag_df,curr_day_pro_df)
        if(nrow(curr_day_allcodes_df) > 0){ #patient might have CDE code , but no proc and diag
          curr_day_allcodes_df$study_id <- curr_id
          curr_pt_perday_table_list[[d]] <- curr_day_allcodes_df
        }
      }
      
      curr_pt_perday_table <- do.call(rbind,curr_pt_perday_table_list)
      patient_day_allcode_tb_list[[p]] <- curr_pt_perday_table
      write.csv(curr_pt_perday_table,paste0(out_dir,"Patinet_UniqueCodePerDay_Table/","ID",curr_id,".csv"),row.names = F)
  }
}

#TODO:
#Count unique code per day for each patients.
#For each patient, for each code, count # of times apperas in each month