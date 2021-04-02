library(stringr)


remove_NA_func<- function(curr_day_codes){
  na_idxes <- which(curr_day_codes == "" | is.na(curr_day_codes) == TRUE)
  if (length(na_idxes) > 0){
    curr_day_codes <- curr_day_codes[-na_idxes]
  }
  return(curr_day_codes)
}

#This scrpte generaete patient per day table,
#'@NOTE: pateint may cross different file, all generated files need to combined later 
#'for get each patients data together

data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/Medicare_Claims_seperated/"
out_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/Medicare_PateintPerDay_table/"

all_files <- list.files(data_dir)
for (f in 1:length(all_files)){
    curr_file <- all_files[f]
    file_idx <- str_match(curr_file, "Medicare_Claims_\\s*(.*?)\\s*.csv")[2]
    file_name <- paste0(data_dir, paste0("Medicare_Claims_",file_idx,".csv"))
    
    diag_cols <- paste0("DGNS_CD",seq(1,25))
    proc_cols <- c(paste0("PRCDRCD",seq(1,25)),"HCPCS_CD")
    drug_cols <- "NDC_CD"
    date_col <- "claims_date"
    
    #read a few line to get colnames
    all_df <- read.csv(file_name, stringsAsFactors = F, header=TRUE)
    colnames(all_df)
    analysis_ids <- unique(all_df[,"study_id"])
    
    #Get patient per day table 
    per_day_table_list <- list()
    for (i in 1:length(analysis_ids)){
      if (i %% 100 == 0){
        print(i)
      }
      curr_id <- analysis_ids[i]
      curr_df <- all_df[which(all_df[,"study_id"] == curr_id),]
      curr_alldates <- unique(curr_df[,"claims_date"])
      
      per_day_table <- as.data.frame(matrix(NA, nrow = length(curr_alldates),ncol = 5))
      colnames(per_day_table) <- c("study_id","claims_date","Diag_Codes","Proc_Codes","Drug_Codes")
      for (j in 1:length(curr_alldates)){
        per_day_table[,"study_id"] <- curr_id
        per_day_table[,"claims_date"] <- curr_alldates
        
        curr_date <- curr_alldates[j]
        curr_day_idxes <- which(curr_df[,"claims_date"] == curr_date)
        
        curr_day_diag <- unique(unlist(curr_df[curr_day_idxes,diag_cols]))
        curr_day_proc <- unique(unlist(curr_df[curr_day_idxes,proc_cols]))
        curr_day_drug <- unique(unlist(curr_df[curr_day_idxes,drug_cols]))
        
        #remove all blanks and NAs
        curr_day_diag <- remove_NA_func(curr_day_diag)
        curr_day_proc <- remove_NA_func(curr_day_proc)
        curr_day_drug <- remove_NA_func(curr_day_drug)
    
        if (length(curr_day_diag) > 0){
          per_day_table[j,"Diag_Codes"] <- paste0(curr_day_diag,collapse = "$$$$")
        }
        if (length(curr_day_proc) > 0){
          per_day_table[j,"Proc_Codes"] <- paste0(curr_day_proc,collapse = "$$$$")
        }
        if (length(curr_day_drug) > 0){
          per_day_table[j,"Drug_Codes"] <- paste0(curr_day_drug,collapse = "$$$$")
        }
        
        
      }
      
      #remove the dates that does not have any type of codes
      na_ct_perDay <- rowSums(is.na(per_day_table[,c("Diag_Codes","Proc_Codes","Drug_Codes")]))
      per_day_table <- per_day_table[-which(na_ct_perDay == 3),]
      
      per_day_table_list[[i]] <- per_day_table
}
    
    perFile_Pts_perDayTb <- do.call(rbind,per_day_table_list)
    write.csv(perFile_Pts_perDayTb,paste0(out_dir,"table",f,".csv"))
}



#'@TODO: Read all generated table and combine them together so that one patient did not have data across files.