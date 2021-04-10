source("/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Code/For_Both_Data/Ultilities.R")

get_code_func <-function(curr_perday_df,record_date_col,typeofcode_cols,curr_start,curr_end){
  # record_date_col <- "claims_date"
  # curr_start <- curr_mon_start
  # curr_end <-curr_mon_end
  #typeofcode_cols <- "Proc_Codes"    # "Proc_Codes","Drug_Codes"
  
  curr_interval_index <- which(curr_perday_df[,record_date_col] >= curr_start &
                                 curr_perday_df[,record_date_col]  < curr_end  )
  
  if (length(curr_interval_index) > 0){ #if any claims in the time window
    curr_interval_clamins <- curr_perday_df[curr_interval_index,]
    curr_codes <- as.character(unlist(curr_interval_clamins[,typeofcode_cols]))
    if(length(curr_codes) >0){ #if any codes
      curr_codes <- remove_NA_func(curr_codes)
      curr_codes <- unlist(strsplit(curr_codes,split = "$$$$",fixed = T))
    }else{
      curr_codes <- NA
    }
    
  }else{
    curr_codes <- NA
  }
  return(curr_codes)
}


create_perMon_count_table <- function(curr_id,n_valid_month,curr_enroll_months,curr_perday_df){
  
  #Diag table
  code_to_analysis_diag <- paste0("Diag_",as.vector(unique_Diag_codes[,"unique_Diag_codes"]))
  code_to_analysis_proc <- paste0("Proc_",as.vector(unique_Proc_codes[,"unique_Proc_codes"]))
  code_to_analysis_drug <- paste0("Drug_",as.vector(unique_Drug_codes[,"unique_Drug_codes"]))
  
  curr_pt_month_table_diag <- as.data.frame(matrix(0, nrow = n_valid_month[i],ncol = length(code_to_analysis_diag) + 2))
  colnames(curr_pt_month_table_diag) <- c("ID","Month_Start_Date",code_to_analysis_diag)
  curr_pt_month_table_diag$ID <- paste0("OriginalID_",curr_id,"_Mon",seq(1,n_valid_month[i]))
  
  curr_pt_month_table_proc <- as.data.frame(matrix(0, nrow = n_valid_month[i],ncol = length(code_to_analysis_proc) + 2))
  colnames(curr_pt_month_table_proc) <- c("ID","Month_Start_Date",code_to_analysis_proc)
  curr_pt_month_table_proc$ID <- paste0("OriginalID_",curr_id,"_Mon",seq(1,n_valid_month[i]))
  
  curr_pt_month_table_drug <- as.data.frame(matrix(0, nrow = n_valid_month[i],ncol = length(code_to_analysis_drug) + 2))
  colnames(curr_pt_month_table_drug) <- c("ID","Month_Start_Date",code_to_analysis_drug)
  curr_pt_month_table_drug$ID <- paste0("OriginalID_",curr_id,"_Mon",seq(1,n_valid_month[i]))
  
  for (j in 1:length(curr_enroll_months)){
    curr_mon_start <- ymd(curr_enroll_months[j]) 
    curr_mon_end <- curr_mon_start + months(1)
    
    #getunique codes in this interval
    curr_code_list_diag <- get_code_func(curr_perday_df,'claims_date',"Diag_Codes",curr_mon_start,curr_mon_end)
    curr_code_list_proc <- get_code_func(curr_perday_df,'claims_date',"Proc_Codes",curr_mon_start,curr_mon_end)
    curr_code_list_drug <- get_code_func(curr_perday_df,'claims_date',"Drug_Codes",curr_mon_start,curr_mon_end)
    
    
    #count each code in interval
    curr_tb <- table(curr_code_list_diag)
    curr_col_indexes <- which(colnames(curr_pt_month_table_diag) %in% paste0("Diag_",names(curr_tb)))
    curr_pt_month_table_diag[j,curr_col_indexes] <- as.numeric(curr_tb)
    
    #count each code in interval
    curr_tb <- table(curr_code_list_proc)
    curr_col_indexes <- which(colnames(curr_pt_month_table_proc) %in% paste0("Proc_",names(curr_tb)))
    curr_pt_month_table_proc[j,curr_col_indexes] <- as.numeric(curr_tb)
    
    #count each code in interval
    curr_tb <- table(curr_code_list_drug)
    curr_col_indexes <- which(colnames(curr_pt_month_table_drug) %in% paste0("Drug_",names(curr_tb)))
    curr_pt_month_table_drug[j,curr_col_indexes] <- as.numeric(curr_tb)
    
    
    curr_pt_month_table_diag[j,"Month_Start_Date"] <- as.character(curr_mon_start)
    curr_pt_month_table_proc[j,"Month_Start_Date"] <- as.character(curr_mon_start)
    curr_pt_month_table_drug[j,"Month_Start_Date"] <- as.character(curr_mon_start)
    
  }
  
  return(list(curr_pt_month_table_diag,curr_pt_month_table_proc,curr_pt_month_table_drug))
}



proj_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/"
data_file1 <- paste0(proj_dir,"/ReCAPSE_Intermediate_Data/0318_21/Medicaid_intermediate_Data/Medicaid_PateintPerDay_Data/All_PerDay_Data_Medicaid.csv")
data_file2 <- paste0(proj_dir,"ReCAPSE_Intermediate_Data/0318_21/Medicare_Intermediate_Data/Medicare_PateintPerDay_Data/All_PerDay_Data_Medicare.csv")
outdir <- paste0(proj_dir,"/ReCAPSE_Intermediate_Data/0318_21/For_Both_Data/")

################################################################################ 
############################## Read  per day data ############################## 
################################################################################ 
Medicaid_perdaydf <- read.csv(data_file1,stringsAsFactors = F)
Medicare_perdday_df <- read.csv(data_file2,stringsAsFactors = F)
Comb_perday_df <- rbind(Medicaid_perdaydf,Medicare_perdday_df)
length(unique(Comb_perday_df[,"study_id"])) #34794

idexes_toupdate <- which(grepl("/",Comb_perday_df[,"claims_date"])==T)
Comb_perday_df[idexes_toupdate,"claims_date"] <- as.character(mdy(Comb_perday_df[idexes_toupdate,"claims_date"]))

idexes_toupdate2 <- which(grepl("-",Comb_perday_df[,"claims_date"])==T)
Comb_perday_df[idexes_toupdate2,"claims_date"] <- as.character(ymd(Comb_perday_df[idexes_toupdate2,"claims_date"]))

write.csv(Comb_perday_df,paste0(outdir,"Comb_PerDay_data.csv"))

################################################################################ 
#####Get ALl unique diag,procedure, drug codes
################################################################################ 
unique_Diag_Codes_list <- list()
unique_Proc_Codes_list <- list()
unique_Drug_Codes_list <- list()
for (i in 1:nrow(Comb_perday_df)){
  if (i %% 100000==0){ print(i)}
  
  unique_Diag_Codes_list[[i]] <- unlist(strsplit(Comb_perday_df[i,"Diag_Codes"],split = "$$$$",fixed = T))
  unique_Proc_Codes_list[[i]] <- unlist(strsplit(Comb_perday_df[i,"Proc_Codes"],split = "$$$$",fixed = T))
  unique_Drug_Codes_list[[i]] <- unlist(strsplit(Comb_perday_df[i,"Drug_Codes"],split = "$$$$",fixed = T))
}

unique_Diag_codes <- unique(unlist(unique_Diag_Codes_list)) #30047 diag codes
unique_Proc_codes <- unique(unlist(unique_Proc_Codes_list)) #17737 procedure codes
unique_Drug_codes <- unique(unlist(unique_Drug_Codes_list)) #33393

#Remove NA
na_idxes <- which(is.na(unique_Diag_codes)==T)
unique_Diag_codes<- unique_Diag_codes[-na_idxes]

na_idxes <- which(is.na(unique_Proc_codes)==T)
unique_Proc_codes<- unique_Proc_codes[-na_idxes]

na_idxes <- which(is.na(unique_Drug_codes)==T)
unique_Drug_codes<- unique_Drug_codes[-na_idxes]

unique_Diag_codes <- as.data.frame(unique_Diag_codes)
unique_Proc_codes <- as.data.frame(unique_Proc_codes)
unique_Drug_codes <- as.data.frame(unique_Drug_codes)


# write.csv(unique_Diag_codes,paste0(outdir,"All_unique_Diag_codes.csv"),row.names = F)
# write.csv(unique_Proc_codes,paste0(outdir,"All_unique_Proc_codes.csv"),row.names = F)
# write.csv(unique_Drug_codes,paste0(outdir,"All_unique_Drug_codes.csv"),row.names = F)

################################################################################ 
### Load Valid Month
################################################################################ 
Valid_Month_df <- read.csv(paste0(outdir,"All_Final_Valid_month.csv"),stringsAsFactors = F)
length(unique(Valid_Month_df[,"ID"])) #28675

################################################################################ 
#Filter by valid month, get monthly code counts table
################################################################################ 
analysis_ID <- unique(intersect(Comb_perday_df[,"study_id"],Valid_Month_df[,"ID"])) #26933
length(analysis_ID)

n_valid_month <- NA
valid_perday_df_list <- list()
for (i in 1:length(analysis_ID)){
  curr_id <- analysis_ID[i]
  curr_enroll_months <- unique(Valid_Month_df[which(Valid_Month_df[,"ID"] == curr_id),"Valid_Month"])
  n_valid_month[i] <- length(curr_enroll_months)
  
  min_month <- min(ymd(curr_enroll_months))
  max_month <- max(ymd(curr_enroll_months))
  
  #Curr_per_day  data
  curr_perday_df <- Comb_perday_df[which(Comb_perday_df[,"study_id"] == curr_id),]
  
  index_tokeep <- which(ymd(curr_perday_df[,"claims_date"]) >= min_month & ymd(curr_perday_df[,"claims_date"]) <= max_month )
  if (length(index_tokeep) > 0 ){
    valid_perday_df_list[[i]] <- curr_perday_df[index_tokeep,]
  }else{
    valid_perday_df_list[[i] <- NULL
  }
  
}


################################################################################ 
#'TODO Need space to hold this
#Per month codes count
n_valid_month <- NA
for (i in 1:length(analysis_ID)){
  curr_id <- analysis_ID[i]
  curr_enroll_months <- unique(Valid_Month_df[which(Valid_Month_df[,"ID"] == curr_id),"Valid_Month"])
  n_valid_month[i] <- length(curr_enroll_months)
  
  #Curr_per_day  data
  curr_perday_df <- Comb_perday_df[which(Comb_perday_df[,"study_id"] == curr_id),]
  
  #per month table for diag, pro and drug
  res <- create_perMon_count_table(curr_id,n_valid_month,curr_enroll_months,curr_perday_df)
  curr_pt_month_table_diag <- res[[1]]
  curr_pt_month_table_proc <- res[[2]]
  curr_pt_month_table_drug <- res[[3]]
  
  outdir1 <- paste0(outdir,"PerMonth_CountTable/Diag/")
  write.csv(curr_pt_month_table_diag,paste0(outdir1,curr_id,".csv"))
  outdir2 <- paste0(outdir,"PerMonth_CountTable/Proc/")
  write.csv(curr_pt_month_table_proc,paste0(outdir2,curr_id,".csv"))
  outdir3 <- paste0(outdir,"PerMonth_CountTable/Drug/")
  write.csv(curr_pt_month_table_drug,paste0(outdir3,curr_id,".csv"))
  
}

################################################################################ 
#'TODO: Add patient outcome, pre-post recurrence, first primary dates
################################################################################ 
