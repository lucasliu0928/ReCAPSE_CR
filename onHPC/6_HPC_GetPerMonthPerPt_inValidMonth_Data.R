library(openxlsx)
library(data.table)
library(lubridate)
library(parallel)
library(foreach)
library(doParallel)

split_andcombine_codes <- function(code_df,code_col){
  # code_df <- curr_df
  # code_col <- "Diag_Codes"
  
  curr_codes <- code_df[,code_col]
  #remove NAs
  na_idxes <- which(is.na(curr_codes)==T)
  if (length(na_idxes) > 0 ){
    curr_codes <- curr_codes[-na_idxes]
  }
  
  
  if (length(curr_codes) >0){ #if remaining code length > 0
    #split to remove duplciated from each day
    curr_unique_splited_codes <- unique(unlist(strsplit(curr_codes,split= "$$$$",fixed = T)))
    curr_combined_codes <- paste0(curr_unique_splited_codes,collapse = "$$$$")
  }else{
    curr_combined_codes <- NA
  }
  
  return(curr_combined_codes)
  
}


valid_month_dir <- "/recapse/intermediate_data/"
data_dir <- "/recapse/intermediate_data/3_perDay_PerPatientData/"
outdir <- "/recapse/intermediate_data/6_perMonthData_inValidMonth_perPatientData/"

# #local
# valid_month_dir <- "/Users/lucasliu/Desktop/intermediate_data/"
# data_dir <- "/Users/lucasliu/Desktop/intermediate_data/3_perDay_PerPatientData/"
# outdir <- "/Users/lucasliu/Desktop/intermediate_data/6_perMonthData_inValidMonth_perPatientData/"


numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores


############################################################
#Load Valid months
############################################################
valid_month_df <- read.xlsx(paste0(valid_month_dir,"5_valid_month_df.xlsx"),sheet = 1)

############################################################
#PerDay files
############################################################
perDay_files <- list.files(data_dir)

# files_processed <- list.files(outdir)
# perDay_files <- perDay_files[-which(perDay_files %in% files_processed)]
# print(length(perDay_files))

#only for pts has valid month
foreach (i = 1: length(perDay_files)) %dopar% {
  curr_file <- perDay_files[i]
  curr_id <- as.numeric(gsub("_perDay_Data.xlsx|ID","",curr_file))
  
  #per day df
  curr_perDay_df <- read.xlsx(paste0(data_dir,curr_file),sheet = 1)
  
  #valid month
  curr_valid_month_df <- valid_month_df[which(valid_month_df$study_id == curr_id),]
  if (nrow(curr_valid_month_df) > 0){ #only output file when pts has valid month
    #Get data in range
    start <- ymd(curr_valid_month_df[,"Valid_Start"])
    end <- ymd(curr_valid_month_df[,"Valid_End"])
    filtered_df <- curr_perDay_df[which(ymd(curr_perDay_df[,"claims_date"]) >= start & 
                                        ymd(curr_perDay_df[,"claims_date"]) <= end ),]
    #Generate Month sqeuence from start to end
    month_seqs <- ymd(seq(start,end,by="months"))
    
    #Get month data
    perMonth_data <- as.data.frame(matrix(NA, nrow = length(month_seqs)-1, ncol = 5))
    colnames(perMonth_data) <- c("study_id","Month_Start","Diag_Codes","Proc_Codes","Drug_Codes")
    perMonth_data[,"study_id"] <- curr_id
    for (t in 1:(length(month_seqs) -1)){
      curr_mon_start <- ymd(month_seqs[t])
      curr_mon_end <- ymd(month_seqs[t+1])
      curr_df <- filtered_df[which(ymd(filtered_df[,"claims_date"]) >= curr_mon_start & 
                                   ymd(filtered_df[,"claims_date"]) < curr_mon_end ),]
      if (nrow(curr_df) > 0){
        curr_month_diag_codes <- split_andcombine_codes(curr_df,"Diag_Codes")
        curr_month_proc_codes <- split_andcombine_codes(curr_df,"Proc_Codes")
        curr_month_drug_codes <- split_andcombine_codes(curr_df,"Drug_Codes")
      }else{
        curr_month_diag_codes <- NA
        curr_month_proc_codes <- NA
        curr_month_drug_codes <- NA
      }
      perMonth_data[t, "Month_Start"] <- as.character(curr_mon_start)
      perMonth_data[t, "Diag_Codes"] <- curr_month_diag_codes
      perMonth_data[t, "Proc_Codes"] <- curr_month_proc_codes
      perMonth_data[t, "Drug_Codes"] <- curr_month_drug_codes
      
    }
    
    write.xlsx(perMonth_data,paste0(outdir,"ID",curr_id,"_","perMonthData_inValidMonth.xlsx"))
    
  }
  
  
}
