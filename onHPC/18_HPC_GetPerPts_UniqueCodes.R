library(openxlsx)
library(data.table)
library(lubridate)
library(parallel)
library(foreach)
library(doParallel)


remove_NA_func<- function(curr_day_codes){
  na_idxes <- which(curr_day_codes == "" | is.na(curr_day_codes) == TRUE)
  if (length(na_idxes) > 0){
    curr_day_codes <- curr_day_codes[-na_idxes]
  }
  return(curr_day_codes)
}


data_dir <- "/recapse/intermediate_data/perDay_ValidMonth_PerPatientData/"
outdir <- "/recapse/intermediate_data/perPatient_uniqueCodes/"

# # #local
# data_dir <- "/Users/lucasliu/Desktop/intermediate_data/perDay_ValidMonth_PerPatientData/"
# outdir <- "/Users/lucasliu/Desktop/intermediate_data/perPatient_uniqueCodes/"


numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores


############################################################
#PerDay files
############################################################
perDay_files <- list.files(data_dir)


#only for pts has valid month
foreach (i = 1: length(perDay_files)) %dopar% {
  curr_file <- perDay_files[i]
  curr_id <- as.numeric(gsub("_perDay_ValidMonth_Data.xlsx|ID","",curr_file))
  
  #per day df
  curr_perDay_df <- read.xlsx(paste0(data_dir,curr_file),sheet = 1)
  curr_diag_codes <- remove_NA_func(curr_perDay_df[,"Diag_Codes"])
  curr_proc_codes <- remove_NA_func(curr_perDay_df[,"Proc_Codes"])
  curr_drug_codes <- remove_NA_func(curr_perDay_df[,"Drug_Codes"])
  
  if (length(curr_diag_codes) > 0){
    curr_unique_diag_codes <- unique(unlist(strsplit(curr_diag_codes,split = "$$$$",fixed = T)))
    unique_diag_codes <- paste0(curr_unique_diag_codes,collapse = "$$$$")  
    
  }else{
    unique_diag_codes <- NA
  }
  if (length(curr_proc_codes) > 0){
     curr_unique_proc_codes <- unique(unlist(strsplit(curr_proc_codes,split = "$$$$",fixed = T)))
     unique_proc_codes <- paste0(curr_unique_proc_codes,collapse = "$$$$")  
     
  }else{
    unique_proc_codes <- NA
  }
  if (length(curr_drug_codes) > 0){
   curr_unique_drug_codes <- unique(unlist(strsplit(curr_drug_codes,split = "$$$$",fixed = T)))
   unique_drug_codes <- paste0(curr_unique_drug_codes,collapse = "$$$$")  
   
  }else{
    unique_drug_codes <- NA
  }
  
  
  
  
  
  curr_unique_codes_df <- cbind.data.frame(unique_diag_codes,unique_proc_codes,unique_drug_codes)
  write.xlsx(curr_unique_codes_df,paste0(outdir,"ID",curr_id,"_","perDay_UniqueCode.xlsx"))
}
