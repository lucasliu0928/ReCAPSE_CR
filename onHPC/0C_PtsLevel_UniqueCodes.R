source("Recapse_Ultility.R")

################################################################################
#Set up parallel computing envir
################################################################################
numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores

#######################################################################
##############              Data dir                     ############## 
#######################################################################
data_dir <- "/recapse/intermediate_data/3A_perDay_PerPatientData/"
outdir   <- "/recapse/intermediate_data/0_Codes/perPatient_UniqueCodes/"

# #local
# data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/3A_perDay_PerPatientData/"
# outdir   <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/0_Codes/perPatient_UniqueCodes/"

################################################################################
#2.Get perDay data for all patients and unique codes
################################################################################
perDay_files <- list.files(data_dir)

foreach (i = 1: length(perDay_files)) %dopar% {
  curr_file <- perDay_files[i]
  curr_id <- as.numeric(gsub("_perDay_Data.xlsx|ID","",curr_file))
  
  #per day df
  curr_perDay_df <- read.xlsx(paste0(data_dir,curr_file),sheet = 1)
  
  #Unique Codes df
  curr_unique_code_df <- as.data.frame(matrix(NA, nrow = 1, ncol = 4))
  colnames(curr_unique_code_df) <- c("study_id","Diag_Codes","Proc_Codes","Drug_Codes")
  curr_unique_code_df[1,"study_id"] <- curr_id
  
  code_cols <- c("Diag_Codes","Proc_Codes","Drug_Codes")
  for(j in 1:length(code_cols)){
    curr_col <- code_cols[j]
    curr_code <- curr_perDay_df[which(is.na(curr_perDay_df[,curr_col])==F),curr_col]
    
    if (length(curr_code) > 0){
      curr_unique_code <- unique(unlist(strsplit(curr_code,split = "$$$$", fixed = T)))
      curr_unique_code_df[1,curr_col] <- paste0(curr_unique_code,collapse = "$$$$")
    }
  }
  
  write.xlsx(curr_unique_code_df,paste0(outdir,"ID",curr_id,"_uniqueCodes.xlsx"))
  
}

