library(openxlsx)
library(data.table)
library(lubridate)
library(parallel)
library(doParallel)

numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores

remove_NA_func<- function(curr_day_codes){
  na_idxes <- which(curr_day_codes == "" | is.na(curr_day_codes) == TRUE)
  if (length(na_idxes) > 0){
    curr_day_codes <- curr_day_codes[-na_idxes]
  }
  return(curr_day_codes)
}


data_dir <- "/recapse/intermediate_data/perPatient_uniqueCodes/"
outdir <- "/recapse/intermediate_data/"

# #local
# data_dir <- "/Users/lucasliu/Desktop/intermediate_data/perPatient_uniqueCodes/"
# outdir <- "/Users/lucasliu/Desktop/intermediate_data/"

############################################################
#unique codes files
############################################################
perPt_codes_files <- list.files(data_dir,full.names = T)

all_unique_codes_df <- as.data.frame(matrix(NA, nrow = length(perPt_codes_files), ncol = 4))
colnames(all_unique_codes_df) <- c("ID","Unique_Diag","Unique_Proc","Unique_Drug")
for (i in 1:length(perPt_codes_files)){ 

  if (i %% 1000 == 0){print(i)}
  curr_file <- perPt_codes_files[i]
  curr_IDs <- gsub(data_dir,"",curr_file)
  curr_IDs <-  gsub("/ID|_perDay_UniqueCode.xlsx","",curr_IDs)
  all_unique_codes_df[i,"ID"] <- curr_IDs
  curr_df <- read.xlsx(curr_file,sheet = 1)
  if (nrow(curr_df) !=0){
    all_unique_codes_df[i,"Unique_Diag"] <- curr_df$unique_diag_codes
    all_unique_codes_df[i,"Unique_Proc"] <- curr_df$unique_proc_codes
    all_unique_codes_df[i,"Unique_Drug"] <- curr_df$unique_drug_codes
  }else{
    all_unique_codes_df[i,"Unique_Diag"] <- NA
    all_unique_codes_df[i,"Unique_Proc"] <- NA
    all_unique_codes_df[i,"Unique_Drug"] <- NA
  }
  
}

write.xlsx(all_unique_codes_df,paste0(outdir,"AllPatient_UniqueCode_inValidMonth.xlsx"))

########################################################################################################################
##There are pts has no codes in valid month, should have be exluced from previous step when generating AllPatient_UniqueCode_inValidMonth.xlsx
########################################################################################################################
#Paralleization
# IDs <- gsub(data_dir,"",perPt_codes_files)
# IDs <-  gsub("/ID|_perDay_UniqueCode.xlsx","",IDs)
# print(length(IDs))
# all_unique_codes_df <- do.call(rbind,mclapply(perPt_codes_files,read.xlsx, mc.cores = numCores))
# print(nrow(all_unique_codes_df))
# #all_unique_codes_df$ID <- IDs
#  
# # write.xlsx(all_unique_codes_df,paste0(outdir,"AllPatient_UniqueCode_inValidMonth.xlsx"))

