library(openxlsx)
library(data.table)
library(lubridate)
library(parallel)
library(foreach)
library(doParallel)

valid_month_dir <- "/recapse/intermediate_data/"
data_dir <- "/recapse/intermediate_data/3_perDay_PerPatientData/"
outdir <- "/recapse/intermediate_data/6_perMonth_inValidMonth_PerPatientData/"

#local
valid_month_dir <- "/Users/lucasliu/Desktop/intermediate_data/"
data_dir <- "/Users/lucasliu/Desktop/intermediate_data/3_perDay_PerPatientData/"
outdir <- "/Users/lucasliu/Desktop/intermediate_data/6_perDay_ValidMonth_PerPatientData/"


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
  i <- 1
  curr_file <- perDay_files[i]
  curr_id <- as.numeric(gsub("_perDay_Data.xlsx|ID","",curr_file))
  
  #per day df
  curr_perDay_df <- read.xlsx(paste0(data_dir,curr_file),sheet = 1)
  
  #valid month
  curr_valid_month_df <- valid_month_df[which(valid_month_df$study_id == curr_id),]
  if (nrow(curr_valid_month_df) > 0){ #only output file when valid month is >0
    #Get data in range
    start <- ymd(curr_valid_month_df[,"Valid_Start"])
    end <- ymd(curr_valid_month_df[,"Valid_End"])
    filtered_df <- curr_perDay_df[which(ymd(curr_perDay_df[,"claims_date"]) >= start & 
                                        ymd(curr_perDay_df[,"claims_date"]) <= end ),]
    #Generate Month sqeuence from start to end
    month_seqs <- ymd(seq(start,end,by="months"))
    
    write.xlsx(filtered_df,paste0(outdir,"ID",curr_id,"_","perDay_ValidMonth_Data.xlsx"))
    
  }
  
  
}
