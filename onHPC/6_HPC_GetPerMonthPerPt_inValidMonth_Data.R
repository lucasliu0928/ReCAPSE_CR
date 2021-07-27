source("Recapse_Ultility.R")

################################################################################
#Set up parallel computing envir
################################################################################
numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores

################################################################################
#Data dir
################################################################################
valid_month_dir <- "/recapse/intermediate_data/"
data_dir <- "/recapse/intermediate_data/3_perDay_PerPatientData/"
outdir <- "/recapse/intermediate_data/6_perMonthData_inValidMonth/"

# #local
# valid_month_dir <- "/Users/lucasliu/Desktop/intermediate_data/"
# data_dir <- "/Users/lucasliu/Desktop/intermediate_data/3_perDay_PerPatientData/"
# outdir <- "/Users/lucasliu/Desktop/intermediate_data/6_perMonthData_inValidMonth/"


############################################################
#Load Valid months
############################################################
valid_month_df <- read.xlsx(paste0(valid_month_dir,"5_valid_month_df.xlsx"),sheet = 1)


############################################################
#PerDay files
############################################################
perDay_files <- list.files(data_dir)

########################################################################################################################
#Use the following code to run in case out of memory when procssing all at one time
########################################################################################################################
files_outputed<- list.files(outdir)
ID_outputed <- gsub("_perMonthData_inValidMonth.xlsx","",files_outputed)
inputfiles_processed <- paste0(ID_outputed,"_perDay_Data.xlsx")
indxes_process <-   which(perDay_files %in% inputfiles_processed)
if (length(indxes_process) > 0){
  perDay_files <- perDay_files[-indxes_process]
}

print(paste0("To Process Files: ",length(perDay_files)))

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
    if (nrow(filtered_df) > 0){ #only if there are calims left after filtering
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
          curr_month_diag_codes <- split_andcombine_codes2(curr_df,"Diag_Codes") #split_andcombine_codes2 returns non_unique codes
          curr_month_proc_codes <- split_andcombine_codes2(curr_df,"Proc_Codes")
          curr_month_drug_codes <- split_andcombine_codes2(curr_df,"Drug_Codes")
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
  
}
