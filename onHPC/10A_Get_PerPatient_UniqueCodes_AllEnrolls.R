################################################################################
#This script get per patient unique codes for Final IDs (For All Enrolled months) in Valid month
################################################################################
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
#onHPC
proj_dir  <- "/recapse/intermediate_data/"

#local
#proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
data_dir1  <- paste0(proj_dir, "6_CleanClaims_InValidMonth/EnrolledMonths_WithPossibleMonthsHasNoCodes3/")
data_dir2  <- paste0(proj_dir, "9_FinalIDs_And_UpdatedPtsChar/")

outdir   <- paste0(proj_dir, "10A_PerPatient_UniqueCodes/WithPossibleMonthsHasNoCodes/")

################################################################################
#get per months files
################################################################################
perMonth_files <- list.files(data_dir1)

################################################################################
#3.Final IDs
################################################################################
Final_ID_df <- read.xlsx(paste0(data_dir2,"9_Final_ID1_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1)
analysis_IDs <- Final_ID_df[,"study_id"]

########################################################################################################################
#Use the following code to run in case out of memory when procssing all at one time
########################################################################################################################
ID_processed <- as.numeric(gsub("_UniqueCodes.xlsx|ID","",list.files(outdir)))
if (length(ID_processed) != 0 ){
  analysis_IDs <- analysis_IDs[-which(analysis_IDs %in% ID_processed)]
}
print(length(analysis_IDs))

foreach (i = 1: length(analysis_IDs)) %dopar% {
  curr_id <- analysis_IDs[i]
  curr_file <- paste0("ID",curr_id,"_perMonthData_Enrolled_inPredictionWindow.xlsx")
  
  #per month df
  curr_perMonth_df <- read.xlsx(paste0(data_dir1,curr_file),sheet = 1)
  
  #Make sure no code has all NAs rows
  #NOTE this was also done in previous code when generate in prediction Window
  curr_perMonth_df <- curr_perMonth_df[,colSums(is.na(curr_perMonth_df))<nrow(curr_perMonth_df)]
  
  #Unique codes 
  curr_unique_codes <- colnames(curr_perMonth_df)[-which(colnames(curr_perMonth_df) %in% c("Enrolled_Month", "study_id" , "Month_Start","Month_End"))]
  curr_unique_codes_df <- as.data.frame(curr_unique_codes)
  colnames(curr_unique_codes_df) <- "Unique_Codes"
  
  if (length(curr_unique_codes) > 0 ){ #Make sure there is any code left in the df, if not, this pts should be excluded for final
    write.xlsx(curr_unique_codes_df,paste0(outdir,"ID",curr_id,"_UniqueCodes.xlsx"))
  }
}


