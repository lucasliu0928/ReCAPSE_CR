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
data_dir <- "/recapse/intermediate_data/6_CleanClaims_InValidMonth/EnrolledMonths_WithPossibleMonthsHasNoCodes3/"
outdir <- "/recapse/intermediate_data/6_CleanClaims_InValidMonth/EnrolledMonths_WithEveryMonthsHasCodes/"

# # # #local
# data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/6_CleanClaims_InValidMonth/EnrolledMonths_WithPossibleMonthsHasNoCodes/"
# outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/6_CleanClaims_InValidMonth/EnrolledMonths_WithEveryMonthsHasCodes/"


############################################################
#Clean Calims Per Patient per month in Valid months
############################################################
perMonth_files <- list.files(data_dir)
analysis_IDs <- as.numeric(gsub("_perMonthData_Enrolled_inPredictionWindow.xlsx|ID","",perMonth_files))

########################################################################################################################
#Use the following code to run in case out of memory when procssing all at one time
########################################################################################################################
ID_processed <- as.numeric(gsub("_perMonthData_Enrolled_inPredictionWindow.xlsx|ID","",list.files(outdir)))
if (length(ID_processed) != 0 ){
  analysis_IDs <- analysis_IDs[-which(analysis_IDs %in% ID_processed)]
}
print(length(analysis_IDs))

foreach (i = 1: length(analysis_IDs)) %dopar% {
  curr_id <- analysis_IDs[i]
  curr_file <- paste0("ID",curr_id,"_perMonthData_Enrolled_inPredictionWindow.xlsx")

  #per month df all enrolled with possible no codes in the month
  enrolled_month_df <- read.xlsx(paste0(data_dir,curr_file),sheet = 1)

  #Only keep the month rows that has at least one code
  codes_index <- 5:ncol(enrolled_month_df)
  if (length(codes_index) > 2){
    enrolled_month_df_filtered <- enrolled_month_df[rowSums(is.na(enrolled_month_df[,codes_index])) != ncol(enrolled_month_df[,codes_index]), ]
  }else{
    enrolled_month_df_filtered <- enrolled_month_df[which(is.na(enrolled_month_df[,codes_index])==F),]
  }
  if(nrow(enrolled_month_df_filtered) > 0 & ncol(enrolled_month_df_filtered) > 4){ #if output if ncols > 4 and nrow > 0 
    write.xlsx(enrolled_month_df_filtered,paste0(outdir,"ID",curr_id,"_","perMonthData_Enrolled_inPredictionWindow.xlsx"))
  }
  

}
