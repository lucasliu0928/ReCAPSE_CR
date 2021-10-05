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
valid_month_dir <- "/recapse/intermediate_data/5_Enrollment_And_Prediction_Months/"
data_dir <- "/recapse/intermediate_data/3_CleanClaims_perPatient_perMonth/"
outdir <- "/recapse/intermediate_data/6_CleanClaims_InValidMonth/EnrolledMonths_WithPossibleMonthsHasNoCodes3/"

# # #local
# valid_month_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/5_Enrollment_And_Prediction_Months/"
# data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/3_CleanClaims_perPatient_perMonth/"
# outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/6_CleanClaims_InValidMonth/EnrolledMonths_WithPossibleMonthsHasNoCodes/"


############################################################
#Load Valid months
############################################################
enrolmment_df <- read.xlsx(paste0(valid_month_dir,"5_enrollment_Months.xlsx"),sheet = 1)
predictionWindow_df <- read.xlsx(paste0(valid_month_dir,"5_prediction_month_df.xlsx"),sheet = 1)


############################################################
#Clean Calims Per Patient per month
############################################################
perMonth_files <- list.files(data_dir)
analysis_IDs <- as.numeric(gsub("_perMonth_Data.xlsx|ID","",perMonth_files))

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
  curr_file <- paste0("ID",curr_id,"_perMonth_Data.xlsx")

  #per month df
  curr_perMonth_df <- read.xlsx(paste0(data_dir,curr_file),sheet = 1)

  #get enrollment months
  curr_enrollment_months_df <- enrolmment_df[which(enrolmment_df[,"study_id"] == curr_id),]
  curr_enroll_months <- curr_enrollment_months_df[,"Enrolled_Month"]

  #get predictio window
  curr_prediction_df <- predictionWindow_df[which(predictionWindow_df[,"study_id"] == curr_id),]
  curr_prediction_start <- curr_prediction_df[,"Prediction_Start"]
  curr_prediction_end   <- curr_prediction_df[,"Prediction_End"]


  #Create a dataframe for all possible enrollment months
  enrolled_month_df <- as.data.frame(matrix(NA, nrow = length(curr_enroll_months), ncol = ncol(curr_perMonth_df) +1 ))
  colnames(enrolled_month_df) <- c("Enrolled_Month",colnames(curr_perMonth_df))
  enrolled_month_df[,"Enrolled_Month"] <- curr_enroll_months

  #Keep claims data that are in enrollment month
  curr_perMonth_df <- curr_perMonth_df[which(curr_perMonth_df[,"Month_Start"] %in% enrolled_month_df[,"Enrolled_Month"]),]

  #Match the time entry between claims data and enrollment month data
  idx_inEnroll <- match(curr_perMonth_df[,"Month_Start"] , enrolled_month_df[,"Enrolled_Month"])
  enrolled_month_df[idx_inEnroll,2:ncol(enrolled_month_df)] <- curr_perMonth_df

  #Fill and keep the enrollment month time and ID when there is no claims available
  noclaims_rows <- which(is.na(enrolled_month_df[,"Month_Start"])==T)
  enrolled_month_df[noclaims_rows,"Month_Start"]  <- enrolled_month_df[noclaims_rows,"Enrolled_Month"]
  enrolled_month_df[noclaims_rows,"study_id"]     <- curr_id
  enrolled_month_df[noclaims_rows,"Month_End"]    <- as.character(ymd(enrolled_month_df[noclaims_rows,"Month_Start"]) + months(1))


  #Keep the per month data in prediction months (predefined start and end point)
  kept_idxes <- which(ymd(enrolled_month_df[,"Enrolled_Month"]) >= ymd(curr_prediction_start) &
                      ymd(enrolled_month_df[,"Enrolled_Month"]) <= ymd(curr_prediction_end) )
  updated_enrolled_month_df <- enrolled_month_df[kept_idxes,]

  #Only keep the Code columns if not all rows are NAs (Due to the exclusion of time, some unique Code found before might now has NA time rows)
  updated_enrolled_month_df <- updated_enrolled_month_df[,colSums(is.na(updated_enrolled_month_df))<nrow(updated_enrolled_month_df)]
  
  #if ncol = 4, then there is no Code left nad nrow >0
  if(ncol(updated_enrolled_month_df) > 4 & nrow(updated_enrolled_month_df) > 0 ){ #if output if ncols > 4
    write.xlsx(updated_enrolled_month_df,paste0(outdir,"ID",curr_id,"_","perMonthData_Enrolled_inPredictionWindow.xlsx"))
  }

}
