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
data_dir1  <- paste0(proj_dir, "8_Characteristics/Patient_Level/")
data_dir2  <- paste0(proj_dir, "7_PrePostLabels_AndAvailibility6mon/A_PrePost_Labels/EnrolledMonths_WithPossibleMonthsHasNoCodes/")

outdir   <- paste0(proj_dir, "8_Characteristics/Month_Level/MonthChar_WithPossibleMonthsHasNoCodes/")


################################################################################
#1.Load patient level char
################################################################################
patient_level_char_df <- read.xlsx(paste0(data_dir1,"8_PatientLevel_char_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1)

################################################################################
#2.Load enrolled months files
################################################################################
enrolled_df_files <- list.files(data_dir2)
analysis_IDs <- as.numeric(gsub("_PreOrPost_MonthlyLabel.xlsx|ID","",enrolled_df_files))

########################################################################################################################
#Use the following code to run in case out of memory when procssing all at one time
########################################################################################################################
ID_processed <- as.numeric(gsub("_MonthChar.xlsx|ID","",list.files(outdir)))
if (length(ID_processed) != 0 ){
  analysis_IDs <- analysis_IDs[-which(analysis_IDs %in% ID_processed)]
}
print(length(analysis_IDs))


#'@TOADDED: will add "DAJCC_T","DAJCC_M","DAJCC_N" later, and num_claims later
foreach (i = 1: length(analysis_IDs)) %dopar% {
  curr_id <- analysis_IDs[i]
  curr_file <- paste0("ID",curr_id,"_PreOrPost_MonthlyLabel.xlsx")
  
  #per month df
  enrolled_month_df <- read.xlsx(paste0(data_dir2,curr_file),sheet = 1)
  
  #Patient level char
  curr_pt_level_df <- patient_level_char_df[which(patient_level_char_df[,"study_id"] == curr_id),]
 
  #process only if pt has per month df and has patient level char df
  if (nrow(enrolled_month_df) >0 & nrow(curr_pt_level_df) >0){
    #first and 2nd event date
    curr_1stevent_date <- mdy(curr_pt_level_df[,"Date_1st_Event"])
    curr_2ndevent_date <- mdy(curr_pt_level_df[,"Date_2nd_Event"])
    
    #date of birth
    curr_dob <-   mdy(curr_pt_level_df[,"date_Birth"])
    

    #construct per patient month level data
    curr_month_level_char_df <- as.data.frame(matrix(NA, nrow =nrow(enrolled_month_df) ,ncol = 20))
    colnames(curr_month_level_char_df) <- c("Age","months_since_dx",                                            "has_second_event",
                                            "months_to_second_event",
                                            "Race", "Site", "Stage","Grade","Laterality",
                                            "er_stat","pr_stat","her2_stat","surg_prim_site",
                                            "reg_age_at_dx","reg_nodes_exam","reg_nodes_pos",
                                            "cs_tum_size","cs_tum_ext","cs_tum_nodes","regional")
    curr_month_level_char_df <- cbind(enrolled_month_df,curr_month_level_char_df)
    
    #add features (This are the same across all rows)
    feature_cols <- c("Race","Site","Stage","Grade","Laterality",
                      "er_stat","pr_stat","her2_stat","surg_prim_site",
                      #"DAJCC_T","DAJCC_M","DAJCC_N",
                      "reg_age_at_dx","reg_nodes_exam","reg_nodes_pos",
                      "cs_tum_size","cs_tum_ext","cs_tum_nodes","regional")
    curr_month_level_char_df[,feature_cols] <- curr_pt_level_df[,feature_cols]
    curr_month_level_char_df[,"has_second_event"] <- curr_pt_level_df[,"SBCE"]
    
    #for each month row, add feature
    for (j in 1:nrow(curr_month_level_char_df)){
          curr_month <- ymd(curr_month_level_char_df[j,"Month_Start"])
          
          curr_month_level_char_df[j,"Age"] <- as.numeric(difftime(curr_month,curr_dob,units = "days")/365)
          curr_month_level_char_df[j,"months_since_dx"] <- as.numeric(difftime(curr_month,curr_1stevent_date,units = "days"))/30 #converted to month
          if (is.na(curr_2ndevent_date) == F){
            curr_month_level_char_df[j,"months_to_second_event"] <- as.numeric(difftime(curr_month,curr_2ndevent_date,units = "days"))/30 #converted to month
            
          }else{
            curr_month_level_char_df[j,"months_to_second_event"] <- NA
          }
      
          # #number of claims in each month
          # curr_perday_file <- paste0(perday_dir,"ID",curr_id,"_perDay_Data.xlsx") #    #1.get per day file
          # 
          # if (file.exists(curr_perday_file) == T){
          #   curr_perday_df <- read.xlsx(curr_perday_file,sheet = 1)
          #   curr_claims <- which(ymd(curr_perday_df$claims_date) >= curr_month & ymd(curr_perday_df$claims_date) < curr_month + days(30))
          #   curr_month_level_char_df[j,"num_claims"] <- length(curr_claims)
          #   
          # }else{
          #   curr_month_level_char_df[j,"num_claims"] <- 0
          # }
    }
    
    write.xlsx(curr_month_level_char_df,paste0(outdir,"ID",curr_id,"_MonthChar.xlsx"))
    
  }
  
}
