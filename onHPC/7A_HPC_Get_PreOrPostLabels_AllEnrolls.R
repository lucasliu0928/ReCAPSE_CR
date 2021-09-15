source("Recapse_Ultility.R")

################################################################################
#Set up parallel computing envir
################################################################################
numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores

###########################################################################################################################
#This script 
#1. Get pre or post labels for each enrolled month
###########################################################################################################################

#onHPC
proj_dir  <- "/recapse/intermediate_data/"
data_dir  <- paste0(proj_dir, "6_CleanClaims_InValidMonth/EnrolledMonths_WithPossibleMonthsHasNoCodes/")
data_dir2 <- paste0(proj_dir, "4_RecurrDates_Outcome_Info/")
outdir    <- paste0(proj_dir, "7_PrePostLabels_AndAvailibility6mon/A_PrePost_Labels/EnrolledMonths_WithPossibleMonthsHasNoCodes/")

# # #local
# proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"
# data_dir  <- paste0(proj_dir, "6_CleanClaims_InValidMonth/EnrolledMonths_WithPossibleMonthsHasNoCodes/")
# data_dir2 <- paste0(proj_dir, "4_RecurrDates_Outcome_Info/")
# outdir    <- paste0(proj_dir, "7_PrePostLabels_AndAvailibility6mon/A_PrePost_Labels/EnrolledMonths_WithPossibleMonthsHasNoCodes/")

#########################################################################################################
#1.Load pateint event type and date data
#########################################################################################################
All_event_df <- read.xlsx(paste0(data_dir2,"4_All_event_df.xlsx"),sheet = 1)
SBCE_df      <- read.xlsx(paste0(data_dir2,"4_SBCE_Label.xlsx"),sheet = 1)


############################################################
#PerMonth files
############################################################
perMonth_files <- list.files(data_dir)
analysis_IDs   <- gsub("_perMonthData_Enrolled_inPredictionWindow.xlsx|ID","",perMonth_files)

############################################################
ID_processed <- gsub("_PreOrPost_MonthlyLabel.xlsx|ID","",list.files(outdir))
if (length(ID_processed) != 0 ){
  analysis_IDs <- analysis_IDs[-which(analysis_IDs %in% ID_processed)]
}
print(length(analysis_IDs))


###########################################################
#Get labels
###########################################################
foreach (i = 1: length(analysis_IDs)) %dopar% {
      curr_id   <- analysis_IDs[i]
      curr_file <- paste0("ID",curr_id,"_perMonthData_Enrolled_inPredictionWindow.xlsx")

      #per month df
      curr_perMonth_df <- read.xlsx(paste0(data_dir,curr_file),sheet = 1)

      #SBCE label
      curr_SBCE_label <- SBCE_df[which(SBCE_df[,"study_id"] == curr_id),"SBCE"]
      
      if (nrow(curr_perMonth_df) != 0 & length(curr_SBCE_label) !=0){ #only do it if has enrollment month df and has labels
        #event date df
        curr_event_df <- All_event_df[which(All_event_df[,"study_id"] == curr_id),]
        curr_date_2ndevent <- curr_event_df[,"Date_2nd_Event"]
  
        #Add columns for pre/post 2nd event on per month df
        curr_perMonth_df$y_PRE_OR_POST_2ndEvent <- NA
        curr_perMonth_df <- curr_perMonth_df[order(curr_perMonth_df[,"Enrolled_Month"],decreasing = F),]
  
        if (curr_SBCE_label == 0 ){ #if no SBCE, all rows assigned to 0
          curr_perMonth_df[,"y_PRE_OR_POST_2ndEvent"] <- 0
        }else if (curr_SBCE_label == 1){#if SBCE :
              #determine the record row for 2nd event happening, assigned to 1
              second_event_month_idx  <- which(ymd(curr_perMonth_df[,"Month_Start"]) <= mdy(curr_date_2ndevent) &
                                               ymd(curr_perMonth_df[,"Month_End"])   >= mdy(curr_date_2ndevent))
              if (length(second_event_month_idx)>0){#'@NOTE: it is possible the 2nd event does not happen in the enrollment months (e.g 2nd event = death)
                  curr_perMonth_df[second_event_month_idx,"y_PRE_OR_POST_2ndEvent"] <- 1
              }
  
              #Determine the row after 2nd event, assigned to 1
              month_indxes_after  <- which(ymd(curr_perMonth_df[,"Month_Start"]) > mdy(curr_date_2ndevent))
              if(length(month_indxes_after) > 0){
                curr_perMonth_df[month_indxes_after,"y_PRE_OR_POST_2ndEvent"] <- 1
              }
              #Determine the row before 2nd event, assigned to 0
              month_indxes_before <- which(ymd(curr_perMonth_df[,"Month_End"]) < mdy(curr_date_2ndevent))
              if(length(month_indxes_before) > 0){
                curr_perMonth_df[month_indxes_before,"y_PRE_OR_POST_2ndEvent"] <- 0
              }
        }
  
       curr_monthly_label_df <- curr_perMonth_df[,c("Enrolled_Month","study_id","Month_Start","Month_End","y_PRE_OR_POST_2ndEvent")]
       write.xlsx(curr_monthly_label_df,paste0(outdir,"ID",curr_id,"_","PreOrPost_MonthlyLabel.xlsx"))
}

}
