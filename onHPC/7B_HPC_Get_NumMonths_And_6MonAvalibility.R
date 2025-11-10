source("Recapse_Ultility.R")
compute_num_months_func <-function(in_Labeldata,event_data,SBCE_data){
  #HasEnoughMonths_InWindow:
  #1).  If SBCE ((Still conosider the death here), 
  #     A. If death, at least have 3 month data before death
  #     B, If recur or diag of 2nd primary, at least 3 month data before and after 2nd event
  #2):  if not SBCE,
  #     aat least 6 months of data avaiable
  
  # in_Labeldata <- label_df1
  # event_data <- All_event_df
  # SBCE_data <- SBCE_df
  
  unique_IDs <- unique(in_Labeldata[,"study_id"])
  
  num_month_df <- as.data.frame(matrix(NA, nrow = length(unique_IDs), ncol = 12))
  colnames(num_month_df) <- c("study_id","SBCE","Second_Event_Type","Second_Event_Date",
                              "Last_Enrolled_Prediction_Month_End",
                              "Num_Enrolled_Prediction_Months",
                              "Num_Month_before_2ndEvent",
                              "Num_Month_AfterOrEqual_2ndEvent",
                              "SBCE_Excluded_DeathLabel",
                              "Num_Month_before_2ndEvent_ExcludedDeath",
                              "Num_Month_AfterOrEqual_2ndEvent_ExcludedDeath",
                              "HasEnoughMonths_InWindow")
  
  for (i in 1:length(unique_IDs)){
    curr_id <- unique_IDs[i]
    num_month_df[i,"study_id"] <- curr_id
    
    #enrollment data with labels
    curr_enroll_df <- in_Labeldata[which(in_Labeldata[,"study_id"] == curr_id),]
    num_month_df[i,"Num_Enrolled_Prediction_Months"] <- nrow(curr_enroll_df)
    num_month_df[i,"Last_Enrolled_Prediction_Month_End"] <- as.character(max(ymd(curr_enroll_df[,"Month_End"]),na.rm = T))
    
    #event type data
    curr_event_type_df <- event_data[which(event_data[,"study_id"] == curr_id),]
    num_month_df[i,"Second_Event_Type"]  <- curr_event_type_df[,"Type_2nd_Event"] 
    num_month_df[i,"Second_Event_Date"]  <- as.character(mdy(curr_event_type_df[,"Date_2nd_Event"])) 
    
    #SBCE label
    curr_SBCE_label <- SBCE_data[which(SBCE_data[,"study_id"] == curr_id),"SBCE"]
    num_month_df[i,"SBCE"]  <- curr_SBCE_label 
    
    num_month_df[i,"Num_Month_before_2ndEvent"]      <-  length(which(curr_enroll_df$y_PRE_OR_POST_2ndEvent==0))
    num_month_df[i,"Num_Month_AfterOrEqual_2ndEvent"] <- length(which(curr_enroll_df$y_PRE_OR_POST_2ndEvent==1))
    
    #SBCE label2
    curr_SBCE_label2 <- SBCE_data[which(SBCE_data[,"study_id"] == curr_id),"SBCE_Excluded_DeathLabel"]
    num_month_df[i,"SBCE_Excluded_DeathLabel"]  <- curr_SBCE_label2 
    num_month_df[i,"Num_Month_before_2ndEvent_ExcludedDeath"]      <-  length(which(curr_enroll_df$y_PRE_OR_POST_2ndEvent_ExcludedDeath==0))
    num_month_df[i,"Num_Month_AfterOrEqual_2ndEvent_ExcludedDeath"] <- length(which(curr_enroll_df$y_PRE_OR_POST_2ndEvent_ExcludedDeath==1))
    
    #Has enough months flag
    if (curr_SBCE_label == 0){ #if no SBCE, at least as 6 month enrollment months
      if( num_month_df[i,"Num_Enrolled_Prediction_Months"] >= 6){
        has_months_flag <- 1
      }else{
        has_months_flag <- 0
      }
    }else if (curr_SBCE_label == 1 & grepl("Death",num_month_df[i,"Second_Event_Type"]) == T){ #if SBCE and 2nd event is death
      if( num_month_df[i,"Num_Month_before_2ndEvent"] >= 3){
        has_months_flag <- 1
      }else{
        has_months_flag <- 0
      }
    }else if (curr_SBCE_label == 1 & grepl("Death",num_month_df[i,"Second_Event_Type"]) == F){ #if SBCE and 2nd event is NOT death
      if( num_month_df[i,"Num_Month_before_2ndEvent"] >= 3 & num_month_df[i,"Num_Month_AfterOrEqual_2ndEvent"] >= 3 ){
        has_months_flag <- 1
      }else{
        has_months_flag <- 0
      }
    }
    
    num_month_df[i,"HasEnoughMonths_InWindow"] <- has_months_flag
  }
  
  return(num_month_df)
}


################################################################################
#Set up parallel computing envir
################################################################################
numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores

###########################################################################################################################
#This script 
#1. Get Num of enrollment months
#2. Get Num of months before/after 2nd event

#3. get exclusion flag for patients does not qualify the following: 
#1).  If SBCE (Still conosider the death here), 
#     A. If death, at least have 3 month data before death
#     B, If recur or diag of 2nd primary, at least 3 month data before and after 2nd event
#2):  if not SBCE,
#     aat least 6 months of data avaiable
###########################################################################################################################
#onHPC
proj_dir  <- "/recapse/intermediate_data/"

#local
#proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
data_dir1  <- paste0(proj_dir, "7_PrePostLabels_AndAvailibility6mon/A_PrePost_Labels/EnrolledMonths_WithPossibleMonthsHasNoCodes/")
#data_dir2  <- paste0(proj_dir, "7_PrePostLabels_AndAvailibility6mon/A_PrePost_Labels/EnrolledMonths_WithEveryMonthsHasCodes/")
data_dir3 <- paste0(proj_dir, "4_RecurrDates_Outcome_Info/")
outdir   <- paste0(proj_dir, "7_PrePostLabels_AndAvailibility6mon/")


#########################################################################################################
#1.Load pateint event type and date data
#########################################################################################################
All_event_df <- read.xlsx(paste0(data_dir3,"4_All_event_df.xlsx"),sheet = 1)
SBCE_df      <- read.xlsx(paste0(data_dir3,"4_SBCE_Label.xlsx"),sheet = 1)


############################################################
#2. For enrollment data with possible month has no claims
############################################################
#Load all data
label_files1 <- list.files(data_dir1)
label_df1 <- do.call(rbind, lapply(paste0(data_dir1,label_files1), read.xlsx))

#Compute num of months
num_months_df1 <- compute_num_months_func(label_df1,All_event_df,SBCE_df)

write.xlsx(num_months_df1,paste0(outdir,"NUM_Months_AvalFlags_WithPossibleMonthsHasNoCodes.xlsx"))

# ############################################################
# #3. For enrollment data with month has at least one code
# ############################################################
# label_files2 <- list.files(data_dir2)
# label_df2 <- do.call(rbind, lapply(paste0(data_dir2,label_files2), read.xlsx))
# 
# #Compute num of months
# num_months_df2 <- compute_num_months_func(label_df2,All_event_df,SBCE_df)
# write.xlsx(num_months_df2,paste0(outdir,"NUM_Months_AvalFlags_WithEveryMonthsHasCodes.xlsx"))
# 


