library(openxlsx)
library(data.table)
library(lubridate)
library(parallel)
library(foreach)
library(doParallel)

numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores


#onHPC
perday_dir <- "/recapse/intermediate_data/3A_perDay_PerPatientData/"
perMonth_dir <- "/recapse/intermediate_data/6_perMonthData_inValidMonth_perPatientData_V2_nonuniquecodes/" #6.V2 has non_unique codes in one month
data_dir <- "/recapse/intermediate_data/"
outdir <- "/recapse/intermediate_data/10_perMonthData_withChar_V2_nonuniquecodes/" #V2 

# #local
# perday_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/3A_perDay_PerPatientData/"
# perMonth_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/6_perMonthData_inValidMonth_perPatientData_V2_nonuniquecodes/"
# data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"
# outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/10_perMonthData_withChar_V2_nonuniquecodes/"


################################################################################ 
#1. Load patient level charateristics
################################################################################ 
Patient_Char_df <- read.xlsx(paste0(data_dir,"8_PatientLevel_charecteristics.xlsx"),sheet = 1)

################################################################################ 
#2. Load Anlaysis ID
################################################################################ 
FinalID_df <- read.xlsx(paste0(data_dir,"9_Final_Analysis_ID.xlsx"),sheet = 1)
Final_IDs <- unique(FinalID_df$study_id) #23378

#########################################################################################################
#3. Load outcome/event type data
#########################################################################################################
updated_All_event_df <- read.xlsx(paste0(data_dir,"4_updated_All_event_df.xlsx"),sheet = 1)


################################################################################ 
#4. Combine all per month data into one dataframe
################################################################################ 
All_perMonthData_list <- list(NA)
for (i in 1:length(Final_IDs)){
  curr_id <- Final_IDs[i]
  
  curr_perMonth_file <- paste0(perMonth_dir,"ID",curr_id,"_perMonthData_inValidMonth.xlsx")
  
  if (file.exists(curr_perMonth_file) == T){
    curr_perMonth_df <- read.xlsx(curr_perMonth_file,sheet = 1)
  }else{
    curr_perMonth_df <- NULL
  }
  
  All_perMonthData_list[[i]] <-curr_perMonth_df
}

All_perMonthData_df <- do.call(rbind,All_perMonthData_list)

print(paste0("Total number of monthes:", nrow(All_perMonthData_df))) #2,119,433


########################################################################################################################
#Use the following code to run in case out of memory when procssing all at one time
########################################################################################################################
files_outputed<- list.files(outdir)
ID_outputed <- as.numeric(gsub("_PerMonthData_WithMonthChar_df.xlsx|ID","",files_outputed))

indxes_processed <- which(Final_IDs %in% ID_outputed)

if (length(indxes_processed) > 0){ #if any Id has been process
  Final_IDs <- Final_IDs[-indxes_processed]
}

print(paste0("To Process IDs: ",length(Final_IDs)))


################################################################################ 
#4. Add charatersitics for the machine learning model (pareall)
################################################################################
site_cols<- paste0("C50",seq(0,9,1))

foreach (i = 1: length(Final_IDs)) %dopar% {
  curr_id <- Final_IDs[i]
  
  #curr per month data 
  curr_perMonth_data <- All_perMonthData_df[which(All_perMonthData_df$study_id == curr_id),]
  #curr patient level char
  curr_pt_level_char_df <- Patient_Char_df[which(Patient_Char_df$study_id == curr_id),]
  
  #construct per patient month level data
  curr_month_level_char_df <- as.data.frame(matrix(NA, nrow =nrow(curr_perMonth_data) ,ncol = 33))
  colnames(curr_month_level_char_df) <- c("study_id","num_claims","Age","months_since_dx",
                                     "Race", site_cols,
                                     "regional","Grade","Laterality",
                                     "er_stat","pr_stat","her2_stat","surg_prim_site",
                                     "DAJCC_T","DAJCC_M","DAJCC_N",
                                     "reg_age_at_dx","reg_nodes_exam","reg_nodes_pos",
                                     "cs_tum_size","cs_tum_ext","cs_tum_nodes",
                                     "months_to_second_event",
                                     "y_PRE_OR_POST_2ndEvent")
  
  curr_month_level_char_df[,"study_id"] <- curr_id
  
  #add features
  feature_cols <- c("Race","regional","Grade","Laterality",
                    "er_stat","pr_stat","her2_stat","surg_prim_site",
                    "DAJCC_T","DAJCC_M","DAJCC_N",
                    "reg_age_at_dx","reg_nodes_exam","reg_nodes_pos",
                    "cs_tum_size","cs_tum_ext","cs_tum_nodes")
  curr_month_level_char_df[,feature_cols] <- curr_pt_level_char_df[,feature_cols]
  
  #curr site
  curr_site <- curr_pt_level_char_df[,"Site"]
  curr_month_level_char_df[,curr_site] <- 1 #curr site col = 1
  curr_month_level_char_df[, site_cols[which(site_cols != curr_site)]] <- 0 #othre site 0
  
  #curr event df
  curr_event <- updated_All_event_df[which(updated_All_event_df[,"study_id"] == curr_id),]
  #first and 2nd event and death date
  curr_1stevent_date <- mdy(curr_event[,"Date_1st_Event"])
  curr_2ndevent_date <- mdy(curr_event[,"Date_2nd_Event"])
  
  #date of birth
  curr_dob <-   mdy(curr_pt_level_char_df$date_Birth)

  
  #for ecurr_dobach  month
  for (j in 1:nrow(curr_month_level_char_df)){
    curr_month <- ymd(curr_perMonth_data[j,"Month_Start"])

    curr_month_level_char_df[j,"Age"] <- as.numeric(difftime(curr_month,curr_dob,units = "days")/365)
    curr_month_level_char_df[j,"months_since_dx"] <- as.numeric(difftime(curr_month,curr_1stevent_date,units = "days"))/30 #converted to month
    if (is.na(curr_2ndevent_date) == F){
      curr_month_level_char_df[j,"months_to_second_event"] <- as.numeric(difftime(curr_month,curr_2ndevent_date,units = "days"))/30 #converted to month
      
    }else{
      curr_month_level_char_df[j,"months_to_second_event"] <- NA
    }
    
    #get outcome label
    if (curr_month_level_char_df[j,"months_to_second_event"] < 0 | is.na(curr_month_level_char_df[j,"months_to_second_event"])==T){
      curr_month_level_char_df[j,"y_PRE_OR_POST_2ndEvent"] <- 0 #pre
    }else{
      curr_month_level_char_df[j,"y_PRE_OR_POST_2ndEvent"] <- 1 #post
    }
    
    #number of claims in each month
    curr_perday_file <- paste0(perday_dir,"ID",curr_id,"_perDay_Data.xlsx") #    #1.get per day file
    
    if (file.exists(curr_perday_file) == T){
      curr_perday_df <- read.xlsx(curr_perday_file,sheet = 1)
      curr_claims <- which(ymd(curr_perday_df$claims_date) >= curr_month & ymd(curr_perday_df$claims_date) < curr_month + days(30))
      curr_month_level_char_df[j,"num_claims"] <- length(curr_claims)
      
    }else{
      curr_month_level_char_df[j,"num_claims"] <- 0
    }
    
   }
  
  curr_comb_df <- cbind(curr_perMonth_data,curr_month_level_char_df[,-1])

  write.xlsx(curr_comb_df,paste0(outdir,"ID",curr_id,"_","PerMonthData_WithMonthChar_df.xlsx"))
  
}


