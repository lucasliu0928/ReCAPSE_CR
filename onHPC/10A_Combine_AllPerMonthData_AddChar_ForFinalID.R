library(openxlsx)
library(data.table)
library(lubridate)
library(parallel)
library(foreach)
library(doParallel)

#onHPC
perday_dir <- "/recapse/intermediate_data/3_perDay_PerPatientData/"
perMonth_dir <- "/recapse/intermediate_data/6_perMonthData_inValidMonth_perPatientData/"
outdir <- "/recapse/intermediate_data/"

# #local
# perday_dir <- "/Users/lucasliu/Desktop/intermediate_data/3_perDay_PerPatientData/"
# perMonth_dir <- "/Users/lucasliu/Desktop/intermediate_data/6_perMonthData_inValidMonth_perPatientData/"
# outdir <- "/Users/lucasliu/Desktop/intermediate_data/"


################################################################################ 
#1. Load patient level charateristics
################################################################################ 
Patient_Char_df <- read.xlsx(paste0(outdir,"8_PatientLevel_charecteristics.xlsx"),sheet = 1)

################################################################################ 
#2. Load Anlaysis ID
################################################################################ 
FinalID_df <- read.xlsx(paste0(outdir,"9_Final_Analysis_ID.xlsx"),sheet = 1)
Final_IDs <- unique(FinalID_df$study_id)

#########################################################################################################
#3. Load outcome/event type data
#########################################################################################################
updated_All_event_df <- read.xlsx(paste0(outdir,"4_updated_All_event_df.xlsx"),sheet = 1)


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

print(paste0("Total number of monthes:", nrow(All_perMonthData_df)))


################################################################################ 
#4. Add charatersitics for the machine learning model (pareall)
################################################################################
#site_cols<- paste0("C",seq(501,509,1))
foreach (i = 1: length(Final_IDs)) %dopar% {
  i <- 1
  curr_id <- Final_IDs[i]
  
  #curr per month data 
  curr_perMonth_data <- All_perMonthData_df[which(All_perMonthData_df$study_id == curr_id),]
  #curr patient level char
  curr_pt_level_char_df <- Patient_Char_df[which(Patient_Char_df$study_id == curr_id),]
  
  
}

################################################################################
#4. Add charatersitics for the machine learning model
################################################################################
site_cols<- paste0("C",seq(501,509,1))
month_level_char_df <- as.data.frame(matrix(NA, nrow =nrow(All_perMonthData_df) ,ncol = 32))
colnames(month_level_char_df) <- c("study_id","num_claims","Age","months_since_dx",
                                   "Race", site_cols,
                                   "regional","Grade","Laterality",
                                   "er_stat","pr_stat","her2_stat","surg_prim_site",
                                   "DAJCC_T","DAJCC_M","DAJCC_N",
                                   "reg_age_at_dx","reg_nodes_exam","reg_nodes_pos",
                                   "cs_tum_size","cs_tum_ext","cs_tum_nodes",
                                   "months_to_second_event",
                                   "y_PRE_OR_POST_2ndEvent")
for (i in 1:nrow(All_perMonthData_df)){
  if (i %% 1000 ==0 ){
    print(i)
  }
  curr_id <- All_perMonthData_df[i,"study_id"]
  month_level_char_df[i,"study_id"] <- curr_id

  #curr patient level char
  curr_pt_level_char_df <- Patient_Char_df[which(Patient_Char_df$study_id == curr_id),]

  #add features
  feature_cols <- c("Race","regional","Grade","Laterality",
                    "er_stat","pr_stat","her2_stat","surg_prim_site",
                    "DAJCC_T","DAJCC_M","DAJCC_N",
                    "reg_age_at_dx","reg_nodes_exam","reg_nodes_pos",
                    "cs_tum_size","cs_tum_ext","cs_tum_nodes")
  month_level_char_df[i,feature_cols] <- curr_pt_level_char_df[,feature_cols]

  #curr site
  curr_site <- curr_pt_level_char_df[,"Site"]
  month_level_char_df[i,curr_site] <- 1 #curr site col = 1
  month_level_char_df[i, site_cols[which(site_cols != curr_site)]] <- 0 #othre site 0

  #curr event df
  curr_event <- updated_All_event_df[which(updated_All_event_df[,"study_id"] == curr_id),]
  #first and 2nd event and death date
  curr_1stevent_date <- mdy(curr_event[,"Date_1st_Event"])
  curr_2ndevent_date <- mdy(curr_event[,"Date_2nd_Event"])

  #date of birth
  curr_diag_year <- as.numeric(unlist(strsplit(as.character(curr_1stevent_date),split = "-"))[[1]])
  curr_age_at_diags <- curr_pt_level_char_df[,"reg_age_at_dx"]
  curr_dob <- curr_diag_year - curr_age_at_diags

  #curernt month
  curr_month <- ymd(All_perMonthData_df[i,"Month_Start"])
  curr_month_yr <- as.numeric(unlist(strsplit(as.character(curr_month),split = "-"))[[1]])

  month_level_char_df[i,"Age"] <- curr_month_yr - curr_dob
  month_level_char_df[i,"months_since_dx"] <- as.numeric(difftime(curr_month,curr_1stevent_date,units = "days"))/30 #converted to month
  if (is.na(curr_2ndevent_date) == F){
    month_level_char_df[i,"months_to_second_event"] <- as.numeric(difftime(curr_month,curr_2ndevent_date,units = "days"))/30 #converted to month

  }else{
    month_level_char_df[i,"months_to_second_event"] <- NA
  }

  if (month_level_char_df[i,"months_to_second_event"] < 0 | is.na(month_level_char_df[i,"months_to_second_event"])==T){
    month_level_char_df[i,"y_PRE_OR_POST_2ndEvent"] <- 0
  }else{
    month_level_char_df[i,"y_PRE_OR_POST_2ndEvent"] <- 1
  }

  #number of claims in each month
  #1.get per day file
  curr_perday_file <- paste0(perday_dir,"ID",curr_id,"_perDay_Data.xlsx")

  if (file.exists(curr_perday_file) == T){
    curr_perday_df <- read.xlsx(curr_perday_file,sheet = 1)
    curr_claims <- which(ymd(curr_perday_df$claims_date) >= curr_month & ymd(curr_perday_df$claims_date) < curr_month + days(30))
    month_level_char_df[i,"num_claims"] <- length(curr_claims)

  }else{
    month_level_char_df[i,"num_claims"] <- 0
  }

}


################################################################################ 
#5. Combine code sand char
################################################################################ 
comb_df <- cbind(All_perMonthData_df,month_level_char_df[,-1])

write.xlsx(comb_df,paste0(outdir,"10_PerMonthData_WithMonthChar_df.xlsx"))

