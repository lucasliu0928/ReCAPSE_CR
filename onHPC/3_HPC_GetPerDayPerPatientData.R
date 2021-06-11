library(openxlsx)
library(data.table)
library(lubridate)

clean_code_func <-function(list_of_codes){
  #list_of_codes <- HCUP_comb[,"Code"]
  
  #1.omitting any codes with non-alphanumeric characters,
  updated_list_of_codes<- gsub("[^[:alnum:]]", " ", list_of_codes)
  
  #2. space
  updated_list_of_codes <- trimws(updated_list_of_codes, which = c("both"), whitespace = "[ \t\r\n]")
  
  #3.decimal
  updated_list_of_codes <- gsub("\\.","",updated_list_of_codes)
  updated_list_of_codes <- gsub("[[:space:]]", "", updated_list_of_codes) #after\\. might resulting in sapce
  
  #Check the number of charter for each code
  n_char <- NA
  for (c in 1:length(updated_list_of_codes)){
    cur_code <- updated_list_of_codes[c]
    n_char[c] <- nchar(cur_code)
  }
  
  #for codes less than 3 characters long
  #.if it is non-numeric, then exclude codes 
  # if it is numeric , then prepending '0'
  l3_idxes <- which(n_char< 3)
  l3_codes <- updated_list_of_codes[l3_idxes]
  if(length(l3_codes) > 0){
    updated_code <- NA
    for (c in 1:length(l3_codes)){
      cur_code <- l3_codes[c]
      if(is.na(as.numeric(cur_code)==T)){ #if NA, then it is non-numeric
        updated_code[c] <- NA #remove
      }else{ #it is numeric , then prepending '0'
        updated_code[c] <- paste0("0",cur_code)
      }
    }
    
    updated_list_of_codes[l3_idxes] <- updated_code
    
    #This might result in NAs(from converting to numeric when it is char) from orignal 
  }
  
  return(updated_list_of_codes)
}

#this function cleans codes in the original claims data by columns 
clean_code_columns <-function(claims_df, col_to_clean){
  #claims_df <- curr_health_df
  #col_to_clean <- c(diag_cols,proc_cols)
  for (j in 1:length(col_to_clean)){
    curr_col <- col_to_clean[j]
    claims_df[,curr_col] <- clean_code_func(claims_df[,curr_col])
  }  
  return(claims_df)
}


remove_NA_func<- function(curr_day_codes){
  na_idxes <- which(curr_day_codes == "" | is.na(curr_day_codes) == TRUE)
  if (length(na_idxes) > 0){
    curr_day_codes <- curr_day_codes[-na_idxes]
  }
  return(curr_day_codes)
}

#These fucntions get per day cleaned codes for all avaiable dates for one pateint
get_perDay_medicaid <- function(patient_ID,medicaid_heath_dir,medicaid_pharm_dir){
  #Codes columns for medicaid
  medicaid_diag_cols <- c("CDE_DIAG_PRIM","CDE_DIAG_2","CDE_DIAG_3","CDE_DIAG_4")
  medicaid_proc_cols <- c("CDE_PROC_PRIM")
  medicaid_drug_cols <- c("CDE_THERA_CLS_AHFS","CDE_NDC")
  
  #I. Load date and clean codes in orignal data
  #check if ID in the dir
  IDs_has_health_files <- as.numeric(gsub("_all_medicaid_healthclaims.xlsx|ID","",list.files(medicaid_heath_dir)))
  if (patient_ID %in% IDs_has_health_files){
    #1.read original health_claims to get proc and diag codes
    health_df <- read.xlsx(paste0(medicaid_heath_dir,"ID",patient_ID,"_all_medicaid_healthclaims.xlsx"),sheet = 1,detectDates = T)
    #clean codes in health_claims
    cleaned_healthClaims <- clean_code_columns(health_df,c(medicaid_diag_cols,medicaid_proc_cols))
  }else{
    cleaned_healthClaims <- NULL
  }
  #2.get original pharmcy_claims to get drug codes
  IDs_has_pharm_files <- as.numeric(gsub("_all_medicaid_pharmclaims.xlsx|ID","",list.files(medicaid_pharm_dir)))
  if (patient_ID %in% IDs_has_pharm_files){
      pharm_df <- read.xlsx(paste0(medicaid_pharm_dir,"ID",patient_ID,"_all_medicaid_pharmclaims.xlsx"),sheet = 1,detectDates = T)
      #clean codes in pharmcy_claims
      cleaned_PharmClaims <- clean_code_columns(pharm_df,medicaid_drug_cols)
  }else{
    cleaned_PharmClaims <- NULL
  }
  
  #II.get all dates
  if (is.null(cleaned_PharmClaims) == F & is.null(cleaned_healthClaims) == F){
    cleaned_healthClaims$DTE_FIRST_SVC <- mdy(cleaned_healthClaims$DTE_FIRST_SVC)
    cleaned_PharmClaims$DTE_FIRST_SVC <- dmy(cleaned_PharmClaims$DTE_FIRST_SVC)
    aval_dates <- unique(c(cleaned_healthClaims[,"DTE_FIRST_SVC"],cleaned_PharmClaims[,"DTE_FIRST_SVC"]))
  }else if (is.null(cleaned_PharmClaims) == T & is.null(cleaned_healthClaims) == F ){
    cleaned_healthClaims$DTE_FIRST_SVC <- mdy(cleaned_healthClaims$DTE_FIRST_SVC)
    aval_dates <- unique(cleaned_healthClaims[,"DTE_FIRST_SVC"])
  }else if(is.null(cleaned_PharmClaims) == F & is.null(cleaned_healthClaims) == T){
    cleaned_PharmClaims$DTE_FIRST_SVC <- dmy(cleaned_PharmClaims$DTE_FIRST_SVC)
    aval_dates <- unique(cleaned_PharmClaims[,"DTE_FIRST_SVC"])
  }
  
  #III.per day table
  per_day_table <- as.data.frame(matrix(NA, nrow = length(aval_dates),ncol = 5))
  colnames(per_day_table) <- c("study_id","claims_date","Diag_Codes","Proc_Codes","Drug_Codes")
  for (j in 1:length(aval_dates)){
    per_day_table[j,"study_id"] <- patient_ID
    
    curr_date <- aval_dates[j]
    per_day_table[j,"claims_date"] <- as.character(curr_date)
    
    #curr day code list and remove all blanks and NAs
    if (is.null(cleaned_healthClaims) == F){ #if has health claims
      curr_day_healthclaims <- cleaned_healthClaims[which(cleaned_healthClaims[,"DTE_FIRST_SVC"] == curr_date),]
      curr_day_diag <- remove_NA_func(unique(unlist(curr_day_healthclaims[,medicaid_diag_cols])))
      curr_day_proc <- remove_NA_func(unique(unlist(curr_day_healthclaims[,medicaid_proc_cols])))
    }else{
      curr_day_diag <- NULL
      curr_day_proc <- NULL
    }
    
    if (is.null(cleaned_PharmClaims) == F){ #if has pharms claims
      curr_day_drugclaims   <- cleaned_PharmClaims[which(cleaned_PharmClaims[,"DTE_FIRST_SVC"] == curr_date),]
      curr_day_drug <- remove_NA_func(unique(unlist(curr_day_drugclaims[,medicaid_drug_cols])))
    }else{
      curr_day_drug <- NULL
    }

    
    #Concatenate all codes
    if (length(curr_day_diag) > 0){
      per_day_table[j,"Diag_Codes"] <- paste0(curr_day_diag,collapse = "$$$$")
    }
    if (length(curr_day_proc) > 0){
      per_day_table[j,"Proc_Codes"] <- paste0(curr_day_proc,collapse = "$$$$")
    }
    if (length(curr_day_drug) > 0){
      per_day_table[j,"Drug_Codes"] <- paste0(curr_day_drug,collapse = "$$$$")
    }
    
    
  }
  
  #remove the dates that does not have any type of codes
  na_ct_perDay <- rowSums(is.na(per_day_table[,c("Diag_Codes","Proc_Codes","Drug_Codes")]))
  indexes_toremove <- which(na_ct_perDay == 3)
  if (length(indexes_toremove) >0 ){
    per_day_table <- per_day_table[-indexes_toremove,]
  }
  
  return(per_day_table)
}

get_perDay_medicare <- function(patient_ID,medicare_dir){
  #Codes columns for medicare
  medicare_diag_cols <- paste0("DGNS_CD",seq(1,25))
  medicare_proc_cols <- c(paste0("PRCDRCD",seq(1,25)),"HCPCS_CD")
  medicare_drug_cols <- c("NDC_CD","PROD_SRVC_ID")
  
  
  #I. Load date and clean codes in orignal data
  #1.read original claim to get proc and diag codes
  claim_df <- read.xlsx(paste0(medicare_dir,"ID",patient_ID,"_all_medicare_claims.xlsx"),sheet = 1,detectDates = T)
  claim_df$claims_date <- mdy(claim_df$claims_date)
  #clean codes in health_claims
  cleaned_Claims_df <- clean_code_columns(claim_df,c(medicare_diag_cols,medicare_proc_cols,medicare_drug_cols))
  
  #II.get all dates
  aval_dates <- unique(cleaned_Claims_df[,"claims_date"])
  
  #III.per day table
  per_day_table <- as.data.frame(matrix(NA, nrow = length(aval_dates),ncol = 5))
  colnames(per_day_table) <- c("study_id","claims_date","Diag_Codes","Proc_Codes","Drug_Codes")
  for (j in 1:length(aval_dates)){
    per_day_table[j,"study_id"] <- patient_ID
    
    curr_date <- aval_dates[j]
    per_day_table[j,"claims_date"] <- as.character(curr_date)
    
    curr_day_claims<- cleaned_Claims_df[which(cleaned_Claims_df[,"claims_date"] == curr_date),]
    
    #curr day code list
    curr_day_diag <- unique(unlist(curr_day_claims[,medicare_diag_cols]))
    curr_day_proc <- unique(unlist(curr_day_claims[,medicare_proc_cols]))
    curr_day_drug <- unique(unlist(curr_day_claims[,medicare_drug_cols]))
    
    #remove all blanks and NAs
    curr_day_diag <- remove_NA_func(curr_day_diag)
    curr_day_proc <- remove_NA_func(curr_day_proc)
    curr_day_drug <- remove_NA_func(curr_day_drug)
    
    #Concatenate all codes
    if (length(curr_day_diag) > 0){
      per_day_table[j,"Diag_Codes"] <- paste0(curr_day_diag,collapse = "$$$$")
    }
    if (length(curr_day_proc) > 0){
      per_day_table[j,"Proc_Codes"] <- paste0(curr_day_proc,collapse = "$$$$")
    }
    if (length(curr_day_drug) > 0){
      per_day_table[j,"Drug_Codes"] <- paste0(curr_day_drug,collapse = "$$$$")
    }
    
    
  }
  
  #remove the dates that does not have any type of codes
  na_ct_perDay <- rowSums(is.na(per_day_table[,c("Diag_Codes","Proc_Codes","Drug_Codes")]))
  indexes_toremove <- which(na_ct_perDay == 3)
  if (length(indexes_toremove) >0 ){
    per_day_table <- per_day_table[-indexes_toremove,]
  }
  
  return(per_day_table)
}

get_perDay_both <- function(patient_ID,medicaid_heath_dir,medicaid_pharm_dir,medicare_dir){
  #Codes columns for medicaid
  medicaid_diag_cols <- c("CDE_DIAG_PRIM","CDE_DIAG_2","CDE_DIAG_3","CDE_DIAG_4")
  medicaid_proc_cols <- c("CDE_PROC_PRIM")
  medicaid_drug_cols <- c("CDE_THERA_CLS_AHFS","CDE_NDC")
  #Codes columns for medicare
  medicare_diag_cols <- paste0("DGNS_CD",seq(1,25))
  medicare_proc_cols <- c(paste0("PRCDRCD",seq(1,25)),"HCPCS_CD")
  medicare_drug_cols <- c("NDC_CD","PROD_SRVC_ID")
  
  
  #I. Load date and clean codes in orignal data
  #check if ID in the dir
  IDs_has_health_files <- as.numeric(gsub("_all_medicaid_healthclaims.xlsx|ID","",list.files(medicaid_heath_dir)))
  if (patient_ID %in% IDs_has_health_files){
    #1.read original health_claims to get proc and diag codes
    health_df <- read.xlsx(paste0(medicaid_heath_dir,"ID",patient_ID,"_all_medicaid_healthclaims.xlsx"),sheet = 1,detectDates = T)
    #clean codes in health_claims
    cleaned_healthClaims <- clean_code_columns(health_df,c(medicaid_diag_cols,medicaid_proc_cols))
  }else{
    cleaned_healthClaims <- NULL
  }
  #2.get original pharmcy_claims to get drug codes
  IDs_has_pharm_files <- as.numeric(gsub("_all_medicaid_pharmclaims.xlsx|ID","",list.files(medicaid_pharm_dir)))
  if (patient_ID %in% IDs_has_pharm_files){
    pharm_df <- read.xlsx(paste0(medicaid_pharm_dir,"ID",patient_ID,"_all_medicaid_pharmclaims.xlsx"),sheet = 1,detectDates = T)
    #clean codes in pharmcy_claims
    cleaned_PharmClaims <- clean_code_columns(pharm_df,medicaid_drug_cols)
  }else{
    cleaned_PharmClaims <- NULL
  }

  #3.read original medicare claim to get proc and diag codes
  claim_df <- read.xlsx(paste0(medicare_dir,"ID",patient_ID,"_all_medicare_claims.xlsx"),sheet = 1,detectDates = T)
  claim_df$claims_date <- mdy(claim_df$claims_date)
  #clean codes in health_claims
  cleaned_Claims_df <- clean_code_columns(claim_df,c(medicare_diag_cols,medicare_proc_cols,medicare_drug_cols))
  
  
  #II.get all dates, it is possible in medicaid, patient only has either pharm or health claims
  if (is.null(cleaned_PharmClaims) == F & is.null(cleaned_healthClaims) == F){ 
    cleaned_healthClaims$DTE_FIRST_SVC <- mdy(cleaned_healthClaims$DTE_FIRST_SVC)
    cleaned_PharmClaims$DTE_FIRST_SVC <- dmy(cleaned_PharmClaims$DTE_FIRST_SVC)
    aval_dates <- unique(c(cleaned_healthClaims[,"DTE_FIRST_SVC"],cleaned_PharmClaims[,"DTE_FIRST_SVC"],cleaned_Claims_df[,"claims_date"]))
  }else if (is.null(cleaned_PharmClaims) == T & is.null(cleaned_healthClaims) == F ){
    cleaned_healthClaims$DTE_FIRST_SVC <- mdy(cleaned_healthClaims$DTE_FIRST_SVC)
    aval_dates <- unique(c(cleaned_healthClaims[,"DTE_FIRST_SVC"],cleaned_Claims_df[,"claims_date"]))
    
  }else if(is.null(cleaned_PharmClaims) == F & is.null(cleaned_healthClaims) == T){
    cleaned_PharmClaims$DTE_FIRST_SVC <- dmy(cleaned_PharmClaims$DTE_FIRST_SVC)
    aval_dates <- unique(c(cleaned_PharmClaims[,"DTE_FIRST_SVC"],cleaned_Claims_df[,"claims_date"]))
  }
  
  #III.per day table
  per_day_table <- as.data.frame(matrix(NA, nrow = length(aval_dates),ncol = 5))
  colnames(per_day_table) <- c("study_id","claims_date","Diag_Codes","Proc_Codes","Drug_Codes")
  for (j in 1:length(aval_dates)){
    per_day_table[j,"study_id"] <- patient_ID
    
    curr_date <- aval_dates[j]
    per_day_table[j,"claims_date"] <- as.character(curr_date)
    
    #curr day code list and remove all blanks and NAs
    #medicare
    curr_day_claims<- cleaned_Claims_df[which(cleaned_Claims_df[,"claims_date"] == curr_date),]
    
    #medicaid
    if (is.null(cleaned_healthClaims) == F){ #if has health claims in medcaid and medicare
      curr_day_healthclaims <- cleaned_healthClaims[which(cleaned_healthClaims[,"DTE_FIRST_SVC"] == curr_date),]
      curr_day_diag <- remove_NA_func(unique(c(unlist(curr_day_healthclaims[,medicaid_diag_cols]),unlist(curr_day_claims[,medicare_diag_cols]))))
      curr_day_proc <- remove_NA_func(unique(c(unlist(curr_day_healthclaims[,medicaid_proc_cols]),unlist(curr_day_claims[,medicare_proc_cols]))))
      
    }else{#only medicare
      curr_day_diag <- remove_NA_func(unique(unlist(curr_day_claims[,medicare_diag_cols]))) 
      curr_day_proc <- remove_NA_func(unique(unlist(curr_day_claims[,medicare_proc_cols])))
    }
    
    if (is.null(cleaned_PharmClaims) == F){ #if has pharms claims and medicare
      curr_day_drugclaims   <- cleaned_PharmClaims[which(cleaned_PharmClaims[,"DTE_FIRST_SVC"] == curr_date),]
      curr_day_drug <- remove_NA_func(unique(c(unlist(curr_day_drugclaims[,medicaid_drug_cols]),unlist(curr_day_claims[,medicare_drug_cols]))))
      
    }else{ #only medicare
      curr_day_drug <- remove_NA_func(unique(unlist(curr_day_claims[,medicare_drug_cols])))
    }
    

    #Concatenate all codes
    if (length(curr_day_diag) > 0){
      per_day_table[j,"Diag_Codes"] <- paste0(curr_day_diag,collapse = "$$$$")
    }
    if (length(curr_day_proc) > 0){
      per_day_table[j,"Proc_Codes"] <- paste0(curr_day_proc,collapse = "$$$$")
    }
    if (length(curr_day_drug) > 0){
      per_day_table[j,"Drug_Codes"] <- paste0(curr_day_drug,collapse = "$$$$")
    }
    
    
  }
  
  #remove the dates that does not have any type of codes
  na_ct_perDay <- rowSums(is.na(per_day_table[,c("Diag_Codes","Proc_Codes","Drug_Codes")]))
  indexes_toremove <- which(na_ct_perDay == 3)
  if (length(indexes_toremove) >0 ){
    per_day_table <- per_day_table[-indexes_toremove,]
  }
  
  return(per_day_table)
}

data_dir <- "/recapse/intermediate_data/"
outdir <- "/recapse/intermediate_data/perDay_PerPatientData/"

# local
# data_dir <- "/Users/lucasliu/Desktop/intermediate_data/"
# outdir <- "/Users/lucasliu/Desktop/intermediate_data/perDay_PerPatientData/"

#######################################################################################
#Load IDs
#######################################################################################
ID_df <- read.xlsx(paste0(data_dir,"All_ID_Source.xlsx"),sheet = 1)
#remove the IDs does not have any claims
ID_df <- ID_df[-which(ID_df[,"in_Medicare"] == 0 & ID_df[,"in_Medicaid"] == 0),]

#get analysis ID by source
analysis_ID <- unique(ID_df$Kcr_ID)
medicare_only_ID <- ID_df[which(ID_df[,"in_Medicare"] == 1 & ID_df[,"in_Medicaid"] == 0),"Kcr_ID"]
medicaid_only_ID <- ID_df[which(ID_df[,"in_Medicare"] == 0 & ID_df[,"in_Medicaid"] == 1 ),"Kcr_ID"]
both_ID <- ID_df[which(ID_df[,"in_Medicare"] == 1 & ID_df[,"in_Medicaid"] == 1 ),"Kcr_ID"]

#######################################################################################
#Get unique codes per day from different sources and combine them 
#######################################################################################
medicaid_heath_dir <- paste0(data_dir, "perPatientData/Medicaid_HealthClaims/")
medicaid_pharm_dir <- paste0(data_dir, "perPatientData/Medicaid_PharmClaims/")
medicare_dir <- paste0(data_dir, "perPatientData/Medicare/")


# start_t <- Sys.time() #inital time
# for (i in 1590:length(analysis_ID)){
#     if (i %% 500 == 0){
#       end_t <- Sys.time()
#       time_d <- end_t - start_t
#       print(paste0(i,", time used:",round(time_d,2)))
#       start_t <- Sys.time()
#     }
#     curr_id <- analysis_ID[i]
# 
#     if (curr_id %in% medicare_only_ID){ #medicare
#       curr_perDay_data <- get_perDay_medicare(curr_id,medicare_dir)
# 
#     }else if (curr_id %in% medicaid_only_ID){ #medcaid
#       curr_perDay_data <- get_perDay_medicaid(curr_id,medicaid_heath_dir,medicaid_pharm_dir)
#       
#     }else if (curr_id %in% both_ID){# from two source and combine
#       curr_perDay_data <- get_perDay_both(curr_id,medicaid_heath_dir,medicaid_pharm_dir,medicare_dir) #298
#     }
#     
#     write.xlsx(curr_perDay_data,paste0(outdir,"ID",curr_id,"_","perDay_Data.xlsx"))
# }

library(parallel)
library(foreach)
library(doParallel)

#######################################################################################
#1.Process Medicaid pharm claims
#######################################################################################
numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores

IDs_processed <-  as.numeric(gsub("_perDay_Data.xlsx|ID","",list.files(outdir)))

analysis_ID <- analysis_ID[-which(analysis_ID %in% IDs_processed)]
print(length(analysis_ID))
foreach (i = 1: length(analysis_ID)) %dopar% {
  curr_id <- analysis_ID[i]
  
  if (curr_id %in% medicare_only_ID){ #medicare
    curr_perDay_data <- get_perDay_medicare(curr_id,medicare_dir)
    
  }else if (curr_id %in% medicaid_only_ID){ #medcaid
    curr_perDay_data <- get_perDay_medicaid(curr_id,medicaid_heath_dir,medicaid_pharm_dir)
    
  }else if (curr_id %in% both_ID){# from two source and combine
    curr_perDay_data <- get_perDay_both(curr_id,medicaid_heath_dir,medicaid_pharm_dir,medicare_dir) #298
  }
  
  write.xlsx(curr_perDay_data,paste0(outdir,"ID",curr_id,"_","perDay_Data.xlsx"))
}
