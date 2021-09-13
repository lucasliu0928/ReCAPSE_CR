source("Recapse_Ultility.R")

#######################################################################################
### Multi-Core set-up
#######################################################################################
numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores

#######################################################################################
data_dir <- "/recapse/intermediate_data/"
outdir <- "/recapse/intermediate_data/3_CleanClaims_perPatient_perMonth/"

# #local
# data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"
# outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/3_CleanClaims_perPatient_perMonth/"

#######################################################################################
#Load IDs
#######################################################################################
ID_df <- read.xlsx(paste0(data_dir,"1_ID_Sources_Info/1_All_ID_Source.xlsx"),sheet = 1)
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
medicaid_heath_dir <- paste0(data_dir, "2_RawClaims_perPatient/Medicaid_HealthClaims/")
medicaid_pharm_dir <- paste0(data_dir, "2_RawClaims_perPatient/Medicaid_PharmClaims/")
medicare_dir <- paste0(data_dir, "2_RawClaims_perPatient/Medicare/")


#######################################################################################
#1.Get per day data
#######################################################################################
IDs_processed <-  as.numeric(gsub("_perMonth_Data.xlsx|ID","",list.files(outdir)))
if (length(IDs_processed) > 0 ){
  analysis_ID <- analysis_ID[-which(analysis_ID %in% IDs_processed)]
}
print(length(analysis_ID))

foreach (i = 1: length(analysis_ID)) %dopar% {
  curr_id <- analysis_ID[i]
  data_res <- read_allClaims(curr_id,medicaid_heath_dir,medicaid_pharm_dir,medicare_dir)
  
  #1. Read all raw claims, if not aval, return NULL
  medicaid_health_df <- data_res[[1]]
  medicaid_pharm_df  <- data_res[[2]]
  medicare_df        <- data_res[[3]]
  
  #2. Clean codes in raw claims
  #2.1 Medicaid Codes columns
  ICD_diag_cols1   <- c("CDE_DIAG_PRIM","CDE_DIAG_2","CDE_DIAG_3","CDE_DIAG_4") #ICD 9 or ICD10
  HCPCS_proc_cols1 <- c("CDE_PROC_PRIM")                                        #HCPCS
  AHFS_drug_cols1  <- c("CDE_THERA_CLS_AHFS")
  NDC_drug_cols1  <- c("CDE_NDC")
  all_medicaid_health_cols <- c(ICD_diag_cols1,HCPCS_proc_cols1)
  all_medicaid_pharms_cols <- c(AHFS_drug_cols1,NDC_drug_cols1)
  
  cleaned_medicaid_health_df <- clean_codes_inPerPtsData(medicaid_health_df,all_medicaid_health_cols,ICD_diag_cols1,HCPCS_proc_cols1)
  cleaned_medicaid_pharm_df <- clean_codes_inPerPtsData(medicaid_pharm_df,all_medicaid_pharms_cols,"","")
  
  #2.2 Medicare Code cols
  ICD_diag_cols2 <- paste0("DGNS_CD",seq(1,25))             #ICD9 or ICD10
  HCPCS_proc_cols2     <- "HCPCS_CD"                        #HCPCS
  ICD_procedure_cols2 <- paste0("PRCDRCD", seq(1,25,1))     #ICD9 or ICD10
  NDC_drug_cols2 <- c("NDC_CD","PROD_SRVC_ID")              #NDC
  all_medicare_cols <- c(ICD_diag_cols2,HCPCS_proc_cols2,ICD_procedure_cols2,NDC_drug_cols2)

  cleaned_medicare_df <- clean_codes_inPerPtsData(medicare_df,
                                                  all_medicare_cols,
                                                  c(ICD_diag_cols2,ICD_procedure_cols2),
                                                  HCPCS_proc_cols2)
  #2.Get all aval dates
  unique_dates1 <- as.character(unique(cleaned_medicaid_health_df[,"DTE_FIRST_SVC"]))
  unique_dates2 <- as.character(unique(cleaned_medicaid_pharm_df[,"DTE_FIRST_SVC"]))
  unique_dates3 <- as.character(unique(cleaned_medicare_df[,"claims_date"]))
  all_unique_dates <- unique(c(unique_dates1,unique_dates2,unique_dates3))
  
  #3.Get start and end dates (Always use frist day of the month)
  start_date <- ymd(paste0(c(strsplit(as.character(min(ymd(all_unique_dates))),"-")[[1]][1:2],"01"),collapse = "-"))
  end_date   <- ymd(paste0(c(strsplit(as.character(max(ymd(all_unique_dates))),"-")[[1]][1:2],"01"),collapse = "-")) + months(1) #ex: end date= 03/15, then changed to 03/01 + 1 month
  
  #4.Get month seq
  month_seqs <- ymd(seq(start_date,end_date,by="months"))
  
  
  #5. Get unique code per month
  unique_codes_PerMonth_list <- list(NA)
  for (t in 1:(length(month_seqs) - 1)){
    curr_start <- month_seqs[t]
    curr_end <- month_seqs[t+1]

    curr_month_df1_Health <- get_claims_inDateRange(cleaned_medicaid_health_df,"DTE_FIRST_SVC",curr_start,curr_end)
    curr_month_df1_Pharm  <- get_claims_inDateRange(cleaned_medicaid_pharm_df,"DTE_FIRST_SVC",curr_start,curr_end)
    curr_month_df2        <- get_claims_inDateRange(cleaned_medicare_df,"claims_date",curr_start,curr_end)
    
    curr_unique_ICD_DIAG  <- get_uniquecodes_perMonth("DIAG_ICD",curr_month_df1_Health,curr_month_df2,ICD_diag_cols1,ICD_diag_cols2)
    curr_unique_ICD_PROC  <- get_uniquecodes_perMonth("PROC_ICD",curr_month_df1_Health,curr_month_df2,NULL,ICD_procedure_cols2)
    curr_unique_HCPC_PROC <- get_uniquecodes_perMonth("PROC_HCPCS",curr_month_df1_Health,curr_month_df2,HCPCS_proc_cols1,HCPCS_proc_cols2)
    curr_unique_NDC_DRUG  <- get_uniquecodes_perMonth("DRUG_NDC",curr_month_df1_Pharm,curr_month_df2,NDC_drug_cols1,NDC_drug_cols2)
    curr_unique_AHFS_DRUG  <- get_uniquecodes_perMonth("DRUG_AHFS",curr_month_df1_Pharm,curr_month_df2,AHFS_drug_cols1,NULL)
    
    curr_unique_all_codes <- c(curr_unique_ICD_DIAG,curr_unique_ICD_PROC,curr_unique_HCPC_PROC,
                              curr_unique_NDC_DRUG,curr_unique_AHFS_DRUG)
    unique_codes_PerMonth_list[[t]] <- curr_unique_all_codes
  }
  
  #6. unique code all months
  all_unique_codes <- unique(unlist(unique_codes_PerMonth_list))
  all_unique_codes <- sort(all_unique_codes)
  
  #7. Unique code per month df (Row: month, Col: one code)
  perMonth_df <- as.data.frame(matrix(NA, nrow = length(month_seqs) - 1, ncol = length(all_unique_codes) + 3))
  colnames(perMonth_df) <- c("study_id","Month_Start","Month_End",all_unique_codes)
  perMonth_df[,"study_id"] <- curr_id
  for (t in 1:(length(month_seqs) - 1)){
    perMonth_df[t,"Month_Start"] <- as.character(month_seqs[t])
    perMonth_df[t,"Month_End"]   <- as.character(month_seqs[t+1])
    curr_codes <- unique_codes_PerMonth_list[[t]]
    perMonth_df[t, curr_codes] <- 1
  }
  
  write.xlsx(perMonth_df,paste0(outdir,"ID",curr_id,"_","perMonth_Data.xlsx"))
}




