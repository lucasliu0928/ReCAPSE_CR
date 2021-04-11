library(stringr)
library(lubridate)
proj_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/" 
source(paste0(proj_dir,"ReCAPSE_Code/Ultilities.R"))

#1. This scrpte read seperated claims data files, and generate per day drug, procedure and diag codes
data_dir <- paste0(proj_dir, "ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/")
out_dir <- paste0(proj_dir,"/ReCAPSE_Intermediate_Data/0318_21/Medicaid_intermediate_Data/Medicaid_PateintPerDay_Data/")

#Load claims
health_claims<- read.csv(paste0(data_dir,"kcr_medicaid_healthclaims_fb0015.csv"),stringsAsFactors = F)
health_claims$DTE_FIRST_SVC <- mdy(health_claims$DTE_FIRST_SVC)

pharm_claims<- read.csv(paste0(data_dir,"KCR_MEDICAID_PHARMCLAIMS_FB0015.csv"),stringsAsFactors = F)
pharm_claims$DTE_FIRST_SVC <- dmy(pharm_claims$DTE_FIRST_SVC) #convert date col

#Analysis Id is the Id exsit in the two files
analysis_IDs <- unique(c(health_claims[,"study_id"],pharm_claims[,"study_id"])) #13021

#Codes columns
diag_cols <- c("CDE_DIAG_PRIM","CDE_DIAG_2","CDE_DIAG_3","CDE_DIAG_4")
proc_cols <- c("CDE_PROC_PRIM")
drug_cols <- c("CDE_THERA_CLS_AHFS","CDE_NDC")

per_day_table_list <- list()
for (i in 1:length(analysis_IDs)){
  if (i %% 1000 == 0){
    print(i)
  }
  
  curr_id <- analysis_IDs[i]
  
  curr_healthclims <- health_claims[which(health_claims[,"study_id"] == curr_id),]
  curr_pharmclims <- pharm_claims[which(pharm_claims[,"study_id"] == curr_id),]

  curr_alldates <- unique(c(curr_healthclims[,"DTE_FIRST_SVC"],curr_pharmclims[,"DTE_FIRST_SVC"]))
  
  per_day_table <- as.data.frame(matrix(NA, nrow = length(curr_alldates),ncol = 5))
  colnames(per_day_table) <- c("study_id","claims_date","Diag_Codes","Proc_Codes","Drug_Codes")
  for (j in 1:length(curr_alldates)){
    per_day_table[,"study_id"] <- curr_id
    per_day_table[,"claims_date"] <- curr_alldates
    
    curr_date <- curr_alldates[j]
    curr_day_healthclaims <- curr_healthclims[which(curr_healthclims[,"DTE_FIRST_SVC"] == curr_date),]
    curr_day_drugclaims<- curr_pharmclims[which(curr_pharmclims[,"DTE_FIRST_SVC"] == curr_date),]
    
    #curr day code list
    curr_day_diag <- unique(unlist(curr_day_healthclaims[,diag_cols]))
    curr_day_proc <- unique(unlist(curr_day_healthclaims[,proc_cols]))
    curr_day_drug <- unique(unlist(curr_day_drugclaims[,drug_cols]))
    
    #clean codes
    curr_day_diag <- clean_code_func(curr_day_diag)
    curr_day_proc <- clean_code_func(curr_day_proc)
    curr_day_drug <- clean_code_func(curr_day_drug)
    
    
    #remove all blanks and NAs
    curr_day_diag <- remove_NA_func(curr_day_diag)
    curr_day_proc <- remove_NA_func(curr_day_proc)
    curr_day_drug <- remove_NA_func(curr_day_drug)
    
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
  
  per_day_table_list[[i]] <- per_day_table
  
}

#'@Question1: what to do witht the patients has no successive enrollment months?
all_perday_df <- do.call(rbind,per_day_table_list)
length(unique(all_perday_df$study_id)) # 13021
write.csv(all_perday_df,paste0(out_dir,"All_PerDay_Data_Medicaid.csv"),row.names = F)
