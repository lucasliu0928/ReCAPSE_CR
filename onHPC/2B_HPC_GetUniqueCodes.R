library(openxlsx)
library(data.table)
source("Recapse_Ultility.R")

################################################################################
#Data dir
################################################################################
#on HPC
data_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
outdir <- "/recapse/intermediate_data/2_Codes_And_Groups/"


#local
data_dir <- "/Volumes/LJL_ExtPro/Data/Testing data for UH3 - Dec 16 2020/"
outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/2_Codes_And_Groups/"

################################################################################
#2.Load claims from medicare
################################################################################
medicare_claims_df <- as.data.frame(fread(paste0(data_dir,"kcr_medicare_claims_fb0015.csv")))

#Codes columns for medicare
medicare_diag_cols <- paste0("DGNS_CD",seq(1,25))
medicare_proc_cols <- c(paste0("PRCDRCD",seq(1,25)),"HCPCS_CD")
medicare_drug_cols <- c("NDC_CD","PROD_SRVC_ID")

#Clean codes
cleaned_medicareClaims_df <- clean_code_columns(medicare_claims_df,c(medicare_diag_cols,medicare_proc_cols,medicare_drug_cols))


################################################################################
#3.Load claims from medicare
################################################################################
medicaid_health_df <- as.data.frame(fread(paste0(data_dir,"kcr_medicaid_healthclaims_fb0015.csv")))
medicaid_pharm_df <- as.data.frame(fread(paste0(data_dir,"KCR_MEDICAID_PHARMCLAIMS_FB0015.csv")))


#Codes columns for medicaid
medicaid_diag_cols <- c("CDE_DIAG_PRIM","CDE_DIAG_2","CDE_DIAG_3","CDE_DIAG_4")
medicaid_proc_cols <- c("CDE_PROC_PRIM")
medicaid_drug_cols <- c("CDE_THERA_CLS_AHFS","CDE_NDC")

#Clean code columns
cleaned_healthClaims <- clean_code_columns(medicaid_health_df,c(medicaid_diag_cols,medicaid_proc_cols))
cleaned_pharmClaims <-  clean_code_columns(medicaid_pharm_df,medicaid_drug_cols)


################################################################################
#3.Get all unique codes 
################################################################################
#Get unique code for medicare
unique_diag_codes1 <- as.data.frame(unique(unlist(cleaned_medicareClaims_df[,medicare_diag_cols])))
colnames(unique_diag_codes1) <- "Unique_Diag_Code"
unique_proc_codes1 <- as.data.frame(unique(unlist(cleaned_medicareClaims_df[,medicare_proc_cols])))
colnames(unique_proc_codes1) <- "Unique_Proc_Code"
unique_drug_codes1 <- as.data.frame(unique(unlist(cleaned_medicareClaims_df[,medicare_drug_cols])))
colnames(unique_drug_codes1) <- "Unique_Drug_Code"

#Get unique code for medicaid
unique_diag_codes2 <- as.data.frame(unique(unlist(cleaned_healthClaims[,medicaid_diag_cols])))
colnames(unique_diag_codes2) <- "Unique_Diag_Code"
unique_diag_codes2$Code_Column_Name <- "CDE"

unique_proc_codes2 <- as.data.frame(unique(unlist(cleaned_healthClaims[,medicaid_proc_cols])))
colnames(unique_proc_codes2) <- "Unique_Proc_Code"
unique_proc_codes2$Code_Column_Name <- "CDE"

unique_drug_codes2 <- as.data.frame(unique(unlist(cleaned_pharmClaims[,medicaid_drug_cols])))
colnames(unique_drug_codes2) <- "Unique_Drug_Code"
unique_drug_codes2$Code_Column_Name <- ""

unique_drug_codes2 <- get_unique_codes(cleaned_pharmClaims,"CDE_THERA_CLS_AHFS","Drug")
unique_drug_codes2 <- get_unique_codes(cleaned_pharmClaims,"CDE_NDC","Drug")

get_unique_codes <- function(claims_df,code_column,code_type){
  #claims_df <- cleaned_pharmClaims
  #code_column <- "CDE_THERA_CLS_AHFS"
  #code_type <- "Drug"
  
  #get unique codes
  unique_codes_df <- as.data.frame(unique(unlist(claims_df[,code_column])))
  colnames(unique_codes_df) <- paste0("Unique_", code_type,"_Code")
  
  #add code column info
  unique_codes_df$Code_Column_Name <- code_column
  
  #remove NA
  na_idxes <- which(is.na(unique_codes_df[,1]) == T)
  unique_codes_df <- unique_codes_df[-na_idxes,]
  
  return(unique_codes_df)
}


#Combine both
Final_unique_diag <- rbind(unique_diag_codes1,unique_diag_codes2)
Final_unique_proc <- rbind(unique_proc_codes1,unique_proc_codes2)
Final_unique_drug <- rbind(unique_drug_codes1,unique_drug_codes2)

################################################################################
#4.Code grouping
################################################################################
