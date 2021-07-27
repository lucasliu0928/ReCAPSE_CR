library(openxlsx)
library(data.table)
source("Recapse_Ultility.R")

################################################################################
#Data dir
################################################################################
#on HPC
data_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
outdir <- "/recapse/intermediate_data/2_Codes_And_Groups/"
# 

# #local
# data_dir <- "/Volumes/LJL_ExtPro/Data/Testing data for UH3 - Dec 16 2020/"
# outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/2_Codes_And_Groups/"

################################################################################
#2.Load claims from medicare
################################################################################
#1.Load cliams
medicare_claims_df <- as.data.frame(fread(paste0(data_dir,"kcr_medicare_claims_fb0015.csv")))

#2.Codes columns for medicare
medicare_diag_cols <- paste0("DGNS_CD",seq(1,25))
medicare_proc_cols <- c(paste0("PRCDRCD",seq(1,25)),"HCPCS_CD")
medicare_drug_cols <- c("NDC_CD","PROD_SRVC_ID")

#3.Clean codes
cleaned_medicareClaims_df <- clean_code_columns(medicare_claims_df,c(medicare_diag_cols,medicare_proc_cols,medicare_drug_cols))


#4.Get unique code 
unique_Diag_df1 <- get_unique_codes(cleaned_medicareClaims_df,medicare_diag_cols,"ICD","Diag")
unique_Proc_df1A <- get_unique_codes(cleaned_medicareClaims_df,paste0("PRCDRCD",seq(1,25)),"ICD","Proc")
unique_Proc_df1B <- get_unique_codes(cleaned_medicareClaims_df,"HCPCS_CD","HCPCs","Proc")
unique_Drug_df1 <- get_unique_codes(cleaned_medicareClaims_df,medicare_drug_cols,"NDC","Drug")


################################################################################
#3.Load claims from medicare
################################################################################
#1.Load cliams
medicaid_health_df <- as.data.frame(fread(paste0(data_dir,"kcr_medicaid_healthclaims_fb0015.csv")))
medicaid_pharm_df <- as.data.frame(fread(paste0(data_dir,"KCR_MEDICAID_PHARMCLAIMS_FB0015.csv")))

#2. Codes columns for medicaid
medicaid_diag_cols <- c("CDE_DIAG_PRIM","CDE_DIAG_2","CDE_DIAG_3","CDE_DIAG_4")
medicaid_proc_cols <- c("CDE_PROC_PRIM")
medicaid_drug_cols <- c("CDE_THERA_CLS_AHFS","CDE_NDC")

#3.Clean code
cleaned_healthClaims <- clean_code_columns(medicaid_health_df,c(medicaid_diag_cols,medicaid_proc_cols))
cleaned_pharmClaims <-  clean_code_columns(medicaid_pharm_df,medicaid_drug_cols)

#4.Get unique code
unique_Diag_df2 <- get_unique_codes(cleaned_healthClaims,medicaid_diag_cols,"ICD","Diag")
unique_Proc_df2 <- get_unique_codes(cleaned_healthClaims,"CDE_PROC_PRIM","HCPCS","Proc")
unique_drug_df2A <- get_unique_codes(cleaned_pharmClaims,"CDE_THERA_CLS_AHFS","AHFS","Drug")
unique_drug_df2B <- get_unique_codes(cleaned_pharmClaims,"CDE_NDC","NDC","Drug")

################################################################################
#4.Combine all unique codes 
################################################################################
#Combine both
Final_diag <- rbind(unique_Diag_df1,unique_Diag_df2)
Final_unique_diag <- Final_diag[!duplicated(Final_diag[,"Unique_Diag_Code"]),]

Final_proc <- rbind(unique_Proc_df1A,unique_Proc_df1B,unique_Proc_df2)
Final_unique_proc <- Final_proc[!duplicated(Final_proc[,"Unique_Proc_Code"]),]

Final_drug <- rbind(unique_Drug_df1,unique_drug_df2A,unique_drug_df2B)
Final_unique_drug <- Final_drug[!duplicated(Final_drug[,"Unique_Drug_Code"]),]

write.xlsx(Final_unique_diag,paste0(outdir,"Unique_Diag_Codes.xlsx"))
write.xlsx(Final_unique_proc,paste0(outdir,"Unique_Proc_Codes.xlsx"))
write.xlsx(Final_unique_drug,paste0(outdir,"Unique_Drug_Codes.xlsx"))

################################################################################
#4.Code grouping
################################################################################
