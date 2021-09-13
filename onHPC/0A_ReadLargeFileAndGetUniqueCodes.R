source("Recapse_Ultility.R")

#######################################################################
##############              Data dir                     ############## 
#######################################################################
data_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
outdir   <- "/recapse/intermediate_data/0_Codes/BeforeClean_UniqueCodes/"

# #local
#data_dir <- "/Volumes/LJL_ExtPro/Data/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/"
#outdir   <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/0_Codes/BeforeClean_UniqueCodes/"
#######################################################################
############################## Medicaid  ############################## 
#######################################################################
#Data
data_df1 <- read.csv(paste0(data_dir,"kcr_medicaid_healthclaims_fb0015.csv"),stringsAsFactors = F)
data_df2 <- read.csv(paste0(data_dir,"KCR_MEDICAID_PHARMCLAIMS_FB0015.csv"),stringsAsFactors = F)

#Codes columns
ICD_diag_cols   <- c("CDE_DIAG_PRIM","CDE_DIAG_2","CDE_DIAG_3","CDE_DIAG_4") #ICD 9 or ICD10
HCPCS_proc_cols <- c("CDE_PROC_PRIM")                                        #HCPCS
AHFS_drug_cols  <- c("CDE_THERA_CLS_AHFS")
NDC_drug_cols   <- c("CDE_NDC")

#1.Unique Diag codes (ICD9 or ICD10)
Diag_ICD_code1 <- get_uniquecodes_onetype(data_df1,"DIAG_ICD9or10",ICD_diag_cols,"Medicaid")

#2.Unique Proc codes (HCPC)
Proc_HCPC_Code1 <- get_uniquecodes_onetype(data_df1,"PROC_HCPCS",HCPCS_proc_cols,"Medicaid")

#3.Unique Drug
Drug_AHFS_Code1 <- get_uniquecodes_onetype(data_df2,"DRUG_THERA_CLS_AHFS",AHFS_drug_cols,"Medicaid")
Drug_NDC_Code1  <- get_uniquecodes_onetype(data_df2,"DRUG_NDC",NDC_drug_cols,"Medicaid")
Drug_comb1 <- rbind(Drug_AHFS_Code1,Drug_NDC_Code1)

write.xlsx(Diag_ICD_code1,paste0(outdir, "0_unique_Diag_Codes_Medicaid.xlsx"))
write.xlsx(Proc_HCPC_Code1,paste0(outdir,"0_unique_Proc_Codes_Medicaid.xlsx"))
write.xlsx(Drug_comb1,paste0(outdir,"0_unique_Drug_Codes_Medicaid.xlsx"))

#######################################################################
############################## Medicare  ############################## 
#######################################################################
#Data (Do not use fread, it converts large interge to scitific notation )
data_df <- read.csv(paste0(data_dir,"kcr_medicare_claims_fb0015.csv"),stringsAsFactors = F)

#Code cols
ICD_diag_cols <- paste0("DGNS_CD",seq(1,25))             #ICD9 or ICD10
HCPCS_proc_cols     <- "HCPCS_CD"                        #HCPCS
ICD_procedure_cols <- paste0("PRCDRCD", seq(1,25,1))     #ICD9 or ICD10
NDC_drug_cols <- c("NDC_CD","PROD_SRVC_ID")              #NDC

#1.Unique Diag codes
#1.1 ICD9 or ICD10 codes
Diag_ICD_code2 <- get_uniquecodes_onetype(data_df,"DIAG_ICD9or10",ICD_diag_cols,"Medicare")

#2. Unique Drug
Drug_NDC_Code2  <- get_uniquecodes_onetype(data_df,"DRUG_NDC",NDC_drug_cols,"Medicare")

#3.Unique Proc codes (HCPC)
Proc_HCPC_Code2 <- get_uniquecodes_onetype(data_df,"PROC_HCPCS",HCPCS_proc_cols,"Medicare")

#3 Unique Proc code (ICD) 
Proc_ICD_Code2  <- get_uniquecodes_onetype(data_df,"PROC_ICD9or10",ICD_procedure_cols,"Medicare")

#Comb Proc
Comb_Proc2 <- rbind(Proc_HCPC_Code2,Proc_ICD_Code2)

write.xlsx(Diag_ICD_code2,paste0(outdir, "0_unique_Diag_Codes_Medicare.xlsx"))
write.xlsx(Drug_NDC_Code2,paste0(outdir,"0_unique_Drug_Codes_Medicare.xlsx"))
write.xlsx(Comb_Proc2,paste0(outdir,"0_unique_Proc_Codes_Medicare.xlsx"))

