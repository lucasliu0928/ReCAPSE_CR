source("Recapse_Ultility.R")

##### Functions for code processes
get_cleancode_onetype <- function(in_data,code_type, code_col){
  # in_data   <- data_df1 
  # code_col <- ICD_diag_cols
  # 
  #Read code columns
  code_data      <-  in_data[,code_col]
  
  #Get non-NA  and non-Blanks codes
  non_na_or_blanks <- which(is.na(code_data) == F & code_data != "",arr.ind = T)
  unique_code_list <- unique(code_data[non_na_or_blanks])
  
  #Clean codes
  unique_code_list <- clean_code_func(unique_code_list)  #Clean codes , may return NAs
  non_na_or_blanks <- which(is.na(unique_code_list) == F & unique_code_list != "") #remove NA again
  unique_code_list <- unique_code_list[non_na_or_blanks]
  
  #Unique code list
  codes_list_df     <-  data.frame(unique_code_list)
  colnames(codes_list_df) <- "CODE"
  codes_list_df$TYPE <- code_type

  return(codes_list_df)
}

#######################################################################
##############              Data dir                     ############## 
#######################################################################
data_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
outdir   <- "/recapse/intermediate_data/0_Codes/"

#local
data_dir <- "/Volumes/LJL_ExtPro/Data/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/"
outdir   <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/0_Codes/"
#######################################################################
############################## Medicaid  ############################## 
#######################################################################
#Data
data_df1 <- data.frame(fread(paste0(data_dir,"kcr_medicaid_healthclaims_fb0015.csv")))
data_df2 <- data.frame(fread(paste0(data_dir,"KCR_MEDICAID_PHARMCLAIMS_FB0015.csv")))

#Codes columns
ICD_diag_cols   <- c("CDE_DIAG_PRIM","CDE_DIAG_2","CDE_DIAG_3","CDE_DIAG_4") #ICD 9 or ICD10
HCPCS_proc_cols <- c("CDE_PROC_PRIM")                                        #HCPCS
AHFS_drug_cols  <- c("CDE_THERA_CLS_AHFS")
NDC_drug_cols   <- c("CDE_NDC")

#1.Unique Diag codes
#1.1 ICD9 or ICD10 codes
Diag_ICD_code1 <- get_cleancode_onetype(data_df1,"DIAG_ICD9or10",ICD_diag_cols)

#2.Unique Proc codes
#2.1 HCPC 
Proc_HCPC_Code1 <- get_cleancode_onetype(data_df1,"PROC_HCPCS",HCPCS_proc_cols)

#3. Unique Drug
Drug_AHFS_Code1 <- get_cleancode_onetype(data_df2,"DRUG_THERA_CLS_AHFS",AHFS_drug_cols)
Drug_NDC_Code1  <- get_cleancode_onetype(data_df2,"DRUG_NDC",NDC_drug_cols)


#######################################################################
############################## Medicare  ############################## 
#######################################################################
#Data
data_df <- data.frame(fread(paste0(data_dir,"kcr_medicare_claims_fb0015.csv")))

#Code cols
ICD_diag_cols <- paste0("DGNS_CD",seq(1,25))             #ICD9 or ICD10
HCPCS_proc_cols     <- "HCPCS_CD"                        #HCPCS
ICD_procedure_cols <- paste0("PRCDRCD", seq(1,25,1))     #ICD9 or ICD10
NDC_drug_cols <- c("NDC_CD","PROD_SRVC_ID")              #NDC

#1.Unique Diag codes
#1.1 ICD9 or ICD10 codes
Diag_ICD_code2 <- get_cleancode_onetype(data_df,"DIAG_ICD9or10",ICD_diag_cols)

#2.Unique Proc codes
#2.1 Unique HCPC 
Proc_HCPC_Code2 <- get_cleancode_onetype(data_df,"PROC_HCPCS",HCPCS_proc_cols)

#2.2 Unique ICD 
Proc_ICD_Code2 <- get_cleancode_onetype(data_df,"PROC_ICD9or10",ICD_procedure_cols)

#3. Unique Drug
Drug_NDC_Code2  <- get_cleancode_onetype(data_df,"DRUG_NDC",NDC_drug_cols)


#######################################################################
############################## Combine   ############################## 
#######################################################################
#ICD Diagnose code
Diag_ICD_code <- rbind(Diag_ICD_code1,Diag_ICD_code2)
Diag_ICD_code <- Diag_ICD_code[!duplicated(Diag_ICD_code[,"CODE"]),] #remove duplicates

#NDC Drug code
Drug_NDC_Code <- rbind(Drug_NDC_Code1,Drug_NDC_Code2)
Drug_NDC_Code <- Drug_NDC_Code[!duplicated(Drug_NDC_Code[,"CODE"]),] #remove duplicates

#AHFS Drug code
Drug_AHFS_Code <- Drug_AHFS_Code1
Drug_AHFS_Code <- Drug_AHFS_Code[!duplicated(Drug_AHFS_Code[,"CODE"]),] #remove duplicates

#HCPC Procedure code
Proc_HCPC_Code <- rbind(Proc_HCPC_Code1,Proc_HCPC_Code2)
Proc_HCPC_Code <- Proc_HCPC_Code[!duplicated(Proc_HCPC_Code[,"CODE"]),] #remove duplicates

#ICD Procedure code
Proc_ICD_Code <- Proc_ICD_Code2
Proc_ICD_Code <- Proc_ICD_Code[!duplicated(Proc_ICD_Code[,"CODE"]),] #remove duplicates

#Combine drug code
final_diag_code_df <- Diag_ICD_code
final_drug_code_df <- rbind(Drug_NDC_Code,Drug_AHFS_Code)
final_proc_code_df <- rbind(Proc_HCPC_Code,Proc_ICD_Code)

write.xlsx(final_diag_code_df,paste0(outdir,"0_Cleaned_Unique_Diag_Codes.xlsx"))
write.xlsx(final_proc_code_df,paste0(outdir,"0_Cleaned_Unique_Proc_Codes.xlsx"))
write.xlsx(final_drug_code_df,paste0(outdir,"0_Cleaned_Unique_Drug_Codes.xlsx"))



