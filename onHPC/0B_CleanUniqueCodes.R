source("Recapse_Ultility.R")

##### Functions for code processes
get_cleancode_onetype <- function(in_data,code_type, code_col){
  # in_data   <- data_df1 
  # code_col <- ICD_diag_cols
  
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
data_dir <- "/recapse/intermediate_data/0_Codes/BeforeClean_UniqueCodes/"
drug_name_dir <- "/recapse/data/"
outdir   <- "/recapse/intermediate_data/0_Codes/AfterClean_UniqueCodes/"

# #local
data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/0_Codes/BeforeClean_UniqueCodes/"
drug_name_dir <- "/Volumes/LJL_ExtPro/Data/ReCAPSE_Data/"
outdir   <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/0_Codes/AfterClean_UniqueCodes/"
#######################################################################
############################## Medicaid  ############################## 
#######################################################################
Diag_code1 <- read.xlsx(paste0(data_dir, "0_unique_Diag_Codes_Medicaid.xlsx"),sheet = 1)
Proc_code1 <- read.xlsx(paste0(data_dir, "0_unique_Proc_Codes_Medicaid.xlsx"),sheet = 1)
Drug_code1 <- read.xlsx(paste0(data_dir, "0_unique_Drug_Codes_Medicaid.xlsx"),sheet = 1)


#######################################################################
############################## Medicare  ############################## 
#######################################################################
Diag_code2 <- read.xlsx(paste0(data_dir, "0_unique_Diag_Codes_Medicare.xlsx"),sheet = 1)
Proc_code2 <- read.xlsx(paste0(data_dir, "0_unique_Proc_Codes_Medicare.xlsx"),sheet = 1)
Drug_code2 <- read.xlsx(paste0(data_dir, "0_unique_Drug_Codes_Medicare.xlsx"),sheet = 1)



##############################################################################
###If codes both in medcaid and medicare, change the CLAIMS source after combine
##############################################################################
common_diag_codes <- intersect(Diag_code1[,"CODE"],Diag_code2[,"CODE"])
common_proc_codes <- intersect(Proc_code1[,"CODE"],Proc_code2[,"CODE"])
common_drug_codes <- intersect(Drug_code1[,"CODE"],Drug_code1[,"CODE"])

#######################################################################
############################## Combine   ############################## 
#######################################################################
#Combine
Comb_diag <- rbind(Diag_code1,Diag_code2)
Comb_proc <- rbind(Proc_code1,Proc_code2)
Comb_drug <- rbind(Drug_code1,Drug_code2)

#If codes both in medcaid and medicare, change the CLAIMS source after combine
Comb_diag[which(Comb_diag[,"CODE"] %in% common_diag_codes),"CLAIM"] <- "BOTH"
Comb_proc[which(Comb_proc[,"CODE"] %in% common_proc_codes),"CLAIM"] <- "BOTH"
Comb_drug[which(Comb_drug[,"CODE"] %in% common_drug_codes),"CLAIM"] <- "BOTH"

#remove duplicates
Comb_diag <- Comb_diag[!duplicated(Comb_diag[,"CODE"]),] #remove duplicates
Comb_proc <- Comb_proc[!duplicated(Comb_proc[,"CODE"]),] #remove duplicates
Comb_drug <- Comb_drug[!duplicated(Comb_drug[,"CODE"]),] #remove duplicates

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

print(table(nchar(as.character(final_proc_code_df$CODE))))

# write.xlsx(final_diag_code_df,paste0(outdir,"0_Cleaned_Unique_Diag_Codes.xlsx"))
# write.xlsx(final_proc_code_df,paste0(outdir,"0_Cleaned_Unique_Proc_Codes.xlsx"))
# write.xlsx(final_drug_code_df,paste0(outdir,"0_Cleaned_Unique_Drug_Codes.xlsx"))

# #Clean unique codes
# Diag_ICD_code1  <- get_cleancode_onetype(data_df1,"DIAG_ICD9or10",ICD_diag_cols)
# Proc_HCPC_Code1 <- get_cleancode_onetype(data_df1,"PROC_HCPCS",HCPCS_proc_cols)
# Drug_AHFS_Code1 <- get_cleancode_onetype(data_df2,"DRUG_THERA_CLS_AHFS",AHFS_drug_cols)
# Drug_NDC_Code1  <- get_cleancode_onetype(data_df2,"DRUG_NDC",NDC_drug_cols)



