source("Recapse_Ultility.R")

count_code_freq_patient_level <- function(unique_code_df,perPts_unique_codes_df,code_col,outdir,out_subdir){
  unique_codes <- unique_code_df[,"CODE"]
  
  total_pts <- nrow(perPts_unique_codes_df)
  SBCE_pts_indexes <- which(perPts_unique_codes_df$SBCE==1)
  nonSBCE_pts_indexes <- which(perPts_unique_codes_df$SBCE==0)
  
  code_ct_df <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(unique_code_df)))
  colnames(code_ct_df) <- colnames(unique_code_df)
  
  code_ct_df$N_PTs_HASCODE <- NA
  code_ct_df$Frac_PTS_HASCODE <- NA
  
  code_ct_df$N_SBCE_PTS_HASCODE <- NA
  code_ct_df$Frac_SBCE_PTS_HASCODE <- NA
  
  code_ct_df$N_nonSBCE_PTS_HASCODE <- NA
  code_ct_df$Frac_nonSBCE_PTS_HASCODE <- NA
  
  
  foreach (i = 1: length(unique_codes)) %dopar% {
    curr_code <- as.character(unique_codes[i])
    search_string <- paste0("\\b",curr_code,"\\b")
    
    code_ct_df[1,1:ncol(unique_code_df)] <- unique_code_df[i,]
    code_ct_df$CODE <- as.character(code_ct_df$CODE)
    
    #all pts
    code_ct_df[1,"N_PTs_HASCODE"]    <- length(which(grepl(search_string,perPts_unique_codes_df[,code_col])==T))
    code_ct_df[1,"Frac_PTS_HASCODE"] <- code_ct_df[1,"N_PTs_HASCODE"] /total_pts
    
    #SBCE pts
    code_ct_df[1,"N_SBCE_PTS_HASCODE"]    <- length(which(grepl(search_string,perPts_unique_codes_df[SBCE_pts_indexes,code_col])==T))
    code_ct_df[1,"Frac_SBCE_PTS_HASCODE"] <-  code_ct_df[1,"N_SBCE_PTS_HASCODE"]/length(SBCE_pts_indexes)
    
    #non_SBCE pts
    code_ct_df[1,"N_nonSBCE_PTS_HASCODE"]    <- length(which(grepl(search_string,perPts_unique_codes_df[nonSBCE_pts_indexes,code_col])==T))
    code_ct_df[1,"Frac_nonSBCE_PTS_HASCODE"] <- code_ct_df[1,"N_nonSBCE_PTS_HASCODE"] /length(nonSBCE_pts_indexes)
    
    write.xlsx(code_ct_df,paste0(outdir,out_subdir, curr_code,".xlsx"))
    
  }
}

#######################################################################
##############              Data dir                     ############## 
#######################################################################
code_data_dir <- "/recapse/intermediate_data/0_Codes/"
data_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
outdir   <- "/recapse/intermediate_data/0_Codes/"

#local
code_data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/0_Codes/"
data_dir <- "/Volumes/LJL_ExtPro/Data/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/"
outdir   <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/0_Codes/"

#######################################################################
#1.Unique codes
#######################################################################
unique_diag_df <- read.xlsx(paste0(code_data_dir,"0_Cleaned_Unique_Diag_Codes.xlsx"),sheet = 1)
unique_proc_df <- read.xlsx(paste0(code_data_dir,"0_Cleaned_Unique_Proc_Codes.xlsx"),sheet = 1)
unique_drug_df <- read.xlsx(paste0(code_data_dir,"0_Cleaned_Unique_Drug_Codes.xlsx"),sheet = 1)

unique_proc_df$CODE
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

data_hasCode <- data_df1[which(data_df1[,c(HCPCS_proc_cols)] == "00179"),]

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
