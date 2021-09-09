source("Recapse_Ultility.R")

#######################################################################
##############              Data dir                     ############## 
#######################################################################
code_data_dir <- "/recapse/intermediate_data/0_Codes/AfterClean_UniqueCodes/"
grping_data_dir <- "/recapse/data/"
outdir   <- "/recapse/intermediate_data/0_Codes/"

#local
code_data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/0_Codes/AfterClean_UniqueCodes/"
grping_data_dir <- "/Volumes/LJL_ExtPro/Data/ReCAPSE_Data/"
outdir   <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/0_Codes/"


################################################################################
#1. Load grping data
################################################################################
#1. Load CCS Groups file:                                      
CCS_df <- load_and_clean_CSS_data(grping_data_dir)
CCS_Diag_df <- CCS_df[which(CCS_df[,"CODE_TYPE"] %in% c("ICD9_Diag","ICD10_Diag")),]
CCS_Proc_df <- CCS_df[which(CCS_df[,"CODE_TYPE"] %in% c("ICD9_Proc","ICD10_Proc")),]

#3. Load Chuback Groups file:                                
Chuback_df <- load_and_clean_Chuback_data(grping_data_dir)
Chuback_Diag_df <- Chuback_df[which(Chuback_df$Code.type %in% c("ICD-9 diagnosis")),]
Chuback_Proc_df <- Chuback_df[which(Chuback_df$Code.type %in% c("HCPC","ICD-9 procedure","CPT", "CPT category II","CPT category III")),]

#4. load Ritzwoller file:  
Ritzwoller_df <- load_and_clean_Ritzwoller_data(grping_data_dir)
Ritzwoller_Diag_df <- Ritzwoller_df[which(Ritzwoller_df$D_or_P == "Diagnostic"),] #48
Ritzwoller_Proc_df <- Ritzwoller_df[which(Ritzwoller_df$D_or_P == "Procedure"),] #1008

#5.Load drug group 
DM3_df <- load_and_clean_DM3_data(grping_data_dir)

################################################################################
#2. Load unique codes data
################################################################################
unique_diag_df <- read.xlsx(paste0(code_data_dir,"0_Cleaned_Unique_Diag_Codes.xlsx"),sheet = 1)
unique_proc_df <- read.xlsx(paste0(code_data_dir,"0_Cleaned_Unique_Proc_Codes.xlsx"),sheet = 1)
unique_drug_df <- read.xlsx(paste0(code_data_dir,"0_Cleaned_Unique_Drug_Codes.xlsx"),sheet = 1)


################################################################################
#3. Code grouping
################################################################################
#1. diagnose codes
grouped_unique_diag_df <- group_codes_into_CCS_func(unique_diag_df,CCS_Diag_df) #add CCS
grouped_unique_diag_df <- group_codes_into_Chuback_func(grouped_unique_diag_df,Chuback_Diag_df) #add chuback
grouped_unique_diag_df <- group_codes_into_Ritzwoller_func(grouped_unique_diag_df,Ritzwoller_Diag_df) #add ritzwoller
write.csv(grouped_unique_diag_df,paste0(outdir,"Unique_Diag_And_Groups_allpts.csv"),row.names = F)

#2. procedure codes
grouped_unique_proc_df <- group_codes_into_CCS_func(unique_proc_df,CCS_Proc_df) #add CCS
grouped_unique_proc_df <- group_codes_into_Chuback_func(grouped_unique_proc_df,Chuback_Proc_df)  #add chuback
grouped_unique_proc_df <- group_codes_into_Ritzwoller_func(grouped_unique_proc_df,Ritzwoller_Proc_df)#add ritzwoller
write.csv(grouped_unique_proc_df,paste0(outdir,"Unique_Proc_And_Groups_allpts.csv"),row.names = F)

#3. DM3 drug codes
grouped_unique_drug_df <- group_drugcodes_into_DM3_func(unique_drug_df,DM3_df)
write.csv(grouped_unique_drug_df,paste0(outdir,"Unique_Drug_And_Groups_allpts.csv"),row.names = F)
