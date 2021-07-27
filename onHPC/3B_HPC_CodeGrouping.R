library(openxlsx)
library(data.table)
source("Recapse_Ultility.R")

################################################################################
#Data dir
################################################################################
#on HPC
data_dir <- "/recapse/intermediate_data/3_perDay_PerPatientData/"
raw_data_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
outdir <- "/recapse/intermediate_data/3B_Codes_And_Groups/"

#local
data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/3_perDay_PerPatientData/"
raw_data_dir <- "/Volumes/LJL_ExtPro/Data/Testing data for UH3 - Dec 16 2020/"
outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/3B_Codes_And_Groups/"

################################################################################
#1. Load data
################################################################################
#1. Get drung names
drug_name_df <- read.csv(paste0(raw_data_dir,"DrugList.csv"),stringsAsFactors = F,header = F)

#2. Load CCS Groups file:                                      
CCS_df <- load_and_clean_CSS_data(raw_data_dir)
CCS_Diag_df <- CCS_df[which(CCS_df[,"CODE_TYPE"] %in% c("ICD9_Diag","ICD10_Diag")),]
CCS_Proc_df <- CCS_df[which(CCS_df[,"CODE_TYPE"] %in% c("ICD9_Proc","ICD10_Proc")),]

#3. Load Chuback Groups file:                                
Chuback_df <- load_and_clean_Chuback_data(raw_data_dir)
Chuback_Diag_df <- Chuback_df[which(Chuback_df$Code.type %in% c("ICD-9 diagnosis")),]
Chuback_Proc_df <- Chuback_df[which(Chuback_df$Code.type %in% c("HCPC","ICD-9 procedure","CPT", "CPT category II","CPT category III")),]

#4. load Ritzwoller file:  
Ritzwoller_df <- load_and_clean_Ritzwoller_data(raw_data_dir)
Ritzwoller_Diag_df <- Ritzwoller_df[which(Ritzwoller_df$D_or_P == "Diagnostic"),] #48
Ritzwoller_Proc_df <- Ritzwoller_df[which(Ritzwoller_df$D_or_P == "Procedure"),] #1008

#5.Load drug group 
DM3_df <- load_and_clean_DM3_data(raw_data_dir)


################################################################################
#2.Get perDay data for all patients and unique codes
################################################################################
perDay_files <- list.files(data_dir)
perDay_df <- do.call(rbind,lapply(paste0(data_dir,perDay_files), read.xlsx))

all_unique_diag_codes <- split_code_strings_to_unique_codes(perDay_df,"Diag_Codes")
unique_diag_df <- as.data.frame(all_unique_diag_codes)
colnames(unique_diag_df) <- "Unique_Diag_Codes"

all_unique_proc_codes <- split_code_strings_to_unique_codes(perDay_df,"Proc_Codes")
unique_proc_df <- as.data.frame(all_unique_proc_codes)
colnames(unique_proc_df) <- "Unique_Proc_Codes"

all_unique_drug_codes <- split_code_strings_to_unique_codes(perDay_df,"Drug_Codes")
unique_drug_df <- as.data.frame(all_unique_drug_codes)
colnames(unique_drug_df) <- "Unique_Drug_Codes"

#Add drug name to unique drug df
drug_name_df <- drug_name_df[which(drug_name_df$V1 %in% all_unique_drug_codes),] #Filter out durg name df for code in claims
unique_drug_df$Drug_name <- NA
for (i in 1:nrow(unique_drug_df)){
  if (i %% 1000 == 0 ){print(i)}
  curr_drug <- unique_drug_df[i,"Unique_Drug_Codes"]
  curr_idxes <- which(drug_name_df$V1 == curr_drug)
  if (length(curr_idxes) > 0){
    unique_drug_df[i,"Drug_name"] <- drug_name_df[curr_idxes,2]
  }
}

#Clean drug names
unique_drug_df[,"Drug_name"] <- gsub("[[:punct:]]"," ",unique_drug_df[,"Drug_name"])
unique_drug_df[,"Drug_name"] <- trimws(unique_drug_df[,"Drug_name"], which = c("both"), whitespace = "[ \t\r\n]")

################################################################################
#3. Code grouping
################################################################################
#1.CCS diagnose codes
grouped_unique_diag_df <- group_codes_into_CCS_func(unique_diag_df,CCS_Diag_df)
length(which(is.na(grouped_unique_diag_df$CCS_CATEGORY)==T)) #1594 no grps
length(unique(grouped_unique_diag_df$CCS_CATEGORY)) #265 grps
write.csv(grouped_unique_diag_df,paste0(outdir,"CCS_grouped_unique_diag_df.csv"),row.names = F)

#2. CCS  procedure codes
grouped_unique_proc_df <- group_codes_into_CCS_func(unique_proc_df,CCS_Proc_df)
length(which(is.na(grouped_unique_proc_df$CCS_CATEGORY)==T)) #10811 no grps
length(unique(grouped_unique_proc_df$CCS_CATEGORY)) #216 grps
write.csv(grouped_unique_proc_df,paste0(outdir,"CCS_grouped_unique_proc_df.csv"),row.names = F)

#3. Chuback diagnose codes
grouped_unique_diag_df_chuback <- group_codes_into_Chuback_func(unique_diag_df,Chuback_Diag_df)
write.csv(grouped_unique_proc_df,paste0(outdir,"Chuabck_grouped_unique_proc_df.csv"),row.names = F)

#4. Chuback procedure codes
grouped_unique_proc_df_chuback <- group_codes_into_Chuback_func(unique_proc_df,Chuback_Proc_df)

#5. Ritzwoller diagnose codes
grouped_unique_diag_df_ritz <- group_codes_into_Ritzwoller_func(unique_diag_df,Ritzwoller_Diag_df)
#6. Chuback procedure codes
grouped_unique_proc_df_ritz <- group_codes_into_Ritzwoller_func(unique_proc_df,Ritzwoller_Proc_df)

#6. DM3 drug codes
grouped_unique_drug_df <- group_drugcodes_into_DM3_func(unique_drug_df,DM3_df)
write.csv(grouped_unique_drug_df,paste0(outdir,"DM3_grouped_unique_drug_df.csv"),row.names = F)
