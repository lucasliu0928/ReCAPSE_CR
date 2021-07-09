source("Recapse_Ultility.R")

#onHPC
data_dir <- "/recapse/intermediate_data/"
drug_name_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
outdir <- "/recapse/intermediate_data/"
grp_dir <- 

#local
data_dir <- "/Users/lucasliu/Desktop/intermediate_data/"
drug_name_dir <- "/Volumes/LJL_ExtPro/Data/Testing data for UH3 - Dec 16 2020/"
outdir <- "/Users/lucasliu/Desktop/intermediate_data/"


################################################################################ 
#1. Load all per month with char data
################################################################################ 
All_data <- read.csv(paste0(data_dir,"10_All_PerMonthData_WithMonthChar_df.csv"), stringsAsFactors = F)

################################################################################ 
#1. Load drug names
################################################################################ 
drug_name_df <- read.csv(paste0(drug_name_dir,"DrugList.csv"),stringsAsFactors = F,header = F)

################################################################################ 
##2. Unique Codes in entire data
################################################################################ 
all_unique_diag_codes <- split_code_strings_to_unique_codes(All_data,"Diag_Codes")
all_unique_diag_codes_df <- as.data.frame(all_unique_diag_codes)
colnames(all_unique_diag_codes_df) <- "Unique_Diag_Codes"
write.csv(all_unique_diag_codes_df,paste0(outdir,"11_all_unique_diag_codes_df.csv"),row.names = F)

all_unique_proc_codes <- split_code_strings_to_unique_codes(All_data,"Proc_Codes")
all_unique_proc_codes_df <- as.data.frame(all_unique_proc_codes)
colnames(all_unique_proc_codes_df) <- "Unique_Proc_Codes"
write.csv(all_unique_proc_codes_df,paste0(outdir,"11_all_unique_proc_codes_df.csv"),row.names = F)

all_unique_drug_codes <- split_code_strings_to_unique_codes(All_data,"Drug_Codes")
all_unique_drug_codes_df <- as.data.frame(all_unique_drug_codes)
colnames(all_unique_drug_codes_df) <- "Unique_Drug_Codes"

#1. Filter out durg name df for code in calims
drug_name_df <- drug_name_df[which(drug_name_df$V1 %in% all_unique_drug_codes),]
#2. Add drug name to unique drug df
all_unique_drug_codes_df$Drug_name <- NA
for (i in 1:nrow(all_unique_drug_codes_df)){
  if (i %% 1000 == 0 ){print(i)}
  curr_drug <- all_unique_drug_codes_df[i,"Unique_Drug_Codes"]
  curr_idxes <- which(drug_name_df$V1 == curr_drug)
  if (length(curr_idxes) > 0){
    all_unique_drug_codes_df[i,"Drug_name"] <- drug_name_df[curr_idxes,2]
  }
}

write.csv(all_unique_drug_codes_df,paste0(outdir,"11_all_unique_drug_codes_df.csv"),row.names = F)

################################################################################ 
##3. Unique Codes Per Patient
################################################################################ 
Final_IDs <- unique(All_data$study_id)

unique_code_perPatient_df <- as.data.frame(matrix(NA, nrow = length(Final_IDs), ncol = 4))
colnames(unique_code_perPatient_df) <- c("study_id","Unique_Diag_Codes","Unique_Proc_Codes","Unique_Drug_Codes")
for (i in 1:length(Final_IDs)){
  if (i %% 1000 == 0){print(i)}
  curr_id <- Final_IDs[i]
  curr_df <- All_data[which(All_data$study_id == curr_id),]
  curr_unique_diag_codes <- split_andcombine_codes(curr_df,"Diag_Codes")
  curr_unique_proc_codes <- split_andcombine_codes(curr_df,"Proc_Codes")
  curr_unique_drug_codes <- split_andcombine_codes(curr_df,"Drug_Codes")
  
  unique_code_perPatient_df[i,"study_id"] <- curr_id
  unique_code_perPatient_df[i,"Unique_Diag_Codes"] <- curr_unique_diag_codes
  unique_code_perPatient_df[i,"Unique_Proc_Codes"] <- curr_unique_proc_codes
  unique_code_perPatient_df[i,"Unique_Drug_Codes"] <- curr_unique_drug_codes
  
}

write.csv(unique_code_perPatient_df,paste0(outdir,"11_unique_codes_perPatient_df.csv"),row.names = F)
