#This script cleaned unique diag/proc/drug codes from per day data in claims
#Since codes in perData data are clean, thus the resulting unique codes are also clean
proj_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/"
source(paste0(proj_dir,"ReCAPSE_Code/Ultilities.R"))


data_dir <- paste0(proj_dir,"/ReCAPSE_Intermediate_Data/0318_21/For_Both_Data/")
outdir <- paste0(proj_dir,"/ReCAPSE_Intermediate_Data/0318_21/For_Both_Data/")


################################################################################ 
#### Load combined per day data
################################################################################ 
Comb_perday_df <- read.csv(paste0(data_dir, "Comb_PerDay_data.csv"),stringsAsFactors = F)
Comb_perday_df <- Comb_perday_df[,-1]

################################################################################ 
#####Get ALl unique diag,procedure, drug codes
################################################################################ 
all_diag_codes <- Comb_perday_df[,"Diag_Codes"]
unique_Diag_codes <- unique(unlist(strsplit(all_diag_codes,split = "$$$$",fixed = T)))  #30047 diag codes
all_Proc_Codes <- Comb_perday_df[,"Proc_Codes"]
unique_Proc_codes <- unique(unlist(strsplit(all_Proc_Codes,split = "$$$$",fixed = T))) #17737 procedure codes
all_Drug_Codes <- Comb_perday_df[,"Drug_Codes"]
unique_Drug_codes <- unique(unlist(strsplit(all_Drug_Codes,split = "$$$$",fixed = T))) #48943 durg codes
#'@NOTE: because some of the drug codes are prepend 0s, but they are acutally should be prepended
#'When processing the drug data in perday data, remember to do this also
unique_Drug_codes <- unique(as.numeric(unique_Drug_codes)) #41271 
  
#Remove NA
na_idxes <- which(is.na(unique_Diag_codes)==T)
unique_Diag_codes<- unique_Diag_codes[-na_idxes]

na_idxes <- which(is.na(unique_Proc_codes)==T)
unique_Proc_codes<- unique_Proc_codes[-na_idxes]

na_idxes <- which(is.na(unique_Drug_codes)==T)
unique_Drug_codes<- unique_Drug_codes[-na_idxes]

unique_Diag_codes_df <- as.data.frame(unique_Diag_codes)

unique_Proc_codes_df <- as.data.frame(unique_Proc_codes)

unique_Drug_codes_df <- as.data.frame(unique_Drug_codes)

nrow(unique_Diag_codes_df) #30046
nrow(unique_Proc_codes_df) #17736
nrow(unique_Drug_codes_df) #41271

write.csv(unique_Diag_codes_df,paste0(outdir,"All_unique_Diag_codes_Cleaned.csv"),row.names = F)
write.csv(unique_Proc_codes_df,paste0(outdir,"All_unique_Proc_codes_Cleaned.csv"),row.names = F)

################################################################################ 
##For drug codes, get the drug name
################################################################################ 
#1. Load drug name list
drug_name_list <- read.csv(paste0(proj_dir,"/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/DrugList.csv"),stringsAsFactors = F,header = F)

##.Get drug name
unique_Drug_codes_df$drug_name <- NA
unique_Drug_codes_df$drug_name <- drug_name_list[match(gsub("Drug_","",unique_Drug_codes_df[,"unique_Drug_codes"]),drug_name_list[,"V1"],),"V2"]
write.csv(unique_Drug_codes_df,paste0(outdir,"All_unique_Drug_codes_Cleaned.csv"),row.names = F)


